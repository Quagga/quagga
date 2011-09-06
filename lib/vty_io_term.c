/* VTY IO TERM -- Telnet Terminal I/O
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Revisions: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
#include "misc.h"

#include "vty_local.h"
#include "vty_io.h"
#include "vty_io_term.h"
#include "vty_io_file.h"
#include "vty_cli.h"
#include "vty_command.h"
#include "vio_fifo.h"

#include "log_local.h"

#include "qstring.h"
#include "keystroke.h"

#include "memory.h"

#include "prefix.h"
#include "filter.h"
#include "privs.h"
#include "sockunion.h"
#include "sockopt.h"
#include "network.h"

#include <arpa/telnet.h>
#include <sys/socket.h>
#include <errno.h>

/*==============================================================================
 * The I/O side of Telnet VTY_TERMINAL.  The CLI side is vty_cli.c.
 *
 * A VTY_TERMINAL comes into being when a telnet connection is accepted, and
 * is closed either on command, or on timeout, or when the daemon is reset
 * or terminated.
 *
 * All VIN_TERM and VOUT_TERM I/O is non-blocking.
 */

/*==============================================================================
 * If possible, will use getaddrinfo() to find all the things to listen on.
 */
#if defined(HAVE_IPV6) && !defined(NRL)
# define VTY_USE_ADDRINFO 1
#else
# define VTY_USE_ADDRINFO 0
#endif

/*==============================================================================
 * Opening and closing VTY_TERMINAL type
 */

static void uty_term_read_ready(vio_vfd vfd, void* action_info) ;
static void uty_term_write_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_term_read_timeout(vio_timer timer,
                                                            void* action_info) ;
static vty_timer_time uty_term_write_timeout(vio_timer timer,
                                                            void* action_info) ;

typedef enum {          /* see uty_term_write()                 */
  utw_null        = 0,

  utw_error,

  utw_done,

  utw_blocked,
  utw_paused,
  utw_more_enter,

} utw_ret_t ;

static utw_ret_t uty_term_write(vio_vf vf) ;

static void uty_term_will_echo(vty_cli cli) ;
static void uty_term_will_suppress_go_ahead(vty_cli cli) ;
static void uty_term_dont_linemode(vty_cli cli) ;
static void uty_term_do_window_size(vty_cli cli) ;
static void uty_term_dont_lflow_ahead(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_TERMINAL -- ie attached to a telnet session.
 *
 * This is called by the accept action for the VTY_TERMINAL listener.
 */
static void
uty_term_open(int sock_fd, union sockunion *su)
{
  node_type_t  node ;
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  /* The initial vty->node will be authentication, unless the host does not
   * require that, in which case it may be a number of other things.
   *
   * Note that setting NULL_NODE at this point will cause the terminal to be
   * closed very quickly, after issuing suitable message.
   */
  node = (host.password != NULL) ? AUTH_NODE : NULL_NODE ;

  if (host.no_password_check)
    {
      if      (host.restricted_mode)
        node = RESTRICTED_NODE;
      else if (host.advanced && (host.enable == NULL))
        node = ENABLE_NODE;
      else
        node = VIEW_NODE;
    } ;

  /* Allocate new vty structure and set up default values.
   *
   * This completes the initialisation of the vty object, except that the
   * execution and vio objects are largely empty.
   */
  vty = uty_new(VTY_TERMINAL, node) ;
  vio = vty->vio ;

  /* Complete the initialisation of the vty_io object.
   *
   * Note that the defaults for:
   *
   *   - read_timeout     -- default = 0     => no timeout
   *   - write_timeout    -- default = 0     => no timeout
   *
   *   - parse_type       -- default = cmd_parse_standard
   *   - reflect_enabled  -- default = false
   *
   * Are OK, except that we want the read_timeout set to the current EXEC
   * timeout value.
   *
   * The text form of the address identifies the VTY.
   */
  vf = uty_vf_new(vio, sutoa(su).str, sock_fd, vfd_socket, vfd_io_read_write) ;

  uty_vin_push( vio, vf, VIN_TERM,  uty_term_read_ready,
                                    uty_term_read_timeout,
                                    0) ;        /* no ibuf required     */
  uty_vout_push(vio, vf, VOUT_TERM, uty_term_write_ready,
                                    uty_term_write_timeout,
                                    4096,       /* obuf is required     */
                                    true) ;     /* after buddy vin      */

  vf->read_timeout  = host.vty_timeout_val ; /* current EXEC timeout    */
  vf->write_timeout = 30 ;                   /* something reasonable    */

  /* Set up the CLI object & initialise                                 */
  vf->cli = uty_cli_new(vf) ;

  /* Issue Telnet commands/escapes to be a good telnet citizen -- not much
   * real negotiating going on -- just a statement of intentions !
   */
  uty_term_will_echo (vf->cli);
  uty_term_will_suppress_go_ahead (vf->cli);
  uty_term_dont_linemode (vf->cli);
  uty_term_do_window_size (vf->cli);
  if (0)
    uty_term_dont_lflow_ahead (vf->cli) ;

  /* Hoover up any currently available input -- possibly Telnet commands/
   * escapes from the other end.
   */
  uty_term_read(vf) ;

  /* Say hello                                                          */
  vty_hello(vty);

  /* If about to authenticate, issue friendly message.
   *
   * If cannot authenticate, issue an error message.
   */
  if      (vty->node == AUTH_NODE)
    vty_out(vty, "User Access Verification\n") ;
  else if (vty->node == NULL_NODE)
    vty_out(vty, "%% Cannot continue because no password is set\n") ;

  /* Enter the command loop.                                            */
  uty_cmd_queue_loop_enter(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Command line fetch from a VTY_TERMINAL -- in vf_open state.
 *
 * Fetching a command line <=> the previous command has completed.
 *
 * Returns:  CMD_SUCCESS  -- have another command line ready to go
 *       or: CMD_WAITING  -- would not wait for input
 *
 * This can be called in any thread.
 *
 * Note that does not signal CMD_EOF because that is handled for the
 * VTY_TERMINAL by the cmd_do_eof "special command".
 *
 * Note that this does no actual I/O, all that is done in the pselect() process,
 * while a command line is collected in the CLI.  So does not here return
 * CMD_IO_ERROR -- any errors are dealt with by signalling the command loop.
 */
extern cmd_return_code_t
uty_term_fetch_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vin_state == vf_open) ;

  return uty_cli_want_command(vf->cli, action) ;
} ;

/*------------------------------------------------------------------------------
 * Command has completed.
 *
 * If is cst_in_progress:
 *
 *   -- set cst_complete
 *
 *   -- collect current context
 *
 *   -- if is cos_idle then force cos_active -- the obuf etc. will be
 *      empty, but we want to make sure that all other buffers empty out,
 *      and that the output side will signal when that's so.
 *
 *   -- kick write_ready to make sure that things move along.
 */
extern void
uty_term_cmd_complete(vio_vf vf, cmd_context context)
{
  vty_cli cli = vf->cli ;

  VTY_ASSERT_LOCKED() ;

  if (cli->state == cst_in_progress)
    {
      cli->state = cst_complete ;

      if (cli->out_state == cos_idle)
        {
          vio_fifo_clear(vf->obuf, false) ;     /* keep end mark        */
          cli->out_state = cos_active ;
        } ;

      *cli->context = *context ;
      cli->auth_context = (   (cli->context->node == AUTH_NODE)
                           || (cli->context->node == AUTH_ENABLE_NODE) ) ;

      uty_term_set_readiness(vf, write_ready) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Showing error context for the VTY_TERMINAL command line.
 *
 * If the stack is at level == 1, then the last full line displayed will be
 * the line in which the error occurred (unless have monitor output, which
 * there is little we can do about).  So there is no further output required.
 * The command line is indented by the current prompt.
 *
 * If the stack is at level > 1, then may or may not have had output separating
 * the command line from the current position, so we output the command line
 * to provide context.
 *
 * Returns: indent position of command line
 */
extern uint
uty_term_show_error_context(vio_vf vf, vio_fifo ebuf, uint depth)
{
  if (depth == 1)
    return uty_cli_prompt_len(vf->cli) ;

  vio_fifo_printf(ebuf, "%% in command line:\n") ;
  vio_fifo_printf(ebuf, " %s\n", qs_make_string(vf->cli->clx)) ;

  return 2 ;
} ;

/*------------------------------------------------------------------------------
 * Push output to the terminal -- always not vf->blocking !
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                "final" => would have waited but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, the
 * background pselect process will complete the output and may signal the
 * result via uty_cmd_signal().
 */
extern cmd_return_code_t
uty_term_out_push(vio_vf vf, bool final)
{
  vty_cli cli = vf->cli ;
  utw_ret_t done ;
  cli_out_state_t out_state ;

  qassert(vf->vout_state == vf_open) ;
  qassert(!vf->blocking) ;

  out_state = cli->out_state & cos_mask ;

  /* If squelching, discard anything we have in the obuf.
   */
  if (vf->vio->cancel)
    vio_fifo_clear(vf->obuf, false) ;   /* keep end mark        */

  /* If we have something in the obuf that needs to be written, then if
   * is cos_idle, make sure the command line is clear, and set cos_active.
   *
   * Note that may be cos_idle | cos_monitor, for which we need to keep the
   * monitor flag but update to cos_active -- the cli will already be wiped.
   */
  if (!vio_fifo_empty(vf->obuf) && (out_state == cos_idle))
    {
      uty_cli_wipe(cli, 0) ;
      cli->out_state ^= (cos_active ^ cos_idle) ;
      vio_lc_counter_reset(cli->olc) ;
    } ;

  /* Give the terminal writing a shove.
   *
   * Note that there may be stuff pending, and stuff in the cbuf, etc.  We
   * depend on the uty_term_write() to know all about what can be done.
   *
   * If final, keep pushing while succeeds in writing without blocking.
   *
   * Note that is only called "final" when closing the vout, by which time
   * the "--more--" handling has been turned off and any output has been
   * released.
   */
  if (!final)
    done = uty_term_write(vf) ;
  else
    {
      do
        {
          vio_lc_counter_reset(cli->olc) ;
          done = uty_term_write(vf) ;
        } while (done == utw_paused) ;
    } ;

  /* Deal with the result
   *
   * If not failed and not final, set write ready if:
   *
   *    not utw_done   => output blocked.
   *
   *    cli->out_state == cos_idle  => CLI may run
   *
   * Return code depends on utw_xxx
   */
  if (done == utw_error)
    return CMD_IO_ERROR ;       /* don't set write ready        */

  if (!final)
    {
      if ((done != utw_done) || (cli->out_state == cos_idle))
        uty_term_set_readiness(vf, write_ready) ;
    } ;

  return (done == utw_done) ? CMD_SUCCESS : CMD_WAITING ;
} ;

/*------------------------------------------------------------------------------
 * Completed the cancelling of command(s) and output.
 *
 * Caller is about to output "^C" to signal completion of the cancellation
 * process -- sets the cli->state to cos_active, to allow final output to
 * proceed, and the command loop continue.
 *
 * Clears any cos_paused !
 *
 * If the current line is "drawn", make sure we are at the end of it, and then
 * clear the "drawn" state, because "^C\n" is about to follow.
 *
 * Clear the line control, to make sure that cannot now interfere.
 */
extern void
uty_term_out_cancelled(vio_vf vf)
{
  vty_cli cli = vf->cli ;

  qassert((cli->out_state & cos_mask) == cos_cancel) ;

  cli->out_state = cos_active | (cli->out_state & cos_monitor) ;

  vio_lc_counter_reset(cli->olc) ;

  uty_cli_cancel(cli, false) ;
} ;

/*------------------------------------------------------------------------------
 * The read side of the vfd has been closed.   Close down CLI as far as
 * possible, given that output may be continuing.
 *
 * Expects to be called once only for the VTY_TERMINAL.
 *
 * There is no difference between final and not-final close in this case.
 *
 * Note that this is only closed when the VTY_TERMINAL is forcibly closed, or
 * when the user quits.
 *
 * Returns:  CMD_SUCCESS   -- all is quiet.
 */
extern cmd_return_code_t
uty_term_read_close(vio_vf vf, bool final)
{
  vty_io  vio ;

  qassert(vf->vin_state == vf_end) ;

  /* Get the vio and ensure that we are all straight                    */
  vio = vf->vio ;
  qassert((vio->vin == vio->vin_base) && (vio->vin == vf)) ;

  /* Kill monitor state                                                 */
  uty_set_monitor(vio, off) ;

  /* Close the CLI as far as possible, leaving output side intact.
   *
   * Can generate some final output, which will be dealt with as the output
   * side is closed.
   */
  uty_cli_close(vf->cli, false) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Try to output the close reason to the given VOUT_TERM.
 *
 * If there is anything pending to be output, discard it, first.  The obuf has
 * already been cleared.
 *
 * This will be pushed out when the VOUT_TERM is finally closed.
 */
extern void
uty_term_close_reason(vio_vf vf, const char* reason)
{
  vio_lc_clear(vf->cli->olc) ;

  vf->cli->out_state = cos_idle ;       /* stamp on monitor or other output
                                         * clear cancel and paused      */
  uty_cli_out(vf->cli, "%% %s%s", reason, uty_cli_newline) ;
} ;

/*------------------------------------------------------------------------------
 * Close the writing side of VTY_TERMINAL.
 *
 * Assumes that the read side has been closed already, and so this is the last
 * thing to be closed.  Any monitor state was turned off earlier when the read
 * side was closed.
 *
 * Kicks the output side:
 *
 *   if final, will push as much as possible until all gone, would block or
 *   gets error.  In any event, closes the cli, final.
 *
 *   if not final, will push another tranche and let the uty_term_ready() keep
 *   pushing until buffers empty and can uty_cmd_signal().
 *
 * Returns:  CMD_SUCCESS    => all written,
 *                             or cannot write anything (more),
 *                             or final and wrote what could
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                "final" => would have waited but did not.
 *
 * NB: if an error occurs while writing, that will have been logged, but there
 *     is nothing more to be done about it here -- so does not return
 *     CMD_IO_ERROR.
 */
extern cmd_return_code_t
uty_term_write_close(vio_vf vf, bool final)
{
  cmd_return_code_t  ret ;
  vty_io  vio ;

  VTY_ASSERT_LOCKED() ;

  /* Get the vio and ensure that we are all straight
   *
   * Can only be the vout_base and must also be the vin_base, and the vin_base
   * must now be closed.
   */
  vio = vf->vio ;
  qassert((vio->vout == vio->vout_base) && (vio->vout == vf)) ;
  qassert((vio->vin  == vio->vin_base)  && (vio->vin->vin_state == vf_closed)) ;

  ret = CMD_SUCCESS ;

  if (vf->vout_state == vf_open)
    {
      ret = uty_term_out_push(vf, final) ;

      if (ret != CMD_WAITING)
        ret = CMD_SUCCESS ;
    } ;

  if (final)
    {
      vf->cli = uty_cli_close(vf->cli, final) ;

      qassert(vio->vty->type == VTY_TERMINAL) ;
      zlog (NULL, LOG_INFO, "Vty connection (fd %d) close",
                                                         vio_vfd_fd(vf->vfd)) ;
    } ;

  return ret ;
} ;

/*==============================================================================
 * Readiness and the VIN_TERM type vin.
 *
 * For TERM stuff the driving force is write ready.  This is used to prompt the
 * VOUT_TERM when there is outstanding output (obviously), but also if there
 * is buffered input in the keystroke stream.
 *
 * The VIN_TERM is read ready permanently, until eof is met.  Note that the
 * read timeout is reset each time uty_term_set_readiness is called.  When
 * eof is met, the VIN_TERM is read closed, which prevents any further setting
 * of read ready and its timeout.
 */

static void uty_term_ready(vio_vf vf) ;

/*------------------------------------------------------------------------------
 * Set read/write readiness -- for VIN_TERM/VOUT_TERM
 *
 * Is permanently read-ready (until eof or no longer vin_state == vf_open).
 */
extern void
uty_term_set_readiness(vio_vf vf, vty_readiness_t ready)
{
  VTY_ASSERT_LOCKED() ;

  if ((ready & write_ready) != 0)
    uty_vf_set_write(vf, on) ;

  uty_vf_set_read(vf, on) ;
} ;

/*------------------------------------------------------------------------------
 * Terminal read ready
 */
static void
uty_term_read_ready(vio_vfd vfd, void* action_info)
{
  vio_vf vf = action_info ;

  qassert(vfd == vf->vfd) ;

  vf->cli->out_state &= ~cos_paused ;   /* read ready clears paused     */

  uty_term_read(vf) ;
  uty_term_ready(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Terminal write ready
 */
static void
uty_term_write_ready(vio_vfd vfd, void* action_info)
{
  vio_vf vf = action_info ;

  qassert(vfd == vf->vfd) ;

  uty_term_ready(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Terminal, something is ready -- read, write or no longer paused.
 *
 *   1. attempts to clear any output it can.
 *
 *      The state of the output affects the CLI, so must always do this before
 *      before invoking the CLI.
 *
 *      If this write enters the "--more--" state, then will have tried to
 *      write away the prompt.
 *
 *   2. invokes the CLI
 *
 *      Which will do either the standard CLI stuff or the special "--more--"
 *      stuff.
 *
 *   3. attempts to write any output there now is.
 *
 *      If the CLI generated new output, as much as possible is written away
 *      now.
 *
 *      If this write enters the "--more--" state, then it returns now_ready,
 *      if the prompt was written away, which loops back to the CLI.
 *
 * Note that this is arranging:
 *
 *   a. to write away the "--more--" prompt as soon as the tranche of output to
 *      which it refers, completes
 *
 *   b. to enter the cli_more_wait CLI for the first time immediately after the
 *      "--more--" prompt is written away.
 *
 * The loop limits itself to one trache of command output each time.
 *
 * Resets the timer because something happened.
 */
static void
uty_term_ready(vio_vf vf)
{
  vty_readiness_t ready ;
  utw_ret_t done, done_before ;
  cli_out_state_t out_state ;

  VTY_ASSERT_LOCKED() ;

  /* Will attempt to write away any pending stuff, then for each call of
   * uty_term_ready, put out another tranche of output (unless in '--more--'
   * state).
   */
  if (!vf->cli->more_enabled)
    vio_lc_counter_reset(vf->cli->olc) ;        /* do one tranche       */

  /* Now loop kicking the CLI and the output, until stops changing.
   *
   * This is because the CLI may generate more to write, and writing stuff
   * away may release the CLI.
   */
  out_state = vf->cli->out_state ;
  done   = utw_null ;
  do
    {
      done_before = done ;

      /* Kick the CLI, which may advance either because there is more input,
       * or because some output has now completed, or for any other reason.
       *
       * This may return write_ready, which is a proxy for CLI ready, and
       * MUST be honoured, even (especially) if the output buffers are empty.
       */
      ready = uty_cli(vf->cli) ;

      /* Now try to write away any new output which may have been generated
       * by the CLI.
       *
       * Note that when enters "--more--" will return utw_more_enter, once,
       * which causes a loop back to uty_cli(), which will start the process.
       * When comes through here a second time, will return utw_done, once
       * any prompt etc has been output.
       */
      done = uty_term_write(vf) ;

      if (done == utw_error)
        return ;                /* quit if in error             */

    } while (done != done_before) ;

  if (done != utw_done)         /* isn't utw_error, either      */
    ready |= write_ready ;

  uty_term_set_readiness(vf, ready) ;

  /* Signal the command loop if the cli->out_state has changed significantly
   */
  if ((vf->cli->out_state == cos_idle) && (out_state != cos_idle))
    uty_cmd_signal(vf->vio, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Read timer has expired.
 *
 * Discard anything in the keystroke stream and set it "eof, timed-out".  This
 * will be picked up by the CLI and a cmd_do_timed-out will float out.
 */
static vty_timer_time
uty_term_read_timeout(vio_timer timer, void* action_info)
{
  vio_vf  vf   = action_info ;

  qassert(timer == vf->vfd->read_timer) ;

  VTY_ASSERT_LOCKED() ;

  keystroke_stream_set_eof(vf->cli->key_stream, true) ; /* timed out    */

  vf->cli->out_state &= ~cos_paused ;

  uty_term_ready(vf) ;

  return 0 ;
 } ;

/*------------------------------------------------------------------------------
 * Write timer has expired.
 *
 * Signal write timeout error.
 */
static vty_timer_time
uty_term_write_timeout(vio_timer timer, void* action_info)
{
  vio_vf  vf   = action_info ;

  qassert(timer == vf->vfd->write_timer) ;

  VTY_ASSERT_LOCKED() ;

  vf->cli->out_state &= ~cos_paused ;

  uty_vf_error(vf, verr_to_vout, 0) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Timeout on the cli->paused timer -- clear paused and treat as CLI ready.
 */
extern void
vty_term_pause_timeout(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  vty_cli cli ;

  VTY_LOCK() ;

  cli = timer_info ;
  assert(cli->pause_timer == qtr) ;

  if (cli->out_state & cos_paused)
    {
      cli->out_state ^= cos_paused ;
      uty_term_ready(cli->vf) ;
    } ;

  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Reading from VTY_TERMINAL.
 *
 * The select/pselect call-back ends up in uty_term_ready().
 */

/*------------------------------------------------------------------------------
 * Hoover up input and shovel into the keystroke stream
 *
 * NB: need not be in the term_ready path.  Indeed, when the VTY_TERMINAL is
 *     initialised, this is called to suck up any telnet preamble.
 *
 * NB: the terminal is permanently read-ready, so the keystroke stream
 *     should be permanently topped up.
 *
 * When reaches EOF on the input eof is set in the keystroke stream, and the
 * vfd is read closed.  Read closing the vfd turns off any timeout and prevents
 * any further setting of read_ready (to avoid permanent read_ready !).  Does
 * NOT set vf->vin_state to vf_end, because that is not true until the
 * keystroke stream is empty.  Once eof is set in the keystroke stream, this
 * code will not attempt any further input from the vfd.
 */
extern void
uty_term_read(vio_vf vf)
{
  keystroke_stream stream ;

  stream = vf->cli->key_stream ;

  if (keystroke_stream_met_eof(stream))
    return ;                    /* already seen EOF                     */

  if (vf->vin_state != vf_open)
    {
      /* If is not vf_open, but also not seen EOF on the keystroke stream,
       * then now is a good moment to empty out the keystroke stream and
       * force it to EOF.
       */
      keystroke_stream_set_eof(vf->cli->key_stream, false) ;
      return ;
    } ;

  /* OK: read from input and pass result to keystroke stream.
   *
   *     Uses a relatively small buffer, which should be fine for actual
   *     terminals !
   */
  while (1)
    {
      unsigned char buf[500] ;
      int get ;

      get = read_nb(vio_vfd_fd(vf->vfd), buf, sizeof(buf)) ;
                                /* -1 <=> error, -2 <=> EOF     */
      if (get == 0)
        return ;

      if (get != -1)
        {
          /* Not error.  get < 0 => EOF and that is set in key_stream.
           */
          keystroke_input(vf->cli->key_stream, buf, get) ;
        }
      else
        {
          /* Error: signal to command loop and force key_stream empty
           */
          uty_vf_error(vf, verr_io_vin, errno) ;
          keystroke_stream_set_eof(vf->cli->key_stream, false) ;
                                                   /* not timed-out    */
        } ;

      if (get > 0)
        continue ;              /* hoover up all there is.      */

      /* Error or EOF -- close the input half of the socket, and we're done.
       *
       * NB: this does not affect the vin_state, which (if not error) continues
       *     vf_open until hits EOF on the keystroke stream.
       */
      vio_vfd_read_close(vf->vfd) ;

      return ;
    } ;
} ;

/*==============================================================================
 * Writing to VOUT_TERM -- driven by ready state.
 *
 * There are two sets of buffering:
 *
 *   cli->cbuf -- command line   -- reflects the status of the command line
 *
 *   vf->obuf  -- command output -- which is written to the terminal only while
 *                                  cos_active.
 *
 * The cli output takes precedence.
 *
 * Output of command stuff is subject to line_control, and may go through the
 * "--more--" mechanism.
 */

static utw_ret_t uty_term_write_lc(vio_line_control lc, vio_vf vf,
                                                                 vio_fifo vff) ;

/*------------------------------------------------------------------------------
 * Have some (more) monitor output to send to the vty.
 *
 * Make sure the command line is clear, then claim screen for monitor output
 * and attempt to empty out buffers.
 */
extern void
uty_term_mon_write(vio_vf vf)
{
  utw_ret_t  done ;

  uty_cli_pre_monitor(vf->cli) ;        /* make sure in a fit state     */

  done = uty_term_write(vf) ;           /* this calls uty_vf_error() if
                                         * there are any errors         */
  if ((done != utw_done) && (done != utw_error))
    uty_term_set_readiness(vf, write_ready) ;
} ;

/*------------------------------------------------------------------------------
 * Write as much as possible of what there is.
 *
 * If there is anything in the CLI buffer, try to empty that.
 *
 * If is cos_monitor, try to empty the mbuf.
 *
 * If cos_active, try to empty the obuf.  Move to more_wait if required.
 *
 * If is not "in_progress", then when all buffers are emptied out, clears
 * itself and the cos_active state.
 *
 * Returns:
 *
 *        utw_error -- I/O error either now or sometime earlier.
 *
 *         utw_done -- have written everything can find
 *
 *      utw_blocked -- write blocked -- some write operation would block
 *
 *                     Need to set write ready & wait for it.
 *
 *       utw_paused -- have written as much as line control allows in one go.
 *
 *                     Note that is *not* in "--more--" state, this is all to
 *                     do with limiting work on each visit.
 *
 *                     Need to set write ready & wait for it.
 *
 *   utw_more_enter -- has just entered "--more--" state
 *
 *                     Need either to set write ready, or call the CLI.
 */
static utw_ret_t
uty_term_write(vio_vf vf)
{
  vty_cli cli = vf->cli ;
  utw_ret_t  ret ;
  int        did ;
  ulen       have, take ;

  VTY_ASSERT_LOCKED() ;

  /* If the vout is neither vf_open nor vf_closing, discard all buffered
   * output, and return all done.
   */
  if (vf->vout_state != vf_open)
    {
      qassert(vf->vout_state == vf_end) ;

      vio_fifo_clear(vf->obuf,  false) ;
      vio_fifo_clear(cli->cbuf, false) ;
      vio_lc_clear(cli->olc) ;

      cli->out_state = cos_idle ;       /* stamps on everything */

      return utw_error ;
    } ;

  /* Any outstanding line control output takes precedence               */
  if (vio_lc_pending(cli->olc))
    {
      ret = uty_term_write_lc(cli->olc, vf, vf->obuf) ;
      if (ret != utw_done)
        return ret ;                    /* utw_blocked or utw_error     */
    } ;

  /* Next: empty out the cli output                                     */
  did = vio_fifo_write_nb(cli->cbuf, vio_vfd_fd(vf->vfd), true) ;

  if (did > 0)
    return utw_blocked ;

  if (did < 0)
    {
      uty_vf_error(vf, verr_io_vout, errno) ;
      return utw_error ;
    } ;

  /* Next: if there is monitor output to deal with, deal with it.
   *
   * Note that the cos_monitor state is set under VTY_LOCK(), so do not
   * need to LOG_LOCK() to discover whether there is anything to do.
   *
   * But the vio->mbuf is filled under LOG_LOCK(), so need to write it
   * under the same.
   */
  if (cli->out_state & cos_monitor)
    {
      LOG_LOCK() ;

      did = vio_fifo_write_nb(vf->vio->mbuf, vio_vfd_fd(vf->vfd), true) ;

      LOG_UNLOCK() ;

      if (did > 0)
        return utw_blocked ;

      if (did < 0)
        {
          uty_vf_error(vf, verr_io_vout, errno) ;
          return utw_error ;
        } ;

      uty_cli_post_monitor(vf->cli) ;   /* clears monitor output state  */
    } ;

  /* If not cos_active we are done, waiting for some external event to move
   * things on.
   */
  if (cli->out_state != cos_active)
    return utw_done ;                   /* can do no more               */

  /* Push the output fifo and any complete line fragments that may be buffered
   * in hand in the line control -- this will stop if the line counter becomes
   * exhausted.
   *
   * Note that this arranges for vio_lc_append() to be called at least once,
   * even if the fifo is empty -- this deals with any parts of a complete
   * line that may be held in the line control due to counter exhaustion.
   *
   * If the fifo is or becomes empty, then if the command has completed, flush
   * out any incomplete line which may be held in the line control.
   */
  vio_fifo_set_hold_mark(vf->obuf) ;    /* released in uty_term_write_lc() */

  have = vio_fifo_get(vf->obuf) ;
  while (1)
    {
      take = vio_lc_append(cli->olc, vio_fifo_get_ptr(vf->obuf), have) ;

      if (take == 0)
        break ;

      have = vio_fifo_step_get(vf->obuf, take) ;

      if (have == 0)
        break ;
    } ;

  if ((have == 0) && (cli->state == cst_complete))
    vio_lc_flush(cli->olc) ;

  ret = uty_term_write_lc(cli->olc, vf, vf->obuf) ;
  if (ret != utw_done)
    return ret ;                /* utw_blocked or utw_error     */

  /* If arrive here, then:
   *
   *   * no output is blocked and no errors have occurred.
   *
   *   * the cli->cbuf is empty.
   *
   *   * the line control iovec buffer is empty.
   *
   * If the fifo is not empty or there is a some part of a complete line in
   * hand, then the line counter must be exhausted.
   */
  if ((have != 0) || vio_lc_have_complete_line_in_hand(cli->olc))
    {
      qassert(vio_lc_counter_is_exhausted(cli->olc)) ;

      if (cli->more_enabled)
        {
          uty_cli_enter_more_wait(cli) ;
          return utw_more_enter ;
        }
      else
        return utw_paused ;     /* artificial block     */
    } ;

  /* Exciting stuff: there is nothing left to output...
   *
   * ...with the sole possible exception of an incomplete line buffered
   * in the line control -- if not cst_complete.
   *
   * ...if is cst_complete, all output has gone, so can fall back to cos_idle.
   */
  qassert(cli->out_state = cos_active) ;

  if (cli->state == cst_complete)
    {
      /* Even more exciting: is cst_complete
       *
       * This means that any incomplete line must have been flushed, above.
       * So all buffers MUST be empty.
       */
      qassert(vio_fifo_empty(vf->obuf) && vio_lc_is_empty(cli->olc)) ;

      cli->out_state = cos_idle ;
    } ;

  return utw_done ;             /* absolutely all done          */
} ;

/*------------------------------------------------------------------------------
 * Write contents of line control iovec buffer (if any).
 *
 * NB: expects that the vf is write_open
 *
 * NB: does nothing other than write() and buffer management.
 *
 * Returns: utw_blocked => blocked
 *          utw_done    => all gone (may still have stuff "in hand")
 *          utw_error   => failed -- error posted to uty_vf_error()
 */
static utw_ret_t
uty_term_write_lc(vio_line_control lc, vio_vf vf, vio_fifo vff)
{
  int did ;

  did = vio_lc_write_nb(vio_vfd_fd(vf->vfd), lc) ;

  if      (did < 0)
    uty_vf_error(vf, verr_io_vout, errno) ;
  else if (did > 0)
    return utw_blocked ;

  vio_fifo_clear_hold_mark(vff) ;       /* finished with FIFO contents  */

  return (did == 0) ? utw_done : utw_error ;
} ;

/*==============================================================================
 * VTY Listener(s) for VTY_TERMINAL
 */

/* Prototypes for listener stuff                                        */

static int uty_term_listen_addrinfo(const char *addr, unsigned short port) ;
static int uty_term_listen_simple(const char *addr, unsigned short port) ;
static int uty_term_listen_open(sa_family_t family, int type, int protocol,
                                     struct sockaddr* sa, unsigned short port) ;
static void uty_term_accept(int sock_listen) ;

/*------------------------------------------------------------------------------
 * Open listener(s) for VTY_TERMINAL -- using getaddrinfo() for preference.
 */
extern void
uty_term_open_listeners(const char *addr, unsigned short port)
{
  int n ;

  if (VTY_USE_ADDRINFO)
    n = uty_term_listen_addrinfo(addr, port);
  else
    n = uty_term_listen_simple(addr, port);

  if (n == 0)
    zlog(NULL, LOG_ERR, "could not open any VTY_TERMINAL listeners") ;
} ;

/*------------------------------------------------------------------------------
 * Open listener(s) for VTY_TERMINAL -- using getaddrinfo()
 *
 * Returns: number of listeners successfully opened.
 */
static int
uty_term_listen_addrinfo(const char *addr, unsigned short port)
{
#if VTY_USE_ADDRINFO    /******************************************************/

# ifndef HAVE_IPV6
#  error Using getaddrinfo() but HAVE_IPV6 is not defined ??
# endif

  int ret;
  int n ;
  struct addrinfo req;
  struct addrinfo *ainfo;
  struct addrinfo *ainfo_save;
  char port_str[16];

  VTY_ASSERT_LOCKED() ;

  /* Want to listen, TCP-wise, on all available address families, on the
   * given port.
   */
  memset (&req, 0, sizeof (struct addrinfo));
  req.ai_flags    = AI_PASSIVE;
  req.ai_family   = AF_UNSPEC;
  req.ai_socktype = SOCK_STREAM;
  snprintf(port_str, sizeof(port_str), "%d", port);

  ret = getaddrinfo (addr, port_str, &req, &ainfo);

  if (ret != 0)
    {
      fprintf (stderr, "getaddrinfo failed: %s\n", eaitoa(ret, errno, 0).str);
      exit (1);
    }

  /* Open up sockets on all AF_INET and AF_INET6 addresses              */
  ainfo_save = ainfo;

  n = 0 ;
  do
    {
      if ((ainfo->ai_family != AF_INET) && (ainfo->ai_family != AF_INET6))
        continue;

      qassert(ainfo->ai_family == ainfo->ai_addr->sa_family) ;

      ret = uty_term_listen_open(ainfo->ai_family, ainfo->ai_socktype,
                                     ainfo->ai_protocol, ainfo->ai_addr, port) ;

      if (ret >= 0)
        ++n ;
    }
  while ((ainfo = ainfo->ai_next) != NULL);

  freeaddrinfo (ainfo_save);

  return n ;

#else
  zabort("uty_serv_sock_addrinfo not implemented") ;
#endif /* VTY_USE_ADDRINFO ****************************************************/
}

/*------------------------------------------------------------------------------
 * Open listener(s) for VTY_TERM -- not using getaddrinfo() !
 *
 * Returns: number of listeners successfully opened.
 */
static int
uty_term_listen_simple(const char *addr, unsigned short port)
{
  int ret;
  int n ;
  union sockunion su_addr ;
  struct sockaddr* sa ;

  VTY_ASSERT_LOCKED() ;

  n = 0 ;       /* nothing opened yet   */

  /* If have an address, see what kind and whether valid                */
  sa = NULL ;

  if (addr != NULL)
    {
      ret = str2sockunion (addr, &su_addr) ;
      if (ret == 0)
        sa = &su_addr.sa ;
      else
        zlog(NULL, LOG_ERR, "bad address %s, cannot listen for VTY", addr);
    } ;

  /* Try for AF_INET                                                    */
  ret = uty_term_listen_open(AF_INET, SOCK_STREAM, 0, sa, port) ;
  if (ret >= 0)
    ++n ;               /* opened socket        */
  if (ret == 1)
    sa = NULL ;         /* used the address     */

#if HAVE_IPV6
  /* Try for AF_INET6                                                   */
  ret = uty_term_listen_open(AF_INET6, SOCK_STREAM, 0, sa, port) ;
  if (ret >= 0)
    ++n ;               /* opened socket        */
  if (ret == 1)
    sa = NULL ;         /* used the address     */
#endif

  /* If not used the address... something wrong                         */
  if (sa != NULL)
    zlog(NULL, LOG_ERR, "could not use address %s, to listen for VTY", addr);

  /* Done                                                               */
  return n ;
}

/*------------------------------------------------------------------------------
 * Open a VTY_TERMINAL listener socket.
 *
 * The sockaddr 'sa' may be NULL or of a different address family, in which
 * case "any" address is used.
 *
 * If the sockaddr 'sa' is used, only the address portion is used.
 *
 * Returns: < 0 => failed
 *         == 0 => OK -- did not use the sockaddr 'sa'.
 *          > 1 => OK -- and did use the sockaddr 'sa'
 */
static int
uty_term_listen_open(sa_family_t family, int type, int protocol,
                                       struct sockaddr* sa, unsigned short port)
{
  union sockunion su[1] ;
  int sock_fd ;
  int ret ;

  VTY_ASSERT_LOCKED() ;

  /* Is there an address and is it for this family ?                    */
  if ((sa != NULL) || (sa->sa_family == family))
    /* Set up sockunion containing required family and address          */
    sockunion_new_sockaddr(su, sa) ;
  else
    {
      /* no address or wrong family -- set up empty sockunion of
       * required family                                                */
      sockunion_init_new(su, family) ;
      sa = NULL ;
    } ;

  /* Open the socket and set its properties                             */
  sock_fd = sockunion_socket(su, type, protocol) ;
  if (sock_fd < 0)
    return -1 ;

  ret = setsockopt_reuseaddr (sock_fd);

  if (ret >= 0)
    ret = setsockopt_reuseport (sock_fd);

  if (ret >= 0)
    ret = set_nonblocking(sock_fd);

#ifdef HAVE_IPV6
  /* Want only IPv6 on AF_INET6 socket (not mapped addresses)
   *
   * This distinguishes 0.0.0.0 from :: -- without this, bind() will reject the
   * attempt to bind to :: after binding to 0.0.0.0.
   */
  if ((ret >= 0) && (family == AF_INET6))
    ret = setsockopt_ipv6_v6only(sock_fd) ;
#endif

  if (ret >= 0)
    ret = sockunion_bind (sock_fd, su, port, (sa == NULL)) ;

  if (ret >= 0)
    ret = sockunion_listen (sock_fd, 3);

  if (ret < 0)
    {
      close (sock_fd);
      return -1 ;
    }

  /* Socket is open -- set VTY_TERMINAL listener going                  */
  uty_add_listener(sock_fd, uty_term_accept) ;

  /* Return OK and signal whether used address or not                   */
  return (sa != NULL) ? 1 : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_TERMINAL
 */
static void
uty_term_accept(int sock_listen)
{
  int sock_fd;
  union sockunion su;
  int ret;
  unsigned int on;
  struct prefix *p ;

  VTY_ASSERT_LOCKED() ;

  /* We can handle IPv4 or IPv6 socket.                                 */
  sockunion_init_new(&su, AF_UNSPEC) ;

  sock_fd = sockunion_accept (sock_listen, &su);

  if (sock_fd < 0)
    {
      if (sock_fd == -1)
        zlog (NULL, LOG_WARNING, "can't accept vty socket : %s",
                                                         errtoa(errno, 0).str) ;
      return ;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock_fd) ;     /* issues WARNING if fails       */
  if (ret < 0)
    {
      close(sock_fd) ;
      return ;
    } ;

  /* New socket is open... worry about access lists                     */
  p = sockunion2hostprefix (&su);
  ret = 0 ;     /* so far, so good              */

  if ((p->family == AF_INET) && host.vty_accesslist_name)
    {
      /* VTY's accesslist apply.                                        */
      struct access_list* acl ;

      if ((acl = access_list_lookup (AFI_IP, host.vty_accesslist_name))
            && (access_list_apply (acl, p) == FILTER_DENY))
        ret = -1 ;
    }

#ifdef HAVE_IPV6
  if ((p->family == AF_INET6) && host.vty_ipv6_accesslist_name)
    {
      /* VTY's ipv6 accesslist apply.                                   */
      struct access_list* acl ;

      if ((acl = access_list_lookup (AFI_IP6, host.vty_ipv6_accesslist_name))
            && (access_list_apply (acl, p) == FILTER_DENY))
        ret = -1 ;
    }
#endif /* HAVE_IPV6 */

  prefix_free (p);

  if (ret != 0)
    {
      zlog (NULL, LOG_INFO, "Vty connection refused from %s", sutoa(&su).str) ;
      close (sock_fd);
      return ;
    } ;

  /* Final options (optional)                                           */
  on = 1 ;
  ret = setsockopt (sock_fd, IPPROTO_TCP, TCP_NODELAY,
                                                    (void*)&on, sizeof (on));
  if (ret < 0)
    zlog (NULL, LOG_INFO, "can't set sockopt to socket %d: %s",
                                               sock_fd, errtoa(errno, 0).str) ;

  /* All set -- create the VTY_TERMINAL and set it going                */
  uty_term_open(sock_fd, &su);

  /* Log new VTY                                                        */
  zlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)", sutoa(&su).str,
                                                                      sock_fd) ;

  return ;
} ;

/*==============================================================================
 * VTY telnet stuff
 *
 * Note that all telnet commands (escapes) and any debug stuff is treated as
 * CLI output.
 */

#define TELNET_OPTION_DEBUG 0           /* 0 to turn off        */

static const char* telnet_commands[256] =
{
  [tn_IAC  ] = "IAC",
  [tn_DONT ] = "DONT",
  [tn_DO   ] = "DO",
  [tn_WONT ] = "WONT",
  [tn_WILL ] = "WILL",
  [tn_SB   ] = "SB",
  [tn_GA   ] = "GA",
  [tn_EL   ] = "EL",
  [tn_EC   ] = "EC",
  [tn_AYT  ] = "AYT",
  [tn_AO   ] = "AO",
  [tn_IP   ] = "IP",
  [tn_BREAK] = "BREAK",
  [tn_DM   ] = "DM",
  [tn_NOP  ] = "NOP",
  [tn_SE   ] = "SE",
  [tn_EOR  ] = "EOR",
  [tn_ABORT] = "ABORT",
  [tn_SUSP ] = "SUSP",
  [tn_EOF  ] = "EOF",
} ;

static const char* telnet_options[256] =
{
  [to_BINARY]       = "BINARY",      /* 8-bit data path                  */
  [to_ECHO]         = "ECHO",        /* echo                             */
  [to_RCP]          = "RCP",         /* prepare to reconnect             */
  [to_SGA]          = "SGA",         /* suppress go ahead                */
  [to_NAMS]         = "NAMS",        /* approximate message size         */
  [to_STATUS]       = "STATUS",      /* give status                      */
  [to_TM]           = "TM",          /* timing mark                      */
  [to_RCTE]         = "RCTE",        /* remote controlled tx and echo    */
  [to_NAOL]         = "NAOL",        /* neg. about output line width     */
  [to_NAOP]         = "NAOP",        /* neg. about output page size      */
  [to_NAOCRD]       = "NAOCRD",      /* neg. about CR disposition        */
  [to_NAOHTS]       = "NAOHTS",      /* neg. about horizontal tabstops   */
  [to_NAOHTD]       = "NAOHTD",      /* neg. about horizontal tab disp.  */
  [to_NAOFFD]       = "NAOFFD",      /* neg. about formfeed disposition  */
  [to_NAOVTS]       = "NAOVTS",      /* neg. about vertical tab stops    */
  [to_NAOVTD]       = "NAOVTD",      /* neg. about vertical tab disp.    */
  [to_NAOLFD]       = "NAOLFD",      /* neg. about output LF disposition */
  [to_XASCII]       = "XASCII",      /* extended ascii character set     */
  [to_LOGOUT]       = "LOGOUT",      /* force logout                     */
  [to_BM]           = "BM",          /* byte macro                       */
  [to_DET]          = "DET",         /* data entry terminal              */
  [to_SUPDUP]       = "SUPDUP",      /* supdup protocol                  */
  [to_SUPDUPOUTPUT] = "SUPDUPOUTPUT",/* supdup output                    */
  [to_SNDLOC]       = "SNDLOC",      /* send location                    */
  [to_TTYPE]        = "TTYPE",       /* terminal type                    */
  [to_EOR]          = "EOR",         /* end or record                    */
  [to_TUID]         = "TUID",        /* TACACS user identification       */
  [to_OUTMRK]       = "OUTMRK",      /* output marking                   */
  [to_TTYLOC]       = "TTYLOC",      /* terminal location number         */
  [to_3270REGIME]   = "3270REGIME",  /* 3270 regime                      */
  [to_X3PAD]        = "X3PAD",       /* X.3 PAD                          */
  [to_NAWS]         = "NAWS",        /* window size                      */
  [to_TSPEED]       = "TSPEED",      /* terminal speed                   */
  [to_LFLOW]        = "LFLOW",       /* remote flow control              */
  [to_LINEMODE]     = "LINEMODE",    /* Linemode option                  */
  [to_XDISPLOC]     = "XDISPLOC",    /* X Display Location               */
  [to_OLD_ENVIRON]  = "OLD_ENVIRON", /* Old - Environment variables      */
  [to_AUTHENTICATION] = "AUTHENTICATION",  /* Authenticate               */
  [to_ENCRYPT]      = "ENCRYPT",     /* Encryption option                */
  [to_NEW_ENVIRON]  = "NEW_ENVIRON", /* New - Environment variables      */
  [to_EXOPL]        = "EXOPL",       /* extended-options-list            */
} ;

/*------------------------------------------------------------------------------
 * For debug.  Put string or value as decimal.
 */
static void
uty_cli_out_dec(vty_cli cli, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cli_out(cli, "%s ", str) ;
  else
    uty_cli_out(cli, "%d ", (int)u) ;
} ;

/*------------------------------------------------------------------------------
 * For debug.  Put string or value as hex.
 */
static void
uty_cli_out_hex(vty_cli cli, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cli_out(cli, "%s ", str) ;
  else
    uty_cli_out(cli, "0x%02x ", (unsigned)u) ;
} ;

/*------------------------------------------------------------------------------
 * Send telnet: "WILL TELOPT_ECHO"
 */
static void
uty_term_will_echo (vty_cli cli)
{
  unsigned char cmd[] = { tn_IAC, tn_WILL, to_ECHO };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (cli, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "suppress Go-Ahead"
 */
static void
uty_term_will_suppress_go_ahead (vty_cli cli)
{
  unsigned char cmd[] = { tn_IAC, tn_WILL, to_SGA };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (cli, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use linemode"
 */
static void
uty_term_dont_linemode (vty_cli cli)
{
  unsigned char cmd[] = { tn_IAC, tn_DONT, to_LINEMODE };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (cli, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "Use window size"
 */
static void
uty_term_do_window_size (vty_cli cli)
{
  unsigned char cmd[] = { tn_IAC, tn_DO, to_NAWS };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (cli, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use lflow"       -- not currently used
 */
static void
uty_term_dont_lflow_ahead (vty_cli cli)
{
  unsigned char cmd[] = { tn_IAC, tn_DONT, to_LFLOW };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (cli, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Process incoming Telnet Option(s)
 *
 * May be called during keystroke iac callback, or when processing CLI
 * keystrokes.
 *
 * In particular: get telnet window size.
 *
 * Returns: true <=> dealt with, for:
 *
 *                    * telnet window size.
 */
extern bool
uty_telnet_command(vio_vf vf, keystroke stroke, bool callback)
{
  uint8_t* p ;
  uint8_t  o ;
  int left ;
  bool dealt_with ;

  vty_cli cli = vf->cli ;

  /* Echo to the other end if required                                  */
  if (TELNET_OPTION_DEBUG)
    {
      uty_cli_wipe(cli, 0) ;

      p    = stroke->buf ;
      left = stroke->len ;

      uty_cli_out_hex(cli, telnet_commands[tn_IAC], tn_IAC) ;

      if (left-- > 0)
        uty_cli_out_dec(cli, telnet_commands[*p], *p) ;
      ++p ;

      if (left-- > 0)
        uty_cli_out_dec(cli, telnet_options[*p], *p) ;
      ++p ;

      if (left > 0)
        {
          while(left-- > 0)
            uty_cli_out_hex(cli, NULL, *p++) ;

          if (stroke->flags & kf_truncated)
            uty_cli_out(cli, "... ") ;

          if (!(stroke->flags & kf_broken))
            {
              uty_cli_out_hex(cli, telnet_commands[tn_IAC], tn_IAC) ;
              uty_cli_out_hex(cli, telnet_commands[tn_SE], tn_SE) ;
            }
        } ;

      if (stroke->flags & kf_broken)
        uty_cli_out (cli, "BROKEN") ;

      uty_cli_out_newline(cli) ;
    } ;

  /* Process the telnet command                                         */
  dealt_with = false ;

  if (stroke->flags != 0)
    return dealt_with ;         /* go no further if broken              */

  p    = stroke->buf ;
  left = stroke->len ;

  qassert(left >= 1) ;            /* must be if not broken !            */
  qassert(stroke->value == *p) ;  /* or something is wrong              */

  ++p ;         /* step past X of IAC X */
  --left ;

  /* Decode the one command that is interesting -- "NAWS"               */
  switch (stroke->value)
  {
    case tn_SB:
      qassert(left > 0) ;       /* or parser failed     */

      o = *p++ ;                /* the option byte      */
      --left ;
      switch(o)
      {
        case to_NAWS:
          if (left != 4)
            {
              zlog(NULL, LOG_WARNING,
                        "RFC 1073 violation detected: telnet NAWS option "
                        "should send %d characters, but we received %d",
                        (3 + 4 + 2), (3 + left + 2)) ;
            }
          else
            {
              int width, height ;

              width   = *p++ << 8 ; width  += *p++ ;
              height  = *p++ << 8 ; height += *p ;

              if (TELNET_OPTION_DEBUG)
                {
                  uty_cli_out(cli, "TELNET NAWS window size received: "
                                   "width %d, height %d", width, height) ;
                  uty_cli_out_newline(cli) ;
                } ;

              uty_cli_set_window(cli, width, height) ;

              dealt_with = true ;
            } ;
          break ;

        default:        /* no other IAC SB <option>     */
          break ;
      } ;
      break ;

    default:            /* no other IAC X               */
      break ;
  } ;

  return dealt_with ;
} ;


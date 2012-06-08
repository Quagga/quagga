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
#include "vty_log.h"
#include "vio_fifo.h"

#include "log_local.h"
#include "log.h"

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

static void uty_term_read_ready(vio_vfd vfd, void* action_info, bool time_out) ;
static void uty_term_write_ready(vio_vfd vfd, void* action_info, bool time_out);

static cmd_ret_t uty_term_write(vio_vf vf) ;
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
uty_term_open(int sock_fd, sockunion su)
{
  node_type_t  node ;
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  zlog (NULL, LOG_INFO, "VTY connection from %s (fd %d)", sutoa(su).str,
                                                                      sock_fd) ;

  /* The initial vty->node will be authentication, unless the host allows for
   * unauthenticated login, in which case it may be a number of other things,
   * in order of precedence:
   *
   *   RESTRICTED_NODE   -- "host.restricted_mode" restricts unauthenticated
   *                        login.
   *
   *   ENABLE_NODE       -- "host.advanced" allows login directly to ENABLE
   *                        AND there is no enable password.
   *
   *   VIEW_NODE         -- otherwise.
   *
   * Note that setting EXIT_NODE at this point will cause the terminal to be
   * closed very quickly, after issuing suitable message.
   */
  node = (host.password.text != NULL) ? AUTH_NODE : EXIT_NODE ;

  if (host.no_password_check)
    {
      if      (host.restricted_mode)
        node = RESTRICTED_NODE;
      else if (host.advanced && (host.enable.text == NULL))
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
   * The timeouts default to:
   *
   *   - read_timeout     -- default = 0     => no timeout
   *   - write_timeout    -- default = 0     => no timeout
   *
   * and we set actual values, below.
   *
   * The text form of the address identifies the VTY.
   *
   * The cmd_exec and cmd_context objects are set up when the command
   * processor is entered.
   */
  vf = uty_vf_new(vio, sutoa(su).str, sock_fd, vfd_socket, vfd_io_read_write) ;

  uty_vin_push( vio, vf, VIN_TERM,  uty_term_read_ready,
                                    0) ;                /* no ibuf      */
  uty_vout_push(vio, vf, VOUT_TERM, uty_term_write_ready,
                                    term_buffer_size,   /* obuf         */
                                    true) ;     /* after buddy vin      */

  vf->read_timeout  = host.vty_timeout_val ; /* current EXEC timeout    */
  vf->write_timeout = 30 ;                   /* something reasonable    */

  /* Set up the CLI object & initialise
   */
  qassert(vf->obuf != NULL) ;
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
  else if (vty->node == EXIT_NODE)
    vty_out(vty, "%% Cannot continue because no password is set\n") ;

  /* Enter the command loop.                                            */
  uty_cmd_queue_loop_enter(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Command line fetch from a VTY_TERMINAL
 *
 * Fetching a command line <=> the previous command has completed.
 *
 * Returns:  CMD_SUCCESS  -- have another command line ready to go
 *           CMD_WAITING  -- need to wait for more input
 *           CMD_HIATUS   -- no command line, hiatus attention required
 *           CMD_IO_ERROR -- failed to set write-ready as required !
 *
 * If have a command line ready to go, then the vf->context now contains it.
 *
 * Note that this does no actual I/O, all that is done in the pselect() process,
 * while a command line is collected in the CLI.
 */
extern cmd_ret_t
uty_term_cmd_line_fetch(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vin_type == VIN_TERM) ;

  return uty_cli_want_command(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Push output to the terminal -- always not vf->blocking !
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- waiting for some *output* operation
 *           CMD_IO_ERROR  -- error or time-out
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, the
 * background pselect process will complete the output and may signal the
 * result via uty_cmd_signal().
 */
extern cmd_ret_t
uty_term_out_push(vio_vf vf)
{
  cmd_ret_t  ret ;

  VTY_ASSERT_LOCKED() ;

  qassert(vf->vout_type  == VOUT_TERM) ;
  qassert(!vf->blocking && !vio_vfd_blocking(vf->vfd)) ;

  ret = uty_term_write(vf) ;

  return uty_term_set_readiness(vf, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Complete the cancelling of command(s) and output.
 *
 * This is called by the hiatus when has closed all other vin and vout,
 * and is finishing off a vio->cancel operation.
 *
 * Clears the obuf and the line control.
 *
 * Cancel may be triggered by a "^C" keystroke -- see uty_cli_callback().
 * For vst_cmd_fetch, vst_cmd_dispatched and vst_cmd_complete, the "^C"

                             TODO

 *
 * Cancel may also be triggered externally, either stopping or suspending
 * the VTY_TERMINAL.
 *
 * uty_cli_out_cancel() ignores vst_mon_active etc (see discussion there), and
 * deals with the underlying state:
 *
 *   vst_cmd_fetch
 *   vst_cmd_dispatched   -- make sure prompt and command line are drawn, then
 *                       cancel with "^C\n" and set vst_cmd_complete.
 *
 *                       uty_cli_out_cancel() returns true.
 *
 *   vst_cmd_more         -- make sure prompt is drawn, then cancel with "^C\n"
 *                       and set vst_cmd_running.
 *
 *                       uty_cli_out_cancel() returns true.
 *
 *   vst_cmd_complete     -- a previous command and all its output has completed
 *                       successfully.
 *
 *                       In this case we *know* there is nothing to cancel.  We
 *                       may even be vst_cmd_complete as the result of a previous
 *                       cancel action.
 *
 *                       Also... we *know* this is not in response to ^C from
 *                       the keyboard, because that does not trigger a
 *                       vio->cancel !
 *
 *                       So, we do NOT need or want a "^C\n".
 *
 *                       uty_cli_out_cancel() returns true <=> have done what
 *                       is required to cancel.
 *
 *   vst_cmd_running  -- output was proceeding, so a "^C\n" is required.
 *
 *                       The ^C indicates that either some output has been
 *                       discarded, or a pipe operation has been terminated
 *                       early, or something else has been interrupted.
 *
 *                       At the very least, if the user typed a ^C, then this
 *                       shows that it has been responded to.
 *
 *                       uty_cli_out_cancel() returns false.
 *
 * Will return from here vst_cmd_running or vst_cmd_complete.
 *
 * Returns:  true <=> has put "^C\n" already (or none required)
 */
extern bool
uty_term_out_cancelled(vio_vf vf)
{
  vty_cli cli ; ;

  cli = vf->cli ;

  vio_fifo_clear(vf->obuf) ;
  vio_lc_clear(cli->olc) ;      /* empty line control and reset */

  return uty_cli_out_cancel(cli) ;
} ;

/*------------------------------------------------------------------------------
 * The read side of the vf is being closed.   Close down CLI as far as
 * possible, given that output may be continuing.
 *
 * Expects to be called once only for the VTY_TERMINAL -- returns CMD_SUCCESS
 * first time !
 *
 * There is nothing special about vst_final in this case.
 *
 * Returns:  CMD_SUCCESS   -- all is quiet.
 */
extern cmd_ret_t
uty_term_read_close(vio_vf vf)
{
  qassert(vf->vin_type == VIN_TERM) ;
  qassert((vf->vio->vin == vf->vio->vin_base) && (vf->vio->vin == vf)) ;

  /* Closing the VTY => stop log monitor
   */
  uty_monitor_set(vf->vio, off) ;

  /* Close the CLI as far as possible, leaving output side intact but turning
   * off any "--more--" handling.
   *
   * Can generate some final output, which will be dealt with as the output
   * side is closed.
   */
  uty_cli_close(vf->cli) ;

  /* Easy: let the caller close the underlying vfd and clear vin_open.
   */
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Try to output the close reason to the given VOUT_TERM.
 *
 * Takes the wrapped value and upgrades '\n' as required..
 */
extern void
uty_term_close_reason(vio_vf vf, qstring wrapped)
{
  int fd ;

  fd = vio_vfd_fd(vf->vfd) ;

  if (fd >= 0)
    {
      ulen len ;
      len = qs_globex_str(wrapped, "\n", uty_cli_newline) ;
      write(fd, qs_body(wrapped), len) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Close the writing side of VTY_TERMINAL.
 *
 * Assumes that the read side has been closed already, and so this is the last
 * thing to be closed.  Any monitor state was turned off earlier when the read
 * side was closed.
 *
 * Kicks the output side.
 *
 * Note that because the VTY_TERMINAL is always the vin_base/vout_base, the
 * write side will be closed "final" just before the vty/vio is closed and
 * freed -- so uty_cli_close() "final" is the point at which the cli is
 * closed and freed.
 *
 * Returns:  CMD_SUCCESS  => done everything possible.
 *           CMD_WAITING  => waiting for output to complete
 *
 * NB: if an error occurs while writing, that will have been logged, but there
 *     is nothing more to be done about it here -- so does not return
 *     CMD_IO_ERROR.
 */
extern cmd_ret_t
uty_term_write_close(vio_vf vf)
{
  cmd_ret_t ret ;
  vty_io    vio ;

  VTY_ASSERT_LOCKED() ;

  /* Get the vio and ensure that we are all straight
   *
   * Can only be the vout_base and must also be the vin_base, and the vin_base
   * must now be closed.
   *
   * NB: closing the input side will have closed down any log monitor.
   */
  vio = vf->vio ;

  qassert((vio->vout == vio->vout_base) && (vf == vio->vout)) ;
  qassert((vio->vin  == vio->vin_base)  && (vf == vio->vin)) ;
  qassert(vf->vout_type  == VOUT_TERM) ;

  qassert(!vio->monitor) ;

  ret = CMD_SUCCESS ;

  /* If this is curtains, get rid of the cli and log closing the vty
   *
   * Otherwise, provided is open, push.
   */
  if (vio->state & vst_final)
    {
      const char* reason ;

      vf->cli = uty_cli_free(vf->cli) ;

      reason = qs_string(vf->vio->close_reason) ;
      if ((reason == NULL) || (*reason == '\0'))
        reason = "closed" ;

      zlog_info("VTY connection (fd %d) %s", vio_vfd_fd(vf->vfd), reason) ;
    }
  else if (vf_active(vf->vout_state))
    {
      ret = uty_term_out_push(vf) ;

      if (ret != CMD_WAITING)
        ret = CMD_SUCCESS ;             /* discard CMD_IO_ERROR !       */
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
 * Incoming "ret":  CMD_SUCCESS   -- clear write-ready
 *                  CMD_WAITING   -- set write-ready
 *                  CMD_IO_ERROR  -- clear read-ready and write-ready
 *
 * Each time this is called, the read timeout is reset.  So the timeout runs
 * from the last activity, in or out.
 *
 * NB: CMD_SUCCESS means that the *output* side of things is not waiting for
 *     anything under its control.
 *
 *     CMD_WAITING means that the *output* side of things *is* waiting for
 *     something under its control.
 *
 * NB: CMD_SUCCESS and vf->vin_waiting means that a cli has control, and
 *     the command loop can wait to hear.
 *
 * Returns: CMD_SUCCESS    -- all well, is not waiting for any *output*
 *          CMD_WAITING    -- is waiting for an *output* operation
 *          CMD_IO_ERROR   -- error while setting read-ready or write-ready
 *                            or CMD_IO_ERROR passed in
 */
extern cmd_ret_t
uty_term_set_readiness(vio_vf vf, cmd_ret_t ret)
{
  on_off_b  read_on, write_on ;
  cmd_ret_t ret_read, ret_write ;
  bool      cli_active ;

  VTY_ASSERT_LOCKED() ;

  /* If the initial write_on == on, then are CMD_WAITING !
   *
   * If cli is active then we are CMD_SUCCESS -- not waiting for *output*.
   * But we leave write_on == on if we got it and if is cli->ready we force
   * write_on == on.
   *
   * If is vst_mon_paused:
   *
   *   write_on -> off         -- output is also held inactive until the timer
   *                              expires, or a keystroke appears.
   *
   *   ret      -> CMD_WAITING -- we are effectively waiting for output to
   *                              continue.
   */
  if (ret != CMD_IO_ERROR)
    {
      write_on = (ret == CMD_WAITING) ? on : off ;
      read_on  = on ;

      cli_active = uty_cli_is_active(vf->cli) != cli_not_active;
      if (cli_active)
        {
          qassert((vf->vio->state & (vst_hiatus_mask | vst_mon_mask)) == 0) ;

          if (vf->cli->ready)
            write_on = on ;

          ret = CMD_SUCCESS ;   /* not waiting for *output*     */
        }
      else if (vf->vio->state & vst_mon_paused)
        {
          write_on = off ;
          ret = CMD_WAITING ;   /* pretend waiting for *output* */
        } ;
    }
  else
    {
      write_on = read_on = off ;
      cli_active = false ;
    } ;

  /* Read-ready is permanently on.  Set write-ready as required.
   *
   * Restarts any time out, and the VTY_TERMINAL time out in particular.
   *
   * Note that uty_vf_set_read_ready() returns CMD_WAITING iff is vin_active,
   * and was able to set read-ready on.  It returns CMD_IO_ERROR iff is
   * vin_active, but was unable to set read-ready on.  So, we can set
   * vf->vin_waiting iff the cli is active and is vin_active and really is
   * read-ready !
   */
  ret_read  = uty_vf_set_read_ready(vf, read_on) ;
  ret_write = uty_vf_set_write_ready(vf, write_on) ;

  /* Set vf->vin_waiting if cli was active *and* we are, in fact, read-ready
   * *and* all is well.
   */
  vf->vin_waiting = cli_active && (ret_read  == CMD_WAITING)
                               && (ret_write != CMD_IO_ERROR) ;

  /* Done -- return prepared response, or CMD_IO_ERROR if anything failed
   */
  if ((ret_read == CMD_IO_ERROR) || (ret_write == CMD_IO_ERROR))
    ret = CMD_IO_ERROR ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Terminal read ready or timed-out
 *
 * If is timed-out, does nothing if is paused.  Otherwise, discard anything in
 * the keystroke stream and set it "eof, timed-out".  This will be picked up by
 * the CLI and a cmd_do_timed-out will float out.  Note that is not signalling
 * a time-out error.
 *
 * If not timed-out, clear any paused state.  If timer is running, it will do
 * nothing when it goes off, because is no longer paused.  Then, read whatever
 * there is into the keystroke stream.
 *
 * Invoke uty_term_ready() -- unless still paused.
 */
static void
uty_term_read_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  vio_vf vf = action_info ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(vfd == vf->vfd) ;
  qassert(vf->vout_type == VOUT_TERM) ;

  if (time_out)
    {
      if (vf->vio->state & vst_mon_paused)
        return ;

      keystroke_stream_set_eof(vf->cli->key_stream, true /* time_out */) ;
    }
  else
    {
      vf->vio->state &= ~vst_mon_paused ;       /* keystroke overrides  */
      uty_term_read(vf) ;
    } ;

  uty_term_ready(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Terminal write ready or timed-out
 *
 * Does nothing if in paused state.
 *
 * If is timed-out, discard anything in the keystroke stream and set it
 * "eof, timed-out".  This will be picked up by the CLI and a cmd_do_timed-out
 * will float out.
 *
 * Otherwise, read what there is into the keystroke stream.
 *
 * In any case, invoke uty_term_ready().
 */
static void
uty_term_write_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vio_vf vf = action_info ;

  assert(vfd == vf->vfd) ;
  qassert(vf->vout_type == VOUT_TERM) ;

  if (vf->vio->state & vst_mon_paused)
    return ;

  if (time_out)
    uty_vf_error(vf, verr_to_vout, 0) ;

  uty_term_ready(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Timeout on the cli->paused timer -- clear paused and treat as CLI ready.
 *
 * NB: if vst_mon_paused has been cleared, does nothing... so can clear the state
 *     at any time, without worrying whether is set, or could be, or not.
 */
extern void
vty_term_pause_timeout(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  vty_cli cli = timer_info ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(cli->pause_timer == qtr) ;

  if (cli->vf->vio->state & vst_mon_paused)
    {
      cli->vf->vio->state &= ~vst_mon_paused ;
      uty_term_ready(cli->vf) ;
    } ;

  VTY_UNLOCK() ;
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
  vty_cli    cli ;
  cmd_ret_t  ret ;

  VTY_ASSERT_LOCKED() ;

  cli = vf->cli ;

  /* Will attempt to write away any pending stuff, then for each call of
   * uty_term_ready, put out another tranche of output (unless in '--more--'
   * state).
   */
  if (!cli->more_enabled)
    vio_lc_counter_reset(cli->olc) ;            /* do one tranche       */

  /* Now kick the CLI and then the output:
   *
   * The CLI may generate further output, so we *must* write away anything
   * we can after calling the CLI.
   *
   * If the output side enters "--more--", it will set cli->ready, which will
   * force write-ready, which in turn will trigger an early return to here, to
   * move things on.
   *
   * If the output side completes all output associated with the last command,
   * will signal CMD_SUCCESS, and that will cause the command loop to proceed
   * to fetch the next command line.
   */
  uty_cli(cli) ;
  ret = uty_term_write(vf) ;

  /* Set read-ready and write-ready and update timeouts as required.  Signal
   * the command loop unless the pselect() side is busy chewing.
   */
  ret = uty_term_set_readiness(vf, ret) ;

  if ((ret != CMD_WAITING) && !vf->vin_waiting)
    uty_cmd_signal(vf->vio, ret) ;
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
 * When reaches EOF on the input eof is set in the keystroke stream, and vf_end
 * is set to turn off read-ready and timeout.
 */
extern void
uty_term_read(vio_vf vf)
{
  keystroke_stream stream ;

  stream = vf->cli->key_stream ;

  if (keystroke_stream_met_eof(stream))
    return ;                    /* already seen EOF                     */

  if (vf->vin_state & vf_cease)
    {
      /* Nothing more required -- discard keystrokes and force eof.
       */
      keystroke_stream_set_eof(vf->cli->key_stream, false /* not timeout */) ;
      return ;
    } ;

  /* OK: read from input and pass result to keystroke stream.
   *
   *     Uses a relatively small buffer, which should be fine for actual
   *     terminals !
   */
  while ((vf->vin_state & vf_end) == 0)
    {
      unsigned char buf[100] ;
      int get ;

      get = read_nb(uty_vf_read_fd(vf), buf, sizeof(buf)) ;
                                /* -1 <=> error, -2 <=> EOF     */
      if (get > 0)
        {
          keystroke_input(vf->cli->key_stream, buf, get) ;
          continue ;            /* hoover up all there is.      */
        } ;

      if (get == 0)
        return ;                /* waiting                      */

      if (get == -2)
        break ;                 /* end                          */

      /* Error.
       */
      keystroke_stream_set_eof(vf->cli->key_stream, false /* not timeout */) ;
      uty_vf_error(vf, verr_io_vin, errno) ;

      return ;
    } ;

  /* Reached end of input -- mark end on keystroke stream and set vf_end.
   */
  keystroke_input_eof(vf->cli->key_stream) ;
  uty_vf_read_stop(vf, vfs_stop_end) ;
} ;

/*==============================================================================
 * Writing to VOUT_TERM.
 *
 * There are two sets of buffering:
 *
 *   cli->cbuf -- command line   -- reflects the status of the command line
 *
 *   vf->obuf  -- command output -- which is written to the terminal only while
 *                                  vst_cmd_running.
 *
 * The cli output takes precedence.
 *
 * Output of command stuff is subject to line_control, and may go through the
 * "--more--" mechanism.
 */
static cmd_ret_t uty_term_write_lc(vio_line_control lc, vio_vf vf,
                                                                 vio_fifo vff) ;
/*------------------------------------------------------------------------------
 * Have some (more) monitor output to send to the vty.
 *
 * Make sure the command line is clear, then claim screen for monitor output
 * and attempt to empty out buffers.
 *
 * Set monitor output state, and then write.
 *
 * If a vst_mon_paused timer was running, clear it before writing more stuff, so
 * will restart timer afterwards.
 *
 * On return, if vst_mon_paused is set, then a timer is running, which, when it
 * goes off, enter the uty_term_ready() and run cli etc.  Note that in
 * single pthread or legacy thread, it may be a while before gets round to
 * pselect() !
 *
 * Otherwise, we set write ready no matter what.  If there is output pending,
 * then that's what's required.  If there is no output pending, this forces
 * an entry to uty_term_ready().  We want uty_term_ready() to run, so that it
 * copes with any side effects of uty_term_write(), and any need to reenter
 * cli in order to redraw the line which was wiped.
 *
 * Note that it would have been possible to simply set write ready, and left
 * the whole thing up to the pselect() loop.  However, that might delay the
 * log monitor output -- particularly if single pthreaded or legacy threaded.
 */
extern void
uty_term_mon_write(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  qassert(vf->vout_type == VOUT_TERM) ;
  qassert(vf->vio->monitor) ;

  uty_cli_wipe(vf->cli) ;

  vf->vio->state = (vf->vio->state & ~vst_mon_paused) | vst_mon_active ;
  ret = uty_term_write(vf) ;

  uty_term_set_readiness(vf, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Write as much as possible of what there is.
 *
 * If there is anything in the CLI buffer, try to empty that.
 *
 * If is vst_mon_active, try to empty the mbuf.
 *
 * If vst_cmd_running, try to empty the obuf.  Move to vst_cmd_more if required.
 *
 * NB: vst_cmd_executing is cleared when the command loop is at the base vin
 *     and the base vout, and the command loop passes through either
 *     vty_cmd_complete() or vyt_hiatus().
 *
 *     If is vst_cmd_running (ignoring vst_mon_active etc.), when the buffers
 *     are emptied out, then we can move to vst_cmd_complete.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *                            does not required write-ready to be set
 *           CMD_WAITING   -- is waiting for something
 *                            need to set write-ready, if not vst_mon_paused
 *           CMD_IO_ERROR  -- error
 *
 * NB: when returns CMD_SUCCESS this does not necessarily mean that all buffers
 *     are empty -- CMD_SUCCESS means that all is OK, but something other than
 *     write-ready may be required to progress things.
 *
 *     when returns CMD_WAITING the output may not be blocked -- CMD_WAITING
 *     means that
 */
static cmd_ret_t
uty_term_write(vio_vf vf)
{
  vty_cli    cli ;
  vty_io     vio ;
  cmd_ret_t  ret ;
  int        did ;
  ulen       have, take ;

  VTY_ASSERT_LOCKED() ;

  cli = vf->cli ;
  vio = vf->vio ;

  /* Do nothing if vf_end.  Also traps vst_final.
   */
  if (vf->vout_state & vf_end)
    return CMD_SUCCESS ;

  qassert((vio->state & vst_final) == 0) ;

  /* Unless is in the cancel process, or blocked earlier writing log monitor
   * stuff: deal with any outstanding line control and/or cli stuff.
   */
  if ((vio->state & (vst_cancel | vst_mon_blocked)) == 0)
    {
      /* Any outstanding line control output takes precedence.
       */
      if (vio_lc_pending(cli->olc))
        {
          ret = uty_term_write_lc(cli->olc, vf, vf->obuf) ;
          if (ret != CMD_SUCCESS)
            return ret ;        /* CMD_WAITING or CMD_IO_ERROR  */
        } ;

      /* Next: empty out the cli output.
       */
      if (!vio_fifo_is_empty(cli->cbuf))
        {
          did = vio_fifo_write_nb(cli->cbuf, uty_vf_write_fd(vf),
                                                  true /* write everything */) ;
          if (did != 0)
            {
              if (did < 0)
                return uty_vf_error(vf, verr_io_vout, errno) ;

              return CMD_WAITING ;
            } ;
        } ;
    } ;

  /* Next: if there is monitor output to deal with, deal with it.
   *
   * Note that the vst_mon_active state is set under VTY_LOCK(), so do not
   * need to LOG_LOCK() to discover whether there is anything to do.
   *
   * But the vio->mbuf is filled under LOG_LOCK(), so need to write it
   * under the same.
   */
  if (vio->state & vst_mon_mask)
    {
      LOG_LOCK() ;

      did = vio_fifo_write_nb(vf->vio->mbuf, uty_vf_write_fd(vf),
                                                  true /* write everything */) ;
      LOG_UNLOCK() ;

      /* hope for the best -- but if did not complete all monitor output
       * then return either CMD_WAITING or CMD_IO_ERROR.
       */
      vio->state &= ~vst_mon_mask ;     /* clear all vst_mon_xxx        */

      if (did != 0)
        {
          if (did < 0)
            return uty_vf_error(vf, verr_io_vout, errno) ;

          vio->state |= (vst_mon_active | vst_mon_blocked) ;
          return CMD_WAITING ;
        } ;

      /* Completed all monitor output, so can clear state and proceed.
       *
       * We have two ways of proceeding:
       *
       *   If we are single pthreaded or legacy threaded or monitor state
       *   has been turned off: return "utw_paused".  If a cli is active, we
       *   set cli->ready, so that the cli will be re-entered, in order to
       *   redraw the line etc.  In any case, if there is anything more to do,
       *   that will wait until the pselect() is run.
       *
       *   If we are multi-threaded and still in monitor state, then we go
       *   to "vst_mon_paused", which sets a short timer after which, when it
       *   expires, will restart activity.  A keystroke arriving will also
       *   restart activity.
       *
       * Should not be here with vst_mon_paused.  If not pausing, make sure it is
       * clear.  If is pausing, does *not* restart the timer -- so do not get
       * trapped here !
       */
      if ((cli->pause_timer == NULL) || !vf->vio->monitor)
        {
          if (uty_cli_is_active(cli) != cli_not_active)
            cli->ready = true ;
        }
      else
        {
          if ((vio->state & vst_mon_paused) == 0)
            qtimer_set(cli->pause_timer, qt_add_monotonic(QTIME(0.2)), NULL) ;

          vio->state |= vst_mon_paused ;
        } ;

      return CMD_WAITING ;
    } ;

  /* If not vst_cmd_running or vst_cmd_running_executing we are done.
   */
  if ((vio->state & (vst_cmd_inner_mask | vst_cancel)) != vst_cmd_running)
    return CMD_SUCCESS ;        /* nothing more to do ATM       */

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
   *
   * We have a hold_marker in the obuf permanently, and which is moved forward
   * in uty_term_write_lc().
   */
  qassert(vio_fifo_have_hold_mark(vf->obuf)) ;

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

  if ((have == 0) && ((vio->state & vst_cmd_executing) == 0))
    vio_lc_flush(cli->olc) ;

  ret = uty_term_write_lc(cli->olc, vf, vf->obuf) ;
  if (ret != CMD_SUCCESS)
    return ret ;                /* CMD_WAITING or CMD_IO_ERROR  */

  /* If arrive here, then:
   *
   *   * no output is blocked and no errors have occurred.
   *
   *   * the cli->cbuf is empty.
   *
   *   * the line control iovec buffer is empty.
   *
   * If the fifo is not empty (have != 0) or there is a some part of a complete
   * line in hand in the line control, then need to go to "--more--" or need to
   * exit because artificially blocked after outputting a "tranche".
   *
   * Note that vio_lc_append() reads until it finds the end of a complete line,
   * before breaking it up into lines to be output.  So:
   *
   *   * if the fifo is *not* empty, then there is something after the last
   *     complete line, so there is more to come, even if it is a blank line.
   *
   *   * if the fifo *is* empty, and the line control does not have a complete
   *     line in hand, we cannot be sure that there is more to come.  What the
   *     line control has in hand, if anything, may be whitespace which will
   *     later be discarded.  So cannot go "--more--" and have output
   *     everything we can, until more is put in the buffer, or the output
   *     is "complete", so that whatever the line control has in hand can
   *     be flushed out.
   */
  if ((have != 0) || vio_lc_have_complete_line_in_hand(cli->olc))
    {
      qassert(vio_lc_counter_is_exhausted(cli->olc)) ;

      if (cli->more_enabled)
        uty_cli_more_enter(cli) ;

      return CMD_WAITING ;
    } ;

  /* Exciting stuff: there is nothing left to output...
   *
   * ...with the sole possible exception of an incomplete line buffered
   * in the line control -- if not "complete".
   */
  qassert((vio->state & vst_cmd_inner_mask) == vst_cmd_running) ;

  if ((vio->state & vst_cmd_executing) == 0)
    {
      /* Even more exciting: is ready to go vst_cmd_complete !
       *
       * All buffers MUST be empty.  Must be vst_cmd_running !
       */
      qassert(vio_fifo_is_empty(vf->obuf) && vio_lc_is_empty(cli->olc)) ;

      vf->vio->state = (vio->state & ~vst_cmd_mask) | vst_cmd_complete ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Write contents of line control iovec buffer (if any).
 *
 * NB: expects that the vf is write_open
 *
 * NB: does nothing other than write() and buffer management.
 *
 * Returns:  CMD_SUCCESS   => all gone (may still have stuff "in hand")
 *           CMD_WAITING   => blocked
 *           CMD_IO_ERROR  => failed -- error posted to uty_vf_error()
 */
static cmd_ret_t
uty_term_write_lc(vio_line_control lc, vio_vf vf, vio_fifo vff)
{
  int did ;

  did = vio_lc_write_nb(uty_vf_write_fd(vf), lc) ;

  if (did < 0)
    return uty_vf_error(vf, verr_io_vout, errno) ;

  if (did > 0)
    return CMD_WAITING ;

  /* We have finished with everything between the hold_mark and the current
   * get_ptr -- we eat that by setting a new hold_mark at the current get_ptr.
   * So, we have a permanent hold_mark in the obuf.
   */
  vio_fifo_set_hold_mark(vff) ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * VTY Listener(s) for VTY_TERMINAL
 */

/* Prototypes for listener stuff                                        */

static int uty_term_listen_addrinfo(const char *addr, unsigned short port) ;
static int uty_term_listen_simple(const char *addr, unsigned short port) ;
static int uty_term_listen_open(sa_family_t family, int type, int protocol,
                                     struct sockaddr* sa, unsigned short port) ;
static void uty_term_accept(int sock_listen_fd) ;

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
  zabort("uty_term_listen_addrinfo not implemented") ;
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

  if (ret >= 0)
    ret = set_close_on_exec(sock_fd) ;

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
uty_term_accept(int sock_listen_fd)
{
  int sock_fd;
  union sockunion su;
  int ret;
  unsigned int on;
  struct prefix *p ;

  VTY_ASSERT_LOCKED() ;

  /* We can handle IPv4 or IPv6 socket.                                 */
  sockunion_init_new(&su, AF_UNSPEC) ;

  sock_fd = sockunion_accept (sock_listen_fd, &su);

  if (sock_fd < 0)
    {
      if (sock_fd == -1)
        zlog (NULL, LOG_WARNING, "can't accept vty socket : %s",
                                                         errtoa(errno, 0).str) ;
      return ;
    }

  /* Really MUST have non-blocking and close-on-exec
   */
  ret = set_nonblocking(sock_fd) ;      /* issues WARNING if fails       */

  if (ret >= 0)
    ret = set_close_on_exec(sock_fd) ;  /* issues WARNING if fails      */

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
      uty_cli_wipe(cli) ;

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


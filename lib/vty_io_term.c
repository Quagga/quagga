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
#include "zconfig.h"
#include "misc.h"

#include "vty_local.h"
#include "vty_io.h"
#include "vty_io_term.h"
#include "vty_cli.h"
#include "vty_command.h"
#include "vio_fifo.h"

#include "qstring.h"
#include "keystroke.h"

#include "memory.h"

#include "prefix.h"
#include "filter.h"
#include "privs.h"
#include "sockunion.h"
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
 *
 *
 *
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

static void uty_term_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_term_read_timeout(vio_timer_t* timer,
                                                            void* action_info) ;
static vty_timer_time uty_term_write_timeout(vio_timer_t* timer,
                                                            void* action_info) ;
static vty_readiness_t uty_term_write(vio_vf vf) ;

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
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new(VTY_TERMINAL) ;
  vio = vty->vio ;

  /* The initial vty->node depends on a number of vty type things, so
   * we set that now.
   *
   * This completes the initialisation of the vty object, except that the
   * execution and vio objects are largely empty.
   */
  if (host.no_password_check)
    {
      if (host.restricted_mode)
        vio->vty->node = RESTRICTED_NODE;
      else if (host.advanced)
        vio->vty->node = ENABLE_NODE;
      else
        vio->vty->node = VIEW_NODE;
    }
  else
    vio->vty->node = AUTH_NODE;

  /* Complete the initialisation of the vty_io object.
   *
   * Note that the defaults for:
   *
   *   - read_timeout     -- default = 0     => no timeout
   *   - write_timeout    -- default = 0     => no timeout
   *
   *   - parse_type       -- default = cmd_parse_standard
   *   - reflect_enabled  -- default = false
   *   - out_enabled      -- default = true iff vfd_io_write
   *
   * Are OK, except that we want the read_timeout set to the current EXEC
   * timeout value.
   *
   * The text form of the address identifies the VTY.
   */
  vf = uty_vf_new(vio, sutoa(su).str, sock_fd, vfd_socket, vfd_io_read_write) ;

  uty_vin_open( vio, vf, VIN_TERM,  uty_term_ready,
                                    uty_term_read_timeout,
                                    0) ;        /* no ibuf required     */
  uty_vout_open(vio, vf, VOUT_TERM, uty_term_ready,
                                    uty_term_write_timeout,
                                    4096) ;     /* obuf is required     */

  vf->read_timeout = host.vty_timeout_val ; /* current EXEC timeout     */

  /* Set up the CLI object & initialise                                 */
  vf->cli = uty_cli_new(vf) ;

  /* When we get here the VTY is set up and all ready to go.            */
  uty_cmd_prepare(vio) ;

  /* Issue Telnet commands/escapes to be a good telnet citizen -- not much
   * real negotiating going on -- just a statement of intentions !
   */
  uty_term_will_echo (vf->cli);
  uty_term_will_suppress_go_ahead (vf->cli);
  uty_term_dont_linemode (vf->cli);
  uty_term_do_window_size (vf->cli);
  if (0)
    uty_term_dont_lflow_ahead (vf->cli) ;

  /* Say hello                                                          */
  vty_hello(vty);

  /* If need password, issue prompt or give up if no password to check
   * against !
   */
  if (vty->node == AUTH_NODE)
    {
      if (host.password != NULL)
        vty_out(vty, "\nUser Access Verification\n\n");
      else
        uty_close(vio, false, qs_set(NULL, "vty password is not set."));
    } ;

  /* Push the output to date and start the CLI                          */
  uty_cmd_out_push(vio) ;
  uty_cli_start(vf->cli, vty->node) ;
} ;

/*------------------------------------------------------------------------------
 * Close the reading side of VTY_TERMINAL, and close down CLI as far as
 * possible, given that output may be continuing.
 *
 * Expects to be called once only for the VTY_TERMINAL.
 */
extern void
uty_term_read_close(vio_vf vf)
{
  vty_io  vio ;

  /* Get the vio and ensure that we are all straight                    */
  vio = vf->vio ;
  assert((vio->vin == vio->vin_base) && (vio->vin == vf)) ;

  /* Do the file side of things
   *
   * Note that half closing the file sets a new timeout, sets read off
   * and write on.
   */
  uty_set_monitor(vio, 0) ;

  uty_cli_close(vf->cli, false) ;

  /* Log closing of VTY_TERM                                            */
  assert(vio->vty->type == VTY_TERMINAL) ;
  uzlog (NULL, LOG_INFO, "Vty connection (fd %d) close", vio_vfd_fd(vf->vfd)) ;
} ;

/*------------------------------------------------------------------------------
 * Close the writing side of VTY_TERMINAL.
 *
 * Pushes any buffered stuff to output and
 *
 */
extern bool
uty_term_write_close(vio_vf vf, bool final)
{
  vty_io  vio ;
  vty_readiness_t ready ;

  /* Get the vio and ensure that we are all straight                    */
  vio = vf->vio ;
  assert((vio->vin == vio->vin_base) && (vio->vin == vf)) ;

  /* Do the file side of things
   *
   * Note that half closing the file sets a new timeout, sets read off
   * and write on.
   */
  uty_set_monitor(vio, 0) ;

  vf->cli->out_active = true ; /* force the issue      */

  do
    {
      vf->cli->out_done = false ;
      ready = uty_term_write(vf) ;
    } while ((ready != write_ready) && vf->cli->out_active) ;

  final = final || !vf->cli->out_active ;

  if (!final)
    uty_term_set_readiness(vf, ready) ;

  vf->cli = uty_cli_close(vf->cli, final) ;

  return final ;
} ;

/*==============================================================================
 * Action routines for VIN_TERM/VOUT_TERM type vin/vout objects
 */

/*==============================================================================
 * Readiness and the VIN_TERM type vin.
 *
 * For TERM stuff the driving force is write ready.  This is used to prompt the
 * VOUT_TERM when there is outstanding output (obviously), but also if there
 * is buffered input in the keystroke stream.
 *
 * The VIN_TERM uses read ready only when it doesn't set write ready.  Does
 * not set both at once.
 */

/*------------------------------------------------------------------------------
 * Set read/write readiness -- for VIN_TERM/VOUT_TERM
 *
 * Note that sets only one of read or write, and sets write for preference.
 */
extern void
uty_term_set_readiness(vio_vf vf, vty_readiness_t ready)
{
  VTY_ASSERT_LOCKED() ;

  uty_vf_set_read(vf,  (ready == read_ready)) ;
  uty_vf_set_write(vf, (ready >= write_ready)) ;
} ;

/*------------------------------------------------------------------------------
 * So there is only one, common, uty_term_ready function, which:
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
uty_term_ready(vio_vfd vfd, void* action_info)
{
  vty_readiness_t ready ;

  vio_vf  vf   = action_info ;

  assert(vfd == vf->vfd) ;

  VTY_ASSERT_LOCKED() ;

  uty_term_write(vf) ;                  /* try to clear outstanding stuff   */
  do
    {
      ready  = uty_cli(vf->cli) ;      /* do any CLI work...               */
      ready |= uty_term_write(vf) ;     /* ...and any output that generates */
    } while (ready >= now_ready) ;

  uty_term_set_readiness(vf, ready) ;
} ;

/*==============================================================================
 * Reading from VTY_TERMINAL.
 *
 * The select/pselect call-back ends up in uty_term_ready().
 */

/*------------------------------------------------------------------------------
 * Read a lump of bytes and shovel into the keystroke stream
 *
 * This function is called from the vty_cli to top up the keystroke buffer,
 * or in the stealing of a keystroke to end "--more--" state.
 *
 * NB: need not be in the term_ready path.  Indeed, when the VTY_TERMINAL is
 *     initialised, this is called to suck up any telnet preamble.
 *
 * Steal keystroke if required -- see keystroke_input()
 *
 * Returns:  0 => nothing available
 *         > 0 => read at least one byte
 *          -1 => EOF (or not open, or failed)
 */
extern int
uty_term_read(vio_vf vf, keystroke steal)
{
  unsigned char buf[500] ;
  int get ;

  if (vf->vin_state != vf_open)
    return -1 ;                 /* at EOF if not open                   */

  get = read_nb(vio_vfd_fd(vf->vfd), buf, sizeof(buf)) ;
  if      (get >= 0)
    keystroke_input(vf->cli->key_stream, buf, get, steal) ;
  else if (get < 0)
    {
      if (get == -1)
        uty_vf_error(vf, "read", errno) ;

      keystroke_input(vf->cli->key_stream, NULL, 0, steal) ;
                                /* Tell keystroke stream that EOF met   */
      get = -1 ;
    } ;

  return get ;
} ;

/*==============================================================================
 * Writing to VOUT_TERM -- driven by ready state.
 *
 * There are two sets of buffering:
 *
 *   cli->cbuf -- command line   -- reflects the status of the command line
 *
 *   vf->obuf  -- command output -- which is written to the file only while
 *                                  out_active.
 *
 * The cli output takes precedence.
 *
 * Output of command stuff is subject to line_control, and may go through the
 * "--more--" mechanism.
 */

static int uty_write_lc(vio_vf vf, vio_fifo vff, vio_line_control lc) ;
static int uty_write_fifo_lc(vio_vf vf, vio_fifo vff, vio_line_control lc) ;

/*------------------------------------------------------------------------------
 * Write as much as possible of what there is.
 *
 * If not cmd_in_progress, clears cli_blocked if both FIFOs are, or become,
 * empty.
 *
 * Note that if !write_open, or becomes !write_open, then the FIFOs are empty
 * and all output instantly successful.
 *
 * Sets write on if prevented from writing everything available for output
 * by write() threatening to block.
 *
 * Returns: write_ready  if should now set write on
 *          now_ready    if should loop back and try again
 *          not_ready    otherwise
 */
static vty_readiness_t
uty_term_write(vio_vf vf)
{
  vty_cli cli = vf->cli ;
  int     ret ;

  VTY_ASSERT_LOCKED() ;

  ret = -1 ;
  while (vf->vout_state == vf_open)
    {
      /* Any outstanding line control output takes precedence           */
      ret = uty_write_lc(vf, vf->obuf, cli->olc) ;
      if (ret != 0)
        break ;

      /* Next: empty out the cli output                                 */
      ret = vio_fifo_write_nb(cli->cbuf, vio_vfd_fd(vf->vfd), true) ;
      if (ret != 0)
        break ;

      /* Finished now if not allowed to progress the command stuff      */
      if (!cli->out_active)
        return not_ready ;      /* done all can do      */

      /* If there is something in the command buffer, do that           */
      if (!vio_fifo_empty(vf->obuf))
        {
#if 0
          if (cli->out_done)
            break ;                     /* ...but not if done once      */

          cli->out_done = true ;        /* done this once               */
#endif
          assert(!cli->more_wait) ;

          ret = uty_write_fifo_lc(vf, vf->obuf, cli->olc) ;
          if (ret != 0)
            {
              if (ret < 0)
                break ;                 /* failed                       */

              if (!cli->more_wait)
                return write_ready ;    /* done a tranche               */

              /* Moved into "--more--" state
               *
               *   * the "--more--" prompt is ready to be written, so do that
               *     now
               *
               *   * if that completes, then want to run the CLI *now* to
               *     perform the first stage of the "--more--" process.
               */
              ret = vio_fifo_write_nb(cli->cbuf, vio_vfd_fd(vf->vfd), true) ;
              if (ret != 0)
                break ;

              return now_ready ;
            } ;
        } ;

      /* Exciting stuff: there is nothing left to output...
       *
       * ... watch out for half closed state.
       */
#if 0
      if (vio->closing)
        {
          if (vio->close_reason != NULL)
            {
              if (cli->drawn || cli->dirty)
                uty_out(vio, "\n") ;
              uty_out(vio, "%% %s\n", vio->close_reason) ;

              vio->close_reason = NULL ;    /* MUST discard now...      */
              continue ;                    /* ... and write away       */
            } ;

          if (!vio->closed)                 /* avoid recursion          */
            uty_close(vio) ;

          return not_ready ;                /* it's all over            */
        } ;
#endif

      if (uty_cli_draw_if_required(cli))
        continue ;                          /* do that now.             */

      /* There really is nothing left to output                         */
      cli->out_active = false ;

      return not_ready ;
    } ;

  /* Arrives here if there is more to do, or failed (or was !write_open)    */

  if (ret > 0)
    return write_ready ;

  if (ret == 0)                 /* just in case         */
    return not_ready ;

  /* If is write_open, then report the error
   *
   * If still read_open, let the reader pick up and report the error, when it
   * has finished anything it has buffered.
   */
  if (vf->vout_state == vf_open)
    uty_vf_error(vf, "write", errno) ;

  /* For whatever reason, is no longer write_open -- clear all buffers.
   */
  vio_fifo_clear(vf->obuf, true) ;      /* throw away cli stuff         */
  uty_cli_out_clear(cli) ;              /* throw away cmd stuff         */

  cli->out_active = false ;

  return not_ready ;                    /* NB: NOT blocked by I/O       */
} ;

/*------------------------------------------------------------------------------
 * Write as much as possible -- for "monitor" output.
 *
 * Outputs only:
 *
 *   a. outstanding line control stuff.
 *
 *   b. contents of CLI buffer
 *
 * And:
 *
 *   a. does not report any errors.
 *
 *   b. does not change anything except the state of the buffers.
 *
 *      In particular, for the qpthreaded world, does not attempt to change
 *      the state of the qfile or any other "thread private" structures.
 *
 * Returns: > 0 => blocked
 *            0 => all gone
 *          < 0 => failed (or !write_open)
 */
#if 0
static int
uty_write_monitor(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;

  if (!vf->write_open)
    return -1 ;

  if (vf->olc != NULL)
    {
      int ret ;
      ret = uty_write_lc(vf, vf->obuf, vf->olc) ;

      if (ret != 0)
        return ret ;
    } ;

  return  vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf), true) ;
} ;
#endif

/*------------------------------------------------------------------------------
 * Write the given FIFO to output -- subject to line control.
 *
 * Note that even if no "--more--" is set, will have set some height, so
 * that does not attempt to empty the FIFO completely all in one go.
 *
 * If the line control becomes "paused", it is time to enter "--more--" state
 * -- unless the FIFO is empty (or "--more--" is not enabled).
 *
 * NB: expects that the sock is write_open
 *
 * Returns: > 0 => blocked   or completed one tranche (more to go)
 *            0 => all gone
 *          < 0 => failed
 */
static int
uty_write_fifo_lc(vio_vf vf, vio_fifo vff, vio_line_control lc)
{
  int     ret ;
  char*   src ;
  size_t  have ;
  vty_cli cli ;

  cli = vf->cli ;

  /* Collect another line_control height's worth of output.
   *
   * Expect the line control to be empty at this point, but it does not have
   * to be.
   */
  vio_lc_set_pause(lc) ;        /* clears lc->paused                    */

  vio_fifo_set_hold_mark(vff) ;

  src = vio_fifo_get(vff, &have) ;
  while ((src != NULL) && (!lc->paused))
    {
      size_t  take ;

      if (src == NULL)
        break ;

      take = vio_lc_append(lc, src, have) ;
      src = vio_fifo_step_get(vff, &have, take) ;
    } ;

  cli->dirty = (lc->col != 0) ;

  /* Write the contents of the line control                             */
  ret = uty_write_lc(vf, vff, lc) ;

  if (ret < 0)
    return ret ;                /* give up now if failed.               */

  if ((ret == 0) && vio_fifo_empty(vff))
    return 0 ;                  /* FIFO and line control empty          */

  /* If should now do "--more--", now is the time to prepare for that.
   *
   * Entering more state issues a new prompt in the CLI buffer, which can
   * be written once line control write completes.
   *
   * The "--more--" cli will not do anything until the CLI buffer has
   * cleared.
   */
  if (lc->paused && cli->more_enabled)
    uty_cli_enter_more_wait(cli) ;

  return 1 ;                    /* FIFO or line control, not empty      */
} ;

/*------------------------------------------------------------------------------
 * Write contents of line control (if any).
 *
 * NB: expects that the sock is write_open
 *
 * NB: does nothing other than write() and buffer management.
 *
 * Returns: > 0 => blocked
 *            0 => all gone
 *          < 0 => failed
 */
static int
uty_write_lc(vio_vf vf, vio_fifo vff, vio_line_control lc)
{
  int ret ;

  ret = vio_lc_write_nb(vio_vfd_fd(vf->vfd), lc) ;

  if (ret <= 0)
    vio_fifo_clear_hold_mark(vff) ;   /* finished with FIFO contents  */

  return ret ;
} ;


#if 0

/*------------------------------------------------------------------------------
 * Start command output -- clears down the line control.
 *
 * Requires that that current line is empty -- restarts the line control
 * on the basis that is at column 0.
 */
extern void
uty_cmd_output_start(vio_vf vf)
{
  if (vf->olc != NULL)
    vio_lc_clear(vf->olc) ;

  vio_fifo_set_hold_mark(vf->obuf) ;    /* mark to keep until all gone  */
} ;

#endif




/*==============================================================================
 * Timer actions for VTY_TERMINAL
 */

/*------------------------------------------------------------------------------
 * Read timer has expired.
 *
 * If closing, then this is curtains -- have waited long enough !
 *
 * TODO .... sort out the VTY_TERMINAL time-out & death-watch timeout
 *
 * Otherwise, half close the VTY and leave it to the death-watch to sweep up.
 */
static vty_timer_time
uty_term_read_timeout(vio_timer_t* timer, void* action_info)
{
  vty_io vio = action_info ;

  VTY_ASSERT_LOCKED() ;

  uty_close(vio, true, qs_set(NULL, "Timed out")) ;

  return 0 ;
 } ;

/*------------------------------------------------------------------------------
 * Write timer has expired.
 *
 * If closing, then this is curtains -- have waited long enough !
 *
 * TODO .... sort out the VTY_TERMINAL time-out & death-watch timeout
 *
 * Otherwise, half close the VTY and leave it to the death-watch to sweep up.
 */
static vty_timer_time
uty_term_write_timeout(vio_timer_t* timer, void* action_info)
{
  vty_io vio = action_info ;

  VTY_ASSERT_LOCKED() ;

  uty_close(vio, true, qs_set(NULL, "Timed out")) ;

  return 0 ;
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
    uzlog(NULL, LOG_ERR, "could not open any VTY_TERMINAL listeners") ;
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

      assert(ainfo->ai_family == ainfo->ai_addr->sa_family) ;

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
        uzlog(NULL, LOG_ERR, "bad address %s, cannot listen for VTY", addr);
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
    uzlog(NULL, LOG_ERR, "could not use address %s, to listen for VTY", addr);

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
  union sockunion su ;
  int sock ;
  int ret ;

  VTY_ASSERT_LOCKED() ;

  /* Is there an address and is it for this family ?                    */
  if ((sa != NULL) || (sa->sa_family == family))
    /* Set up sockunion containing required family and address          */
    sockunion_new_sockaddr(&su, sa) ;
  else
    {
      /* no address or wrong family -- set up empty sockunion of
       * required family                                                */
      sockunion_init_new(&su, family) ;
      sa = NULL ;
    } ;

  /* Open the socket and set its properties                             */
  sock = sockunion_socket(family, type, protocol) ;
  if (sock < 0)
    return -1 ;

  ret = sockopt_reuseaddr (sock);

  if (ret >= 0)
    ret = sockopt_reuseport (sock);

  if (ret >= 0)
    ret = set_nonblocking(sock);

#if defined(HAVE_IPV6) && defined(IPV6_V6ONLY)
  /* Want only IPV6 on ipv6 socket (not mapped addresses)
   *
   * This distinguishes 0.0.0.0 from :: -- without this, bind() will reject the
   * attempt to bind to :: after binding to 0.0.0.0.
   */
  if ((ret >= 0) && (sa->sa_family == AF_INET6))
  {
    int on = 1;
    ret = setsockopt (sock, IPPROTO_IPV6, IPV6_V6ONLY, (void *)&on, sizeof(on));
  }
#endif

  if (ret >= 0)
    ret = sockunion_bind (sock, &su, port, sa) ;

  if (ret >= 0)
    ret = sockunion_listen (sock, 3);

  if (ret < 0)
    {
      close (sock);
      return -1 ;
    }

  /* Socket is open -- set VTY_TERMINAL listener going                  */
  uty_add_listener(sock, uty_term_accept) ;

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
        uzlog (NULL, LOG_WARNING, "can't accept vty socket : %s",
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
      uzlog (NULL, LOG_INFO, "Vty connection refused from %s", sutoa(&su).str) ;
      close (sock_fd);
      return ;
    } ;

  /* Final options (optional)                                           */
  on = 1 ;
  ret = setsockopt (sock_fd, IPPROTO_TCP, TCP_NODELAY,
                                                    (void*)&on, sizeof (on));
  if (ret < 0)
    uzlog (NULL, LOG_INFO, "can't set sockopt to socket %d: %s",
                                               sock_fd, errtoa(errno, 0).str) ;

  /* All set -- create the VTY_TERMINAL and set it going                */
  uty_term_open(sock_fd, &su);

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)", sutoa(&su).str,
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

  passert(left >= 1) ;            /* must be if not broken !            */
  passert(stroke->value == *p) ;  /* or something is wrong              */

  ++p ;         /* step past X of IAC X */
  --left ;

  /* Decode the one command that is interesting -- "NAWS"               */
  switch (stroke->value)
  {
    case tn_SB:
      passert(left > 0) ;       /* or parser failed     */

      o = *p++ ;                /* the option byte      */
      --left ;
      switch(o)
      {
        case to_NAWS:
          if (left != 4)
            {
              uzlog(NULL, LOG_WARNING,
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

/*==============================================================================
 * Output to vty which are set to "monitor".
 *
 * This is VERY TRICKY.
 *
 * If not running qpthreaded, then the objective is to get the message away
 * immediately -- do not wish it to be delayed in any way by the thread
 * system.
 *
 * So proceed as follows:
 *
 *   a. wipe command line        -- which adds output to the CLI buffer
 *
 *   b. write the CLI buffer to the sock and any outstanding line control.
 *
 *   c. write the monitor output.
 *
 *      If that does not complete, put the tail end to the CLI buffer.
 *
 *   d. restore any command line -- which adds output to the CLI buffer
 *
 *   e. write the CLI buffer to the sock
 *
 * If that all succeeds, nothing has changed as far as the VTY stuff is
 * concerned -- except that possibly some CLI output was sent before it got
 * round to it.
 *
 * Note that step (b) will deal with any output hanging around from an
 * earlier step (e).  If cannot complete that, then does not add fuel to the
 * fire -- but the message will be discarded.
 *
 * If that fails, or does not complete, then can set write on, to signal that
 * there is some output in the CLI buffer that needs to be sent, or some
 * error to be dealt with.
 *
 * The output should be tidy.
 *
 * To cut down the clutter, step (d) is performed only if the command line
 * is not empty (or if in cli_more_wait).  Once a the user has started to enter
 * a command, the prompt and the command will remain visible.
 *
 * When logging an I/O error for a vty that happens to be a monitor, the
 * monitor-ness has already been turned off.  The monitor output code does not
 * attempt to log any errors, sets write on so that the error will be picked
 * up that way.
 *
 * However, in the event of an assertion failure, it is possible that an
 * assertion will fail inside the monitor output.  The monitor_busy flag
 * prevents disaster.  It is also left set if I/O fails in monitor output, so
 * will not try to use the monitor again.
 *
 * Note that an assertion which is false for all vty monitors will recurse
 * through all the monitors, setting each one busy, in turn !
 *


 * TODO: sort out write on in the qpthreads world ??
 *
 * The problem is that the qpselect structure is designed to be accessed ONLY
 * within the thread to which it belongs.  This makes it impossible for the
 * monitor output to set/clear read/write on the vty sock... so some way
 * around this is required.
 */

/*------------------------------------------------------------------------------
 * Output logging information to all vty which are set to "monitor".
 */
extern void
uty_log(struct logline* ll, struct zlog *zl, int priority,
                                                 const char *format, va_list va)
{
  vty_io  vio ;

  VTY_ASSERT_LOCKED() ;

  vio = sdl_head(vio_monitors_base) ;

  if (vio == NULL)
    return ;                    /* go no further if no "monitor" vtys   */

  /* Prepare line for output.                                           */
  uvzlog_line(ll, zl, priority, format, va, llt_crlf) ; /* with crlf    */

  /* write to all known "monitor" vty
   *
   */
  while (vio != NULL)
    {
#if 0
      if (!vio->monitor_busy)
        {
          int ret ;

          vio->monitor_busy = 1 ;       /* close the door               */

          uty_cli_pre_monitor(vio, ll->len - 2) ;  /* claim the console */

          ret = uty_write_monitor(vio) ;
          if (ret == 0)
            {
              ret = write_nb(vio_vfd_fd(vf->vfd), ll->line, ll->len) ;

              if (ret >= 0)
                {
                  ret = uty_cli_post_monitor(vio, ll->line + ret,
                                                  ll->len  - ret) ;
                  if (ret > 0)
                    ret = uty_write_monitor(vio) ;
                } ;
            } ;

          if (ret != 0)
            /* need to prod   */ ;

          if (ret >= 0)
            vio->monitor_busy = 0 ;
        } ;
#endif
      vio = sdl_next(vio, mon_list) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Async-signal-safe version of vty_log for fixed strings.
 *
 * This is last gasp operation.
 */
void
vty_log_fixed (const char *buf, size_t len)
{
  vty_io  vio ;

  /* Write to all known "monitor" vty
   *
   * Forget all the niceties -- about to die in any case.
   */
  vio = sdl_head(vio_monitors_base) ;
  while (vio != NULL)
    {
      write(vio_vfd_fd(vio->vout_base->vfd), buf, len) ;
      write(vio_vfd_fd(vio->vout_base->vfd), "\r\n", 2) ;

      vio = sdl_next(vio, mon_list) ;
    } ;
} ;

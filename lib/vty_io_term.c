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

#include "zebra.h"

#include "vty_io_term.h"
#include "vty_cli.h"

#include "qstring.h"
#include "keystroke.h"

#include "memory.h"

#include "prefix.h"
#include "filter.h"
#include "privs.h"
#include "sockunion.h"
#include "network.h"

#include <arpa/telnet.h>
#include <sys/un.h>             /* for VTYSH    */
#include <sys/socket.h>

#define VTYSH_DEBUG 0

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_TERMINAL -- ie attached to a telnet session.
 *
 * This is called by the accept action for the VTY_TERMINAL listener.
 */
static void
uty_term_accept_new(int sock_fd, union sockunion *su)
{
  struct vty *vty ;
  vty_io vio ;
  enum vty_readiness ready ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new(VTY_TERMINAL, sock_fd) ;
  vio = vty->vio ;

  /* The text form of the address identifies the VTY                    */
  vty->vio->name = sockunion_su2str (su, MTYPE_VTY_NAME);

  /* Set the initial node                                               */
  if (no_password_check)
    {
      if (restricted_mode)
        vty->node = RESTRICTED_NODE;
      else if (host.advanced)
        vty->node = ENABLE_NODE;
      else
        vty->node = VIEW_NODE;
    }
  else
    vty->node = AUTH_NODE;

  /* Pick up current timeout setting                                    */
  vio->sock.v_timeout = vty_timeout_val;

  /* Use global 'lines' setting, as default.  May be -1 => unset        */
  vio->lines = host.lines ;

  /* For VTY_TERM use vio_line_control for '\n' and "--more--"          */
  vio->cmd_lc = vio_lc_init_new(NULL, 0, 0) ;
  uty_set_height(vio) ;         /* set initial state    */

  /* Initialise the CLI, ready for start-up messages etc.               */
  uty_cli_init(vio) ;

  /* Reject connection if password isn't set, and not "no password"     */
  if ((host.password == NULL) && (host.password_encrypt == NULL)
                                                        && ! no_password_check)
    {
      uty_close(vio, "Vty password is not set.");
      vty = NULL;
    }
  else
    {
      /* Say hello to the world. */
      vty_hello (vty);

      if (! no_password_check)
        uty_output (vty, "\nUser Access Verification\n\n");
    } ;

  /* Now start the CLI and set a suitable state of readiness            */
  ready = uty_cli_start(vio) ;
  uty_sock_set_readiness(&vio->sock, ready) ;
} ;

/*------------------------------------------------------------------------------
 * Construct vio_vf structure for new VTY_TERMINAL type VTY, and add same.
 *
 *
 */

static void
uty_term_new(vty_io vio, int sock_fd)
{
  vio_vf      vf ;

  enum vty_readiness ready ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* May only be the first vio_vf !                                     */
  assert((vio->vin_base == NULL) && (vio->vout_base == NULL)) ;

  /* Construct and add to the vio                                       */
  vf = uty_vf_new(vio, sock_fd, vfd_socket, vfd_io_read_write) ;

  uty_vin_add(vio, vf, VIN_TERM, uty_term_ready, uty_term_read_timeout) ;
  uty_vout_add(vio, vf, VIN_TERM, uty_term_ready, uty_term_write_timeout) ;

  /* Allocate and initialise a keystroke stream     TODO: CSI ??        */
  vio->key_stream = keystroke_stream_new('\0', uty_cli_iac_callback, vio) ;

  /* Pick up current timeout setting                                    */
  vf->read_timeout = vty_timeout_val;

  /* Use global 'lines' setting, as default.  May be -1 => unset        */
  vio->lines = host.lines ;

  /* For VTY_TERM use vio_line_control for '\n' and "--more--"          */
  vf->olc = vio_lc_init_new(NULL, 0, 0) ;
  uty_set_height(vio) ;         /* set initial state    */

  /* Initialise the CLI, ready for start-up messages etc.               */
  uty_cli_init(vio) ;

  /* Reject connection if password isn't set, and not "no password"     */
  if ((host.password == NULL) && (host.password_encrypt == NULL)
                                                        && ! no_password_check)
    {
      uty_close(vio, "Vty password is not set.");
      vty = NULL;
    }
  else
    {
      /* Say hello to the world. */
      vty_hello (vty);

      if (! no_password_check)
        uty_output (vty, "\nUser Access Verification\n\n");
    } ;

  /* Now start the CLI and set a suitable state of readiness            */
  ready = uty_cli_start(vio) ;
  uty_sock_set_readiness(&vio->sock, ready) ;
} ;



/*------------------------------------------------------------------------------
 *
 */
extern void
uty_term_half_close(vio_vf vf)
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

  /* Discard everything in the keystroke stream and force it to EOF     */
  if (vio->key_stream != NULL)
    keystroke_stream_set_eof(vio->key_stream) ;

  /* Turn off "--more--" so that all output clears without interruption.
   *
   * If is sitting on a "--more--" prompt, then exit the wait_more CLI.
   */
  vio->cli_more_enabled = 0 ;

  if (vio->cli_more_wait)
    uty_cli_exit_more_wait(vio) ;

  /* If a command is not in progress, enable output, which will clear
   * the output buffer if there is anything there, plus any close reason,
   * and then close.
   *
   * If command is in progress, then this process will start when it
   * completes.
   */
  if (!vio->cmd_in_progress)
    vio->cmd_out_enabled = 1 ;

  /* Log closing of VTY_TERM                                            */
  assert(vio->vty->type == VTY_TERMINAL) ;
  uzlog (NULL, LOG_INFO, "Vty connection (fd %d) close", uty_vf_fd(vf)) ;
} ;

/*------------------------------------------------------------------------------
 * vprintf to VTY_TERM
 *
 * All output goes to output fifo until command completes.
 *
 * NB: MUST be cmd_in_progress
 *
 *     Discards output if the socket is not open for whatever reason.
 */
extern int
uty_term_vprintf(vio_vf vf, const char *format, va_list args)
{
  VTY_ASSERT_LOCKED() ;

  if (!vf->write_open)
    return 0 ;                      /* discard output if not open ! */

  assert(vf->vio->cmd_in_progress) ;

  return vio_fifo_vprintf(vf->obuf, format, args) ;
} ;

/*------------------------------------------------------------------------------
 * Set/Clear "monitor" state:
 *
 *  set:   if VTY_TERM and not already "monitor" (and write_open !)
 *  clear: if is "monitor"
 */
extern void
uty_set_monitor(vty_io vio, bool on)
{
  VTY_ASSERT_LOCKED() ;

  if      (on && !vio->monitor)
    {
      if ((vio->type == VTY_TERM) && vio->sock.write_open)
        {
          vio->monitor = 1 ;
          sdl_push(vio_monitors_base, vio, mon_list) ;
        } ;
    }
  else if (!on && vio->monitor)
    {
      vio->monitor = 0 ;
      sdl_del(vio_monitors_base, vio, mon_list) ;
    }
} ;

/*==============================================================================
 * Action routines for VIN_TERM/VOUT_TERM type vin/vout objects
 */

static enum vty_readiness uty_term_write(vio_vf vf) ;

/*==============================================================================
 * Readiness and the VIN_TERM type vin.
 *
 * For TERM stuff the driving force is write ready.  This is used to prompt the
 * VOUT_TERM when there is outstanding output (obviously), but also if there
 * is buffered input in the keystroke stream.
 *
 * The VIN_TERM uses read ready only when it doesn't set write ready.  Does
 * not set both at once.
 *
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
uty_term_ready(vio_fd vfd, void* action_info)
{
  enum vty_readiness ready ;

  vio_vf  vf   = action_info ;
  vty_io  vio =  vf->vio ;

  VTY_ASSERT_LOCKED() ;

  vio->cmd_out_done = 0 ;               /* not done any command output yet  */

  uty_term_write(vf) ;                  /* try to clear outstanding stuff   */
  do
    {
      ready  = uty_cli(vio) ;           /* do any CLI work...               */
      ready |= uty_term_write(vf) ;     /* ...and any output that generates */
    } while (ready >= now_ready) ;

  uty_file_set_readiness(vf, ready) ;
} ;

/*==============================================================================
 * Reading from VTY_TERM.
 *
 * The select/pselect call-back ends up in uty_read_ready().
 *
 * Note that uty_write_ready() also calls uty_read_ready, in order to kick the
 * current CLI.
 */

/*------------------------------------------------------------------------------
 * Read a lump of bytes and shovel into the keystroke stream
 *
 * Steal keystroke if required -- see keystroke_input()
 *
 * Returns:  0 => nothing available
 *         > 0 => read at least one byte
 *          -1 => EOF (or not open, or failed)
 */
extern int
uty_read (vty_io vio, keystroke steal)
{
  unsigned char buf[500] ;
  int get ;

  if (!vio->sock.read_open)
    return -1 ;                 /* at EOF if not open           */

  get = read_nb(vio->sock.fd, buf, sizeof(buf)) ;
  if      (get >= 0)
    keystroke_input(vio->key_stream, buf, get, steal) ;
  else if (get < 0)
    {
      if (get == -1)
        uty_file_error(vio, "read") ;

      vio->sock.read_open = 0 ;
      keystroke_input(vio->key_stream, NULL, 0, steal) ;

      get = -1 ;
    } ;

  return get ;
} ;

/*==============================================================================
 * Writing to VOUT_TERM -- driven by ready state.
 *
 * There are two sets of buffering:
 *
 *   cli -- command line   -- which reflects the status of the command line
 *
 *   cmd -- command output -- which is written to the file only while
 *                            cmd_out_enabled.
 *
 * The cli output takes precedence.
 *
 * Output of command stuff is subject to line_control, and may go through the
 * "--more--" mechanism.
 */

static int uty_write_lc(vio_vf vf, vio_fifo vfifo, vio_line_control lc) ;
static int uty_write_fifo_lc(vio_vf vf, vio_fifo vfifo, vio_line_control lc) ;

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
static enum vty_readiness
uty_term_write(vio_vf vf)
{
  vty_io  vio = vf->vio ;
  int     ret ;

  VTY_ASSERT_LOCKED() ;

  ret = -1 ;
  while (vf->write_open)
    {
      /* Any outstanding line control output takes precedence           */
      if (vf->olc != NULL)
        {
          ret = uty_write_lc(vf, vf->obuf, vf->olc) ;
          if (ret != 0)
            break ;
        }

      /* Next: empty out the cli output                                 */
      ret = vio_fifo_write_nb(&vf->cli_obuf, vio->sock.fd, true) ;
      if (ret != 0)
        break ;

      /* Finished now if not allowed to progress the command stuff      */
      if (!vio->cmd_out_enabled)
        return not_ready ;      /* done all can do      */

      /* Last: if there is something in the command buffer, do that     */
      if (!vio_fifo_empty(vf->obuf))
        {
          if (vio->cmd_out_done)
            break ;                     /* ...but not if done once      */

          vio->cmd_out_done = 1 ;       /* done this once               */

          assert(!vio->cli_more_wait) ;

          if (vio->cmd_lc != NULL)
            ret = uty_write_fifo_lc(vf, vf->obuf, vf->olc) ;
          else
            ret = vio_fifo_write_nb(vf->obuf, vio->sock.fd, true) ;

          /* If moved into "--more--" state@
           *
           *   * the "--more--" prompt is ready to be written, so do that now
           *
           *   * if that completes, then want to run the CLI *now* to perform the
           *     first stage of the "--more--" process.
           */
          if (vio->cli_more_wait)
            {
              ret = vio_fifo_write_nb(vf->obuf, vio->sock.fd, true) ;
              if (ret == 0)
                return now_ready ;
            } ;

          if (ret != 0)
            break ;
        }

      /* Exciting stuff: there is nothing left to output...
       *
       * ... watch out for half closed state.
       */
      if (vio->half_closed)
        {
          if (vio->close_reason != NULL)
            {
              vio->cmd_in_progress = 1 ;    /* TODO: not use vty_out ?  */

              struct vty* vty = vio->vty ;
              if (vio->cli_drawn || vio->cli_dirty)
                vty_out(vty, VTY_NEWLINE) ;
              vty_out(vty, "%% %s%s", vio->close_reason, VTY_NEWLINE) ;

              vio->cmd_in_progress = 0 ;

              vio->close_reason = NULL ;    /* MUST discard now...      */
              continue ;                    /* ... and write away       */
            } ;

          if (!vio->closed)                 /* avoid recursion          */
            uty_close(vio) ;

          return not_ready ;                /* it's all over            */
        } ;

      /* For VTY_TERM: if the command line is not drawn, now is a good
       * time to do that.
       */
      if (vio->vty_type == VTY_TERM)
        if (uty_cli_draw_if_required(vio))
          continue ;                    /* do that now.                 */

      /* There really is nothing left to output                         */
      return not_ready ;
    } ;

  /* Arrives here if there is more to do, or failed (or was !write_open)    */

  if (ret >= 0)
    return write_ready ;

  /* If is write_open, then report the error
   *
   * If still read_open, let the reader pick up and report the error, when it
   * has finished anything it has buffered.
   */
  if (vf->write_open)
    {
      if (!vf->read_open)
        uty_file_error(vio, "write") ;

      vf->write_open = false ;          /* crash close write            */
    } ;

  /* For whatever reason, is no longer write_open -- clear all buffers.
   */
  vio_fifo_clear(vf->obuf) ;            /* throw away cli stuff         */
  uty_out_clear(vio) ;                  /* throw away cmd stuff         */

  vio->close_reason = NULL ;            /* too late for this            */

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

  return  vio_fifo_write_nb(vf->obuf, uty_vf_fd(vf), true) ;
} ;

/*------------------------------------------------------------------------------
 * Write the given FIFO to output -- subject to possible line control.
 *
 * Note that even if no "--more--" is set, will have set some height, so
 * that does not attempt to empty the FIFO completely all in one go.
 *
 * If the line control becomes "paused", it is time to enter "--more--" state
 * -- unless the FIFO is empty (or "--more--" is not enabled).
 *
 * NB: expects that the sock is write_open
 *
 * Returns: > 0 => blocked   or completed one tranche
 *            0 => all gone
 *          < 0 => failed
 */
static int
uty_write_fifo_lc(vio_vf vf, vio_fifo vfifo, vio_line_control lc)
{
  int     ret ;
  char*   src ;
  size_t  have ;

  /* Collect another line_control height's worth of output.
   *
   * Expect the line control to be empty at this point, but it does not have
   * to be.
   */
  vio_lc_set_pause(lc) ;        /* clears lc->paused                    */

  src = vio_fifo_get_rdr(vfifo, &have) ;

  while ((src != NULL) && (!lc->paused))
    {
      size_t  take ;
      take = vio_lc_append(lc, src, have) ;
      src  = vio_fifo_step_rdr(vfifo, &have, take) ;
    } ;

  vf->vio->cli_dirty = (lc->col != 0) ;

  /* Write the contents of the line control                             */
  ret = uty_write_lc(vf, vfifo, lc) ;

  if (ret < 0)
    return ret ;                /* give up now if failed.               */

  if ((ret == 0) && vio_fifo_empty(vfifo))
    return 0 ;                  /* FIFO and line control empty          */

  /* If should now do "--more--", now is the time to prepare for that.
   *
   * Entering more state issues a new prompt in the CLI buffer, which can
   * be written once line control write completes.
   *
   * The "--more--" cli will not do anything until the CLI buffer has
   * cleared.
   */
  if (lc->paused && vf->vio->cli_more_enabled)
    uty_cli_enter_more_wait(vf->vio) ;

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
uty_write_lc(vio_vf vf, vio_fifo vfifo, vio_line_control lc)
{
  int ret ;

  ret = vio_lc_write_nb(uty_vf_fd(vf), lc) ;

  if (ret <= 0)
    vio_fifo_sync_rdr(vfifo) ;     /* finished with FIFO contents  */

  return ret ;
} ;

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
} ;

/*------------------------------------------------------------------------------
 * Set the effective height for line control (if any)
 *
 * If using line_control, may enable the "--more--" output handling.
 *
 * If not, want some limit on the amount of stuff output at a time.
 *
 * Sets the line control window width and height.
 * Sets cli_more_enabled if "--more--" is enabled.
 */
extern void
uty_set_height(vio_vf vf)
{
  vty_io vio = vf->vio ;
  bool   on ;

  on = 0 ;              /* default state        */

  if ((vf->olc != NULL) && !vio->half_closed)
    {
      int height ;

      height = 0 ;      /* default state        */

      if ((vio->width) != 0)
        {
          /* If window size is known, use lines or given height         */
          if (vio->lines >= 0)
            height = vio->lines ;
          else
            {
              /* Window height, leaving one line from previous "page"
               * and one line for the "--more--" -- if at all possible
               */
              height = vio->height - 2 ;
              if (height < 1)
                height = 1 ;
            } ;
        }
      else
        {
          /* If window size not known, use lines if that has been set
           * explicitly for this terminal.
           */
          if (vio->lines_set)
            height = vio->lines ;
        } ;

      if (height > 0)
        on = 1 ;        /* have a defined height        */
      else
        height = 200 ;  /* but no "--more--"            */

      vio_lc_set_window(vio->cmd_lc, vio->width, height) ;
    } ;

  vio->cli_more_enabled = on ;
} ;

/*==============================================================================
 * Timer for VTY_TERM (and VTY_SHELL_SERV).
 */

/*------------------------------------------------------------------------------
 * Timer has expired.
 *
 * If half_closed, then this is curtains -- have waited long enough !
 *
 * Otherwise, half close the VTY and leave it to the death-watch to sweep up.
 */
static void
uty_timer_expired (vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->half_closed)
    return uty_close(vio) ;             /* curtains                     */

  uty_close(vio, "Timed out") ;         /* bring input side to a halt   */
 } ;

















/*------------------------------------------------------------------------------
 * Set read/write readiness -- for VTY_TERM
 *
 * Note that for VTY_TERM, set only one of read or write, and sets write for
 * preference.
 */
extern void
uty_file_set_readiness(vio_vf vf, enum vty_readiness ready)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vio_fd_set_read(vf, (ready == read_ready)) ;
  vio_fd_set_write(vf, (ready >= write_ready)) ;
} ;

/*------------------------------------------------------------------------------
 * Set a new timer value.
 */
extern void
uty_file_set_timer(vio_vf vf, unsigned long timeout)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf->v_timeout = timeout ;
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
 * Accept action -- create and dispatch VTY_TERM
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

  if ((p->family == AF_INET) && vty_accesslist_name)
    {
      /* VTY's accesslist apply.                                        */
      struct access_list* acl ;

      if ((acl = access_list_lookup (AFI_IP, vty_accesslist_name)) &&
          (access_list_apply (acl, p) == FILTER_DENY))
        ret = -1 ;
    }

#ifdef HAVE_IPV6
  if ((p->family == AF_INET6) && vty_ipv6_accesslist_name)
    {
      /* VTY's ipv6 accesslist apply.                                   */
      struct access_list* acl ;

      if ((acl = access_list_lookup (AFI_IP6, vty_ipv6_accesslist_name)) &&
          (access_list_apply (acl, p) == FILTER_DENY))
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

  /* All set -- create the VTY_TERM                                     */
  uty_term_accept_new(sock_fd, &su);

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)", sutoa(&su).str,
                                                                      sock_fd) ;

  return ;
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
      if (!vio->monitor_busy)
        {
          int ret ;

          vio->monitor_busy = 1 ;       /* close the door               */

          uty_cli_pre_monitor(vio, ll->len - 2) ;  /* claim the console */

          ret = uty_write_monitor(vio) ;
          if (ret == 0)
            {
              ret = write_nb(uty_vf_fd(vf), ll->line, ll->len) ;

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
      write(uty_vf_fd(vf), buf, len) ;
      write(uty_vf_fd(vf), "\r\n", 2) ;

      vio = sdl_next(vio, mon_list) ;
    } ;
} ;

/* VTY IO Functions
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

#include "vty.h"
#include "vty_io.h"
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

/*==============================================================================
 * VTY Command Output -- base functions
 *
 * During command processing the output sent here is held until the command
 * completes.
 */

static int uty_config_write(vty_io vio, bool all) ;

/*------------------------------------------------------------------------------
 * VTY output function -- cf fprintf
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
uty_out (struct vty *vty, const char *format, ...)
{
  int result;
  VTY_ASSERT_LOCKED() ;
  va_list args;
  va_start (args, format);
  result = uty_vout(vty, format, args);
  va_end (args);
  return result;
}

/*------------------------------------------------------------------------------
 * VTY output function -- cf vfprintf
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 *
 * NB: for VTY_TERM and for VTY_SHELL_SERV -- this is command output:
 *
 *     * MAY NOT do any command output if !cmd_enabled
 *
 *        * first, the life of a vty is not guaranteed unless cmd_in_progress,
 *          so should not attempt to use a vty anywhere other than command
 *          execution.
 *
 *        * second, cmd_out_enabled is false most of the time, and is only
 *          set true when a command completes, and it is time to write away
 *          the results.
 *
 *     * all output is placed in the vio->cmd_obuf.  When the command completes,
 *       the contents of the cmd_obuf will be written away -- subject to line
 *       control.
 *
 *     * output is discarded if the vty is no longer write_open
 */
extern int
uty_vout(struct vty *vty, const char *format, va_list args)
{
  vty_io vio ;
  int    ret ;

  VTY_ASSERT_LOCKED() ;

  vio = vty->vio ;

  switch (vio->type)
  {
    case VTY_STDOUT:
    case VTY_SHELL:
      ret = vprintf (format, args) ;
      break ;

    case VTY_STDERR:
      ret = vfprintf (stderr, format, args) ;
      break ;

    case VTY_CONFIG_WRITE:
      ret = vio_fifo_vprintf(&vio->cmd_obuf, format, args) ;
      if ((ret > 0) && vio_fifo_full_lump(&vio->cmd_obuf))
        ret = uty_config_write(vio, false) ;
      break ;

    case VTY_TERM:
    case VTY_SHELL_SERV:
      assert(vio->cmd_in_progress) ;

      if (!vio->sock.write_open)
        return 0 ;                      /* discard output if not open ! */

      /* fall through....       */

    case VTY_CONFIG_READ:
      ret = vio_fifo_vprintf(&vio->cmd_obuf, format, args) ;
      break ;

    default:
      zabort("impossible VTY type") ;
  } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Clear the contents of the command output FIFO etc.
 *
 * NB: does not change any of the cli_blocked/cmd_in_progress/cli_wait_more/etc
 *     flags -- competent parties must deal with those
 */
extern void
uty_out_clear(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  vio_fifo_clear(&vio->cmd_obuf) ;

  if (vio->cmd_lc != NULL)
    vio_lc_clear(vio->cmd_lc) ;
} ;

/*------------------------------------------------------------------------------
 * Flush the contents of the command output FIFO to the given file.
 *
 * Takes no notice of any errors !
 */
extern void
uty_out_fflush(vty_io vio, FILE* file)
{
  char*   src ;
  size_t  have ;

  VTY_ASSERT_LOCKED() ;

  fflush(file) ;

  while ((src = vio_fifo_get_lump(&vio->cmd_obuf, &have)) != NULL)
    {
      fwrite(src, 1, have, file) ;
      vio_fifo_got_upto(&vio->cmd_obuf, src + have) ;
    } ;

  fflush(file) ;
} ;

/*==============================================================================
 * The watch dog.
 *
 * The watch dog starts up every now and checks:
 *
 *   * for changes to the host name, which should be reflected in the
 *     prompt for any terminals.
 *
 *   * the death watch list
 */

enum { vty_watch_dog_interval = 5 } ;

static void vty_watch_dog_qnexus(qtimer qtr, void* timer_info, qtime_t when) ;
static int vty_watch_dog_thread(struct thread *thread) ;

static void uty_watch_dog_bark(void) ;
static bool uty_death_watch_scan(void) ;

/*------------------------------------------------------------------------------
 * Start watch dog -- the first time a VTY is created.
 */
extern void
uty_watch_dog_start()
{
  if (vty_cli_nexus)
    vty_watch_dog.qnexus = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                                   NULL, NULL) ;

  uty_watch_dog_bark() ;        /* start up by barking the first time   */
}

/*------------------------------------------------------------------------------
 * Stop watch dog timer -- at close down.
 *
 * Final run along the death-watch
 *
 */
extern void
uty_watch_dog_stop(void)
{
  if (vty_watch_dog.anon != NULL)
    {
      if (vty_cli_nexus)
        qtimer_free(vty_watch_dog.qnexus) ;
      else
        thread_cancel(vty_watch_dog.thread) ;
    } ;

  uty_death_watch_scan() ;      /* scan the death-watch list            */
}

/*------------------------------------------------------------------------------
 * qnexus watch dog action
 */
static void
vty_watch_dog_qnexus(qtimer qtr, void* timer_info, qtime_t when)
{
  VTY_LOCK() ;

  uty_watch_dog_bark() ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * thread watch dog action
 */
static int
vty_watch_dog_thread(struct thread *thread)
{
  VTY_LOCK() ;

  vty_watch_dog.thread = NULL ;
  uty_watch_dog_bark() ;

  VTY_UNLOCK() ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Watch dog action
 */
static void
uty_watch_dog_bark(void)
{
  uty_check_host_name() ;       /* check for host name change           */

  uty_death_watch_scan() ;      /* scan the death-watch list            */

  /* Set timer to go off again later                                    */
  if (vty_cli_nexus)
    qtimer_set(vty_watch_dog.qnexus,
                               qt_add_monotonic(QTIME(vty_watch_dog_interval)),
                                                         vty_watch_dog_qnexus) ;
  else
    {
      if (vty_watch_dog.thread != NULL)
        thread_cancel (vty_watch_dog.thread);
      vty_watch_dog.thread = thread_add_timer (vty_master,
                           vty_watch_dog_thread, NULL, vty_watch_dog_interval) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Scan the death watch list.
 *
 * A vty may finally be freed if it is closed and there is no command in
 * progress.
 */
static bool
uty_death_watch_scan(void)
{
  vty_io  vio ;
  vty_io  next ;

  next = vio_death_watch ;
  while (next != NULL)
    {
      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      if (vio->closed && !vio->cmd_in_progress)
        {
          uty_close(vio) ;      /* closes again to ensure that all buffers
                                   are released.                            */

          sdl_del(vio_death_watch, vio, vio_list) ;

          XFREE(MTYPE_VTY, vio->vty) ;
          XFREE(MTYPE_VTY, vio) ;
        } ;
    } ;

  return (vio_death_watch == NULL) ;
} ;

/*==============================================================================
 * Prototypes.
 */
static void uty_sock_init_new(vio_sock sock, int fd, void* info) ;
static void uty_sock_half_close(vio_sock sock) ;
static void uty_sock_close(vio_sock sock) ;

static void vty_read_qnexus (qps_file qf, void* file_info) ;
static void vty_write_qnexus (qps_file qf, void* file_info) ;
static void vty_timer_qnexus (qtimer qtr, void* timer_info, qtime_t when) ;

static int vty_read_thread (struct thread *thread) ;
static int vty_write_thread (struct thread *thread) ;
static int vty_timer_thread (struct thread *thread) ;

static void vtysh_read_qnexus (qps_file qf, void* file_info) ;
static int vtysh_read_thread (struct thread *thread) ;

static enum vty_readiness uty_write(vty_io vio) ;

/*==============================================================================
 * Creation and destruction of VTY objects
 */

/*------------------------------------------------------------------------------
 * Allocate new vty struct
 *
 * Allocates and initialises basic vty and vty_io structures, setting the
 * given type.
 *
 * Note that where is not setting up a vty_sock, this *may* be called from
 * any thread.
 *
 * NB: may not create a VTY_CONFIG_WRITE type vty directly
 *
 *     see: vty_open_config_write() and vty_close_config_write()
 *
 * NB: the sock_fd *must* be valid for VTY_TERM and VTY_SHELL_SERV.
 *     (So MUST be in the CLI thread to set those up !)
 *
 *     the sock_fd is ignored for everything else.
 *
 * Returns: new vty
 */
extern struct vty *
uty_new(enum vty_type type, int sock_fd)
{
  struct vty *vty ;
  struct vty_io* vio ;

  VTY_ASSERT_LOCKED() ;

  /* If this is a VTY_TERM or a VTY_SHELL, place     */
  switch (type)
  {
    case VTY_TERM:              /* Require fd -- Telnet session         */
    case VTY_SHELL_SERV:        /* Require fd -- Unix socket            */
      assert(sock_fd >= 0) ;
      break ;

    case VTY_CONFIG_WRITE:
      zabort("may not make a new VTY_CONFIG_WRITE VTY") ;
      break ;

    case VTY_CONFIG_READ:
    case VTY_STDOUT:
    case VTY_STDERR:
    case VTY_SHELL:
      sock_fd = -1 ;            /* No fd -- output to stdout/stderr     */
      break ;

    default:
      zabort("unknown VTY type") ;
  } ;

  /* Basic allocation                                                   */
  vty = XCALLOC (MTYPE_VTY, sizeof (struct vty));
  vio = XCALLOC (MTYPE_VTY, sizeof (struct vty_io)) ;

  vty->vio = vio ;
  vio->vty = vty ;

  /* Zeroising the vty_io structure has set:
   *
   *   name                = NULL -- no name, yet
   *
   *   vio_list      both pointers NULL
   *   mon_list      both pointers NULL
   *
   *   half_closed         = 0    -- NOT half closed (important !)
   *   closed              = 0    -- NOT closed      (important !)
   *   close_reason        = NULL -- no reason, yet
   *
   *   real_type           = 0    -- not material
   *   file_fd             = 0    -- not material
   *   file_error          = 0    -- not material
   *
   *   key_stream          = NULL -- no key stream (always empty, at EOF)
   *
   *   cli_drawn           = 0    -- not drawn
   *   cli_dirty           = 0    -- not dirty
   *   cli_prompt_len      = 0 )
   *   cli_extra_len       = 0 )     not material
   *   cli_echo_suppress   = 0 )
   *
   *   cli_prompt_node     = 0    -- not material
   *   cli_prompt_set      = 0    -- so prompt needs to be constructed
   *
   *   cli_blocked         = 0    -- not blocked
   *   cmd_in_progress     = 0    -- no command in progress
   *   cmd_out_enabled     = 0    -- command output is disabled
   *   cli_wait_more       = 0    -- not waiting for response to "--more--"
   *
   *   cli_more_enabled    = 0    -- not enabled for "--more--"
   *
   *   cmd_out_done        = 0    -- not material
   *
   *   cli_do              = 0    == cli_do_nothing
   *
   *   cmd_lc              = NULL -- no line control
   *
   *   fail                = 0    -- no login failures yet
   *
   *   hist                = empty vector
   *   hp                  = 0    -- at the beginning
   *   hindex              = 0    -- the beginning
   *
   *   width               = 0    -- unknown console width
   *   height              = 0    -- unknown console height
   *
   *   lines               = 0    -- no limit
   *   lines_set           = 0    -- no explicit setting
   *
   *   monitor             = 0    -- not a monitor
   *   monitor_busy        = 0    -- not a busy monitor
   *
   *   config              = 0    -- not holder of "config" mode
   */
  confirm(cli_do_nothing == 0) ;
  confirm(AUTH_NODE == 0) ;     /* default node type    */

  vio->type   = type ;

  /* Zeroising the vty structure has set:
   *
   *   node      = 0  TODO: something better for node value ????
   *   buf       = NULL -- no command line, yet
   *   parsed    = NULL -- no parsed command, yet
   *   lineno    = 0    -- nothing read, yet
   *   index     = NULL -- nothing, yet
   *   index_sub = NULL -- nothing, yet
   */
  if (type == VTY_TERM)
    vty->newline = "\n" ;       /* line control looks after "\r\n"      */
  else
    vty->newline = "\n" ;

  /* Initialise the vio_sock,                                           */
  uty_sock_init_new(&vio->sock, sock_fd, vio) ;

  /* Make sure all buffers etc. are initialised clean and empty.
   *
   * Note that no buffers are actually allocated at this stage.
   */
  qs_init_new(&vio->cli_prompt_for_node, 0) ;

  qs_init_new(&vio->cl,  0) ;
  qs_init_new(&vio->clx, 0) ;

  vio_fifo_init_new(&vio->cli_obuf, 2 * 1024) ; /* allocate in 2K lumps */

  vio_fifo_init_new(&vio->cmd_obuf, 8 * 1024) ;

  /* Place on list of known vio/vty                                     */
  sdl_push(vio_list_base, vio, vio_list) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_TERM -- ie attached to a telnet session.
 *
 * Returns: new vty
 */
static struct vty *
uty_new_term(int sock_fd, union sockunion *su)
{
  struct vty *vty ;
  vty_io vio ;
  enum vty_readiness ready ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new (VTY_TERM, sock_fd) ;
  vio = vty->vio ;

  /* Allocate and initialise a keystroke stream     TODO: CSI ??        */
  vio->key_stream = keystroke_stream_new('\0', uty_cli_iac_callback, vio) ;

  /* Set the socket action functions                                    */
  if (vty_cli_nexus)
    {
      vio->sock.action.read.qnexus  = vty_read_qnexus ;
      vio->sock.action.write.qnexus = vty_write_qnexus ;
      vio->sock.action.timer.qnexus = vty_timer_qnexus ;
    }
  else
    {
      vio->sock.action.read.thread  = vty_read_thread ;
      vio->sock.action.write.thread = vty_write_thread ;
      vio->sock.action.timer.thread = vty_timer_thread ;
    } ;

  /* The text form of the address identifies the VTY                    */
  vio->name = sockunion_su2str (su, MTYPE_VTY_NAME);

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
      uty_half_close (vio, "Vty password is not set.");
      vty = NULL;
    }
  else
    {
      /* Say hello to the world. */
      vty_hello (vty);

      if (! no_password_check)
        uty_out (vty, "%sUser Access Verification%s%s", VTY_NEWLINE,
                                                     VTY_NEWLINE, VTY_NEWLINE);
    } ;

  /* Now start the CLI and set a suitable state of readiness            */
  ready = uty_cli_start(vio) ;
  uty_sock_set_readiness(&vio->sock, ready) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_SHELL_SERV -- ie attached to a vtysh session.
 *
 * Returns: new vty
 */
static struct vty *
uty_new_shell_serv(int sock_fd)
{
  struct vty *vty ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new (VTY_SHELL_SERV, sock_fd) ;
  vio = vty->vio ;

  /* Set the action functions                                           */
  if (vty_cli_nexus)
    {
      vio->sock.action.read.qnexus  = vtysh_read_qnexus ;
      vio->sock.action.write.qnexus = vty_write_qnexus ;
      vio->sock.action.timer.qnexus = NULL ;
    }
  else
    {
      vio->sock.action.read.thread  = vtysh_read_thread ;
      vio->sock.action.write.thread = vty_write_thread ;
      vio->sock.action.timer.thread = NULL ;
    } ;

  vty->node = VIEW_NODE;

  /* Kick start the CLI etc.                                            */
    uty_sock_set_readiness(&vio->sock, write_ready) ;

  return vty;
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

/*------------------------------------------------------------------------------
 * Return "name" of VTY
 *
 * For VTY_TERM this is the IP address of the far end of the telnet connection.
 */
extern const char*
uty_get_name(vty_io vio)
{
  return (vio->name != NULL) ? vio->name : "?" ;
} ;

/*------------------------------------------------------------------------------
 * Closing down VTY for reading.
 *
 * For VTY_TERM  (must be in CLI thread):
 *
 *   * shut the socket for reading
 *   * discard all buffered input, setting it to "EOF"
 *   * turns off any monitor status !
 *   * drop down to RESTRICTED_NODE
 *
 * For VTY_SHELL_SERV  (must be in CLI thread):
 *
 *   * shut the socket for reading
 *   * discard all buffered input
 *   * drop down to RESTRICTED_NODE
 *
 * In all cases:
 *
 *   * place on death watch
 *   * set the vty half_closed
 *   * sets the reason for closing (if any given)
 *
 * For VTY_TERM and VTY_SHELL_SERV, when the output side has emptied out all
 * the buffers, the VTY is closed.
 *
 * May already have set the vio->close_reason, or can set it now.  (Passing a
 * NULL reason has no effect on any existing posted reason.)
 */
extern void
uty_half_close (vty_io vio, const char* reason)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->half_closed)
    return ;

  if (reason != NULL)
    vio->close_reason = reason ;

  /* Do the file side of things
   *
   * Note that half closing the file sets a new timeout, sets read off
   * and write on.
   */
  uty_sock_half_close(&vio->sock) ;
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

  /* Make sure no longer holding the config symbol of power             */
  uty_config_unlock(vio->vty, RESTRICTED_NODE) ;

  /* Log closing of VTY_TERM                                            */
  if (vio->type == VTY_TERM)
    uzlog (NULL, LOG_INFO, "Vty connection (fd %d) close", vio->sock.fd) ;

  /* Move to the death watch list                                       */
  sdl_del(vio_list_base, vio, vio_list) ;
  sdl_push(vio_death_watch, vio, vio_list) ;

  vio->half_closed = 1 ;
} ;

/*------------------------------------------------------------------------------
 * Closing down VTY.
 *
 * Shuts down everything and discards all buffers etc. etc.
 *
 * If cmd_in_progress, cannot complete the process -- but sets the closed
 * flag.
 *
 * Can call vty_close() any number of times.
 *
 * The vty structure is placed on death watch, which will finally free the
 * structure once no longer cmd_in_progress.
 */
extern void
uty_close (vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  /* Empty all the output buffers                                       */
  vio_fifo_reset_keep(&vio->cli_obuf) ;
  vio_fifo_reset_keep(&vio->cmd_obuf) ;
  vio->cmd_lc = vio_lc_reset_free(vio->cmd_lc) ;

  /* If not already closed, close.                                      */
  if (!vio->closed)
    {
      uty_half_close(vio, NULL) ;   /* place on death watch -- if not
                                       already done                     */
      uty_cli_close(vio) ;          /* tell the CLI to stop             */

      vio->closed = 1 ;             /* now closed (stop uty_write()
                                       from recursing)                  */

      if (vio->sock.write_open)
        uty_write(vio) ;            /* last gasp attempt                */

      uty_sock_close(&vio->sock) ;

    } ;

  /* Nothing more should happen, so can now release almost everything,
   * the exceptions being the things that are related to a cmd_in_progress.
   *
   * All writing to buffers is suppressed, and as the sock has been closed,
   * there will be no more read_ready or write_ready events.
   */
  if (vio->name != NULL)
    XFREE(MTYPE_VTY_NAME, vio->name) ;

  vio->key_stream = keystroke_stream_free(vio->key_stream) ;

  qs_free_body(&vio->cli_prompt_for_node) ;
  qs_free_body(&vio->cl) ;

  {
    qstring line ;
    while ((line = vector_ream_keep(&vio->hist)) != NULL)
      qs_reset_free(line) ;
  } ;

  /* The final stage cannot be completed if cmd_in_progress.
   *
   * The clx is pointed at by vty->buf -- containing the current command.
   *
   * Once everything is released, can take the vty off death watch, and
   * release the vio and the vty.
   */
  if (!vio->cmd_in_progress)
    {
      qs_free_body(&vio->clx) ;
      vio->vty->buf = NULL ;
    } ;
} ;

/*==============================================================================
 * For writing configuration file by command, temporarily redirect output to
 * an actual file.
 */

/*------------------------------------------------------------------------------
 * Set the given fd as the VTY_FILE output.
 */
extern void
vty_open_config_write(struct vty* vty, int fd)
{
  vty_io vio ;

  VTY_LOCK() ;

  vio = vty->vio ;

  assert((vio->type != VTY_CONFIG_WRITE) && (vio->type != VTY_NONE)) ;

  vio->real_type  = vio->type ;

  vio->type       = VTY_CONFIG_WRITE ;
  vio->file_fd    = fd ;
  vio->file_error = 0 ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Write away configuration file stuff -- all or just the full lump(s).
 *
 * Returns: > 0 => blocked
 *            0 => all gone (up to last lump if !all)
 *          < 0 => failed -- see vio->file_error
 */
static int
uty_config_write(vty_io vio, bool all)
{
  int ret ;

  VTY_ASSERT_LOCKED() ;

  if (vio->file_error == 0)
    {
      ret = vio_fifo_write_nb(&vio->cmd_obuf, vio->file_fd, all) ;

      if (ret < 0)
        vio->file_error = errno ;
    }
  else
    ret = -1 ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Write away any pending stuff, and return the VTY to normal.
 */
extern int
vty_close_config_write(struct vty* vty)
{
  vty_io vio ;
  int err ;

  VTY_LOCK() ;

  vio = vty->vio ;

  assert((vio->type == VTY_CONFIG_WRITE) && (vio->real_type != VTY_NONE)) ;

  uty_config_write(vio, true) ; /* write all that is left       */

  err = vio->file_error ;

  vio->type       = vio->real_type ;
  vio->file_fd    = -1 ;
  vio->file_error = 0 ;

  VTY_UNLOCK() ;

  return err ;
} ;

/*==============================================================================
 * vio_sock level operations
 */

/*------------------------------------------------------------------------------
 * Initialise a new vio_sock structure.
 *
 * Requires that: the vio_sock structure is not currently in use.
 *
 *                if fd >= 0 then: sock is open and ready read and write
 *                      otherwise: sock is not open
 *
 *                there are no errors, yet.
 *
 * Sets timeout to no timeout at all -- timeout is optional.
 *
 * NB: MUST be in the CLI thread if the fd is >= 0 !
 */
static void
uty_sock_init_new(vio_sock sock, int fd, void* info)
{
  VTY_ASSERT_LOCKED() ;

  if (fd >= 0)
    VTY_ASSERT_CLI_THREAD() ;

  memset(sock, 0, sizeof(struct vio_sock)) ;

  /* Zeroising the structure has set:
   *
   *   action        = all the actions set NULL
   *
   *   error_seen    = 0  -- no error, yet
   *
   *   qf            = NULL -- no qfile, yet
   *   t_read        = NULL )  no threads, yet
   *   t_write       = NULL )
   *
   *   v_timeout     = 0    -- no timeout set
   *   timer_runing  = 0    -- not running, yet
   *   t_timer       = NULL -- no timer thread, yet
   *   qtr           = NULL -- no qtimer, yet
   */
  sock->fd          = fd ;
  sock->info        = info ;

  sock->read_open   = (fd >= 0) ;
  sock->write_open  = (fd >= 0) ;

  if ((fd >= 0) && vty_cli_nexus)
    {
      sock->qf = qps_file_init_new(NULL, NULL);
      qps_add_file(vty_cli_nexus->selection, sock->qf, sock->fd, sock->info);
    } ;
} ;

/*------------------------------------------------------------------------------
 * Restart the timer.
 *
 * If a timeout time is set, then start or restart the timer with that value.
 *
 * If no timeout time is set, and the timer is running, unset it.
 */
static void
uty_sock_restart_timer(vio_sock sock)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (sock->v_timeout != 0)
    {
      assert(sock->action.timer.anon != NULL) ;

      if (vty_cli_nexus)
        {
          if (sock->qtr == NULL)    /* allocate qtr if required     */
            sock->qtr = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                             NULL, sock->info) ;
          qtimer_set(sock->qtr, qt_add_monotonic(QTIME(sock->v_timeout)),
                                                    sock->action.timer.qnexus) ;
        }
      else
        {
          if (sock->t_timer != NULL)
            thread_cancel (sock->t_timer);
          sock->t_timer = thread_add_timer (vty_master,
                       sock->action.timer.thread, sock->info, sock->v_timeout) ;
        } ;

      sock->timer_running = 1 ;
    }
  else if (sock->timer_running)
    {
      if (vty_cli_nexus)
        {
          if (sock->qtr != NULL)
            qtimer_unset(sock->qtr) ;
        }
      else
        {
          if (sock->t_timer != NULL)
            thread_cancel (sock->t_timer) ;
        } ;

      sock->timer_running = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set read on/off
 *
 * Returns: the on/off state set
 */
static bool
uty_sock_set_read(vio_sock sock, bool on)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (sock->fd < 0)
    return 0 ;

  if (on)
    {
      assert(sock->action.read.anon != NULL) ;

      if (vty_cli_nexus)
          qps_enable_mode(sock->qf, qps_read_mnum, sock->action.read.qnexus) ;
      else
        {
          if (sock->t_read != NULL)
            thread_cancel(sock->t_read) ;

          sock->t_read = thread_add_read(vty_master,
                               sock->action.read.thread, sock->info, sock->fd) ;
        } ;
    }
  else
    {
      if (vty_cli_nexus)
        qps_disable_modes(sock->qf, qps_read_mbit) ;
      else
        {
          if (sock->t_read != NULL)
            thread_cancel (sock->t_read) ;
        } ;
    } ;

  return on ;
} ;

/*------------------------------------------------------------------------------
 * Set write on/off
 *
 * Returns: the on/off state set
 */
static bool
uty_sock_set_write(vio_sock sock, bool on)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (sock->fd < 0)
    return 0 ;

  if (on)
    {
      assert(sock->action.write.anon != NULL) ;

      if (vty_cli_nexus)
        qps_enable_mode(sock->qf, qps_write_mnum, sock->action.write.qnexus) ;
      else
        {
          if (sock->t_write != NULL)
            thread_cancel(sock->t_write) ;

          sock->t_write = thread_add_write(vty_master,
                              sock->action.write.thread, sock->info, sock->fd) ;
        } ;
    }
  else
    {
      if (vty_cli_nexus)
        qps_disable_modes(sock->qf, qps_write_mbit) ;
      else
        {
          if (sock->t_write != NULL)
            thread_cancel (sock->t_write) ;
        } ;
    } ;

  return on ;
} ;

/*------------------------------------------------------------------------------
 * Set read/write readiness -- for VTY_TERM
 *
 * Note that for VTY_TERM, set only one of read or write, and sets write for
 * preference.
 */
extern void
uty_sock_set_readiness(vio_sock sock, enum vty_readiness ready)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  uty_sock_set_read(sock, (ready == read_ready)) ;
  uty_sock_set_write(sock, (ready >= write_ready)) ;
} ;

/*------------------------------------------------------------------------------
 * Set a new timer value.
 */
extern void
uty_sock_set_timer(vio_sock sock, unsigned long timeout)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  sock->v_timeout = timeout ;
  if (sock->timer_running)
    uty_sock_restart_timer(sock) ;
} ;

/*------------------------------------------------------------------------------
 * Close given vty sock for reading.
 *
 * Sets timer to timeout for clearing any pending output.
 *
 * NB: if there is a socket, MUST be in the CLI thread
 */
static void
uty_sock_half_close(vio_sock sock)
{
  VTY_ASSERT_LOCKED() ;

  sock->read_open = 0 ;         /* make sure                    */

  if (sock->fd < 0)
    return ;                    /* nothing more if no socket    */

  VTY_ASSERT_CLI_THREAD() ;

  shutdown(sock->fd, SHUT_RD) ; /* actual half close            */

  uty_sock_set_read(sock, off) ;
  uty_sock_set_write(sock, on) ;
  sock->v_timeout = 30 ;        /* for output to clear          */
  uty_sock_restart_timer(sock) ;
} ;

/*------------------------------------------------------------------------------
 * Close given vio_sock, completely -- shut down any timer.
 *
 * Structure is cleared of everything except the last error !
 *
 * NB: if there is a socket, MUST be in the CLI thread
 */
static void
uty_sock_close(vio_sock sock)
{
  VTY_ASSERT_LOCKED() ;

  sock->read_open   = 0 ;       /* make sure                    */
  sock->write_open  = 0 ;

  if (sock->fd < 0)
    {
      assert( (sock->qf      == NULL)
           && (sock->qtr     == NULL)
           && (sock->t_read  == NULL)
           && (sock->t_write == NULL)
           && (sock->t_timer == NULL) ) ;
      return ;                  /* no more to be done here      */
    } ;

  VTY_ASSERT_CLI_THREAD() ;
  close(sock->fd) ;

  if (vty_cli_nexus)
    {
      assert((sock->qf != NULL) && (sock->fd == qps_file_fd(sock->qf))) ;
      qps_remove_file(sock->qf) ;
      qps_file_free(sock->qf) ;
      sock->qf = NULL ;
    } ;

  sock->fd      = -1 ;

  if (sock->t_read != NULL)
    thread_cancel(sock->t_write) ;
  if (sock->t_write != NULL)
    thread_cancel(sock->t_write) ;

  sock->t_read  = NULL ;
  sock->t_write = NULL ;

  sock->info              = NULL ;
  sock->action.read.anon  = NULL ;
  sock->action.write.anon = NULL ;
  sock->action.timer.anon = NULL ;

  if (sock->qtr != NULL)
    qtimer_free(sock->qtr) ;
  if (sock->t_timer != NULL)
    thread_cancel(sock->t_timer) ;

  sock->v_timeout = 0 ;
  sock->qtr       = NULL ;
  sock->t_timer   = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Dealing with an I/O error on VTY socket
 *
 * If this is the first error for this VTY, produce suitable log message.
 *
 * If is a "monitor", turn that off, *before* issuing log message.
 */
static int
uty_sock_error(vty_io vio, const char* what)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* can no longer be a monitor !  *before* any logging !               */
  uty_set_monitor(vio, 0) ;

  /* if this is the first error, log it                                 */
  if (vio->sock.error_seen == 0)
    {
      const char* type ;
      switch (vio->type)
      {
        case VTY_TERM:
          type = "VTY Terminal" ;
          break ;
        case VTY_SHELL_SERV:
          type = "VTY Shell Server" ;
          break ;
        default:
          zabort("unknown VTY type for uty_sock_error()") ;
      } ;

      vio->sock.error_seen = errno ;
      uzlog(NULL, LOG_WARNING, "%s: %s failed on fd %d: %s",
                type, what, vio->sock.fd, errtoa(vio->sock.error_seen, 0).str) ;
    } ;

  return -1 ;
} ;

/*==============================================================================
 * Readiness and the VTY_TERM type VTY.
 *
 * For VTY_TERM the driving force is write ready.  This is used to prompt the
 * VTY_TERM when there is outstanding output (obviously), but also if there
 * is buffered input in the keystroke stream.
 *
 * The VTY_TERM uses read ready only when it doesn't set write ready.  Does
 * not set both at once.
 *
 * So there is only one, common, uty_ready function, which:
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
uty_ready(vty_io vio)
{
  enum vty_readiness ready ;

  VTY_ASSERT_LOCKED() ;

  vio->cmd_out_done = 0 ;         /* not done any command output yet    */

  uty_write(vio) ;                /* try to clear outstanding stuff     */
  do
    {
      ready  = uty_cli(vio) ;     /* do any CLI work...                 */
      ready |= uty_write(vio) ;   /* ...and any output that generates   */
    } while (ready >= now_ready) ;

  uty_sock_set_readiness(&vio->sock, ready) ;
  uty_sock_restart_timer(&vio->sock) ;
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
 * Callback -- qnexus: ready to read -> kicking CLI
 */
static void
vty_read_qnexus(qps_file qf, void* file_info)
{
  vty_io vio = file_info;

  VTY_LOCK() ;

  assert((vio->sock.fd == qf->fd) && (vio == vio->sock.info)) ;

  uty_ready(vio) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Callback -- threads: ready to read -> kicking CLI
 */
static int
vty_read_thread(struct thread *thread)
{
  vty_io vio = THREAD_ARG (thread);

  VTY_LOCK() ;

  assert(vio->sock.fd == THREAD_FD (thread) && (vio == vio->sock.info)) ;

  vio->sock.t_read = NULL ;     /* implicitly   */
  uty_ready(vio);

  VTY_UNLOCK() ;
  return 0 ;
}

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
        uty_sock_error(vio, "read") ;

      vio->sock.read_open = 0 ;
      keystroke_input(vio->key_stream, NULL, 0, steal) ;

      get = -1 ;
    } ;

  return get ;
} ;

/*==============================================================================
 * The write sock action for VTY_TERM type VTY
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

static int uty_write_lc(vty_io vio, vio_fifo vf, vio_line_control lc) ;
static int uty_write_fifo_lc(vty_io vio, vio_fifo vf, vio_line_control lc) ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to write -> try to empty buffers
 */
static void
vty_write_qnexus(qps_file qf, void* file_info)
{
  vty_io vio  = file_info ;

  VTY_LOCK() ;

  assert((vio->sock.fd == qf->fd) && (vio == vio->sock.info)) ;

  uty_ready(vio) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Callback -- thread: ready to write -> try to empty buffers
 */
static int
vty_write_thread(struct thread *thread)
{
  vty_io vio  = THREAD_ARG (thread);

  VTY_LOCK() ;

  assert(vio->sock.fd == THREAD_FD (thread) && (vio == vio->sock.info)) ;

  vio->sock.t_write = NULL;     /* implicitly   */
  uty_ready(vio) ;

  VTY_UNLOCK() ;
  return 0 ;
}

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
uty_write(vty_io vio)
{
  int ret ;

  VTY_ASSERT_LOCKED() ;

  ret = -1 ;
  while (vio->sock.write_open)
    {
      /* Any outstanding line control output takes precedence           */
      if (vio->cmd_lc != NULL)
        {
          ret = uty_write_lc(vio, &vio->cmd_obuf, vio->cmd_lc) ;
          if (ret != 0)
            break ;
        }

      /* Next: empty out the cli output                                 */
      ret = vio_fifo_write_nb(&vio->cli_obuf, vio->sock.fd, true) ;
      if (ret != 0)
        break ;

      /* Finished now if not allowed to progress the command stuff      */
      if (!vio->cmd_out_enabled)
        return not_ready ;      /* done all can do      */

      /* Last: if there is something in the command buffer, do that     */
      if (!vio_fifo_empty(&vio->cmd_obuf))
        {
          if (vio->cmd_out_done)
            break ;                     /* ...but not if done once      */

          vio->cmd_out_done = 1 ;       /* done this once               */

          assert(!vio->cli_more_wait) ;

          if (vio->cmd_lc != NULL)
            ret = uty_write_fifo_lc(vio, &vio->cmd_obuf, vio->cmd_lc) ;
          else
            ret = vio_fifo_write_nb(&vio->cmd_obuf, vio->sock.fd, true) ;

          /* If moved into "--more--" state@
           *
           *   * the "--more--" prompt is ready to be written, so do that now
           *
           *   * if that completes, then want to run the CLI *now* to perform the
           *     first stage of the "--more--" process.
           */
          if (vio->cli_more_wait)
            {
              ret = vio_fifo_write_nb(&vio->cli_obuf, vio->sock.fd, true) ;
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
      if (vio->type == VTY_TERM)
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
  if (vio->sock.write_open)
    {
      if (!vio->sock.read_open)
        uty_sock_error(vio, "write") ;

      vio->sock.write_open = 0 ;        /* crash close write    */
    } ;

  /* For whatever reason, is no longer write_open -- clear all buffers.
   */
  vio_fifo_clear(&vio->cli_obuf) ;      /* throw away cli stuff */
  uty_out_clear(vio) ;                  /* throw away cmd stuff */

  vio->close_reason = NULL ;            /* too late for this    */

  return not_ready ;                   /* NB: NOT blocked by I/O       */
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
uty_write_monitor(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  if (!vio->sock.write_open)
    return -1 ;

  if (vio->cmd_lc != NULL)
    {
      int ret ;
      ret = uty_write_lc(vio, &vio->cmd_obuf, vio->cmd_lc) ;

      if (ret != 0)
        return ret ;
    } ;

  return  vio_fifo_write_nb(&vio->cli_obuf, vio->sock.fd, true) ;
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
uty_write_fifo_lc(vty_io vio, vio_fifo vf, vio_line_control lc)
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

  src = vio_fifo_get_rdr(vf, &have) ;

  while ((src != NULL) && (!lc->paused))
    {
      size_t  take ;
      take = vio_lc_append(lc, src, have) ;
      src  = vio_fifo_step_rdr(vf, &have, take) ;
    } ;

  vio->cli_dirty = (lc->col != 0) ;

  /* Write the contents of the line control                             */
  ret = uty_write_lc(vio, vf, lc) ;

  if (ret < 0)
    return ret ;                /* give up now if failed.               */

  if ((ret == 0) && vio_fifo_empty(vf))
    return 0 ;                  /* FIFO and line control empty          */

  /* If should now do "--more--", now is the time to prepare for that.
   *
   * Entering more state issues a new prompt in the CLI buffer, which can
   * be written once line control write completes.
   *
   * The "--more--" cli will not do anything until the CLI buffer has
   * cleared.
   */
  if (lc->paused && vio->cli_more_enabled)
    uty_cli_enter_more_wait(vio) ;

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
uty_write_lc(vty_io vio, vio_fifo vf, vio_line_control lc)
{
  int ret ;

  ret = vio_lc_write_nb(vio->sock.fd, lc) ;

  if (ret <= 0)
    vio_fifo_sync_rdr(vf) ;     /* finished with FIFO contents  */

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Start command output -- clears down the line control.
 *
 * Requires that that current line is empty -- restarts the line control
 * on the basis that is at column 0.
 */
extern void
uty_cmd_output_start(vty_io vio)
{
  if (vio->cmd_lc != NULL)
    vio_lc_clear(vio->cmd_lc) ;
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
uty_set_height(vty_io vio)
{
  bool on ;

  on = 0 ;              /* default state        */

  if ((vio->cmd_lc != NULL) && !vio->half_closed)
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

  uty_half_close(vio, "Timed out") ;    /* bring input side to a halt   */
 } ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: deal with timer timeout.
 */
static void
vty_timer_qnexus (qtimer qtr, void* timer_info, qtime_t when)
{
  vty_io vio = timer_info ;

  VTY_LOCK() ;

  uty_timer_expired(vio);

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Callback -- thread: deal with timer timeout.
 */
static int
vty_timer_thread (struct thread *thread)
{
  vty_io vio = THREAD_ARG (thread);

  VTY_LOCK() ;

  vio->sock.t_timer = NULL ;    /* implicitly   */

  uty_timer_expired(vio) ;

  VTY_UNLOCK() ;
  return 0;
}

/*==============================================================================
 * VTY Listener(s)
 *
 * Have listeners for VTY_TERM and VTY_SHELL_SERV types of VTY.
 */

typedef struct vty_listener* vty_listener ;

struct vty_listener
{
  vty_listener    next ;        /* ssl type list        */

  enum vty_type   type ;

  struct vio_sock sock ;
};

/* List of listeners so can tidy up.                                    */
static vty_listener vty_listeners_list  = NULL ;

/* Prototypes for listener stuff                                        */
static int uty_serv_sock_addrinfo (const char *hostname, unsigned short port) ;
static int uty_serv_sock(const char* addr, unsigned short port) ;
static int uty_serv_sock_open(sa_family_t family, int type, int protocol,
                                     struct sockaddr* sa, unsigned short port) ;
static int uty_serv_vtysh(const char *path) ;
static int vty_accept_thread(struct thread *thread) ;
static void vty_accept_qnexus(qps_file qf, void* listener) ;
static int uty_accept(vty_listener listener, int listen_sock) ;
static int uty_accept_term(vty_listener listener) ;
static int uty_accept_shell_serv (vty_listener listener) ;

static void uty_serv_start_listener(int fd, enum vty_type type) ;

/*------------------------------------------------------------------------------
 * If possible, will use getaddrinfo() to find all the things to listen on.
 */

#if defined(HAVE_IPV6) && !defined(NRL)
# define VTY_USE_ADDRINFO 1
#else
# define VTY_USE_ADDRINFO 0
#endif

/*------------------------------------------------------------------------------
 * Open VTY listener(s)
 *
 *   addr   -- address ) to listen for VTY_TERM connections
 *   port   -- port    )
 *   path   -- path for VTYSH connections -- if VTYSH_ENABLED
 */
extern void
uty_open_listeners(const char *addr, unsigned short port, const char *path)
{
  VTY_ASSERT_LOCKED() ;

  /* If port is set to 0, do not listen on TCP/IP at all!       */
  if (port)
    {
      int n ;

      if (VTY_USE_ADDRINFO)
        n = uty_serv_sock_addrinfo(addr, port);
      else
        n = uty_serv_sock(addr, port);

      if (n == 0)
        uzlog(NULL, LOG_ERR, "could not open any VTY listeners") ;
    }

  /* If want to listen for vtysh, set up listener now           */
  if (VTYSH_ENABLED && (path != NULL))
    uty_serv_vtysh(path) ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY listener
 *
 *   addr   -- address ) to listen for VTY_TERM connections
 *   port   -- port    )
 *   path   -- path for VTYSH connections -- if VTYSH_ENABLED
 */
extern void
uty_close_listeners(void)
{
  vty_listener listener ;

  VTY_ASSERT_LOCKED() ;

  while ((listener = ssl_pop(&listener, vty_listeners_list, next)) != NULL)
    {
      uty_sock_close(&listener->sock) ; /* no ceremony, no flowers      */
      XFREE(MTYPE_VTY, listener) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Open listener(s) for VTY_TERM -- using getaddrinfo().
 *
 * Returns: number of listeners successfully opened.
 */
static int
uty_serv_sock_addrinfo (const char *hostname, unsigned short port)
{
#if VTY_USE_ADDRINFO

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

  ret = getaddrinfo (hostname, port_str, &req, &ainfo);

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

      ret = uty_serv_sock_open(ainfo->ai_family, ainfo->ai_socktype,
                                     ainfo->ai_protocol, ainfo->ai_addr, port) ;
      if (ret >= 0)
        ++n ;
    }
  while ((ainfo = ainfo->ai_next) != NULL);

  freeaddrinfo (ainfo_save);

  return n ;

#else
  zabort("uty_serv_sock_addrinfo not implemented") ;
#endif /* VTY_USE_ADDRINFO */
}

/*------------------------------------------------------------------------------
 * Open listener(s) for VTY_TERM -- not using getaddrinfo() !
 *
 * Returns: number of listeners successfully opened.
 */
static int
uty_serv_sock(const char* addr, unsigned short port)
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
  ret = uty_serv_sock_open(AF_INET, SOCK_STREAM, 0, sa, port) ;
  if (ret >= 0)
    ++n ;               /* opened socket        */
  if (ret == 1)
    sa = NULL ;         /* used the address     */

#if HAVE_IPV6
  /* Try for AF_INET6                                                   */
  ret = uty_serv_sock_open(AF_INET6, SOCK_STREAM, 0, sa, port) ;
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
 * Open a VTY_TERM listener socket.
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
uty_serv_sock_open(sa_family_t family, int type, int protocol,
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

  /* Socket is open -- set VTY Term listener going                      */
  uty_serv_start_listener(sock, VTY_TERM) ;

  /* Return OK and signal whether used address or not                   */
  return (sa != NULL) ? 1 : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Open a VTY_SHEL_SERV listener socket (UNIX domain).
 *
 * Returns: < 0 => failed
 *         >= 0 => OK
 */
static int
uty_serv_vtysh(const char *path)
{
  int ret;
  int sock, sa_len, path_len ;
  struct sockaddr_un sa_un ;
  mode_t old_mask;
  struct zprivs_ids_t ids;

  VTY_ASSERT_LOCKED() ;

  /* worry about the path length                                        */
  path_len = strlen(path) + 1 ;
  if (path_len >= (int)sizeof(sa_un.sun_path))
    {
      uzlog(NULL, LOG_ERR, "path too long for unix stream socket: '%s'", path);
      return -1 ;
    } ;

  /* First of all, unlink existing socket                               */
  unlink (path);

  /* Make UNIX domain socket.                                           */
  sock = socket (AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0)
    {
      uzlog(NULL, LOG_ERR, "Cannot create unix stream socket: %s",
                                                         errtoa(errno, 0).str) ;
      return -1 ;
    }

  /* Bind to the required path                                          */
  memset (&sa_un, 0, sizeof(sa_un));
  sa_un.sun_family = AF_UNIX;
  strncpy (sa_un.sun_path, path, sizeof(sa_un.sun_path) - 1);

  sa_len = SUN_LEN(&sa_un) ;

#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
  sa_un.sun_len = sa_len ;
#endif

  old_mask = umask (0007);

  ret = bind (sock, (struct sockaddr *) &sa_un, sa_len) ;
  if (ret < 0)
    uzlog(NULL, LOG_ERR, "Cannot bind path %s: %s", path, errtoa(errno, 0).str);

  if (ret >= 0)
    ret = set_nonblocking(sock);

  if (ret >= 0)
    {
      ret = listen (sock, 5);
      if (ret < 0)
        uzlog(NULL, LOG_ERR, "listen(fd %d) failed: %s", sock,
                                                         errtoa(errno, 0).str) ;
    } ;

  zprivs_get_ids(&ids);

  if (ids.gid_vty > 0)
    {
      /* set group of socket */
      if ( chown (path, -1, ids.gid_vty) )
        uzlog (NULL, LOG_ERR, "uty_serv_vtysh: could chown socket, %s",
                                                         errtoa(errno, 0).str) ;
    }

  umask (old_mask);

  /* Give up now if failed along the way                                */
  if (ret < 0)
    {
      close (sock) ;
      return -1 ;
    } ;

  /* Socket is open -- set VTY Term listener going                      */
  uty_serv_start_listener(sock, VTY_SHELL_SERV) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Socket is open -- set a VTY listener going
 *
 * Note that the vyt_listener structure is passed to the accept action function.
 */
static void
uty_serv_start_listener(int fd, enum vty_type type)
{
  vty_listener listener ;

  listener = XCALLOC(MTYPE_VTY, sizeof (struct vty_listener));

  ssl_push(vty_listeners_list, listener, next) ;
  uty_sock_init_new(&listener->sock, fd, listener) ;

  listener->type = type ;

  if (vty_cli_nexus)
    listener->sock.action.read.qnexus = vty_accept_qnexus ;
  else
    listener->sock.action.read.thread = vty_accept_thread ;

  uty_sock_set_read(&listener->sock, on) ;
} ;

/*------------------------------------------------------------------------------
 * Accept action for the thread world -- create and dispatch VTY
 */
static int
vty_accept_thread(struct thread *thread)
{
  vty_listener listener = THREAD_ARG(thread) ;
  int result ;

  VTY_LOCK() ;

  result = uty_accept(listener, THREAD_FD(thread));

  uty_sock_set_read(&listener->sock, on) ;

  VTY_UNLOCK() ;
  return result ;
} ;

/*------------------------------------------------------------------------------
 * Accept action for the qnexus world -- create and dispatch VTY
 */
static void
vty_accept_qnexus(qps_file qf, void* listener)
{
  VTY_LOCK() ;

  uty_accept(listener, qf->fd);

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_TERM or VTY_SHELL_SERV
 */
static int
uty_accept(vty_listener listener, int listen_sock)
{
  VTY_ASSERT_LOCKED() ;

  assert(listener->sock.fd == listen_sock) ;

  switch (listener->type)
  {
    case VTY_TERM:
      return uty_accept_term(listener) ;

    case VTY_SHELL_SERV:
      return uty_accept_shell_serv(listener) ;

    default:
      zabort("unknown vty type") ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_TERM
 */
static int
uty_accept_term(vty_listener listener)
{
  int sock_fd;
  union sockunion su;
  int ret;
  unsigned int on;
  struct prefix *p ;

  VTY_ASSERT_LOCKED() ;

  /* We can handle IPv4 or IPv6 socket.                                 */
  sockunion_init_new(&su, AF_UNSPEC) ;

  sock_fd = sockunion_accept (listener->sock.fd, &su);

  if (sock_fd < 0)
    {
      if (sock_fd == -1)
        uzlog (NULL, LOG_WARNING, "can't accept vty socket : %s",
                                                         errtoa(errno, 0).str) ;
      return -1;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock_fd) ;     /* issues WARNING if fails      */
  if (ret < 0)
    {
      close(sock_fd) ;
      return -1 ;
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
      return 0;
    } ;

  /* Final options (optional)                                           */
  on = 1 ;
  ret = setsockopt (sock_fd, IPPROTO_TCP, TCP_NODELAY,
                                                    (void*)&on, sizeof (on));
  if (ret < 0)
    uzlog (NULL, LOG_INFO, "can't set sockopt to socket %d: %s",
                                               sock_fd, errtoa(errno, 0).str) ;

  /* All set -- create the VTY_TERM                                     */
  uty_new_term(sock_fd, &su);

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)", sutoa(&su).str,
                                                                      sock_fd) ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_SHELL_SERV
 */
static int
uty_accept_shell_serv (vty_listener listener)
{
  int sock_fd ;
  int ret ;
  int client_len ;
  struct sockaddr_un client ;

  VTY_ASSERT_LOCKED() ;

  client_len = sizeof(client);
  memset (&client, 0, client_len);

  sock_fd = accept(listener->sock.fd, (struct sockaddr *) &client,
                                         (socklen_t *) &client_len) ;

  if (sock_fd < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't accept vty shell socket : %s",
                                                         errtoa(errno, 0).str) ;
      return -1;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock_fd) ;      /* issues WARNING if fails      */
  if (ret < 0)
    {
      close(sock_fd) ;
      return -1 ;
    } ;

  /* All set -- create the VTY_SHELL_SERV                               */
  if (VTYSH_DEBUG)
      printf ("VTY shell accept\n");

  uty_new_shell_serv(sock_fd) ;

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty shell connection (fd %d)", sock_fd);
  return 0;
}

/*==============================================================================
 * Reading from the VTY_SHELL_SERV type sock.
 *
 * The select/pselect call-back ends up in utysh_read_ready().
 */

/*------------------------------------------------------------------------------
 * Ready to read -> kicking the "SHELL_SERV CLI"
 *
 * End up here when there is something ready to be read.
 *
 * Will also end up here if an error has occurred, the other end has closed,
 * this end has half closed, etc.  This fact is used to kick the CLI even when
 * there is no data to be read.
 *
 * Note that nothing is actually read here -- reading is done in the CLI itself,
 * if required.
 *
 * The CLI decides whether to re-enable read, or enable write, or both.
 */
static void
utysh_read_ready(vty_io vio)
{
  uty_sock_set_read(&vio->sock, off) ;

  /* TODO: need minimal "CLI" for VTY_SHELL_SERV
   *       NB: when output from command is flushed out, must append the
   *           following four bytes: '\0' '\0' '\0' <ret>
   *           Where <ret> is the command return code.
   */
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to read -> kicking the "SHELL_SERV CLI"
 */
static void
vtysh_read_qnexus(qps_file qf, void* file_info)
{
  vty_io vio = file_info;

  VTY_LOCK() ;

  assert((vio->sock.fd == qf->fd) && (vio == vio->sock.info)) ;

  utysh_read_ready(vio) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Callback -- threads: ready to read -> kicking the "SHELL_SERV CLI"
 */
static int
vtysh_read_thread(struct thread *thread)
{
  vty_io vio = THREAD_ARG (thread);

  VTY_LOCK() ;

  assert(vio->sock.fd == THREAD_FD (thread) && (vio == vio->sock.info)) ;

  vio->sock.t_read = NULL ;     /* implicitly   */
  utysh_read_ready(vio);

  VTY_UNLOCK() ;
  return 0 ;
}

/*------------------------------------------------------------------------------
 * Read a lump of bytes and shovel into the command line buffer
 *
 * Lines coming in are terminated by '\0'.
 *
 * Assumes that the incoming command line is empty or otherwise incomplete.
 *
 * Moves stuff from the "buf" qstring and appends to "cl" qstring, stopping
 * when get '\0' or empties the "buf".
 *
 * When empties "buf", reads a lump from the sock.
 *
 * Returns:  0 => command line is incomplete
 *           1 => have a complete command line
 *          -1 => EOF (or not open, or failed)
 */
extern int
utysh_read (vty_io vio, qstring cl, qstring buf)
{
  int    get ;
  char*  cp ;
  char*  ep ;
  size_t have ;

  while (1)
    {
      /* process what there is in the buffer                            */
      if (buf->len > buf->cp)
        {
          cp   = qs_cp_char(buf) ;
          ep   = qs_ep_char(buf) ;
          have = ep - cp ;

          ep = memchr(cp, '\0', have) ;
          if (ep != NULL)
            have = ep - cp ;    /* have upto, but excluding '\0'        */

          if (have > 0)         /* take what have                       */
            {
              qs_insert(cl, cp, have) ;
              cl->cp  += have ;
              buf->cp += have ;
            } ;

          if (ep != NULL)       /* if found '\0'                        */
            {
              qs_term(cl) ;     /* '\0' terminate       */
              ++buf->cp ;       /* step past it         */
              return 1 ;        /* have a complete line   <<<<<<<<<<<<< */
            }
        } ;

      /* buffer is empty -- try and get some more stuff                 */
      assert(buf->len == buf->cp) ;

      if (!vio->sock.read_open)
        return -1 ;             /* at EOF if not open     <<<<<<<<<<<<< */

      qs_need(buf, 500) ;       /* need a reasonable lump               */
      qs_clear(buf) ;           /* set cp = len = 0                     */

      get = read_nb(vio->sock.fd, buf->body, buf->size) ;
      if      (get > 0)
        buf->len = get ;
      else if (get == 0)
        return 0 ;              /* have an incomplete line <<<<<<<<<<<< */
      else
        {
          if (get == -1)
            uty_sock_error(vio, "read") ;

          vio->sock.read_open = 0 ;

          return -1 ;           /* at EOF or failed       <<<<<<<<<<<<< */
        } ;
    } ;
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
              ret = write_nb(vio->sock.fd, ll->line, ll->len) ;

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
      write(vio->sock.fd, buf, len) ;
      write(vio->sock.fd, "\r\n", 2) ;

      vio = sdl_next(vio, mon_list) ;
    } ;
} ;

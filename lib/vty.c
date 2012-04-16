/* VTY external interface
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
#include "lib/version.h"

#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "vty.h"
#include "vty_local.h"
#include "vty_io.h"
#include "vty_io_file.h"
#include "vty_io_std.h"
#include "vty_command.h"
#include "vty_cli.h"
#include "vty_log.h"
#include "vio_fifo.h"
#include "log_local.h"
#include "vty_vtysh.h"

#include "list_util.h"

#include "command.h"
#include "command_local.h"
#include "command_execute.h"
#include "command_parse.h"
#include "memory.h"
#include "mqueue.h"
#include "qstring.h"
#include "qs_pcsub.h"
#include "qpath.h"
#include "network.h"

/*==============================================================================
 * The vty family comprises:
 *
 *   vty          -- level visible from outside the vty/command/log family
 *                   and within those families.
 *
 *   vty_common.h -- definitions used by both external and internal users
 *   vty_local.h  -- definitions used within the family only
 *
 *   vty_io       -- top level of the vio handling
 *
 *   vty_command  -- functions called by the command family
 *   vty_log      -- functions called by the log family
 *
 *   vty_cli      -- terminal command line handling
 *   vty_vtysh    -- vtysh client support
 *   vty_io_term  -- terminal (telnet) I/O
 *   vty_io_vtysh -- vtysh I/O
 *   vty_io_file  -- file and pipe I/O
 *   vty_io_std   -- stdout and stderr I/O
 *
 *   vty_io_basic -- common low level I/O handling
 *                   encapsulates the differences between qpselect and legacy
 *                   thread/select worlds.
 *
 *   vio_lines    -- for terminal: handles width, CRLF, line counting etc.
 *   vio_fifo     -- for all vty types, indefinite size buffer
 *   qiovec       -- using writev() to output contents of buffers
 */

/*==============================================================================
 * Variables etc. (see vty_local.h)
 */

/* The mutex and related debug counters
 */
qpt_mutex vty_mutex ;

int vty_lock_count  = 0 ;

#if VTY_DEBUG
int vty_assert_fail = 0 ;
#endif

/* In the qpthreads world, have nexus for the CLI and one for the Routeing
 * Engine.  Some commands are processed directly in the CLI, most have to
 * be sent to the Routeing Engine.
 *
 * If not in the qpthreads world, vty_cli_nexus == vty_cmd_nexus == NULL.
 *
 * If in the qpthreads world these vty_cli_nexus == vty_cmd_nexus if not
 * actually running pthreaded.
 */
bool vty_nexus ;                /* true <=> in the qpthreads world      */
bool vty_multi_nexus ;          /* true <=> more than one qpthread      */

qpn_nexus vty_cli_nexus     = NULL ;
qpn_nexus vty_cmd_nexus     = NULL ;

/* List of all known vio                                                */
vio_list_t vio_live_list    = INIT_DL_BASE_PAIR ;

/* List of all vty which are in monitor state.                          */
vio_list_t vio_monitor_list = INIT_DL_BASE_PAIR ;

/* List of child processes in our care                                  */
vio_child vio_childer_list  = NULL ;

/* See vty_child_signal_nexus_set()                                     */
qpt_mutex vty_child_signal_mutex = NULL ;
qpn_nexus vty_child_signal_nexus = NULL ;

/*------------------------------------------------------------------------------
 * VTYSH stuff
 */

/* Integrated configuration file path -- for VTYSH
 */
const char* integrate_default = SYSCONFDIR INTEGRATE_DEFAULT_CONFIG ;

/*
 *
 */
volatile sig_atomic_t vty_SIGINT = false ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void uty_cmd_init (void) ;
static void uty_init (void) ;
static void uty_reset (const char* why, bool curtains) ;

/*------------------------------------------------------------------------------
 * Tracking the initialisation state.
 */
enum vty_init_state
{
  vty_init_pending     = 0,     /* first and lowest numbered state      */
  vty_init_1st_stage,
  vty_init_2nd_stage,
  vty_init_started,
  vty_init_suspended,
  vty_init_stopped,
  vty_init_terminated           /* final and highest numbered state     */
};

static enum vty_init_state vty_init_state ;

static void (*vty_restart_action)(void) ;

/*==============================================================================
 * Public Interface
 */

static void uty_dispatch_action(qpn_nexus dst, void (*action)(void)) ;
static void uty_qpn_action(mqueue_block mqb, mqb_flag_t flag) ;
static int uty_thread_action(struct thread* thread) ;
static void vty_resume_action(void) ;

/*------------------------------------------------------------------------------
 * Install vty's own commands like `who' command
 *
 * Called by cmd_table_init() -- which must be called *before* vty_init() or
 * vty_init_for_vtysh().
 */
extern void
vty_cmd_init (void)
{
  assert(vty_init_state == vty_init_pending) ;

  uty_cmd_init() ;
} ;

/*------------------------------------------------------------------------------
 * Initialise vty handling (threads and pthreads) for all but vtysh
 *
 * This runs before any pthreads or nexus stuff starts -- so is, implicitly,
 * in the CLI thread and no locking is required (and would have no effect).
 *
 * NB: is called before the configuration file is read for the first time.
 *
 * NB: from this point forward can do vty things *provided* is *blocking*.
 *
 * NB: may be called once and once only.
 */
extern void
vty_init (void)
{
  uty_init () ;

  vty_init_state = vty_init_1st_stage ;
} ;

/*------------------------------------------------------------------------------
 * Initialise vty handling for vtysh
 *
 * Jumps directly to vty_init_started -- no further initialisation is required.
 *
 * Note that this means that the watch dog is not started and no listeners are
 * set up.
 *
 * NB: may be called once and once only.
 */
extern void
vty_init_for_vtysh(void)
{
  uty_init () ;

  vty_init_state = vty_init_started ;
} ;

/*------------------------------------------------------------------------------
 * Common initialisation for vty (threads and pthreads)
 *
 * This runs before any pthreads or nexus stuff starts -- so is, implicitly,
 * in the CLI thread.
 *
 * NB: may be called once and once only.
 */
static void
uty_init (void)
{
  qassert(!qpthreads_enabled) ;

  assert(vty_init_state == vty_init_pending) ;

  vty_mutex           = NULL ;

  ddl_init(vio_live_list) ;     /* no VTYs yet                          */
  vio_childer_list    = NULL ;

  vty_nexus           = false ; /* not running qnexus-wise              */
  vty_multi_nexus     = false ; /* not more than one thread either      */
  vty_cli_nexus       = NULL ;
  vty_cmd_nexus       = NULL ;

  vty_child_signal_mutex = NULL ;
  vty_child_signal_nexus = NULL ;       /* none, yet                    */

  uty_monitor_init() ;
} ;

/*------------------------------------------------------------------------------
 * Further initialisation for qpthreads.
 *
 * This is done during "second stage" initialisation, when all nexuses have
 * been set up and the qpthread_enabled state established.
 *
 * This is before any threads have been started, so is, implicitly, in the
 * CLI thread.
 *
 * Need to know where the CLI nexus and the Routeing Engine nexus are.
 *
 * Initialise mutex.
 *
 * Cannot lock or assert in CLI thread while initialising those things !
 *
 * NB: may be called once and once only.
 */
extern void
vty_init_r (qpn_nexus cli, qpn_nexus cmd)
{
  assert(vty_init_state == vty_init_1st_stage) ;

  vty_nexus       = true ;
  vty_multi_nexus = (cli != cmd) ;
  vty_cli_nexus   = cli ;
  vty_cmd_nexus   = cmd ;

  vty_mutex              = qpt_mutex_new(qpt_mutex_recursive, "VTY");
  vty_child_signal_mutex = qpt_mutex_new(qpt_mutex_quagga, "VTY Child Signal");

  vty_init_state = vty_init_2nd_stage ;
} ;

/*------------------------------------------------------------------------------
 * Start the VTY going for all daemons except the vtysh.
 *
 * This starts the listeners for VTY_TERMINAL and VTY_VTYSH_SERVER.
 *
 * Also starts the watch dog.
 *
 * This is run during early morning start, after any daemonisation, but before
 * any threads are started -- so is, implicitly, in the CLI thread.
 *
 * This is done *after* the configuration file is read for the first time.
 *
 * NB: may be called once and once only.
 */
extern void
vty_start(const char *addr, unsigned short port, const char *path)
{
  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  assert( (vty_init_state == vty_init_1st_stage)
       || (vty_init_state == vty_init_2nd_stage) ) ;

  uty_watch_dog_start() ;

  host.vty_listen_addr   = (addr != NULL) ? XSTRDUP(MTYPE_HOST, addr)
                                          : NULL ;
  host.vty_listen_port   = port ;
  host.vtysh_listen_path = (path != NULL) ? XSTRDUP(MTYPE_HOST, path)
                                          : NULL ;

  uty_open_listeners() ;

  vty_init_state = vty_init_started ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Suspend (if can, otherwise Stop) all known VTY, and turn off all listeners.
 * Dispatch given action for execution in the cmd nexus.
 *
 * Chief use is SIGHUP and other configuration reload mechanisms.
 *
 * For SIGHUP, procedure is:
 *
 *   SIGHUP handled by cli nexus:
 *
 *     invokes the handler set up by the daemon, which calls this to
 *     (a) suspend vty, (b) set "restart action" NULL, (c) dispatch the
 *     given "suspend action" for execution in the cmd_nexus.
 *
 *   The cmd nexus executes the "suspend action"
 *
 *     while the vty are being suspended, this function runs in the cmd nexus,
 *     doing whatever it needs to do to suspend itself.
 *
 *     When this is done, calls vty_dispatch_restart() to schedule the
 *     restart action, once all vty are suspended -- if they are already all
 *     suspended, dispatches the restart action immediately.
 *
 *   If vty are not all suspended
 *
 *     as each one becomes suspended or stops, will call uty_suspended() to see
 *     if is time to dispatch the "restart action".
 *
 *   The cmd nexus executes the "restart action"
 *
 *     which is likely to reload the configuration and may do other stuff.
 *
 *     At the end of the "restart action",
 *
 * This is called in the CLI thread.
 *
 * Returns:  string for logging.
 *
 * String returned is expected to be added to eg: "SIGHUP: %s to reload ..."
 */
extern const char*
vty_suspend(const char* why, void (*suspend_action)(void))
{
  const char* result ;
  vty_io next ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  switch (vty_init_state)
    {
      case vty_init_pending:
      case vty_init_1st_stage:
      case vty_init_2nd_stage:
        result = "too early" ;
        break ;

      case vty_init_started:
        uty_close_listeners() ;

        next = ddl_head(vio_live_list) ;
        while (next != NULL)
          {
            vty_io vio ;

            vio  = next ;
            next = sdl_next(vio, vio_list) ;

            uty_cmd_loop_suspend(vio, why) ;
          } ;

        vty_restart_action = NULL ;           /* not set, yet */
        vty_init_state = vty_init_suspended ;

        uty_dispatch_action(vty_cmd_nexus, suspend_action) ;

        result = "about" ;
        break ;

      case vty_init_suspended:
        result = "already started" ;
        break ;

      case vty_init_stopped:
      case vty_init_terminated:
        result = "too late" ;
        break ;

      default:
        result = "UNABLE (BUG)" ;
        break ;
    } ;

  VTY_UNLOCK() ;

  return result ;
} ;

/*------------------------------------------------------------------------------
 * Set the action to restart once all vty are suspended.
 *
 * Does nothing if not vty_init_suspended -- so if has been stopped in the
 * meantime, has no effect.
 *
 * If all vty are already suspended, will dispatch the restart action for
 * execution in the cmd nexus, now.  Otherwise, leaves pending, for when all
 * vty are suspended.
 *
 * Can be called in any thread.
 */
extern void
vty_dispatch_restart(void (*action)(void))
{
  VTY_LOCK() ;

  if (vty_init_state == vty_init_suspended)
    {
      vty_restart_action = action ;
      uty_suspended_scan() ;
    } ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Check whether all vty are now suspended, and if so, dispatch the restart
 * action if there is one and clear it.
 *
 * Does nothing if not vty_init_suspended -- so if has been stopped in the
 * meantime, has no effect.
 *
 * Can be called in any thread.
 */
extern void
uty_suspended_scan(void)
{
  bool   dispatch ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  if (vty_init_state != vty_init_suspended)
    return ;

  dispatch = (vty_restart_action != NULL) ;   /* dispatch if have action */

  vio = ddl_head(vio_live_list) ;
  while ((vio != NULL) && dispatch)
    {
      dispatch = (vio->state & vst_suspended) ;
      vio = sdl_next(vio, vio_list) ;
    } ;

  if (dispatch)
    {
      /* clear the action flag and ...*/
      uty_dispatch_action(vty_cmd_nexus, vty_restart_action) ;
      vty_restart_action = NULL ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Send message to the relevant nexus to execute the given action function.
 *
 * If running nexus-wise, enqueue a one time message.
 * If running legacy threads use an event.
 *
 * Can be called in any thread.
 */
static void
uty_dispatch_action(qpn_nexus dst, void (*action)(void))
{
  qassert((dst == vty_cli_nexus) || (dst == vty_cmd_nexus)) ;

  if (vty_nexus)
    {
      mqueue_block mqb ;

      mqb = mqb_init_new(NULL, uty_qpn_action, action) ;
      mqueue_enqueue(dst->queue, mqb, mqb_priority) ;
    }
  else
    {
      thread_add_event(master, uty_thread_action, action, 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Execute action -- free the mqb.
 */
static void
uty_qpn_action(mqueue_block mqb, mqb_flag_t flag)
{
  void (*action)(void) ;

  assert(vty_nexus) ;                   /* must be running qnexus-wise  */

  action = mqb_get_arg0(mqb);

  mqb_free(mqb) ;

  if (flag == mqb_action)
    action() ;
} ;

/*------------------------------------------------------------------------------
 * Execute action -- in the legacy threads world.
 */
static int
uty_thread_action(struct thread* thread)
{
  void (*action)(void) ;

  action = THREAD_ARG(thread) ;
  action() ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Suspended vty may now run again.
 *
 * Does nothing if not vty_init_suspended -- so if has been stopped in the
 * meantime, has no effect.
 *
 * This can be called in any thread (but probably called in the Routing Engine
 * thread) -- sends message to the CLI thread to do the work.
 */
extern void
vty_resume(void)
{
  VTY_LOCK() ;

  if (vty_init_state == vty_init_suspended)
    {
      uty_dispatch_action(vty_cli_nexus, vty_resume_action) ;

      vty_init_state = vty_init_started ;
    } ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Suspended vty may now run again.
 *
 * This is run in the CLI thread.
 */
static void
vty_resume_action(void)
{
  vty_io next ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  next = ddl_head(vio_live_list) ;
  while (next != NULL)
    {
      vty_io vio ;

      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      uty_cmd_loop_resume(vio) ;
    } ;

  uty_open_listeners() ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Stop all known VTY, and turn off all listeners.
 *
 * Sole use is SIGINT/SIGTERM and other close-down mechanisms.
 *
 * This is called in the CLI thread.
 *
 * The expectation is that this is called as early as possible in the shut down
 * process, while message queues and events are still being processed.
 *
 * The next operation should be vty_terminate().
 */
extern void
vty_stop(const char* why, void (*stop_action)(void))
{
  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  switch (vty_init_state)
    {
      case vty_init_pending:
        break ;

      case vty_init_1st_stage:
      case vty_init_2nd_stage:
      case vty_init_started:
      case vty_init_suspended:
        uty_reset(why, false) ;
        vty_init_state = vty_init_stopped ;
        uty_dispatch_action(vty_cmd_nexus, stop_action) ;
        break ;

      case vty_init_stopped:
      case vty_init_terminated:
        break ;

      default:
        break ;
    } ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * System shut-down
 *
 * In the pthreads world, all threads other than the main (CLI) thread have
 * been joined -- so this is, implicitly, in the CLI thread.
 *
 * Close all known vty and release all memory -- discard all pending output.
 *
 * NB: this may be done in any initialisation state.
 *
 * Note that all the locking stuff does nothing if not qpthreads_enabled, so
 * these may be done in any state of initialisation.  (It is assumed that the
 * switch into qpthreads_enabled is an atomic action... so all second stage
 * initialisation completes together.)
 */
extern void
vty_terminate (void)
{
  if ( (vty_init_state != vty_init_pending) &&
       (vty_init_state != vty_init_terminated)  )
    {
      VTY_ASSERT_CLI_THREAD() ;
      VTY_LOCK() ;

      qassert(!qpthreads_active) ;

      uty_reset("Terminating", true) ;

      VTY_UNLOCK() ;

      uty_watch_dog_stop() ;
      vty_child_close_register() ;

      vty_mutex              = qpt_mutex_destroy(vty_mutex);
      vty_child_signal_mutex = qpt_mutex_destroy(vty_child_signal_mutex);
    } ;

  vty_init_state = vty_init_terminated ;
} ;

/*------------------------------------------------------------------------------
 * Reset -- for SIGTERM/SIGINT or at final curtain -- vty_terminate.
 *
 * Closes listeners and then stops all command loops, and closes vty if can
 * or if must (at curtains).
 *
 * If not curtains, the command loop may continue, in vio->stop state, as
 * it pushes out any pending stuff.  This will hang when message/thread
 * processing stops.  When vty_terminate() is called, will again attempt to
 * stop the command loop, but this time will force the issue and close the
 * vty.
 */
static void
uty_reset(const char* why, bool curtains)
{
  vty_io next ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  uty_close_listeners() ;

  next = ddl_head(vio_live_list) ;
  while (next != NULL)
    {
      vty_io vio ;

      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      uty_cmd_loop_stop(vio, why, curtains) ;
    } ;
} ;

/*==============================================================================
 * General VTY output.
 *
 * This is used during command execution, to output the results of commands.
 */

/*------------------------------------------------------------------------------
 * VTY output -- cf fprintf(stdout, ...) !
 *
 * This is for command output, which may later be suppressed
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
vty_out(struct vty *vty, const char *format, ...)
{
  int     ret ;
  va_list args ;

  VTY_LOCK() ;

  va_start (args, format) ;
  ret = vio_fifo_vprintf(vty->vio->obuf, format, args) ;
  va_end (args) ;

  VTY_UNLOCK() ;
  return ret ;
}

/*------------------------------------------------------------------------------
 * VTY error output -- cf fprintf(stderr, ...) !
 *
 * This is for (serious) error output, where that may be early in the morning
 * or at some other time where there is no terminal to send output to.
 *
 * If vout is VOUT_STDOUT, sends output to "stderr".
 *
 * Otherwise, sends output to the vty, in the usual way !
 */
extern void
vty_err(struct vty *vty, const char *format, ...)
{
  va_list args ;

  VTY_LOCK() ;

  va_start (args, format) ;

  if (vty->vio->vout->vout_type == VOUT_STDOUT)
    uty_std_err_vprintf(format, args) ;
  else
    vio_fifo_vprintf(vty->vio->obuf, format, args) ;

  va_end (args) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * VTY output -- cf write
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
vty_write(struct vty *vty, const void* buf, int n)
{
  VTY_LOCK() ;

  vio_fifo_put_bytes(vty->vio->obuf, buf, n) ;

  VTY_UNLOCK() ;
  return 0 ;
}

/*------------------------------------------------------------------------------
 * VTY output -- output a given numnber of spaces
 *
 * This is for command output, which may be suppressed
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */

/*                                         1         2         3         4 */
/*                                1234567890123456789012345678901234567890 */
const char vty_spaces_string[] = "                                        " ;
CONFIRM(VTY_MAX_SPACES == (sizeof(vty_spaces_string) - 1)) ;

extern int
vty_out_indent(struct vty *vty, int indent)
{
  int     ret ;

  ret = 0 ;
  while ((indent > 0) && (ret >= 0))
    {
      ret = vty_out(vty, VTY_SPACES(indent)) ;
      indent -= VTY_MAX_SPACES ;
    }

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Say hello to vty interface.
 */
extern void
vty_hello (struct vty *vty)
{
  qpath       path ;
  qstring     motd ;

  /* For debug, immediately output the debug banner.
   */
  if (qdebug)
    vty_out (vty, debug_banner, host.full_progname, "\n") ;

  /* Collect path of any motdfile and take copy of any motd string.
   *
   * Require VTY_LOCK() to access host structure, and we take copies of the
   * data accessed.
   *
   * If there is no motdfile, then will use the motd string.  If there is no
   * motd string, will output nothing at all.
   */
  VTY_LOCK() ;

  path = (host.motdfile != NULL) ? qpath_dup(host.motdfile) : NULL ;
  motd = qs_set_str(NULL, host.motd) ;

  VTY_UNLOCK() ;

  /* If there is a motd file, read its contents into the motd qstring.
   *
   * The primitive line imager makes sure there are no control characters
   * other than '\n'.
   */
  if (path != NULL)
    {
      vty_line_image  vli ;

      vli = uty_fd_line_image_open(qpath_string(path)) ;

      if (vli->fd >= 0)
        {
          qs_set_len_nn(motd, 0) ;

          while (1)
            {
              int r ;

              r = uty_fd_line_image_read(vli) ;

              if (r <= 0)
                break ;

              qs_append_n(motd, vli->buffer, r) ;
            } ;
        } ;

      if (vli->fd < 0)
        {
          vty_out(vty, "%% Failed while %s motd file '%s': %s\n", vli->when,
                                           vli->name, errtoa(vli->err, 0).str) ;
          qs_set_len_nn(motd, 0) ;
        } ;

      uty_fd_line_image_close(vli) ;
    } ;

  /* If the motd string is not empty then process and output same.
   *
   * Processing will substitute:
   *
   *   %% -> %
   *
   *   %P -> QUAGGA_PROGNAME
   *   %C -> QUAGGA_COPYRIGHT
   *   %V -> QUAGGA_VERSION
   *   %D -> name of daemon
   *
   *   %d -> __DATE__
   *   %t -> __TIME__
   *
   *   %q -> if QDEBUG: " QDEBUG/QDEBUG_NAME"
   *              else: ""
   *
   * And will strip any trailing spaces and ensure result ends in '\n'.
   */
  if (qs_len_nn(motd) != 0)
    {
      char* p, * e ;

      static qs_pcsub_table_t table = {
          { .action = 'P', .parser = qs_pcsub_default, .extra = 0 },
          { .action = 'C', .parser = qs_pcsub_default, .extra = 1 },
          { .action = 'V', .parser = qs_pcsub_default, .extra = 2 },
          { .action = 'D', .parser = qs_pcsub_default, .extra = 3 },
          { .action = 'd', .parser = qs_pcsub_default, .extra = 4 },
          { .action = 't', .parser = qs_pcsub_default, .extra = 5 },
          { .action = 'q', .parser = qs_pcsub_default, .extra = 6 },

          { .action = '%' }
      } ;

      qs_pcsub(motd, table, QUAGGA_PROGNAME,            /* %P =>  0 */
                            QUAGGA_COPYRIGHT,           /* %C =>  1 */
                            QUAGGA_VERSION,             /* %V =>  2 */
                            host.progname,              /* %D =>  3 */

                            __DATE__,                   /* %d =>  4 */
                            __TIME__,                   /* %t =>  5 */
#if QDEBUG
                            " QDEBUG/" QDEBUG_NAME,     /* %q =>  6 */
#else
                            "",
#endif
                            NULL) ;

      /* Final scan, replacing any control characters other than '\n' by ' ',
       * just in case the motd string or one of the substitutions contains
       * such rubbish.
       */
      p = qs_char_nn(motd) ;
      e = qs_ep_char_nn(motd) ;

      while (p < e)
        {
          byte b ;

          b = *p ;
          if ((b < ' ') && (b != '\n'))
            *p = ' ' ;

          ++p ;
        } ;

      /* Trim any trailing spaces and ensure all ends in a '\n'
       */
      p = qs_char_nn(motd) ;
      while ((e > p) && (*(e - 1) == ' '))
        --e ;

      qs_set_len_nn(motd, e - p) ;

      if ((e == p) || (*(e - 1) != '\n'))
        qs_append_n(motd, "\n", 1) ;

      /* Now we write the qstring to the vty buffer.
       */
      vty_write(vty, qs_char_nn(motd), qs_len_nn(motd)) ;
    } ;

  /* Tidy up and fin
   */
  qs_free(motd) ;
  qpath_free(path) ;
} ;

/*------------------------------------------------------------------------------
 * "cat" file to vty
 */
extern cmd_ret_t
vty_cat_file(vty vty, qpath path, const char* desc)
{
  cmd_ret_t       ret ;
  vty_line_image  vli ;

  ret = CMD_SUCCESS ;

  vli = uty_fd_line_image_open(qpath_string(path)) ;

  while ((vli->fd >= 0) && (ret == CMD_SUCCESS))
    {
      int r ;

      r = uty_fd_line_image_read(vli) ;

      if (r <= 0)
        break ;

      vty_write(vty, vli->buffer, r) ;
      ret = vty_cmd_out_push(vty) ;
    } ;

  if (vli->fd < 0)
    {
      vty_out(vty, "%% Failed while %s %s '%s': %s\n", vli->when,
                                     desc, vli->name, errtoa(vli->err, 0).str) ;
      ret = CMD_WARNING ;
    } ;

  uty_fd_line_image_close(vli) ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Clear the contents of the command output FIFO etc.
 */
extern void
vty_out_clear(vty vty)
{
  VTY_LOCK() ;
  uty_out_clear(vty->vio) ;
  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Support for vtysh integrated configuration.
 *
 * Each node that generates configuration can break that up into groups of
 * configuration lines, and those groups may themselves be broken into
 * groups.
 *
 * Each group has a name, whose scope is the current node or current group
 * (if nested).
 *
 * Where more than one daemon generates configuration for the same group (by
 * name) the groups will be merged.
 */

/*------------------------------------------------------------------------------
 * If is vty->config_to_vtysh put a #vtysh-config-group xxxxx to the vty.
 *
 * The string constructed from the format and arguments is the name of the
 * group.  Case is significant in the name, but leading and trailing spaces
 * are not, and multiple spaces are compressed to one space.
 */
extern void
vty_out_vtysh_config_group (struct vty *vty, const char *format, ...)
{
  if (vty->config_to_vtysh)
    {
      va_list args ;

      VTY_LOCK() ;

      vio_fifo_put_string(vty->vio->obuf, "#vtysh-config-group ") ;

      va_start (args, format) ;
      vio_fifo_vprintf(vty->vio->obuf, format, args) ;
      va_end (args) ;

      vio_fifo_put_string(vty->vio->obuf, "\n") ;

      VTY_UNLOCK() ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * If is vty->config_to_vtysh put a #vtysh-config-group-end to the vty.
*/
extern void
vty_out_vtysh_config_group_end (struct vty *vty)
{
  if (vty->config_to_vtysh)
    vty_out(vty, "#vtysh-config-group-end\n") ;
} ;

/*==============================================================================
 * Functions for VTY_GET_LONG
 *               VTY_GET_INTEGER_RANGE
 */

/*------------------------------------------------------------------------------
 * Decode string using stroul().
 *
 * Accepts everything which stroul(s, ..., 0) will accept.  Note that this
 * includes values which are -ve (start with '-') and values which have an
 * explicit '+'.  Negative values are cast to unsigned on return from strtoul.
 *
 * Rejects strings which have anything after the last digit.
 *
 * Rejects empty strings.
 *
 * If fails, puts error message to the vty.
 *
 * Returns:  if OK: value returned by strtoul() and errno == 0
 *            else: ULONG_MAX                   and errno == EINVAL
 */
extern ulong
vty_get_long(vty vty, const char* name, const char* str)
{
  ulong result ;
  char *endptr = NULL;

  errno = 0 ;
  result = strtoul (str, &endptr, 0) ;

  if ((*endptr == '\0') && (endptr != str) && (errno == 0))
    return result ;

  vty_out (vty, "%% Invalid %s value\n", name) ;
  errno = EINVAL ;

  return ULONG_MAX ;
} ;

/*------------------------------------------------------------------------------
 * Decode string using stroul() and check for value in the given *unsigned*
 * range.
 *
 * Accepts everything which stroul(s, ..., 0) will accept.  Note that this
 * includes values which are -ve (start with '-') and values which have an
 * explicit '+'.  Negative values are cast to unsigned on return from strtoul.
 *
 * Rejects strings which have anything after the last digit.
 *
 * Rejects empty strings.
 *
 * Rejects values outside the given unsigned range.
 *
 * If fails, puts error message to the vty, and sets errno == EINVAL or ERANGE.
 *
 * Returns:  if OK: value returned by strtoul() and errno == 0
 *            else: ULONG_MAX                   and errno == EINVAL
 */
extern ulong
vty_get_integer_range(vty vty, const char* name, const char* str,
                                                           ulong min, ulong max)
{
  ulong result ;

  result = vty_get_long(vty, name, str) ;

  if ((errno == 0) && (result >= min) && (result <= max))
    return result ;

  if (errno == 0)
    {
      vty_out (vty, "%% Out of range %s value: %lu ", name, result) ;
      if (result < min)
        vty_out(vty, "< minimum %lu\n", min) ;
      else
        vty_out(vty, "> maximum %lu\n", max) ;
      errno = ERANGE ;
    } ;

  return max ;
} ;

/*==============================================================================
 * Deal with SIGCHLD "event".
 *
 * NB: if there is a nexus to be signalled, we do that *before* attempting to
 *     lock the VTY -- because in that case the VTY will be locked by that
 *     nexus !
 */
extern void
vty_sigchld(void)
{
  vty_child_signal_nexus_signal() ;

  VTY_LOCK() ;
  uty_sigchld() ;
  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Reading of configuration file
 *
 * The reading of the configuration file occurs at two times:
 *
 *   1. early in the morning, before daemonisation, and before any threads
 *      or nexuses have been set up.
 *
 *      In the qpthreads world, this means that it is running in the main (CLI)
 *      and only thread and nexus.
 *
 *   2. at SIGHUP time.
 *
 *      In the qpthreads world, this is running in whatever thread is executing
 *      commands.
 *
 * Sets up a VTY_STDOUT onto which a VIN_CONFIG is pushed, from which to
 * execute commands.  All commands are run directly in the thread -- no
 * commands are queued.
 */
static bool vty_read_config_file (vty vty, int conf_fd, qpath path,
                                                          bool ignore_warnings,
                                                          bool show_warnings) ;
static int vty_report_open_config_failed(vty vty, qpath conf, bool required,
                                                                  bool backup) ;

/*------------------------------------------------------------------------------
 * Read the given configuration file.
 *
 * If the config_file == NULL (ie no -f xxxx), use the config_default.
 */
extern void
vty_read_config (const char *config_file,
                 const char *config_default)
{
  bool own_flag ;

  if (config_file == NULL)
    {
      config_file = config_default ;
      own_flag = false ;
    }
  else
    own_flag   = true ;

  cmd_host_config_set (config_file, own_flag, NULL, false, false) ;
  cmd_host_pthreaded(false, NULL) ;

  vty_read_config_new(true /* ignore_warnings */, false /* show_warnings */) ;
}

/*------------------------------------------------------------------------------
 * Read the configuration file -- unless depending on vtysh.
 *
 * In any case: if is host.newborn will invoke host.init_second_stage().
 */
extern void
vty_read_config_new(bool ignore_warnings, bool show_warnings)
{
  qpath path ;

  VTY_LOCK() ;

  if (host.config_file != NULL)
    path = qpath_dup(host.config_file) ;
  else
    path = NULL ;               /* depending on vtysh ! */

  /* XXX worry about configuration default state... for SIGHUP reload.
   */
  host.vty_timeout_val = VTY_TIMEOUT_DEFAULT;

  VTY_UNLOCK() ;

  if (path != NULL)
    {
      vty   vty ;
      int   conf_fd ;
      bool  ok ;

      vty = vty_std_write_open("config file") ;

      conf_fd = vty_open_config_file(vty, path, true /* required */) ;

      ok = (conf_fd >= 0) ;

      if (ok)
        {
          if (qdebug)
            fprintf(stderr , "Reading config file: %s\n", qpath_string(path)) ;

          /* Read the configuration file in the configuration command loop,
           * closing and discarding vty when finished.
           */
          ok = vty_read_config_file(vty, conf_fd, path, ignore_warnings,
                                                        show_warnings) ;
          if (qdebug)
            fprintf(stderr, "Finished reading config file\n") ;
        } ;

      vty_close(vty) ;          /* the vty is released  */
      qpath_free(path) ;

      if (!ok)
        exit(1) ;
    } ;

  /* Finished the configuration step.  If not yet done the second stage
   * initialisation, better do it now.
   */
  cmd_init_second_stage() ;
} ;

/*------------------------------------------------------------------------------
 * Try to open given configuration file.
 *
 * If fails to open main config file, and file does not exist, try to open the
 * back-up version and restore the file.
 *
 * Issues error messages to the given vty by vty_err().
 *
 * Returns: >=  0 => OK, this is the fd
 *          == -1 => failed (hard)
 *          == -2 => not found && not required (soft)
 */
extern int
vty_open_config_file(vty vty, qpath path, bool required)
{
  qpath  saved, temp ;
  int    conf_fd, sav_fd, tmp_fd ;
  bool   restored ;

  saved     = NULL ;            /* none, yet            */
  temp      = NULL ;            /* none, yet            */

  restored  = false ;           /* not attempted, yet   */
  conf_fd   = -1 ;
  sav_fd    = tmp_fd = -1 ;     /* none, yet            */

  /* try to open the configuration file                                 */
  while (1)
    {
      bool ok ;
      int  err ;

      conf_fd = uty_fd_file_open(qpath_string(path), vfd_io_read, 0) ;

      if (conf_fd >= 0)
        break ;                 /* success !            */

      /* Failed to open the configuration file.
       *
       * Say what happened.
       *
       * If the configuration file is not required, exit now, with whatever
       * the error is -- will not attempt to restore from backup.
       *
       * If received a hard error -- ie this is not simply "not found", exit
       * with the hard error.
       *
       * If have already restored the configuration file, but could not then
       * find it... treat as hard error !
       */
      conf_fd = vty_report_open_config_failed(vty, path, required, false) ;

      if (!required)
        break ;                 /* give up in any case if !required     */

      if (conf_fd == -1)
        break ;                 /* give up on hard error                */

      conf_fd = -1 ;            /* all errors from now on are hard      */

      if (restored)
        break ;                 /* give up if failed to find restored   */

      /* Now set about restoring the backup configuration file, if we can
       * find one.
       *
       * Note that we open the file genuinely O/S blocking, so that can copy
       * simple mindedly.
       */
      saved = qpath_dup(path) ;
      qpath_extend_str(saved, CONF_BACKUP_EXT) ;

      sav_fd = uty_fd_file_open(qpath_string(saved),
                                          vfd_io_read | vfd_io_os_blocking, 0) ;
      if (sav_fd < 0)
        {
          vty_report_open_config_failed(vty, saved, true, true) ;
          break ;
        } ;

      /* construct a temporary file and copy saved configuration to it.
       */
      temp = qpath_dup(path) ;
      tmp_fd = qpath_mkstemp(temp) ;    /* opened blocking              */

      if (tmp_fd < 0)
        {
          err = errno ;
          vty_err(vty, "%% Failed to create temporary file %s: %s\n",
                                       qpath_string(temp), errtoa(err, 0).str) ;
          break ;
        } ;

      /* Copy the saved configuration to the new temporary file
       */
      if (copyn(tmp_fd, sav_fd) != 0)   /* TODO -- non-blocking ??      */
        {
          err = errno ;
          vty_err(vty, "%% Failed to copy Backup configuration file %s: %s\n",
                                      qpath_string(saved), errtoa(err, 0).str) ;
          break ;
        } ;

      /* Now set permissions and restore name
       */
      ok = chmod(qpath_string(temp), CONFIGFILE_MASK) == 0 ;

      if (ok)
        ok = link(qpath_string(temp), qpath_string(path)) == 0 ;

      if (!ok)
        {
          err = errno ;
          vty_err(vty, "%% Failed to restore configuration file %s: %s\n",
                                       qpath_string(path), errtoa(err, 0).str) ;
          break ;
        } ;

      /* Wonderful -- have restored backup configuration file, which will
       * now attempt to open.
       */
      vty_err(vty, "WARNING: using backup configuration file!\n");

      restored = true ;
    } ;

  /* Tidy up and return the result.
   */
  if (sav_fd >= 0)
    close(sav_fd) ;

  if (tmp_fd >= 0)
    {
      close(tmp_fd) ;
      unlink(qpath_string(temp)) ;
    } ;

  qpath_free(saved) ;
  qpath_free(temp) ;

  return conf_fd ;
} ;

/*------------------------------------------------------------------------------
 * Failed to open the given configuration file
 *
 * Establish whether file exists, and report -- vty_err() -- accordingly.
 *
 * Report error if file or something else of the same name exists.
 *
 * If file does not exist, report not found if the file is required.
 *
 * Return:  -1 => hard error (file exists)
 *          -2 => file not found
 */
static int
vty_report_open_config_failed(vty vty, qpath conf, bool required, bool backup)
{
  int  ret ;
  int  err ;

  err = errno ;
  ret = (qpath_stat_is_file(conf) != ENOENT) ? -1 : -2 ;

  if (ret == -1)
    /* Exists and failed to open
     */
    vty_err(vty, "%% Failed to open %sconfiguration file %s: %s\n",
              backup ? "backup " : "", qpath_string(conf), errtoa(err, 0).str) ;
  else if (required)
    /* Does not exist, and is required
     */
    vty_err(vty, "%% %sonfiguration file %s: not found\n",
                                backup ? "Backup c" : "C", qpath_string(conf)) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Read the given configuration file -- using the config loop.
 *
 * Expects the vty to be vio->blocking, with the
 *
 * May have a function to call after the first actual command is processed.
 * This mechanism supports the setting of qpthreads-ness by configuration file
 * command.
 *
 * In the qpthreads world:
 *
 *   * when the configuration is first read, this runs in the CLI thread
 *     (the main and only thread).
 *
 *   * when the configuration is reread, this runs in the command processor
 *     thread.
 *
 *     All consoles are shut down, so there can be no interference from that
 *     quarter.
 *
 * so all commands are executed directly.
 *
 * Returns:  true <=> OK
 */
static bool
vty_read_config_file(vty vty, int conf_fd, qpath path, bool ignore_warnings,
                                                       bool show_warnings)
{
  cmd_ret_t   ret ;
  qtime_t     taking ;
  const char* name ;

  name = qpath_string(path) ;

  if (vty_cmd_config_loop_prepare(vty, conf_fd, path, ignore_warnings,
                                                      show_warnings))
    {
      bool ignore ;
      uint warnings ;

      /* Do the work.
       */
      taking = qt_get_monotonic() ;

      ret = cmd_read_config(vty, &warnings) ;

      taking = (qt_get_monotonic() - taking) / (QTIME_SECOND / 1000) ;

      /* Worry about the result
       *
       * By the time the configuration has been read, should have set the
       * logging going, so should be able to get any warnings etc away.
       */
      ignore = (ret == CMD_WARNING) && ignore_warnings ;

      if      (ret == CMD_SUCCESS)
        zlog_info("Finished reading configuration '%s' in %d.%d secs -- OK",
                             name, (int)(taking / 1000), (int)(taking % 1000)) ;
      else if (ret == CMD_WARNING)
        zlog_warn("Finished reading configuration '%s' -- %d WARNINGS -- %s",
                                 name, warnings, ignore ? "ignored" : "FATAL") ;
      else
        zlog_err("Finished reading configuration '%s' -- FAILED (%d warnings)",
                                                               name, warnings) ;
      if (ignore)
        ret = CMD_SUCCESS ;
    }
  else
    {
      /* This should be impossible -- before reading the configuration,
       * all other vty are closed.
       */
      zlog_err("%s: Failed to gain config capability !", __func__) ;
      ret = CMD_ERROR ;
    } ;

  return (ret == CMD_SUCCESS) ;
} ;

/*==============================================================================
 * Writing of configuration to configuration file
 *
 * Is passed a call-back which outputs a given node's configuration to the
 * vty -- returning false if the node has no configuration (or whose
 * configuration is empty).
 *
 * Within a daemon only one write configuration command can run at any
 * moment.  Uses a well known temporary output file name, and insists that it
 * does not exist and cand be opened exclusively, which ensures only one
 * process can be writing configuration at the same time.  It also ensures that
 * if something goes wrong, the operator tidies up before we risk losing any
 * good configuration file(s).
 *
 * TODO: what sense does it make to have an output pipe with this command !!??
 *
 * Returns:  CMD_SUCCESS    -- fine
 *           CMD_ERROR      -- something failed, error reported
 *           CMD_IO_ERROR   -- some I/O error occurred, error posted
 *           CMD_CANCEL     -- cancel picked up
 */
extern cmd_ret_t
vty_write_config_file(vty vty, qpath conf,
                        int (*write_config_node)(svty vty, node_type_t node),
                                                                bool integrated)
{
  qpath       temp ;
  qpath       save ;
  cmd_ret_t   ret, retw ;
  int         fd, err ;
  const char* conf_name ;
  const char* temp_name ;
  const char* save_name ;
  const char* tag ;

  tag = integrated ? "integrated " : "" ;

  err = 0 ;                     /* so far, so good      */
  ret = CMD_ERROR ;             /* but fear the worst   */

  /* Set up the file names
   */
  temp      = qpath_dup(conf) ;
  qpath_extend_str(temp, CONF_TEMP_EXT) ;

  save      = qpath_dup(conf) ;
  qpath_extend_str(save, CONF_BACKUP_EXT) ;

  conf_name = qpath_string(conf) ;
  temp_name = qpath_string(temp) ;
  save_name = qpath_string(save) ;

  /* Open the configuration temporary file for write -- will create, but
   * it must not exist.
   *
   * If it does exist, it likely that some previous configuration write
   * operation failed (at a late stage, during renames, which is unexpected),
   * and it is best that the (human) operator makes sure that any mess is
   * cleared up before proceeding.
   *
   * Note that being able to open the well known temporary file in this way,
   * we are guaranteeing that only one attempt to write configuration can
   * be active at any time.
   *
   * NB: file is opened non-blocking -- vty_config_write_open() sets "pseudo"
   *     blocking.
   */
  fd = uty_fd_file_open(temp_name, vfd_io_write | vfd_io_excl, CONFIGFILE_MASK);

  if (fd < 0)
    {
      err = errno ;

      if (err == EEXIST)
        {
          vty_out(vty,
            "%% %sconfiguration temporary file %s exists...\n"
            "%% ...either a previous configuration write operation failed "
                                               "or another is in progress...\n"
            "%% ...having checked that nothing valuable will be lost, "
                                                         "delete the file...\n"
            "%% ...and try again.\n",
            tag, temp_name) ;

          err = 0 ;
        }
      else
        {
          vty_out(vty, "%% Cannot create %sconfiguration temporary file %s",
                                                               tag, temp_name) ;
        } ;

      goto finished;
    } ;

  /* Make sure we have the required file mode... should be, since we just
   * created the file, but this does not hurt !
   */
  if (chmod(temp_name, CONFIGFILE_MASK) != 0)
    {
      err = errno ;
      vty_out(vty, "%% chmod failed for %sconfiguration temporary file %s",
                                                               tag, temp_name) ;
      goto finished;
    } ;

  /* Push vout for configuration file (vfd_io_ps_blocking) and output friendly
   * heading
   */
  vty_config_write_open(vty, fd) ;

  vty_out (vty, "!\n"
                "! Quagga %sconfiguration%s%s saved from %s\n"
                "!   %s\n"
                "!\n",
                tag,
                integrated ? "" : " for ",
                integrated ? "" : daemon_name,
                integrated ? "vtysh" : "vty",
                quagga_timestamp(0).str) ;

  /* Run through the nodes and get call-back to do the required, sending
   * output to the vty.
   */
  vty->config_to_vtysh = false ;        /* make sure            */
  ret = vty_write_config(vty, write_config_node) ;
  qassert(ret != CMD_WAITING) ;

  /* Done with file, so close.
   *
   * NB: the underlying vfd is a bit special, it has vfd_io_no_close set, so
   *     that as we now close the vin, everything is as normal, *except* that
   *     the fd is not, in fact, closed.
   */
  retw = vty_config_write_close(vty) ;
  qassert(retw != CMD_WAITING) ;

  if (ret == CMD_SUCCESS)
    ret = retw ;                        /* use if otherwise OK  */

  /* Sort out what, if anything, went wrong.
   */
  if (ret != CMD_SUCCESS)
    {
      /* Note that any I/O error has been posted to vio->ebuf, which will
       * be picked up when CMD_IO_ERROR is processed.
       */
      qassert((ret == CMD_IO_ERROR) || (ret == CMD_CANCEL)) ;

      if (ret != CMD_CANCEL)
        vty_out(vty,
                "%% Failed while writing %sconfiguration temporary file %s\n",
                                                               tag, temp_name) ;
      err = 0 ;
      goto finished;
    } ;

  ret = CMD_ERROR ;             /* fear the worst, again        */

  /* Now we can close the temp file.
   */
  while (1)
    {
      int rc ;

      rc = fsync(fd) ;
      if ((rc >= 0) || (errno == EINVAL))
        rc = close(fd) ;

      if (rc >= 0)
        break ;

      if (errno == EINTR)
        continue ;

      err = errno ;
      vty_out (vty, "%% Failed closing %sconfiguration temporary file %s",
                                                               tag, temp_name) ;
      goto finished;
    } ;

  /* Successfully written and closed a shiny new .conf.new.
   *
   * From now on will retain .conf.new if anything fails.  In the code above,
   * refuses to proceed if a .conf.new exists... so we hang on to the newly
   * written configuration until the operator sorts things out.
   */
  fd = -1 ;                     /* File is closed               */
  sync() ;                      /* possibly voodoo, but we try  */

  /* Now move files around to save any existing .conf, and be ready to rename
   * the .conf.new to .conf.
   *
   * If the .conf file exists, then want to rename that as the .conf.sav file.
   */
  err = qpath_stat_is_file(conf) ;
  if (err != ENOENT)
    {
      /* If could not stat() the .conf file, or if it is something other than a
       * file, then give up immediately.
       */
      if (err != 0)
        {
          if (err < 0)
            {
              vty_out(vty,
                "%% %sconfiguration file %s exists\n"
                "%% but is not a simple file -- unsafe to proceed\n",
                tag, conf_name) ;

              err = 0 ;
            }
          else
            {
              vty_out(vty,
                "%% unsafe to proceed while state of %sconfiguration file "
                                                      "cannot be established\n"
                "%% stat(%s) failed",
                tag, conf_name) ;
            } ;

          goto finished ;
        } ;

      /* Save .conf file as .conf.sav file, discarding any previous.
       *
       * At this point we have: .conf.new, .conf & .conf.sav.
       *
       * Discarding the .conf.sav leaves us with the old .conf and the new one,
       * which is fine.  We assume that renaming .conf to .conf.sav will either
       * work or will fail, leaving .conf exactly as it was.  So, if fails
       * will end up with at least: .conf.new & .conf.  If succeeds, we end
       * up with .conf.new and .conf.sav.
       *
       * The worst case would be for the renaming of .conf to fail in such
       * a way that it is lost -- though this *really* should not happen.  In
       * that case we would end up with .conf.new, only.  Note that this code
       * will not delete .conf.new -- so that at least will be preserved.
       *
       * To minimise chances of the rename .conf to .conf.sav failing at all,
       * we, carefully, unlink .conf.sav, first.
       */
      err = qpath_stat_is_file(save) ;
      if (err != ENOENT)
        {
          /* If could not stat() the .conf.sav file, or if it is something other
           * than a file, then give up immediately.
           */
          if (err != 0)
            {
              if (err < 0)
                {
                  vty_out(vty,
                    "%% %sconfiguration backup file %s exists\n"
                    "%% but is not a simple file -- unsafe to proceed\n",
                    tag, save_name) ;

                  err = 0 ;
                }
              else
                {
                  vty_out(vty,
                    "%% unsafe to proceed while state of %sconfiguration "
                                           "backup file cannot be established\n"
                    "%% stat(%s) failed",
                    tag, save_name) ;
                } ;

              goto finished ;
            } ;

          /* Exists and is ordinary file, so should be able to unlink, but
           * proceed with caution in any case.
           */
          if (unlink(save_name) < 0)
            {
              err = errno ;
              vty_out(vty, "%% Cannot delete %sconfiguration backup file %s",
                                                               tag, save_name) ;
              goto finished;
            } ;
        } ;

      /* Now rename .conf -> .conf.sav
       *
       * At this point, the .conf.sav should not exist, so the rename should
       * be a shoo-in.
       */
      if (rename(conf_name, save_name) < 0)
        {
          err = errno ;
          vty_out(vty, "%% Cannot rename %sconfiguration file %s -> %s",
                                                    tag, conf_name, save_name) ;
          goto finished;
        } ;

      sync() ;                      /* possibly voodoo, but we try  */
    } ;

  /* Final step: rename .conf.new -> .conf
   *
   * At this point, the .conf should not exist, so the rename should
   * be a shoo-in.
   */
  if (rename(temp_name, conf_name) < 0)
    {
      err = errno ;
      vty_out(vty, "%% Cannot rename %sconfiguration file %s -> %s",
                                                    tag, temp_name, conf_name) ;
      goto finished;
    }

  sync() ;                      /* possibly voodoo, but we try  */

  /* Success !
   */
  vty_out (vty, "Written %sconfiguration file %s\n", tag, conf_name) ;

  ret = CMD_SUCCESS ;
  err = 0 ;

  /* On exit.
   *
   *   * complete any error message
   *
   *   * if we still have the temp file open, unlink and close it.
   */
finished:
  if (err != 0)
    vty_out(vty, ": %s\n", errtoa(err, 0).str) ;

  if (fd >= 0)
    {
      unlink(temp_name) ;       /* Discard incomplete file      */
      close(fd) ;
    } ;

  qpath_free(temp) ;
  qpath_free(save) ;

  return ret;
} ;

/*------------------------------------------------------------------------------
 * For all possible nodes, invoke the given call-back to write any
 * configuration for that node.
 *
 * Return from the call-back:
 *
 *   <  0 => do not push the output (probably because nothing to push)
 *   == 0 => push the output
 *   >  0 => add "!" separator, and push the output
 *
 * If not vty->config_to_vtysh, add an "end" at the end.
 *
 * Pushes the output as goes along, and at the end.
 *
 * Returns:  CMD_SUCCESS
 *           CMD_WAITING    -- only if vout is not blocking.
 *           CMD_IO_ERROR
 *           CMD_CANCEL
 */
extern cmd_ret_t
vty_write_config(vty vty, int (*write_config_node)(svty vty, node_type_t node))
{
  node_type_t node ;

  if (vty->config_to_vtysh)
    vty_out (vty, "#vtysh-config-daemon %s\n", daemon_name) ;

  for (node = MIN_NODE ; node <= MAX_NODE ; ++node)
    {
      int did ;

      did = write_config_node(vty, node) ;

      if (did > 0)
        vty_out (vty, "!\n");

      if (did >= 0)
        {
          cmd_ret_t ret ;

          ret = vty_cmd_out_push(vty) ;    /* Push stuff so far    */

          if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
            {
              qassert((ret == CMD_IO_ERROR) || (ret == CMD_CANCEL)) ;
              return ret ;
            } ;
        } ;
    } ;

  if (!vty->config_to_vtysh)
    vty_out (vty, "end\n") ;

  return vty_cmd_out_push(vty) ;
} ;

/*==============================================================================
 * Commands
 *
 */
DEFUN_CALL (config_who,
       config_who_cmd,
       "who",
       "Display who is on vty\n")
{
  unsigned int i = 0;
  vty_io vio_who ;

  VTY_LOCK() ;

  vio_who = ddl_head(vio_live_list) ;

  while (vio_who != NULL)
    {
      uty_out(vty->vio, "%svty[%d] connected from %s.\n",
               (vio_who->config != 0) ? "*" : " ", ++i, uty_get_name(vio_who)) ;
      vio_who = ddl_next(vio_who, vio_list) ;
    } ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Show history
 */
DEFUN_CALL (show_history,
       show_history_cmd,
       "show history",
       SHOW_STR
       "Display the session command history\n")
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  switch (vty->type)
    {
      case VTY_TERMINAL:
        uty_cli_hist_show(vty->vio->vin_base->cli) ;
        ret = CMD_SUCCESS ;
        break ;

      case VTY_VTYSH:
        if (vtysh_cmd_call_backs != NULL)
          ret = vtysh_cmd_call_backs->show_history(vty) ;
        else
          {
            uty_out(vty->vio, "%% %s: no vtysh_cmd_call_backs !\n", __func__) ;
            ret = CMD_ERROR ;
          } ;
        break ;

      default:
        ret = CMD_SUCCESS ;
        break ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
}

/*------------------------------------------------------------------------------
 * "terminal length 99" and "terminal no length"
 *
 * Limited effect:
 *
 *   * VTY_TERMINAL: only affects the *current* vty.
 *
 *   * VTY_VTYSH:    only affects the vtysh pager setting.
 *
 * Setting: lines == 0 turns *off* the "--more--" paging.
 *          lines <  0 uses whatever the terminal says or some other default
 *                     and turns *on* the "--more--" paging unless no
 *                     default number of lines is available.
 */
static cmd_ret_t
vty_set_lines(vty vty, int lines)
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  switch (vty->type)
    {
      case VTY_TERMINAL:
        uty_cli_set_lines(vty->vio->vin_base->cli, lines, true) ;
        ret = CMD_SUCCESS ;
        break ;

      case VTY_VTYSH:
        if (vtysh_cmd_call_backs != NULL)
          ret = vtysh_cmd_call_backs->terminal_length(vty, lines) ;
        else
          {
            uty_out(vty->vio, "%% %s: no vtysh_cmd_call_backs !\n", __func__) ;
            ret = CMD_ERROR ;
          } ;
        break ;

      default:
        ret = CMD_SUCCESS ;
        break ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

DEFUN_CALL (config_terminal_length, config_terminal_length_cmd,
       "terminal length <0-512>",
       "Set terminal line parameters\n"
       "Set number of lines on a screen\n"
       "Number of lines on screen (0 for no pausing)\n")
{
  return vty_set_lines(vty, strtol(argv[0], NULL, 0)); /* parsed already  */
}

DEFUN_CALL (config_terminal_length_none, config_terminal_length_none_cmd,
    "terminal length none",
    "Set terminal line parameters\n"
    "Set number of lines on a screen\n"
    "No length -- no pausing\n")
{
  return vty_set_lines(vty, 0);
}

DEFUN_CALL (config_terminal_no_length, config_terminal_no_length_cmd,
    "terminal no length",
    "Set terminal line parameters\n"
    NO_STR
    "Set number of lines on a screen\n")
{
  return vty_set_lines(vty, -1);
}

ALIAS_CALL (config_terminal_no_length, config_terminal_length_default_cmd,
    "terminal length default",
    "Set terminal line parameters\n"
    "Set number of lines on a screen\n"
    "Default number of lines for screen\n") ;

/*------------------------------------------------------------------------------
 * Move to vty configuration mode.
 */
DEFUN_ATTR (line_vty,
            line_vty_cmd,
            "line vty",
            "Configure a terminal line\n"
            "Virtual terminal\n",
            CMD_ATTR_DIRECT + CMD_ATTR_NODE + VTY_NODE)
{
  VTY_LOCK() ;
  vty->node = VTY_NODE;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * "service terminal-length 99" and "no service terminal-length"
 *
 * Set the default terminal length for the next vty to open.
 */
static cmd_ret_t
set_host_lines(int lines)
{
  VTY_LOCK() ;

  host.lines = lines ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

DEFUN_CALL (service_terminal_length, service_terminal_length_cmd,
       "service terminal-length <0-512>",
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "Number of lines of VTY (0 means no line control)\n")
{
  return set_host_lines(strtol(argv[0], NULL, 0)) ;     /* parsed already */
} ;

DEFUN_CALL (service_terminal_length_none, service_terminal_length_none_cmd,
       "service terminal-length none",
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "No length -- no pausing\n")
{
  return set_host_lines(0) ;
} ;

DEFUN_CALL (no_service_terminal_length, no_service_terminal_length_cmd,
       "no service terminal-length [<0-512>]",
       NO_STR
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "Number of lines of VTY (0 means no line control)\n")
{
  return set_host_lines(-1) ;
}

ALIAS_CALL (no_service_terminal_length, service_terminal_length_default_cmd,
    "service terminal-length none",
    "Set up miscellaneous service\n"
    "System wide terminal length configuration\n"
    "Default number of lines for terminal\n") ;

/*------------------------------------------------------------------------------
 * Set time out value.
 *
 * Affects the current *and* any future VTY_TERMINAL or VTY_VTYSH_SERVER type
 * VTY.
 */
static int
exec_timeout (struct vty *vty, const char *min_str, const char *sec_str)
{
  unsigned long timeout = 0;

  VTY_LOCK() ;

  /* min_str and sec_str are already checked by parser.  So it must be
     all digit string. */
  if (min_str)
    {
      timeout = strtol (min_str, NULL, 10);
      timeout *= 60;
    }
  if (sec_str)
    timeout += strtol (sec_str, NULL, 10);

  host.vty_timeout_val = timeout;

  uty_set_timeout(vty->vio, timeout) ;  /* update own timeout, if required  */

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (exec_timeout_min,
       exec_timeout_min_cmd,
       "exec-timeout <0-35791>",
       "Set timeout value\n"
       "Timeout value in minutes\n")
{
  return exec_timeout (vty, argv[0], NULL);
}

DEFUN_CALL (exec_timeout_sec,
       exec_timeout_sec_cmd,
       "exec-timeout <0-35791> <0-2147483>",
       "Set the EXEC timeout\n"
       "Timeout in minutes\n"
       "Timeout in seconds\n")
{
  return exec_timeout (vty, argv[0], argv[1]);
}

DEFUN_CALL (no_exec_timeout,
       no_exec_timeout_cmd,
       "no exec-timeout",
       NO_STR
       "Set the EXEC timeout\n")
{
  return exec_timeout (vty, NULL, NULL);
}

/*------------------------------------------------------------------------------
 * Set vty access class.
 */
DEFUN_CALL (vty_access_class,
       vty_access_class_cmd,
       "access-class WORD",
       "Filter connections based on an IP access list\n"
       "IP access list\n")
{
  VTY_LOCK() ;

  XFREE(MTYPE_HOST, host.vty_accesslist_name);

  host.vty_accesslist_name = XSTRDUP(MTYPE_HOST, argv[0]);

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/* Clear vty access class. */
DEFUN_CALL (no_vty_access_class,
       no_vty_access_class_cmd,
       "no access-class [WORD]",
       NO_STR
       "Filter connections based on an IP access list\n"
       "IP access list\n")
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  if ((argc == 0) || ( (host.vty_accesslist_name != NULL) &&
                       (strcmp(host.vty_accesslist_name, argv[0]) == 0) ))
    {
      XFREE(MTYPE_HOST, host.vty_accesslist_name);
                        /* sets host.vty_accesslist_name = NULL    */
      ret = CMD_SUCCESS ;
    }
  else
    {
      vty_out(vty, "Access-class is not currently applied to vty\n") ;
      ret = CMD_WARNING;
    } ;

  VTY_UNLOCK() ;
  return ret;
}

#ifdef HAVE_IPV6
/* Set vty access class. */
DEFUN_CALL (vty_ipv6_access_class,
       vty_ipv6_access_class_cmd,
       "ipv6 access-class WORD",
       IPV6_STR
       "Filter connections based on an IP access list\n"
       "IPv6 access list\n")
{
  VTY_LOCK() ;

  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);

  host.vty_ipv6_accesslist_name = XSTRDUP(MTYPE_HOST, argv[0]);

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/* Clear vty access class. */
DEFUN_CALL (no_vty_ipv6_access_class,
       no_vty_ipv6_access_class_cmd,
       "no ipv6 access-class [WORD]",
       NO_STR
       IPV6_STR
       "Filter connections based on an IP access list\n"
       "IPv6 access list\n")
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  if ((argc == 0) || ( (host.vty_ipv6_accesslist_name != NULL) &&
                       (strcmp(host.vty_ipv6_accesslist_name, argv[0]) == 0) ))
    {
      XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
                        /* sets host.vty_ipv6_accesslist_name = NULL    */
      ret = CMD_SUCCESS ;
    }
  else
    {
      vty_out(vty, "IPv6 access-class is not currently applied to vty\n") ;
      ret = CMD_WARNING;
    } ;

  VTY_UNLOCK() ;
  return ret;
}
#endif /* HAVE_IPV6 */

/*------------------------------------------------------------------------------
 * vty login -- enable/disable password check
 *
 * If "login" (no_password_check == false), must pass the AUTH_NODE password
 * check in order to run a VTY_TERMINAL.  Will not run a VTY_TERMINAL at all if
 * no password is set.
 *
 * If "no login", a VTY_TERMINAL will start in RESTRICTED_NODE/VIEW_NODE/
 * ENABLE_NODE, without needing to pass the AUTH_NODE password -- whether or
 * not a password is set.
 */
DEFUN_CALL (vty_login,
       vty_login_cmd,
       "login",
       "Enable password checking\n")
{
  VTY_LOCK() ;
  host.no_password_check = false ;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_vty_login,
       no_vty_login_cmd,
       "no login",
       NO_STR
       "Enable password checking\n")
{
  VTY_LOCK() ;
  host.no_password_check = true ;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * initial mode -- enable/disable RESTRICTED_NODE
 *
 * If RESTRICTED_NODE is enabled, then if "no login" will start a VTY_TERMINAL
 * in that restricted state.
 *
 * This takes precedence over "advanced-vty".
 *
 * This only affects the initial state of a VTY_TERMINAL.  In particular, once
 * a VTY_TERMINAL starts in RESTRICTED_NODE it will stay there until it is
 * able to change up to ENABLE_NODE (or CONFIG_NODE).
 */
DEFUN_CALL (vty_restricted_mode,
       vty_restricted_mode_cmd,
       "anonymous restricted",
       "Restrict view commands available in anonymous, unauthenticated vty\n")
{
  VTY_LOCK() ;
  host.restricted_mode = true;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (vty_no_restricted_mode,
       vty_no_restricted_mode_cmd,
       "no anonymous restricted",
       NO_STR
       "Enable password checking\n")
{
  VTY_LOCK() ;
  host.restricted_mode = false;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Enable/Disable advanced-vty
 *
 * The "advanced-vty" setting only has effect if there is no enable password.
 *
 * So iff "advanced-vty" *and* no enable password:
 *
 *   * when a VTY_TERMINAL starts: if "no login" *and* "no anonymous restricted"
 *     will start in ENABLE_NODE.
 *
 *   * thereafter, if in VIEW_NODE *and* a password is set, then on passing the
 *     password check, will enter ENABLE_NODE.
 */
DEFUN_CALL (service_advanced_vty,
       service_advanced_vty_cmd,
       "service advanced-vty",
       "Set up miscellaneous service\n"
       "Enable advanced mode vty interface\n")
{
  VTY_LOCK() ;
  host.advanced = true;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_advanced_vty,
       no_service_advanced_vty_cmd,
       "no service advanced-vty",
       NO_STR
       "Set up miscellaneous service\n"
       "Enable advanced mode vty interface\n")
{
  VTY_LOCK() ;
  host.advanced = false;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Enable/Disable monitoring of logging on the *current* VTY_TERMINAL
 */
DEFUN_CALL (terminal_monitor,
       terminal_monitor_cmd,
       "terminal monitor",
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")
{
  VTY_LOCK() ;
  uty_monitor_set(vty->vio, on);
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (terminal_no_monitor,
       terminal_no_monitor_cmd,
       "terminal no monitor",
       "Set terminal line parameters\n"
       NO_STR
       "Copy debug output to the current terminal line\n")
{
  VTY_LOCK() ;
  uty_monitor_set(vty->vio, off);
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

ALIAS_CALL (terminal_no_monitor,
       no_terminal_monitor_cmd,
       "no terminal monitor",
       NO_STR
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")

/*==============================================================================
 * Output the current configuration
 *
 * Returns: CMD_SUCCESS
 */
static int
vty_config_write (struct vty *vty)
{
  vty_io vio ;

  VTY_LOCK() ;                  /* while accessing the host.xx          */

  vio = vty->vio ;

  uty_out (vio, "line vty\n");

  if (host.vty_accesslist_name)
    uty_out (vio, " access-class %s\n", host.vty_accesslist_name);

  if (host.vty_ipv6_accesslist_name)
    uty_out (vio, " ipv6 access-class %s\n", host.vty_ipv6_accesslist_name);

  /* exec-timeout */
  if (host.vty_timeout_val != VTY_TIMEOUT_DEFAULT)
    uty_out (vio, " exec-timeout %ld %ld\n", host.vty_timeout_val / 60,
                          	             host.vty_timeout_val % 60);

  /* login */
  if (host.no_password_check)
    uty_out (vio, " no login\n");

  if (host.restricted_mode != restricted_mode_default)
    {
      if (restricted_mode_default)
        uty_out (vio, " no anonymous restricted\n");
      else
        uty_out (vio, " anonymous restricted\n");
    }

  uty_out (vio, "!\n");

  VTY_UNLOCK() ;

  return CMD_SUCCESS;
}

/*==============================================================================
 * The cwd at start-up.
 */

/*------------------------------------------------------------------------------
 * Get cwd as at start-up.  Never changed -- so no locking required.
 */
extern qpath
vty_getcwd (qpath qp)
{
  VTY_LOCK() ;

  qp = qpath_copy(qp, host.cwd) ;

  VTY_UNLOCK() ;

return qp ;
}

/*==============================================================================
 * Access functions for vio values, where locking is or might be required.
 */
#if 0

static bool
vty_is_terminal(struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = uty_is_terminal(vty) ;
  VTY_UNLOCK() ;
  return result;
}

static bool
vty_is_shell_server (struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = uty_is_shell_server(vty) ;
  VTY_UNLOCK() ;
  return result;
}

static bool
vty_is_shell_client (struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = uty_is_shell_client(vty) ;
  VTY_UNLOCK() ;
  return result;
}

#endif

/*==============================================================================
 *
 */
const char* wordlist[] =
  {
      "Lorem",
      "ipsum",
      "dolor",
      "magna",
      "vita",
      "brevis",
      "Aliquot",
      "in",
      "tempura",
      "mores",
      "ad",
      "Astronomica",
      "per",
      "impedimenta",
      "quod",
      "et",
      "sed",
      "semper",
      "ut",
      "Elisium",
      "est",
  };

#if QDEBUG

DEFUN (delay_secs,
       delay_secs_cmd,
       "delay <0-600> secs <0-10000> lines",
       "Delay for a number of seconds and spit out a number of lines\n"
       "Delay time\n"
       "Delay time units\n"
       "How much to output\n"
       "Output units\n")
{
  unsigned long delay ;
  unsigned long lines ;

  unsigned long unit ;

  delay = strtol(argv[0], NULL, 10) ;
  lines = strtol(argv[1], NULL, 10) ;

  vty_out(vty, "delay %d secs %d lines\n", (int)delay, (int)lines) ;

  unit = (lines * 100) / delay ;

  while (delay--)
    {
      char  buf[200] ;
      char* e ;
      int   n ;
      int   w = sizeof(wordlist) / sizeof(char*) ;

      sleep(1) ;

      n = ((rand() % (unit + 1)) + (unit / 2)) / 100 ;

      if ((n > (int)lines) || (delay == 0))
        n = lines ;

      lines -= n ;

      while (n--)
        {
          char* q ;
          const char* p ;
          int a ;

          q = buf ;
          e = buf + (rand() % 120) + 30 ;

          if ((rand() % 6) == 0)
            e = buf ;

          a = (rand() % 4) == 1 ;
          while (q < e)
            {
              int s ;
              s = 0 ;
              if (a == 1)
                s = (rand() % 5) + 1 ;
              else if (a > 1)
                s = 1 ;

              while (s--)
                *q++ = ' ' ;

              p = wordlist[rand() % w] ;
              while (*p != '\0')
                *q++ = *p++ ;

              a = (rand() % 4) + 1 ;
            } ;

          if (n != 0)
            *q++ = '\n' ;
          *q++ = '\0' ;

          vty_out(vty, buf) ;
        } ;
    } ;

  return CMD_SUCCESS;
}
#endif   /* QDEBUG      */

/*==============================================================================
 * The VTY commands
 *
 * These are commands which apply only to a VTY_TERMINAL, or to the
 * configuration of a VTY_TERMINAL.
 *
 * These are installed in vtysh, but not for execution in the VTYSH_VD.
 *
 * These are not installed for BASIC_VD at all.
 *
 * The entries marked TERM do not execute in any real daemon when in the
 * vtysh.  Where they are not executable in VTYSH_VD either, they will either
 * do nothing (configuration files) or a warning will be issued (interactive
 * commands).
 */
CMD_INSTALL_TABLE(static, vty_cmd_table, ALL_RDS) =
{
  { RESTRICTED_NODE, &config_terminal_length_cmd                        },
  { RESTRICTED_NODE, &config_terminal_no_length_cmd                     },
  { RESTRICTED_NODE, &config_terminal_length_none_cmd                   },
  { RESTRICTED_NODE, &config_terminal_length_default_cmd                },
  { RESTRICTED_NODE, &config_who_cmd                                    },
  { RESTRICTED_NODE, &show_history_cmd                                  },

  { VIEW_NODE,       &config_terminal_length_cmd         , TERM | VTYSH_VD },
  { VIEW_NODE,       &config_terminal_no_length_cmd      , TERM | VTYSH_VD },
  { VIEW_NODE,       &config_terminal_length_none_cmd    , TERM | VTYSH_VD },
  { VIEW_NODE,       &config_terminal_length_default_cmd , TERM | VTYSH_VD },
  { VIEW_NODE,       &config_who_cmd                                    },
  { VIEW_NODE,       &show_history_cmd                   , TERM         },

  { ENABLE_NODE,     &config_terminal_length_cmd         , TERM | VTYSH_VD },
  { ENABLE_NODE,     &config_terminal_no_length_cmd      , TERM | VTYSH_VD },
  { ENABLE_NODE,     &config_terminal_length_none_cmd    , TERM | VTYSH_VD },
  { ENABLE_NODE,     &config_terminal_length_default_cmd , TERM | VTYSH_VD },
  { ENABLE_NODE,     &config_who_cmd                                    },
  { ENABLE_NODE,     &show_history_cmd                   , TERM | VTYSH_VD },

  { ENABLE_NODE,     &terminal_monitor_cmd               , TERM         },
  { ENABLE_NODE,     &terminal_no_monitor_cmd            , TERM         },
  { ENABLE_NODE,     &no_terminal_monitor_cmd            , TERM         },

  { CONFIG_NODE,     &line_vty_cmd                 /* -> VTY_NODE */    },

  { CONFIG_NODE,     &service_terminal_length_cmd                       },
  { CONFIG_NODE,     &no_service_terminal_length_cmd                    },
  { CONFIG_NODE,     &service_terminal_length_none_cmd                  },
  { CONFIG_NODE,     &service_terminal_length_default_cmd               },

  { CONFIG_NODE,     &service_advanced_vty_cmd                          },
  { CONFIG_NODE,     &no_service_advanced_vty_cmd                       },

  /* Configuration node for vty
   */
  { VTY_NODE,        &exec_timeout_min_cmd                              },
  { VTY_NODE,        &exec_timeout_sec_cmd                              },
  { VTY_NODE,        &no_exec_timeout_cmd                               },
  { VTY_NODE,        &vty_access_class_cmd                              },
  { VTY_NODE,        &no_vty_access_class_cmd                           },
  { VTY_NODE,        &vty_login_cmd                                     },
  { VTY_NODE,        &no_vty_login_cmd                                  },
  { VTY_NODE,        &vty_restricted_mode_cmd                           },
  { VTY_NODE,        &vty_no_restricted_mode_cmd                        },
#ifdef HAVE_IPV6
  { VTY_NODE,        &vty_ipv6_access_class_cmd                         },
  { VTY_NODE,        &no_vty_ipv6_access_class_cmd                      },
#endif /* HAVE_IPV6 */

#if QDEBUG
  { VIEW_NODE,       &delay_secs_cmd                                    },
  { RESTRICTED_NODE, &delay_secs_cmd                                    },
  { ENABLE_NODE,     &delay_secs_cmd                                    },
#endif

  CMD_INSTALL_END
} ;

/*------------------------------------------------------------------------------
 * Install vty's own commands like `who' command.
 */
static void
uty_cmd_init (void)
{
  cmd_install_node_config_write (VTY_NODE, vty_config_write);
  cmd_install_table(vty_cmd_table) ;
} ;

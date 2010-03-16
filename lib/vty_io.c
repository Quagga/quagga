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
 * ALL vty command output ends up here.
 *
 *   vty == NULL    => vprintf(stdout, ...)
 *   VTY_SHELL      => vprintf(stdout, ...)
 *   VTY_STDOUT     => vprintf(stdout, ...)
 *
 *   VTY_FILE       => write(fd, ....)
 *
 *   VTY_TERM       => command FIFO
 *   VTY_SHELL_SERV => command FIFO
 *
 * During command processing the output sent here is held until the command
 * completes.
 */

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
 */
extern int
uty_vout(struct vty *vty, const char *format, va_list args)
{
  enum vty_type type ;
  vty_io vio ;
  int    len ;

  VTY_ASSERT_LOCKED() ;

  /* Establish type of vty -- if any                                    */
  if (vty != NULL)
    {
      vio = vty->vio ;
      if (!vio->file.write_open)
        return 0 ;                   /* discard output if not open !    */

      type = vio->type ;
    }
  else
    {
      vio = NULL ;
      type = VTY_STDOUT ;
    } ;

  /* Output -- process depends on type                                  */
  switch (type)
  {
    case VTY_STDOUT:
    case VTY_SHELL:
      len = vprintf (format, args);
      break ;

    case VTY_FILE:
    case VTY_TERM:
    case VTY_SHELL_SERV:

      len = qs_vprintf(&vio->cmd_vbuf, format, args) ;
      if (len > 0)
        {
          if (type == VTY_FILE)
            len = write(vio->file.fd, vio->cmd_vbuf.body, len) ;
          else
            vio_fifo_put(&vio->cmd_obuf, vio->cmd_vbuf.body, len) ;
        } ;
      break ;

    default:
      zabort("impossible VTY type") ;
  } ;

  return len;
} ;

/*------------------------------------------------------------------------------
 * Discard the current contents of the command FIFO
 *
 * TODO: worry about line control ??
 */
extern void
uty_out_discard(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  vio_fifo_set_empty(&vio->cmd_obuf) ;
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

/*------------------------------------------------------------------------------
 * Watch dog action
 */
static void
uty_watch_dog_bark(void)
{
  uty_check_host_name() ;       /* check for host name change           */

  /* TODO: death watch scan                                             */

  /* Set timer to go off again later                                    */
  if (vty_cli_nexus)
    qtimer_set(vty_watch_dog.qnexus, qt_add_monotonic(vty_watch_dog_interval),
                                                         vty_watch_dog_qnexus) ;
  else
    {
      if (vty_watch_dog.thread != NULL)
        thread_cancel (vty_watch_dog.thread);
      vty_watch_dog.thread = thread_add_timer (vty_master,
                           vty_watch_dog_thread, NULL, vty_watch_dog_interval) ;
    } ;
} ;

static void
uty_watch_dog_start()
{
  if (vty_cli_nexus)
    vty_watch_dog.qnexus = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                                   NULL, NULL) ;

  uty_watch_dog_bark() ;        /* start up by barking the first time   */
}

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
  uty_watch_dog_bark() ;
  VTY_UNLOCK() ;
  return 0 ;
} ;

/*==============================================================================
 * Prototypes.
 */
static void uty_file_init_new(vio_file file, int fd, void* info) ;
static void uty_file_half_close(vio_file file) ;
static void uty_file_close(vio_file file) ;

static void vty_read_qnexus (qps_file qf, void* file_info) ;
static void vty_write_qnexus (qps_file qf, void* file_info) ;
static void vty_timer_qnexus (qtimer qtr, void* timer_info, qtime_t when) ;

static int vty_read_thread (struct thread *thread) ;
static int vty_write_thread (struct thread *thread) ;
static int vty_timer_thread (struct thread *thread) ;

static void vtysh_read_qnexus (qps_file qf, void* file_info) ;
static int vtysh_read_thread (struct thread *thread) ;

/*==============================================================================
 * Creation and destruction of VTY objects
 */

/*------------------------------------------------------------------------------
 * Allocate new vty struct
 *
 * Allocates and initialises vty_io structure, complete with:
 *
 *   Output buffer
 *   Input buffer
 *   qpselect file -- added to CLI nexus  ) if running CLI nexus
 *   qtimer                               )
 *
 * Adds to the known vty's -- which locks/unlocks momentarily.
 *
 * Returns: new vty
 */
extern struct vty *
uty_new (int fd, enum vty_type type)
{
  struct vty *vty ;
  struct vty_io* vio ;

  VTY_ASSERT_LOCKED() ;

  if (vty_watch_dog.anon == NULL)
    uty_watch_dog_start() ;

  vty = XCALLOC (MTYPE_VTY, sizeof (struct vty));
  vio = XCALLOC (MTYPE_VTY, sizeof (struct vty_io)) ;

  vty->vio = vio ;
  vio->vty = vty ;

  /* Zeroising the vty structure has set:
   *
   *   node      = 0  TODO: something better for node value ????
   *   buf       = NULL -- no command line, yet
   *   index     = NULL -- nothing, yet
   *   index_sub = NULL -- nothing, yet
   */
  confirm(AUTH_NODE == 0) ;     /* default node type    */

  if (type == VTY_TERM)
    vty->newline = "\r\n" ;
  else
    vty->newline = "\n" ;

  /* Zeroising the vty_io structure has set:
   *
   *   vio_list      both pointers NULL
   *
   *   half_closed         = 0 -- NOT half closed (important !)
   *   timed_out           = 0 -- NOT timed out
   *
   *   mon_list      both pointers NULL
   *
   *   name                = NULL -- no name, yet
   *
   *   cli_drawn           = 0 -- not drawn
   *   cli_prompt_len      = 0 )
   *   cli_extra_len       = 0 ) not material
   *   cli_echo_suppress   = 0 )
   *
   *   cli_prompt_node     = 0 -- not material
   *   cli_prompt_set      = 0 -- so prompt needs to be constructed
   *
   *   cli_blocked         = 0 -- not blocked
   *   cmd_in_progress     = 0 -- no command in progress
   *
   *   cli_do              = 0 = cli_do_nothing
   *
   *   cmd_wait_more       = 0 -- not waiting for response to "--more--"
   *
   *   fail                = 0 -- no login failures yet
   *
   *   hist                = empty vector
   *   hp                  = 0 -- at the beginning
   *   hindex              = 0 -- the beginning
   *
   *   width               = 0 -- unknown console width
   *   height              = 0 -- unknown console height
   *
   *   lines               = 0 -- unset
   *
   *   monitor             = 0 -- not a monitor
   *
   *   config              = 0 -- not holder of "config" mode
   */
  confirm(cli_do_nothing == 0) ;

  vio->type   = type ;

  uty_file_init_new(&vio->file, fd, vio) ;

  vio->key_stream = keystroke_stream_new('\0') ;  /* TODO: CSI ??       */

  qs_init_new(&vio->ibuf, 0) ;

  qs_init_new(&vio->cli_prompt_for_node, 0) ;

  qs_init_new(&vio->cl,  0) ;
  qs_init_new(&vio->clx, 0) ;

  qs_init_new(&vio->cli_vbuf, 0) ;
  vio_fifo_init_new(&vio->cli_obuf, 4 * 1024) ; /* allocate in 4K lumps */

  qs_init_new(&vio->cmd_vbuf, 0) ;
  vio_fifo_init_new(&vio->cmd_obuf, 16 * 1024) ;

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
uty_new_term(int vty_sock, union sockunion *su)
{
  struct vty *vty ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new (vty_sock, VTY_TERM) ;
  vio = vty->vio ;

  /* Set the action functions                                           */
  if (vty_cli_nexus)
    {
      vio->file.action.read.qnexus  = vty_read_qnexus ;
      vio->file.action.write.qnexus = vty_write_qnexus ;
      vio->file.action.timer.qnexus = vty_timer_qnexus ;
    }
  else
    {
      vio->file.action.read.thread  = vty_read_thread ;
      vio->file.action.write.thread = vty_write_thread ;
      vio->file.action.timer.thread = vty_timer_thread ;
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
  vio->file.v_timeout = vty_timeout_val;

  /* Use global 'lines' setting, otherwise is unset                     */
  if (host.lines >= 0)
    vio->lines = host.lines;
  else
    vio->lines = -1;

  /* Setting up terminal.                                               */
  uty_will_echo (vio);
  uty_will_suppress_go_ahead (vio);
  uty_dont_linemode (vio);
  uty_do_window_size (vio);
  if (0)
    uty_dont_lflow_ahead (vio) ;

  /* Set CLI into state waiting for output to complete.                 */
  vio->cli_blocked      = 1 ;
  uty_file_set_write(&vio->file, on) ;

  /* Reject connection if password isn't set, and not "no password"     */
  if ((host.password == NULL) && (host.password_encrypt == NULL)
                                                        && ! no_password_check)
    {
      uty_out (vty, "Vty password is not set.%s", VTY_NEWLINE);
      uty_half_close (vio);
      return NULL;
    }

  /* Say hello to the world. */
  vty_hello (vty);

  if (! no_password_check)
    uty_out (vty, "%sUser Access Verification%s%s", VTY_NEWLINE,
                                                     VTY_NEWLINE, VTY_NEWLINE);

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_SHELL_SERV -- ie attached to a vtysh session.
 *
 * Returns: new vty
 */
static struct vty *
uty_new_shell_serv(int vty_sock)
{
  struct vty *vty ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new (vty_sock, VTY_SHELL_SERV) ;
  vio = vty->vio ;

  /* Set the action functions                                           */
  if (vty_cli_nexus)
    {
      vio->file.action.read.qnexus  = vtysh_read_qnexus ;
      vio->file.action.write.qnexus = vty_write_qnexus ;
      vio->file.action.timer.qnexus = NULL ;
    }
  else
    {
      vio->file.action.read.thread  = vtysh_read_thread ;
      vio->file.action.write.thread = vty_write_thread ;
      vio->file.action.timer.thread = NULL ;
    } ;

  vty->node = VIEW_NODE;

  /* Enable the command output to clear the output to date, and set cli
   * state to blocked waiting for that output to complete.
   */
  vio->cli_blocked        = 1 ;
  uty_file_set_write(&vio->file, on) ;

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
      if ((vio->type == VTY_TERM) && vio->file.write_open)
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
 * Shuts the read side and discards any buffered input.
 *
 * Leaves the output running, but places the VTY on "death watch".  When
 * all output completes and there is no queued command or anything else
 * active, the VTY is finally put to sleep.
 */
extern void
uty_half_close (vty_io vio)
{
  char* line ;

  VTY_ASSERT_LOCKED() ;

  if (vio->half_closed)
    return ;                    /* cannot do it again   */

  vio->half_closed = 1 ;

  uzlog (NULL, LOG_INFO, "Vty connection (fd %d) close", vio->file.fd) ;

  uty_file_half_close(&vio->file) ;
  uty_set_monitor(vio, 0) ;

  keystroke_stream_free(vio->key_stream) ;
  qs_free_body(&vio->ibuf) ;

  uty_cli_wipe(vio) ;

  while ((line = vector_ream_keep(&vio->hist)) != NULL)
    XFREE(MTYPE_VTY_HIST, line) ;

  /* Hit the width, height and lines so that all output clears without
   * interruption.
   */
  vio->width   = 0 ;
  vio->height  = 0 ;
  vio->lines   = 0 ;

  /* Make sure no longer holding the config symbol of power             */
  uty_config_unlock(vio->vty, AUTH_NODE) ;

  /* Move to the death watch list                                       */
  sdl_del(vio_list_base, vio, vio_list) ;
  sdl_push(vio_death_watch, vio, vio_list) ;
} ;

/*------------------------------------------------------------------------------
 * Closing down VTY.
 *
 * Shuts down everything and discards all buffers etc. etc.
 *
 * If not already on it, places the VTY on "death watch".  When there is no
 * queued command or anything else active, the VTY is finally put to sleep.
 */
extern void
uty_close (vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  uty_file_close(&vio->file) ;  /* bring the file to a complete stop    */

  uty_half_close(vio) ;         /* deal with the input side, and place on
                                   death watch -- if not already done   */

  qs_free_body(&vio->cli_prompt_for_node) ;
  qs_free_body(&vio->cl) ;
  qs_free_body(&vio->clx) ;
  qs_free_body(&vio->cli_vbuf) ;
  qs_free_body(&vio->cmd_vbuf) ;

  vio_fifo_reset_keep(&vio->cli_obuf) ;
  vio_fifo_reset_keep(&vio->cmd_obuf) ;

  vio->vty->buf = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Closing down VTY completely.
 *
 * Shuts down everything and discards all buffers etc. etc.
 *
 * If not already on it, places the VTY on "death watch".  When there is no
 * queued command or anything else active, the VTY is finally put to sleep.
 */
extern void
uty_full_close (vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  uty_file_close(&vio->file) ;  /* bring the file to a complete stop    */

  uty_half_close(vio) ;         /* deal with the input side, and place on
                                   death watch -- if not already done   */

  qs_free_body(&vio->cli_prompt_for_node) ;
  qs_free_body(&vio->cl) ;
  qs_free_body(&vio->clx) ;
  qs_free_body(&vio->cli_vbuf) ;
  qs_free_body(&vio->cmd_vbuf) ;

  vio_fifo_reset_keep(&vio->cli_obuf) ;
  vio_fifo_reset_keep(&vio->cmd_obuf) ;

  vio->vty->buf = NULL ;
} ;

/*==============================================================================
 * Dealing with am I/O error on VTY
 *
 * If this is the first error for this VTY, produce suitable log message.
 *
 * If is a "monitor", turn that off, *before* issuing log message.
 */
static int
uty_io_error(vty_io vio, const char* what)
{
  /* can no longer be a monitor !                                       */
  uty_set_monitor(vio, 0) ;

  /* if this is the first error, log it                                 */
  if (vio->file.error_seen == 0)
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
          zabort("unknown VTY type for uty_io_error()") ;
      } ;

      vio->file.error_seen = errno ;
      uzlog(NULL, LOG_WARNING, "%s: %s failed on fd %d: %s",
                type, what, vio->file.fd, safe_strerror(vio->file.error_seen)) ;
    } ;

  return -1 ;
} ;

/*==============================================================================
 * vio_file level operations
 */

/*------------------------------------------------------------------------------
 * Initialise a new vio_file structure.
 *
 * Requires that: the vio_file structure is not currently in use.
 *
 *                if fd >= 0 then: file is open and ready read and write
 *                      otherwise: file is not open
 *
 *                there are no errors, yet.
 *
 * Sets timeout to no timeout at all -- timeout is optional.
 */
static void
uty_file_init_new(vio_file file, int fd, void* info)
{
  memset(file, 0, sizeof(struct vio_file)) ;

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
  file->fd          = fd ;
  file->info        = info ;

  file->read_open   = (fd >= 0) ;
  file->write_open  = (fd >= 0) ;

  if (vty_cli_nexus)
    {
      file->qf = qps_file_init_new(NULL, NULL);
      if (fd >= 0)
        qps_add_file(vty_cli_nexus->selection, file->qf, file->fd, file->info);
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
uty_file_restart_timer(vio_file file)
{
  if (file->v_timeout != 0)
    {
      assert(file->action.timer.anon != NULL) ;

      if (vty_cli_nexus)
        {
          if (file->qtr == NULL)    /* allocate qtr if required     */
            file->qtr = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                             NULL, file->info) ;
          qtimer_set(file->qtr, qt_add_monotonic(QTIME(file->v_timeout)),
                                                    file->action.timer.qnexus) ;
        }
      else
        {
          if (file->t_timer != NULL)
            thread_cancel (file->t_timer);
          file->t_timer = thread_add_timer (vty_master,
                       file->action.timer.thread, file->info, file->v_timeout) ;
        } ;

      file->timer_running = 1 ;
    }
  else if (file->timer_running)
    {
      if (vty_cli_nexus)
        {
          if (file->qtr != NULL)
            qtimer_unset(file->qtr) ;
        }
      else
        {
          if (file->t_timer != NULL)
            thread_cancel (file->t_timer) ;
        } ;

      file->timer_running = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set a new timer value.
 */
extern void
uty_file_set_timer(vio_file file, unsigned long timeout)
{
  file->v_timeout = timeout ;
  if (file->timer_running)
    uty_file_restart_timer(file) ;
} ;

/*------------------------------------------------------------------------------
 * Set read on/off -- restart timer.
 */
extern void
uty_file_set_read(vio_file file, bool on)
{
  if (file->fd < 0)
    return ;

  if (on)
    {
      assert(file->action.read.anon != NULL) ;

      if (vty_cli_nexus)
        {
          qps_enable_mode(file->qf, qps_read_mnum, file->action.read.qnexus) ;
        }
      else
        {
          if (file->t_read != NULL)
            thread_cancel(file->t_read) ;

          file->t_read = thread_add_read(vty_master,
                               file->action.read.thread, file->info, file->fd) ;
        } ;
    }
  else
    {
      if (vty_cli_nexus)
        {
          qps_disable_modes(file->qf, qps_read_mbit) ;
        }
      else
        {
          if (file->t_read != NULL)
            thread_cancel (file->t_read) ;
        } ;
    } ;

    uty_file_restart_timer(file) ;
} ;

/*------------------------------------------------------------------------------
 * Set write on/off -- restart timer.
 */
extern void
uty_file_set_write(vio_file file, bool on)
{
  if (file->fd < 0)
    return ;

  if (on)
    {
      assert(file->action.write.anon != NULL) ;

      if (vty_cli_nexus)
        {
          qps_enable_mode(file->qf, qps_write_mnum, file->action.write.qnexus) ;
        }
      else
        {
          if (file->t_write != NULL)
            thread_cancel(file->t_write) ;

          file->t_write = thread_add_write(vty_master,
                              file->action.write.thread, file->info, file->fd) ;
        } ;
    }
  else
    {
      if (vty_cli_nexus)
        {
          qps_disable_modes(file->qf, qps_write_mbit) ;
        }
      else
        {
          if (file->t_write != NULL)
            thread_cancel (file->t_write) ;
        } ;
    } ;

  uty_file_restart_timer(file) ;
} ;

/*------------------------------------------------------------------------------
 * Close given vty file for reading.
 *
 * Sets timer to timeout for clearing any pending output.
 */
static void
uty_file_half_close(vio_file file)
{
  VTY_ASSERT_LOCKED() ;

  if (file->fd >= 0)
    {
      shutdown(file->fd, SHUT_RD) ;     /* actual half close    */

      file->v_timeout = 30 ;            /* for output to clear  */
      uty_file_set_read(file, off) ;
    } ;

  file->read_open = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Close given vio_file, completely -- shut down any timer.
 *
 * Structure is cleared of everything except the last error !
 */
static void
uty_file_close(vio_file file)
{
  VTY_ASSERT_LOCKED() ;

  if (file->fd >= 0)
    close(file->fd) ;

  if (vty_cli_nexus && (file->fd >= 0))
    qps_remove_file(file->qf) ;

  if (file->qf != NULL)
    qps_file_free(file->qf) ;

  if (file->t_read != NULL)
    thread_cancel(file->t_write) ;
  if (file->t_write != NULL)
    thread_cancel(file->t_write) ;

  file->fd      = -1 ;
  file->qf      = NULL ;
  file->t_read  = NULL ;
  file->t_write = NULL ;

  file->info              = NULL ;
  file->action.read.anon  = NULL ;
  file->action.write.anon = NULL ;
  file->action.timer.anon = NULL ;

  file->read_open   = 0 ;
  file->write_open  = 0 ;

  if (file->qtr != NULL)
    qtimer_free(file->qtr) ;
  if (file->t_timer != NULL)
    thread_cancel(file->t_timer) ;

  file->v_timeout = 0 ;
  file->qtr       = NULL ;
  file->t_timer   = NULL ;
} ;

/*==============================================================================
 * Reading from the VTY_TERM type file.
 *
 * The select/pselect call-back ends up in uty_read_ready().
 *
 * Note that uty_write_ready() also calls uty_read_ready, in order to kick the
 * current CLI.
 */

/*------------------------------------------------------------------------------
 * Ready to read -> kicking CLI
 *
 * Have two CLI: one (trivial one) when waiting on "--more--",
 *               and the standard one.
 *
 * End up here when there is something ready to be read.
 *
 * Also ends up here when was write_ready and did not block in uty_write.
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
uty_read_ready(vty_io vio)
{
  uty_file_set_read(&vio->file, off) ;  /* restarts timer       */

  /* Execute the required command processor                     */
  if (vio->cmd_wait_more)
    uty_cli_wait_more(vio) ;    /* run "--more--" CLI           */
  else
    uty_cli(vio) ;              /* run standard CLI             */
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to read -> kicking CLI
 */
static void
vty_read_qnexus(qps_file qf, void* file_info)
{
  vty_io vio = file_info;

  VTY_LOCK() ;

  assert((vio->file.fd == qf->fd) && (vio == vio->file.info)) ;

  uty_read_ready(vio) ;

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

  assert(vio->file.fd == THREAD_FD (thread) && (vio == vio->file.info)) ;

  vio->file.t_read = NULL ;     /* implicitly   */
  uty_read_ready(vio);

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

  if (!vio->file.read_open)
    return -1 ;                 /* at EOF if not open           */

  get = read_nb(vio->file.fd, buf, sizeof(buf)) ;
  if      (get > 0)
    keystroke_input(vio->key_stream, buf, get, steal) ;
  else if (get < 0)
    {
      if (get == -1)
        uty_io_error(vio, "read") ;

      vio->file.read_open = 0 ;
      keystroke_input(vio->key_stream, NULL, 0, steal) ;

      get = -1 ;
    } ;

  return get ;
} ;

/*==============================================================================
 * The write file action for VTY_TERM type VTY
 *
 * There are two sets of buffering:
 *
 *   cli -- command line   -- which reflects the status of the command line
 *
 *   cmd -- command output -- which is not written to the file while
 *                            cmd_in_progress.
 *
 * The cli output takes precedence.
 *
 * Output of command stuff is subject to line_control, and may go through the
 * "--more--" mechanism.
 */

static bool uty_write(vty_io vio) ;
static int uty_flush_fifo(vty_io vio, vio_fifo vf,
                                        struct vty_line_control* line_control) ;
static void uty_empty_out_fifos(vty_io vio) ;

/*------------------------------------------------------------------------------
 * Flush as much as possible of what there is.
 *
 * May end up with:
 *
 *   * something in the buffers waiting to go, but output is currently
 *     threatening to block.
 *
 *     in this case will have set write on, and things will progress when next
 *     write_ready.
 *
 *   * otherwise:
 *
 *     will be set write off, so does a read_ready in order t kick the CLI,
 *     which may wish to set either read or write on.
 */
static void
uty_write_ready(vty_io vio)
{
  bool blocked ;

  VTY_ASSERT_LOCKED() ;

  uty_file_set_write(&vio->file, off) ; /* restarts timer, too          */

  blocked = uty_write(vio) ;

  if (!blocked)
    uty_read_ready(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to write -> try to empty buffers
 */
static void
vty_write_qnexus(qps_file qf, void* file_info)
{
  vty_io vio  = file_info ;

  VTY_LOCK() ;

  assert((vio->file.fd == qf->fd) && (vio == vio->file.info)) ;

  uty_write_ready(vio) ;

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

  assert(vio->file.fd == THREAD_FD (thread) && (vio == vio->file.info)) ;

  vio->file.t_write = NULL;     /* implicitly   */
  uty_write_ready(vio) ;

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
 * Sets write on if prevented from outputting everything available for output
 * by write() threatening to block.
 *
 * Sets read on if enters cmd_wait_more state.
 *
 * Returns: true <=> blocked by I/O
 *
 * Note that this means that returns true iff sets write on.
 */
static bool
uty_write(vty_io vio)
{
  int ret ;

  VTY_ASSERT_LOCKED() ;

  /* empty the CLI FIFO
   *
   * NB: if the file is !write_open, or if it fails during output here
   *     and becomes !write_open, then ret == 0 -- as if everything
   *     has been written.
   */
  ret = uty_flush_fifo(vio, &vio->cli_obuf, NULL) ;
  if (ret != 0)
    return 1 ;          /* blocked by I/O       */

  if ((vio->cmd_in_progress) || (vio->cmd_wait_more))
    return 0 ;          /* not blocked by I/O   */

  /* write from the command FIFO
   *
   *  NB: if the file is !write_open, or if it fails during output here
   *     and becomes !write_open, then ret == 0 -- as if everything
   *     has been written.
   */
  ret = uty_flush_fifo(vio, &vio->cmd_obuf, &vio->line_control) ;
  if (ret == 1)
    return 1 ;          /* blocked by I/O       */

  if (ret == 2)
    {
      /* Want now to wait for "--more--"
       *
       * Note that this produces CLI output, which must deal with here.
       */
      uty_cli_want_more(vio) ; /* NB: sets cmd_wait_more                */

      ret = uty_flush_fifo(vio, &vio->cli_obuf, NULL) ;
      if (ret == 1)
        return 1 ;      /* blocked by I/O       */

      if (vio->file.write_open)
        return 0 ;      /* not blocked by I/O   */
    } ;

  /* Reach here iff both CLI and command FIFOs are empty and is not
   * cmd_in_progress
   */
  vio->cli_blocked = 0 ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Flush the given FIFO to output -- subject to possible line control.
 *
 * If ends up needing to write more, sets write on.
 *
 * Returns: 0 => written everything there is -- or not (now) write_open
 *          1 => written everything that could -- needs to write more
 *          2 => written everything that could -- needs a "--more--"
 */
static int
uty_flush_fifo(vty_io vio, vio_fifo vf, struct vty_line_control* line_control)
{
  char*   src ;
  size_t  have ;
  int     done ;
  bool    wait_more ;

  if (!vio->file.write_open)
    {
      uty_empty_out_fifos(vio) ;
      return 0 ;
    } ;

  wait_more = 0 ;

  while ((src = vio_fifo_get_lump(vf, &have)) != NULL)
    {
      if (line_control != NULL) /* TODO: line control           */
        {
          /* Account for what happens if output have bytes from src...
           * ... and if necessary reduce "have".
           *
           * set wait_more if now need to wait
           */
        } ;

      done = write_nb(vio->file.fd, src, have) ;

      if (done < 0)
        {
          uty_io_error(vio, "write") ;

          vio->file.write_open = 0 ;
          uty_empty_out_fifos(vio) ;
          return 0 ;            /* no longer open               */
        }

      vio_fifo_got_upto(vf, src + done) ;

      if (done < (int)have)
        {
          if (line_control != NULL)
            {
              /* "put back" have - done bytes for next time     */
            } ;

          uty_file_set_write(&vio->file, on) ;
          return 1 ;            /* output is full               */
        } ;

      /* If now wants to wait for a "--more--", then exit
       *
       * Note that the line_control cannot tell if the place it wants to
       * stop is, in fact, the end of the FIFO -- can only tell that
       * now...
       */
      if (wait_more)
        return vio_fifo_empty(vf) ? 0 : 2 ;
    } ;

  return 0 ;                    /* all gone                     */
} ;

/*------------------------------------------------------------------------------
 * Empty the output FIFOs
 *
 * This is for use when the output has failed or is closed.
 */
static void
uty_empty_out_fifos(vty_io vio)
{
  vio_fifo_set_empty(&vio->cli_obuf) ;
  vio_fifo_set_empty(&vio->cmd_obuf) ;

  vio->cmd_wait_more = 0 ;
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

  uty_half_close(vio) ;                 /* bring input side to a halt   */

  vio->timed_out = 1 ;                  /* why stopped                  */
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

  vio->file.t_timer = NULL ;    /* implicitly   */

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

  struct vio_file file ;
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
      uty_file_close(&listener->file) ; /* no ceremony, no flowers      */
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
      fprintf (stderr, "getaddrinfo failed: %s\n", gai_strerror (ret));
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
                                                         safe_strerror(errno));
      umask (old_mask);
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
    uzlog(NULL, LOG_ERR, "Cannot bind path %s: %s", path, safe_strerror(errno));

  if (ret >= 0)
    ret = set_nonblocking(sock);

  if (ret >= 0)
    {
      ret = listen (sock, 5);
      if (ret < 0)
        uzlog(NULL, LOG_ERR, "listen(fd %d) failed: %s", sock,
                                                          safe_strerror(errno));
    } ;

  zprivs_get_ids(&ids);

  if (ids.gid_vty > 0)
    {
      /* set group of socket */
      if ( chown (path, -1, ids.gid_vty) )
        {
          uzlog (NULL, LOG_ERR, "uty_serv_vtysh: could chown socket, %s",
                                                        safe_strerror (errno) );
        }
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
  uty_file_init_new(&listener->file, fd, listener) ;

  listener->type = type ;

  if (vty_cli_nexus)
    listener->file.action.read.qnexus = vty_accept_qnexus ;
  else
    listener->file.action.read.thread = vty_accept_thread ;

  uty_file_set_read(&listener->file, on) ;
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

  uty_file_set_read(&listener->file, on) ;

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

  assert(listener->file.fd == listen_sock) ;

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
  int sock;
  union sockunion su;
  int ret;
  unsigned int on;
  struct prefix *p ;
  char buf[SU_ADDRSTRLEN] ;

  VTY_ASSERT_LOCKED() ;

  /* We can handle IPv4 or IPv6 socket.                                 */
  sockunion_init_new(&su, 0) ;

  sock = sockunion_accept (listener->file.fd, &su);

  if (sock < 0)
    {
      if (sock == -1)
        uzlog (NULL, LOG_WARNING, "can't accept vty socket : %s",
                                                         safe_strerror (errno));
      return -1;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock) ;     /* issues WARNING if fails      */
  if (ret < 0)
    {
      close(sock) ;
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
      uzlog (NULL, LOG_INFO, "Vty connection refused from %s",
                                    sockunion2str (&su, buf, sizeof(buf)));
      close (sock);
      return 0;
    } ;

  /* Final options (optional)                                           */
  on = 1 ;
  ret = setsockopt (sock, IPPROTO_TCP, TCP_NODELAY,
                                                    (void*)&on, sizeof (on));
  if (ret < 0)
    uzlog (NULL, LOG_INFO, "can't set sockopt to sock %d: %s",
                                          (int)sock, safe_strerror (errno));

  /* All set -- create the VTY_TERM                                     */
  uty_new_term(sock, &su);

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)",
                              sockunion2str (&su, buf, sizeof(buf)), sock);

  return 0;
}

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_SHELL_SERV
 */
static int
uty_accept_shell_serv (vty_listener listener)
{
  int sock ;
  int ret ;
  int client_len ;
  struct sockaddr_un client ;

  VTY_ASSERT_LOCKED() ;

  client_len = sizeof(client);
  memset (&client, 0, client_len);

  sock = accept(listener->file.fd, (struct sockaddr *) &client,
                                         (socklen_t *) &client_len) ;

  if (sock < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't accept vty shell socket : %s",
                                                        safe_strerror (errno));
      return -1;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock) ;     /* issues WARNING if fails      */
  if (ret < 0)
    {
      close(sock) ;
      return -1 ;
    } ;

  /* All set -- create the VTY_SHELL_SERV                               */
  if (VTYSH_DEBUG)
      printf ("VTY shell accept\n");

  uty_new_shell_serv(sock) ;

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty shell connection (fd %d)", sock);
  return 0;
}

/*==============================================================================
 * Reading from the VTY_SHELL_SERV type file.
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
  uty_file_set_read(&vio->file, off) ;

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

  assert((vio->file.fd == qf->fd) && (vio == vio->file.info)) ;

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

  assert(vio->file.fd == THREAD_FD (thread) && (vio == vio->file.info)) ;

  vio->file.t_read = NULL ;     /* implicitly   */
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
 * When empties "buf", reads a lump from the file.
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

      if (!vio->file.read_open)
        return -1 ;             /* at EOF if not open     <<<<<<<<<<<<< */

      qs_need(buf, 500) ;       /* need a reasonable lump               */
      qs_set_empty(buf) ;       /* set cp = len = 0                     */

      get = read_nb(vio->file.fd, buf->body, buf->size) ;
      if      (get > 0)
        buf->len = get ;
      else if (get == 0)
        return 0 ;              /* have an incomplete line <<<<<<<<<<<< */
      else
        {
          if (get == -1)
            uty_io_error(vio, "read") ;

          vio->file.read_open = 0 ;

          return -1 ;           /* at EOF or failed       <<<<<<<<<<<<< */
        } ;
    } ;
} ;

/*==============================================================================
 * Output to vty which are set to "monitor".
 *
 * If there is something in the command FIFO and command is not in progress,
 * then throw logging away -- console is busy dealing with the output from
 * some command.
 *
 * Wipes the command line and flushes the output.  If the CLI FIFO is now
 * empty, add the logging line to it and flush.  Enable read, so that the
 * CLI will be reentered, and the command line restored in due course.
 */

/*------------------------------------------------------------------------------
 * Output logging information to all vty which are set to "monitor".
 */
extern void
uty_log(struct logline* ll, struct zlog *zl, int priority,
                                                 const char *format, va_list va)
{
  vty_io  vio ;
  vty_io  next ;

  VTY_ASSERT_LOCKED() ;

  next = sdl_head(vio_monitors_base) ;

  if (next == NULL)
    return ;                    /* go no further if no "monitor" vtys   */

  /* Prepare line for output.                                           */
  uvzlog_line(ll, zl, priority, format, va, 1) ;  /* with crlf          */

  /* write to all known "monitor" vty
   *
   * While writing to a given "monitor" the monitor flag is cleared.  This
   * means that if the write fails, and logs a message, then will recurse
   * through here -- but won't log to the monitor that has failed.
   *
   * If one of the other monitors fails during this process, will recurse
   * again, now with two monitors with their monitor flags cleared.
   *
   * Once the output (and any recursed output) has completed, then the
   * monitor flag is restored -- but only if the vty is still write_open.
   *
   * A monitor that is not write_open at the end of this, is removed from the
   * monitors list.  The current vio *cannot* be the current vio at a higher
   * level in any recursion stack, because... if anything higher up the stack
   * will have their monitor flag cleared, and therefore have been stepped
   * over at the current level.
   */
  while (next != NULL)
    {
      vio = next ;

      if ( vio->monitor          /* may be temporarily not a monitor     */
          && (vio->cmd_in_progress || vio_fifo_empty(&vio->cmd_obuf)) )
        {
          vio->monitor = 0 ;    /* avoid recursion                      */

          uty_cli_wipe(vio) ;
          uty_write(vio) ;

          if (vio_fifo_empty(&vio->cli_obuf) && vio->file.write_open)
            {
              vio_fifo_put(&vio->cli_obuf, ll->line, ll->len) ;
              uty_write(vio) ;
            } ;

          uty_file_set_read(&vio->file, on) ;

          /* It is possible that something failed, so that is no longer
           * write_open, and should no longer be a monitor.
           */
          vio->monitor = vio->file.write_open ;
        } ;

      next = sdl_next(vio, mon_list) ;

      /* take self off list if no onger a monitor               */
      if (!vio->monitor)
        sdl_del(vio_monitors_base, vio, mon_list) ;
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
      write(vio->file.fd, buf, len) ;
      write(vio->file.fd, "\r\n", 2) ;

      vio = sdl_next(vio, mon_list) ;
    } ;
} ;

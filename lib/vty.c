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

#include "vty.h"
#include "vty_local.h"
#include "vty_io.h"
#include "vty_io_file.h"
#include "vty_command.h"
#include "vty_cli.h"
#include "vty_log.h"
#include "vio_fifo.h"
#include "log_local.h"

#include "list_util.h"

#include "command.h"
#include "command_local.h"
#include "command_execute.h"
#include "command_parse.h"
#include "memory.h"
#include "mqueue.h"
#include "qstring.h"
#include "qpath.h"
#include "network.h"

/*==============================================================================
 * The vty family comprises:
 *
 *   vty          -- level visible from outside the vty/command/log family
 *                   and within those families.
 *
 *   vty_common.h -- definitions ...
 *   vty_local.h
 *
 *   vty_io       -- top level of the vio handling
 *
 *   vty_command  -- functions called by the command family
 *   vty_log      -- functions called by the log family
 *
 *   vty_cli      -- terminal command line handling
 *   vty_io_term  -- terminal (telnet) I/O
 *   vty_io_vsh   -- vtysh I/O
 *   vty_io_file  -- file I/O
 *   vty_io_shell -- system shell I/O
 *
 *   vty_io_basic -- common low level I/O handling
 *                   encapsulates the differences between qpselect and legacy
 *                   thread/select worlds.
 *
 *   vio_lines    -- for terminal: handles width, CRLF, line counting etc.
 *   vio_fifo     --
 *   qiovec
 *
 */


/*==============================================================================
 * Variables etc. (see vty_local.h)
 */

/* The mutex and related debug counters                                 */
qpt_mutex_t vty_mutex ;

int vty_lock_count  = 0 ;

#if VTY_DEBUG
int vty_assert_fail = 0 ;
#endif

/* For thread handling -- initialised in vty_init                       */
struct thread_master* vty_master = NULL ;

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

qpn_nexus vty_cli_nexus    = NULL ;
qpn_nexus vty_cmd_nexus    = NULL ;

/* List of all known vio                                                */
vty_io vio_live_list       = NULL ;

/* List of all vty which are in monitor state.                          */
vty_io vio_monitor_list    = NULL ;

/* List of all vty which are on death watch                             */
vty_io vio_death_watch     = NULL ;

/* List of child processes in our care                                  */
vio_child vio_childer_list = NULL ;

/* See vty_child_signal_nexus_set()                                     */
qpt_mutex_t vty_child_signal_mutex ;
qpn_nexus vty_child_signal_nexus = NULL ;

/*------------------------------------------------------------------------------
 * VTYSH stuff
 */

/* Integrated configuration file path -- for VTYSH                      */
char integrate_default[] = SYSCONFDIR INTEGRATE_DEFAULT_CONFIG ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void uty_reset (bool final, const char* why) ;
static void uty_init_commands (void) ;

//static bool vty_terminal (struct vty *);
//static bool vty_shell_server (struct vty *);
//static bool vty_shell_client (struct vty *);

/*------------------------------------------------------------------------------
 * Tracking the initialisation state.
 */
enum vty_init_state
{
  vty_init_pending     = 0,     /* first and lowest numbered state      */
  vty_init_1st_stage,
  vty_init_2nd_stage,
  vty_init_started,
  vty_init_reset,
  vty_init_terminated           /* final and highest numbered state     */
};

static enum vty_init_state vty_init_state ;

/*==============================================================================
 * Public Interface
 */

/*------------------------------------------------------------------------------
 * Initialise vty handling (threads and pthreads)
 *
 * Install vty's own commands like `who' command.
 *
 * This runs before any pthreads or nexus stuff starts -- so is, implicitly,
 * in the CLI thread.
 *
 * NB: may be called once and once only.
 */
extern void
vty_init (struct thread_master *master_thread)
{
  VTY_ASSERT_CLI_THREAD() ;     /* True if !qpthreads_enabled           */
  VTY_LOCK() ;                  /* Does nothing if !qpthreads_enabled   */

  assert(vty_init_state == vty_init_pending) ;

  vty_master = master_thread;   /* Local pointer to the master thread   */

  vio_live_list       = NULL ;  /* no VTYs yet                          */
  vio_death_watch     = NULL ;
  vio_childer_list    = NULL ;

  vty_nexus           = false ; /* not running qnexus-wise              */
  vty_multi_nexus     = false ; /* not more than one thread either      */
  vty_cli_nexus       = NULL ;
  vty_cmd_nexus       = NULL ;

  vty_child_signal_nexus = NULL ;       /* none, yet                    */

  uty_watch_dog_init() ;        /* empty watch dog                      */

  uty_init_monitor() ;

  uty_init_commands() ;         /* install nodes                        */

  vty_init_state = vty_init_1st_stage ;

  VTY_UNLOCK() ;
}

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

  qpt_mutex_init(vty_mutex, qpt_mutex_recursive);

  qpt_mutex_init(vty_child_signal_mutex, qpt_mutex_quagga);

  vty_init_state = vty_init_2nd_stage ;
} ;

/*------------------------------------------------------------------------------
 * Initialisation for vtysh application.
 *
 * TODO: work out what this needs to do !  (If anything.)
 */
extern void
vty_init_vtysh (void)
{
  VTY_LOCK() ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Start the VTY going.
 *
 * This starts the listeners for VTY_TERMINAL and VTY_SHELL_SERVER.
 *
 * Also starts the watch dog.
 *
 * This is run during early morning start, after any daemonisation, but before
 * any threads are started -- so is, implicitly, in the CLI thread.
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

  uty_open_listeners(addr, port, path) ;

  vty_init_state = vty_init_started ;
  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Reset all VTY status for reasons unknown -- probably SIGHUP
 */
extern void
vty_reset()
{
  vty_reset_because("Reset") ;
}

/*------------------------------------------------------------------------------
 * Reset all VTY status
 *
 * This is done in response to SIGHUP/SIGINT/SIGTERM -- and runs in the
 * CLI thread (if there is one).
 *
 * Closes all VTY, leaving the death watch to tidy up once all output and any
 * command in progress have completed.
 *
 * Closes all listening sockets.
 *
 * TODO: revoke ?
 *
 * NB: old code discarded all output and hard closed all the VTY...
 */
extern void
vty_reset_because(const char* why)
{
  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  if (vty_init_state != vty_init_reset)
    {
      assert(vty_init_state == vty_init_started) ;

      uty_reset(false, why) ;   /* not final !  */

      vty_init_state = vty_init_reset ;
    } ;
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Restart the VTY, following a vty_reset().
 *
 * This starts the listeners for VTY_TERMINAL and VTY_SHELL_SERVER, again.
 *
 * NB: may be called once, and once only, *after* a vty_reset().
 *
 * NB: need not be in the CLI thread (if any).
 */
struct vty_restart_args
{
  char*          addr ;
  unsigned short port ;
  char*          path ;
} ;
MQB_ARGS_SIZE_OK(vty_restart_args) ;

static void vty_restart_action(mqueue_block mqb, mqb_flag_t flag) ;
static void uty_restart(const char *addr, unsigned short port,
                                                             const char *path) ;
extern void
vty_restart(const char *addr, unsigned short port, const char *path)
{
  VTY_LOCK() ;

  /* If not running qnexus-wise, call uty_restart directly.
   *
   * Otherwise, construct and dispatch message to do a uty_restart.
   */
  if (!vty_nexus)
    uty_restart(addr, port, path) ;
  else
    {
      mqueue_block mqb ;
      struct vty_restart_args* args ;

      mqb  = mqb_init_new(NULL, vty_restart_action, vty_cli_nexus) ;
      args = mqb_get_args(mqb) ;

      if (addr != NULL)
        args->addr = XSTRDUP(MTYPE_TMP, addr) ;
      else
        args->addr = NULL ;

      args->port = port ;

      if (path != NULL)
        args->path = XSTRDUP(MTYPE_TMP, path) ;
      else
        args->path = NULL ;

      mqueue_enqueue(vty_cli_nexus->queue, mqb, mqb_priority) ;
    } ;

  VTY_UNLOCK() ;
} ;

/* Deal with the uty_restart message                                    */
static void
vty_restart_action(mqueue_block mqb, mqb_flag_t flag)
{
  struct vty_restart_args* args ;
  args = mqb_get_args(mqb) ;

  if (flag == mqb_action)
    {
      VTY_LOCK() ;

      uty_restart(args->addr, args->port, args->path) ;

      VTY_UNLOCK() ;
    } ;

  if (args->addr != NULL)
    XFREE(MTYPE_TMP, args->addr) ;
  if (args->path != NULL)
    XFREE(MTYPE_TMP, args->path) ;
} ;

/* Do the actual restart                                                */
static void
uty_restart(const char *addr, unsigned short port, const char *path)
{
  VTY_ASSERT_LOCKED() ;
  assert(vty_init_state == vty_init_reset) ;

  uty_open_listeners(addr, port, path) ;

  vty_init_state = vty_init_started ;
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
  VTY_ASSERT_CLI_THREAD() ;

  if (  (vty_init_state == vty_init_pending)
     || (vty_init_state == vty_init_terminated)  )
    return ;                            /* nothing to do !      */

  VTY_LOCK() ;

  assert(  (vty_init_state > vty_init_pending)
        && (vty_init_state < vty_init_terminated)  ) ;

  uty_reset(true, "Shut down") ;        /* final reset          */

  vty_child_close_register() ;

  VTY_UNLOCK() ;

  qpt_mutex_destroy(vty_mutex, 0);
  qpt_mutex_destroy(vty_child_signal_mutex, 0);

  vty_init_state = vty_init_terminated ;
}

/*------------------------------------------------------------------------------
 * Reset -- for SIGHUP or at final curtain.
 *
 * For SIGHUP is called via vty_reset_because(), and is sitting in the
 * vty_cli_nexus (if pthreaded) with the message queues still running.
 *
 * For final curtain will
 *
 *
 *  is called by vty_terminate(), by which time all qnexus
 * have been shut down, so no message queues and no timers etc, are running.
 *
 *
 * Closes listeners.
 *
 * Revokes any outstanding commands and close (SIGHUP) or close_final
 * (curtains) all VTY.
 *
 *
 *
 * Resets the vty timeout and access lists.
 *
 * When reach final reset it should not be possible for there to be any
 * commands still in progress.  If there are, they are simply left on the
 * death-watch list... there is no pressing need to do anything more radical,
 * and the presence of anything on the death watch is grounds for some debug
 * activity !
 */
static void
uty_reset (bool curtains, const char* why)
{
  vty_io vio ;
  vty_io next ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  uty_close_listeners() ;

  next = sdl_head(vio_live_list) ;
  while (next != NULL)
    {
      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      uty_close(vio, why, curtains) ;
    } ;

  host.vty_timeout_val = VTY_TIMEOUT_DEFAULT;

  XFREE(MTYPE_HOST, host.vty_accesslist_name) ;
                                /* sets host.vty_accesslist_name = NULL */

  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
                           /* sets host.vty_ipv6_accesslist_name = NULL */

  if (curtains)
    uty_watch_dog_stop() ;      /* and final death watch run    */
} ;

/*==============================================================================
 * Opening and closing VTY.
 *
 * VTY without a socket may be opened and closed at will.
 *
 * TODO: sort out the relationship between the non-socket VTY and vty_reset()
 */

/*------------------------------------------------------------------------------
 * Create a new VTY of the given type
 *
 * The type may NOT be: VTY_TERMINAL or VTY_SHELL_SERVER
 *
 * Appears only to be used by vtysh !!     TODO ????
 */
extern vty
vty_open(vty_type_t type, node_type_t node)
{
  struct vty* vty ;

  VTY_LOCK() ;
  vty = uty_new(type, node) ;
  VTY_UNLOCK() ;

  return vty ;
} ;

/*==============================================================================
 * General VTY output.
 *
 * This is used during command execution, to output the results of commands.
 *
 * All these end up in uty_vout -- see vty_io.
 */

/*------------------------------------------------------------------------------
 * VTY output -- cf fprintf !
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
 * VTY output -- output the current time in standard form, to the second.
 */
extern void
vty_time_print (struct vty *vty, int cr)
{
  char buf [timestamp_buffer_len];

  quagga_timestamp(0, buf, sizeof(buf)) ;

  if (cr)
    vty_out(vty, "%s\n", buf);
  else
    vty_out(vty, "%s ", buf);

  return;
}

/*------------------------------------------------------------------------------
 * Say hello to vty interface.
 */
extern void
vty_hello (struct vty *vty)
{
  qpath       path ;
  const char* string ;

  VTY_LOCK() ;

  path = (host.motdfile != NULL) ? qpath_dup(host.motdfile) : NULL ;
  string = host.motd ;

  VTY_UNLOCK() ;

  if (qdebug)
    vty_out (vty, "%s\n", debug_banner);

  if      (path != NULL)
    vty_cat_file(vty, path, "motd file") ;
  else if (string != NULL)
    vty_out(vty, "%s", string);

  /* This "\n" is a bit magic... if the motd file does not end in a "\n",
   * then this makes sure that we start on a new line.
   *
   * Similarly, if the motd string doesn't end '\n', then this makes sure.
   *
   * This will also trim trailing space from the end of the motd message.
   *
   * Generally these will end in '\n', so this produces the extra blank line
   * before the cheerful "User authentication" message, which is the most
   * likely next line.
   */
  vty_out(vty, "\n") ;

  qpath_free(path) ;
} ;

/*------------------------------------------------------------------------------
 * "cat" file to vty
 */
extern cmd_return_code_t
vty_cat_file(vty vty, qpath path, const char* desc)
{
  int   fd ;
  void* buf ;

  fd = uty_vfd_file_open(qpath_string(path), vfd_io_read | vfd_io_blocking) ;

  if (fd < 0)
    {
      vty_out (vty, "Cannot open %s file '%s': %s (%s)\n", desc,
                                  qpath_string(path), errtostr(errno, 0).str,
                                                     errtoname(errno, 0).str) ;
      return CMD_WARNING;
    } ;

  enum { buffer_size = 64 * 1024 } ;
  buf = XMALLOC(MTYPE_TMP, buffer_size) ;

  while (1)
    {
      int r ;

      r = readn(fd, buf, buffer_size) ;

      if (r > 0)
        vty_write(vty, buf, r) ;        // TODO push ??
      else
        {
          if (r == 0)
            break ;

          // TODO error handling ....
        } ;
    } ;

  close(fd) ;

  return CMD_SUCCESS;
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
 * Sets up a VTY_CONFIG_READ in which to execute commands.  This has no CLI
 * and no socket.  All output is buffered in the cmd_obuf.  All commands are
 * run directly in the thread -- no commands are queued.
 */

static int vty_use_backup_config (qpath path) ;
static void vty_read_config_file (int conf_fd, const char* name,
                                  cmd_command first_cmd, bool ignore_warnings,
                                                                bool full_lex) ;

/*------------------------------------------------------------------------------
 * Read the given configuration file.
 */
extern void
vty_read_config (const char *config_file,
                 const char *config_default)
{
  vty_read_config_first_cmd_special(config_file, config_default, NULL, true);
}

/*------------------------------------------------------------------------------
 * Read the given configuration file.
 *
 * The config_file (-f argument) is used if specified.
 *
 * If config_file is NULL, use the config_default.
 *
 * If using the config_default, if VTYSH_ENABLED, look for "vtysh" in the name.
 * If find "vtysh" and find the "integrate_default" file, then do nothing
 * now -- expect vtysh to connect in due course and provide the configuration.
 *
 * The config_file or config_default may be relative file names.
 *
 * May have a function to call after the first actual command is processed.
 * This mechanism supports the setting of qpthreads-ness by configuration file
 * command.
 */
extern void
vty_read_config_first_cmd_special(const char *config_file,
                                  const char *config_default,
                                  cmd_command first_cmd,
                                  bool ignore_warnings)
{
  const char *name ;
  qpath  path ;
  int    conf_fd ;

  /* Deal with VTYSH_ENABLED magic                                      */
  if (VTYSH_ENABLED && (config_file == NULL))
    {
      int ret;
      struct stat conf_stat;

      /* !!!!PLEASE LEAVE!!!!
       * This is NEEDED for use with vtysh -b, or else you can get
       * a real configuration food fight with a lot garbage in the
       * merged configuration file it creates coming from the per
       * daemon configuration files.  This also allows the daemons
       * to start if there default configuration file is not
       * present or ignore them, as needed when using vtysh -b to
       * configure the daemons at boot - MAG
       */

      /* Stat for vtysh Zebra.conf, if found startup and wait for
       * boot configuration
       */

      if ( strstr(config_default, "vtysh") == NULL)
        {
          ret = stat (integrate_default, &conf_stat);
          if (ret >= 0)
            return;             /* TODO leaves host.config_file NULL    */
        }
    } ;

  /* Use default if necessary, and deal with constructing full path     */
  if (config_file == NULL)
    config_file = config_default ;

  path = qpath_make(config_file, host.cwd) ;
  name = qpath_string(path) ;

  /* try to open the configuration file                                 */
  conf_fd = uty_vfd_file_open(name, vfd_io_read) ;

  if (conf_fd < 0)
    {
      fprintf (stderr, "%s: failed to open configuration file %s: %s\n",
                                      __func__, name, errtostr(errno, 0).str) ;

      conf_fd = vty_use_backup_config (path);
      if (conf_fd >= 0)
        fprintf (stderr, "WARNING: using backup configuration file!\n");
      else
        {
          fprintf (stderr, "can't open backup configuration file [%s%s]\n",
                                                        name, CONF_BACKUP_EXT);
          exit(1);
        }
    } ;

  if (qdebug)
    fprintf(stderr, "Reading config file: %s\n", name);

  vty_read_config_file(conf_fd, name, first_cmd, ignore_warnings, false);

  cmd_host_config_set(path) ;

  qpath_free(path) ;

  if (qdebug)
    fprintf(stderr, "Finished reading config file\n");
}

/*------------------------------------------------------------------------------
 * Try to use a backup configuration file.
 *
 * Having failed to open the file "<path>", if there is a file called
 * "<path>.sav" that can be opened for reading, then:
 *
 *   - make a copy of that file
 *   - call it "<path>"
 *   - return an open FILE
 *
 * Returns: <  0 => no "<path>.sav", or failed doing any of the above
 *          >= 0 otherwise, fd file.
 */
static int
vty_use_backup_config (qpath path)
{
  qpath       temp ;
  char*       name ;
  int         sav_fd, tmp_fd, err ;
  bool        ok ;

  /* construct the name "<fullname>.sav", and try to open it.           */
  temp = qpath_dup(path) ;
  qpath_extend_str(temp, CONF_BACKUP_EXT) ;

  sav_fd = -1 ;
  tmp_fd = -1 ;

  sav_fd = uty_vfd_file_open(qpath_string(temp),
                                                vfd_io_read | vfd_io_blocking) ;

  /* construct a temporary file and copy "<fullpath.sav>" to it.        */
  qpath_extend_str(temp, ".XXXXXX") ;
  name = qpath_char_string(temp) ;

  if (sav_fd >= 0)
    tmp_fd = mkstemp(name) ;

  ok = ((sav_fd >= 0) && (tmp_fd >= 0)) ;

  if (ok)
    ok = (copyn(tmp_fd, sav_fd) == 0) ;

  err = errno ;

  if (tmp_fd >= 0)
    close(tmp_fd) ;
  if (sav_fd >= 0)
    close(sav_fd) ;

  /* If now OK, then have copied the .sav to the temporary file.        */

  if (ok)
    {
      /* Make sure that have the required file status                   */
      ok = chmod(name, CONFIGFILE_MASK) == 0 ;

      /* Finally, make a link with the original name                    */
      if (ok)
        ok = link(name, qpath_string(path)) == 0 ;

      err = errno ;
    } ;

  if (tmp_fd >= 0)      /* if made a temporary, done with it now        */
    unlink(name) ;

  qpath_free(temp) ;    /* done with the qpath                          */

  if (ok)
    return uty_vfd_file_open(qpath_string(path), vfd_io_read) ;

  errno = err ;
  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Read the given configuration file.
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
 */
static void
vty_read_config_file (int fd, const char* name, cmd_command first_cmd,
                                            bool ignore_warnings, bool full_lex)
{
  cmd_return_code_t ret ;
  vty     vty ;

  vty = vty_config_read_open(fd, name, full_lex) ;

  vty_cmd_loop_prepare(vty) ;

  zlog_info("Started reading configuration: %s", name) ;

  ret = cmd_read_config(vty, first_cmd, ignore_warnings) ;

  zlog_info("Finished reading configuration%s",
                                (ret == CMD_SUCCESS) ? "." : " -- FAILED") ;

  vty_cmd_loop_exit(vty) ;

  if (ret != CMD_SUCCESS)
    exit(1) ;
} ;

/*------------------------------------------------------------------------------
 * Push the given fd as the VOUT_CONFIG.
 *
 * Note that this is a "blocking" vf, so can open and close in the cmd thread.
 */
extern void
vty_open_config_write(vty vty, int fd)
{
  vty_io vio ;
  vio_vf vf ;

  VTY_LOCK() ;

  vio = vty->vio ;

  vf = uty_vf_new(vio, "config write", fd, vfd_file,
                                                vfd_io_read | vfd_io_blocking) ;
  uty_vout_push(vio, vf, VOUT_CONFIG, NULL, NULL, 32 * 1024) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Write away any pending stuff, and pop the VOUT_CONFIG.
 */
extern cmd_return_code_t
vty_close_config_write(struct vty* vty, bool final)
{
  cmd_return_code_t ret ;
  VTY_LOCK() ;

  ret = uty_vout_pop(vty->vio, final) ;

  VTY_UNLOCK() ;

  return ret ;
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
  vty_io vio ;

  VTY_LOCK() ;

  vio = vio_live_list ;         /* once locked                          */

  while (vio != NULL)           /* TODO: show only VTY_TERM ???         */
    {
      vty_out(vty, "%svty[%d] connected from %s.\n",
	             vio->vty->config ? "*" : " ", i, uty_get_name(vio)) ;
      vio = sdl_next(vio, vio_list) ;
    } ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/* Move to vty configuration mode. */
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
 * Set time out value.
 *
 * Affects this and any future VTY_TERMINAL or VTY_SHELL_SERVER type VTY.
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

/* Set vty access class. */
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
  cmd_return_code_t ret ;

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
  cmd_return_code_t ret ;

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

/* vty login. */
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

/* initial mode. */
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

DEFUN_CALL (terminal_monitor,
       terminal_monitor_cmd,
       "terminal monitor",
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")
{
  VTY_LOCK() ;
  uty_set_monitor(vty->vio, true);
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
  uty_set_monitor(vty->vio, false);
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

ALIAS_CALL (terminal_no_monitor,
       no_terminal_monitor_cmd,
       "no terminal monitor",
       NO_STR
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")

DEFUN_CALL (show_history,
       show_history_cmd,
       "show history",
       SHOW_STR
       "Display the session command history\n")
{
  VTY_LOCK() ;

  if (vty->type == VTY_TERMINAL)
    uty_cli_hist_show(vty->vio->vin->cli) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*==============================================================================
 * Output the current configuration
 *
 * Returns: CMD_SUCCESS
 */
static int
vty_config_write (struct vty *vty)
{
  vty_io vio ;

  VTY_LOCK() ;                  /* while accessing the host.xxx         */

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

void
vty_set_lines(struct vty *vty, int lines)
{
  VTY_LOCK() ;

  if (vty->type == VTY_TERMINAL)
    uty_cli_set_lines(vty->vio->vin->cli, lines, true) ;

  VTY_UNLOCK() ;
}

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

          *q++ = '\n' ;
          *q++ = '\0' ;

          vty_out(vty, buf) ;
        } ;
    } ;

  return CMD_SUCCESS;
}

/*==============================================================================
 * The VTY command nodes
 */

struct cmd_node vty_node =
{
  VTY_NODE,
  "%s(config-line)# ",
  1,
};

/*------------------------------------------------------------------------------
 * Install vty's own commands like `who' command.
 */
static void
uty_init_commands (void)
{
  VTY_ASSERT_LOCKED() ;

  install_node (&vty_node, vty_config_write);

  install_element (VIEW_NODE, &delay_secs_cmd);
  install_element (ENABLE_NODE, &delay_secs_cmd);

  install_element (RESTRICTED_NODE, &config_who_cmd);
  install_element (RESTRICTED_NODE, &show_history_cmd);
  install_element (VIEW_NODE, &config_who_cmd);
  install_element (VIEW_NODE, &show_history_cmd);
  install_element (ENABLE_NODE, &config_who_cmd);
  install_element (CONFIG_NODE, &line_vty_cmd);
  install_element (CONFIG_NODE, &service_advanced_vty_cmd);
  install_element (CONFIG_NODE, &no_service_advanced_vty_cmd);
  install_element (CONFIG_NODE, &show_history_cmd);
  install_element (ENABLE_NODE, &terminal_monitor_cmd);
  install_element (ENABLE_NODE, &terminal_no_monitor_cmd);
  install_element (ENABLE_NODE, &no_terminal_monitor_cmd);
  install_element (ENABLE_NODE, &show_history_cmd);

  install_default (VTY_NODE);
  install_element (VTY_NODE, &exec_timeout_min_cmd);
  install_element (VTY_NODE, &exec_timeout_sec_cmd);
  install_element (VTY_NODE, &no_exec_timeout_cmd);
  install_element (VTY_NODE, &vty_access_class_cmd);
  install_element (VTY_NODE, &no_vty_access_class_cmd);
  install_element (VTY_NODE, &vty_login_cmd);
  install_element (VTY_NODE, &no_vty_login_cmd);
  install_element (VTY_NODE, &vty_restricted_mode_cmd);
  install_element (VTY_NODE, &vty_no_restricted_mode_cmd);
#ifdef HAVE_IPV6
  install_element (VTY_NODE, &vty_ipv6_access_class_cmd);
  install_element (VTY_NODE, &no_vty_ipv6_access_class_cmd);
#endif /* HAVE_IPV6 */
} ;

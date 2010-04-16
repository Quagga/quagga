/* VTY top level
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
#include <stdbool.h>
#include "version.h"

#include "vty_io.h"
#include "vty.h"
#include "uty.h"
#include "vty_cli.h"

#include "list_util.h"

#include "command.h"
#include "command_queue.h"
#include "command_execute.h"
#include "memory.h"
#include "log.h"
#include "mqueue.h"

/*==============================================================================
 * Variables etc. (see uty.h)
 */

/* The mutex and related debug counters                                 */
qpt_mutex_t vty_mutex ;

#if VTY_DEBUG

int vty_lock_count  = 0 ;
int vty_assert_fail = 0 ;

#endif

/* For thread handling -- initialised in vty_init                       */
struct thread_master* vty_master = NULL ;

/* In the qpthreads world, have nexus for the CLI and one for the Routeing
 * Engine.  Some commands are processed directly in the CLI, most have to
 * be sent to the Routeing Engine.
 */
qpn_nexus vty_cli_nexus  = NULL ;
qpn_nexus vty_cmd_nexus  = NULL ;

/* List of all known vio                                                */
vty_io vio_list_base      = NULL ;

/* List of all vty which are in monitor state.                          */
vty_io vio_monitors_base  = NULL ;

/* List of all vty which are on death watch                             */
vty_io vio_death_watch    = NULL ;

/* Vty timeout value -- see "exec timeout" command                      */
unsigned long vty_timeout_val = VTY_TIMEOUT_DEFAULT;

/* Vty access-class command                                             */
char *vty_accesslist_name = NULL;

/* Vty access-class for IPv6.                                           */
char *vty_ipv6_accesslist_name = NULL;

/* Current directory -- initialised in vty_init()                       */
static char *vty_cwd = NULL;

/* Configure lock -- only one vty may be in CONFIG_NODE or above !      */
bool vty_config = 0 ;

/* Login password check override.                                       */
bool no_password_check = 0;

/* Restrict unauthenticated logins?                                     */
const bool restricted_mode_default = 0 ;
      bool restricted_mode         = 0 ;

/* Watch-dog timer.                                                     */
union vty_watch_dog vty_watch_dog = { NULL } ;

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
static void vty_save_cwd (void) ;

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
  VTY_LOCK() ;                  /* Does nothing if !qpthreads_enabled   */
  VTY_ASSERT_CLI_THREAD() ;     /* True if !qpthreads_enabled           */

  assert(vty_init_state == vty_init_pending) ;

  vty_master = master_thread;   /* Local pointer to the master thread   */

  vty_save_cwd ();              /* need cwd for config reading          */

  vio_list_base       = NULL ;  /* no VTYs yet                          */
  vio_monitors_base   = NULL ;
  vio_death_watch     = NULL ;

  vty_cli_nexus       = NULL ;  /* not running qnexus-wise              */
  vty_cmd_nexus       = NULL ;

  vty_watch_dog.anon  = NULL ;  /* no watch dog                         */

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

  vty_cli_nexus = cli ;
  vty_cmd_nexus = cmd ;

  qpt_mutex_init(&vty_mutex, qpt_mutex_recursive);

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
 * This starts the listeners for VTY_TERM and VTY_SHELL_SERV.
 *
 * Also starts the watch dog.
 *
 * This is run during early morning start, after any daemonisation, but before
 * any threads are started -- so is, implicitly, in the CLI thread.
 *
 * NB: may be called once and once only.
 *
 * NB: MUST be in the CLI thread (if any).
 */
extern void
vty_start(const char *addr, unsigned short port, const char *path)
{
  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

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
 * This is done in response to SIGHUP -- and runs in the CLI thread.
 *
 * Half closes all VTY, leaving the death watch to tidy up once all output
 * and any command in progress have completed.
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
  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(vty_init_state == vty_init_started) ;

  uty_reset(0, why) ;   /* not final !  */

  vty_init_state = vty_init_reset ;
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Restart the VTY, following a vty_reset().
 *
 * This starts the listeners for VTY_TERM and VTY_SHELL_SERV, again.
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

static void uty_restart_action(mqueue_block mqb, mqb_flag_t flag) ;
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
  if (!vty_cli_nexus)
    uty_restart(addr, port, path) ;
  else
    {
      mqueue_block mqb ;
      struct vty_restart_args* args ;

      mqb  = mqb_init_new(NULL, uty_restart_action, vty_cli_nexus) ;
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

      mqueue_enqueue(vty_cli_nexus->queue, mqb, 0) ;
    } ;

  VTY_UNLOCK() ;
} ;

/* Deal with the uty_restart message                                    */
static void
uty_restart_action(mqueue_block mqb, mqb_flag_t flag)
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
  if (  (vty_init_state == vty_init_pending)
     || (vty_init_state == vty_init_terminated)  )
    return ;            /* nothing to do !      */

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(  (vty_init_state > vty_init_pending)
        && (vty_init_state < vty_init_terminated)  ) ;

  uty_reset(1, "Shut down") ;   /* final reset          */

  VTY_UNLOCK() ;

  qpt_mutex_destroy(&vty_mutex, 0);

  vty_init_state = vty_init_terminated ;
}

/*------------------------------------------------------------------------------
 * Reset -- final or for SIGHUP
 *
 * Closes listeners.
 *
 * Closes (final) or half closes (SIGHUP) all VTY, and revokes any outstanding
 * commands.
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

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  uty_close_listeners() ;

  next = sdl_head(vio_list_base) ;
  while (next != NULL)
    {
      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      cq_revoke(vio->vty) ;

      if (why != NULL)
        vio->close_reason = why ;

      if (curtains)
        uty_close(vio) ;
      else
        uty_half_close(vio, why) ;
    } ;

  vty_timeout_val = VTY_TIMEOUT_DEFAULT;

  if (vty_accesslist_name)
    {
      XFREE(MTYPE_VTY, vty_accesslist_name);
      vty_accesslist_name = NULL;
    }

  if (vty_ipv6_accesslist_name)
    {
      XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);
      vty_ipv6_accesslist_name = NULL;
    }

  if (curtains && vty_cwd)
    XFREE (MTYPE_TMP, vty_cwd);

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
 * The type may NOT be: VTY_TERM or VTY_SHELL_SERV
 */
extern struct vty *
vty_open(enum vty_type type)
{
  struct vty* vty ;

  VTY_LOCK() ;
  vty = uty_new(type, -1) ;   /* fails for VTY_TERM or VTY_SHELL_SERV   */
  VTY_UNLOCK() ;

  return vty ;
} ;

/*------------------------------------------------------------------------------
 * Close the given VTY
 */
extern void
vty_close (struct vty *vty)
{
  VTY_LOCK() ;
  uty_close(vty->vio) ;
  VTY_UNLOCK() ;
}

/*==============================================================================
 * General VTY output.
 *
 * This is mostly used during command execution, to output the results of the
 * command.
 *
 * All these end up in uty_vout -- see vty_io.
 */

/*------------------------------------------------------------------------------
 * VTY output -- cf fprintf !
 */
extern int
vty_out (struct vty *vty, const char *format, ...)
{
  int result;

  VTY_LOCK() ;
  va_list args;
  va_start (args, format);
  result = uty_vout(vty, format, args);
  va_end (args);
  VTY_UNLOCK() ;
  return result;
}

/*------------------------------------------------------------------------------
 * VTY output -- output a given numnber of spaces
 */

/*                                         1         2         3         4 */
/*                                1234567890123456789012345678901234567890 */
const char vty_spaces_string[] = "                                        " ;
CONFIRM(VTY_MAX_SPACES == (sizeof(vty_spaces_string) - 1)) ;

extern void
vty_out_indent(struct vty *vty, int indent)
{
  while (indent > 0)
    {
      vty_out(vty, VTY_SPACES(indent)) ;
      indent -= VTY_MAX_SPACES ;
    }
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
    vty_out (vty, "%s%s", buf, VTY_NEWLINE);
  else
    vty_out (vty, "%s ", buf);

  return;
}

/*------------------------------------------------------------------------------
 * Say hello to vty interface.
 */
void
vty_hello (struct vty *vty)
{
  VTY_LOCK() ;

#ifdef QDEBUG
  uty_out (vty, "%s%s", debug_banner, VTY_NEWLINE);
#endif
  if (host.motdfile)
    {
      FILE *f;
      char buf[4096];

      f = fopen (host.motdfile, "r");
      if (f)
        {
          while (fgets (buf, sizeof (buf), f))
            {
              char *s;
              /* work backwards to ignore trailing isspace() */
              for (s = buf + strlen (buf); (s > buf) && isspace ((int)*(s - 1));
                   s--);
              *s = '\0';
              uty_out (vty, "%s%s", buf, VTY_NEWLINE);
            }
          fclose (f);
        }
      else
        uty_out (vty, "MOTD file %s not found%s", host.motdfile, VTY_NEWLINE);
    }
  else if (host.motd)
    uty_out (vty, "%s", host.motd);

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Clear the contents of the command output FIFO etc.
 */
extern void
vty_out_clear(struct vty* vty)
{
  VTY_LOCK() ;
  uty_out_clear(vty->vio) ;
  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Command Execution
 */

/*------------------------------------------------------------------------------
 * Execute command -- adding to history is not empty or just comment
 *
 * This is for VTY_TERM type VTY.
 *
 * Outputs diagnostics if fails to parse.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
uty_command(struct vty *vty)
{
  enum cmd_return_code ret;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(vty->vio->type == VTY_TERM) ;

  /* Parse the command and add to history (if not empty)                */
  ret = cmd_parse_command(vty,
                         cmd_parse_completion + cmd_parse_do + cmd_parse_tree) ;
  if (ret != CMD_EMPTY)
    uty_cli_hist_add (vty->vio, vty->buf) ;

  /* If parsed and not empty, dispatch                                  */
  if (ret == CMD_SUCCESS)
    {
#ifdef CONSUMED_TIME_CHECK
      RUSAGE_T before;
      RUSAGE_T after;
      unsigned long realtime, cputime;

      GETRUSAGE(&before);
#endif /* CONSUMED_TIME_CHECK */

      ret = cmd_dispatch(vty, cmd_may_queue) ;

#ifdef CONSUMED_TIME_CHECK
      GETRUSAGE(&after);
      if ((realtime = thread_consumed_time(&after, &before, &cputime)) >
                                                            CONSUMED_TIME_CHECK)
        /* Warn about CPU hog that must be fixed. */
        uzlog(NULL, LOG_WARNING,
                     "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                                        realtime/1000, cputime/1000, vty->buf) ;
#endif /* CONSUMED_TIME_CHECK */
    } ;

  /* Deal with the return code                                          */
  switch (ret)
  {
    case CMD_ERR_AMBIGUOUS:
      uty_out (vty, "%% Ambiguous command.%s", VTY_NEWLINE);
      break;

    case CMD_ERR_NO_MATCH:
      uty_out (vty, "%% Unknown command.%s", VTY_NEWLINE) ;
      break;

    case CMD_ERR_INCOMPLETE:
      uty_out (vty, "%% Command incomplete.%s", VTY_NEWLINE);
      break;

    default:
      break ;
  } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Authentication of vty
 *
 * During AUTH_NODE and AUTH_ENABLE_NODE, when a command line is dispatched by
 * any means this function is called.
 *
 * Note that if the AUTH_NODE password fails too many times, the terminal is
 * closed.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
uty_auth (struct vty *vty, const char *buf, enum cli_do cli_do)
{
  char *passwd = NULL;
  enum node_type next_node = 0;
  int fail;
  char *crypt (const char *, const char *);
  enum cmd_return_code ret ;

  vty_io  vio = vty->vio ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* What to do ?
   *
   * In fact, all the exotic command terminators simply discard any input
   * and return.
   */
  switch (cli_do)
  {
    case cli_do_nothing:
    case cli_do_ctrl_c:
    case cli_do_ctrl_z:
      return CMD_SUCCESS ;

    case cli_do_command:
      break ;

    case cli_do_ctrl_d:
    case cli_do_eof:
      return uty_cmd_close(vty, "End") ;

    default:
      zabort("unknown or invalid cli_do") ;
  } ;

  /* Ordinary command dispatch -- see if password is OK.                */
  switch (vty->node)
  {
    case AUTH_NODE:
      if (host.encrypt)
	passwd = host.password_encrypt;
      else
	passwd = host.password;
      if (host.advanced)
	next_node = host.enable ? VIEW_NODE : ENABLE_NODE;
      else
	next_node = VIEW_NODE;
      break;

    case AUTH_ENABLE_NODE:
      if (host.encrypt)
	passwd = host.enable_encrypt;
      else
	passwd = host.enable;
      next_node = ENABLE_NODE;
      break;

    default:
      zabort("unknown node type") ;
  }

  if (passwd)
    {
      if (host.encrypt)
	fail = strcmp (crypt(buf, passwd), passwd);
      else
	fail = strcmp (buf, passwd);
    }
  else
    fail = 1;

  ret = CMD_SUCCESS ;

  if (! fail)
    {
      vio->fail = 0;
      vty->node = next_node;	/* Success ! */
    }
  else
    {
      vio->fail++;
      if (vio->fail >= 3)
	{
	  if (vty->node == AUTH_NODE)
	    {
	      ret = uty_cmd_close(vty, "Bad passwords, too many failures!%s") ;
	    }
	  else
	    {
	      /* AUTH_ENABLE_NODE */
	      vio->fail = 0;
	      uty_out (vty, "%% Bad enable passwords, too many failures!%s",
	                                                           VTY_NEWLINE);
	      vty->node = restricted_mode ? RESTRICTED_NODE : VIEW_NODE;

	      ret = CMD_WARNING ;
	    }
	}
    }

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command line "exit" command -- aka "quit"
 *
 * Falls back one NODE level.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
vty_cmd_exit(struct vty* vty)
{
  enum cmd_return_code ret ;

  VTY_LOCK() ;

  ret = CMD_SUCCESS ;
  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      if (vty_shell (vty))
        exit (0);
      else
        ret = uty_cmd_close(vty, "Exit") ;
      break;
    case CONFIG_NODE:
      uty_config_unlock (vty, ENABLE_NODE);
      break;
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case BGP_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case MASC_NODE:
    case RMAP_NODE:
    case VTY_NODE:
      vty->node = CONFIG_NODE ;
      break;
    case BGP_VPNV4_NODE:
    case BGP_IPV4_NODE:
    case BGP_IPV4M_NODE:
    case BGP_IPV6_NODE:
    case BGP_IPV6M_NODE:
      vty->node = BGP_NODE ;
      break;
    case KEYCHAIN_KEY_NODE:
      vty->node = KEYCHAIN_NODE ;
      break;
    default:
      break;
    }

  VTY_UNLOCK() ;
  return ret ;
}

/*------------------------------------------------------------------------------
 * Command line "end" command
 *
 * Falls back to ENABLE_NODE.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
vty_cmd_end(struct vty* vty)
{
  VTY_LOCK() ;

  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      /* Nothing to do. */
      break;
    case CONFIG_NODE:
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case BGP_NODE:
    case BGP_VPNV4_NODE:
    case BGP_IPV4_NODE:
    case BGP_IPV4M_NODE:
    case BGP_IPV6_NODE:
    case BGP_IPV6M_NODE:
    case RMAP_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case KEYCHAIN_KEY_NODE:
    case MASC_NODE:
    case VTY_NODE:
      uty_config_unlock (vty, ENABLE_NODE);
      break;
    default:
      break;
    }

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Result of command is to close the input.
 *
 * Posts the reason for the close.
 *
 * Returns: CMD_CLOSE
 */
extern enum cmd_return_code
uty_cmd_close(struct vty *vty, const char* reason)
{
  vty->vio->close_reason = reason ;
  return CMD_CLOSE ;
} ;

/*------------------------------------------------------------------------------
 * Command line ^C action.
 *
 * Ignores contents of command line (including not adding to history).
 *
 * Fall back to ENABLE_NODE if in any one of a number of nodes.
 *
 * Resets the history pointer.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
uty_stop_input(struct vty *vty)
{
  vty_io  vio = vty->vio ;

  VTY_ASSERT_LOCKED() ;

  switch (vty->node)
    {
    case CONFIG_NODE:
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case BGP_NODE:
    case RMAP_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case KEYCHAIN_KEY_NODE:
    case MASC_NODE:
    case VTY_NODE:
      uty_config_unlock (vty, ENABLE_NODE) ;
      break;
    default:
      /* Unknown node, we have to ignore it. */
      break;
    }

  /* Set history pointer to the latest one. */
  vio->hp = vio->hindex;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Command ^Z action.
 *
 * Ignores contents of command line (including not adding to history).
 *
 * Fall back to ENABLE_NODE if in any one of a number of nodes.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
uty_end_config (struct vty *vty)
{
  VTY_ASSERT_LOCKED() ;

  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      /* Nothing to do. */
      break;
    case CONFIG_NODE:
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case BGP_NODE:
    case BGP_VPNV4_NODE:
    case BGP_IPV4_NODE:
    case BGP_IPV4M_NODE:
    case BGP_IPV6_NODE:
    case BGP_IPV6M_NODE:
    case RMAP_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case KEYCHAIN_KEY_NODE:
    case MASC_NODE:
    case VTY_NODE:
      uty_config_unlock (vty, ENABLE_NODE) ;
      break;
    default:
      /* Unknown node, we have to ignore it. */
      break;
    }

  return CMD_SUCCESS ;
}

/*------------------------------------------------------------------------------
 * Command ^D action -- when nothing else on command line.
 *
 * Same as "exit" command.
 *
 * Returns: command return code
 */
extern enum cmd_return_code
uty_down_level (struct vty *vty)
{
  return vty_cmd_exit(vty) ;
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

static FILE * vty_use_backup_config (char *fullpath) ;
static void vty_read_file (FILE *confp, struct cmd_element* first_cmd,
                                                         bool ignore_warnings) ;

/*------------------------------------------------------------------------------
 * Read the given configuration file.
 */
extern void
vty_read_config (char *config_file,
                 char *config_default)
{
  vty_read_config_first_cmd_special(config_file, config_default, NULL, 1);
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
vty_read_config_first_cmd_special(char *config_file,
                                  char *config_default,
                                  struct cmd_element* first_cmd,
                                  bool ignore_warnings)
{
  char cwd[MAXPATHLEN];
  FILE *confp = NULL;
  char *fullpath;
  char *tmp = NULL;

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
            return;
        }
    } ;

  /* Use default if necessary, and deal with constructing full path     */
  if (config_file == NULL)
    config_file = config_default ;

  if (! IS_DIRECTORY_SEP (config_file[0]))
    {
      getcwd (cwd, sizeof(cwd)) ;
      tmp = XMALLOC (MTYPE_TMP, strlen (cwd) + strlen (config_file) + 2) ;
      sprintf (tmp, "%s/%s", cwd, config_file);
      fullpath = tmp;
    }
  else
    {
      tmp = NULL ;
      fullpath = config_file;
    } ;

  /* try to open the configuration file                                 */
  confp = fopen (fullpath, "r");

  if (confp == NULL)
    {
      fprintf (stderr, "%s: failed to open configuration file %s: %s\n",
                                    __func__, fullpath, errtostr(errno, 0).str);

      confp = vty_use_backup_config (fullpath);
      if (confp)
        fprintf (stderr, "WARNING: using backup configuration file!\n");
      else
        {
          fprintf (stderr, "can't open backup configuration file [%s%s]\n",
                                                    fullpath, CONF_BACKUP_EXT);
          exit(1);
        }
    } ;

#ifdef QDEBUG
  fprintf(stderr, "Reading config file: %s\n", fullpath);
#endif

  vty_read_file (confp, first_cmd, ignore_warnings);
  fclose (confp);

  host_config_set (fullpath);

#ifdef QDEBUG
  fprintf(stderr, "Finished reading config file\n");
#endif

  if (tmp)
    XFREE (MTYPE_TMP, tmp);
}

/*------------------------------------------------------------------------------
 * Try to use a backup configuration file.
 *
 * Having failed to open the file "<fullpath>", if there is a file called
 * "<fullpath>.sav" that can be opened for reading, then:
 *
 *   - make a copy of that file
 *   - call it "<fullpath>"
 *   - return an open FILE
 *
 * Returns: NULL => no "<fullpath>.sav", or faild doing any of the above
 *          otherwise, returns FILE* for open file.
 */
static FILE *
vty_use_backup_config (char *fullpath)
{
  char *tmp_path ;
  struct stat buf;
  int ret, tmp, sav;
  int c;
  char buffer[4096] ;

  enum { xl = 32 } ;
  tmp_path = malloc(strlen(fullpath) + xl) ;

  /* construct the name "<fullname>.sav", and try to open it.           */
  confirm(xl > sizeof(CONF_BACKUP_EXT)) ;
  sprintf (tmp_path, "%s%s", fullpath, CONF_BACKUP_EXT) ;

  sav = -1 ;
  if (stat (tmp_path, &buf) != -1)
    sav = open (tmp_path, O_RDONLY);

  if (sav < 0)
    {
      free (tmp_path);
      return NULL;
    } ;

  /* construct a temporary file and copy "<fullpath.sav>" to it.        */
  confirm(xl > sizeof(".XXXXXX"))
  sprintf (tmp_path, "%s%s", fullpath, ".XXXXXX") ;

  /* Open file to configuration write. */
  tmp = mkstemp (tmp_path);
  if (tmp < 0)
    {
      free (tmp_path);
      close(sav);
      return NULL;
    }

  while((c = read (sav, buffer, sizeof(buffer))) > 0)
    write (tmp, buffer, c);

  close (sav);
  close (tmp);

  /* Make sure that have the required file status                       */
  if (chmod(tmp_path, CONFIGFILE_MASK) != 0)
    {
      unlink (tmp_path);
      free (tmp_path);
      return NULL;
    }

  /* Make <fullpath> be a name for the new file just created.           */
  ret = link (tmp_path, fullpath) ;

  /* Discard the temporary, now                                         */
  unlink (tmp_path) ;
  free (tmp_path) ;

  /* If link was successful, try to open -- otherwise, failed.          */
  return (ret == 0) ? fopen (fullpath, "r") : NULL ;
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
vty_read_file (FILE *confp, struct cmd_element* first_cmd, bool ignore_warnings)
{
  enum cmd_return_code ret ;
  struct vty *vty ;

  /* Set up configuration file reader VTY -- which buffers all output   */
  vty = vty_open(VTY_CONFIG_READ);
  vty->node = CONFIG_NODE;

  /* Make sure we have a suitable buffer, and set vty->buf to point at
   * it -- same like other command execution.
   */
  qs_need(&vty->vio->clx, VTY_BUFSIZ) ;
  vty->buf = qs_chars(&vty->vio->clx) ;

  /* Execute configuration file                                         */
  ret = config_from_file (vty, confp, first_cmd, &vty->vio->clx,
                                                              ignore_warnings) ;

  VTY_LOCK() ;

  if (ret != CMD_SUCCESS)
    {
      fprintf (stderr, "%% while processing line %u of the configuration:\n"
                       "%s", vty->lineno, vty->buf) ;

      switch (ret)
      {
        case CMD_WARNING:
          fprintf (stderr, "%% Warning...\n");
          break;

        case CMD_ERROR:
          fprintf (stderr, "%% Error...\n");
          break;

        case CMD_ERR_AMBIGUOUS:
           fprintf (stderr, "%% Ambiguous command.\n");
           break;

        case CMD_ERR_NO_MATCH:
          fprintf (stderr, "%% There is no such command.\n");
          break;

        case CMD_ERR_INCOMPLETE:
          fprintf (stderr, "%% Incomplete command.\n");
          break;

        default:
          fprintf(stderr, "%% (unknown cause %d)\n", ret) ;
          break ;
       } ;

      uty_out_fflush(vty->vio, stderr) ;  /* flush command output buffer   */

      exit (1);
    } ;

  uty_close(vty->vio) ;
  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Configuration node/state handling
 *
 * At most one VTY may hold the configuration symbol of power at any time.
 */

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power
 *
 * If succeeds, set the given node.
 *
 * Returns: true <=> now own the symbol of power.
 */
extern bool
vty_config_lock (struct vty *vty, enum node_type node)
{
  bool result;

  VTY_LOCK() ;

  if (vty_config == 0)
    {
      vty->vio->config = 1 ;
      vty_config       = 1 ;
    } ;

  result = vty->vio->config;

  if (result)
    vty->node = node ;

  VTY_UNLOCK() ;

  return result;
}

/*------------------------------------------------------------------------------
 * Give back the configuration symbol of power -- if own it.
 *
 * Set the given node -- which must be <= MAX_NON_CONFIG_NODE
 */
extern void
vty_config_unlock (struct vty *vty, enum node_type node)
{
  VTY_LOCK() ;
  uty_config_unlock(vty, node);
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Give back the configuration symbol of power -- if own it.
 *
 * Set the given node -- which must be <= MAX_NON_CONFIG_NODE
 */
extern void
uty_config_unlock (struct vty *vty, enum node_type node)
{
  VTY_ASSERT_LOCKED() ;
  if ((vty_config == 1) && (vty->vio->config == 1))
    {
      vty->vio->config = 0;
      vty_config       = 0;
    }

  assert(node <= MAX_NON_CONFIG_NODE) ;
  vty->node = node ;
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

  vio = vio_list_base ;
  while (vio != NULL)   /* TODO: show only VTY_TERM ???         */
    {
      uty_out (vty, "%svty[%d] connected from %s.%s",
	       vio->config ? "*" : " ",
	       i, uty_get_name(vio), VTY_NEWLINE);
      vio = sdl_next(vio, vio_list) ;
    } ;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/* Move to vty configuration mode. */
DEFUN_CALL (line_vty,
       line_vty_cmd,
       "line vty",
       "Configure a terminal line\n"
       "Virtual terminal\n")
{
  VTY_LOCK() ;
  vty->node = VTY_NODE;
  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/* Set time out value. */
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

  vty_timeout_val = timeout;

  if (vty_term(vty) || vty_shell_serv(vty))
    uty_sock_set_timer(&vty->vio->sock, timeout) ;

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

  if (vty_accesslist_name)
    XFREE(MTYPE_VTY, vty_accesslist_name);

  vty_accesslist_name = XSTRDUP(MTYPE_VTY, argv[0]);

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
  int result = CMD_SUCCESS;

  VTY_LOCK() ;
  if (! vty_accesslist_name || (argc && strcmp(vty_accesslist_name, argv[0])))
    {
      uty_out (vty, "Access-class is not currently applied to vty%s",
	       VTY_NEWLINE);
      result = CMD_WARNING;
    }
  else
    {
      XFREE(MTYPE_VTY, vty_accesslist_name);
      vty_accesslist_name = NULL;
    }

  VTY_UNLOCK() ;
  return result;
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
  if (vty_ipv6_accesslist_name)
    XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);

  vty_ipv6_accesslist_name = XSTRDUP(MTYPE_VTY, argv[0]);

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
  int result = CMD_SUCCESS;

  VTY_LOCK() ;

  if (! vty_ipv6_accesslist_name ||
      (argc && strcmp(vty_ipv6_accesslist_name, argv[0])))
    {
      uty_out (vty, "IPv6 access-class is not currently applied to vty%s",
	       VTY_NEWLINE);
      result = CMD_WARNING;
    }
  else
    {
      XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);

      vty_ipv6_accesslist_name = NULL;
    }

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}
#endif /* HAVE_IPV6 */

/* vty login. */
DEFUN_CALL (vty_login,
       vty_login_cmd,
       "login",
       "Enable password checking\n")
{
  VTY_LOCK() ;
  no_password_check = 0;
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
  no_password_check = 1;
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
  restricted_mode = 1;
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
  restricted_mode = 0;
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
  host.advanced = 1;
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
  host.advanced = 0;
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
  int index;

  VTY_LOCK() ;

  for (index = vty->vio->hindex + 1; index != vty->vio->hindex;)
    {
      qstring line ;

      if (index == VTY_MAXHIST)
	{
	  index = 0;
	  continue;
	}

      line = vector_get_item(&vty->vio->hist, index) ;
      if (line != NULL)
	uty_out (vty, "  %s%s", line->char_body, VTY_NEWLINE);

      index++;
    }

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
  vty_out (vty, "line vty%s", VTY_NEWLINE);

  if (vty_accesslist_name)
    vty_out (vty, " access-class %s%s",
	     vty_accesslist_name, VTY_NEWLINE);

  if (vty_ipv6_accesslist_name)
    vty_out (vty, " ipv6 access-class %s%s",
	     vty_ipv6_accesslist_name, VTY_NEWLINE);

  /* exec-timeout */
  if (vty_timeout_val != VTY_TIMEOUT_DEFAULT)
    vty_out (vty, " exec-timeout %ld %ld%s",
	     vty_timeout_val / 60,
	     vty_timeout_val % 60, VTY_NEWLINE);

  /* login */
  if (no_password_check)
    vty_out (vty, " no login%s", VTY_NEWLINE);

  if (restricted_mode != restricted_mode_default)
    {
      if (restricted_mode_default)
        vty_out (vty, " no anonymous restricted%s", VTY_NEWLINE);
      else
        vty_out (vty, " anonymous restricted%s", VTY_NEWLINE);
    }

  vty_out (vty, "!%s", VTY_NEWLINE);

  return CMD_SUCCESS;
}

/*==============================================================================
 * The cwd at start-up.
 */

/*------------------------------------------------------------------------------
 * Save cwd
 *
 * This is done early in the morning so that any future operations on files
 * can use the original cwd if required.
 */
static void
vty_save_cwd (void)
{
  char cwd[MAXPATHLEN];
  char *c;

  c = getcwd (cwd, MAXPATHLEN);

  if (!c)
    {
      chdir (SYSCONFDIR);
      getcwd (cwd, MAXPATHLEN);
    }

  vty_cwd = XSTRDUP(MTYPE_TMP, cwd) ;
} ;

/*------------------------------------------------------------------------------
 * Get cwd as at start-up.  Never changed -- so no locking required.
 */
char *
vty_get_cwd ()
{
  return vty_cwd;
}

/*==============================================================================
 * Access functions for VTY values, where locking is or might be required.
 */

bool
vty_shell (struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = (vty->vio->type == VTY_SHELL) ;
  VTY_UNLOCK() ;
  return result;
}

bool
vty_term(struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = (vty->vio->type == VTY_TERM);
  VTY_UNLOCK() ;
  return result;
}

bool
vty_shell_serv (struct vty *vty)
{
  bool result;
  VTY_LOCK() ;
  result = (vty->vio->type == VTY_SHELL_SERV);
  VTY_UNLOCK() ;
  return result;
}

enum node_type
vty_get_node(struct vty *vty)
{
  int result;
  VTY_LOCK() ;
  result = vty->node;
  VTY_UNLOCK() ;
  return result;
}

void
vty_set_node(struct vty *vty, enum node_type node)
{
  VTY_LOCK() ;
  vty->node = node;
  VTY_UNLOCK() ;
}

void
vty_set_lines(struct vty *vty, int lines)
{
  VTY_LOCK() ;
  vty->vio->lines     = lines;
  vty->vio->lines_set = 1 ;
  uty_set_height(vty->vio) ;
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

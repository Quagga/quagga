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

#include "vty_io.h"
#include "vty.h"
#include "uty.h"
#include "vty_cli.h"

#include "list_util.h"

#include "command.h"
#include "memory.h"
#include "log.h"

/*==============================================================================
 * Variables etc.
 */

/*------------------------------------------------------------------------------
 * Static and Global (see uty.h) Variables
 */

/* The mutex and related debug counters                                 */
qpt_mutex_t vty_mutex ;

#if VTY_DEBUG

int vty_lock_count       = 0 ;
int vty_lock_assert_fail = 0 ;

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
static void uty_reset (bool final) ;
static void uty_init_commands (void) ;
static void vty_save_cwd (void) ;

/*==============================================================================
 * Public Interface
 */

/*------------------------------------------------------------------------------
 * Initialise vty handling (threads and pthreads)
 *
 * Install vty's own commands like `who' command.
 */
extern void
vty_init (struct thread_master *master_thread)
{
  VTY_LOCK() ;

  vty_master = master_thread;   /* Local pointer to the master thread   */

  vty_save_cwd ();              /* need cwd for config reading          */

  vio_list_base       = NULL ;  /* no VTYs yet                          */
  vio_monitors_base   = NULL ;
  vio_death_watch     = NULL ;

  vty_cli_nexus       = NULL ;  /* not running qnexus-wise              */
  vty_cmd_nexus       = NULL ;

  vty_watch_dog.anon  = NULL ;  /* no watch dog                         */

  uty_init_commands() ;         /* install nodes                        */

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Further initialisation for qpthreads.
 *
 * This is done during "second stage" initialisation, when all nexuses have
 * been set up and the qpthread_enabled state established.
 *
 * Need to know where the CLI nexus and the Routeing Engine nexus are.
 *
 * Initialise mutex.
 */
extern void
vty_init_r (qpn_nexus cli, qpn_nexus cmd)
{
  vty_cli_nexus = cli ;
  vty_cmd_nexus = cmd ;

  qpt_mutex_init(&vty_mutex, qpt_mutex_recursive);
} ;

/*------------------------------------------------------------------------------
 * Initialise the listeners for VTY_TERM and VTY_SHELL_SERV VTY
 *
 * This is done after the configuration file has been read.
 */
extern void
vty_serv_sock(const char *addr, unsigned short port, const char *path)
{
  VTY_LOCK() ;
  uty_open_listeners(addr, port, path) ;
  VTY_UNLOCK() ;
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
 * Create a new VTY of the given type
 */
extern struct vty *
vty_new (int fd, enum vty_type type)
{
  struct vty* vty ;

  VTY_LOCK() ;
  vty = uty_new(fd, type);
  VTY_UNLOCK() ;

  return vty ;
} ;

/*------------------------------------------------------------------------------
 * Close the given VTY completely
 */
extern void
vty_close (struct vty *vty)
{
  VTY_LOCK() ;
  uty_close(vty->vio);
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Reset all VTY status
 *
 * This is done just before the configuration file is re-read (SIGHUP).
 *
 * Half closes all VTY, leaving the death watch to tidy up once all output has
 * completed.
 *
 * NB: old code discarded all output and hard closed all the VTY...
 *
 * TODO: ...SIGHUP while a command is queued ?
 *
 * Closes all listening sockets.
 */
extern void
vty_reset(void)
{
  VTY_LOCK() ;
  uty_reset(0) ;        /* not final !  */
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * System shut-down
 *
 * Reset all known vty and release all memory.
 */
extern void
vty_terminate (void)
{
  VTY_LOCK() ;
  uty_reset(1) ;        /* final reset  */
  VTY_UNLOCK() ;

  qpt_mutex_destroy(&vty_mutex, 0);
}

/*------------------------------------------------------------------------------
 * Reset -- final or for SIGHUP
 */
static void
uty_reset (bool curtains)
{
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  uty_close_listeners() ;

  while ((vio = sdl_pop(&vio, vio_list_base, vio_list)) != NULL)
    {
      uty_half_close(vio) ;       /* TODO: reason for close       */

      if (curtains)
        uty_full_close(vio) ;
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
} ;

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

extern int
vty_out_indent(struct vty *vty, int indent)
{
  while (indent > VTY_MAX_SPACES)
    {
      int ret = vty_out(vty, VTY_SPACES(indent)) ;
      if (ret < 0)
        return ret ;
      indent -= VTY_MAX_SPACES ;
    }
  return vty_out(vty, VTY_SPACES(indent)) ;
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

/*==============================================================================
 * Command Execution
 */

/*------------------------------------------------------------------------------
 * Execute command, adding it to the history if not empty or comment
 *
 * Outputs diagnostics if fails to parse.
 *
 * Returns: CMD_xxxx result.
 */
extern int
uty_command(struct vty *vty, const char *buf)
{
  int ret;
  vector vline;
  const char *protocolname;

  VTY_ASSERT_LOCKED() ;

  /* Split readline string up into the vector                   */
  vline = cmd_make_strvec (buf);

  if (vline == NULL)
    return CMD_SUCCESS;         /* quit if empty or comment     */

  uty_cli_hist_add (vty->vio, buf) ;

#ifdef CONSUMED_TIME_CHECK
  {
    RUSAGE_T before;
    RUSAGE_T after;
    unsigned long realtime, cputime;

    GETRUSAGE(&before);
#endif /* CONSUMED_TIME_CHECK */

//VTY_UNLOCK() ;
  ret = cmd_execute_command (vline, vty, NULL, vty_cmd_nexus, vty_cli_nexus, 0);
//VTY_LOCK() ;

  /* Get the name of the protocol if any */
  protocolname = uzlog_get_proto_name(NULL);

#ifdef CONSUMED_TIME_CHECK
    GETRUSAGE(&after);
    if ((realtime = thread_consumed_time(&after, &before, &cputime)) >
        CONSUMED_TIME_CHECK)
      /* Warn about CPU hog that must be fixed. */
      uzlog(NULL, LOG_WARNING, "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                realtime/1000, cputime/1000, buf);
  }
#endif /* CONSUMED_TIME_CHECK */

  if (ret != CMD_SUCCESS)
    switch (ret)
      {
      case CMD_WARNING:
        if (vty->vio->type == VTY_FILE)
          uty_out (vty, "Warning...%s", VTY_NEWLINE);
        break;
      case CMD_ERR_AMBIGUOUS:
        uty_out (vty, "%% Ambiguous command.%s", VTY_NEWLINE);
        break;
      case CMD_ERR_NO_MATCH:
        uty_out (vty, "%% [%s] Unknown command: %s%s", protocolname, buf, VTY_NEWLINE);
        break;
      case CMD_ERR_INCOMPLETE:
        uty_out (vty, "%% Command incomplete.%s", VTY_NEWLINE);
        break;
      }
  cmd_free_strvec (vline);

  return ret;
}

/*------------------------------------------------------------------------------
 * Authentication of vty
 *
 * During AUTH_NODE and AUTH_ENABLE_NODE, when a command line is dispatched by
 * any means this function is called.
 *
 * Note that if the AUTH_NODE password fails too many times, the terminal is
 * closed.
 *
 * Returns: 0 <=> not queued.
 */
extern int
uty_auth (struct vty *vty, const char *buf, enum cli_do cli_do)
{
  char *passwd = NULL;
  enum node_type next_node = 0;
  int fail;
  char *crypt (const char *, const char *);

  vty_io  vio = vty->vio ;

  VTY_ASSERT_LOCKED() ;

  /* What to do ?
   *
   * In fact, all the exotic command terminators simply discard any input
   * and return.
   */
  switch (cli_do)
  {
    case cli_do_nothing:
    case cli_do_ctrl_c:
    case cli_do_ctrl_d:
    case cli_do_ctrl_z:
      return 0 ;

    case cli_do_command:
      break ;

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
	      uty_out (vty, "%% Bad passwords, too many failures!%s",
	                                                           VTY_NEWLINE);
	      uty_half_close(vio) ;
	    }
	  else
	    {
	      /* AUTH_ENABLE_NODE */
	      vio->fail = 0;
	      uty_out (vty, "%% Bad enable passwords, too many failures!%s",
	                                                           VTY_NEWLINE);
	      vty->node = restricted_mode ? RESTRICTED_NODE : VIEW_NODE;
	    }
	}
    }

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Command line "exit" command -- aka "quit"
 *
 * Falls back one NODE level.
 *
 * Returns: 0 <=> not queued.
 */
extern int
vty_cmd_exit(struct vty* vty)
{
  VTY_LOCK() ;          /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      if (vty_shell (vty))
        exit (0);
//      else
//        vty_set_status(vty, VTY_CLOSE);
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

  VTY_UNLOCK() ;        /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  return 0 ;
}

/*------------------------------------------------------------------------------
 * Command line "end" command
 *
 * Falls back to ENABLE_NODE.
 *
 * Returns: 0 <=> not queued.
 */
extern int
vty_cmd_end(struct vty* vty)
{
  VTY_LOCK() ;          /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

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

  VTY_UNLOCK() ;        /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
  return 0 ;
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
 * Returns: 0 <=> not queued.
 */
extern int
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

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Command ^Z action.
 *
 * Ignores contents of command line (including not adding to history).
 *
 * Fall back to ENABLE_NODE if in any one of a number of nodes.
 *
 * Returns: 0 <=> not queued.
 */
extern int
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

  return 0 ;
}

/*------------------------------------------------------------------------------
 * Command ^D action -- when nothing else on command line.
 *
 * Same as "exit" command.
 *
 * Returns: 0 <=> not queued.
 */
extern int
uty_down_level (struct vty *vty)
{
  return vty_cmd_exit(vty) ;
} ;

/*==============================================================================
 * Reading of configuration file
 */

static FILE * vty_use_backup_config (char *fullpath) ;
static void vty_read_file (FILE *confp, void (*after_first_cmd)(void)) ;

/*------------------------------------------------------------------------------
 * Read the given configuration file.
 */
extern void
vty_read_config (char *config_file,
                 char *config_default)
{
  vty_read_config_first_cmd_special(config_file, config_default, NULL);
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
                                                 void (*after_first_cmd)(void))
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
                                    __func__, fullpath, safe_strerror (errno));

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

  vty_read_file (confp, after_first_cmd);
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
 */
static void
vty_read_file (FILE *confp, void (*after_first_cmd)(void))
{
  int ret;
  struct vty *vty;

  /* TODO: sort out what VTY Type should use for reading config file    */
  vty = vty_new (0, VTY_TERM); /* stdout */
  vty->node = CONFIG_NODE;

  /* Make sure we have a suitable buffer, and set vty->buf to point at
   * it -- same like other command execution.
   */
  qs_need(&vty->vio->clx, VTY_BUFSIZ) ;
  vty->buf = qs_chars(&vty->vio->clx) ;

  /* Execute configuration file                                         */
  ret = config_from_file (vty, confp, after_first_cmd, &vty->vio->clx) ;

  VTY_LOCK() ;

  if ( !((ret == CMD_SUCCESS) || (ret == CMD_ERR_NOTHING_TODO)) )
    {
      switch (ret)
       {
         case CMD_ERR_AMBIGUOUS:
           fprintf (stderr, "Ambiguous command.\n");
           break;
         case CMD_ERR_NO_MATCH:
           fprintf (stderr, "There is no such command.\n");
           break;
       }
      fprintf (stderr, "Error occurred while processing:\n%s\n", vty->buf);

      exit (1);
    }

  uty_half_close (vty->vio);
  VTY_UNLOCK() ;
}

#ifdef QDEBUG
/* Tell all terminals that we are shutting down */
void
vty_goodbye (void)
{
  unsigned int i;
  struct vty *vty;

  VTY_LOCK() ;

  if (vtyvec)
    {
      for (i = 0; i < vector_active (vtyvec); i++)
        {
          if (((vty = vector_slot (vtyvec, i)) != NULL) && vty->vio->type == VTY_TERM)
            {
              uty_cout(vty, QUAGGA_PROGNAME " is shutting down%s", VTY_NEWLINE);

              /* Wake up */
              if (vty_cli_nexus)
                vty_event (VTY_WRITE, vty->vio->fd, vty);
            }
        }
      if (qpthreads_enabled)
        qpt_thread_signal(vty_cli_nexus->thread_id, SIGMQUEUE);
    }

    VTY_UNLOCK() ;
}
#endif

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
  vty->vio->file.v_timeout = timeout;
//  vty_event (VTY_TIMEOUT_RESET, 0, vty);

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
      const char* line ;

      if (index == VTY_MAXHIST)
	{
	  index = 0;
	  continue;
	}

      line = vector_get_item(&vty->vio->hist, index) ;
      if (line != NULL)
	uty_out (vty, "  %s%s", line, VTY_NEWLINE);

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

int
vty_shell (struct vty *vty)
{
  VTY_LOCK() ;
  int result;
  result = (vty->vio->type == VTY_SHELL) ? 1 : 0 ;
  VTY_UNLOCK() ;
  return result;
}

int
vty_shell_serv (struct vty *vty)
{
  VTY_LOCK() ;
  int result;
  result = ((vty->vio->type == VTY_SHELL_SERV) ? 1 : 0);
  VTY_UNLOCK() ;
  return result;
}

int
vty_get_node(struct vty *vty)
{
  int result;
  VTY_LOCK() ;
  result = vty->node;
  VTY_UNLOCK() ;
  return result;
}

void
vty_set_node(struct vty *vty, int node)
{
  VTY_LOCK() ;
  vty->node = node;
  VTY_UNLOCK() ;
}

int
vty_get_type(struct vty *vty)
{
  int result;
  VTY_LOCK() ;
  result = vty->vio->type;
  VTY_UNLOCK() ;
  return result;
}

int
vty_get_lines(struct vty *vty)
{
  int result;
  VTY_LOCK() ;
  result = vty->vio->lines;
  VTY_UNLOCK() ;
  return result;
}

void
vty_set_lines(struct vty *vty, int lines)
{
  VTY_LOCK() ;
  vty->vio->lines = lines;
  VTY_UNLOCK() ;
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

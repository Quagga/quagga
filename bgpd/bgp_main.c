/* Main routine of bgpd.
   Copyright (C) 1996, 97, 98, 1999 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include <zebra.h>
#include <stdbool.h>

#include "vector.h"
#include "vty.h"
#include "command.h"
#include "getopt.h"
#include "thread.h"
#include <lib/version.h>
#include "memory.h"
#include "prefix.h"
#include "log.h"
#include "privs.h"
#include "sigevent.h"
#include "zclient.h"
#include "routemap.h"
#include "filter.h"
#include "plist.h"
#include "qpnexus.h"
#include "qlib_init.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_mplsvpn.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_dump.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_nexthop.h"
#include "bgpd/bgp_regex.h"
#include "bgpd/bgp_clist.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_filter.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_zebra.h"

/* bgpd options, we use GNU getopt library. */
static const struct option longopts[] =
{
  { "daemon",      no_argument,       NULL, 'd'},
  { "config_file", required_argument, NULL, 'f'},
  { "pid_file",    required_argument, NULL, 'i'},
  { "bgp_port",    required_argument, NULL, 'p'},
  { "listenon",    required_argument, NULL, 'l'},
  { "vty_addr",    required_argument, NULL, 'A'},
  { "vty_port",    required_argument, NULL, 'P'},
  { "retain",      no_argument,       NULL, 'r'},
  { "no_kernel",   no_argument,       NULL, 'n'},
  { "user",        required_argument, NULL, 'u'},
  { "group",       required_argument, NULL, 'g'},
  { "version",     no_argument,       NULL, 'v'},
  { "dryrun",      no_argument,       NULL, 'C'},
  { "help",        no_argument,       NULL, 'h'},
  { "threaded",    no_argument,       NULL, 't'},
  { "ignore_warnings", no_argument,   NULL, 'I'},
  { 0 }
};
/* Configuration file and directory.                                    */
char config_default[] = SYSCONFDIR BGP_DEFAULT_CONFIG;

/* Route retain mode flag.                                              */
static bool retain_mode         = false;

/* Have started terminating the program                                 */
static bool program_terminating = false ;

/* whether to ignore warnings in configuration file                     */
static bool config_ignore_warnings = false;

/* whether configured to run with qpthreads                             */
static bool config_threaded     = false ;

/* whether configured to run as an AS2 speaker                          */
static bool config_as2_speaker  = false ;

/* Master of threads. */
struct thread_master *master;

/* Manually specified configuration file name.  */
char *config_file = NULL;

/* Have we done the second stage initialization? */
static int done_2nd_stage_init = 0;
/* Process ID saved for use by init system */
static const char *pid_file = PATH_BGPD_PID;

/* VTY port number and address.  */
int   vty_port = BGP_VTY_PORT;
char *vty_addr = NULL;

/* privileges */
static zebra_capabilities_t _caps_p [] =
{
    ZCAP_BIND,
    ZCAP_NET_RAW,
};

struct zebra_privs_t bgpd_privs =
{
#if defined(QUAGGA_USER) && defined(QUAGGA_GROUP)
  .user = QUAGGA_USER,
  .group = QUAGGA_GROUP,
#endif
#ifdef VTY_GROUP
  .vty_group = VTY_GROUP,
#endif
  .caps_p = _caps_p,
  .cap_num_p = sizeof(_caps_p)/sizeof(_caps_p[0]),
  .cap_num_i = 0,
};

/* Help information display. */
static void
usage (char *progname, int status)
{
  if (status != 0)
    fprintf (stderr, "Try `%s --help' for more information.\n", progname);
  else
    {
      printf (
  "Usage : %s [OPTION...]\n"
  "\n"
  "Daemon which manages kernel routing table management and redistribution "
  "between different routing protocols.\n"
  "\n"
  "-d, --daemon       Runs in daemon mode\n"
  "-f, --config_file  Set configuration file name\n"
  "-i, --pid_file     Set process identifier file name\n"
  "-p, --bgp_port     Set bgp protocol's port number\n"
  "-l, --listenon     Listen on specified address (implies -n)\n"
  "-A, --vty_addr     Set vty's bind address\n"
  "-P, --vty_port     Set vty's port number\n"
  "-r, --retain       When program terminates, retain added route by bgpd.\n"
  "-n, --no_kernel    Do not install route to kernel.\n"
  "-u, --user         User to run as\n"
  "-g, --group        Group to run as\n"
  "-v, --version      Print program version\n"
  "-C, --dryrun       Check configuration for validity and exit\n"
  "-h, --help         Display this help and exit\n"
  "-t, --threaded     Use pthreads\n"
  "-I, --ignore_warnings  Ignore warnings while reading configuration file\n"
  "-2, --as2          Do not advertise AS4 capability\n"
  "\n"
  "Report bugs to %s\n", progname, ZEBRA_BUG_ADDRESS);
    }

  exit (status);
}
/*==============================================================================
 * Signal Handling.
 *
 * Actual signals are caught in lib/sigevent.  When a signal is caught, a flag
 * is set and the immediate signal handler returns.
 *
 * Those flags are polled in the qpnexus loop, and the Quagga level signal
 * handler called -- in the main (CLI) thread.
 */

/* signal definitions */
void sighup (void);
void sigint (void);
void sigusr1 (void);
void sigusr2 (void);

/* prototypes */
static void bgp_exit (int);
static void init_second_stage(int pthreads);
static void bgp_in_thread_init(void);
static void routing_start(void) ;
static void routing_finish(void) ;
static int routing_foreground(void);
static int routing_background(void);
static void sighup_action(mqueue_block mqb, mqb_flag_t flag);
static void sighup_enqueue(void);
static void sigterm_action(mqueue_block mqb, mqb_flag_t flag);
static void sigterm_enqueue(void);

static struct quagga_signal_t bgp_signals[] =
{
  {
    .signal = SIGHUP,
    .handler = &sighup,
  },
  {
    .signal = SIGUSR1,
    .handler = &sigusr1,
  },
  {
    .signal = SIGUSR2,
    .handler = &sigusr2,
  },
  {
    .signal = SIGINT,
    .handler = &sigint,
  },
  {
    .signal = SIGTERM,
    .handler = &sigint,
  },
};

/*------------------------------------------------------------------------------
 * SIGHUP handler.
 *
 * The vty level is reset, closing all terminals and vtysh servers, and
 * closing all listeners.
 *
 * A message is sent to the Routeing Engine to restart.
 *
 * When the Routeing Engine has restarted, it will send a message to the CLI
 * to restart the listeners.
 */
void
sighup (void)
{
  zlog (NULL, LOG_INFO, "SIGHUP received");

  vty_reset_because("Reloading configuration");
  sighup_enqueue();             /* tell the Routeing Engine     */

}

/* SIGINT handler. */
void
sigint (void)
{
  zlog_notice ("Terminating on signal");

  /* tell the routing engine to send notifies to peers and wait
   * for all sessions to be disabled */
  sigterm_enqueue();
}

/* SIGUSR1 handler. */
void
sigusr1 (void)
{
  zlog_rotate (NULL);
}

/* SIGUSR2 handler. */
void
sigusr2 (void)
{
  /* Used to signal message queues */
  if (qpthreads_enabled)
    return;
  else
    exit(1);
}

/*------------------------------------------------------------------------------
 * Final exit code...
 *
 * ...try to free up allocations we know about so that diagnostic tools such as
 * valgrind are able to better illuminate leaks.
 *
 * Zebra route removal and protocol teardown are not meant to be done here.
 * For example, "retain_mode" may be set.
 *
 * Note that by the time reach here, only the main (CLI) thread is running,
 *
 */
static void
bgp_exit (int status)
{
  struct bgp *bgp;
  struct listnode *node, *nnode;
  struct interface *ifp;
  extern struct zclient *zclient;
  extern struct zclient *zlookup;

  /* it only makes sense for this to be called on a clean exit */
  assert (status == 0);

  /* reverse bgp_master_init */
  for (ALL_LIST_ELEMENTS (bm->bgp, node, nnode, bgp))
    bgp_delete (bgp);
  list_free (bm->bgp);

  /* reverse bgp_zebra_init/if_init */
  if (retain_mode)
    if_add_hook (IF_DELETE_HOOK, NULL);
  for (ALL_LIST_ELEMENTS (iflist, node, nnode, ifp))
    {
      struct listnode *c_node, *c_nnode;
      struct connected *c;

      for (ALL_LIST_ELEMENTS (ifp->connected, c_node, c_nnode, c))
        bgp_connected_delete (c);

      if_delete (ifp);
    }
  list_free (iflist);

  /* curtains                                   */
  zlog_notice ("Terminated");

  /* reverse bgp_attr_init */
  bgp_attr_finish ();

  /* reverse bgp_dump_init */
  bgp_dump_finish ();

  /* reverse bgp_route_init */
  bgp_route_finish ();

  /* reverse bgp_route_map_init/route_map_init */
  route_map_finish ();

  /* reverse bgp_scan_init */
  bgp_scan_finish ();

  /* reverse access_list_init */
  access_list_add_hook (NULL);
  access_list_delete_hook (NULL);
  access_list_reset ();

  /* reverse bgp_filter_init */
  as_list_add_hook (NULL);
  as_list_delete_hook (NULL);
  bgp_filter_reset ();

  /* reverse prefix_list_init */
  prefix_list_add_hook (NULL);
  prefix_list_delete_hook (NULL);
  prefix_list_reset ();

  /* reverse community_list_init */
  community_list_terminate (bgp_clist);

  cmd_terminate ();
  vty_terminate ();
  if (zclient)
    zclient_free (zclient);
  if (zlookup)
    zclient_free (zlookup);

  /* reverse bgp_master_init */
  if (master)
    thread_master_free (master);

  if (zlog_default)
    closezlog (zlog_default);
  zlog_default = NULL ;

  if (qpthreads_enabled)
    {
      qpn_reset_free(routing_nexus);
      qpn_reset_free(bgp_nexus);
    } ;
  cli_nexus = qpn_reset_free(cli_nexus);

  if (CONF_BGP_DEBUG (normal, NORMAL))
    log_memstats_stderr ("bgpd");

  qexit (status);
}

/*------------------------------------------------------------------------------
 * Second stage initialisation and qpthreads_enabled.
 *
 * Really want to do this before the configuration file is read.  However,
 * also want to allow qpthreads to be enabled by configuration file.
 *
 * So... configuration file reader has a mechanism to look for a given
 * command as the *first* in the file and:
 *
 *   1. if it's there, invoke the command in the usual way
 *
 *   2. if it's not there, invoke the command but with a NULL set of arguments,
 *      which signals the "default" nature of the call.
 *
 * This mechanism is used so that the "threaded_cmd" is the time at which
 * second stage initialisation is done.  (But only once -- not on rereading
 * the configuration file.)
 */

/* Threaded command.  If present must be the first command in the
 * configuration file.  If not the first command it will log and abort.
 */
DEFUN_HID_CALL (threaded,
       threaded_cmd,
       "threaded",
       "Use pthreads\n")
{
  if (argv != NULL)
    config_threaded = 1 ;       /* Explicit command => turn on threading */

  if (!done_2nd_stage_init)
    init_second_stage(config_threaded) ;
  else
    vty_out(vty, "pthreads are %s\n", qpthreads_enabled ? "enabled"
                                                        : "disabled") ;
  return CMD_SUCCESS;
}

/* Enable or disables pthreads.  Create the nexus(es). Perform
 * any post nexus creation initialization. The nexus(es) need
 * to be created as soon as we know the pthread state so that
 * the message queues are available for the configuration data.
 */
static void
init_second_stage(int pthreads)
{
  assert(!done_2nd_stage_init) ;

  done_2nd_stage_init = 1;

  qlib_init_second_stage(pthreads);
  bgp_peer_index_mutex_init();

  /* Make nexus for main thread, always needed */
  cli_nexus = qpn_init_new(cli_nexus, 1); /* main thread */

  /* if using pthreads create additional nexus */
  if (qpthreads_enabled)
    {
      bgp_nexus     = qpn_init_new(bgp_nexus, 0);
      routing_nexus = qpn_init_new(routing_nexus, 0);
    }
  else
    {
      /* we all share the single nexus and single thread */
      bgp_nexus     = cli_nexus;
      routing_nexus = cli_nexus;
    }

  /* Tell thread stuff to use this qtimer pile                          */
  thread_set_qtimer_pile(routing_nexus->pile) ;

  /* Nexus hooks.
   * Beware if !qpthreads_enabled then there is only 1 nexus object
   * with all nexus pointers being aliases for it.
   */
  qpn_add_hook_function(&routing_nexus->in_thread_init, routing_start) ;
  qpn_add_hook_function(&bgp_nexus->in_thread_init,  bgp_in_thread_init) ;

  qpn_add_hook_function(&routing_nexus->in_thread_final, routing_finish) ;
  qpn_add_hook_function(&bgp_nexus->in_thread_final, bgp_close_listeners) ;

  qpn_add_hook_function(&routing_nexus->foreground, routing_foreground) ;
  qpn_add_hook_function(&bgp_nexus->foreground, bgp_connection_queue_process) ;

  qpn_add_hook_function(&routing_nexus->background, routing_background) ;

  confirm(qpn_hooks_max >= 2) ;

  /* vty and zclient can use either nexus or threads.
   * For bgp client we always want nexus, regardless of pthreads.
   */
  vty_init_r(cli_nexus, routing_nexus);
  zclient_init_r(routing_nexus);

  /* Now we have our nexus we can init BGP. */
  /* BGP related initialization.  */
  bgp_init ();

  /* Sort CLI commands. */
  sort_node ();
}

/* Main routine of bgpd. Treatment of argument and start bgp finite
   state machine is handled at here. */
int
main (int argc, char **argv)
{
  char *p;
  int  opt;
  bool daemon_mode = false ;
  bool dryrun      = false ;
  char *progname;
  int  tmp_port;

  /* Set umask before anything for security */
  umask (0027);

#ifdef QDEBUG
  fprintf(stderr, "%s\n", debug_banner);
#endif

  qlib_init_first_stage();

  /* Preserve name of myself. */
  progname = ((p = strrchr (argv[0], '/')) ? ++p : argv[0]);

  zlog_default = openzlog (progname, ZLOG_BGP,
			   LOG_CONS|LOG_NDELAY|LOG_PID, LOG_DAEMON);

  /* BGP master init. */
  bgp_master_init ();

  /* Command line argument treatment. */
  while (1)
    {
      opt = getopt_long (argc, argv, "df:i:hp:l:A:P:rnu:g:vCtI2", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  break;
	case 'd':
	  daemon_mode = true ;
	  break;
	case 'f':
	  config_file = optarg;
	  break;
        case 'i':
          pid_file = optarg;
          break;
	case 'p':
	  tmp_port = atoi (optarg);
	  if (tmp_port <= 0 || tmp_port > 0xffff)
	    bm->port = BGP_PORT_DEFAULT;
	  else
	    bm->port = tmp_port;
	  break;
	case 'A':
	  vty_addr = optarg;
	  break;
	case 'P':
          /* Deal with atoi() returning 0 on failure, and bgpd not
             listening on bgp port... */
          if (strcmp(optarg, "0") == 0)
            {
              vty_port = 0;
              break;
            }
          vty_port = atoi (optarg);
	  if (vty_port <= 0 || vty_port > 0xffff)
	    vty_port = BGP_VTY_PORT;
	  break;
	case 'r':
	  retain_mode = true ;
	  break;
	case 'l':
	  bm->address = optarg;
	  /* listenon implies -n */
	case 'n':
	  bgp_option_set (BGP_OPT_NO_FIB);
	  break;
	case 'u':
	  bgpd_privs.user = optarg;
	  break;
	case 'g':
	  bgpd_privs.group = optarg;
	  break;
	case 'v':
	  print_version (progname);
	  exit (0);
	  break;
	case 'C':
	  dryrun = true ;
	  break;
	case 'h':
	  usage (progname, 0);
	  break;
        case 't':
          config_threaded = true ;
          break;
        case 'I':
          config_ignore_warnings = true ;
	  break ;
        case '2':
          config_as2_speaker = true ;
          break ;
	default:
	  usage (progname, 1);
	  break;
	}
    }

  /* Make thread master. */
  master = bm->master;

  /* Initializations. */
  srand (time (NULL));
  signal_init (master, Q_SIGC(bgp_signals), bgp_signals);
  zprivs_init (&bgpd_privs);
  cmd_init (1);
  install_element (CONFIG_NODE, &threaded_cmd);
  vty_init (master);
  memory_init ();

  /* Read config file.
   *
   * NB: second state initialisation is done in the threaded_cmd, which must
   *     either be the first command in the file, or is executed by default
   *     before the first command in the file.
   *
   * NB: if fails to open the configuration file, fails to read anything, or
   *     it is completely empty (or effectively so), then may still need to do
   *     second stage initialisation.
   */
  done_2nd_stage_init = 0 ;

  vty_read_config_first_cmd_special(config_file, config_default,
                                        &threaded_cmd, config_ignore_warnings) ;

  if (!done_2nd_stage_init)
    init_second_stage(config_threaded) ;

  bm->as2_speaker = config_as2_speaker ;

  /* Start execution only if not in dry-run mode */
  if (dryrun)
    return(0);

  /* only the calling thread survives in the child after a fork
   * so ensure we haven't created any threads yet
   */
  assert(!qpthreads_thread_created);

  /* Turn into daemon if daemon_mode is set.    */
  if (daemon_mode && daemon (0, 0) < 0)
    {
      zlog_err("BGPd daemon failed: %s", errtoa(errno, 0).str);
      return (1);
    }

  /* Process ID file creation.                  */
  pid_output (pid_file);

  /* Ready to run VTY now.                      */
  vty_start(vty_addr, vty_port, BGP_VTYSH_PATH);

  /* Print banner. */
#ifdef QDEBUG
  zlog_notice("%s", debug_banner);
#endif
  zlog_notice ("BGPd %s%s starting: vty@%d, bgp@%s:%d",
               QUAGGA_VERSION,
               (qpthreads_enabled ? " pthreaded" : ""),
	       vty_port,
	       (bm->address ? bm->address : "<all>"),
	       (int)bm->port);

  /* Launch finite state machine(s) */
  if (qpthreads_enabled)
    {
      void * thread_result = NULL;

      qpn_exec(routing_nexus);
      qpn_exec(bgp_nexus);
      qpn_exec(cli_nexus);      /* must be last to start - on main thread */

      /* terminating, wait for all threads to finish */
      thread_result = qpt_thread_join(routing_nexus->thread_id);
      thread_result = qpt_thread_join(bgp_nexus->thread_id);
    }
  else
    {
      qpn_exec(cli_nexus);      /* only nexus - on main thread          */
    }

  /* Note that from this point forward is running in the main (CLI) thread
   * and any other threads have been joined and their nexuses freed.
   */
  bgp_exit(0);
}

/* bgp_nexus in-thread initialization                   */
static void
bgp_in_thread_init(void)
{
  bgp_open_listeners(bm->address, bm->port);
}

/* routing_nexus in-thread initialization -- for gdb !  */

static int routing_started = 0 ;

static void
routing_start(void)
{
  routing_started = 1 ;
}

static void
routing_finish(void)
{
  routing_started = 0 ;
}

/* legacy threads in routing engine                     */
static int
routing_foreground(void)
{
  return thread_dispatch(master) ;
}

/* background threads in routing engine */
static int
routing_background(void)
{
  return thread_dispatch_background(master) ;
}

/*==============================================================================
 * SIGHUP and SIGTERM
 */

/*------------------------------------------------------------------------------
 * SIGHUP: message sent to Routeing engine and the action it then takes.
 *
 * TODO: should SIGHUP be a priority message (!)
 */
static void
sighup_enqueue(void)
{
  mqueue_block mqb = mqb_init_new(NULL, sighup_action, NULL) ;

  mqueue_enqueue(routing_nexus->queue, mqb, mqb_priority) ;
}

/* dispatch a command from the message queue block */
static void
sighup_action(mqueue_block mqb, mqb_flag_t flag)
{
  if ((flag == mqb_action) && !program_terminating)
    {
      zlog_info ("bgpd restarting!");

      bgp_terminate (0, 0); /* send notifies */
      bgp_reset ();

      /* Reload config file. */
      vty_read_config (config_file, config_default);

      /* Create VTY's socket */
      vty_restart(vty_addr, vty_port, BGP_VTYSH_PATH);

      /* Try to return to normal operation. */
    }

  mqb_free(mqb);
}

/*------------------------------------------------------------------------------
 * Foreground task to see if all peers have been deleted yet.
 */
static int
program_terminate_if_all_peers_deleted(void)
{
  if (bm->peer_linger_count == 0)
    {
      /* ask remaining pthreads to die
       *
       * Note that qpn_terminate does nothing if it has been called once
       * already.
       */
      if (qpthreads_enabled && routing_nexus != NULL)
        qpn_terminate(routing_nexus);

      if (qpthreads_enabled && bgp_nexus != NULL)
        qpn_terminate(bgp_nexus);

      if (cli_nexus != NULL)
        qpn_terminate(cli_nexus) ;
    } ;

  return 0 ;            /* nothing to do, really.       */
} ;

/*------------------------------------------------------------------------------
 * SIGTERM: message sent to Routeing engine and the action it then takes.
 */
static void
sigterm_enqueue(void)
{
  mqueue_block mqb = mqb_init_new(NULL, sigterm_action, NULL) ;

  mqueue_enqueue(routing_nexus->queue, mqb, mqb_priority) ;
} ;

/* dispatch a command from the message queue block */
static void
sigterm_action(mqueue_block mqb, mqb_flag_t flag)
{
  if ((flag == mqb_action) && !program_terminating)
    {
      /* send notify to all peers, wait for all sessions to be disables
       * then terminate all pthreads
       */
      program_terminating = true ;

      bgp_terminate(1, retain_mode);

      qpn_add_hook_function(&routing_nexus->foreground,
                                       program_terminate_if_all_peers_deleted) ;
    }

  mqb_free(mqb);
} ;


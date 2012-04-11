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

#include "zebra.h"
#include "misc.h"

#include "memory.h"
#include "vector.h"
#include "vty.h"
#include "command.h"
#include "getopt.h"
#include "thread.h"
#include "lib/version.h"
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
#include "qfstring.h"

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
#include "bgpd/bgp_vty.h"
#include "bgpd/bgp_zebra.h"

/* Configuration file and directory.                                    */
char config_default[] = SYSCONFDIR BGP_DEFAULT_CONFIG;

/* Route retain mode flag.                                              */
static bool retain_mode         = false;

/* Have started terminating the program                                 */
static bool program_terminating = false ;

/* whether to ignore warnings in configuration file                     */
static bool config_ignore_warnings = false;

/* whether configured to run with qpthreads                             */
static bool pthreaded_option     = false ;

/* whether configured to run as an AS2 speaker                          */
static bool config_as2_speaker  = false ;

/* Process ID saved for use by init system                              */
static const char *pid_file = PATH_BGPD_PID;

/* VTY port number and address.                                         */
int   vty_port = BGP_VTY_PORT;
char *vty_addr = NULL;

/* privileges                                                           */
static zebra_capabilities_t _caps_p [] =
{
    ZCAP_BIND,
    ZCAP_NET_RAW,
    ZCAP_NET_ADMIN,
};

struct zebra_privs_t bgpd_privs =
{
#if defined(QUAGGA_USER) && defined(QUAGGA_GROUP)
  .user      = QUAGGA_USER,
  .group     = QUAGGA_GROUP,
#endif
#ifdef VTY_GROUP
  .vty_group = VTY_GROUP,
#endif
  .caps_p    = _caps_p,
  .cap_num_p = sizeof(_caps_p)/sizeof(_caps_p[0]),
  .cap_num_i = 0,
};

/*==============================================================================
 *
 */
static void bgp_signal_init(void) ;
static void bgp_cmd_init(void) ;

/* prototypes */
static void bgp_exit (int);
static void bgp_init_second_stage(bool pthreads);
static void bgp_in_thread_init(void);
static void routing_start(void) ;
static void routing_finish(void) ;
static int routing_foreground(void);
static int routing_background(void);
static void bgp_show_nexus_cmd_init(void) ;

/*==============================================================================
 * Arguments and the main function for bgpd.
 */

/*------------------------------------------------------------------------------
 * bgpd options, we use GNU getopt library.
 */
static const struct option longopts[] =
{
  { "as2",         no_argument,       NULL, '2'},
  { "vty_addr",    required_argument, NULL, 'A'},
  { "int_boot",    no_argument,       NULL, 'b'},
  { "dryrun",      no_argument,       NULL, 'C'},
  { "daemon",      no_argument,       NULL, 'd'},
  { "config_file", required_argument, NULL, 'f'},
  { "int_config",  required_argument, NULL, 'F'},
  { "group",       required_argument, NULL, 'g'},
  { "help",        no_argument,       NULL, 'h'},
  { "pid_file",    required_argument, NULL, 'i'},
  { "ignore_warnings", no_argument,   NULL, 'I'},
  { "listenon",    required_argument, NULL, 'l'},
  { "no_kernel",   no_argument,       NULL, 'n'},
  { "bgp_port",    required_argument, NULL, 'p'},
  { "vty_port",    required_argument, NULL, 'P'},
  { "integrated",  no_argument,       NULL, 'Q'},
  { "retain",      no_argument,       NULL, 'r'},
  { "threaded",    no_argument,       NULL, 't'},
  { "user",        required_argument, NULL, 'u'},
  { "version",     no_argument,       NULL, 'v'},
  { "watch-dog",   required_argument, NULL, 'W'},
  { "socket",      required_argument, NULL, 'z'},
  { 0 }
};

/*------------------------------------------------------------------------------
 * Help information display.
 */
static void
usage (const char *progname, int status)
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
  "-F, --int_config   Set integrated configuration file and name\n"
  "-Q, --integrated   Use integrated configuration file\n"
  "-b, --int_boot     Expect vtysh to provide configuration\n"
  "-i, --pid_file     Set process identifier file name\n"
  "-z, --socket       Set path of zebra socket\n"
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
  "-W, --watch-dog    Enable watch-dog pthread\n"
  "\n"
  "Report bugs to %s\n", progname, ZEBRA_BUG_ADDRESS);
    }

  exit (status);
}

/*------------------------------------------------------------------------------
 * Main routine of bgpd. Treatment of argument and start bgp finite
 * state machine is handled at here.
 */
int
main (int argc, char **argv)
{
  bool daemon_mode, dryrun, own_config, int_config, int_boot ;
  bool invalid_option ;
  const char* own_config_file ;
  const char* int_config_file ;

  if (qdebug)
    fprintf(stderr, debug_banner, argv[0], "\n") ;

  /* First things first -- and qlib_init_first_stage() is absolutely first.
   */
  qlib_init_first_stage(0027);

  host_init(argv[0]) ;

  zlog_default = openzlog (cmd_host_program_name(), ZLOG_BGP,
                                      LOG_CONS|LOG_NDELAY|LOG_PID, LOG_DAEMON) ;

  /* BGP master init -- creates and initialises the bm->master thread_master.
   */
  bgp_master_init ();

  /* Command line argument treatment.
   */
  daemon_mode     = false ;             /* no -d                */
  dryrun          = false ;             /* no -C                */
  own_config      = false ;             /* no -f xxxx           */
  own_config_file = config_default ;    /* ditto                */
  int_config      = false ;             /* no -F xxxx or -Q     */
  int_config_file = NULL ;              /* no -F                */
  int_boot        = false ;             /* no -b                */

  invalid_option  = false ;

  while (1)
    {
      int  result ;
      int  val ;
      int  opt;

      opt = getopt_long (argc, argv, "2A:bCdf:Fg:hi:Il:np:P:Qrtu:vW:z:",
                                                                 longopts, 0);
      if (opt == EOF)
        break;

      switch (opt)
        {
        case 0:
          break;

        /* Expect vtysh to provide configuration.
         */
        case 'b':
          int_boot    = true ;
          break;

        /* Enter daemon mode once read configuration.
         */
        case 'd':
          daemon_mode = true ;
          break;

        /* Set own configuration file name
         */
        case 'f':
          own_config_file = optarg;
          own_config     = true ;
          break;

        /* Set name for and use integrated configuration file
         */
        case 'F':
          int_config_file = optarg;
          int_config      = true ;
          break;

        /* Use integrated configuration file -- redundant if -F
         */
        case 'Q':
          int_config      = true ;
          break;

        /* Set pid file name
         */
        case 'i':
          pid_file = optarg;
          break;

        /* Set zebra server path.
         */
        case 'z':
          zclient_serv_path_set (optarg);
          break;

        /* Set port to listen on for BGP -- 0 => default.
         */
        case 'p':
          val = strtoul_xr(optarg, &result, NULL, 0, 0xFFFF) ;

          if (result >= 0)
            {
              if (val == 0)
                bm->port = BGP_PORT_DEFAULT ;
              else
                bm->port = val ;
            }
          else
            {
              fprintf(stderr, "%% invalid BGP port '-p %s'\n", optarg) ;
              invalid_option = true ;
            } ;
          break;

        /* Set address to listen on for VTY connections.
         */
        case 'A':
          vty_addr = optarg;
          break;

        /* Set port to listen on for VTY connections -- 0 => none.
         */
        case 'P':
          val = strtoul_xr(optarg, &result, NULL, 0, 0xFFFF) ;

          if (result >= 0)
            vty_port = val ;
          else
            {
              fprintf(stderr, "%% invalid BGP port '-p %s'\n", optarg) ;
              invalid_option = true ;
            } ;
          break;

        /* Retain routes after exit from bgpd
         */
        case 'r':
          retain_mode = true ;
          break;

        /* Set address to listen on for BGP
         */
        case 'l':
          bm->address = optarg;
          fall_through ;                /* NB: -l => -n                 */

        /* Set "no FIB" option
         */
        case 'n':
          bgp_option_set (BGP_OPT_NO_FIB);
          break;

        /* Set user to run as.
         */
        case 'u':
          bgpd_privs.user = optarg;
          break;

        /* Set group to run as.
         */
        case 'g':
          bgpd_privs.group = optarg;
          break;

        /* Print program version and exit.
         */
        case 'v':
          cmd_print_version (cmd_host_program_name());
          exit (0);
          break;

        /* Set "dryrun" option -- read config file and then exit
         */
        case 'C':
          dryrun = true ;
          break;

        /* Show "help"
         */
        case 'h':
          usage (cmd_host_program_name(), 0);
          break;

        /* Set threaded option
         */
        case 't':
          pthreaded_option = true ;
          break;

        /* Set ignore warnings for configuration reading
         */
        case 'I':
          config_ignore_warnings = true ;
          break ;

        /* Set ASN2 speaker state
         */
        case '2':
          config_as2_speaker = true ;
          break ;

        /* Enable Watch-Dog
         */
        case 'W':
          if (!qpn_wd_prepare(optarg))
            invalid_option = true ;
          break;

        /* Show usage "help" and exit
         */
        default:
          usage (cmd_host_program_name(), 1);
          break;
        }
    }

  if (invalid_option)
    exit(1) ;

  /* Initializations. */
  bgp_signal_init() ;

  zprivs_init_dry (&bgpd_privs, dryrun);        /* lowers privileges    */

  bgp_cmd_init() ;                              /* all commands         */

  vty_init ();

  /* Read config file.
   *
   * NB: second state initialisation is done in the threaded_cmd, which must
   *     either be the first command in the file, or is executed by default
   *     before the first command in the file.
   *
   * NB: if fails to open the configuration file, fails to read anything, or
   *     it is completely empty (or effectively so), then may still need to do
   *     second stage initialisation.
   *
   *     Also, if there is a vtysh integrated configuration file, will not
   *     read any bgpd configuration, so will still need to do the second stage
   *     initialisation.
   */
  cmd_host_config_set (own_config_file, own_config,
                       int_config_file, int_config, int_boot) ;

  cmd_host_pthreaded(pthreaded_option, bgp_init_second_stage) ;

  vty_read_config_new(config_ignore_warnings, false /* show_warnings */) ;

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
  if (qdebug)
    zlog_notice(debug_banner, argv[0], "");

  zlog_notice ("BGPd %s%s starting: vty@%d, bgp@%s:%d",
               QUAGGA_VERSION,
               (qpthreads_enabled ? " pthreaded" : ""),
               vty_port,
               (bm->address ? bm->address : "<all>"),
               (int)bm->port);

  /* Launch finite state machine(s) */
  if (qpthreads_enabled)
    {
      qpn_wd_start() ;          /* if set up                            */

      qpn_exec(routing_nexus);
      qpn_exec(bgp_nexus);
      qpn_exec(cli_nexus);      /* must be last to start - on main thread */

      /* terminating, wait for all threads to finish */
      qpt_thread_join(routing_nexus->thread_id);
      qpt_thread_join(bgp_nexus->thread_id);

//    qpn_wd_stop() ;           /* if started                           */
    }
  else
    {
      qpn_wd_start() ;          /* if set up                            */

      qpn_exec(cli_nexus);      /* only nexus - on main thread          */

//    qpn_wd_stop() ;           /* if started                           */
    }

  /* Note that from this point forward is running in the main (CLI) thread
   * and any other threads have been joined
   */
  qpt_clear_qpthreads_active() ;

  zprivs_terminate (&bgpd_privs);

  bgp_exit(0);
}

/*------------------------------------------------------------------------------
 * Initialise command handling and all bgpd nodes/commands
 */
static void
bgp_cmd_init(void)
{
  cmd_table_init (BGPD);

  bgp_vty_cmd_init() ;          /* Installs all the bgpd nodes  */

  bgp_dump_cmd_init() ;
  bgp_debug_cmd_init() ;
  bgp_filter_cmd_init() ;
  bgp_mplsvpn_cmd_init() ;
  bgp_route_cmd_init() ;
  bgp_route_map_cmd_init() ;
  bgp_scan_cmd_init() ;
  bgp_show_nexus_cmd_init() ;
  community_list_cmd_init() ;

  access_list_cmd_init() ;
  prefix_list_cmd_init();

  cmd_table_complete() ;
} ;

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
 *   2. if it's not there, invoke the command but with a *negative* count of
 *      arguments, which signals the "default" nature of the call.
 *
 * This mechanism is used so that the "threaded_cmd" is the time at which
 * second stage initialisation is done.  (But only once -- not on rereading
 * the configuration file.)
 */

/*------------------------------------------------------------------------------
 * Enable or disables pthreads.
 *
 * Create the nexus(es).
 *
 * Perform any post nexus creation initialization.
 *
 * The nexus(es) need to be created as soon as we know the pthread state so
 * that the message queues are available for configuration commands to send
 * messages to the bgp_engine.
 */
static void
bgp_init_second_stage(bool pthreads)
{
  qlib_init_second_stage(pthreads);

  bgp_peer_index_init_r();

  /* Make nexus for main thread, always needed */
  cli_nexus = qpn_init_new(cli_nexus, 1,                /* main thread  */
                         qpthreads_enabled ? "CLI"
                                           : "soliton");

  /* if using pthreads create additional nexus */
  if (qpthreads_enabled)
    {
      bgp_nexus     = qpn_init_new(bgp_nexus, 0, "BGP Engine");
      routing_nexus = qpn_init_new(routing_nexus, 0, "Routing Engine");
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
} ;

/*------------------------------------------------------------------------------
 * bgp_nexus in-thread initialization
 *
 * This is invoked when the BGP Engine nexus is started.
 */
static void
bgp_in_thread_init(void)
{
  bgp_open_listeners(bm->address, bm->port);
} ;

/*------------------------------------------------------------------------------
 * routing_nexus in-thread initialisation/finish -- for gdb !
 *
 * This is invoked when the Routing Engine nexus is started.
 */
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

/*------------------------------------------------------------------------------
 * legacy threads in routing engine
 */
static int
routing_foreground(void)
{
  return thread_dispatch(master) ;
}

/*------------------------------------------------------------------------------
 * background threads in routing engine
 */
static int
routing_background(void)
{
  return thread_dispatch_background(master) ;
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
 * so may release things that belong to any thread !
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

  /* dismantle the peer index   */
  bgp_peer_index_finish() ;

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
  prefix_list_reset (free_it);

  /* reverse community_list_init */
  community_list_terminate (bgp_clist);

  cmd_table_terminate ();
  vty_terminate ();

  if (zclient)
    zclient_free (zclient);
  if (zlookup)
    zclient_free (zlookup);

  if (zlog_default)
    closezlog (zlog_default);
  zlog_default = NULL ;

  if (qpthreads_enabled)
    {
      qpn_reset(routing_nexus, free_it);
      qpn_reset(bgp_nexus, free_it);
    } ;
  cli_nexus = qpn_reset(cli_nexus, free_it);

  host_finish() ;

  qexit (status, (CONF_BGP_DEBUG (normal, NORMAL) || qdebug)) ;
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

static void sighup (void);
static void sigterm (void);
static void sigusr1 (void);

static void sighup_suspend_action(void) ;
static void sighup_restart_action(void) ;
static void sigterm_action(void) ;

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
    .signal = SIGTERM,
    .handler = &sigterm,
  },
  {
    .signal = SIGINT,
    .handler = &sigterm,
  },
};

/*------------------------------------------------------------------------------
 *
 */
static void
bgp_signal_init(void)
{
  signal_init(master, Q_SIGC(bgp_signals), bgp_signals) ;
} ;

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
  const char* result ;

  result = vty_suspend("reloading configuration (SIGHUP)",
                                                         sighup_suspend_action);

  zlog_info("SIGHUP received: %s to reload configuration", result) ;
} ;

/*------------------------------------------------------------------------------
 * SIGINT and SIGTERM handler.
 */
void
sigterm(void)
{
  zlog_notice ("Terminating on signal (SIGTERM/SIGINT)");

  vty_stop("signal (SIGTERM/SIGINT)", sigterm_action);
}

/*------------------------------------------------------------------------------
 * SIGUSR1 handler.
 */
void
sigusr1 (void)
{
  zlog_rotate (NULL);
}

/*==============================================================================
 * SIGHUP and SIGTERM
 */

/*------------------------------------------------------------------------------
 * This runs in the Routing Engine.
 *
 * The SIGHUP was dealt with above (in the CLI thread), and has been passed,
 * message-wise to the Routing Engine -- as a priority message.  This at least
 * means that the Routing Engine completed any command that it might have been
 * executing when the SIGHUP arrived.
 *
 * The CLI thread has set about suspending any running VTY.
 *
 * When vty_dispatch_restart() is called, the vty will arrange to wait until
 * all VTY have suspended, and will then call sighup_restart_action.
 */
static void
sighup_suspend_action(void)
{
  zlog_info ("bgpd restarting!");

  bgp_terminate (false, false);
  bgp_reset ();

  vty_dispatch_restart(sighup_restart_action) ;
} ;

/*------------------------------------------------------------------------------
 * This runs in the Routing Engine.
 *
 * All vty are now suspended.  Can now re-read the configuration.  Once that
 * starts it will run to completion -- unless interrupted by SIGTERM/SIGINT.
 *
 * When vty_resume is called, any remaining vty will resume -- unless has
 * been interrupted by SIGTERM/SIGINT.
 */
static void
sighup_restart_action(void)
{
  vty_read_config_new(config_ignore_warnings, false /* show_warnings */) ;

  vty_resume() ;
} ;

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
 * This runs in the Routing Engine.
 *
 *
 */
static void
sigterm_action(void)
{
  if (!program_terminating)
    {
      /* send notify to all peers, wait for all sessions to be disables
       * then terminate all pthreads
       */
      program_terminating = true ;

      bgp_terminate(true, retain_mode);

      qpn_add_hook_function(&routing_nexus->foreground,
                                       program_terminate_if_all_peers_deleted) ;
    }
} ;

/*==============================================================================
 * CLI for showing what nexuses are up to
 */
static void bgp_do_show_nexus(struct vty* vty, qpn_nexus qpn) ;

DEFUN_CALL(bgp_show_nexus,
      bgp_show_nexus_cmd,
      "show nexus (all|cli|routing|bgp)",
      SHOW_STR
      "For nexus activity\n"
      "all nexuses\n"
      "CLI nexus\n"
      "Routing Engine\n"
      "BGP Engine\n")
{
  if ((*(argv[0]) == 'a') && qpthreads_enabled)
    {
      bgp_do_show_nexus(vty, cli_nexus) ;
      bgp_do_show_nexus(vty, bgp_nexus) ;
      bgp_do_show_nexus(vty, routing_nexus) ;
    }
  else
    {
      qpn_nexus qpn = cli_nexus ;

      if (qpthreads_enabled)
        {
          if (*(argv[0]) == 'r')
            qpn = routing_nexus ;
          else if (*(argv[0]) == 'b')
            qpn = bgp_nexus ;
        } ;

      bgp_do_show_nexus(vty, qpn) ;
    } ;

  return CMD_SUCCESS ;
} ;

CMD_INSTALL_TABLE(static, bgp_main_show_nexus_cmd_table, BGPD) =
{
  { VIEW_NODE,       &bgp_show_nexus_cmd                                },
  { ENABLE_NODE,     &bgp_show_nexus_cmd                                },

  CMD_INSTALL_END
} ;

static void
bgp_show_nexus_cmd_init(void)
{
  cmd_install_table(bgp_main_show_nexus_cmd_table) ;
} ;

static void
bgp_do_show_nexus(struct vty* vty, qpn_nexus qpn)
{
  qpn_stats_t prev ;
  qpn_stats_t curr ;

  qtime_t  now ;
  qtime_t  delta ;
  qtime_t  idle_delta ;

  qpn_get_stats(qpn, &curr, &prev) ;
  now = qt_get_monotonic() ;

  delta = curr.last_time - prev.last_time ;
  idle_delta = curr.idle - prev.idle ;

  vty_out(vty, "%16s updated %s ago: %s(%s/%s) cycles\n",
          qpn->name,
          qfs_time_period(curr.last_time - now, 0).str,
          qfs_dec_value(curr.cycles, pf_scale).str,
          qfs_dec_value(curr.cycles - prev.cycles, pf_scale | pf_plus_nz).str,
          qfs_time_period(delta, pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) active  %s(%s) idle\n",
      qfs_time_period((curr.last_time - curr.start_time) - curr.idle, 0).str,
      qfs_time_period(delta - idle_delta, pf_plus_nz).str,
      qfs_time_period(curr.idle, 0).str,
      qfs_time_period(idle_delta, pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) signals",
      qfs_dec_value(curr.signals, pf_scale).str,
      qfs_dec_value(curr.signals - prev.signals, pf_scale | pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) foreg.",
      qfs_dec_value(curr.foreg, pf_scale).str,
      qfs_dec_value(curr.foreg - prev.foreg, pf_scale | pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) dispatch\n",
      qfs_dec_value(curr.dispatch, pf_scale).str,
      qfs_dec_value(curr.dispatch - prev.dispatch, pf_scale | pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) i/o act.",
      qfs_dec_value(curr.io_acts, pf_scale).str,
      qfs_dec_value(curr.io_acts - prev.io_acts, pf_scale | pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) timers",
      qfs_dec_value(curr.timers, pf_scale).str,
      qfs_dec_value(curr.timers - prev.timers, pf_scale | pf_plus_nz).str) ;

  vty_out(vty, "  %s(%s) backg.\n",
      qfs_dec_value(curr.backg, pf_scale).str,
      qfs_dec_value(curr.backg - prev.backg, pf_scale | pf_plus_nz).str) ;
} ;



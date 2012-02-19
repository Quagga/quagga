/* main routine.
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
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
#ifndef VTYSH_EXTRACT_PL

#include <zebra.h>

#include <lib/version.h>
#include "qlib_init.h"
#include "getopt.h"
#include "command.h"
#include "thread.h"
#include "filter.h"
#include "memory.h"
#include "prefix.h"
#include "log.h"
#include "privs.h"
#include "sigevent.h"

#include "zebra/rib.h"
#include "zebra/zserv.h"
#include "zebra/rtread.h"
#include "zebra/debug.h"
#include "zebra/router-id.h"
#include "zebra/interface.h"

/* Zebra instance */
struct zebra_t zebrad =
{
  .rtm_table_default = 0,
};

/* process id. */
pid_t pid;

/* zebra_rib's workqueue hold time. Private export for use by test code only */
extern int rib_process_hold_time;

/* Command line options. */
struct option longopts[] =
{
  { "batch",       no_argument,       NULL, 'b'},
  { "daemon",      no_argument,       NULL, 'd'},
  { "config_file", required_argument, NULL, 'f'},
  { "help",        no_argument,       NULL, 'h'},
  { "vty_addr",    required_argument, NULL, 'A'},
  { "vty_port",    required_argument, NULL, 'P'},
  { "version",     no_argument,       NULL, 'v'},
  { "rib_hold",	   required_argument, NULL, 'r'},
  { 0 }
};

zebra_capabilities_t _caps_p [] =
{
  ZCAP_NET_ADMIN,
  ZCAP_SYS_ADMIN,
  ZCAP_NET_RAW,
};

/* Default configuration file path. */
char config_default[] = SYSCONFDIR DEFAULT_CONFIG_FILE;

/* Process ID saved for use by init system */
const char *pid_file = PATH_ZEBRA_PID;

/* Help information display. */
static void
usage (const char *progname, int status)
{
  if (status != 0)
    fprintf (stderr, "Try `%s --help' for more information.\n", progname);
  else
    {
      printf ("Usage : %s [OPTION...]\n\n"\
	      "Daemon which manages kernel routing table management and "\
	      "redistribution between different routing protocols.\n\n"\
	      "-b, --batch        Runs in batch mode\n"\
	      "-d, --daemon       Runs in daemon mode\n"\
	      "-f, --config_file  Set configuration file name\n"\
	      "-A, --vty_addr     Set vty's bind address\n"\
	      "-P, --vty_port     Set vty's port number\n"\
	      "-r, --rib_hold	  Set rib-queue hold time\n"\
              "-v, --version      Print program version\n"\
	      "-h, --help         Display this help and exit\n"\
	      "\n"\
	      "Report bugs to %s\n", progname, ZEBRA_BUG_ADDRESS);
    }

  exit (status);
}

static unsigned int test_ifindex = 0;

/* testrib commands */
DEFUN (test_interface_state,
       test_interface_state_cmd,
       "state (up|down)",
       "configure interface\n"
       "up\n"
       "down\n")
{
  struct interface *ifp;
  if (argc < 1)
    return CMD_WARNING;

  ifp = vty->index;
  if (ifp->ifindex == IFINDEX_INTERNAL)
    {
      ifp->ifindex = ++test_ifindex;
      ifp->mtu = 1500;
      ifp->flags = IFF_BROADCAST|IFF_MULTICAST;
    }

  switch (argv[0][0])
    {
      case 'u':
        SET_FLAG (ifp->flags, IFF_UP);
        if_add_update (ifp);
        printf ("up\n");
        break;
      case 'd':
        UNSET_FLAG (ifp->flags, IFF_UP);
        if_delete_update (ifp);
        printf ("down\n");
        break;
      default:
        return CMD_WARNING;
    }
  return CMD_SUCCESS;
}

/* Note that this command is not to be installed for vtysh
 */
CMD_INSTALL_TABLE(static, zebra_test_cmd_table, ZEBRA) =
{
  { INTERFACE_NODE,  &test_interface_state_cmd, 0, ~VTYSH_VD },

  CMD_INSTALL_END
} ;

static void
test_cmd_init (void)
{
  cmd_install_table(zebra_test_cmd_table) ;
}

/* SIGHUP handler. */
static void
sighup (void)
{
  zlog_info ("SIGHUP received");

  /* Reload of config file. */
  ;
}

/* SIGINT handler. */
static void
sigint (void)
{
  zlog_notice ("Terminating on signal");

  exit (0);
}

/* SIGUSR1 handler. */
static void
sigusr1 (void)
{
  zlog_rotate (NULL);
}

struct quagga_signal_t zebra_signals[] =
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
    .signal = SIGINT,
    .handler = &sigint,
  },
  {
    .signal = SIGTERM,
    .handler = &sigint,
  },
};

/* Main startup routine. */
int
main (int argc, char **argv)
{
  char *vty_addr = NULL;
  int vty_port = 0;
  int batch_mode = 0;
  int daemon_mode = 0;
  char *config_file = NULL;
  struct thread thread;

  /* First things first -- and qlib_init_first_stage() is absolutely first.
   */
  qlib_init_first_stage(0027);
  host_init(argv[0]) ;

  zlog_default = openzlog (cmd_host_program_name(), ZLOG_ZEBRA,
			   LOG_CONS|LOG_NDELAY|LOG_PID, LOG_DAEMON);

  while (1)
    {
      int opt;

      opt = getopt_long (argc, argv, "bdf:hA:P:r:v", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  break;
	case 'b':
	  batch_mode = 1;
	case 'd':
	  daemon_mode = 1;
	  break;
	case 'f':
	  config_file = optarg;
	  break;
	case 'A':
	  vty_addr = optarg;
	  break;
	case 'P':
	  /* Deal with atoi() returning 0 on failure, and zebra not
	     listening on zebra port... */
	  if (strcmp(optarg, "0") == 0)
	    {
	      vty_port = 0;
	      break;
	    }
	  vty_port = atoi (optarg);
	  break;
	case 'r':
	  rib_process_hold_time = atoi(optarg);
	  break;
	case 'v':
	  cmd_print_version (cmd_host_program_name());
	  exit (0);
	  break;
	case 'h':
	  usage (cmd_host_program_name(), 0);
	  break;
	default:
	  usage (cmd_host_program_name(), 1);
	  break;
	}
    }

  /* port and conf file mandatory */
  if (!vty_port || !config_file)
    {
      fprintf (stderr, "Error: --vty_port and --config_file arguments"
                       " are both required\n");
      usage (cmd_host_program_name(), 1);
    }

  /* Make master thread emulator. */
  zebrad.master = master ;

  /* Vty related initialize. */
  signal_init (zebrad.master, Q_SIGC(zebra_signals), zebra_signals);

  cmd_table_init (ZEBRA);

  access_list_cmd_init ();
  zebra_debug_cmd_init ();
  zebra_vty_cmd_init();

  cmd_table_complete();

  vty_init ();

  if_init();
  zebra_debug_init ();
  zebra_if_init ();
  test_cmd_init ();

  /* Zebra related initialize. */
  rib_init ();
  access_list_init ();

  /* Make kernel routing socket. */
  kernel_init ();
  route_read ();
  zebra_vty_init();

  /* Sort VTY commands. */
  sort_node ();

  /* Configuration file read*/
  vty_read_config (config_file, config_default);

  /* Clean up rib. */
  rib_weed_tables ();

  /* Exit when zebra is working in batch mode. */
  if (batch_mode)
    exit (0);

  /* Daemonize. */
  if (daemon_mode && daemon (0, 0) < 0)
    {
      perror("daemon start failed");
      exit (1);
    }

  /* Needed for BSD routing socket. */
  pid = getpid ();

  /* Make vty server socket. */
  vty_start(vty_addr, vty_port, "/tmp/test_zebra");

  /* Print banner. */
  zlog_notice ("Zebra %s starting: vty@%d", QUAGGA_VERSION, vty_port);

  while (thread_fetch (zebrad.master, &thread))
    thread_call (&thread);

  /* Not reached... */
  return 0;
}

#endif /* VTYSH_EXTRACT_PL */

/*
 * $Id: main.c,v 1.1 2005/04/25 16:42:24 paul Exp $
 *
 * This file is part of Quagga.
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <zebra.h>

#include <lib/version.h>
#include "getopt.h"
#include "qlib_init.h"
#include "thread.h"
#include "vty.h"
#include "command.h"
#include "memory.h"

extern void test_init();

struct option longopts[] =
{
  { "daemon",      no_argument,       NULL, 'd'},
  { "config_file", required_argument, NULL, 'f'},
  { "help",        no_argument,       NULL, 'h'},
  { "vty_addr",    required_argument, NULL, 'A'},
  { "vty_port",    required_argument, NULL, 'P'},
  { "version",     no_argument,       NULL, 'v'},
  { 0 }
};

DEFUN (daemon_exit,
       daemon_exit_cmd,
       "daemon-exit",
       "Make the daemon exit\n")
{
  exit(0);
}

extern int test_timer (struct thread *thread) ;

static int timer_count;
extern int
test_timer (struct thread *thread)
{
  int *count = THREAD_ARG(thread);

  printf ("run %d of timer\n", (*count)++);
  thread_add_timer (master, test_timer, count, 5);
  return 0;
}

static void
test_timer_init()
{
  thread_add_timer (master, test_timer, &timer_count, 10);
}

static void
test_vty_init()
{
  cmd_install_command (VIEW_NODE, &daemon_exit_cmd, BASIC_VD);  /* TODO */
}

/* Help information display. */
static void
usage (const char *progname, int status)
{
  if (status != 0)
    fprintf (stderr, "Try `%s --help' for more information.\n", progname);
  else
    {
      printf ("Usage : %s [OPTION...]\n\
Daemon which does 'slow' things.\n\n\
-d, --daemon       Runs in daemon mode\n\
-f, --config_file  Set configuration file name\n\
-A, --vty_addr     Set vty's bind address\n\
-P, --vty_port     Set vty's port number\n\
-v, --version      Print program version\n\
-h, --help         Display this help and exit\n\
\n\
Report bugs to %s\n", progname, ZEBRA_BUG_ADDRESS);
    }
  exit (status);
}


/* main routine. */
int
main (int argc, char **argv)
{
  char *vty_addr = NULL;
  int vty_port = 4000;
  int daemon_mode = 0;
  struct thread thread;
  char *config_file = NULL;

  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  while (1)
    {
      int opt;

      opt = getopt_long (argc, argv, "dhf:A:P:v", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  break;
        case 'f':
          config_file = optarg;
          break;
	case 'd':
	  daemon_mode = 1;
	  break;
	case 'A':
	  vty_addr = optarg;
	  break;
	case 'P':
          /* Deal with atoi() returning 0 on failure */
          if (strcmp(optarg, "0") == 0)
            {
              vty_port = 0;
              break;
            }
          vty_port = atoi (optarg);
          vty_port = (vty_port ? vty_port : 4000);
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

  /* Library inits. */
  cmd_table_init (BASIC_VD);

  test_vty_init ();

  cmd_table_complete();

  /* Other initialisation
   */
  vty_init ();

  /* Change to the daemon program. */
  if (daemon_mode && daemon (0, 0) < 0)
    {
      fprintf(stderr, "daemon failed: %s", strerror(errno));
      exit (1);
    }

  /* Create VTY socket */
  vty_start(vty_addr, vty_port, "/tmp/.heavy.sock");

  /* Configuration file read*/
  if (!config_file)
    usage (cmd_host_program_name(), 1);
  vty_read_config (config_file, NULL);

  test_timer_init();

  test_init();

  /* Fetch next active thread. */
  while (thread_fetch (master, &thread))
    thread_call (&thread);

  /* Not reached. */
  exit (0);
}


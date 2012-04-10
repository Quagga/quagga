/* Virtual terminal interface shell.
 * Copyright (C) 2000 Kunihiro Ishiguro
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

#include <zebra.h>

#include <sys/un.h>
#include <setjmp.h>
#include <sys/wait.h>
#include <pwd.h>
#include <stdio.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "lib/version.h"
#include "lib/qlib_init.h"
#include "getopt.h"
#include "command.h"
#include "memory.h"

#include "vtysh/vtysh.h"
#include "vtysh/vtysh_user.h"

#include "lib/vty_local.h"
#include "lib/vty_command.h"
#include "lib/vty_io.h"
#include "lib/vty_vtysh.h"

#include "lib/distribute.h"
#include "lib/filter.h"
#include "lib/if.h"
#include "lib/if_rmap.h"
#include "lib/keychain.h"
#include "lib/plist.h"
#include "lib/routemap.h"

/*==============================================================================
 * Local variables.
 */

/* History file name for interactive mode
 */
static char history_file[MAXPATHLEN];

/* Command logging for -c
 */
static FILE *logfile;

/*==============================================================================
 * Signal handling.
 */
volatile sig_atomic_t execute_flag = 0 ;

static volatile sig_atomic_t in_readline  = false ;
static volatile sig_atomic_t in_child     = false ;
static volatile sig_atomic_t in_progress  = false ;

static sigjmp_buf jmpbuf ;      /* siglongjmp used in interactive mode  */

#if 0

static volatile sig_atomic_t jmpflag = false ;  /* true => siglongjmp   */

static volatile sig_atomic_t exitflag = false ; /* true => exit on ^C   */
#endif

/*------------------------------------------------------------------------------
 * SIGINT handler.  This function takes care of user's ^C input.
 *
 * Note that exitflag takes precedence.
 */
static void
sigint (int sig)
{
  /* If readline is executing...
   */
  if (in_readline)
    {
//    fprintf(stderr, "[^C-in_readline]") ;

      in_readline = false ;

      siglongjmp (jmpbuf, vtysh_lj_ctrl_c) ;
    } ;

  /* If child process is executing...
   */
  if (in_child)
    return ;

  /* Hit the vty with a cancel
   */
  if (in_progress)
    {
//    fprintf(stderr, "[^C-in_progress]") ;
      vty_SIGINT = true ;
    } ;

#if 0
  if (exitflag)
    exit(99) ;

///* Check this process is not child process. */
//if (! execute_flag)
//  {
//    rl_initialize ();
//    printf ("\n");
//    rl_forced_update_display ();
//  }
  if (jmpflag == false)
    return ;


#endif
} ;

/*------------------------------------------------------------------------------
 * SIGTSTP handler.  This function takes care of user's ^Z input.
 */
static void
sigtstp (int sig)
{
  /* If readline is executing...
   */
  if (in_readline)
    {
      fprintf(stderr, "...seen ^Z") ;

      in_readline = false ;

      siglongjmp (jmpbuf, vtysh_lj_ctrl_z) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * SIGCHLD handler.
 *
 * This exists only so that a pselect() will be interrupted when a shell pipe
 * completes.
 */
static void
sigchld (int sig)
{
} ;

/*------------------------------------------------------------------------------
 * Set signal handler for given signal.
 *
 * We don't use sigevent because vtysh doesn't use threads.
 */
static RETSIGTYPE *
vtysh_signal_set (int signo, void (*func)(int))
{
  int ret;
  struct sigaction sig;
  struct sigaction osig;

  sig.sa_handler = func;
  sigemptyset (&sig.sa_mask);
  sig.sa_flags = 0;
#ifdef SA_RESTART
  sig.sa_flags |= SA_RESTART;
#endif /* SA_RESTART */

  ret = sigaction (signo, &sig, &osig);

  if (ret < 0)
    return (SIG_ERR);
  else
    return (osig.sa_handler);
}

/*------------------------------------------------------------------------------
 * Initialization of signal handlers.
 */
static void
vtysh_signal_init ()
{

#if 0
  jmpflag    = false ;                          /* do not siglongjmp    */
  exitflag   = false ;                          /* do not exit on ^C    */
#endif

  in_readline  = false ;
  in_child     = false ;
  in_progress  = false ;

  vtysh_signal_set (SIGINT,  sigint) ;          /* ^C                   */
  vtysh_signal_set (SIGTSTP, sigtstp) ;         /* ^Z                   */

  vtysh_signal_set (SIGCHLD, sigchld) ;         /* child process exit   */

  vtysh_signal_set (SIGPIPE, SIG_IGN) ;         /* deal with as error   */
} ;

/*==============================================================================
 * Logging
 */


/*------------------------------------------------------------------------------
 *
 */
static void log_it(const char *line)
{
  time_t t = time(NULL);
  struct tm *tmp = localtime(&t);
  const char *user = getenv("USER") ? : "boot";
  char tod[64];

  strftime(tod, sizeof tod, "%Y%m%d-%H:%M.%S", tmp);

  fprintf(logfile, "%s:%s %s\n", tod, user, line);
}

/*==============================================================================
 * VTY shell options and main routine.
 */
typedef enum
{
  vtysh_mode_dryrun,
  vtysh_mode_command,
  vtysh_mode_boot,
  vtysh_mode_interactive,

} vtysh_mode_t ;

typedef struct vtysh_options
{
  bool no_error ;
  bool quiet ;
  bool echo_command ;
  bool ignore_warnings ;
  uint lexical_level ;

  daemon_set_t daemons, required ;

} vtysh_options_t ;

static int vtysh_cmd_mode(vty vtysh, qstring commands,
                                                     vtysh_options_t* options) ;
static int vtysh_boot_mode(vty vtysh, vtysh_options_t* options) ;
static int vtysh_interactive_mode(vty vtysh, vtysh_options_t* options) ;

/*------------------------------------------------------------------------------
 * Help information display.
 */
static void
usage (int status)
{
  if (status != 0)
    fprintf (stderr, "Try `%s --help' for more information.\n",
                                                       cmd_host_program_name());
  else
    printf (
      "Usage : %s [OPTION...]\n"
      "\n"
      "Integrated shell for Quagga routing software suite. \n"
      "\n"
      "-C, --dryrun             Check configuration for validity and exit\n"
      "-b, --boot               Execute boot startup configuration\n"
      "-c, --command            Execute argument as command\n"
      "-d, --daemon             Connect only to the specified daemon(s)\n"
      "-f, --config_file        Set configuration file name\n"
      "-F, --int_config         Set integrated configuration file name\n"
      "-Q, --integrated         Use integrated configuration file\n"
      "-E, --echo               Echo prompt and command in -c mode\n"
      "-I, --ignore_warnings    Ignore warnings in config files and -c mode\n"
      "-1, --lexical-level-1    For -c: execute at lexical-level 1\n"
      "-P, --prefix-off         Turn off [daemon] output prefix\n"
      "-q, --quiet              Suppress output in -c mode\n"
      "-h, --help               Display this help and exit\n"
      "\n" \
      "Note that multiple commands may be executed from the command\n"
      "line by passing multiple -c args, or by embedding linefeed\n"
      "characters in one or more of the commands.\n"
      "\n"
      "Report bugs to %s\n", cmd_host_program_name(), ZEBRA_BUG_ADDRESS);

  exit (status);
}

/*------------------------------------------------------------------------------
 * VTY shell options, we use GNU getopt library.
 */
struct option longopts[] =
{
  { "boot",                 no_argument,             NULL, 'b'},
  { "command",              required_argument,       NULL, 'c'},
  { "lexical-level-1",      no_argument,             NULL, '1'},
  { "daemon",               required_argument,       NULL, 'd'},
  { "config_file",          required_argument,       NULL, 'f'},
  { "int_config",           required_argument,       NULL, 'F'},
  { "integrated",           no_argument,             NULL, 'Q'},
  { "dryrun",               no_argument,             NULL, 'C'},
  { "echo",                 no_argument,             NULL, 'E'},
  { "ignore_warnings",      no_argument,             NULL, 'I'},
  { "prefix-off",           no_argument,             NULL, 'P'},
  { "quiet",                no_argument,             NULL, 'q'},

  { "help",                 no_argument,             NULL, 'h'},

  /* The following are maintained for compatibility with older versions,
   * and are not documented in the help, above.
   */
  { "eval",                 required_argument,       NULL, 'e'},
  { "noerror",              no_argument,             NULL, 'n'},

  { 0 }
} ;

/*------------------------------------------------------------------------------
 * vtysh
 *
 * Runs in three modes, depending on the arguments:
 *
 *   * command line command mode: if at least one -c/--command is given
 *
 *   * boot mode: if -b/--boot is given (and there is no -c/--command)
 *
 *   * interactive mode: if neither of the above
 *
 * Having read the arguments:
 *
 *   * set up the configuration files.
 *
 *     In the absence of -F and/or -Q and/or -b, or in the presence of -f:
 *
 *       * service integrated-vtysh-config is set "off", before any
 *         configuration is read.
 *
 *       * the vtysh will start up reading the vtysh's own configuration
 *         (if any -- it is not an error if no file exists, unless -f is
 *         specified.)
 *
 *       * the ~~/ directory will be set to suit
 *
 *     If -F and/or -Q and/or -b are specified (and -f is not):
 *
 *       * service integrated-vtysh-config is set "on", before any
 *         configuration is read.
 *
 *       * the vtysh will start up reading the integrated configuration
 *
 *       * the ~~/ directory will be set to suit
 *
 *     NB: when reading the vtysh own config the "~~/" directory is either that
 *         of the vtysh own config, or that of the integrated config, depending.
 *
 *         Also, if "service integrated-vtysh-config" is set or cleared in the
 *         configuration, the "~~/" directory can change.
 *
 *     The "~~/" directory is a moveable feast for vtysh !
 *
 *   * if there is a -C/--dryrun option, then return 0.
 *
 *   * if there is at least one -c/--command, do that and return
 *
 *   * if there is -b/--boot, do that and return
 *
 *   * otherwise: run interactive command handling
 */
int
main (int argc, char **argv, char **env)
{
  int opt;
  const char* p ;

  int          exit_code ;
  vtysh_mode_t mode ;

  const char* own_config_file  = SYSCONFDIR VTYSH_DEFAULT_CONFIG ;
  const char* int_config_file  = NULL ;

  qstring daemon_list = NULL ;
  qstring commands    = NULL ;

  vtysh_options_t options ;

  bool dryrun              = false ;
  bool boot_flag           = false ;
  bool own_config          = false ;
  bool int_config          = false ;
  bool no_prefix           = false ;

  options.no_error         = false ;
  options.quiet            = false ;
  options.echo_command     = false ;
  options.ignore_warnings  = false ;
  options.lexical_level    = 0 ;
  options.daemons          = ALL_RDS ;
  options.required         = 0 ;

  if (qdebug)
    fprintf(stderr, "vtysh -- qdebug mode\n") ;

  /* First things first -- and qlib_init_first_stage() is absolutely first.
   */
  qlib_init_first_stage(0027);
  host_init(argv[0]) ;

  /* And as early as possible set stdout buffer, line buffered
   */
  setvbuf(stdout, NULL, _IOLBF, 1000) ;

  /* if logging open now
   *
   * TODO: this is only used in -c and does not report any problems opening
   *       etc...
   *
   *       Should this log -b ?
   *
   *       Should this log interactive sessions ?
   *
   *       What about more general logging and vtysh ?
   */
  if ((p = getenv("VTYSH_LOG")) != NULL)
    logfile = fopen(p, "a");

  /* Option handling.
   */
  while (1)
    {
      opt = getopt_long (argc, argv, "bc:e:d:f:F:QnCEIqPh1", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  break;

	case 'b':
	  boot_flag       = true;
	  break;

        /* collect one or more -c inline commands.
         *
         * Separate with "\n", so each -c will generate at least a blank
         * line !
         */
	case 'c':
	case 'e':
	  commands = qs_trim(commands, '\n') ;
          commands = qs_append_str(commands, optarg) ;
	  break;

        case '1':
          options.lexical_level = 1 ;
          break;

	/* Collect daemon names to connect to
         *
         * Must be able to connect to all the named daemons, or will fail.
         *
         * Here we append all -d options together, terminating each by ' '
         * and discarding any leading "=".
         *
         * When those are processed the resulting string is treated as
         * a list of daemon names separated by (\s*,\s*)+
         *
         * Can, therefore: -d bgpd -d zebra
         *             or: -d=bgpd,zebra
         *             or: -d "  bgpd , zebra  "
         *
         * and so on.
         */
	case 'd':
          p = optarg ;
          while ((*p == ' ') || (*p == '='))
            ++p ;

          daemon_list = qs_trim(daemon_list, ' ') ;
          daemon_list = qs_append_str(daemon_list, p) ;
	  break;

	/* Specify vtysh own configuration file
	 *
	 * See notes on -f, -F and -Q.
	 */
        case 'f':
          own_config_file = optarg;
          own_config      = true ;
          break;

        /* Specify integrated configuration file
         *
         * See notes on -f, -F and -Q.
         */
        case 'F':
          int_config_file = optarg;
          int_config      = true ;
          break;

        /* Use vtysh own configuration file (Quagga.conf)
         *
         * See notes on -f, -F and -Q.
         */
        case 'Q':
          int_config      = true ;
          break;

        /* Suppress output -- deprecated
         *
         * Turns off -E.  Acts as -I and -q together.
         *
         * For -c, turns off *all* output while executing commands.
         */
        case 'n':
          options.no_error        = true ;

          options.echo_command    = false ;
          options.ignore_warnings = true ;
          options.quiet           = true ;
          break;

        /* Dry run -- does everything up to and including reading vtysh
         * configuration and connecting to daemons, but does not do -b/-c or
         * enter interactive mode.
         */
        case 'C':
          dryrun          = true ;
          break;

        /* For -c -- echo command lines -- suppressed by -q.
         */
	case 'E':
	  options.echo_command    = true;
	  break;

	/* Ignore warnings: do not report warnings and do not stop (default is
	 *                  the opposite).
	 *
	 * Applies while reading configuration files and while executing -c.
	 *
	 * Does not apply to interactive mode, except when reading vtysh
	 * configuration.
	 */
        case 'I':
          options.ignore_warnings = true ;
          break;

        /* quiet: reduce output.
         *
         * In general: suppress hello and other such messages, so suppress
         *             anything which is not generated by the execution of a
         *             command
         *
         * For configuration files and -c: suppress ordinary command output.
         * (This is the default for configuration files.)
         *
         * For interactive mode: no other effect.
         */
        case 'q':
          options.quiet           = true ;
          break;

        /* No Prefix -- suppress "[<name>] " prefix on output
         */
        case 'P':
          no_prefix       = true ;
          break;

        /* help
         */
        case 'h':
          usage (0);
          break;

        /* unrecognised option
         */
	default:
	  usage (1);
	  break;
	} ;
    } ;

  /* Set up own signal handling.
   *
   * Note that since the vtysh does not use either legacy thread or the
   * qpthreads, we avoid using the library thread handling.
   */
  vtysh_signal_init ();

  /* Do cmd_table_init() -- starts in ENABLE_NODE -- and install vtysh own
   * commands and all commands collected from all supported daemons.
   */
  vtysh_cmd_init() ;

  /* Will not be qpthread -- after all commands have been installed
   */
  qlib_init_second_stage(false) ;

  /* Set up the configuration files and initial service integrated-vtysh-config.
   *
   * NB: host.config_file is set up to be:
   *
   *       own_config_file, if -f, or if none of -F, -Q and -b
   *       int_config_file, otherwise.
   *
   *     service integrated-vtysh-config is set:
   *
   *       false, if none of -F, -Q and -b.
   *       true,  otherwise
   *
   *     host.config_dir is set follow service integrated-vtysh-config
   */
  cmd_host_config_set (own_config_file, true,
                       int_config_file,
                               (int_config || boot_flag) & !own_config, false) ;

  vtysh_set_integrated_config(int_config || boot_flag) ;

  /* Set up the vtysh_vty -- from now on, can output via this -- and vty_err()
   * will go to stderr !
   */
  vtysh_vty_init(no_prefix) ;           /* set up the vtysh_vty */

  /* Other initialisation.
   */
  vtysh_user_init ();

  prefix_list_init() ;
  access_list_init() ;
  if_rmap_init() ;
  keychain_init() ;

  /* Establish the mode and worry about duff argument combinations.
   */
  mode = vtysh_mode_dryrun ;    /* -C/--dryrun takes absolute priority  */

  if (commands != NULL)
    {
      /* -c/--command -- takes precedence
       *
       * There is no: -C/--dryrun
       *
       * Ignores: -b/--boot
       *
       * -E/--echo is overridden by -q/--quiet (don't report -n/--noerror)
       *
       * -q/--quiet           applies to commands and to reporting connections
       * -I/--ignore_warnings applies to commands and to vtysh config
       */
      if (!dryrun)
        mode = vtysh_mode_command ;

      if (boot_flag)
        fprintf(stderr, "-b/--boot ignored in -c/--command mode\n") ;

      if (options.echo_command && options.quiet)
        {
          fprintf(stderr, "-E/--echo overridden by -q/--quiet\n") ;
          options.echo_command = false ;
        } ;
    }
  else if (boot_flag)
    {
      /* -b/--boot takes precedence
       *
       * There is no: -c/--command (or -e/--eval)
       *
       * Ignores: -E/--echo (don't report -n/--noerror)
       *
       * -q/--quiet           applies to reporting connections etc
       * -I/--ignore_warnings applies vtysh and integrated configurations
       */
      if (!dryrun)
        mode = vtysh_mode_boot ;

      if (options.echo_command)
        fprintf(stderr, "-E/--echo ignored in boot mode\n") ;

      if (options.lexical_level != 0)
        fprintf(stderr, "-1/--lexical-level-1 ignored in boot mode\n") ;
    }
  else
    {
      /* default is interactive mode, which defaults to lexical-level 1
       *
       * There are no: -c/--command (or -e/--eval) or -b/--boot
       *
       * Ignores: -E/--echo (don't report -n/--noerror)
       *
       * -q/--quiet           applies to reporting connections
       * -I/--ignore_warnings applies to vtysh config
       */
      options.lexical_level = 1 ;

      if (!dryrun)
        mode = vtysh_mode_interactive ;

      if (options.echo_command)
        fprintf(stderr, "-E/--echo ignored in interactive mode\n") ;
    } ;

   /* Read vtysh configuration file (if any) before connecting to daemons.
   * (This is the "vtysh.conf" file.)
   *
   * This will ignore all commands except those which apply to the vtysh
   * itself, since has not connected to any daemons yet.
   *
   * If -f is specified ("own_config"), then must have configuration file.
   *
   * If is "dryrun", then must have configuration file, otherwise is entirely
   * optional (so dryrun reports if there is a configuration file or not).
   */
  vtysh_read_config(vtysh_vty, host.config_file, own_config || dryrun,
                            options.ignore_warnings, options.quiet && !dryrun) ;

  /* Process the -d options
   */
  if (daemon_list != NULL)
    {
      options.daemons  = vtysh_daemons_list_ok(vtysh_vty, daemon_list) ;
      options.required = options.daemons ;
    } ;

  /* Make sure we pass authentication before proceeding.
   */
  vtysh_auth ();

  /* Now run one of: vtysh_cmd_mode()
   *                 vtysh_boot_mode()
   *                 vtysh_interactive_mode()
   *             or: do nothing for -C/--dryrun
   */
  switch (mode)
    {
      case vtysh_mode_dryrun:
        exit_code = 0 ;
        break ;

      case vtysh_mode_command:
        if (vtysh_daemons_connect(vtysh_vty, options.daemons, options.required))
          exit_code = vtysh_cmd_mode(vtysh_vty, commands, &options) ;
        else
          exit_code = 1 ;
        break ;

      case vtysh_mode_boot:
        if (vtysh_daemons_connect(vtysh_vty, options.daemons, options.required))
          exit_code = vtysh_boot_mode(vtysh_vty, &options) ;
        else
          exit_code = 1 ;
        break ;

      case vtysh_mode_interactive:
        exit_code = vtysh_interactive_mode(vtysh_vty, &options) ;
        break ;

      default:
        qassert(false) ;
    } ;

  /* Make sure all output is complete by pushing vtysh_vty
   */
  vty_cmd_out_push(vtysh_vty) ;

  /* Be tidy and close all the daemons -- and we're done.
   */
  vty_vtysh_close() ;

  return exit_code ;
} ;

/*------------------------------------------------------------------------------
 * Execute commands from -c arguments.
 *
 * Note that the vtysh_vty has been initialised in ENABLE_NODE.
 *
 * Sets the given options->lexical-level.  And:
 *
 *   options->echo_command    => turn on command line reflection (if not quiet)
 *   options->quiet           => turn off out_ordinary, out_warning and warn_stop
 *   options->ignore_warnings => turn off out_warning and warn_stop
 *
 *   options->no_error        => turn off all output and warn_stop
 *
 * Returns:  0 -- OK, reached end of -c arguments or executed an "exit".
 *           1 -- failed in some way
 */
static int
vtysh_cmd_mode(vty vtysh, qstring commands, vtysh_options_t* options)
{
  cmd_context context ;
  const char* cmd ;

  if (!vtysh_connected_ok(vtysh, options->quiet, true /* fail */))
    return 1 ;

  vty_vtysh_promote(vtysh) ;        /* -> VOUT_VTYSH        */

  context = vtysh->vio->vin->context ;

  /* Set required lexical-level
   */
  context->full_lex = (options->lexical_level == 1) ;

  /* Set initial state depending on no_error and quiet
   */
  if (options->no_error || options->quiet)
    {
      context->reflect      = false ;
      context->out_ordinary = false ;
      context->out_warning  = false ;
      context->warn_stop    = false ;
    }
  else
    {
      context->reflect      = false ;
      context->out_ordinary = true ;
      context->out_warning  = true ;
      context->warn_stop    = true ;
    } ;

  /* no_error takes precedence, but others take precedence over quiet or the
   * default.
   */
  if (!options->no_error)
    {
      if (options->echo_command)
        context->reflect    = true ;

      if (options->ignore_warnings)
        {
          context->out_warning  = false ;
          context->warn_stop    = false ;
        } ;
    } ;

  /* The pager is set off -- all output is to stdout, except for some errors.
   *
   * "no_error" is misnamed -- what it does is suppress all output to stdout,
   * as well as continuing even if command returns a CMD_WARNING.
   */
  vtysh_pager_set(0) ;          /* make sure pager off  */

  if (options->no_error)
    freopen("/dev/null", "w", stdout) ;

  /* Eat contents of the command qstring: commands terminated by '\n',
   */
  qs_reduce(commands, "\n", "\n") ;

  while ((cmd = qs_next_word(commands)) != NULL)
    {
      cmd_ret_t ret ;

      if (logfile)
        log_it(cmd) ;

      ret = vtysh_execute(vtysh, cmd, 0 /* no prompt (yet) */) ;

      if (ret == CMD_SUCCESS)
        continue ;

      if (ret == CMD_STOP)
        break ;

      return 1 ;
    } ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Read the integrated configuration file ("Quagga.conf")
 *
 * Note that when each daemon starts, if the integrated configuration file
 * exists the daemon does *not* read its own configuration file.
 *
 * Always starts lexical-level 0.  And:
 *
 *   options->quiet           => turn off progress etc. messages
 *   options->ignore_warnings => turn off out_warning and warn_stop
 *
 * Returns:  0 -- OK, reached end of configuration file or executed an "exit".
 *           1 -- failed in some way
 */
static int
vtysh_boot_mode(vty vtysh, vtysh_options_t* options)
{
  if (!vtysh_connected_ok(vtysh, options->quiet, true /* fail */))
    return 1 ;

  if (vtysh_read_config(vtysh, host.int_config_file, true /* required */,
                                      options->ignore_warnings, options->quiet))
    return 0 ;
  else
    return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Execute interactive commands -- using readline() library.
 *
 * Note that the vtysh_vty has been initialised in ENABLE_NODE.
 *
 * Sets the given options->lexical-level.  And:
 *
 *   options->quiet           => turn off hello, progress etc. messages
 *
 * Returns:  0 -- OK, reached end of -c arguments or executed an "exit".
 *           1 -- failed in some way
 */
static int
vtysh_interactive_mode(vty vtysh, vtysh_options_t* options)
{
  cmd_ret_t   ret ;
  cmd_context context ;

  /* Start as if executing phantom "start" command, whose results will be
   * the output generated below -- vst_cmd_running_executing.
   */
  vty_vtysh_promote(vtysh) ;            /* -> VOUT_VTYSH        */

  context = vtysh->vio->vin->context ;

  /* Set required lexical-level
   */
  context->full_lex = (options->lexical_level == 1) ;

  /* Set context for interactive mode
   */
  context->reflect      = false ;
  context->out_ordinary = true ;
  context->out_warning  = true ;
  context->warn_stop    = true ;

  /* Complete preparation
   *
   * Say hello, unless quiet.  Then connect to daemons as require, and show
   * what we are connected to (unless quiet).
   *
   * Then proceed as if a command action has just succeeded.
   */
  uty_vtysh_out_prep(vtysh->vio, NULL /* no pager */) ;
  if (!options->quiet)
    {
      vty_hello(vtysh) ;
      vty_cmd_out_push(vtysh) ;
    } ;

  if (!vtysh_daemons_connect(vtysh, options->daemons, options->required))
    return 1 ;

  vtysh_connected_ok(vtysh, options->quiet, false /* not fail */) ;

  ret = vty_cmd_complete(vtysh, CMD_SUCCESS) ;
  while ((ret != CMD_SUCCESS) && (ret != CMD_STOP))
    ret = vty_cmd_hiatus(vtysh, ret) ;

  if (ret == CMD_STOP)
    return 0 ;

  /* Prepare to read lines and page out
   */
  vtysh_pager_set(-1) ;         /* set pager to default         */
  vtysh_readline_init() ;

  /* Preparation for longjmp() and fielding same.
   */
  switch (sigsetjmp (jmpbuf, 1))
    {
      case vtysh_lj_init:
        using_history();
        snprintf(history_file, sizeof(history_file), "%s/.history_quagga",
                                                              getenv("HOME")) ;
        read_history(history_file);
        ret = CMD_SUCCESS ;
        break ;

      case vtysh_lj_ctrl_c:
        rl_free_line_state() ;
        rl_crlf() ;

        ret = CMD_SUCCESS ;
        break ;

      case vtysh_lj_ctrl_z:
        rl_free_line_state() ;
        rl_crlf() ;

        ret = vtysh_execute(vtysh, "end", 0) ;
        break ;

      default:
        zabort("impossible return from sigsetjmp") ;
    } ;

  /* Main command loop.
   */
  while (ret != CMD_STOP)
    {
      char*       line_read ;

      /* Get a line from the user.
       */
      in_readline = true ;
      line_read   = readline(uty_cmd_prompt(vtysh->vio,
                                            vtysh->vio->vin->context->node)) ;
      in_progress = true ;
      in_readline = false ;

      if (line_read == NULL)
        break ;

      /* If the line has any text in it, save it on the history. But only if
       * last command in history isn't the same one. */
      if (*line_read != '\0')
        {
          HIST_ENTRY* last;

          last = previous_history();
          if (!last || strcmp (last->line, line_read) != 0)
            {
              add_history (line_read);
              append_history(1, history_file);
            } ;
        } ;

      ret = vtysh_execute(vtysh, line_read, qs_len_nn(vtysh->vio->prompt)) ;

      free(line_read) ;

      in_progress = false ;
    } ;

  history_truncate_file(history_file, 1000) ;
  printf ("\n");

  return 0 ;
} ;


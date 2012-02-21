/*
   $Id$

   Command interpreter routine for virtual terminal [aka TeletYpe]
   Copyright (C) 1997, 98, 99 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your
option) any later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "misc.h"
#include "version.h"

#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include "memory.h"
#include "thread.h"
#include "vector.h"
#include "qstring.h"
#include "qtime.h"
#include "workqueue.h"
#include "command.h"
#include "command_local.h"
#include "command_parse.h"
#include "command_execute.h"
#include "command_queue.h"
#include "log_local.h"
#include "vty_local.h"
#include "vty_command.h"
#include "vty_io.h"
#include "vty_log.h"
#include "network.h"
#include "vty_vtysh.h"

/*==============================================================================
 * Default motd string and debug hello message.
 */
static const char* default_motd =
"\n"
"Hello, this is %P: %D (version %V%q)\n"
"%C\n"
"\n" ;

const char* debug_banner =
    QUAGGA_PROGNAME ": %s v"QUAGGA_VERSION " QDEBUG/" QDEBUG_NAME " "
    __DATE__ " " __TIME__ "%s" ;

/*==============================================================================
 * The daemon(s) for which the command handler has been configured.
 *
 * This is set by cmd_table_init()
 *
 * For all daemons except vtysh, this will be the daemon itself.
 *
 * For vtysh this will be all the daemons it supports, plus itself.
 *
 * extern in command_local.h
 */
daemon_set_t daemons_set  = 0 ;
const char*  daemon_name  = "" ;

/*==============================================================================
 * Host information structure -- shared across command/vty
 *
 * Must have VTY_LOCK() or not be running multiple pthreads to access this !
 */
struct host host =
{
    /* Host name of this router.                                */
    .name               = NULL,  /* set by host_init     */
    .name_set           = false, /* set by command       */
    .name_gen           = 0,     /* set by host_init     */

    /* Password for vty interface.                              */
    .password           = { .text = NULL, .encrypted = false },

    /* Enable password                                          */
    .enable             = { .text = NULL, .encrypted = false },

    /* System wide terminal lines.                              */
    .lines              = -1,   /* unset                */

    /* Log filename.                                            */
    .logfile            = NULL,

    /* config file files                                        */
    .own_config_file    = NULL,
    .int_config_file    = NULL,
    .config_file        = NULL,
    .config_dir         = NULL,

    /* Initialisation and configuration                         */
    .pthreads_allowed   = false,
    .pthreaded_option   = false,
    .pthreaded_config   = false,
    .first_config_cmd   = false,
    .newborn            = true,
    .init_second_stage  = NULL,

    /* Flags for services                                       */
    .advanced           = false,
    .encrypt            = false,

    /* Banner configuration.                                    */
    .motd               = NULL,
    .motdfile           = NULL,

    /* Nobody has the config symbol of power                    */
    .config             = 0,
    .config_brand       = 0,

    /* allow VTY to start without password                      */
    .no_password_check  = false,

    /* if VTY starts without password, use RESTRICTED_NODE      */
    .restricted_mode    = false,

    /* Vty timeout value -- see "exec-timeout" command          */
    .vty_timeout_val    = VTY_TIMEOUT_DEFAULT,

    /* vty access-class for IPv4 and IPv6                       */
    .vty_accesslist_name      = NULL,
    .vty_ipv6_accesslist_name = NULL,

    /* How to configure listeners -- set by vty_start()         */
    .vty_listen_addr    = NULL,
    .vty_listen_port    = 0,
    .vtysh_listen_path  = NULL,

    /* Current directory                                        */
    .cwd                = NULL,

    /* Program name(s)                                          */
    .full_progname      = "*progname*",
    .progname           = "*progname*",

    /* __DATE__ & __TIME__ from last compilation                */
    .date               = __DATE__,
    .time               = __TIME__,
} ;

/*==============================================================================
 * host.name handling and other host structure access.
 *
 * If the host.name is set explicitly by command then host.name_set is true,
 * and things are simple.
 *
 * Otherwise, need to ask the system.  Does that once at start up, and if the
 * host.name is unset by command -- so there should always be a valid host.name.
 *
 * However, it is conceivable that the host name changes (!).  So, when asking
 * for cmd_host_name(), can ask for the system to be asked afresh (if the name
 * is not explicitly set).
 *
 * The VTY watch-dog timer refreshes the host.name on a regular basis,
 * cmd_check_host_name(), so need not ask for a fresh host.name, unless wish
 * to guarantee to be absolutely up to date.
 *
 * The VTY prompts need the current host name, but that is debounced using the
 * host.name_gen value.  host.name_gen is incremented each time the host.name
 * actually changes.  It is thought unlikely that this will ever wrap round,
 * but it is guaranteed not to be zero.
 *
 * The fact that the host.name can change is reflected in the need to hold
 * the VTY_LOCK while have the host.name in hand.
 */

static void cmd_get_sys_host_name(void) ;
static void cmd_new_host_name(const char* name) ;
static void cmd_config_dir_set (qpath config) ;

/*------------------------------------------------------------------------------
 * Get the host name: (a) from an explicit host name command
 *                or: (b) from the last time the system was asked.
 *
 * Note that the system is asked regularly by the watch dog.
 */
extern const char*
cmd_host_name(bool fresh)
{
  VTY_ASSERT_LOCKED() ;

  if (!host.name_set && fresh)
    cmd_get_sys_host_name() ;

  return host.name ;
} ;

/*------------------------------------------------------------------------------
 * Set (or unset) the host name from an explicit host name command.
 *
 * If unsets, immediately asks the system for the system host name (which may
 * be the same !).
 */
static cmd_ret_t
cmd_set_host_name(const char* name)
{
  VTY_LOCK() ;

  host.name_set = (name != NULL) ;
  if (host.name_set)
    cmd_new_host_name(name) ;
  else
    cmd_get_sys_host_name() ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Get the host name from the system and set host.name to that.
 *
 * NB: result will not be NULL, whatever happens will get at least an empty
 *     '\0' terminated string.
 *
 * NB: called early in the morning to initialise host.name and to set
 *     host.name_gen != 0.
 */
static void
cmd_get_sys_host_name(void)
{
  struct utsname info ;

  VTY_ASSERT_LOCKED() ;

  uname (&info) ;
  cmd_new_host_name(info.nodename) ;
} ;

/*------------------------------------------------------------------------------
 * Set host.name to (possibly) new value.
 *
 * Does nothing if new name == old name, otherwise increments name_gen.
 */
static void
cmd_new_host_name(const char* name)
{
  VTY_ASSERT_LOCKED() ;

  if ((host.name == NULL) || (strcmp(host.name, name) != 0))
    {
      XFREE(MTYPE_HOST, host.name) ;
      host.name = XSTRDUP(MTYPE_HOST, name) ;
      do ++host.name_gen ; while (host.name_gen == 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set configuration file values in host:
 *
 *   host.own_config_file -- full path of daemon's own configuration file
 *   host.int_config_file -- full path of integrated configuration file, if any
 *
 * And these, according to which configuration file we should read.
 *
 *   host.config_file     -- pointer to own or integrated, as required
 *   host.config_dir      -- full path of directory part of host.config_file
 *
 * We have choice when reading configuration:
 *
 *   1. read none at all -- wait for vtysh to provide -- applies if:
 *
 *        -b is specified and is VTYSH_ENABLED
 *
 *      host.config_file ) set as (4), below
 *      host.config_dir  )
 *
 *   2. read the integrated configuration -- applies if:
 *
 *        -F xxxx and/or -Q is specified
 *
 *      host.config_file is set to integrated configuration
 *      host.config_dir  is set to directory of same.
 *
 *   3. read the daemon's own configuration -- applies if:
 *
 *        neither -F xxxx and/or -Q is specified
 *
 *      AND:
 *
 *        -f xxxx is specified or is !VTYSH_ENABLED
 *        or is VTYSH_ENABLED but the default own_config_file contains "vtysh"
 *        or is VTYSH_ENABLED but the host.int_config_file does not exist
 *
 *      host.config_file is set to daemon's own configuration
 *      host.config_dir  is set to directory of same.
 *
 *   4. read none at all -- wait for vtysh to provide -- applies if:
 *
 *        is VTYSH_ENABLED and none of -F, -Q or -f is specified
 *
 *      AND:
 *
 *        the integrated configuration file exists.
 *
 *      host.config_file is set to NULL -- no config to be read.
 *      host.config_dir  is set to the *integrated* configuration directory, so
 *                                 that there is, still, a ~~/ directory.
 *
 * Always writes the daemon's own configuration.
 *
 * Note that -F, -Q and -b are "new".  If those are ignored, the above rules
 * correspond to the "old" behaviour:
 */
#if 0
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
            return;             /* NB: leaves host.config_file NULL     */
        }
    } ;
#endif
/*
 * Arguments are:
 *
 *   own_config_file   -- as set by -f xxxx or by default
 *   own_flag          -- true <=>  -f xxxx
 *   int_config_file   -- as set by -F xxxx or NULL
 *   int_flag          -- true <=>  -F xxxx and/or -Q
 *   int_boot          -- true <=>  -b
 */
extern void
cmd_host_config_set (const char* own_config_file, bool own_flag,
                     const char* int_config_file, bool int_flag, bool int_boot)
{
  qpath config ;

  VTY_LOCK() ;

  if (int_config_file == NULL)
    int_config_file = integrate_default ;

  /* Set the configuration files
   */
  host.own_config_file = qpath_make_path(host.own_config_file,
                                              own_config_file, host.cwd) ;
  host.int_config_file = qpath_make_path(host.int_config_file,
                                              int_config_file, host.cwd) ;

  /* Choose which configuration file to read and which config directory to
   * use for ~~/.
   *
   * Arrange for:
   *
   *   int_boot == true <=> let vtysh provide configuration
   *
   *   int_flag == true <=> use integrated configuration
   *
   *          otherwise <=> use ordinary configuration
   */
  if (VTYSH_ENABLED)
    {
      if (!int_boot && !own_flag && !int_flag)
        {
          if (strstr(own_config_file, "vtysh") == NULL)
            int_boot = (qpath_stat_is_file(host.int_config_file) == 0) ;
        } ;
    }
  else
    int_boot = false ;

  if       (int_boot)
    {
      /* We depend on vtysh: setting host.config_file to NULL disables reading
       * of configuration.  Sets integrated config file directory for ~~/.
       */
      host.config_file = NULL ;
      config           = host.int_config_file ;
    }
  else if (int_flag)
    {
      /* -F and/or -Q => read the integrated configuration directly
       */
      host.config_file = host.int_config_file ;
      config           = host.int_config_file ;
    }
  else
    {
      /* Otherwise, read own configuration file.
       */
      host.config_file = host.own_config_file ;
      config           = host.own_config_file ;
    } ;

  cmd_config_dir_set (config) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Set host.config_dir according to the given config file path.
 */
static void
cmd_config_dir_set (qpath config_file)
{
  VTY_ASSERT_LOCKED() ;

  host.config_dir = qpath_copy(host.config_dir, config_file) ;
  qpath_shave(host.config_dir) ;
} ;

/*------------------------------------------------------------------------------
 * Set second stage initialisation and initial pthreaded state (-t option).
 *
 * MUST precede first configuration file reading !
 */
extern void
cmd_host_pthreaded(bool pthreaded_option, init_second_stage init_second_stage)
{
  qassert(host.newborn && !qpthreads_decided()) ;

  host.pthreads_allowed  = (init_second_stage != NULL) ;
  host.pthreaded_option  = pthreaded_option ;
  host.pthreaded_config  = false ;              /* default */
  host.init_second_stage = init_second_stage ;
} ;

/*------------------------------------------------------------------------------
 * Other host.xxxx access functions
 */
extern const char*
cmd_host_full_program_name(void)
{
  return host.full_progname ;
} ;

extern const char*
cmd_host_program_name(void)
{
  return host.progname ;
} ;

/*==============================================================================
 * Daemon bit/ord/name handling
 */

/* Names for daemons may have commands for
 */
static const char* daemon_name_table[] =
{
    [BGPD_ORD]      = "bgpd",
    [ISISD_ORD]     = "isisd",
    [OSPFD_ORD]     = "ospfd",
    [OSPF6D_ORD]    = "ospf6d",
    [RIPD_ORD]      = "ripd",
    [RIPNGD_ORD]    = "ripngd",
    [ZEBRA_ORD]     = "zebra",

    [BASIC_ORD]     = "basic",
    [VTYSH_ORD]     = "vtysh",

    [TERM_ORD]      = NULL,

    [DAEMON_INVALID_ORD] = NULL
} ;

#define DAEMON_LIST "bgpd isisd ospfd ospf6d ripd ripngd zebra vtysh"

/* Names for the daemons in a set of same
 */
static const char* daemon_bit_name_table[] =
{
    [BGPD_ORD]      = "BGPD",
    [OSPFD_ORD]     = "OSPFD",
    [OSPF6D_ORD]    = "OSPF6D",
    [ISISD_ORD]     = "ISISD",
    [RIPD_ORD]      = "RIPD",
    [RIPNGD_ORD]    = "RIPNGD",
    [ZEBRA_ORD]     = "ZEBRA",

    [BASIC_ORD]     = "BASIC_VD",
    [VTYSH_ORD]     = "VTYSH_VD",

    [TERM_ORD]      = "TERM",

    [DAEMON_INVALID_ORD] = NULL
} ;

/*------------------------------------------------------------------------------
 * Map ls bit of given set of daemons to ordinal for daemon
 *
 * Maps zero or out of range value to DAEMON_INVALID_ORD
 */
static daemon_ord_t
cmd_daemon_ord(daemon_set_t daemons)
{
  daemon_ord_t ord ;

  ord = 0 ;
  while (((daemons & 1) == 0) && (ord <= DAEMON_MAX_ORD))
    {
      confirm(DAEMON_INVALID_ORD == (DAEMON_MAX_ORD + 1)) ;
      ++ord ;
      daemons >>= 1 ;
    } ;

  qassert(ord != DAEMON_INVALID_ORD) ;

  return ord ;
} ;

/*------------------------------------------------------------------------------
 * Map daemon ordinal to daemon set member
 *
 * Maps out of range value to DAEMON_NONE (0)
 */
static daemon_set_t
cmd_daemon_bit(daemon_ord_t ord)
{
  qassert(ord <= DAEMON_MAX_ORD) ;

  if (ord <= DAEMON_MAX_ORD)
    return 1 << ord ;
  else
    return DAEMON_NONE ;
} ;

/*------------------------------------------------------------------------------
 * Map ls bit of given set of daemons to name of daemon
 *
 * Names include "basic" (for BASIC_VD) but nothing for "TERM".
 */
static const char*
cmd_daemon_name(daemon_set_t daemons, bool forge)
{
  const char* name ;

  name = daemon_name_table[cmd_daemon_ord(daemons)] ;

  if ((name != NULL) || !forge)
    return name ;

  qassert(false) ;

  if (daemons == 0)
    return "*none*" ;

  if (cmd_daemon_ord(daemons) == DAEMON_INVALID_ORD)
    return "*invalid*" ;
  else
    return "*unknown*" ;
} ;

/*------------------------------------------------------------------------------
 * Map ls bit of given set of daemons to name of bit
 *
 * Names include "TERM".
 */
static const char*
cmd_daemon_bit_name(daemon_set_t daemons, bool forge) __attribute__((unused)) ;
static const char*
cmd_daemon_bit_name(daemon_set_t daemons, bool forge)
{
  const char* name ;

  name = daemon_bit_name_table[cmd_daemon_ord(daemons)] ;

  if ((name != NULL) || !forge)
    return name ;

  qassert(false) ;

  if (daemons == 0)
    return "*NONE*" ;

  if (cmd_daemon_ord(daemons) == DAEMON_INVALID_ORD)
    return "*INVALID*" ;
  else
    return "*UNKNOWN*" ;
} ;

/*------------------------------------------------------------------------------
 * Map set of daemons to a list of daemon names, separated by space.
 *
 * Returns:  qstring -- created if required.
 */
extern qstring
cmd_daemons_make_list(qstring qs, daemon_set_t daemons)
{
  if (qs == NULL)
    qs = qs_new(40) ;
  else
    qs_clear(qs) ;

  if (daemons == 0)
    qs_append_str(qs, "none") ;
  else
    {
      while (daemons != 0)
        {
          if (qs_len_nn(qs) != 0)
            qs_append_str(qs, " ") ;

          qs_append_str(qs, cmd_daemon_name(daemons, true /* forge */)) ;

          daemons &= (daemons - 1) ;
        } ;
    } ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Map a list of daemon names, separated by space(s) and/or "," to set of
 * daemons.
 *
 * Allows any number of repeats, and for an empty list.
 *
 * NB: does not allow any short forms -- must get exact match for each.
 *
 * Reduces given qstring to empty if all OK, otherwise, to a list of
 * unrecognised names.
 *
 * Returns:  set of daemons.
 */
extern daemon_set_t
cmd_daemons_from_list(qstring list)
{
  daemon_set_t daemons ;
  qstring work ;

  const char* w ;

  work = qs_set(NULL, list) ;
  qs_clear(list) ;

  qs_reduce(work, " ,", NULL) ;

  daemons = 0 ;
  while ((w = qs_next_word(work)) != NULL)
    {
      daemon_ord_t ord ;

      ord = 0 ;
      while (1)
        {
          const char* name ;

          name = daemon_name_table[ord] ;

          if ((name != NULL) && (strcmp(name, w) == 0))
            {
              daemons |= cmd_daemon_bit(ord) ;
              break ;
            } ;

          if (ord == DAEMON_MAX_ORD)
            {
              if (qs_len_nn(list) == 0)
                qs_append_str(list, " ") ;

              qs_append_str(list, "'") ;
              qs_append_str(list, w) ;
              qs_append_str(list, "'") ;

              break ;
            } ;

          ++ord ;
        } ;
    } ;

  qs_free(work) ;

  return daemons ;
} ;

/*------------------------------------------------------------------------------
 * "#daemon" and "#daemons" command.
 *
 * Sets the set of daemons that any further commands are intended for.
 *
 * NB: this is really only intended for use in configuration files, so there
 *     is no support for short form daemon names.
 *
 * For most types of vty, this sets the daemons to parse for and the daemons
 * to execute for.  For all daemons other than the vtysh, this will,
 * effectively be either turning on or off parsing and execution.
 *
 * For VTY_VTYSH -- which means, for vtysh itself -- sets only the daemons
 * to *execute* for.  So all commands are always parsed, but the set of daemons
 * to whom the commands are dispatched will vary.
 */
DEFUN_ATTR (set_daemons,
            set_daemons_cmd,
            "daemons .DAEMONS",
            "Set daemon(s) for commands which follow\n"
            "Daemons, any or all of: "DAEMON_LIST"\n",
            CMD_ATTR_DIRECT | CMD_ATTR_FIRST)
{
  daemon_set_t daemons ;

  daemons = cmd_deamon_list_arg(vty, argc, argv) ;

  if (daemons == 0)
    return CMD_ERROR ;

  vty->exec->context->daemons = daemons & daemons_set ;

  return CMD_SUCCESS ;
} ;

ALIAS_ATTR (set_daemons,
            set_daemon_cmd,
            "daemon DAEMON",
            "Set daemon(s) for commands which follow\n"
            "Daemon, any one of: "DAEMON_LIST"\n",
            CMD_ATTR_DIRECT | CMD_ATTR_FIRST)

/*------------------------------------------------------------------------------
 * Process command arguments as list of daemon names
 *
 * Returns:  set of daemons -- empty <=> no good, error message output
 */
extern daemon_set_t
cmd_deamon_list_arg(vty vty, int argc, argv_t argv)
{
  daemon_set_t daemons ;
  qstring      list ;
  char*   args ;

  args = argv_concat(argv, argc, 0) ;
  list = qs_set_str(NULL, args) ;

  daemons = cmd_daemons_from_list(list) ;

  if (qs_len(list) == 0)
    {
      if (daemons == 0)
        vty_out(vty, "%% empty list of daemons given\n") ;
    }
  else
    {
      vty_out(vty, "%% invalid daemon(s): %s\n", qs_string(list)) ;
      daemons = 0 ;
    } ;

  qs_free(list) ;
  XFREE(MTYPE_TMP, args) ;

  return daemons ;
} ;

/*------------------------------------------------------------------------------
 * Other meta commands for vtysh config
 */
DEFUN_ATTR (set_vtysh_config_node,
            set_vtysh_config_node_cmd,
            "vtysh-config-node NODE-NAME",
            "Set vtysh configuration node for configuration which follows\n"
            "The configuration node name\n",
            CMD_ATTR_DIRECT | CMD_ATTR_FIRST)
{
  vty_out(vty, "%% #vtysh-config-node %s only valid in vtysh-config-write\n",
                                                                      argv[0]) ;
  return CMD_WARNING ;
} ;

DEFUN_ATTR (set_vtysh_config_section,
            set_vtysh_config_section_cmd,
            "vtysh-config-section <0-999999>",
            "Set vtysh configuration section for configuration which follows\n"
            "The configuration section number\n",
            CMD_ATTR_DIRECT | CMD_ATTR_FIRST)
{
  vty_out(vty, "%% #vtysh-config-section %s only valid in vtysh-config-write\n",
                                                                      argv[0]) ;
  return CMD_WARNING ;
} ;

/*==============================================================================
 * Construction of the nodes and commands for command processing, and access to
 * all that data.
 *
 * Each node has a cmd_node_t entry in the statically allocated and initialised
 * node_table.  The static initialisation sets the basic properties of each
 * node.
 *
 * To use a node it must be installed, which completes the initialisation of
 * the node and installs the basic commands common to every active node.
 *
 * Note that each node has a configuration writer property, and configuration
 * files are written by visiting the installed nodes, in node number order.
 * Some nodes have no commands (signalled by not having a prompt string), and
 * exist only to group together some configuration.
 *
 * Commands for each node are then installed in the node's cmd_vector.  The
 * installation of commands "compiles" the cmd_command objects, and adds a
 * pointer to each one to the node's cmd_vector.
 *
 * Once all commands have been installed, the commands in each node's cmd_vector
 * are sorted into alphabetical order, for help purposes.  Once that is done,
 * the node and command data is complete, and commands may be executed.
 *
 * Now, commands may be parsed in more than one pthread:
 *
 *   1. configuration reading is done at start up in the (main and only -- CLI)
 *      pthread, but on re-read is done in the command processing pthread.
 *
 *   2. when reading a pipe, commands are parsed in the command processing
 *      pthread.
 *
 * At start up there is only one pthread, so there is no real problem there.
 * But at other times, if all commands were passed, one by one from the CLI
 * pthread to the command processing pthread, they would not necessarily be
 * executed together (unless a new mechanism were invented), and there would
 * be some extra cost for large configuration files or piping in large
 * amounts of configuration.
 *
 * The main consequence of this is that the node/command data is treated as
 * static, once it is complete, and must not be used until it is complete.
 * Further, the installing of nodes/command data must be done only in the
 * main (CLI) pthread.  Both these conditions are achieved by installing all
 * nodes and commands and then calling cmd_table_complete() *before* any
 * pthreads are started and *before* any configuration file is read.
 *
 * The other consequences are:
 *
 *   1. all possible nodes must be present in the node_table[] below.
 *
 *      It is not possible to create new nodes dynamically.
 *
 *      The vtysh needs a complete map... which would otherwise need a
 *      mechanism for daemons to transmit the dynamic nodes, and any commands
 *      to the vtysh !
 *
 *   2. all possible commands must be installed fairly early in the morning...
 *      before any configuration file is read.
 *
 *      So, while there is a "compile" step for each command, the command set
 *      is essentially static.
 *
 *      The vtysh needs a complete map of commands... which would otherwise
 *      need a mechanism for daemons to transmit dynamic commands to the vtysh !
 *
 * So... while this may appear dynamic, it is really essentially static, and
 * the result is essentially read-only.
 */

/* The basic commands in each active node
 */
static cmd_command_t config_exit_cmd ;
static cmd_command_t config_quit_cmd ;
static cmd_command_t config_end_cmd ;
static cmd_command_t config_help_cmd ;
static cmd_command_t config_list_cmd ;

/* May not install nodes/commands once the table is complete.
 *
 * May not parse commands until the table is complete.
 */
static bool node_table_complete = false ;

/* Prototypes
 */
static cmd_node cmd_install_node(node_type_t node) ;
static void qassert_usable_node(node_type_t node, node_type_t and_not) ;

/*------------------------------------------------------------------------------
 * Node structures for all nodes.
 *
 * The values:
 *
 *   node   -- self reference
 *   name   -- string form of node
 *
 * Are constants, and may be accessed even if the node has not been installed.
 *
 * Note that where nothing is specified, then in general:
 *
 *   .parent  => CONFIG_NODE
 *   .exit    => .parent
 *   .end     => ENABLE_NODE
 *
 * but see cmd_install_node(), below.
 *
 * Where a node has a NULL prompt, it is really a place-holder for various
 * amounts of configuration -- which is output in node number order of
 * installed nodes.
 */
static cmd_node_t node_table[] =
{
  /* The NULL_NODE and EXIT_NODE are never installed, and have no commands
   */
  [NULL_NODE] =
    {
      .node     = NULL_NODE,
      .name     = "NULL",
      .prompt   = NULL,

      .daemons  = 0,                    /* will not install     */
    },

  [EXIT_NODE] =
    {
      .node     = EXIT_NODE,
      .name     = "EXIT",
      .prompt   = NULL,

      .daemons  = 0,                    /* will not install     */
    },

  /* The META_NODE is always installed, and contains the meta commands
   *
   * The prompt should never be seen, but is present just in case, and also
   * so that the node is marked executable !
   */
  [META_NODE] =
    {
      .node     = META_NODE,
      .name     = "META",
      .prompt   = "#",                  /* should never use !           */

      .parse_strict  = true,            /* special for meta commands    */

      .daemons  = ALL_RDS | ALL_VDS,

      .parent   = META_NODE,            /* self => no parent            */
    },

  /*----------------------------------------------------------------------------
   * Your basic nodes.
   */
  [VIEW_NODE] =
    {
      .node     = VIEW_NODE,
      .name     = "VIEW",
      .prompt   = "%s> ",

      .daemons  = ALL_RDS | VTYSH_VD,

      .parent   = VIEW_NODE,            /* self => no parent            */
    },

  [RESTRICTED_NODE] =
    {
      .node     = RESTRICTED_NODE,
      .name     = "RESTRICTED",
      .prompt   = "%s$ ",

      .daemons  = ALL_RDS,

      .parent   = RESTRICTED_NODE,      /* self => no parent            */
    },

  [AUTH_NODE] =
    {
      .node     = AUTH_NODE,
      .name     = "AUTH",
      .prompt   = "Password: ",

      .daemons  = ALL_RDS,

      .parent   = AUTH_NODE,            /* self => no parent            */
    },

  [AUTH_ENABLE_NODE] =
    {
      .node     = AUTH_ENABLE_NODE,
      .name     = "AUTH_ENABLE",
      .prompt   = "Enable Password: ",

      .daemons  = ALL_RDS,

      .parent   = AUTH_ENABLE_NODE,     /* self => no parent            */
    },

  [ENABLE_NODE] =
    {
      .node     = ENABLE_NODE,
      .name     = "ENABLE",
      .prompt   = "%s# ",

      .daemons  = ALL_RDS | ALL_VDS,    /* Every daemon                 */

      .parent   = ENABLE_NODE,          /* self => no parent            */
    },

  [CONFIG_NODE] =
    {
      .node     = CONFIG_NODE,
      .name     = "CONFIG",
      .prompt   = "%s(config)# ",

      .daemons  = ALL_RDS | ALL_VDS,    /* Every daemon                 */

      .parent   = ENABLE_NODE,

      .config_to_vtysh = true
    },

  /*----------------------------------------------------------------------------
   * Various common nodes
   */
  [DEBUG_NODE] =
    {
      .node     = DEBUG_NODE,
      .name     = "DEBUG",
      .prompt   = NULL,

      .daemons  = ALL_RDS,

      .config_to_vtysh = true,
    },

  [INTERFACE_NODE] =
    {
      .node     = INTERFACE_NODE,
      .name     = "INTERFACE",
      .prompt   = "%s(config-if)# ",

      .daemons  = INTERFACE_DS,

      .config_to_vtysh = true,
    },

  [ACCESS_NODE] =
    {
      .node     = ACCESS_NODE,
      .name     = "ACCESS",
      .prompt   = NULL,

      .daemons  = ALL_RDS,

      .config_to_vtysh = true,
    },

  [ACCESS_IPV6_NODE] =
    {
      .node     = ACCESS_IPV6_NODE,
      .name     = "ACCESS-IPv6",
      .prompt   = NULL,

      .daemons  = ALL_RDS,

      .config_to_vtysh = true,
    },

  [KEYCHAIN_NODE] =
    {
      .node     = KEYCHAIN_NODE,
      .name     = "KEYCHAIN",
      .prompt   ="%s(config-keychain)# ",

      .daemons  = RIPD,

      .config_to_vtysh = true
    },

  [KEYCHAIN_KEY_NODE] =
    {
      .node     = KEYCHAIN_KEY_NODE,
      .name     = "KEYCHAIN-KEY",
      .prompt   = "%s(config-keychain-key)# ",

      .daemons  = RIPD,

      .parent   = KEYCHAIN_NODE,

      .config_to_vtysh = true
    },

  [PREFIX_NODE] =
    {
      .node     = PREFIX_NODE,
      .name     = "PREFIX",
      .prompt   = NULL,

      .daemons  = ALL_RDS,

      .config_to_vtysh = true
    },

  [PREFIX_IPV6_NODE] =
    {
      .node     = PREFIX_IPV6_NODE,
      .name     = "PREFIX-IPv6",
      .prompt   = NULL,

      .daemons  = ALL_RDS,

      .config_to_vtysh = true
    },

  [RMAP_NODE] =
    {
      .node     = RMAP_NODE,
      .name     = "ROUTE-MAP",
      .prompt   = "%s(config-route-map)# ",

      .daemons  = RMAP_DS,

      .config_to_vtysh = true
    },

  [SMUX_NODE] =
    {
      .node     = SMUX_NODE,
      .name     = "SMUX",
      .prompt   = NULL,

      .daemons  = RIPD | OSPFD | OSPF6D | BGPD | ZEBRA,
    },

  [VTY_NODE] =
    {
      .node     = VTY_NODE,
      .name     = "VTY",
      .prompt   = "%s(config-line)# ",

      .daemons  = ALL_RDS,

      .config_to_vtysh = true
    },

  /*----------------------------------------------------------------------------
   * Your bgpd nodes
   */
  [BGP_NODE] =
    {
      .node     = BGP_NODE,
      .name     = "BGP",
      .prompt   = "%s(config-router)# ",

      .daemons  = BGPD,

      .config_to_vtysh = true,
    },

  [BGP_IPV4_NODE] =
    {
      .node     = BGP_IPV4_NODE,
      .name     = "BGP-IPv4",
      .prompt   = "%s(config-router-af)# ",

      .daemons  = BGPD,

      .parent   = BGP_NODE,

      .config_to_vtysh = true,
    },

  [BGP_IPV4M_NODE] =
    {
      .node     = BGP_IPV4M_NODE,
      .name     = "BGP-IPv4-M",
      .prompt   = "%s(config-router-af)# ",

      .daemons  = BGPD,

      .parent   = BGP_NODE,

      .config_to_vtysh = true,
    },

  [BGP_IPV6_NODE] =
    {
      .node     = BGP_IPV6_NODE,
      .name     = "BGP-IPv6",
      .prompt   = "%s(config-router-af)# ",

      .daemons  = BGPD,

      .parent   = BGP_NODE,

      .config_to_vtysh  = true,
    },

  [BGP_IPV6M_NODE] =
    {
      .node     = BGP_IPV6M_NODE,
      .name     = "BGP-IPv6-M",
      .prompt   = "%s(config-router-af)# ",

      .daemons  = BGPD,

      .parent   = BGP_NODE,

      .config_to_vtysh  = true,
    },

  [BGP_VPNV4_NODE] =
    {
      .node     = BGP_VPNV4_NODE,
      .name     = "BGP-VPNv4",
      .prompt   = "%s(config-router-af)# ",

      .daemons  = BGPD,

      .parent   = BGP_NODE,

      .config_to_vtysh = true,
    },

  [COMMUNITY_LIST_NODE]=
    {
      .node     = COMMUNITY_LIST_NODE,
      .name     = "COMMUNITY-LIST",
      .prompt   = NULL,

      .daemons  = BGPD,

      .config_to_vtysh = true,
    },

  [AS_LIST_NODE] =
    {
      .node     = AS_LIST_NODE,
      .name     = "AS-LIST",
      .prompt   = NULL,

      .daemons  = BGPD,

      .config_to_vtysh = true,
    },

  [DUMP_NODE] =
    {
      .node     = DUMP_NODE,
      .name     = "BGP-DUMP",
      .prompt   = NULL,

      .daemons  = BGPD,

      .config_to_vtysh = true,
    },

  /*----------------------------------------------------------------------------
   * Your isisd nodes
   */
  [ISIS_NODE] =
    {
      .node     = ISIS_NODE,
      .name     = "ISIS",
      .prompt   = "%s(config-router)# ",

      .daemons  = ISISD,

      .config_to_vtysh = true,
    },

  /*----------------------------------------------------------------------------
   * Your ospf nodes
   */
  [OSPF_NODE] =
    {
      .node     = OSPF_NODE,
      .name     = "OSPF",
      .prompt   = "%s(config-router)# ",

      .daemons  = OSPFD,

      .config_to_vtysh = true,
    },

  [OSPF6_NODE] =
    {
      .node     = OSPF6_NODE,
      .name     = "OSPFv6",
      .prompt   = "%s(config-ospf6)# ",

      .daemons  = OSPF6D,

      .config_to_vtysh = true,
     },

  /*----------------------------------------------------------------------------
   * Your RIP nodes
   */
  [RIP_NODE] =
    {
      .node     = RIP_NODE,
      .name     = "RIP",
      .prompt   = "%s(config-router)# ",

      .daemons  = RIPD,

      .config_to_vtysh = true,
    },

  [RIPNG_NODE] =
    {
      .node     = RIPNG_NODE,
      .name     = "RIPNG",
      .prompt   = "%s(config-router)# ",

      .daemons  = RIPNGD,

      .config_to_vtysh = true,
    },

  /*----------------------------------------------------------------------------
   * Your zebra nodes
   */
  [ZEBRA_NODE] =
    {
      .node     = ZEBRA_NODE,
      .name     = "ZEBRA",
      .prompt   = "%s(config-zebra)# ", /* OR config-router !   */

      .daemons  = OSPF6D | RIPD | RIPNGD,
    },

  [PROTOCOL_NODE] =
    {
      .node     = PROTOCOL_NODE,
      .name     = "PROTOCOL",
      .prompt   = NULL,

      .daemons  = ZEBRA,

      .config_to_vtysh = true,
    },

  [IP_NODE] =
    {
      .node     = IP_NODE,
      .name     = "IP",
      .prompt   = NULL,

      .daemons  = ZEBRA,

      .config_to_vtysh = true,
    },

  [TABLE_NODE] =
    {
      .node     = TABLE_NODE,
      .name     = "TABLE",
      .prompt   = NULL,

      .daemons  = ZEBRA,

      .config_to_vtysh = true,
    },

  [FORWARDING_NODE] =
    {
      .node     = FORWARDING_NODE,
      .name     = "FORWARDING",
      .prompt   = NULL,

      .daemons  = ZEBRA,

      .config_to_vtysh = true,
    },
#if 0
  /*----------------------------------------------------------------------------
   * The INVALID_NODE -- placeholder for looking up an invalid node
   *
   * Never installed.
   */
  [INVALID_NODE] =
    {
      .node     = INVALID_NODE,
      .name     = "*INVALID*",
      .prompt   = NULL,

      .daemons  = 0,            /* will not install     */
    }
#endif
} ;

/* Check the size of the table, and note that any missing nodes will be
 * configured as NULL_NODE -- but with a NULL name.
 */
CONFIRM((sizeof(node_table) / sizeof(cmd_node_t)) == NUMBER_OF_NODES) ;
CONFIRM(NULL_NODE == 0) ;

static void cmd_node_parent_check(node_type_t node) ;

/*------------------------------------------------------------------------------
 * Initialise the node_table.
 *
 * To be done early in the morning, once, before any nodes or commands are
 * installed.  Each node has a number of preset fields (see above) and is then
 * further initialised as follows:
 *
 *   .node            -- preset
 *   .name            -- preset
 *
 *   .daemons         -- preset, adjusted by cmd_install_node()
 *
 *                       For installed nodes the preset value is masked
 *                       down to the active daemons -- per daemon_set.
 *
 *   .prompt          -- preset
 *
 *   .config_to_vtysh -- preset
 *
 *   .installed       -- initially false, set by cmd_install_node()
 *
 *   .config_write    -- initially NULL, set by cmd_install_node_config_write()
 *
 *   .cmd_vector      -- initially NULL, set by cmd_install_node()
 *
 *   .parse_strict    -- preset
 *
 *   .executable      -- set here
 *
 *                       The node may contain executable commands, if or when
 *                       it is installed.
 *
 *   .parent        )
 *   .exit_to       ) -- may be preset, or will be set to default, see below
 *   .end_to        )
 *
 * The .parent is set for all nodes -- all nodes, except NULL_NODE, have a
 * parent, even if it is themselves.
 *
 * A node is .executable if it has a non-NULL .prompt, except for AUTH_NODE
 * and AUTH_ENABLE_NODE.
 *
 * Default .parent node:
 *
 *   * all nodes <  MIN_CONFIG_NODE are their own parents (except NULL_NODE)
 *   *     node  <= MAX_CONFIG_NODE has  ENABLE_NODE as parent
 *   * all nodes >  MAX_CONFIG_NODE have NODE_CONFIG as parent
 *
 * The .exit_to and .end_to are set only if node is .executable, and are set
 * NULL_NODE (here) otherwise.  Note: META_NODE is .executable, but .exit_to &
 * .end_to are not relevant, so are set NULL_NODE (here).
 *
 * Default .exit_to, if .executable, is goto parent, except that if node is
 * its own parent, goto EXIT_NODE.  Note that:
 *
 *   * all nodes <  MIN_ENABLE_NODE have parent == self.
 *   * all nodes >  MAX_ENABLE_NODE have parent != self
 *
 * Default .end_to, if .executable:
 *
 *   * all nodes <  MIN_CONFIG_NODE end_to themselves
 *   * all nodes >= MIN_CONFIG_NODE end to parent ENABLE node
 */
static void
cmd_node_table_init(void)
{
  node_type_t node ;

  /* Fix up the .parent and set other initial values.
   */
  for (node = MIN_NODE ; node <= MAX_NODE ; ++node)
    {
      cmd_node cn ;

      cn = &node_table[node] ;

      cn->installed    = false ;
      cn->config_write = NULL ;
      cn->cmd_vector   = NULL ;

      if ((cn->prompt != NULL) && (node != AUTH_NODE)
                               && (node != AUTH_ENABLE_NODE) )
        cn->executable = true ;
      else
        cn->executable = false ;

      if (node == NULL_NODE)
        cn->parent = NULL_NODE ;
      else
        {
          if (cn->parent == NULL_NODE)
            {
              if      (node <  MIN_CONFIG_NODE)
                cn->parent = node ;
              else if (node <= MAX_CONFIG_NODE)
                cn->parent = ENABLE_NODE ;
              else
                cn->parent = CONFIG_NODE ;
            } ;
        } ;
    } ;

  /* Check that the parent for all nodes is valid -- ensures the tree is
   * valid and contains no loops !
   */
  for (node = MIN_NODE ; node <= MAX_NODE ; ++node)
    cmd_node_parent_check(node) ;

  /* Fix up the .end_to and .exit_to where those are not set, and check for
   * validity where are set.
   *
   * Forces all not .executable, and META_NODE to NULL_NODE.  (Note that
   * AUTH_NODE and AUTH_ENABLE_NODE are not executable, while META_NODE is.)
   *
   * NB: we have just checked that all .parent are valid, so we can assume that
   *     here.
   */
  for (node = MIN_NODE ; node <= MAX_NODE ; ++node)
    {
      cmd_node cn ;
      node_type_t exit_to, end_to ;

      cn = &node_table[node] ;

      if (!cn->executable || (node == META_NODE))
        {
          exit_to = NULL_NODE ;
          end_to  = NULL_NODE ;
        }
      else
        {
          exit_to = cn->parent ;
          if (exit_to == node)
            exit_to = EXIT_NODE ;

          end_to  = node ;
          while (end_to >= MIN_CONFIG_NODE)
            end_to = node_table[end_to].parent ;

          if (cn->exit_to != NULL_NODE)
            qassert(cn->exit_to == exit_to) ;

          if (cn->end_to  != NULL_NODE)
            qassert(cn->exit_to == end_to) ;

        } ;

      cn->exit_to = exit_to ;
      cn->end_to  = end_to ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Check that given node has valid parent, grandparent, etc.
 *
 * Valid parent etc are:
 *
 *   NULL_NODE      -- NULL_NODE (self !)
 *
 *   SPECIAL nodes  -- themselves
 *
 *   VIEW nodes     -- themselves
 *
 *   ENABLE nodes   -- themselves or another ENABLE node whose parent or
 *                     grandparent etc is its own parent (so, no circles).
 *
 *   CONFIG nodes   -- an ENABLE node or another CONFIG node whose parent or
 *                     grandparent etc has an ENABLE parent (so, no circles).
 *
 *   SPECIFIC nodes -- a CONFIG node or another SPECIFIC node whose parent or
 *                     grandparent etc has a CONFIG parent (so, no circles).
 */
static void
cmd_node_parent_check(node_type_t node)
{
  ullong visit ;                /* loop check           */

  confirm((sizeof(ullong) * 8) >= MAX_NODE) ;

  if (node <= MAX_VIEW_NODE)
    {
      /* For NULL_NODE and all SPECIAL and VIEW nodes -- parent must be self.
       */
      confirm(NULL_NODE        == MIN_NODE) ;
      confirm(MIN_SPECIAL_NODE == (NULL_NODE + 1)) ;
      confirm(MIN_VIEW_NODE    == (MAX_SPECIAL_NODE + 1)) ;

      passert(node_table[node].parent == node) ;

      return ;
    } ;

  passert(node <= MAX_NODE) ;

  /* Now have ENABLE, CONFIG or SPECIFIC node
   */
  visit = 0 ;

  while (1)
    {
      node_type_t pnode ;

      visit |= ((ullong)1 << node) ;

      pnode = node_table[node].parent ;
      if (pnode == node)
        {
          /* Found terminal node, which MUST be an ENABLE node
           */
          passert((node >= MIN_ENABLE_NODE) && (node <= MAX_ENABLE_NODE)) ;

          return ;
        } ;

      /* Not terminal, so check that pnode is valid parent of node.
       */
      if (node <= MAX_VIEW_NODE)
        {
          passert(false) ;
        }
      else if (node <= MAX_ENABLE_NODE)
        {
          confirm(MIN_ENABLE_NODE == (MAX_VIEW_NODE + 1)) ;

          /* Parent of an ENABLE node must be an ENABLE node
           */
          passert((pnode >= MIN_ENABLE_NODE) && (pnode <= MAX_ENABLE_NODE)) ;
        }
      else if (node <= MAX_CONFIG_NODE)
        {
          confirm(MIN_CONFIG_NODE == (MAX_ENABLE_NODE + 1)) ;

          /* Parent of a CONFIG node must be an ENABLE or CONFIG node
           */
          passert((pnode >= MIN_ENABLE_NODE) && (pnode <= MAX_CONFIG_NODE)) ;
        }
      else if (pnode <= MAX_SPECIFIC_NODE)
        {
          confirm(MIN_SPECIFIC_NODE == (MAX_CONFIG_NODE + 1)) ;

          /* Parent of a SPECIFIC node must be a CONFIG or SPECIFIC node
           */
          passert((pnode >= MIN_CONFIG_NODE) && (pnode <= MAX_SPECIFIC_NODE)) ;
        }
      else
        {
          confirm(MAX_NODE == MAX_SPECIFIC_NODE) ;
          passert(false) ;
        } ;

      /* Advance to pnode, now we know pnode is valid, and loop check.
       */
      node = pnode ;

      passert((visit & ((ullong)1 << node)) == 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Return address of cmd_node for given node -- return NULL_NODE if out of
 * range.
 *
 * Note that will happily return NULL_NODE, EXIT_NODE and any undefined nodes
 * (if the node space were ever to be sparse).
 */
inline static cmd_node
cmd_cmd_node(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return &node_table[((node >= MIN_NODE) && (node <= MAX_NODE)) ? node
                                                                : NULL_NODE
                    ] ;
} ;

/*------------------------------------------------------------------------------
 * Return address of cmd_node for given node -- return NULL_NODE if out of
 * range.
 *
 * Note that will happily return NULL_NODE, EXIT_NODE and any undefined nodes
 * (if the node space were ever to be sparse).
 *
 * Requires that the command table is complete.
 */
extern cmd_node
cmd_get_cmd_node(node_type_t node)
{
  qassert(node_table_complete) ;

  return cmd_cmd_node(node) ;
} ;

/*------------------------------------------------------------------------------
 * Return name of given node
 *
 * Doesn't matter whether node is installed or not.  Invalid and unknown nodes
 * return "NULL" and "*UNKNOWN*" respectively.
 */
extern const char*
cmd_node_name(node_type_t node)
{
  const char* name ;

  name = cmd_cmd_node(node)->name ;

  return (name != NULL) ? name : "*UNKNOWN*" ;
} ;

/*------------------------------------------------------------------------------
 * Return node of given name -- complete match
 *
 * Returns:  NULL_NODE if fails to recognise name
 */
extern node_type_t
cmd_node_by_name(const char* name)
{
  node_type_t node ;

  for (node = MIN_NODE ; node <= MAX_NODE ; ++node)
    {
      if ((node_table[node].name != NULL)
                                 && (strcmp(node_table[node].name, name) == 0))
        return node ;
    } ;

  return NULL_NODE ;
} ;

/*------------------------------------------------------------------------------
 * Return prompt string for the specified node -- if any !
 *
 * Returns NULL for any node that has no commands (except for the special
 * cases of AUTH_NODE and AUTH_ENABLE_NODE).  Also returns NULL for invalid or
 * unknown nodes.
 */
extern const char *
cmd_prompt(node_type_t node)
{
  return cmd_cmd_node(node)->prompt ;
} ;

/*------------------------------------------------------------------------------
 * Return daemons supported by node.
 *
 * Will be none if the node is NULL_NODE, EXIT_NODE, invalid or an
 * unknown node.
 *
 * Does not care whether is installed or not, but once installed the daemons
 * supported is those supported now that the node is installed.
 */
extern daemon_set_t
cmd_node_daemons(node_type_t node)
{
  return cmd_cmd_node(node)->daemons ;
} ;

/*------------------------------------------------------------------------------
 * Return parent node -- will be NULL_NODE for NULL_NODE, EXIT_NODE, invalid or
 *                       an unknown node.
 *
 * Some nodes (<= MAX_ENABLE_NODE) are their own parents, signalling an end to
 * how far up the tree one can traverse.
 */
extern node_type_t
cmd_node_parent(node_type_t node)
{
  return cmd_cmd_node(node)->parent ;
} ;

/*------------------------------------------------------------------------------
 * Return first CONFIG node ancestor of a SPECIFIC node
 *
 * Returns NULL_NODE if node is not SPECIFIC, or we don't find CONFIG node
 *                      (the later should be impossible !).
 */
extern node_type_t
cmd_node_config_parent(node_type_t node)
{
  while (1)
    {
      node_type_t pnode ;

      if (!cmd_node_is_specific(node))
        return NULL_NODE ;

      pnode = node_table[node].parent ;

      if (cmd_node_is_config(pnode))
        return pnode ;

      if ((pnode == node) || (pnode == NULL_NODE))
        return NULL_NODE ;

      node = pnode ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Return first ENABLE node ancestor etc of a CONFIG or SPECIFIC node
 *
 * Returns NULL_NODE if node is not CONFIG or SPECIFIC.
 */
extern node_type_t
cmd_node_enable_parent(node_type_t node)
{
  if (cmd_node_is_cs(node))
    return node_table[node].end_to ;
  else
    return NULL_NODE ;
} ;

/*------------------------------------------------------------------------------
 * Return exit_to node -- returns EXIT_NODE if node is not installed, or is
 *                        invalid, or is unknown, or has no valid exit.
 */
extern node_type_t
cmd_node_exit_to(node_type_t node)
{
  node_type_t  enode ;

  enode = cmd_cmd_node(node)->exit_to ;

  qassert(enode != NULL_NODE) ;

  return (enode != NULL_NODE) ? enode : EXIT_NODE ;
} ;

/*------------------------------------------------------------------------------
 * Return end_to node -- returns EXIT_NODE if node is not installed, or is
 *                       invalid, or is unknown, or has no valid end.
 */
extern node_type_t
cmd_node_end_to(node_type_t node)
{
  node_type_t  enode ;

  enode = cmd_cmd_node(node)->end_to ;

  qassert(enode != NULL_NODE) ;

  return (enode != NULL_NODE) ? enode : EXIT_NODE ;
} ;

/*------------------------------------------------------------------------------
 * Return whether given node is installed, or not.
 *
 * Will not be installed if the node is NULL_NODE, EXIT_NODE, invalid or an
 * unknown node.
 */
extern bool
cmd_node_is_installed(node_type_t node)
{
  return cmd_cmd_node(node)->installed ;
} ;

/*------------------------------------------------------------------------------
 * Return whether given node is executable, or not.
 *
 * Will not be executable if the node is NULL_NODE, EXIT_NODE, invalid or an
 * unknown node.
 */
extern bool
cmd_node_is_executable(node_type_t node)
{
  return cmd_cmd_node(node)->executable ;
} ;

/*------------------------------------------------------------------------------
 * Node is a SPECIAL node
 */
extern bool
cmd_node_is_special(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return (node >= MIN_SPECIAL_NODE) && (node <= MAX_SPECIAL_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node is a VIEW node
 */
extern bool
cmd_node_is_view(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return (node >= MIN_VIEW_NODE)    && (node <= MAX_VIEW_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node is an ENABLE node
 */
extern bool
cmd_node_is_enable(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return (node >= MIN_ENABLE_NODE)  && (node <= MAX_ENABLE_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node is a CONFIG node
 */
extern bool
cmd_node_is_config(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return (node >= MIN_CONFIG_NODE) && (node <= MAX_CONFIG_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node is a SPECIFIC node
 */
extern bool
cmd_node_is_specific(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  return (node >= MIN_SPECIFIC_NODE)  && (node <= MAX_SPECIFIC_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node requires the configuration symbol of power
 */
extern bool
cmd_node_is_config_lock(node_type_t node)
{
  return cmd_node_is_cs(node) ;
} ;

/*------------------------------------------------------------------------------
 * Node is ENABLE, CONFIG or SPECIFIC
 */
extern bool
cmd_node_is_ecs(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  confirm(MAX_ENABLE_NODE == (MIN_CONFIG_NODE   - 1)) ;
  confirm(MAX_CONFIG_NODE == (MIN_SPECIFIC_NODE - 1)) ;

  return (node >= MIN_ENABLE_NODE)  && (node <= MAX_SPECIFIC_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Node is CONFIG or SPECIFIC
 */
extern bool
cmd_node_is_cs(node_type_t node)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)) ;

  confirm(MAX_CONFIG_NODE == (MIN_SPECIFIC_NODE - 1)) ;

  return (node >= MIN_CONFIG_NODE)  && (node <= MAX_SPECIFIC_NODE) ;
} ;

/*------------------------------------------------------------------------------
 * Is given node an ancestor of the given child.
 *
 * NB: for these purposes a node cannot be its own ancestor -- so will return
 *     false if node == child, or if child is its own parent.
 */
extern bool
cmd_node_is_ancestor(node_type_t node, node_type_t child)
{
  while (child > MIN_CONFIG_NODE)
    {
      node_type_t p ;

      p = cmd_node_parent(child) ;

      if (p == child)
        break ;                 /* stop if hit dead-end         */

      if (p == node)
        return true ;           /* node is senior to child      */

      child = p ;
    } ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Is given child a descendant of the given node.
 *
 * NB: for these purposes a node cannot be its own descendant -- so will return
 *     false if child == node, or if child is its own parent.
 */
extern bool
cmd_node_is_decendant(node_type_t child, node_type_t node)
{
  return cmd_node_is_ancestor(node, child) ;
} ;

/*------------------------------------------------------------------------------
 * Choose which node to set when restoring a command context
 *
 * There may be context beyond the current node level (!) -- that is to say,
 * that there may be state which is related to the node level but which we know
 * nothing about.  So we cannot simply restore the old node -- much as one
 * would like to.  What we aim to do is to return to the highest node possible,
 * but no higher than the old node.
 *
 * The rules of context are:
 *
 *   * all nodes which are not SPECIFIC have no unique context
 *
 *     in particular, can execute an ENABLE command in any context, and
 *     any node.
 *
 *     This means that we can bounce between not SPECIFIC nodes trivially.
 *
 *   * an ancestor node's context is a subset of any child context
 *
 *     This means that can move from child to ancestor without needing to
 *     update any (other) context.
 *
 * If the new and old nodes are not equal we must choose which node can safely
 * return to:
 *
 *   * if old is not SPECIFIC
 *
 *     return to old -- whatever it was it has no extra context, so we can
 *     safely restore the old node.
 *
 *   * otherwise, if new is not SPECIFIC (old is SPECIFIC)
 *
 *     Because old was SPECIFIC, then any unique context the old node had has
 *     been lost... so the best we can do is fall back to old's CONFIG parent.
 *
 *   * otherwise (both new and old are SPECIFIC)
 *
 *       - can return to old iff old is an ancestor of new.
 *
 *         old will have subset of new's context
 *
 *       - can stay in new iff new is an ancestor of old
 *
 *         new will have subset of old's context
 *
 *       - if old and new have a common SPECIFIC ancestor, can fall back to
 *         that.
 *
 *         the common ancestor will have a subset of new's context.
 *
 *       - otherwise, must fall back to old's "CONFIG" parent
 *
 *         because there is no part of new's context which applies.
 *
 * NB: it is the caller's responsibility to worry about the config symbol of
 *     power.
 */
extern node_type_t
cmd_node_restore(node_type_t old, node_type_t new)
{
  if ((new == old) || !cmd_node_is_specific(old))
    return old ;

  if (cmd_node_is_specific(new))
    {
      /* We now have old and new nodes which may or may not have any context in
       * common, and which are both SPECIFIC.  (new != old).
       *
       * Track back and if we find a mutual SPECIFIC ancestor, then return
       * that.  Note that this includes the case where one is the ancestor of
       * the other.
       */
      do
        {
          node_type_t pold ;

          qassert(cmd_node_is_specific(old) && cmd_node_is_specific(new)) ;

          /* Starting with "old", see if it or any of its SPECIFIC node
           * ancestors are the same as the current "new".
           *
           * First time around the loop, "new" is the original "new", so this
           * will find whether the original "new" is an ancestor of "old".
           *
           * On subsequent times around the loop, "new" is the parent of the
           * previous "new", so this will also find whether "old" is an
           * ancestor of the original "new".
           */
          pold = old ;

          do
            {
              if (pold == new)
                return pold ;       /* common SPECIFIC parent of old and new */

              pold = cmd_node_parent(pold) ;
            }
          while (cmd_node_is_specific(pold)) ;

          qassert(cmd_node_is_config(pold)) ;       /* Parent of SPECIFIC   */

          /* Track back to parent of current new.
           *
           * If that is not a SPECIFIC node, then
           */
          new = cmd_node_parent(new) ;
        }
      while (cmd_node_is_specific(new)) ;

      /* Have tracked "new" back beyond SPECIFIC nodes, so the new and old can
       * have no context in common.
       */
      qassert(cmd_node_is_config(new)) ;            /* Parent of SPECIFIC   */
    } ;

  /* We have found new and old have no context in common, so the best we can do
   * is to restore to the CONFIG parent of old.
   */
  return cmd_node_config_parent(old) ;
} ;


/*------------------------------------------------------------------------------
 * Install configuration write function for the given node.
 *
 * Installs node, if required (and allowed).
 *
 * NB: may install a configuration write function only for nodes which
 *     are >= MIN_CONFIG_NODE.
 *
 * NB: may install a configuration write function once only.
 */
extern void
cmd_install_node_config_write(node_type_t node, int (*config_write) (vty))
{
  cmd_node cn ;

  if (node < MIN_CONFIG_NODE)
    {
      fprintf (stderr, "%s: node %s may not have a config_write\n",
                                                __func__, cmd_node_name(node)) ;
      exit (1);
    } ;

  cn = cmd_install_node(node) ;

  if (cn->config_write != NULL)
    {
      fprintf (stderr, "%s: node %s already has a config_write\n",
                                                __func__, cmd_node_name(node)) ;
      exit (1);
    } ;

  cn->config_write = config_write ;
} ;

/*------------------------------------------------------------------------------
 * Run down given cmd_install_table, and install commands whose daemon set
 * intersects with the global daemons_set.
 *
 * Installs nodes, if required (and allowed).
 *
 * Each command table has a daemons setting which applies globally to all
 * commands in the table.
 *
 * Each command table item also has a local add_daemons setting and a
 * del_daemons setting, which apply to that single command.
 *
 * Any item->del_daemons setting is a mask, eg: ~VTYSH_VD
 *
 * The effective daemon for a command is the combination of the table and
 * item settings:
 *
 *   daemons = (table->daemons | item->add_daemons)
 *
 *   if (item->del_daemons != 0) daemons &= item->del_daemons
 *
 * In the simple case, a table will just have a table->daemons, and no item
 * will have any add_daemons or del_daemons.
 *
 * A table which contains commands which should be installed for some
 * daemons but not others can be implemented using add_daemons and/or
 * del_daemons on the relevant items.
 *
 * The extract.pl program in vtysh gathers together all commands for all
 * daemons (but not for the Quagga lib) into one large install table.  In doing
 * so it merges all the install tables it finds, taking each table's global
 * daemons setting and merging it with each table item's add_daemons.  The
 * collected table has no global daemons setting.
 *
 * When installing for VTYSH_VD we implicitly install for the magic TERM marker.
 * This marker is used when executing commands in the vtysh, and is present
 * on commands which do not execute in any real daemon from the vtysh.
 *
 * Also, when installing for VTYSH_VD we implicitly ignore commands for the
 * RESTRICTED_NODE.
 */
extern void
cmd_install_table(cmd_table table)
{
  cmd_table_item item ;
  daemon_set_t install_daemons ;
  bool vtysh ;

  install_daemons = daemons_set ;

  vtysh = (install_daemons & VTYSH_VD) != 0 ;

  if (vtysh)
    install_daemons |= TERM ;           /* keep the magic marker        */

  for (item = table->body ; item->cmd != NULL ; ++item )
    {
      daemon_set_t daemons ;

      if (vtysh && (item->node == RESTRICTED_NODE))
        continue ;

      daemons = (table->daemons | item->add_daemons) & install_daemons ;

      if (item->del_daemons != 0)
        daemons &= item->del_daemons ;

      if (daemons != 0)
        cmd_install_command(item->node, item->cmd, daemons) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Install a command into a node and add daemons to it.
 *
 * Installs node, if required (and allowed).
 *
 * A command may appear in a number of nodes, but the command is the same in
 * all of them, and applies to the same set of daemons -- this allows a single
 * command definition to appear in more than one node.  Once a cmd_command has
 * been installed in one node, it will never be changed, even if installed in
 * another node -- except for the daemons to which it applies.
 *
 * For daemons other than the vtysh, commands will only be installed in nodes
 * which are used by the daemon.  For the vtysh, however, it is possible for
 * a command to be installed in node 'a', for execution by daemon 'x', but in
 * node 'b', for execution by daemon 'y'.  Each node is marked as being for a
 * given set of daemons, so the vtysh ands the node's daemon set with the
 * command's daemon set before deciding where to execute a given command.
 *
 * Note that this does not address the possibility that two commands may
 * be the same at a text level, but be different as far as the command system
 * is concerned -- attempting to execute such a command will throw out an
 * "ambiguous" error.
 */
extern void
cmd_install_command(node_type_t node, cmd_command cmd, daemon_set_t daemons)
{
  cmd_node cn ;

  /* Find node and install if required (and allowed).
   */
  cn = cmd_install_node(node) ;

  /* Check that can install the given command.
   *
   * Note NULL_NODE, EXIT_NODE, invalid and unknown nodes are never installed.
   */
  if (cn->cmd_vector == NULL)
    {
      fprintf (stderr, "%s: node %s is not a command node (command '%s')\n",
                                   __func__, cmd_node_name(node), cmd->string) ;
      exit (1);
    } ;

  /* OK, install and, if necessary, compile the command.
   */
  vector_set (cn->cmd_vector, cmd);
  cmd->daemons |= daemons ;

  if (cmd->items == NULL)
    cmd_compile(cmd);

 /* Post compilation check for reasonable cmd_command !                */
  cmd_compile_check(cmd) ;
} ;

/*------------------------------------------------------------------------------
 * Install node, if not already installed and is allowed.
 *
 * Installs only if the set to the intersection between the node's preset
 * daemons and the global daemons_set is not empty.
 *
 * Failing to install is a FATAL error -- should not get this far if node
 * is inapplicable.
 *
 * If succeeds (and is not already installed), sets:
 *
 *  .daemons            -- masks preset value with the daemons_set.
 *
 *  .installed          -- set true !
 *
 *  .cmd_vector         -- set
 *
 *  .config_write       -- set NULL.
 */
static cmd_node
cmd_install_node(node_type_t node)
{
  cmd_node     cn ;
  daemon_set_t daemons ;

  qassert(!node_table_complete && !qpthreads_active) ;
  qassert_usable_node(node, EXIT_NODE) ;

  cn = cmd_cmd_node(node) ;

  if (cn->installed)
    return cn ;                 /* Already installed !  */

  /* Make sure we are allowed to install this node.
   */
  daemons = cn->daemons & daemons_set ;

  if (daemons == DAEMON_NONE)
    {
      qstring qs1, qs2 ;

      qs1 = cmd_daemons_make_list(NULL, cn->daemons) ;
      qs2 = cmd_daemons_make_list(NULL, daemons_set) ;

      fprintf (stderr, "%s: node %s is for daemons %s, not %s\n",
                                            __func__, cmd_node_name(node),
                                               qs_string(qs1), qs_string(qs2)) ;
      exit (1);
    } ;

  /* Worry about whether have already installed, and if not, install.
   */
  cn->installed    = true ;
  cn->daemons      = daemons ;
  cn->config_write = NULL ;     /* make sure    */

  /* If node is executable, then need to set up a cmd_vector.  Also need to
   * make sure that the node allows for the vtysh, if running in same.
   *
   * If node is executable and is not the META_NODE, install the basic
   * commands:
   *
   *   exit, quit and end are installed to be executed in the given daemons.
   *
   *     For the vtysh this means that it can leave a, even if the relevant
   *     daemon(s) are not active (any more).
   *
   *   help and list commands.
   *
   *     For the vtysh, the help and list commands are installed for it to
   *     execute, *not* the various daemons.
   *
   * Note that we do not set up a cmd_vector if the node cannot have commands.
   * And if we do set up a cmd_vector, it is never empty !
   */
  if (!cn->executable)
    return cn ;

  qassert((node != AUTH_NODE) && (node != AUTH_ENABLE_NODE)) ;

  cn->cmd_vector = vector_init_new(NULL, 20) ;

  cn->daemons = (daemons |= (daemons_set & VTYSH_VD)) ;

  if (node == META_NODE)
    return cn ;

  cmd_install_command (node, &config_exit_cmd, daemons) ;
  cmd_install_command (node, &config_quit_cmd, daemons) ;
  cmd_install_command (node, &config_end_cmd,  daemons) ;

  cmd_install_command (node, &config_help_cmd, daemons | TERM) ;
  cmd_install_command (node, &config_list_cmd, daemons | TERM) ;

  return cn ;
} ;

/*------------------------------------------------------------------------------
 * Assert that given node is:
 *
 *   * not NULL_NODE
 *   * not the given node
 *   * not invalid
 *   * not unknown
 */
static void
qassert_usable_node(node_type_t node, node_type_t and_not)
{
  qassert((node >= MIN_NODE) && (node <= MAX_NODE)
                             && (node != NULL_NODE)
                             && (node == node_table[node].node) /* unknown */
                             && (node != and_not)) ;
} ;

/*------------------------------------------------------------------------------
 * Compare two command's string.  Used in sort_node ()
 */
static int
cmp_node (const struct cmd_command **a, const struct cmd_command **b)
{
  return strcmp ((*a)->string, (*b)->string);
}

#if 0 //<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-

static int
cmp_desc (const void *p, const void *q)
{
  const struct desc *a = *(struct desc * const *)p;
  const struct desc *b = *(struct desc * const *)q;

  return strcmp (a->cmd, b->cmd);
}

#endif //->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->>

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Concatenate argv argument into a single string, inserting ' ' between each
 * argument.
 *
 * Returns an XMALLOC'd MTYPE_TMP string, which the call must dispose of.
 */
extern char *
argv_concat (const char* const* argv, int argc, int shift)
{
  int i;
  size_t len;
  char *str;
  char *p;

  len = 0;
  for (i = shift; i < argc; i++)
    len += strlen(argv[i])+1;

  if (!len)
    return NULL;

  p = str = XMALLOC(MTYPE_TMP, len);
  for (i = shift; i < argc; i++)
    {
      size_t arglen;
      memcpy(p, argv[i], (arglen = strlen(argv[i])));
      p += arglen;
      *p++ = ' ';
    }
  *(p-1) = '\0';
  return str;
} ;

/*==============================================================================
 * Version and other help
 */

/*------------------------------------------------------------------------------
 * This is called from main when a daemon is invoked with -v or --version.
 *
 * Because this may be called before much has been set up, this does not use
 * anything terriply fancy !
 */
extern void
cmd_print_version (const char *progname)
{
  printf ("%s version %s\n", progname, QUAGGA_VERSION);
  printf ("%s\n", QUAGGA_COPYRIGHT);
}

/*------------------------------------------------------------------------------
 * Show version.
 */
DEFUN_CALL (show_version,
            show_version_cmd,
            "show version",
            SHOW_STR
            "Displays zebra version\n")
{
  VTY_LOCK() ;

  uty_out (vty->vio, QUAGGA_PROGNAME ": %s v" QUAGGA_VERSION " (%s%s).\n",
                              (host.progname != NULL) ? host.progname : "?",
                                  (host.name != NULL) ? host.name     : "?",
                                      vty_multi_nexus ? " pthreaded"  : "") ;
  uty_out (vty->vio, "%s\n", QUAGGA_COPYRIGHT) ;

  VTY_UNLOCK() ;

  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Help display function for all nodes -- used in all nodes.
 */
DEFUN_CALL (config_help,
       config_help_cmd,
       "help",
       "Description of the interactive help system\n")
{
  vty_out (vty,
      "Quagga VTY provides advanced help feature.  When you need help,\n"
      "anytime at the command line please press '?'.\n"
      "\n"
      "If nothing matches, the help list will be empty and you must backup\n"
      "until entering a '?' shows the available options.\n"
      "Two styles of help are provided:\n"
      "  1. Full help is available when you are ready to enter a\n"
      "     command argument (e.g. 'show ?') and describes each possible\n"
      "     argument.\n"
      "  2. Partial help is provided when an abbreviated argument is entered\n"
      "     and you want to know what arguments match the input\n"
      "     (e.g. 'show me?'.)\n"
      "\n") ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * List all not deprecated and not hidden commands -- used in all nodes.
 */
DEFUN_CALL (config_list,
            config_list_cmd,
            "list",
            "Print command list\n")
{
  uint        i;
  cmd_node    cn ;
  cmd_command cmd ;

  cn = cmd_cmd_node(vty->node) ;

  if (cn->cmd_vector != NULL)
    for (i = 0; i < vector_length (cn->cmd_vector); i++)
      if ( ((cmd = vector_get_item (cn->cmd_vector, i)) != NULL)
            && ((cmd->attr & (CMD_ATTR_DEPRECATED | CMD_ATTR_HIDDEN)) == 0) )
        vty_out (vty, "  %s\n", cmd->string);

  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * echo command
 */
DEFUN_HID_CALL (do_echo,
              echo_cmd,
              "echo .MESSAGE",
              "Echo a message back to the vty\n"
              "The message to echo\n")
{
  char *message;

  message = argv_concat(argv, argc, 0) ;
  vty_out (vty, "%s\n", message ? message : "") ;
  XFREE(MTYPE_TMP, message);

  return CMD_SUCCESS;
}

/*==============================================================================
 * Commands and other stuff related to:
 *
 *   * end (and ^Z)   -- if is CONFIG or SPECIFIC, go to parent ENABLE node.
 *
 *                       Otherwise, go to parent node, or do nothing if no
 *                       parent.
 *
 *                       This is installed in all nodes with commands (except
 *                       for META_NODE.
 *
 *   * exit           -- go to parent node or if there is no parent, go to
 *                       EXIT_NODE.
 *
 *                       This is installed in all nodes with commands (except
 *                       for META_NODE.
 *
 *   * enable         -- go to ENABLE_NODE, if can.
 *
 *                       This is installed in VIEW_NODE and RESTRICTED_NODE.
 *
 *                       It is also installed in ENABLE_NODE (and hence is
 *                       available anywhere), where it is a synonym for 'end' !
 *
 *                       For configuration reading and for VTY_VTYSH_SERVER and
 *                       VTY_VTYSH, no password is required.
 *
 *                       For VTY_TERMINAL, must already have authenticated
 *                       once, or must be able to enter AUTH_ENABLE_NODE.
 *
 *                       There is currently only one ENABLE node.
 *
 *                       Note, however, that all ENABLE_NODE commands are
 *                       available at ENABLE_NODE and above !
 *
 *   * disable        -- go to VIEW_NODE (aka User Exec).
 *
 *                       This is installed in ENABLE_NODE *only*.
 */

/*------------------------------------------------------------------------------
 * Enter a CONFIG NODE, possibly via password check.
 *
 * If the parser established that must authenticate, then may fail here if
 * we are not in the right state to run the authentication.
 *
 * The authentication itself may fail...
 *
 * If the parser established that can enter CONFIG node directly, that's what
 * happens, provided can gain the symbol of power !
 *
 * NB: installed in VIEW_NODE, RESTRICTED_NODE and ENABLE_NODE.
 */
DEFUN_ATTR (config_terminal,
            config_terminal_cmd,
            "configure terminal",
            "Configuration from vty interface\n"
            "Configuration terminal\n",
            CMD_ATTR_DIRECT + cmd_sp_configure)
{
  if (vty->exec->parsed->nnode == AUTH_ENABLE_NODE)
    return vty_cmd_can_auth_enable(vty) ;

  return vty_cmd_config_lock(vty, vty->exec->parsed->nnode) ;
} ;

ALIAS_ATTR (config_terminal,
            config_enable_configure_cmd,
            "enable configure",
            "Turn on privileged mode command\n"
            "Configuration terminal\n",
            CMD_ATTR_DIRECT + cmd_sp_configure)

/*------------------------------------------------------------------------------
 * Enter ENABLE_NODE, possibly via password check.
 *
 * If the parser established that can enter ENABLE_NODE directly, that's what
 * happens.
 *
 * If the parser established that must authenticate, then may fail here if
 * we are not in the right state to run the authentication.
 *
 * The authentication itself may fail...
 *
 * NB: installed in VIEW_NODE, RESTRICTED_NODE and ENABLE_NODE.
 */
DEFUN_ATTR (enable,
            config_enable_cmd,
            "enable",
            "Turn on privileged mode command\n",
            CMD_ATTR_DIRECT + cmd_sp_enable)
{
  if (vty->exec->parsed->nnode == ENABLE_NODE)
    return CMD_SUCCESS ;

  /* Otherwise, must authenticate to enter ENABLE_NODE.                 */
  return vty_cmd_can_auth_enable(vty) ;
} ;

/*------------------------------------------------------------------------------
 * disable command: end enabled state -> VIEW_NODE.
 *
 * NB: although only installed in ENABLE_NODE, it will be implicitly available
 *     in all higher nodes -- as a quick way of crashing out to VIEW_NODE !
 */
DEFUN_ATTR(disable,
           config_disable_cmd,
           "disable",
           "Turn off privileged mode command\n",
           CMD_ATTR_DIRECT + CMD_ATTR_NODE + VIEW_NODE)
{
  return CMD_SUCCESS ;  /* will disable to parsed->nnode        */
}

/*------------------------------------------------------------------------------
 * exit command: down one node level, including exit command processor.
 */
DEFUN_ATTR(config_exit,
           config_exit_cmd,
           "exit",
           "Exit current mode and down to previous mode\n",
           CMD_ATTR_DIRECT + cmd_sp_exit)
{
  return CMD_SUCCESS ;  /* will exit to parsed->nnode   */
}

/* quit is alias of exit.                                               */
ALIAS_ATTR (config_exit,
            config_quit_cmd,
            "quit",
            "Exit current mode and down to previous mode\n",
            CMD_ATTR_DIRECT + cmd_sp_exit) ;

/*------------------------------------------------------------------------------
 * end command: down to enable mode.
 */
DEFUN_ATTR (config_end,
            config_end_cmd,
            "end",
            "End current mode and change to enable mode\n",
            CMD_ATTR_DIRECT + cmd_sp_end)
{
  return CMD_SUCCESS ;  /* will end to parsed->nnode    */
}

/*==============================================================================
 * Logging configuration.
 *
 */
static const struct facility_map {
  int facility;
  const char *name;
} syslog_facilities[] =
  {
    { LOG_KERN,   "kern"   },
    { LOG_USER,   "user"   },
    { LOG_MAIL,   "mail"   },
    { LOG_DAEMON, "daemon" },
    { LOG_AUTH,   "auth"   },
    { LOG_SYSLOG, "syslog" },
    { LOG_LPR,    "lpr"    },
    { LOG_NEWS,   "news"   },
    { LOG_UUCP,   "uucp"   },
    { LOG_CRON,   "cron"   },
#ifdef LOG_FTP
    { LOG_FTP, "ftp"       },
#endif
    { LOG_LOCAL0, "local0" },
    { LOG_LOCAL1, "local1" },
    { LOG_LOCAL2, "local2" },
    { LOG_LOCAL3, "local3" },
    { LOG_LOCAL4, "local4" },
    { LOG_LOCAL5, "local5" },
    { LOG_LOCAL6, "local6" },
    { LOG_LOCAL7, "local7" },
    { 0, NULL },
  };

/*------------------------------------------------------------------------------
 * Map log facility number to name.
 */
static const char *
facility_name(int facility)
{
  const struct facility_map *fm;

  for (fm = syslog_facilities ; (fm->name != NULL) ; fm++)
    if (fm->facility == facility)
      return fm->name;

  return "";
}

/*------------------------------------------------------------------------------
 * Map log facility name to number.
 *
 * Allows partial matches, as long as is unambiguous.
 *
 * Puts error message to vty if does not match (if vty != NULL).
 */
static int
facility_match(vty vty, const char* s)
{
  const struct facility_map *fm ;
  int   m ;
  ulen  l ;

  m = -1 ;
  l = strlen(s) ;

  for (fm = syslog_facilities ; (fm->name != NULL) ; fm++)
    {
      if (strncmp(fm->name, s, l) == 0)
        {
          if (m < 0)
            m = fm->facility ;
          else
            {
              m = -1 ;
              break ;
            } ;
        }
    } ;

  if ((m < 0) && (vty != NULL))
    vty_out(vty, "%% did not recognise facility '%s'\n", s) ;

  return m ;
} ;

/*------------------------------------------------------------------------------
 * Map log level name to number.
 *
 * Allows partial matches, as long as is unambiguous.
 *
 * Puts error message to vty if does not match (if vty != NULL).
 */
static int
level_match(vty vty, const char* s)
{
  int   level ;
  int   m ;
  ulen  l ;

  m = ZLOG_DISABLED ;
  l = strlen(s) ;

  for ( level = 0 ; zlog_priority [level] != NULL ; level++ )
    {
      if (strncmp (zlog_priority[level], s, l) == 0)
        {
          if (m == ZLOG_DISABLED)
            m = level ;
          else
            {
              m = ZLOG_DISABLED ;
              break ;
            } ;
        } ;
    } ;

  if ((m == ZLOG_DISABLED) && (vty != NULL))
    vty_out(vty, "%% did not recognise log level '%s'\n", s) ;

  return m ;
} ;

/*------------------------------------------------------------------------------
 * Put message to log at given log level
 */
DEFUN_CALL (config_logmsg,
       config_logmsg_cmd,
       "logmsg "LOG_LEVELS" .MESSAGE",
       "Send a message to enabled logging destinations\n"
       LOG_LEVEL_DESC
       "The message to send\n")
{
  int level;
  char *message;

  if ((level = level_match(vty, argv[0])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  message = argv_concat(argv, argc, 1);
  zlog(NULL, level, "%s", (message ? message : ""));
  XFREE(MTYPE_TMP, message);
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Show state of logging
 */
DEFUN_CALL (show_logging,
       show_logging_cmd,
       "show logging",
       SHOW_STR
       "Show current logging configuration\n")
{
  int lvl ;

  vty_out (vty, "Syslog logging: ");
  if ((lvl = zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG)) == ZLOG_DISABLED)
    vty_out (vty, "disabled\n");
  else
    vty_out (vty, "level %s, facility %s, ident %s\n", zlog_priority[lvl],
                 facility_name(zlog_get_facility(NULL)), zlog_get_ident(NULL)) ;

  vty_out (vty, "Stdout logging: ");
  if ((lvl = zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT)) == ZLOG_DISABLED)
    vty_out (vty, "disabled\n");
  else
    vty_out (vty, "level %s\n", zlog_priority[lvl]) ;

  vty_out (vty, "Monitor logging: ");
  if ((lvl = zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR)) == ZLOG_DISABLED)
    vty_out (vty, "disabled\n");
  else
    vty_out (vty, "level %s\n", zlog_priority[lvl]);

  vty_out (vty, "File logging: ");
  if (((lvl = zlog_get_maxlvl(NULL, ZLOG_DEST_FILE)) == ZLOG_DISABLED) ||
                                                            !zlog_is_file(NULL))
    vty_out (vty, "disabled\n");
  else
    {
      char * filename = zlog_get_filename(NULL);
      vty_out (vty, "level %s, filename %s\n", zlog_priority[lvl], filename) ;
      free(filename);
    }

  vty_out (vty, "Protocol name: %s\n", zlog_get_proto_name(NULL));
  vty_out (vty, "Record priority: %s\n",
                    (zlog_get_record_priority(NULL) ? "enabled" : "disabled")) ;
  vty_out (vty, "Timestamp precision: %d\n",
                                           zlog_get_timestamp_precision(NULL)) ;

  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * stdout logging level
 */
DEFUN_CALL (config_log_stdout,
       config_log_stdout_cmd,
       "log stdout",
       "Logging control\n"
       "Set stdout logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_STDOUT, zlog_get_default_lvl(NULL));
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_stdout_level,
       config_log_stdout_level_cmd,
       "log stdout "LOG_LEVELS,
       "Logging control\n"
       "Set stdout logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(vty, argv[0])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_level (NULL, ZLOG_DEST_STDOUT, level);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_stdout,
       no_config_log_stdout_cmd,
       "no log stdout [LEVEL]",
       NO_STR
       "Logging control\n"
       "Cancel logging to stdout\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_STDOUT, ZLOG_DISABLED);
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * monitor logging level
 *
 * Each VTY has its own logging level for monitor logging, and its own
 * enable/disable.  When logging is enabled, the current monitor logging
 * level is set for the VTY.
 *
 * The monitor logging level is a bit special -- setting this level affects
 * the current VTY (if it is a VTY_TERMINAL) and any future VTY.  It also
 * affects the level which will be written away to any configuration file.
 */
DEFUN_CALL (config_log_monitor,
       config_log_monitor_cmd,
       "log monitor",
       "Logging control\n"
       "Set terminal line (monitor) logging level\n")
{
  int level = zlog_get_default_lvl(NULL) ;
  zlog_set_level (NULL, ZLOG_DEST_MONITOR, level) ;
  vty_monitor_set_level(vty, level) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_monitor_level,
       config_log_monitor_level_cmd,
       "log monitor "LOG_LEVELS,
       "Logging control\n"
       "Set terminal line (monitor) logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(vty, argv[0])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_level (NULL, ZLOG_DEST_MONITOR, level);
  vty_monitor_set_level(vty, level) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_monitor,
       no_config_log_monitor_cmd,
       "no log monitor [LEVEL]",
       NO_STR
       "Logging control\n"
       "Disable terminal line (monitor) logging\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_MONITOR, ZLOG_DISABLED);
  vty_monitor_set_level(vty, ZLOG_DISABLED) ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Set new logging file and level -- "log file FILENAME [LEVEL]"
 *
 * Note that even if fail to open the new log file, will set host.logfile.
 *
 * Failure here is an error.
 */
static int
set_log_file(struct vty *vty, const char *fname, int loglevel)
{
  int    err ;

  VTY_LOCK() ;

  host.logfile = uty_cmd_path_name_complete(host.logfile, fname,
                                                           vty->exec->context) ;
  err  = zlog_set_file (NULL, qpath_string(host.logfile), loglevel) ;

  VTY_UNLOCK() ;

  if (err == 0)
    return CMD_SUCCESS ;

  vty_out(vty, "%% failed to open log file %s: %s (%s)\n",
                                                  qpath_string(host.logfile),
                                                        errtostr(err, 0).str,
                                                        errtoname(err, 0).str) ;
  return CMD_WARNING ;
}

DEFUN_CALL (config_log_file,
       config_log_file_cmd,
       "log file FILENAME",
       "Logging control\n"
       "Logging to file\n"
       "Logging filename\n")
{
  return set_log_file(vty, argv[0], zlog_get_default_lvl(NULL));
}

DEFUN_CALL (config_log_file_level,
       config_log_file_level_cmd,
       "log file FILENAME " LOG_LEVELS,
       "Logging control\n"
       "Logging to file\n"
       "Logging filename\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(vty, argv[1])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  return set_log_file(vty, argv[0], level);
}

DEFUN_CALL (no_config_log_file,
       no_config_log_file_cmd,
       "no log file [FILENAME]",
       NO_STR
       "Logging control\n"
       "Cancel logging to file\n"
       "Logging file name\n")
{
  VTY_LOCK() ;

  zlog_reset_file (NULL);

  host.logfile = qpath_free(host.logfile) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

ALIAS_CALL (no_config_log_file,
       no_config_log_file_level_cmd,
       "no log file FILENAME LEVEL",
       NO_STR
       "Logging control\n"
       "Cancel logging to file\n"
       "Logging file name\n"
       "Logging level\n")

/*------------------------------------------------------------------------------
 * syslog logging configuration
 */
DEFUN_CALL (config_log_syslog,
       config_log_syslog_cmd,
       "log syslog",
       "Logging control\n"
       "Set syslog logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, zlog_get_default_lvl(NULL));
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_syslog_level,
       config_log_syslog_level_cmd,
       "log syslog "LOG_LEVELS,
       "Logging control\n"
       "Set syslog logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(vty, argv[0])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, level);
  return CMD_SUCCESS;
}

DEFUN_DEP_CALL (config_log_syslog_facility,
                  config_log_syslog_facility_cmd,
                  "log syslog facility "LOG_FACILITIES,
                  "Logging control\n"
                  "Logging goes to syslog\n"
                  "(Deprecated) Facility parameter for syslog messages\n"
                  LOG_FACILITY_DESC)
{
  int facility;

  if ((facility = facility_match(vty, argv[0])) < 0)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, zlog_get_default_lvl(NULL));
  zlog_set_facility(NULL, facility);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_syslog,
       no_config_log_syslog_cmd,
       "no log syslog [LEVEL]",
       NO_STR
       "Logging control\n"
       "Cancel logging to syslog\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, ZLOG_DISABLED);
  return CMD_SUCCESS;
}

ALIAS_CALL (no_config_log_syslog,
       no_config_log_syslog_facility_cmd,
       "no log syslog facility "LOG_FACILITIES,
       NO_STR
       "Logging control\n"
       "Logging goes to syslog\n"
       "Facility parameter for syslog messages\n"
       LOG_FACILITY_DESC)

DEFUN_CALL (config_log_facility,
       config_log_facility_cmd,
       "log facility "LOG_FACILITIES,
       "Logging control\n"
       "Facility parameter for syslog messages\n"
       LOG_FACILITY_DESC)
{
  int facility;

  if ((facility = facility_match(vty, argv[0])) < 0)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_facility(NULL, facility);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_facility,
       no_config_log_facility_cmd,
       "no log facility [FACILITY]",
       NO_STR
       "Logging control\n"
       "Reset syslog facility to default (daemon)\n"
       "Syslog facility\n")
{
  zlog_set_facility(NULL, LOG_DAEMON);
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Deprecated log trap commands
 */
DEFUN_DEP_CALL (config_log_trap,
                config_log_trap_cmd,
                "log trap "LOG_LEVELS,
                "Logging control\n"
                "(Deprecated) Set logging level and default for all destinations\n"
                LOG_LEVEL_DESC)
{
  int level ;

  if ((level = level_match(vty, argv[0])) == ZLOG_DISABLED)
    return CMD_ERROR ;  /* parser SHOULD have spotted this      */

  zlog_set_default_lvl_dest (NULL, level);
  return CMD_SUCCESS;
}

DEFUN_DEP_CALL (no_config_log_trap,
                  no_config_log_trap_cmd,
                  "no log trap [LEVEL]",
                  NO_STR
                  "Logging control\n"
                  "Permit all logging information\n"
                  "Logging level\n")
{
  zlog_set_default_lvl(NULL, LOG_DEBUG);
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Logging of priority and timestamp precision settings
 */
DEFUN_CALL (config_log_record_priority,
       config_log_record_priority_cmd,
       "log record-priority",
       "Logging control\n"
       "Log the priority of the message within the message\n")
{
  zlog_set_record_priority(NULL, 1) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_record_priority,
       no_config_log_record_priority_cmd,
       "no log record-priority",
       NO_STR
       "Logging control\n"
       "Do not log the priority of the message within the message\n")
{
  zlog_set_record_priority(NULL, 0) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_timestamp_precision,
       config_log_timestamp_precision_cmd,
       "log timestamp precision <0-6>",
       "Logging control\n"
       "Timestamp configuration\n"
       "Set the timestamp precision\n"
       "Number of subsecond digits\n")
{
  int timestamp_precision;

  VTY_GET_INTEGER_RANGE("Timestamp Precision",
                                           timestamp_precision, argv[0], 0, 6) ;
  zlog_set_timestamp_precision(NULL, timestamp_precision);

  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_timestamp_precision,
       no_config_log_timestamp_precision_cmd,
       "no log timestamp precision",
       NO_STR
       "Logging control\n"
       "Timestamp configuration\n"
       "Reset the timestamp precision to the default value of 0\n")
{
  zlog_set_timestamp_precision(NULL, 0);
  return CMD_SUCCESS;
}

/*==============================================================================
 * Writing and showing configuration
 */

static int config_write_file_node(vty vty, node_type_t node) ;

/*------------------------------------------------------------------------------
 * This function writes the configuration of this host to the vty.
 *
 * This is the configuration writing associated with the CONFIG_NODE.
 */
static int
config_write_host(vty vty)
{
  vty_io vio ;

  VTY_LOCK() ;

  vio = vty->vio ;

  if (host.pthreaded_config)
    uty_out (vio, "pthreads on\n");

  if (host.name_set)
    uty_out (vio, "hostname %s\n", host.name);

  if (host.password.text != NULL)
    {
      if (host.password.encrypted)
        uty_out (vio, "password 8 %s\n", host.password.text);
      else
        uty_out (vio, "password %s\n", host.password.text);
    } ;

  if (host.enable.text != NULL)
    {
      if (host.enable.encrypted)
        uty_out (vio, "enable password 8 %s\n", host.enable.text);
      else
        uty_out (vio, "enable password %s\n", host.enable.text);
    } ;

  if (zlog_get_default_lvl(NULL) != LOG_DEBUG)
    {
      uty_out (vio, "! N.B. The 'log trap' command is deprecated.\n");
      uty_out (vio, "log trap %s\n", zlog_priority[zlog_get_default_lvl(NULL)]);
    }

  if (host.logfile && (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != ZLOG_DISABLED))
    {
      uty_out (vio, "log file %s", qpath_string(host.logfile));
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_FILE)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != ZLOG_DISABLED)
    {
      uty_out (vio, "log stdout");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) == ZLOG_DISABLED)
    uty_out(vio,"no log monitor%s",VTY_NEWLINE);
  else if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) != zlog_get_default_lvl(NULL))
    uty_out(vio,"log monitor %s\n",
	    zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR)]);

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != ZLOG_DISABLED)
    {
      uty_out (vio, "log syslog");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_facility(NULL) != LOG_DAEMON)
    uty_out (vio, "log facility %s\n", facility_name(zlog_get_facility(NULL)));

  if (zlog_get_record_priority(NULL) == 1)
    uty_out (vio, "log record-priority\n");

  if (zlog_get_timestamp_precision(NULL) > 0)
    uty_out (vio, "log timestamp precision %d\n",
                                            zlog_get_timestamp_precision(NULL));

  if (host.advanced)
    uty_out (vio, "service advanced-vty\n");

  if (host.encrypt)
    uty_out (vio, "service password-encryption\n");

  if (host.lines >= 0)
    uty_out (vio, "service terminal-length %d\n", host.lines);

  if      (host.motdfile)
    uty_out (vio, "banner motd file %s\n", qpath_string(host.motdfile));
  else if (! host.motd)
    uty_out (vio, "no banner motd\n");

  VTY_UNLOCK() ;

  return 1;
}

/*------------------------------------------------------------------------------
 * Write current configuration into file
 *
 * Several aliases:  write file
 *                   write
 *                   write memory
 *                   copy running-config startup-config
 */
DEFUN (config_write_file,
       config_write_file_cmd,
       "write file",
       "Write running configuration to file, memory or terminal\n"
       "Write to configuration file\n")
{
  cmd_ret_t ret ;
  qpath     path ;

  /* If we are in the VTY_VTYSH (the vtysh's own vty), then we need to
   * call its write_integrated_config.
   */
  if (vty->type == VTY_VTYSH)
    {
      if (vtysh_cmd_call_backs != NULL)
        return vtysh_cmd_call_backs->write_integrated_config(vty) ;

      vty_out(vty, "%% %s: no vtysh_cmd_call_backs !\n", __func__) ;
      return CMD_ERROR ;
    } ;

  /* Get the target path, if any
   */
  VTY_LOCK() ;
  path = (host.config_file != NULL) ? qpath_dup(host.config_file) : NULL ;
  VTY_UNLOCK() ;

  /* Check and see if we are operating under vtysh configuration
   */
  if (path == NULL)
    {
      vty_out (vty, "%% Depending on vtysh for configuration, "
                                       "so cannot write own configuration.\n") ;
      return CMD_WARNING;
    } ;

  /* Do the business
   */
  vty->config_to_vtysh = false ;        /* make sure    */
  ret = vty_write_config_file(vty, path, config_write_file_node,
                                                   false /* not integrated */) ;
  qpath_free(path) ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Write configuration (if any) for given node to the vty.
 */
static int
config_write_file_node(vty vty, node_type_t node)
{
  cmd_node cn ;

  cn = cmd_cmd_node(node) ;

  if (cn->config_write == NULL)
    return -1 ;

  if (vty->config_to_vtysh)
    {
      if (cn->config_to_vtysh)
        vty_out (vty, "#vtysh-config-node %s\n", cmd_node_name(node)) ;
      else
        return -1 ;
    } ;

  return cn->config_write(vty) ;
} ;

ALIAS (config_write_file,
       config_write_cmd,
       "write",
       "Write running configuration to file\n")
//     "Write running configuration to file, memory or terminal\n")

ALIAS (config_write_file,
       config_write_memory_cmd,
       "write memory",
       "Write running configuration to file, memory or terminal\n"
       "Write configuration to memory (same as write file)\n")

ALIAS (config_write_file,
       copy_runningconfig_startupconfig_cmd,
       "copy running-config startup-config",
       "Copy configuration\n"
       "Copy running config to... \n"
       "Copy running config to startup config (same as write file)\n")

/* Need to know all the aliases of "write" -- for vtysh !
 */
static const cmd_command config_write_aliases[] =
{
    &config_write_file_cmd,
    &config_write_cmd,
    &config_write_memory_cmd,
    &copy_runningconfig_startupconfig_cmd,
    NULL
} ;

/* Command for vtysh *only*, to write integrated configuration file, without
 * having to set the config state in vtysh !
 */
DEFUN (config_write_integrated,
       config_write_integrated_cmd,
       "write integrated-vtysh-config",
       "Write running configuration\n"
       "Write integrated configuration file\n")
{
  /* Must be in the VTY_VTYSH (the vtysh's own vty) !
   */
  if (vty->type == VTY_VTYSH)
    {
      if (vtysh_cmd_call_backs != NULL)
        return vtysh_cmd_call_backs->write_integrated_config(vty) ;

      vty_out(vty, "%% %s: no vtysh_cmd_call_backs !\n", __func__) ;
      return CMD_ERROR ;
    } ;

  vty_out(vty, "%% write integrated-vtysh-config is only valid in vtysh\n") ;
  return CMD_ERROR ;
} ;

/*------------------------------------------------------------------------------
 * Write current configuration to the vty.
 *
 * Three aliases:  write terminal      ) for vtysh, these are both vtysh *only*
 *                 show running-config ) commands, and will show the integrated
 *                                     ) configuration.
 *
 *           and:  #vtysh-config-write ) this meta command is *only* used by
 *                                     ) the vtysh to collect the configuration
 *                                     ) of real daemons.
 */
static cmd_ret_t
show_configuration(vty vty, bool config_to_vtysh)
{
  cmd_ret_t ret ;

  vty->config_to_vtysh = config_to_vtysh ;

  if (!config_to_vtysh)
    vty_out (vty, "\n"
                  "Current configuration for %s:\n"
                  "!\n", daemon_name) ;
  else
    vty_out (vty, "#daemon %s\n", daemon_name) ;

  ret = vty_write_config(vty, config_write_file_node) ;

  vty->config_to_vtysh = false ;

  return CMD_SUCCESS;
} ;

/*------------------------------------------------------------------------------
 * For the vtysh we can break configuration stuff into sections.
 *
 * See vtysh_config() for discussion of configuration sections.
 */
extern void
cmd_show_config_section(vty vty, uint section)
{
  if (vty->config_to_vtysh)
    vty_out (vty, "#vtysh-config-section %u\n", section) ;
} ;

DEFUN (config_write_terminal,
       config_write_terminal_cmd,
       "write terminal",
       "Write running configuration to file, memory or terminal\n"
       "Write to terminal\n")
{
  /* If we are in the VTY_VTYSH (the vtysh's own vty), then we need to
   * call its write_integrated_config.
   */
  if (vty->type == VTY_VTYSH)
    {
      if (vtysh_cmd_call_backs != NULL)
        return vtysh_cmd_call_backs->show_integrated_config(vty) ;

      vty_out(vty, "%% %s: no vtysh_cmd_call_backs !\n", __func__) ;
      return CMD_ERROR ;
    } ;

  /* In vtysh this command is a vtysh *only* command, so is an error if arrives
   * from vtysh !
   */
  if (vty->type == VTY_VTYSH_SERVER)
    {
      vty_out(vty,
           "%% write terminal/show running-config are not valid from vtysh\n") ;
      return CMD_ERROR ;
    } ;

  /* Otherwise, we are in an ordinary daemon.
   */
  return show_configuration(vty, false) ;
} ;

ALIAS (config_write_terminal,
       show_running_config_cmd,
       "show running-config",
       SHOW_STR
       "running configuration\n")

/* Meta command to send configuration to vtysh.
 */
DEFUN (config_vtysh_config_write,
       config_vtysh_config_write_cmd,
       "vtysh-config-write",
       "Write running configuration to vtysh\n")
{
  if (vty->type == VTY_VTYSH_SERVER)
    return show_configuration(vty, true) ;

  vty_out(vty, "%% #vtysh-config-write is only valid from vtysh\n") ;

  return CMD_WARNING ;
} ;

/*------------------------------------------------------------------------------
 * Write startup configuration into the terminal.
 */
DEFUN (show_startup_config,
       show_startup_config_cmd,
       "show startup-config",
       SHOW_STR
       "Contents of startup configuration\n")
{
  cmd_ret_t ret ;
  qpath     path ;

  VTY_LOCK() ;
  path = (host.config_file != NULL) ? qpath_dup(host.config_file) : NULL ;
  VTY_UNLOCK() ;

  if (path == NULL)
    {
      vty_out (vty, "%% Cannot show configuration file, using vtysh.\n");
      return CMD_WARNING;
    } ;

  ret = vty_cat_file(vty, path, "configuration file") ;
  qpath_free(path) ;

  return ret ;
}

/*------------------------------------------------------------------------------
 * Hostname configuration
 */
DEFUN_CALL (config_hostname,
       hostname_cmd,
       "hostname WORD",
       "Set system's network name\n"
       "This system's network name\n")
{
  if (!isalpha((int) *argv[0]))
    {
      vty_out (vty, "Please specify string starting with alphabet\n");
      return CMD_WARNING;
    }

  return cmd_set_host_name(argv[0]) ;
}

DEFUN_CALL (config_no_hostname,
       no_hostname_cmd,
       "no hostname [HOSTNAME]",
       NO_STR
       "Reset system's network name\n"
       "Host name of this router\n")
{
  return cmd_set_host_name(NULL) ;
}

/*==============================================================================
 * Password setting/clearing/encryption etc.
 */

static void password_new(password_t* password, const char* text,
                                                 bool encrypted, bool encrypt) ;
static void password_free(password_t* password) ;

/*------------------------------------------------------------------------------
 * Password setting function -- common for password and enable password.
 */
static cmd_ret_t
do_set_password(vty vty, password_t* password, int argc, argv_t argv)
{
  cmd_ret_t ret ;

  /* Argument check. */
  if (argc == 0)
    {
      vty_out (vty, "Please specify password.\n");
      return CMD_WARNING;
    }

  VTY_LOCK() ;
  ret = CMD_SUCCESS ;

  if (argc == 2)
    {
      /* Encrypted password argument                            */

      if (*argv[0] == '8')
        {
          password_new(password, argv[1], true /*encrypted */,
                                         false /* encrypt */) ;
        }
      else
        {
          vty_out(vty, "Unknown encryption type.\n");
          ret = CMD_WARNING;
        }
    }
  else
    {
      /* Plaintext password argument
       */
      if (!isalnum ((int) *argv[0]))
        {
          vty_out(vty, "Please specify string starting with alphanumeric\n");
          ret = CMD_WARNING ;
        }
      else
        {
          /* If host.encrypt, only keeps the encrypted password.
           */
          password_new(password, argv[0], false /* encrypted */, host.encrypt) ;
        } ;
    } ;

  VTY_UNLOCK() ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Make a new password string and store it -- freeing any existing.
 *
 * If is not already encrypted may be encrypted.
 *
 * NB: allows for encrypting the existing stored password, by freeing the
 *     current password after creating the new one.
 */
static void
password_new(password_t* password, const char* text, bool encrypted,
                                                     bool encrypt)
{
  qstring cypher = NULL ;
  char*   new_text ;

  if (!encrypted && encrypt)
    {
      cypher    = qcrypt(text, NULL) ;
      text      = qs_string(cypher) ;
      encrypted = true ;
    } ;

  new_text = XSTRDUP(MTYPE_HOST, text) ;

  password_free(password);      /* if any       */

  password->encrypted = encrypted ;
  password->text      = new_text ;

  qs_free(cypher) ;             /* if any       */
} ;

/*------------------------------------------------------------------------------
 * Free an existing password string & set !encrypted
 */
static void
password_free(password_t* password)
{
  if (password->text != NULL)
    XFREE(MTYPE_HOST, password->text) ; /* sets password->text = NULL   */
  password->encrypted = false ;
} ;

/*------------------------------------------------------------------------------
 * See if given candidate matches the given password.
 */
extern bool
cmd_password_check(password_t* password, const char* candidate)
{
  qstring cypher ;
  bool    pass ;

  cypher    = NULL ;

  if (password->encrypted)
    {
      cypher    = qcrypt(candidate, password->text) ;
      candidate = qs_string(cypher) ;
    } ;

  pass = (strcmp(candidate, password->text) == 0) ;

  qs_free(cypher) ;         /* if any       */

  return pass ;
} ;

/*------------------------------------------------------------------------------
 * VTY interface password set
 */
DEFUN_CALL (config_password, password_cmd,
       "password (8) WORD",
       "Assign the terminal connection password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN line password string\n")
{
  return do_set_password(vty, &host.password, argc, argv) ;
} ;

ALIAS_CALL (config_password, password_text_cmd,
       "password LINE",
       "Assign the terminal connection password\n"
       "The UNENCRYPTED (cleartext) line password\n")

/* VTY enable password set. */
DEFUN_CALL (config_enable_password, enable_password_cmd,
       "enable password (8) WORD",
       "Modify enable password parameters\n"
       "Assign the privileged level password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN 'enable' password string\n")
{
  return do_set_password(vty, &host.enable, argc, argv) ;
} ;

ALIAS_CALL (config_enable_password,
       enable_password_text_cmd,
       "enable password LINE",
       "Modify enable password parameters\n"
       "Assign the privileged level password\n"
       "The UNENCRYPTED (cleartext) 'enable' password\n")

/* VTY enable password delete. */
DEFUN_CALL (no_config_enable_password, no_enable_password_cmd,
       "no enable password",
       NO_STR
       "Modify enable password parameters\n"
       "Assign the privileged level password\n")
{
  VTY_LOCK() ;

  password_free(&host.enable) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (service_password_encrypt,
       service_password_encrypt_cmd,
       "service password-encryption",
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  VTY_LOCK() ;

  host.encrypt = true ;

  /* If we have an unencrypted password in hand, convert that now.
   * If not, retain any already encrypted password.
   */
  if (!host.password.encrypted && (host.password.text != NULL))
    password_new(&host.password, host.password.text, false /* encrypted */,
                                                     true  /* encrypt */) ;

  if (!host.enable.encrypted   && (host.enable.text != NULL))
    password_new(&host.enable, host.enable.text, false /* encrypted */,
                                                 true  /* encrypt */) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_password_encrypt,
       no_service_password_encrypt_cmd,
       "no service password-encryption",
       NO_STR
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  VTY_LOCK() ;

  /* Keep any existing passwords, encrypted or not.
   */
  host.encrypt = false ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*==============================================================================
 * MOTD commands and set up.
 *
 * Note that can set a MOTD file that does not exist at the time.  A friendly
 * message warns about this, but it is not an error.  The message will not be
 * seen while reading the configuration file -- but it is not worth stopping
 * the configuration file reader for this !
 */
DEFUN_CALL (banner_motd_file,
       banner_motd_file_cmd,
       "banner motd file FILE",
       "Set banner\n"
       "Banner for motd\n"
       "Banner from a file\n"
       "Filename\n")
{
  int    err ;

  VTY_LOCK() ;

  host.motdfile = uty_cmd_path_name_complete(host.motdfile,
                                                  argv[0], vty->exec->context) ;
  err  = qpath_stat_is_file(host.motdfile) ;

  if (err != 0)
    {
      vty_out(vty, "NB: '%s': ", qpath_string(host.motdfile)) ;
      if (err < 0)
        vty_out(vty, "is not a file\n") ;
      else
        vty_out(vty, "%s (%s)\n", errtostr(err, 0).str,
                                  errtoname(err, 0).str) ;
    } ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

DEFUN_CALL (banner_motd_default,
       banner_motd_default_cmd,
       "banner motd default",
       "Set banner string\n"
       "Strings for motd\n"
       "Default string\n")
{
  VTY_LOCK() ;

  host.motd = default_motd ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_banner_motd,
       no_banner_motd_cmd,
       "no banner motd",
       NO_STR
       "Set banner string\n"
       "Strings for motd\n")
{
  VTY_LOCK() ;

  host.motd     = NULL ;
  host.motdfile = qpath_free(host.motdfile) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*------------------------------------------------------------------------------
 * Set current directory
 */
DEFUN_CALL (do_chdir,
       chdir_cmd,
       "chdir DIR",
       "Set current directory\n"
       "Directory to set\n")
{
  cmd_ret_t ret ;
  qpath     path ;
  int       err ;

  ret = CMD_SUCCESS ;

  VTY_LOCK() ;
  path = uty_cmd_path_name_complete(NULL, argv[0], vty->exec->context) ;
  VTY_UNLOCK() ;

  err  = qpath_stat_is_directory(path) ;

  if (err == 0)
    qpath_copy(vty->exec->context->dir_cd, path) ;
  else
    {
      vty_out(vty, "%% chdir %s: ", qpath_string(path)) ;
      if (err < 0)
        vty_out(vty, "is not a directory\n") ;
      else
        vty_out(vty, "%s (%s)\n", errtostr(err, 0).str,
                                  errtoname(err, 0).str) ;
      ret = CMD_WARNING ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Show given directory path
 */
DEFUN_CALL (do_show_dir,
       show_dir_cmd,
       "show directory DIR",
       SHOW_STR
       "Show directory\n"
       "Directory to show\n")
{
  cmd_ret_t ret ;
  qpath     path ;
  int       err ;

  ret = CMD_SUCCESS ;

  VTY_LOCK() ;
  path = uty_cmd_path_name_complete(NULL, argv[0], vty->exec->context) ;
  VTY_UNLOCK() ;

  err  = qpath_stat_is_directory(path) ;

  if (err == 0)
    vty_out(vty, "%s\n", qpath_string(path)) ;
  else
    {
      vty_out(vty, "%% '%s' ", qpath_string(path)) ;
      if (err < 0)
        vty_out(vty, "is not a directory\n") ;
      else
        vty_out(vty, "cannot be found: %s\n", errtoa(err, 0).str) ;
      ret = CMD_WARNING ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Set the lexical level for further command processing.
 *
 * Note that for vtysh this is a vtysh only command -- all vytsh servers run
 * at lexical-level 0, and are fed commands with pipes etc stripped.
 */
DEFUN_ATTR (lexical_level,
       lexical_level_cmd,
       "lexical-level <0-1>",
       "Set lexical level\n"
       "The required lexical level\n",
       CMD_ATTR_DIRECT | CMD_ATTR_FIRST)
{
  int level ;

  level = strtol(argv[0], NULL, 0) ;

  vty->exec->context->full_lex = (level != 0) ;

  return CMD_SUCCESS;
}

/*==============================================================================
 * Turning on pthreading
 */
static cmd_ret_t cmd_set_pthreaded(vty vty, bool pthreaded) ;

/*------------------------------------------------------------------------------
 * "threaded" command -> "pthreads on"
 *
 * "pthreads on/off"  -- if state not changing, do nothing
 *
 *                       if daemon does not support pthreads, complain if
 *                       trying to set pthreads on.
 *
 *                       if pthreading not decided yet, set as required.
 *                       Note that "-t" overrides, silently.
 *
 *                       Otherwise, we cannot actually change the state of
 *                       pthreading, but we change what any future saved
 *                       configuration will say.  Issue message to say what
 *                       is going on.  NB: this is an ordinary output message,
 *                       so will by default be suppressed on configuration
 *                       reading.
 *
 * "no pthreads"  -> "pthreads off"
 *
 * "show pthreads"    -- show current actual state.
 *
 */
DEFUN_ATTR (threaded,
       threaded_cmd,
       "threaded",
       "Use pthreads\n",
       (CMD_ATTR_DIRECT | CMD_ATTR_FIRST))
{
  if (!qpthreads_decided() && host.pthreads_allowed)
    {
      host.pthreaded_config = true ;
      return CMD_SUCCESS ;
    } ;

  return cmd_set_pthreaded(vty, true) ;
} ;

DEFUN_ATTR (pthreads_on_off,
       pthreads_on_off_cmd,
       "pthreads (on|off)",
       "Set pthreads state\n",
       (CMD_ATTR_DIRECT | CMD_ATTR_FIRST))
{
  return cmd_set_pthreaded(vty, strcmp(argv[0], "on") == 0) ;
}

DEFUN_ATTR (no_pthreads,
       no_pthreads_cmd,
       "no pthreads",
       NO_STR
       "Use pthreads\n",
       (CMD_ATTR_DIRECT | CMD_ATTR_FIRST))
{
  return cmd_set_pthreaded(vty, false) ;
} ;

DEFUN_ATTR (show_pthreads,
       show_pthreads_cmd,
       "show pthreads",
       SHOW_STR
       "pthreads state\n",
       CMD_ATTR_DIRECT)
{
  vty_out(vty, "pthreads are %s", qpthreads_enabled ? "on" : "off") ;

  if (qpthreads_enabled != host.pthreaded_config)
    vty_out(vty, "; if configuration is saved, pthreads will be %s",
                                         host.pthreaded_config ? "on" : "off") ;
  vty_out(vty, "\n") ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * If we can, set pthreaded to the given value.
 */
static cmd_ret_t
cmd_set_pthreaded(vty vty, bool pthreaded)
{
  if (pthreaded == host.pthreaded_config)
    return CMD_SUCCESS ;                /* easy if no change            */

  if (!host.pthreads_allowed)
    {
      qassert(pthreaded) ;              /* must be trying to set !      */

      vty_out(vty, "%% pthreads are not supported by %s\n", daemon_name) ;
      return CMD_WARNING ;
    } ;

  /* We now know we are changing the value, and we are allowed to do so.
   */
  host.pthreaded_config = pthreaded ;   /* set new required value       */

  if (qpthreads_decided())
    {
      /* It is too late to make a change to the current daemon, but if
       * configuration is saved, will affect next incarnation.
       */
      vty_out(vty, "pthreads are %s: cannot change that now, but if "
                                                    "configuration is saved,\n"
                   "pthreads will be %s\n",
                                             qpthreads_enabled ? "on" : "off",
                                         host.pthreaded_config ? "on" : "off") ;
    } ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * Command handling initialisation and termination.
 *
 * This sets up the command handling, and installs the basic nodes and all the
 * commands for configuration and log handling etc.  Also installs the VTY_NODE,
 * and the command for the VTY_TERMINAL.
 *
 *------------------------------------------------------------------------------
 * For vtysh there are a few very special commands, which are linked back
 * to the vtysh.
 *
 *   1) "terminal length 99" and "terminal no length"
 *
 *      Installed to execute in the vtysh, only.  They all will call the pager
 *      length setting function in the vtysh.
 *
 *   2) "show history"
 *
 *      Installed to execute in the vtysh, only.  Will call the history
 *      output function in the vtysh.
 *
 *   3) "write vtysh"
 *
 *      Installed to execute in client daemons, only, as a *hidden* command
 *      which will execute in VTY_VTYSH_SERVER *only*.
 *
 *      Does a "write terminal" back to the vtysh, adding "!!NODE=name(99)!!"
 *      between nodes.
 *
 *   4) "write terminal" and "show running-config"
 *
 *      Installed to execute in the vtysh, only.  They will call the vtysh's
 *      integrated configuration gatherer and display result.
 *
 *   5) "write file", "write memory", "write" and
 *      "copy running-config startup-config"
 *
 *      By default, installed to run in each daemon, only.  So, by default each
 *      client will write its own configuration file.
 *
 *      Can be set -- see cmd_set_vtysh_integrated() -- to run in the VTYSH_VD
 *      only... in which case will call the vtysh's integrated configuration
 *      gatherer and write result to the vtysh integrated configuration file.
 *
 *   6) "write integrated-vtysh-config"
 *
 *      Installed for VTYSH_VD only.  Will call the vtysh's integrated
 *      configuration gatherer and write result to the vtysh integrated
 *      configuration file.
 *
 * The vtysh_cmd_call_backs structure (see vty_vtysh.h) contains all the
 * necessary call backs, and that is initialised when the vtysh's own vty is
 * initialised.
 *
 * The function cmd_set_integrated_vtysh_config() changes the state of the
 * "write file" command and all its aliases, so that it is executed only
 * in the vtysh, when the vtysh's "service integrated-vtysh-config" is set.
 */

/*------------------------------------------------------------------------------
 * These are the commands available in all daemons
 *
 * Note that only a few of these will install for BASIC_VD.
 */
CMD_INSTALL_TABLE(static, command_all_cmd_table, ALL_RDS | VTYSH_VD) =
{
  /* Basic commands for changing between the basic nodes.
   *
   * Note that the end/exit/quit commands are installed when the node is
   * installed.
   *
   * Note that RESTRICTED_NODE commands are implicitly ignored when
   * installing for VTYSH_VD.
   */
  { RESTRICTED_NODE, &lexical_level_cmd                , TERM           },
  { RESTRICTED_NODE, &config_enable_cmd                                 },
  { RESTRICTED_NODE, &config_enable_configure_cmd                       },
  { RESTRICTED_NODE, &config_terminal_cmd                               },

  { VIEW_NODE,       &lexical_level_cmd                , TERM           },
  { VIEW_NODE,       &config_enable_cmd                                 },
  { VIEW_NODE,       &config_enable_configure_cmd                       },
  { VIEW_NODE,       &config_terminal_cmd                               },

  { ENABLE_NODE,     &lexical_level_cmd                , TERM           },
  { ENABLE_NODE,     &config_enable_cmd                , BASIC_VD       },
  { ENABLE_NODE,     &config_enable_configure_cmd      , BASIC_VD       },
  { ENABLE_NODE,     &config_terminal_cmd              , BASIC_VD       },
  { ENABLE_NODE,     &config_disable_cmd                                },

  /* Other commands which are executed in both the vtysh and all connected
   * daemons, or only in the vtysh daemon if marked "TERM"
   */
  { RESTRICTED_NODE, &show_version_cmd                                  },
  { RESTRICTED_NODE, &echo_cmd                         , TERM           },
  { RESTRICTED_NODE, &chdir_cmd                        , TERM           },
  { RESTRICTED_NODE, &show_dir_cmd                     , TERM           },

  { VIEW_NODE,       &show_version_cmd                                  },
  { VIEW_NODE,       &echo_cmd                         , TERM           },
  { VIEW_NODE,       &chdir_cmd                        , TERM           },
  { VIEW_NODE,       &show_dir_cmd                     , TERM           },

  { ENABLE_NODE,     &show_version_cmd                                  },
  { ENABLE_NODE,     &echo_cmd                         , TERM           },
  { ENABLE_NODE,     &chdir_cmd                        , TERM           },
  { ENABLE_NODE,     &show_dir_cmd                     , TERM           },

  { ENABLE_NODE,     &config_write_terminal_cmd        , TERM           },
  { ENABLE_NODE,     &show_running_config_cmd          , TERM           },
  { ENABLE_NODE,     &show_startup_config_cmd          , TERM           },

  { ENABLE_NODE,     &config_write_integrated_cmd , VTYSH_VD , ~ALL_RDS },

  { CONFIG_NODE,     &hostname_cmd                                      },
  { CONFIG_NODE,     &no_hostname_cmd                                   },

  /* The meta commands
   */
  { META_NODE,       &lexical_level_cmd                , TERM           },

  { META_NODE,       &set_daemons_cmd                  , TERM           },
  { META_NODE,       &set_daemon_cmd                   , TERM           },

  { META_NODE,       &set_vtysh_config_node_cmd        , TERM           },
  { META_NODE,       &set_vtysh_config_section_cmd     , TERM           },

  CMD_INSTALL_END
} ;

/*------------------------------------------------------------------------------
 * Commands which are required for all real daemons -- so not VTYSH_VD
 * or BASIC_VD.
 *
 * When installing for vtysh these commands are installed, but for execution
 * in connected daemons, only -- these are mostly configuration commands.
 */
CMD_INSTALL_TABLE(static, command_real_cmd_table, ALL_RDS) =
{
  { RESTRICTED_NODE, &show_pthreads_cmd                                 },
  { VIEW_NODE,       &show_pthreads_cmd                                 },

  { ENABLE_NODE,     &show_logging_cmd                                  },
  { ENABLE_NODE,     &config_logmsg_cmd                                 },

  { ENABLE_NODE,     &show_pthreads_cmd                                 },
  { ENABLE_NODE,     &config_write_file_cmd                             },
  { ENABLE_NODE,     &config_write_cmd                                  },
  { ENABLE_NODE,     &config_write_memory_cmd                           },
  { ENABLE_NODE,     &copy_runningconfig_startupconfig_cmd              },

  { META_NODE,       &config_vtysh_config_write_cmd                     },

  { CONFIG_NODE,     &threaded_cmd                                      },
  { CONFIG_NODE,     &pthreads_on_off_cmd                               },
  { CONFIG_NODE,     &no_pthreads_cmd                                   },

  { CONFIG_NODE,     &password_cmd                                      },
  { CONFIG_NODE,     &password_text_cmd                                 },
  { CONFIG_NODE,     &enable_password_cmd                               },
  { CONFIG_NODE,     &enable_password_text_cmd                          },
  { CONFIG_NODE,     &no_enable_password_cmd                            },

  { CONFIG_NODE,     &service_password_encrypt_cmd                      },
  { CONFIG_NODE,     &no_service_password_encrypt_cmd                   },

  { CONFIG_NODE,     &config_log_stdout_cmd                             },
  { CONFIG_NODE,     &config_log_stdout_level_cmd                       },
  { CONFIG_NODE,     &no_config_log_stdout_cmd                          },
  { CONFIG_NODE,     &config_log_monitor_cmd                            },
  { CONFIG_NODE,     &config_log_monitor_level_cmd                      },
  { CONFIG_NODE,     &no_config_log_monitor_cmd                         },
  { CONFIG_NODE,     &config_log_file_cmd                               },
  { CONFIG_NODE,     &config_log_file_level_cmd                         },
  { CONFIG_NODE,     &no_config_log_file_cmd                            },
  { CONFIG_NODE,     &no_config_log_file_level_cmd                      },
  { CONFIG_NODE,     &config_log_syslog_cmd                             },
  { CONFIG_NODE,     &config_log_syslog_level_cmd                       },
  { CONFIG_NODE,     &config_log_syslog_facility_cmd                    },
  { CONFIG_NODE,     &no_config_log_syslog_cmd                          },
  { CONFIG_NODE,     &no_config_log_syslog_facility_cmd                 },
  { CONFIG_NODE,     &config_log_facility_cmd                           },
  { CONFIG_NODE,     &no_config_log_facility_cmd                        },
  { CONFIG_NODE,     &config_log_trap_cmd                               },
  { CONFIG_NODE,     &no_config_log_trap_cmd                            },
  { CONFIG_NODE,     &config_log_record_priority_cmd                    },
  { CONFIG_NODE,     &no_config_log_record_priority_cmd                 },
  { CONFIG_NODE,     &config_log_timestamp_precision_cmd                },
  { CONFIG_NODE,     &no_config_log_timestamp_precision_cmd             },

  { CONFIG_NODE,     &banner_motd_default_cmd                           },
  { CONFIG_NODE,     &banner_motd_file_cmd                              },
  { CONFIG_NODE,     &no_banner_motd_cmd                                },

  CMD_INSTALL_END
} ;

/*------------------------------------------------------------------------------
 * Initialise command handling
 *
 * Must be done before installing any nodes or commands (!) and certainly
 * before doing anything that might execute commands (!).
 *
 * The daemon value given is the actual daemon for which we are initialising.
 * For an ordinary daemon that's itself, but for vtysh is VTYSH_VD | ALL_RDS.
 *
 * Installs basic nodes and commands as required -- including the VTY_NODE and
 * vty commands (as required).
 *
 * Once all other nodes and commands have been installed, then *must* call
 * cmd_table_complete().
 *
 * The basic nodes are:
 *
 *   VIEW_NODE        -- installed only in real daemons and the VTYSH_VD, so
 *                       not BASIC_VD.
 *
 *   RESTRICTED_NODE  -- installed only in real daemons, so not BASIC_VD or
 *                       VTYSH_VD.
 *
 *   AUTH_NODE        -- special node used only for input of password to enter
 *                       VIEW_NODE -- no actual commands are associated with
 *                       this node.
 *
 *                       This node is installed only in real daemons, so not
 *                       BASIC_VD or VTYSH_VD.
 *
 *   AUTH_ENABLE_NODE -- as AUTH_NODE, but for entry to ENABLE_NODE
 *
 *   ENABLE_NODE      -- available in all daemons including BASIC_VD.
 *
 *   CONFIG_NODE      -- available in all daemons including BASIC_VD.
 */
extern void
cmd_table_init (daemon_set_t daemons)
{
  cmd_parser_init() ;

  /* Globals telling us who we are
   */
  daemons_set  = daemons ;

  if (daemons & VTYSH_VD)
    daemon_name = cmd_daemon_name(VTYSH_VD, false) ;
  else
    {
      if (daemons != cmd_daemon_bit(cmd_daemon_ord(daemons)))
        {
          fprintf(stderr, "%s(): invalid daemons 0x%X", __func__, daemons) ;
          exit(1) ;
        } ;

      daemon_name = cmd_daemon_name(daemons, false /* don't forge */) ;

      if (daemon_name == NULL)
        {
          fprintf(stderr, "%s(): %s daemon", __func__,
                                  cmd_daemon_name(daemons, true /* forge */) ) ;
          exit(1) ;
        } ;
    } ;

  /* Scan the node table and check/complete the
   */
  cmd_node_table_init() ;

  /* Install commands for the basic nodes
   */
  cmd_install_table(command_all_cmd_table) ;
  cmd_install_table(command_real_cmd_table) ;

  cmd_install_table(thread_cmd_table) ;
  cmd_install_table(memory_cmd_table) ;
  cmd_install_table(workqueue_cmd_table) ;

  cmd_install_node_config_write(CONFIG_NODE, config_write_host);

  /* Install the vty commands, too.
   */
  vty_cmd_init() ;
} ;

/*------------------------------------------------------------------------------
 * Set integrated-vtysh-config on/off
 *
 * This *must* only be used in the vtysh !
 *
 * Setting integrated-vtysh-config on:
 *
 *   * changes the "write file" command and all its aliases from a daemon
 *     command to a VTYSH_VD *only* command.  The vtysh command then writes its
 *     integrated file.
 *
 *   * sets host.config_file to host.int_config_file
 *
 *   * sets host,config_dir to suit
 *
 * Setting integrated-vtysh-config off:
 *
 *   * restores the commands to being daemon commands, each writing the
 *     daemon's own configuration file.
 *
 *   * sets host.config_file to host.own_config_file
 *
 *   * sets host.config_dir to suit
 */
extern void
cmd_set_integrated_vtysh_config(on_off_b integrated)
{
  const cmd_command* p_alias ;

  VTY_LOCK() ;

  p_alias = config_write_aliases ;
  while (1)
    {
      cmd_command  cmd ;

      cmd = *p_alias++ ;
      if (cmd == NULL)
        break ;

      if (integrated)
        cmd->daemons |= TERM | VTYSH_VD ;
      else
        cmd->daemons &= ~ (TERM | VTYSH_VD) ;
    } ;

  if (integrated)
    host.config_file = host.int_config_file ;
  else
    host.config_file = host.own_config_file ;

  cmd_config_dir_set (host.config_file) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * The construction of the node/command table is now complete.
 *
 * Sort each node's command elements according to command string, which keeps
 * things tidy from a help point of view.
 */
extern void
cmd_table_complete(void)
{
  unsigned int i ;

  qassert(!node_table_complete) ;
  qassert(!qpthreads_enabled) ;

  for (i = MIN_NODE; i <= MAX_NODE; i++)
    vector_sort(node_table[i].cmd_vector, (vector_sort_cmp*)cmp_node) ;
                                  /* does nothing if cmd_vector is NULL */

  node_table_complete = true ;
} ;

/*------------------------------------------------------------------------------
 * Close down command interface.
 *
 * Dismantle the node_vector and all commands.
 */
extern void
cmd_table_terminate (void)
{
  uint i ;

  /* Ream out the vector of command nodes.                              */
  for (i = MIN_NODE ; i <= MAX_NODE ; ++i)
    {
      cmd_node     cmd_node;
      cmd_command  cmd ;

      cmd_node = &node_table[i] ;

      /* Ream out the (embedded) vector of commands per node.           */
      while ((cmd = vector_ream(cmd_node->cmd_vector, free_it)) != NULL)
        {
          /* Ream out the vector of items for each command.
           *
           * Note that each cmd is a static structure, which may appear in
           * more than one cmd_vector -- but the "compiled" portions are
           * dynamically allocated.
           */
          cmd_item next_item ;

          while ((next_item = vector_ream(cmd->items, free_it)) != NULL)
            {
              do
                {
                  cmd_item item ;
                  item      = next_item ;
                  next_item = item->next ;
                  XFREE(MTYPE_CMD_ITEM, item);
                }
              while (next_item != NULL) ;
            } ;

          cmd->items = NULL ;                           /* gone         */

          XFREE (MTYPE_CMD_STRING, cmd->r_doc) ;
        } ;

      cmd_node->cmd_vector = NULL ;
    } ;
} ;

/*==============================================================================
 * The initialisation and shut down of the host data structure.
 */
static char full_progname[200] ;
static char date_reformed[20] ;

/*------------------------------------------------------------------------------
 * Complete the initialisation of the host structure.
 *
 * This should done very early in the morning, before lowering privileges, to
 * minimise chance of not being able to get the cwd.  If cwd itself is not
 * accessible in lowered privilege state, that will later become clear.
 *
 * Expected to be done shortly after qlib_init_first_stage(), but in any case
 * *before* any pthreads start running.  Access to the host structure requires
 * the VTY_LOCK() for those elements which may be changed by commands et al.
 *
 * The host structure is static, and is mostly initialised statically.
 *
 * NB: sets srand(time(NULL)).
 *
 * NB: to be called once only
 */
extern void
host_init(const char* arg0)
{
  uint  cl ;
  char* p, * e ;
  const char* s, * y ;

  qassert(!qpthreads_enabled) ;

  /* Is new born -- nothing configured, yet
   */
  host.newborn = true ;

  /* Get the current working directory -- must succeed !
   */
  host.cwd = qpath_getcwd(NULL) ;

  if (host.cwd == NULL)
    {
      fprintf(stderr, "Cannot getcwd(): %s\n", errtoa(errno, 0).str) ;
      exit(1) ;
    } ;

  /* Set program name and full program name.
   *
   * We make a copy of the full program name, but to a static, so that it
   * does not need to be released, and will not appear in any memory statistics,
   * particularly at shut down.  The program name is used by memory_finish(),
   * which is the very last thing we do, so there is no opportunity to release
   * memory !
   */
  cl = strlen(arg0) + 1 ;               /* amount to copy       */
  p  = full_progname ;
  if (cl > sizeof(full_progname))
    {
      int i ;

      if (*arg0 == '/')
        *p++ = *arg0 ;                  /* copy leading '/'     */

      for (i = 0 ; i < 3 ; i++)
        *p++ = '.' ;                    /* insert "..."         */

      arg0 += cl ;
      cl = (full_progname + sizeof(full_progname)) - p ;
      arg0 -= cl ;
    } ;
  memcpy(p, arg0, cl) ;

  host.full_progname = full_progname ;
  host.progname = ((p = strrchr (full_progname, '/')) ? ++p : full_progname) ;

  /* Pointer to the default motd string cannot be set statically.
   */
  host.motd         = default_motd ;

  /* Start the config symbol of power branding in a random place.
   */
  srand (time (NULL));
  host.config_brand = rand() ;

  /* Start the host name generation at a random value, and then get the
   * system's view of the host name, which will update the generation, and
   * make sure it is not zero.
   */
  host.name_gen = rand() ;
  cmd_get_sys_host_name() ;

  /* Reform the date from the standard "Mmm dd yyyy" to "dd-Mmm-yyyy"
   */
  p = date_reformed ;
  e = date_reformed + sizeof(date_reformed) - 1 ;
  s = host.date ;

  while ((*s != ' ') && (*s != '\0'))
    ++s ;
  while (*s == ' ')
    ++s ;               /* advance to dd        */
  while ((*s != ' ') && (*s != '\0') && (p < e))
    *p++ = *s++ ;
  if (p < e)
    *p++ = '-' ;
  while (*s == ' ')
    ++s ;               /* advance to yyyy      */
  y = s ;
  while ((*s != ' ') && (*s != '\0') && (p < e))
    *p++ = *s++ ;
  if (p < e)
    *p++ = '-' ;
  while ((*y != '\0') && (p < e))
    *p++ = *y++ ;

  *p++ = '\0' ;

  host.date = date_reformed ;
} ;

/*------------------------------------------------------------------------------
 * If is newborn, now is the time to run host.init_second_stage, if any.
 *
 * In any case: clears host.newborn and host.first_config_cmd
 */
extern void
cmd_init_second_stage(void)
{
  if ((host.newborn) && (host.init_second_stage != NULL))
    host.init_second_stage(host.pthreaded_config || host.pthreaded_option) ;

  host.newborn = host.first_config_cmd = false ;
} ;

/*------------------------------------------------------------------------------
 * Shut down the host structure, which frees off paths and other names which
 * have been allocated dynamically.
 *
 * This must be done *after* any pthreads stop running.
 */
extern void
host_finish(void)
{
  qassert(!qpthreads_active) ;

  XFREE(MTYPE_HOST, host.name);
  password_free(&host.password);
  password_free(&host.enable);
  host.logfile         = qpath_free(host.logfile) ;
  host.motdfile        = qpath_free(host.motdfile) ;
  host.own_config_file = qpath_free(host.own_config_file) ;
  host.int_config_file = qpath_free(host.int_config_file) ;
  host.config_dir      = qpath_free(host.config_dir) ;
  XFREE(MTYPE_HOST, host.vty_accesslist_name);
  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
  XFREE(MTYPE_HOST, host.vty_listen_addr) ;
  XFREE(MTYPE_HOST, host.vtysh_listen_path) ;

  host.cwd             = qpath_free(host.cwd) ;
} ;

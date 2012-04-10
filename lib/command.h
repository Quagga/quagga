/*
 * Zebra configuration command interface routine
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _ZEBRA_COMMAND_H
#define _ZEBRA_COMMAND_H

#include "misc.h"

#include "command_common.h"
#include "vty.h"

#include "vector.h"
#include "qstring.h"

/*==============================================================================
 * This contains everything required for command handling from outside the
 * library.
 */

/*------------------------------------------------------------------------------
 * Macros for the construction of commands.
 */

/* Legacy name for cmd_command
 */
#define cmd_element cmd_command

/* Generic struct cmd_command
 *
 * Must have one or both of .attr= and .daemon= arguments.
 */
#define CMD_COMMAND_STRUCT(_func, _name, _cstr, _hstr, ...) \
static cmd_command_t _name = \
  { \
    .string = _cstr,    \
    .func   = _func,    \
    .doc    = _hstr,    \
    __VA_ARGS__,        \
    \
    .items    = NULL,   \
    .nt_min   = 0,      \
    .nt       = 0,      \
    .nt_max   = 0,      \
    .vararg   = NULL,   \
    .r_doc    = NULL,   \
  } ;

/* Generation of the DEFUN_CMD_STRUCT has one level of indirection, to allow
 * for extracting of commands for vtysh.
 *
 * The extraction process scans for VTYSH_CMD_INSTALL to do the business.
 */
#ifndef VTYSH_EXTRACT_PL
#define CMD_INSTALL(...) CMD_COMMAND_STRUCT(__VA_ARGS__)
#else
#undef  VTYSH_CMD_INSTALL
#define CMD_INSTALL(...) VTYSH_CMD_INSTALL{->>__VA_ARGS__<<-};
#endif

/* Generic command -- must have one or both of .attr= and .daemon=
 */
#define DEFUN_CMD(_func, _name, _cstr, _hstr, ...) \
  static cmd_function _func; \
  CMD_INSTALL(_func, _name, _cstr, _hstr, __VA_ARGS__) \
  static DEFUN_CMD_FUNCTION(_func)

/* Generic alias -- must have one or both of .attr= and .daemon=
 */
#define ALIAS_CMD(_func, _name, _cstr, _hstr, ...) \
  CMD_INSTALL(_func, _name, _cstr, _hstr, __VA_ARGS__)

/* DEFUN for vty command interface. Little bit hacky ;-).               */
#define DEFUN(_func, _name, _cmdstr, _helpstr) \
  DEFUN_CMD(_func, _name, _cmdstr, _helpstr, .attr=0)

#define DEFUN_ATTR(_func, _name, _cstr, _hstr, _attr) \
  DEFUN_CMD(_func, _name, _cstr, _hstr, .attr=_attr)

#define DEFUN_CALL(_func, _name, _cstr, _hstr) \
  DEFUN_ATTR(_func, _name, _cstr, _hstr, CMD_ATTR_DIRECT)

#define DEFUN_HIDDEN(_func, _name, _cstr, _hstr) \
  DEFUN_ATTR(_func, _name, _cstr, _hstr, CMD_ATTR_HIDDEN)

#define DEFUN_HID_CALL(_func, _name, _cstr, _hstr) \
  DEFUN_ATTR(_func, _name, _cstr, _hstr, (CMD_ATTR_DIRECT | CMD_ATTR_HIDDEN))

#define DEFUN_DEPRECATED(_func, _name, _cstr, _hstr) \
  DEFUN_ATTR (_func, _name, _cstr, _hstr, CMD_ATTR_DEPRECATED)

#define DEFUN_DEP_CALL(_func, _name, _cstr, _hstr) \
  DEFUN_ATTR (_func, _name, _cstr, _hstr, \
                                        (CMD_ATTR_DIRECT | CMD_ATTR_DEPRECATED))

/* DEFUN_NOSH for commands that vtysh should ignore                     */
#define DEFUN_NOSH(_func, _name, _cstr, _hstr) \
  DEFUN(_func, _name, _cstr, _hstr)

/* DEFSH for vtysh.                                                     */
#define DEFSH(_daem, _name, _cstr, _hstr, ...) \
  CMD_INSTALL(NULL, _name, _cstr, _hstr, .daemon=_daem, __VA_ARGS__)

/* DEFUN + DEFSH                                                        */
#define DEFUNSH(_daem, _func, _name, _cstr, _hstr) \
  DEFUN_CMD(_func, _name, _cstr, _hstr, .attr=0, .daemon=_daem)

/* DEFUN + DEFSH with attributes                                        */
#define DEFUNSH_ATTR(_daem, _func, _name, _cstr, _hstr, _attr) \
  DEFUN_CMD(_func, _name, _cstr, _hstr, .attr=_attr, .daemon=_daem) \

#define DEFUNSH_HIDDEN(_daem, _func, _name, _cstr, _hstr) \
  DEFUNSH_ATTR (_daem, _func, _name, _cstr, _hstr, CMD_ATTR_HIDDEN)

#define DEFUNSH_DEPRECATED(_daem, _func, _name, _cstr, _hstr) \
  DEFUNSH_ATTR (_daem, _func, _name, _cstr, _hstr, CMD_ATTR_DEPRECATED)

/* ALIAS macro which define existing command's alias.                   */
#define ALIAS(_func, _name, _cstr, _hstr) \
  ALIAS_CMD(_func, _name, _cstr, _hstr, .attr=0)

#define ALIAS_ATTR(_func, _name, _cstr, _hstr, _attr) \
  ALIAS_CMD(_func, _name, _cstr, _hstr, .attr=_attr)

#define ALIAS_CALL(_func, _name, _cstr, _hstr) \
  ALIAS_ATTR(_func, _name, _cstr, _hstr, CMD_ATTR_DIRECT)

#define ALIAS_HIDDEN(_func, _name, _cstr, _hstr) \
  ALIAS_ATTR(_func, _name, _cstr, _hstr, CMD_ATTR_HIDDEN)

#define ALIAS_DEPRECATED(_func, _name, _cstr, _hstr) \
  ALIAS_ATTR(_func, _name, _cstr, _hstr, CMD_ATTR_DEPRECATED)

#define ALIAS_SH(_daem, _func, _name, _cstr, _hstr) \
  ALIAS_CMD(_func, _name, _cstr, _hstr, .attr=0, .daemon=_daem)

#define ALIAS_SH_ATTR(_daem, _func, _name, _cstr, _hstr, _attr) \
  ALIAS_CMD(_func, _name, _cstr, _hstr, .attr=_attr, .daemon=_daem)

#define ALIAS_SH_HIDDEN(_daem, _func, _name, _cstr, _hstr) \
  ALIAS_SH_ATTR(_daem, _func, _name, _cstr, _hstr, CMD_ATTR_HIDDEN)

#define ALIAS_SH_DEPRECATED(_daem, _func, _name, _cstr, _hstr) \
  ALIAS_SH_ATTR(_daem, _func, _name, _cstr, _hstr, CMD_ATTR_DEPRECATED)


#define DEFVTYSH(...) CMD_COMMAND_STRUCT(NULL, __VA_ARGS__)


/*------------------------------------------------------------------------------
 * Macros for the creation of tables of commands to install
 */
#define CMDextern
#define CMDstatic static

#define CMD_INSTALL_TABLE_STRUCT(_sc, _name, _daem, ...) \
  static   cmd_table_body _name##_body ;\
  CMD##_sc cmd_table _name = {{ _name##_body, _daem, __VA_ARGS__ }} ;\
  static   cmd_table_body _name##_body

/* Generation of the DEFUN_CMD_STRUCT has one level of indirection, to allow
 * for extracting of commands for vtysh.
 *
 * The extraction process scans for VTYSH_CMD_INSTALL to do the business.
 *
 * The arguments are:  1: static or extern, for the definition of the table
 *                     2: the name of the table
 *                     3: the daemon set for the table
 *                     4: *optional* -- the del_daemon set for the table
 *
 * NB: the extract.pl assumes that the arguments can be identified by simple
 *     split(/,/) -- so please avoid the ',' operator !!
 *
 *     the extract.pl works on the output from cpp, so comments are not an
 *     issue -- feel free to add as much comment as you wish !
 */
#ifndef VTYSH_EXTRACT_PL
#define CMD_INSTALL_TABLE(...) CMD_INSTALL_TABLE_STRUCT(__VA_ARGS__)
#else
#undef  VTYSH_CMD_INSTALL
#define CMD_INSTALL_TABLE(...) VTYSH_CMD_INSTALL_TABLE{->>__VA_ARGS__
#endif

/* The end of table marker expands differently to help VTYSH_EXTRACT_PL
 */
#ifndef VTYSH_EXTRACT_PL
#define CMD_INSTALL_END { NULL_NODE, NULL }
#else
#undef  VTYSH_CMD_INSTALL
#define CMD_INSTALL_END <<-
#endif

/*------------------------------------------------------------------------------
 * Common descriptions.
 */
#define SHOW_STR "Show running system information\n"
#define IP_STR "IP information\n"
#define IPV6_STR "IPv6 information\n"
#define NO_STR "Negate a command or set its defaults\n"
#define REDIST_STR "Redistribute information from another routing protocol\n"
#define CLEAR_STR "Reset functions\n"
#define RIP_STR "RIP information\n"
#define BGP_STR "BGP information\n"
#define OSPF_STR "OSPF information\n"
#define NEIGHBOR_STR "Specify neighbor router\n"
#define DEBUG_STR "Debugging functions (see also 'undebug')\n"
#define UNDEBUG_STR "Disable debugging functions (see also 'debug')\n"
#define ROUTER_STR "Enable a routing process\n"
#define AS_STR "AS number\n"
#define MBGP_STR "MBGP information\n"
#define MATCH_STR "Match values from routing table\n"
#define SET_STR "Set values in destination routing protocol\n"
#define OUT_STR "Filter outgoing routing updates\n"
#define IN_STR  "Filter incoming routing updates\n"
#define V4NOTATION_STR "specify by IPv4 address notation(e.g. 0.0.0.0)\n"
#define OSPF6_NUMBER_STR "Specify by number\n"
#define INTERFACE_STR "Interface infomation\n"
#define IFNAME_STR "Interface name(e.g. ep0)\n"
#define IP6_STR "IPv6 Information\n"
#define OSPF6_STR "Open Shortest Path First (OSPF) for IPv6\n"
#define OSPF6_ROUTER_STR "Enable a routing process\n"
#define OSPF6_INSTANCE_STR "<1-65535> Instance ID\n"
#define SECONDS_STR "<1-65535> Seconds\n"
#define ROUTE_STR "Routing Table\n"
#define PREFIX_LIST_STR "Build a prefix list\n"
#define OSPF6_DUMP_TYPE_LIST "(neighbor|interface|area|lsa|zebra|config|dbex|"\
"spf|route|lsdb|redistribute|hook|asbr|prefix|abr)"
#define ISIS_STR "IS-IS information\n"
#define AREA_TAG_STR "[area tag]\n"

#define CONF_BACKUP_EXT ".sav"
#define CONF_TEMP_EXT   ".new"

/* IPv4 only machine should not accept IPv6 address for peer's IP
   address.  So we replace VTY command string like below. */
#ifdef HAVE_IPV6
#define NEIGHBOR_CMD       "neighbor (A.B.C.D|X:X::X:X) "
#define NO_NEIGHBOR_CMD    "no neighbor (A.B.C.D|X:X::X:X) "
#define NEIGHBOR_ADDR_STR  "Neighbor address\nIPv6 address\n"
#define NEIGHBOR_CMD2      "neighbor (A.B.C.D|X:X::X:X|WORD) "
#define NO_NEIGHBOR_CMD2   "no neighbor (A.B.C.D|X:X::X:X|WORD) "
#define NEIGHBOR_ADDR_STR2 "Neighbor address\nNeighbor IPv6 address\nNeighbor tag\n"
#else
#define NEIGHBOR_CMD       "neighbor A.B.C.D "
#define NO_NEIGHBOR_CMD    "no neighbor A.B.C.D "
#define NEIGHBOR_ADDR_STR  "Neighbor address\n"
#define NEIGHBOR_CMD2      "neighbor (A.B.C.D|WORD) "
#define NO_NEIGHBOR_CMD2   "no neighbor (A.B.C.D|WORD) "
#define NEIGHBOR_ADDR_STR2 "Neighbor address\nNeighbor tag\n"
#endif /* HAVE_IPV6 */

/*------------------------------------------------------------------------------
 * Prototypes.
 */
extern void cmd_print_version (const char *);

extern void host_init(const char* arg0) ;
extern void host_finish(void) ;

extern void cmd_host_config_set(const char* own_config_file, bool own_flag,
                                const char* int_config_file, bool int_flag,
                                                             bool int_boot);
extern void cmd_host_pthreaded(bool pthreaded_option,
                                          init_second_stage init_second_stage) ;

extern void cmd_table_init (daemon_set_t daemons);
extern void cmd_install_node_config_write(node_type_t node,
                                                    int (*config_write) (vty)) ;
extern void cmd_install_table(cmd_table table) ;

extern void cmd_install_command (node_type_t node, cmd_command cmd,
                                                         daemon_set_t daemons) ;
extern void cmd_table_complete (void);
#define sort_node() cmd_table_complete()
extern void cmd_table_terminate (void);

extern void cmd_set_integrated_vtysh_config(on_off_b integrated) ;

extern const char* cmd_host_name(bool fresh) ;
extern const char* cmd_host_program_name(void) ;
extern const char* cmd_host_full_program_name(void) ;

/* Concatenates argv[shift] through argv[argc-1] into a single NUL-terminated
   string with a space between each element (allocated using
   XMALLOC(MTYPE_TMP)).  Returns NULL if shift >= argc. */
extern char *argv_concat (const char* const* argv, int argc, int shift);

/*------------------------------------------------------------------------------
 * Global variables
 */
extern const char *debug_banner ;

extern daemon_set_t deamons_set ;
extern const char*  daemon_name ;

#endif /* _ZEBRA_COMMAND_H */

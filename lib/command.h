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
 * Turn off these macros when using cpp with extract.pl
 */
#ifndef VTYSH_EXTRACT_PL

/* helper defines for end-user DEFUN* macros                            */
#define DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, attrs, dnum) \
  struct cmd_command cmdname = \
  { \
    .string = cmdstr,   \
    .func   = funcname, \
    .doc    = helpstr,  \
    .attr   = attrs,    \
    .daemon = dnum,     \
    \
    .items    = NULL,   \
    .nt_min   = 0,      \
    .nt       = 0,      \
    .nt_max   = 0,      \
    .vararg   = NULL,   \
    .r_string = NULL,   \
    .r_doc    = NULL,   \
  } ;

#define DEFUN_CMD_FUNC_DECL(funcname) \
  static cmd_function funcname;

#define DEFUN_CMD_FUNC_TEXT(funcname) \
  static DEFUN_CMD_FUNCTION(funcname)

/* DEFUN for vty command interface. Little bit hacky ;-).               */
#define DEFUN(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_FUNC_DECL(funcname) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, 0, 0) \
  DEFUN_CMD_FUNC_TEXT(funcname)

#define DEFUN_ATTR(funcname, cmdname, cmdstr, helpstr, attr) \
  DEFUN_CMD_FUNC_DECL(funcname) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, attr, 0) \
  DEFUN_CMD_FUNC_TEXT(funcname)

#define DEFUN_CALL(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_ATTR (funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DIRECT)

#define DEFUN_HIDDEN(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_ATTR (funcname, cmdname, cmdstr, helpstr, CMD_ATTR_HIDDEN)

#define DEFUN_HID_CALL(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_ATTR (funcname, cmdname, cmdstr, helpstr, \
                                            (CMD_ATTR_DIRECT | CMD_ATTR_HIDDEN))

#define DEFUN_DEPRECATED(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_ATTR (funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DEPRECATED)

#define DEFUN_DEP_CALL(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_ATTR (funcname, cmdname, cmdstr, helpstr, \
                                        (CMD_ATTR_DIRECT | CMD_ATTR_DEPRECATED))

/* DEFUN_NOSH for commands that vtysh should ignore                     */
#define DEFUN_NOSH(funcname, cmdname, cmdstr, helpstr) \
  DEFUN(funcname, cmdname, cmdstr, helpstr)

/* DEFSH for vtysh.                                                     */
#define DEFSH(daemon, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(NULL, cmdname, cmdstr, helpstr, 0, daemon)

/* DEFUN + DEFSH                                                        */
#define DEFUNSH(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_FUNC_DECL(funcname) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, 0, daemon) \
  DEFUN_CMD_FUNC_TEXT(funcname)

/* DEFUN + DEFSH with attributes                                        */
#define DEFUNSH_ATTR(daemon, funcname, cmdname, cmdstr, helpstr, attr) \
  DEFUN_CMD_FUNC_DECL(funcname) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, attr, daemon) \
  DEFUN_CMD_FUNC_TEXT(funcname)

#define DEFUNSH_HIDDEN(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUNSH_ATTR (daemon, funcname, cmdname, cmdstr, helpstr, CMD_ATTR_HIDDEN)

#define DEFUNSH_DEPRECATED(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUNSH_ATTR (daemon, funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DEPRECATED)

/* ALIAS macro which define existing command's alias.                   */
#define ALIAS(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, 0, 0)

#define ALIAS_ATTR(funcname, cmdname, cmdstr, helpstr, attr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, attr, 0)

#define ALIAS_CALL(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DIRECT, 0)

#define ALIAS_HIDDEN(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, CMD_ATTR_HIDDEN, 0)

#define ALIAS_DEPRECATED(funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DEPRECATED, 0)

#define ALIAS_SH(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, 0, daemon)

#define ALIAS_SH_HIDDEN(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, CMD_ATTR_HIDDEN, daemon)

#define ALIAS_SH_DEPRECATED(daemon, funcname, cmdname, cmdstr, helpstr) \
  DEFUN_CMD_COMMAND(funcname, cmdname, cmdstr, helpstr, CMD_ATTR_DEPRECATED,\
                                                                         daemon)

#endif /* VTYSH_EXTRACT_PL */

/* Common descriptions. */
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
#define OSPF6_DUMP_TYPE_LIST \
"(neighbor|interface|area|lsa|zebra|config|dbex|spf|route|lsdb|redistribute|hook|asbr|prefix|abr)"
#define ISIS_STR "IS-IS information\n"
#define AREA_TAG_STR "[area tag]\n"

#define CONF_BACKUP_EXT ".sav"

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

/* Prototypes. */
extern void cmd_init (int);
extern void cmd_terminate (void);

extern void print_version (const char *);

extern void install_node (struct cmd_node *, int (*) (struct vty *));
extern void install_default (node_type_t);
extern void install_element (node_type_t, struct cmd_command *);
extern void sort_node (void);

extern const char* cmd_host_name(bool fresh) ;

extern node_type_t cmd_node_parent(node_type_t node) ;
extern node_type_t cmd_node_exit_to(node_type_t node) ;
extern node_type_t cmd_node_end_to(node_type_t node) ;

/* Concatenates argv[shift] through argv[argc-1] into a single NUL-terminated
   string with a space between each element (allocated using
   XMALLOC(MTYPE_TMP)).  Returns NULL if shift >= argc. */
extern char *argv_concat (const char* const* argv, int argc, int shift);

/* Export typical functions. */
extern struct cmd_command config_end_cmd;
extern struct cmd_command config_exit_cmd;
extern struct cmd_command config_quit_cmd;
extern struct cmd_command config_help_cmd;
extern struct cmd_command config_list_cmd;
extern char *host_config_file (void);
extern void host_config_set (const char *);

#ifdef QDEBUG
extern const char *debug_banner ;
#endif

#endif /* _ZEBRA_COMMAND_H */

/* Command handler node_type stuff -- header
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

#ifndef _ZEBRA_COMMAND_COMMON_H
#define _ZEBRA_COMMAND_COMMON_H

#include "misc.h"
#include "vector.h"

/*------------------------------------------------------------------------------
 * These are the command levels/contexts.
 *
 * NB: this is the order in which configuration is written to the
 *     configuration file.
 */
enum node_type
{
  NULL_NODE   = 0,      /* For when need "not a node"                         */

  AUTH_NODE,            /* VTY login -> VIEW_NODE                             */
  RESTRICTED_NODE,      /* if no login required, may use this node            */
  VIEW_NODE,            /* aka user EXEC                                      */
  AUTH_ENABLE_NODE,     /* enable login -> ENABLE_NODE/CONFIG_NODE            */
  ENABLE_NODE,          /* aka privileged EXEC                                */

  MIN_DO_SHORTCUT_NODE = ENABLE_NODE,
                        /* May not "do xxx" at any node lower                 */
  MAX_NON_CONFIG_NODE  = ENABLE_NODE,
                        /* May not be higher than this without owning
                         * the configuration symbol of power                  */

  CONFIG_NODE,          /* aka global configuration mode                      */

  MIN_CONFIG_NODE      = CONFIG_NODE,
                        /* May not change context to any node lower           */

  SERVICE_NODE,         /* unused !                                           */
  DEBUG_NODE,           /* debug config write only                            */
  AAA_NODE,             /* unused !                                           */
  KEYCHAIN_NODE,	/* see: keychain.c                                    */
  KEYCHAIN_KEY_NODE,    /* see: keychain.c    -- child of KEYCHAIN_NODE       */
  INTERFACE_NODE,       /* interface commands                                 */
  ZEBRA_NODE,           /* router zebra commands                              */
  TABLE_NODE,           /* rtm_table config write -- see zserv.c              */
  RIP_NODE,		/* router rip commands                                */
  RIPNG_NODE,           /* router ripng commands                              */
  BGP_NODE,		/* router bgp commands                                */
  BGP_VPNV4_NODE,       /* address-family vpnv4          -- child of BGP_NODE */
  BGP_IPV4_NODE,        /* address-family ipv4 (unicast) -- child of BGP_NODE */
  BGP_IPV4M_NODE,       /* address-family ipv4 multicast -- child of BGP_NODE */
  BGP_IPV6_NODE,        /* address-family ipv6 (unicast) -- child of BGP_NODE */
  BGP_IPV6M_NODE,       /* address-family ipv6 multicast -- child of BGP_NODE */
  OSPF_NODE,            /* router ospf commands                               */
  OSPF6_NODE,           /* router ospf6 commands                              */
  ISIS_NODE,            /* router isis commands                               */
  MASC_NODE,            /* unused !  RFC 2909 Multicast Address-Set Claim     */
  IRDP_NODE,		/* unused !  ICMP Router Discovery Protocol           */
  IP_NODE,              /* zebra_ip_config write only -- see zebra_vty.c      */
  ACCESS_NODE,          /* access list config write only -- see filter.c      */
  PREFIX_NODE,		/* prefix list config write only -- see plist.c       */
  ACCESS_IPV6_NODE,     /* access list config write only -- see filter.c      */
  PREFIX_IPV6_NODE,     /* prefix list config write only -- see plist.c       */
  AS_LIST_NODE,         /* AS list config write only -- see bgp_filter.c      */
  COMMUNITY_LIST_NODE,  /* Community list config write only -- see bgp_vty.c  */
  RMAP_NODE,            /* route-map commands                                 */
  SMUX_NODE,            /* SNMP config write only -- see smux.c               */
  DUMP_NODE,            /* BGP dump config write only -- see bgp_dump.c       */
  FORWARDING_NODE,      /* forwarding config write -- see zserv.c             */
  PROTOCOL_NODE,        /* protocol config write -- see zebra_vty.c           */
  VTY_NODE,             /* line vty commands                                  */

  MAX_PLUS_1_NODE,
  MAX_NODE   = MAX_PLUS_1_NODE - 1
} ;
typedef enum node_type node_type_t ;

/*------------------------------------------------------------------------------
 * Return values for command handling.
 *
 * NB: when a command is executed it may return CMD_SUCCESS, CMD_WARNING
 *     or CMD_ERROR.
 *
 *     In all cases any output required (including any warning or error
 *     messages) must already have been output.
 *
 *     CMD_WARNING will stop configuration reader, unless ignore warning
 *     option is set.
 *
 *     CMD_ERROR will always stop the configuration reader.
 *
 *     If there is no output and either CMD_WARNING or CMD_ERROR, then will
 *     output a general warning message.
 *
 *     All other return codes are for use within the command handler.
 */
enum cmd_return_code
{
  CMD_SUCCESS     = 0,          /* used generally                       */

  /* Return codes suitable for command execution functions              */

  CMD_WARNING     =  1,
  CMD_ERROR,

  /* Return codes from the command parser                               */

  CMD_EMPTY,                    /* parser: nothing to execute           */

  CMD_ERR_PARSING,              /* parser: general parser error         */
  CMD_ERR_NO_MATCH,             /* parser: command/argument not recognised  */
  CMD_ERR_AMBIGUOUS,            /* parser: more than on command matches */
  CMD_ERR_INCOMPLETE,

  CMD_CLOSE,                    /* command: used by "exit"              */



  CMD_WAITING,                  /* I/O: waiting for more input          */
  CMD_EOF,                      /* I/O: nothing more to come            */

  CMD_HIATUS,                   /* Something requires attention         */

  CMD_IO_ERROR,                 /* I/O -- failed :-(                    */
  CMD_IO_TIMEOUT,               /* I/O -- timed out :-(                 */

  /* For the chop ????                          */


  CMD_COMPLETE_FULL_MATCH,      /* cmd_completion returns               */
  CMD_COMPLETE_MATCH,
  CMD_COMPLETE_LIST_MATCH,
  CMD_COMPLETE_ALREADY,


  CMD_SUCCESS_DAEMON,           /* parser: success & command is for vtysh ? */
} ;

typedef enum cmd_return_code cmd_return_code_t ;

/*------------------------------------------------------------------------------
 * Structure for each node -- root of all commands for the node.
 *
 * See install_node().
 */
struct vty ;                    /* Forward reference                    */

struct cmd_node
{
  node_type_t   node ;          /* who we are                           */

  const char*   prompt ;        /* prompt string for vty                */

  bool  config_to_vtysh ;       /* configuration goes to vtysh ?        */

  node_type_t   parent ;        /* parent when parsing commands         */
  node_type_t   exit_to ;       /* where to go on "exit"                */
  node_type_t   end_to ;        /* where to go on "end", "^C" or "^Z"   */

  int (*config_write) (struct vty*) ;   /* configuration write function */

  vector_t cmd_vector;          /* Vector of this node's commands.      */
} ;

typedef struct cmd_node  cmd_node_t ;
typedef struct cmd_node* cmd_node ;

/*------------------------------------------------------------------------------
 * Commands -- contents of the nodes' cmd_vector(s).
 *
 * A cmd_command is a static structure, which contains dynamic elements
 * which are set when a command is installed.  Note that is not uncommon for
 * one cmd_command to appear in more than one node.
 *
 * The command attributes affect:
 *
 *   * the parsing of the command -- in particular how the next_node is
 *     established.
 *
 *   * whether the command is shown in help (or some forms of help if
 *     deprecated.
 *
 *   * whether the command can be executed directly in the cli thread
 *     (avoiding having to wait for the cmd thread's attention -- this may be
 *      less useful now that all commands are treated a "priority" messages
 *      going into the cmd thread).
 *
 * If the command is marked CMD_ATTR_NODE, then the CMD_ATTR_MASK will
 * extract the node_type_t that the command will set, if CMD_SUCCESS.  This
 * means that can parse commands without executing them.
 *
 * If the command is not marked CMD_ATTR_NODE, then the CMD_ATTR_MASK will
 * extract the cmd_special_t value for the command -- which will be
 * interesting if it isn't cmd_sp_simple.
 */
enum cmd_attr
{
  CMD_ATTR_SIMPLE     = 0,              /* bit significant              */

  CMD_ATTR_NODE       = BIT(7),         /* sets given node              */
  CMD_ATTR_MASK       = CMD_ATTR_NODE - 1,

  CMD_ATTR_DEPRECATED = BIT(12),
  CMD_ATTR_HIDDEN     = BIT(13),        /* not shown in help            */
  CMD_ATTR_DIRECT     = BIT(14),        /* can run in cli thread        */
};
typedef enum cmd_attr cmd_attr_t ;

CONFIRM(CMD_ATTR_MASK >= (cmd_attr_t)MAX_NODE) ;

/* Special commands, which require extra processing at parse time.      */
enum cmd_special
{
  cmd_sp_simple  = 0,

  cmd_sp_end,
  cmd_sp_exit,
  cmd_sp_enable,
  cmd_sp_configure,

  cmd_sp_max_plus_1,
  cmd_sp_max     = cmd_sp_max_plus_1 - 1
} ;
typedef enum cmd_special cmd_special_t ;

CONFIRM(CMD_ATTR_MASK >= (cmd_attr_t)cmd_sp_max) ;

/* Command functions and macros to define same                          */

struct cmd_command ;
typedef struct cmd_command* cmd_command ;

typedef const char* const argv_t[] ;

#define DEFUN_CMD_ARG_UNUSED __attribute__ ((unused))
#define DEFUN_CMD_FUNCTION(name) \
  enum cmd_return_code name (cmd_command self   DEFUN_CMD_ARG_UNUSED, \
                             struct vty* vty    DEFUN_CMD_ARG_UNUSED, \
                             int argc           DEFUN_CMD_ARG_UNUSED, \
                             argv_t argv        DEFUN_CMD_ARG_UNUSED)

typedef DEFUN_CMD_FUNCTION((cmd_function)) ;

/* The cmd_command structure itself                                     */

struct cmd_item ;               /* Defined in command_parse.h           */

struct cmd_command
{
  const char*   string ;        /* Command specification by string.     */
  cmd_function* func ;
  const char*   doc ;           /* Documentation of this command.       */
  int           daemon ;        /* Daemon to which this command belong. */
  cmd_attr_t    attr ;          /* Command attributes                   */

  vector        items ;         /* Vector of pointers to cmd_item(s)    */

  uint          nt_min ;        /* excluding [option](s)                */
  uint          nt ;            /* count of all items                   */
  uint          nt_max ;        /* "infinite" if .vararg                */

  struct cmd_item*  vararg ;    /* if there is a vararg item            */

  char*         r_doc ;         /* rendered documentation               */

//char*         config ;        /* Configuration string                 */
//vector        subconfig ;     /* Sub configuration string             */
};

#endif /* _ZEBRA_COMMAND_COMMON_H */

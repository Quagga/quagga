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
 * These are the command nodes/contexts.
 *
 * In this context, "context" means any state which may be associated with a
 * command node.  In particular, the vty->index field contains state whose
 * meaning depends on the current node -- but there may be any amount of other
 * state elsewhere, the command system does not know.  The rules governing
 * that context are described below.
 *
 * There are "bands" of nodes with their own properties:
 *
 *   * SPECIAL -- used, if at all, temporarily
 *
 *        EXIT_NODE   -- temporary state on the way out
 *
 *                       ie at EOF on a file or pipe, or about to close VTY
 *                       TERMINAL, VTYSH SERVER or VTYSH itself.
 *
 *        META_NODE   -- contains the meta-commands.  Will never be in this
 *                       node.
 *
 *     These nodes have no node changing commands, including no "end" and
 *     "exit".
 *
 *     These nodes have no context of their own.
 *
 *   * VIEW    -- used in VTY Terminal
 *
 *        AUTH_NODE   -- default initial state.
 *
 *                       May move up to VIEW_NODE or ENABLE node once has
 *                       authenticated.
 *
 *        VIEW_NODE   -- default state after AUTH_NODE, and optional initial
 *                       state.
 *
 *                       May move up to an ENABLE or a CONFIG node on command,
 *                       usually via AUTH_ENABLE_NODE.
 *
 *        RESTRICTED_NODE -- optional initial state.
 *
 *                       Similar to VIEW_NODE, but with fewer commands.
 *
 *        AUTH_ENABLE_NODE -- state for authentication prior to ENABLE or
 *                       CONFIG nodes.
 *
 *     Can change up to ENABLE or CONFIG, but "end" does not return here,
 *     so VIEW is not treated as the parent of those nodes.
 *
 *     These nodes have no parents and no children:
 *
 *       "end"  has no effect.
 *
 *       "exit" means goto "EXIT_NODE".
 *
 *     These nodes have no context of their own.
 *
 *   * ENABLE  -- general, but privileged, commands
 *
 *     For all but VTY Terminal, these are the basic nodes.  For VTY Terminal,
 *     may reach here by command from a VIEW node, but in may start here.
 *
 *     There is currently only one ENABLE_NODE, but it would be possible to
 *     have more than one, and even one or more trees of such nodes.
 *
 *     These nodes have no parents, except where they are descendants of
 *     other ENABLE nodes:
 *
 *       "end"  means goto parent ENABLE node, if any, otherwise do nothing.
 *
 *       "exit" means goto parent ENABLE node, if any, otherwise goto EXIT_NODE.
 *
 *     These nodes may have children: CONFIG or other ENABLE nodes.
 *
 *     These nodes have no context of their own.
 *
 *     The ENABLE nodes have the property that commands may run in ANY CONTEXT.
 *     In particular, all children of a given ENABLE node may execute all
 *     the commands in that node, implicitly and without changing node before
 *     hand (but the command may change node itself).
 *
 *   * CONFIG  -- general configuration commands
 *
 *     Require the configuration symbol of power.
 *
 *     CONFIG nodes are reached by command from ENABLE (or VIEW).
 *
 *     There is currently only one CONFIG_NODE, but it would be possible to
 *     have more than one, and even one or more trees of such nodes.
 *
 *     These nodes have parents, CONFIG or ENABLE nodes:
 *
 *       "end"  means goto parent (or grandparent etc) ENABLE node
 *
 *       "exit" means goto parent node
 *
 *     These nodes may have children: SPECIFIC or other CONFIG.
 *
 *     These nodes have no context of their own.
 *
 *   * SPECIFIC -- configuration nodes which may have their own context.
 *
 *     Require the configuration symbol of power.
 *
 *     SPECIFIC nodes are reached by command from CONFIG.
 *
 *     There are many SPECIFIC nodes and trees of such nodes.
 *
 *     These nodes have parents, CONFIG or other SPECIFIC nodes:
 *
 *       "end"  means goto ancestor ENABLE node (the immediate parent of the
 *              most senior CONFIG ancestor).
 *
 *       "exit" means goto parent node
 *
 *     These nodes may have children: other SPECIFIC nodes.
 *
 *     These nodes may have context of their own, but the context of a parent
 *     node MUST be a subset of any descendant's.  So that:
 *
 *       * when moving down the tree (from parent to child) extra context
 *         (state) may be set/created -- usually by the command that changes
 *         the node -- provided the parent context is preserved.
 *
 *       * when moving up the tree (from child to parent) nothing special need
 *         be done.
 *
 * NB: configuration is written out in the order of the underlying node numbers.
 */
enum node_type
{
  NULL_NODE   = 0,      /* For when need "not a node"                         */

  /* The SPECIAL nodes ---------------------------------------------------------
   */
  MIN_SPECIAL_NODE  = NULL_NODE + 1,

  EXIT_NODE,            /* Temporary node on the way to the door              */

  META_NODE,            /* Temporary node for meta command execution          */

  MAX_SPECIAL_NODE  = META_NODE,

  /* The VIEW nodes ------------------------------------------------------------
   */
  MIN_VIEW_NODE     = MAX_SPECIAL_NODE + 1,

  AUTH_NODE,            /* VTY login -> VIEW_NODE                             */

  RESTRICTED_NODE,      /* if no login required, may use this node            */
  VIEW_NODE,            /* aka user EXEC                                      */

  AUTH_ENABLE_NODE,     /* enable login -> ENABLE_NODE/CONFIG_NODE            */

  MAX_VIEW_NODE     = AUTH_ENABLE_NODE,

  /* The ENABLE nodes-----------------------------------------------------------
   */
  MIN_ENABLE_NODE   = MAX_VIEW_NODE + 1,

  ENABLE_NODE,          /* aka privileged EXEC                                */

  MAX_ENABLE_NODE   = ENABLE_NODE,
                        /* Only the one, ATM                                  */

  /* The CONFIG nodes-----------------------------------------------------------
   */
  MIN_CONFIG_NODE   = MAX_ENABLE_NODE + 1,

  CONFIG_NODE,          /* aka global configuration mode                      */

  MAX_CONFIG_NODE   = CONFIG_NODE,
                        /* Only the one, ATM                                  */
  /* The SPECIFIC nodes---------------------------------------------------------
   */
  MIN_SPECIFIC_NODE = MAX_CONFIG_NODE + 1,

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

  MAX_SPECIFIC_NODE = VTY_NODE,

  /* All real nodes + the NULL_NODE are in the range MIN_NODE..MAX_NODE
   * and the NULL_NODE == MIN_NODE.
   *
   * So any new real node MUST precede this.
   */
  NUMBER_OF_NODES,

  MIN_NODE     = 0,     /* unsigned value !                                   */
  MAX_NODE     = NUMBER_OF_NODES - 1,
} ;

typedef enum node_type node_type_t ;

CONFIRM((NULL_NODE == MIN_NODE) && (MIN_NODE == 0)) ;

/* We may assume:
 *
 *  that: SPECIAL < VIEW < ENABLE < CONFIG < SPECIFIC
 *
 *   and: there is nothing between, before or after
 *
 *   and: (therefore) that everything >= MIN_CONFIG_NODE requires the
 *        configuration symbol of power.
 *
 *   and: (therefore) that everything > MAX_CONFIG_NODE may have specific
 *        context.
 */
CONFIRM(MIN_SPECIAL_NODE  == (MIN_NODE          + 1)) ; /* First not NULL ! */
CONFIRM(MIN_VIEW_NODE     == (MAX_SPECIAL_NODE  + 1)) ;
CONFIRM(MIN_ENABLE_NODE   == (MAX_VIEW_NODE     + 1)) ;
CONFIRM(MIN_CONFIG_NODE   == (MAX_ENABLE_NODE   + 1)) ;
CONFIRM(MIN_SPECIFIC_NODE == (MAX_CONFIG_NODE   + 1)) ;
CONFIRM(MAX_SPECIFIC_NODE == MAX_NODE) ;

/*------------------------------------------------------------------------------
 * The daemons known to the command handling
 *
 * The "real" daemons are the ones known to vtysh !
 *
 * These ordinals specify the order in which daemons are called by vtysh.
 */
typedef enum DAEMON_ORD
{
  BGPD_ORD      = 0,

  OSPFD_ORD     = 1,
  OSPF6D_ORD    = 2,

  ISISD_ORD     = 3,

  RIPD_ORD      = 4,
  RIPNGD_ORD    = 5,

  ZEBRA_ORD     = 6,

  DAEMON_COUNT  = 7,            /* Number of real daemons               */

  BASIC_ORD     = 13,           /* Basic "virtual daemon"               */
  VTYSH_ORD     = 14,           /* vtysh "virtual daemon"               */

  TERM_ORD      = 15,           /* magic marker                         */

  DAEMON_INVALID_ORD,
  DAEMON_MAX_ORD  = DAEMON_INVALID_ORD - 1,

} daemon_ord_t ;

typedef enum DAEMON
{
  DAEMON_NONE   = 0,

  ZEBRA         = BIT(ZEBRA_ORD),

  RIPD          = BIT(RIPD_ORD),
  RIPNGD        = BIT(RIPNGD_ORD),

  OSPFD         = BIT(OSPFD_ORD),
  OSPF6D        = BIT(OSPF6D_ORD),

  BGPD          = BIT(BGPD_ORD),
  ISISD         = BIT(ISISD_ORD),

  ALL_RDS       = ZEBRA | RIPD | RIPNGD | OSPFD | OSPF6D | BGPD | ISISD,

  RMAP_DS       = ZEBRA | RIPD | RIPNGD | OSPFD | OSPF6D | BGPD,

  INTERFACE_DS  = ZEBRA | RIPD | RIPNGD | OSPFD | OSPF6D        | ISISD,

  BASIC_VD      = BIT(BASIC_ORD),
  VTYSH_VD      = BIT(VTYSH_ORD),

  ALL_VDS       = BASIC_VD | VTYSH_VD,

  TERM          = BIT(TERM_ORD),        /* magic marker bit     */
} daemon_bit_t ;

typedef daemon_bit_t daemon_set_t ;

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

  /* Return codes suitable for command execution functions
   */
  CMD_WARNING     =  1,         /* command: not 100% successful         */
  CMD_ERROR,                    /* command: failed badly                */

  /* Return codes from the command parser
   */
  CMD_ERR_PARSING,              /* parser: general parser error         */
  CMD_ERR_NO_MATCH,             /* parser: command/argument not recognised  */
  CMD_ERR_AMBIGUOUS,            /* parser: matched more than one command    */
  CMD_ERR_INCOMPLETE,

  /* Return codes used in command loop
   */
  CMD_STOP,                     /* loop: stop and close vty (final)     */

  CMD_CANCEL,                   /* loop: stop and close down to base
                                 *       vin/vout and discard output.   */

  /* Return codes from I/O layers
   */
  CMD_HIATUS,                   /* I/O: enter hiatus because some vin/vout
                                 *      is not vf_open as required/expected
                                 *      or because some other attention is
                                 *      required.                       */
  CMD_WAITING,                  /* I/O: waiting for more input          */
  CMD_IO_ERROR,                 /* I/O: error or time-out               */

  /* Meta values
   */
  CMD_RET_COUNT,                /* number of return codes               */

  CMD_RET_MIN  = 0,             /* unsigned value                       */
  CMD_RET_MAX  = CMD_RET_COUNT - 1
} ;

typedef enum cmd_return_code cmd_ret_t ;

/*------------------------------------------------------------------------------
 * Structure for each node -- root of all commands for the node.
 *
 * See cmd_install_node().
 */
struct vty ;                    /* Forward reference                    */

struct cmd_node
{
  const node_type_t node ;      /* who we are                           */
  const char const* name ;      /* who we are as text                   */

  daemon_set_t  daemons ;       /* restricted to this set of daemons    */

  const char const* prompt ;    /* prompt string for vty
                                 * NULL => no commands in this node     */

  bool  config_to_vtysh ;       /* configuration goes to vtysh ?        */

  bool  installed ;             /* set by cmd_install_node()            */

  int (*config_write) (struct vty*) ;   /* configuration write function */

  vector        cmd_vector ;    /* node's commands, if any.             */
  const bool    parse_strict ;  /* all of node's commands are "strict"  */

  bool  executable ;            /* node which may contain commands      */

  node_type_t   parent ;        /* parent when parsing commands         */
  node_type_t   exit_to ;       /* where to go on "exit"                */
  node_type_t   end_to ;        /* where to go on "end" or "^Z"         */
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
  CMD_ATTR_SIMPLE     = 0,

  CMD_ATTR_NODE       = BIT(7),         /* sets given node              */
  CMD_ATTR_MASK       = CMD_ATTR_NODE - 1,

  CMD_ATTR_DEPRECATED = BIT( 8),
  CMD_ATTR_HIDDEN     = BIT( 9),        /* not shown in help            */
  CMD_ATTR_DIRECT     = BIT(10),        /* can run in cli thread        */
  CMD_ATTR_FIRST      = BIT(11),        /* a "first_config_command"     */
};
typedef enum cmd_attr cmd_attr_t ;

CONFIRM(CMD_ATTR_MASK >= (cmd_attr_t)MAX_NODE) ;

/* Special commands, which require extra processing at parse time.
 */
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
typedef struct cmd_command  cmd_command_t ;
typedef struct cmd_command* cmd_command ;

typedef const char* const argv_t[] ;

#define DEFUN_CMD_ARG_UNUSED __attribute__ ((unused))
#define DEFUN_CMD_FUNCTION(name) \
  cmd_ret_t name(cmd_command self   DEFUN_CMD_ARG_UNUSED, \
                 struct vty* vty    DEFUN_CMD_ARG_UNUSED, \
                 int    argc        DEFUN_CMD_ARG_UNUSED, \
                 argv_t argv        DEFUN_CMD_ARG_UNUSED)

typedef DEFUN_CMD_FUNCTION((cmd_function)) ;

/* The cmd_command structure itself                                     */

struct cmd_item ;               /* Defined in command_parse.h           */
typedef struct cmd_item* cmd_item ;

struct cmd_command
{
  const char*   string ;        /* Command specification by string.     */
  cmd_function* func ;
  const char*   doc ;           /* Documentation of this command.       */
  daemon_set_t  daemons ;       /* Daemons this command applies to      */
  cmd_attr_t    attr ;          /* Command attributes                   */

  vector        items ;         /* Vector of pointers to cmd_item(s)    */

  uint          nt_min ;        /* excluding [option](s)                */
  uint          nt ;            /* count of all items                   */
  uint          nt_max ;        /* "infinite" if .vararg                */

  cmd_item      vararg ;        /* if there is a vararg item            */

  char*         r_doc ;         /* rendered documentation               */

//char*         config ;        /* Configuration string                 */
//vector        subconfig ;     /* Sub configuration string             */
} ;

/*------------------------------------------------------------------------------
 * Structures for tables of commands to be installed
 *
 * NB: The order of entries in the cmd_install_table_item is FIXED, because
 *     the initialisers for hundreds of commands depend on this !
 *
 *     The mechanism also depends on the initialiser setting unspecified fields
 *     to zero -- as per standard C.
 *
 * See notes on cmd_install_table()
 */
typedef const struct cmd_table_item
{
  const node_type_t     node ;
  const cmd_command     cmd ;
  const daemon_set_t    add_daemons ;   /* local for command    */
  const daemon_set_t    del_daemons ;   /* local for command    */
} cmd_table_body[], * cmd_table_item ;

struct cmd_table
{
  cmd_table_item        body ;          /* pointer to body      */
  const daemon_set_t    daemons ;       /* global for table     */
} ;

typedef const struct cmd_table cmd_table_t ;
typedef cmd_table_t cmd_table[] ;

/*------------------------------------------------------------------------------
 * Where daemon uses pthreads, it must provide a second stage initialisation
 * function -- see cmd_host_config_set() & vty_read_config()
 */
typedef void (*init_second_stage)(bool pthreaded) ;

#endif /* _ZEBRA_COMMAND_COMMON_H */

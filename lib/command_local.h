/* Command handler -- header for stuff used within command/vty
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

#ifndef _ZEBRA_COMMAND_LOCAL_H
#define _ZEBRA_COMMAND_LOCAL_H

#include "command_common.h"             /* First and foremost ! */

#include "vty_local.h"
#include "vector.h"
#include "qpath.h"

/*==============================================================================
 * This is for access to some things in command.c which are not required
 * by external users, who use command.h.
 *
 * This is for use within the command/vty family.
 *
 * Should not be used with command.h !  (Except in command.c itself.)
 *
 * This may duplicate things published in command.h, but also includes things
 * which are not intended for "external" use.
 */

/*----------------------------------------------------------------------------*/

#define MSG_CMD_ERR_AMBIGUOUS      "Ambiguous command"
#define MSG_CMD_ERR_NO_MATCH       "Unrecognised command"
#define MSG_CMD_ERR_NO_MATCH_old   "There is no matched command"

/*------------------------------------------------------------------------------
 * Password and its encryption state
 */
typedef struct
{
  char* text ;
  bool  encrypted ;

} password_t ;

/*------------------------------------------------------------------------------
 * Host configuration variable
 */
typedef unsigned long int name_gen_t ;

struct host
{
  /* Host name of this router.                                          */
  char*      name ;
  bool       name_set ;         /* set by command                       */
  name_gen_t name_gen ;         /* incremented each time name changes   */

  /* Password for vty interface.                        */
  password_t password;

  /* Enable password                                    */
  password_t enable;

  /* System wide terminal lines default                 */
  int        lines;

  /* Log filename.                                      */
  qpath      logfile;

  /* config file names etc of this host                 */
  qpath      own_config_file ;
  qpath      int_config_file ;
  qpath      config_file ;
  qpath      config_dir ;

  /* Initialisation and configuration                   */
  bool       pthreads_allowed ;
  bool       pthreaded_option ;
  bool       pthreaded_config ;
  bool       first_config_cmd ;
  bool       newborn ;
  init_second_stage init_second_stage ;

  /* Flags for services                                 */
  bool       advanced;
  bool       encrypt;

  /* Banner configuration.                              */
  const char* motd ;
  qpath      motdfile;

  /* Someone has the config symbol of power             */
  ulong      config ;           /* zero <=> no owner    */
  ulong      config_brand ;

  /* Allow vty to start without password                */
  bool       no_password_check ;

  /* Restrict unauthenticated logins?                   */
  bool       restricted_mode ;

  /* vty timeout value -- see "exec-timeout" command    */
  unsigned long vty_timeout_val ;

  /* vty access-class for IPv4 and IPv6                 */
  char*      vty_accesslist_name ;
  char*      vty_ipv6_accesslist_name ;

  /* How to configure listeners                         */
  char*      vty_listen_addr ;
  ushort     vty_listen_port ;
  char*      vtysh_listen_path  ;

  /* Current directory -- set in host_init()            */
  qpath      cwd ;

  /* Program name -- set in host_init(), never unset    */
  const char* full_progname ;
  const char* progname ;

  /* __DATE__ & __TIME__ from last compilation          */
  const char* date ;
  const char* time ;
} ;

enum
{
  restricted_mode_default = false,
} ;

/* Structure declared in command.c                                      */
extern struct host host ;

/*==============================================================================
 * This is the stuff that defines the context in which commands are parsed,
 * and then executed.
 *
 * The context lives in the vin, so each time a new vin is pushed/popped, the
 * context is (implicitly) pushed/popped, and any state that depends on same
 * must be updated.
 */

/* Command qualifiers
 */
typedef enum cmd_do
{
  cmd_do_nothing  = 0,  /* no action required                   */

  cmd_do_command  = 1,  /* dispatch the current command line    */

  cmd_do_ctrl_c,        /* received ^C                          */
  cmd_do_ctrl_d,        /* received ^D                          */
  cmd_do_ctrl_z,        /* received ^Z                          */

  cmd_do_eof,           /* hit "EOF"                            */
  cmd_do_timed_out,     /* terminal timed out                   */

  cmd_do_count,         /* number of different cli_do_xxx       */

  cmd_do_mask        = 0x0F,

  cmd_do_auth        = 0x10,

  cmd_do_keystroke   = 0xFF,    /* special for keystroke reader */

} cmd_do_t ;

CONFIRM(cmd_do_count <= (cmd_do_mask + 1)) ;

struct cmd_context
{
  /* The node between commands and the result of the last command executed
   */
  node_type_t   node ;                  /* updated on CMD_SUCCESS       */

  node_type_t   cnode ;                 /* follows node, except for
                                         * vtysh server.                */

  /* The daemons that we are currently parsing and executing for
   */
  daemon_set_t  daemons ;

  /* Command qualifier and line, dispatched for execution.
   */
  cmd_do_t      to_do ;
  qstring       line ;

  node_type_t   vxnode ;                /* xnode for VIN_VTYSH_SERVER   */
  node_type_t   vcnode ;                /* cnode for VIN_VTYSH_SERVER   */

  /* These properties affect the parsing of command lines.
   */
  bool          full_lex ;              /* as required                  */

  bool          parse_execution ;       /* parsing to execute           */

  bool          parse_only ;            /* do not execute               */

  bool          parse_strict ;          /* no incomplete keywords       */
  bool          parse_no_do ;           /* no 'do' commands             */
  bool          parse_no_tree ;         /* no tree walking              */

  bool          can_enable ;            /* no (further) password needed */

  /* These properties affect the execution of parsed commands.
   */
  bool          reflect ;               /* per the pipe                 */
  bool          out_ordinary ;          /* per the base vin or pipe     */
  bool          out_warning ;           /* per the base vin or pipe     */
  bool          warn_stop ;             /* per the base vin or pipe     */

  /* Special for AUTH_ENABLE_NODE -- going from/to
   */
  node_type_t   onode ;                 /* VIEW_NODE or RESTRICTED_NODE */
  node_type_t   tnode ;                 /* ENABLE_NODE or CONFIG_NODE   */

  /* The current directories.
   */
  qpath         dir_cd ;                /* chdir directory              */
  qpath         dir_here ;              /* "~./" directory              */
} ;

typedef struct cmd_context  cmd_context_t ;
typedef struct cmd_context* cmd_context ;

/*==============================================================================
 * Functions in command.c
 */
extern void cmd_init_second_stage(void) ;

extern const char* cmd_host_name(bool fresh) ;

extern bool cmd_password_check(password_t* password, const char* candidate) ;

extern const char* cmd_prompt(node_type_t node) ;

extern daemon_set_t cmd_daemons_from_list(qstring qs) ;
extern qstring cmd_daemons_make_list(qstring qs, daemon_set_t daemons) ;

extern daemon_set_t cmd_node_daemons(node_type_t node) ;
extern bool cmd_node_is_installed(node_type_t node) ;
extern bool cmd_node_is_executable(node_type_t node) ;
extern cmd_node cmd_get_cmd_node(node_type_t node) ;
extern const char* cmd_node_name(node_type_t node) ;
extern node_type_t cmd_node_by_name(const char* name) ;

extern bool cmd_node_is_special(node_type_t node) ;
extern bool cmd_node_is_view(node_type_t node) ;
extern bool cmd_node_is_enable(node_type_t node) ;
extern bool cmd_node_is_config(node_type_t node) ;
extern bool cmd_node_is_specific(node_type_t node) ;
extern bool cmd_node_is_config_lock(node_type_t node) ;
extern bool cmd_node_is_ecs(node_type_t node) ;
extern bool cmd_node_is_cs(node_type_t node) ;
extern bool cmd_node_is_ancestor(node_type_t node, node_type_t child) ;
extern bool cmd_node_is_decendant(node_type_t child, node_type_t node) ;

extern node_type_t cmd_node_parent(node_type_t node) ;
extern node_type_t cmd_node_exit_to(node_type_t node) ;
extern node_type_t cmd_node_end_to(node_type_t node) ;
extern node_type_t cmd_node_config_parent(node_type_t node) ;
extern node_type_t cmd_node_enable_parent(node_type_t node) ;

extern node_type_t cmd_node_restore(node_type_t old, node_type_t new) ;

extern daemon_set_t cmd_deamon_list_arg(vty vty, int argc, argv_t argv) ;

/*==============================================================================
 * Globals for who we are
 */
daemon_set_t daemons_set ;
const char*  daemon_name ;

#endif /* _ZEBRA_COMMAND_LOCAL_H */

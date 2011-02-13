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
  char*      password;
  bool       password_encrypted ;

  /* Enable password                                    */
  char*      enable;
  bool       enable_encrypted ;

  /* System wide terminal lines default                 */
  int        lines;

  /* Log filename.                                      */
  char*      logfile;

  /* config file name of this host                      */
  char*      config_file ;

  /* Flags for services                                 */
  bool       advanced;
  bool       encrypt;

  /* Banner configuration.                              */
  const char* motd;
  char*      motdfile;

  /* Someone has the config symbol of power             */
  bool       config ;

  /* Allow VTY to start without password                */
  bool       no_password_check ;

  /* Restrict unauthenticated logins?                   */
  bool       restricted_mode ;

  /* Vty timeout value -- see "exec timeout" command    */
  unsigned long vty_timeout_val ;

  /* Vty access-class command                           */
  char*      vty_accesslist_name ;

  /* Vty access-class for IPv6.                         */
  char*      vty_ipv6_accesslist_name ;

  /* Current directory -- initialised in vty_init()     */
  char*      vty_cwd ;
} ;

enum
{
  restricted_mode_default = false,
} ;

/* Structure declared in command.c                                      */
extern struct host host ;

/*------------------------------------------------------------------------------
 * Command qualifiers
 */
enum cmd_do
{
  cmd_do_nothing  = 0,  /* no action required                   */

  cmd_do_command  = 1,  /* dispatch the current command line    */

  cmd_do_ctrl_c,        /* received ^c                          */
  cmd_do_ctrl_d,        /* received ^d                          */
  cmd_do_ctrl_z,        /* received ^z                          */

  cmd_do_eof,           /* hit "EOF"                            */

  cmd_do_count,         /* number of different cli_do_xxx       */

  cmd_do_mask        = 0x0F,
  cmd_do_auth        = 0x10,
  cmd_do_auth_enable = 0x20,
} ;

CONFIRM(cmd_do_count <= (cmd_do_mask + 1)) ;

typedef enum cmd_do cmd_do_t ;

/*------------------------------------------------------------------------------
 * Vector of nodes -- defined in command.c, declared here so the parser can
 * reach it.
 */
extern vector node_vector ;

/*----------------------------------------------------------------------------*/

#define MSG_CMD_ERR_AMBIGUOUS      "Ambiguous command"
#define MSG_CMD_ERR_NO_MATCH       "Unrecognised command"
#define MSG_CMD_ERR_NO_MATCH_old   "There is no matched command"

/*==============================================================================
 * Functions in command.c
 */

extern const char* cmd_host_name(bool fresh) ;
extern char *host_config_file(void);
extern void host_config_set(const char* file_name);

extern const char* cmd_prompt(node_type_t node) ;

extern node_type_t cmd_node_parent(node_type_t node) ;
extern node_type_t cmd_node_exit_to(node_type_t node) ;
extern node_type_t cmd_node_end_to(node_type_t node) ;

#endif /* _ZEBRA_COMMAND_LOCAL_H */

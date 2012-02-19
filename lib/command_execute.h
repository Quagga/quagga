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

#ifndef _ZEBRA_COMMAND_EXECUTE_H
#define _ZEBRA_COMMAND_EXECUTE_H

#include "command_local.h"
#include "command_parse.h"
#include "vty_common.h"
#include "qstring.h"
#include "mqueue.h"
#include "qpnexus.h"
#include "thread.h"

/*==============================================================================
 * This is stuff which is used to parse and then execute commands.
 */

/* State of the execution loop
 */
enum cmd_exec_state
{
  exec_null   = 0,      /* not started, yet                     */
  exec_fetch,           /* fetch command line                   */
  exec_open_pipes,      /* open pipes on command line           */
  exec_execute,         /* execute standard command             */
  exec_special,         /* execute special command              */
  exec_cmd_complete,    /* command completed                    */
  exec_hiatus,          /* while issues are dealt with          */
  exec_stopped,         /* command loop has stopped             */
} ;
typedef enum cmd_exec_state cmd_exec_state_t ;

struct cmd_exec
{
  vty           vty ;           /* parent                       */

  cmd_context   context ;       /* current context at all times */

  cmd_parsed    parsed ;        /* parsing and its result       */

  uint    password_failures ;   /* AUTH_NODE & AUTH_ENABLE_NODE */

  cmd_exec_state_t  state ;     /* for cq_process               */
  qpn_nexus         locus ;     /* for cq_process               */

  cmd_ret_t ret ;       /* for cq_process               */

  union
  {
    mqueue_block    mqb ;       /* for cq_process               */
    struct thread*  thread ;
  } cq ;
} ;

typedef struct cmd_exec cmd_exec_t ;

/*==============================================================================
 * Functions
 */
extern cmd_exec cmd_exec_new(vty vty, cmd_context context) ;
extern cmd_exec cmd_exec_free(cmd_exec exec) ;

extern cmd_ret_t cmd_read_config(vty vty, uint* warnings) ;
extern cmd_ret_t cmd_open_pipes(vty vty) ;

extern cmd_ret_t cmd_execute(vty vty) ;

extern cmd_context cmd_context_new(void) ;
extern void cmd_context_init(cmd_context context, vty_type_t type,
                                                  node_type_t node) ;
extern void cmd_context_config_set(cmd_context context, bool ignore_warnings,
                                                        bool show_warnings) ;
extern cmd_context cmd_context_save(cmd_context tos) ;
extern cmd_context cmd_context_restore(cmd_context tos, cmd_context saved) ;

extern cmd_context cmd_context_copy(cmd_context dst, cmd_context src) ;
extern cmd_context cmd_context_free(cmd_context context) ;

/*==============================================================================
 * Inlines
 */

Inline bool
cmd_is_direct(cmd_parsed parsed)
{
  return ((parsed->cmd->attr & CMD_ATTR_DIRECT) != 0) ;
} ;

#endif /* _ZEBRA_COMMAND_EXECUTE_H */

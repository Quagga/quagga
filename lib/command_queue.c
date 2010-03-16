/* Command Message Queue
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <zebra.h>

#include "mqueue.h"
#include "qpnexus.h"
#include "memory.h"
#include "command_queue.h"

/*------------------------------------------------------------------------------
 * Form of message passed with command to be executed
 */

struct cq_command_args
{
  qpn_nexus ret_nexus ;

  struct cmd_element *cmd ;

  enum node_type  cnode ;               /* vty->node before execution   */
  enum node_type  onode ;               /* vty->node before "do"        */

  short int       do_shortcut ;         /* true => is "do" command      */

  short int       argc ;                /* count of arguments           */
  short int       ret ;                 /* return code                  */
} ;
MQB_ARGS_SIZE_OK(cq_command_args) ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void cq_action(mqueue_block mqb, mqb_flag_t flag);
static void cq_return(mqueue_block mqb, mqb_flag_t flag);

/*------------------------------------------------------------------------------
 * Enqueue vty and argv[] for execution in given nexus.
 */
void
cq_enqueue(struct vty *vty, struct cmd_parsed* parsed, qpn_nexus to_nexus,
                                                       qpn_nexus from_nexus)
{
  int i;
  struct cq_command_args* args ;

  mqueue_block mqb = mqb_init_new(NULL, cq_action, vty) ;
  args = mqb_get_args(mqb) ;

  args->cmd         = parsed->cmd ;
  args->cnode       = parsed->cnode ;
  args->onode       = parsed->onode ;
  args->do_shortcut = parsed->do_shortcut ;
  args->argc        = parsed->argc ;

  args->ret_nexus  = from_nexus ;
  args->ret        = CMD_SUCCESS ;

  for (i = 0; i < parsed->argc; ++i)
    mqb_push_argv_p(mqb, XSTRDUP(MTYPE_MARSHAL, parsed->argv[i]));

  mqueue_enqueue(to_nexus->queue, mqb, 0) ;
}

/*------------------------------------------------------------------------------
 * Dispatch a command from the message queue block
 *
 * When done (or revoked/deleted) return the message, so that the sender knows
 * that the command has been dealt with (one way or another).
 */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  struct vty *vty;
  struct cq_command_args* args ;

  vty  = mqb_get_arg0(mqb);
  args = mqb_get_args(mqb) ;

  if (flag == mqb_action)
    {
      const char** argv = mqb_get_argv(mqb) ;

      args->ret = (args->cmd->func)(args->cmd, vty, args->argc, argv) ;
    }
  else
    args->ret = CMD_QUEUED ;

  mqb_set_action(mqb, cq_return) ;
  mqueue_enqueue(args->ret_nexus->queue, mqb, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Accept return from command executed in another thread.
 *
 * The command line processing for the vty may be stalled (with read mode
 * disabled) waiting for the return from the command.
 *
 * If the message is being revoked/deleted the state of the vty is still
 * updated (to show that the command has completed) BUT nothing is kicked.
 * It is up to the revoke/delete function to deal with any possibility of the
 * vty remaining stalled.
 */
static void
cq_return(mqueue_block mqb, mqb_flag_t flag)
{
  struct vty *vty ;
  struct cq_command_args* args ;
  int    i ;
  void** argv ;
  struct cmd_parsed parsed ;

  vty  = mqb_get_arg0(mqb) ;
  args = mqb_get_args(mqb) ;

  /* clean up                                                           */
  argv = mqb_get_argv(mqb) ;

  for (i = 0; i < args->argc; ++i)
    XFREE(MTYPE_MARSHAL, argv[i]);

  /* signal end of command -- passing the action state                  */
  parsed.cmd         = args->cmd ;
  parsed.cnode       = args->cnode ;
  parsed.onode       = args->onode ;
  parsed.do_shortcut = args->do_shortcut ;
  parsed.argc        = 0 ;
  cmd_post_command(vty, &parsed, args->ret, (flag == mqb_action)) ;

  /* update the state of the vty -- passing the "action" state          */
  vty_queued_result(vty, args->ret, (flag == mqb_action));

  if (qpthreads_enabled)
    qpt_thread_signal(vty_cli_nexus->thread_id, SIGMQUEUE);

  mqb_free(mqb);
}

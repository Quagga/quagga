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
#include "command_execute.h"
#include "vty.h"
#include "uty.h"
#include "vector.h"
#include "qstring.h"

/*------------------------------------------------------------------------------
 * Form of message passed with command to be executed
 */

struct cq_command_args
{
  enum cmd_return_code  ret ;   /* return code from command     */
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
cq_enqueue(struct vty *vty, qpn_nexus dst)
{
  struct cq_command_args* args ;

  mqueue_block mqb = mqb_init_new(NULL, cq_action, vty) ;
  args = mqb_get_args(mqb) ;

  args->ret         = CMD_QUEUED ;

  mqueue_enqueue(dst->queue, mqb, 0) ;
}

/*------------------------------------------------------------------------------
 * Dispatch a command from the message queue block
 *
 * When done (or revoked/deleted) return the message, so that the sender knows
 * that the command has been dealt with (one way or another).
 *
 * Note that if the command is revoked the return is set to CMD_QUEUED.
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
      args->ret = cmd_dispatch_call(vty) ;
      assert(args->ret != CMD_QUEUED) ;   /* avoid confusion !  */
    }
  else
    args->ret = CMD_QUEUED ;

  mqb_set_action(mqb, cq_return) ;
  mqueue_enqueue(vty_cli_nexus->queue, mqb, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Accept return from command which has completed.
 *
 * The command line processing for the vty may be stalled (with read mode
 * disabled) waiting for the return from the command.
 *
 * Do not care whether the message is being revoked or not... the command
 * has completed and that must be signalled to the CLI.  Any pending output
 * is released.
 *
 * The command itself may have been revoked before it was executed.  That
 * makes no difference either... the output buffers will simply be empty.
 * However, the return code is CMD_QUEUED, to signal the fact that the command
 * was never executed.
 */
static void
cq_return(mqueue_block mqb, mqb_flag_t flag)
{
  struct vty *vty ;
  struct cq_command_args* args ;

  vty  = mqb_get_arg0(mqb) ;
  args = mqb_get_args(mqb) ;

  /* signal end of command                                              */
  cmd_post_command(vty, args->ret) ;
  vty_queued_result(vty, args->ret) ;

//if (qpthreads_enabled)
//  qpt_thread_signal(vty_cli_nexus->thread_id, SIGMQUEUE);

  mqb_free(mqb);
}

/*------------------------------------------------------------------------------
 * Revoke any messages related to the given VTY
 *
 * Revokes in vty_cmd_nexus -- so before command is started
 *     and in vty_cli_nexus -- so after command has completed
 *
 * Can do nothing about any command actually being executed in the
 * vty_cmd_nexus.
 */
void
cq_revoke(struct vty *vty)
{
  mqueue_revoke(vty_cmd_nexus->queue, vty) ;
  mqueue_revoke(vty_cli_nexus->queue, vty) ;
}


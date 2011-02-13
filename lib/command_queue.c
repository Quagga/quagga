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

#include "zconfig.h"            /* for CONSUMED_TIME_CHECK      */
#include "misc.h"

#include "qpnexus.h"
#include "mqueue.h"
#include "thread.h"

#include "command_queue.h"
#include "command_execute.h"
#include "vty_command.h"
#include "vty_io.h"
#include "log.h"

/*==============================================================================
 * This command loop processes commands for VTY_TERMINAL and VTY_SHELL_SERVER.
 *
 * Commands appear from those sources, driven by socket I/O.  Once a command
 * line is ready it is passed out of the I/O pselect/select driven code
 * as a message, and enters the command loop.
 *
 * If pipes are involved that is all dealt with in the command loop, which
 * runs until the vin/vout stacks return to 0 -- the VTY_TERMINAL and
 * VTY_SHELL_SERVER.
 *
 * There are further issues:
 *
 *   1) in the qpthreads world, commands are parsed in the CLI thread, but most
 *      are executed in the Routing thread.  So parsed commands are passed, by
 *      message between threads.
 *
 *   2) input is expected to be non-blocking -- so the command loop will
 *      exit if a command line cannot be delivered immediately, and will be
 *      returned to later.
 *
 *   3) while a VTY is in the command loop it is marked vio->cmd_running.
 *
 *      While that is true, the vty and the vty->exec are in the hands
 *      of the command loop.  The vty->vio is always accessed under VTY_LOCK().
 *
 *   4) opening pipes is done in the CLI thread, in case of any possible
 *      blocking.
 *
 *   5) all output is to fifo buffers -- when output is pushed the CLI side
 *      is kicked to manage all output via pselect/select.
 *
 *      In vty_io_basic() it is possible to set read/write ready and associated
 *      timeouts while running in the CMD thread, but that too depends on the
 *      passing of messages to the CLI thread.
 *
 * The smooth running of the command handling depends on the continued
 * running of the CLI thread.
 *
 * To close a VTY must (eventually) arrange for vio->cmd_running to be cleared.
 * While a vty is vio->cmd_running, it must be in one of these states:
 *
 *   - on the vty_cli_nexus queue      (or the combined queue)
 *   - executing in the vty_cli_nexus  (or the single "both" nexus)
 *   - on the vty_cmd_nexus queue      (if different)
 *   - executing in the vty_cmd_nexus  (if different)
 *
 * Or, in the legacy threads world:
 *
 *   - on the event queue
 *   - executing
 *
 * Where there is only one pthread (and in the legacy threads world) things are
 * easy.
 *
 *
 * This revoke runs in the CLI thread, and will catch the message if it is
 * on either queue, and vty_revoke() will deal with it -- still in the CLI
 * thread.
 *
 * The command cannot be running in the CLI thread, since that is where we
 * are !
 *
 * That leaves the command running in the CMD thread.  That will run to
 * completion... the VTY may be closed in the meantime, which will shut down
 * the reading side, so the command loop will come to a halt quite quickly.
 * Note, however, that the smooth running of this process requires the CLI
 * thread and its messages to be
 *
 *
 */




/*------------------------------------------------------------------------------
 * Prototypes
 */
static void cq_enqueue(struct vty *vty, qpn_nexus dst, cmd_exec_state_t state,
                                                        cmd_return_code_t ret) ;
static void cq_action(mqueue_block mqb, mqb_flag_t flag);
static int cq_thread(struct thread* thread) ;
static void cq_process(vty vty) ;

/*------------------------------------------------------------------------------
 * Enqueue vty for parse and execution of a command.
 *
 * Sets the vio->cmd_running flag, which will be cleared only when the
 * command is completed (including any nested pipes etc.) or when the vty
 * is blocked on input or it is revoked.
 *
 * Note that from now on, exec->ret reflects the state of the return
 * code when the vty was last enqueued.
 *
 * While vio_cmd_running is set, the CLI side MUST NOT fiddle with the main
 * part of the VTY or with the vty->exec state.
 */
extern void
cq_dispatch(vty vty, cmd_do_t to_do, qstring line)
{
  VTY_ASSERT_CLI_THREAD() ;

  vty->exec->to_do = to_do ;
  vty->exec->line  = line ;
  vty->vio->cmd_running = true ;
  cq_enqueue(vty, vty_cli_nexus,
                     (to_do == cmd_do_command) ? exec_parse
                                               : exec_special, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue vty for fetching a command line from an in_pipe which has just
 * received more input.
 *
 * Sets the vio->cmd_running flag, which will be cleared only when the
 * command is completed (including any nested pipes etc.) or when the vty
 * is blocked on input or it is revoked.
 *
 * While vio_cmd_running is set, the CLI side MUST NOT fiddle with the main
 * part of the VTY or with the vty->exec state.
 */
extern void
cq_go_fetch(vty vty)
{
  VTY_ASSERT_CLI_THREAD() ;

  vty->vio->cmd_running = true ;
  cq_enqueue(vty, vty_cli_nexus, exec_fetch, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue vty for execution in given nexus or issue thread event.
 *
 * Note that preserves the return code state.
 */
static void
cq_enqueue(vty vty, qpn_nexus dst, cmd_exec_state_t state,
                                                        cmd_return_code_t ret)
{
  cmd_exec exec = vty->exec ;

  assert(vty->vio->cmd_running) ;
  assert((dst == vty_cli_nexus) || (dst == vty_cmd_nexus)) ;

  exec->locus = dst ;
  exec->state = state ;
  exec->ret   = ret ;

  if (vty_nexus)
    {
      mqueue_block mqb ;

      if ((mqb = exec->cq.mqb) == NULL)
        mqb = exec->cq.mqb = mqb_init_new(NULL, cq_action, vty) ;

      mqueue_enqueue(dst->queue, mqb, (dst == vty_cmd_nexus) ? mqb_priority
                                                             : mqb_ordinary) ;
    }
  else
    {
      assert(vty_cli_nexus == vty_cmd_nexus) ;
      exec->cq.thread = thread_add_event(vty_master, cq_thread, vty, 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the qpthreads world.
 *
 * Note that if the command is revoked....
 */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  vty   vty;

  assert(vty_nexus) ;                   /* must be running qnexus-wise  */

  vty  = mqb_get_arg0(mqb);

  assert(vty->exec->cq.mqb == mqb) ;
  assert(vty->vio->cmd_running) ;

  if (flag == mqb_action)
    return cq_process(vty) ;            /* do not touch vty on way out  */

  mqb_free(mqb) ;
  vty->exec->cq.mqb = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the legacy threads world.
 *
 * Note that if the command is revoked....
 */
static int
cq_thread(struct thread* thread)
{
  vty   vty = THREAD_ARG(thread) ;

  assert(vty->exec->cq.thread == thread) ;
  assert(vty->vio->cmd_running) ;

  vty->exec->cq.thread = NULL ;

  cq_process(vty) ;                     /* do not touch vty on way out  */

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Process command(s) queued from VTY_TERMINAL or from VTY_SHELL_SERVER.
 *
 * To get into the process loop, or to get back to it when more input has
 * arrived a message is sent:
 *
 *   cli -> cli   -- exec_parse   -- when a command line has been gathered
 *                                   from input and is ready to be processed.
 *
 *   cli -> cli   -- exec_fetch   -- when was waiting for more input from an
 *                                   in_pipe of some sort.
 *
 * In the single-pthread world, where the vty_cli_nexus is the same as the
 * vty_cmd_nexus (either because not running qpthreaded, or because are
 * running in the legacy threads world), things are reasonably straightforward.
 * The process runs to completion or until an in_pipe would block, and the
 * above are the only messages in the system.
 *
 * In the multi-pthread world, things are more complicated...  The above
 * messages are sent, and in addition the following are sent back and forth to
 * transfer between threads in the following states:
 *
 *   cmd -> cli   -- exec_open_pipes
 *   cli -> cmd   -- exec_execute
 *   cmd -> cli   -- exec_complete
 *
 * Note that this means that in the multi-pthread world, only one sort of
 * message is sent to the vty_cmd_nexus.
 *
 * The vty_io_basic stuff allows the multi-pthread vty_cmd_nexus to set read
 * and/or write ready state -- which may generate messages to the vty_cli_nexus.
 *
 * NB: if blocks in exec_fetch, then vty_cmd_fetch_line() will have cleared
 *     vio->cmd_running -- so on return from cq_process the vty MAY HAVE BEEN
 *     DELETED.
 */
static void
cq_process(vty vty)
{
  cmd_exec          exec   = vty->exec ;
  cmd_parsed        parsed = exec->parsed ;
  cmd_return_code_t ret    = exec->ret ;

  /* Have switch wrapped in a while(1) so that can change state by setting
   * exec->state and doing "continue".
   *
   * Breaking out of the switch forces the exec state to exec_complete,
   * with the current ret, in the CLI thread.
   *
   * The exec locus is either vty_cli_nexus or vty_cmd_nexus.  If these
   * are equal (including both NULL, in the legacy threads world), then is
   * running single threaded -- otherwise is running multi-threaded.
   */
  while (1)
    {
      switch(exec->state)
      {
        /*--------------------------------------------------------------------
         * Should not get here in exec_null state.
         */
        case exec_null:
          zabort("exec->state == exec_null") ;
          break ;

        /*--------------------------------------------------------------------
         * Deal with the "spacial" commands
         */
        case exec_special:
          ret = vty_cmd_special(vty) ;
          if (ret != CMD_SUCCESS)
            break ;

          exec->state = exec_fetch ;
          /* continue by falling through        */

        /*--------------------------------------------------------------------
         * Need another command to execute => in_pipe !
         *
         * Note that at vin_depth == 0 this will return CMD_EOF, and will
         * drop out of the loop exec_complete.
         *
         * Will also receive CMD_EOF if the VTY has been closed.
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_fetch_line() may set read and/or write ready -- so in
         *   vty_cmd_nexus may generate message to vty_cli_nexus.
         */
        case exec_fetch:
          ret = vty_cmd_fetch_line(vty) ;

          if (ret != CMD_SUCCESS)
            {
              /* If is CMD_WAITING, then the vty_cmd_fetch_line() will
               * have prepared for command to be re-queued when there is more
               * to be read.  NB: vio->cmd_running has been cleared, so
               * vty MAY HAVE BEEN DELETED !
               *
               * If is CMD_EOF then is "closing" or reached EOF on top-most
               * pipe.
               */
              if (ret == CMD_WAITING)
                return ;                /* <<<< DONE, pro tem           */

              break ;                   /* => exec_complete             */
            } ;

          if (exec->to_do != cmd_do_command)
            {
              exec->state = exec_special ;
              continue ;
            } ;

          exec->state = exec_parse ;
          /* continue by falling through        */

        /*--------------------------------------------------------------------
         * Parse command in hand
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_reflect_line() may set read and/or write ready -- so in
         *   vty_cmd_nexus may generate message to vty_cli_nexus.
         */
        case exec_parse:
          cmd_tokenise(parsed, exec->line) ;
          ret = cmd_parse_command(parsed, vty->node,
                                       exec->parse_type | cmd_parse_execution) ;
          if (ret != CMD_SUCCESS)
            {
              if (ret != CMD_EMPTY)
                break ;         /* stop on *any* parsing issue          */

              /* Empty lines from in_pipes we simply ignore.
               *
               * Don't expect to see them otherwise, but if we do then need
               * to complete the command execution process.
               */
              ret = CMD_SUCCESS ;

              exec->state = exec_success ;
              continue ;
            } ;

          /* reflection now -- output always succeeds                   */
          if (exec->reflect_enabled)
            vty_cmd_reflect_line(vty) ;

          /* continue by falling through        */

        /*--------------------------------------------------------------------
         * Pipe work if any
         *
         * Will receive CMD_EOF if the VTY has been closed.
         *
         * If multi-threaded: must be in vty_cli_nexus to proceed -- so may
         * generate message to transfer to vty_cli_nexus.
         */
        case exec_open_pipes:
          if ((parsed->parts & cmd_parts_pipe) != 0)
            {
              exec->state = exec_open_pipes ;

              /* If running pthreaded, do open pipes in vty_cli_nexus   */
              if (exec->locus != vty_cli_nexus)
                return cq_enqueue(vty, vty_cli_nexus, exec_open_pipes, ret) ;

              /* Now in vty_cli_nexus                                   */
              ret = cmd_open_pipes(vty) ;
              if (ret != CMD_SUCCESS)
                break ;                 /* quit if open fails           */
            } ;

          exec->state = exec_execute ;
          /* continue by falling through        */

        /*--------------------------------------------------------------------
         * Execute command in hand
         *
         * If multi-threaded: some commands can run in either thread, most must
         * run in the vty_cmd_nexus -- so may generate message to transfer to
         * the vty_cmd_nexus.
         */
        case exec_execute:
          if ((parsed->parts & cmd_part_command) != 0)
            {
              /* If running pthreaded, do most commands in vty_cmd_nexus  */
              if ((exec->locus != vty_cmd_nexus) &&
                                                       (!cmd_is_direct(parsed)))
                return cq_enqueue(vty, vty_cmd_nexus, exec_execute, ret) ;

              /* Standard command handling                              */
#ifdef CONSUMED_TIME_CHECK
              {
                  RUSAGE_T before;
                  RUSAGE_T after;
                  unsigned long realtime, cputime;

                  GETRUSAGE(&before);
#endif /* CONSUMED_TIME_CHECK */

                  ret = cmd_execute(vty) ;

#ifdef CONSUMED_TIME_CHECK
                  GETRUSAGE(&after);
                  realtime = thread_consumed_time(&after, &before, &cputime) ;
                  if (realtime > CONSUMED_TIME_CHECK)
                    /* Warn about CPU hog that must be fixed. */
                    uzlog(NULL, LOG_WARNING,
                        "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                                        realtime/1000, cputime/1000,
                                        qs_make_string(exec->line)) ;
              } ;
#endif /* CONSUMED_TIME_CHECK */

              if (ret != CMD_SUCCESS)
                break ;                 /* stop                         */
            } ;

          exec->state = exec_success ;
          /* continue by falling through        */

        /*--------------------------------------------------------------------
         * Command has completed successfully -- so push the output.
         *
         * If the vout_depth > vin_depth, pops the vout's -- deals with single
         * command lines with a pipe output.
         *
         * Output cannot block, so this always succeeds.
         *
         * Then loop back to fetch another command line, if can.
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_success() may set write ready -- so in vty_cmd_nexus may
         *   generate message to vty_cli_nexus.
         */
        case exec_success:
          assert(ret == CMD_SUCCESS) ;

          vty_cmd_success(vty) ;

          exec->state = exec_fetch ;
          continue ;

        /*--------------------------------------------------------------------
         * End of the command loop !
         *
         * If multi-threaded: must return to the vty_cli_nexus.
         */
        case exec_complete:
          if (exec->locus != vty_cli_nexus)
            break ;                     /* Will send back to the cli    */

          /* Now in the vty_cli_nexus                                   */

          exec->state = exec_null ;     /* all done !                   */

          vty_cmd_loop_exit(vty, ret) ; /* clears vio->cmd_running      */

          return ;                      /* <<<< Finally finished !      */

        /*----------------------------------------------------------------------
         * Unknown exec->state !
         */
        default:
          zabort("unknown exec->state") ;
          break ;
      } ;

      /* Have broken out of the switch().  This means that for good or ill,
       * the command is complete.  If we are not in the vty_cli_nexus, need to
       * send back to the vty_cli_nexus for handling.
       *
       * At all times we treat CMD_EOF and CMD_SUCCESS.
       *
       * Otherwise, can continue in exec_complete state.
       */
      if (ret == CMD_EOF)
        ret = CMD_SUCCESS ;

      if (exec->locus != vty_cli_nexus)
        return cq_enqueue(vty, vty_cli_nexus, exec_complete, ret) ;

      exec->state = exec_complete ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Revoke any message associated with the given vty from the command queue.
 *
 * This is used when a VTY_TERMINAL or a VTY_SHELL_SERVER is being closed.
 *
 * See cq_process above for discussion of what messages there may be.  At any
 * time there is at most one message in flight.
 *
 * If we find a message in flight, then we vty_cmd_loop_exit() to bring things
 * to a stop tidily.
 *
 * In the single-threaded world, expect that a command, once started will run
 * to conclusion, or until blocked on an in_pipe.  So after this revoke the
 * vty should not be vio->cmd_running.
 *
 * In the multi-threaded world, this revoke will catch any vty which is on
 * either the vty_cli_nexus or vty_cmd_nexus queues, and force completion.
 * After this revoke, vio->cmd_running will be true iff the command is
 * currently being executed in the vty_cmd_nexus -- we expect that to run to
 * conclusion or block on an in_pipe, shortly, which will be collected when
 * the vty_cli_nexus message queue is next processed.
 *
 * Note that the revoke does not affect any vty_cli_nexus messages associated
 * with the vty_io_basic operations.
 */
extern void
cq_revoke(vty vty)
{
  int ret ;

  VTY_ASSERT_CLI_THREAD() ;

  if (vty_nexus)
    {
      ret = mqueue_revoke(vty_cmd_nexus->queue, vty, 1) ;

      if ((ret == 0) && (vty_cli_nexus != vty_cmd_nexus))
        ret = mqueue_revoke(vty_cli_nexus->queue, vty, 1) ;
    }
  else
    ret = thread_cancel_event(vty_master, vty) ;

  if (ret != 0)
    {
      vty->exec->state = exec_null ;
      vty_cmd_loop_exit(vty, vty->exec->ret) ;
    } ;
} ;




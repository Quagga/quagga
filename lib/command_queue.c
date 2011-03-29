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
 * There are further issues:     fix to current state TODO
 *
 *   1) in the qpthreads world, commands are parsed in the CLI thread, but most
 *      are executed in the Routing thread.  So parsed commands are passed, by
 *      message between threads.
 *
 *   2) input is expected to be non-blocking -- so the command loop will
 *      exit if a command line cannot be delivered immediately, and will be
 *      returned to later.
 *
 *   3) while a VTY is in the command loop it is marked vio->cmd_running.  TODO
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
 * While a vty is vio->cmd_running, it must be in one of these states:   TODO
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
static void cq_process(vty vty, cmd_exec_state_t state, cmd_return_code_t ret) ;

/*------------------------------------------------------------------------------
 * Enqueue vty to enter the command loop -- must be exec_null
 *
 * Expects the vty start up process to have output some cheerful messages,
 * which is treated as a dummy "login" command.  So the loop is entered at
 * "exec_done_cmd", which will push out the output, deal with the return code,
 * and loop round to fetch the first command line (if required).
 */
extern void
cq_loop_enter(vty vty, cmd_return_code_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  assert(vty->exec->state == exec_null) ;

  cq_enqueue(vty, vty_cli_nexus, exec_done_cmd, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Continue (resume) at hiatus -- must be exec_hiatus
 */
extern void
cq_continue(vty vty, cmd_return_code_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  assert(vty->exec->state == exec_hiatus) ;

  cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue vty for execution in given nexus or issue thread event.
 *
 * Will execute in the current exec->state, passing in the given return
 * code.
 */
static void
cq_enqueue(vty vty, qpn_nexus dst, cmd_exec_state_t state,
                                   cmd_return_code_t ret)
{
  cmd_exec exec = vty->exec ;

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
 * Note that if the command is revoked....  TODO
 */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  vty   vty;

  assert(vty_nexus) ;                   /* must be running qnexus-wise  */

  vty  = mqb_get_arg0(mqb);

  assert(vty->exec->cq.mqb == mqb) ;

  if (flag == mqb_action)
    return cq_process(vty, vty->exec->state, vty->exec->ret) ;
                                       /* do not touch vty on way out   */

  /* Revoke action.                                                     */
  mqb_free(mqb) ;
  vty->exec->cq.mqb = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the legacy threads world.
 *
 * Note that if the command is revoked....  TODO
 */
static int
cq_thread(struct thread* thread)
{
  vty   vty = THREAD_ARG(thread) ;

  assert(vty->exec->cq.thread == thread) ;

  vty->exec->cq.thread = NULL ;

  cq_process(vty, vty->exec->state, vty->exec->ret) ;
                                            /* do not touch vty on way out  */
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Process command(s) queued from VTY_TERMINAL or from VTY_SHELL_SERVER.
 *
 * This is essentially a coroutine, where the state is in the vty, noting that:
 *
 *   vty->exec->state   is the "return address"
 *   vty->exec->ret     is the "return code"
 *
 * The command loop runs at the mqueue level in the qpnexus world, or is
 * driven by "events" in the legacy threads world.  The two ways into the
 * loop are:
 *
 *   cq_loop_enter()   -- called once and once only to start the loop.
 *
 *   cq_continue()     -- called when some I/O process has completed and
 *                        the loop is in hiatus, waiting for I/O.
 *
 * The one way out of the loop is when it hits a hiatus, which is triggered
 * by any operation not returning CMD_SUCCESS.  In hiatus, vty_cmd_hiatus()
 * is called and given the current return code to deal with.  It will
 * return:
 *
 *    CMD_SUCCESS   => can proceed to fetch the next command
 *    CMD_WAITING   => must leave the command loop, waiting for I/O
 *    CMD_EOF       => close the vty and leave command loop
 *    CMD_IO_ERROR  => loop back and deal with the error
 *
 * The cq_loop_enter() and cq_continue() operations send a message (to the
 * cli thread, in the mult-pthreaded world), whose action routine is to call
 * cq_process().
 *
 * In the multi-pthreaded world, opening and closing files MUST be done in the
 * cli thread.  Most commands MUST be executed in the cmd thread.  So the
 * command loop is passed between the threads in a number of places.  (To keep
 * life simple, switches back to the cli thread if ends up waiting for I/O.)
 *
 * The vty_io_basic stuff allows the multi-pthread vty_cmd_nexus to set read
 * and/or write ready state -- which may generate messages to the vty_cli_nexus.
 */
static void
cq_process(vty vty, cmd_exec_state_t state, cmd_return_code_t ret)
{
  cmd_exec    exec   = vty->exec ;
  cmd_parsed  parsed = exec->parsed ;

  /* Have switch wrapped in a while(1) so that can change state by setting
   * exec->state and doing "continue".
   *
   * Breaking out of the switch forces the exec state to exec_hiatus,
   * with the current ret (which will switch to the CLI thread).
   *
   * The exec locus is either vty_cli_nexus or vty_cmd_nexus.  If these
   * are equal (including both NULL, in the legacy threads world), then is
   * running single threaded -- otherwise is running multi-threaded.
   */
  while (1)
    {
     switch(state)
      {
        /*--------------------------------------------------------------------
         * Should not get here in exec_null state.
         */
        case exec_null:
          zabort("exec state == exec_null") ;
          break ;

        /*--------------------------------------------------------------------
         * Need another command to execute => in_pipe !
         *
         * If multi-threaded: may be in either thread.  If vty_cmd_fetch_line()
         * cannot, for any reason, return a command line, will return something
         * other than CMD_SUCCESS -- which enters the exec_hiatus.
         */
        case exec_fetch:
          ret = vty_cmd_fetch_line(vty) ;

          if (ret != CMD_SUCCESS)
            break ;

          if (exec->action->to_do != cmd_do_command)
            {
              state = exec_special ;
              continue ;
            } ;

          fall_through ;                /* with ret == CMD_SUCCESS      */

        /*--------------------------------------------------------------------
         * Parse command in hand
         *
         * If multi-threaded: may be in either thread.
         */
          cmd_tokenize(parsed, exec->action->line, exec->context->full_lex) ;

          ret = cmd_parse_command(parsed, exec->context) ;
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

              state = exec_done_cmd ;
              continue ;
            } ;

          /* reflection now                                             */
          if (exec->reflect)
            {
              ret = vty_cmd_reflect_line(vty) ;

              if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
                break ;
            } ;

          fall_through ;

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
              /* If running pthreaded, do open pipes in vty_cli_nexus   */
              if (exec->locus != vty_cli_nexus)
                return cq_enqueue(vty, vty_cli_nexus, exec_open_pipes, ret) ;

              /* Now in vty_cli_nexus                                   */
              ret = cmd_open_pipes(vty) ;
              if (ret != CMD_SUCCESS)
                break ;                 /* quit if open fails           */
            } ;

          fall_through ;                /* with ret == CMD_SUCCESS      */

        /*--------------------------------------------------------------------
         * Execute command in hand (if any)
         *
         * If multi-threaded: some commands can run in either thread, most must
         * run in the vty_cmd_nexus -- so may generate message to transfer to
         * the vty_cmd_nexus.
         */
        case exec_execute:
          if ((parsed->parts & cmd_part_command) != 0)
            {
              /* If running pthreaded, do most commands in vty_cmd_nexus  */
              if ((exec->locus != vty_cmd_nexus) && (!cmd_is_direct(parsed)))
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
                    zlog(NULL, LOG_WARNING,
                        "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                                        realtime/1000, cputime/1000,
                                        qs_make_string(exec->action->line)) ;
              } ;
#endif /* CONSUMED_TIME_CHECK */
            } ;

          fall_through ;

        /*--------------------------------------------------------------------
         * Command has completed -- if successful, push output and loop back
         * to fetch another command.
         *
         * Break if not successful, or push fails or must wait.
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_success() may set write ready -- so in vty_cmd_nexus may
         *   generate message to vty_cli_nexus.
         */
        case exec_done_cmd:
          if (ret == CMD_SUCCESS)
            ret = vty_cmd_success(vty) ;

          if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
            break ;                     /* stop                         */

          state = exec_fetch ;
          continue ;

        /*--------------------------------------------------------------------
         * Deal with the "special" commands
         */
        case exec_special:
          if (exec->locus != vty_cli_nexus)
            return cq_enqueue(vty, vty_cli_nexus, exec_special, ret) ;

          ret = vty_cmd_special(vty) ;
          if (ret != CMD_SUCCESS)
            break ;

          state = exec_done_cmd ;
          continue ;

        /*--------------------------------------------------------------------
         * Hiatus state -- some return code to be dealt with !
         */
        case exec_hiatus:
          if (exec->locus != vty_cli_nexus)
            return cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;

          while (1)
            {
              /* Let vty_cmd_hiatus() deal with the return code, and adjust
               * the stack as required.
               */
              ret = vty_cmd_hiatus(vty, ret) ;

              if (ret == CMD_SUCCESS)
                break ;

              if (ret == CMD_WAITING)
                {
                  exec->state = exec_hiatus ;
                  return ;              /* <<< DONE, pro tem            */
                } ;

              if (ret == CMD_EOF)
                {
                  exec->state = exec_stopped ;
                  vty_cmd_loop_exit(vty) ;

                  return ;              /* <<< DONE, permanently        */
                } ;

              if (ret == CMD_IO_ERROR)
                continue ;

              zabort("invalid return from vty_cmd_hiatus()") ;
            } ;

          state = exec_fetch ;
          continue ;                    /* can fetch, again             */

        /*--------------------------------------------------------------------
         * Should not get here in exec_stopped state.
         */
        case exec_stopped:
          zabort("exec state == exec_exit") ;
          break ;

        /*----------------------------------------------------------------------
         * Unknown exec->state !
         */
        default:
          zabort("unknown exec->state") ;
          break ;
      } ;

      /* Have broken out of the switch() => exec_hiatus                 */
      state = exec_hiatus ;
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
 * In the single-threaded world (including legacy threads), messages or events
 * are used to enter or resume the command loop.  If a message or event can
 * be revoked, then the command loop is effectively stopped.
 *
 * In the multi-threaded world, messages are used to enter or resume the
 * command loop, and to transfer it to and from the cli and cmd threads.  If
 * a message can be revoked, the command loop is effectively stopped.
 *
 * Note that the revoke does not affect any vty_cli_nexus messages associated
 * with the vty_io_basic operations.
 *
 * Returns:  true <=> have revoked a pending message/event
 */
extern bool
cq_revoke(vty vty)
{
  int ret ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  if (vty_nexus)
    {
      ret = mqueue_revoke(vty_cli_nexus->queue, vty, 1) ;

      if ((ret == 0) && vty_multi_nexus)
        ret = mqueue_revoke(vty_cmd_nexus->queue, vty, 1) ;
    }
  else
    ret = thread_cancel_event(vty_master, vty) ;

  /* If revoked message/event, the command loop is stopped.             */
  if (ret != 0)
    vty->exec->state = exec_stopped ;

  return (ret != 0) ;
} ;




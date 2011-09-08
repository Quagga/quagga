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
 * The command loop is a form of co-routine, which is "called" by sending a
 * message (or by legacy threads events).  In the multi-pthread world, the
 * command loop may run in either the vty_cli_nexus or the vty_cmd_nexus, and
 * may be passed from one to the other by sending a message.
 *
 * The command loop is usually in one of three states:
 *
 *   vc_running   -- the command loop is running, doing things.
 *
 *                   In this state the vty  and the vty->exec are in the hands
 *                   of the command loop.
 *
 *                   The vty->vio is always accessed under VTY_LOCK().
 *
 *                   Note that one of the things the command loop may be
 *                   doing is waiting for a message to be picked up (or
 *                   a legacy thread event to be dispatched).
 *
 *   vc_waiting   -- the command loop is waiting for something to happen
 *                   (typically some I/O), and will stay there until a
 *                   message is sent (or a legacy threads event).
 *
 *                   To be waiting the command loop must have entered "hiatus"
 *                   and then exited.
 *
 * When a command loop is or has been stopped, to goes to vc_stopped state.
 *
 * There are further issues:
 *
 *   * In the mult-pthreaded world, commands may be parsed in either pthread,
 *     but most are executed in the Routing thread.  So may switch pthreads
 *     after parsing a command -- by sending a message.
 *
 *   * opening pipes is done in the vty_cli_nexus, in case of any possible
 *     blocking and because the pselect() stuff is all done in the
 *     vty_cli_nexus.
 *
 *   * all output is via fifo buffers -- when output is pushed, as much as
 *     possible is done immediately, but if necessary the pselect() process
 *     (in the vty_cli_nexus) is left to keep things moving.
 *
 *     In vty_io_basic() it is possible to set read/write ready and associated
 *     timeouts while running in the vty_cmd_nexus, but note that this is done
 *     by sending messages to the CLI thread (if multi-pthreaded).
 *
 * The smooth running of the command handling depends on the continued
 * running of the CLI thread.
 *
 * To close a VTY must (eventually) arrange for the state to not be vc_running.
 * While a vty is vc_running, the command loop may be in either nexus, and
 * may be:
 *
 *   - on the vty_cli_nexus queue                  (or the combined queue)
 *   - executing cq_process() in the vty_cli_nexus (or the single "both" nexus)
 *   - on the vty_cmd_nexus queue                  (if different)
 *   - executing cq_process() in the vty_cmd_nexus (if different)
 *
 * where being on the queue means that there is a message waiting to be
 * picked up on that queue, which will end up calling cq_process().
 *
 * Or, in the legacy threads world:
 *
 *   - on the event queue  -- waiting for event handler to call cq_process()
 *   - executing cq_process()
 *
 * To bring the command loop to a stop will call cq_revoke to revoke any
 * pending message or pending legacy event.  If that succeeds, then the
 * command loop can be stopped immediately.  If not, then vc_running implies
 * it is executing in cq_process() in one of the nexuses.
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
 * "exec_cmd_done", which will deal with the return code, push any output, and
 * loop round to fetch the first command line (if required).
 */
extern void
cq_loop_enter(vty vty, cmd_return_code_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  qassert(vty->exec->state == exec_null) ;

  cq_enqueue(vty, vty_cli_nexus, exec_cmd_done, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Continue (resume) at hiatus -- must be exec_hiatus
 */
extern void
cq_continue(vty vty, cmd_return_code_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  qassert(vty->exec->state == exec_hiatus) ;

  cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue vty for execution in given nexus or issue thread event.
 *
 * Will execute in the current exec->state, passing in the given return
 * code.
 *
 * Note that the vio->state *must* be vc_running.
 */
static void
cq_enqueue(vty vty, qpn_nexus dst, cmd_exec_state_t state,
                                   cmd_return_code_t ret)
{
  cmd_exec exec = vty->exec ;

  qassert((dst == vty_cli_nexus) || (dst == vty_cmd_nexus)) ;

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
      qassert(vty_cli_nexus == vty_cmd_nexus) ;
      exec->cq.thread = thread_add_event(vty_master, cq_thread, vty, 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the qpthreads world.
 *
 * Note that if the message is revoked, then the state is set vc_stopped.
 * We revoke when stopping a command loop -- see cq_revoke, below.  If the
 * message is revoked at any other time then the command loop is, indeed,
 * stopped -- the I/O side may plow on until buffers empty, but the
 * command loop will not cq_continue().  At
 */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  vty   vty;

  assert(vty_nexus) ;                   /* must be running qnexus-wise  */

  vty  = mqb_get_arg0(mqb);

  qassert(vty->exec->cq.mqb == mqb) ;

  if (flag == mqb_action)
    cq_process(vty, vty->exec->state, vty->exec->ret) ;
                                       /* do not touch vty on way out   */
  else
    {
      /* Revoke action.                                                 */
      mqb_free(mqb) ;
      vty->exec->cq.mqb = NULL ;

      vty_cmd_set_stopped(vty) ;        /* enforced stop of loop        */
    } ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the legacy threads world.
 */
static int
cq_thread(struct thread* thread)
{
  vty   vty = THREAD_ARG(thread) ;

  qassert(vty->exec->cq.thread == thread) ;

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
 *    CMD_STOP      => close the vty and leave command loop
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
 * and/or write ready state -- which may generate messages to the
 * vty_cli_nexus.
 *
 * NB: on exit from cq_process the VTY may have been closed down and released,
 *     so do NOT depend on its existence.
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
            return ;

          /*--------------------------------------------------------------------
           * Hiatus state -- some return code to be dealt with !
           *
           * NB: we only reach here after breaking out of the switch, or
           *     on cq_continue().
           */
          case exec_hiatus:
            while (1)
              {
                /* Let vty_cmd_hiatus() deal with the return code and/or any
                 * stop/error/etc trap, and adjust the stack as required.
                 */
                ret = vty_cmd_hiatus(vty, ret) ;

                if (ret == CMD_SUCCESS)
                  break ;                 /* back to exec_fetch           */

                if (ret == CMD_WAITING)
                  return ;                /* <<< DONE, pro tem            */

                if (ret == CMD_IO_ERROR)
                  continue ;              /* give error back to hiatus    */

                if (ret == CMD_STOP)
                  {
                    exec->state = exec_stopped ;
                    vty_cmd_loop_exit(vty) ;

                    return ;              /* <<< DONE, permanently        */
                  } ;

                zabort("invalid return from vty_cmd_hiatus()") ;
              } ;

            fall_through ;

          /*--------------------------------------------------------------------
           * Need another command to execute => in_pipe !
           *
           * If multi-threaded: may be in either thread.
           *
           * If vty_cmd_fetch_line() cannot, for any reason, return a command
           * line, will return something other than CMD_SUCCESS -- which enters
           * the exec_hiatus (transferring to vty_cli_nexus).
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

            /* Parse command in hand
             *
             * If multi-threaded: may be in either thread.
             */
            cmd_tokenize(parsed, exec->action->line, exec->context->full_lex) ;

            ret = cmd_parse_command(parsed, exec->context) ;
            if (ret != CMD_SUCCESS)
              break ;                   /* on *any* parsing issue       */

            if ((parsed->parts & cmd_parts_execute) == 0)
              {
                /* Empty lines from in_pipes appear here.
                 *
                 * Don't expect to see them otherwise, but in any case need to
                 * complete the command execution process.
                 */
                state = exec_cmd_success ;
                continue ;
              } ;

            /* reflect if required -- CMD_WAITING is fine, let the output side
             *                        deal with that.
             */
            if (exec->reflect)
              {
                ret = vty_cmd_reflect_line(vty) ;

                if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
                  break ;               /* CMD_IO_ERROR or CMD_HIATUS   */
              } ;

          /*--------------------------------------------------------------------
           * Pipe work if any
           *
           * Will receive CMD_EOF if the VTY has been closed.
           *
           * If multi-threaded: must be in vty_cli_nexus to proceed.
           */
          case exec_open_pipes:
            if ((parsed->parts & cmd_parts_pipe) != 0)
              {
                /* If running pthreaded, do open pipes in vty_cli_nexus
                 */
                if (exec->locus != vty_cli_nexus)
                  return cq_enqueue(vty, vty_cli_nexus, exec_open_pipes, ret) ;

                /* Now in vty_cli_nexus
                 */
                ret = cmd_open_pipes(vty) ;
                if (ret != CMD_SUCCESS)
                  break ;                 /* quit if open fails           */

                /* Finish if this is pipe only
                 */
                if ((parsed->parts & cmd_part_command) == 0)
                  {
                    state = exec_cmd_success ;
                    continue ;
                  } ;
              } ;

            fall_through ;

          /*--------------------------------------------------------------------
           * If multi-threaded: most commands run in the vty_cmd_nexus, some
           * run in the vty_cli_nexus -- transfer to the required nexus, as
           * required.
           */
            qassert((parsed->parts & cmd_part_command) != 0);

            if (cmd_is_direct(parsed))
              {
                if (exec->locus != vty_cli_nexus)
                  return cq_enqueue(vty, vty_cli_nexus, exec_execute, ret) ;
              }
            else
              {
                if (exec->locus != vty_cmd_nexus)
                  return cq_enqueue(vty, vty_cmd_nexus, exec_execute, ret) ;
              }

            fall_through ;

          /*--------------------------------------------------------------------
           * Execute command -- now that is in the required nexus.
           */
          case exec_execute:
#ifdef CONSUMED_TIME_CHECK
            {
              RUSAGE_T before;
              RUSAGE_T after;
              unsigned long realtime, cputime;

              if (!cmd_is_direct(parsed))
                GETRUSAGE(&before);
#endif /* CONSUMED_TIME_CHECK */

            ret = cmd_execute(vty) ;

#ifdef CONSUMED_TIME_CHECK
              if (!cmd_is_direct(parsed))
                {
                  GETRUSAGE(&after);
                  realtime = thread_consumed_time(&after, &before, &cputime) ;
                  if (realtime > CONSUMED_TIME_CHECK)
                    /* Warn about CPU hog that must be fixed. */
                    zlog(NULL, LOG_WARNING,
                      "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                                        realtime/1000, cputime/1000,
                                        qs_make_string(exec->action->line)) ;
                } ;
            } ;
#endif /* CONSUMED_TIME_CHECK */

        /*--------------------------------------------------------------------
         * Command has completed somehow -- this is the loop entry point.
         */
        case exec_cmd_done:
          if (ret != CMD_SUCCESS)
            break ;

          fall_through ;

        /*--------------------------------------------------------------------
         * Command has completed successfully, push output and loop back
         * to fetch another command.
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_success() may set write ready -- so in vty_cmd_nexus may
         *   generate message to vty_cli_nexus.
         */
        case exec_cmd_success:
          qassert(ret == CMD_SUCCESS) ;

          ret = vty_cmd_success(vty) ;  /* CMD_WAITING is fine, let the
                                         * output side deal with that.  */

          if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
            break ;

          state = exec_fetch ;
          continue ;

        /*--------------------------------------------------------------------
         * Deal with the "special" commands
         */
        case exec_special:
          if (exec->locus != vty_cli_nexus)
            return cq_enqueue(vty, vty_cli_nexus, exec_special, ret) ;

          ret = vty_cmd_special(vty) ;

          state = exec_cmd_done ;
          continue ;

        /*--------------------------------------------------------------------
         * Should not get here in exec_stopped state.
         */
        case exec_stopped:
          zabort("exec state == exec_exit") ;
          return ;

        /*----------------------------------------------------------------------
         * Unknown exec->state !
         */
        default:
          zabort("unknown exec->state") ;
          return ;
      } ;

     /* Have broken out of the switch() => exec_hiatus
      *
      * Something has returned a return code that causes entry to the hiatus
      * state to sort out.
      *
      * If we are not in the vty_cli_nexus, then must pass the problem
      * to the vty_cli_nexus.  If the return code is CMD_STOP, or there
      * is a CMD_STOP signal, then drop the config symbol of power
      * immediately -- see uty_close().
      */
     if (exec->locus != vty_cli_nexus)
       {
         vty_cmd_check_stop(vty, ret) ;
         return cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;
       } ;

     state = vty->exec->state = exec_hiatus ;

     continue ;
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
 * If succeeds in revoking, then will have set the state to vc_stopped, and
 * will have released the message block or legacy thread object.
 *
 * Returns:  true <=> have revoked a pending message/event
 *
 * NB: if no command loop has ever been started for this vty, then will not
 *     find anything to revoke.
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
    {
      ret = thread_cancel_event(vty_master, vty) ;

      if (ret != 0)
        {
          /* Make sure in same state as after mqueue_revoke.            */
          vty_cmd_set_stopped(vty) ;
          vty->exec->cq.thread = NULL ;
        } ;
    } ;

  /* If revoked message/event, the command loop is stopped.             */
  if (ret != 0)
    vty->exec->state = exec_stopped ;

  return (ret != 0) ;
} ;




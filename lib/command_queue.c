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
#include "log.h"

/*==============================================================================
 * This command loop processes commands for VTY_TERMINAL and VTY_VTYSH_SERVER.
 *
 * The command loop is a form of co-routine, which is "called" by sending a
 * message (or by legacy threads events).  In the multi-pthread world, the
 * command loop may run in either the vty_cli_nexus or the vty_cmd_nexus, and
 * may be passed from one to the other by sending a message.
 *
 * This command loop is usually in one of:
 *
 *   vcl_cq_running -- the command loop is running, doing things.
 *
 *                    In this state the vty  and the vty->exec are in the hands
 *                    of the command loop.
 *
 *                    The vty->vio is always accessed under VTY_LOCK().
 *
 *                    Note that one of the things the command loop may be
 *                    doing is waiting for a message to be picked up (or
 *                    a legacy thread event to be dispatched).
 *
 *   vcl_cq_waiting -- the command loop is waiting for something to happen
 *                    (typically some I/O), and will stay there until a
 *                    message is sent (or a legacy threads event).
 *
 *                    To be waiting the command loop must have entered "hiatus"
 *                    and then exited.
 *
 * When a command loop is or has been stopped, to goes to vcl_stopped state.
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
 * To close a VTY must (eventually) arrange for the state to not be
 * vcl_cq_running.  While a vty is vcl_cq_running, the command loop may be in
 * either nexus, and may be:
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
 * command loop can be stopped immediately.  If not, then vcl_cq_running implies
 * it is executing in cq_process() in one of the nexuses.
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static cmd_exec_state_t cq_enqueue(struct vty *vty, qpn_nexus dst,
                                        cmd_exec_state_t state, cmd_ret_t ret) ;
static void cq_action(mqueue_block mqb, mqb_flag_t flag);
static int cq_thread(struct thread* thread) ;
static cmd_exec_state_t cq_process(vty vty) ;

/*------------------------------------------------------------------------------
 * Enqueue vty to enter the command loop -- must be exec_null
 *
 * Expects the vty start up process to have output some cheerful messages,
 * which is treated as a dummy "login" command.  So the loop is entered at
 * "exec_cmd_complete", which will deal with the return code, push any output,
 * and loop round to fetch the first command line (if required).
 *
 * The incoming return code may be CMD_SUCCESS, CMD_WARNING or CMD_ERROR.
 */
extern void
cq_loop_enter(vty vty, cmd_ret_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  qassert(vty->exec->state == exec_null) ;

  vty->exec->locus = vty_cli_nexus ;

  if (vty_nexus && (vty->exec->cq.mqb == NULL))
    vty->exec->cq.mqb = mqb_init_new(NULL, cq_action, vty) ;

  cq_enqueue(vty, vty_cli_nexus, exec_cmd_complete, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Resume at hiatus in cli nexus -- must be exec_hiatus
 */
extern void
cq_loop_resume(vty vty, cmd_ret_t ret)
{
  VTY_ASSERT_CLI_THREAD() ;

  qassert(vty->exec->state == exec_hiatus) ;

  if (vty->exec->state == exec_hiatus)
    cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue vty for execution in given nexus or issue thread event.
 *
 * Will execute in the current exec->state, passing in the given return
 * code.
 *
 * Note that *must* be vcl_cq_running.
 *
 * Returns:  the now current exec->state
 *           if is exec_stopped, then vty_close()
 */
static cmd_exec_state_t
cq_enqueue(vty vty, qpn_nexus dst, cmd_exec_state_t state, cmd_ret_t ret)
{
  cmd_exec exec = vty->exec ;

  qassert((dst == vty_cli_nexus) || (dst == vty_cmd_nexus)) ;

  VTY_LOCK() ;

  qassert(vty->vio->cl_state == vcl_cq_running) ;

  exec->locus = dst ;           /* VTY_LOCKED   */
  exec->state = state ;
  exec->ret   = ret ;

  if (vty_nexus)
    {
      qassert(exec->cq.mqb != NULL) ;

      /* Using mqb_priority on the vty_cmd_nexus has two effects:
       *
       *   1. commands being dispatched take priority over other work.
       *
       *   2. for single pthread, all messages related to the command queue
       *      itself (in particular, signals) take priority.
       *
       *      (For single pthread vty_cmd_nexus == vty_cli_nexus.)
       */
      mqueue_enqueue(dst->queue, exec->cq.mqb,
                                      (dst == vty_cmd_nexus) ? mqb_priority
                                                             : mqb_ordinary) ;
    }
  else
    {
      qassert(vty_cli_nexus == vty_cmd_nexus) ;
      exec->cq.thread = thread_add_event(master, cq_thread, vty, 0) ;
    } ;

  VTY_UNLOCK() ;

  return state ;
} ;

/*------------------------------------------------------------------------------
 * Revoke any message associated with the given vty from the command queue.
 *
 * Must be vcl_cq_running or vcl_cq_waiting.
 *
 * This is used when a VTY_TERMINAL or a VTY_VTYSH_SERVER is being closed.
 *
 * See cq_process below for discussion of what messages there may be.  At any
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
 * If succeeds in revoking, then sets the state to vcl_cq_waiting, in the
 * cli nexus and in exec_hiatus.
 *
 * NB: if no command loop has ever been started for this vty, then will not
 *     find anything to revoke.
 *
 * NB: if is vcl_cq_waiting already, then should not find anything to revoke,
 *     but does no harm to try !
 *
 * Returns:  true <=> the command loop is now vcl_cq_waiting
 */
static bool
cq_revoke(vty_io vio, cmd_ret_t ret)
{
  vty      vty ;
  cmd_exec exec ;
  bool     waiting ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  vty  = vio->vty ;
  exec = vty->exec ;

  qassert( (vio->cl_state == vcl_cq_running) ||
           (vio->cl_state == vcl_cq_waiting) ) ;

  waiting = false ;

  if (exec->state == exec_null)
    return false ;              /* just in case                 */

  if ((vio->cl_state != vcl_cq_running) && (vio->cl_state != vcl_cq_waiting))
    return false ;              /* just in case                 */

  /* If is vcl_cq_waiting, then do not expect to find */

  if (vty_nexus)
    {
      /* We have the VTY_LOCK().  Before mqb is placed on a queue, the
       * exec->locus is set, under VTY_LOCK.  So when revoking we can
       * check the correct queue !
       *
       * Note that thanks to the magic of mqb_revoke(), if the message has
       * been revoked in some other way (as part of closing down a nexus,
       * or its message queue), then we will discover the revoked message.
       */
      waiting = mqb_revoke(exec->cq.mqb, exec->locus->queue) ;
    }
  else
    {
      /* If succeeds in cancelling the event, the thread object is discarded.
       *
       * If does not succeed, then there is no thread object !
       */
      if (exec->cq.thread != NULL)
        {
          waiting = thread_cancel_event(master, vty) != 0 ;
          exec->cq.thread = NULL ;
        } ;
    } ;

  /* If have revoked, from the perspective of the command loop, this looks a
   * lot like CMD_CANCEL/CMD_STOP, so that's what we do... force the loop back
   * into the hands of the CLI, as if about to resume at the hiatus with one
   * of those signals.
   *
   * If was already vcl_cq_waiting, then can do the same.
   */
  if (vio->cl_state == vcl_cq_waiting)
    {
      waiting = true ;

      qassert(exec->locus == vty_cli_nexus) ;
      qassert(exec->state = exec_hiatus) ;
    } ;

  if (waiting)
    {
      exec->locus   = vty_cli_nexus ;
      exec->state   = exec_hiatus ;
      exec->ret     = ret ;

      vio->cl_state = vcl_cq_waiting ;
    } ;

  return waiting ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the qpthreads world.
 *
 * We do nothing on mqb_destroy.  Leaves cq_revoke() to pick this up.
 */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  vty   vty;

  assert(vty_nexus) ;                   /* must be running qnexus-wise  */

  vty  = mqb_get_arg0(mqb);

  qassert(vty->exec->cq.mqb == mqb) ;

  if (flag == mqb_action)
    {
      cmd_exec_state_t state ;

      state = cq_process(vty) ;

      if (state == exec_stopped)
        vty_close(vty) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Deal with command message -- in the legacy threads world.
 */
static int
cq_thread(struct thread* thread)
{
  cmd_exec_state_t state ;
  vty vty = THREAD_ARG(thread) ;

  vty->exec->cq.thread = NULL ;

  state = cq_process(vty) ;

  if (state == exec_stopped)
    vty_close(vty) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Try to stop the cq command loop.
 *
 * If there is a message in transit, then revoke it, forcing the command loop
 * to exec_hiatus,
 *
 * If the command loop is (then) vcl_cq_waiting, then enter the command loop
 * directly, with CMD_STOP.
 *
 * It may be that message queue or legacy thread handling is about to stop.
 * We pass in CMD_STOP, so the command loop should promptly start closing
 * down everything, and not return until everything is done, and the vty
 * has been closed, or something blocks while trying to close.  By calling
 * the command loop directly, we guarantee that it runs at least once !
 *
 * Returns:  true  <=> can now close the vty -- uty_close().
 */
extern bool
cq_loop_stop(vty_io vio)
{
  cmd_exec_state_t state ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  qassert( (vio->cl_state == vcl_cq_running) ||
           (vio->cl_state == vcl_cq_waiting) ) ;

  if (cq_revoke(vio, CMD_STOP))
    {
      vio->cl_state = vcl_cq_running ;
      state = cq_process(vio->vty) ;
    }
  else
    state = ~exec_stopped ;

  return (state == exec_stopped) ;
} ;

/*------------------------------------------------------------------------------
 * Process command(s) queued from VTY_TERMINAL or from VTY_VTYSH_SERVER.
 *
 * This is essentially a coroutine, where the state is in the vty, noting that:
 *
 *   vty->exec->state   is the "return address"
 *   vty->exec->ret     is the "return code"
 *
 * Though while we are here, those are held in variables.
 *
 * The command loop runs at the mqueue level in the qpnexus world, or is
 * driven by "events" in the legacy threads world.  The three ways into the
 * loop are:
 *
 *   cq_loop_enter()   -- called once and once only to start the loop.
 *
 *   cq_loop_resume()     -- called when some I/O process has completed and
 *                        the loop is in hiatus, waiting for I/O.
 *
 *   cq_loop_stop()    -- called when want to stop command loop, forces
 *                        exec_hiatus, ret == CMD_STOP !
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
 * The cq_loop_enter() and cq_loop_resume() operations send a message (to the
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
 *
 * Returns:  the now current exec->state
 *           if is exec_stopped, then vty_close() or uty_close() the vty.
 */
static cmd_exec_state_t
cq_process(vty vty)
{
  cmd_exec         exec ;
  cmd_parsed       parsed ;
  cmd_context      context ;
  cmd_exec_state_t state ;
  cmd_ret_t        ret ;

  /* Note that we have a single parser object for execution.
   *
   * Also, the context object is kept up to date as pipes are pushed and
   * popped -- so the pointer to it remains valid.
   */
  exec    = vty->exec ;
  parsed  = exec->parsed ;
  context = exec->context ;

  qassert(parsed  != NULL) ;
  qassert(context != NULL) ;

  /* Have switch wrapped in a while(1) so that can change state by setting
   * state and doing "continue".
   *
   * Breaking out of the switch forces the state to exec_hiatus, with the
   * current ret (which will switch to the CLI thread).
   *
   * The exec locus is either vty_cli_nexus or vty_cmd_nexus.  If these
   * are equal (including both NULL, in the legacy threads world), then is
   * running single threaded -- otherwise is running multi-threaded.
   */
  state = exec->state ;
  ret   = exec->ret ;

  while (1)
    {
      switch(state)
        {
          /*--------------------------------------------------------------------
           * Should *not* be here in any of these states.
           *
           * Returns in the current state.
           *
           * For exec_stopped, that should trigger a vty_close(), and that will
           * be the end of it.
           *
           * For exec_null and anything else the command loop should stall
           * (because cq_loop_resume() only re-enters the loop at exec_hiatus).
           */
          case exec_null:
          case exec_stopped:
          default:
            qassert(false) ;
            return exec->state = state ;

          /*--------------------------------------------------------------------
           * Hiatus state -- some return code to be dealt with !
           *
           * NB: we only reach here after breaking out of the switch, or
           *     on cq_loop_resume().
           */
          case exec_hiatus:
            qassert(exec->locus == vty_cli_nexus) ;

            while (1)
              {
                /* Let vty_cmd_hiatus() deal with the return code and/or any
                 * stop/error/etc trap, and adjust the stack as required.
                 */
                ret = vty_cmd_hiatus(vty, ret) ;

                if (ret == CMD_SUCCESS)
                  break ;               /* back to exec_fetch           */

                if (ret == CMD_IO_ERROR)
                  continue ;            /* give error back to hiatus    */

                /* Nothing more to be done here.
                 *
                 * The exec->ret is not really significant, but we set it for
                 * completeness.
                 *
                 * For CMD_WAITING, we expect to re-enter the loop at
                 * exec_hiatus, with the return code from a future "signal".
                 *
                 * For CMD_STOP, we expect the vty to be closed.
                 *
                 * Should not have any other return code here... but we return
                 * in the same way as CMD_WAITING.  If nothing is ever
                 * signalled, the command loop will simply hang.
                 */
                exec->ret = ret ;

                if (ret != CMD_STOP)
                  return exec->state = exec_hiatus ;
                                        /* <-<- DONE, pro tem <-<-<-<-<-*/

                return exec->state = exec_stopped ;
                                        /* <-<- DONE, permanently <-<-<-*/
              } ;

            fall_through ;

          /*--------------------------------------------------------------------
           * Need another command to execute => in_pipe !
           *
           * If multi-threaded: may be in either thread.
           *
           * If vty_cmd_line_fetch() cannot, for any reason, return a command
           * line, will return something other than CMD_SUCCESS -- which enters
           * the exec_hiatus (transferring to vty_cli_nexus).
           */
          case exec_fetch:
            ret = vty_cmd_line_fetch(vty) ;

            if (ret != CMD_SUCCESS)
              break ;

            if (context->to_do != cmd_do_command)
              {
                state = exec_special ;
                continue ;
              } ;

            /* Parse command in hand
             *
             * If multi-threaded: may be in either thread.
             */
            cmd_tokenize(parsed, context->line, context->full_lex) ;

            ret = cmd_parse_command(parsed, context) ;

            if (ret != CMD_SUCCESS)
              break ;                   /* on *any* parsing issue       */

            if ((parsed->parts & cmd_parts_execute) == 0)
              {
                /* Empty lines from in_pipes appear here.
                 *
                 * Don't expect to see them otherwise, but in any case need to
                 * complete the command execution process.
                 */
                state = exec_cmd_complete ;
                continue ;
              } ;

            /* reflect if required -- CMD_WAITING is fine, let the output side
             *                        deal with that.
             */
            if (context->reflect)
              {
                ret = vty_cmd_reflect_line(vty) ;

                if ((ret != CMD_SUCCESS) && (ret != CMD_WAITING))
                  break ;       /* CMD_IO_ERROR or CMD_CANCEL   */
              } ;

          /*--------------------------------------------------------------------
           * Pipe work if any
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
                    state = exec_cmd_complete ;
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
                                        qs_string(exec->context->line)) ;
                } ;
            } ;
#endif /* CONSUMED_TIME_CHECK */

        /*--------------------------------------------------------------------
         * Command has completed somehow -- this is the loop entry point.
         *
         * Return code may be: CMD_SUCCESS, CMD_WARNING, CMD_ERROR, CMD_CANCEL,
         *                     or CMD_IO_ERROR.
         *
         * Pushes output -- note that the push is forced, even if the output
         * buffers are empty, so that the command completion is signalled to
         * the output side.
         *
         * Will either loop back to fetch the next command line, or fall into
         * the hiatus.
         *
         * If multi-threaded: may be in either thread:
         *
         *   vty_cmd_complete() may set write ready -- so in vty_cmd_nexus may
         *   generate message to vty_cli_nexus.
         */
        case exec_cmd_complete:
          ret = vty_cmd_complete(vty, ret) ;

          if (ret != CMD_SUCCESS)
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

          state = exec_cmd_complete ;
          continue ;
      } ;

     /* Have broken out of the switch() => exec_hiatus
      *
      * Something has returned a return code that causes entry to the hiatus
      * state to sort out.
      *
      * If we are not in the vty_cli_nexus, then must pass the problem
      * to the vty_cli_nexus.
      */
     if (exec->locus != vty_cli_nexus)
       return cq_enqueue(vty, vty_cli_nexus, exec_hiatus, ret) ;

     state = exec_hiatus ;

     continue ;
    } ;
} ;

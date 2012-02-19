/* Quagga Pthreads support -- header
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
#include <signal.h>

#include "qpnexus.h"
#include "memory.h"
#include "thread.h"
#include "sigevent.h"

/* prototypes */
static void* qpn_start(void* arg);
static void qpn_in_thread_init(qpn_nexus qpn);

/*==============================================================================
 * Quagga Nexus Interface -- qpn_xxxx
 *

 */

/*------------------------------------------------------------------------------
 * Initialise the qpnexus handling -- to be done as soon as state of
 * qpthreads_enabled is established.
 */
extern void
qpn_init(void)
{
  qpt_data_create(qpn_self) ;   /* thread specific data */
} ;

/*------------------------------------------------------------------------------
 * Set the thread's qpn_self to point at its qpnexus.
 */
static void
qpn_self_knowledge(qpn_nexus qpn)
{
  qpt_data_set_value(qpn_self, qpn) ;
} ;

/*==============================================================================
 * Initialisation, add hook, free etc.
 *
 */

/*------------------------------------------------------------------------------
 * Initialise a nexus -- allocating it if required.
 *
 * If main_thread is set then no new thread will be created when qpn_exec() is
 * called, instead the finite state machine will be run in the calling thread.
 *
 * The main thread will only block the message queue's signal.
 *
 * Non-main threads will block most signals.
 *
 * Returns the qpn_nexus.
 */
extern qpn_nexus
qpn_init_new(qpn_nexus qpn, bool main_thread, const char* name)
{
  if (qpn == NULL)
    qpn = XCALLOC(MTYPE_QPN_NEXUS, sizeof(struct qpn_nexus)) ;
  else
    memset(qpn, 0,  sizeof(struct qpn_nexus)) ;

  qpn->name        = name ;

  qpn->selection   = qps_selection_init_new(qpn->selection);
  qpn->pile        = qtimer_pile_init_new(qpn->pile);
  qpn->queue       = mqueue_init_new(qpn->queue, mqt_signal_unicast);
  qpn->main_thread = main_thread;
  qpn->start       = qpn_start;

  qpt_spin_init(qpn->stats_slk) ;

  if (main_thread)
    {
      qpn->thread_id = qpt_thread_self();
      qpn_self_knowledge(qpn) ;
    } ;

  return qpn;
} ;

/*------------------------------------------------------------------------------
 * Add a hook function to the given nexus.
 */
extern void
qpn_add_hook_function(qpn_hook_list list, void* hook)
{
  passert(list->count < qpn_hooks_max) ;
  list->hooks[list->count++] = hook ;
} ;

/*------------------------------------------------------------------------------
 * Reset given nexus and, if required, free the nexus structure.
 *
 * Free timers, selection, message queue and its thread signal.
 *
 * Leaves all pointers to these things NULL -- which generally means that the
 * object is empty or otherwise out of action.
 */
extern qpn_nexus
qpn_reset(qpn_nexus qpn, free_keep_b free_structure)
{
  qps_file qf;
  qtimer qtr;

  if (qpn == NULL)
    return NULL;

  /* The qtimer pile.  If there are any timers still in the pile, they are
   * removed and marked "inactive".  The owners of these timers are responsible
   * for finally freeing them (if required).
   */
  if (qpn->pile != NULL)
    {
      while ((qtr = qtimer_pile_ream(qpn->pile, free_it)))
        { ; } ;
      qpn->pile = NULL ;
    }

  /* The file selection pile.  If there are any files still in the selection,
   * they are removed.  The owners of these file are responsible for finally
   * closing them and freeing the qf.
   */
  if (qpn->selection != NULL)
    {
      while ((qf = qps_selection_ream(qpn->selection, free_it)))
        { ; } ;
      qpn->selection = NULL ;
    }

  qpn->queue = mqueue_reset(qpn->queue, free_it);
  qpn->mts   = mqueue_thread_signal_reset(qpn->mts, free_it);

  if (free_structure)
    XFREE(MTYPE_QPN_NEXUS, qpn) ;       /* sets qpn = NULL      */

  return qpn ;
} ;

/*==============================================================================
 * Execution of a nexus
 */

/*------------------------------------------------------------------------------
 * If not main qpthread create new qpthread.
 *
 * For all qpthreads: start the thread !
 */
extern void
qpn_exec(qpn_nexus qpn)
{
  if (qpn->main_thread)
    qpn->start(qpn);
  else
    qpt_thread_create(qpn->start, qpn, NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Pthread routine
 *
 * Processes:
 *
 *   1) Main thread only -- signals.
 *
 *   2) High priority pending work -- event hooks.
 *
 *   3) Messages coming from other pthreads -- mqueue_queue.
 *
 *   4) All priority pending work -- event hooks.
 *
 *   5) I/O -- qpselect
 *
 *      This deals with all active sockets for read/write/connect/accept.
 *
 *      Each time a socket is readable, one message is read and dispatched.
 *      The pselect timeout is set to be when the next timer is due.
 *
 *   6) Timers -- qtimers
 *
 *   7) Low priority pending work
 */
static void*
qpn_start(void* arg)
{
  qpn_nexus qpn = arg;
  mqueue_block mqb;
  int actions;
  qtime_mono_t now ;
  qtime_t      max_wait ;
  unsigned i;
  bool this ;
  bool prev ;
  bool wait ;
  bool idle ;

  /* now in our thread, complete initialisation                         */
  qpn_in_thread_init(qpn);

  /* custom in-thread initialization                                    */
  for (i = 0; i < qpn->in_thread_init.count ;)
    ((qpn_init_function*)(qpn->in_thread_init.hooks[i++]))() ;

  /* Until required to terminate, loop                                  */
  prev = true ;
  this = true ;
  idle = false ;
  while (!qpn->terminate)
    {
      ++qpn->raw.cycles ;

      /* Signals are highest priority -- only execute for main thread   */
      if (qpn->main_thread)
        {
          int ret = quagga_sigevent_process() ;
          if (ret != 0)
            {
              this = true ;
              ++qpn->raw.signals ;
            } ;
        } ;

      /* Foreground hooks, if any.                                      */
      for (i = 0; i < qpn->foreground.count ; ++i)
        {
          int ret = ((qpn_hook_function*)(qpn->foreground.hooks[i]))() ;
          if (ret != 0)
            {
              this = true ;
              ++qpn->raw.foreg ;
            } ;
        } ;

      /* take stuff from the message queue
       *
       * If nothing done this time and last time around the loop then will
       * arrange to wait iff the queue is empty first time through.
       *
       * If there is nothing on the queue, then "wait" => the queue is set so
       * that anything added to the queue will generate a signal.
       *
       * If there is something in the queue, then we are not "idle".
       */
      wait = !this && !prev ;
      mqb = mqueue_dequeue(qpn->queue, wait, qpn->mts) ;

      if (mqb != NULL)
        {
          uint done ;

          this = true ;
          wait = false ;

          done = 0 ;
          do
            {
              mqb_dispatch(mqb, mqb_action);
              ++done ;

              if (done == 200)
                break ;

              mqb = mqueue_dequeue(qpn->queue, false, NULL) ;
            } while (mqb != NULL) ;

          qpn->raw.dispatch += done ;
        } ;

      /* If we have done nothing this time around, see if anything in the
       * background.
       *
       * If do something in the background, then set "this".
       */
      if (!this)
        {
          for (i = 0; i < qpn->background.count ; ++i)
            {
              int ret = ((qpn_hook_function*)(qpn->background.hooks[i]))() ;
              if (ret != 0)
                {
                  this = true ;
                  ++qpn->raw.backg ;
                } ;
            } ;
        } ;

      /* Prepare to block for some input, output, signal or timeout
       *
       * Note: only if is completely "idle" -- which includes found nothing to
       *       do in the background -- will we actually block.
       */
      now = qt_get_monotonic() ;

      idle = (!this && !prev) ;
      if (idle)
        max_wait = qtimer_pile_top_wait(qpn->pile, QTIME(MAX_PSELECT_WAIT),
                                                                          now) ;
      else
        max_wait = 0 ;

      /* We are about to do a pselect, which may wait.  Now is the time to
       * set the "raw" current time, and publish the stats.
       */
      qpn->raw.last_time = now ;

      qpt_spin_lock(qpn->stats_slk) ;
      qpn->stats = qpn->raw ;
      qpt_spin_unlock(qpn->stats_slk) ;

      /* Do pselect, which may now wait
       *
       * After pselect, if is "wait", then will have set the message queue
       * waiting, which can now be cleared.  If is "idle", any time since
       * "raw.last_time" must be counted as idle time.
       *
       * Remember current "done" as "prev", and set done depending on I/O
       * action count.
       */
      actions = qps_pselect(qpn->selection, max_wait) ;

      now = qt_get_monotonic() ;

      if (idle)
        qpn->raw.idle += now - qpn->raw.last_time ;

      if (wait)
        mqueue_done_waiting(qpn->queue, qpn->mts) ;

      prev = this ;
      this = (actions != 0) ;           /* actions < 0 => Signal        */

      if (actions > 0)
        {
          qpn->raw.io_acts += actions ;

          do
            actions = qps_dispatch_next(qpn->selection) ;
          while (actions > 0) ;
        } ;

      /* process timers -- also counts as one activity
       */
      while (qtimer_pile_dispatch_next(qpn->pile, now))
        {
          ++qpn->raw.timers ;
          this = true ;                 /* done something in this pass  */
        } ;
    } ;

  /* custom in-thread finalization                                      */
  for (i = qpn->in_thread_final.count; i > 0 ;)
    ((qpn_init_function*)(qpn->in_thread_final.hooks[--i]))() ;

  return NULL;
}

/*------------------------------------------------------------------------------
 * Now running in our thread, do common initialisation
 */
static void
qpn_in_thread_init(qpn_nexus qpn)
{
  sigset_t sigmask[1];

  memset(&qpn->raw, 0, sizeof(qpn_stats_t)) ;
  qpn->raw.start_time = qt_get_monotonic() ;
  qpn->raw.last_time  = qpn->raw.start_time ;

  qpt_spin_lock(qpn->stats_slk) ;
  qpn->prev_stats = qpn->stats = qpn->raw ;     /* share start time etc.  */
  qpt_spin_unlock(qpn->stats_slk) ;

  qpn->thread_id = qpt_thread_self();
  qpn_self_knowledge(qpn) ;

  /* Signal mask.
   *
   * The main thread blocks nothing, except SIG_INTERRUPT.  So (a) all
   * signals other than the "hard cases" are routed to the main thread, and
   * (b) SIG_INTERRUPT is masked until it is unmasked in pselect.
   *
   * Other threads block everything except the hard cases and SIG_INTERRUPT.
   */
  if (qpn->main_thread)
    sigmakeset(sigmask, SIG_INTERRUPT, -1) ;
  else
    siginvset(sigmask, signal_get_hard_set()) ;

  qpt_thread_sigmask(SIG_BLOCK, sigmask, NULL);

  /* The signal mask to be used during pselect()                        */
  sigcopyset(qpn->pselect_mask, sigmask) ;
  sigdelset(qpn->pselect_mask, SIG_INTERRUPT) ;
  qpn->pselect_signal = SIG_INTERRUPT ;

  /* Now we have thread_id and mask, prep for using message queue.      */
  if (qpn->queue != NULL)
    qpn->mts = mqueue_thread_signal_init(qpn->mts, qpn->thread_id,
                                                                SIG_INTERRUPT) ;
  if (qpn->selection != NULL)
    qps_set_signal(qpn->selection, qpn->pselect_mask);
} ;

/*------------------------------------------------------------------------------
 * Ask the thread to terminate itself quickly and cleanly.
 *
 * Does nothing if terminate already set.
 */
void
qpn_terminate(qpn_nexus qpn)
{
  if (!qpn->terminate)
    {
      qpn->terminate = true ;

      /* wake up any pselect */
      if (qpthreads_enabled)
        qpt_thread_signal(qpn->thread_id, SIG_INTERRUPT);
    } ;
} ;

/*------------------------------------------------------------------------------
 * Get a copy of the current stats for the given nexus.
 *
 * This copies the stats to the "prev_stats" area in the nexus.
 */
extern void
qpn_get_stats(qpn_nexus qpn, qpn_stats curr, qpn_stats prev)
{
  qpt_spin_lock(qpn->stats_slk) ;

  *prev = qpn->prev_stats ;
  *curr = qpn->stats ;

  qpn->prev_stats = qpn->stats ;

  qpt_spin_unlock(qpn->stats_slk) ;
} ;

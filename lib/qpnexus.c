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
#include "qtime.h"
#include "vargs.h"
#include "pthread_safe.h"
#include "qpath.h"
#include "command.h"
#include <stdio.h>
#include <sys/select.h>

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void* qpn_start(void* arg);
static void qpn_in_thread_init(qpn_nexus qpn);

/*==============================================================================
 * Quagga Nexus Interface -- qpn_xxxx
 *
 */
static qtime_t qpn_max_pselect_wait = QTIME(MAX_PSELECT_WAIT) ;

static qtime_t qpn_wd_interval      = 0 ;

/* Walker object for walking nexuses
 */
typedef struct qpn_nexus_walk* qpn_nexus_walk ;

struct qpn_nexus_walk
{
  struct dl_list_pair(qpn_nexus_walk) list ;

  qpn_nexus walk_next ;
};

typedef struct qpn_nexus_walk  qpn_nexus_walk_t[1] ;

/* List of all known nexuses.
 *
 * Must own the spin_lock to add /remove entries on this list, and when doing
 * so must update the "walk" if that is set.
 *
 * Must own the spin lock to perform walk.  We know that
 */
static struct
{
  qpt_spin_t    slk ;

  struct dl_base_pair(qpn_nexus) nexuses ;

  struct dl_base_pair(qpn_nexus_walk) walkers ;

} qpn_nexus_list[1] ;

/*------------------------------------------------------------------------------
 * Initialise the qpnexus handling -- to be done as soon as state of
 * qpthreads_enabled is established and *before* any watch-dog is started.
 */
extern void
qpn_init(void)
{
  qpn_max_pselect_wait = (qpn_wd_interval == 0) ? QTIME(MAX_PSELECT_WAIT)
                                                : qpn_wd_interval * 7 / 16 ;

  memset(qpn_nexus_list, 0, sizeof(qpn_nexus_list)) ;

  qpt_spin_init(qpn_nexus_list->slk) ;

  qpt_data_create(qpn_self) ;   /* thread specific data */
} ;

/*==============================================================================
 * Initialisation, add hook, free etc.
 *
 */
static void qpn_self_knowledge(qpn_nexus qpn) ;
static void qpn_nexus_add(qpn_nexus qpn) ;
static void qpn_nexus_del(qpn_nexus qpn) ;

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
    qpn_self_knowledge(qpn) ;

  return qpn;
} ;

/*------------------------------------------------------------------------------
 * Complete the initialisation of nexus, once thread is running.
 *
 * Collect thread's id and cputime clock id
 *
 * Set the thread's qpn_self to point at its qpnexus.
 *
 * Add nexus to list of live ones.
 */
static void
qpn_self_knowledge(qpn_nexus qpn)
{
  qpn->thread_id    = qpt_thread_self();
  qpn->cpu_clock_id = qpt_get_cpu_clock(qpn->thread_id) ;

  qpt_data_set_value(qpn_self, qpn) ;

  qpn_nexus_add(qpn) ;
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

  /* Immediately remove from list of known nexuses !
   */
  qpn_nexus_del(qpn) ;

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

/*------------------------------------------------------------------------------
 * Add qpn to the list of nexuses
 *
 * If there is a walker about to hit the end of the list, update.
 */
static void
qpn_nexus_add(qpn_nexus qpn)
{
  qpn_nexus_walk walk ;

  qpt_spin_lock(qpn_nexus_list->slk) ;

  ddl_append(qpn_nexus_list->nexuses, qpn, list) ;

  walk = ddl_head(qpn_nexus_list->walkers) ;
  while (walk != NULL)
    {
      if (walk->walk_next == NULL)
        walk->walk_next = qpn ;

      walk = ddl_next(walk, list) ;
    } ;

  qpt_spin_unlock(qpn_nexus_list->slk) ;
} ;

/*------------------------------------------------------------------------------
 * Delete qpn from the list of nexuses
 *
 * If there is a walker about to fetch the deleted item, update.
 *
 * Note that while any walker is busy extracting information from the nexus,
 * it will hold the list spin-lock.
 */
static void
qpn_nexus_del(qpn_nexus qpn)
{
  qpn_nexus_walk walk ;

  qpt_spin_lock(qpn_nexus_list->slk) ;

  ddl_append(qpn_nexus_list->nexuses, qpn, list) ;

  walk = ddl_head(qpn_nexus_list->walkers) ;
  while (walk != NULL)
    {
      if (walk->walk_next == qpn)
        walk->walk_next = ddl_next(qpn, list) ;

      walk = ddl_next(walk, list) ;
    } ;

  qpt_spin_unlock(qpn_nexus_list->slk) ;
} ;

/*------------------------------------------------------------------------------
 * Start walk of list of nexuses
 *
 * Adds given walker to the list of walkers and initialises to point at first
 * on the list.
 */
static void
qpn_nexus_walk_start(qpn_nexus_walk walk)
{
  qpt_spin_lock(qpn_nexus_list->slk) ;

  memset(walk, 0, sizeof(qpn_nexus_walk_t)) ;

  ddl_append(qpn_nexus_list->walkers, walk, list) ;
  walk->walk_next = ddl_head(qpn_nexus_list->nexuses) ;

  qpt_spin_unlock(qpn_nexus_list->slk) ;
}

/*------------------------------------------------------------------------------
 * Lock list and get the next nexus on our walk.
 *
 * If walk finished, release the lock.
 *
 * Returns: NULL <=> nothing more to -- does not hold spin-lock
 *          address of next nexus    -- *DOES* hold the spin-lock
 *
 * NB: should NOT hold on to the nexus for long !  Should extract the required
 *     information and then qpn_nexus_walk_step().
 */
static qpn_nexus
qpn_nexus_walk_next(qpn_nexus_walk walk)
{
  qpn_nexus next ;

  qpt_spin_lock(qpn_nexus_list->slk) ;

  next = walk->walk_next ;

  if (next == NULL)
    qpt_spin_unlock(qpn_nexus_list->slk) ;

  return next ;
} ;

/*------------------------------------------------------------------------------
 * Step the walker to the next nexus and release the spin lock.
 *
 * The walker must at this point have extracted all that was needed from the
 * current nexus.
 */
static void
qpn_nexus_walk_step(qpn_nexus_walk walk)
{
  walk->walk_next = ddl_next(walk->walk_next, list) ;

  qpt_spin_unlock(qpn_nexus_list->slk) ;
} ;

/*------------------------------------------------------------------------------
 * End walk of list of nexuses
 */
static void
qpn_nexus_walk_end(qpn_nexus_walk walk)
{
  qpt_spin_lock(qpn_nexus_list->slk) ;

  ddl_del(qpn_nexus_list->walkers, walk, list) ;

  qpt_spin_unlock(qpn_nexus_list->slk) ;
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
  bool wait_mq ;
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
      wait_mq = !this && !prev ;
      mqb = mqueue_dequeue(qpn->queue, wait_mq, qpn->mts) ;

      if (mqb != NULL)
        {
          uint done ;

          this    = true ;
          wait_mq = false ;

          done = 0 ;
          do
            {
              mqb_dispatch(mqb, mqb_action);
              ++done ;

              if (done == 200)
                break ;

              mqb = mqueue_dequeue(qpn->queue, wait_mq, NULL) ;
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
        max_wait = qtimer_pile_top_wait(qpn->pile, qpn_max_pselect_wait, now) ;
      else
        max_wait = 0 ;

      /* We are about to do a pselect, which may wait.  Now is the time to
       * set the "raw" current time, and publish the stats.
       */
      qpn->raw.last_time = now ;

      qpt_spin_lock(qpn->stats_slk) ;

      qpn->stats    = qpn->raw ;
      if (idle)
        qpn->idleness = 1 ;

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
        {
          qpn->raw.idle += now - qpn->raw.last_time ;

          qpt_spin_lock(qpn->stats_slk) ;
          qpn->idleness = 0 ;
          qpt_spin_unlock(qpn->stats_slk) ;
        } ;

      if (wait_mq)
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

  if (!qpn->main_thread)
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

/*==============================================================================
 * Watch-dog pthread to keep track of all other pthreads, and to look out
 * for:
 *
 *   1) pthread not running -- when the watch-dog is enabled, the longest
 *      each will wait for I/O or Timers is half the watch-dog interval,
 *      so we can tell if the pthread is not running.
 *
 *   2) mutexes locked for long periods.
 *
 *   3) cpu consumption by each pthread
 *
 *   4) large scale wobbles of the monotonic clock
 *
 */
static volatile bool qpn_wd_running  = false ;
static volatile bool qpn_wd_stop     = false ;

static qpt_thread_t qpn_wd_thread_id ;
static qpt_spin_t   qpn_wd_spin_lock ;

static FILE*    qpn_wd_log_file ;
static qpath    qpn_wd_log_path ;

static void* qpn_wd_activity(void* args) ;
static void qpn_wd_log(const char* format, ...)         PRINTF_ATTRIBUTE(1, 2) ;
static void qpn_wd_check_intervals(qtime_t sleep, qtime_t mono_interval,
                                      qtime_t real_interval, uint eintr_count) ;
static void qpn_wd_check_nexuses(qtime_t interval, qstring report) ;

/*------------------------------------------------------------------------------
 * Initialise the watch-dog -- completely dormant !
 */
extern void
qpn_wd_start_up(void)
{
  qpn_wd_interval = 0 ;         /* => do not start !    */

  qpn_wd_running  = false ;
  qpn_wd_stop     = false ;

  qpn_wd_log_file = NULL ;
  qpn_wd_log_path = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Prepare the watch-dog from command line
 *
 * Take command line argument and set the watch-dog interval and file
 *
 * Returns:  true  <=> OK
 *           false <=> invalid argument or not able to open the file
 *                     messages output to stderr
 */
extern bool
qpn_wd_prepare(const char* arg)
{
  uint  interval ;
  char* rest ;

  if (qpn_wd_interval != 0)
    {
      fprintf(stderr, "*** watch-dog already set, cannot -W%s\n", arg) ;
      return false ;
    } ;

  while ((*(const unsigned char*)arg <= ' ') && (*arg != '\0'))
    ++arg ;

  if (*arg == '=')
    {
      do
        ++arg ;
      while ((*(const unsigned char*)arg <= ' ') && (*arg != '\0')) ;
    } ;

  errno = 0 ;
  interval = strtoul(arg, &rest, 10) ;

  if ((errno != 0) || (interval == 0) || (interval > 60) ||
                                            ((*rest != '\0') && (*rest != ',')))
    {
      fprintf(stderr, "*** invalid watch-dog interval in '%s'\n", arg) ;
      return false ;
    } ;

  while ((*(unsigned char*)rest <= ' ') && (*rest != '\0'))
    ++rest ;

  if (*rest == '\0')
    {
      int fd = dup(fileno(stderr)) ;

      if (fd < 0)
        {
          fprintf(stderr, "*** failed to dup(stderr): '%s'\n",
                                                         errtoa(errno, 0).str) ;
          return false ;
        } ;

      qpn_wd_log_file = fdopen(fd, "a") ;

      if (qpn_wd_log_file == NULL)
        {
          fprintf(stderr, "*** failed to fdopen(stderr): '%s'\n",
                                                         errtoa(errno, 0).str) ;
          return false ;
        } ;

      qpn_wd_log_path = qpath_set(NULL, "stderr") ;
    }
  else
    {
      const char* mode ;

      do
        ++rest ;                /* step past ','        */
      while ((*(unsigned char*)rest <= ' ') && (*rest != '\0')) ;

      mode = "w" ;
      if (*rest == '+')
        {
          mode = "a" ;

          do
            ++rest ;            /* step past '+'        */
          while ((*(unsigned char*)rest <= ' ') && (*rest != '\0')) ;
        } ;

      qpn_wd_log_path = qpath_complete(qpath_set(NULL, rest),
                                                             vty_getcwd(NULL)) ;

      qpn_wd_log_file = fopen(qpath_string(qpn_wd_log_path), mode) ;

      if (qpn_wd_log_file == NULL)
        {
          fprintf(stderr, "*** failed to open(%s, %s): '%s'\n",
                    qpath_string(qpn_wd_log_path), mode, errtoa(errno, 0).str) ;
          qpath_free(qpn_wd_log_path) ;
          return false ;
        } ;
    } ;

  qpn_wd_interval = QTIME(interval) ;   /* start when the time comes    */

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Start the watch-dog if it has been set to run
 *
 * Must be called *before* any qpn_exec().
 */
extern void
qpn_wd_start(void)
{
  assert(!qpn_wd_running) ;

  if (qpn_wd_interval != 0)
    {
      qpt_spin_init(qpn_wd_spin_lock) ;
      qpn_wd_thread_id = qpt_thread_create(qpn_wd_activity, NULL, NULL) ;
    } ;
}

/*------------------------------------------------------------------------------
 * Stop the watch-dog
 */
extern void
qpn_wd_finish(uint interval)
{
  if (qpn_wd_interval != 0)
    {
      qpt_spin_lock(qpn_wd_spin_lock) ;

      qpn_wd_stop = true ;

      qpt_thread_signal(qpn_wd_thread_id, SIG_INTERRUPT) ;

      qpt_spin_unlock(qpn_wd_spin_lock) ;

      qpt_thread_join(qpn_wd_thread_id) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Check the qpn_wd_stop flag
 */
static bool
qpn_wd_stop_now(void)
{
  bool stop ;

  qpt_spin_lock(qpn_wd_spin_lock) ;
  stop = qpn_wd_stop ;
  qpt_spin_unlock(qpn_wd_spin_lock) ;

  return stop ;
} ;

/*------------------------------------------------------------------------------
 * Watch-Dog Activity
 *
 */
enum { use_nanosleep = true } ;

static void*
qpn_wd_activity(void* args)
{
  sigset_t sigmask[1], sigwait[1] ;
  const char* using ;
  qstring report ;
  qtime_t sleep, mono_end ;

  using  = use_nanosleep ? "nanosleep" : "pselect" ;
  report = qs_new(100) ;

  /* Start
   */
  qpn_wd_log("Watch-Dog started (%s)", using) ;

  /* Preparation.
   *
   * Block all signals except the hard cases and prepare a sig mask for when
   * waiting in pselect() or for when running with nanosleep.
   */
  siginvset(sigmask, signal_get_hard_set()) ;
  sigcopyset(sigwait, sigmask) ;
  sigdelset(sigwait, SIG_INTERRUPT) ;

  if (use_nanosleep)
    qpt_thread_sigmask(SIG_BLOCK, sigwait, NULL);
  else
    qpt_thread_sigmask(SIG_BLOCK, sigmask, NULL) ;

  qpn_wd_running = true ;

  /* The watch-dog loop
   */
  sleep    = qpn_wd_interval ;
  mono_end = qt_get_realtime() ;
  while (!qpn_wd_stop_now())
    {
      struct timespec ns[1] ;
      uint eintr_count ;
      qtime_t mono_start, mono_interval, mono_prev_end ;
      qtime_t real_start, real_interval, real_end ;

      /* Set the sleep to account for the time we have spent doing the
       * checks and generating the logging.
       *
       * We want to run the watch-dog pretty much every qpn_wd_interval, but
       * if it tales a long tome to run the checks, or something goes
       */
      mono_prev_end = mono_end ;
      mono_start    = qt_get_monotonic() ;

      sleep = qpn_wd_interval - (mono_start - mono_prev_end) ;
      if ((sleep <= 0) || (sleep > qpn_wd_interval))
        sleep = qpn_wd_interval ;

      /* Wait for the watch-dog interval, as measured by CLOCK_REALTIME,
       * recording the CLOCK_MONOTONIC time when we start.
       *
       * Loop here if get a Signal, counting (in case this leads to an
       * apparent clock inconsistency).
       */
      if (use_nanosleep)
        qtime2timespec(ns, sleep) ;

      real_end   = qt_get_realtime() ;
      real_start = real_end ;

      eintr_count = 0 ;
      while (1)
        {
          struct timespec ts[1] ;
          int     ret ;

          if (use_nanosleep)
            {
              *ts = *ns ;
              ret = nanosleep(ts, ns) ;         /* CLOCK_REALTIME       */
            }
          else
            {
              qtime_t wait ;

              wait = sleep + (real_start - real_end) ;
              qtime2timespec(ts, wait > 0 ? wait : 0) ;

              ret = pselect(0, NULL, NULL, NULL, ts, sigwait) ;
            } ;

          real_end = qt_get_realtime() ;

          if (ret == 0)
            break ;

          ret = errno ;

          if (ret == EINTR)
            {
              if (qpn_wd_stop_now())
                break ;

              ++eintr_count ;
              continue ;
            } ;

          /* Something broken -- log error and exit the pthread
           */
          qpn_wd_log("%s() returned error: %s", using, errtoa(ret, 0).str) ;
          qpn_wd_stop = true ;

          break ;
        } ;

      mono_end = qt_get_monotonic() ;

      mono_interval = mono_end - mono_start ;
      real_interval = real_end - real_start ;

      if (qpn_wd_stop_now())
        break ;

      /* Check the mono_interval and real_interval -- reporting the eintr_count
       * if those are not satisfactory
       */
      qpn_wd_check_intervals(sleep, mono_interval, real_interval, eintr_count) ;

      /* Look for CPU utilisation and whether any nexus is stalled
       */
      qpn_wd_check_nexuses(mono_end - mono_prev_end, report) ;

      qpn_wd_log("%s", qs_string(report)) ;

    } ;

  /* Termination
   */
  qpn_wd_log("Watch-Dog terminated (%s)", using) ;

  qpt_spin_destroy(qpn_wd_spin_lock) ;

  fclose(qpn_wd_log_file) ;

  qpath_free(qpn_wd_log_path) ;
  qs_free(report) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Convert given qtime_t to a double in seconds
 */
static double
qpn_wd_float(qtime_t t)
{
  return (double)(t) / (double)QTIME(1) ;
} ;

/*------------------------------------------------------------------------------
 * Worry about the interval since the last time -- as measure by both the
 * CLOCK_REALTIME and the CLOCK_MONOTONIC
 *
 */
static void
qpn_wd_check_intervals(qtime_t sleep, qtime_t mono_interval,
                                      qtime_t real_interval, uint eintr_count)
{
  qtime_t delta ;

  /* The CLOCK_REALTIME interval should be close to the requested
   * qpn_wd_interval.
   *
   * If differ by more than +/- 1sec -- report issue.
   */
  delta = real_interval - sleep ;
  if ((delta <= -QTIME(1)) || (delta >= +QTIME(1)))
    qpn_wd_log("*** REALTIME interval delta=%+6.3f (%5.3f - %5.3f)",
                           qpn_wd_float(delta), qpn_wd_float(real_interval),
                                                qpn_wd_float(sleep)) ;

  /* The CLOCK_REALTIME and CLOCK_MONOTONIC intervals should be close to each
   * other
   *
   * If differ by more than +/- 0.01sec -- report issue.
   */
  delta = real_interval - mono_interval ;
  if ((delta <= -(QTIME(1) / 100)) || (delta >= +(QTIME(1) / 100)))
    qpn_wd_log("*** REALTIME - MONOTONIC delta = %+6.3f (%5.3f - %5.3f)",
                             qpn_wd_float(delta), qpn_wd_float(real_interval),
                                                  qpn_wd_float(mono_interval)) ;
} ;

/*------------------------------------------------------------------------------
 * Worry about what each nexus has been up to in the last mono_interval
 * nano seconds.
 */
static void
qpn_wd_check_nexuses(qtime_t interval, qstring report)
{
  qpn_nexus_walk_t walk ;
  qpn_nexus        qpn ;

  qs_clear(report) ;

  qpn_nexus_walk_start(walk) ;

  while ((qpn = qpn_nexus_walk_next(walk)) != NULL)
    {
      uint    idleness ;
      qtime_t this_cpu ;
      qtime_t last_cpu ;
      char    name[16 + 1] ;
      ulen    len ;
      double  pc_cpu ;

      /* Extract the data
       */
      qpt_spin_lock(qpn->stats_slk) ;   /*<-<-<-<-<-<-<-<-<-<-<-*/

      if (qpn->idleness == 0)
        idleness = 0 ;
      else
        idleness = ++qpn->idleness ;

      this_cpu = qpt_cpu_time(qpn->cpu_clock_id)  ;

      last_cpu = qpn->cpu_time ;
      qpn->cpu_time = this_cpu ;

      len = strlen(qpn->name) ;
      if (len > 16)
        len = 16 ;
      memcpy(name, qpn->name, len) ;
      name[len] = '\0' ;

      qpt_spin_unlock(qpn->stats_slk) ; /*<-<-<-<-<-<-<-<-<-<-<-*/

      /* Step to next nexus and release lock on the nexus list
       */
      qpn_nexus_walk_step(walk) ;

      /* Check for stalled and collect output for this nexus
       */
      if (idleness > 2)
        qpn_wd_log("*** %s pthread stalled -- %u", name, idleness - 2) ;

      pc_cpu = (double)((this_cpu - last_cpu) * 100) / (double)interval ;

      qs_printf_a(report, " %16s%s%5.2f%%", name,
                         (idleness == 0) ? "+" : (idleness == 2) ? " " : "*",
                                                                       pc_cpu) ;
    } ;

  qpn_nexus_walk_end(walk) ;
} ;

/*------------------------------------------------------------------------------
 * Write a watch-dog log message -- preceded by the current time,
 *                                  followed by "\n"
 *
 * Note that we use a separate file to write the watch-do log to, partly to
 * avoid cluttering up the real logging, and partly to ensure the minimum
 * interaction with the rest of the system.
 */
static void
qpn_wd_log(const char* format, ...)
{
  struct timeval clock[1] ;
  struct tm      tm[1] ;
  char           time_buf[30] ; /* "9999-99-99 99:99:99"       */

  va_list va;

  /* Construct and output timestamp
   */
  gettimeofday(clock, NULL);

  localtime_r(&clock->tv_sec, tm);
  strftime(time_buf, sizeof(time_buf), "%F %T" , tm) ;

  fprintf(qpn_wd_log_file, "%s ", time_buf) ;

  va_start(va, format);
  vfprintf(qpn_wd_log_file, format, va) ;
  va_end (va);

  fprintf(qpn_wd_log_file, "\n") ;
} ;

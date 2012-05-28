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
static void* qpn_loop(qpt_thread qpth);
static void qpn_in_thread_init(qpn_nexus qpn);

/*==============================================================================
 * Quagga Nexus Interface -- qpn_xxxx
 *
 */
static qtime_t qpn_max_pselect_wait = QTIME(MAX_PSELECT_WAIT) ;

static qtime_t qpn_wd_interval      = 0 ;

/* Watchdog enabled and debug flags
 */
static bool qpn_wd_enabled  = false ;

enum { qpn_wd_debug  = false } ;

/* The qpnexus qpt_thread_type
 */
static qpt_thread_type_ct qpn_nexus_thread_type =
  {
      .name         = "qpnexus",

      .data_destroy = NULL,     /* dealt with by qpn_reset()    */
      .ret_destroy  = NULL,     /* no return value from qpnexus */
  } ;

/*------------------------------------------------------------------------------
 * Initialise the qpnexus handling -- to be done as soon as state of
 * qpthreads_enabled is established and *before* any watch-dog is started.
 */
extern void
qpn_init(void)
{
  qpn_max_pselect_wait = (qpn_wd_interval == 0) ? QTIME(MAX_PSELECT_WAIT)
                                                : qpn_wd_interval * 7 / 16 ;
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
 * NB: *must* initialise the main_thread qpnexus *first*.
 *
 * The main thread will only block the message queue's signal.
 *
 * Non-main threads will block most signals.
 *
 * Returns the qpn_nexus.
 *
 * NB: this is done in "second stage" initialisation, as soon as we know
 *     whether is going to run qpthreads_enabled or not.
 *
 *     That occurs *before* any daemonisation.
 */
extern qpn_nexus
qpn_init_new(qpn_nexus qpn, bool main_thread, const char* name)
{
  if (qpn == NULL)
    qpn = XCALLOC(MTYPE_QPN_NEXUS, sizeof(struct qpn_nexus)) ;
  else
    memset(qpn, 0,  sizeof(struct qpn_nexus)) ;

  /* Zeroising has set:
   *
   *    name              -- X       -- set below
   *
   *    list              -- NULLs   -- put on list when exec'ed
   *
   *    started           -- false   -- set when exec'd
   *    terminate         -- false   -- not even started, yet !
   *    main_thread       -- false   -- set below, if required
   *
   *    qpth              -- X       -- set below
   *
   *    pselect_mask      -- all zeros
   *    pselect_signal    -- none
   *
   *    selection         -- X       -- set below
   *
   *    pile              -- X       -- set below
   *
   *    queue             -- X       -- set below
   *    mts               -- none
   *
   *    loop              -- X       -- set below
   *
   *    in_thread_init    -- NULLs   -- none, yet
   *    in_thread_final   -- NULLs   -- none, yet
   *    foreground        -- NULLs   -- none, yet
   *    background        -- NULLs   -- none, yet
   *
   *    stats_slk         -- X       -- set below
   *
   *    raw               -- all zero
   *    stats             -- all zero
   *    prev_stats        -- all zero
   *
   *    idleness          -- 0       -- not seen idle, yet
   *    cpu_time          -- 0
   *
   *    cpu_clock_id      -- X       -- set when exec'ed
   */
  qpn->name        = name ;

  qpn->selection   = qps_selection_init_new(qpn->selection);
  qpn->pile        = qtimer_pile_init_new(qpn->pile);
  qpn->queue       = mqueue_init_new(qpn->queue, mqt_signal_unicast, name);
  qpn->main_thread = main_thread;
  qpn->loop        = qpn_loop;

  qpt_spin_init(qpn->stats_slk) ;

 /* We set up the qpt_thread object now, and arrange for it to point at the
  * qpn_nexus.  The thread can find its qpt_thread, and the "data" pointer
  * there points at the qpn_nexus.
  */
  qpn->qpth        = qpt_thread_init(main_thread ? qpt_init_main
                                                 : qpt_init_system_scope,
                                                                         name) ;
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

  /* Immediately destroy related qpt_thread
   */
  qassert(qpn->qpth->type == &qpn_nexus_thread_type) ;

  qpn->qpth = qpt_thread_destroy(qpn->qpth) ;

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
 * Start the main qpnexus qpt_thread.
 *
 * Must be done before any other qpt_threads are created.
 */
extern void
qpn_main_start(qpn_nexus qpn)
{
  assert(qpn->main_thread) ;

  qpt_main_thread_start(qpn->qpth, &qpn_nexus_thread_type, qpn) ;

  qpt_thread_detach(qpn->qpth) ;
} ;

/*------------------------------------------------------------------------------
 * If not main qpnexus create new pthread for the qpnexus and set it going.
 *
 * For the main qpnexus, enters the qpnexus->loop, and does not return until
 * that exits.
 *
 * For other qpnexus, create the pthread and set it going in its qpnexus->loop.
 */
extern void
qpn_exec(qpn_nexus qpn)
{
  if (qpn->main_thread)
    qpn->loop(qpn->qpth);
  else
    qpt_thread_create(qpn->qpth, &qpn_nexus_thread_type, qpn, qpn->loop) ;
} ;

/*------------------------------------------------------------------------------
 * The qpnexus loop.
 *
 * The all qpnexus run in this loop.
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
qpn_loop(qpt_thread qpth)
{
  qpn_nexus qpn ;
  mqueue_block mqb;
  int actions;
  qtime_mono_t now ;
  qtime_t      max_wait ;
  unsigned i;
  bool this ;
  bool prev ;
  bool wait_mq ;
  bool idle ;

  /* First things absolutely first
   */
  qpn = qpt_thread_start(qpth) ;

  /* now in our thread, complete initialisation
   */
  qpn_in_thread_init(qpn);

  /* custom in-thread initialization
   */
  for (i = 0; i < qpn->in_thread_init.count ;)
    ((qpn_init_function*)(qpn->in_thread_init.hooks[i++]))() ;

  /* Until required to terminate, loop
   */
  prev = true ;
  this = true ;
  idle = false ;
  while (!qpn->terminate)
    {
      ++qpn->raw.cycles ;

      /* Signals are highest priority -- only execute for main thread
       */
      if (qpn->main_thread)
        {
          int ret = quagga_sigevent_process() ;
          if (ret != 0)
            {
              this = true ;
              ++qpn->raw.signals ;
            } ;
        } ;

      /* Foreground hooks, if any.
       */
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

      qpn->stats = qpn->raw ;
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
      if (qpn_wd_debug && qpn_wd_enabled && ((rand() % 100) == 0))
        max_wait += QTIME(1) * ((rand() % 111) + 10) ;

      actions = qps_pselect(qpn->selection, max_wait) ;

      now = qt_get_monotonic() ;

      if (idle)
        {
          qpn->raw.idle += now - qpn->raw.last_time ;

          if (qpn_wd_enabled)
            {
              qpt_spin_lock(qpn->stats_slk) ;
              qpn->idleness = 0 ;
              qpt_spin_unlock(qpn->stats_slk) ;
            } ;
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

  /* custom in-thread finalization
   */
  for (i = qpn->in_thread_final.count; i > 0 ;)
    ((qpn_init_function*)(qpn->in_thread_final.hooks[--i]))() ;

  /* Last things last
   */
  return qpt_thread_end(NULL) ;
}

/*------------------------------------------------------------------------------
 * Now running in our thread, do common initialisation
 */
static void
qpn_in_thread_init(qpn_nexus qpn)
{
  sigset_t sigmask[1];

  /* Reset the raw statistics and the spin-lock which protects same.
   */
  memset(&qpn->raw, 0, sizeof(qpn_stats_t)) ;

  qpn->raw.start_time = qt_get_monotonic() ;
  qpn->raw.last_time  = qpn->raw.start_time ;

  qpt_spin_lock(qpn->stats_slk) ;
  qpn->prev_stats = qpn->stats = qpn->raw ;     /* share loop time etc.  */
  qpt_spin_unlock(qpn->stats_slk) ;

#if 0

  /* Complete the initialisation of nexus, once thread is running.
   *
   * Collect thread's id and cputime clock id
   *
   * Set the thread's qpn_self to point at its qpnexus.
   *
   * Add nexus to list of live ones and set "started".
   */
  qpn->cpu_clock_id = qpt_get_cpu_clock_id(qpn->thread_id) ;

  qpn_nexus_add(qpn) ;          /* may now be visible to watch-dog      */
#endif

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

  /* The signal mask to be used during pselect()
   */
  sigcopyset(qpn->pselect_mask, sigmask) ;
  sigdelset(qpn->pselect_mask, SIG_INTERRUPT) ;
  qpn->pselect_signal = SIG_INTERRUPT ;

  /* Now we have thread_id and mask, prep for using message queue.
   */
  if (qpn->queue != NULL)
    qpn->mts = mqueue_thread_signal_init(qpn->mts, qpn->qpth, SIG_INTERRUPT) ;
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
        qpt_thread_raise(qpn->qpth, SIG_INTERRUPT);
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
static qpt_spin_t qpn_wd_spin_lock ;    /* protects qpn_wd_stop flag    */

static volatile bool qpn_wd_stop = false ;

static qpt_thread qpn_wd_qpth    = NULL ;

static FILE*    qpn_wd_log_file  = NULL ;
static qpath    qpn_wd_log_path  = NULL ;

/* The qpnexus watchdog qpt_thread_type
 */
static qpt_thread_type_ct qpn_nexus_watch_dog_type =
  {
      .name         = "qpnexus watchdog",

      .data_destroy = NULL,     /* no data              */
      .ret_destroy  = NULL,     /* no return value      */
  } ;

/* Prototypes
 */
static void* qpn_wd_activity(qpt_thread qpth) ;
static void qpn_wd_log(const char* format, ...)         PRINTF_ATTRIBUTE(1, 2) ;
static void qpn_wd_check_intervals(qtime_t sleep, qtime_t mono_interval,
                                      qtime_t real_interval, uint eintr_count) ;
static void qpn_wd_check_nexuses(qstring report) ;
static void qpn_wd_check_mutexes(void) ;

/*------------------------------------------------------------------------------
 * Initialise the watch-dog -- completely dormant !
 */
extern void
qpn_wd_start_up(void)
{
  qpn_wd_enabled  = false ;
  qpn_wd_interval = 0 ;

  qpn_wd_stop     = false ;

  qpn_wd_qpth     = NULL ;      /* not started !        */

  qpn_wd_log_file = NULL ;
  qpn_wd_log_path = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Prepare the watch-dog from command line
 *
 * Take command line argument and set the watch-dog interval and file.
 *
 * Option is: -W\=?\d+(\,\+?filename)?
 *
 *            Where can have whitespace (actually, anything <= ' ') before
 *            any of the elements:  \=  \d  \, \+ and the filename.
 *
 *            The \d+ must be a decimal number > 0 (leading zeros ignored).
 *
 * If no filename is given, output is to stderr.  If +filename, the file
 * is appended to.
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

  if (qpn_wd_enabled)
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

  while ((*(unsigned char*)rest <= ' ') && (*rest != '\0'))
    ++rest ;

  if ((errno != 0) || (interval == 0) || (interval > 60) ||
                                            ((*rest != '\0') && (*rest != ',')))
    {
      fprintf(stderr, "*** invalid watch-dog interval in '%s'\n", arg) ;
      return false ;
    } ;

  qpn_wd_interval = QTIME(interval) ;

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

      qassert(*rest == ',') ;

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

  qpn_wd_enabled  = true ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Start the watch-dog if it has been set to run
 *
 * Must be called *before* any qpn_exec().
 *
 *
 */
extern void
qpn_wd_start(void)
{
  assert(qpn_wd_qpth == NULL) ;

  if (qpn_wd_enabled)
    {
      qpt_thread qpth ;

      qpt_spin_init(qpn_wd_spin_lock) ;

      qpth = qpt_thread_init(qpt_init_system_scope, "Watch-Dog") ;

      qpt_thread_detach(qpth) ;
      qpn_wd_qpth = qpt_thread_set_ref(qpth) ;

      qpt_thread_create(qpth, &qpn_nexus_watch_dog_type,
                                          NULL /* no data */, qpn_wd_activity) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Stop the watch-dog
 */
extern void
qpn_wd_finish(void)
{
  if (qpn_wd_enabled)
    {
      qpt_spin_lock(qpn_wd_spin_lock) ;
      qpn_wd_stop = true ;
      qpt_spin_unlock(qpn_wd_spin_lock) ;

      qpt_thread_raise(qpn_wd_qpth, SIG_INTERRUPT) ;

      qpn_wd_qpth = qpt_thread_clear_ref(qpn_wd_qpth) ;
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
qpn_wd_activity(qpt_thread qpth)
{
  sigset_t sigmask[1], sigwait[1] ;
  const char* using ;
  qstring report ;
  qtime_t sleep, mono_end ;
  uint debug = 0 ;

  /* First things absolutely first
   */
  qpt_thread_start(qpth) ;

  /* Log loop
   */
  using  = use_nanosleep ? "nanosleep" : "pselect" ;
  qpn_wd_log("Watch-Dog started (%s)", using) ;

  report = qs_new(100) ;

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
       * recording the CLOCK_MONOTONIC time when we loop.
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

      if (qpn_wd_stop_now())
        break ;

      if (qpn_wd_debug && ((rand() % 50) == 0))
        {
          /* For testing purposes... fiddle with the clock and the nexus
           * idleness.
           */
          qtime_t delta = (QTIME(1) / 4) * ((rand() % 21) - 5) ;

          switch (debug % 3)
            {
              case 0:           /* fiddle with real_end         */
                real_end += delta ;
                break ;

              case 1:           /* fiddle with mono_end         */
                mono_end += delta ;
                break ;

              case 2:           /* fiddle with both ends        */
                real_end += delta ;
                mono_end += delta ;
                break ;
            } ;

          ++debug ;
        }

      mono_interval = mono_end - mono_start ;
      real_interval = real_end - real_start ;

      /* Check the mono_interval and real_interval -- reporting the eintr_count
       * if those are not satisfactory
       */
      qpn_wd_check_intervals(sleep, mono_interval, real_interval, eintr_count) ;

      /* Look for CPU utilisation and whether any nexus is stalled
       */
      qpn_wd_check_nexuses(report) ;

      qpn_wd_log("%s", qs_string(report)) ;

      /* Check that we can lock all the known mutexes
       */
      qpn_wd_check_mutexes() ;
    } ;

  /* Termination
   */
  qpn_wd_log("Watch-Dog terminated (%s)", using) ;

  qpt_spin_destroy(qpn_wd_spin_lock) ;

  fclose(qpn_wd_log_file) ;

  qpath_free(qpn_wd_log_path) ;
  qs_free(report) ;

  /* Last things last
   */
  return qpt_thread_end(NULL) ;
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
 *
 * We walk the nexuses and fetch the cpu-time for each one, so the interval
 * given needs to be the interval since the last time we did this.
 */
static void
qpn_wd_check_nexuses(qstring report)
{
  qpt_thread qpth ;

  qs_clear(report) ;

  qpth = NULL ;
  while ((qpth = qpt_thread_walk(qpth)) != NULL)
    {
      qpn_nexus   qpn ;
      uint        idleness ;
      qpt_thread_state_t state ;
      qpt_thread_stats_t prev ;
      qtime_t     interval ;
      double      pc_cpu ;
      const char* tag ;

      /* Only report on the types of qpt_thread we know about
       */
      if (qpth->type != &qpn_nexus_thread_type)
        continue ;

      qpn = qpth->data ;

      /* Take copy of previous stats and get the latest
       */
      *prev = *qpn->qpth_stats ;
      state = qpt_thread_get_stats(qpth, qpn->qpth_stats) ;

      /* See if the qpnexus has been idle
       */
      idleness = 0 ;

      if (state == qpts_running)
        {
          qpt_spin_lock(qpn->stats_slk) ;       /*<-<-<-<-<-<-<-<-<-<-<-*/

          if (qpn->idleness != 0)
            idleness = ++qpn->idleness ;

          qpt_spin_unlock(qpn->stats_slk) ;     /*<-<-<-<-<-<-<-<-<-<-<-*/
        } ;

      /* Check for stalled and collect output for this nexus
       */
      if (idleness > 2)
        qpn_wd_log("*** %s nexus stalled -- %u", qpth->name, idleness - 2) ;

      interval = qpn->qpth_stats->cpu_when - prev->cpu_when ;

      if (interval > 0)
        pc_cpu = (double)((qpn->qpth_stats->cpu_used - prev->cpu_used) * 100)
                                                            / (double)interval ;
      else
        pc_cpu = (double)0.0 ;

      if (pc_cpu > (double)999.00)
        pc_cpu = (double)999.99 ;

      switch (idleness)
        {
          /* Tags: "!" => not running
           *       "+" => active when watch-dog ran
           *       " " => idle when watch-dog ran
           *       "*" => idle since last time watch-dog ran
           */
          case 0:
            tag = (state == qpts_running) ? "+" : "!" ;
            break ;

          case 1:
            tag = "?" ;         /* not possible !       */
            break ;

          case 2:
            tag = " " ;
            break ;

          default:
            tag = "*" ;
            break ;
        } ;

      qs_printf_a(report, " %16s%s%6.2f%%", qpth->name, tag, pc_cpu) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Worry whether we can pick up all known mutexes in under 1 sec each.
 */
static void
qpn_wd_check_mutexes(void)
{
  qpt_mutex     mx ;

  mx = NULL ;
  while ((mx = qpt_mutex_step_next(mx)) != NULL)
    {
      if (qpt_mutex_timedlock(mx, QTIME(1)))
        qpt_mutex_unlock(mx) ;
      else
        qpn_wd_log("*** failed to lock %s mutex", mx->name) ;
    } ;
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

  fflush(qpn_wd_log_file) ;
} ;

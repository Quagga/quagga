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

#include <signal.h>

#include "qpthreads.h"
#include "memory.h"

/*==============================================================================
 * Quagga Pthread Interface -- qpt_xxxx
 *
 * Here (and in qpthreads.h) are captured all the pthreads features used in
 * Quagga.
 *
 * This provides:
 *
 *   * "wrappers" around functions which should not fail, but whose return
 *     code it is best to check... at least in a debug environment.
 *
 *   * the possibility of a separate no pthreads build where pthread facilities
 *     are either dummied out or otherwise dealt with.
 *
 *   * the ability to add any work-arounds which may be required if poorly
 *     conforming pthreads implementations are encountered
 *
 * Pthread Thread Attributes -- Scheduling
 * =======================================
 *
 * Pthreads defines some useful looking real-time scheduling features.
 *
 * One would like to be able to give I/O intensive threads an advantage over
 * CPU bound threads.
 *
 * Unfortunately, conformance allows a system to have its own scheduling
 * system -- so long as the standard ones are implemented.  Further, there is
 * no way of telling what priority values are reasonable, even in the standard
 * scheduling policies.
 *
 * The approach taken here is that by default a thread will be created with
 * the system default attributes -- which may mean inheriting the creating
 * thread's scheduling attributes.
 *
 * It is also possible to construct a set of attributes, using the most
 * obviously useful properties.  It is envisaged that this may be used when a
 * configuration file is used to set locally sensible values.   The attributes
 * supported are:
 *
 *    * attr_detached       -- whether to start detached or not
 *    * attr_inherit_sched  -- whether to inherit scheduling attributes
 *    * attr_sched_scope    -- scheduling scope
 *    * attr_sched_policy   -- scheduling policy
 *    * attr_sched_priority -- scheduling priority
 *
 * See qpt_thread_attr_init, below.
 *
 * Not supported here are:
 *
 *    * attr_guardsize
 *    * attr_stack
 *    * attr_stacksize
 *
 * Pthread Mutex Attributes -- Error Checking
 * ==========================================
 *
 * Mutexes are kept simple, only attr_type is used, and that by default.
 *
 * POSIX defines four types of mutex:
 *
 *   _NORMAL      no ownership check   -- owner will deadlock if locks mutex !
 *                                     -- undefined what happens if unlock
 *                                        mutex not owned by self !
 *                no recursive locking
 *
 *   _ERRORCHECK  checks for ownership on lock and unlock
 *                no recursive locking
 *
 *   _RECURSIVE   checks for ownership on lock and unlock
 *                counts up locks and counts down unlocks
 *
 *                This looks useful, but goes wrong with condition variables !
 *
 *   _DEFAULT     undefined whether checks owner or not, on lock and/or unlock.
 *                no recursive locking
 *
 * See qpthreads.h for discussion of Quagga's standard type (QPT_MUTEX_TYPE).
 *
 * Other attributes are left in their default state:
 *
 *   * attr_prioceiling  -- default undefined
 *   * attr_protocol     -- default undefined
 *   * attr_pshared      -- defaults to _PROCESS_PRIVATE
 *
 * For the time being it is assumed that these are too exotic.
 *
 * Pthread Condition Variable Attributes
 * =====================================
 *
 * Condition variables have only two attributes:
 *
 *   * attr_clock        -- which clock to use
 *   * attr_pshared      -- defaults to _PROCESS_PRIVATE
 *
 * The use a clock other than Quagga's standard (QPT_COND_CLOCK_ID) is possible,
 * but not recommended.  (See qpthreads.h for discussion of this.)
 *
 * Pthread Specific Signal Handling
 * ================================
 *
 * In a threaded application, need to use pthread_sigmask (not sigproc_mask).
 * (Can use pthread_sigmask in a single threaded application.)
 *
 * To direct a signal at a given thread need pthread_kill. *
 */

/*==============================================================================
 * Thread creation and attributes.
 *
 * Threads may be created with a given set of attributes if required.
 *
 * qpt_thread_attr_init() will initialise a set of attributes including the
 * current standard scheduling attributes.  It is envisaged that configuration
 * options may be used to specify these.
 *
 * qpt_thread_create() creates a thread using the given attributes.  If those
 * are NULL, then the system defaults are used.
 */

/* Initialise a set of attributes -- setting the scheduling options.
 *
 * Options:
 *
 *   qpt_attr_joinable       -- the default if nothing specified.
 *   qpt_attr_detached       -- overrides qpt_attr_joinable.
 *
 *   qpt_attr_sched_inherit  -- all scheduling attributes are to be inherited.
 *                              No explicit scheduling attributes may be set.
 *
 *   qpt_attr_sched_scope    -- set explicit, given, scope.
 *   qpt_attr_sched_policy   -- set explicit, given, policy
 *   qpt_attr_sched_priority -- set explicit, given, priority
 *
 * If none of the _sched_ options are given, then the scheduling attributes are
 * left to whatever default values the system chooses.
 *
 * If the _sched_inherit option is specified, none of the other _sched_ options
 * may be specified.
 *
 * If any of the explicit scheduling options are given, they are set in this
 * order.  If only some of these options are given, then the caller is
 * assuming that the system will choose sensible defaults.
 *
 * The scope, policy and priority arguments are use only if the corresponding
 * option is specified.
 *
 * Returns the address of the qpt_thread_attr_t structure.
 */
qpt_thread_attr_t*
qpt_thread_attr_init(qpt_thread_attr_t* attr, enum qpt_attr_options opts,
                                          int scope, int policy, int priority)
{
  int err ;

  assert((opts & ~qpt_attr_known) == 0) ;

  /* Initialise thread attributes structure (allocating if required.)   */
  if (attr == NULL)
    attr = XMALLOC(MTYPE_QPT_THREAD_ATTR, sizeof(qpt_thread_attr_t)) ;

  err = pthread_attr_init(attr) ;
  if (err != 0)
    zabort_err("pthread_attr_init failed", err) ;

  /* If not qpt_attr_detached, then set joinable.       */
  err = pthread_attr_setdetachstate(attr,
                         (opts & qpt_attr_detached) ? PTHREAD_CREATE_DETACHED
                                                    : PTHREAD_CREATE_JOINABLE) ;
  if (err != 0)
    zabort_err("pthread_attr_setdetachstate failed", err) ;

  /* If setting anything to do with scheduling...       */
  if (opts & qpt_attr_sched_setting)
    {
      /* Either we inherit or we set explicit parameters.       */

      err = pthread_attr_setinheritsched(attr,
                    (opts & qpt_attr_sched_inherit) ? PTHREAD_INHERIT_SCHED
                                                    : PTHREAD_EXPLICIT_SCHED) ;
      if (err != 0)
        zabort_err("pthread_attr_setinheritsched", err) ;

      if (opts & qpt_attr_sched_inherit)
        assert((opts & qpt_attr_sched_explicit) == 0) ;
      else
        {
          if (opts & qpt_attr_sched_scope)
            {
              err = pthread_attr_setscope(attr, scope) ;
              if (err != 0)
                zabort_err("pthread_attr_setscope failed", err) ;
            } ;
          if (opts & qpt_attr_sched_policy)
            {
              err = pthread_attr_setschedpolicy(attr, scope) ;
              if (err != 0)
                zabort_err("pthread_attr_setschedpolicy failed", err) ;
            } ;
          if (opts & qpt_attr_sched_priority)
            {
              struct sched_param sparm ;
              err = pthread_attr_getschedparam(attr, &sparm) ;
              if (err != 0)
                zabort_err("pthread_attr_getschedparam failed", err) ;
              sparm.sched_priority = priority ;
              err = pthread_attr_setschedparam(attr, &sparm) ;
              if (err != 0)
                zabort_err("pthread_attr_setschedparam failed", err) ;
            } ;
        } ;
    } ;

  /* Done -- return qpt_thread_attr_t*     */
  return attr ;
} ;

/* Create Thread with given attributes (if any).
 *
 * If no attributes are given (attr == NULL) the thread is created with system
 * default attributes -- *except* that it is created joinable.
 *
 * Returns the qpt_thread_t "thread id".
 */
qpt_thread_t
qpt_thread_create(void* (*start)(void*), void* arg, qpt_thread_attr_t* attr)
{
  qpt_thread_attr_t thread_attr ;
  qpt_thread_t      thread_id ;
  int default_attr ;
  int err ;

  default_attr = (attr == NULL) ;
  if (default_attr)
    attr = qpt_thread_attr_init(&thread_attr, qpt_attr_joinable, 0, 0, 0) ;

  err = pthread_create(&thread_id, attr, start, arg) ;
  if (err != 0)
    zabort_err("pthread_create failed", err) ;

  if (default_attr)
    {
      err = pthread_attr_destroy(attr) ;        /* being tidy */
      if (err != 0)
        zabort_err("pthread_attr_destroy failed", err) ;
    } ;

  return thread_id ;
} ;

/*==============================================================================
 * Mutex initialise and destroy.
 */

/* Initialise Mutex (allocating if required).
 *
 * Options:
 *
 *   qpt_mutex_quagga      -- see qpthreads.h for discussion of this.
 *   qpt_mutex_normal      -- ie PTHREAD_MUTEX_NORMAL
 *   qpt_mutex_recursive   -- ie PTHREAD_MUTEX_RECURSIVE
 *   qpt_mutex_errorcheck  -- ie PTHREAD_MUTEX_ERRORCHECK
 *   qpt_mutex_default     -- system default
 *
 * Of these _recursive is the most likely alternative to _quagga...  BUT do
 * remember that such mutexes DO NOT play well with condition variables.
 */
qpt_mutex_t*
qpt_mutex_init(qpt_mutex_t* mx, enum qpt_mutex_options opts)
{
  pthread_mutexattr_t mutex_attr ;
  int type ;
  int err ;

  if (mx == NULL)
    mx = XMALLOC(MTYPE_QPT_MUTEX, sizeof(qpt_mutex_t)) ;

  /* Set up attributes so we can set the mutex type     */
  err = pthread_mutexattr_init(&mutex_attr);
  if (err != 0)
    zabort_err("pthread_mutexattr_init failed", err) ;

  switch(opts)
  {
    case qpt_mutex_quagga:
      type = QPT_MUTEX_TYPE ;
      break ;
    case qpt_mutex_normal:
      type = PTHREAD_MUTEX_NORMAL ;
      break ;
    case qpt_mutex_recursive:
      type = PTHREAD_MUTEX_RECURSIVE ;
      break ;
    case qpt_mutex_errorcheck:
      type = PTHREAD_MUTEX_ERRORCHECK ;
      break ;
    case qpt_mutex_default:
      type = PTHREAD_MUTEX_DEFAULT ;
      break ;
    default:
      zabort("Invalid qpt_mutex option") ;
  } ;

#ifndef PTHREAD_MUTEXATTR_SETTYPE_MISSING
  err = pthread_mutexattr_settype(&mutex_attr, type);
  if (err != 0)
    zabort_err("pthread_mutexattr_settype failed", err) ;
#endif

  /* Now we're ready to initialize the mutex itself     */
  err = pthread_mutex_init(mx, &mutex_attr) ;
  if (err != 0)
    zabort_err("pthread_mutex_init failed", err) ;

  /* Be tidy with the attributes                        */
  err = pthread_mutexattr_destroy(&mutex_attr) ;
  if (err != 0)
    zabort_err("pthread_mutexattr_destroy failed", err) ;

  /* Done: return the mutex                             */
  return mx ;
} ;

/* Destroy given mutex, and (if required) free it.
 *
 * Returns NULL.
*/
qpt_mutex_t*
qpt_mutex_destroy(qpt_mutex_t* mx, int free_mutex)
{
  int err ;

  err = pthread_mutex_destroy(mx) ;
  if (err != 0)
    zabort_err("pthread_mutex_destroy failed", err) ;

  if (free_mutex)
      XFREE(MTYPE_QPT_MUTEX, mx) ;

  return NULL ;
} ;

/*==============================================================================
 * Condition Variable initialise and destroy.
 */

/* Initialise Condition Variable (allocating if required).
 *
 * Options:
 *
 *   qpt_cond_quagga     -- use Quagga's default clock
 *   qpt_cond_realtime   -- force CLOCK_REALTIME
 *   qpt_cond_monotonic  -- force CLOCK_MONOTONIC  (if available)
 */
qpt_cond_t*
qpt_cond_init(qpt_cond_t* cv, enum qpt_cond_options opts)
{
  pthread_condattr_t cond_attr ;
  int clock ;
  int err ;

  if (cv == NULL)
    cv = XMALLOC(MTYPE_QPT_COND, sizeof(qpt_cond_t)) ;

  /* Set up attributes so we can set the  type     */
  err = pthread_condattr_init(&cond_attr);
  if (err != 0)
    zabort_err("pthread_condattr_init failed", err) ;

  switch(opts)
  {
    case qpt_cond_quagga:
      clock = QPT_COND_CLOCK_ID ;
      break ;
    case qpt_cond_realtime:
      clock = CLOCK_REALTIME ;
      break ;
    case qpt_cond_monotonic:
      clock = CLOCK_MONOTONIC ;
      break ;
    default:
      zabort("Invalid qpt_cond option") ;
  } ;

  err = pthread_condattr_setclock(&cond_attr, clock);
  if (err != 0)
    zabort_err("pthread_condattr_setclock failed", err) ;

  /* Now we're ready to initialize the condition variable itself        */
  err = pthread_cond_init(cv, &cond_attr) ;
  if (err != 0)
    zabort_err("pthread_cond_init failed", err) ;

  /* Be tidy with the attributes                        */
  err = pthread_condattr_destroy(&cond_attr) ;
  if (err != 0)
    zabort_err("pthread_condattr_destroy failed", err) ;

  /* Done: return the condition variable                */
  return cv ;
} ;

/* Destroy given mutex, and (if required) free it.
 *
 * Returns NULL.
*/
qpt_cond_t*
qpt_cond_destroy(qpt_cond_t* cv, int free_cond)
{
  int err ;

  err = pthread_cond_destroy(cv) ;
  if (err != 0)
    zabort_err("pthread_cond_destroy failed", err) ;

  if (free_cond)
      XFREE(MTYPE_QPT_COND, cv) ;

  return NULL ;
} ;

/*==============================================================================
 * Signal Handling.
 */

/* Set thread signal mask
 *
 * Thin wrapper around pthread_sigmask.
 *
 * zaborts if gets any error.
 */
void
qpt_thread_sigmask(int how, const sigset_t* set, sigset_t* oset)
{
  int err ;

  err = pthread_sigmask(how, set, oset) ;
  if (err != 0)
    zabort_err("pthread_sigmask failed", err) ;
} ;

/* Send given thread the given signal
 *
 * Thin wrapper around pthread_kill.
 *
 * zaborts if gets any error.
 */
void
qpt_thread_signal(qpt_thread_t thread, int signum)
{
  int err ;

  err = pthread_kill(thread, signum) ;
  if (err != 0)
    zabort_err("pthread_kill failed", err) ;
} ;

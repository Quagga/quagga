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

#ifndef _ZEBRA_QPTHREADS_H
#define _ZEBRA_QPTHREADS_H

#include <stdint.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>

#include "zassert.h"
#include "qtime.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Quagga Pthread Interface -- qpt_xxxx
 *
 * Here are captured all the pthreads features used in Quagga.
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
 */

#if !defined(_POSIX_THREADS) || (_POSIX_THREADS <= 0)
#error Require _POSIX_THREADS
#endif

/*==============================================================================
 * Data types
 */

typedef pthread_t        qpt_thread_t ;
typedef pthread_mutex_t  qpt_mutex_t ;
typedef pthread_cond_t   qpt_cond_t ;

typedef pthread_attr_t   qpt_thread_attr_t ;

/*==============================================================================
 * Thread Creation -- see qpthreads.c for further discussion.
 */

enum qpt_attr_options
{
  qpt_attr_joinable        = 0,         /* the default for Quagga       */

  qpt_attr_detached        = 0x0001,    /* otherwise will set joinable  */

  qpt_attr_sched_inherit   = 0x0002,    /* otherwise will use default   */

  qpt_attr_sched_scope     = 0x0004,    /* otherwise inherit/default    */
  qpt_attr_sched_policy    = 0x0008,    /* otherwise inherit/default    */
  qpt_attr_sched_priority  = 0x0010,    /* otherwise inherit/default    */
} ;

#define qpt_attr_sched_explicit  ( qpt_attr_sched_scope   \
                                 | qpt_attr_sched_policy  \
                                 | qpt_attr_sched_priority )

#define qpt_attr_sched_setting   ( qpt_attr_sched_inherit \
                                 | qpt_attr_sched_explicit )

#define qpt_attr_known ( qpt_attr_detached | qpt_attr_sched_setting )

extern qpt_thread_attr_t*
qpt_thread_attr_init(qpt_thread_attr_t* attr, enum qpt_attr_options opts,
                                          int scope, int policy, int priority) ;
extern qpt_thread_t
qpt_thread_create(void* (*start)(void*), void* arg, qpt_thread_attr_t* attr) ;

/*==============================================================================
 * Thread self knowledge.
 */

Inline qpt_thread_t qpt_thread_self(void)
{
  return pthread_self() ;
} ;

/*==============================================================================
 * Mutex handling.
 *
 * Quagga's default mutex type is:
 *
 *   * PTHREAD_MUTEX_ERRORCHECK unless NDEBUG && NDEBUG_QPTHREADS
 *   * QPT_MUTEX_TYPE_DEFAULT
 *
 * QPT_MUTEX_TYPE_DEFAULT may be set elsewhere.  If it is not set then it is
 * set here to be PTHREAD_MUTEX_NORMAL.
 *
 * NB: on the face of it PTHREAD_MUTEX_NORMAL should be the fastest.  It is
 *     possible that PTHREAD_MUTEX_DEFAULT may have system specific semantics
 *     that make it faster than the standard _NORMAL.  It is also possible that
 *     a given system may elect to provide a safer mutex than the _NORMAL by
 *     default.
 *
 *     If _DEFAULT is faster than _NORMAL, then QPT_MUTEX_TYPE_DEFAULT may be
 *     used to override this choice.
 *
 * NB: do not (currently) support pthread_mutex_timedlock().
 */

enum qpt_mutex_options
{
  qpt_mutex_quagga      = 0x0000,       /* Quagga's default     */
  qpt_mutex_normal      = 0x0001,
  qpt_mutex_recursive   = 0x0002,
  qpt_mutex_errorcheck  = 0x0003,
  qpt_mutex_default     = 0x0004,       /* system default       */
} ;

#ifndef QPT_MUTEX_TYPE_DEFAULT
# define QPT_MUTEX_TYPE_DEFAULT  PTHREAD_MUTEX_NORMAL
#endif

#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
# define QPT_MUTEX_TYPE  QPT_MUTEX_TYPE_DEFAULT
#else
# define QPT_MUTEX_TYPE  PTHREAD_MUTEX_ERRORCHECK
#endif

extern qpt_mutex_t*
qpt_mutex_init(qpt_mutex_t* mx, enum qpt_mutex_options opts) ;

extern qpt_mutex_t*
qpt_mutex_destroy(qpt_mutex_t* mx, int free_mutex) ;

#define qpt_mutex_destroy_keep(mx) qpt_mutex_destroy(mx, 0)
#define qpt_mutex_destroy_free(mx) qpt_mutex_destroy(mx, 1)

Inline void
qpt_mutex_lock(qpt_mutex_t* mx) ;       /* do nothing if mx == NULL     */

Inline int
qpt_mutex_trylock(qpt_mutex_t* mx) ;    /* do nothing if mx == NULL     */

Inline void
qpt_mutex_unlock(qpt_mutex_t* mx) ;     /* do nothing if mx == NULL     */

/*==============================================================================
 * Condition Variable handling
 *
 * Quagga's default clock for condition variables is QPT_COND_CLOCK_ID, which
 * may be set elsewhere.  If it is not set then it is set here to be:
 *
 *   * CLOCK_MONOTONIC if available
 *   * CLOCK_REALTIME otherwise  -- this is the standard default.
 *
 * QPT_COND_CLOCK_MONOTONIC is set if CLOCK_MONOTONIC is used (and must be set
 * if QPT_COND_CLOCK_ID is set elsewhere to something that is monotonic).
 *
 * NB: qpt_cond_get_timeout_time uses QPT_COND_CLOCK_ID.
 *
 *     If a specific clock is required, it can be set... but the user of the
 *     condition variable must take care to base time-out times on that clock.
 *
 * NB: static initialisation of condition variables is not supported, to avoid
 *     confusion between the standard default and Quagga's default.
 */

#ifndef QPT_COND_CLOCK_ID
# ifndef HAVE_CLOCK_MONOTONIC
#  define QPT_COND_CLOCK_ID  CLOCK_REALTIME
#  undef  QPT_COND_CLOCK_MONOTONIC
# else
#  define QPT_COND_CLOCK_ID  CLOCK_MONOTONIC
#  define QPT_COND_CLOCK_MONOTONIC  1
# endif
#endif

enum qpt_cond_options
{
  qpt_cond_quagga      = 0x0000,  /* Quagga's default   */
  qpt_cond_realtime    = 0x0001,  /* standard default   */
  qpt_cond_monotonic   = 0x0002,
} ;

extern qpt_cond_t*
qpt_cond_init(qpt_cond_t* cv, enum qpt_cond_options opts) ;

extern qpt_cond_t*
qpt_cond_destroy(qpt_cond_t* cv, int free_cond) ;

#define qpt_cond_destroy_keep(cv) qpt_cond_destroy(cv, 0)
#define qpt_cond_destroy_free(cv) qpt_cond_destroy(cv, 1)

Inline void
qpt_cond_wait(qpt_cond_t* cv, qpt_mutex_t* mx) ;

Inline qtime_t
qpt_cond_get_timeout_time(qtime_t wait) ;

Inline int
qpt_cond_timedwait(qpt_cond_t* cv, qpt_mutex_t* mx, qtime_t tot) ;

Inline void
qpt_cond_signal(qpt_cond_t* cv) ;

Inline void
qpt_cond_broadcast(qpt_cond_t* cv) ;

/*==============================================================================
 * Mutex inline functions
 */

/* Lock given mutex.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */

Inline void
qpt_mutex_lock(qpt_mutex_t* mx)         /* do nothing if mx == NULL     */
{
  if (mx != NULL)
    {
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
      pthread_mutex_lock(mx) ;
#else
      int err = pthread_mutex_lock(mx) ;
      if (err != 0)
        zabort_err("pthread_mutex_lock failed", err) ;
#endif
    } ;
} ;

/* Try to lock given mutex.
 *
 * Returns: lock succeeded (1 => have locked, 0 => unable to lock).
 *
 * Has to check the return value, so zabort_errno if not EBUSY.
 */

Inline int
qpt_mutex_trylock(qpt_mutex_t* mx)      /* do nothing if mx == NULL     */
{
  if (mx != NULL)
    {
      int err = pthread_mutex_trylock(mx) ;
      if (err == 0)
        return 1 ;                      /* success: it's locked.        */
      if (err == EBUSY)
        return 0 ;                      /* unable to lock               */

      zabort_err("pthread_mutex_trylock failed", err) ;
                                        /* crunch                       */
    } ;
} ;

/* Unlock given mutex.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_mutex_unlock(qpt_mutex_t* mx)       /* do nothing if mx == NULL     */
{
  if (mx != NULL)
    {
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
      pthread_mutex_unlock(mx) ;
#else
      int err = pthread_mutex_unlock(mx) ;
      if (err != 0)
        zabort_err("pthread_mutex_unlock failed", err) ;
#endif
    } ;
} ;

/*==============================================================================
 * Condition variable inline functions
 */

/* Wait for given condition variable.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */

Inline void
qpt_cond_wait(qpt_cond_t* cv, qpt_mutex_t* mx)
{
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
  pthread_cond_wait(cv, mx) ;
#else
  int err = pthread_cond_wait(cv, mx) ;
  if (err != 0)
    zabort_err("pthread_cond_wait failed", err) ;
#endif
} ;

/* Get a time-out time (for use with qpt_cond_timedwait).
 *
 * Returns the current system time plus the given wait time.
 */

Inline qtime_t
qpt_cond_get_timeout_time(qtime_t wait)
{
  return qt_clock_gettime(QPT_COND_CLOCK_ID) + wait ;
} ;

/* Wait for given condition variable or time-out.
 *
 * Returns: wait succeeded (1 => success, 0 => timed-out).
 *
 * The time-out is an *absolute* time expressed as a qtime_t.
 *
 * NB: qpt_cond_get_timeout_time() should be used to generate the required
 *     time-out.  That uses CLOCK_
 *
 * Has to check the return value, so zabort_errno if not EBUSY.
 */

Inline int
qpt_cond_timedwait(qpt_cond_t* cv, qpt_mutex_t* mx, qtime_t tot)
{
  struct timespec ts ;

  int err = pthread_cond_timedwait(cv, mx, qtime2timespec(&ts, tot)) ;
  if (err == 0)
    return 1 ;                  /* got condition        */
  if (err == ETIMEDOUT)
    return 0 ;                  /* got time-out         */

  zabort_err("pthread_cond_timedwait failed", err) ;
                                /* crunch               */
} ;

/* Signal given condition.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */

Inline void
qpt_cond_signal(qpt_cond_t* cv)
{
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
  pthread_cond_signal(cv) ;
#else
  int err = pthread_cond_signal(cv) ;
  if (err != 0)
    zabort_err("pthread_cond_signal failed", err) ;
#endif
} ;

/* Broadcast given condition.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_cond_broadcast(qpt_cond_t* cv)
{
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
  pthread_cond_broadcast(cv) ;
#else
  int err = pthread_cond_broadcast(cv) ;
  if (err != 0)
    zabort_err("pthread_cond_broadcast failed", err) ;
#endif
} ;

/*==============================================================================
 * Signal Handling.
 */
void
qpt_thread_sigmask(int how, const sigset_t* set, sigset_t* oset) ;

void
qpt_thread_signal(qpt_thread_t thread, int signum) ;

#endif /* _ZEBRA_QPTHREADS_H */

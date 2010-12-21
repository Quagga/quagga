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

#include "misc.h"
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>

#include "zassert.h"
#include "qtime.h"

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
 * Global Switch -- this allows the library to be run WITHOUT pthreads !
 *
 * Nearly every qpthreads function is a NOP if !qpthreads_enabled.
 *
 * Early in the morning a decision may be made to enable qpthreads -- that must
 * be done before any threads are created (or will zabort) and before any
 * mutexes and condition variables are initialised (or it will be too late).
 *
 * Use: qpthreads_enabled        -- to test for the enabled-ness
 *      qpthreads_enabled_freeze -- to test and freeze unset if not yet enabled
 */

#define qpthreads_enabled         ((const uint8_t)qpthreads_enabled_flag)
#define qpthreads_enabled_freeze  qpt_freeze_qpthreads_enabled()

#define qpthreads_thread_created  ((const uint8_t) \
                                                  qpthreads_thread_created_flag)

/*==============================================================================
 * Data types
 */

typedef pthread_t        qpt_thread_t ;
typedef pthread_mutex_t  qpt_mutex_t ;
typedef pthread_cond_t   qpt_cond_t ;

typedef pthread_attr_t   qpt_thread_attr_t ;

typedef qpt_mutex_t*     qpt_mutex ;
typedef qpt_cond_t*      qpt_cond ;

/*==============================================================================
 * Thread Creation -- see qpthreads.c for further discussion.
 *
 * NB: it is a FATAL error to attempt these if !qpthreads_enabled.
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

extern qpt_thread_attr_t*       /* FATAL error if !qpthreads_enabled    */
qpt_thread_attr_init(qpt_thread_attr_t* attr, enum qpt_attr_options opts,
                                          int scope, int policy, int priority) ;
extern qpt_thread_t             /* FATAL error if !qpthreads_enabled    */
qpt_thread_create(void* (*start)(void*), void* arg, qpt_thread_attr_t* attr) ;

extern void*                    /* do nothing if !qpthreads_enabled     */
qpt_thread_join(qpt_thread_t thread_id) ;

/*==============================================================================
 * qpthreads_enabled support -- NOT FOR PUBLIC CONSUMPTION !
 */
Private uint8_t qpthreads_enabled_flag ;        /* DO NOT TOUCH THIS PLEASE  */
Private uint8_t qpthreads_thread_created_flag ; /* DO NOT TOUCH THIS PLEASE  */

Private int
qpt_set_qpthreads_enabled(int how) ;    /* qpthreads_enabled := how          */

Private int
qpt_freeze_qpthreads_enabled(void) ;    /* get and freeze qpthreads_enabled  */

/*==============================================================================
 * Thread self knowledge -- even when !qpthreads_enabled there is one thread
 */
Inline qpt_thread_t qpt_thread_self(void)
{
  return pthread_self() ;
} ;

/*------------------------------------------------------------------------------
 * Thread equality -- returns true iff threads are *equal*
 *                 -- even when !qpthreads_enabled there is one thread
 */
Inline bool qpt_threads_equal(qpt_thread_t a_id, qpt_thread_t b_id)
{
  return pthread_equal(a_id, b_id) != 0 ;
} ;

/*------------------------------------------------------------------------------
 * Thread identity -- returns true iff current thread is the given thread
 *                 -- even when !qpthreads_enabled there is one thread
 */
Inline bool qpt_thread_is_self(qpt_thread_t id)
{
  pthread_t self = pthread_self() ;
  return pthread_equal(self, id) != 0 ;
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
 * NB: if NOT qpthreads_enabled, all mutex actions are EMPTY.  This allows
 *     code to be made thread-safe for when pthreads is running, but to work
 *     perfectly well without pthreads.
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

extern qpt_mutex                        /* freezes qpthreads_enabled    */
qpt_mutex_init_new(qpt_mutex mx, enum qpt_mutex_options opts) ;

#define qpt_mutex_init qpt_mutex_init_new

extern qpt_mutex                  /* do nothing if !qpthreads_enabled   */
qpt_mutex_destroy(qpt_mutex mx, int free_mutex) ;

#define qpt_mutex_destroy_keep(mx) qpt_mutex_destroy(mx, 0)
#define qpt_mutex_destroy_free(mx) qpt_mutex_destroy(mx, 1)

Inline void
qpt_mutex_lock(qpt_mutex mx) ;    /* do nothing if !qpthreads_enabled   */

Inline int
qpt_mutex_trylock(qpt_mutex mx) ; /* always succeeds if !qpthreads_enabled */

Inline void
qpt_mutex_unlock(qpt_mutex mx) ;  /* do nothing if !qpthreads_enabled   */

/*==============================================================================
 * Condition Variable handling
 *
 * Quagga's clock for condition variables is QPT_COND_CLOCK_ID, which
 * may be set elsewhere.  If it is not set then it is set here to be:
 *
 *   * CLOCK_MONOTONIC if available
 *   * CLOCK_REALTIME otherwise  -- this is the standard default.
 *
 * QPT_COND_CLOCK_MONOTONIC is set if CLOCK_MONOTONIC is used (and must be set
 * if QPT_COND_CLOCK_ID is set elsewhere to something that is monotonic).
 *
 * NB: the time-out time passed to qpt_cond_timedwait() is a qtime_mono_t
 *     time (so based on qtime's monotonic time, which is CLOCK_MONOTONIC if
 *     that is available.
 *
 *     Otherwise, there is a conversion step from qtime_mono_t to whatever the
 *     timebase for the condition variable is.
 *
 * NB: static initialisation of condition variables is not supported, to avoid
 *     confusion between the standard default and Quagga's default.

 * NB: if NOT qpthreads_enabled, all condition actions are EMPTY.  This allows
 *     code to be made thread-safe for when pthreads is running, but to work
 *     perfectly well without pthreads.
 */

#ifndef QPT_COND_CLOCK_ID
# ifdef HAVE_CLOCK_MONOTONIC
#  define QPT_COND_CLOCK_ID  CLOCK_MONOTONIC
#  define QPT_COND_CLOCK_MONOTONIC  1
# else
#  define QPT_COND_CLOCK_ID  CLOCK_REALTIME
#  undef  QPT_COND_CLOCK_MONOTONIC
# endif
#endif

enum qpt_cond_options
{
  qpt_cond_quagga      = 0x0000,  /* Quagga's default   */
} ;

extern qpt_cond                   /* freezes qpthreads_enabled          */
qpt_cond_init_new(qpt_cond cv, enum qpt_cond_options opts) ;

extern qpt_cond                   /* do nothing if !qpthreads_enabled   */
qpt_cond_destroy(qpt_cond cv, int free_cond) ;

#define qpt_cond_destroy_keep(cv) qpt_cond_destroy(cv, 0)
#define qpt_cond_destroy_free(cv) qpt_cond_destroy(cv, 1)

Inline void                       /* do nothing if !qpthreads_enabled   */
qpt_cond_wait(qpt_cond cv, qpt_mutex mx) ;

extern int                        /* returns  !qpthreads_enabled   */
qpt_cond_timedwait(qpt_cond cv, qpt_mutex mx, qtime_mono_t timeout_time) ;

Inline void                       /* do nothing if !qpthreads_enabled   */
qpt_cond_signal(qpt_cond cv) ;

Inline void                       /* do nothing if !qpthreads_enabled   */
qpt_cond_broadcast(qpt_cond cv) ;

/*==============================================================================
 * Mutex inline functions
 */

/* Lock given mutex  -- or do nothing if !qpthreads_enabled.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */

Inline void
qpt_mutex_lock(qpt_mutex mx)
{
  if (qpthreads_enabled)
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

/* Try to lock given mutex  -- every time a winner if !qpthreads_enabled.
 *
 * Returns: lock succeeded (1 => have locked, 0 => unable to lock).
 *
 * Has to check the return value, so zabort_errno if not EBUSY.
 */

Inline int
qpt_mutex_trylock(qpt_mutex mx)
{
  if (qpthreads_enabled)
    {
      int err = pthread_mutex_trylock(mx) ;
      if (err == 0)
        return 1 ;                      /* success: it's locked.        */
      if (err == EBUSY)
        return 0 ;                      /* unable to lock               */

      zabort_err("pthread_mutex_trylock failed", err) ;
    }
  else
    return 1 ;
} ;

/* Unlock given mutex  -- or do nothing if !qpthreads_enabled.
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_mutex_unlock(qpt_mutex mx)
{
  if (qpthreads_enabled)
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

/* Wait for given condition variable  -- do nothing if !qpthreads_enabled
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_cond_wait(qpt_cond cv, qpt_mutex mx)
{
  if (qpthreads_enabled)
    {
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
      pthread_cond_wait(cv, mx) ;
#else
      int err = pthread_cond_wait(cv, mx) ;
      if (err != 0)
        zabort_err("pthread_cond_wait failed", err) ;
#endif
    } ;
} ;

/* Signal given condition   -- do nothing if !qpthreads_enabled
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_cond_signal(qpt_cond cv)
{
  if (qpthreads_enabled)
    {
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
      pthread_cond_signal(cv) ;
#else
      int err = pthread_cond_signal(cv) ;
      if (err != 0)
        zabort_err("pthread_cond_signal failed", err) ;
#endif
    } ;
} ;

/* Broadcast given condition   -- do nothing if !qpthreads_enabled
 *
 * Unless both NCHECK_QPTHREADS and NDEBUG are defined, checks that the
 * return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_cond_broadcast(qpt_cond cv)
{
  if (qpthreads_enabled)
    {
#if defined(NDEBUG) && defined(NDEBUG_QPTHREADS)
      pthread_cond_broadcast(cv) ;
#else
      int err = pthread_cond_broadcast(cv) ;
      if (err != 0)
        zabort_err("pthread_cond_broadcast failed", err) ;
#endif
    } ;
} ;

/*==============================================================================
 * Signal Handling.
 */
void                            /* FATAL error if !qpthreads_enabled  */
qpt_thread_sigmask(int how, const sigset_t* set, sigset_t* oset) ;

void                            /* FATAL error if !qpthreads_enabled  */
qpt_thread_signal(qpt_thread_t thread, int signum) ;

#endif /* _ZEBRA_QPTHREADS_H */

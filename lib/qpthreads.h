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
#if HAVE_PTHREAD_H
#include <pthread.h>
#else
#error "do not HAVE_PTHREAD_H ???"
#endif
#if HAVE_PTHREAD_NP_H           /* For FreeBSD  */
#include <pthread_np.h>
#endif
#include <sched.h>
#include <unistd.h>
#include <errno.h>

#include "zassert.h"
#include "qtime.h"
#include "qstring.h"
#include "list_util.h"

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

/*------------------------------------------------------------------------------
 * Sort out QPTHREADS_DEBUG.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if QPTHREADS_NO_DEBUG is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set QPTHREADS_DEBUG    == 0 to turn off debug
 *       *  or set QPTHREADS_DEBUG    != 0 to turn on debug
 *       *  or set QPTHREADS_NO_DEBUG != 0 to force debug off
 */

#ifdef QPTHREADS_DEBUG          /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(QPTHREADS_DEBUG)
#  undef  QPTHREADS_DEBUG
#  define QPTHREADS_DEBUG 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define QPTHREADS_DEBUG QDEBUG
#endif

#ifdef QPTHREADS_NO_DEBUG       /* Override, if defined                 */
# if IS_NOT_ZERO_OPTION(QPTHREADS_NO_DEBUG)
#  undef  QPTHREADS_DEBUG
#  define QPTHREADS_DEBUG 0
# endif
#endif

enum { qpthreads_debug = QPTHREADS_DEBUG } ;

/*==============================================================================
 * Global Switch -- this allows the library to be run WITHOUT pthreads !
 *
 * Nearly every qpthreads function is a NOP if either !qpthreads_enabled or
 * !qpthreads_active.
 *
 * Early in the morning a decision may be made to enable qpthreads -- that must
 * be done before any threads are created (or will zabort) and before any
 * mutexes and condition variables are initialised (or it will be too late).
 *
 * During shut down, if all but one pthread has been brought to a halt, then
 * it is safe to do many things which during normal running require locks etc.
 * to be obtained.  The qpthreads_active state is for those things which no
 * longer apply once all but one pthread has been stopped.
 *
 * Use: qpthreads_enabled   -- to test for the enabled-ness
 *      qpthreads_active    -- main pthread started and pthreads not (yet)
 *                             shut down
 *      qpthreads_freeze()  -- to test and freeze unset if not yet enabled
 */

#define qpthreads_enabled  ((const bool)qpthreads_enabled_flag)
#define qpthreads_active   ((const bool)qpthreads_active_flag)

#define qpthreads_main_started  ((const bool)qpthreads_main_started_flag)

extern bool qpthreads_freeze(void) ;
extern bool qpthreads_decided(void) ;

/*==============================================================================
 * Data types
 */

/*------------------------------------------------------------------------------
 * The qpt_xxx versions of the basic pthread_xxx structures
 */
typedef struct qpt_thread*  qpt_thread ;

typedef struct qpt_mutex*   qpt_mutex ;

typedef pthread_cond_t      qpt_cond_t[1] ;
typedef pthread_cond_t*     qpt_cond ;

typedef pthread_spinlock_t  qpt_spin_t[1] ;
typedef pthread_spinlock_t* qpt_spin ;

/*------------------------------------------------------------------------------
 * Arguments for the initialisation of a qpt_thread object.
 */
enum qpt_init
{
  qpt_init_main,

  qpt_init_system_scope,
  qpt_init_process_scope,
} ;

typedef enum qpt_init qpt_init_t ;

/*------------------------------------------------------------------------------
 * The struct qpt_thread_attr captures the attributes of the underlying
 * pthread.
 *
 * Some attributes are set when the pthread is created, and some may be set
 * both then and changed thereafter.  For the main (or only) pthread the
 * attributes which cannot be set after pthread creation are (implicitly)
 * unchangeable.
 *
 * The attributes we know about are:
 *
 *   * scope                   -- process/system at creation time *only*.
 *
 *     aka contention scope.  Two are specified: process and system scopes.
 *
 *     POSIX is vague in this area.  It specifies that if a process contains
 *     both process and system scope pthreads, the interaction between them is
 *     unspecified.  POSIX does not specify what the scope of the main pthread
 *     is (AFAICS) -- though if all other pthreads have the same scope the main
 *     pthread is assumed to follow suit.  A system may support one or both of
 *     process and system.
 *
 *     The system default is implementation defined.
 *
 *     FreeBSD and Linux, for example, only support system scope, which
 *     simplifies things.
 *
 *     Generally one would prefer system scope, and that is the default here.
 *
 *   * scheduling policy and parameters   -- settable at any time
 *
 *     POSIX is wonderfully vague in this area, and most things are
 *     implementation defined.
 *
 *     There are four known policies:
 *
 *       SCHED_FIFO
 *       SCHED_RR       -- these are preemptive and the same as each other,
 *                         except that in SCHED_RR a running pthread may be
 *                         preempted by another at the same priority after some
 *                         time period.
 *
 *                         The only known parameter is a priority.
 *
 *       SCHED_OTHER    -- this is non-premptive, but otherwise may or may not
 *                         be the same as one of the above.  According to
 *                         POSIX, it is provided so that applications can
 *                         "indicate that they no longer need a realtime
 *                         scheduling policy".
 *
 *                         SCHED_OTHER may have a priority value, though its
 *                         meaning os implementation defined.  It may have
 *                         other, implementation defined, parameters.
 *
 *       SCHED_SPORADIC -- this is an optional policy, not further considered
 *                         here.  However, it uses three further fields in the
 *                         scheduling params -- which confirms the need to
 *                         handle those as "partly opaque".
 *
 *     The POSIX "struct sched_param" contains only one known entry -- the
 *     priority -- except for SCHED_SPORADIC, for which three other entries are
 *     specified.  The min and max allowed priorities are given by
 *     sched_get_priority_min() and sched_get_priority_max() for the policy
 *     in question.
 *
 *     This whole area is implementation and system specific, so here we use
 *     the defaults as given.  If at some point some configuration options
 *     are implemented to override the default, then extra facilities will
 *     be required to apply those.
 *
 *   * inherit scheduling properties
 *
 *     The default for this is implementation defined.  Linux man page for
 *     pthread_attr_setschedinherit() says that PTHREAD_INHERIT_SCHED is the
 *     default.
 *
 *     The properties inherited are (by POSIX): scope, policy and params.
 *
 *     The effect on scope, policy and params of clearing the inherit option
 *     (ie setting PTHREAD_EXPLICIT_SCHED) is not defined.
 *
 *     If the inherit option is set by default, what are the scope, policy
 *     and params set to ?  Given that a single set of attributes may be used
 *     many times, presumably the attributes are *not* the attributes that will
 *     be inherited (since many threads could use the attribute set).  So,
 *     are they undefined or are they the default attributes (so that clearing
 *     the inherit option naturally takes them) ?  Or does clearing the inherit
 *     option have a side effect which sets the policy and params ?  Or does
 *     clearing the inherit option require the policy and params to be set
 *     explicitly ?  If the last, how do we establish what policy and
 *     params (particularly for SCHED_OTHER) should be adopted ?
 *
 *     Given that we feel that PTHREAD_SCOPE_SYSTEM is the appropriate scope,
 *     how do we ensure that is what is asked for, given that we have no idea
 *     how to set the policy and params ?
 *
 *     Finally, is it possible that *different* policy and params are suitable
 *     for the two contention scopes ?  Would PTHREAD_SCOPE_PROCESS be running
 *     SCHED_RR or SCHED_FIFO ?  Or some policy quite different ?
 *
 *     Would the scheduling policy and params of the main thread be a suitable
 *     default for PTHREAD_SCOPE_SYSTEM ?
 *
 *     What is more, it may or may not be necessary to have special privileges
 *     in order to change the scheduling properties.
 *
 *     Who can tell ?
 *
 *     See qpt_thread_init() for the chosen approach to inheritance and
 *     default scope/policy/param.
 *
 *   * guardsize, stackaddr & stacksize
 *
 *     These can only be set at pthread creation time.
 *
 *     We use the defaults.  No support is currently provided for changing
 *     any of these.
 *
 *   * cancel state & type   -- settable once the pthread starts running.
 *
 *     The default cancel state and type are PTHREAD_CANCEL_ENABLE and
 *     PTHREAD_CANCEL_DEFERRED.
 *
 *     At present there is no qpt_thread_cancel(), so the cancel state and
 *     type are moot.
 */

/* Scheduling attributes -- those which may be changed once a pthread is
 * running (or at any time, for the main thread).
 */
struct qpt_sched
{
  int     policy ;              /* SCHED_FIFO/_RR/_OTHER etc.           */

  struct sched_param param[1] ; /* At least "sched_priority"            */

  int     min_priority ;
  int     max_priority ;
} ;

typedef struct qpt_sched* qpt_sched ;
typedef struct qpt_sched  qpt_sched_t[1] ;

/* The attributes of a qpt_thread.
 */
struct qpt_attr
{
  /* If it is possible to get the actual pthread attributes, then when those
   * are read this flag is set.
   */
  bool      actual ;

  /* Once a thread has been detached, it can never by joined.
   *
   * The POSIX default for all threads is joinable.
   *
   * If !qpthreads_enabled, then the main (and only) thread is marked as
   * detached.
   */
  bool      detached ;

  /* cancel_disabled and cancel_async are initialised false -- as per the
   * POSIX default.
   *
   * Changed by qpt_set_cancel_state() and qpt_set_cancel_type() -- TBA.
   *
   * If !qpthreads_enabled, then these will stay false.
   *
   * These are not members of the pthread_attr_t, but are here so that the
   * complete state of a running thread is all in one place.
   */
  bool      cancel_disabled ;   /* default is false: state = enabled    */
  bool      cancel_async ;      /* default is false: type  = deferred   */

  /* Scheduling attributes.
   *
   * The scheduling contention scope is set when the qpt_thread is initialised,
   * and when actual pthread attributes are read.
   *
   * The inheritance of scheduling attributes is not supported by the qpt code,
   * but we capture the state here when actual pthread attributes are read.
   *
   * The qpt_sched_t values (policy/param and min_/max_priority) are settable
   * at any time and are settable if !qpthreads_enabled for the main and only
   * thread.  Once the main thread is started or a pthread is created, these
   * will reflect the actual scheduling parameters.
   */
  int       sched_scope ;       /* PTHREAD_SCOPE_PROCESS/_SYSTEM        */

  bool      sched_inherited ;
  bool      sched_main_start ;  /* do qpt_sched_set() at main start     */

  qpt_sched_t sched ;           /* policy/param and min_/max_priority   */

  /* For all threads these are set to the defaults when the qpt_thread is
   * initialised -- and the stack_address is set NULL.  If the actual values
   * are available, those are read when the pthread is created.
   *
   * If !qpthread_enabled these are all zero.
   *
   * It is not specified that the main pthread will have the default guard and
   * stack size -- but its hard to see why it would not.
   */
  size_t    guard_size ;
  size_t    stack_size ;
  void*     stack_addr ;
} ;

typedef struct qpt_attr  qpt_attr_t[1] ;
typedef struct qpt_attr* qpt_attr ;

/*------------------------------------------------------------------------------
 * The struct qpt_thread allows us to keep track of which pthreads have
 * started and have been joined.  It is used by the "watch-dog" to keep track
 * of the state and cpu time utilisation, per thread.
 *
 * So, we have a qpt_thread type, which is a pointer to the information we
 * keep about each qpt_thread we have not yet discarded.
 */
enum qpt_thread_state
{
  qpts_initial  = 0,    /* not on known list    */

  qpts_running,
  qpts_ended,
  qpts_joined,

  qpts_final,           /* not on known list    */
} ;

typedef enum qpt_thread_state qpt_thread_state_t ;

/* The qpt_thread stats capture when it started, when it ended, when the
 * cpu clock (if any) was last read and what its value was.
 */
typedef struct qpt_thread_stats* qpt_thread_stats ;

struct qpt_thread_stats
{
  qtime_mono_t  start ;

  qtime_mono_t  cpu_when ;      /* when last reading was taken  */
  qtime_t       cpu_used ;      /* what the last reading was    */

  qtime_mono_t  end ;
} ;

typedef struct qpt_thread_stats qpt_thread_stats_t[1] ;

/* The qpt_thread_type is a structure which allows the qpt_thread_destroy
 * level to free the "data" and/or "ret" values when destroying a qpt_thread
 * object.
 *
 * Users of the qpt_thread stuff may use this to distinguish different types
 * of pthreads.
 *
 * The "destroy" function is passed the relevant entry from the qpt_thread,
 * which (NB) may be NULL -- so the "destroy" function(s) also act as more
 * general destroy hooks.
 *
 * A NULL type is ignored, and NULL destroy functions are also ignored.
 *
 * NB: the destroy functions are most likely going to be called in some
 *     pthread *other* than the pthread itself.
 *
 *     If a qth_thread is destroyed at qpt_finish() time -- for example a
 *     detached pthread that does not destroy itself -- then the destroy
 *     functions will be called in tha last remaining pthread, with
 *     !qpthreads_active.
 */
typedef void (*qpt_destroy_func)(void* val) ;

typedef const struct qpt_thread_type* qpt_thread_type ;

struct qpt_thread_type
{
  const char*   name ;                  /* Name of the type             */

  qpt_destroy_func  data_destroy ;      /* to destroy qpth->data        */
  qpt_destroy_func  ret_destroy ;       /* to destroy qpth->ret         */
} ;

typedef const struct qpt_thread_type qpt_thread_type_ct ;
typedef struct qpt_thread_type qpt_thread_type_t ;

/* For each thread a qpt_thread structure is created.  This includes the
 * main thread, whether or not is running qpthreads_enabled.  The following
 * affect the progress of a thread and its qpt_thread structure, and will
 * work when !qpthreads_enabled, unless specified otherwise:
 *
 *   qpt_thread_init()       -- allocates and initialises the structure
 *
 *                              sets qpts_initial.
 *
 *                              must be done for the main (and possibly only)
 *                              thread, before any other threads.  May be done
 *                              any time *after* qpthreads_enabled has been
 *                              decided.
 *
 *                              NB: freezes qpthreads_enabled state.
 *
 *   qpt_main_thread_start() -- does what qpt_thread_create() and
 *                              qpt_thread_start() do, but for the main (and
 *                              possibly only) thread.
 *
 *                              this is when the main thread has reached the
 *                              point that it is deemed to be "started", which
 *                              must be:
 *
 *                                - after any daemonisation.
 *
 *                                - after qpt_thread_init(), so implicitly
 *                                  after qpthreads_enabled is decided
 *
 *                                - before any other threads are created.
 *
 *                              NB: sets qpthreads_active = qpthreads_enabled
 *
 *                                  So from this moment on mutex etc. operations
 *                                  are real actions (if qpthreads_enabled).
 *
 *   qpt_thread_create()     -- creates and sets new pthread running
 *
 *                              fills in pth_id and cpu_clock_id, and sets
 *                              the starting stats
 *
 *                              adds qpt_thread to the known threads
 *
 *                              sets qpts_running
 *
 *                              logs the start of the qpt_thread.
 *
 *                              must NOT be used if !qpthreads_enabled (!).
 *
 *   qpt_thread_start()      -- this must be the first thing that a new
 *                              pthread does.
 *
 *                              sets the thread specific qpt_self pointer to
 *                              point at the qpt_thread.
 *
 *                              does nothing at all for the main thread (and
 *                              hence nothing at all if !qpthreads_enabled).
 *
 *   qpt_thread_end()        -- fills in final stats and sets the ret value
 *
 *                              sets qpts_ended
 *
 *   qpt_thread_join()       -- sets qpts_joined (if not already joined, or
 *                              is detached).
 *
 *   qpt_thread_destroy()    -- if there is only one reference to the
 *                              qpt_thread, it is destroyed.
 *
 * Note that while access to the qpt_thread can be protected by the qpt_mutex,
 * some other mechanism is required to deal with dangling references to the
 * qpt_thread when it is freed.
 *
 * Note that all qpt_thread objects are dynamically allocated.  So we do not
 * define qpt_thread_t, so there is no temptation to allocate a static or
 * embedded qpt_thread !
 */
struct qpt_thread
{
  /* The state is set/changed under the list mutex.
   */
  qpt_thread_state_t state ;

  /* The main thread state is set very early for the main pthread, and not
   * for any other, and never changes.
   */
  bool          main ;

  /* The name is set when the structure is created, and not changed thereafter.
   */
  char          name[30] ;

  /* The structure is not destroyed unless the reference count is zero, or
   * falls to zero when the destroy flag is set.
   *
   * In qpt_thread_set_ref() increments the refcount.
   * In qpt_thread_clear_ref() decrements the refcount, and if a destroy action
   * is pending, the structure destroyed if the count reaches zero.
   */
  bool          destroy ;
  uint          refcount ;

  /* In qpt_thread_create() and qpt_main_thread_start(), the structure is
   * placed on the list of known qpt_thread.  When it is destroyed, it is
   * removed from that list.
   */
  struct dl_list_pair(qpt_thread) list ;

  /* The attributes the pthread has.
   */
  qpt_attr_t    attr ;

  pthread_attr_t* pth_attr ;

  /* The pthread and cpu_clock are set in qpt_thread_create() and
   * qpt_main_thread_start(), under the mutex.
   *
   * It is not known whether the cpu_clock can be accessed once the thread has
   * exited or been joined.  To be safe, we read the clock only if is
   * qpts_running (and when the qpt_thread starts and ends).
   *
   * If !qpthread_enabled the pth_id is invalid, but the cpu_clock_id is
   * set to CLOCK_PROCESS_CPUTIME_ID (if available).
   *
   * If required cpu clock is not available, cpu_clock_id is set to
   * CLOCK_REALTIME -- for want of anything better.
   */
  pthread_t     pth_id ;

  clockid_t     cpu_clock_id ;

  /* We implement qpt_thread_join() without using pthread_join()...
   *
   * ...which finesses the problem of two threads trying to join another at
   * the same time, and allows for the main pthread to join itself at shut-down.
   *
   * ...also, for detached pthreads we want to be able to "collect" any which
   * are still running at shut-down.
   *
   * When qpthreads_enabled we need to be able to wait until the pthread in
   * question has ended.
   */
  qpt_cond_t    end_cond ;

  bool          enjoined ;      /* true <=> 1 or more joiners waiting   */

  /* The "type" allows qpt_thread_destroy() to destroy any "data" or "ret"
   * which has not already been dealt with.
   *
   * The type may also be used to distinguish different uses of qpt_threads.
   * (So, all qpt_thread are held on the one list, but a walker of that list
   * can distinguish different types of same.)
   *
   * NB: this is set in qpt_thread_create() and qpt_main_thread_start(), and
   *     may not be changed thereafter -- nor may the contents be changed.
   */
  qpt_thread_type type ;

  /* The data pointer is set by qpt_thread_create() and not changed thereafter.
   * So, the pointer may be read and used while the qpt_thread object exists.
   *
   * The qpt_thread is passed to the new pthread as the pthread_create() "arg"
   * -- so the pthread knows its qpt_thread "id" and can access its data
   * through here.
   *
   * The data belongs to the pthread -- and may be used for thread specific
   * data.  If there are contents of the data which are shared, then those must
   * be protected by their own locking.  The pthread may free this (and set
   * to NULL) before ending, or may leave that to the pthread which "owns" the
   * qpt_thread after qpt_thread_join().
   *
   * If the qpt_thread is detached, the pthread is responsible for this and
   * MUST free it if required.
   */
  void*         data ;

  /* The ret pointer is set by qpt_thread_end() and not changed thereafter.
   *
   * Once the qpt_thread is joined, the "owner" is responsible for this.
   *
   * If the qpt_thread is detached, the pthread is responsible for this and
   * MUST free it if required.
   */
  void*         ret ;

  /* The stats are set and read under the qpt mutex.
   */
  qpt_thread_stats_t stats ;
} ;

/*------------------------------------------------------------------------------
 * The struct qpt_mutex allows "watch-dog" and other debug stuff to keep track
 * of all mutexes in the system.
 *
 * Note that all qpt_mutex objects are dynamically allocated.  So we do not
 * define qpt_mutex_t, so there is no temptation to allocate a static or
 * embedded qpt_mutex !
 */
struct qpt_mutex
{
  pthread_mutex_t pm[1] ;

  struct dl_list_pair(qpt_mutex) list ;

  char  name[30] ;

  bool  destroy ;
  uint  held ;
} ;

/*==============================================================================
 * Start up, qpthreads_enabled setting etc
 */
Private bool qpthreads_enabled_flag ;        /* DO NOT TOUCH THIS PLEASE  */
Private bool qpthreads_active_flag ;         /* DO NOT TOUCH THIS PLEASE  */
Private bool qpthreads_main_started_flag ;   /* DO NOT TOUCH THIS PLEASE  */

extern void qpt_start_up(int cputime, int thread_cputime) ;
extern const char* qpt_set_new_thread_options(const char* opts) ;
extern void qpt_second_stage(bool want_enabled) ;
extern void qpt_finish(void) ;

/*==============================================================================
 * Thread Creation etc -- see qpthreads.c for further discussion.
 */
extern qpt_thread qpt_thread_init(qpt_init_t init, const char* name) ;
extern bool qpt_thread_detach(qpt_thread qpth) ;
extern qpt_thread_state_t qpt_thread_get_attr(qpt_thread qpth, qpt_attr attr) ;
extern bool qpt_sched_set_policy(qpt_sched sched, int policy) ;
extern qpt_sched qpt_thread_get_sched(qpt_thread qpth, qpt_sched sched) ;
extern bool qpt_thread_set_sched(qpt_thread qpth, qpt_sched sched) ;
extern void qpt_main_thread_start(qpt_thread qpth,
                                             qpt_thread_type type, void* data) ;
extern void qpt_thread_create(qpt_thread qpth,
                                             qpt_thread_type type, void* data,
                                                   void* (*start)(qpt_thread)) ;
extern void* qpt_thread_start(qpt_thread qpth) ;
extern void* qpt_thread_end(void* ret) ;
extern qpt_thread_state_t qpt_thread_join(qpt_thread qpth) ;
extern qpt_thread qpt_thread_set_ref(qpt_thread qpth) ;
extern qpt_thread qpt_thread_clear_ref(qpt_thread qpth) ;
extern qpt_thread qpt_thread_destroy(qpt_thread qpth) ;
extern void qpt_thread_collect(void) ;

extern qpt_thread qpt_thread_walk(qpt_thread qpth) ;
extern qpt_thread_state_t qpt_thread_get_stats(qpt_thread qpth,
                                                       qpt_thread_stats stats) ;

/*==============================================================================
 * Signal Handling.
 *
 * If !qpthreads_enabled these map to process level equivalents.
 */
extern void qpt_thread_sigmask(int how, const sigset_t* set, sigset_t* oset) ;
extern void qpt_thread_raise(qpt_thread qpth, int signum) ;

/*==============================================================================
 * Mutex handling.
 *
 * Quagga's default mutex type is:
 *
 *   * PTHREAD_MUTEX_ERRORCHECK if QPTHREADS_DEBUG
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
 * NB: if NOT qpthreads_active, all mutex actions are EMPTY.  This allows
 *     code to be made thread-safe for when pthreads is running, but to work
 *     perfectly well without pthreads and once pthreads have been brought to
 *     a close.
 *
 * NB: do not (currently) support pthread_mutex_timedlock().
 */

enum qpt_mutex_options
{
  qpt_mutex_quagga      = 0,            /* Quagga's default     */
  qpt_mutex_normal,
  qpt_mutex_recursive,
  qpt_mutex_errorcheck,
  qpt_mutex_default,                    /* system default       */
} ;

typedef enum qpt_mutex_options qpt_mutex_options_t ;

#ifndef QPT_MUTEX_TYPE_DEFAULT
# define QPT_MUTEX_TYPE_DEFAULT  PTHREAD_MUTEX_NORMAL
#endif

enum
{
#if QPTHREADS_DEBUG
  QPT_MUTEX_TYPE  = PTHREAD_MUTEX_ERRORCHECK
#else
  QPT_MUTEX_TYPE  = QPT_MUTEX_TYPE_DEFAULT
#endif
} ;

extern qpt_mutex qpt_mutex_new(qpt_mutex_options_t opts, const char* name) ;
extern qpt_mutex qpt_mutex_destroy(qpt_mutex mx) ;
extern qpt_mutex qpt_mutex_step_next(qpt_mutex mx) ;
extern void qpt_mutex_step_last(qpt_mutex mx) ;
Inline void qpt_mutex_lock(qpt_mutex mx) ;
Inline bool qpt_mutex_trylock(qpt_mutex mx) ;
extern bool qpt_mutex_timedlock(qpt_mutex mx, qtime_t timeout) ;
Inline void qpt_mutex_unlock(qpt_mutex mx) ;

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

 * NB: if NOT qpthreads_active, all condition actions are EMPTY.  This allows
 *     code to be made thread-safe for when pthreads is running, but to work
 *     perfectly well without pthreads and once pthreads have been brought to
 *     a close.
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

extern qpt_cond qpt_cond_init_new(qpt_cond cv, enum qpt_cond_options opts) ;
extern qpt_cond qpt_cond_destroy(qpt_cond cv, free_keep_b free_cond) ;
Inline void qpt_cond_wait(qpt_cond cv, qpt_mutex mx) ;
extern bool qpt_cond_timedwait(qpt_cond cv, qpt_mutex mx,
                                                    qtime_mono_t timeout_time) ;
Inline void qpt_cond_signal(qpt_cond cv) ;
Inline void qpt_cond_broadcast(qpt_cond cv) ;

/*==============================================================================
 * Spinlock handling
 *
 * Spinlocks are pretty trivial -- requiring only to be initialised, locked,
 * unlocked and, finally, destroyed.
 *
 * NB: recursive spinlocks are not supported !
 *
 * NB: if NOT qpthreads_active, locking and unlocking always succeed.  This
 *     allows code to be made thread-safe for when pthreads is running, but to
 *     work perfectly well without pthreads.
 */
extern void qpt_spin_init(qpt_spin slk) ;
extern void qpt_spin_destroy(qpt_spin slk) ;
Inline void qpt_spin_lock(qpt_spin slk) ;
Inline void qpt_spin_unlock(qpt_spin slk) ;

/*==============================================================================
 * Mutex inline functions
 */

Private void qpt_mutex_lock_failed(qpt_mutex mx, int err) ;
Private void qpt_mutex_trylock_failed(qpt_mutex mx, int err) ;
Private void qpt_mutex_unlock_failed(qpt_mutex mx, int err) ;

/*------------------------------------------------------------------------------
 * Lock given mutex  -- or do nothing if !qpthreads_active.
 *
 * Checks that the return value is valid -- zabort_errno if it isn't.
 */

Inline void
qpt_mutex_lock(qpt_mutex mx)
{
  if (qpthreads_active)
    {
      int err = pthread_mutex_lock(mx->pm) ;
      if (err != 0)
        qpt_mutex_lock_failed(mx, err) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Try to lock given mutex  -- every time a winner if !qpthreads_active.
 *
 * Returns: lock succeeded (false <=> unable to lock).
 *
 * Checks that the return value is valid -- zabort_errno if it isn't.
 */
Inline bool
qpt_mutex_trylock(qpt_mutex mx)
{
  if (qpthreads_active)
    {
      int err = pthread_mutex_trylock(mx->pm) ;
      if (err != 0)
        {
          if (err != EBUSY)
            qpt_mutex_trylock_failed(mx, err) ;

          return false ;        /* unable to lock               */
        } ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Unlock given mutex  -- or do nothing if !qpthreads_active.
 *
 * Checks that the return value is valid -- zabort_err if it isn't.
 */
Inline void
qpt_mutex_unlock(qpt_mutex mx)
{
  if (qpthreads_active)
    {
      int err = pthread_mutex_unlock(mx->pm) ;
      if (err != 0)
        qpt_mutex_unlock_failed(mx, err) ;
    } ;
} ;

/*==============================================================================
 * Condition variable inline functions
 */

/*------------------------------------------------------------------------------
 * Wait for given condition variable  -- do nothing if !qpthreads_active
 *
 * Checks that the return value is valid -- zabort_err if it isn't.
 */
Inline void
qpt_cond_wait(qpt_cond cv, qpt_mutex mx)
{
  if (qpthreads_active)
    {
      int err = pthread_cond_wait(cv, mx->pm) ;
      if (err != 0)
        zabort_err("pthread_cond_wait failed", err) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Signal given condition   -- do nothing if !qpthreads_active
 *
 * Checks that the return value is valid -- zabort_err if it isn't.
 */
Inline void
qpt_cond_signal(qpt_cond cv)
{
  if (qpthreads_active)
    {
      int err = pthread_cond_signal(cv) ;
      if (err != 0)
        zabort_err("pthread_cond_signal failed", err) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Broadcast given condition   -- do nothing if !qpthreads_active
 *
 * Checks that the return value is valid -- zabort_err if it isn't.
 */
Inline void
qpt_cond_broadcast(qpt_cond cv)
{
  if (qpthreads_active)
    {
      int err = pthread_cond_broadcast(cv) ;
      if (err != 0)
        zabort_err("pthread_cond_broadcast failed", err) ;
    } ;
} ;

/*==============================================================================
 * Spinlock inline functions
 */

/*------------------------------------------------------------------------------
 * Lock spinlock  -- do nothing if !qpthreads_active
 *
 * Checks that the return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_spin_lock(qpt_spin slk)
{
  if (qpthreads_active)
    {
      int err = pthread_spin_lock(slk) ;
      if (err != 0)
        zabort_err("pthread_spin_lock failed", err) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Unlock spinlock  -- do nothing if !qpthreads_active
 *
 * Checks that the return value is valid -- zabort_errno if it isn't.
 */
Inline void
qpt_spin_unlock(qpt_spin slk)
{
  if (qpthreads_active)
    {
      int err = pthread_spin_unlock(slk) ;
      if (err != 0)
        zabort_err("pthread_spin_unlock failed", err) ;
    } ;
} ;

/*==============================================================================
 * Thread Specific Data Handling.
 *
 * Note that if !qpthreads_enabled, this maintains the data value, without
 * using any pthread primitives.
 *
 * Note also that this does not support the pthread value destructor -- because
 * cannot support that for non qpthreads_enabled (straightforwardly, anyway).
 */
union qpt_own_data
{
  pthread_key_t key ;           /* if qpthreads_enabled         */

  struct
    {
      void*     value ;
      void      (*destructor)(void*) ;
    } proxy ;                   /* otherwise                    */
} ;

typedef union qpt_own_data  qpt_own_data_t[1] ;
typedef union qpt_own_data* qpt_own_data ;

extern void qpt_own_data_create(qpt_own_data data, void (*destructor)(void*)) ;
extern void qpt_own_data_delete(qpt_own_data data) ;

/*------------------------------------------------------------------------------
 * Set thread specific data value -- value is void*
 */
Inline void
qpt_own_data_set_value(qpt_own_data data, void* value)
{
  if (qpthreads_enabled)
    {
      int err = pthread_setspecific(data->key, value) ;
      if (err != 0)
        zabort_err("pthread_setspecific failed", err) ;
    }
  else
    data->proxy.value = value ;
} ;

/*------------------------------------------------------------------------------
 * Get thread specific data value -- value is void*
 */
Inline void*
qpt_own_data_get_value(qpt_own_data data)
{
  if (qpthreads_enabled)
    return pthread_getspecific(data->key) ;
  else
    return data->proxy.value ;
} ;

/*==============================================================================
 * Thread self knowledge -- even when !qpthreads_enabled there is one
 * qpt_thread -- the main thread.
 *
 * The thread-specific data item qpt_self is set by qpt_thread_start() and
 * by qpt_main_thread_start().
 */

qpt_own_data_t qpt_self ;

/*------------------------------------------------------------------------------
 * Get the current pthread's qpt_thread object.
 *
 * If is !qpthreads_enabled, this returns the qpt_thread for the main thread.
 */
Inline qpt_thread
qpt_thread_self(void)
{
  return qpt_own_data_get_value(qpt_self) ;
} ;

/*------------------------------------------------------------------------------
 * Get the current pthread's qpt_thread "data" value.
 *
 * If is !qpthreads_enabled, this returns the "data" for the main thread.
 */
Inline void*
qpt_thread_self_data(void)
{
  return ((qpt_thread)qpt_own_data_get_value(qpt_self))->data ;
} ;

#endif /* _ZEBRA_QPTHREADS_H */

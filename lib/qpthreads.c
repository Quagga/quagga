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
#include <string.h>

#include "qlib_init.h"
#include "qpthreads.h"
#include "memory.h"
#include "log.h"
#include "qstring.h"
#include "qtime.h"

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
 * Continued Working Without Pthreads
 * ==================================
 *
 * A big Global Switch -- qpthreads_enabled -- is used to control whether the
 * system is pthreaded or not.
 *
 * The initial state is qpthreads_enabled == false (0).
 *
 * The function qpt_second_stage() should be called when the
 * application has decided whether to use qpthreads or not.  (But does not have
 * to call this if it is happy to proceed in the default -- disabled -- state.)
 *
 * If qpthreads_enabled is never set, then the system runs without pthreads,
 * and all the mutex and condition variable functions are NOPs.  This allows,
 * for example, mutex operations to be placed where they are needed for
 * thread-safety, without affecting the code when running without pthreads.
 *
 * There are two related flags:
 *
 *   * qpthreads_thread_started
 *
 *     This is set when the main thread (other than the main pthread) is
 *     "started".
 *
 *     The main thread must not be "started" until after any daemonisation,
 *     mostly because the thread cpu clock does not (in at least one case)
 *     survive the fork().
 *
 *     Other pthreads must not be created until after daemonisation, because
 *     they definitely do not survive the fork().  We police that as a side
 *     effect of requiring the main pthread to be "started" before any other
 *     pthreads are created.
 *
 *     A pthread cannot be created unless qpthreads_enabled.
 *
 *   * qpthreads_active
 *
 *     This is set when the main thread (other than the main pthread) is
 *     "started", if is qpthreads_enabled.
 *
 *     This controls the action of mutexes and condition variables etc.  So,
 *     until the main pthread is "started" all mutex etc operations are NOPs.
 *     Which is intended to allow for initialisation in the main thread to
 *     proceed while not all mutexes etc have been set up.
 *
 *     This does mean that all mutexes etc MUST be expected to be UNLOCKED
 *     at the moment that the main pthread is "started".
 *
 *     The qpthreads_active flag may be cleared once all but one pthreads have
 *     stopped.  This is to allow shut down operations to proceed without
 *     requiring mutex locks etc, so that those can be shit down too.
 *
 * There are a very few operations which require qpthreads_enabled, in
 * particular:
 *
 *   * qpt_thread_create
 *
 * A few operations "freeze" the state of qpthreads_enabled.  Any call of these
 * before qpthreads are enabled, causes the state to be frozen, disabled.  This
 * means that any later attempt to enable qpthreads will be refused.  These
 * operations include:
 *
 *   * qpt_main_thread_start
 *   * qpt_mutex_init_new
 *   * qpt_cond_init_new
 *   * qpt_splin_init_new
 *
 * This allows the application to decide as late as possible (but no later)
 * whether to enable pthreads.  If a mutex, condition variable or spin lock has
 * been initialised before the application gets around to enabling qpthreads,
 * that will be trapped when qpthreads is finally enabled.
 *
 * Pthread Requirements
 * ====================
 *
 * This is assuming support for 1003.1-2004 -- XOPEN Issue 6, with [THR], [SPI]
 * and [XSI] options.
 *
 * In 1003.1-2008, XOPEN issue 7, [THR], [SPI] and pthread_mutexattr_settype()
 * have been moved to Base.
 *
 * The [XSI] is required for pthread_mutexattr_settype(), only.
 *
 * If qpt_thread_attr_init() uses:
 *
 *   pthread_attr_getinheritsched()/_setinheritshed()    [TPS]
 *   pthread_attr_getscope()/_setscope()                 [TPS]
 *   pthread_attr_getschedpolicy()/_setschedpolicy()     [TPS]
 *   pthread_attr_getschedparam()/_setschedparam()       [THR]
 *
 * but they are only required if explicit scheduling attributes are being set.
 * (So, could be dropped where not supported.)
 *
 * Amongst the things which are NOT required:
 *
 *   pthread_attr_getguardsize()/_setguardsize()          [XSI]
 *   pthread_attr_getstack()/_setstack()                  [TSA TSS]
 *   pthread_attr_getstackaddr()/_setstackaddr()          [TSA OB]
 *   pthread_attr_getstacksize()/_setstacksize()          [TSA TSS]
 *
 *   pthread_barrier_xxx()                                [BAR]
 *
 *   pthread_condattr_getpshared()/_setpshared()          [TSH]
 *
 *   pthread_mutex_getprioceiling()/_setprioceiling()     [TPP]
 *   pthread_mutex_timedlock()                            [TMO]      pro tem
 *   pthread_mutexattr_getprioceiling()/_setprioceiling() [TPP]
 *   pthread_mutexattr_getprotocol()/_setprotocol()       [TPP TPI]
 *   pthread_mutexattr_getpshared()/_setpshared()         [TSH]
 *
 *   pthread_rwlock_xxx()                                 [THR]      pro tem
 *   pthread_rwlockattr_init()/_destroy()                 [THR]      pro tem
 *   pthread_rwlockattr_getpshared()/_setpshared()        [TSH]
 *
 * [CS] (Clock Select) is assumed if HAVE_CLOCK_MONOTONIC.
 *
 * NB: it is essential that pthread_kill() delivers the signal to the target
 *     thread only -- ie, it must be POSIX compliant.  That rules out the old
 *     (2.4) LinuxThreads.  For Linux, 2.6 (or greater) is required, with
 *     NPTL (these days generally included in glibc).
 *
 * NB: for glibc to give all the required features, either _GNU_SOURCE or
 *     _XOPEN_SOURCE must be set *before* the first #include <features.h>.
 *     _XOPEN_SOURCE=600 is sufficient.
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
 * In general this code provides for use of the default scheduling, but does
 * attempt to use system contention scope, if there is a choice.
 *
 * See qpt_new_thread_options(), qpt_thread_init() and qpt_thread_set_sched().
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

/* We quite definitely need these
 */
CONFIRM(_POSIX_THREADS     > 0) ;
CONFIRM(_POSIX_SPIN_LOCKS  > 0) ;

/* We expect to find _POSIX_THREAD_SAFE_FUNCTIONS -- but FreeBSD 9.0, not.
 *
 * POSIX-2004 says that _POSIX_SPIN_LOCKS => _POSIX_THREAD_SAFE_FUNCTIONS
 * (though in the context of support for "Advanced Realtime Threads", which
 * includes _POSIX_SPIN_LOCKS)
 */
#if (_POSIX_THREAD_SAFE_FUNCTIONS <= 0)
#warning Expect _POSIX_THREAD_SAFE_FUNCTIONS -- goes with _POSIX_SPIN_LOCKS (?)
#endif

/*==============================================================================
 * Options for setting new pthread attributes.
 *
 * POSIX is sufficiently vague that it is possible that some default scheduling
 * attributes cannot be established at run-time, and must be set by run time
 * option or configuration.
 *
 * If pthread_attr_get_np() or pthread_getattr_np() are available, then we have
 * access to the "actual" values of these attributes.  Otherwise, we have
 * access to the "default" values, which is what pthread_attr_init() is
 * supposed to give.
 *
 * Note that if we cannot get the "actual" settings, then the contention scope
 * of the main thread is a complete mystery -- but, luckily, we don't really
 * care.
 *
 * As discussed elsewhere, there are difficulties with the "defaults",
 * including: (a) it is not clear whether those are the actual values -- or
 * some conventional, "use default" values; (b) it is not known whether they
 * remain up to date when attributes are set -- so if SCHED_RR has a different
 * default priority to SCHED_OTHER, does changing from a default of SCHED_OTHER
 * change the priority ?
 *
 * Nevertheless, this code's default action is to accept the defaults provided
 * by the implementation, assuming that the implementation does something
 * sensible, that is:
 *
 *   * after setting PTHREAD_EXPLICIT_SCHED, the scope, policy and params
 *     are, indeed, the defaults.
 *
 *   * then after setting the required (or only available) scope, the policy
 *     and params are, indeed, the defaults for that scope.
 *
 *   * then after setting the required (or default) policy, the params are,
 *     indeed, the defaults for the policy.
 *
 * If any of the above is not true, then can override this code's default
 * action by options, as follows:
 *
 *   * scheduling policy and params:
 *
 *       'D' -- use "default" from pthread_attr_init() after setting
 *              PTHREAD_EXPLICIT_SCHED and the required scope.
 *
 *              This is the default (!).
 *
 *       'M' -- use same as main thread when the system started.
 *
 *              This is the first option if the default action does not work.
 *
 *              This assumes that whatever the main thread contention scope
 *              is, its policy and param are valid.  Mostly we expect the main
 *              thread to be bog-standard SCHED_OTHER, so this seems like a
 *              safe assumption.
 *
 *              If this is not the case, then will need to set a specific
 *              policy and params.
 *
 *       'O' -- use SCHED_OTHER  )
 *       'F' -- use SCHED_FIFO   ) must then specify params as below.
 *       'R' -- use SCHED_RR     )
 *
 *       'N' -- if none of the above work satisfactorily, then this option
 *              turns off all fiddling with the attributes, and creates all
 *              threads with whatever the system provides.
 *
 *              Note that goes through all the motions as if using the defaults,
 *              but at creation time, uses a NULL set of attributes.
 *
 *   * scheduling params, if policy is 'O', 'F' or 'R':
 *
 *       'D' -- use "default" from pthread_attr_init() after setting
 *              PTHREAD_EXPLICIT_SCHED, the required scope and the required
 *              policy.
 *
 *              This depends on the system doing something sensible !
 *
 *       'A' -- zeroise the params and use the average of the min/max priority.
 *
 *       '+/-999' -- zeroise the params and set the priority as given.
 *
 *              Clamp to within the min/max range and log an error if that is
 *              necessary.
 */
enum qpt_use_policy
{
  qpt_use_default_policy,       /* 'D'                  */
  qpt_use_main_policy,          /* 'M'                  */
  qpt_use_given_policy,         /* 'O', 'F' and 'R'     */
} ;

enum qpt_use_params
{
  qpt_use_default_params,       /* 'D'                          */
  qpt_use_main_params,          /* set with qpt_use_main_policy */
  qpt_use_given_params,         /* 'A' or '+/-999'              */
} ;

typedef enum qpt_use_policy qpt_use_policy_t ;
typedef enum qpt_use_params qpt_use_params_t ;

static bool qpt_use_null_attributes    = false ;

static qpt_use_policy_t qpt_use_policy = qpt_use_default_policy ;
static qpt_use_params_t qpt_use_params = qpt_use_default_params ;

static qpt_sched_t qpt_given_sched ;

/* The initial main thread policy and params are captured here when the
 * main qpt_thread is initialised.
 *
 * To ensure this is done only once, and before any other qpt_thread is
 * initialised, we have a flag.
 */
static qpt_sched_t qpt_initial_sched ;

static bool qpt_main_thread_initialised = false ;

/*==============================================================================
 * Keeping track of pthreads.
 *
 * We keep track of all pthreads by allocating a qpt_thread structure for each
 * one, including the main (and possibly only) thread.
 *
 * All known qpt_thread are kept on a list of same.  The entire list and all
 * its contents are protected by a mutex.
 *
 * A big reason for keeping track in this way is for diagnostics.  Access to
 * those require the mutex.  The thread operations above are *rare*, but any
 * diagnostic code should take care not to hang on to the mutex for any longer
 * than is necessary.
 *
 * A pthread can find its *own* qpt_thread by qpt_thread_self(), and the data
 * set in the qpt_thread by qpt_thread_self_data().  Since the thread is
 * running, it is assumed that no locking is required for this !
 */

/* List of known qpt_thread objects.
 */
static struct dl_base_pair(qpt_thread) qpt_thread_list ;

/* The mutex used to control access to the qpt_thread objects and the list of
 * same.
 */
static qpt_mutex qpt_thread_list_mutex  = NULL ;

static volatile bool   qpt_thread_list_locked = false ;

/*------------------------------------------------------------------------------
 * Lock and unlock the qpt_mutex.
 */
static void
LOCK_QPT(void)
{
  qpt_mutex_lock(qpt_thread_list_mutex) ;
  qpt_thread_list_locked = true ;
} ;

static void
UNLOCK_QPT(void)
{
  qpt_thread_list_locked = false ;
  qpt_mutex_unlock(qpt_thread_list_mutex) ;
} ;

inline static void
ASSERT_QPT_LOCKED(void)
{
  qassert(qpt_thread_list_locked) ;
} ;

inline static void
ASSERT_QPT_LOCKED_IF_ACTIVE(qpt_thread qpth)
{
  if ((qpth->state != qpts_initial) && (qpth->state != qpts_final))
    ASSERT_QPT_LOCKED() ;
} ;

/* Base of list of known mutexes, complete with spinlock to control update
 * Base of list of known mutexes, complete with spinlock to control update
 * of same.
 *
 * Zeroised at qpt_start_up().  Spin-Lock initialised when pthread_enabled.
 */
static struct
{
  struct dl_base_pair(qpt_mutex) base ;

  qpt_spin_t  slk ;

} qpt_mutexes ;

/*==============================================================================
 * The Global Switch and other pthread properties
 *
 * The state of the switch is:  unset    -- implicitly not enabled
 *                              frozen   -- implicitly frozen disabled
 *                              set      -- explicitly enabled/disabled
 *
 * "frozen" means that "qpthreads_freeze()" has been called, and the state was
 * unset at the time -- so some initialisation has frozen qpthreads_enabled
 * as disabled.
 */
enum qpthreads_enabled_state
{
  qpt_state_unset   = 0,
  qpt_state_frozen  = 1,
  qpt_state_set     = 2,
} ;
typedef enum qpthreads_enabled_state qpthreads_enabled_state_t ;

static qpthreads_enabled_state_t qpthreads_enabled_state = qpt_state_unset ;

bool qpthreads_enabled_flag          = false ;
bool qpthreads_active_flag           = false ;
bool qpthreads_main_started_flag   = false ;

/* At run-time we check whether thread and/or process cpu clocks are supported.
 *
 * This is done very early in the morning after consulting sysconf()
 */
static bool qpt_have_thread_cpu_clock    = false ;
static bool qpt_have_process_cpu_clock   = false ;

/* When the main thread is initialised, checks which of PTHREAD_SCOPE_PROCESS
 * and/or PTHREAD_SCOPE_SYSTEM are available -- POSIX does not require both.
 *
 * If only one scope is available, then qpt_scope_fixed is set true, and
 * qpt_scope is set to the one available scope.
 *
 * If both are available, sets qpt_scope to the preferred scope, to whit
 * PTHREAD_SCOPE_SYSTEM.
 *
 * If !qpthreads_enabled, sets as if fixed to PTHREAD_SCOPE_SYSTEM.
 */
static bool qpt_scope_fixed ;
static int  qpt_scope ;

/*==============================================================================
 * Start-up, option setting, setting of qpthreads_enabled, shut down etc.
 */

static bool qpt_sched_get_minmax_priority(qpt_sched sched) ;
static void qpt_thread_finish(void) ;

/*------------------------------------------------------------------------------
 * First stage initialisation -- before any pthreads are started
 *
 * Set all flags.
 */
extern void
qpt_start_up(int cputime, int thread_cputime)
{
  ddl_init(qpt_thread_list) ;

  qpthreads_enabled_flag        = false ;
  qpthreads_active_flag         = false ;
  qpthreads_main_started_flag = false ;

  qpt_have_process_cpu_clock    = cputime        > 0 ;
  qpt_have_thread_cpu_clock     = thread_cputime > 0 ;

  qpt_scope_fixed               = true ;
  qpt_scope                     = PTHREAD_SCOPE_SYSTEM ;

  qpt_use_null_attributes       = false ;

  qpt_use_policy                = qpt_use_default_policy ;
  qpt_use_params                = qpt_use_default_params ;
  memset(qpt_given_sched,  0, sizeof(qpt_sched_t)) ;

  memset(&qpt_mutexes, 0, sizeof(qpt_mutexes)) ;

  qpt_main_thread_initialised   = false ;

  memset(qpt_initial_sched, 0, sizeof(qpt_sched_t)) ;
} ;

/*------------------------------------------------------------------------------
 * Set the options which control the setting of new pthread attributes.
 *
 * This is designed to be used as command line options, just in case the
 * default handling of attributes does not work.
 *
 * Takes string.  NULL or empty => use all "unforced" options.
 *
 * Otherwise: a short, cryptic string of options
 *
 *            <policy>[<priority>]
 *
 *            where:  <policy>   is one of 'D', 'M', 'O', 'F', 'R' or 'N'
 *
 *                    <priority> is one of 'D', 'A' or '+/-999'
 *
 *            noting that the priority may be present only if the <policy> is
 *            'O', 'F' or 'R'.
 *
 * The meaning of these options is given above.
 */
extern const char*
qpt_set_new_thread_options(const char* opts)
{
  qassert(qpt_use_null_attributes == false) ;
  qassert(qpt_use_policy          == qpt_use_default_policy) ;
  qassert(qpt_use_params          == qpt_use_default_params) ;

  memset(qpt_given_sched, 0, sizeof(qpt_sched_t)) ;     /* make sure    */

  if ((opts == NULL) || (*opts == '\0'))
    return NULL ;

  /* step past a leading '='
   */
  if (*opts == '=')
    ++opts ;

  /* Deal with <policy>
   */
  switch (*opts)
    {
      case 'D':
      case 'd':
        break ;                 /* explicit default     */

      case 'M':
      case 'm':
        qpt_use_policy = qpt_use_main_policy ;
        qpt_use_params = qpt_use_main_params ;
        break ;

      case 'O':
      case 'o':
        qpt_use_policy = qpt_use_given_policy ;
        qpt_given_sched->policy = SCHED_OTHER ;
        break ;

      case 'F':
      case 'f':
        qpt_use_policy = qpt_use_given_policy ;
        qpt_given_sched->policy = SCHED_FIFO ;
        break ;

      case 'R':
      case 'r':
        qpt_use_policy = qpt_use_given_policy ;
        qpt_given_sched->policy = SCHED_FIFO ;
        break ;

      case 'N':
      case 'n':
        qpt_use_null_attributes = true ;
        break ;

      default:
        return "unknown policy option" ;
    } ;

  ++opts ;

  /* Deal with <priority>
   */
  if (qpt_use_policy == qpt_use_given_policy)
    {
      char* end ;
      int   ret ;
      bool  known ;

      known = qpt_sched_get_minmax_priority(qpt_given_sched) ;

      if (!known)
        return "given policy is unknown (!)" ;

      switch (*opts)
        {
          case '\0':
            break ;             /* use default  */

          case 'D':
          case 'd':
            ++opts ;
            break ;             /* use default  */

          case 'A':
          case 'a':
            qpt_use_params = qpt_use_given_params ;

            /* NB: rest of qpt_given_sched zeroised, above.
             */
            qpt_given_sched->param->sched_priority =
              (qpt_given_sched->max_priority + qpt_given_sched->min_priority)
                                                                           / 2 ;
            ++opts ;
            break ;

            case '+':
            case '-':
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
              qpt_use_params  = qpt_use_given_params ;

              /* NB: rest of qpt_given_sched zeroised, above.
               */
              qpt_given_sched->param->sched_priority =
                   strtol_xr(opts, &ret, &end, qpt_given_sched->min_priority,
                                               qpt_given_sched->max_priority) ;

              if (ret == strtox_range)
                return "priority value is out of range" ;

              if (ret == strtox_invalid)
                return "invalid priority value" ;

              opts = end ;
              break ;

            default:
              return "unknown params option" ;
        } ;
    } ;

  /* Must now be at the end
   */
  if (*opts == '\0')
    return NULL ;
  else
    return "malformed options" ;
} ;

/*------------------------------------------------------------------------------
 * Second Stage Start Up.
 *
 * At this point qpthreads_enabled is decided.
 */
extern void
qpt_second_stage(bool want_enabled)
{
  switch (qpthreads_enabled_state)
    {
      case qpt_state_unset:
        qassert(!qpthreads_enabled_flag) ;

        /* If we are enabling qpthreads, then now is the time to complete
         * some initialisation.
         */
        qpthreads_enabled_state = qpt_state_set ;

        if (want_enabled)
          {
            qpthreads_enabled_flag = true ;

            qpt_spin_init(qpt_mutexes.slk) ;

            qpt_thread_list_mutex = qpt_mutex_new(qpt_mutex_errorcheck,
                                                            "qpt_thread_list") ;
          } ;

        break ;

      case qpt_state_frozen:
        qassert(!qpthreads_enabled_flag) ;

        qpthreads_enabled_state = qpt_state_set ;

        if (want_enabled)
          zabort("qpthreads_enabled has been frozen: cannot now enable") ;

        break ;

      case qpt_state_set:
        if (qpthreads_enabled_flag && !want_enabled)
          zabort("qpthreads_enabled is already enabled: cannot now disable") ;

        if (!qpthreads_enabled_flag && want_enabled)
          zabort("qpthreads_enabled is already disabled: cannot now enable") ;

        break ;

      default:
        zabort("invalid state of qpthreads_enabled_state") ;
        break ;
    } ;

  qpt_own_data_create(qpt_self, NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Get state of qpthreads_enabled, and freeze to disabled if not yet set.
 *
 * Where some initialisation depends on the state of qpthreads_enabled(), this
 * forces disabled if that initialisation precedes the explicit setting of that
 * flag.
 */
extern bool
qpthreads_freeze(void)
{
  if (qpthreads_enabled_state == qpt_state_unset)
    {
      assert(!qpthreads_enabled_flag) ;
      qpthreads_enabled_state = qpt_state_frozen ;
    } ;

  return qpthreads_enabled_flag ;
} ;

/*------------------------------------------------------------------------------
 * Has the state of qpthreads been decided, yet ?
 *
 * Note that for daemons with no interaction with the pthreads stuff, the state
 * is never decided.
 */
extern bool
qpthreads_decided(void)
{
  return qpthreads_enabled_state != qpt_state_unset ;
} ;

/*------------------------------------------------------------------------------
 * Shut down the qpt_xxx stuff.
 *
 * If not already collected all pthreads, do so now.
 *
 * With only one pthread remaining, can clear qpthreads_active flag.
 *
 * Destroy all remaining qpt_thread objects.
 *
 * Destroy now redundant mutex, spinlock etc.
 */
extern void
qpt_finish(void)
{
  /* Finish off all qpt_thread -- clears qpthreads_enabled.
   */
  qpt_thread_finish() ;

  /* Now destroy local mutex and spin locks
   */
  qpt_thread_list_mutex = qpt_mutex_destroy(qpt_thread_list_mutex) ;

  qpt_spin_destroy(qpt_mutexes.slk) ;

  qpt_own_data_delete(qpt_self) ;
} ;

/*------------------------------------------------------------------------------
 * Clear qpthreads_active -- for shut down.
 *
 * Once all but one (probably the main) pthread have been brought to a halt,
 * can turn off qpthreads_active, so that mutex locks and other operations are
 * short circuited, which is useful during final shut down.
 */
static void
qpt_clear_qpthreads_active(void)
{
  qpthreads_active_flag = false ;
} ;

/*==============================================================================
 * Thread initialisation, creation, starting, ending, joining etc.
 *
 */
static void qpt_thread_do_end(qpt_thread qpth) ;
static bool qpt_thread_do_collect(qpt_thread qpth, bool join) ;

static void qpt_thread_list_del(qpt_thread qpth) ;
static void qpt_thread_free(qpt_thread qpth) ;

static void qpt_pthread_attr_init(qpt_thread qpth) ;
static void qpt_pthread_attr_clear_inheritsched(qpt_thread qpth) ;
static bool qpt_pthread_attr_actual(qpt_thread qpth) ;
static void qpt_pthread_attr_copy(qpt_thread qpth) ;
static void qpt_pthread_attr_destroy(qpt_thread qpth) ;

static bool qpt_pthread_attr_getdetachstate(qpt_thread qpth) ;
static bool qpt_pthread_attr_getinheritsched(qpt_thread qpth) ;
static int qpt_pthread_attr_getscope(qpt_thread qpth) ;
static int qpt_pthread_attr_getschedpolicy(qpt_thread qpth) ;
static struct sched_param qpt_pthread_attr_getschedparam(qpt_thread qpth) ;
static size_t qpt_pthread_attr_getguardsize(qpt_thread qpth) ;
static size_t qpt_pthread_attr_getstacksize(qpt_thread qpth) ;
static void* qpt_pthread_attr_getstackaddr(qpt_thread qpth) ;

static void qpt_pthread_attr_setdetachstate(pthread_attr_t* pth_attr) ;
static void qpt_pthread_attr_setscope(pthread_attr_t* pth_attr, int scope) ;
static void qpt_pthread_attr_setschedpolicy(pthread_attr_t* pth_attr,
                                                                   int policy) ;
static void qpt_pthread_attr_set_sched(pthread_attr_t* pth_attr,
                                                            qpt_sched_t sched) ;
static void qpt_sched_set(qpt_thread qpth) ;
static void qpt_pthread_detach(qpt_thread qpth) ;

static void qpt_sched_get_current(qpt_thread qpth) ;
static void qpt_sched_get_process(qpt_sched sched) ;
static void qpt_sched_get_pthread(qpt_sched sched, pthread_t pth_id) ;

static void qpt_thread_done_create(qpt_thread qpth, pthread_t pth_id,
                                             qpt_thread_type type, void* data) ;
static void qpt_thread_log_options(void) ;
static qstring qpt_thread_log_start(qpt_thread qpth) ;

static void qpt_get_cpu_clock_id(qpt_thread qpth) ;
static void qpt_get_cpu_time(qpt_thread qpth) ;

/*------------------------------------------------------------------------------
 * Allocate and initialise a new qpt_thread object.
 *
 * All qpt_threads -- including the main thread -- have a qpt_thread object.
 * (Even if !qpthreads_enabled.)
 *
 * For the main thread this may be done at a convenient moment after the state
 * of qpthreads_enabled has been decided.
 *
 * For other threads this may be done at a convenient moment before the thread
 * is created, but *after* qpthreads_enabled has been set.
 *
 * Note that initialising a qpth_thread object does not place it on the list of
 * known threads -- that happens when the thread is created (or when the
 * main thread is "started") !
 *
 * The init is separate from create so that:
 *
 *    (a) the main thread handled as much as possible in the same way as other
 *        threads
 *
 *    (b) attributes can be set in the qpt_thread before the thread is created.
 *
 *        This is imperative for scope (and stack size), and may be convenient
 *        for other attributes (scheduling policy and params and detached
 *        state -- though those can be set once the thread has started).
 *
 * Once a qpt_thread is initialised, it contains the default attributes, and
 * a set of pthtread_attr_t.  For the main thread this is limited:
 *
 *   * the contention scope is not known
 *
 *   * the guard size is not known, but is presumably the default
 *
 *   * the stack size is not known, but is presumably the default
 *
 *   * the stack address is not known
 *
 * but the scheduling policy and param are set from the *process* policy and
 * param.  When the main thread is started will fetch the actual values if
 * possible.  (If !qpthreads_enabled, the the only things we will ever know
 * are the scheduling policy and param, as set here).
 *
 * POSIX and the inheritance of scheduling scope, policy and params is ill-
 * defined, so the qpt_thread primitives do not support PTHREAD_INHERIT_SCHED,
 * and during the initialisation here PTHREAD_EXPLICIT_SCHED is set.
 *
 * For any thread other than the main thread, between qpt_thread_init() and
 * qpt_thread_create(), it is possible to:
 *
 *   qpt_thread_get_attr()    -- fetch copy of current attributes
 *
 *   qpt_thread_detach()      -- once detached cannot be set joinable.
 *
 *   qpt_thread_set_sched()   -- to set scope, policy and param.
 *
 *                               where those are derived from is outside
 *                               the scope of these functions.  But can
 *                               qpt_thread_get_sched() as a seed.
 *
 * For the main thread, can do the above after main thread start.
 *
 * NB: can initialise at most one main thread, and must initialise that before
 *     initialising any other thread and it MUST actually be the main, or only,
 *     pthread !
 *
 * NB: the scope setting for a thread is a preference.  If the scope is not
 *     available, will silently use whatever is available.
 *
 * NB: the reference count is initialised to "1".  The caller is deemed to "own"
 *     the qpt_thread object, and may destroy it before the pthread is created
 *     (or before main thread start).
 *
 *     Any other holders of references to the qpt_thread should call
 *     qpt_thread_set_ref() and qpt_thread_clear_ref() -- unless they have some
 *     other means to guarantee that they will not use the reference after the
 *     qpt_thread has been destroyed.
 */
extern qpt_thread
qpt_thread_init(qpt_init_t init, const char* name)
{
  qpt_thread qpth ;
  bool main_thread ;
  int err ;

  main_thread = (init == qpt_init_main) ;

  if (main_thread)
    assert(!qpt_main_thread_initialised) ;
  else
    assert(qpt_main_thread_initialised && qpthreads_enabled) ;

  /* Create and initialise qpt_thread object.
   *
   * Zeroising sets:
   *
   *   * state            -- qpts_initial
   *
   *   * main             -- X      -- set below
   *
   *   * destroy          -- false
   *   * refcount         -- 0
   *
   *   * name             -- all '\0'   -- see below
   *
   *   * list             -- NULLs  -- put on list below
   *
   *   * attr             -- all 0  -- defaults, see below
   *
   *   * pth_attr         -- NULL   -- default attributes are fetched, below
   *                                   if is qpthreads_enabled.
   *
   *   * pth_id           -- X      -- set when thread created or at main start
   *   * cpu_clock_id     -- X      -- set when thread created or at main start
   *
   *   * end_cond         -- X      -- initialised when created/started
   *   * enjoined         -- false  -- nobody waiting to join, yet
   *
   *   * type             -- NULL   -- set when thread created or at main start
   *   * data             -- NULL   -- set when thread created or at main start
   *
   *   * returned         -- NULL   -- not valid until qpts_exited
   *
   *   * stats            -- all 0  -- not valid in qpts_initial
   *
   * Zeroising the attr sets:
   *
   *   * actual           -- false  -- not the actual values
   *
   *   * detached         -- false  -- the POSIX default
   *
   *   * cancel_disabled  -- false  -- the POSIX initial state for pthread
   *   * cancel_async     -- false  -- ditto
   *
   *   * sched            -- scheduling options
   *
   *     - policy         -- X      -- set below
   *     - param          -- 0's    -- set below or just priority set below
   *     - min priority   -- X      -- set below
   *     - max_priority   -- X      -- set below
   *
   *   * sched_scope      -- X      -- set below
   *   * sched_main_start -- false  -- nothing to do at main start
   *   * sched_inherited  -- false  -- we don't use this
   *
   *   * guard_size       -- 0      -- unknown -- set below to default if !main
   *   * stack_size       -- 0      -- unknown -- set below to default if !main
   *   * stack_addr       -- 0      -- unknown
   */
  qpth = XCALLOC(MTYPE_QPT_THREAD, sizeof(*qpth)) ;

  confirm(qpts_initial == 0) ;

  strncpy(qpth->name, name, sizeof(qpth->name) - 1) ;

  qpth->main = main_thread ;

  /* At this point, if is main we freeze qpthread_enabled, and if we are
   * qpthreads_enabled, pick up the default pthread attributes -- forcing
   * PTHREAD_EXPLICIT_SCHED straight away.
   *
   * Copy the default attributes to the attr.
   */
  if (qpthreads_freeze())
    {
      qpt_pthread_attr_init(qpth) ;
      qpt_pthread_attr_clear_inheritsched(qpth) ;
      qpt_pthread_attr_copy(qpth) ;
    }

  /* Now, for the main_thread we need to sort out (a) what scope(s) we have,
   * (b) pull the policy/params for the process and (c) set up
   * qpt_initial_sched.
   *
   * For other threads we need to sort out the default attributes.
   */
  if (main_thread)
    {
      /* Probe to see which scope or scopes are supported.
       *
       * If !qptreads_enabled, assume scope is fixed to PTHREAD_SCOPE_SYSTEM
       */
      qpt_scope_fixed = true ;
      qpt_scope       = PTHREAD_SCOPE_SYSTEM ;

      if (qpthreads_enabled)
        {
          int scope_count ;

          scope_count = 0 ;

          err = pthread_attr_setscope(qpth->pth_attr, PTHREAD_SCOPE_SYSTEM) ;
          if (err == 0)
            {
              qpt_scope = PTHREAD_SCOPE_SYSTEM ;
              scope_count  += 1 ;
            } ;

          err = pthread_attr_setscope(qpth->pth_attr, PTHREAD_SCOPE_PROCESS) ;
          if (err == 0)
            {
              qpt_scope = PTHREAD_SCOPE_SYSTEM ;    /* NB: preferred  */
              scope_count  += 1 ;
            } ;

          if (scope_count == 0)
            zabort_err("pthreads_attr_setscope() failed for both "
                          "PTHREAD_SCOPE_SYSTEM & PTHREAD_SCOPE_PROCESS", err) ;

          if (scope_count == 2)
            qpt_scope_fixed = false ;
        } ;

      /* No longer interested in the default pthread attributes (if any)
       */
      qpt_pthread_attr_destroy(qpth) ;

      /* Fill in qpth->sched from the *process* policy/param -- this works
       * for !qpthreads_anabled an qpthread_enabled alike.
       *
       * Unless we have access to the "actual" thread we will never know the
       * scope for the main (and possibly only) thread.  Here we assume that
       * the scope is either the one and only scope, or the preferred scope
       * (PTHREAD_SCOPE_SYSTEM).
       */
      qpth->attr->sched_scope = qpt_scope ;
      qpt_sched_get_process(qpth->attr->sched) ;

      /* Can now set qpt_initial_sched, which may later be used as the basis
       * for other thread attributes.
       */
      *qpt_initial_sched = *(qpth->attr->sched) ;

      qpt_main_thread_initialised = true ;
    }
  else
    {
      /* Attributes for creating a new pthread.
       *
       * Has copied the pthread defaults to qpth->pth_attr and qpth->attr,
       * after overriding inheritance.
       *
       * Here we set the scheduling scope, policy and params as required.
       */
      qpt_sched sched ;

      sched = qpth->attr->sched ;

      /* Set either the preferred scope, or the only scope available.
       *
       * Note that we set the qpth->pth_attr scope also: (a) for use when the
       * thread is created, and (b) to collect any side effects (on policy and
       * params).
       */
      qpth->attr->sched_scope = qpt_scope ;     /* set preferred scope  */

      switch (init)
        {
          case qpt_init_process_scope:
            if (!qpt_scope_fixed)
              qpth->attr->sched_scope = PTHREAD_SCOPE_PROCESS ;
            break ;

          case qpt_init_system_scope:
            if (!qpt_scope_fixed)
              qpth->attr->sched_scope = PTHREAD_SCOPE_SYSTEM ;
            break ;

          default:
            zabort("invalid qpt_init_t") ;
        } ;

      qpt_pthread_attr_setscope(qpth->pth_attr, qpth->attr->sched_scope) ;

      /* Choose default policy or that set by option.
       *
       * We assume that the default and the initial policies are known (since
       * the system gave them to us).  If we are using a given policy, that
       * was checked when it was set.
       *
       * Note that we set the qpth->pth_attr policy also: (a) for use when
       * thread is created, and (b) to collect any side effects (on policy and
       * params).
       *
       * Note that glibc defaults to PTHREAD_INHERIT_SCHED (mistake !), and
       * sets policy SCHED_OTHER and priority 0.  However, there is a bug (Bug
       * 7007 dating to 2008) such that setting PTHREAD_EXPLICIT_SCHED is not
       * enough -- it is necessary to set the policy and (or?) param explicitly.
       * It so happens that the following sets both policy and params, so we
       * are fine.
       */
      switch (qpt_use_policy)
        {
          case qpt_use_default_policy:
            sched->policy = qpt_pthread_attr_getschedpolicy(qpth) ;
            break ;

          case qpt_use_main_policy:
            sched->policy = qpt_initial_sched->policy ;

            qassert(qpt_use_params == qpt_use_main_params) ;
            break ;

          case qpt_use_given_policy:
            sched->policy = qpt_given_sched->policy ;
            break ;

          default:
            zabort("invalid qpt_use_policy_t") ;
        } ;

      qpt_sched_get_minmax_priority(sched) ;

      /* Choose default params or those set by option.
       *
       * Note that we set the qpth->pth_attr policy also, for use when thread
       * is created.
       */
      switch (qpt_use_params)
        {
          case qpt_use_default_params:
            /* Set the required policy before fetching the param, just in
             * case that has the side effect of changing the default param.
             */
            qpt_pthread_attr_setschedpolicy(qpth->pth_attr, sched->policy) ;
            *(sched->param) = qpt_pthread_attr_getschedparam(qpth) ;
            break ;

          case qpt_use_main_params:
            *(sched->param) = *(qpt_initial_sched->param) ;
            break ;

          case qpt_use_given_params:
            *(sched->param) = *(qpt_given_sched->param) ;
            break ;

          default:
            zabort("invalid qpt_use_params_t") ;
        } ;

      qpt_pthread_attr_set_sched(qpth->pth_attr, sched) ;
    } ;

  /* Return the new object
   */
  return qpth ;
} ;

/*------------------------------------------------------------------------------
 * Start the main qpt_thread.
 *
 * This should be done after any daemonisation, and before any other pthreads
 * are created.
 *
 * NB: sets qpthreads_active if qpthreads_enabled
 *
 *     So... from this moment on mutex and other operations will be real
 *     operations if is qpthreads_enabled.
 *
 * Note that this is done even if no other threads are going to be started.
 * When is !qpthreads_enabled, this arranges for single thread thread running
 * to look like a minimal multi-thread environment.
 *
 * Does what qpt_thread_create() and qpt_thread_start() do for all other
 * threads -- with suitable variation when !qpthreads_enabled.
 */
extern void
qpt_main_thread_start(qpt_thread qpth, qpt_thread_type type, void* data)
{
  pthread_t pth_id ;
  qstring   log_mess ;

  assert(qpth->main && qpt_main_thread_initialised
                    && !qpthreads_main_started_flag) ;

  qpthreads_main_started_flag = true ;
  qpthreads_active_flag       = qpthreads_enabled ;

  /* Complete the set up of the qpt_thread object, set pthread specific data
   * (or dummy equivalent if !qpthreads_enabled) so that qpt_thread_self()
   * works, apply any pending scheduling parameters and detach the pthread if
   * required.
   */
  if (qpthreads_enabled)
    pth_id = pthread_self() ;
  else
    memset(&pth_id, 0, sizeof(pthread_t)) ;

  LOCK_QPT() ;

  qpt_thread_done_create(qpth, pth_id, type, data) ;

  if (qpth->attr->sched_main_start)
    qpt_sched_set(qpth) ;

  if (qpth->attr->detached && qpthreads_enabled)
    qpt_pthread_detach(qpth) ;

  log_mess = qpt_thread_log_start(qpth) ;

  qpt_own_data_set_value(qpt_self, qpth) ;

  UNLOCK_QPT() ;

  /* Log the start of the main pthread.
   */
  zlog_info("%s", qs_string(log_mess)) ;
  qs_free(log_mess) ;

  /* Log any command line options for further pthreads
   */
  if (qpthreads_enabled)
    qpt_thread_log_options() ;
} ;

/*------------------------------------------------------------------------------
 * Create a new pthread for the prepared qpt_thread -- adding it to the list of
 * known qpt_thread.
 *
 * The given "data" is set in the qpt_thread, and the given pthread function
 * is called and passed a pointer to the qpt_thread.
 *
 * The first thing the created thread does should be qpt_thread_start().
 *
 * NB: FATAL error to attempt this is !qptthreads_active.
 */
extern void
qpt_thread_create(qpt_thread qpth, qpt_thread_type type, void* data,
                                                     void* (*start)(qpt_thread))
{
  typedef void* (*pthread_func)(void*) ;

  int        err ;
  pthread_t  pth_id ;
  qstring    log_mess ;
  pthread_attr_t* pth_attr ;

  if (!qpthreads_active)
    zabort("cannot create threads when is !qpthreads_active") ;

  assert(!qpth->main && (qpth->state == qpts_initial)) ;

  LOCK_QPT() ;

  pth_attr = qpth->pth_attr ;

  if (qpt_use_null_attributes)
    {
      /* Get a fresh set of default pthread attributes (discarding any existing)
       * and copy those to the qpth->attr.
       *
       * This is for the logging -- we really use a NULL set of attributes for
       * creation.
       */
      qpt_pthread_attr_init(qpth) ;
      qpt_pthread_attr_copy(qpth) ;

      pth_attr = NULL ;
    } ;

  err = pthread_create(&pth_id, pth_attr, (pthread_func)start, qpth) ;
  if (err != 0)
    zabort_err(qfs_gen("pthread_create failed for '%s'", qpth->name).str, err) ;

  qpt_thread_done_create(qpth, pth_id, type, data) ;
  log_mess = qpt_thread_log_start(qpth) ;

  UNLOCK_QPT() ;

  zlog_info("%s", qs_string(log_mess)) ;
  qs_free(log_mess) ;
} ;

/*------------------------------------------------------------------------------
 * Have just created the given qpt_thread, with the given pthread_t.
 *
 * NB: this is done by the creating pthread -- NOT the created pthread (except
 *     when "creating" the main thread)
 *
 * For the main pthread this may be !qpthreads_enabled.
 *
 * Requires: for pthreads other than the main: the pthread_t for the newly
 *           created pthread.  Also, the qpt lock must have been acquired
 *           before creating the pthread, and must not be released until after
 *           this returns.
 *
 *           for the main thread: the pthread_t for the main thread (if is
 *           qpthreads_enabled).  For consistency with other threads, requires
 *           the qpt lock.
 *
 *           For all threads requires the value for the "data" entry in the
 *           qpt_thread.
 *
 * Sets:
 *
 *   qpth->pth_id         -- as given
 *
 *   qpth->data           -- as given
 *
 *   qpth->cpu_clock_id   -- pthread_getcpuclockid(), if qpthreads_enabled
 *                                                               and available.
 *                           CLOCK_PROCESS_CPUTIME_ID, if !qpthreads_enabled
 *                                                               and available.
 *
 *   qpth->stats.start    -- now
 *   qpth->stats.cpu_when -- now
 *   qpth->stats.cpu_used -- cpu used to date
 *
 *   qpth->state to qpts_running, under mutex.
 *
 * Adds qpt_thread to the list of known threads.
 *
 * NB: does not set qpt_self -- that has to be done in the pthread itself.
 */
static void
qpt_thread_done_create(qpt_thread qpth, pthread_t pth_id, qpt_thread_type type,
                                                                     void* data)
{
  ASSERT_QPT_LOCKED() ;

  assert(qpth->state == qpts_initial) ;

  /* No longer interested in the attributes (if any)
   */
  qpt_pthread_attr_destroy(qpth) ;

  /* Complete the set up of the qpt_thread
   */
  qpth->pth_id = pth_id ;
  qpth->type   = type ;
  qpth->data   = data ;

  if (qpthreads_enabled)
    qpt_cond_init_new(qpth->end_cond, qpt_cond_quagga) ;

  qpt_get_cpu_clock_id(qpth) ;

  /* Set running and append to list.
   */
  qpth->state = qpts_running ;
  ddl_append(qpt_thread_list, qpth, list) ;

  /* Now take first cpu usage and set the start time -- must be qpts_running
   * for this.
   */
  qpt_get_cpu_time(qpth) ;
  qpth->stats->start = qpth->stats->cpu_when ;
} ;

/*------------------------------------------------------------------------------
 * The current thread has just started.
 *
 * This should be the first thing that a pthread does when it is created.
 *
 * For the main thread, the work is done in qpt_main_thread_start, so does
 * nothing here.
 *
 * NB: for all but the main thread, gains QPT LOCK -- which means that the
 *     child pthread will be held here until the parent has completed the
 *     qpt_thread_create() action.
 *
 * Sets the qpt_self thread specific data, so that the thread can find its own
 * qpt_thread.
 *
 * Returns:  qpth->data -- as set when the qpt_thread was initialised.
 */
extern void*
qpt_thread_start(qpt_thread qpth)
{
  if (!qpth->main)
    {
      assert(qpthreads_enabled) ;

      LOCK_QPT() ;

      qpt_own_data_set_value(qpt_self, qpth) ;

      UNLOCK_QPT() ;
    } ;

  return qpth->data ;
} ;

/*------------------------------------------------------------------------------
 * Detach the given qpt_thread.
 *
 * The default state for a qpt_thread is joinable.  The difference between a
 * joinable qpt_thread and a detached qpt_thread is that:
 *
 *   (a) a detached qpt_thread is not expected to return a "ret" value...
 *
 *       ...any "ret" value is ignored, but will be given to the qpt_thread's
 *       destroy_ret destructor when the qpt_thread is destroyed.
 *
 *   (b) a joinable qpt_thread *must* be joined before it is destroyed...
 *
 *       ...so that any "ret" value is returned first.
 *
 * All qpt_thread (including the main thread) must, eventually, call
 * qpt_thread_end().  When qpt_finish() or qpt_thread_collect() are called,
 * they wait for all known threads to come to a tidy end, before destroying
 * the qpt_thread.  It is expected that all joinable qpt_threads will be
 * joined before qpt_finish() or qpt_thread_collect() -- but if not, they
 * will be treated as if they were detached, and any "ret" is lost.
 *
 * The effect depends on the state of the qpt_thread, if the qpt_thread is
 * not already detached:
 *
 *   * qpts_initial   -- qpt_thread is set detached, and (if qpthreads_enabled)
 *                       the underlying pthread is set detached when it is
 *                       created or (for the main pthread) at
 *                       qpt_main_thread_start().
 *
 *   * qpts_running   -- qpt_thread is set detached, and (if qpthreads_enabled)
 *                       the underlying pthread is set detached.
 *
 *                       qpt_thread_join() has no effect once the qpt_thread is
 *                       detached.
 *
 *                       qpt_thread_destroy() can be used, once the qpt_thread
 *                       has ended -- by qpt_thread_end().
 *
 *   * qpts_ended     -- qpt_thread is set detached.
 *
 *                       qpt_thread_join() has no effect once the qpt_thread is
 *                       detached.
 *
 *                       qpt_thread_destroy() can be used.
 *
 *   * qpts_joined    -- too late to set detached -- has no effect.
 *
 *   * qpts_final     -- too late to set detached -- has no effect.
 *
 * Returns: resulting detached state
 */
extern bool
qpt_thread_detach(qpt_thread qpth)
{
  bool detached ;

  LOCK_QPT() ;

  if (!qpth->attr->detached)
    {
      switch (qpth->state)
        {
          case qpts_initial:
            if (!qpth->main && qpthreads_enabled)
              qpt_pthread_attr_setdetachstate(qpth->pth_attr) ;

            qpth->attr->detached = true ;
            break ;

          case qpts_running:
            if (qpthreads_enabled)
              qpt_pthread_detach(qpth) ;

            qpth->attr->detached = true ;
            break ;

          case qpts_ended:
            qpth->attr->detached = true ;
            break ;

          default:
            break ;
        } ;
    } ;

  detached = qpth->attr->detached ;

  UNLOCK_QPT() ;

  return detached ;
} ;

/*------------------------------------------------------------------------------
 * End the current qpt_thread with the given "ret" pointer
 *
 * The qpt_thread *MUST* be qpts_running.
 *
 * Sets the qpt_thread ret field, fetches the final cpu usage, sets the
 * end time and sets the qpt_thread to be qpts_ended.
 *
 * This applies to the main thread, whether is qpthreads_enabled or not.
 *
 * NB: this does NOT pthread_exit() or any other form of exit.
 *
 *     The purpose of this function is to update the state of the qpt_thread
 *     prior to some form of exit.
 *
 *     For the main (or the last) thread, this may be done as part of the final
 *     shut-down.
 *
 *     For other pthreads this should be done (very) shortly before exiting the
 *     pthread -- anything else will cause *serious* disruption.
 *
 * If the pthread is detached, the pthread may wish to qpt_thread_destroy()
 * itself, or leave that to another.
 *
 * Returns:  the "ret" given, so that:
 *
 *             * last line of pthread function may be:
 *
 *                  return qpt_thread_end(....) ;
 *
 *             * or can:
 *
 *                  pthread_exit(qpt_thread_end(....)) ;
 *
 * NB: the "ret" pointer may be returned by the thread when (if) it then exits,
 *     but the value returned by qpt_thread_join() is the value returned here.
 */
extern void*
qpt_thread_end(void* ret)
{
  qpt_thread qpth ;

  qpth = qpt_thread_self() ;

  LOCK_QPT() ;

  assert(qpth->state == qpts_running) ;

  qpth->ret = ret ;

  qpt_thread_do_end(qpth) ;

  UNLOCK_QPT() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Join given qpt_thread.
 *
 * Does nothing if is detached.  Attempting to join a detached qpt_thread is a
 * mistake, unless caller has a reference, not least because the qpt_thread may
 * have been destroyed !
 *
 * Does nothing if is qpts_initial.
 *
 * Can join self, even if !qpthreads_enabled.  If the qpt_thread has ended,
 * then will be joined in the usual way.  If the qpt_thread is still apparently
 * running, then is treated as if it had just ended and then joined.
 *
 * It is possible for multiple qpt_threads to attempt to join a given
 * qpt_thread.  In this case all joiners will wait for the given qpt_thread
 * to end (if it is running).  Only the first joiner will succeed, and it
 * is deemed to the "own" the qpt_thread.  The "owner" of the qpt_thread may
 * read the qpth->ret and qpth->data AND is responsible for freeing those,
 * if required.  When joining self, the pthread will not own itself if there
 * is another joiner waiting !
 *
 * Returns: state of qpt_thread before joined:
 *
 *    qpts_initial   -- has not got going -- remains qpts_initial
 *
 *                      the qpt_thread may or may not be set to be detached
 *                      when (or if) it is created or starts.
 *
 *                      the caller is already the "owner" of the qpth_thread.
 *
 *    qpts_ended     -- the caller is the "owner" of the qpth_thread and the
 *                      pthread is no more.
 *
 *                      if caller is joining self, then was either:
 *
 *                        - qpts_running -- in which case qpth->ret will be
 *                                          NULL.                                         set outside this code.
 *
 *                        - qpts_ended   -- in which case qpth->ret will be
 *                                          whatever ws set at end time.
 *
 *                      cannot tell the difference.  [It is possible that some
 *                      other code may change qpth->ret... in which case you
 *                      are on your own.]
 *
 *    qpts_joined    -- the qpt_thread has already been joined (possibly by
 *                      another qpt_thread !).
 *
 *    qpts_running   -- is detached !
 *
 * NB: on return, if caller is not the owner, then the qpt_thread may no longer
 *     exist (unless the caller has a reference).
 */
extern qpt_thread_state_t
qpt_thread_join(qpt_thread qpth)
{
  qpt_thread_state_t state ;

  LOCK_QPT() ;

  state = qpth->state ;

  switch (state)
    {
      case qpts_initial:
        break ;

      case qpts_running:
        if (!qpth->attr->detached)
          {
            /* If we own the qpt_thread after collecting it, then we get to
             * set it qpts_joined.
             *
             * If we do not "own" the qpt_thread, it may no longer exist
             */
            if (qpt_thread_do_collect(qpth, true /* join */))
              {
                state = qpts_ended ;    /* Caller "owns" qpt_thread     */
                qpth->state = qpts_joined ;
              }
            else
              state = qpts_joined ;     /* Some other pthread owns      */
          } ;

        break ;

      case qpts_ended:
        qpth->state = qpts_joined ;
        break ;

      case qpts_joined:
        break ;

      default:
        zabort(qfs_gen("unknown qpts_thread_state_t %d", state).str) ;
        break ;
    } ;

  UNLOCK_QPT() ;

  return state ;
} ;

/*------------------------------------------------------------------------------
 * Destroy given qpt_thread (if can and if any).
 *
 * The caller is expected to "own" the qpt_thread, but if the refcount is
 * not zero, the actual destruction is deferred.
 *
 * The process of destruction removes the qpt_thread from the list of known
 * qpt_threads, after which the structure can be destroyed without need to
 * hold the lock.
 *
 * If the type is not NULL, and the relevant destroy functions are not NULL,
 * then will use them to destroy any "data" and/or "ret" values.  Otherwise,
 * it is the caller's responsibility to deal with those.
 *
 * The qpt_thread must be: qpts_initial
 *                     or: qpts_ended  and detached
 *                     or: qpts_joined and !detached
 *
 * If the qpt_thread is not in a suitable state, then does nothing at all,
 * other than log an error.
 *
 * Returns:  NULL
 *
 * NB: a joinable pthread *must* be joined before it is destroyed.  This
 *     includes the main (and possibly only) pthread -- so the main thread
 *     must qpt_thread_end() same like any other, and then either be joined
 *     or have been detached same like any other.
 */
extern qpt_thread
qpt_thread_destroy(qpt_thread qpth)
{
  if (qpth != NULL)
    {
      bool valid, destroy ;

      LOCK_QPT() ;

      valid = ( (qpth->state == qpts_initial) ||
              ( (qpth->state == qpts_ended)  &&  qpth->attr->detached ) ||
              ( (qpth->state == qpts_joined) && !qpth->attr->detached ) ) ;

      if (valid)
        {
          qpth->destroy = true ;

          destroy = (qpth->refcount == 0) ;

          if (destroy)
            qpt_thread_list_del(qpth) ;
        }
      else
        {
          qassert(false) ;      /* throw a wobbly if qdebug     */
          destroy = false ;
        } ;

      UNLOCK_QPT() ;

      if (destroy)
        qpt_thread_free(qpth) ;
      else if (!valid)
        zlog_err("%s(): qpt_thread '%s' state=%u detached=%u", __func__,
                                qpth->name, qpth->state, qpth->attr->detached) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Collect all outstanding qpt_thread.
 *
 * Once all qpt_thread have ended, clear qpthreads_active.
 *
 * At this point it is assumed that any running pthreads are in the process
 * of coming to an end, and either no new pthreads will be created or they
 * too will come (promptly) to an end.
 *
 * By the time this completes, all qpt_thread will have ended and/or been
 * joined.
 *
 * Some may still exist, but will be destroyed when qpt_finish() is
 * called.
 *
 * NB: this is *not* a substitute for qpt_thread_join().
 *
 *     For joinable pthreads the parent of the pthread should take steps to
 *     join and tidy up after it.  That should be done *before* this is called.
 *
 *     For detached pthreads the pthread itself should take steps to
 *     destroy the qpt_thread and deal with any "data" value, but the
 *     "data_destroy" function in the type can deal with this.
 *
 *     This function is intended to enable the orderly close down when there
 *     are one or more detached pthreads, by waiting until all but the current
 *     pthread have come to an end.
 */
extern void
qpt_thread_collect(void)
{
  qpt_thread qpth ;

  /* Walk the list and collect anything which is still running.
   *
   * This will cope if pthreads are still being created, because any new
   * pthreads are appended to the list.
   *
   * During the walk qpt_thread objects may be destroyed by other joiners, or
   * detached qpt_threads destroying themselves.
   */
  qpth    = NULL ;

  while ((qpth = qpt_thread_walk(qpth)) != NULL)
    {
      LOCK_QPT() ;

      if (qpth->state == qpts_running)
        qpt_thread_do_collect(qpth, false /* not join */) ;

      UNLOCK_QPT() ;
    } ;

  /* By the time we reach here, all qpt_threads must be at least qpts_ended,
   * and may be qpts_joined.
   *
   * We can now clear the qpthreads_active flag, which effectively turns off
   * mutex etc. operations and *outlaws* creation of new threads.
   */
  qpt_clear_qpthreads_active() ;
} ;

/*------------------------------------------------------------------------------
 * Collect and destroy all outstanding qpt_thread, clearing qpthreads_active.
 *
 * For completeness, does: qpt_thread_collect() -- see above.
 *
 * Then, set about destroying any remaining qpt_thread.
 *
 * The purpose of this function is to tidy up any qpt_thread which have not
 * already been destroyed by the responsible party.  It is expected that the
 * refcount will be zero, so the qpt_thread will be destroyed, but it doesn't
 * really matter if it isn't.
 */
static void
qpt_thread_finish(void)
{
  qpt_thread qpth, next ;

  qpt_thread_collect() ;

  /* Note we expect the qpt_thread_destroy to succeed, and remove the
   * qpt_thread, but the loop will cope if not -- so will cope if the refcount
   * is not zero or the qpt_thread is not in the right state, for whatever
   * reason.
   */
  next = ddl_head(qpt_thread_list) ;
  while (next != NULL)
    {
      qpth = next ;
      next = ddl_next(qpth, list) ;

      qassert(qpth->refcount == 0) ;

      if (qpth->refcount == 0)
        qpt_thread_destroy(qpth) ;
      else
        zlog_err("%s(): qpt_thread '%s' has refcount %u", __func__,
                                                   qpth->name, qpth->refcount) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Walk the current qpt_thread_list.
 *
 * Use:
 *
 *   qpt_thread qpth ;
 *   ....
 *   qpth = NULL ;
 *   while ((qpth = qpt_thread_walk(qpth)) != NULL)
 *     {
 *     } ;
 *
 * During the walk may do...
 *
 * NB: MUST complete the walk, or MUST qpt_thread_clear_ref() the last qpth
 *     processed.
 */
extern qpt_thread
qpt_thread_walk(qpt_thread qpth)
{
  qpt_thread prev ;

  prev = qpth ;

  LOCK_QPT() ;

  if (prev != NULL)
    qpth = ddl_next(prev, list) ;
  else
    qpth = ddl_head(qpt_thread_list) ;

  if (qpth != NULL)
    ++qpth->refcount ;

  UNLOCK_QPT() ;

  if (prev != NULL)
    {
      qassert(prev->refcount > 0) ;
      qpt_thread_clear_ref(prev) ;
    } ;

  return qpth ;
} ;

/*------------------------------------------------------------------------------
 * Collect the given qpt_thread.
 *
 * MUST be locked, and the qpt_thread MUST be qpts_running -- can collect self.
 *
 * Returns:  true <=> was "join" and calling pthread now "owns" the qpt_thread.
 *
 *          false  => if was "join", another pthread now "owns" the qpt_thread.
 *                    otherwise, ownership is unknown.
 *
 * NB: the the caller is not now the owner, then the qpt_thread may now no
 *     longer exist.
 *
 * NB: does nothing with the "ret" value.
 */
static bool
qpt_thread_do_collect(qpt_thread qpth, bool join)
{
  bool owner ;

  ASSERT_QPT_LOCKED() ;

  qassert(qpth->state == qpts_running) ;

  /* If we are joining, then if we are the first joiner, we will "own" the
   * qpt_thread, and we here set the qpt_thread "enjoined".
   *
   * If not joining, then allow a joiner to turn up and gain ownership.  But
   * in any case make no assumption as to ownership.
   */
  owner = false ;

  if (join && !qpth->enjoined)
    owner = qpth->enjoined = true ;

  /* We are not collecting self, so need to wait for the pthread to end.
   *
   * Waiting for the condition releases and then recovers the mutex...
   * ...so it must not recursive !
   * We are collecting ourself, so do an implied "end".
   */
  if (qpth != qpt_thread_self())
    {
      qpt_thread_list_locked = false ;
      qpt_cond_wait(qpth->end_cond, qpt_thread_list_mutex) ;
      qpt_thread_list_locked = true ;
    }
  else
    qpt_thread_do_end(qpth) ;

  return owner ;
} ;

/*------------------------------------------------------------------------------
 * End the given qpt_thread
 *
 * MUST be locked, and the qpt_thread MUST be qpts_running and MUST be self !
 *
 * NB: does nothing with the "ret" value.
 */
static void
qpt_thread_do_end(qpt_thread qpth)
{
  ASSERT_QPT_LOCKED() ;

  assert((qpth->state == qpts_running) && (qpth == qpt_thread_self())) ;

  qpt_get_cpu_time(qpth) ;
  qpth->stats->end = qpth->stats->cpu_when ;

  qpth->state = qpts_ended ;
  qpt_cond_broadcast(qpth->end_cond) ;  /* does nothing if !qpthreads_enabled */
} ;

/*------------------------------------------------------------------------------
 * Register a reference to the given qpt_thread.
 *
 * Caller has a currently valid qpt_thread, and wishes to keep a reference
 * which will remain valid until it is cleared.
 *
 * Returns:  the qpt_thread
 */
extern qpt_thread
qpt_thread_set_ref(qpt_thread qpth)
{
  LOCK_QPT() ;

  ++qpth->refcount ;

  UNLOCK_QPT() ;

  return qpth ;
} ;

/*------------------------------------------------------------------------------
 * Clear a reference to the given qpt_thread (if any).
 *
 * Caller must assume that this destroys the qpt_thread -- which it will if
 * there are no other references.
 *
 * Returns:  NULL
 */
extern qpt_thread
qpt_thread_clear_ref(qpt_thread qpth)
{
  bool destroy ;

  LOCK_QPT() ;

  assert(qpth->refcount > 0) ;

  --qpth->refcount ;
  destroy = qpth->destroy && (qpth->refcount == 0) ;

  UNLOCK_QPT() ;

  if (destroy)
    qpt_thread_destroy(qpth) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Remove given qpt_thread from the list of qpt_thread (if is on it) -- because
 * is about to destroy it.
 *
 * If is qpts_initial, then is not on the known list, and need do nothing.
 *
 * Otherwise, remove from the known list and set qpts_final.
 */
static void
qpt_thread_list_del(qpt_thread qpth)
{
  ASSERT_QPT_LOCKED() ;

  qassert( (qpth->state == qpts_initial) ||
          ((qpth->state == qpts_ended) && qpth->attr->detached) ||
           (qpth->state == qpts_joined) ) ;

  qassert(qpth->destroy && (qpth->refcount == 0)) ;

  if (qpth->state != qpts_initial)
    {
      ddl_del(qpt_thread_list, qpth, list) ;
      qpth->state = qpts_final ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Free the given qpt_thread and any pth_attr it may have.
 *
 * If the type is not NULL, and the relevant destroy functions are not NULL,
 * then will use them to destroy any "data" and/or "ret" values.  Otherwise,
 * is the caller's responsibility to deal with any such values.
 *
 * Requires refcount to be 0 -- ie: after qpt_thread_list_del().
 *
 * NB: assumes that the caller is now the sole owner of a reference to the
 *     qpt_thread -- so no lock is required.
 */
static void
qpt_thread_free(qpt_thread qpth)
{
  qassert(qpth->destroy && (qpth->refcount == 0)) ;

  qpt_pthread_attr_destroy(qpth) ;

  if (qpth->state != qpts_initial)
    {
      qpt_cond_destroy(qpth->end_cond, keep_it /* embedded */) ;

      if (qpth->type != NULL)
        {
          if (qpth->type->ret_destroy != NULL)
            {
              qpth->type->ret_destroy(qpth->ret) ;
              qpth->ret = NULL ;
            } ;

          if (qpth->type->data_destroy != NULL)
            {
              qpth->type->data_destroy(qpth->data) ;
              qpth->data = NULL ;
            } ;
        } ;
    } ;

  XFREE(MTYPE_QPT_THREAD, qpth) ;
} ;

/*==============================================================================
 * Thread/Process Stats and CPU clock handling
 */

/*------------------------------------------------------------------------------
 * Get the current state of the given qpt_thread, and the latest stats
 */
extern qpt_thread_state_t
qpt_thread_get_stats(qpt_thread qpth, qpt_thread_stats stats)
{
  qpt_thread_state_t state ;

  LOCK_QPT() ;

  qpt_get_cpu_time(qpth) ;

  state = qpth->state ;
  *stats = *(qpth->stats) ;

  UNLOCK_QPT() ;

  return state ;
} ;

/*------------------------------------------------------------------------------
 * Set the cpu_clock_id for the given qpth.
 *
 * Early in the morning it is established what clocks are supported.
 *
 * If is qpthreads_enabled will use what pthread_getcpuclockid() provides, if
 * that is supported -- otherwise returns CLOCK_REALTIME.
 *
 * If is !qpthreads_enabled, this must be the main (and only) pthread, and will
 * use the CLOCK_PROCESS_CPUTIME_ID, if that is supported -- otherwise returns
 * CLOCK_REALTIME
 *
 * Establishes early in the morning whether is supported or not.  When this is
 * called for the main thread, will log "info" if the required clock is not
 * available.
 */
static void
qpt_get_cpu_clock_id(qpt_thread qpth)
{
  qpth->cpu_clock_id = CLOCK_REALTIME ; /* => no CPU time available     */

  if (qpthreads_enabled)
    {
      /* We want the thread specific cpu clock
       */
#ifdef _POSIX_THREAD_CPUTIME
# if _POSIX_THREAD_CPUTIME >= 0
      if (qpt_have_thread_cpu_clock)
        {
          clockid_t got_id ;
          int err ;

          err = pthread_getcpuclockid(qpth->pth_id, &got_id) ;

          if (err == 0)
            {
              switch (got_id)
                {
                  case CLOCK_REALTIME:
                    zlog_err("pthread_getcpuclockid() returned: "
                                                           "CLOCK_REALTIME !") ;
                    break ;

#  ifdef HAVE_CLOCK_MONOTONIC
                  case CLOCK_MONOTONIC:
                    zlog_err("pthread_getcpuclockid() returned: "
                                                          "CLOCK_MONOTONIC !") ;
                    break ;
#  endif

#  ifdef _POSIX_CPUTIME
#   if _POSIX_CPUTIME >= 0
                  case CLOCK_PROCESS_CPUTIME_ID:
                    zlog_err("pthread_getcpuclockid() returned: "
                                                 "CLOCK_PROCESS_CPUTIME_ID !") ;
                    break ;
#   endif
#  endif
                  case CLOCK_THREAD_CPUTIME_ID:
                    zlog_err("pthread_getcpuclockid() returned: "
                                                  "CLOCK_THREAD_CPUTIME_ID !") ;
                    break ;

                  default:
                    /* The clock_id we have been given is plausible enough,
                     * so we use it.
                     */
                    qpth->cpu_clock_id = got_id ;
                    zlog_info("pthread_getcpuclockid() returned: %d", got_id) ;
                    break ;
                } ;
            }
          else
            {
              zlog_err("pthread_getcpuclockid failed: %s", errtoa(err,0).str) ;
            } ;
        }
      else
        {
          if (qpth->main)
            zlog_info("sysconf() says pthread_getcpuclockid() "
                                                          " is not supported") ;
        } ;
# else
      if (qpth->main)
        zlog_info("_POSIX_THREAD_CPUTIME says pthread_getcpuclockid() "
                                                           "is not supported") ;
# endif
#else
      if (qpth->main)
        zlog_info("_POSIX_THREAD_CPUTIME is not defined !!") ;
#endif
    }
  else
    {
      /* We want CLOCK_PROCESS_CPUTIME_ID -- for the main pthread.
       */
      assert(qpth->main) ;

#ifdef _POSIX_CPUTIME
# if _POSIX_CPUTIME >= 0
      if (qpt_have_process_cpu_clock)
        qpth->cpu_clock_id = CLOCK_PROCESS_CPUTIME_ID ;
      else
        {
          if (qpth->main)
            zlog_info("sysconf() says CLOCK_PROCESS_CPUTIME_ID "
                                                           "is not supported") ;
        } ;
# else
      if (qpth->main)
        zlog_info("_POSIX_CPUTIME says CLOCK_PROCESS_CPUTIME_ID "
                                                             "is not supported") ;
# endif
#else
      if (qpth->main)
        zlog_info("_POSIX_CPUTIME is not defined !!") ;
#endif
    } ;
} ;

/*------------------------------------------------------------------------------
 * Refresh the qpt_thread cpu time -- if is qpts_running
 *
 * Sets last.read and last.cpu times.
 *
 * NB: MUST have the LOCK_QPT.
 */
static void
qpt_get_cpu_time(qpt_thread qpth)
{
  ASSERT_QPT_LOCKED() ;

  if (qpth->state == qpts_running)
    {
      enum { target_delta = QTIME(1) / 10000 } ;        /* 0.1 milli-sec */

      confirm(target_delta > 0) ;

      qtime_t before, after ;
      qtime_t delta  = target_delta ;

      while (1)
        {
          before = qt_get_monotonic() ;

          if (qpth->cpu_clock_id != CLOCK_REALTIME)
            qpth->stats->cpu_used = qt_clock_gettime(qpth->cpu_clock_id) ;

          after = qt_get_monotonic() ;

          if ((after - before) <= delta)
            break ;

          delta *= 2 ;          /* avoid getting trapped        */
        } ;

      qpth->stats->cpu_when = (before + after) / 2 ;
    };
} ;

/*==============================================================================
 * qpt_thread attributes and getting/setting scheduling properties.
 */

/*------------------------------------------------------------------------------
 * Get copy of current attributes for the given qpt_thread.
 *
 * Note that unless some external means is used to avoid it, the copy may be
 * out of date by the time this function returns, or may go out of date !
 *
 * Returns the state of the qpt_thread at the time the attributes were copied.
 */
extern qpt_thread_state_t
qpt_thread_get_attr(qpt_thread qpth, qpt_attr attr)
{
  qpt_thread_state_t state ;

  LOCK_QPT() ;

  state = qpth->state ;
  *attr = *(qpth->attr) ;

  UNLOCK_QPT() ;

  return state ;
} ;

/*------------------------------------------------------------------------------
 * Get copy of the scheduling attributes for the given qpt_thread.
 *
 * May be used to set the scheduling attributes of a new thread to the
 * attributes of the current -- ie: in place of PTHREAD_INHERIT_SCHED:
 *
 *   qpt_sched_t bar ;
 *   .....
 *   qpt_thread_set_sched(foo, qpt_thread_get_sched(qpt_thread_self(), bar)) ;
 *
 * Note that unless some external means is used to avoid it, the copy may be
 * out of date by the time this function returns, or may go out of date !
 */
extern qpt_sched
qpt_thread_get_sched(qpt_thread qpth, qpt_sched sched)
{
  LOCK_QPT() ;

  *sched = *(qpth->attr->sched) ;

  UNLOCK_QPT() ;

  return sched ;
} ;

/*------------------------------------------------------------------------------
 * Set scheduling policy in the given scheduling attributes, and set the min/max
 * allowed priorities.
 *
 * This may be used to fill in a policy/param to set via qpt_thread_set_sched().
 * The caller cab use this to discover (a) whether the given policy is
 * supported, and if so, what the min/max priorities are.
 *
 * NB: it is possible that the scheduling param for a given policy may have
 *     attributes other than priority -- this is system dependent.  If so, it
 *     is the caller's responsibility to set those as required.
 *
 * NB: has no effect on the scheduling param.  It is the callers responsibility
 *     to set something sensible before calling qpt_thread_set_sched().
 *
 * This has no effect on any thread.
 */
extern bool
qpt_sched_set_policy(qpt_sched sched, int policy)
{
  sched->policy = policy ;
  return qpt_sched_get_minmax_priority(sched) ;
} ;

/*------------------------------------------------------------------------------
 * Set policy and params scheduling properties for the given qpt_thread.
 *
 * If is qpts_initial the policy and params are stored until the thread is
 * created, or for the main thread, main thread start.
 *
 * If is qpts_running, this has immediate effect.  If is qpthreads_enabled,
 * then uses pthread_setschedparam(), otherwise uses sched_setscheduler() (for
 * the main and only thread).
 *
 * NB: it is not known whether special privileges are required to set some or
 *     all policy/params.   TODO !!
 *
 * Does nothing if the given policy is not known, but logs an error.
 *
 * Clamps the priority to between the min/max allowed for the policy, and logs
 * a warning if has to do this.
 *
 * Ignored if qpt_thread is not qpts_initial or qpts_running.
 *
 * Returns:  true <=> known policy and was qpts_initial or qpts_running
 */
extern bool
qpt_thread_set_sched(qpt_thread qpth, qpt_sched sched)
{
  bool done ;

  /* Make sure we have the limits for requested policy and reject any attempt
   * to set an unknown policy -- log as error.
   *
   * Clamp the priority within the limits, and warn if has to do so.
   */
  done = qpt_sched_get_minmax_priority(sched) ;

  if (!done)
    {
      zlog_err("Attempt to set unknown scheduling policy %d for '%s' thread",
                                                    sched->policy, qpth->name) ;
      return false ;
    } ;

  if (sched->param->sched_priority < sched->min_priority)
    {
      zlog_warn("Attempt to set priority %d < minumum %d for '%s' thread",
                sched->param->sched_priority, sched->min_priority, qpth->name) ;

      sched->param->sched_priority = sched->min_priority ;
    } ;

  if (sched->param->sched_priority > sched->max_priority)
    {
      zlog_warn("Attempt to set priority %d > maximum %d for '%s' thread",
                sched->param->sched_priority, sched->max_priority, qpth->name) ;

      sched->param->sched_priority = sched->max_priority ;
    } ;

  /* Now apply if is qpts_initial or qpts_running
   *
   */
  LOCK_QPT() ;

  switch (qpth->state)
    {
      case qpts_initial:
        *(qpth->attr->sched) = *sched ;

        if (qpth->main)
          qpth->attr->sched_main_start = true ;
        else
          qpt_pthread_attr_set_sched(qpth->pth_attr, sched) ;

        done = true ;
        break ;

      case qpts_running:
        *(qpth->attr->sched) = *sched ;

        qpt_sched_set(qpth) ;

        done = true ;
        break ;

      default:
        done = false ;
        break ;
    } ;

  UNLOCK_QPT() ;

  return done ;
} ;

/*==============================================================================
 * Logging functions
 */

static qfb_nam_t qpt_scope_name(int scope) ;
static qfb_nam_t qpt_policy_name(int policy) ;

/*------------------------------------------------------------------------------
 * Log any command line options for further pthreads
 */
static void
qpt_thread_log_options(void)
{
  const char* dp ;

  if (qpt_use_params == qpt_use_default_params)
    dp = " with default params" ;
  else
    dp = "" ;

  switch (qpt_use_policy)
    {
      case qpt_use_default_policy:
        break ;

      case qpt_use_main_policy:
        zlog_info("When creating pthreads will use same scheduling policy"
                                            " as main pthread, %s%s",
                      qpt_policy_name(qpt_initial_sched->policy ).str, dp) ;
        break ;

      case qpt_use_given_policy:
        zlog_info("When creating pthreads will use scheduling policy %s%s",
                        qpt_policy_name(qpt_given_sched->policy ).str, dp) ;
        break ;

      default:
        zabort("invalid qpt_use_policy_t") ;
    } ;

  switch (qpt_use_params)
    {
      case qpt_use_default_params:
          break ;

      case qpt_use_main_params:
        zlog_info("When creating pthreads will use same scheduling params"
                            " as main pthread, priority=%d (%d..%d)",
                                 qpt_initial_sched->param->sched_priority,
                                 qpt_initial_sched->max_priority,
                                 qpt_initial_sched->min_priority) ;
        break ;

      case qpt_use_given_params:
        zlog_info("When creating pthreads will use given scheduling params,"
                                             " priority=%d (%d..%d)",
                                 qpt_given_sched->param->sched_priority,
                                 qpt_given_sched->max_priority,
                                 qpt_given_sched->min_priority) ;
        break ;

      default:
        zabort("invalid qpt_use_params_t") ;
    } ;

  if (qpt_use_null_attributes)
    zlog_info("When creating pthreads will use NULL (all default)"
                                                 " scheduling attributes") ;
} ;

/*------------------------------------------------------------------------------
 * Construct a string describing the state of the given qpt_thread when it has
 * just started -- ready for logging.
 *
 * Returns:  brand new qstring -- which the call must free.
 */
static qstring
qpt_thread_log_start(qpt_thread qpth)
{
  qpt_attr_t  attr_init ;
  qpt_attr    attr ;
  qstring     qs ;

  ASSERT_QPT_LOCKED() ;

  qs = qs_new(500) ;

  /* Make a copy of the attributes the thread was created with.
   */
  attr = qpth->attr ;
  *attr_init = *attr ;

  /* If we can, get the actual pthread attributes -- does nothing if !qpthreads
   * enabled -- and transfer same to the qpth->attr as "actual".
   *
   * Otherwise, at least get the scheduling policy and param.
   */
  if (qpt_pthread_attr_actual(qpth))
    {
      qpt_pthread_attr_copy(qpth) ;
      attr->actual = true ;
    }
  else
    qpt_sched_get_current(qpth) ;

  /* No longer interested in the attributes (if any)
   */
  qpt_pthread_attr_destroy(qpth) ;

  /* If is qpthreads_enabled, show the start of a pthread, along with its
   * detached, scheduling inheritance and scheduling scope.
   *
   * If is not qpthreads_enabled, show the "start" of the main and only thread.
   */
  if (qpthreads_enabled)
    {
      qs_printf_a(qs, "pthread '%s'%s started:", qpth->name,
                                                   qpth->main ? "(main)" : "") ;

      if (qpt_use_null_attributes && !qpth->main)
        qs_append_str(qs, " *NULL Attributes*") ;

      /* Show Detached/Joinable and if Inherit-Sched (which do not expect) show
       * that also.
       */
      qs_printf_a(qs, " %s%s", attr->detached ? "Detached"
                                              : "Joinable",
                               attr->sched_inherited ? " Inherit-Sched (!!)"
                                                     : "") ;
      /* Show the scope.
       *
       * If we have the actual scope then:
       *
       *   * we are surprised if that is not the requested scope (except for
       *     main, where we did not request anything).
       *
       *   * if the scope is known to be fixed, we are (very) surprised if the
       *     actual scope is not the only available one.
       */
      qs_printf_a(qs, " %s", qpt_scope_name(attr->sched_scope).str) ;

      if (attr->actual)
        {
          qs_append_str(qs, "(actual)") ;

          if ((attr->sched_scope != attr_init->sched_scope) && !qpth->main)
            qs_printf_a(qs, " *but requested=%s*",
                                       qpt_scope_name(attr_init->sched_scope).str) ;

          if (qpt_scope_fixed && (attr->sched_scope != qpt_scope))
            qs_printf_a(qs, " *but only scope=%s*", qpt_scope_name(qpt_scope).str) ;
        }
      else if (qpth->main)
        qs_append_str(qs, "(assumed)") ;
      else
        qs_append_str(qs, "(requested)") ;
    }
  else
    {
      qassert(qpth->main) ;

      qs_printf_a(qs, "'%s' started (!qpthreads_enabled):", qpth->name) ;
    } ;

  /* Show the scheduling policy.
   *
   * We are surprised if that is not the requested scope (except for main,
   * where we did not request anything).
   */
  qs_printf_a(qs, " %s", qpt_policy_name(attr->sched->policy).str) ;

  if ((attr->sched->policy != attr_init->sched->policy) && !qpth->main)
    qs_printf_a(qs, " *but requested=%s*]",
                                qpt_policy_name(attr_init->sched->policy).str) ;

  /* Show the scheduling param/priority.
   *
   * We are surprised if that is not the requested priority (except for main,
   * where we did not request anything).
   */
  qs_printf_a(qs, " Priority=%d (%d..%d)", attr->sched->param->sched_priority,
                         attr->sched->min_priority, attr->sched->max_priority) ;

  if (sizeof(attr->sched->param) != sizeof(int))
    qs_printf_a(qs, " (plus %d bytes of other params)",
                              (int)(sizeof(attr->sched->param) - sizeof(int))) ;

  if (attr->sched->param->sched_priority !=
                                        attr_init->sched->param->sched_priority)
    qs_printf_a(qs, " [*requested priority was %d*]",
                                      attr_init->sched->param->sched_priority) ;

  /* If qpthreads_enabled, show what we know about the stack.
   */
  if (qpthreads_enabled)
    {
      if (attr->actual)
        qs_printf_a(qs, " Guard-Size=%u Stack=%u/%p(actual)",
                                                    (uint)attr->guard_size,
                                                    (uint)attr->stack_size,
                                                          attr->stack_addr) ;
      else
        qs_printf_a(qs, " Guard-Size=%u Stack=%u/unknown(default)",
                                                    (uint)attr->guard_size,
                                                    (uint)attr->stack_size) ;
    } ;

  /* Done.
   */
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Construct qfb_name_t for PTHREAD_SCOPE_XXX
 */
static qfb_nam_t
qpt_scope_name(int scope)
{
  qfb_nam_t QFB_QFS(qfb, qfs) ;

  switch(scope)
    {
      case PTHREAD_SCOPE_SYSTEM:
        qfs_append(qfs, " System-Scope") ;
        break ;

      case PTHREAD_SCOPE_PROCESS:
        qfs_append(qfs, " Process-Scope") ;
        break ;

      default:
        qfs_printf(qfs, " *unknown-scope=%d*", scope) ;
        break ;
    } ;

  qfs_term(qfs) ;

  return qfb ;
} ;

/*------------------------------------------------------------------------------
 * Construct qfb_name_t for SCHED_XXX
 */
static qfb_nam_t
qpt_policy_name(int policy)
{
  qfb_nam_t QFB_QFS(qfb, qfs) ;

  switch(policy)
    {
      case SCHED_OTHER:
        qfs_append(qfs, " SCHED_OTHER") ;
        break ;

      case SCHED_FIFO:
        qfs_append(qfs, " SCHED_FIFO") ;
        break ;

      case SCHED_RR:
        qfs_append(qfs, " SCHED_RR") ;
        break ;

#ifdef SCHED_SPORADIC
      case SCHED_SPORADIC:
        qfs_append(qfs, " SCHED_SPORADIC") ;
        break ;
#endif

      default:
        qfs_printf(qfs, " SCHED_UNKNOWN=%d", policy) ;
        break ;
    } ;

  qfs_term(qfs) ;

  return qfb ;
} ;

/*==============================================================================
 * Collection of functions to call a pthread_xxx function, and deal with any
 * error (by aborting, in general).
 *
 * In some cases these functions hide some implementation detail, including:
 *
 *   * the use of different functions if is not qpthreads_enabled.
 *
 *   * the possible lack of some non-POSIX function.
 *
 *   * limitations on the available facilities.
 */

/*------------------------------------------------------------------------------
 * Detach the given pthread -- assuming is currently joinable.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static void
qpt_pthread_detach(qpt_thread qpth)
{
  int err ;

  err = pthread_detach(qpth->pth_id) ;
  if (err != 0)
    zabort_err(qfs_gen("qpt_pthread_detach('%s') failed", qpth->name).str,
                                                                          err) ;
} ;

/*------------------------------------------------------------------------------
 * Get empty pthread attributes and set in given qpt_thread.
 *
 * NB: discards any existing attributes !
 */
static void
qpt_pthread_attr_new(qpt_thread qpth)
{
  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr)
    qpt_pthread_attr_destroy(qpth) ;

  qpth->pth_attr = XCALLOC(MTYPE_TMP, sizeof(pthread_attr_t)) ;
} ;

/*------------------------------------------------------------------------------
 * Get default set of pthread attributes.
 *
 * NB: discards any existing attributes !
 */
static void
qpt_pthread_attr_init(qpt_thread qpth)
{
  int err ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  qpt_pthread_attr_new(qpth) ;

  err = pthread_attr_init(qpth->pth_attr) ;
  if (err != 0)
    zabort_err("pthread_attr_init() failed", err) ;
} ;

/*------------------------------------------------------------------------------
 * Getting of actual pthread attributes -- try to hide as far as possible
 * the configuration issues.
 */
enum
{
  qpt_have_attr_actual =
#if defined(HAVE_PTHREAD_GETATTR_NP) || defined(HAVE_PTHREAD_ATTR_GET_NP)
    true
#else
    false
#endif
} ;

#if defined(HAVE_PTHREAD_GETATTR_NP)
/* We prefer pthread_getattr_np() if it is available.
 *
 * To use it, we need only an empty attribute structure.
 */
# define pp_qpt_pthread_attr_actual_prep qpt_pthread_attr_new
# define pp_qpt_pthread_attr_actual_get  pthread_getattr_np

#elif defined(HAVE_PTHREAD_ATTR_GET_NP)
/* To use pthread_attr_get_np() we need a freshly initialised attribute
 * structure.
 */
# define pp_qpt_pthread_attr_actual_prep qpt_pthread_attr_init
# define pp_qpt_pthread_attr_actual_get  pthread_attr_get_np

#else
/* Dummies
 */
# define pp_qpt_pthread_attr_actual_prep qpt_pthread_attr_new
# define pp_qpt_pthread_attr_actual_get  qpt_pthread_getattr_dummy

static int
qpt_pthread_getattr_dummy(pthread_t pth_id, pthread_attr_t* pth_attr)
{
  return 0 ;
} ;

#endif

/*------------------------------------------------------------------------------
 * Get actual pthread attributes for the given pthread_t -- if the system
 * supports it and is qpthreads_enabled.
 *
 * NB: if can get the actual attributes, replaces any existing qpth->pth_attr
 *     set.
 *
 *     if cannot get the actual attributes, retains any existing qpth->pth_attr
 *     set.
 */
static bool
qpt_pthread_attr_actual(qpt_thread qpth)
{
  if (qpthreads_enabled && qpt_have_attr_actual)
    {
      int err ;

      ASSERT_QPT_LOCKED() ;

      pp_qpt_pthread_attr_actual_prep(qpth) ;
      err  = pp_qpt_pthread_attr_actual_get(qpth->pth_id, qpth->pth_attr) ;

      if (err != 0)
        zabort_err(STRING_VALUE(pp_qpt_pthread_attr_actual_get)
                                                             "() failed", err) ;

      return true ;
   }
  else
    return false ;
} ;

/*------------------------------------------------------------------------------
 * Copy attributes from qpth->pth_attr to qpth->attr.
 */
static void
qpt_pthread_attr_copy(qpt_thread qpth)
{
  qpt_attr attr ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  attr = qpth->attr ;

  attr->detached        = qpt_pthread_attr_getdetachstate(qpth) ;

  attr->sched_inherited = qpt_pthread_attr_getinheritsched(qpth) ;

  attr->sched_scope     = qpt_pthread_attr_getscope(qpth) ;
  attr->sched->policy   = qpt_pthread_attr_getschedpolicy(qpth) ;
  *(attr->sched->param) = qpt_pthread_attr_getschedparam(qpth) ;

  attr->guard_size      = qpt_pthread_attr_getguardsize(qpth) ;
  attr->stack_size      = qpt_pthread_attr_getstacksize(qpth) ;
  attr->stack_addr      = qpt_pthread_attr_getstackaddr(qpth) ;

  qpt_sched_get_minmax_priority(attr->sched) ;
} ;

/*------------------------------------------------------------------------------
 * Get PTHREAD_CREATE_DETACHED state from the given pthread_attr_t.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static bool
qpt_pthread_attr_getdetachstate(qpt_thread qpth)
{
  bool detached ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  detached = false ;            /* Unless definitely PTHREAD_INHERIT_SCHED */

  if (qpth->pth_attr != NULL)
    {
      int err, detach ;

      err = pthread_attr_getdetachstate(qpth->pth_attr, &detach) ;
      if (err != 0)
        zabort_err("pthread_attr_getdetachstate() failed", err) ;

      switch (detach)
        {
          case PTHREAD_CREATE_DETACHED:
            detached = true ;
            break ;

          case PTHREAD_CREATE_JOINABLE:
            break ;

          default:
            zabort(qfs_gen("pthread_attr_getdetachstate()"
                                  " returned unknown value: %d", detach).str) ;
        } ;
    } ;

  return detached ;
} ;

/*------------------------------------------------------------------------------
 * Clear scheduling attribute inheritance: ie set PTHREAD_EXPLICIT_SCHED
 *
 * Changes the given qpt_thread's pth_attr (if any).
 *
 * NB: the effect of this on the scope, policy and param in the attributes
 *     is not defined by POSIX -- at all.
 *
 *     The assumption is that a sensible implementation will arrange for the
 *     attributes to take their default values.  Or they may already be set
 *     to those, so that setting PTHREAD_EXPLICIT_SCHED works "naturally".
 */
static void
qpt_pthread_attr_clear_inheritsched(qpt_thread qpth)
{
  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

      err = pthread_attr_setinheritsched(qpth->pth_attr,
                                                       PTHREAD_EXPLICIT_SCHED) ;
      if (err != 0)
        zabort_err("pthreads_attr_setinheritsched(PTHREAD_EXPLICIT_SCHED) "
                                                                "failed", err) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Get scheduling inheritance from the given qpt_thread's pth_attr (if any).
 */
static bool
qpt_pthread_attr_getinheritsched(qpt_thread qpth)
{
  bool inherited ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  inherited = false ;           /* Unless definitely PTHREAD_INHERIT_SCHED */

  if (qpth->pth_attr != NULL)
    {
      int err, inherit ;

      err = pthread_attr_getinheritsched(qpth->pth_attr, &inherit) ;
      if (err != 0)
        zabort_err("pthread_attr_getinheritsched() failed", err) ;

      switch (inherit)
        {
          case PTHREAD_INHERIT_SCHED:
            inherited = true ;
            break ;

          case PTHREAD_EXPLICIT_SCHED:
            break ;

          default:
            zabort(qfs_gen("pthread_attr_getinheritsched()"
                                  " returned unknown value: %d", inherit).str) ;
        } ;
    } ;

  return inherited ;
} ;

/*------------------------------------------------------------------------------
 * Get scheduling scope from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns the (preferred) qpt_scope.
 *
 * NB: in the (unlikely) event that the system returns a scope which was
 *     previously found to be unavailable, logs an error, but returns the
 *     scope as received.
 */
static int
qpt_pthread_attr_getscope(qpt_thread qpth)
{
  int scope ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

      err = pthread_attr_getscope(qpth->pth_attr, &scope) ;
      if (err != 0)
        zabort_err("pthread_attr_getscope() failed", err) ;

      switch (scope)
        {
          case PTHREAD_SCOPE_PROCESS:
            if (qpt_scope_fixed && (qpt_scope != scope))
              zlog_err("pthread_attr_getscope() returned PTHREAD_SCOPE_PROCESS"
                                        " which was previously *unavailable*") ;
            break ;

          case PTHREAD_SCOPE_SYSTEM:
            if (qpt_scope_fixed && (qpt_scope != scope))
              zlog_err("pthread_attr_getscope() returned PTHREAD_SCOPE_SYSTEM"
                                        " which was previously *unavailable*") ;
            break ;

          default:
            zabort(qfs_gen("pthread_attr_getscope() returned unknown value: %d",
                                                                   scope).str) ;
        } ;
    }
  else
    scope = qpt_scope ;

  return scope ;
} ;

/*------------------------------------------------------------------------------
 * Get scheduling policy from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns the SCHED_OTHER.
 */
static int
qpt_pthread_attr_getschedpolicy(qpt_thread qpth)
{
  int policy ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

      err = pthread_attr_getschedpolicy(qpth->pth_attr, &policy) ;
      if (err != 0)
        zabort_err("pthread_attr_getschedpolicy() failed", err) ;
    }
  else
    policy = SCHED_OTHER ;

  return policy ;
} ;

/*------------------------------------------------------------------------------
 * Get scheduling param from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns a zeroed set of sched_param.
 */
static struct sched_param
qpt_pthread_attr_getschedparam(qpt_thread qpth)
{
  struct sched_param param ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  memset(&param, 0, sizeof(struct sched_param)) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

      err = pthread_attr_getschedparam(qpth->pth_attr, &param) ;
      if (err != 0)
        zabort_err("pthread_attr_getschedparam() failed", err) ;
    } ;

  return param ;
} ;

/*------------------------------------------------------------------------------
 * Get guard size  from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns zero.
 */
static size_t
qpt_pthread_attr_getguardsize(qpt_thread qpth)
{
  size_t size ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

      err = pthread_attr_getguardsize(qpth->pth_attr, &size) ;
      if (err != 0)
        zabort_err("pthread_attr_getguardsize() failed", err) ;
    }
  else
    size = 0 ;

  return size ;
} ;

/*------------------------------------------------------------------------------
 * Get stack address from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns zero.
 */
static size_t
qpt_pthread_attr_getstacksize(qpt_thread qpth)
{
  size_t size ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;

#ifdef HAVE_PTHREAD_ATTR_GETSTACK
      void* addr ;

      err = pthread_attr_getstack(qpth->pth_attr, &addr, &size) ;
      if (err != 0)
        zabort_err("pthread_attr_getstack() failed", err) ;
#else
      err = pthread_attr_getstacksize(qpth->pth_attr, &size) ;
      if (err != 0)
        zabort_err("pthread_attr_getstacksize() failed", err) ;
#endif
    }
  else
    size = 0 ;

  return size ;
} ;

/*------------------------------------------------------------------------------
 * Get stack address from the given qpt_thread's pth_attr (if any).
 *
 * If no pth_attr, returns NULL.
 */
static void*
qpt_pthread_attr_getstackaddr(qpt_thread qpth)
{
  void* addr ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      int err ;
#ifdef HAVE_PTHREAD_ATTR_GETSTACK
      size_t size ;
      err = pthread_attr_getstack(qpth->pth_attr, &addr, &size) ;
      if (err != 0)
        zabort_err("pthread_attr_getstack() failed", err) ;
#else
      err = pthread_attr_getstackaddr(qpth->pth_attr, &addr) ;
      if (err != 0)
        zabort_err("pthread_attr_getstackaddr() failed", err) ;
#endif
    }
  else
    addr = NULL ;

  return addr ;
} ;

/*------------------------------------------------------------------------------
 * Set PTHREAD_CREATE_DETACHED in the given pthread_attr_t.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static void
qpt_pthread_attr_setdetachstate(pthread_attr_t* pth_attr)
{
  int err ;

  err = pthread_attr_setdetachstate(pth_attr, PTHREAD_CREATE_DETACHED) ;
  if (err != 0)
    zabort_err("pthread_attr_setdetachstate(PTHREAD_CREATE_DETACHED) failed",
                                                                          err) ;
} ;

/*------------------------------------------------------------------------------
 * Set scheduling scope in the given pthread_attr_t.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static void
qpt_pthread_attr_setscope(pthread_attr_t* pth_attr, int scope)
{
  int err ;

  err = pthread_attr_setscope(pth_attr, scope) ;
  if (err != 0)
    zabort_err(qfs_gen("pthread_attr_setscope(%d) failed", scope).str, err) ;
} ;

/*------------------------------------------------------------------------------
 * Set scheduling policy and params in the given pthread_attr_t.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static void
qpt_pthread_attr_setschedpolicy(pthread_attr_t* pth_attr, int policy)
{
  int err ;

  err = pthread_attr_setschedpolicy(pth_attr, policy) ;
  if (err != 0)
    zabort_err(qfs_gen("pthread_attr_setschedpolicy(%d) failed", policy).str,
                                                                          err) ;
} ;

/*------------------------------------------------------------------------------
 * Set scheduling policy and params in the given pthread_attr_t.
 *
 * This is a simple wrapper, to deal with any error return.
 */
static void
qpt_pthread_attr_set_sched(pthread_attr_t* pth_attr, qpt_sched_t sched)
{
  int err ;

  qpt_pthread_attr_setschedpolicy(pth_attr, sched->policy) ;

  err = pthread_attr_setschedparam(pth_attr, sched->param) ;
  if (err != 0)
    zabort_err(qfs_gen("pthread_attr_setschedparam(priority=%d) failed",
                                       sched->param->sched_priority).str, err) ;
} ;

/*------------------------------------------------------------------------------
 * Destroy the qpt_thread's set of pthread attributes, if any.
 *
 * Frees the memory allocated in qpt_thread_attr_new()
 */
static void
qpt_pthread_attr_destroy(qpt_thread qpth)
{
  int err ;

  ASSERT_QPT_LOCKED_IF_ACTIVE(qpth) ;

  if (qpth->pth_attr != NULL)
    {
      err = pthread_attr_destroy(qpth->pth_attr) ;
      if (err != 0)
        zabort_err("pthread_attr_destroy() failed", err) ;

      XFREE(MTYPE_TMP, qpth->pth_attr) ;  /* sets qpth->pth_attr NULL   */
    } ;
} ;

/*==============================================================================
 * Getting and setting the scheduling attributes.
 *
 * The contention scope can be set only when a new pthread is created, and is
 * otherwise pretty much ignored.
 *
 * The policy and params (and as subset of params, the priority) can be set at
 * any time while a pthread is running, and at any time, if !qpthreads_enabled.
 * However, it is possible that some elevated privilege is required to set
 * some or all policy or params -- currently a TODO !
 */

/*------------------------------------------------------------------------------
 * Get the current scheduling policy and param for the given qpt_thread
 *
 * If qpthreads_enabled, get the pthread stuff, otherwise get the process stuff.
 *
 * Fills in: sched->policy
 *           sched->param
 *           sched->min_priority ) according to the policy
 *           sched->max_priority )
 *
 * NB: must be locked (or !qpthread_enabled) and qpts_running
 */
static void
qpt_sched_get_current(qpt_thread qpth)
{
  if (qpthreads_enabled)
    {
      qassert(qpth->state == qpts_running) ;
      qpt_sched_get_pthread(qpth->attr->sched, qpth->pth_id) ;
    }
  else
    qpt_sched_get_process(qpth->attr->sched) ;
} ;

/*------------------------------------------------------------------------------
 * Get the current *process* scheduling policy and param
 *
 * Fills in: sched->policy
 *           sched->param
 *           sched->min_priority ) according to the policy
 *           sched->max_priority )
 */
static void
qpt_sched_get_process(qpt_sched sched)
{
  int  ret ;
  bool known ;

  memset(sched, 0, sizeof(*sched)) ;

  ret = sched_getscheduler(0) ;
  if (ret == -1)
    zabort_err("sched_getscheduler(0) failed", errno) ;

  sched->policy = ret ;

  ret = sched_getparam(0, sched->param) ;
  if (ret == -1)
    zabort_err("sched_getparam(0) failed", errno) ;

  known = qpt_sched_get_minmax_priority(sched) ;
  if (!known)
    zlog_err("sched_getscheduler(0) returned policy %d, "
                                         "which is unknown ??", sched->policy) ;

  if (sched->param->sched_priority < sched->min_priority)
    zlog_err("sched_getparam(0) returned priority %d, "
                      "which is < minimum %d ??", sched->param->sched_priority,
                                                  sched->min_priority) ;
  if (sched->param->sched_priority > sched->max_priority)
    zlog_err("sched_getparam(0) returned priority %d, "
                      "which is > maximum %d ??", sched->param->sched_priority,
                                                  sched->max_priority) ;
} ;

/*------------------------------------------------------------------------------
 * Get the current *pthread* scheduling policy and param
 *
 * Fills in: sched->policy
 *           sched->param
 *           sched->min_priority ) according to the policy
 *           sched->max_priority )
 *
 * NB: must be locked, qpthread_enabled and qpts_running
 */
static void
qpt_sched_get_pthread(qpt_sched sched, pthread_t pth_id)
{
  int  err ;
  bool known ;

  ASSERT_QPT_LOCKED() ;
  qassert(qpthreads_enabled) ;

  memset(sched, 0, sizeof(*sched)) ;

  err = pthread_getschedparam(pth_id, &sched->policy, sched->param) ;
  if (err != 0)
    zabort_err("sched_getscheduler(0) failed", err) ;

  known = qpt_sched_get_minmax_priority(sched) ;
  if (!known)
    zlog_err("pthread_getschedparam() returned policy %d, "
                                         "which is unknown ??", sched->policy) ;

  if (sched->param->sched_priority < sched->min_priority)
    zlog_err("pthread_getschedparam() returned priority %d, "
                      "which is < minimum %d ??", sched->param->sched_priority,
                                                  sched->min_priority) ;
  if (sched->param->sched_priority > sched->max_priority)
    zlog_err("pthread_getschedparam() returned priority %d, "
                      "which is > maximum %d ??", sched->param->sched_priority,
                                                  sched->max_priority) ;
} ;

/*------------------------------------------------------------------------------
 * Get the min/max available priorities for the (now) current sched->policy.
 *
 * If the policy is not recognised, sets both min/max to zero.
 *
 * Returns: true <=> policy is recognised.
 */
static bool
qpt_sched_get_minmax_priority(qpt_sched sched)
{
  sched->min_priority = sched_get_priority_min(sched->policy) ;
  sched->max_priority = sched_get_priority_min(sched->policy) ;

  if ((sched->min_priority != -1) && (sched->max_priority != -1))
    return true ;               /* OK !         */

  if (errno != EINVAL)
    zabort_err(qfs_gen("sched_get_priority_min/max(%d) failed",
                                                    sched->policy).str, errno) ;

  sched->min_priority = 0 ;
  sched->max_priority = 0 ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Set the scheduling policy and params for a *running* thread (including main
 * and possibly only thread).
 *
 * If is qpthreads_enabled use pthread_setschedparam().  Otherwise, must be
 * main thread and we use sched_setscheduler().
 *
 * Must have LOCK_QPT and be qpts_running.
 *
 * Assumes that the policy and priority etc. are all valid -- so will crash
 * if either is refused.
 *
 * NB: it is not know what policy/params, if any, require some elevated
 *     privilege.  TODO !
 */
static void
qpt_sched_set(qpt_thread qpth)
{
  int err ;
  const char* what ;

  ASSERT_QPT_LOCKED() ;

  assert(qpth->state == qpts_running) ;

  if (qpthreads_enabled)
    {
      err = pthread_setschedparam(qpth->pth_id, qpth->attr->sched->policy,
                                                qpth->attr->sched->param) ;
      if (err == 0)
        return ;

      what = "pthread_setschedparam" ;
    }
  else
    {
      int ret ;

      qassert(qpth->main) ;

      ret = sched_setscheduler(0, qpth->attr->sched->policy,
                                                qpth->attr->sched->param) ;
      if (ret == 0)
        return ;

      what = "sched_setscheduler" ;
      err  = errno ;
    } ;

  zabort_err(qfs_gen("%s() policy=%d, priority=%d failed for '%s'", what,
                       qpth->attr->sched->policy,
                       qpth->attr->sched->param->sched_priority,
                                                         qpth->name).str, err) ;
} ;

/*==============================================================================
 * Mutex initialise and destroy.
 *
 * For use by a "watch-dog", every qpt_mutex is held on a list of mutexes,
 * and is given a name.  The list is protected by a spinlock.  To allow a
 * qpt_mutex to be "held" by "watch-dog(s)" they must all be allocated
 * dynamically.
 */
static void qpt_mutex_do_destroy(qpt_mutex mx) ;
static void qpt_mutex_abort(qpt_mutex mx, int err, const char* op) ;

/*------------------------------------------------------------------------------
 * Allocate and Initialise Mutex
 *
 * Does nothing if !qpthreads_enabled -- but freezes the state (attempting to
 * later enable qpthreads will be a FATAL error).
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
 *
 * Returns the mutex -- or NULL if !qpthreads_enabled.
 */
extern qpt_mutex
qpt_mutex_new(qpt_mutex_options_t opts, const char* name)
{
  qpt_mutex mx ;
  pthread_mutexattr_t mutex_attr ;
  int  type ;
  int  err ;

  if (!qpthreads_freeze())
    return NULL ;

  mx = XCALLOC(MTYPE_QPT_MUTEX, sizeof(*mx)) ;

  /* Zeroising sets:
   *
   *   pm             -- X         -- set below
   *   list           -- NULLs     -- set below
   *   name           -- all '\0'  -- set below
   *
   *   held           -- 0         -- not held
   *   destroy        -- false     -- not scheduled for destruction
   */

  /* Set the name
   */
  strncpy(mx->name, name, sizeof(mx->name) - 1) ;

  /* Set up attributes so we can set the mutex type
   */
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

  err = pthread_mutexattr_settype(&mutex_attr, type);
  if (err != 0)
    zabort_err("pthread_mutexattr_settype failed", err) ;

  /* Now we're ready to initialize the mutex itself
   */
  err = pthread_mutex_init(mx->pm, &mutex_attr) ;
  if (err != 0)
    qpt_mutex_abort(mx, err, "pthread_mutex_init") ;

  /* Be tidy with the attributes
   */
  err = pthread_mutexattr_destroy(&mutex_attr) ;
  if (err != 0)
    zabort_err("pthread_mutexattr_destroy failed", err) ;

  /* Add to the list of known mutexes
   *
   * Note that "held" and "destroy" are already set as required, and that
   * the name field has been zeroised.
   */
  qpt_spin_lock(qpt_mutexes.slk) ;
  ddl_append(qpt_mutexes.base, mx, list) ;
  qpt_spin_unlock(qpt_mutexes.slk) ;

  /* Done: return the mutex
   */
  return mx ;
} ;

/*------------------------------------------------------------------------------
 * Destroy given mutex (if any), and free it
 *                                       -- or do nothing if !qpthreads_enabled.
 *
 * Returns NULL.
 */
extern qpt_mutex
qpt_mutex_destroy(qpt_mutex mx)
{
  if (qpthreads_enabled && (mx != NULL))
    {
      bool  destroy ;

      /* Deal with the interaction with the list of mutexes, under spin-lock.
       */
      qpt_spin_lock(qpt_mutexes.slk) ;

      destroy = (mx->held == 0) ;

      if (destroy)
        {
          /* The mx is not held, so can remove from the list and proceed
           * to destroy.
           */
          ddl_del(qpt_mutexes.base, mx, list) ;
        }
      else
        {
          /* The mx is held, so cannot destroy now.
           *
           * Mark for destruction when the held count hits zero.
           */
          mx->destroy = true ;
        } ;

      qpt_spin_unlock(qpt_mutexes.slk) ;

      /* If not held, proceed to destroy.
       */
      if (destroy)
        qpt_mutex_do_destroy(mx) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Step to next mutex in list of mutexes.
 *
 * Releases the current mutex object (if any) and takes hold of the next mutex
 * object (if any).
 *
 * NB: release/hold does *not* mean lock/unlock the mutex, but releasing/gaining
 *     a hold on the mutex object.
 *
 *     This is for watch-dog and other debug stuff to be able to walk the
 *     list of mutexes.
 *
 * To start a walk along the mutexes, start with mx = NULL.  If walk until
 * this function returns NULL, then all holds will have been released.  See
 * qpt_mutex_step_last() if wish to stop part way through a walk.
 */
extern qpt_mutex
qpt_mutex_step_next(qpt_mutex mx)
{
  qpt_mutex mx_next ;
  bool destroy ;

  /* Under the spin-lock:
   *
   *   1) unlock and decide whether need to destroy the current mutex (if any)
   *
   *   2) step to next mutex on list, or start with the head
   *
   *   3) lock the new mutex (if any)
   *
   *   4) remove current mutex from list if about to destroy it.
   */
  qpt_spin_lock(qpt_mutexes.slk) ;

  if (mx != NULL)
    {
      assert(mx->held > 0) ;

      --mx->held ;

      destroy = mx->destroy && (mx->held == 0) ;

      mx_next = ddl_next(mx, list) ;
    }
  else
    {
      destroy = false ;
      mx_next = ddl_head(qpt_mutexes.base) ;
    } ;

  if (mx_next != NULL)
    ++mx_next->held ;

  if (destroy)
    ddl_del(qpt_mutexes.base, mx, list) ;

  qpt_spin_unlock(qpt_mutexes.slk) ;

  /* Now, if required to destroy the mutex, we have removed it from the
   * list, so is in our hands entirely.
   */
  if (destroy)
    qpt_mutex_do_destroy(mx) ;

  /* Return the next mutex (if any)
   */
  return mx_next ;
} ;

/*------------------------------------------------------------------------------
 * Release hold on the given mutex, and destroy it if required.
 */
extern void
qpt_mutex_step_last(qpt_mutex mx)
{
  bool destroy ;

  if (mx == NULL)
    return ;                    /* do nothing if no mutex !     */

  /* Under the spin-lock unlock and decide whether need to destroy the
   * current mutex, and if so remove from the list.
   */
  qpt_spin_lock(qpt_mutexes.slk) ;

  assert(mx->held > 0) ;

  --mx->held ;

  destroy = mx->destroy && (mx->held == 0) ;

  if (destroy)
    ddl_del(qpt_mutexes.base, mx, list) ;

  qpt_spin_unlock(qpt_mutexes.slk) ;

  /* Now, if required to destroy the mutex, we have removed it from the
   * list, so is in our hands entirely.
   */
  if (destroy)
    qpt_mutex_do_destroy(mx) ;
} ;

/*------------------------------------------------------------------------------
 * Lock given mutex or time-out
 *                         -- or return immediate success if !qpthreads_active.
 *
 * Returns: wait succeeded (false <=> timed-out).
 *
 * NB: timeout time is an relative CLOCK_REALTIME time.
 *
 * Has to check the return value, so zabort_errno if not EBUSY.
 */
extern bool
qpt_mutex_timedlock(qpt_mutex mx, qtime_t timeout)
{
  if (qpthreads_active)
    {
      struct timespec ts ;
      int err ;

      confirm(_POSIX_TIMERS > 0) ;

      err = pthread_mutex_timedlock(mx->pm, qtime2timespec(&ts,
                                                    qt_add_realtime(timeout))) ;
      if (err != 0)
        {
          if (err != ETIMEDOUT)
            qpt_mutex_abort(mx, err, "pthread_mutex_timedlock") ;

          return false ;
        } ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Actual destruction of qpt_mutex.
 *
 * Must no longer be on the list of mutexes.
 *
 * Note that if the destruction of the actual mutex fails, we do not release
 * the enclosing memory.
 */
static void
qpt_mutex_do_destroy(qpt_mutex mx)
{
  int   err ;

  err = pthread_mutex_destroy(mx->pm) ;

  if (err == 0)
    XFREE(MTYPE_QPT_MUTEX, mx) ;
  else
    {
      /* If we are closing down, then not much point aborting, and may
       * as well make it look as if succeeded if wanted to free it.
       */
      if (qpthreads_active)
        qpt_mutex_abort(mx, err, "pthread_mutex_destroy") ;
      else
        zlog_err("pthread_mutex_destroy(%s) failed: %s", mx->name,
                                                           errtoa(err, 0).str) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Failed to pthread_mutex_lock().
 */
Private void
qpt_mutex_lock_failed(qpt_mutex mx, int err)
{
  qpt_mutex_abort(mx, err, "pthread_mutex_lock") ;
} ;

/*------------------------------------------------------------------------------
 * Failed to pthread_mutex_trylock() -- not EBUSY !
 */
Private void
qpt_mutex_trylock_failed(qpt_mutex mx, int err)
{
  qpt_mutex_abort(mx, err, "pthread_mutex_trylock") ;
} ;

/*------------------------------------------------------------------------------
 * Failed to pthread_mutex_unlock().
 */
Private void
qpt_mutex_unlock_failed(qpt_mutex mx, int err)
{
  qpt_mutex_abort(mx, err, "pthread_mutex_unlock") ;
} ;

/*------------------------------------------------------------------------------
 * Failed in some mutex operation -- fatal !
 */
static void
qpt_mutex_abort(qpt_mutex mx, int err, const char* op)
{
  zabort(qfs_gen("%s(%s) failed: %s", op, mx->name, errtoa(err, 0).str).str) ;
} ;

/*==============================================================================
 * Condition Variable initialise and destroy.
 */

/*------------------------------------------------------------------------------
 * Initialise Condition Variable (allocating if required).
 *
 * Does nothing if !qpthreads_enabled -- but freezes the state (attempting to
 * later enable qpthreads will be a FATAL error).
 *
 * Options:
 *
 *   qpt_cond_quagga     -- use Quagga's default clock
 *   qpt_cond_realtime   -- force CLOCK_REALTIME
 *   qpt_cond_monotonic  -- force CLOCK_MONOTONIC  (if available)
 *
 * NB: FATAL error to attempt this if !qptthreads_enabled.
 *
 * Returns the condition variable -- or original cv if !qpthreads_enabled.
 */
extern qpt_cond
qpt_cond_init_new(qpt_cond cv, enum qpt_cond_options opts)
{
  pthread_condattr_t cond_attr ;
  int err ;

  if (!qpthreads_freeze())
    {
      if (cv != NULL)
        memset(cv, 0x0F, sizeof(qpt_cond_t)) ;
      return cv ;
    } ;

  if (cv == NULL)
    cv = XMALLOC(MTYPE_QPT_COND, sizeof(qpt_cond_t)) ;

  /* Set up attributes so we can set the  type     */
  err = pthread_condattr_init(&cond_attr);
  if (err != 0)
    zabort_err("pthread_condattr_init failed", err) ;

  switch(opts)
  {
    case qpt_cond_quagga:
      break ;
    default:
      zabort("Invalid qpt_cond option") ;
  } ;

  err = pthread_condattr_setclock(&cond_attr, QPT_COND_CLOCK_ID);
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

/*------------------------------------------------------------------------------
 * Destroy given condition variable, and (if required) free it
 *                                       -- or do nothing if !qpthreads_enabled.
 *
 * NB: if !qpthreads_enabled qpt_cond_init_new() will not have allocated
 *     anything, so there can be nothing to release -- so does nothing, but
 *     returns the original condition variable address (if any).
 *
 * Returns NULL if freed the condition variable, otherwise the address of same.
 */
extern qpt_cond
qpt_cond_destroy(qpt_cond cv, free_keep_b free_cond)
{
  if (qpthreads_enabled && (cv != NULL))
    {
      int err ;

      err = pthread_cond_destroy(cv) ;

      if (err == 0)
        {
          if (free_cond)
            XFREE(MTYPE_QPT_COND, cv) ; /* sets cv == NULL      */
        }
      else
        {
          /* If we are closing down, then not much point aborting, and may as
           * well make it look as if succeeded if wanted to free it.
           */
          if (qpthreads_active)
            zabort_err("pthread_cond_destroy failed", err) ;
          else
            zlog_err("pthread_cond_destroy failed (%s)", errtoa(err, 0).str) ;

          if (free_cond)
            cv = NULL ;
        } ;
    } ;

  return cv ;
} ;

/*------------------------------------------------------------------------------
 * Wait for given condition variable or time-out
 *                         -- or return immediate success if !qpthreads_active.
 *
 * Returns: wait succeeded (false <=> timed-out).
 *
 * NB: timeout time is an absolute qtime_mono_t (monotonic time).
 *
 * Has to check the return value, so zabort_errno if not EBUSY.
 */
extern bool
qpt_cond_timedwait(qpt_cond cv, qpt_mutex mx, qtime_mono_t abs_timeout)
{
  struct timespec ts ;
  int err ;

  if (qpthreads_active)
    {
      if (QPT_COND_CLOCK_ID != CLOCK_MONOTONIC)
        {
          abs_timeout = qt_clock_gettime(QPT_COND_CLOCK_ID)
                                         + (abs_timeout - qt_get_monotonic()) ;
        } ;

      err = pthread_cond_timedwait(cv, mx->pm,
                                            qtime2timespec(&ts, abs_timeout)) ;
      if (err == 0)
        return true ;               /* got condition        */
      if (err == ETIMEDOUT)
        return false ;              /* got time-out         */

      zabort_err("pthread_cond_timedwait failed", err) ;
    }
  else
    return true ;
} ;

/*==============================================================================
 * Spinlock initialise and destroy.
 */

/*------------------------------------------------------------------------------
 * Initialise Spinlock -- NB: no allocation option
 *
 * Does nothing if !qpthreads_enabled -- but freezes the state.
 */
extern void
qpt_spin_init(qpt_spin slk)
{
  int err ;

  if (!qpthreads_freeze())
    return ;

  enum {
#ifndef PTHREAD_PROCESS_PRIVATE
    pthread_process_private = 0
#else
    pthread_process_private = PTHREAD_PROCESS_PRIVATE
#endif
  } ;

  err = pthread_spin_init(slk, pthread_process_private) ;
  if (err != 0)
    zabort_err("pthread_spin_init failed", err) ;
} ;

/*------------------------------------------------------------------------------
 * Destroy given spin lock -- NB: no free option
 *                                       -- or do nothing if !qpthreads_enabled.
 */
extern void
qpt_spin_destroy(qpt_spin slk)
{
  if (qpthreads_enabled && (slk != NULL))
    {
      int err = pthread_spin_destroy(slk) ;

      if (err != 0)
        {
          /* If we are closing down, then not much point aborting.
           */
          if (qpthreads_active)
            zabort_err("pthread_spin_destroy failed", err) ;
          else
            zlog_err("pthread_spin_destroy failed (%s)", errtoa(err, 0).str) ;
        } ;
    } ;
} ;

/*==============================================================================
 * Signal Handling.
 */

/*------------------------------------------------------------------------------
 * Set thread signal mask.
 *
 * If !qpthreads_enabled will use sigprocmask(), otherwise pthread_sigmask().
 *
 * In fact pthread_sigmask() works for single-threaded processes, but we avoid
 * depending on pthread library if it's not essential.
 *
 * zaborts if gets any error.
 */
extern void
qpt_thread_sigmask(int how, const sigset_t* set, sigset_t* oset)
{
  if (oset != NULL)
    sigemptyset(oset) ;         /* to make absolutely sure      */

  if (qpthreads_enabled)
    {
      int err = pthread_sigmask(how, set, oset) ;
      if (err != 0)
        zabort_err("pthread_sigmask failed", err) ;
    }
  else
    {
      if (sigprocmask(how, set, oset) != 0)
        zabort_errno("sigprocmask failed") ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Send given thread the given signal -- if not qpthreads_enabled, send signal
 * to the process.
 *
 * Does nothing if is not qpts_running.
 *
 * NB: caller is responsible for ensuring that the qpt_thread is valid, for
 *     example by holding a reference to it !
 *
 * Thin wrapper around pthread_kill or raise().
 *
 * zaborts if gets any error.
 */
extern void
qpt_thread_raise(qpt_thread qpth, int signum)
{
  LOCK_QPT() ;

  if (qpth->state == qpts_running)
    {
      if (qpthreads_enabled)
        {
          int err ;
          err = pthread_kill(qpth->pth_id, signum) ;
          if (err != 0)
            zabort_err("pthread_kill() failed", err) ;
        }
      else
        {
          int ret ;
          ret = raise(signum) ;
          if (ret < 0)
            zabort_err("raise() failed", errno) ;
        } ;
    } ;

  UNLOCK_QPT() ;
} ;

/*==============================================================================
 * Thread Specific Data Handling.
 *
 * When creating a key for a piece of thread specific data one could:
 *
 *   a. arrange for all keys to be created before any threads are
 *      created -- or at least before any have a need for the key.
 *
 *   b. use pthread_once() to protect the creation of the key.
 *
 *      Note that there does not appear to be a way of distinguishing a key
 *      that has been created from one that has not.
 *
 * For !qpthreads_enabled systems, the "thread specific" data is embedded in
 * the qpt_data object.
 */

/*------------------------------------------------------------------------------
 * Create the given thread specific data.
 *
 * NB: if no value is ever set, then qpt_data_get_value() will return NULL
 *     (whether qpthreads_enabled, or not).
 */
extern void
qpt_own_data_create(qpt_own_data data, void (*destructor)(void*))
{
  memset(data, 0, sizeof(union qpt_own_data)) ;

  if (qpthreads_freeze())
    {
      int err = pthread_key_create(&data->key, destructor) ;
      if (err != 0)
        zabort_err("pthread_key_create failed", err) ;
    }
  else
    data->proxy.destructor = destructor ;
} ;

/*------------------------------------------------------------------------------
 * Delete the given thread specific data.
 *
 * NB: it is the caller's responsibility to release any memory the value of
 *     the thread specific data may refer to.
 */
extern void
qpt_own_data_delete(qpt_own_data data)
{
  if (qpthreads_enabled)
    {
      int err = pthread_key_delete(data->key) ;
      if (err != 0)
        zabort_err("pthread_key_delete failed", err) ;
    }
  else
    {
      if ((data->proxy.value != NULL) && (data->proxy.destructor != NULL))
        data->proxy.destructor(data->proxy.value) ;
    } ;

  memset(data, 0, sizeof(union qpt_own_data)) ;
} ;

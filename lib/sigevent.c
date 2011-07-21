/* Quagga signal handling functions.
 * Copyright (C) 2004 Paul Jakma,
 *
 * This file is part of Quagga.
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "zebra.h"
#include "misc.h"
#include "sigevent.h"
#include "log.h"
#include "vty.h"
#include "qpnexus.h"
#include "qpthreads.h"

#include <stdarg.h>

/*------------------------------------------------------------------------------
 * Want to get some context for core and exit handlers.
 */
#ifdef HAVE_UCONTEXT_H
#ifdef GNU_LINUX
/* get REG_EIP from ucontext.h */
#ifndef __USE_GNU
#define __USE_GNU
#endif /* __USE_GNU */
#endif /* GNU_LINUX */
#include <ucontext.h>
#endif /* HAVE_UCONTEXT_H */

/*------------------------------------------------------------------------------
 * Use SA_SIGINFO type handlers throughout
 */
#ifndef SA_SIGINFO
#error Sorry... require SA_SIGINFO
#endif

typedef void sig_handler(int signo, siginfo_t* info, void* context) ;

/*==============================================================================
 * Signal handling for Quagga.
 *
 * The objectives are:
 *
 *   1) to handle the abnormal terminations so that they are logged, and
 *      any available information logged with them.
 *
 *   2) to ignore a number of signals that have no significance
 *
 *   3) to catch some signals such that they are treated as events in either
 *      the qpthreads or the legacy threads worlds.
 *
 *      For the qpthreads world, these are all handled in the main thread.
 *
 *      These may not be any of the "hard" signals or any of the signals
 *      reserved by the library.
 *
 *   4) to catch some signals such that they cause an "interrupt" to, e.g.
 *      pselect(), but have no other effect.
 *
 *      SIGUSR2 is reserved for this purpose for qpthreads world
 *      (aka SIG_INTERRUPT).
 *
 * Signal disposition is established early in the morning, and is static from
 * then on.
 */

/*==============================================================================
 * Signal Sets.
 *
 * The following signal sets are initialised by signal_init().
 *
 *   * hard_signals   -- signals that mean that the program has misbehaved, and
 *                       should exit, now -- e.g. SIGSEGV or SIGILL.
 *
 *                       In the pthreaded world, these signals are handled
 *                       (or at least, not blocked) by all threads, and it is
 *                       expected that they will be given to the thread which
 *                       has failed.
 *
 *                       These signals are not blocked.
 *
 *                       This includes SIGKILL and SIGSTOP, which cannot be
 *                       blocked, caught or ignored.
 *
 *   * core_signals   -- signals that by default are terminate + core, so this
 *                       more or less the hard_signals, except:
 *
 *                         * excludes SIGKILL and SIGSTOP
 *
 *                         * includes at least one (SIGQUIT) signal that is
 *                           not a hard_signal.
 *
 *                       The default action is to catch these signals, log the
 *                       event and then abort() -- having turned off the
 *                       SIGABRT handler !
 *
 *   * exit_signals   -- signals that by default are terminate, so this
 *                       includes, e.g., SIGTERM or SIGINT.
 *
 *                       The default action is to catch these signals, log the
 *                       event and then exit().
 *
 *   * ignore_signals -- signals which, by default, we wish to ignore, so are
 *                       set to SIG_IGN.
 *
 *   * qsig_signals   -- signals which are caught, and are later signalled to
 *                       quagga_sigevent_process().
 *
 *                       Note that multiple signals may be seen before
 *                       quagga_sigevent_process() is run, but they will only
 *                       generate one event.
 *
 *   * qsig_interrupts -- signals which are caught, but otherwise ignored,
 *                       so their only function is to interrupt -- in particular
 *                       to interrupt pselect().
 *
 *   * qsig_reserved  -- signal which the library reserves for itself.
 *
 * In the pthreads world, all signals other than the hard_signals are blocked
 * by all threads other than the main thread.
 *
 * Note that we leave SIGTRAP alone -- so do not disturb debuggger(s).
 */
static bool signals_initialised = false ;

static sigset_t  hard_signals[1] ;
static sigset_t  core_signals[1] ;
static sigset_t  exit_signals[1] ;
static sigset_t  ignore_signals[1] ;
static sigset_t  qsig_signals[1] ;
static sigset_t  qsig_interrupts[1] ;
static sigset_t  qsig_reserved[1] ;

static void qsig_add(int signo, qsig_event* event) ;
static int signal_set_set(sigset_t* set, sig_handler* handler, bool required) ;
static int signal_set(int signo, sig_handler* handler, bool required) ;

static void __attribute__ ((noreturn))
  core_handler(int signo, siginfo_t *info, void *context) ;
static void __attribute__ ((noreturn))
  exit_handler(int signo, siginfo_t* info, void* context) ;
static void
  quagga_signal_handler(int signo, siginfo_t* info, void* context) ;
static void
  quagga_interrupt_handler(int signo, siginfo_t* info, void* context) ;

/*------------------------------------------------------------------------------
 * The following signals are not known to POSIX (2008) or are extensions.
 */
#ifndef  SIGEMT
# define SIGEMT  0
#endif
#ifndef  SIGIO
# define SIGIO   0
#endif
#ifndef  SIGIOT
# define SIGIOT  0
#endif
#ifndef  SIGPOLL
# define SIGPOLL 0
#endif
#ifndef  SIGPROF
# define SIGPROF 0
#endif
#ifndef  SIGPWR
# define SIGPWR  0
#endif
#ifndef  SIGSTKFLT
# define SIGSTKFLT 0
#endif
#ifndef  SIGSYS
# define SIGSYS  0
#endif
#ifndef  SIGVTALRM
# define SIGVTALRM 0
#endif
#ifndef  SIGWINCH
# define SIGWINCH 0
#endif
#ifndef  SIGXRES
# define SIGXRES 0
#endif

/*------------------------------------------------------------------------------
 * The signal handling below assumes that no signal that we have any interest
 * in will have a signal number greater than the following.
 *
 * All the signals we are interested in are initialised in signal_init, so
 * asserts in the code below will trap, very early on, any signal with a
 * larger number than this.
 *
 * This value is used to place a very outside limit on the signal numbers that
 * will attempt to deal with.
 */
enum { SIG_MAX = 128, SIG_COUNT } ;

/* The value is established at signal_init() time, as the maximum signal
 * number to consider -- established by sigaddset() on an empty set.
 */
int sig_max = 0 ;

/*------------------------------------------------------------------------------
 * Quagga signals descriptor struct
 *
 * Maps real signal numbers to "qso" ordinals...  A relatively limited number
 * of signals need to be fed into the event system, this mechanism minimises
 * the work required to discover which signal has gone off.
 *
 * The qsig actions are held in a small vector in the static sigmaster
 * structure.
 */
enum
{
  sig_null   =   0,             /* no real signal is this               */
  sig_min    =   1,             /* first real signal is at least this   */
} ;
typedef uchar sig_num_t ;       /* signal number                        */
CONFIRM(SIG_MAX <= UCHAR_MAX) ;

enum
{
  qso_null   =   0,             /* no qsig uses this                    */
  qso_min    =   1,             /* first qsig is this                   */
  qso_max    =  10,             /* only a handful are really required   */

  qso_count,                    /* number qs ordinals                   */
} ;
typedef uchar qs_ord_t ;        /* qs ordinal                           */

/*------------------------------------------------------------------------------
 * Static structure for all known qsig_signals.
 *
 * Note that the qsig_interrupts are not recorded here !
 */
struct quagga_sigevent_master
{
  qs_ord_t      qsigc ;                 /* number of signals known      */
  volatile sig_atomic_t caught[qso_count] ;

  qsig_event*   event[qso_count] ;      /* how to deal with them        */

  qs_ord_t      map[SIG_COUNT] ;        /* real signal to qs ordinal    */

#ifdef SIGEVENT_SCHEDULE_THREAD
  struct thread *t;
#endif

} qsig_master ;

/*------------------------------------------------------------------------------
 * Initialise signals.
 *
 *   1. construct the signal sets discussed above.
 *
 *   2. set default handlers for: core_signals   -- core_handler()
 *                                exit_signals   -- exit_handler()
 *                                ignore_signals -- SIG_IGN
 *
 *   3. set handlers for signals used by library
 *
 *   4. set handlers for signals used by the daemon.
 *
 * This is done once, and once only, early in the morning.
 */
extern void
signal_init (struct thread_master *m, int sigc,
             struct quagga_signal_t signals[])
{
  int i ;

  /* Set sig_max by experiment                                          */
  {
    sigset_t trial[1] ;
    int      signo ;

    sigemptyset(trial) ;
    for (signo = sig_min ; signo <= SIG_COUNT ; ++signo)
      {
        if (sigaddset(trial, signo) < 0)
          break ;
      } ;

    --signo ;           /* last acceptable signo        */
    if ((signo < sig_min) || (signo > SIG_MAX))
      zabort("cannot establish reasonable 'sig_max'") ;

    sig_max = signo ;
  } ;

  /* Construct the standard sets of signals.                            */
  sigmakeset(hard_signals, SIGABRT, SIGBUS, SIGFPE, SIGILL, SIGKILL,
                           SIGSEGV, SIGSTOP, SIGXCPU, SIGXFSZ,
                           SIGSYS,
                           SIGEMT, SIGIOT, SIGXRES,
                           -1) ;

  sigcopyset(core_signals, hard_signals) ;
  sigaddset(core_signals, SIGQUIT) ;
  sigdelset(core_signals, SIGKILL) ;
  sigdelset(core_signals, SIGSTOP) ;

  sigmakeset(exit_signals, SIGALRM, SIGHUP, SIGINT, SIGTERM, SIGUSR1, SIGUSR2,
                           SIGIO, SIGPOLL, SIGPROF, SIGPWR, SIGSTKFLT,
                           SIGVTALRM,
                           -1) ;

  sigmakeset(ignore_signals, SIGCHLD, SIGCONT, SIGPIPE, SIGTSTP, SIGTTIN,
                             SIGTTOU, SIGURG, SIGWINCH,
                           -1) ;

  /* Initialise the sig_master, qsig_signals and qsig_interrupts.
   *
   * Reserve all the hard_signals.
   */
  memset(&qsig_master, 0, sizeof(qsig_master)) ;

  sigemptyset(qsig_signals) ;
  sigemptyset(qsig_interrupts) ;
  sigcopyset(qsig_reserved, hard_signals) ;

  /* The signals used by the library.
   *
   * Added to qsig_signals or qsig_interrupts and to qsig_reserved.
   */
  qsig_add(SIGCHLD, vty_sigchld) ;
  qsig_add(SIG_INTERRUPT, NULL) ;

  /* Now collect the daemon's own signals.
   *
   * Added to qsig_signals or qsig_interrupts and to qsig_reserved.
   */
  for (i = 0 ; i < sigc ; ++i)
    qsig_add(signals[i].signal, signals[i].handler) ;

  /* In case signals with different names are the same, and to make the
   * required reservations, let qsig_reserved take precedence over exit_signals
   * and those take precedence over ignore_signals.
   */
  sigsubsets(exit_signals,   qsig_reserved) ;
  sigsubsets(ignore_signals, qsig_reserved) ;
  sigsubsets(ignore_signals, exit_signals) ;

  /* Also, remove anything from core_signals which is now qsig_signals or
   * qsig_interrupts (possibly SIGQUIT !).
   */
  sigsubsets(core_signals, qsig_signals) ;
  sigsubsets(core_signals, qsig_interrupts) ;

  /* Install handlers                                                   */
  signal_set_set(core_signals,    core_handler, false) ;
  signal_set_set(exit_signals,    exit_handler, false) ;
  signal_set_set(ignore_signals,  NULL, false) ;
  signal_set_set(qsig_signals,    quagga_signal_handler, true) ;
  signal_set_set(qsig_interrupts, quagga_interrupt_handler, true) ;

  /* If using a timer thread to scan for signal events, start that now.
   */
#ifdef SIGEVENT_SCHEDULE_THREAD
  sig_master.t =
    thread_add_timer (m, quagga_signal_timer, &sig_master,
                      QUAGGA_SIGNAL_TIMER_INTERVAL);
#endif /* SIGEVENT_SCHEDULE_THREAD */

  /* Signals are now initialised                                        */
  signals_initialised = true ;
} ;

/*------------------------------------------------------------------------------
 * Get the hard_signals set
 */
extern const sigset_t*
signal_get_hard_set(void)
{
  assert(signals_initialised) ;
  return hard_signals ;
} ;

/*------------------------------------------------------------------------------
 * Add signal to those to be caught either for quagga_sigevent_process() or
 * to then be dropped (so called interrupt signals).
 *
 * Checks that signal is not amongst the reserved signals.
 *
 * Add signal to the reserved signals.
 */
static void
qsig_add(int signo, qsig_event* event)
{
  int s ;

  s = sigismember(qsig_reserved, signo) ;
  if ((s < 0) || (signo > sig_max))
    zabort("invalid or unknown signal number") ;
  if (s > 0)
    zabort("signal is reserved (or already set)") ;

  sigaddset(qsig_reserved, signo) ;

  if (event == NULL)
    sigaddset(qsig_interrupts, signo) ;
  else
    {
      sigaddset(qsig_signals, signo) ;

      if (qsig_master.qsigc >= qso_count)
        zabort("too many signals to be caught") ;

      ++qsig_master.qsigc ;

      qsig_master.map[signo] = qsig_master.qsigc ;
      qsig_master.event[qsig_master.qsigc] = event ;

    } ;
} ;

/*==============================================================================
 * The event level handling of qsig_signals, delivered via qsig_master.
 */

/*------------------------------------------------------------------------------
 * check if signals have been caught and run respective event functions
 *
 * Returns: 0 => nothing to do
 *         -1 => failed
 *        > 0 => done this many signals
 */
extern int
quagga_sigevent_process (void)
{
  int i;
  int done ;
#ifdef SIGEVENT_BLOCK_SIGNALS
  /* shouldnt need to block signals, but potentially may be needed */
  sigset_t newmask, oldmask;

  /*
   * Block most signals, but be careful not to defer SIGTRAP because
   * doing so breaks gdb, at least on NetBSD 2.0.  Avoid asking to
   * block SIGKILL, just because we shouldn't be able to do so.
   */
  sigfillset (&newmask);
  sigdelset (&newmask, SIGTRAP);
  sigdelset (&newmask, SIGKILL);

  if ( (sigprocmask (SIG_BLOCK, &newmask, &oldmask)) < 0)
    {
      zlog_err ("quagga_signal_timer: couldnt block signals!");
      return -1;
    }
#endif /* SIGEVENT_BLOCK_SIGNALS */

  done = 0 ;
  if (qsig_master.caught[qso_null] != 0)
    {
      qsig_master.caught[qso_null] = 0;
      /* must not read or set sigmaster.caught[0] after here,
       * race condition with per-sig caught flags if one does
       */
      for (i = 1 ; i <= qsig_master.qsigc ; i++)
        {
          if (qsig_master.caught[i] != 0)
            {
              qsig_master.caught[i] = 0;
              (qsig_master.event[i])() ;
              ++done ;
            }
        }
    }

#ifdef SIGEVENT_BLOCK_SIGNALS
  if ( sigprocmask (SIG_UNBLOCK, &oldmask, NULL) < 0 );
    return -1;
#endif /* SIGEVENT_BLOCK_SIGNALS */

  return done ;
}

/*------------------------------------------------------------------------------
 * Optional timer thread to poll for signals
 */
#ifdef SIGEVENT_SCHEDULE_THREAD
/* timer thread to check signals. Shouldnt be needed */
int
quagga_signal_timer (struct thread *t)
{
  struct quagga_sigevent_master_t *sigm;
  struct quagga_signal_t *sig;
  int i;

  sigm = THREAD_ARG (t);
  sigm->t = thread_add_timer (sigm->t->master, quagga_signal_timer, &sigmaster,
                              QUAGGA_SIGNAL_TIMER_INTERVAL);
  return quagga_sigevent_process ();
}
#endif /* SIGEVENT_SCHEDULE_THREAD */

/*==============================================================================
 * The signal handlers.
 */
static void * program_counter(void *context) ;

/*------------------------------------------------------------------------------
 * Terminate + Core
 */
static void __attribute__ ((noreturn))
core_handler(int signo, siginfo_t *info, void *context)
{
  zlog_signal(signo, "aborting...", info, program_counter(context)) ;
  zabort_abort();
}

/*------------------------------------------------------------------------------
 * Terminate
 */
static void __attribute__ ((noreturn))
exit_handler(int signo, siginfo_t* info, void* context)
{
  zlog_signal(signo, "exiting...", info, program_counter(context));
  _exit(128+signo);
}

/*------------------------------------------------------------------------------
 * Generic signal handler -- captures signals in the sig_master caught vector.
 *
 * quagga_sigevent_process() deals with the caught signals.
 */
static void
quagga_signal_handler(int signo, siginfo_t* info, void* context)
{
  qs_ord_t qso ;

  if (sigismember(qsig_signals, signo) <= 0)
    {
      zlog_signal(signo, "quagga_signal_handler: unknown or invalid signal",
                                               info, program_counter(context)) ;
      zabort_abort();
    } ;

  qso = qsig_master.map[signo] ;

  /* Set individual caught flag before composite.
   *
   * This works if quagga_signal_handler() and quagga_sigevent_process()
   * were to run at the same time -- unlikely though that may be.
   *
   * If quagga_sigevent_process() sees individual flag before the composite,
   * that's fine -- worst that happens is will run through a further time
   * and not find anything.
   *
   * If flags were set in the opposite order, could clear the composite
   * and miss the individual, so lose the signal till the next time the
   * composite is set.
   */
  qsig_master.caught[qso]      = 1 ;
  qsig_master.caught[qso_null] = 1 ;
} ;

/*------------------------------------------------------------------------------
 * Generic signal handler -- where signal is to be caught, and immediately
 * dropped.
 */
static void
quagga_interrupt_handler(int signo, siginfo_t* info, void* context)
{
  if (sigismember(qsig_interrupts, signo) <= 0)
    {
      zlog_signal(signo, "quagga_interrupt_handler: unknown or invalid signal",
                                               info, program_counter(context)) ;
      zabort_abort();
    } ;
} ;

/*------------------------------------------------------------------------------
 * Extract program counter from context.
 *
 * XXX This function should be enhanced to support more platforms
 *     (it currently works only on Linux/x86).
 */
static void *
program_counter(void *context)
{
#ifdef HAVE_UCONTEXT_H
# ifdef GNU_LINUX
#  ifdef REG_EIP
  if (context)
    return (void *)(((ucontext_t *)context)->uc_mcontext.gregs[REG_EIP]);
#  endif /* REG_EIP */
#  ifdef REG_RIP
  if (context)
    return (void *)(((ucontext_t *)context)->uc_mcontext.gregs[REG_RIP]);
#  endif /* REG_RIP */
# endif /* GNU_LINUX */
#endif /* HAVE_UCONTEXT_H */
  return NULL;
} ;

/*==============================================================================
 * Signal clearing for abort() and fork()/vfork().
 */

/*------------------------------------------------------------------------------
 * Set default sigaction for given signo
 */
static int
sigaction_set_default(int signo)
{
  struct sigaction act[1] ;

  memset(act, 0, sizeof(act)) ; /* inter alia, clear sa_flags   */
  act->sa_handler = SIG_DFL ;   /* return to default state      */
  sigemptyset(&act->sa_mask) ;  /* no extra masking             */

  return sigaction(signo, act, NULL) ;
} ;

/*------------------------------------------------------------------------------
 * When finally aborting, need to turn off the handling of SIGABRT, and need
 * to make sure that the signal is not blocked.
 */
extern void
quagga_sigabrt_no_trap(void)
{
  sigset_t set[1] ;

  sigaction_set_default(SIGABRT) ;

  sigemptyset(set) ;
  sigaddset(set, SIGABRT) ;
  qpt_thread_sigmask(SIG_UNBLOCK, set, NULL) ;
                               /* sigprocmask() if !qpthreads_enabled   */
} ;

/*------------------------------------------------------------------------------
 * Having forked, make sure that all signals are in default state and that
 * no signals are blocked.
 *
 * Expects not to fail.
 */
extern void
quagga_signal_reset(void)
{
  sigset_t set[1] ;
  int      signo ;

  /* Before changing the handling of any signals, mask everything and
   * clear out any pending signals.
   */
  sigfillset(set) ;
  sigprocmask(SIG_SETMASK, set, NULL) ;

  while (1)
    {
      sigpending(set) ;
      if (sighasmember(set) == 0)
        break ;
      sigwait(set, &signo) ;
    } ;

  /* Set all signals to default handler.                                */
  for (signo = sig_min ; signo <= sig_max ; ++signo)
    {
      if ((signo == SIGKILL) || (signo == SIGSTOP))
        continue ;

      sigaction_set_default(signo) ;
    } ;

  /* Unmask everything                                                  */
  sigemptyset(set) ;
  sigprocmask(SIG_SETMASK, set, NULL) ;
} ;

/*==============================================================================
 * Functions to install signal handlers.
 */

/*------------------------------------------------------------------------------
 * Set given handler for given set of signals.  NULL handler => SIG_IGN.
 *
 * Returns:  < 0 => failed  -- value is - failing signo !
 */
static int
signal_set_set(sigset_t* set, sig_handler* handler, bool required)
{
  int     signo ;

  signo = 0 ;
  for (signo = sig_min ; signo <= sig_max ; ++signo)
    {
      int s ;
      s = sigismember(set, signo) ;
      if (s < 0)
        break ;
      if (s > 0)
        if (signal_set(signo, handler, required) < 0)
          return -signo ;
    } ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set given handler for given signal.  NULL handler => SIG_IGN.
 *
 * Returns:  < 0 => failed
 */
#ifndef  SA_INTERRUPT
# define SA_INTERRUPT 0
#endif
#ifndef  SA_RESTART
# define SA_RESTART   0
#endif

static int
signal_set(int signo, sig_handler* handler, bool required)
{
  struct sigaction act[1] ;

  /* If a signal handler is already set for the given signal then we leave
   * that as it is -- unless this is one of the "required" signals, used by
   * Quagga.
   */
  sigaction(signo, NULL, act) ;

  if ((act->sa_handler != SIG_DFL) && (act->sa_handler != SIG_IGN))
    {
      if (!required)
        return 0 ;
    } ;

  /* Set our handler                                                    */

  memset(act, 0, sizeof(struct sigaction)) ;

  if (handler == NULL)
    {
      act->sa_handler   = SIG_IGN ;
      act->sa_flags     = 0 ;
    }
  else
    {
      act->sa_sigaction = handler ;
      act->sa_flags     = SA_SIGINFO ;
    } ;

  sigfillset (&act->sa_mask) ;          /* mask everything              */

  if (signo == SIGALRM)
    act->sa_flags |= SA_INTERRUPT ;     /* want SIGALRM to interrupt    */
  else
    act->sa_flags |= SA_RESTART ;       /* all others want restart      */

  act->sa_flags |= SA_NOCLDSTOP ;

  return sigaction (signo, act, NULL) ;
} ;

/*==============================================================================
 * Additional signal set support.
 */

/*------------------------------------------------------------------------------
 * Make a signal set.
 *
 * Takes variable list of signal number arguments:
 *
 *   * ignores zeros
 *
 *   * stops on first value < 0
 */
extern void
sigmakeset(sigset_t* set, ...)
{
  va_list va ;
  int     signo ;

  va_start(va, set) ;

  sigemptyset(set) ;
  while ((signo = va_arg(va, int)) >= 0)
    {
      if (signo != 0)
        if (sigaddset(set, signo) < 0)
          zabort("invalid signal number") ;
    } ;

  va_end(va) ;
} ;

/*------------------------------------------------------------------------------
 * Copy a signal set.
 */
extern void
sigcopyset(sigset_t* dst, const sigset_t* src)
{
  *dst = *src ;
} ;

/*------------------------------------------------------------------------------
 * Add signal set 'b' into set 'a'.
 */
extern void
sigaddsets(sigset_t* a, const sigset_t* b)
{
  int     signo ;

  for (signo = sig_min ; signo < SIG_MAX ; ++signo)
    {
      int s ;
      s = sigismember(b, signo) ;
      if (s < 0)
        break ;
      if (s > 0)
        sigaddset(a, signo) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Subtract signal set 'b' from set 'a'.
 */
extern void
sigsubsets(sigset_t* a, const sigset_t* b)
{
  int     signo ;

  for (signo = sig_min ; signo < SIG_MAX ; ++signo)
    {
      int s ;
      s = sigismember(b, signo) ;
      if (s < 0)
        break ;
      if (s > 0)
        sigdelset(a, signo) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Make set 'a' be the inverse of set 'b'
 */
extern void
siginvset(sigset_t* a, const sigset_t* b)
{
  int     signo ;

  sigfillset(a) ;

  for (signo = sig_min ; signo < SIG_MAX ; ++signo)
    {
      int s ;
      s = sigismember(b, signo) ;
      if (s < 0)
        break ;
      if (s > 0)
        sigdelset(a, signo) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * See if there is any intersection between two sets.
 *
 * Returns:  first signo of intersection -- may be more !
 *           0 <=> none
 */
extern int
sigincommon(const sigset_t* a, const sigset_t* b)
{
  int     signo ;

  for (signo = sig_min ; signo < SIG_MAX ; ++signo)
    {
      int s ;
      s = sigismember(a, signo) ;
      if (s < 0)
        return 0 ;
      if ((s > 0) && (sigismember(b, signo) > 0))
        return signo ;
    } ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * See if there is anything in the given set.
 *
 * Returns:  first signo found -- may be more !
 *           0 <=> none
 */
extern int
sighasmember(const sigset_t* a)
{
  int     signo ;

  for (signo = sig_min ; signo < SIG_MAX ; ++signo)
    {
      int s ;
      s = sigismember(a, signo) ;
      if (s < 0)
        return 0 ;
      if (s > 0)
        return signo ;
    } ;

  return 0 ;
} ;

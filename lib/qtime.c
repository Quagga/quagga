/* Quagga realtime and monotonic clock handling -- functions
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

#include <sys/times.h>
#include <errno.h>

#include "qtime.h"
#include "qfstring.h"
#include "pthread_safe.h"
#include "log.h"

/*==============================================================================
 * This is a collection of functions and (in qtime.h) macros and inline
 * functions which support system time and a monotonic clock.
 *
 * TODO: introduce mutex for crafted monotonic time, and initialisation
 *       routine for that: which can preset the various variables... but
 *       unless is guaranteed to be called, must leave the on-the-fly
 *       initialisation...  could also start a watchdog at that point.
 */

/*==============================================================================
 * Replacement for CLOCK_MONOTONIC.
 *
 * With thanks to Joakim Tjernlund for reminding everyone of the return value
 * from times() !
 *
 * times() is defined to return a value which is the time since some fixed time
 * before the application started (or when the application started).  This time
 * is measured in units of sysconf(_SC_CLK_TCK) ticks per second.
 *
 * The only tricky bit is that the value returned (of type clock_t) is a
 * signed integer, which can overflow.  It is not defined exactly how it
 * does this... This code assumes that the system will wrap around in some
 * obvious way.  The base of the time for this clock may be when the *system*
 * started... so when it overflows may depend on how long the system has been
 * up... which suggests that some sensible wrap around is likely (?).
 *
 * The qtime_t value is in nano-seconds.
 *
 * The result from times() is in units of sysconf(_SC_CLK_TCK) ticks per second.
 *
 * If clock_t is a signed 32-bit integer, which is kept +ve, then the clock
 * overflows/wraps round in 2^31 ticks which is:
 *
 *   at     100 ticks/sec: > 248 days
 *   at   1,000 ticks/sec: >  24 days
 *   at  10,000 ticks/sec: >  59 hours
 *
 * For safety, this asserts that sysconf(_SC_CLK_TCK) <= 1,000,000 for
 * sizeof(clock_t) > 4, but <= 1,000 for sizeof(clock_t) == 4.
 *
 * (It appears that 60, 100, 250 and 1,000 ticks/sec. are popular options.)
 *
 * If sizeof(clock_t) > 4, it is assumed large enough never to wrap around.
 * (But seems unlikely that such a system would not support CLOCK_MONOTONIC !)
 *
 * When clock_t is a 32-bit integer must be at least ready for wrap around.
 * We take the clock_t signed values and widen to 64-bit signed, so we have
 * the current sample (this) and the previous one (last), and two cases to
 * consider:
 *
 *   * +ve wrap around -- so value is 31-bit unsigned, and wraps from large
 *                        +ve value to small +ve value.
 *
 *       step = this - last   will be -ve
 *
 *     'last' will be some value ((INT32_MAX + 1) - x), and 'this' will be some
 *     (relatively) small value y.  The step is x + y, we have:
 *
 *       step = y - ((INT32_MAX + 1) - x) = (x + y) - (INT32_MAX + 1)
 *
 *     so we correct by adding (INT32_MAX + 1).
 *
 *   * -ve wrap around -- so value is 32-bit signed, and wraps from a large
 *                        +ve value to a very -ve value.
 *
 *       step = this - last   will be -ve
 *
 *     'last will' be some value (INT32_MAX + 1) - x, and 'this' will be some
 *     value (y - (INT32_MAX + 1)).  The step is x + y, we have:
 *
 *       step = (y - (INT32_MAX + 1)) - ((INT32_MAX + 1) - x)
 *            = (x + y) - 2 * (INT32_MAX + 1)
 *
 *     so we correct by adding (INT32_MAX + 1).
 *
 * In both cases the wrap around gives an apparently -ve 'step', and that is
 * corrected by adding (INT32_MAX + 1) until it goes +ve.
 *
 * In any event, a step > 24 hours is taken to means that something has gone
 * very, very badly wrong.
 *
 * NB: it is assumed that qt_craft_monotonic will be called often enough to
 *     ensure that the check on the step size will not be triggered !
 *
 * NB: it is assumed that times() does not simply stick if it overflows.
 *
 * TODO: Add a watchdog to monitor the behaviour of this clock ?
 */

CONFIRM((sizeof(clock_t) >= 4) && (sizeof(clock_t) <= 8)) ;

#ifdef GNU_LINUX
#define TIMES_TAKES_NULL 1
#else
#define TIMES_TAKES_NULL 0
#endif

static int64_t monotonic          = 0 ; /* monotonic clock in _SC_CLK_TCK's */
static int64_t last_times_sample  = 0 ; /* last value returned by times()   */

static int64_t step_limit         = 0 ; /* for sanity check                 */

static int64_t times_clk_tcks     = 0 ; /* sysconf(_SC_CLK_TCK)             */
static qtime_t times_scale_q      = 0 ; /* 10**9 / times_clk_tcks           */
static qtime_t times_scale_r      = 0 ; /* 10**9 % times_clk_tcks           */

qtime_mono_t
qt_craft_monotonic(void) {
#if !TIMES_TAKES_NULL
  struct tms dummy[1] ;
#endif
  clock_t   this_times_sample ;

  /* Set up times_scale_q & times_scale_q if not yet done.              */
  if (times_clk_tcks == 0)      /* Is zero until it's initialized       */
    {
      lldiv_t qr ;
      confirm(sizeof(qtime_t) <= sizeof(long long int)) ;

      times_clk_tcks = sysconf(_SC_CLK_TCK) ;
      passert((times_clk_tcks > 0) &&
              (times_clk_tcks <= (sizeof(clock_t) > 4) ? 1000000
                                                       :    1000)) ;

      qr = lldiv(QTIME_SECOND, times_clk_tcks) ;
      times_scale_q = qr.quot ;
      times_scale_r = qr.rem ;

      step_limit = (int64_t)24 * 60 * 60 * times_clk_tcks ;
    } ;

  /* No errors are defined for times(), but a return of -1 is defined   */
  /* to indicate an error condition, with errno saying what it is !     */
  /*                                                                    */
  /* The following deals carefully with this -- cannot afford for the   */
  /* clock either to jump or to get stuck !                             */

#if TIMES_TAKES_NULL
# define TIMES_ARG  NULL
#else
# define TIMES_ARG  dummy
#endif

  this_times_sample = times(TIMES_ARG) ;
  if (this_times_sample == -1)            /* deal with theoretical error  */
    {
       errno = 0 ;
       this_times_sample = times(TIMES_ARG) ;
       if (errno != 0)
         zabort_errno("times() failed") ;
    } ;
#undef TIMES_ARG

  /* If clock_t is large enough, treat as monotonic (!).
   *
   * Otherwise calculate the difference between this sample and the
   * previous one -- the step.
   *
   * We do the sum in signed 64 bits, and the samples are signed 64 bits.
   */
  if (sizeof(clock_t) > 4)
    monotonic = this_times_sample ;
  else
    {
      int64_t step ;

      step = this_times_sample - last_times_sample ;

      while (step < 0)
        {
          /* If times() wraps unsigned, then result needs INT32_MAX + 1
           * adding to it to get to +ve result.
           *
           * If times() wraps signed, then result needs INT32_MAX + 1 adding
           * to it *twice*.
           */
          step += (uint64_t)INT32_MAX + 1 ;
        } ;

      if (step > step_limit)
        zabort("Sudden large monotonic clock jump") ;

      monotonic        += step ;
      last_times_sample = this_times_sample ;
    } ;

  /* Scale to qtime_t units.                                            */
  if (times_scale_r == 0)
    return monotonic * times_scale_q ;
  else
    return (monotonic * times_scale_q) +
                                ((monotonic * times_scale_r) / times_clk_tcks) ;
} ;

/*------------------------------------------------------------------------------
 * Get crafted monotonic time -- in seconds
 */
extern time_t
qt_craft_mono_secs(void)
{
  qt_craft_monotonic() ;        /* update the monotonic counter */

  return monotonic / times_clk_tcks ;
} ;

/*==============================================================================
 * A simple minded random number generator.
 *
 * Designed to be reasonably unpredictable... particularly the ms bits !
 */

static inline uint32_t
qt_rand(uint64_t q, uint64_t s)
{
  /* Takes q ^ s and reduces to 32 bits by xoring ms and ls halves
   * then uses Knuth recommended linear congruent to randomise that, so that
   * most of the original bits affect the result.
   *
   * Note that linear congruent tends to be "more random" in the ms bits.
   */
  q ^= s ;
  q = (q ^ (q >> 32)) & 0xFFFFFFFF ;
  return ((q * 2650845021) + 5) & 0xFFFFFFFF ;
} ;

extern uint32_t
qt_random(uint32_t seed)
{
  uint32_t x, y ;
  uint64_t t ;

  t = qt_get_realtime() ;

  /* Set x by munging the time, the address of x, the current contents of x,
   * and the "seed".  (Munge the seed a bit for good measure.)
   */
  x = qt_rand(t ^ (uint64_t)x ^ (uint64_t)&x, seed ^ 3141592653) ;
                  /* munge the address and the contents with the seed   */

  /* Set y by munging the time, the address of y, the current contents of y,
   * and the "seed".  (Munge the seed a bit for good measure.)
   */
  y = qt_rand(t ^ (uint64_t)y ^ (uint64_t)&y, seed ^ 3562951413) ;
                  /* munge the current real time with the seed          */

  /* Return x and y munged together.
   *
   * Note that we swap the halves of y before munging, in order to spread
   * the "more random" part of y down to the ls end of the result.
   */
  return x ^ ((y >> 16) & 0xFFFF) ^ ((y & 0xFFFF) << 16) ;
} ;

/*==============================================================================
 * Error handling
 */

/*------------------------------------------------------------------------------
 * clock_gettime() for the given clock_id has failed
 *
 * See: qt_clock_gettime()
 */
Private qtime_t
qt_clock_gettime_failed(clockid_t clock_id)
{
  int err = errno ;

  if (clock_id == CLOCK_REALTIME)
    zabort(qfs_gen("failed to get CLOCK_REALTIME: %s",
                                                      errtoa(err, 0).str).str) ;

#ifdef HAVE_CLOCK_MONOTONIC
  if (clock_id == CLOCK_MONOTONIC)
    zabort(qfs_gen("failed to get CLOCK_MONOTONIC: %s",
                                                      errtoa(err, 0).str).str) ;
#endif

  zlog_err("failed to clock_gettime(%d): %s", clock_id, errtoa(err, 0).str) ;

  return 0 ;
} ;

/*==============================================================================
 * Tracking the local timezone, so can:
 *
 *   a) rapidly convert clock_gettime(CLOCK_REALTIME, ...) times
 *
 *   b) do that thread-safe
 *
 *   c) do that async-signal-safe
 *
 * Assumptions:
 *
 *   a) that timezones are on at most 5 minute boundaries (15 probably!)
 *
 *   b) that DST changes are at least 60 days apart
 *
 *   c) that DST changes occur on times which are on 5 minute boundaries
 *      (60 probably -- but this means that the DST change is on a 5 minute
 *       bounderay in local and epoch times !)
 *
 * Sets up and maintains a table containing 8 entries:
 *
 *   [-3] previous - 2
 *   [-2] previous - 1
 *   [-1] previous     -- previous 0-7 days
 *   [ 0] current      -- current  0-7 days
 *   [+1] next         -- next     0-7 days
 *   [+2] next     + 1
 *   [+3] next     + 2
 *   [ X] sentinal
 *
 * These are configured before any threads or anything else very much runs, so
 * they are essentially static.  There is a "current index", which is set to
 * '0' to start with.
 *
 * Each entry comprises:
 *
 *   * start time      -- entry is valid for epoch times >= start
 *   * end time        -- entry is valid for epoch times <  end
 *   * offset          -- add to epoch time to get local
 *
 * When set up the current timezone initially starts on the nearest 5 minute
 * boundary in the past, and covers up to 7 days into the future, unless the
 * timezone changes in that time.  The timezones on either side are set
 * similarly.
 *
 * At most one of these timezones may be a short one -- so this covers at least
 * 14 days into the past and 21 into the future, and as time advances, upto
 * 21 days into the past and down to 14 days into the future.
 *
 * Maximum range is 56 days -- which is within the assumed 60 days between
 * DST changes.
 *
 * When time advances past the current entry the next, next + 1 and + 2 cover
 * at least 14 days (21 if none are short entries).
 *
 * Every now and then (say every 5 minutes) a background process can check the
 * current time.  If that is no longer in the current entry, needs to update
 * the table.  Assuming time is moving forward: sets sentinal to be the next
 * 0-7 days following the current last entry, and updates the "current index".
 *
 * BIG ASSUMPTION: that the "current index" value is written atomically, wrt
 *                 to threads as well as signals.
 *
 *                 It doesn't matter if a thread or signal action code picks
 *                 up an out of date "current index" value, because all the
 *                 entries for the old state are still valid.
 *
 *                 No entry is changed while it is covered by the current
 *                 index -3..+3.
 *
 * This works fine, UNLESS the clock_gettime(CLOCK_REALTIME, ...) changes
 * dramatically -- as might happen if the operator adjusts the system clock a
 * long way !
 *
 * To cope with this, a spare set of 8 entries are kept, and a new table can
 * be built (under mutex).  The worst that happens is that threads may be
 * blocked waiting for the table to be updated.
 *
 * If the table is found to be out of date when a signal is bringing the
 * system down, then the times logged will just have to use either the first
 * or the last entry, and have done with it.
 */


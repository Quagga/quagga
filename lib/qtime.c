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

#include <sys/times.h>
#include <errno.h>

#include "zassert.h"
#include "qtime.h"

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
 *
 * When clock_t is a 32-bit integer must be at least ready for wrap around.
 * There are two cases:
 *
 *   * +ve wrap around.  new < old value, and new >= 0
 *
 *       step = (INT32_MAX - old + 1) + new
 *
 *   * -ve wrap around.  new < old value, and new <  0 (and old > 0)
 *
 *       step = (INT32_MAX - old + 1) - (INT32_MIN - new)
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
#undef  TIMES_TAKES_NULL
#endif

static uint64_t monotonic          = 0 ;  /* monotonic clock in _SC_CLK_TCK's */
static int64_t  last_times_sample  = 0 ;  /* last value returned by times()   */

static uint64_t step_limit         = 0 ;  /* for sanity check                 */

static int64_t  times_clk_tcks     = 0 ;  /* sysconf(_SC_CLK_TCK)             */
static qtime_t  times_scale_q      = 0 ;  /* 10**9 / times_clk_tcks           */
static qtime_t  times_scale_r      = 0 ;  /* 10**9 % times_clk_tcks           */

qtime_mono_t
qt_craft_monotonic(void) {
  struct tms dummy ;
  int64_t    this_times_sample ;
  uint64_t   step ;

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

      step_limit = (uint64_t)24 * 60 * 60 * times_clk_tcks ;
    } ;

  /* No errors are defined for times(), but a return of -1 is defined   */
  /* to indicate an error condition, with errno saying what it is !     */
  /*                                                                    */
  /* The following deals carefully with this -- cannot afford for the   */
  /* clock either to jump or to get stuck !                             */

#ifdef TIMES_TAKES_NULL
  this_times_sample = times(NULL) ;       /* assume this saves effort !   */
#else
  this_times_sample = times(&dummy) ;
#endif

  if (this_times_sample == -1)            /* deal with theoretical error  */
    {
       errno = 0 ;
       this_times_sample = times(&dummy) ;
       if (errno != 0)
         zabort_errno("times() failed") ;
    } ;

  /* Calculate the step and verify sensible.                            */
  /*                                                                    */
  /* Watch out for huge jumps and/or time going backwards.              */
  /* For 32-bit clock_t, look out for wrap-around.                      */

  if ((sizeof(clock_t) > 4) || (this_times_sample > last_times_sample))
    /* time going backwards will appear as HUGE step forwards.          */
    step = (uint64_t)(this_times_sample - last_times_sample) ;
  else
    {
      if (this_times_sample > 0)
        /* both samples +ve => +ve wrap around.                         */
        step = (uint64_t)( ((int64_t)INT32_MAX - last_times_sample + 1)
                                                        + this_times_sample  ) ;
      else
        /* this sample -ve and last sample +ve => -ve wrap round        */
        /* this sample -ve and last sample -ve => time gone backwards   */
        /*                     (which appears as a HUGE step forwards). */
        step = (uint64_t)( ((int64_t)INT32_MAX - last_times_sample + 1)
                                  - ((int64_t)INT32_MIN - this_times_sample) ) ;
    } ;

  /*  TODO: better error messaging for large clock jumps.               */
  if (step > step_limit)
    zabort("Sudden large monotonic clock jump") ;

  /* Advance the monotonic clock in sysconf(_SC_CLK_TCK) units.         */
  monotonic += step ;

  /* Remember what we got, for next time.                               */
  last_times_sample = this_times_sample ;

  /* Scale to qtime_t units.                                            */
  if (times_scale_r == 0)
    return monotonic * times_scale_q ;
  else
    return (monotonic * times_scale_q) +
                                ((monotonic * times_scale_r) / times_clk_tcks) ;
} ;

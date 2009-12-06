/* Quagga Realtime Clock handling -- functions
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
 * signed integer, which may wrap round.  It is not defined exactly how it
 * does this... but it is generally assumed that this_sample - last_sample will
 * give the time between the samples.
 *
 * The qtime_t value is in nano-seconds.
 *
 * The result from times() is in units of sysconf(_SC_CLK_TCK) ticks per second.
 *
 * NB: it is assumed that qt_craft_monotonic will be called often enough to
 *     ensure that it is not fooled by the clock wrapping round.
 *
 *     Assuming that clock_t is a signed 32-bit integer, which is kept +ve,
 *     then the clock wraps round in 2^31 ticks which is:
 *
 *       at     1,000 ticks/sec: > 24 days !
 *       at 1,000,000 ticks/sec: > 35 minutes
 *
 *     So this should be a safe assumption -- particularly as 60, 100, 250 and
 *     1000 ticks per second appear to be the popular options.
 *
 *     For safety, this asserts that sysconf(_SC_CLK_TCK) <= 1,000,000.
 */

#ifdef GNU_LINUX
#define TIMES_TAKES_NULL 1
#else
#undef  TIMES_TAKES_NULL
#endif

static uint64_t monotonic          = 0 ;  /* monotonic clock in _SC_CLK_TCK's */
static uint64_t last_times_sample  = 0 ;  /* last value returned by times()   */

static int64_t  times_clk_tcks     = 0 ;  /* sysconf(_SC_CLK_TCK)             */
static qtime_t  times_scale_q      = 0 ;  /* 10**9 / times_clk_tcks           */
static qtime_t  times_scale_r      = 0 ;  /* 10**9 % times_clk_tcks           */

qtime_t
qt_craft_monotonic(void) {
  struct tms dummy ;
  uint64_t   this_times_sample ;

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

  if (this_times_sample == (uint64_t)-1)  /* deal with theoretical error  */
    {
       errno = 0 ;
       this_times_sample = times(&dummy) ;
       if (errno != 0)
         zabort_errno("times() failed") ;
    } ;

  /* Advance the monotonic clock in sysconf(_SC_CLK_TCK) units.         */
  monotonic += (this_times_sample - last_times_sample) ;

  /* Set up times_scale_q & times_scale_q if not yet done               */
  if (times_clk_tcks == 0)      /* Is zero until it's initialized       */
    {
      lldiv_t qr ;
      confirm(sizeof(qtime_t) <= sizeof(long long int)) ;

      times_clk_tcks = sysconf(_SC_CLK_TCK) ;
      passert((times_clk_tcks > 0) && (times_clk_tcks <= 1000000)) ;

      qr = lldiv(QTIME_SECOND, times_clk_tcks) ;
      times_scale_q = qr.quot ;
      times_scale_r = qr.rem ;

      last_times_sample = this_times_sample ;
    } ;

  /* Scale to qtime_t units.                                            */
  if (times_scale_r == 0)
    return monotonic * times_scale_q ;
  else
    return (monotonic * times_scale_q) +
                                ((monotonic * times_scale_r) / times_clk_tcks) ;
} ;

/* Quagga realtime and monotonic clock handling -- header
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

#ifndef _ZEBRA_QTIME_H
#define _ZEBRA_QTIME_H

#include "misc.h"

#include <time.h>
#include <sys/time.h>
#include <unistd.h>

/*==============================================================================
 * qtime_t -- signed 64-bit integer.
 *
 * The various time functions work in terms of the structures:
 *
 *   timespec   -- tv_secs    seconds
 *                 tv_nsecs   nano-seconds
 *
 *   timeval    -- tv_secs    seconds
 *                 tv_usecs   micro-seconds
 *
 * Given a 64-bit integer it is much easier to do operations on a 64 bit
 * (signed) nano-second value.  That gives > 34 bits for the seconds count,
 * and counts from zero to > 290 years.
 */

typedef int64_t qtime_t ;

typedef qtime_t qtime_real_t ;  /* qtime_t value, realtime time-base      */
typedef qtime_t qtime_mono_t ;  /* qtime_t value, monotonic time-base     */

typedef qtime_t qtime_tod_t ;   /* qtime_t value, timeofday time-base...  */
                                /* ...just in case != CLOCK_REALTIME !    */

/* A qtime_t second       123456789 -- nano-seconds             */
#define QTIME_SECOND     1000000000
#define TIMESPEC_SECOND  1000000000
#define TIMEVAL_SECOND   1000000

/* Macro to convert time in seconds to a qtime_t                */
/* Note that the time to convert may be a float.                */
#define QTIME(s) ((qtime_t)((s) * (qtime_t)QTIME_SECOND))

/*==============================================================================
 * Conversion functions
 */
Inline qtime_t timespec2qtime(struct timespec* p_ts) ;
Inline qtime_t timeval2qtime(struct timeval* p_tv) ;
Inline struct timespec* qtime2timespec(struct timespec* p_ts, qtime_t qt) ;
Inline struct timeval* qtime2timeval(struct timeval* p_tv, qtime_t qt) ;

/*==============================================================================
 * Clocks.
 *
 * Here is support for:
 *
 *   * System Clock
 *
 *     This can be read using either clock_gettime(CLOCK_REALTIME, &ts) or
 *     gettimeofday(&tv, NULL) -- which (are believed to) return the same clock,
 *     but in different units.
 *
 *   * Monotonic Clock
 *
 *     Using clock_gettime(CLOCK_MONOTONIC, &ts) if it is available, otherwise
 *     a manufactured equivalent using times() -- see qt_craft_monotonic().
 */

Inline qtime_real_t qt_get_realtime(void) ;
                                /* clock_gettime(CLOCK_REALTIME, ...)   */
Inline qtime_mono_t qt_add_realtime(qtime_t interval) ;
                                /* qt_get_realtime() + interval         */

Inline qtime_mono_t qt_get_monotonic(void) ;
                                /* clock_gettime(CLOCK_MONOTONIC, ...)  */
                                /* OR equivalent using times()          */
Inline qtime_mono_t qt_add_monotonic(qtime_t interval) ;
                                /* qt_get_monotonic() + interval        */

/* These are provided just in case gettimeofday() != CLOCK_REALTIME     */
Inline qtime_tod_t qt_get_timeofday(void) ;
                                /* gettimeofday(&tv, NULL)              */
Inline qtime_tod_t qt_add_timeofday(qtime_t interval) ;
                                /* qt_get_timeofday() + interval        */

/*==============================================================================
 * Primitive random number generation.
 *
 * Uses time and other stuff to produce something which is not particularly
 * predictable.
 */
extern uint32_t qt_random(uint32_t seed) ;

/*==============================================================================
 * Inline conversion functions
 */

/*------------------------------------------------------------------------------
 * Convert timespec to qtime_t
 *
 * Returns qtime_t value.
 */
Inline qtime_t
timespec2qtime(struct timespec* p_ts)
{
  return QTIME(p_ts->tv_sec) + p_ts->tv_nsec ;
  confirm(QTIME_SECOND == TIMESPEC_SECOND) ;
} ;

/*------------------------------------------------------------------------------
 * Convert timeval to qtime_t
 *
 * Returns qtime_t value.
 */
Inline qtime_t
timeval2qtime(struct timeval* p_tv)
{
  return QTIME(p_tv->tv_sec) + (p_tv->tv_usec * 1000) ;
  confirm(QTIME_SECOND == TIMEVAL_SECOND      * 1000) ;
} ;

/*------------------------------------------------------------------------------
 * Convert qtime_t to timespec
 *
 * Takes address of struct timespec and returns that address.
 */
Inline struct timespec*
qtime2timespec(struct timespec* p_ts, qtime_t qt)
{
  lldiv_t imd = lldiv(qt, QTIME_SECOND) ;
  confirm(sizeof(long long) >= sizeof(qtime_t)) ;

  p_ts->tv_sec  = imd.quot ;
  p_ts->tv_nsec = imd.rem ;
  confirm(TIMESPEC_SECOND == QTIME_SECOND) ;

  return p_ts ;
} ;

/*------------------------------------------------------------------------------
 * Convert timespec to qtime_t
 *
 * Takes address of struct timespec and returns that address.
 */
Inline struct timeval*
qtime2timeval(struct timeval* p_tv, qtime_t qt)
{
  lldiv_t imd = lldiv(qt, QTIME_SECOND) ;
  confirm(sizeof(long long) >= sizeof(qtime_t)) ;

  p_tv->tv_sec  = imd.quot ;
  p_tv->tv_usec = imd.rem / 1000 ;
  confirm(TIMEVAL_SECOND  * 1000 == QTIME_SECOND) ;

  return p_tv ;
} ;

/*==============================================================================
 * Inline Clock Functions.
 */

/* Function to manufacture a monotonic clock.   */
Private qtime_mono_t qt_craft_monotonic(void) ;

/*------------------------------------------------------------------------------
 * Read given clock & return a qtime_t value.
 *
 * While possibility of error is essentially theoretical, must treat it as a
 * FATAL error -- cannot continue with broken time value !
 */

Inline qtime_t
qt_clock_gettime(clockid_t clock_id)
{
  struct timespec ts ;

  if (clock_gettime(clock_id, &ts) != 0)
    zabort_errno("clock_gettime failed") ;

  return timespec2qtime(&ts) ;
} ;

/*------------------------------------------------------------------------------
 * clock_gettime(CLOCK_REALTIME, ...) -- returning qtime_t value
 *
 * While possibility of error is essentially theoretical, must treat it as a
 * FATAL error -- cannot continue with broken time value !
 */
Inline qtime_real_t
qt_get_realtime(void)
{
  return qt_clock_gettime(CLOCK_REALTIME) ;
} ;

/*------------------------------------------------------------------------------
 * qt_get_realtime() + interval
 */
Inline qtime_real_t
qt_add_realtime(qtime_t interval)
{
  return qt_get_realtime() + interval;
} ;

/*------------------------------------------------------------------------------
 * clock_gettime(CLOCK_MONOTONIC, ...) OR qt_craft_monotonic()
 *                                                   -- returning qtime_t value
 *
 * While possibility of error is essentially theoretical, must treat it as a
 * FATAL error -- cannot continue with broken time value !
 */
Inline qtime_mono_t
qt_get_monotonic(void)
{
#ifdef HAVE_CLOCK_MONOTONIC
  return qt_clock_gettime(CLOCK_MONOTONIC) ;
#else
  return qt_craft_monotonic() ;
#endif
} ;

/*------------------------------------------------------------------------------
 * qt_get_monotonic() + interval
 */
Inline qtime_mono_t
qt_add_monotonic(qtime_t interval)
{
  return qt_get_monotonic() + interval;
} ;

/*------------------------------------------------------------------------------
 * gettimeofday(&tv, NULL) -- returning qtime_t value
 */
Inline qtime_tod_t
qt_get_timeofday(void)
{
  struct timeval tv ;
  gettimeofday(&tv, NULL) ;
  return timeval2qtime(&tv) ;
}

/*------------------------------------------------------------------------------
 * qt_get_timeofday() + interval
 */
Inline qtime_tod_t
qt_add_timeofday(qtime_t interval)
{
  return qt_get_timeofday() + interval;
} ;

#endif /* _ZEBRA_QTIME_H */

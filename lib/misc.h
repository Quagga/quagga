/* Miscellaneous basic definitions
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef _ZEBRA_MISC_H
#define _ZEBRA_MISC_H

/* "zconfig.h" is included at the start of this "misc.h", and at the start
 * of "zebra.h".  This ensures that we get <features.h> defined early, so
 * that all other #includes get the same set of features.
 */
#include "zconfig.h"

/* Stuff which we generally expect to have
*/
#include <string.h>
#include <limits.h>
#include <unistd.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <arpa/inet.h>          /* htons etc.           */

#include "confirm.h"
#include "zassert.h"

/*------------------------------------------------------------------------------
 * Odds and sods
 */

/* Bit number to bit mask
 */
#define BIT(b)  (1 << b)

/* The LS bit of a given value
 */
#define LS_BIT(v) ((v) ^ ((v) & ((v) - 1)))

/* The given value is a power of 2
 */
#define IS_POW_OF_2(v) (((v) & ((v) - 1)) == 0)

/* Just in case there are compiler issues
 */
#define Inline static inline

/* For things which have to be made extern -- typically because they are
 * used by an inline function -- but are not for public consumption.
 */
#define Private extern

/* For use in switch/case
 */
#define fall_through

/* always_inline -- where available !
 */
#ifdef __GNUC__
#define Always_Inline __attribute__((always_inline))
#endif

/* No Return -- where available
 */
#define NO_RETURN __attribute__ ((noreturn))

/*------------------------------------------------------------------------------
 * Various names for true/false pairs
 *
 * And noting that explicit cast to bool do what you would want.
 */
CONFIRM(((bool)99 == true) && ((bool)0 == false)) ;

enum on_off
{
  on   = true,
  off  = false
} ;
typedef enum on_off on_off_b ;

enum add
{
  add     = true,
  no_add  = false
} ;
typedef enum add add_b ;

enum free_keep
{
  free_it = true,
  keep_it = false
} ;
typedef enum free_keep free_keep_b ;

/*------------------------------------------------------------------------------
 * Various integer stuff
 *
 * We really want to be able to assume that an int is at least 32 bits
 * and that a long is at least 64 bits !  (And short is at least 16 bits.)
 */
CONFIRM(USHRT_MAX >= 0xFFFF) ;
CONFIRM(UINT_MAX  >= 0xFFFFFFFF) ;
CONFIRM(ULONG_MAX >= 0xFFFFFFFFFFFFFFFF) ;

/* Some useful shorthand                                                */
typedef unsigned char  byte ;
typedef unsigned char  uchar ;

typedef unsigned short ushort ;

typedef unsigned int   uint ;
typedef unsigned int   usize ;
typedef unsigned int   ulen ;

enum
{
  USIZE_MAX  = UINT_MAX,
  ULEN_MAX   = UINT_MAX,
} ;

typedef          int   ssize ;
typedef          int   slen ;

typedef unsigned long  ulong ;

typedef          long long llong ;
typedef unsigned long long ullong ;

/*       cvp  == const void*         -- ptr to constant void
 *       cvp* == void * const*       -- ptr to ptr to constant void
 * const cvp* == void const* const*  -- ptr to constant ptr to constant void
 */
typedef const void* cvp ;

/*------------------------------------------------------------------------------
 * htonq/ntohq -- sadly missing elsewhere
 */
union htonq_ntohq {
  uint64_t q ;
  struct {
    uint32_t ms ;
    uint32_t ls ;
  } l ;
} ;

CONFIRM( (sizeof(union htonq_ntohq) == 8)
                                 && (offsetof(union htonq_ntohq, l.ms) == 0)
                                 && (offsetof(union htonq_ntohq, l.ls) == 4) ) ;

#ifdef __GNUC__
Inline uint64_t htonq(uint64_t q) __attribute__((always_inline)) ;
Inline uint64_t ntohq(uint64_t q) __attribute__((always_inline)) ;
#endif

Inline uint64_t
htonq(uint64_t hq)
{
#ifdef __GNUC__
  #if   BYTE_ORDER == BIG_ENDIAN

  return q ;

  #elif BYTE_ORDER == LITTLE_ENDIAN

  return __builtin_bswap64(hq) ;

  #else
    #error BYTE_ORDER is neither BIG_ENDIAN nor LITTLE_ENDIAN !
  #endif
#else
  union htonq_ntohq t ;

  t.l.ms = htonl(hq >> 32) ;
  t.l.ls = htonl(hq      ) ;

  return t.q ;
#endif
} ;

Inline uint64_t
ntohq(uint64_t nq)
{
#ifdef __GNUC__
  #if   BYTE_ORDER == BIG_ENDIAN

  return q ;

  #elif BYTE_ORDER == LITTLE_ENDIAN

  return __builtin_bswap64(nq) ;

  #else
    #error BYTE_ORDER is neither BIG_ENDIAN nor LITTLE_ENDIAN !
  #endif
#else
  union htonq_ntohq t ;

  t.q = nq ;

  return ((uint64_t)ntohl(t.l.ms) << 32) | (uint64_t)ntohl(t.l.ls) ;
#endif
} ;

/*------------------------------------------------------------------------------
 * Macros for sexing value of compilation options.
 *
 * In particular allow a blank option to be treated as true, and a zero option
 * to be treated as false.
 *
 * NB: the option MUST be defined, and must be decimal numeric !!
 */
#define STRING_VALUE_INNER(x)  #x
#define STRING_VALUE(x) STRING_VALUE_INNER(x)

#define IS_BLANK_OPTION(x)     IS_BLANK_OPTION_INNER(x)
#define IS_ZERO_OPTION(x)      IS_ZERO_OPTION_INNER(x)
#define IS_NOT_ZERO_OPTION(x)  IS_NOT_OPTION_ZERO_INNER(x)

#define IS_BLANK_OPTION_INNER(x)    (1##x##1 ==  11)
#define IS_ZERO_OPTION_INNER(x)     (1##x##1 == 101)
#define IS_NOT_ZERO_OPTION_INNER(x) (1##x##1 != 101)

/* If QDEBUG is defined, make QDEBUG_NAME and set QDEBUG
 *
 *  Numeric value for QDEBUG: undefined      => 0
 *                            defined, blank => 1
 *                            defined, 0     => 0
 *                            defined, other => other
 *
 * Template for turning compilation option into a value.
 */
#ifdef QDEBUG
# if IS_BLANK_OPTION(QDEBUG)
#  undef  QDEBUG
#  define QDEBUG 1
# endif
#else
# define QDEBUG 0
#endif

enum { qdebug = QDEBUG } ;

#ifndef QDEBUG_NAME
# define QDEBUG_NAME STRING_VALUE(QDEBUG)
#endif

/*==============================================================================
 * Other useful functions
 */
typedef enum
{
  strtox_signed   = 1,          /* OK and '+' or '-' was present        */
  strtox_ok       = 0,          /* OK                                   */

  strtox_invalid  = -1,         /* badly formed number                  */
  strtox_range    = -2,         /* out of range                         */
} strtox_t ;

extern long  strtol_x(const char* restrict str, strtox_t* p_tox, char** endp) ;
extern ulong strtoul_x(const char* restrict str, strtox_t* p_tox, char** endp) ;
extern long  strtol_xr(const char* restrict str, strtox_t* p_tox, char** endp,
                                                                  long min,
                                                                  long max) ;
extern ulong strtoul_xr(const char* restrict str, strtox_t* p_tox, char** endp,
                                                                   ulong min,
                                                                   ulong max) ;

#endif /* _ZEBRA_MISC_H */

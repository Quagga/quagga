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

/* This is horrible... but for some purposes wish to turn *off* __USE_GNU.
 *
 * e.g: to persuade <string.h> to give POSIX version of strerror_r !!!
 */
#ifdef NO_USE_GNU
# undef  NO_USE_GNU
# ifdef  __USE_GNU
#  define NO_USE_GNU 1
#  undef __USE_GNU
# endif
#endif

/* Stuff which we generally expect to have                              */

#include <string.h>
#include <limits.h>
#include <unistd.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "confirm.h"
#include "zassert.h"

/* Bit number to bit mask                                               */
#define BIT(b)  (1 << b)

/* Just in case there are compiler issues                               */
#define Inline static inline

/* For things which have to be made extern -- typically because they are
 * used by an inline function -- but are not for public consumption.
 */
#define Private extern

/* For use in switch/case                                               */
#define fall_through

/* Other names of true/false                                            */
enum on_off
{
  on   = true,
  off  = false
} ;
typedef enum on_off on_off_b ;

/* Whether to add or not on lookup.                                     */
enum add
{
  add     = true,
  no_add  = false
} ;
typedef enum add add_b ;

/* Used in object "reset" functions (destructors)                       */
enum free_keep
{
  free_it = true,
  keep_it = false
} ;
typedef enum free_keep free_keep_b ;

/* We really want to be able to assume that an int is at least 32 bits
 * and that a long is at least 64 bits !
 */
CONFIRM(UINT_MAX  >= 0xFFFFFFFF) ;
CONFIRM(ULONG_MAX >= 0xFFFFFFFFFFFFFFFF) ;

/* Some useful shorthand                                                */
typedef unsigned char byte ;
typedef unsigned char uchar ;

typedef unsigned int  uint ;
typedef unsigned int  usize ;
typedef unsigned int  ulen ;

typedef          int  ssize ;
typedef          int  slen ;

typedef unsigned long ulong ;

/*       cvp  == const void*         -- ptr to constant void
 *       cvp* == void * const*       -- ptr to ptr to constant void
 * const cvp* == void const* const*  -- ptr to constant ptr to constant void
 */
typedef const void* cvp ;

/* Macros for sexing value of compilation options.
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
 *  Numeric value for QDEBUG:  undefined      => 0
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

#endif /* _ZEBRA_MISC_H */

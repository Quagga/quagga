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

/* Stuff which we generally expect to have                              */
#include <stddef.h>
#include <stdint.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "zassert.h"

/* Bit number to bit mask                                               */
#define BIT(b)  (1 << b)

/* Just in case there are compiler issues                               */
#define Inline static inline

/* For things which have to be made extern -- typically because they are
 * used by an inline function -- but are not for public consumption.
 */
#define Private extern

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

/* We really want to be able to assume that an int is at least 32 bits  */
CONFIRM(UINT_MAX >= 0xFFFFFFFF) ;

/* Some useful shorthand                                                */
typedef unsigned char byte ;
typedef unsigned char uchar ;
typedef unsigned int  uint ;
typedef unsigned int  usize ;
typedef unsigned int  ulen ;

#endif /* _ZEBRA_MISC_H */

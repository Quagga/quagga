/* Temporary string handling -- header
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

#ifndef _ZEBRA_TSTRING_H
#define _ZEBRA_TSTRING_H

#include "misc.h"
#include "zassert.h"
#include "memory.h"

/*==============================================================================
 * tstrings are allocated on the stack, but if (unexpectedly) the standard
 * size is not enough, then they can be allocated dynamically.
 *
 * To declare a "tstring":
 *
 *   tstring(foo, 64) ;   // creates a "tstring" variable called "foo"
 *                        // with 64 char buffer.
 *
 * Can then:
 *
 *   s = tstring_set_len(foo, n) ;  // ensures have buffer for n+1 chars
 *   s = tstring_set(foo, "...") ;  // copies "..." (with '\0') to buffer
 *   s = tstring_set_n(foo, q, n) ; // copies n characters from q to buffer
 *                                     and '\0' terminates
 *
 * If can fit stuff in the buffer, will do so.  Otherwise will allocate an
 * MTYPE_TMP buffer to work in.
 *
 * And before leaving the scope of "foo" must:
 *
 *   tstring_free(foo) ;    // releases any dynamically allocated memory.
 */

struct tstring
{
  usize   size ;
  char*   str ;
  char*   alloc ;
} ;

typedef struct tstring tstring[1] ;

/* tstring(foo, 93) ;   -- declare the variable "foo".          */
#define tstring_t(name, sz) \
  char      _zlxq_##name##_b[ ((sz) + 7) & 0xFFFFFFF8] ; \
  tstring name = { { .size  = ((sz) + 7) & 0xFFFFFFF8, \
                                      .str   = _zlxq_##name##_b, \
                                      .alloc = NULL } }

/*------------------------------------------------------------------------------
 * Ensure the tstring "foo" can accomodate at least "len" characters plus the
 * terminating '\0'.
 *
 * Returns: address of buffer
 *
 * NB: address of buffer may not be the same as returned by a previous operation
 *     on foo.  Also, previous contents of foo may be lost.
 */
Inline char*
tstring_set_len(struct tstring* ts, usize len)
{
  if (len >= ts->size)
    {
      ts->size = len + 1 ;
      ts->str  = ts->alloc = XREALLOC(MTYPE_TMP, ts->alloc, len + 1) ;
    } ;

  return ts->str ;
} ;

/*------------------------------------------------------------------------------
 * Copy "len" characters from "src" to the tstring "foo", and append a
 * terminating '\0'.
 *
 * The "src" address is ignored if "len" == 0 (sets "foo" to be empty string).
 *
 * Returns: address of buffer
 *
 * NB: address of buffer may not be the same as returned by a previous operation
 *     on foo.  Also, previous contents of foo may be lost.
 */
static inline char*
tstring_set_n(struct tstring* ts, const char* str, usize len)
{
  char* tss = tstring_set_len(ts, len) ;

  if (len > 0)
    memcpy(tss, str, len) ;
  *(tss + len) = '\0' ;

  return tss ;
} ;

/*------------------------------------------------------------------------------
 * Copy the string "str" to the tstring "foo", with terminating '\0'.
 *
 * If "str" is NULL, sets "foo" to be an empty string.
 *
 * Returns: address of buffer
 *
 * NB: address of buffer may not be the same as returned by a previous operation
 *     on foo.  Also, previous contents of foo may be lost.
 */
static inline char*
tstring_set(struct tstring* ts, const char* str)
{
  return tstring_set_n(ts, str, (str != NULL) ? strlen(str) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * If have dynamically allocated buffer for tstring "foo", release it now.
 */
static inline void
tstring_free(struct tstring* ts)
{
  if (ts->alloc != NULL)
    XFREE(MTYPE_TMP, ts->alloc) ;       /* sets ts->alloc NULL  */
} ;

#endif /* _ZEBRA_TSTRING_H */

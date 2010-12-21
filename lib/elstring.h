/* Length/String string handling -- header
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

#ifndef _ZEBRA_ELSTRING_H
#define _ZEBRA_ELSTRING_H

#include "misc.h"
#include "zassert.h"
#include "memory.h"

/*==============================================================================
 * This is some very simple support for strings which are Length/Body
 * objects.
 *
 * NB: this object knows NOTHING about memory allocation etc.  The objective
 *     is the simplest possible encapsulation of strings which are NOT '\0'
 *     terminated.
 *
 *
 *
 */
struct elstring
{
  union
  {
    void*       v ;     /* may be NULL iff len == 0             */
    const void* cv ;
  } body ;

  ulen  len ;
  bool  term ;          /* true <=> body is '\0' terminated     */
} ;

typedef struct elstring  elstring_t[1] ;
typedef struct elstring* elstring ;

/* Setting an elstring object to all zeros is enough to initialise it to
 * an empty string.
 */
enum
{
  ELSTRING_INIT_ALL_ZEROS = true
} ;

/*------------------------------------------------------------------------------
 * Various forms of body -- NB:
 */

Inline void*
els_body_nn(elstring els)
{
  return els->body.v ;
} ;

Inline void*
els_body(elstring els)
{
  return (els != NULL) ? els_body_nn(els)  : NULL ;
} ;

Inline ulen
els_len_nn(elstring els)
{
  return els->len ;
} ;

Inline ulen
els_len(elstring els)
{
  return (els != NULL) ? els_len_nn(els) : 0 ;
} ;

Inline bool
els_term_nn(elstring els)
{
  return els->term ;
} ;

Inline bool
els_term(elstring els)
{
  return (els != NULL) ? els_term_nn(els) : false ;
} ;

/*==============================================================================
 * All so simple that everything is implemented as Inline
 */

/*------------------------------------------------------------------------------
 * Initialise or create a new elstring
 */
Inline elstring
els_init_new(elstring els)
{
  if (els == NULL)
    els = XCALLOC(MTYPE_TMP, sizeof(elstring_t)) ;
  else
    memset(els, 0, sizeof(elstring_t)) ;

  confirm(ELSTRING_INIT_ALL_ZEROS) ;

  return els ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring value from ordinary string.
 *
 * NB: elstring MUST NOT be NULL.
 *
 * NB: treats str == NULL as a zero length string.
 *
 * NB: sets "term" unless str == NULL.
 */
Inline void
els_set_nn(elstring els, const void* str)
{
  els->body.cv = str ;
  els->len     = (str != NULL) ? strlen(str) : 0 ;
  els->term    = (str != NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring value from ordinary string.
 *
 * Creates elstring object if required (ie if els == NULL).
 *
 * See: els_set_nn.
 */
Inline elstring
els_set(elstring els, const void* str)
{
  if (els == NULL)
    els = XCALLOC(MTYPE_TMP, sizeof(elstring_t)) ;

  els_set_nn(els, str) ;

  return els ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring value from body + length.
 *
 * NB: sets term = false.
 *
 * NB: elstring MUST NOT be NULL.
 *
 * NB: treats str == NULL as a zero length string.
 */
Inline void
els_set_n_nn(elstring els, const void* body, ulen len)
{
  els->body.cv = body ;
  els->len     = len ;
  els->term    = false ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring value from body + length.
 *
 * Creates elstring object if required (ie if els == NULL).
 *
 * See els_set_n_nn.
 */
Inline elstring
els_set_n(elstring els, const void* body, ulen len)
{
  if (els == NULL)
    els = XCALLOC(MTYPE_TMP, sizeof(elstring_t)) ;

  els_set_n_nn(els, body, len) ;

  return els ;
} ;

/*------------------------------------------------------------------------------
 * Clear contents of an elstring (if any)
 *
 * NB: it is the callers responsibility to free the contents of the elstring.
 *     if that is required, before freeing the elstring itself.
 */
Inline void
els_clear(elstring els)
{
  if (els != NULL)
    {
      els->body.v  = NULL ;
      els->len     = 0 ;
      els->term    = false ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Release dynamically allocated elstring.
 *
 * Returns NULL.
 *
 * NB: it is the callers responsibility to free the contents of the elstring.
 *     if that is required, before freeing the elstring itself.
 */
Inline elstring
els_free(elstring els)
{
  if (els != NULL)
    XFREE(MTYPE_TMP, els) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring length.  And set term false.
 *
 * NB: it is the caller's responsibility to set a valid length !!
 *
 * NB: elstring MUST NOT be NULL.
 */
Inline void
els_set_len_nn(elstring els, ulen len)
{
  els->len     = len ;
  els->term    = false ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring body.  And set term false.
 *
 * NB: it is the caller's responsibility to set a valid body !!
 *
 * NB: elstring MUST NOT be NULL.
 */
Inline void
els_set_body_nn(elstring els, const void* body)
{
  els->body.cv  = body ;
  els->term     = false ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring terminated.
 *
 * NB: it is the caller's responsibility to set a valid body !!
 *
 * NB: elstring MUST NOT be NULL.
 */
Inline void
els_set_term_nn(elstring els, bool term)
{
  els->term     = term ;
} ;

/*------------------------------------------------------------------------------
 * Compare two elstrings -- returns the usual -ve, 0, +ve cmp result.
 */
Inline int
els_cmp(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  al, bl ;
  ulen  n ;

  ap = els_body(a) ;
  bp = els_body(b) ;
  al = els_len(a) ;
  bl = els_len(b) ;

  n = (al <= bl) ? al : bl ;

  while (n)
    {
      int d = *ap++ - *bp++ ;
      if (d != 0)
        return d ;
      --n ;
    } ;

  return al < bl ? -1 : (al == bl) ? 0 : +1 ;
} ;

/*------------------------------------------------------------------------------
 * Are two elstrings equal ?  -- returns true if strings equal.
 */
Inline bool
els_equal(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  n ;

  n = els_len(b) ;
  if (n != els_len(a))
    return false ;

  ap = els_body(a) ;
  bp = els_body(b) ;

  while (n)
    {
      if (*ap++ != *bp++)
        return false ;
      --n ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Is 'b' a leading substring of 'a' ?  -- returns true if it is.
 *
 * If 'b' is empty it is always a leading substring.
 */
Inline int
els_substring(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  n ;

  n = els_len(b) ;
  if (n > els_len(a))
    return false ;

  ap = els_body(a) ;
  bp = els_body(b) ;

  while (n)
    {
      if (*ap++ != *bp++)
        return false ;
      --n ;
    } ;

  return true ;
} ;

#endif /* _ZEBRA_ELSTRING_H */


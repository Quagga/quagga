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

/*==============================================================================
 * This is some very simple support for strings which are Length/Body
 * objects.
 *
 * NB: this object knows NOTHING about memory allocation etc.  The objective
 *     is the simplest possible encapsulation of strings which are NOT '\0'
 *     terminated.
 *
 * NB: this object knows NOTHING about whether there is a '\0' beyond the
 *     'len' of the string.
 */
struct elstring
{
  union
  {
    void*       v ;     /* may be NULL iff len == 0             */
    const void* cv ;
  } body ;

  ulen  len ;
} ;

typedef struct elstring  elstring_t[1] ;
typedef struct elstring* elstring ;

/* Setting an elstring object to all zeros is enough to initialise it to
 * an empty string.
 */
enum { ELSTRING_INIT_ALL_ZEROS = true } ;

#define ELSTRING_INIT { { { NULL }, 0 } }

/*==============================================================================
 * Pointer pair and unsigned pointer pair and const versions.
 */
struct pp
{
  char*   p ;
  char*   e ;
} ;
typedef struct pp  pp_t[1] ;
typedef struct pp* pp ;

struct cpp
{
  const char*   p ;
  const char*   e ;
} ;
typedef struct cpp  cpp_t[1] ;
typedef struct cpp* cpp ;

/*==============================================================================
 * NULLs for all types of pp and for els
 */
Inline void pp_null(pp p)   { p->p = p->e = NULL ; } ;
Inline void cpp_null(cpp p) { p->p = p->e = NULL ; } ;

Inline void els_null(elstring els) { els->body.v = NULL ; els->len = 0 ; } ;

/*==============================================================================
 * Access functions.
 */

Inline void* els_body(elstring els) ;
Inline void* els_body_nn(elstring els) ;
Inline ulen els_len(elstring els) ;
Inline ulen els_len_nn(elstring els) ;
Inline void* els_end(elstring els) ;
Inline void* els_end_nn(elstring els) ;

Inline void els_pp(pp p, elstring els) ;
Inline void els_pp_nn(pp p, elstring els) ;
Inline void els_cpp(cpp p, elstring els) ;
Inline void els_cpp_nn(cpp p, elstring els) ;

Inline void*
els_body(elstring els)
{
  return (els != NULL) ? els_body_nn(els)  : NULL ;
} ;

Inline void*
els_body_nn(elstring els)
{
  return els->body.v ;
} ;

Inline ulen
els_len(elstring els)
{
  return (els != NULL) ? els_len_nn(els) : 0 ;
} ;

Inline ulen
els_len_nn(elstring els)
{
  return els->len ;
} ;

Inline void*
els_end(elstring els)
{
  return (els != NULL) ? els_end_nn(els) : NULL ;
} ;

Inline void*
els_end_nn(elstring els)
{
  return (void*)((char*)els->body.v + els->len);
} ;

Inline void
els_pp(pp p, elstring els)
{
  if (els != NULL)
    els_pp_nn(p, els) ;
  else
    pp_null(p) ;
} ;

Inline void
els_pp_nn(pp p, elstring els)
{
  p->p = els->body.v ;
  p->e = p->p + els->len ;
} ;

Inline void
els_cpp(cpp p, elstring els)
{
  if (els != NULL)
    els_cpp_nn(p, els) ;
  else
    cpp_null(p) ;
} ;

Inline void
els_cpp_nn(cpp p, elstring els)
{
  p->p = els->body.cv ;
  p->e = p->p + els->len ;
} ;

/*==============================================================================
 * All so simple that most is implemented as Inline
 */

extern elstring els_init_new(elstring els) ;
extern elstring els_new(void) ;
extern elstring els_free(elstring els) ;

extern int els_cmp(elstring a, elstring b) ;
extern int els_cmp_str(elstring a, const char* s) ;
extern int els_nn_cmp(const void* ap, ulen al, const void* bp, ulen bl) ;
extern int els_cmp_word(elstring a, elstring w) ;
extern int els_cmp_sig(elstring a, elstring b) ;
extern bool els_equal(elstring a, elstring b) ;
extern bool els_substring(elstring a, elstring b) ;
extern ulen els_sub_len(elstring a, elstring b) ;

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
    els = els_new() ;

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
    els = els_new() ;

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
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring 'len'.
 *
 * NB: it is the caller's responsibility to set a valid body !!
 *
 * NB: elstring MUST NOT be NULL.
 */
Inline void
els_set_len_nn(elstring els, ulen len)
{
  els->len     = len ;
} ;

/*------------------------------------------------------------------------------
 * Set elstring body.
 *
 * NB: it is the caller's responsibility to set a valid body !!
 *
 * NB: it is the caller's responsibility to set a valid 'len'.
 *
 * NB: elstring MUST NOT be NULL.
 */
Inline void
els_set_body_nn(elstring els, const void* body)
{
  els->body.cv  = body ;
} ;

#endif /* _ZEBRA_ELSTRING_H */

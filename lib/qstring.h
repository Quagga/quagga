/* Some string handling -- header
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

#ifndef _ZEBRA_QSTRING_H
#define _ZEBRA_QSTRING_H

#include "misc.h"
#include "vargs.h"
#include "zassert.h"
#include "memory.h"
#include "elstring.h"

/*==============================================================================
 * These "qstrings" address address the lack of a flexible length string in 'C'.
 *
 * This is not a general purpose strings module, but provides a limited number
 * of useful string operations such that the caller does not need to worry
 * about the length of the string, and allocating space and so on.
 *
 * The caller does, however, have to explicitly release the contents of a
 * qstring when it is done with.
 *
 *
 *
 */
struct qstring
{
  elstring_t  els ;             /* *embedded*                   */

  usize       size ;            /* of the els body              */

  usize       cp ;

  usize       b_size ;
  void*       b_body ;
} ;

typedef struct qstring  qstring_t[1] ;
typedef struct qstring* qstring ;

/* Setting an qstring object to all zeros is enough to initialise it to
 * an empty string -- including the embedded elstring.
 */
CONFIRM(ELSTRING_INIT_ALL_ZEROS) ;
enum
{
  QSTRING_INIT_ALL_ZEROS = true
} ;

/*------------------------------------------------------------------------------
 * Access functions for body of qstring -- to take care of casting pointers
 *
 * NB: if the body has not yet been allocated, these functions will return
 *     NULL or NULL + the offset.
 */
Inline elstring
qs_els_nn(qstring qs)
{
  return qs->els ;
} ;

Inline elstring
qs_els(qstring qs)
{
  return qs->els ;
} ;

Inline void*
qs_body_nn(qstring qs)  /* pointer to body of qstring (not NULL)        */
{
  return els_body_nn(qs->els) ;
} ;

Inline void*            /* pointer to body of qstring           */
qs_body(qstring qs)
{
  return (qs != NULL) ? qs_body_nn(qs) : NULL ;
} ;

Inline void             /* set pointer to body of qstring (not NULL)    */
qs_set_body_nn(qstring qs, const void* body)
{
  els_set_body_nn(qs->els, body) ;      /* sets term = fase     */
} ;

Inline ulen             /* length of qstring (not NULL)         */
qs_len_nn(qstring qs)
{
  return els_len_nn(qs->els) ;
} ;

Inline ulen             /* length of qstring                    */
qs_len(qstring qs)
{
  return (qs != NULL) ? qs_len_nn(qs) : 0 ;
} ;

Inline void             /* set length of qstring (not NULL)     */
qs_do_set_len_nn(qstring qs, ulen len)
{
  els_set_len_nn(qs->els, len) ;        /* sets term = false    */
} ;

Inline ulen             /* cp of qstring (not NULL)             */
qs_cp_nn(qstring qs)
{
  return qs->cp ;
} ;

Inline ulen             /* cp of qstring                        */
qs_cp(qstring qs)
{
  return (qs != NULL) ? qs_cp_nn(qs) : 0 ;
} ;

Inline void             /* set cp of qstring (not NULL)         */
qs_do_set_cp_nn(qstring qs, ulen cp)
{
  qs->cp = cp ;
} ;

Inline char*            /* pointer to given offset in qstring   */
qs_char_at_nn(qstring qs, usize off)
{
  char* p ;
  p = qs_body_nn(qs) ;
  return (p != NULL) ? p + off : NULL ;
} ;

Inline char*            /* pointer to given offset in qstring   */
qs_char_at(qstring qs, usize off)
{
  return (qs != NULL) ? qs_char_at_nn(qs, off) : NULL ;
} ;

Inline char*            /* pointer to 'cp' offset in qstring    */
qs_cp_char(qstring qs)
{
  return (qs != NULL) ? qs_char_at_nn(qs, qs_cp_nn(qs)) : NULL ;
} ;

Inline char*            /* pointer to 'len' offset in qstring   */
qs_ep_char(qstring qs)
{
  return (qs != NULL) ? qs_char_at_nn(qs, qs_len_nn(qs)) : NULL ;
} ;

Inline bool             /* whether qstring is known to be terminated    */
qs_term_nn(qstring qs)
{
  return els_term_nn(qs->els) ;
} ;

Inline void             /* set qstring is known to be terminated        */
qs_set_term_nn(qstring qs, bool how)
{
  return els_set_term_nn(qs->els, how) ;
} ;

/*==============================================================================
 * Functions
 */

extern qstring qs_new(void) ;
extern qstring qs_init_new(qstring qs, usize len) ;
extern qstring qs_reset(qstring qs, free_keep_b free_structure) ;

Private void qs_make_to_size(qstring qs, usize len, free_keep_b free) ;

extern qstring qs_set(qstring qs, const char* src) ;
extern qstring qs_set_n(qstring qs, const char* src, usize n) ;

extern qstring qs_append(qstring qs, const char* src) ;
extern qstring qs_append_n(qstring qs, const char* src, usize n) ;

extern qstring qs_copy(qstring dst, qstring src) ;

extern qstring qs_set_alias(qstring qs, const char* src) ;
extern qstring qs_set_alias_n(qstring qs, const char* src, usize len) ;

extern qstring qs_printf(qstring qs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern qstring qs_vprintf(qstring qs, const char *format, va_list args) ;

extern usize qs_insert(qstring qs, const void* src, usize n) ;
extern void qs_replace(qstring qs, const void* src, usize n) ;
extern usize qs_delete(qstring qs, usize n) ;

extern int qs_cmp_sig(qstring a, qstring b) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Clear contents of qstring -- preserves any qstring body, but sets len = 0.
 *
 * Does nothing if qstring is NULL
 *
 * Sets 'cp' = 'len' = 0.
 *
 * If is an alias qstring, discard the alias.
 *
 * NB: does not create a qstring body if there isn't one.
 */
Inline void
qs_clear(qstring qs)
{
  if (qs != NULL)
    {
      qs_do_set_len_nn(qs, 0) ;         /* sets term == false   */
      if (qs->size == 0)
        {
          qs_set_body_nn(qs, qs->b_body) ;
          qs->size = qs->b_size ;
        } ;
      qs->cp  = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Need space for a string of 'slen' characters (plus possible '\0').
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Returns: address of qstring -- with body that can be written upto and
 *          including 'slen' + 1.
 *
 * NB: has no effect on 'len' -- even if 'len' > 'slen'.
 *
 * NB: has no effect on 'cp'  -- even if 'cp'  > 'len' or 'cp' > 'slen'.
 *
 * NB: if this is a aliased qstring, the alias is discarded and term = false.
 */
Inline qstring
qs_need(qstring qs, usize slen)
{
  if (qs == NULL)
    qs = qs_init_new(NULL, slen) ;      /* Make the qstring if required */
  else
    if (slen >= qs->size)               /* for alias qs->size == 0 !    */
      qs_make_to_size(qs, slen, free_it) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set 'len' -- allocate or extend body as required.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Returns: address of qstring -- with body that can be written upto and
 *          including 'len' + 1.
 *
 * Sets 'cp' to the (new) 'len' if 'cp' > 'len'.
 *
 * NB: if this is a aliased qstring, a copy is made of all of the original body,
 *     even if that is longer than the required 'slen'.  (And term = false.)
 */
Inline qstring
qs_set_len(qstring qs, usize len)
{
  if (qs == NULL)
    qs = qs_init_new(NULL, len) ;       /* Make the qstring if required */
  else
    if (len >= qs->size)                /* for alias qs->size == 0 !    */
      qs_make_to_size(qs, len, keep_it) ;

  qs_do_set_len_nn(qs, len) ;

  if (qs->cp > len)
    qs->cp = len ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Chop to given length -- will neither allocate nor extend body.
 *
 * Does nothing if qstring is NULL.
 *
 * Does not change the 'len' if it is <= length to chop to.
 *
 * Sets 'cp' to the (new) 'len' if 'cp' > 'len'.
 *
 * NB: if this is a aliased qstring, then it remains an aliased string, but
 *     shorter and term = false (unless no change made to the length).
 */
Inline void
qs_chop(qstring qs, usize clen)
{
  if (qs != NULL)
    {
      usize len = qs_len_nn(qs) ;
      if (len > clen)
        qs_do_set_len_nn(qs, (len = clen)) ;    /* sets term = false    */
      if (qs->cp > len)
        qs->cp = len ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set 'cp' -- allocate or extend body as required.
 *
 * Allocates the qstring, if required.
 *
 * Returns: address of qstring
 *
 * NB: if there was no body, allocates a body for the string, even if 'cp' == 0.
 *
 * NB: if new 'cp' > 'len', extends body (or allocates one), and sets 'len' to
 *     'cp'.  If this is an alias qstring, a copy of the string is made.
 */
Inline qstring
qs_set_cp(qstring qs, usize cp)
{
  if (qs == NULL)
    qs = qs_new(qs) ;

  if (qs->size <= cp)


  if ((qs == NULL) || (cp >(cp > qs_len_nn(qs)))
    qs = qs_set_len(qs, cp) ;
  qs->cp = cp ;
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * If not "term": set '\0' at qs->len -- extending body as required.
 *
 * Does NOT affect qs->cp or qs->len.
 *
 * Returns address of body -- NULL if the qstring is NULL
 *
 * NB: if this is an alias, and it is not terminated, make a copy before adding
 *     terminating '\0'.
 */
Inline void
qs_terminate_nn(qstring qs)
{
  if (!qs_term_nn(qs))
    {
      usize len ;

      len = qs_len_nn(qs) ;

      if (len >= qs->size)              /* alias has size == 0          */
        qs_make_to_size(qs, len) ;      /* make sure can insert '\0'    */

      *qs_char_at_nn(qs, len) = '\0' ;

      qs_set_term_nn(qs, true) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Return pointer to '\0' terminated string value.
 *
 * If qs is NULL or body is NULL returns pointer to constant empty '\0'
 * terminated string.
 *
 * If string is terminated, return address of string, which may be an alias
 * address.
 *
 * Otherwise, makes sure that there is a '\0' at the qs->len position, making
 * a copy of any aliased string if required, and returns address of result.
 *
 * NB: value returned may be the address of the qstring body, or the address of
 *     an aliased string.  In any event, the string should not be changed or
 *     reset until this pointer has been discarded !
 */
Inline const char*
qs_string(qstring qs)
{
  if ((qs == NULL) || (qs_len_nn(qs) == 0))
    return "" ;

  qs_terminate_nn(qs) ;

  return qs_body_nn(qs) ;
} ;

/*------------------------------------------------------------------------------
 * Assuming the given address is within the size of the given qstring,
 * set qs->len and insert '\0' terminator there.
 *
 * Does NOT affect qs->cp.
 *
 * NB: must NOT be a NULL qs.
 *
 * NB: must NOT be an aliased qstring.
 *
 * NB: must NOT have a NULL body.
 */
Inline void
qs_term_here(qstring qs, char* here)
{
  assert((here >= qs->s.char_body) && (here < (qs->s.char_body + qs->size))) ;

  qs->len = (here - qs->s.char_body) ;
  *here = '\0' ;
} ;

#endif /* _ZEBRA_QSTRING_H */

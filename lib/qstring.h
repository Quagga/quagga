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

#include "zebra.h"

#include <stddef.h>
#include <stdint.h>

#ifndef Inline
#define Inline static inline
#endif

/* GCC have printf type attribute check.  */
#ifdef __GNUC__
#define PRINTF_ATTRIBUTE(a,b) __attribute__ ((__format__ (__printf__, a, b)))
#else
#define PRINTF_ATTRIBUTE(a,b)
#endif /* __GNUC__ */

/*==============================================================================
 * These "qstrings" address the ...
 *
 *
 *
 */

typedef struct qstring  qstring_t ;
typedef struct qstring* qstring ;

struct qstring
{
  void*   body ;
  size_t  size ;

  size_t  len ;
  size_t  cp ;
} ;

/*------------------------------------------------------------------------------
 * Access functions for body of qstring -- to take care of casting pointers
 *
 * NB: if the body has not yet been allocated, these functions will return
 *     NULL or NULL + the offset.
 */
Inline char*                    /* pointer to body of qstring           */
qs_chars(qstring qs)
{
  return (char*)qs->body ;
} ;

Inline unsigned char*           /* pointer to body of qstring           */
qs_bytes(qstring qs)
{
  return (unsigned char*)qs->body ;
} ;

Inline char*                    /* pointer to given offset in qstring   */
qs_chars_at(qstring qs, size_t off)
{
  return qs_chars(qs) + off ;
} ;

Inline unsigned char*           /* pointer to given offset in qstring   */
qs_bytes_at(qstring qs, size_t off)
{
  return qs_bytes(qs) + off ;
} ;

Inline char*                    /* pointer to 'cp' offset in qstring    */
qs_cp_char(qstring qs)
{
  return qs_chars_at(qs, qs->cp) ;
} ;

Inline unsigned char*           /* pointer to 'cp' offset in qstring    */
qs_cp_byte(qstring qs)
{
  return qs_bytes_at(qs, qs->cp) ;
} ;

Inline char*                    /* pointer to 'len' offset in qstring   */
qs_ep_char(qstring qs)
{
  return qs_chars_at(qs, qs->len) ;
} ;

Inline unsigned char*           /* pointer to 'len' offset in qstring   */
qs_ep_byte(qstring qs)
{
  return qs_bytes_at(qs, qs->len) ;
} ;

/*==============================================================================
 * Functions
 */

extern qstring
qs_init_new(qstring qs, size_t len) ;

extern size_t
qs_alloc(qstring qs, size_t len) ;

extern void
qs_free_body(qstring qs) ;

extern qstring
qs_reset(qstring qs, int free_structure) ;

#define qs_reset_keep(qs) qs_reset(qs, 0)
#define qs_reset_free(qs) qs_reset(qs, 1)

extern int
qs_printf(qstring qs, const char* format, ...)      PRINTF_ATTRIBUTE(2, 3) ;

extern int
qs_vprintf(qstring qs, const char *format, va_list args) ;

extern size_t
qs_set(qstring qs, const char* s) ;

extern size_t
qs_set_n(qstring qs, const char* s, size_t len) ;

Inline size_t
qs_need(qstring qs, size_t len) ;

Inline size_t
qs_set_len(qstring qs, size_t len) ;

Inline void
qs_set_empty(qstring qs) ;

Inline size_t
qs_len(qstring qs) ;

Inline size_t
qs_size(qstring qs) ;

Inline void*
qs_term(qstring qs) ;

Inline size_t
qs_insert(qstring qs, const void* src, size_t n) ;

Inline void
qs_replace(qstring qs, const void* src, size_t n) ;

Inline size_t
qs_delete(qstring qs, size_t n) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Need space for a string of 'len' characters (plus possible '\0').
 *
 * Returns: size of the qstring body
 *          (which includes the extra space allowed for '\0')
 *
 * NB: asking for 0 bytes will cause a body to be allocated, ready for any
 *     '\0' !
 *
 * NB: has no effect on 'cp' or 'len'.
 */
Inline size_t
qs_need(qstring qs, size_t len)
{
  if (len < qs->size)
    {
      assert(qs->body != NULL) ;
      return qs->size ;
    }
  else
    return qs_alloc(qs, len) ;
} ;

/*------------------------------------------------------------------------------
 * Set 'len' -- allocate or extend body as required.
 *
 * Returns: size of the qstring body
 *          (which includes the extra space allowed for '\0')
 *
 * NB: asking for 0 bytes will cause a body to be allocated, ready for any
 *     '\0' !
 *
 * NB: has no effect on 'cp' -- even if 'cp' > 'len'.
 */
Inline size_t
qs_set_len(qstring qs, size_t len)
{
  qs->len = len ;
  return qs_need(qs, len) ;
} ;

/*------------------------------------------------------------------------------
 * Reset contents of qstring.
 *
 * Sets 'cp' = 'len' = 0.  Sets first byte of body (if any) to NULL.
 */
Inline void
qs_set_empty(qstring qs)
{
  qs->len = 0 ;
  qs->cp  = 0 ;
  if (qs->body != NULL)
    *((char*)qs->body) = '\0' ;
} ;

/*------------------------------------------------------------------------------
 * Get length of qstring -- by doing strlen() -- and record it in qs->len.
 *
 * Returns: the string length
 *
 * NB: if no body has been allocated, length = 0
 */
Inline size_t
qs_len(qstring qs)
{
  return qs->len = (qs->body != NULL) ? strlen(qs_chars(qs)) : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get size of qstring body.
 *
 * NB: if no body has been allocated, size = 0
 */
Inline size_t
qs_size(qstring qs)
{
  return qs->size ;
} ;

/*------------------------------------------------------------------------------
 * Set '\0' at qs->len -- allocate or extend body as required.
 *
 * Returns address of body.
 */
Inline void*
qs_term(qstring qs)
{
  size_t len ;
  if ((len = qs->len) >= qs->size)
    qs_alloc(qs, len) ;

  *qs_chars_at(qs, len) = '\0' ;

  return qs->body ;
} ;

/*------------------------------------------------------------------------------
 * Insert 'n' bytes at 'cp' -- moves anything cp..len up.
 *
 * Increases 'len'. but does not affect 'cp'.
 *
 * Returns: number of bytes beyond 'cp' that were moved before insert.
 *
 * NB: if 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 *     one or more undefined bytes.
 *
 * NB: the string is NOT re-terminated.
 */
Inline size_t
qs_insert(qstring qs, const void* src, size_t n)
{
  size_t after ;
  char* p ;

  if (qs->len < qs->cp)
    qs->len = qs->cp ;
  after = qs->len - qs->cp ;

  qs_set_len(qs, qs->len + n) ; /* set len and ensure have space        */

  p = qs_cp_char(qs) ;
  if (after > 0)
    memmove (p + n, p, after) ;

  if (n > 0)
    memmove(p, src, n) ;

  return after ;
} ;

/*------------------------------------------------------------------------------
 * Replace 'n' bytes at 'cp' -- extending if required.
 *
 * May increase 'len'. but does not affect 'cp'.
 *
 * NB: if 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 *     one or more undefined bytes.
 *
 * NB: the string is NOT re-terminated.
 */
Inline void
qs_replace(qstring qs, const void* src, size_t n)
{
  if (qs->len < qs->cp + n)
    qs_set_len(qs, qs->cp + n) ;   /* set len and ensure have space     */

  if (n > 0)
    memmove(qs_cp_char(qs), src, n) ;
} ;

/*------------------------------------------------------------------------------
 * Remove 'n' bytes at 'cp' -- extending if required.
 *
 * May change 'len'. but does not affect 'cp'.
 *
 * Returns: number of bytes beyond 'cp' that were moved before insert.
 *
 * NB: if 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 *     one or more undefined bytes.
 *
 * NB: the string is NOT re-terminated.
 */
Inline size_t
qs_delete(qstring qs, size_t n)
{
  size_t after ;
  char* p ;

  /* If deleting up to or beyond len, then simply set len == cp         */
  if ((qs->cp + n) >= qs->len)
    {
      qs_set_len(qs, qs->cp) ;  /* set len, looks after cp > len        */
      return 0 ;                /* nothing after                        */
    }

  /* There is at least one byte after cp (so body must exist)           */
  after = qs->len - (qs->cp + n) ;

  if (n > 0)
    {
      p = qs_cp_char(qs) ;
      memmove (p, p + n, after) ;

      qs->len -= n ;
    } ;

  return after ;
} ;

#endif /* _ZEBRA_QSTRING_H */

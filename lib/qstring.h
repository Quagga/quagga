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

#include "memory.h"

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
 * These "qstrings" address address the lack of a flexible length string in 'C'.
 *
 * This is not a general purpose strings module, but provides a limited number
 * of useful string operations such that the caller does not need to worry
 * about the length of the string, and allocating space and so on.
 *
 * The caller does, however, have to explicitly release the contents of a
 * qstring when it is done with.
 */

typedef struct qstring  qstring_t ;
typedef struct qstring* qstring ;

struct qstring
{
  union
  {
    void*               body ;
    const void*         const_body ;
    char*               char_body ;
    unsigned char*      uchar_body ;
  } ;
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

extern qstring qs_init_new(qstring qs, size_t len) ;
extern qstring qs_make_to_length(qstring qs, size_t len) ;
extern void qs_free_body(qstring qs) ;
extern qstring qs_reset(qstring qs, int free_structure) ;

#define qs_reset_keep(qs) qs_reset(qs, 0)
#define qs_reset_free(qs) qs_reset(qs, 1)

Inline qstring qs_new(void) ;
Inline qstring qs_dummy(qstring qs, const char* src, int pos) ;

extern qstring qs_printf(qstring qs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern qstring qs_vprintf(qstring qs, const char *format, va_list args) ;

extern qstring qs_set(qstring qs, const char* src) ;
extern qstring qs_set_n(qstring qs, const char* src, size_t n) ;

extern qstring qs_append(qstring qs, const char* src) ;
extern qstring qs_append_n(qstring qs, const char* src, size_t n) ;

Inline qstring qs_need(qstring qs, size_t len) ;
Inline qstring qs_set_len(qstring qs, size_t len) ;
extern qstring qs_add_len(qstring qs, size_t n, char** p_ep) ;
Inline void qs_clear(qstring qs) ;
Inline size_t qs_len(qstring qs) ;
Inline size_t qs_size(qstring qs) ;
Inline void* qs_term(qstring qs) ;

Inline size_t qs_insert(qstring qs, const void* src, size_t n) ;
Inline void qs_replace(qstring qs, const void* src, size_t n) ;
Inline size_t qs_delete(qstring qs, size_t n) ;

extern qstring qs_copy(qstring dst, qstring src) ;
extern int qs_cmp_sig(qstring a, qstring b) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Make a brand new, completely empty qstring
 */
Inline qstring
qs_new(void)
{
  /* Zeroising has set:
   *
   *   body   = NULL -- no body
   *   size   = 0    -- no body
   *
   *   len    = 0
   *   cp     = 0
   *
   * Nothing more to do unless initial size != 0
   */
  return XCALLOC(MTYPE_QSTRING, sizeof(qstring_t)) ;
} ;

/*------------------------------------------------------------------------------
 * Construct a "dummy" qstring from the given string.
 *
 * Allocates a qstring if required.
 *
 * This sets: body  = the src
 *            len   = strlen(src) (0 if src is NULL)
 *            cp    = 0    if 'pos' is zero
 *                    len  otherwise
 *            size  = 0
 *
 * The zero size means that the qstring handling will not attempt to free
 * the body, nor will it write to it...  Operations which require the qstring
 * to have a size will allocate a new body, and discard this one.
 *
 * Returns: the address of the dummy qstring.
 */
Inline qstring
qs_dummy(qstring qs, const char* src, int pos)
{
  if (qs == NULL)
    qs = qs_new() ;

  qs->const_body = src ;
  qs->len        = (src != NULL) ? strlen(src) : 0 ;
  qs->cp         = (pos == 0) ? 0 : qs->len ;
  qs->size       = 0 ;

  return qs ;
}

/*------------------------------------------------------------------------------
 * Need space for a string of 'len' characters (plus possible '\0').
 *
 * Allocates the qstring, if required.
 *
 * Returns: address of qstring
 *
 * NB: asking for 0 bytes will cause a body to be allocated, ready for any
 *     '\0' !
 *
 * NB: has no effect on 'cp' or 'len'. (Will be zero if new qstring allocated.)
 */
Inline qstring
qs_need(qstring qs, size_t len)
{
  if ((qs == NULL) || (len >= qs->size))
    return qs_make_to_length(qs, len) ;

  assert(qs->body != NULL) ;
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set 'len' -- allocate or extend body as required.
 *
 * Allocates the qstring, if required.
 *
 * Returns: address of qstring
 *
 * NB: setting len == 0 bytes will cause a body to be allocated, ready for any
 *     '\0' !
 *
 * NB: has no effect on 'cp' -- even if 'cp' > 'len'.
 *
 * NB: if this is a "dummy" qstring, a copy is made of the original body.
 */
Inline qstring
qs_set_len(qstring qs, size_t len)
{
  qs = qs_need(qs, len) ;
  qs->len = len ;
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Reset contents of qstring.
 *
 * Does nothing if qstring is NULL
 *
 * Sets 'cp' = 'len' = 0.  Sets first byte of body (if any) to NULL.
 *
 * For "dummy" qstring, discards the body.
 */
Inline void
qs_clear(qstring qs)
{
  if (qs != NULL)
    {
      qs->len = 0 ;
      qs->cp  = 0 ;
      if (qs->size > 0)
        *((char*)qs->body) = '\0' ;
      else
        qs->body = NULL ;
    } ;
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
  return (qs != NULL) ? (qs->len = (qs->body != NULL) ? strlen(qs_chars(qs))
                                                      : 0)
                      : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get size of qstring body.
 *
 * NB: if no body has been allocated, size == 0
 *     if qstring is NULL, size == 0
 *
 * NB: if this is a "dummy" qstring, size == 0.
 */
Inline size_t
qs_size(qstring qs)
{
  return (qs != NULL) ? qs->size : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get address of current end of qstring body -- ie byte at 'len'.
 *
 * NB: allocates body if required.
 *
 *     There will be space for '\0' after 'len', so the address returned
 *     is within the real body of the string.
 *
 * NB: if this is a "dummy" qstring, a copy is made of the original body.
 *
 * NB: address of qstring may NOT be NULL.
 */
Inline void*
qs_end(qstring qs)
{
  if (qs->len >= qs->size)
    qs_make_to_length(qs, qs->len) ;    /* allows for trailing '\0'     */

  return (char*)qs->body + qs->len ;
} ;

/*------------------------------------------------------------------------------
 * Set '\0' at qs->len -- allocate or extend body as required.
 *
 * Returns address of body -- NULL if the qstring is NULL
 *
 * NB: if this is a "dummy" qstring, a copy is made of the original body.
 */
Inline void*
qs_term(qstring qs)
{
  size_t len ;

  if (qs == NULL)
    return NULL ;

  if ((len = qs->len) >= qs->size)
    qs_make_to_length(qs, len) ;

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
 * NB: qstring MUST NOT be NULL
 *
 * NB: if 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 *     one or more undefined bytes.
 *
 * NB: the string is NOT re-terminated.
 *
 * NB: if this is a "dummy" qstring, a copy is made of the original body.
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
 * NB: qstring MUST NOT be NULL
 *
 * NB: if 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 *     one or more undefined bytes.
 *
 * NB: the string is NOT re-terminated.
 *
 * NB: if this is a "dummy" qstring, a copy is made of the original body.
 */
Inline void
qs_replace(qstring qs, const void* src, size_t n)
{
  if ((qs->len < qs->cp + n) || (qs->size == 0))
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
 * NB: qstring MUST NOT be NULL
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

  /* Watch out for "dummy"                                              */
  if (qs->size == 0)
    qs_make_to_length(qs, qs->len) ;

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

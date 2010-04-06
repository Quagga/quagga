/* Some string handling
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#include "qstring.h"

#include "memory.h"
#include "zassert.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Initialise qstring -- allocate if required.
 *
 * If non-zero len is given, a body is allocated (for at least len + 1).
 *
 * Returns: address of qstring
 *
 * NB: assumes initialising a new structure.  If not, then caller should
 *     use qs_reset() or qs_clear().
 */
extern qstring
qs_init_new(qstring qs, size_t len)
{
  if (qs == NULL)
    qs = qs_new() ;
  else
    memset(qs, 0, sizeof(qstring_t)) ;  /* see qs_new()         */

  if (len != 0)
    return qs_make_to_length(qs, len) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Allocate or reallocate so that string is big enough for the given length.
 *
 * Allocate qstring if required.  Returns with a body with size > 0.
 *
 * Allocates to 16 byte boundaries with space for '\0' beyond given length.
 *
 * Returns: address of qstring
 *
 * NB: allocates new body if the size == 0.
 *
 *     If the qstring is a "dummy", its contents are now copied to the new
 *     real qstring body -- up to a maximum of the new length.
 */
extern qstring
qs_make_to_length(qstring qs, size_t len)
{
  size_t size = (len + 0x10) & ~(size_t)(0x10 - 1) ;

  if (qs == NULL)
    qs = qs_new() ;

  if (size > qs->size)
    {
      if (qs->size == 0)
        {
          void* old ;
          old = qs->body ;

          qs->size = size ;
          qs->body = XMALLOC(MTYPE_QSTRING_BODY, qs->size) ;

          if ((qs->len != 0) && (old != NULL))
            memcpy(qs->body, old, (qs->len <= len) ? qs->len : len) ;
        }
      else
        {
          qs->size *= 2 ;
          if (qs->size < size)
            qs->size = size ;
          qs->body = XREALLOC(MTYPE_QSTRING_BODY, qs->body, qs->size) ;
        } ;
    };

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Add 'n' to the current string length, allocating or extending the body as
 * required.
 *
 * Allocate qstring if required.  Returns with a body with size > 0.
 *
 * Allocates to 16 byte boundaries with space for '\0' beyond new length.
 *
 * Returns: address of qstring
 *
 *    also: sets char** p_ep to point at the *end* of the new len.
 *
 * NB: allocates new body if the size == 0.
 *
 *     If the qstring is a "dummy", its contents are now copied to the new
 *     real qstring body -- up to a maximum of the new length.
 */
extern qstring
qs_add_len(qstring qs, size_t n, char** p_ep)
{
  size_t len ;
  len = (qs != NULL) ? qs->len + n : n ;

  qs = qs_make_to_length(qs, len) ;

  qs->len = len ;

  *p_ep = (char*)qs->body + len ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Free body of qstring -- zeroise size, len and cp
 *
 * Does nothing if qstring is NULL
 *
 * NB: frees the body if the size != 0.  So, a "dummy" qstring will not retain
 *     the old body.
 */
extern void
qs_free_body(qstring qs)
{
  if (qs != NULL)
    {
      if (qs->size != 0)
        XFREE(MTYPE_QSTRING_BODY, qs->body) ; /* sets qs->body = NULL */

      qs->size = 0 ;
      qs->len  = 0 ;
      qs->cp   = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Reset qstring -- free body and, if required, free the structure.
 *
 * If not freeing the structure, zeroise size, len and cp -- qs_free_body()
 *
 * Returns: NULL if freed the structure
 *          address of structure (if any), otherwise
 *
 * NB: frees the body if the size != 0.  So, a "dummy" qstring will not retain
 *     the old body.
 */
extern qstring
qs_reset(qstring qs, int free_structure)
{
  if (qs != NULL)
    {
      if (qs->size != 0)
        XFREE(MTYPE_QSTRING_BODY, qs->body) ;   /* sets qs->body = NULL */

      if (free_structure)
        XFREE(MTYPE_QSTRING, qs) ;              /* sets qs = NULL       */
      else
        {
          qs->size = 0 ;
          qs->len  = 0 ;
          qs->cp   = 0 ;
        } ;
    } ;
  return qs ;
} ;

/*==============================================================================
 * printf() and vprintf() type functions
 */

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf printf()
 *
 * Allocate qstring if required.
 *
 * Returns: address of qstring if OK
 *          NULL if failed (unlikely though that is)
 */
extern qstring
qs_printf(qstring qs, const char* format, ...)
{
  va_list args;

  va_start (args, format);
  qs = qs_vprintf(qs, format, args);
  va_end (args);

  return qs;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf vprintf()
 *
 * Allocate qstring if required.
 *
 * Returns: address of qstring if OK
 *          NULL if failed (unlikely though that is)
 */
extern qstring
qs_vprintf(qstring qs, const char *format, va_list args)
{
  va_list  ac ;
  int      len ;
  qstring qqs ;

  qqs = qs ;
  if (qs == NULL)
    qs = qs_new() ;

  while (1)
    {
     /* Note that vsnprintf() returns the length of what it would like to have
      * produced, if it had the space.  That length does not include the
      * trailing '\0'.
      *
      * Also note that given a zero length the string address may be NULL, and
      * the result is still the length required.
      */
      va_copy(ac, args);
      qs->len = len = vsnprintf (qs->body, qs->size, format, ac) ;
      va_end(ac);

      if (len < 0)
        break ;

      if (len < (int)qs->size)
        return qs ;

      qs_make_to_length(qs, len) ;
    } ;

  if (qqs == NULL)
    qs_reset_free(qs) ;         /* discard what was allocated   */
  else
    qs->len = 0 ;

  return NULL ;
} ;

/*==============================================================================
 * Other operations
 */

/*------------------------------------------------------------------------------
 * Set qstring to be copy of the given string.
 *
 * Allocates a qstring, if required.
 *
 * Sets qs->len to the length of the string (excluding trailing '\0')
 *
 * NB: if src == NULL, sets qstring to be zero length string.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_set(qstring qs, const char* src)
{
  qs = qs_set_len(qs, (src != NULL) ? strlen(src) : 0) ;
  if (qs->len != 0)
    memcpy(qs->body, src, qs->len + 1) ;
  else
    *((char*)qs->body) = '\0' ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be leading 'n' bytes of given string.
 *
 * Allocates qstring if required.
 *
 * Inserts '\0' terminator after the 'n' bytes copied.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: src string MUST be at least 'n' bytes long.
 *
 * NB: src may not be NULL unless n == 0.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_set_n(qstring qs, const char* src, size_t n)
{
  qs = qs_set_len(qs, n) ;      /* ensures have body > n        */
  if (n != 0)
    memcpy(qs->body, src, n) ;

  *((char*)qs->body + n) = '\0' ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Append given string to a qstring.
 *
 * Allocates a qstring, if required.
 *
 * Sets qs->len to the length of the result (excluding trailing '\0')
 *
 * NB: if src == NULL, appends nothing -- but result will be '\0' terminated.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring qs_append(qstring qs, const char* src)
{
  size_t n ;
  char* ep ;

  n = (src != NULL) ? strlen(src) : 0 ;

  qs = qs_add_len(qs, n, &ep) ;
  ep = (char*)qs->body + qs->len ;

  if (n != 0)
    memcpy(ep - n, src, n + 1) ;
  else
    *ep = '\0' ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be leading 'n' bytes of given string.
 *
 * Allocates qstring if required.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: src string MUST be at least 'n' bytes long.
 *
 * NB: src may not be NULL unless n == 0.
 *
 * NB: if n == 0, appends nothing -- but result will be '\0' terminated.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_append_n(qstring qs, const char* src, size_t n)
{
  char* ep ;

  qs = qs_add_len(qs, n, &ep) ;

  if (n != 0)
    memcpy(ep - n, src, n) ;

  *ep = '\0' ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Copy one qstring to another
 *
 * If both are NULL, returns NULL.
 *
 * Otherwise if dst is NULL, creates a new qstring.
 *
 * Sets dst:  body = copy of src->len bytes of src->body -- '\0' terminated.
 *            cp   = src->cp
 *            len  = src->len
 *
 * Where a NULL src has zero cp and len.
 *
 * If not NULL, the destination is guaranteed to have a body, and that will be
 * '\0' terminated.
 *
 * Returns: the destination qstring
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_copy(qstring dst, qstring src)
{
  size_t n ;

  if (src == NULL)
    {
      if (dst == NULL)
        return dst ;

      n       = 0 ;
      dst->cp = 0 ;
    }
  else
    {
      if (dst == NULL)
        dst = qs_new() ;

      n       = src->len ;
      dst->cp = src->cp ;
    } ;

  qs_set_len(dst, n) ;

  if (n > 0)
    memcpy(dst->body, src->body, n) ;

  *((char*)dst->body + n) = '\0' ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Compare significant parts of two qstrings.
 *
 * By significant, mean excluding leading/trailing isspace() and treating
 * multiple isspace() as single isspace().
 *
 * Compares the 'len' portions of the strings.
 *
 * If either is NULL, it is deemed to be an empty string.
 *
 * Returns: -1 => a <  b
 *           0 => a == b
 *          +1 => a >  b
 */
extern int
qs_cmp_sig(qstring a, qstring b)
{
  const unsigned char* p_a ;
  const unsigned char* e_a ;
  const unsigned char* p_b ;
  const unsigned char* e_b ;

  /* Set up pointers and dispense with leading and trailing isspace()
   *
   * Dummy up if NULL
   */
  if (a != NULL)
    {
      p_a = a->body ;
      e_a = p_a + a->len ;

      while ((p_a < e_a) && isspace(*p_a))
        ++p_a ;
      while ((p_a < e_a) && isspace(*(e_a - 1)))
        --e_a ;
    }
  else
    {
      p_a = NULL ;
      e_a = NULL ;
    }

  if (b != NULL)
    {
      p_b = b->body ;
      e_b = p_b + b->len ;

      while ((p_b < e_b) && isspace(*p_b))
        ++p_b ;
      while ((p_b < e_b) && isspace(*(e_b - 1)))
        --e_b ;
    }
  else
    {
      p_b = NULL ;
      e_b = NULL ;
    } ;

  /* Now set about finding the first difference                         */
  while ((p_a != e_a) && (p_b != e_b))
    {
      if (isspace(*p_a) && isspace(*p_b))
        {
          do { ++p_a ; } while isspace(*p_a) ;
          do { ++p_b ; } while isspace(*p_b) ;
        } ;

      if (*p_a != *p_b)
        return (*p_a < *p_b) ? -1 : +1 ;

      ++p_a ;
      ++p_b ;
    } ;

  /* No difference before ran out of one or both                        */
  if      (p_a != e_a)
    return +1 ;
  else if (p_b != e_b)
    return -1 ;
  else
    return  0 ;
} ;

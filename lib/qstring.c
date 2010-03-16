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
 *     use qs_reset() or qs_set_empty().
 */
extern qstring
qs_init_new(qstring qs, size_t len)
{
  if (qs == NULL)
    qs = XCALLOC(MTYPE_QSTRING, sizeof(qstring_t)) ;
  else
    memset(qs, 0, sizeof(qstring_t)) ;

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

  if (len != 0)
    qs_alloc(qs, len) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Allocate or reallocate so that string is big enough for the given length.
 *
 * Allocates to 16 byte boundaries.
 *
 * Returns: the number of bytes *allocated*, which includes the byte for
 *          possible trailing '\0'.
 *
 * NB: allocates EXTRA space for trailing '\0' beyond given length.
 */
extern size_t
qs_alloc(qstring qs, size_t len)
{
  len = (len + 0x10) & ~(size_t)(0x10 - 1) ;

  if (qs->body == NULL)
    {
      assert(qs->size == 0) ;
      qs->size = len ;
      qs->body = XMALLOC(MTYPE_QSTRING_BODY, qs->size) ;
    }
  else
    {
      assert(qs->size > 0) ;
      qs->size *= 2 ;
      if (qs->size < len)
        qs->size = len ;
      qs->body = XREALLOC(MTYPE_QSTRING_BODY, qs->body, qs->size) ;
    } ;

  return qs->size ;
} ;

/*------------------------------------------------------------------------------
 * Free body of qstring -- zeroise size, len and cp
 */
extern void
qs_free_body(qstring qs)
{
  if (qs->body != NULL)
    XFREE(MTYPE_QSTRING_BODY, qs->body) ;       /* sets qs->body = NULL */

  qs->size = 0 ;
  qs->len  = 0 ;
  qs->cp   = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Reset qstring -- free body and, if required, free the structure.
 *
 * If not freeing the structure, zeroise size, len and cp -- qs_free_body()
 *
 * Returns: NULL if freed the structure
 *          address of structure, otherwise
 */
extern qstring
qs_reset(qstring qs, int free_structure)
{
  if (qs->body != NULL)
    XFREE(MTYPE_QSTRING_BODY, qs->body) ;       /* sets qs->body = NULL */

  if (free_structure)
    XFREE(MTYPE_QSTRING, qs) ;                  /* sets qs = NULL       */
  else
    {
      qs->size = 0 ;
      qs->len  = 0 ;
      qs->cp   = 0 ;
    } ;

  return qs ;
} ;

/*==============================================================================
 * printf(0 and vprintf() type functions
 */

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf printf()
 */
extern int
qs_printf(qstring qs, const char* format, ...)
{
  va_list args;
  int result ;

  va_start (args, format);
  result = qs_vprintf(qs, format, args);
  va_end (args);

  return result;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf vprintf()
 *
 * Note that vsnprintf() returns the length of what it would like to have
 * produced, if it had the space.  That length does not include the trailing
 * '\0'.
 *
 * Also note that given a zero length the string address may be NULL, and the
 * result is still the length required.
 */
extern int
qs_vprintf(qstring qs, const char *format, va_list args)
{
  va_list  ac ;
  int len ;

  while (1)
    {
      va_copy(ac, args);
      qs->len = len = vsnprintf (qs->body, qs->size, format, ac) ;
      va_end(ac);

      if (len < (int)qs->size)
        return len ;            /* quit if done (or error)              */

      qs_alloc(qs, len) ;
    } ;
} ;

/*==============================================================================
 * Other operations
 */

/*------------------------------------------------------------------------------
 * Set qstring to be copy of the given string.
 *
 * Sets qs->len to the length of the string (excluding trailing '\0')
 *
 * NB: if stc == NULL, sets qstring to be zero length string.
 */
extern size_t
qs_set(qstring qs, const char* src)
{
  qs_set_len(qs, (src != NULL) ? strlen(src) : 0) ;
  if (qs->len != 0)
    memcpy(qs->body, src, qs->len + 1) ;
  else
    *((char*)qs->body) = '\0' ;

  return qs->len ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be leading 'n' bytes of given string.
 *
 * NB: src string MUST be at least that long.
 *
 * NB: src may not be NULL unless len == 0.
 */
extern size_t
qs_set_n(qstring qs, const char* src, size_t n)
{
  qs_need(qs, n) ;      /* sets qs->len */
  if (n != 0)
    memcpy(qs->body, src, n) ;

  *((char*)qs->body + n) = '\0' ;

  return n ;
} ;

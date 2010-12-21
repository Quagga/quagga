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

#include <stdio.h>
#include <ctype.h>

#include "qstring.h"

#include "memory.h"
#include "zassert.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Create a new, empty qs
 */
extern qstring
qs_new(void)
{
  /* zeroising sets a completely empty qstring -- see qs_init_new()     */
  return XCALLOC(MTYPE_QSTRING, sizeof(qstring_t)) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise qstring -- allocate if required.
 *
 * If non-zero slen is given, a body is allocated (for at least slen + 1).
 * If zero slen is given, no body is allocated.
 *
 * Sets qs->len = qs->cp = 0.  '\0' terminates body if allocates one.
 *
 * Returns: address of qstring
 *
 * NB: assumes initialising a new structure.  If not, then caller should
 *     use qs_reset() or qs_clear().
 */
extern qstring
qs_init_new(qstring qs, usize slen)
{
  if (qs == NULL)
    qs = qs_new() ;
  else
    memset(qs, 0, sizeof(qstring_t)) ;

  confirm(QSTRING_INIT_ALL_ZEROS) ;

  /* Zeroising has set:
   *
   *   body   = NULL -- no body
   *   size   = 0    -- no body
   *
   *   len    = 0
   *   cp     = 0
   *
   *   b_body = NULL -- no body buffer
   *   b_size = 0    -- no body buffer
   */

  if (slen != 0)
    qs_make_to_size(qs, slen, false) ;

  return qs ;
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
qs_reset(qstring qs, free_keep_b free_structure)
{
  if (qs != NULL)
    {
      if (qs->b_body != NULL)
        XFREE(MTYPE_STRING, qs->b_body) ;       /* sets qs->b_body = NULL  */

      if (free_structure)
        XFREE(MTYPE_QSTRING, qs) ;              /* sets qs = NULL          */
      else
        memset(qs, 0, sizeof(qstring_t)) ;      /* see qs_init_new         */
    } ;
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Allocate or reallocate body so that is big enough for the given "slen".
 *
 * If the qstring is currently an alias, copies all of the alias to a new
 * body -- so always returns a non-alias qstring.
 *
 * Returns with a body with size > 0.  Allocates to 16 byte boundaries with
 * space for '\0' beyond given length.
 *
 * Does NOT affect qs->len or qs->cp.  Does NOT re-terminate.
 *
 * NB: will allocate a new body even if the slen == 0.
 *
 * NB: always copies all of any aliased string (even if the slen == 0).
 *
 * NB: sets terminated false
 */
Private void
qs_make_to_size(qstring qs, usize slen, bool keep)
{
  usize size ;
  usize alen ;

  /* Worry about alias.  If we keep it, we keep all of it.              */
  if (keep && (qs->size == 0))
    {
      alen = qs_len_nn(qs) ;    /* alias stuff to keep                  */
      if (slen <= alen)
        slen = alen + 1 ;       /* making sure can do that.             */
    }
  else
    alen = 0 ;                  /* no alias stuff to keep               */

  /* Calculate the new size -- multiple of 16, >= 16.                   */
  size = (slen + 0x10) & ~(usize)(0x10 - 1) ;
  dassert(size != 0) ;

  /* If that requires the body to be extended, do that now              */
  if (size > qs->b_size)
    {
      /* Need to allocate or extend the buffer.
       *
       * If current size is not zero, extend by doubling size or making at
       * least the multiple of 16 calculated for new len.
       */
      qs->b_size *= 2 ;
      if (qs->b_size < size)
        qs->b_size = size ;
      qs->b_body = XREALLOC(MTYPE_STRING, qs->b_body, qs->b_size) ;
    } ;

  /* If this is a non-empty alias, copy all or part of it.              */
  if (alen != 0)
    memcpy(qs->b_body, qs_body_nn(qs), alen) ;

  /* Update body and size, and no longer known to be terminated         */
  qs_set_body_nn(qs, qs->b_body) ;
  qs->size = qs->b_size ;


} ;

/*==============================================================================
 * Setting value of qstring
 */

/*------------------------------------------------------------------------------
 * Set qstring to be copy of the given string.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Allocates a body and copies src to it, adding '\0'.  Treats src == NULL as
 * an empty string.
 *
 * Sets qs->len to the length of the string (excluding trailing '\0').
 * Sets qs->cp  == 0.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_set(qstring qs, const char* src)
{
  return qs_set_n(qs, src, (src != NULL ? strlen(src) : 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be leading 'n' bytes of given string.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Allocates a body and copies 'n' bytes from src to it, adding '\0'.  The
 * src pointer is ignored if n == 0.
 *
 * Sets qs->len to the length of the string (excluding trailing '\0').
 * Sets qs->cp  == 0.
 *
 * Returns: address of the qstring copied to.
 *
 * NB: if n == 0, src may be NULL
 *
 * NB: if n > 0, src string MUST be at least 'n' bytes long.
 *
 * NB: if copying to a dummy qstring, the old body is simply discarded.
 */
extern qstring
qs_set_n(qstring qs, const char* src, usize len)
{
  char* p ;

  qs = qs_new_len(qs, len) ;    /* ensures have body > n        */

  p = qs_char_nn(qs) ;

  if (len != 0)
    memcpy(p, src, len) ;

  *(p + len) = '\0' ;

  qs_set_term_nn(qs, true) ;
  qs->cp = 0 ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Append given string to a qstring -- adding at qs->len position.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Allocates or extends the body and copies bytes from src to it, adding '\0'.
 * Treats src == NULL as an empty string.
 *
 * Sets qs->len to the length of the result (excluding trailing '\0')
 * Does not change qs->cp.
 *
 * Returns: address of the qstring appended to.
 *
 * NB: if appending to a dummy qstring, the old body is copied first.
 */
extern qstring qs_append(qstring qs, const char* src)
{
  return qs_append_n(qs, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Append leading 'n' bytes of given string to a qstring -- adding at qs->len
 * position.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Allocates or extends the body and copies 'n' bytes from src to it,
 * adding '\0'.  The src pointer is ignored if n == 0.
 *
 * Sets qs->len to the length of the result (excluding trailing '\0')
 * Does not change qs->cp.
 *
 * Returns: address of the qstring appended to.
 *
 * NB: if n == 0, src may be NULL
 *
 * NB: if n > 0, src string MUST be at least 'n' bytes long.
 *
 * NB: if appending to a dummy qstring, the old body is copied first.
 */
extern qstring
qs_append_n(qstring qs, const char* src, usize n)
{
  char* ep ;

  qs = qs_add_len(qs, n, &ep) ;

  if (n != 0)
    memcpy(ep, src, n) ;

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
  usize n ;

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

  qs_set_len(dst, n) ;          /* TODO: Copies alias !!        */

  if (n > 0)
    memcpy(dst->s.body, src->s.body, n) ;

  *(dst->s.char_body + n) = '\0' ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Construct a qstring which is an alias for the given string.
 *
 * Allocates a qstring if required.
 *
 * Given string must be '\0' terminated.
 *
 * Does NOT copy the given string, but sets the qstring to be a pointer to it.
 *
 * NB: it is the caller's responsibility to ensure that the original string
 *     stays put for however long the qstring is an alias for it.
 *
 *     It is also the caller's responsibility to see that the original string
 *     is discarded as required (once the alias is no longer required.)
 *
 * NB: if the qstring is changed in any way, a copy of the aliased string will
 *     be made first.
 *
 * NB: if a pointer to the body of the qstring is taken, then while that is in
 *     use, the qstring must not be released, so that the alias is not
 *     released.
 *
 * Returns: the address of the qstring.
 */
extern qstring
qs_set_alias(qstring qs, const char* src)
{
  if (qs == NULL)
    qs = qs_init_new(NULL, 0) ;

  /* Make the alias.  Note that any existing b_body and b_size are preserved,
   * so that any current body can be reused at a later date.
   */
  qs->s.const_body = (src != NULL) ? src : "" ;
  qs->len          = strlen(src) ;
  qs->cp           = 0 ;
  qs->size         = 0 ;        /* <=> this is an alias !       */
  qs->terminated   = true ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Construct a qstring which is an alias for the 'n' given characters.
 *
 * Allocates a qstring if required.
 *
 * Given characters are assumed not to be '\0' terminated.
 *
 * Does NOT copy the given characters, but sets the qstring to be a pointer to
 * them.
 *
 * NB: it is the caller's responsibility to ensure that the original characters
 *     stays put for however long the qstring is an alias for them.
 *
 *     It is also the caller's responsibility to see that the original
 *     characters are discarded as required (once the alias is no longer
 *     required.)
 *
 * NB: if the qstring is changed in any way, a copy of the aliased characters
 *     will be made first.
 *
 * NB: if a pointer to the body of the qstring is taken, then while that is in
 *     use, the qstring must not be released, so that the alias is not
 *     released.
 *
 * Returns: the address of the qstring.
 */
extern qstring
qs_set_alias_n(qstring qs, const char* src, usize len)
{
  if (qs == NULL)
    qs = qs_init_new(NULL, 0) ;

  if (len == 0)
    src = "" ;
  else
    assert(src != NULL) ;

  /* Make the alias.  Note that any existing b_body and b_size are preserved,
   * so that any current body can be reused at a later date.
   */
  qs->s.const_body = src ;
  qs->len          = len ;
  qs->cp           = 0 ;
  qs->size         = 0 ;        /* <=> this is an alias !       */
  qs->terminated   = false ;

  return qs ;
} ;















/*------------------------------------------------------------------------------
 * Add 'n' to the current string length, allocating or extending the body.
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * Returns with a body with size > 0.  Allocates to 16 byte boundaries with
 * space for '\0' beyond given length.
 *
 * Does NOT affect qs->cp.
 * Does set the new qs->len -- qs->len += n
 * Does NOT reterminate.
 *
 * Returns: address of qstring
 *
 *    also: sets char** p_ep to point at the *end* of the old len.
 *
 * NB: will allocate a new body even if the new len == 0.
 *
 * NB: always copies all of any aliased string (even if the slen == 0).
 */
extern qstring
qs_add_len(qstring qs, usize n, char** p_ep)
{
  usize slen ;
  usize len ;

  len  = qs_len(qs) ;
  slen = len + n ;

  /* Set the new length -- creating if required.
   *
   * Will always return with a body and no longer an alias (if was one).
   */
  qs = qs_set_len(qs, slen) ;

  /* Set pointer to old end (len) position.             */
  *p_ep = ((char*)qs_body_nn(qs)) + len ;

  return qs ;
} ;

/*==============================================================================
 * printf() and vprintf() type functions
 */

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf printf()
 *
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * If OK:
 *
 *   Sets qs->len to the length of the null terminated result.
 *   Does NOT affect qs->cp.
 *
 * If fails:
 *
 *   Sets qs->len = qs->cp = 0 and terminates to zero length.
 *
 * Returns: address of qstring if OK
 *          NULL if failed (unlikely though that is) -- qstring set empty.
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
 * Allocate qstring if required (setting qs->len = qs->cp = 0).
 *
 * If OK:
 *
 *   Sets qs->len to the length of the null terminated result.
 *   Does NOT affect qs->cp.
 *
 * If fails:
 *
 *   Sets qs->len = qs->cp = 0 and terminates to zero length.
 *
 * Returns: address of qstring if OK
 *          NULL if failed (unlikely though that is)
 */
extern qstring
qs_vprintf(qstring qs, const char *format, va_list args)
{
  va_list  ac ;
  int      slen ;
  qstring  qqs ;

  qqs = qs ;
  if (qs == NULL)
    qs = qs_new() ;             /* sets size == 0               */
  else
    qs_set_len_nn(qs, 0) ;      /* Forget current contents      */

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
      slen = vsnprintf (qs_body_nn(qs), qs->size, format, ac) ;
      va_end(ac);

      if (slen < 0)
        break ;                         /* Quit if failed               */

      if ((usize)slen < qs->size)
        {
          qs_set_len_nn(qs, slen) ;
          return qs ;                   /* Exit if succeeded            */
        } ;

      qs_make_to_size(qs, slen) ;       /* Extend body to required len  */
    } ;

  if (qqs == NULL)
    qs_reset(qs, free_it) ;             /* discard what was allocated   */
  else
    qs_clear(qs) ;

  return NULL ;
} ;

/*==============================================================================
 * Other operations
 */

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
      p_a = a->s.uchar_body ;
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
      p_b = b->s.uchar_body ;
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
          do { ++p_a ; } while (isspace(*p_a)) ;
          do { ++p_b ; } while (isspace(*p_b)) ;
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
 * NB: if this is a aliased qstring, a copy is made of the original body.
 */
extern usize
qs_insert(qstring qs, const void* src, usize n)
{
  usize after ;
  usize len ;
  char* p ;

  qs->terminated = false ;      /* NB: require qs != NULL !             */

  len = qs_len_nn(qs) ;
  if (len < qs->cp)             /* make len = max(len, cp) !            */
    len = qs->cp ;

  after = len - qs->cp ;

  qs_set_len(qs, len + n) ;     /* set len and ensure have space
                                   Makes copy of any aliased string.    */
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
 * NB: if this is a aliased qstring, a copy is made of the original body.
 */
extern void
qs_replace(qstring qs, const void* src, usize n)
{
  usize len ;

  qs->terminated = false ;      /* NB: require qs != NULL !             */

  len = qs_len_nn(qs) ;
  if (len < (qs->cp + n))       /* make len = max(len, cp + n)          */
    len = qs->cp + n ;

  qs_set_len(qs, len) ;         /* set len and ensure have space.
                                   Makes copy of any aliased string.    */

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
 *
 * NB: if this is a aliased qstring, a copy is made of the original body.
 */
extern usize
qs_delete(qstring qs, usize n)
{
  usize after ;
  char* p ;
  usize len ;

  qs->terminated = false ;      /* NB: require qs != NULL !             */

  len = qs_len_nn(qs) ;

  /* If deleting to or beyond 'len', force len to cp                    */
  if ((qs->cp + n) >= len)
    {
      len = qs->cp ;
      qs_set_len_nn(qs, len) ;  /* truncate now, so that if this is an
                                   aliased string, only copy what is
                                   going to be kept.                    */
      after = 0 ;               /* nothing to move                      */
    }
  else
    after = len - (qs->cp + n) ;

  qs_set_len(qs, len) ;         /* set len and ensure have space.
                                   Makes copy of any aliased string.    */



  /* Watch out for "dummy"                                              */
  if (qs->size == 0)
    qs_make_to_size(qs, len) ;

  /* If deleting up to or beyond len, then simply set len == cp
   * note that this may reduce or increase len !
   */
  if ((qs->cp + n) >= len)
    {
      if (qs->cp < len)
        qs_set_len_nn(qs, qs->cp) ;     /* discard stuff after qs->cp   */

      qs_set_len(qs, qs->cp) ;          /* set len                              */
      return 0 ;                /* nothing after                        */
    }

  /* There is at least one byte after cp (so body must exist)           */
  after = len - (qs->cp + n) ;

  if (n > 0)
    {
      p = qs_cp_char(qs) ;
      memmove (p, p + n, after) ;

      qs_set_len_nn(qs, len - n) ;
    } ;

  return after ;
} ;

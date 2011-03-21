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

#include "stdio.h"

#include "qstring.h"
#include "memory.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Create a new body or extend existing one to accommodate at least slen + 1.
 *
 * Sets size to multiple of 8 (minimum 16), with at least 9 bytes free beyond
 * the requested slen.
 *
 * Does not affect 'len' or 'cp'.
 *
 * If 'keep_alias', ONLY sets 'b_size' & 'b_body'.
 *
 * If !'keep_alias', sets the elstring body & size & clears 'alias'.
 */
static inline void
qs_new_body(qstring qs, usize slen, bool keep_alias)
{
  qs->b_size = (slen + 0x10) & ~(usize)(0x08 - 1) ;
  qs->b_body = XREALLOC(MTYPE_STRING, qs->b_body, qs->b_size) ;

  if (!keep_alias)
    qs_set_real_body_nn(qs) ;
} ;

/*------------------------------------------------------------------------------
 * Create a new, empty qs
 *
 * If non-zero slen is given, a body is allocated (size = slen + 1).
 * If zero slen is given, no body is allocated.
 *
 * Sets 'len' = 'cp' = 0.
 *
 * Returns: address of qstring
 */
extern qstring
qs_new(usize slen)
{
  qstring qs ;

  /* zeroising sets a completely empty qstring -- see qs_init_new()     */

  qs = XCALLOC(MTYPE_QSTRING, sizeof(qstring_t)) ;

  confirm(QSTRING_INIT_ALL_ZEROS) ;

  if (slen != 0)
    qs_new_body(qs, slen, false) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Create a new, empty qs with body
 */
extern qstring
qs_new_with_body(usize slen)
{
  return qs_new(slen | 1) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise qstring -- allocate if required.
 *
 * If non-zero slen is given, a body is allocated (size = slen + 1).
 * If zero slen is given, no body is allocated.
 *
 * Sets 'len' = 'cp' = 0.
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
    return qs_new(slen) ;

  memset(qs, 0, sizeof(qstring_t)) ;

  confirm(QSTRING_INIT_ALL_ZEROS) ;

  /* Zeroising has set:
   *
   *   empty elstring -- no body, 'len' = 0
   *
   *   size   = 0     -- no elstring body
   *
   *   cp     = 0
   *
   *   b_body = NULL  -- no body buffer
   *   b_size = 0     -- no body buffer
   *
   *   alias  = false -- not an alias qstring
   */

  if (slen != 0)
    qs_new_body(qs, slen, false) ;

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

      confirm(QSTRING_INIT_ALL_ZEROS) ;
    } ;
  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Allocate or reallocate body so that is big enough for the given 'slen'.
 *
 * qstring may NOT be NULL.
 *
 * Returns with a body with size > 0 (even if 'slen' == 0).
 *
 * Expects, but does not require, that 'slen' > 'size'.
 *
 * Does not change 'cp' or 'len' (even if 'len' > 'slen').
 *
 * If is an alias, copies min('len', 'alen', 'slen') characters to the body.
 */
Private void
qs_make_to_size(qstring qs, usize slen, usize alen)
{
  /* If body needs to be extended, do that now                          */
  if (slen >= qs->b_size)
    qs_new_body(qs, slen, (qs->alias && (alen != 0))) ;
                                            /* keeps alias if required  */

  /* Worry about alias.  If we keep it, we keep all of it.              */
  if (qs->alias)
    {
      /* alias or empty                         */
      usize len = qs_len_nn(qs) ;

      if (alen >= len)
        alen = len ;            /* keep min(alen, len)                  */

      if (alen > slen)
        alen = slen ;           /* keep only to new len.                */

      if (alen != 0)
        memcpy(qs->b_body, qs_body_nn(qs), alen) ;

      qs_set_real_body_nn(qs) ;
    } ;
} ;

/*==============================================================================
 * Setting value of qstring
 *
 * Copy the given string to the qstring, allocating qstring and/or extending
 * it as required.
 *
 * Any alias is simply discarded.
 *
 * Sets 'len' to new length
 * Sets 'cp'  = 0
 *
 * Returns: address of the qstring (allocated if required).
 */

/*------------------------------------------------------------------------------
 * Set qstring to be copy of the given string.
 *
 * Treats src == NULL as an empty string.  Otherwise src must be a '\0'
 * terminated string.
 *
 * Does not copy or count the '\0' terminator.
 *
 * See notes above.
 */
extern qstring
qs_set(qstring qs, const char* src)
{
  return qs_set_n(qs, src, (src != NULL ? strlen(src) : 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be copy of leading 'n' bytes of given string.
 *
 * If n == 0, src is ignored (and may be NULL)
 * If n > 0, src string MUST be at least 'n' bytes long.
 *
 * See notes above.
 */
extern qstring
qs_set_n(qstring qs, const char* src, usize len)
{
  qs = qs_new_size(qs, len) ;           /* ensures have body > len      */

  if (len != 0)
    memcpy(qs_char_nn(qs), src, len) ;

  qs_set_len_nn(qs, len) ;
  qs_set_cp_nn(qs, 0) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be copy of given qstring contents.
 *
 * If the given qstring is an alias, then the contents of the alias are copied
 * (so the result is not an alias).  See qs_copy() for the alternative.
 *
 * See notes above -- and note that 'cp' is set to 0.
 */
extern qstring
qs_set_qs(qstring qs, qstring src)
{
  return qs_set_n(qs, qs_body(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be copy of given elstring contents.
 *
 * See notes above.
 */
extern qstring
qs_set_els(qstring qs, elstring src)
{
  return qs_set_n(qs, els_body(src), els_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring with given pattern to given length.
 *
 * Repeats the given pattern as many times as necessary to get to the given
 * length -- using a final partial piece of the pattern as required.
 *
 * If the pattern is zero length, fills with spaces !
 *
 * See notes above.
 */
extern qstring
qs_set_fill(qstring qs, usize len, const char* src)
{
  return qs_set_fill_n(qs, len, src, (src != NULL ? strlen(src) : 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring with given pattern to given length.
 *
 * Repeats the given pattern as many times as necessary to get to the given
 * length -- using a final partial piece of the pattern as required.
 *
 * See notes above.
 */
extern qstring
qs_set_fill_n(qstring qs, usize len, const char* src, usize flen)
{
  char*  p ;
  char*  q ;
  usize  left ;

  qs = qs_new_size(qs, len) ;           /* ensures have body > len      */

  if (len != 0)
    {
      if (flen == 0)
        {
          src  = "          " ;
          flen = strlen(src) ;
        } ;

      if (len < flen)
        flen = len ;

      q = p = qs_char_nn(qs) ;
      memcpy(p, src, flen) ;
      p    += flen ;
      left  = len - flen ;

      while (left > 0)
        {
          if (left < flen)
            flen = left ;

          memcpy(p, q, flen) ;
          p    += flen ;
          left -= flen ;

          flen += flen ;
        } ;
    } ;

  qs_set_len_nn(qs, len) ;
  qs_set_cp_nn(qs, 0) ;

  return qs ;
} ;

/*==============================================================================
 * Appending to a qstring
 *
 * Copy the given string to the end of the given qstring (at 'len'),
 * allocating qstring and/or extending it as required.
 *
 * If this is an alias, it is copied to before being appended to (even if
 * appending nothing).
 *
 * Can append to NULL or empty qstring.
 *
 * Sets 'len' to new length.
 * Does not affect 'cp'.
 *
 * Will work even if the stuff being appended is somewhere in the body of the
 * qstring !!
 *
 * Returns: address of the qstring (allocated if required).
 */

/*------------------------------------------------------------------------------
 * Append given qstring to a qstring.
 *
 * See notes above.
 */
extern qstring
qs_append(qstring qs, qstring src)
{
  return qs_append_str_n(qs, qs_body(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Append given string to a qstring.
 *
 * Treats src == NULL as an empty string.  Otherwise src must be a '\0'
 * terminated string.
 *
 * Does not copy or count the '\0' terminator.
 *
 * See notes above.
 */
extern qstring qs_append_str(qstring qs, const char* src)
{
  return qs_append_str_n(qs, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Append leading 'n' bytes of given string to a qstring.
 *
 * If n == 0, src may be NULL
 * If n > 0, src string MUST be at least 'n' bytes long.
 *
 * See notes above.
 */
extern qstring
qs_append_str_n(qstring qs, const char* src, usize n)
{
  qs = qs_extend(qs, n) ; /* allocate, copy any alias, extend body,
                             set new length, etc                        */

  if (n != 0)
    memmove(qs_ep_char_nn(qs) - n, src, n) ;

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Append given elstring to a qstring.
 *
 * See notes above.
 */
extern qstring
qs_append_els(qstring qs, elstring src)
{
  return qs_append_str_n(qs, els_body(src), els_len(src)) ;
} ;

/*==============================================================================
 * Setting of alias.
 *
 * Does NOT copy the given string, but sets the qstring to be a pointer to it.
 * This means that:
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
 * Preserves any existing qstring body.
 *
 * Returns: address of the qstring (allocated if required).
 */

/*------------------------------------------------------------------------------
 * Set qstring to be an alias for the given string.
 *
 * Treats src == NULL as an empty string.  Otherwise src must be a '\0'
 * terminated string.
 *
 * Does not count the '\0' terminator.
 *
 * See notes above.
 */
extern qstring
qs_set_alias(qstring qs, const char* src)
{
  return qs_set_alias_n(qs, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be an alias for the leading 'n' bytes of given string.
 *
 * If n == 0, src may be NULL
 * If n > 0, src string MUST be at least 'n' bytes long.
 *
 * See notes above.
 */
extern qstring
qs_set_alias_n(qstring qs, const char* src, usize n)
{
  if (qs == NULL)
    qs = qs_new(0) ;

  /* Make the alias.  Note that any existing b_body and b_size are preserved,
   * so that any current body can be reused at a later date.
   */
  qs_set_body_nn(qs, (n != 0) ? src : "") ;
  qs_set_len_nn(qs, n) ;
  qs_set_cp_nn(qs, 0) ;
  qs->alias = true ;
  qs->size  = 0 ;               /* => empty or alias                    */

  return qs ;
} ;

/*------------------------------------------------------------------------------
 * Set qstring to be an alias for the given qstring.
 *
 * If the src is not an alias, then the qstring is an alias for the body of
 * src -- so must be careful not to disturb that !
 *
 * If the src is an alias, then the qstring is another alias.
 *
 * See notes above.
 */
extern qstring
qs_set_alias_qs(qstring qs, qstring src)
{
  return qs_set_alias_n(qs, qs_body(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Construct a qstring which is an alias for the given elstring.
 *
 * If n == 0, src may be NULL
 * If n > 0, src string MUST be at least 'n' bytes long.
 *
 * See notes above.
 */
extern qstring
qs_set_alias_els(qstring qs, elstring src)
{
  return qs_set_alias_n(qs, els_body(src), els_len(src)) ;
} ;

/*==============================================================================
 * Copying of qstring
 */

/*------------------------------------------------------------------------------
 * Copy one qstring to another -- allocating/extending as required.
 *
 * If both are NULL, returns NULL.
 * Otherwise if dst is NULL, creates a new qstring.
 *
 * If src is NULL it is treated as zero length, with 'cp' == 0.
 *
 * If src is not an alias, a copy is made to dst.
 * If src is an alias, dst becomes another alias for the same thing.
 *
 * If dst is an alias, that is discarded.
 *
 * Copies the src 'cp' to the dst.
 *
 * Returns: the destination qstring (allocated if required).
 */
extern qstring
qs_copy(qstring dst, qstring src)
{
  if (src == NULL)
    {
      qs_clear(dst) ;                   /* if dst not NULL, clear it    */
      return dst ;
    } ;

  if (src->alias)
    dst = qs_set_alias_qs(dst, src) ;
  else
    dst = qs_set_qs(dst, src) ;

  qs_set_cp_nn(dst, qs_cp_nn(src)) ;    /* copy in the src cp.          */

  return dst ;
} ;

/*==============================================================================
 * printf() and vprintf() type functions
 *
 * Allocate and/or extend qstring as required.
 *
 * Any alias is discarded.
 *
 *   Sets 'len'  = length of the '\0' terminated result (less the '\0').
 *   Sets 'cp'   = 0
 *
 * If fails and qs != NULL:
 *
 *   Sets 'len'  = 0
 *   Sets 'cp'   = 0
 *   But retains any existing body.
 *
 * Returns: address of qstring if OK
 *          NULL if failed (unlikely though that is)
 */

/*------------------------------------------------------------------------------
 * Formatted print to qstring -- cf printf()
 *
 * See notes above.
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
 * See notes above.
 */
extern qstring
qs_vprintf(qstring qs, const char *format, va_list args)
{
  va_list  ac ;
  int      slen ;
  qstring  qqs ;

  qqs = qs ;            /* NULL => need to make qs                      */
  if (qs == NULL)
    qqs = qs_new(0) ;   /* Sets size, cp & len = 0                      */
  else
    qs_clear(qqs) ;     /* Sets cp & len = 0, discard any alias, but
                           keep existing body                           */

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
      slen = vsnprintf(qs_body_nn(qqs), qqs->size, format, ac) ;
      va_end(ac);

      if (slen < 0)
        break ;                         /* failed                       */

      if ((usize)slen < qqs->size)
        {
          qs_set_len_nn(qqs, slen) ;
          return qqs ;                  /* succeeded                    */
        } ;

      qs_make_to_size(qqs, slen, 0) ;   /* need space for slen          */
    } ;

  /* Failed... discard anything that has been allocated.                */
  if (qs == NULL)
    qs_reset(qqs, free_it) ;

  return NULL ;
} ;

/*==============================================================================
 * Other operations
 */

/*------------------------------------------------------------------------------
 * Replace 'r' bytes at 'cp' by 'n' bytes -- extending if required.
 *
 * May increase or decrease 'len'. but does not affect 'cp'.
 *
 * If the given src is NULL, do not insert anything, just leave the space
 * ready for it.
 *
 * Returns: number of bytes beyond 'cp' that now exist.
 *
 * qstring MUST NOT be NULL
 *
 * If 'cp' > 'len', then sets 'len' = 'cp' first -- which will introduce
 * one or more undefined bytes.
 *
 * If this is a aliased qstring, a copy is made, so is no longer an alias.
 */
extern usize
qs_replace(qstring qs, usize r, const void* src, usize n)
{
  usize cp, len, nlen, after ;
  const char* ap ;
  char* np ;

  len  = qs_len_nn(qs) ;
  cp   = qs_cp_nn(qs) ;

  if ((cp + r) >= len)
    /* Replacing up to or beyond the end of the string                  */
    after = 0 ;
  else
    /* Replacing section short of the end of the string                 */
    after = len - (cp + r) ;

  nlen = cp + n + after ;
  if (nlen >= qs->b_size)
    qs_new_body(qs, nlen, qs->alias) ;  /* keeping any alias            */

  ap = np = qs->b_body ;

  if (qs->alias)
    {
      ap = qs_body_nn(qs) ;     /* copy from the alias                  */

      uint before = cp ;
      if (before > len)
        before = len ;

      if (before > 0)
        memmove(np, ap, before) ;

      qs_set_real_body_nn(qs) ;
    } ;

  if (after > 0)                /* move the after part before inserting */
    memmove(np + cp + n, ap + cp + r, after) ;

  if ((n > 0) && (src != NULL)) /* insert                               */
    memmove(np + cp, src, n) ;

  /* Set new 'len'                                                      */
  qs_set_len_nn(qs, nlen) ;

  return n + after ;
} ;

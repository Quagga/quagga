/* Length/String string handling
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
#include "misc.h"
#include <ctype.h>

#include "elstring.h"
#include "memory.h"

/*==============================================================================
 * Create and free
 */

/*------------------------------------------------------------------------------
 * Create a new elstring
 */
extern elstring
els_new(void)
{
  return XCALLOC(MTYPE_TMP, sizeof(elstring_t)) ;

  confirm(ELSTRING_INIT_ALL_ZEROS) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise or create a new elstring
 */
extern elstring
els_init_new(elstring els)
{
  if (els == NULL)
    els = els_new() ;
  else
    memset(els, 0, sizeof(elstring_t)) ;

  confirm(ELSTRING_INIT_ALL_ZEROS) ;

  return els ;
} ;

/*------------------------------------------------------------------------------
 * Release dynamically allocated elstring.
 *
 * Returns NULL.
 *
 * NB: it is the callers responsibility to free the contents of the elstring.
 *     if that is required, before freeing the elstring itself.
 */
extern elstring
els_free(elstring els)
{
  if (els != NULL)
    XFREE(MTYPE_TMP, els) ;

  return NULL ;
} ;

/*==============================================================================
 * Various comparisons
 */

/*------------------------------------------------------------------------------
 * Compare two elstrings -- returns the usual -ve, 0, +ve cmp result.
 *
 * NULL elstring is treated as empty.
 */
extern int
els_cmp(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  al, bl ;
  ulen  n ;

  ap = els_body(a) ;            /* NULL if a is NULL                    */
  bp = els_body(b) ;
  al = els_len(a) ;             /* zero if a is NULL                    */
  bl = els_len(b) ;

  n = (al <= bl) ? al : bl ;    /* min(al, bl)                          */

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
 * Compare elstring against word.
 *
 *   Returns:  -1 => elstring and word match to end of elstring
 *              0 => elstring and word match completely
 *             +1 => elstring and word do not match
 *
 * NULLs are treated as empty strings.
 *
 * An empty elstring will completely match an empty word.
 * An empty elstring will partly match any non-empty word.
 */
extern int
els_cmp_word(elstring a, const char* w)
{
  const uchar* ap, * ep ;
  ulen  al, wl ;

  al = els_len(a) ;                     /* zero if a is NULL            */
  wl = (w != NULL) ? strlen(w) : 0 ;    /* zero if w is NULL            */

  if (al > wl)
    return +1 ;                         /* no match if longer           */

  if (al == 0)
    return (wl == 0) ? 0 : -1 ;         /* exact or partial if empty    */

  ap = els_body_nn(a) ;

  /* Neither string is empty                                            */

  if (*ap != *w)
    return +1 ;                 /* quick out if no match for 1st char   */

  ep = ap + al - 1 ;
  while (ap < ep)
    {
      if (*++ap != *++w)
        return 1 ;
    } ;

  return al == wl ? 0 : -1 ;    /* full or partial match                */
} ;

/*------------------------------------------------------------------------------
 * Compare significant parts of two qstrings -- returns the usual -ve, 0, +ve
 * cmp result.
 *
 * By significant, mean excluding leading/trailing isspace() and treating
 * multiple isspace() as single isspace().
 *
 * Compares the 'len' portions of the strings.
 *
 * NULL elstring is treated as empty.
 */
extern int
els_cmp_sig(elstring a, elstring b)
{
  cpp_t ap ;
  cpp_t bp ;

  /* Set up pointers and dispense with leading and trailing isspace()
   *
   * Dummy up if NULL
   */
  els_cpp(ap, a) ;              /* NULL if a is NULL                    */

  while ((ap->p < ap->e) && isspace(*ap->p))
    ++ap->p ;
  while ((ap->p < ap->e) && isspace(*(ap->e - 1)))
    --ap->e ;

  els_cpp(bp, b) ;

  while ((bp->p < bp->e) && isspace(*bp->p))
    ++bp->p ;
  while ((bp->p < bp->e) && isspace(*(bp->e - 1)))
    --bp->e ;

  /* Now set about finding the first difference                         */
  while ((ap->p != ap->e) && (bp->p != bp->e))
    {
      if (isspace(*ap->p) && isspace(*bp->p))
        {
          do { ++ap->p ; } while (isspace(*ap->p)) ;
          do { ++bp->p ; } while (isspace(*bp->p)) ;
        } ;

      if (*ap->p != *bp->p)
        return (*ap->p < *bp->p) ? -1 : +1 ;

      ++ap->p ;
      ++bp->p ;
    } ;

  /* No difference before ran out of one or both                        */
  if      (ap->p != ap->e)
    return +1 ;
  else if (bp->p != bp->e)
    return -1 ;
  else
    return  0 ;
} ;

/*------------------------------------------------------------------------------
 * Are two elstrings equal ?  -- returns true if strings equal.
 */
extern bool
els_equal(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  n ;

  n = els_len(b) ;              /* zero if b is NULL    */
  if (n != els_len(a))
    return false ;

  ap = els_body(a) ;            /* NULL if a is NULL    */
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
extern bool
els_substring(elstring a, elstring b)
{
  const uchar* ap ;
  const uchar* bp ;
  ulen  n ;

  n = els_len(b) ;              /* zero if b is NULL    */
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


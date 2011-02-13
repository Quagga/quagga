/* Some primitive path handling
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

#include "qpath.h"
#include "qstring.h"

#include "zassert.h"

/*==============================================================================
 * Some primitive path handling, based on qstrings.
 *
 *
 *==============================================================================
 * Path Reduction
 *
 * As per POSIX, multiple '/' count as single '/', except for the very special
 * case of exactly two '/' at the start of a path.  (That case is referred to
 * here as a "double root".)
 *
 * So this code reduces runs of '/' to single '/' -- except for the special
 * case.
 *
 * This code also replaces "/./" by "/".
 *
 *
 */

/*------------------------------------------------------------------------------
 * Initialise a brand new qpath -- allocate if required.
 *
 * If a path is given, set that path -- allocating body even if path is zero
 * length.
 *
 * If no path is given, leaves qpath with no body.
 *
 * If path is given, the qpath is set and reduced (see above).
 *
 * Returns: address of qpath
 *
 * NB: assumes initialising a new structure.  If not, then caller should
 *     use qpath_reset() or qs_clear().
 */
extern qpath
qpath_init_new(qpath qp, const char* path)
{
  if (qp == NULL)
    qp = XCALLOC(MTYPE_QPATH, sizeof(qpath_t)) ;
  else
    memset(qp, 0, sizeof(qpath_t)) ;

  /* Worry about fields other than the path             */

  qs_init_new(qp->path, 0) ;

  if (path != NULL)
    qpath_set(qp, path) ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Reset contents of given qpath, freeing the structure if required.
 *
 * Discards all the contents of the qpath.
 */
extern qpath
qpath_reset(qpath qp, bool free_structure)
{
  if (qp == NULL)
    return NULL ;

  qs_reset_keep(&qp->path, keep_it) ;

  if (free_structure)
    XFREE(MTYPE_QPATH, qp) ;            /* sets qp = NULL       */
  else
    /* Worry about fields other than the path             */
    ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Set given qpath to copy of the given string -- allocate if required.
 *
 * If setting an existing qpath, discards any existing contents -- so the qpath
 * MUST have been initialised at some time (qpath_init_new).  Keeps any body
 * that has been allocated if possible.
 *
 * Reduces the path (see above).
 *
 * Sets the path len, but does not touch the path cp.
 */
extern qpath
qpath_set(qpath qp, const char* path)
{
  if (qp == NULL)
    qp = qpath_init_new(NULL, path) ;
  else
    {
      if (path != NULL)
        qs_set(&qp->path, path) ;
      else
        qs_clear(qp->path) ;
      /* Worry about fields other than the path             */
    } ;

  qpath_reduce(qp) ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Reduce multiple '/' to single '/' (except for exactly "//" at start).
 *
 * Reduce "/./" to "/".
 */
static void
qpath_reduce(qpath qp, size_t off)
{
  qpath   part ;
  qstring qs ;
  char*   sp ;
  char*   p ;
  char*   q ;

  if (qp == NULL)
    {
      assert(off == 0) ;
      return ;                  /* NULL qpath is empty                  */
    } ;

  qs = &qp->path ;
  assert(off <= qs->len) ;      /* Make sure 'off' is kosher            */

  sp = qs_chars(qs) ;           /* NULL if qpath is completely empty    */

  if (sp == NULL)
    return ;                    /* NULL path part is completely empty   */

  p = sp + off ;

  /* Deal with special case of "//" at start.
   *
   * If find "//x", where x is anything other than '/', step past the first
   * '/'.  Could step past both "//", but that stops it seeing "//./zzz"
   */
  if ((*p == '/') && (*(p + 1) == '/') && (*(p + 2) != '/'))
    ++p ;

  /* Scan to see if there is anything that needs to be fixed.
   *
   * Looking for "//" and "/./".
   */
  while (1)
    {
      if (*p++ == '\0')
        return ;                /* nothing to do if hit end of string   */

      if ( (*p == '/') || ((*p == '.') && (*(p + 1) == '/')) )
        {
          if (*(p - 1) == '/')
            break ;             /* found "//" or "/./"                  */
        }
    } ;

  /* Rats... there is something to be fixed.
   *
   * *p is second '/' of "//" or '.' of "/./".
   */
  q = p ;

  while (*p != '\0')
    {
      /* Step past any number of '/' and any number of "./".            */
      while (1)
        {
          while (*p == '/')
            ++p ;

          if ((*p != '.') || (*p != '/'))
            break ;

          p += 2 ;
        } ;

      /* Scan, copying stuff, until get to '\0' or find "//" or "/./"   */
      while (*p != '\0')
        {
          *q++ = *p++ ;         /* copy non-'\0'                        */

          if ( (*p == '/') || ((*p == '.') && (*(p + 1) == '/')) )
            {
              if (*(p - 1) == '/')
                break ;         /* found "//" or "/./"                  */
            } ;
        } ;
    } ;

  /* Adjust the length and terminate                                    */

  qs->len = (q - sp) ;      /* set the new length (shorter !)   */
  *q = '\0' ;               /* and terminate                    */
} ;

/*------------------------------------------------------------------------------
 * Make a copy of the given qpath.
 *
 * Creates a brand new qpath object, which is a full copy of the given one.
 *
 * The result is an empty qpath if the given one is NULL.
 */
extern qpath
qpath_copy(qpath qp_x)
{
  return qpath_init_new(NULL, qpath_path(qp_x)) ;
} ;

/*------------------------------------------------------------------------------
 * Make a copy of a qpath to the given qpath.
 *
 * If required, creates new qpath object -- so qpath_copy_to(NULL, ...) is the
 * same as qpath_copy(...).
 *
 * The result is an empty qpath if the given one is NULL.
 */
extern qpath
qpath_copy_to(qpath qp, qpath qp_x)
{
  return qpath_set(qp, qpath_path(qp_x)) ;
} ;

/*==============================================================================
 * Pop the last part of the given path.
 *
 * The cases are (where 'a' is anything except '/' and 'z' is anything):
 *
 *   1. ""        -- empty path
 *
 *                    - path unchanged
 *                    - return empty part
 *
 *   2. "aaa"     -- the path is a single part
 *
 *                    - part removed from path, leaves ""
 *                    - return the part
 *
 * Note that in (1) and (2) there is no '/', in the following there is at least
 * one '/'.
 *
 *   3. "/"       -- root only, or
 *      "//"         double root only
 *
 *                    - path unchanged
 *                    - return empty part
 *
 *   4. "/aaa"    -- one remaining part before the root,
 *      "//aaa"      or one remaining part after double route
 *
 *                    - part removed from path, leaves "/" or "//"
 *                    - return the part
 *
 *   5. "zzz/"    -- empty last part
 *
 *                    - "/" removed from end of path
 *                    - return empty part
 *
 *   6. "zzz/aaa" -- non-empty last part
 *
 *                    - part and "/" removed from end of path
 *                    - return part
 *
 * Note that other forms of multiple '/' have been reduced, already.
 */
extern qpath
qpath_pop(qpath qp)
{
  qpath   part ;
  qstring qs ;
  char*   sp ;
  char*   p ;

  /* Get pointers to start and end of path                              */
  if (qp != NULL)
    {
      qs = &qp->path ;

      sp = qs_chars(qs) ;       /* NULL if qpath is completely empty    */
      p  = qs_ep_char(qs) ;     /* points at trailing '\0'              */
    }
  else
    qs = sp = p = NULL ;

  /* Deal with NULL-ness                                                */
  if (sp == NULL)
    {
      assert(p == sp) ;         /* ie qs->len == 0 if qs->body == NULL  */

      return qpath_init_new(NULL, NULL) ;
    } ;

  /* Track back to last '/'                     */
  while ( (p > sp) && (*(p - 1) != '/') )
    --p ;

  /* Construct result which is from p forwards
   *
   * p == NULL if the given path is completely empty.
   */
  part = qpath_init_new(NULL, p) ;

  /* If what remains is not empty, and not just "/" or "//", remove trailing '/'
   *
   * If p == sp,     there was no '/', so path ends up empty.
   * If p == sp + 1, there is one '/', and that is at the front of the path
   * If p == sp + 2, there is either "//" or "a/"
   * If p >  sp + 2, there is "aa/"
   */
  if (p >= (sp + 2))    /* if "//", "a/" or "aa/" etc.          */
    {
      /* Unless is special case of "//"...                      */
      if ( ! ((p == (sp + 2)) && (*sp == '/')) )
        --p ;           /* ... discard trailing '/'             */
    } ;

  qs->len = (p - sp) ;  /* set the new length (shorter !)       */
  *p = '\0' ;           /* and terminate                        */

  /* Return the part we hacked off                              */
  return part ;
} ;

/*==============================================================================
 * Shift off the first part of the given path.
 *
 * The cases are (where 'a' is anything except '/' and 'z' is anything):
 *
 *   1. ""        -- empty path
 *
 *                    - path unchanged
 *                    - return empty part
 *
 *   2. "aaa"     -- the path is a single part
 *
 *                    - part removed from path, leaves ""
 *                    - return the part
 *
 * Note that in (1) and (2) there is no '/', in the following there is at least
 * one '/'.
 *
 *   3. "/"       -- root only, or
 *      "//"         double root
 *
 *                    - remove the "/" or "//" -- result is empty
 *                    - return "/" or "//" part
 *
 *   4. "/azz"    -- root followed by stuff, or
 *      "//zzz"      double root followed by stuff.
 *
 *                    - remove the "/" or "//"
 *                    - return "/" or "//" part
 *
 *   5. "aaa/"    -- something followed by first '/' which is end of path
 *
 *                    - remove upto and including '/' -- result is empty
 *                    - return upto but excluding '/'
 *
 *   6. "aaa/azz" -- something followed by first '/' followed by something else
 *
 *                    - remove upto and including '/'
 *                    - return upto but excluding '/'
 *
 * Note that other forms of multiple '/' have been reduced, already.
 */
extern qpath
qpath_shift(qpath qp)
{
  qpath   part ;
  qstring qs ;
  char*   sp ;
  char*   p ;
  char*   q ;

  /* Get pointers to start and end of path                              */
  if (qp != NULL)
    {
      qs = &qp->path ;
      sp = qs_chars(qs) ;       /* NULL if qpath is completely empty    */
    }
  else
    qs = sp = NULL ;

  /* Deal with NULL-ness                                                */
  if (sp == NULL)
    return qpath_init_new(NULL, NULL) ;

  p = sp ;

  /* Set p such that sp..p-1 is to be shifted off.
   * And q such that  q..end-1 is to be kept.
   */
  if (*sp == '/')
    {
      ++p ;             /* single root          */
      if (*p == '/')
        ++p ;           /* double root          */
      q = p ;
    }
  else
    {
      while ((*p != '/') && (*p != '/0'))
        ++p ;           /* step to '/' or end   */

      if (*p == '/')
        q = p + 1 ;     /* don't keep '/'       */
      else
        q = p ;         /* keep '\0' !          */
    } ;

  /* Construct qpath for shifted off stuff              */
  part = qpath_init_new(NULL, NULL) ;

  /* Hack off the shifted off stuff & '/' if required   */


  /* Return the part we hacked off                      */
  return part ;
} ;

/*------------------------------------------------------------------------------
 * Push one path onto the end of the given path.
 *
 * If the given path is NULL, creates a new, empty qpath to push onto.
 *
 * The given path is assumed to be the path to a "directory".  An empty
 * given path is treated as "the current directory".
 *
 * If the path to be pushed starts '/' or '~', then it is trimmed, removing
 * leading characters upto and including '/' (stopping at '\0' if no '/' found).
 *
 * If path to be pushed onto is not empty, and does not end '/', then an '/'
 * is appended before the path is pushed.
 *
 * Note that this means:
 *
 *   -- pushing an empty path or one which is just "/", will leave the path
 *      ending "/" -- unless the given path is empty.
 *
 *   -- cannot create a rooted path by pushing a path onto an empty path.
 *
 *   -- pushing a "homed" path "~...." is assumed to be pushing onto the
 *      required "home".
 *
 * The resulting path is reduced (see above).
 */

extern qpath
qpath_push(qpath qp, qpath qp_a)
{
  return qpath_push_str(qp, qpath_path(qp_a)) ;
} ;

/*------------------------------------------------------------------------------
 * Push path string onto the end of the given path.
 *
 * See above for discussion of "push" operation.
 */
extern qpath
qpath_push_str(qpath qp, const char* path)
{
  qstring   qs ;
  char*     ep ;
  char*     sp ;
  size_t    len ;
  size_t    off ;

  if (qp == NULL)
    qp = qpath_init_new(NULL, NULL) ;

  qs = &qp->path ;

  /* Trim the path to be pushed:
   *
   *   1. discard from any leading '~' to the first '/' or to '\0'.
   *
   *   2. then discard any leading '/'
   *
   *   3. then establish length of result.
   */
  if (path != NULL)
    {
      if (*path == '~')
        do
          {
            ++path ;
          } while ((*path != '/') && (*path != '\0')) ;

      while (*path == '/')
        ++path ;                /* Step past leading '/'                */
      len = strlen(path) ;
    }
  else
    len = 0 ;

  /* Worry about whether need to add a '/' to the path before pushing   */
  sp  = qs_char(qs) ;
  ep  = qs_ep_char(qs) ;        /* points at trailing '\0'              */

  if (sp == NULL)
    assert(ep == sp) ;          /* ie qs->len == 0 if qs->body == NULL  */

  off = qs->len ;               /* where new stuff starts               */

  if (ep != sp)
    {
      if (*(ep - 1) == '/')
        --off ;                 /* step back to the '/'                 */
      else
        {
          /* Destination is not empty and does not end '/', so append one.
           *
           * Note that we ensure there is space for the path which are
           * about to push, so at most one allocation required.
           */
          qs_need(qs, (ep - sp) + 1 + len) ;
          qs_append_n(qs, "/", 1) ;
        } ;
    } ;

  /* Now push path                                                      */
  qs_append_n(qs, path, len) ;

  /* Reduce the new part of the result, and return
   *
   * Note that the 'off' points at the '/' which precedes the new stuff.
   * So will spot "/./" where the new stuff starts "./".
   */
  qpath_reduce(qp, off) ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Join two paths to create a new path.
 *
 * Copies the destination path and then pushes the other path onto it.
 */
extern qpath
qpath_join(qpath qp, qpath qp_a)
{
  qpath qp_n ;

  qp_n = qpath_copy(qp) ;
  return qpath_push(qp_n, qp_a) ;
} ;

/*------------------------------------------------------------------------------
 * Join path string to the given path to create a new path.
 *
 * Copies the destination path and then pushes the path string onto it.
 */
extern qpath
qpath_join_str(qpath qp, const char* path)
{
  qpath qp_n ;

  qp_n = qpath_copy(qp) ;
  return qpath_push_str(qp_n, path) ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start and end '/' ?
 */
extern bool
qpath_sex(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return qp_empty ;           /* NULL qpath is empty          */

  sp  = qs_chars(&qp->path) ;

  if ((sp == NULL) || (*sp == '\0'))
    return qp_empty ;           /* NULL body or just '\0'       */

  if (*sp == '~')
    return qp_homed ;

  if (*sp == '/')
    {
      ++sp ;
      if (*sp == '\0')
        return qp_root ;
      if (*sp != '/')
        return qp_absolute ;

      ++sp ;
      if (*sp == '\0')
        return qp_double_root ;
      else
        return qp_double_absolute ;
    } ;



  qp_empty,             /* nothing at all                       */
  qp_relative,          /* something, not starting '/'          */
  qp_root,              /* "/" all on its own                   */
  qp_absolute,          /* something, starting with single '/'  */
  qp_homed,             /* something, starting with '~'         */
  qp_double_root,       /* "//" all on its own                  */
  qp_double_absolute    /* something, starting with "//"        */



  return ((qp->path).len == 1) && (*sp == '/') ;
} ;

/*------------------------------------------------------------------------------
 * Is there anything there ?
 */
extern bool
qpath_is_empty(qpath qp)
{
  if (qp == NULL)
    return true ;               /* NULL qpath is empty          */

  return (qp->path).len == 0 ;
} ;

/*------------------------------------------------------------------------------
 * Is it: not empty, not starting '/' (or '//') and not starting '~'
 */
extern bool
qpath_is_relative(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath is not relative   */

  sp = qs_chars(&qp->path) ;

  return (sp != NULL) && (*sp != '/') && (*sp != '.') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start and end '/' ?
 */
extern bool
qpath_is_root(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be root    */

  sp = qs_chars(&qp->path) ;

  return ((qp->path).len == 1) && (*sp == '/') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start '/' (and not '//') ?
 *
 * Note that just "/" (ie root) will return true => it is also absolute.
 */
extern bool
qpath_is_absolute(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be absolute        */

  sp = qs_chars(&qp->path) ;

  return (sp != NULL) && (*sp == '/') && (*(sp + 1) != '/') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start '~'
 */
extern bool
qpath_is_homed(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be homed   */

  sp = qs_chars( &qp->path) ;

  return (sp != NULL) && (*sp == '~') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start and end '/' ?
 */
extern bool
qpath_is_double_root(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be root    */

  sp = qs_chars(&qp->path) ;

  return ((qp->path).len == 2) && (*sp == '/') && (*(sp + 1) == '/') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path start '/' (and not '//') ?
 *
 * Note that just "/" (ie root) will return true => it is also absolute.
 */
extern bool
qpath_is_double_absolute(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be absolute        */

  sp = qs_chars(&qp->path) ;

  return (sp != NULL) && (*sp == '/') && (*(sp + 1) == '/') ;
} ;

/*------------------------------------------------------------------------------
 * Does the given path *end* '/', or is it ~aaaa with no '/' at all.
 *
 * Note that root and double route return directory true.
 */
extern bool
qpath_is_directory(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be directory       */

  sp = qs_chars(&qp->path) ;

  if ((sp == NULL) || (*sp = '\0'))
    return false ;              /* Empty qpath cannot be directory      */

  ep = qs_ep_char(qs) ;

  if (*(ep - 1) == '/')
    return true ;               /* Ends '/'                             */

  if (*sp == '~')
    {
      while (*(++sp) != '/')
        {
          if (*sp == '\0')      /* Starts '~' and no '/' found          */
            return true ;
        } ;
    } ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Is the given path an atom
 *
 * Note that root and double route are .
 */
extern bool
qpath_is_atom(qpath qp)
{
  char*     sp ;

  if (qp == NULL)
    return false ;              /* NULL qpath cannot be directory       */

  sp = qs_chars(&qp->path) ;

  if ((sp == NULL) || (*sp = '\0'))
    return false ;              /* Empty qpath cannot be directory      */

  ep = qs_ep_char(qs) ;

  if (*(ep - 1) == '/')
    return true ;               /* Ends '/'                             */

  if (*sp == '~')
    {
      while (*(++sp) != '/')
        {
          if (*sp == '\0')      /* Starts '~' and no '/' found          */
            return true ;
        } ;
    } ;

  return false ;
} ;


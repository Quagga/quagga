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

#include <unistd.h>
#include <errno.h>

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

static void qpath_reduce(qpath qp) ;

/*------------------------------------------------------------------------------
 * Initialise a brand new qpath -- allocate if required.
 *
 * The result is a path with a not-empty body.  All qpath values may be assumed
 * to have a body at all times.
 *
 * Returns: address of qpath
 *
 * NB: assumes initialising a new structure.  If not, then caller should
 *     use qpath_reset() or qpath_clear().
 */
extern qpath
qpath_init_new(qpath qp)
{
  if (qp == NULL)
    qp = XCALLOC(MTYPE_QPATH, sizeof(qpath_t)) ;
  else
    memset(qp, 0, sizeof(qpath_t)) ;

  qs_init_new(qp->path, 50) ;           /* Always have a body   */

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Create a new qpath if don't already have one.
 */
inline static qpath
qpath_make_if_null(qpath qp)
{
  if (qp == NULL)
    qp = qpath_init_new(NULL) ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Reset contents of given qpath, freeing the structure if required.
 *
 * Discards all the contents of the qpath.
 */
extern qpath
qpath_reset(qpath qp, free_keep_b free_structure)
{
  if (qp == NULL)
    return NULL ;

  qs_reset(qp->path, keep_it) ;

  if (free_structure)
    XFREE(MTYPE_QPATH, qp) ;            /* sets qp = NULL       */
  else
    ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Clear down given qpath -- retaining any body, but setting it empty.
 */
extern qpath
qpath_clear(qpath qp)
{
  if (qp == NULL)
    return NULL ;

  qs_clear(qp->path) ;

  return qp ;
} ;

/*------------------------------------------------------------------------------
 * Set given qpath to copy of the given string -- allocate if required.
 *
 * Reduces the path (see above).
 */
extern qpath
qpath_set(qpath dst, const char* src)
{
  return qpath_set_n(dst, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Set given qpath to copy of the given qstring -- allocate if required.
 *
 * Reduces the path (see above).
 */
extern qpath
qpath_set_qs(qpath dst, const qstring src)
{
  return qpath_set_n(dst, qs_char(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Set given qpath to copy of the given string -- allocate if required.
 *
 * Reduces the path (see above).
 */
extern qpath
qpath_set_n(qpath dst, const char* src, ulen n)
{
  dst = qpath_make_if_null(dst) ;

  qs_set_n(dst->path, src, n) ;

  qpath_reduce(dst) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Copy qpath to the given qpath -- creating qpath if required.
 * The result is an empty qpath if the given one is NULL.
 */
extern qpath
qpath_copy(qpath dst, const qpath src)
{
  if (src != NULL)
    return qpath_set_n(dst, qs_char_nn(src->path), qs_len_nn(src->path)) ;
  else
    return qpath_set_n(dst, NULL, 0) ;
} ;

/*==============================================================================
 * Interfaces to system.
 */

/*------------------------------------------------------------------------------
 * Get the current working directory -- creates a qpath if required.
 *
 * Returns:  the (new) qpath if OK
 *           NULL if not OK -- any existing qpath is cleared.
 *
 * If fails will be because some directory on the way back to the root is
 * not readable or searchable (!).
 */
extern qpath
qpath_getcwd(qpath dst)
{
  qpath  od ;

  od = dst ;
  dst = qpath_make_if_null(dst) ;

  qs_new_size(dst->path, 50) ;

  while (1)
    {
      void*  r ;
      usize  s ;

      s = qs_size_nn(dst->path) ;
      r = getcwd(qs_char_nn(dst->path), s) ;

      if (r != NULL)
        {
          qs_set_strlen_nn(dst->path) ;
          return dst ;                  /* exit here if OK.             */
        } ;

      if (errno != ERANGE)
        break ;                         /* exit here on failure         */

      qs_new_size(dst->path, s * 2) ;
    } ;

  if (od == NULL)
    qpath_reset(dst, free_it) ;
  else
    qpath_clear(dst) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set the current working directory
 *
 * Returns:  0 <=> OK
 *           errno otherwise
 */
extern int
qpath_setcwd(qpath qp)
{
  return (chdir(qpath_string(qp)) == 0) ? 0 : errno ;
} ;

/*------------------------------------------------------------------------------
 * Do "stat" for given path.
 *
 * Returns:  0 <=> OK
 *           errno otherwise
 */
extern int
qpath_stat(qpath qp, struct stat* sbuf)
{
  return (stat(qpath_string(qp), sbuf) == 0) ? 0 : errno ;
} ;

/*------------------------------------------------------------------------------
 * Is given path a file we might access -- according to stat ?
 *
 * Returns: -1 <=> no    -- can access etc, but it's not a file
 *           0 <=> yes
 *           errno otherwise
 */
extern int
qpath_stat_is_file(qpath qp)
{
  struct stat sbuf[1] ;
  int err ;

  err = qpath_stat(qp, sbuf) ;

  return (err == 0) ? (S_ISREG(sbuf->st_mode) ? 0 : -1)
                    : err ;
} ;

/*------------------------------------------------------------------------------
 * Is given path a directory we might access -- according to stat ?
 *
 * Returns: -1 <=> no    -- can access etc, but it's not a directory
 *           0 <=> yes
 *           errno otherwise
 */
extern int
qpath_stat_is_directory(qpath qp)
{
  struct stat sbuf[1] ;
  int err ;

  err = qpath_stat(qp, sbuf) ;

  return (err == 0) ? (S_ISDIR(sbuf->st_mode) ? 0 : -1)
                    : err ;
} ;

/*==============================================================================
 * Path editing functions
 *
 *
 */

/*------------------------------------------------------------------------------
 * Shave any file part off the end of the given path.
 *
 * This treats anything after the last '/' of the path as being the "file part".
 *
 * The cases are (where 'a' is anything except '/' and 'z' is anything):
 *
 *   1. ""        -- empty                -> unchanged
 *
 *   2. "aaa"     -- file part only       -> ""
 *
 *   3. "/"       -- root only, or        -> unchanged
 *      "//"         double root only     -> unchanged
 *
 *   4. "/aaa"    -- routed file          -> "/"
 *      "//aaa"      double routed file   -> "//"
 *
 *   5. "zzz/"    -- no file part         -> unchanged
 *
 *   6. "zzz/aaa" -- non-empty file part  -> "zzz/"
 *
 * Ensures that the path is "reduced" before shaving.
 *
 * Creates a new, empty path if qp is NULL.
 */
extern qpath
qpath_shave(qpath qp)
{
  pp_t p ;

  qp = qpath_make_if_null(qp) ;

  qs_pp_nn(p, qp->path) ;

  /* Track back to last '/'                     */
  while ( (p->e > p->p) && (*(p->e - 1) != '/') )
    --p->e ;

  qs_set_len_nn(qp->path, p->e - p->p) ;

  return qp ;
} ;

#if 0

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
qpath_pop(qpath to, qpath from)
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
#endif

/*==============================================================================
 * Append, Prepend and Complete
 */

static ulen qpath_trim_home(const char* p, ulen n) ;

/*------------------------------------------------------------------------------
 * Append one path (src) onto the end of the given path (dst).
 *
 * If the dst path is NULL, creates a new, empty qpath to append to.
 *
 * The dst path is assumed to be the path to a "directory".  An empty dst path
 * is treated as "the current directory".
 *
 * If src path starts '/' or '~', then it is trimmed, removing leading
 * characters up to and including '/'.
 *
 * If dst path is not empty, and does not end '/', then an '/' is appended
 * before the src is appended.
 *
 * Note that this means:
 *
 *   -- appending an empty path or one which is just "/", will leave the dst
 *      path ending "/" -- unless the dst path is empty.
 *
 *   -- cannot create a rooted path by appending a path onto an empty path.
 *
 *   -- appending a "homed" path "~..../" is assumed to be appending to the
 *      required "home".
 *
 * The resulting path is reduced (see above).
 */
extern qpath
qpath_append(qpath dst, const qpath src)
{
  if (src != NULL)
    return qpath_append_str_n(dst, qs_char_nn(src->path),
                                   qs_len_nn(src->path)) ;
  else
    return qpath_append_str_n(dst, NULL, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Append src qstring onto the end of the dst path.
 *
 * See above for discussion of "append" operation.
 */
extern qpath
qpath_append_qs(qpath dst, const qstring src)
{
  return qpath_append_str_n(dst, qs_char(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Append src string onto the end of the dst path.
 *
 * See above for discussion of "append" operation.
 */
extern qpath
qpath_append_str(qpath dst, const char* src)
{
  return qpath_append_str_n(dst, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Append src string of given length onto the end of the dst path.
 *
 * See above for discussion of "append" operation.
 */
extern qpath
qpath_append_str_n(qpath dst, const char* src, ulen n)
{
  ulen l ;

  dst = qpath_make_if_null(dst) ;

  /* Trim the path to be appended:
   *
   *   1. discard from any leading '~' to the first '/' or to '\0'.
   *
   *   2. then discard any leading '/'
   *
   *   3. then establish length of result.
   */
  l = n ;
  n = qpath_trim_home(src, n) ;
  src += (l - n) ;              /* step past stuff trimmed              */

  /* Worry about whether need to add a '/' to the path before pushing   */
  if (qs_len_nn(dst->path) != 0)
    {
      if (*(qs_ep_char_nn(dst->path) - 1) != '/')
        qs_append_str_n(dst->path, "/", 1) ;
    } ;

  /* Now append the src                                                 */
  qs_append_str_n(dst->path, src, n) ;

  /* Reduce the new part of the result, and return                      */
  qpath_reduce(dst) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Extend given dst path by simply affixing the given src path.
 *
 * Does not introduce any '/' or any other stuff.
 *
 * The resulting path is reduced (see above).
 */
extern qpath
qpath_extend(qpath dst, const qpath src)
{
  if (src != NULL)
    return qpath_extend_str_n(dst, qs_char_nn(src->path),
                                   qs_len_nn(src->path)) ;
  else
    return qpath_extend_str_n(dst, NULL, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Extend given dst path by simply affixing the given src qstring.
 */
extern qpath
qpath_extend_qs(qpath dst, const qstring src)
{
  return qpath_extend_str_n(dst, qs_char(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Extend given dst path by simply affixing the given src string.
 */
extern qpath
qpath_extend_str(qpath dst, const char* src)
{
  return qpath_extend_str_n(dst, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Extend given dst path by simply affixing the given src string of the given
 * length.
 */
extern qpath
qpath_extend_str_n(qpath dst, const char* src, ulen n)
{
  dst = qpath_make_if_null(dst) ;

  qs_append_str_n(dst->path, src, n) ;

  qpath_reduce(dst) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Prepend src path onto front of dst path.
 *
 * Like append, where the dst ends up being the dst appended to the src.
 */
extern qpath
qpath_prepend(qpath dst, const qpath src)
{
  if (src != NULL)
    return qpath_prepend_str_n(dst, qs_char_nn(src->path),
                                    qs_len_nn(src->path)) ;
  else
    return qpath_prepend_str_n(dst, NULL, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Prepend src qstring onto front of dst path.
 *
 * Like append, where the dst ends up being the dst appended to the src.
 */
extern qpath
qpath_prepend_qs(qpath dst, const qstring src)
{
  return qpath_prepend_str_n(dst, qs_char(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Prepend src string onto front of dst path.
 *
 * Like append, where the dst ends up being the dst appended to the src.
 */
extern qpath
qpath_prepend_str(qpath dst, const char* src)
{
  return qpath_prepend_str_n(dst, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * Prepend src string of given length onto front of dst path.
 *
 * Like append, where the dst ends up being the dst appended to the src.
 */
extern qpath
qpath_prepend_str_n(qpath dst, const char* src, ulen n)
{
  char* p ;
  ulen  r ;
  bool  need_slash ;

  dst = qpath_make_if_null(dst) ;

  /* Trim the path to be prepended to:
   *
   *   1. discard from any leading '~' to the first '/' (or end).
   *
   *   2. then discard any leading '/'
   *
   *   3. then establish length of any part to be replaced.
   */
  r = qs_len_nn(dst->path) ;
  r -= qpath_trim_home(qs_char_nn(dst->path), r) ;

  /* Worry about whether need to add a '/' to the path before pushing   */
  need_slash = (n > 0) && (*(src + n - 1) != '/') ;

  /* Make room for src and possible slash in qstring                    */
  qs_set_cp_nn(dst->path, 0) ;
  qs_replace(dst->path, r, NULL, n + (need_slash ? 1 : 0)) ;

  /* Now copy in the src                                                */
  p = qs_char_nn(dst->path) ;

  if (n > 0)
    memmove(p, src, n) ;

  if (need_slash)
    *(p + n) = '/' ;

  /* Reduce the new part of the result, and return                      */
  qpath_reduce(dst) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Make a qpath from the given string, completing it, if required, by
 * prepending the given directory qpath.
 */
extern qpath
qpath_make(const char* src, const qpath dir)
{
  if (*src == '/')
    return qpath_set(NULL, src) ;

  return qpath_append_str(qpath_dup(dir), src) ;
} ;

/*------------------------------------------------------------------------------
 * If given dst path is not rooted (does not start with '/', prepend the
 * given src path to it.  Result is reduced.
 */
extern qpath
qpath_complete(qpath dst, const qpath src)
{
  if (src != NULL)
    return qpath_prepend_str_n(dst, qs_char_nn(src->path),
                                    qs_len_nn(src->path)) ;
  else
    return qpath_prepend_str_n(dst, NULL, 0) ;
} ;

/*------------------------------------------------------------------------------
 * If given dst path is not rooted (does not start with '/', prepend the
 * given src qstring to it.  Result is reduced.
 */
extern qpath
qpath_complete_qs(qpath dst, const qstring src)
{
  return qpath_complete_str_n(dst, qs_char(src), qs_len(src)) ;
} ;

/*------------------------------------------------------------------------------
 * If given dst path is not rooted (does not start with '/', prepend the
 * given src string to it.  Result is reduced.
 */
extern qpath
qpath_complete_str(qpath dst, const char* src)
{
  return qpath_prepend_str_n(dst, src, (src != NULL) ? strlen(src) : 0) ;
} ;

/*------------------------------------------------------------------------------
 * If given dst path is not rooted (does not start with '/', prepend the
 * given src string of given length to it.  Result is reduced.
 */
extern qpath
qpath_complete_str_n(qpath dst, const char* src, ulen n)
{
  dst = qpath_make_if_null(dst) ;

  if ((qs_len_nn(dst->path) == 0) || (*(qs_char_nn(dst->path)) == '/'))
    qpath_prepend_str_n(dst, src, n) ;
  else
    qpath_reduce(dst) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Trim leading '~' up to and including one or more '/'.
 *
 * Return remaining length after the trim.
 */
static ulen
qpath_trim_home(const char* p, ulen n)
{
  if ((n > 0) && (*p == '~'))
    {
      do                        /* Step past leadin '~' to first '/'    */
        {
          ++p ;
          --n ;
        } while ((n > 0) && (*p != '/')) ;
    } ;

  while ((n > 0) && (*p == '/'))
    {
      ++p ;                     /* Step past leading '/'                */
      --n ;
    } ;

  return n ;
} ;

/*==============================================================================
 *
 */

#if 0

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

#endif

/*==============================================================================
 *
 *
 *
 */
/*------------------------------------------------------------------------------
 * Reduce multiple '/' to single '/' (except for exactly "//" at start).
 *
 * Reduce "/./" to "/".
 */
static void
qpath_reduce(qpath qp)
{
  char*   sp ;
  char*   p ;
  char*   q ;

  sp = qs_make_string(qp->path) ;
  p = sp ;

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
      if (*p == '\0')
        return ;                /* scanned to end                       */

      if (*p++ != '/')          /* scanning for '/'                     */
        continue ;

      if (*p == '/')
        break ;                 /* second '/'                           */

      if (*p != '.')
        continue ;              /* not "//" and not "/."                */

      if (*(p+1) == '/')
        break ;                 /* found "/./"                          */
    } ;

  /* Rats... there is something to be fixed.
   *
   * *p is second '/' of "//" or '.' of "/./".
   */
  q = p ;                       /* keep the first '/'                   */

  while (*p != '\0')
    {
      ++p ;                     /* step past '.' or second '/'          */

      /* Step past any number of '/' and any number of "./".            */
      while (*p != '\0')
        {
          while (*p == '/')     /* eat any number of these              */
            ++p ;

          if (*p != '.')        /* done if not '.'                      */
            break ;

          if (*(p+1) != '/')    /* done if not "./"                     */
            break ;

          p += 2 ;              /* Step past "./"                       */
        } ;

      /* Here we have *p which is not '/' and not "./", so unless is '\0'
       * there is at least one character to move across.
       *
       * Copying stuff, until get to '\0' or find "//" or "/./"
       */
      while (*p != '\0')
        {
          *q++ = *p ;           /* copy non-'\0'                        */

          if (*p++ != '/')
            continue ;          /* keep going if wasn't '/'             */

          if (*p == '/')
            break ;             /* second '/'                           */

          if (*p != '.')
            continue ;          /* not "//" and not "/."                */

          if (*(p+1) == '/')
            break ;             /* found "/./"                          */
        } ;
    } ;

  /* Adjust the length and terminate                                    */

  qs_set_len_nn(qp->path, q - sp) ;   /* set the new length (shorter !) */
} ;





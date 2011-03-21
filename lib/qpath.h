/* Some primitive path handling -- header
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

#ifndef _ZEBRA_QPATH_H
#define _ZEBRA_QPATH_H

#include "misc.h"
#include "qstring.h"
#include "memory.h"
#include "sys/stat.h"

/*==============================================================================
 * For these purposes a path is "parts" separated by one more '/' characters.
 *
 * As per POSIX, a pair of leading "//" (not three or more leading '/') is
 * very special.
 *
 * The following sexes of qpath are established after any "path reduction"
 * has been done.  Path reduction removes extraneous '/'.
 */

typedef enum qpath_sex qpath_sex_t ;

enum qpath_sex
{
  qp_empty,             /* nothing at all                       */
  qp_relative,          /* something, not starting '/'          */
  qp_root,              /* "/" all on its own                   */
  qp_absolute,          /* something, starting with single '/'  */
  qp_homed,             /* something, starting with '~'         */
  qp_double_root,       /* "//" all on its own                  */
  qp_double_absolute    /* something, starting with "//"        */
} ;

/* The qpath structure is largely a qstring, but one in which there is always
 * a body, even if it only contains "\0", and the len is kept up to date.
 */
struct qpath
{
  qstring_t     path ;          /* Embedded     */
} ;

typedef struct qpath  qpath_t[1] ;
typedef struct qpath* qpath ;

/*==============================================================================
 * Functions
 */

extern qpath qpath_init_new(qpath qp) ;
extern qpath qpath_reset(qpath qp, free_keep_b free_structure) ;
extern qpath qpath_clear(qpath qp) ;

Inline qpath qpath_new(void) ;
Inline qpath qpath_free(qpath qp) ;

Inline const char* qpath_string(qpath qp) ;
Inline char* qpath_char_string(qpath qp) ;
Inline ulen qpath_len(qpath qp) ;
Inline const qstring qpath_qs(qpath qp) ;

extern qpath qpath_set(qpath dst, const char* src) ;
extern qpath qpath_set_n(qpath dst, const char* src, ulen n) ;
extern qpath qpath_set_qs(qpath dst, const qstring src) ;
extern qpath qpath_copy(qpath dst, const qpath src) ;
Inline qpath qpath_dup(const qpath qp) ;
Inline qpath qpath_dup_str(const char* src) ;

extern qpath qpath_getcwd(qpath dst) ;
extern int qpath_setcwd(qpath dst) ;
extern int qpath_stat(qpath qp, struct stat* stat) ;
extern int qpath_stat_is_file(qpath qp) ;
extern int qpath_stat_is_directory(qpath qp) ;

extern qpath qpath_shave(qpath qp) ;

extern qpath qpath_append(qpath dst, const qpath src) ;
extern qpath qpath_append_qs(qpath dst, const qstring src) ;
extern qpath qpath_append_str(qpath dst, const char* src) ;
extern qpath qpath_append_str_n(qpath dst, const char* src, ulen n) ;

extern qpath qpath_extend(qpath dst, const qpath src) ;
extern qpath qpath_extend_qs(qpath dst, const qstring src) ;
extern qpath qpath_extend_str(qpath dst, const char* src) ;
extern qpath qpath_extend_str_n(qpath dst, const char* src, ulen n) ;

extern qpath qpath_prepend(qpath dst, const qpath src) ;
extern qpath qpath_prepend_qs(qpath dst, const qstring src) ;
extern qpath qpath_prepend_str(qpath dst, const char* src) ;
extern qpath qpath_prepend_str_n(qpath dst, const char* src, ulen n) ;

extern qpath qpath_make(const char* src, const qpath dir) ;

extern qpath qpath_complete(qpath dst, const qpath src) ;
extern qpath qpath_complete_qs(qpath dst, const qstring src) ;
extern qpath qpath_complete_str(qpath dst, const char* src) ;
extern qpath qpath_complete_str_n(qpath dst, const char* src, ulen n) ;

/*==============================================================================
 * Inline stuff
 */

/*------------------------------------------------------------------------------
 * Create new qpath.
 */
Inline qpath
qpath_new(void)
{
  return qpath_init_new(NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Free qpath.
 */
Inline qpath
qpath_free(qpath qp)
{
  return qpath_reset(qp, free_it) ;
} ;

/*------------------------------------------------------------------------------
 * Duplicate qpath -- result will need to be freed.
 */
Inline qpath
qpath_dup(const qpath qp)
{
  return qpath_copy(NULL, qp) ;
} ;

/*------------------------------------------------------------------------------
 * Duplicate string as a qpath -- result will need to be freed.
 */
Inline qpath
qpath_dup_str(const char* src)
{
  return qpath_set(NULL, src) ;
} ;

/*------------------------------------------------------------------------------
 * Get *temporary* pointer to actual path contained in the given qpath.
 *
 * This is *temporary* to the extent that when the qpath is changed or freed,
 * this pointer will be INVALID -- you have been warned.
 *
 * This is a *const* pointer.
 *
 * For a NULL qpath, or an empty qpath, returns pointer to an empty string
 * ('\0' terminated "").
 */
Inline const char*
qpath_string(qpath qp)
{
  return (qp != NULL) ? qs_make_string(qp->path) : "" ;
} ;

/*------------------------------------------------------------------------------
 * Get *temporary* pointer to actual path contained in the given qpath.
 *
 * This is *temporary* to the extent that when the qpath is changed or freed,
 * this pointer will be INVALID -- you have been warned.
 *
 * This is a *const* pointer.
 *
 * qpath may *not* be NULL qpath.
 */
Inline char*
qpath_char_string(qpath qp)
{
  return qs_make_string(qp->path) ;
} ;

/*------------------------------------------------------------------------------
 * Get length of the given qpath -- zero if NULL
 */
Inline ulen
qpath_len(qpath qp)
{
  return (qp != NULL) ? qs_len_nn(qp->path) : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get *temporary* pointer to qstring rendering of the given path.
 *
 * This is *temporary* to the extent that when the qpath is changed or freed,
 * this pointer will be INVALID -- you have been warned.
 *
 * This is a *const* pointer.
 *
 * For a NULL qpath returns NULL qstring.
 */
Inline const qstring
qpath_qs(qpath qp)
{
  return (qp != NULL) ? qp->path : NULL ;
} ;

#endif /* _ZEBRA_QPATH_H */

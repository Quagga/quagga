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

#include "zebra.h"
#include "misc.h"
#include "memory.h"







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

typedef struct qpath  qpath_t ;
typedef struct qpath* qpath ;

/* The qpath structure is largely a qstring, but one in which there is always
 * a body, even if it only contains "\0", and the len is kept up to date.
 */
struct qpath
{
  qstring_t     path ;
} ;

/*==============================================================================
 * Functions
 */

extern qpath
qpath_init_new(qpath qp, const char* path) ;

extern qpath
qpath_reset(qpath qp, bool free_structure) ;

Inline qpath
qpath_reset_free(qpath qp) { qpath_reset(qp, true) ; } ;

Inline qpath
qpath_reset_keep(qpath qp) { qpath_reset(qp, false) ; } ;

Inline const char*
qpath_path(qpath qp) ;

extern qpath
qpath_trim(qpath qp) ;

extern qpath
qpath_copy(qpath qp) ;

extern qpath
qpath_pop(qpath qp) ;

extern qpath
qpath_push(qpath qp, qpath qp_a) ;

extern qpath
qpath_join(qpath qp, qpath qp_a) ;

/*==============================================================================
 * Inline stuff
 */

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
qpath_path(qpath qp)
{
  return qs_string(qp != NULL ? &qp->path : qp) ;
} ;

#endif /* _ZEBRA_QPATH_H */

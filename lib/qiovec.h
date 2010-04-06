/* Flexible iovec  -- header
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

#ifndef _ZEBRA_QIOVEC_H
#define _ZEBRA_QIOVEC_H

#include "zebra.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Flexible size "struct iovec"
 *
 * NB: a completely zero structure is a valid, empty qiovec.
 */
typedef struct qiovec* qiovec ;
typedef struct qiovec  qiovec_t ;
struct qiovec
{
  struct iovec* vec ;           /* the actual iovec array       */

  bool          writing ;       /* started, but not finished    */

  unsigned      i_get ;         /* next entry to get            */
  unsigned      i_put ;         /* next entry to put            */

  unsigned      i_alloc ;       /* number of entries allocated  */
} ;

/*==============================================================================
 * Functions
 */

extern qiovec qiovec_init_new(qiovec qiov) ;
extern qiovec qiovec_reset(qiovec qiov, bool free_structure) ;

#define qiovec_reset_keep(qiov) qiovec_reset(qiov, 0)
#define qiovec_reset_free(qiov) qiovec_reset(qiov, 1)

Inline bool qiovec_empty(qiovec qiov) ;
extern void qiovec_clear(qiovec qiov) ;
extern void qiovec_push(qiovec qiov, const void* base, size_t len) ;
extern int qiovec_write_nb(int fd, qiovec qiov) ;

extern int iovec_write_nb(int fd, struct iovec* p_iov, int n) ;
Inline void iovec_set(struct iovec* p_iov, const void* base, size_t len) ;

/*------------------------------------------------------------------------------
 * Is given qiov empty ?
 *
 * NB: arranges to never add zero length entries to the iovec vector, so
 *     is empty when there are no active entries.
 */
Inline bool
qiovec_empty(qiovec qiov)
{
  return (qiov->i_get == qiov->i_put) ;
} ;

/*------------------------------------------------------------------------------
 * Set a given struct iovec
 *
 * Gets around the fact that the standard structure does not have a const
 * pointer !
 */
#include "miyagi.h"

Inline void
iovec_set(struct iovec* p_iov, const void* base, size_t len)
{
  p_iov->iov_base = miyagi(base) ;
  p_iov->iov_len  = len ;
} ;

#endif /* _ZEBRA_QIOVEC_H */

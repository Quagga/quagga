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

#include "misc.h"
#include <sys/uio.h>            /* iovec stuff                  */

/*==============================================================================
 * Alternative naming for struct iovec and its entries.
 */
struct qiov_item
{
  const char*  base ;
  size_t       len ;
} ;

typedef struct qiov_item* qiov_item ;
typedef struct qiov_item  qiov_item_t[1] ;
typedef struct qiov_item* qiov_vector ;

union
{
  struct qiov_item   q ;
  struct iovec       s ;
} union_iovec ;

CONFIRM(sizeof(struct qiov_item) == sizeof(struct iovec)) ;

CONFIRM(offsetof(struct qiov_item, base) == offsetof(struct iovec, iov_base)) ;
CONFIRM(sizeof  (union_iovec.q.    base) == sizeof  (union_iovec.s.iov_base)) ;
CONFIRM(offsetof(struct qiov_item, len)  == offsetof(struct iovec, iov_len)) ;
CONFIRM(sizeof  (union_iovec.q.    len)  == sizeof  (union_iovec.s.iov_len)) ;

/*==============================================================================
 * Flexible size "struct iovec"
 *
 * NB: a completely zero structure is a valid, empty qiovec.
 */
struct qiovec
{
  qiov_vector   vec ;           /* the actual iovec array       */

  uint          i_get ;         /* next entry to get            */
  uint          i_put ;         /* next entry to put            */

  uint          i_alloc ;       /* number of entries allocated  */

  bool          writing ;       /* started, but not finished    */
} ;

typedef struct qiovec* qiovec ;
typedef struct qiovec  qiovec_t[1] ;

enum
{
  QIOVEC_INIT_ALL_ZEROS = true
} ;

/*==============================================================================
 * Functions
 */

extern qiovec qiovec_init_new(qiovec qiov) ;
extern qiovec qiovec_reset(qiovec qiov, bool free_structure) ;
Inline qiovec qiovec_free(qiovec qiov) ;

Inline bool qiovec_empty(qiovec qiov) ;
extern size_t qiovec_length(qiovec qiov) ;
extern void qiovec_clear(qiovec qiov) ;
extern int qiovec_write_nb(int fd, qiovec qiov) ;

Inline uint qiovec_count(qiovec qiov) ;
Inline void qiovec_push_this(qiovec qiov, const void* base, size_t len) ;
Inline void qiovec_push(qiovec qiov, qiov_item item) ;
Inline void qiovec_pop(qiovec qiov, qiov_item item) ;
Inline void qiovec_shift(qiovec qiov, qiov_item item) ;
Inline void qiovec_unshift(qiovec qiov, qiov_item item) ;

extern int iovec_write_nb(int fd, struct iovec* p_iov, int n) ;
Inline void iovec_set(struct iovec* p_iov, const void* base, size_t len) ;

Private void qiovec_extend(qiovec qiov) ;
Private void qiovec_shuffle(qiovec qiov) ;

/*------------------------------------------------------------------------------
 * Free given qiovec
 *
 * It is the caller's responsibility to free anything that the qiovec may
 * point to.
 *
 * Returns:  NULL
 */
Inline qiovec qiovec_free(qiovec qiov)
{
  return qiovec_reset(qiov, free_it) ;
} ;

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
 * How many entries in the given qiov ?
 */
Inline uint
qiovec_count(qiovec qiov)
{
  return (qiov->i_put - qiov->i_get) ;
} ;

/*------------------------------------------------------------------------------
 * Push to qiov -- same as qiovec_push, but with item.
 *
 * Ignores zero length items.
 */
Inline void
qiovec_push_this(qiovec qiov, const void* base, size_t len)
{
  qiov_item_t item ;

  item->base = base ;
  item->len  = len ;
  qiovec_push(qiov, item) ;
} ;

/*------------------------------------------------------------------------------
 * Push (copy of) item to qiov -- ie append it to the end of the vector.
 */
Inline void
qiovec_push(qiovec qiov, qiov_item item)
{
  if (item->len != 0)
    {
      if (qiov->i_put >= qiov->i_alloc)
        qiovec_extend(qiov) ;

      qiov->vec[qiov->i_put++] = *item ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Pop last item from qiov -- return empty item if none.
 */
Inline void
qiovec_pop(qiovec qiov, qiov_item item)
{
  if (qiov->i_get < qiov->i_put)
    {
      *item = qiov->vec[--qiov->i_put] ;
      if (qiov->i_get == qiov->i_put)
        qiov->i_get = qiov->i_put = 0 ;
    }
  else
    {
      assert(qiov->i_get == qiov->i_put) ;
      item->base = NULL ;
      item->len  = 0 ;

      qiov->i_get = qiov->i_put = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Shift first item off qiov -- return empty item if none.
 */
Inline void
qiovec_shift(qiovec qiov, qiov_item item)
{
  if (qiov->i_get < qiov->i_put)
    {
      *item = qiov->vec[qiov->i_get++] ;
      if (qiov->i_get == qiov->i_put)
        qiov->i_get = qiov->i_put = 0 ;
    }
  else
    {
      assert(qiov->i_get == qiov->i_put) ;
      item->base = NULL ;
      item->len  = 0 ;

      qiov->i_get = qiov->i_put = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Unshift item onto qiov -- ie prepend it to the start of the vector.
 */
Inline void
qiovec_unshift(qiovec qiov, qiov_item item)
{
  if (qiov->i_get == 0)
    {
      if (qiov->i_put == 0)
        return qiovec_push(qiov, item) ;

      qiovec_shuffle(qiov) ;
    } ;

  if (item->len != 0)
    qiov->vec[--qiov->i_get] = *item ;
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

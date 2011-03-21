/* Flexible iovec handler
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
#include "zconfig.h"            /* Otherwise IOV_MAX is not defined !   */

#include <errno.h>
#include "misc.h"
#include "qdebug_nb.h"

#include "memory.h"
#include "miyagi.h"

#include "qiovec.h"

/*==============================================================================
 * IOV_MAX is defined by POSIX to appear in <limits.h>.
 *
 * This does not appear unless "zconfig.h" is included, first...
 * ...but although it compiles OK, Eclipse cannot see where the value has
 * come from.
 */
#ifdef IOV_MAX                  /* Stops Eclipse whinging       */
#if IOV_MAX < 64                /* check for a reasonable value */
#error IOV_MAX < 64
#endif
#endif

/*==============================================================================
 * Initialise, allocate and reset qiovec
 */

/*------------------------------------------------------------------------------
 * Initialise new qiovec -- allocate if required.
 *
 * This is for initialising a new structure.  Any pre-exiting contents are
 * lost.
 *
 * Returns: address of qiovec
 */
extern qiovec
qiovec_init_new(qiovec qiov)
{
  if (qiov == NULL)
    qiov = XCALLOC(MTYPE_QIOVEC, sizeof(struct qiovec)) ;
  else
    memset(qiov, 0, sizeof(struct qiovec)) ;

  /* Zeroising has set:
   *
   *   vec       = NULL - no array, yet
   *   writing   = false -- no writing going on
   *
   *   i_get     = 0     -- next entry to get
   *   i_put     = 0     -- next entry to put
   *
   *   i_alloc   = 0;    -- no entries allocated
   *
   * Nothing more is required.
   */

  return qiov ;
} ;

/*------------------------------------------------------------------------------
 * Reset qiovec (if any) -- release body and (if required) the structure.
 *
 * Returns: address of qiovec (if any) -- NULL if structure released
 */
extern qiovec
qiovec_reset(qiovec qiov, bool free_structure)
{
  if (qiov != NULL)
    {
      if (qiov->vec != NULL)
        XFREE(MTYPE_QIOVEC_VEC, qiov->vec) ;

      if (free_structure)
        XFREE(MTYPE_QIOVEC, qiov) ;  /* sets qiov = NULL     */
      else
        qiovec_init_new(qiov) ;      /* re-initialise        */
    } ;

  return qiov ;
} ;

/*------------------------------------------------------------------------------
 * Clear given qiovec.
 */
extern void
qiovec_clear(qiovec qiov)
{
  qiov->i_get   = 0 ;
  qiov->i_put   = 0 ;
  qiov->writing = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Establish total length of the qiov -- add up all the item lengths.
 */
extern size_t
qiovec_length(qiovec qiov)
{
  if (qiov->i_put > qiov->i_get)
    {
      size_t    length ;
      uint      n ;
      qiov_item item ;

      n    = qiov->i_put - qiov->i_get ;
      item = &qiov->vec[qiov->i_get] ;

      length = item->len ;
      while (--n)
        length += (++item)->len ;

      return length ;
    }
  else
    {
      assert(qiov->i_get == qiov->i_put) ;
      qiov->i_get = qiov->i_put = 0 ;

      return 0 ;
    } ;
} ;


/*==============================================================================
 * Extending and shuffling qiovec in order to support push and unshift
 * operations.
 *
 * Note that if unshift becomes common, then should adjust the following.
 * Also address the tendency to reset i_get and i_put to zero whenever they
 * are found to be equal.
 */
enum
{
  i_get_limit  = 200,   /* in qiovec_extend, shuffle vector down if
                           i_get is >= i_get_limit                      */

  i_alloc_unit = 100,   /* allocate in lots of this much                */

  i_get_margin = 20,    /* in qiovec_shuffle, if have to reorganise,
                           allow for this many unshifts.                */

  i_put_margin = 40,    /* in qiovec_shuffle, if have to reorganise,
                           allow for this many pushes.                  */
} ;

static void qiovec_move_vector(qiovec qiov, uint new_i_get) ;
static void qiovec_new_vector(qiovec qiov, uint items) ;

/*------------------------------------------------------------------------------
 * Extend -- see qiovec_push.
 *
 * NB: underlying vector may be reorganised, reallocated, etc.  All pointers
 *     to items are now INVALID.
 */
Private void
qiovec_extend(qiovec qiov)
{
  assert(qiov->i_put <= qiov->i_alloc) ;
  assert(qiov->i_get <= qiov->i_put) ;

  assert( ((qiov->i_alloc == 0) && (qiov->vec == NULL))
       || ((qiov->i_alloc != 0) && (qiov->vec != NULL)) ) ;

  /* Short circuit if can make space by resetting indexes               */
  if ((qiov->i_put == qiov->i_get) && (qiov->i_alloc != 0))
    {
      qiov->i_put = 0 ;
      qiov->i_get = 0 ;

      return ;
    } ;

  if (qiov->i_get > i_get_limit)        /* keep in check            */
    qiovec_move_vector(qiov, 0) ;
  else
    qiovec_new_vector(qiov, i_alloc_unit) ;
} ;

/*------------------------------------------------------------------------------
 * Shuffle to allow for qiovec_unshift.
 *
 * Don't expect this to happen often -- but will do it if required.
 *
 * On exit the i_get will be > 0.
 *
 * NB: underlying vector may be reorganised, reallocated, etc.  All pointers
 *     to items are now INVALID.
 */
Private void
qiovec_shuffle(qiovec qiov)
{
  assert(qiov->i_put <= qiov->i_alloc) ;
  assert(qiov->i_get <= qiov->i_put) ;

  assert( ((qiov->i_alloc == 0) && (qiov->vec == NULL))
       || ((qiov->i_alloc != 0) && (qiov->vec != NULL)) ) ;

  if (qiov->i_get > 0)
    return ;                    /* shouldn't be here, really    */

  /* If do not have enough spare space for both margins, allocate extra to
   * the tune of both margins -- will leave at least the margins and at
   * most nearly twice those margins.
   *
   * This will allocate a vector is there is none at all.
   */
  if ((qiov->i_alloc - qiov->i_put) < (i_get_margin + i_put_margin))
    qiovec_new_vector(qiov, i_get_margin + i_put_margin) ;

  /* Shuffle into place and update i_get and i_put.             */
  qiovec_move_vector(qiov, i_get_margin) ;

  confirm(i_get_margin > 0) ;
} ;

/*------------------------------------------------------------------------------
 * Move contents of vector and set a new i_get (and i_put to suit).
 */
static void
qiovec_move_vector(qiovec qiov, uint new_i_get)
{
  memmove(&qiov->vec[new_i_get], &qiov->vec[qiov->i_get],
                           (qiov->i_put - qiov->i_get) * sizeof(qiovec_t)) ;

  qiov->i_put -= qiov->i_get - new_i_get ;
  qiov->i_get  = new_i_get ;
} ;

/*------------------------------------------------------------------------------
 * Allocate or reallocate vector for given number of items.
 */
static void
qiovec_new_vector(qiovec qiov, uint items)
{
  qiov->i_alloc += items ;

  qiov->vec = XREALLOC(MTYPE_QIOVEC_VEC, qiov->vec,
                                             qiov->i_alloc * sizeof(qiovec_t)) ;
} ;

/*==============================================================================
 * Writing qiovec and iovec
 */

static ssize_t writev_qdebug_nb(int fd, struct iovec p_iov[], int n) ;

/*------------------------------------------------------------------------------
 * Write given qiovec -- assuming NON-BLOCKING.
 *
 * Does nothing if the qiovec is empty.
 *
 * Loops internally if gets EINTR.
 *
 * When there is nothing left to output, resets the i_put & i_get to zero.
 *
 * Returns: > 0 => one or more bytes left to output
 *            0 => all done -- zero bytes left to output
 *           -1 => failed -- see errno
 */
extern int
qiovec_write_nb(int fd, qiovec qiov)
{
  int n ;
  int l ;

  n = qiov->i_put - qiov->i_get ;

  l = iovec_write_nb(fd, (struct iovec*)(&qiov->vec[qiov->i_get]), n) ;

  if (l == 0)
    {
      qiov->writing = false ;
      qiov->i_get   = qiov->i_put = 0 ;
    }
  else
    {
      qiov->writing = true ;
      qiov->i_get  += (n - l) ;
    } ;

  return l ;
} ;

/*------------------------------------------------------------------------------
 * Write given iovec -- assuming NON-BLOCKING.
 *
 * Does nothing if given zero iovec entries (and array may be NULL).
 *
 * Loops internally if gets EINTR.
 *
 * If does not manage to write everything, then:
 *
 *   -- updates the length field of all entries up to and including the
 *      last one for which data has been written.
 *
 *   -- updates the address field of the first entry that still has some
 *      data to be output.
 *
 *   Can call this again with the same 'p_iov' and the same 'n' -- the entries
 *   which have zero lengths will be stepped over.  Output will continue from
 *   where it left off.
 *
 *   Alternatively, if this returns 'l', then do "p_iov += n - l", and set
 *   "n = l" before calling this again.
 *
 * Returns: > 0 => number of entries left to output
 *            0 => all done -- nothing left to output
 *           -1 => failed -- see errno
 */
extern int
iovec_write_nb(int fd, struct iovec p_iov[], int n)
{
  assert(n >= 0) ;

  /* Skip past any leading zero length entries                          */
  while ((n > 0) && (p_iov->iov_len == 0))
    {
      ++p_iov ;
      --n ;
    } ;

  while (n > 0)
    {
      ssize_t ret ;

      if (qdebug_nb)
        ret = writev_qdebug_nb(fd, p_iov, (n < IOV_MAX ? n : IOV_MAX)) ;
      else
        ret = writev          (fd, p_iov, (n < IOV_MAX ? n : IOV_MAX)) ;

      if (ret > 0)
        {
          while (ret > 0)
            {
              if (ret >= (ssize_t)p_iov->iov_len)
                {
                  assert(n > 0) ;
                  ret -= p_iov->iov_len ;
                  p_iov->iov_len = 0 ;
                  ++p_iov ;
                  --n ;
                }
              else
                {
                  p_iov->iov_base = (char*)p_iov->iov_base + ret ;
                  p_iov->iov_len -= ret ;
                  ret = 0 ;
                } ;
            } ;

        }
      else if (ret == 0)
        break ;                 /* not sure can happen... but
                                   cannot assume will go away   */
      else
        {
          int err = errno ;
          if ((err == EAGAIN) || (err == EWOULDBLOCK))
            break ;
          if (err != EINTR)
//          assert(0) ;         // Pro tem
            return -1 ;         /* failed                       */
        } ;
    } ;

  return n ;
} ;

/*==============================================================================
 * Simulation of writev() for debug purposes -- generates lots of partial
 * writes and lots of blocking
 *
 */

static qrand_seq_t wseq = QRAND_SEQ_INIT(4001) ;

static const int blocking_errs[] = { EAGAIN, EWOULDBLOCK, EINTR } ;

/*------------------------------------------------------------------------------
 * Simulate writev() with tiny output buffers and lots of blocking.
 */
static ssize_t
writev_qdebug_nb(int fd, struct iovec p_iov[], int n)
{
  struct iovec* q_iov ;
  size_t  len ;
  ssize_t ret ;

  assert(n > 0) ;

  if (qrand(wseq, 3) == 0)      /* 1/3 chance of blocking       */
    {
      errno = blocking_errs[qrand(wseq, 3)] ;
      return -1 ;
    } ;

  /* Process 1..n or 1..3 entries                               */
  n = qrand(wseq, (n < 3) ? n : 3) ;
  q_iov = p_iov + n ;
  ++n ;

  /* If new last entry is not zero length, write only part of
   * it.
   */
  len = q_iov->iov_len ;
  if (len > 0)
    q_iov->iov_len = qrand(wseq, (len < 200) ? len : 200) + 1 ;

  ret = writev(fd, p_iov, n) ;

  q_iov->iov_len = len ;  /* restore true length of entry       */

  return ret ;
} ;



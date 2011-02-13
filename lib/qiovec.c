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
qiovec_init_new(qiovec viov)
{
  if (viov == NULL)
    viov = XCALLOC(MTYPE_QIOVEC, sizeof(struct qiovec)) ;
  else
    memset(viov, 0, sizeof(struct qiovec)) ;

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

  return viov ;
} ;

/*------------------------------------------------------------------------------
 * Reset qiovec (if any) -- release body and (if required) the structure.
 *
 * Returns: address of qiovec (if any) -- NULL if structure released
 */
extern qiovec
qiovec_reset(qiovec viov, bool free_structure)
{
  if (viov != NULL)
    {
      if (viov->vec != NULL)
        XFREE(MTYPE_QIOVEC_VEC, viov->vec) ;

      if (free_structure)
        XFREE(MTYPE_QIOVEC, viov) ;  /* sets viov = NULL     */
      else
        qiovec_init_new(viov) ;      /* re-initialise        */
    } ;

  return viov ;
} ;

/*------------------------------------------------------------------------------
 * Clear given qiovec.
 */
extern void
qiovec_clear(qiovec viov)
{
  viov->i_get   = 0 ;
  viov->i_put   = 0 ;
  viov->writing = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Push item to given qiovec
 *
 * NB: avoids pushing zero length items.
 */
extern void
qiovec_push(qiovec viov, const void* base, size_t len)
{
  struct iovec* p_iov ;

  if (len == 0)
    return ;

  if (viov->i_put >= viov->i_alloc)
    {
      size_t size ;
      assert(viov->i_put == viov->i_alloc) ;

      assert( ((viov->i_alloc == 0) && (viov->vec == NULL))
           || ((viov->i_alloc != 0) && (viov->vec != NULL)) ) ;

      if (viov->i_get > 200)            /* keep in check        */
        {
          size = (viov->i_put - viov->i_get) * sizeof(struct iovec) ;
          if (size != 0)
            memmove(viov->vec, &viov->vec[viov->i_get], size) ;
          viov->i_put -= viov->i_get ;
          viov->i_get  = 0 ;
        }
      else
        {
          viov->i_alloc += 100 ;        /* a sizable chunk      */

          size = viov->i_alloc * sizeof(struct iovec) ;
          if (viov->vec == NULL)
            viov->vec = XMALLOC(MTYPE_QIOVEC_VEC, size) ;
          else
            viov->vec = XREALLOC(MTYPE_QIOVEC_VEC, viov->vec, size) ;
        } ;
    } ;

  p_iov = &viov->vec[viov->i_put++] ;

  p_iov->iov_base = miyagi(base) ;
  p_iov->iov_len  = len ;
} ;

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
qiovec_write_nb(int fd, qiovec viov)
{
  int n ;
  int l ;

  n = viov->i_put - viov->i_get ;

  l = iovec_write_nb(fd, &viov->vec[viov->i_get], n) ;

  if (l == 0)
    {
      viov->writing = 0 ;
      viov->i_get   = viov->i_put = 0 ;
    }
  else
    {
      viov->writing = 1 ;
      viov->i_get  += (n - l) ;
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
  ssize_t ret ;

  assert(n >= 0) ;

  /* Skip past any leading zero length entries                          */
  while ((n > 0) && (p_iov->iov_len == 0))
    {
      ++p_iov ;
      --n ;
    } ;

  while (n > 0)
    {
      ret = writev(fd, p_iov, (n < IOV_MAX ? n : IOV_MAX)) ;

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
            return -1 ;         /* failed                       */
        } ;
    } ;

  return n ;
} ;

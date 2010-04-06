/* VTY I/O FIFO
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

#include <stddef.h>
#include <string.h>

#include "vio_fifo.h"
#include "network.h"
#include "list_util.h"

#include "memory.h"
#include "zassert.h"

/*==============================================================================
 * VTY I/O FIFO manages an arbitrary length byte-wise FIFO buffer.
 *
 * The FIFO is arranged as lumps of some given size.  Lumps are allocated
 * as and when necessary, and released once emptied.
 *
 * The last lump is never released.  So, it may be that only one lump is
 * ever needed.
 *
 * When releasing lumps, keeps one lump "spare", to be reused as necessary.
 * This is used in ... TODO <<<<  And is released...
 *
 *------------------------------------------------------------------------------
 * Implementation notes:
 *
 * The FIFO is initialised with all pointers NULL -- so with no lumps at all.
 *
 * Once a lump has been allocated there is always one lump in the FIFO.
 *
 * The following are expected to be true:
 *
 *   * put_ptr == get_ptr    => FIFO empty
 *
 *   * put_ptr == tail->end  -- at all times (NULL when no lumps)
 *
 *     put_ptr >= tail->data )  otherwise something is broken
 *     put_ptr <= tail->end  )
 *
 *   * get_ptr == head->end  -- when there is more than one lump
 *     get_ptr <= put_ptr    -- when there is only one lump
 *
 *     get_ptr >= head->data )  otherwise something is broken
 *     get_ptr <= head->end  )
 *
 *   * put_ptr == put_end    => tail lump is full
 *     put_ptr <  put_end    => space exists in the tail lump
 *     put_ptr >  put_end    => broken
 *
 *   * get_ptr == get_end    => head lump is empty
 *                              BUT if there is only one lump, make sure that
 *                              get_end == put_ptr.
 *     get_ptr <  get_end    => data exists in the head lump
 *     get_ptr >  get_end    => broken
 *
 * Note that:
 *
 *   * when the get_ptr reaches the put_ptr the pointers are reset to the
 *     start of the one and only lump.
 *
 *     Everywhere that the get_ptr is moved, must check for meeting the
 *     put_ptr and reset pointers.  At the same time, when reaches the end of
 *     a lump, gets rid of it.
 *
 *   * when advancing the put_ptr does not check for advancing the get_end.
 *
 *     The one exception to this, is that when the put_ptr advances to a new
 *     block, if there was one lump, sets the get_end to the end of that block.
 *
 *     Everywhere that the get_end is used, must check for there being one
 *     lump and the possibility that put_ptr has changed.
 */

/*==============================================================================
 * Initialisation, allocation and freeing of FIFO and lumps thereof.
 */

/* Return default size, or given size rounded up to 16 byte boundary    */
static size_t
vio_fifo_size(size_t size)
{
  if (size == 0)
    return 4096 ;
  else
    return ((size + 16 - 1) / 16) * 16 ;
} ;

/*==============================================================================
 * Initialise VTY I/O FIFO -- allocating if required.
 */
extern vio_fifo
vio_fifo_init_new(vio_fifo vf, size_t size)
{
  if (vf == NULL)
    vf = XCALLOC(MTYPE_VIO_FIFO, sizeof(vio_fifo_t)) ;
  else
    memset(vf, 0, sizeof(vio_fifo_t)) ;

  /* Zeroising the the vio_fifo_t has set:
   *
   *    lump      -- base pair, both pointers NULL => list is empty
   *
   *    put_ptr   -- NULL ) no lump to put anything into
   *    put_end   -- NULL ) put_ptr == put_end   => no room in current lump
   *
   *    get_ptr   -- NULL ) no lump to get anything from
   *    get_end   -- NULL ) get_ptr -- get_end => nothing left in current lump
   *
   *    rdr_lump  -- NULL ) no rdr_lump
   *    rdr_ptr   -- NULL
   *
   *    spare     -- NULL  no spare lump
   *
   * ALSO  put_ptr == get_ptr => FIFO is empty !
   */

  vf->size = vio_fifo_size(size) ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return vf ;
}

/*------------------------------------------------------------------------------
 * Free contents of given FIFO, and free FIFO structure as well, if required.
 *
 * Does nothing if given a NULL pointer -- must already have been freed !
 *
 * If does not free the FIFO structure, resets it all empty.
 *
 * Frees *all* FIFO lumps.
 *
 * See also: vio_fifo_reset_keep(vio_fifo)
 *           vio_fifo_reset_free(vio_fifo)
 */
extern vio_fifo
vio_fifo_reset(vio_fifo vf, int free_structure)
{
  vio_fifo_lump lump ;

  if (vf == NULL)
    return NULL ;

  while (ddl_pop(&lump, vf->base, list) != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, lump) ;

  if (vf->spare != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, vf->spare) ;

  if (free_structure)
    XFREE(MTYPE_VIO_FIFO, vf) ;         /* sets vf = NULL       */
  else
    vio_fifo_init_new(vf, vf->size) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * The FIFO is empty, with one lump -- reset all pointers.
 */
inline static void
vio_fifo_ptr_reset(vio_fifo vf, vio_fifo_lump lump)
{
  if (vf->rdr_lump != NULL)
    {
      assert((lump == vf->rdr_lump) && (vf->rdr_ptr == vf->get_ptr)) ;
      vf->rdr_ptr = lump->data ;
    } ;

  /* Note that sets the lump->end to the true lump->end                 */
  vf->get_ptr = vf->get_end = vf->put_ptr = lump->data ;
  vf->put_end = lump->end = lump->data + lump->size ;
} ;

/*------------------------------------------------------------------------------
 * The FIFO is utterly empty, with ZERO lumps -- unset all pointers.
 */
inline static void
vio_fifo_ptr_unset(vio_fifo vf)
{
  assert((ddl_head(vf->base) == NULL) && (ddl_tail(vf->base) == NULL)) ;

  vf->one     = false ;

  vf->put_ptr = NULL ;
  vf->put_end = NULL ;
  vf->get_ptr = NULL ;
  vf->get_end = NULL ;

  vf->rdr_lump = NULL ;
  vf->rdr_ptr  = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Clear out contents of FIFO -- will continue to use the FIFO.
 *
 * Keeps one FIFO lump.  (Frees everything else, including any spare.)
 */
extern void
vio_fifo_clear(vio_fifo vf)
{
  vio_fifo_lump tail ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  assert(vf != NULL) ;

  tail = ddl_tail(vf->base) ;

  if (tail != NULL)
    {
      while (ddl_head(vf->base) != tail)
        {
          vio_fifo_lump lump ;
          ddl_pop(&lump, vf->base, list) ;
          XFREE(MTYPE_VIO_FIFO_LUMP, lump) ;
        } ;

      vf->rdr_lump  = NULL ;            /* clear rdr            */
      vf->rdr_ptr   = NULL ;

      vf->one = true ;
      vio_fifo_ptr_reset(vf, tail) ;
    }
  else
    vio_fifo_ptr_unset(vf) ;

  if (vf->spare != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, vf->spare) ;   /* sets vf->spare = NULL  */

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*------------------------------------------------------------------------------
 * See how much room there is in the FIFO.
 *
 * If no lumps have been allocated, returns the size of the lump that would
 * allocate.
 *
 * Otherwise, returns the amount of space available *without* allocating any
 * further lumps.
 *
 * Returns: room available as described
 */
extern size_t
vio_fifo_room(vio_fifo vf)
{
  if (vf->put_ptr != NULL)
    return vf->put_end - vf->put_ptr ;
  else
    return vf->size ;
} ;

/*------------------------------------------------------------------------------
 * Allocate another lump for putting into.
 *
 * Call when (vf->put_ptr >= vf->put_end) -- asserts that they are equal.
 *
 * Set the put_ptr/put_end pointers to point at the new lump.
 *
 * If this is the first lump allocated, set the get_ptr/get_end pointers too.
 *
 * If have just filled the first lump on the list, update the get_end pointer
 * to reflect the fact that the out lump is now full.
 */
extern void
vio_fifo_lump_new(vio_fifo vf, size_t size)
{
  vio_fifo_lump lump ;
  int      first_alloc ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  passert(vf->put_ptr == vf->put_end) ; /* must be end of tail lump     */

  if (vf->one)
    vf->get_end = vf->put_ptr ;         /* update get_end               */

  lump = ddl_tail(vf->base) ;

  first_alloc = (lump == NULL) ;        /* extra initialisation needed  */

  if (first_alloc)
    assert(vf->put_ptr == NULL) ;       /* must all be NULL together    */
  else
    assert(vf->put_ptr == lump->end) ;  /* must be end of tail lump     */

  size = vio_fifo_size(size) ;

  if ((vf->spare != NULL) && (vf->spare->size >= size))
    {
      lump = vf->spare ;
      vf->spare = NULL ;
    }
  else
    {
      lump = XMALLOC(MTYPE_VIO_FIFO_LUMP,
                                       offsetof(vio_fifo_lump_t, data[size])) ;
      lump->size = size ;
    } ;

  lump->end = lump->data + lump->size ;

  ddl_append(vf->base, lump, list) ;

  vf->one     = first_alloc ;

  vf->put_ptr = lump->data ;
  vf->put_end = lump->end ;

  if (first_alloc)
    {
      vf->get_ptr = vf->put_ptr ;       /* get_ptr == put_ptr => empty  */
      vf->get_end = vf->put_ptr ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Release lump, head or tail (or both) and update pointers.
 *
 * Note that this does release the lump if it is the only lump.
 *
 * Do nothing if nothing is yet allocated.
 *
 * If releasing the only lump in the FIFO, resets all pointers to NULL.
 *
 * If releasing the head lump:
 *
 *   * the lump MUST be finished with -- so vf->get_ptr must be at the end
 *
 *     If releasing the only lump, the FIFO MUST be empty.
 *
 *   * if the lump is the current vf->rdr_lump, the reader must be at the
 *     end too -- ie it must be the same as the vf->get_ptr !
 *
 * If releasing the tail lump:
 *
 *   * the lump MUST be empty
 *
 *     If releasing the only lump, the FIFO MUST be empty.
 *
 *   * if the lump is the current vf->rdr_lump, the reader must be at the
 *     end too -- ie it must be the same as the vf->get_ptr !
 */
static void
vio_fifo_lump_release(vio_fifo vf, vio_fifo_lump lump)
{
  vio_fifo_lump head ;
  vio_fifo_lump tail ;
  vio_fifo_lump free ;
  bool release_head ;
  bool release_tail ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  /* Prepare and check whether removing head or tail (or both)          */
  head = ddl_head(vf->base) ;
  tail = ddl_tail(vf->base) ;

  release_head = (lump == head) ;
  release_tail = (lump == tail) ;

  assert(release_head || release_tail) ;

  /* Unless nothing ever allocated -- release the lump.                 */
  free = lump ;                 /* expect to free the lump      */
  if (lump != NULL)
    {
      vio_fifo_lump keep ;

      /* Consistency checks                                     */
      if (release_head)
        {
          if (release_tail)
            assert(vf->get_ptr == vf->put_ptr) ;
          else
            assert(vf->get_ptr == lump->end) ;

          if (vf->rdr_lump == lump)
            assert(vf->rdr_ptr == vf->get_ptr) ;
        }
      else if (release_tail)
        {
          assert(vf->put_ptr == lump->data) ;

          if (vf->rdr_lump == lump)
            assert(vf->rdr_ptr == vf->put_ptr) ;
        } ;

      /* Remove lump from FIFO and decide whether to keep as spare, or
       * which of spare and this to free.
       */
      ddl_del(vf->base, lump, list) ;

      keep = vf->spare ;        /* expect to keep current spare */

      if ((keep == NULL) || (keep->size < lump->size))
        {
          keep = lump ;
          free = vf->spare ;
        } ;

      vf->spare = keep ;

      head = ddl_head(vf->base) ;   /* changed if released head */
      tail = ddl_tail(vf->base) ;   /* changed if released tail */
    } ;

  /* Now update pointers... depending on what was released and what have
   * left.
   */
  if (head == NULL)
    {
      /* Deal with FIFO that now has no lumps or had none to start with     */
      if (lump != NULL)
        assert(vf->one) ;

      vio_fifo_ptr_unset(vf) ;
    }
  else
    {
      /* Have at least one lump left -- so must have had at least two ! */
      assert(!vf->one) ;

      vf->one = (head == tail) ;        /* update       */

      if (release_head)
        {
          /* Released the head.
           *
           * Update the vf->get_ptr and the vf->get_end.
           */
          vf->get_ptr = head->data ;
          if (vf->one)
            vf->get_end = vf->put_ptr ;
          else
            vf->get_end = head->end ;

          /* Update vf->rdr_ptr and vf->rdr_lump.       */
         if (vf->rdr_lump == lump)
           {
             vf->rdr_lump = head ;
             vf->rdr_ptr  = head->data ;
           } ;
        }
      else
        {
          /* Released the tail.
           * Update the vf->put_ptr and vf->put_end
           */
          vf->put_ptr = vf->put_end = tail->end ;

         /* Update vf->rdr_ptr and vf->rdr_lump.        */
         if (vf->rdr_lump == lump)
           {
             vf->rdr_lump = tail ;
             vf->rdr_ptr  = tail->end ;
           } ;
        } ;
    } ;

  /* Finally, free any lump that is actually to be freed                */

  if (free != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, free) ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Re-allocate lump for putting into.
 *
 * Call when vf->put_ptr == start of last lump, and that lump is not big
 * enough !
 *
 * There must be at least one lump.
 *
 * Updates put_ptr/put_end pointers to point at the new lump.
 *
 * Updates get_ptr/get_end pointers if required.
 *
 * Updates rdr_ptr if required.
 */
static void
vio_fifo_lump_renew(vio_fifo vf, vio_fifo_lump lump, size_t size)
{
  bool    rdr_set ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  /* FIFO may not be completely empty.
   * This must be the last lump.
   * The last lump must be empty.
   */
  assert((lump != NULL) && (lump == ddl_tail(vf->base))) ;

  /* Remove the last, *empty* lump, and update all pointers to suit.    */
  rdr_set = (vf->rdr_lump == lump) ;

  vio_fifo_lump_release(vf, lump) ;

  /* Now allocate a new lump with the required size                     */
  vio_fifo_lump_new(vf, size) ;

  /* Restore the rdr_ptr, if required                                   */
  if (rdr_set)
    {
      vio_fifo_lump tail ;

      tail = ddl_tail(vf->base) ;

      vf->rdr_lump = tail ;
      vf->rdr_ptr  = tail->data ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*==============================================================================
 * Put data to the FIFO.
 */

/*------------------------------------------------------------------------------
 * Store 'n' bytes -- allocate new lump if current is exhausted.
 */
extern void
vio_fifo_put(vio_fifo vf, const char* src, size_t n)
{
  size_t take ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  while (n > 0)
    {
      if (vf->put_ptr >= vf->put_end)
        vio_fifo_lump_new(vf, vf->size) ; /* traps put_ptr > put_end    */

      take = (vf->put_end - vf->put_ptr) ;
      if (take > n)
        take = n ;

      memcpy(vf->put_ptr, src, take) ;
      vf->put_ptr += take ;

      src += take ;
      n   -= take ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to fifo -- cf printf()
 *
 * Returns: >= 0 -- number of bytes written
 *           < 0 -- failed (unlikely though that is)
 */
extern int
vio_fifo_printf(vio_fifo vf, const char* format, ...)
{
  va_list args;
  int      len ;

  va_start (args, format);
  len = vio_fifo_vprintf(vf, format, args);
  va_end (args);

  return len;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to fifo -- cf vprintf()
 *
 * Returns: >= 0 -- number of bytes written
 *           < 0 -- failed (unlikely though that is)
 */
extern int
vio_fifo_vprintf(vio_fifo vf, const char *format, va_list args)
{
  va_list  ac ;
  int      len ;
  int      have ;
  size_t   size ;
  vio_fifo_lump lump ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  size = vf->size ;             /* standard allocation size             */
  while (1)
    {
      /* Find what space is left in the tail lump, and allocate a new,
       * empty lump if required.
       */
      if (vf->put_ptr >= vf->put_end)
        vio_fifo_lump_new(vf, size) ;   /* traps put_ptr > put_end      */

      have = vf->put_end - vf->put_ptr ;
      assert(have > 0) ;

      /* Note that vsnprintf() returns the length of what it would like to
       * have produced, if it had the space.  That length does not include
       * the trailing '\0'.
       */
      va_copy(ac, args) ;
      len = vsnprintf(vf->put_ptr, have, format, ac) ;
      va_end(ac) ;

      if (len < have)
        {
          if (len < 0)
            break ;             /* quit if failed       */

          vf->put_ptr += len ;
          break ;               /* done                 */
        } ;

      /* Not able to complete the operation in the current buffer.
       *
       * If the required space (len + 1) is greater than the standard
       * allocation, then need to increase the allocation for the next lump.
       *
       * If the current lump is empty, need to renew it with a fresh lump of
       * the now known required size.
       *
       * If the current lump is not empty, need to cut the end off and then
       * allocate a fresh lump (of the standard or now known required size).
       */
      if (len >= (int)size)
        size = len + 1 ;                /* need a non-standard size     */

      lump = ddl_tail(vf->base) ;

      if (vf->put_ptr == lump->data)
        /* Need to replace the last, empty, lump with another empty lump, but
         * big enough.
         */
        vio_fifo_lump_renew(vf, lump, size) ;
      else
        /* Need to cut this lump short, and allocate new lump at top of loop.
         */
        lump->end = vf->put_end = vf->put_ptr ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return len ;
} ;

/*==============================================================================
 * Get data from the FIFO.
 */

static bool vio_fifo_get_next_lump(vio_fifo vf) ;

/*------------------------------------------------------------------------------
 * Get ready to read something out of the FIFO.
 *
 * Makes sure vf->get_end is up to date (if required) and if the FIFO is not
 * empty, makes sure vf->get_ptr points at the next byte to be read.
 *
 * Returns: true <=> there is something in the FIFO.
 */
static inline bool
vio_fifo_get_ready(vio_fifo vf)
{
  assert(vf->rdr_lump == NULL) ;

  if (vf->one)
    vf->get_end = vf->put_ptr ;         /* make sure have everything    */

  if (vf->get_ptr >= vf->get_end)
    if (!vio_fifo_get_next_lump(vf))
      return 0 ;                        /* quit now if nothing there    */

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Get upto 'n' bytes.
 *
 * Returns: number of bytes got -- may be zero.
 */
extern size_t
vio_fifo_get(vio_fifo vf, void* dst, size_t n)
{
  size_t have ;
  void*  dst_in ;

  if (!vio_fifo_get_ready(vf))
    return 0 ;                          /* quit now if nothing there    */

  dst_in = dst ;
  while (n > 0)
    {
      have = vf->get_end - vf->get_ptr ;

      if (have > n)
        have = n ;

      memcpy(dst, vf->get_ptr, have) ;
      vf->get_ptr += have ;
      dst = (char*)dst + have ;

      if (vf->get_ptr >= vf->get_end)   /* deal with exhausted lump     */
        if (!vio_fifo_get_next_lump(vf))
          break ;                       /* quit if nothing more to come */

      n -= have ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return (char*)dst - (char*)dst_in ;
} ;

/*------------------------------------------------------------------------------
 * Get byte -- the long winded way.
 *
 * See the inline vio_fifo_get_byte().
 *
 * The version is used when the get_ptr is at or just before the end of the
 * current lump.  Looks after all the necessary pointer updates associated with
 * hitting end of lump, or hitting end of FIFO.
 *
 * Returns: 0x00..0xFF -- byte value      (as an int)
 *          -1         => FIFO is empty.
 */

extern int
vio_fifo_get_next_byte(vio_fifo vf)
{
  unsigned char u ;

  if (!vio_fifo_get_ready(vf))
    return -1 ;                         /* quit now if nothing there    */

  u = *vf->get_ptr++ ;

  /* As soon as reach the end want either to discard empty lump, or reset
   * the pointers.
   */
  if (vf->get_ptr >= vf->get_end)       /* deal with exhausted lump     */
    vio_fifo_get_next_lump(vf) ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return u ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to a lump of bytes.
 *
 * Returns: address of next byte to get, *p_have = number of bytes available
 *      or: NULL => FIFO is empty,       *p_have = 0
 *
 * If the FIFO is not empty, will return pointer to at least one byte.
 *
 * Returns number of bytes to the end of the current lump.  There may be
 * further lumps beyond the current one.
 */
extern void*
vio_fifo_get_lump(vio_fifo vf, size_t* p_have)
{
  if (!vio_fifo_get_ready(vf))
    {
      *p_have = 0 ;
      return NULL ;
    } ;

  *p_have = (vf->get_end - vf->get_ptr) ;
  return vf->get_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Advance FIFO to position reached.
 *
 * Having done vio_fifo_get_lump(), can take any number of bytes (up to the
 * number that "have"), then call this function to advance the pointers.
 *
 * The "here" argument must the the address returned by vio_fifo_get_lump()
 * plus the number of bytes taken.
 */
extern void
vio_fifo_got_upto(vio_fifo vf, void* here)
{
  vf->get_ptr = here ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  if (vf->get_ptr >= vf->get_end)
    vio_fifo_get_next_lump(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Write contents of FIFO -- assuming non-blocking file
 *
 * Will write all of FIFO, or upto but excluding the last lump.
 *
 * Returns: > 0 => blocked
 *            0 => all gone (up to last lump if !all)
 *          < 0 => failed -- see errno
 *
 * Note: will work perfectly well for a non-blocking file -- which should
 *       never return EAGAIN/EWOULDBLOCK, so will return from here "all gone".
 */
extern int
vio_fifo_write_nb(vio_fifo vf, int fd, bool all)
{
  char*   src ;
  size_t  have ;
  int     done ;

  while ((src = vio_fifo_get_lump(vf, &have)) != NULL)
    {
      if (!all && vf->one)
        break ;                         /* don't write last lump        */

      done = write_nb(fd, src, have) ;

      if (done < 0)
        return -1 ;                     /* failed                       */

      vio_fifo_got_upto(vf, src + done) ;

      if (done < (int)have)
        return 1 ;                      /* blocked                      */
    } ;

  return 0 ;                            /* all gone                     */
} ;

/*------------------------------------------------------------------------------
 * Get the current rdr_end value.
 *
 * Unlike get_end, do not have a field for this, but find it each time.
 */
inline static char*
vio_fifo_rdr_end(vio_fifo vf)
{
  if (vf->rdr_lump == ddl_tail(vf->base))
    return vf->put_ptr ;
  else
    return vf->rdr_lump->end ;
} ;

/*------------------------------------------------------------------------------
 * Get the current rdr position -- sets it up if not currently set.
 *
 * Returns: address of next byte to get, *p_have = number of bytes available
 *      or: NULL => FIFO is empty,       *p_have = 0
 *
 * If the FIFO is not empty, will return pointer to at least one byte.
 *
 * Returns number of bytes to the end of the current lump.  There may be
 * further lumps beyond the current one.
 *
 * NB: unless returns FIFO is empty, it is a mistake to now do any "get"
 *     operation other than vio_fifo_step_rdr(), until do vio_fifo_sync_rdr()
 *     or vio_fifo_drop_rdr.
 */
extern void*
vio_fifo_get_rdr(vio_fifo vf, size_t* p_have)
{
  if (!vio_fifo_get_ready(vf))
    {
      *p_have = 0 ;
      return NULL ;
    } ;

  if (vf->rdr_lump == NULL)     /* set up new rdr if required   */
    {
      vf->rdr_lump = ddl_head(vf->base) ;
      vf->rdr_ptr  = vf->get_ptr ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  *p_have = vio_fifo_rdr_end(vf) - vf->rdr_ptr ;
  return vf->rdr_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Step the rdr forward by the given number of bytes.
 *
 * Returns: address of next byte to get, *p_have = number of bytes available
 *      or: NULL => FIFO is empty,       *p_have = 0
 *
 * If the FIFO is not empty, will return pointer to at least one byte.
 *
 * Returns number of bytes to the end of the current lump.  There may be
 * further lumps beyond the current one.
 *
 * NB: this does not change the get pointers, so all the data being stepped
 *     over is preserved in the FIFO, until vio_fifo_sync_rdr().
 *
 * NB: the step may NOT exceed the last reported "have".
 */
extern void*
vio_fifo_step_rdr(vio_fifo vf, size_t* p_have, size_t step)
{
  char* rdr_end ;

  assert(vf->rdr_lump != NULL) ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  rdr_end = vio_fifo_rdr_end(vf) ;
  vf->rdr_ptr += step ;

  if (vf->rdr_ptr >= rdr_end)
    {
      assert(vf->rdr_ptr == rdr_end) ;

      if (vf->rdr_lump != ddl_tail(vf->base))
        {
          vf->rdr_lump = ddl_next(vf->rdr_lump, list) ;
          vf->rdr_ptr  = vf->rdr_lump->data ;

          rdr_end = vio_fifo_rdr_end(vf) ;
        } ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  *p_have = (rdr_end - vf->rdr_ptr) ;
  return (*p_have > 0) ? vf->rdr_ptr : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Move FIFO get position to the rdr position, if any.
 *
 * This clears the rdr position, and removes all data between the current and
 * new get positions from the FIFO.
 */
extern void
vio_fifo_sync_rdr(vio_fifo vf)
{
  vio_fifo_lump head ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  if (vf->rdr_lump == NULL)
    return ;

  while ((head = ddl_head(vf->base)) != vf->rdr_lump)
    {
      vf->get_ptr = vf->get_end ;       /* jump to end of lump  */
      vio_fifo_lump_release(vf, head) ;
    } ;

  vf->get_ptr  = vf->rdr_ptr ;          /* jump to rdr_ptr      */

  vf->rdr_lump = NULL ;                 /* clear the rdr        */
  vf->rdr_ptr  = NULL ;

  if (vf->one)
    {
      if (vf->put_ptr == vf->get_ptr)   /* reset pointers if FIFO empty  */
        vio_fifo_ptr_reset(vf, head) ;
      else
        vf->get_end = vf->put_ptr ;
    }
  else
    vf->get_end = head->end ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Drop the rdr position (if any).
 *
 * This clears the rdr position leaving the get position and FIFO unchanged.
 */
extern void
vio_fifo_drop_rdr(vio_fifo vf)
{
  VIO_FIFO_DEBUG_VERIFY(vf) ;

  vf->rdr_lump = NULL ;
  vf->rdr_ptr  = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Move on to next lump to get stuff from.
 *
 * Advance pointers etc. so that have at least one byte available, unless
 * the FIFO is entirely empty.
 *
 * This should be called if (vf->get_ptr >= vf->get_end) -- asserts that
 * these are equal !
 *
 * NB: when there is only one block, it may be that get_end is out of date,
 *     and should be advanced to the current put_ptr position.
 *
 *     That is done here, but may be worth updating get_end before testing
 *     against get_ptr.
 *
 * Returns: true <=> at least one byte in FIFO.
 *
 * NB: if finds that the FIFO is empty, resets the pointers to the start
 *     of the last lump -- does not release the last lump.
 */
static bool
vio_fifo_get_next_lump(vio_fifo vf)
{
  vio_fifo_lump head ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
  assert(vf->get_ptr == vf->get_end) ;

  head = ddl_head(vf->base) ;   /* current lump for get     */

  /* Deal with the simple case of one lump, first.
   *
   * To save work when putting data into the FIFO (particularly when putting
   * a byte at a time) does not keep the vf->get_end up to date (when there is
   * only one lump).
   *
   * If the FIFO is empty, reset pointers and return empty.
   */
  if (vf->one)
    {
      assert( (head != NULL) && (head == ddl_tail(vf->base)) ) ;

      if (vf->get_ptr < vf->put_ptr)
        {
          /* Had an out of date vf->get_end                     */
          vf->get_end = vf->put_ptr ;

          return true ;         /* FIFO not empty               */
        } ;

      assert(vf->get_ptr == vf->put_ptr) ;

      /* FIFO is empty -- reset pointers and exit               */
      vio_fifo_ptr_reset(vf, head) ;

      return false ;            /* FIFO empty                   */
    } ;

  /* Release the head and update pointers
   *
   * Deals with possibility that nothing has yet been allocated
   */
  vio_fifo_lump_release(vf, head) ;

  return (vf->get_ptr < vf->get_end) ;
} ;

/*==============================================================================
 * For debug purposes -- verify the state of the given FIFO
 */
Private void
vio_fifo_verify(vio_fifo vf)
{
  vio_fifo_lump head ;
  vio_fifo_lump lump ;
  vio_fifo_lump tail ;

  head = ddl_head(vf->base) ;
  tail = ddl_tail(vf->base) ;

  /* If nothing allocated, should all be NULL & !vf->one        */
  /* If something allocated, tail must not be NULL              */
  if (head == NULL)
    {
      if ( (tail != NULL)
          || (vf->put_ptr  != NULL)
          || (vf->put_end  != NULL)
          || (vf->get_ptr  != NULL)
          || (vf->rdr_lump != NULL)
          || (vf->rdr_ptr  != NULL)
          || (vf->one) )
        zabort("nothing allocated, but not all NULL") ;
      return ;
    }
  else
    {
      if (tail == NULL)
        zabort("head pointer not NULL, but tail pointer is") ;
    } ;

  /* Check that all the pointers are within respective lumps
   *
   * Know that put_end is always tail->end, but get_end need not be.
   */
  if ( (tail->data > vf->put_ptr)
      ||            (vf->put_ptr > vf->put_end)
      ||                          (vf->put_end != tail->end) )
    zabort("put pointers outside the tail lump") ;

  if ( (head->data > vf->get_ptr)
      ||            (vf->get_ptr > vf->get_end)
      ||                          (vf->get_end > head->end) )
    zabort("get pointers outside the head lump") ;

  /* If head == tail, should be vf->one, etc.                   */
  if (head == tail)
    {
      if (!vf->one)
        zabort("have one lump, but !vf->one") ;

      if (vf->get_end > vf->put_ptr)
        zabort("get_end is greater than put_ptr when vf->one") ;
    }
  else
    {
      if (vf->one)
        zabort("have two or more lumps, but vf->one is true") ;

      if (vf->get_end != head->end)
        zabort("get_end is not head->end when !vf->one") ;
    } ;

  /* If have an rdr_lump -- make sure everything else is valid  */
  if (vf->rdr_lump != NULL)
    {
      lump = head ;
      while (lump != vf->rdr_lump)
        {
          if (lump == tail)
            zabort("rdr_lump is not part of FIFO") ;
          lump = ddl_next(lump, list) ;
        } ;

      if ( (lump->data > vf->rdr_ptr)
          ||            (vf->rdr_ptr > lump->end) )
        zabort("rdr_ptr outside its lump") ;

      if ( (lump == head) && (vf->rdr_ptr < vf->get_ptr))
         zabort("rdr_ptr is less than get_ptr in first lump") ;

      if ( (lump == tail) && (vf->rdr_ptr > vf->put_ptr))
         zabort("rdr_ptr is greater than put_ptr in last lump") ;
    }
  else
    {
      if (vf->rdr_ptr != NULL)
        zabort("rdr_ptr not NULL when rdr_lump is") ;
    }
} ;

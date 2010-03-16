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
   *    lump    -- base pair, both pointers NULL => list is empty
   *
   *    put_ptr -- NULL ) no lump to put anything into
   *    put_end -- NULL ) put_ptr == put_end   => no room in current lump
   *
   *    get_ptr -- NULL ) no lump to get anything from
   *    get_end -- NULL ) get_ptr -- get_end => nothing left in current lump
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

  if (free_structure)
    XFREE(MTYPE_VIO_FIFO, vf) ;         /* sets vf = NULL       */
  else
    vio_fifo_init_new(vf, vf->size) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * Set FIFO empty, discarding current contents -- will continue to use the FIFO.
 */
extern void
vio_fifo_set_empty(vio_fifo vf)
{
  vio_fifo_lump lump ;

  assert(vf != NULL) ;

  while (ddl_head(vf->base) != ddl_tail(vf->base))
    {
      ddl_pop(&lump, vf->base, list) ;
      XFREE(MTYPE_VIO_FIFO_LUMP, lump) ;
    } ;

  lump = ddl_head(vf->base) ;
  if (lump != NULL)
    {
      vf->get_ptr = vf->get_end = vf->put_ptr = lump->data ;
                                  vf->put_end = lump->end ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Allocate another lump for putting into.
 *
 * Call when (vf->put_ptr >= vf->put_end) -- asserts that the pointers are equal.
 *
 * Set the put_ptr/put_end pointers to point at the new lump.
 *
 * If this is the first lump allocated, set the get_ptr/get_end pointers too.
 *
 * If have just filled the first lump on the list, update the get_end pointer
 * to reflect the fact that the out lump is now full.
 */
extern void
vio_fifo_lump_new(vio_fifo vf)
{
  vio_fifo_lump lump ;
  size_t   size ;
  int      first_alloc ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  passert(vf->put_ptr == vf->put_end) ; /* must be end of tail lump     */

  lump = ddl_tail(vf->base) ;

  /* When there is only one lump, the get_end tracks the put_ptr.
   * But when there is more than one lump, it must be the end of that lump.
   */
  if (vf->one)
    vf->get_end = lump->end ;

  first_alloc = (lump == NULL) ;        /* extra initialisation needed  */

  if (first_alloc)
    assert(vf->put_ptr == NULL) ;       /* must all be NULL together    */
  else
    assert(vf->put_ptr == lump->end) ;  /* must be end of tail lump     */

  size = vio_fifo_size(vf->size) ;
  lump = XMALLOC(MTYPE_VIO_FIFO_LUMP, offsetof(vio_fifo_lump_t, data[size])) ;
  lump->end = (char*)lump->data + vf->size ;

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
        vio_fifo_lump_new(vf) ;   /* traps broken vf->put_ptr > vf->put_end */

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
 * Returns: address of next byte to get, *have = number of bytes available
 *      or: NULL => FIFO is empty,       *have = 0
 *
 * If the FIFO is not empty, will return pointer to at least one byte.
 *
 * Returns number of bytes to the end of the current lump.  There may be
 * further lumps beyond the current one.
 */
extern void*
vio_fifo_get_lump(vio_fifo vf, size_t* have)
{
  if (!vio_fifo_get_ready(vf))
    {
      *have = 0 ;
      return NULL ;
    } ;

  *have = (vf->get_end - vf->get_ptr) ;
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
 *     of the last lump.
 */
static bool
vio_fifo_get_next_lump(vio_fifo vf)
{
  vio_fifo_lump head ;
  vio_fifo_lump tail ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;
  assert(vf->get_ptr == vf->get_end) ;

  head = ddl_head(vf->base) ;   /* current lump for put                 */
  tail = ddl_tail(vf->base) ;   /* current lump for get                 */

  /* Deal with case of one lump only                                    */
  if (vf->one)
    {
      assert( (head != NULL)
           && (head == tail) ) ;

      if (vf->get_ptr == vf->put_ptr)
        {
          /* FIFO is empty -- reset pointers and exit           */
          vf->get_ptr = vf->get_end = vf->put_ptr = head->data ;
          assert(vf->put_end == head->end) ;

          return 0 ;            /* FIFO empty                   */
        } ;

      /* Had an out of date vf->get_end                         */
      assert(vf->get_end < vf->put_ptr) ;
      vf->get_end = vf->put_ptr ;

      return 1 ;                /* FIFO not empty after all     */
    } ;

  /* Deal with case of not yet allocated                        */
  if (head == NULL)
    {
      assert( (tail == NULL)
           && (vf->put_ptr == vf->get_ptr) );

      return 0 ;                /* FIFO empty                   */
    } ;

  /* Deal with (remaining) case of two or more lumps            */
  assert(vf->get_ptr == head->end) ;

  ddl_del_head(vf->base, list) ;
  XFREE(MTYPE_VIO_FIFO_LUMP, head) ;

  head = ddl_head(vf->base) ;
  assert(head != NULL) ;

  vf->one = (head == tail) ;

  vf->get_ptr = head->data ;    /* at start of next lump        */

  if (vf->one)
    vf->get_end = vf->put_ptr ; /* up to current put            */
  else
    vf->get_end = head->end ;   /* up to end of lump            */

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return (vf->get_ptr < vf->get_end) ;
} ;

/*==============================================================================
 * For debug purposes -- verify the state of the given FIFO
 */
Private void
vio_fifo_verify(vio_fifo vf)
{
  vio_fifo_lump head ;
  vio_fifo_lump tail ;

  head = ddl_head(vf->base) ;
  tail = ddl_tail(vf->base) ;

  /* If nothing allocated, should all be NULL & !vf->one        */
  /* If something allocated, tail must not be NULL              */
  if (head == NULL)
    {
      if ( (tail != NULL)
          || (vf->put_ptr != NULL)
          || (vf->put_end != NULL)
          || (vf->get_ptr != NULL)
          || (vf->get_end != NULL)
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
   *     */
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
    }
} ;

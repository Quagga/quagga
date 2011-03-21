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

#include "misc.h"
#include <stdio.h>
#include <string.h>

#include "vio_fifo.h"
#include "network.h"

#include "memory.h"

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
 *
 *------------------------------------------------------------------------------
 * Implementation notes:
 *
 * The FIFO is initialised with all pointers NULL -- so with no lumps at all.
 *
 * Once a lump has been allocated there is always one lump in the FIFO.
 *
 * The hold_mark allows the get_ptr to move forward, but retaining the data in
 * the FIFO until the hold_mark is cleared.  Can move the get_ptr back to the
 * hold_mark to reread the data.
 *
 * The end_mark allows put_ptr to move forward, but the new data cannot be got
 * from the FIFO until the end_mark is cleared.  Can discard the new data
 * and move the put_ptr back to the end_mark.
 *
 * There are four lumps of interest:
 *
 *   * head      -- where the hold_mark is, if there is one.
 *
 *   * get_lump  -- where the get_ptr is.
 *                  Same as head when no hold_mark.
 *
 *   * end_lump  -- where the end_mark is, if there is one.
 *                  Same as tail when no end mark.
 *
 *   * tail      -- where the put_ptr is.
 *
 * Some or all of those may be the same, depending on how big the FIFO is.
 *
 * The following are expected to be true:
 *
 *   * set                  <=> at least one lump in the FIFO
 *
 *   * as_one               <=> set && get_lump == tail && !end_mark
 *                           => get_end moves with put_ptr
 *
 *   * hold_mark             => there is a hold mark & hold_ptr is valid
 *                                                   & head lump contains mark
 *    !hold_mark             => get_lump == head & hold_ptr == NULL
 *
 *   * end_mark              => there is an end_mark & end_end is valid
 *                                                   & end_lump contains mark
 *    !end_mark              => end_lump == tail & end_ptr == NULL
 *
 *   * put_ptr == get_ptr    => FIFO empty -- unless hold_mark and
 *                              hold_ptr != get_ptr.
 *
 *   * put_end == tail->end  -- at all times (NULL when no lumps)
 *
 *     put_ptr >= tail->data )  otherwise something is broken
 *     put_ptr <= tail->end  )
 *
 *   * get_end == get_lump->end -- when get_lump != end_lump
 *             == end_end       -- when get_lump == end_lump & end_mark set
 *             <= put_ptr       -- when get_lump == end_lump & no end_mark
 *
 *                                 See note below on get_end and as_one flag.
 *
 *     get_ptr >= get_lump->data )  otherwise something is broken
 *     get_ptr <= get_lump->end  )
 *
 *   * put_ptr == put_end    => tail lump is full
 *     put_ptr <  put_end    => space exists in the tail lump
 *     put_ptr >  put_end    => broken
 *
 *   * get_ptr == get_end    => nothing to get from current get_lump
 *                              BUT if as_one, make sure that get_end == put_ptr
 *     get_ptr <  get_end    => data exists in the current get_lump
 *     get_ptr >  get_end    => broken
 *
 * Note that:
 *
 *   * while get_ptr <= get_end can get stuff without worrying about other
 *     pointers or moving between lumps etc.  It is permissible to leave
 *     get_ptr == get_end -- this will be tidied up on the next get operation,
 *     or any other time vio_fifo_sync_get() is called.  Leaving get_ptr in
 *     that state delays the discard of the now empty lump, but has no other
 *     real downside.
 *
 *     Similarly, while put_ptr <= put_end, can put stuff without worrying
 *     about other pointers or moving between lumps etc.
 *
 *     When getting, if get_ptr == get_end, or require more data than is
 *     immediately available, need to use vio_fifo_sync_get(), to do that.
 *
 *   * the value of get_end depends on whether get_lump == end_lump, and then
 *     whether there is an end_mark.
 *
 *     When get_lump == end_lump && !end_mark, then get_end may be out of date
 *     because get_ptr has been advanced.  This is dealt with by
 *     vio_fifo_sync_get(), which uses the as_one flag to signal that it should
 *     set get_end = put_ptr and then (re)check for anything to get.
 *
 *     The as_one flag is there to save a little work.  Making sure that the
 *     get_end is up to date can be done often when getting from the FIFO.  So
 *     the maintenance of the flag should be worth the effort.
 *
 *   * some care must be taken to ensure that are not fooled by the
 *     ambiguity of a pointer to the end of one lump and a pointer to the
 *     start of the next -- these are really equal, but they don't look as if
 *     they are !
 *
 *       - put_ptr   -- if at the end of the last lump there is no next lump !
 *
 *                      When a new lump is added, the put_ptr advances to the
 *                      start of the new last lump.
 *
 *       - end_end   -- when set from the put_ptr, this can be at the end of
 *                      the last lump, but as above, there is no next lump.
 *
 *                      When a new lump is added, if the end_end is at the
 *                      end of the last lump, it is moved to the start of the
 *                      new last lump (along with the out_ptr).
 *
 *                      So... end_end will never be ambiguous.
 *
 *       - get_ptr   -- this can be ambiguous.
 *
 *                      When getting bytes, if the segment get_ptr..get_end
 *                      is sufficient, then nothing else is required.
 *
 *                      Otherwise, vio_fifo_sync_get() will sort things out,
 *                      including resetting all pointers if the FIFO has been
 *                      emptied.
 *
 *       - hold_ptr  -- when set from get_ptr this could be at the end of the
 *                      first lump -- but when vio_fifo_sync_get() is called,
 *                      that will be spotted and sorted out.
 *
 *                      If get_ptr is set from an ambiguous hold_ptr, that is
 *                      also taken care of by the next vio_fifo_sync_get().
 *
 *                      When doing things with hold_ptr, does a number of
 *                      vio_fifo_sync_get() operations, so the hold_ptr should
 *                      not, in practice, be ambiguous.
 *
 *   * Before the first lump is allocated the FIFO appears empty (of course)
 *     but may have hold_mark and/or end_mark set, and these work as expected.
 */

/*==============================================================================
 * Initialisation, allocation and freeing of FIFO and lumps thereof.
 */

/*------------------------------------------------------------------------------
 * Return default size, or given size rounded up to 128 byte boundary
 */
static size_t
vio_fifo_size(size_t size)
{
#if VIO_FIFO_DEBUG
#warning VIO_FIFO_DEBUG and 29 byte lumps !
  return 29 ;
#else
  if (size == 0)
    return 4096 ;
  else
    return ((size + 128 - 1) / 128) * 128 ;
#endif
} ;

/*------------------------------------------------------------------------------
 * Set and return true end of lump.
 *
 * End of lump can be set short of true end when putting stuff, if wish to
 * move to next lump early (eg if not enough room left in current lump).
 *
 * When reusing a lump, need to restore the true lump end.
 */
static inline char*
vio_fifo_true_lump_size(vio_fifo_lump lump)
{
  return lump->end = lump->data + lump->size ;
} ;

/*==============================================================================
 * Initialise VTY I/O FIFO -- allocating if required.
 */
extern vio_fifo
vio_fifo_init_new(vio_fifo vff, size_t size)
{
  if (vff == NULL)
    vff = XCALLOC(MTYPE_VIO_FIFO, sizeof(vio_fifo_t)) ;
  else
    memset(vff, 0, sizeof(vio_fifo_t)) ;

  /* Zeroising the the vio_fifo_t has set:
   *
   *    base      -- base pair, both pointers NULL => list is empty
   *
   *    set       -- false -- nothing set, yet.
   *    as_one    -- false -- get_lump != tail or end_mark
   *    hold_mark -- false -- no hold mark
   *    end_mark  -- false -- no end mark
   *
   *    hold_ptr  -- NULL   no hold_mark
   *
   *    get_lump  -- NULL )
   *    get_ptr   -- NULL ) no lump to get anything from
   *    get_end   -- NULL ) get_ptr -- get_end => nothing left in current lump
   *
   *    end_lump  -- NULL   no lump at end of what can get
   *    end_end   -- NULL   no end_mark
   *
   *    put_ptr   -- NULL ) no lump to put anything into
   *    put_end   -- NULL ) put_ptr == put_end   => no room in current lump
   *
   *    size      -- 0      no size set for lumps (yet)
   *
   *    spare     -- NULL   no spare lump
   */
  confirm(VIO_FIFO_INIT_ALL_ZEROS) ;

  if (size != 0)
    vff->size = vio_fifo_size(size) ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  return vff ;
}

/*------------------------------------------------------------------------------
 * Free contents of given FIFO, and free FIFO structure as well, if required.
 *
 * Does nothing if given a NULL pointer -- must already have been freed !
 *
 * If does not free the FIFO structure, resets it all empty, keeping the
 * current size setting.
 *
 * Frees *all* FIFO lumps.
 *
 * See also: vio_fifo_reset_keep(vio_fifo)
 *           vio_fifo_reset_free(vio_fifo)
 */
extern vio_fifo
vio_fifo_reset(vio_fifo vff, free_keep_b free_structure)
{
  vio_fifo_lump lump ;

  if (vff == NULL)
    return NULL ;

  while (ddl_pop(&lump, vff->base, list) != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, lump) ;

  if (vff->spare != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, vff->spare) ;

  confirm(free_it == true) ;

  if (free_structure)
    XFREE(MTYPE_VIO_FIFO, vff) ;        /* sets vff = NULL       */
  else
    vio_fifo_init_new(vff, vff->size) ;

  return vff ;
} ;

/*------------------------------------------------------------------------------
 * The FIFO has one lump -- set all pointers.
 *
 * Preserves end_mark  -- setting to new put_ptr position
 * Preserves hold_mark -- setting to new put_ptr position
 *
 * Sets as_one if no end_mark.
 */
inline static void
vio_fifo_ptr_set(vio_fifo vff, vio_fifo_lump lump)
{
  vff->as_one    = !vff->end_mark ;

  vff->put_ptr   = lump->data ;
  vff->put_end   = vio_fifo_true_lump_size(lump) ;

  vff->hold_ptr  = vff->hold_mark ? vff->put_ptr : NULL ;

  vff->get_lump  = lump ;
  vff->get_ptr   = vff->put_ptr ;
  vff->get_end   = vff->put_ptr ;

  vff->end_lump  = lump ;
  vff->end_end   = vff->end_mark  ? vff->put_ptr : NULL ;
} ;

/*------------------------------------------------------------------------------
 * The FIFO is empty, with one lump -- reset all pointers.
 *
 * Preserves end_mark  -- setting to new put_ptr position
 * Preserves hold_mark -- setting to new put_ptr position
 *
 * Sets as_one if no end_mark.
 */
inline static void
vio_fifo_ptr_reset(vio_fifo vff, vio_fifo_lump lump)
{
  assert(vff->set) ;
  assert(lump == ddl_tail(vff->base)) ;  /* must be tail         */
  assert(lump == ddl_head(vff->base)) ;  /* and must be head     */

  vio_fifo_ptr_set(vff, lump) ;
} ;

/*------------------------------------------------------------------------------
 * Synchronise get_end and put_ptr (if required)
 *
 * When in the same lump, the get_end and the put_ptr should be the same.  But
 * maintaining that every time something is put into the buffer is a pain, so
 * we allow the put_ptr to get ahead, and re-synchronise when get_ptr hits
 * get_end, or any other time we need the pointers to be straight.
 *
 * The as_one flag takes a little maintenance...  it means:
 *
 *   (get_lump == tail) && set && !end_mark
 *
 * If get_ptr is at the end of what can be got from the current lump, advances
 * to the next lump if possible, releasing anything that can be released.
 *
 * If the FIFO is empty (with one lump), will crash all pointers down to start
 * of current lump.  The FIFO is empty if get_ptr == put_ptr, and there are no
 * bytes held before the get_ptr.
 *
 * NB: object of the hold_mark is to allow users of the FIFO to store pointers
 *     to sections of the FIFO returned by vio_fifo_get_lump() and stepped
 *     over by vio_fifo_got_upto().  Note that where there is nothing to
 *     read vio_fifo_get_lump() returns NULL -- so do not have to preserve
 *     the hold_ptr when hold_ptr == get_ptr (== end_end) == put_ptr -- that
 *     is, if hold_ptr == get_ptr, we can move the hold_ptr around with the
 *     get_ptr.
 *
 * Can reach empty state without realising it, because the "get" stuff is not
 * required to check every time a byte is read -- in fact, it is not necessary
 * to check until cannot get anything because get_ptr == get_end.
 *
 * NB: with an end_mark there is the ambiguous position at the end of a
 *     lump -- which is the same as the start of the next lump, if any.
 *
 *     Elsewhere we advance the end_end if it was at the very end of the FIFO
 *     and we advance the put_ptr to a new lump.  But we cope here in any
 *     case.
 *
 * Returns: true  <=> there is something in the (now) current get_lump.
 *          false <=> there is nothing else to be got (though if there is an
 *                    end_mark, there may be stuff beyond that).
 */
static bool vio_fifo_sync_get_next(vio_fifo vff) ;
static void vio_fifo_release_head(vio_fifo vff, vio_fifo_lump lump) ;

inline static bool
vio_fifo_sync_get(vio_fifo vff)
{
  if (vff->as_one)                      /* false if !vff->set            */
    vff->get_end = vff->put_ptr ;

  if (vff->get_ptr < vff->get_end)      /* both NULL if !vff->set       */
    return true ;                       /* have at least one byte       */

  return vio_fifo_sync_get_next(vff) ;
} ;

/*------------------------------------------------------------------------------
 * See if fifo really is or is not empty (get_ptr == end_end or == put_ptr).
 *
 * Used by vio_fifo_empty().  Called iff get_ptr >= get_end !
 *
 * So need to vio_fifo_sync_get() and test again against end_end & put_ptr.
 */
Private bool
vio_fifo_do_empty(vio_fifo vff)
{
  return ! vio_fifo_sync_get(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Set a new get_lump/get_ptr/get_end set.  And set as_one to suit.
 */
inline static void
vio_fifo_set_get_ptr(vio_fifo vff, void* ptr, vio_fifo_lump lump)
{
  vff->get_lump = lump ;
  vff->get_ptr  = ptr ;

  if      (lump != vff->end_lump)
    {
      vff->get_end = lump->end ;
      vff->as_one  = false ;
    }
  else if (vff->end_mark)
    {
      vff->get_end = vff->end_end ;
      vff->as_one  = false ;
    }
  else
    {
      vff->get_end = vff->put_ptr ;
      vff->as_one  = true ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Move on to next lump to get stuff from.
 *
 * For use by vio_fifo_sync_get() *ONLY*.
 *
 * Asserts that (vff->get_ptr == vff->get_end) and assumes that if as_one, then
 * get_end == put_ptr !
 *
 * Returns: true <=> at least one byte in FIFO available to get.
 */
static bool
vio_fifo_sync_get_next(vio_fifo vff)
{
  bool hold_empty ;

  assert(vff->get_ptr == vff->get_end) ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  if (!vff->set)
    return false ;              /* quit before get into any trouble     */

  /* Worry about whether there is anything held.
   *
   * NB: we know that at this point the get_ptr == get_end, so is at the end
   *     of the current get_lump.  The hold_ptr cannot be ahead of get_ptr,
   *     so the test hold_ptr == get_ptr is sufficient to detect that there
   *     is nothing, in fact, held.
   */
  hold_empty = !vff->hold_mark || (vff->hold_ptr == vff->get_ptr) ;

  /* If we are not at the end_lump, step forward, discarding current
   * lump unless hold_mark.
   */
  if (vff->get_lump != vff->end_lump)
    {
      vio_fifo_lump next ;
      next = ddl_next(vff->get_lump, list) ;

      vio_fifo_set_get_ptr(vff, next->data, next) ;

      if (hold_empty)
        {
          vio_fifo_release_head(vff, next) ;

          /* If there is a hold_mark, then had hold_ptr == get_ptr
           * and this is where we keep hold_ptr & get_ptr in sync.
           */
          if (vff->hold_mark)
            vff->hold_ptr = vff->get_ptr ;
        } ;

      /* Return the get state now                                       */
      if (vff->get_ptr < vff->get_end)
        return true ;
    } ;

  /* Still have get_ptr == get_end => nothing more to get.
   *
   * Check now for empty FIFO, and reset all pointers if that is the case.
   */
  if ((vff->get_ptr == vff->put_ptr) && hold_empty)
    {
      if (vff->hold_mark)
        assert(vff->hold_ptr == vff->get_ptr) ;

      if (vff->end_mark)
        assert(vff->end_end == vff->put_ptr) ;

      vio_fifo_ptr_reset(vff, vff->end_lump) ;
    }

  return false ;        /* nothing to get               */
} ;

/*------------------------------------------------------------------------------
 * Clear out contents of FIFO -- will continue to use the FIFO.
 *
 * If required, clears any hold mark and/or end mark.
 *
 * Keeps one FIFO lump.  (Frees everything else, including any spare.)
 *
 * Does nothing if there is no FIFO !
 */
extern void
vio_fifo_clear(vio_fifo vff, bool clear_marks)
{
  if (vff == NULL)
    return ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  vff->as_one    = vff->set ;
  if (clear_marks)
    {
      vff->hold_mark = false ;  /* Discard marks                */
      vff->end_mark  = false ;
    } ;

  if (vff->set)
    {
      vio_fifo_lump lump ;

      while (1)
        {
          lump = ddl_head(vff->base) ;
          if (lump == ddl_tail(vff->base))
            break ;

          ddl_pop(&lump, vff->base, list) ;
          XFREE(MTYPE_VIO_FIFO_LUMP, lump) ;
        } ;

      vio_fifo_ptr_reset(vff, lump) ;
    } ;

  if (vff->spare != NULL)
    XFREE(MTYPE_VIO_FIFO_LUMP, vff->spare) ;  /* sets vff->spare = NULL */

  VIO_FIFO_DEBUG_VERIFY(vff) ;
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
vio_fifo_room(vio_fifo vff)
{
  if (vff->set)
    return vff->put_end - vff->put_ptr ;
  else
    return vff->size ;
} ;

/*------------------------------------------------------------------------------
 * Need a new lump to put stuff into.
 *
 * Call when (vff->put_ptr >= vff->put_end) -- asserts that they are equal.
 *
 * If the FIFO is, in fact, empty but with at least one lump, then does not
 * allocate anything more, but releases all lumps but the last lump and then
 * resets all pointers to the start of that lump.
 *
 * Otherwise, allocates a new lump (or reuses the spare) to the requested size,
 * and updates all pointers as required.  (Allocates to vff->size if
 * requested size is zero.)
 *
 * NB: if there is an end_mark, makes sure that it advances with the put_ptr
 *     if currently end_end == the put_ptr.
 */
Private void
vio_fifo_lump_new(vio_fifo vff, size_t size)
{
  vio_fifo_lump lump ;
  size_t  std_size ;

  passert(vff->put_ptr == vff->put_end) ; /* must be end of tail lump
                                             (or both NULL)             */
  VIO_FIFO_DEBUG_VERIFY(vff) ;

  /* First, make sure that the get side is synchronised, which may advance
   * various pointers, release lumps and possibly reset now empty FIFO.
   *
   * If synchronising the get side does not yield space, then the FIFO is
   * either not set or it has something in it (including held stuff).
   */
  vio_fifo_sync_get(vff) ;

  if (vff->put_ptr < vff->put_end)
    return ;                    /* Done if have created space           */

  /* If we can use the spare, do so, otherwise make it to size          */
  lump       = vff->spare ;
  vff->spare = NULL ;

  std_size = vio_fifo_size(vff->size) ; /* normalised standard           */

  if (size <= std_size)
    size = std_size ;           /* use standard as a minimum            */
  else
#if VIO_FIFO_DEBUG
    size |= 3 ;                 /* most of the time a little bigger     */
#else
    size = vio_fifo_size(size) ; /* normalise requested size            */
#endif

  if ((lump == NULL) || (lump->size < size))
    {
      /* If there was no spare, lump == NULL and XREALLOC == XMALLOC.
       * If there was a spare that was too small, better extend that than
       * keep a sub-standard spare.
       */
      lump = XREALLOC(MTYPE_VIO_FIFO_LUMP, lump,
                                        offsetof(vio_fifo_lump_t, data[size])) ;
      lump->size = size ;
      lump->end  = lump->data + size ;
    } ;

  ddl_append(vff->base, lump, list) ;

  /* If not just allocated the first lump, set the put_ptr and normalise any
   * end_mark or update end_lump.
   *
   * If is first block to be allocated, set all pointers, taking into account
   * any end_mark and/or hold_mark.
   */
  if (vff->set)
    {
      /* Allocated new lump on the end of a not-empty fifo.
       *
       * Have to watch out for cases where the get_ptr and/or end_end are
       * equal to the put_ptr, which is about to move to the start of a new
       * lump, so as to avoid ambiguity !
       */

      if (vff->get_ptr == vff->put_ptr)
        {
          /* If the fifo is empty, the vio_fifo_sync_get() above will have
           * spotted it.  So can only get here iff there is something held in
           * the fifo behind the get_ptr.
           *
           * If was as_one, is still as_one.
           *
           * If was not as_one then must have end_mark with end_end == put_ptr,
           * which will be dealt with below.
           */
          assert(vff->hold_mark && (vff->get_ptr != vff->hold_ptr)) ;

          vff->get_ptr  = lump->data ;
          vff->get_end  = lump->data ;
          vff->get_lump = lump ;
        }
      else
        {
          /* If were as_one, then will no longer be, because put_ptr is about
           * to advance to the start of the new lump.
           */
          vff->as_one = false ;
        } ;

      if (vff->end_mark)
        {
          assert(!vff->as_one) ;

          /* The end_end follows the put_ptr iff they are equal
           *
           * If the get_ptr also equals the put_ptr, it has already advanced.
           */
          if (vff->end_end == vff->put_ptr)
            {
              vff->end_end   = lump->data ;
              vff->end_lump  = lump ;
            } ;
        }
      else
        {
          /* No end_mark => end_lump simply follows the put_ptr.        */
          vff->end_lump = lump ;
        } ;

      vff->put_ptr = lump->data ;
      vff->put_end = vio_fifo_true_lump_size(lump) ;
    }
  else
    {
      /* Allocated lump for previously empty fifo -- set all pointers to the
       * start of the lump, except for put_end.
       */
      vff->set = true ;
      vio_fifo_ptr_set(vff, lump) ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Release the given lump, provided it is neither get_lump nor end_lump.
 *
 * If don't have a spare lump, keep this one.  Otherwise, keep larger of
 * this and current spare.
 */
static void
vio_fifo_release_lump(vio_fifo vff, vio_fifo_lump lump)
{
  assert(lump != NULL) ;
  assert(lump != vff->get_lump) ;
  assert(lump != vff->end_lump) ;

  if (vff->spare != NULL)
    {
      vio_fifo_lump free ;

      free = vff->spare ;

      if (free->size > lump->size)
        {
          free = lump ;
          lump = vff->spare ;
        } ;

      XFREE(MTYPE_VIO_FIFO_LUMP, free) ;
    } ;

  vff->spare = lump ;
} ;

/*------------------------------------------------------------------------------
 * Release lumps from head up to, but not including, given lump.
 *
 * NB: must be "set" and must not attempt to release the get_lump or the
 *     end_lump.
 *
 *     So MUST advance at least the get_lump before calling this.
 *
 * It is the caller's responsibility to update get and/or hold pointers.
 */
static void
vio_fifo_release_head(vio_fifo vff, vio_fifo_lump upto)
{
  assert(vff->set) ;

  while (upto != ddl_head(vff->base))
    {
      vio_fifo_lump lump ;
      vio_fifo_release_lump(vff, ddl_pop(&lump, vff->base, list)) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Release lumps from tail back to, but not including, given lump.
 *
 * NB: must be "set" and must not attempt to release the get_lump or the
 *     end_lump.
 *
 * It is the caller's responsibility to update end and/or put pointers.
 */
static void
vio_fifo_release_tail(vio_fifo vff, vio_fifo_lump backto)
{
  assert(vff->set) ;

  while (backto != ddl_tail(vff->base))
    {
      vio_fifo_lump lump ;
      vio_fifo_release_lump(vff, ddl_crop(&lump, vff->base, list)) ;
    } ;
} ;

/*==============================================================================
 * Put data to the FIFO.
 */

/*------------------------------------------------------------------------------
 * Put 'n' bytes -- allocating as required.
 */
extern void
vio_fifo_put_bytes(vio_fifo vff, const char* src, size_t n)
{
  VIO_FIFO_DEBUG_VERIFY(vff) ;

  while (n > 0)
    {
      size_t take ;

      if (vff->put_ptr >= vff->put_end)
        vio_fifo_lump_new(vff, 0) ;     /* traps put_ptr > put_end    */

      take = (vff->put_end - vff->put_ptr) ;
      if (take > n)
        take = n ;

      memcpy(vff->put_ptr, src, take) ;
      vff->put_ptr += take ;

      src += take ;
      n   -= take ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to FIFO -- cf printf()
 *
 * Returns: >= 0 -- number of bytes written
 *           < 0 -- failed (unlikely though that is)
 */
extern int
vio_fifo_printf(vio_fifo vff, const char* format, ...)
{
  va_list args;
  int      len ;

  va_start (args, format);
  len = vio_fifo_vprintf(vff, format, args);
  va_end (args);

  return len;
} ;

/*------------------------------------------------------------------------------
 * Formatted print to FIFO -- cf vprintf()
 *
 * Does nothing if vff is NULL !
 *
 * Returns: >= 0 -- number of bytes written
 *           < 0 -- failed (unlikely though that is)
 *
 * NB: does not extend an existing lump in order to make things fit, but
 *     splits the result across two lumps.  This ensures that at all times
 *     pointers into existing lumps are stable -- so pointer returned by
 *     vio_fifo_get_lump(), for example, cannot be upset !
 */
extern int
vio_fifo_vprintf(vio_fifo vff, const char *format, va_list args)
{
  va_list  ac ;
  int      len ;
  int      have ;
  int      had ;
  int      need ;
  char*    last ;

  if (vff == NULL)
    return 0 ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  /* First the simple way.
   *
   * Note that vsnprintf() returns the length of what it would like to
   * have produced, if it had the space.  That length does not include
   * the trailing '\0'.  In the meantime, it has output as much as it
   * can, with a trailing '\0'.
   */
  assert(vff->put_ptr <= vff->put_end) ;

  have = vff->put_end - vff->put_ptr ;  /* what can do in current lump
                                           if any.                      */
  va_copy(ac, args) ;
  len = vsnprintf(vff->put_ptr, have, format, ac) ;
  va_end(ac) ;

  if ((len < have) || (len == 0))       /* OK, or failed !              */
    {
      if (len > 0)
        vff->put_ptr += len ;   /* advance put_ptr as required          */

      return len ;
    } ;

  /* Now know that we need len + 1 bytes in all to complete the task, and
   * that it has written have - 1 bytes to the existing lump (if any).
   *
   * Also, len > 0.
   *
   * Allocate a new lump in which we can write the entire result, even if
   * that is a non-standard size.
   */
  need = len + 1 ;              /* need includes the '\0'               */
  had  = have ;

  if (had > 0)
    vff->put_ptr += had ;       /* step to the end                      */
  last = vff->put_ptr ;         /* point at end of lump (NULL if none)  */

  vio_fifo_lump_new(vff, need) ;/* new lump to do it all                */

  have = vff->put_end - vff->put_ptr ;
  assert(have >= need) ;        /* have >= 2                            */

  /* We really expect to get the same result a second time !            */
  va_copy(ac, args) ;
  len = vsnprintf(vff->put_ptr, have, format, ac) ;
  va_end(ac) ;

  /* Since have >= what previously said it needed, things have gone
   * badly wrong if the new len is >= have.
   *
   * Also, things have gone badly wrong if new len is < what previously
   * had, which was then not enough !
   *
   * Also, things have gone badly wrong if new len == 0, because previously
   * it was > 0 !
   */
  if ((len >= have) || (len < had) || (len == 0))
    return (len < 0) ? len : -1 ;

  /* Move result around if required -- len >= had                       */
  have = len ;
  if (had > 0)
    {
      char* frag ;
      frag = vff->put_ptr + had ;   /* first character to keep  */
      *(last - 1) = *(frag - 1) ;   /* replace the '\0'         */

      have -= had ;                 /* amount to keep           */
      if (have > 0)
        memmove(vff->put_ptr, frag, have) ;
    } ;

  /* Advance the put_ptr past what we have in the new lump.             */
  vff->put_ptr += have ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  return len ;
} ;

/*------------------------------------------------------------------------------
 * Read part of file into FIFO -- assuming non-blocking file
 *
 * Will read up to the end of the current lump, then will read as may whole
 * lumps as are requested -- request of 0 reads up to the end of the current
 * lump (at least 1 byte).  Will stop if would block.
 *
 * Except where blocking intervenes, this reads in units of the lump size.
 *
 * Returns: 0..n -- number of bytes read
 *         -1 => failed -- see errno
 *         -2 => EOF met immediately
 *
 * Note: will work perfectly well for a non-blocking file -- which should
 *       never return EAGAIN/EWOULDBLOCK, so will return from here with
 *       something, error or EOF.
 */
extern int
vio_fifo_read_nb(vio_fifo vff, int fd, uint request)
{
  size_t total ;

  total = 0 ;

  do
    {
      int  got ;

      if (vff->put_ptr >= vff->put_end)
        {
          vio_fifo_lump_new(vff, 0) ;     /* traps put_ptr > put_end      */

          if (request > 0)
            --request ;
        } ;

      got = read_nb(fd, vff->put_ptr, vff->put_end - vff->put_ptr) ;

      if (got <= 0)
        {
          if (got == -2)                /* EOF met                      */
            return (total > 0) ? (int)total : got ;
          else
            return (got  == 0) ? (int)total : got ;
        } ;

      vff->put_ptr += got ;
      total        += got ;

    } while (request > 0) ;

  return total ;
} ;

/*==============================================================================
 * Copy operations -- from one fifo to another.
 */

/*------------------------------------------------------------------------------
 * Copy src fifo (everything from get_ptr to end_mark or put_ptr) to dst fifo.
 *
 * Create a dst fifo if there isn't one.
 *
 * Appends to the dst fifo.
 *
 * Does not change the src fifo in any way.
 */
extern vio_fifo
vio_fifo_copy(vio_fifo dst, vio_fifo src)
{
  if (dst == NULL)
    dst = vio_fifo_init_new(dst, 0) ;

  if ((src != 0) && (src->set))
    {
      vio_fifo_lump src_lump ;
      char*         src_ptr ;

      src_lump = src->get_lump ;
      src_ptr  = src->get_ptr ;

      while (1)
        {
          char* src_end ;

          if (src_lump != src->end_lump)
            src_end = src_lump->end ;
          else
            src_end = (src->end_mark) ? src->end_end : src->put_ptr ;

          vio_fifo_put_bytes(dst, src_ptr, src_end - src_ptr) ;

          if (src_lump == src->end_lump)
            break ;

          src_lump = ddl_next(src_lump, list) ;
          src_ptr  = src_lump->data ;
        } ;
    } ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Copy tail of src fifo (everything from end_mark to put_ptr) to dst fifo.
 *
 * Create a dst fifo if there isn't one.
 *
 * Appends to the dst fifo.
 *
 * Does not change the src fifo in any way.
 */
extern vio_fifo
vio_fifo_copy_tail(vio_fifo dst, vio_fifo src)
{
  if (dst == NULL)
    dst = vio_fifo_init_new(dst, 0) ;

  if ((src != 0) && (src->end_mark) && (src->set))
    {
      vio_fifo_lump src_lump ;
      char*         src_ptr ;
      vio_fifo_lump tail ;

      src_lump = src->end_lump ;
      src_ptr  = src->end_end ;
      tail     = ddl_tail(src->base) ;

      while (1)
        {
          char* src_end ;

          if (src_lump != tail)
            src_end = src_lump->end ;
          else
            src_end = src->put_ptr ;

          vio_fifo_put_bytes(dst, src_ptr, src_end - src_ptr) ;

          if (src_lump == tail)
            break ;

          src_lump = ddl_next(src_lump, list) ;
          src_ptr  = src_lump->data ;
        } ;
    } ;

  return dst ;
} ;

/*==============================================================================
 * End Mark Operations.
 */

/*------------------------------------------------------------------------------
 * Set end_mark at the current put position.
 *
 * If there was an end_mark before, move it (forward) to the current put_ptr,
 * which keeps everything in between in the FIFO.
 *
 * Set the get_end to the new reality.
 */
extern void
vio_fifo_set_end_mark(vio_fifo vff)
{
  if (vff->set)
    {
      vio_fifo_sync_get(vff) ;  /* in case is currently empty   */

      vff->end_lump  = ddl_tail(vff->base) ;
      vff->end_end   = vff->put_ptr ;

      vff->get_end   = (vff->get_lump == vff->end_lump) ? vff->end_end
                                                        : vff->get_lump->end ;
    } ;

  vff->as_one   = false ;       /* not as_one with end_mark     */
  vff->end_mark = true ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * If there is an end mark, advance it to the put_ptr.
 *
 * If there was an end_mark before, move it (forward) to the current put_ptr,
 * which keeps everything in between in the FIFO.
 *
 * If there was no end_mark before, do nothing.
 *
 * Set the get_end to the new reality.
 */
extern void
vio_fifo_step_end_mark(vio_fifo vff)
{
  if (vff->end_mark)
    vio_fifo_set_end_mark(vff) ;
} ;

/*------------------------------------------------------------------------------
 * If there is an end_mark, clear it -- everything between end mark and
 * current put_ptr is kept in the FIFO.
 *
 * Set the get_end to the new reality.
 */
extern void
vio_fifo_clear_end_mark(vio_fifo vff)
{
  if (vff->end_mark)
    {
      vff->end_mark = false ;

      if (vff->set)
        {
          vff->end_lump  = ddl_tail(vff->base) ;
          vff->end_end   = NULL ;

          vff->as_one    = (vff->get_lump == vff->end_lump) ;
                                        /* since now no end_mark        */
          if (!vff->as_one)
            vff->get_end = vff->get_lump->end ;
                                        /* would have been end_end      */

          vio_fifo_sync_get(vff) ;      /* sets get_end if as_one and
                                           tidies up if now empty (!)   */
        } ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Move put_ptr back to the end mark, if any, and discard data.
 *
 * If there is an end_mark, keep it if required.
 *
 * If there is no end mark, do nothing.
 */
extern void
vio_fifo_back_to_end_mark(vio_fifo vff, bool keep)
{
  if (vff->end_mark)
    {
      if (vff->set)
        {
          vio_fifo_release_tail(vff, vff->end_lump) ;

          vff->put_ptr  = vff->end_end ;
          vff->put_end  = vio_fifo_true_lump_size(vff->end_lump) ;

          /* If retaining the existing end_mark, we retain the end_end and
           * the current as_one (false).
           *
           * Otherwise...
           */
          if (!keep)
            {
              vff->end_mark = false ;
              vff->end_end  = NULL ;
              vff->as_one   = (vff->get_lump == vff->end_lump) ;
            } ;

          vio_fifo_sync_get(vff) ;              /* in case now empty    */

          VIO_FIFO_DEBUG_VERIFY(vff) ;
        }
      else
        vff->end_mark = keep ;
    } ;
} ;

/*==============================================================================
 * Get data from the FIFO.
 */

/*------------------------------------------------------------------------------
 * Get upto 'n' bytes.
 *
 * Returns: number of bytes got -- may be zero.
 */
extern size_t
vio_fifo_get_bytes(vio_fifo vff, void* dst, size_t n)
{
  size_t have ;
  void*  dst_in ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  dst_in = dst ;
  while (vio_fifo_sync_get(vff) && (n > 0))
    {
      have = vff->get_end - vff->get_ptr ;

      if (have > n)
        have = n ;

      memcpy(dst, vff->get_ptr, have) ;
      vff->get_ptr += have ;
      dst = (char*)dst + have ;

      n -= have ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;

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
Private int
vio_fifo_get_next_byte(vio_fifo vff)
{
  if (vio_fifo_sync_get(vff))
    return (uchar)*vff->get_ptr++ ;

  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to as many bytes as are available in the current lump (or next
 * lump if nothing available in the current).
 *
 * Returns: address of next byte to get, *p_have = number of bytes available
 *      or: NULL => FIFO is empty,       *p_have = 0
 *
 * If the FIFO is not empty and not at the end_mark, will return pointer to at
 * least one byte.  There may be more bytes to get in further lumps.
 */
extern void*
vio_fifo_get(vio_fifo vff, size_t* p_have)
{
  if (vio_fifo_sync_get(vff))
    {
      *p_have = (vff->get_end - vff->get_ptr) ;
      return vff->get_ptr ;
    } ;

  *p_have = 0 ;
  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Step FIFO past bytes used.
 *
 * Can be called after a vio_fifo_get() or vio_fifo_step_get().
 *
 * NB: the "step" argument MUST not exceed the "have" previously returned.
 */
extern void
vio_fifo_step(vio_fifo vff, size_t step)
{
  vff->get_ptr += step ;
  vio_fifo_sync_get(vff) ;      /* ensure up to date with that  */

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Step FIFO past bytes used, the get pointer to as many bytes as are available
 * in the current lump (or next lump if nothing available in the current).
 *
 * Same as vio_fifo_step() followed by vio_fifo_get().
 *
 * Can be called after a vio_fifo_get() or vio_fifo_step_get().
 *
 * NB: the "step" argument MUST not exceed the "have" previously returned.
 */
extern void*
vio_fifo_step_get(vio_fifo vff, size_t* p_have, size_t step)
{
  vff->get_ptr += step ;

  if (vio_fifo_sync_get(vff))
    {
      *p_have = (vff->get_end - vff->get_ptr) ;
      return vff->get_ptr ;
    } ;

  *p_have = 0 ;
  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Write contents of FIFO -- assuming non-blocking file
 *
 * Will write all of FIFO up to end mark or put_ptr, or upto but excluding
 * the end_lump.
 *
 * Returns: > 0 => blocked
 *            0 => all gone (up to last lump if !all)
 *          < 0 => failed -- see errno
 *
 * Note: will work perfectly well for a non-blocking file -- which should
 *       never return EAGAIN/EWOULDBLOCK, so will return from here "all gone".
 */
extern int
vio_fifo_write_nb(vio_fifo vff, int fd, bool all)
{
  char*   src ;
  size_t  have ;

  while ((src = vio_fifo_get(vff, &have)) != NULL)
    {
      int     done ;

      if ((vff->get_lump == vff->end_lump) && !all)
        break ;                 /* don't write last lump        */

      done = write_nb(fd, src, have) ;

      if (done < 0)
        return -1 ;             /* failed                       */

      vio_fifo_step(vff, done) ;

      if (done < (int)have)
        return 1 ;              /* blocked                      */
    } ;

  return 0 ;                    /* all gone                     */
} ;

/*------------------------------------------------------------------------------
 * Write contents of FIFO -- assuming blocking file
 *
 * Will write all of FIFO up to end mark or put_ptr.
 *
 * Returns:   0 => all gone
 *          < 0 => failed -- see errno
 *
 * Note: will work perfectly well for a non-blocking file -- which should
 *       never return EAGAIN/EWOULDBLOCK, so will return from here "all gone".
 */
extern int
vio_fifo_fwrite(vio_fifo vff, FILE* file)
{
  char*   src ;
  size_t  have ;

  while ((src = vio_fifo_get(vff, &have)) != NULL)
    {
      size_t done ;

      done = fwrite(src, have, 1, file) ;

      if (done < 1)
        return -1 ;             /* failed                       */

      vio_fifo_step(vff, have) ;
    } ;

  return 0 ;                    /* all gone                     */
} ;

/*------------------------------------------------------------------------------
 * Skip get_ptr to the current end -- which may be the current end_mark.
 *
 * Does not clear any hold_mark or end_mark.
 */
extern void
vio_fifo_skip_to_end(vio_fifo vff)
{
  vio_fifo_sync_get(vff) ;      /* ensure all straight          */

  /* Setting the get_ptr to the start of the end_lump does the bulk
   * of the work -- then just skip to the get_end which that sets.
   */
  vio_fifo_set_get_ptr(vff, vff->end_lump->data, vff->end_lump) ;
  vff->get_ptr = vff->get_end ;

  vio_fifo_sync_get(vff) ;      /* crunch                       */
} ;

/*==============================================================================
 * Hold Mark Operations.
 */

/*------------------------------------------------------------------------------
 * If there is a hold_mark, clear it -- discard all contents up to the
 * current get_ptr.
 *
 * Set hold_mark at the current get_ptr.
 */
extern void
vio_fifo_set_hold_mark(vio_fifo vff)
{
  if (vff->set)
    {
      if (vff->hold_mark)
        vio_fifo_clear_hold_mark(vff) ; /* clear existing mark & sync   */
      else
        vio_fifo_sync_get(vff) ;        /* ensure all straight          */

      vff->hold_ptr  = vff->get_ptr ;
    } ;

  vff->hold_mark = true ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * If there is a hold_mark, clear it -- discard all contents up to the
 * current get_ptr.
 *
 * The get_ptr is synchronised.
 */
extern void
vio_fifo_clear_hold_mark(vio_fifo vff)
{
  /* Make sure all is up to date, and in particular that the get_ptr
   * is not sitting at the end of a lump when there is a following lump.
   */
  vio_fifo_sync_get(vff) ;

  if ((vff->hold_mark) && (vff->set))
    {
      /* Release everything upto but not including the current get_lump.
       *
       * This has no effect on the get_ptr etc.  so they remain straight.
       */
      vio_fifo_release_head(vff, vff->get_lump) ;

      vff->hold_ptr  = NULL ;
    } ;

  vff->hold_mark = false ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * If there is an hold_mark, reset get_ptr *back* to it, and leave the mark
 * set or clear.
 *
 * If there is no hold mark, set one at the current position, if required.
 */
extern void
vio_fifo_back_to_hold_mark(vio_fifo vff, bool mark)
{
  if (vff->hold_mark)
    {
      if (vff->set)
        {
          vio_fifo_set_get_ptr(vff, vff->hold_ptr, ddl_head(vff->base)) ;
                                        /* Set back to hold position    */

          vff->end_mark = mark ;        /* new state                    */
          if (!mark)
            vff->hold_ptr = NULL ;      /* clear if required            */

          vio_fifo_sync_get(vff) ;      /* to be absolutely sure !      */
        } ;

      VIO_FIFO_DEBUG_VERIFY(vff) ;
    }
  else if (mark)
    vio_fifo_set_hold_mark(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Set the get_ptr to the hold_mark plus the given offset.
 *
 * If the offset is zero, and there was no hold mark, set one at the current
 * get_ptr.
 *
 * If the offset is not zero, it is a mistake to do this if there is no
 * hold_mark, or if the offset would take the get_ptr beyond the end_mark
 * (if any) or the put_ptr.
 */
extern void
vio_fifo_set_get_wrt_hold(vio_fifo vff, size_t hold_offset)
{
  if (hold_offset == 0)
    {
      /* Offset of zero can be set under all conditions                 */
      vio_fifo_back_to_hold_mark(vff, true) ;
    }
  else
    {
      vio_fifo_lump lump ;
      char*         ptr ;

      /* There must be a hold_mark and must have something held         */
      assert(vff->hold_mark && vff->set) ;

      lump = ddl_head(vff->base) ;
      ptr  = vff->hold_ptr ;

      while (1)
        {
          size_t have ;

          if (lump == vff->end_lump)
            have = (vff->end_mark ? vff->end_end : vff->put_ptr) - ptr ;
          else
            have = lump->end - ptr ;

          if (have <= hold_offset)
            break ;

          hold_offset -= have ;

          assert(lump != vff->end_lump) ;

          lump = ddl_next(lump, list) ;
          ptr  = lump->data ;
        } ;

      /* Note that may be about to set the get_ptr to the end of the
       * current lump, which will be correct if that is the end of the
       * fifo, but in any case is dealt with by vio_fifo_sync_get().
       */
      vio_fifo_set_get_ptr(vff, ptr + hold_offset, lump) ;
      vio_fifo_sync_get(vff) ;
    } ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*==============================================================================
 * For debug purposes -- verify the state of the given FIFO
 */
Private void
vio_fifo_verify(vio_fifo vff)
{
  vio_fifo_lump head ;
  vio_fifo_lump lump ;
  vio_fifo_lump tail ;

  head = ddl_head(vff->base) ;
  tail = ddl_tail(vff->base) ;

  /* If nothing allocated, should all be NULL & !vff->set       */
  /* If something allocated, tail must not be NULL              */
  if (head == NULL)
    {
      if ( (tail != NULL)
          || (vff->set)
          || (vff->as_one)
          || (vff->hold_ptr != NULL)
          || (vff->get_lump != NULL)
          || (vff->get_ptr  != NULL)
          || (vff->get_end  != NULL)
          || (vff->end_lump != NULL)
          || (vff->end_end  != NULL)
          || (vff->put_ptr  != NULL)
          || (vff->put_end  != NULL) )
        zabort("nothing allocated, but not all NULL") ;
      return ;
    }
  else
    {
      if (tail == NULL)
        zabort("head not NULL, but tail is") ;
    } ;

  /* Must now be set !                                                  */
  if (!vff->set)
    zabort("head not NULL, but set is false") ;

  /* Make sure that the lump pointers all work
   *
   * When finished, know that head <= get_lump <= end_lump <= tail.
   */
  lump = head ;
  while (lump != vff->get_lump)
    {
      lump = ddl_next(lump, list) ;
      if (lump == NULL)
        zabort("ran out of lumps looking for get_lump") ;
    } ;

  while (lump != vff->end_lump)
    {
      lump = ddl_next(lump, list) ;
      if (lump == NULL)
        zabort("ran out of lumps looking for end_lump") ;
    } ;

  while (lump != tail)
    {
      lump = ddl_next(lump, list) ;
      if (lump == NULL)
        zabort("ran out of lumps looking for tail") ;
    } ;

  /* Check that all the pointers are within respective lumps
   *
   * Know that put_end is always tail->end, but get_end need not be.
   *
   * When finished, know that:
   *
   *   - get_lump == head if !hold_mark
   *   - end_lump == tail if !end_mark
   *   - that all pointers are within their respective lumps
   *   - all ptr are <= their respective ends
   *   - if hold_mark: hold_ptr <= get_ptr or head != get_lump
   *   - if end_mark:  end_end  <= put_ptr or tail != end_lump
   */
  if (vff->hold_mark)
    {
      if ( (head->data > vff->hold_ptr)
          ||            (vff->hold_ptr > head->end) )
        zabort("hold pointer outside the head lump") ;

      if ((vff->get_lump == head) && (vff->hold_ptr > vff->get_ptr))
        zabort("hold pointer greater than get pointer") ;
    }
  else
    {
      if (vff->hold_ptr != NULL)
        zabort("no hold_mark, but hold pointer not NULL") ;
      if (vff->get_lump != head)
        zabort("no hold_mark, but get_lump is not head") ;
    } ;

  if ( (vff->get_lump->data > vff->get_ptr)
      ||                     (vff->get_ptr > vff->get_end)
      ||                                    (vff->get_end > vff->get_lump->end))
    zabort("get pointers outside the get lump") ;

  if (vff->end_mark)
    {
      if ( (vff->end_lump->data > vff->end_end)
          ||                     (vff->end_end > vff->end_lump->end) )
        zabort("end pointer outside the end lump") ;

      if ((vff->end_lump == tail) && (vff->end_end > vff->put_ptr))
        zabort("end pointer greater than put pointer") ;
    }
  else
    {
      if (vff->end_end != NULL)
        zabort("no end_mark, but end end not NULL") ;
      if (vff->end_lump != tail)
        zabort("no end_mark, but end_lump is not tail") ;
    } ;

  if ( (tail->data > vff->put_ptr)
      ||            (vff->put_ptr > vff->put_end)
      ||                           (vff->put_end != tail->end) )
    zabort("put pointers outside the tail lump") ;

  /* The as_one state & get_end
   */
  if      (vff->get_lump != vff->end_lump)
    {
      if (vff->as_one)
        zabort("get_lump != end_lump, but as_one true") ;
      if (vff->get_end != vff->get_lump->end)
        zabort("get_lump != end_lump, but get_end != get_lump->end") ;
    }
  else if (vff->end_mark)
    {
      if (vff->as_one)
        zabort("end_mark true, but as_one also true") ;
      if (vff->get_end != vff->end_end)
        zabort("get_lump == end_lump and end_mark, but get_end != end_end") ;
    }
  else
    {
      if (!vff->as_one)
        zabort("get_lump == end_lump and !end_mark, but as_one not true") ;
      if (vff->get_end > vff->put_ptr)
        zabort("is as_one, but get_end > put_ptr") ;
    } ;
} ;

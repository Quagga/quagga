/* Generic vector interface routine -- functions
 * Copyright (C) 1997 Kunihiro Ishiguro
 *
 * 24-Nov-2009  -- extended to add a number of new operations on vectors.
 *                 Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <zebra.h>

#include "vector.h"
#include "memory.h"

/* Vectors are implemented as a structure which points to an array of pointers
 * to vector items.  That array -- the body of the vector -- can change size,
 * and therefore may move around in memory.
 *
 * The vector structure may be statically allocated, embedded in another
 * structure, or allocated dynamically.  In any case the vector operations
 * require the address of the vector structure -- see typedef for vector.
 *
 * Vector items are accessed by index, fetching and storing pointers to
 * those items.  Can also push and pop items.
 *
 * A vector has a known (logical) end position.  Everything beyond the end is
 * defined to be NULL.  When a vector is initialised it is set empty.
 *
 * At any given moment the vector body has a limit on the number of items it
 * can accommodate (the physical end).
 *
 * A vector will grow to accommodate what is put into it.  Adding items beyond
 * the (logical) end moves it.  Adding items beyond the (physical) limit causes
 * the body to be extended to suit, and to leave some spare space for future
 * expansion.
 *
 * While the vector is small (see VECTOR_LIMIT_DOUBLE_MAX) the body will grow by
 * doubling in size.  When it is larger, it grows to be multiples of
 * VECTOR_LIMIT_DOUBLE_MAX.
 *
 * Deleting items reduces the (logical) end position, but does NOT release
 * memory -- the (physical) limit is not changed.
 *
 * To release memory: vector_chop will release everything beyond the current
 * end; vector_decant will create a new body of exactly the current size,
 * releasing the old body; vector_discard will release everything beyond a
 * given position.
 *
 * NB: you can set a vector item to be NULL.  If you set a vector item beyond
 *     the current end, NULL items are inserted in the vector.
 *
 * NB: when setting a vector item it is the caller's responsibility to
 *     deallocate any pre-existing value of the item.
 *
 * NB: when deleting items it is also the caller's responsibility to deallocate
 *     any values that require it.
 *
 * Implementation Notes
 *
 * Everything beyond the (logical) end is implicitly NULL.
 *
 * Actual memory between (logical) end and (physical) limit is UNDEFINED.  So
 * when advancing the end some care has to be taken to ensure that any new
 * items in the vector are either set to something or cleared to NULL.
 *
 * It would have been possible to ensure that everything between end and limit
 * is cleared to NULL, but that is more work -- in particular it creates work
 * when it is not always required.
 */

#define P_ITEMS_SIZE(n) SIZE(p_vector_item, n)

/*==============================================================================
 * Initialisation, allocation, reset etc.
 */

/*------------------------------------------------------------------------------
 * Initialise a brand new vector, setting it empty.
 *
 * Allocates vector structure if none given -- that is, if v == NULL.
 *
 * If size is given as zero, no body is allocated, otherwise body of exactly
 * the required size is allocated.
 *
 * NB: discards any existing vector body -- so it is the caller's responsibility
 *     to release any existing body, and any items in that body.
 */
extern vector
vector_init_new(vector v, vector_length_t limit)
{
  if (v == NULL)
    v = XCALLOC(MTYPE_VECTOR, sizeof(struct vector)) ;
  else
    memset(v, 0, sizeof(struct vector)) ;

  if (limit != 0)
    {
      v->p_items = XMALLOC(MTYPE_VECTOR_BODY, P_ITEMS_SIZE(limit)) ;
      v->limit   = limit ;
    } ;

  return v ;
} ;

/*------------------------------------------------------------------------------
 * Initialize vector : allocate memory and return vector.
 *                     allocates body with at least 1 entry.
 *
 * This is a "legacy" function.
 */
extern vector
vector_init (vector_length_t limit)
{
  return vector_init_new(NULL, limit ? limit : 1) ;     /* at least 1 entry */
} ;

/*------------------------------------------------------------------------------
 * Basic: free the vector body and the vector structure.
 *
 * NB: it is the caller's responsibility to release any vector item values
 *     *before* doing this.
 */
void
vector_free (vector v)
{
  XFREE (MTYPE_VECTOR_BODY, v->p_items);
  XFREE (MTYPE_VECTOR, v);
} ;

/*------------------------------------------------------------------------------
 * Re-initialise vector (or create new one), setting it empty.
 *
 * Allocates vector structure if none given -- that is, if v == NULL.
 *
 * If size is given as zero, no body is allocated, but any existing body is
 * retained.  (vector_reset() will discard body.)
 *
 * Otherwise ensures existing body is at least the required size, or a body
 * of exactly the required size is allocated.
 *
 * NB: when re-initialising an existing vector it is the caller's responsibility
 *     to release any vector item values *before* doing this.
 * */
extern vector
vector_re_init(vector v, vector_length_t limit)
{
  if ((v == NULL) || (v->p_items == NULL))
    return vector_init_new(v, limit) ;

  v->end = 0 ;

  if (v->limit < limit)
    {
      v->p_items = XREALLOC(MTYPE_VECTOR_BODY, v->p_items,
                                                          P_ITEMS_SIZE(limit)) ;
      v->limit = limit ;
    } ;

  return v ;
} ;

/*------------------------------------------------------------------------------
 * Free the vector body, and (if required) free the vector structure.
 *
 * Return NULL if releases vector, otherwise the address of the vector.
 *
 * NB: it is the caller's responsibility to release any vector item values
 *     *before* doing this.
 */
vector
vector_reset(vector v, free_keep_b free_structure)
{
  if (v == NULL)
    return NULL ;       /* allow for already freed vector       */

  if (v->p_items != NULL)
    XFREE(MTYPE_VECTOR_BODY, v->p_items) ;

  if (free_structure)
    {
      confirm(free_it == true) ;
      XFREE(MTYPE_VECTOR, v) ;
      return NULL ;
    }
  else
    return vector_init_new(v, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Set vector length to be (at least) the given fixed length.
 *
 * There must be a vector.
 *
 * Does nothing if the vector is already as long or longer than the given
 * length.
 *
 * If the body is not big enough for the new length, allocates or extends to
 * exactly the new length.  Otherwise, leaves body as it is.
 *
 * Appends NULLs as required to extend to the required length.
 *
 * Note that the existing contents of the vector are preserved in all cases.
 */
Private void
vector_set_new_min_length(vector v, vector_length_t len)
{
  assert (v != NULL) ;

  if (len > v->limit)
    {
      if (v->p_items == NULL)
        v->p_items = XMALLOC(MTYPE_VECTOR_BODY, P_ITEMS_SIZE(len)) ;
      else
        v->p_items = XREALLOC(MTYPE_VECTOR_BODY, v->p_items,
                                                            P_ITEMS_SIZE(len)) ;
      v->limit = len ;
    } ;

  if (v->end < len)
    vector_extend(v, len) ;
} ;

/*------------------------------------------------------------------------------
 * Pop item from vector, stepping past any NULLs.
 * If vector is empty, free the body and, if required, the vector structure.
 *
 * Useful for emptying out and discarding a vector:
 *
 *     while ((p_v = vector_ream_out(v, 1)))
 *       ... do what's required to release the item p_v
 *
 * Returns NULL if vector was empty and has now been freed as required.
 */
p_vector_item
vector_ream(vector v, free_keep_b free_structure)
{
  p_vector_item p_v ;

  if (v == NULL)
    return NULL ;

  while (v->end != 0)
    {
      p_v = v->p_items[--v->end] ;
      if (p_v != NULL)
        return p_v ;    /* return non-NULL item */
    } ;

  /* vector is empty: free the body, and (if required) the vector structure.  */
  vector_reset(v, free_structure) ;

  return NULL ;         /* signals end          */
} ;

/*==============================================================================
 * Unset item, condensing and trimming vector.
 *
 * These are legacy operations.
 */

/*------------------------------------------------------------------------------
 * Unset item at given index (ie set it NULL).
 *
 * Return the old value of the item.
 *
 * If the item at the current (logical) end of the vector is NULL, move the
 * end backwards until finds a non-NULL item, or the vector becomes empty.
 */
extern p_vector_item
vector_unset_item(vector v, vector_index_t i)
{
  p_vector_item was ;

  if (i < v->end)
    {
      was = v->p_items[i] ;
      v->p_items[i] = NULL ;
    }
  else if (v->end == 0)
    return NULL ;       /* avoid test for last entry NULL if is empty   */
  else
    was = NULL ;

  if (v->p_items[v->end - 1] == NULL)
    vector_trim(v) ;

  return was ;
} ;

/*------------------------------------------------------------------------------
 * Trim any NULL entries at the current (logical) end of the vector.
 *
 * Returns the (new) length (end) of the vector.
 */
extern vector_length_t
vector_trim(vector v)
{
  vector_length_t e = v->end ;
  while ((e > 0) && (v->p_items[e - 1] == NULL))
    --e ;
  v->end = e ;
  return e ;
} ;

/*------------------------------------------------------------------------------
 * Removes any NULL entries from the given vector.
 *
 * Returns the (new) length (end) of the vector.
 */
extern vector_length_t
vector_condense(vector v)
{
  vector_length_t e = 0 ;
  vector_index_t j ;

  /* Find first NULL, if any                    */
  while ((e < v->end) && (v->p_items[e] != NULL))
    ++e ;

  /* Quit if no NULLs (or vector is empty)      */
  if (e == v->end)
    return e ;

  /* Shuffle any remaining non-NULL down        */
  for (j = e + 1 ; j < v->end ; ++j)
    if (v->p_items[j] != NULL)
      v->p_items[e++] = v->p_items[j] ;

  v->end = e ;

  return e ;
} ;

/*==============================================================================
 * Inserting and deleting items.
 */

/*------------------------------------------------------------------------------
 * Insert item in vector, before item at given position
 * Move items and extend vector as required.
 */
extern void
vector_insert_item(vector v, vector_index_t i, void* p_v)
{
  if ((i == v->end) && (i < v->limit))
    ++v->end ;
  else
    vector_insert(v, i, 1) ;

  v->p_items[i] = (p_vector_item)p_v ;
} ;

/*------------------------------------------------------------------------------
 * Insert item in vector at given position with rider:
 *
 *   rider <  0 -- insert before the item at the given position
 *   rider == 0 -- insert at the given position -- REPLACING any existing value
 *   rider >  0 -- insert after the item at the given position
 *
 * NB: when an item is replaced, it is the caller's responsibility to release
 *     any memory used by the item, if required.
 *
 * Move items and extend vector as required.
 */
extern void
vector_insert_item_here(vector v, vector_index_t i, int rider,
                                                          p_vector_item p_v)
{
  if (rider == 0)
    return vector_set_item(v, i, p_v) ;

  if (rider > 0)
    ++i ;       /* insert before next item */
  vector_insert_item(v, i, p_v) ;
} ;

/*------------------------------------------------------------------------------
 * Delete item from vector.
 *
 * Move items as required.  Reduces number of items in the vector (unless
 * the item in question is beyond the end of the vector !)
 *
 * Returns: the item that has just been deleted.
 *
 * NB: it is the caller's responsibility to release memory used by any
 *     current value of the item, if required.
 *
 * NB: does NOT change the size of the vector body.
 */
extern p_vector_item
vector_delete_item(vector v, vector_index_t i)
{
  p_vector_item p_e ;
  if (i < v->end)
    {
      p_e = v->p_items[i] ;     /* pick up the current value */
      if (i != (v->end - 1))
        vector_delete(v, i, 1) ;
      else
        v->end = i ;
      return p_e ;
    }
  else
    return NULL ;
} ;

/*==============================================================================
 * Moving items within vector.
 */

/*------------------------------------------------------------------------------
 * Move item in vector from source position to destination position.
 *
 * Moves intervening items up or down as required.
 *
 * Extends vector to include the destination, if required.
 *
 * A source item beyond the end of the vector is implicitly NULL.
 */
extern void
vector_move_item(vector v, vector_index_t i_dst, vector_index_t i_src)
{
  p_vector_item* pp_s ;
  p_vector_item* pp_d ;
  p_vector_item p_e ;

  vector_length_t old_end = v->end ;

  /* Worry about whether both source and destination exist.        */
  if (i_dst >= old_end)
    {
      vector_insert(v, i_dst, 1) ; /* ensure destination exists    */
      if (i_src >= old_end)
        return ;                   /* both were beyond the end     */
    }
  else if (i_src >= old_end)
    {
      i_src = old_end ;            /* clamp to just beyond last    */
      vector_insert(v, i_src, 1) ; /* create empty entry           */
    } ;

  if (i_dst == i_src)              /* avoid work and edge case     */
    return ;

  /* Both src and dst are within the vector and src != dst         */
  pp_s = &v->p_items[i_src] ;     /* address of src entry          */
  pp_d = &v->p_items[i_dst] ;     /* address of dst entry          */
  p_e = *pp_s ;                   /* pick up item to move          */
  if (i_src < i_dst)
    memmove(pp_s, pp_s+1, P_ITEMS_SIZE(i_dst - i_src)) ;
  else
    memmove(pp_d+1, pp_d, P_ITEMS_SIZE(i_src - i_dst)) ;
  *pp_d = p_e ;                   /* put down the item to move     */
} ;

/*------------------------------------------------------------------------------
 * Move item in vector to given position with rider:
 *
 *   rider <  0 -- move to before the item at the given position
 *   rider == 0 -- move to replace item at the given position
 *   rider >  0 -- insert after the item at the given position
 *
 * NB: it is the caller's responsibility to release the any existing value
 *     that will be replaced.
 *
 * Move items and extend vector as required.
 */
extern void
vector_move_item_here(vector v, vector_index_t i_dst, int rider,
                                                           vector_index_t i_src)
{
  if (rider != 0)
    {
      if (rider > 0)
        ++i_dst ;
      vector_move_item(v, i_dst, i_src) ;
    }
  else
    {
      /* to replace: copy and then delete.  */
      vector_set_item(v, i_dst, vector_get_item(v, i_src)) ;
      vector_delete_item(v, i_src) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Reverse vector: reverse the order of items in the vector.
 */
extern void
vector_reverse(vector v)
{
  if (v != NULL)
    vector_part_reverse(v, 0, v->end) ;
} ;

/*------------------------------------------------------------------------------
 * Reverse portion of vector.
 */
extern void
vector_part_reverse(vector v, vector_index_t i, vector_length_t n)
{
  vector_index_t j ;

  if (v == NULL)
    return ;

  if ((i + n) > v->limit)
    vector_extend(v, i + n) ;   /* ensure portion exists        */

  if (n <= 1)
    return ;

  j = i + n - 1 ;               /* j > i, because n > 1         */
  do
    {
      p_vector_item p_i = v->p_items[i] ;
      v->p_items[i++]   = v->p_items[j] ;
      v->p_items[j--]   = p_i ;
    } while (j > i) ;
} ;

/*==============================================================================
 * Copying, moving and appending entire vectors.
 */

static void vector_new_limit(vector v, vector_length_t new_end) ;

/*------------------------------------------------------------------------------
 * Shallow vector copy -- copies pointers to item values, not the values.
 *
 * Creates a new vector.
 *
 * NB: creates new vector with same limit as existing one, but copies only
 *     the known items (ie up to end, not up to limit).
 */
vector
vector_copy (vector v)
{
  vector new = vector_init_new(NULL, v->limit) ;

  new->end = v->end;

  if (v->limit > 0)
    memcpy(new->p_items, v->p_items, P_ITEMS_SIZE(v->end)) ;

  return new;
}

/*------------------------------------------------------------------------------
 * Shallow vector copy -- copies pointers to item values, not the values.
 * Creates a new vector or re-initialises an existing one.
 *
 * NB: creates new vector with same limit as existing one, but copies only
 *     the known items (ie up to end, not up to limit).
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to release any existing items if that is required.
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to ensure the vector structure is currently valid.
 *
 * NB: do NOT try copying a vector to itself !!
 */
vector
vector_copy_here(vector dst, vector src)
{
  assert((src != NULL) && (src != dst)) ;

  dst = vector_re_init(dst, src->limit) ;

  dst->end = src->end;

  if (src->end > 0)
    memcpy(dst->p_items, src->p_items, P_ITEMS_SIZE(src->end)) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Vector move -- moves body of vector.
 *
 * Creates a new vector or re-initialises an existing one.
 * Leaves the source vector empty -- does not release the structure.
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to release any existing items if that is required.
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to ensure the vector structure is currently valid.
 *
 * NB: do NOT try moving a vector to itself !!
 */
extern vector
vector_move_here(vector dst, vector src)
{
  assert((src != NULL) && (src != dst)) ;

  if (dst != NULL)
    dst = vector_reset(dst, 0) ;     /* Reset to deallocate any existing body */
  else
    dst = vector_init_new(dst, 0) ;  /* Create new structure sans body.       */

  *dst = *src ;                      /* Copy the vector structure             */

  vector_init_new(src, 0) ;          /* Set empty, forgetting body            */

  return dst ;
}

/*------------------------------------------------------------------------------
 * Shallow vector copy append -- copies pointers to item values, not the values.
 *
 * Appends copied pointers to the destination vector.
 *
 * Creates a new destination vector if required.
 *
 * NB: Can append to self.
 */
extern vector
vector_copy_append(vector dst, vector src)
{
  vector_index_t new_end ;

  assert(src != NULL) ;

  if (dst != NULL)
    {
      new_end = dst->end + src->end ;
      if (new_end > dst->limit)
        vector_new_limit(dst, new_end) ;
    }
  else
    {
      new_end = src->end ;
      vector_init_new(dst, new_end) ;
    } ;

  if (src->end)
    memcpy(&dst->p_items[dst->end], src->p_items, P_ITEMS_SIZE(src->end)) ;

  dst->end = new_end ;          /* Done last, allows for append to self ! */
  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Vector move append -- moves pointers to item values.
 *
 * Appends moved pointers to the destination vector.
 *
 * Creates a new destination vector if required (dst == NULL).
 *
 * Leaves the source vector empty -- does not release the structure.
 *
 * NB: do NOT try moving a vector to itself !!
 */
extern vector
vector_move_append(vector dst, vector src)
{
  assert((src != NULL) && (src != dst)) ;

  if ((dst == NULL) || (dst->end == 0))
    return vector_move_here(dst, src) ;  /* Easy way to do it if dst empty  */

  vector_copy_append(dst, src) ;  /* Extend dst and copy src      */
  vector_init_new(src, 0) ;       /* Set empty, forgetting body   */

  return dst ;
} ;

/*==============================================================================
 * Portmanteau splice/extract/replace function.
 *
 * All take a portion of 'src' vector and:
 *
 * splice:
 *
 *   a) replace a portion of the 'dst' vector by a portion of the 'src' vector
 *      copying the replaced portion of the 'dst' vector to the 'to' vector
 *   b) either: leave 'src' unchanged               -- copy
 *          or: remove the stuff copied from 'src'  -- move
 *
 *   Arguments:  to_copy  -- true
 *               to       -- vector, or NULL => allocate new vector
 *               dst      -- vector
 *               i_dst    -- start of portion to replace
 *               n_dst    -- length of portion to replace.  0 => insertion.
 *               src      -- vector, or NULL => nothing to copy/move
 *               i_src    -- start of portion to copy/move
 *               n_src    -- length of portion to copy/move.  0 => nothing.
 *               src_move -- true => move, otherwise copy.
 *
 *   Returns:    the (possibly new) 'to' vector
 *
 *   NB: 'to', 'dst' and 'src' must be distinct vectors.
 *
 * extract:
 *
 *   a) copy a portion of the 'src' vector to the 'to' vector
 *   c) either: leave 'src' unchanged               -- copy
 *          or: remove the stuff copied from 'src'  -- move
 *
 *   Arguments:  to_copy  -- true
 *               to       -- vector, or NULL => allocate new vector
 *               dst      -- NULL
 *               i_dst    -- ignored
 *               n_dst    -- ignored
 *               src      -- vector, or NULL => nothing to copy/move
 *               i_src    -- start of portion to copy/move
 *               n_src    -- length of portion to copy/move.  0 => nothing.
 *               src_move -- true => move, otherwise copy.
 *
 *   Returns:    the (possibly new) 'to' vector
 *
 *   NB: 'to' and 'src' must be distinct vectors.
 *
 * replace:
 *
 *   a) replace a portion of the 'dst' vector by a portion of the 'src' vector
 *   b) either: leave 'src' unchanged               -- copy
 *          or: remove the stuff copied from 'src'  -- move
 *
 *   Arguments:  to_copy  -- false
 *               to       -- ignored
 *               dst      -- vector
 *               i_dst    -- start of portion to replace
 *               n_dst    -- length of portion to replace.  0 => insertion.
 *               src      -- vector, or NULL => nothing to copy/move
 *               i_src    -- start of portion to copy/move
 *               n_src    -- length of portion to copy/move.  0 => nothing.
 *               src_move -- true => move, otherwise copy.
 *
 *   Returns:    original 'to' argument
 *
 *   NB: 'dst' and 'src' must be distinct vectors.
 *
 * All copies are shallow -- pointers to item values are copied, not the values.
 *
 * NB: any existing contents of the 'to' vector are discarded.
 *
 * NB: it is the caller's responsibility to release memory allocated to any
 *     items which are discarded in these operations.
 *
 * NB: for splice and replace, the resulting destination vector will be at
 *     least i_dst + n_src long.  (Even if is copying actual or implied NULLs
 *     from the source.)
 *
 * NB: where new vectors are created, they will be of exactly the required size.
 *
 * NB: where an existing vector is reused, it is the caller's responsibility
 *     to ensure the vector structure is currently valid (by vector_init_new()
 *     or by ensuring it is zeroized).
 */
extern vector
vector_sak(int to_copy, vector to,
         vector dst, vector_index_t i_dst, vector_length_t n_dst,
         vector src, vector_index_t i_src, vector_length_t n_src, int src_move)
{
  int dst_replace ;               /* true => replace portion of 'dst'   */

  vector_index_t new_dst_end = 0 ;  /* new end of dst                   */

  vector_length_t n_dst_nulls ;   /* number of implicit NULLs to add    */
  vector_length_t n_dst_move ;    /* number of items to move up or down */
  vector_length_t n_src_real ;    /* number of items to really copy     */
  vector_length_t n_src_nulls ;   /* number of implicit NULLs to "copy" */

  assert((to  == NULL) || (dst == NULL) || (to  != dst)) ;
  assert((src == NULL) || (dst == NULL) || (src != dst)) ;

  /* Worry about how much we really have in the source vector.          */

  n_src_real  = n_src ;         /* assume all real              */
  n_src_nulls = 0 ;             /* so no NULLs to "copy"        */

  if (n_src != 0)
    {
      if ((src == NULL) || (i_src >= src->end))
        n_src_real = 0 ;
      else if ((i_src + n_src) > src->end)
        n_src_real = src->end - i_src ;
      n_src_nulls = n_src - n_src_real ;
    } ;

  /* If no 'dst' vector, then this is an extract.                       */

  n_dst_move  = 0 ;             /* assume nothing to move       */
  n_dst_nulls = 0 ;             /* assume no NULLs to add       */

  if (dst == NULL)
    /* For extract: set up dst, i_dst and n_dst so that can copy to the */
    /*              'to' vector as if from 'dst'.                       */
    {
      dst_replace = 0 ;                 /* no replacement operation     */
      dst   = src ;                     /* copy from here               */
      i_dst = i_src ;
      n_dst = n_src_real ;
    }
  else
    /* Reduce n_dst to the number of actual items to be replaced.       */
    /*                                                                  */
    /* Calculate the new end of 'dst'.                                  */
    {
      dst_replace = 1 ;                 /* have replacement to do       */
      if (i_dst >= dst->end)
        /* If i_dst is beyond the end of 'dst', then there is nothing   */
        /* to replace (so set n_dst == 0).  Will be adding n_src items  */
        /* at i_dst -- so new end must be i_dst + n_src.                */
        {
          n_dst_nulls = i_dst - dst->end ;  /* fill from end to i_dst   */
          n_dst       = 0 ;                 /* nothing to replace       */
          new_dst_end = i_dst + n_src ;     /* all beyond current end   */
        }
      else
        /* If i_dst + n_dst is beyond the end of 'dst', reduce n_dst to */
        /* number of items up to the end.                               */
        /* Will remove n_dst items and insert n_src, so end will move   */
        /* by n_src - n_dst.                                            */
        {
          if ((i_dst + n_dst) > dst->end)
            n_dst = dst->end - i_dst ;  /* what we actually replace     */
          else if (n_dst != n_src)
            n_dst_move = dst->end - (i_dst + n_dst) ;
                                        /* what we move up or down      */

          new_dst_end = dst->end + n_src - n_dst ;
                                        /* end depends on amount added  */
                                        /* & amount actually replaced   */
        } ;
    } ;

  /* Copy portion of 'dst' (or of 'src') to 'to', if required.          */
  /*                                                                    */
  /* Have arranged: n_dst -- number of items to copy, all existent      */
  /*                dst   -- vector to copy from -- if n_dst > 0        */
  /*                i_dst -- first item to copy  -- if n_dst > 0        */

  if (to_copy)
    {
      to = vector_re_init(to, n_dst) ;  /* reinitialise or create       */
      to->end = n_dst ;
      if (n_dst > 0)
        memcpy(to->p_items, &dst->p_items[i_dst], P_ITEMS_SIZE(n_dst)) ;
    } ;

  /* Replace portion of 'dst' by portion of 'src', if required.         */
  /*                                                                    */
  /* Have arranged:                                                     */
  /*                                                                    */
  /*  new_dst_end  -- end of dst once dust settles                      */
  /*  n_dst_nulls  -- number of NULLs to insert at dst->end to fill up  */
  /*                  to i_dst (when i_dst is beyond old end.)          */
  /*  n_dst_move   -- number of items in dst to move up or down to      */
  /*                  leave n_src item hole at i_dst to fill in.        */
  /*  n_src_real   -- number of real src items at i_src to copy to dst  */
  /*                  at i_dst.                                         */
  /*  n_src_nulls  -- number of nulls to add to fill to i_dst + n_src.  */

  if (dst_replace)
    {
      if (new_dst_end > dst->limit)   /* extend body if required */
        vector_new_limit(dst, new_dst_end) ;

      if (n_dst_nulls > 0)
        memset(&dst->p_items[dst->end], 0, P_ITEMS_SIZE(n_dst_nulls)) ;
      if (n_dst_move > 0)
        memmove(&dst->p_items[i_dst + n_dst], &dst->p_items[i_dst + n_src],
                                                     P_ITEMS_SIZE(n_dst_move)) ;
      if (n_src_real > 0)
        memcpy(&dst->p_items[i_dst], &src->p_items[i_src],
                                                     P_ITEMS_SIZE(n_src_real)) ;
      if (n_src_nulls > 0)
        memset(&dst->p_items[i_dst + n_src_real], 0,
                                                    P_ITEMS_SIZE(n_src_nulls)) ;
      dst->end = new_dst_end ;
    } ;

  /* Delete portion of 'src', if required (and have 'src' !)            */

  if (src_move && (n_src_real != 0))
    vector_delete(src, i_src, n_src_real) ;

  /* Done -- return 'to' vector as promised.                            */

  return to ;
} ;

/*==============================================================================
 * Legacy Vector Operations
 */

/*------------------------------------------------------------------------------
 * Set value to the smallest empty slot.
 *
 * Returns: index of slot used.
 */
extern int
vector_set (vector v, void *val)
{
  vector_index_t i;

  i = 0 ;
  while (1)
    {
      if (i == v->end)
        {
          i = vector_extend_by_1(v) ;
          break ;
        }

      if (v->p_items[i] == NULL)
        break ;

      ++i ;
    } ;

  v->p_items[i] = val;

  return i ;
}

/*------------------------------------------------------------------------------
 * Set value to specified index slot.
 *
 * Returns: index of slot (as given)
 */
extern int
vector_set_index (vector v, vector_index_t i, void *val)
{
  vector_ensure (v, i);
  v->p_items[i] = val;
  return i;
}

/*------------------------------------------------------------------------------
 * Look up vector -- get the i'th item.
 *
 * Returns: the i'th item -- NULL if item is null, or i >= logical len of vector
 */
extern p_vector_item
vector_lookup (vector v, vector_index_t i)
{
  if (i >= v->end)
    return NULL;
  return v->p_items[i];
}

/*------------------------------------------------------------------------------
 * Lookup vector, ensure it -- get i'th item and ensure logical len > i.
 *
 * Returns: the i'th item -- NULL if item is null
 */
extern p_vector_item
vector_lookup_ensure (vector v, vector_index_t i)
{
  vector_ensure (v, i);
  return v->p_items[i];
}

/*------------------------------------------------------------------------------
 * Count the number of not empty slots.
 */
extern vector_length_t
vector_count (vector v)
{
  vector_index_t  i;
  vector_length_t count = 0;

  for (i = 0; i < v->end; i++)
    if (v->p_items[i] != NULL)
      count++;

  return count;
}

/*==============================================================================
 * Sorting and Searching vector.
 */

/*------------------------------------------------------------------------------
 * Sort the given vector.
 *
 * NB: the comparison function receives a pointer to the pointer to the
 *     vector item's value.
 *
 * NB: if there are NULL items in the vector, the comparison function MUST
 *     be ready for them.
 */
extern void
vector_sort(vector v, vector_sort_cmp* cmp)
{
  if (v->end <= 1)
    return ;            /* Stop dead if 0 or 1 items */

  typedef int qsort_cmp(const void*, const void*) ;

  qsort(v->p_items, v->end, sizeof(p_vector_item), (qsort_cmp*)cmp) ;
} ;

/*------------------------------------------------------------------------------
 * Perform binary search on the given vector.
 *
 * The vector MUST be sorted in the order implied by the comparison function
 * given.  The vector need not contain unique values, but this search makes
 * no effort to select any particular instance of a sequence of equals.
 *
 * Returns:
 *
 *   result ==  0: found an equal value.
 *                 index returned is of first entry found which is equal to
 *                 the value sought.  There may be other equal values, before
 *                 and/or after this one in the vector.
 *
 *   result == +1: value not found and vector not empty.
 *                 index returned is of the largest entry whose value is less
 *                 than the value sought.
 *                 (The value sought belongs after this point.)
 *
 *   result == -1: value is less than everything in the vector, or the
 *                 vector is empty.
 *                 index returned is 0
 *
 * NB: The comparison function takes arguments which are:
 *
 *       const void**  pointer to pointer to value being searched for.
 *       const void**  pointer to pointer to vector item to compare with
 *
 *     The value being searched for need not be in the same form as a vector
 *     item.  However, if it is then the same comparison function can be used
 *     to sort and search.
 *
 * NB: if there are NULL items in the vector, the comparison function MUST
 *     be ready for them.
 */
extern vector_index_t
vector_bsearch(vector v, vector_bsearch_cmp* cmp, const void* p_val,
                                                                    int* result)
{
  vector_index_t il, iv, ih ;
  int c ;

  if (v->end <= 1)
    {
      *result = (v->end == 0) ? -1 : cmp(&p_val, (const void**)&v->p_items[0]) ;
      return 0 ;                /* Stop dead if 0 or 1 items */
    } ;

  /* We have at least two items.    */
  il = 0 ;
  ih = v->end - 1 ;

  /* Pick off the edge cases: >= last and <= first.  */
  if ((c = cmp(&p_val, (const void**)&v->p_items[ih])) >= 0)
    {
      *result = c ;     /* 0 => found.  +1 => val > last        */
      return ih ;       /* return high index.                   */
    } ;
  if ((c = cmp(&p_val, (const void**)&v->p_items[il])) <= 0)
    {
      *result = c ;     /* 0 => found.  -1 => val < first      */
      return il ;       /* return low index.                   */
    }

  /* Now binary chop.  We know that item[il] < val < item[ih]   */
  /*                   We also know that il < ih                */
  while (1)
    {
      iv = (il + ih) / 2 ;
      if (iv == il)     /* true if (ih == il+1)                         */
        {
          *result = +1 ;
          return il ;   /* return il: item[il] < val < item[il+1]       */
        } ;
      /* We now know that il < iv < ih  */
      c = cmp(&p_val, (const void**)&v->p_items[iv]) ;
      if (c == 0)
        {
          *result = 0 ;
          return iv ;   /* found !!                             */
        }
      if (c <  0)
        ih = iv ;       /* step down    iv > il, so new ih > il */
      else
        il = iv ;       /* step up      iv < ih, so new il < ih */
    } ;
} ;

/*==============================================================================
 * Mechanics for adding/deleting items and managing the vector (logical) end
 * and (physical) limit.
 */

/* Extract the LS bit of unsigned integer 'x'.  */
#define lsbit(x) ((x) & ((~(x)) + 1))
/* Round 'x' up to a multiple of 'm'            */
#define multiple(x, m) ((((x) + (m) - 1) / (m)) * (m))

/*------------------------------------------------------------------------------
 * Set new limit to suit new end for given vector.
 *
 * The new limit will be at least: VECTOR_LIMIT_MIN.
 *
 * While the vector is relatively small, the limit is doubled until there
 * is at least 1/8 of the new vector free.
 *
 * Beyond VECTOR_LIMIT_DOUBLE_MAX, however, the limit is set to the
 * smallest multiple of VECTOR_LIMIT_DOUBLE_MAX which gives at least
 * VECTOR_LIMIT_SLACK_MIN free entries beyond the new end.
 *
 * This is an attempt to balance the cost of repeated reallocations of
 * memory against the cost of possible wasted space at the end of the
 * vector.
 *
 * NB: the new_limit depends entirely on the new end position.  (Current
 *     end position is ignored.)
 *
 * NB: the new limit may be less than the current limit, in which case the
 *     vector body is reduced in size.
 *
 * Except for any size set when the vector is initialised, the vector body
 * size will be a power of 2 or a multiple of VECTOR_LIMIT_DOUBLE_MAX.
 * (Vectors are regular in their habits, which may help the memory allocator).
 *
 * TODO: what to do if calculation of new_limit overflows, or calculation
 *       of P_ITEMS_SIZE will ?
 */
static void
vector_new_limit(vector v, vector_index_t new_end)
{
  vector_length_t old_limit = v->limit ;
  vector_length_t new_limit ;

  if (new_end > ((VECTOR_LIMIT_DOUBLE_MAX * 7) / 8))
    {
      new_limit = multiple(new_end + VECTOR_LIMIT_SLACK_MIN,
                                                      VECTOR_LIMIT_DOUBLE_MAX) ;
    }
  else
    {
      /* Want the new_limit to be a power of 2.                           */
      /* If the old_limit was a power of 2, start from there.             */
      /* Otherwise start from a power of 2 less than new_end: either the  */
      /* minimum value or a value mid way to VECTOR_LIMIT_DOUBLE_MAX.     */
      if ( (old_limit != 0) && (old_limit == lsbit(old_limit))
                            && (new_end >= old_limit) )
        new_limit = old_limit ;
      else
        new_limit = (new_end < VECTOR_LIMIT_MID) ? VECTOR_LIMIT_MIN
                                                 : VECTOR_LIMIT_MID ;
      while (new_end > ((new_limit * 7) / 8))
        new_limit *= 2 ;
    } ;

  v->p_items =
             XREALLOC(MTYPE_VECTOR_BODY, v->p_items, P_ITEMS_SIZE(new_limit)) ;

  v->limit = new_limit ;
} ;

/*------------------------------------------------------------------------------
 * Extend vector and set new (logical) end.
 *
 * Extends body if required.
 * Ensures everything between old and new end is set NULL.
 *
 * NB: expects new end > old end, but copes with new end <= old end.
 */
extern void
vector_extend(vector v, vector_length_t new_end)
{
  vector_length_t old_end = v->end ;

  if (new_end > v->limit)
    vector_new_limit(v, new_end) ;
  v->end = new_end ;

  if (new_end > old_end)
    memset(&v->p_items[old_end], 0, P_ITEMS_SIZE(new_end - old_end)) ;
} ;

/*------------------------------------------------------------------------------
 * Insert entries into vector: insert 'n' NULL entries at location 'i'.
 *
 * Updates end (and limit) to be at least 'i' + 'n'.
 * (So if 'i' < end then end becomes end + 'n', else end becomes 'i' + 'n'.)
 */
extern void
vector_insert(vector v, vector_index_t i, vector_length_t n)
{
  vector_length_t old_end, new_end ;
  vector_length_t n_above ;

  /* If i < old end, then we are inserting n NULLs, and need
   *                      to shuffle at least one item up.
   *                 else we are setting new end to i + n and need to NULL
   *                      fill from old end to the new end.
   */
  old_end = v->end ;
  if (i < old_end)
    {
      if (n == 0)
        return ;                /* give up now if not inserting anything  */
      n_above = old_end - i ;   /* number of items to shuffle up.. >= 1   */
      new_end = old_end + n ;
    }
  else
    {
      n_above = 0 ;             /* nothing to shuffle up.                 */
      new_end = i + n ;
      i = old_end ;             /* where to zeroize from                  */
      n = new_end - old_end ;   /* how much to zeroize                    */
    } ;

  /* Now we extend the body if we need to.                  */
  if (new_end > v->limit)
    vector_new_limit(v, new_end) ;
  v->end = new_end ;

  if (n_above > 0)
    memmove(&v->p_items[i + n], &v->p_items[i], P_ITEMS_SIZE(n_above)) ;

  memset(&v->p_items[i], 0, P_ITEMS_SIZE(n)) ;
} ;

/*------------------------------------------------------------------------------
 * Delete items from vector: delete 'n' items at location 'i'.
 *
 * Does nothing if 'i' is beyond current end of vector or if 'n' == 0.
 *
 * Deletes from 'i' to end if less than 'n' items to the end.
 *
 * NB: does NOT change the size of the body.
 *
 * NB: it is the caller's responsibility to have released any memory allocated
 *     for the items that are being deleted.
*/
extern void
vector_delete(vector v, vector_index_t i, vector_length_t n)
{
  vector_length_t old_end, new_end ;

  old_end = v->end ;

  if ((i >= old_end) || (n == 0))
    return ;

  /* If i + n < old_end, we have 1 or more items to keep and move down */
  if ((i + n) < old_end)
    {
      memmove(&v->p_items[i], &v->p_items[i + n],
                                              P_ITEMS_SIZE(old_end - (i + n))) ;
      new_end = old_end - n ;
    }
  else
    {
      new_end = i ;             /* We are keeping nothing above 'i' */
                                /* NB: new_end < old_end            */
    } ;

  v->end = new_end ;            /* account for stuff dropped        */
} ;

/*------------------------------------------------------------------------------
 * Discard entries from vector: discard entries from location 'i' onwards.
 *
 * Releases memory from 'i' onwards.
 * Releases the body altogether if this sets the vector empty ('i' == 0).
 * Sets new end of vector iff 'i' < current end.
 *
 * Does nothing if 'i' is beyond current limit (physical end) of vector.
 *
 * NB: it is the caller's responsibility to have released any memory allocated
 *     for the items that are being discarded.
*/
extern void
vector_discard(vector v, vector_index_t i)
{
  if (i >= v->limit)
    return ;

  if (i == 0)
    vector_reset(v, 0) ;        /* reset, without releasing the structure */
  else
    {
      v->limit = i ;
      if (i < v->end)
        v->end = i ;
      v->p_items = XREALLOC(MTYPE_VECTOR_BODY, v->p_items, P_ITEMS_SIZE(i)) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Chop vector at the current (logical) end.
 *
 * Releases the body altogether if the vector is currently empty.
 */
extern void
vector_chop(vector v)
{
  vector_length_t new_limit = v->end ;

  if (new_limit == 0)
    vector_reset(v, 0) ;        /* reset, without releasing the structure */
  else if (new_limit != v->limit)
    {
      v->limit = new_limit ;
      v->p_items = XREALLOC(MTYPE_VECTOR_BODY, v->p_items,
                                                      P_ITEMS_SIZE(new_limit)) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Decant vector into a new body allocated to current logical size, and
 * release the old body.
 *
 * Releases the body altogether if the vector is currently empty.
*/
extern void
vector_decant(vector v)
{
  p_vector_item* p_old_body ;
  vector_length_t new_limit = v->end ;

  if (new_limit == 0)
    vector_reset(v, 0) ;          /* reset, without releasing the structure */
  else
    {
      p_old_body = v->p_items ;

      vector_init_new(v, new_limit) ;   /* initialise with new body     */

      memcpy(v->p_items, p_old_body, P_ITEMS_SIZE(new_limit)) ;
                                        /* copy the old body across     */
      v->end = new_limit ;

      XFREE(MTYPE_VECTOR_BODY, p_old_body) ;
    } ;
} ;

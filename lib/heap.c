/* Generic heap data structure -- functions.
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include "heap.h"
#include "memory.h"

/* Heaps are implemented as a structure which includes a vector structure.
 * So items in the heap are items in a vector, which are pointers to the item
 * values.
 *
 * The heap structure may be statically allocated, embedded in another
 * structure, or allocated dynamically.  In any case the heap operations
 * require the address of the heap structure -- see typedef for heap.
 *
 * An essential component of a heap is its comparison function.  So, cannot
 * use a heap before it has been initialised and the comparison function set.
 * (This is unlike a vector, which may be implicitly initialised empty by
 * zeroizing the vector structure.)
 *
 * Items may be pushed onto or popped off the heap, which is organised so that
 * the top item has the smallest value -- according to the heap's comparison
 * function.  (For equal values, the order is undefined.)
 *
 * The top item in the heap may be examined, and its value may be updated.
 * (Updating the top item is more efficient than popping and then pushing it.)
 *
 * Items may be deleted from the heap.  Items may have their value updated
 * while they are in the heap.  Both of these operations cause the heap to be
 * reorganised to maintain the heap's partial ordering.  Note: these operations
 * require knowledge of where in the heap the item is -- which, by default, is
 * done by a linear scan of the heap.  For large heaps, there is the option to
 * keep a "backlink" from the item to it's heap position.
 *
 * Vectors may be pushed onto a heap -- copying or moving the contents.
 *
 * Heaps may popped to a vector -- copying or moving the contents.  The
 * resulting vector is fully sorted.
 *
 * ----------------------------
 * Comparison function for heap.
 *
 *   int heap_cmp(...** a, ...**) ...
 *
 * Must return -1, 0, +1 : where -1 => a < b, 0 => a == b & +1 => a > b
 *
 * Heap will sort "smallest" to the top.  If you want the biggest at the top,
 * return -1 * usual comparison.  Note that the effective heap ordering for
 * equal values is, essentially, random.
 *
 * NB: like other comparison functions (cf qsort) the parameters are pointers
 *     to pointers to the value.
 *
 * NB: there should never be NULL items in the heap.
 */

/* Forward reference the mechanics.	*/
static void heap_bubble(heap h, vector_index i, p_vector_item p_v) ;
static void heap_bubble_up(heap h, vector_index i, p_vector_item p_v);
static void heap_bubble_down(heap h, vector_index i, p_vector_item p_v)  ;

/*==============================================================================
 * Initialisation, allocation, reset etc.
 */

static heap
heap_setup(heap h, int new_vector, vector_index size, heap_cmp* cmp,
                                int with_backlink, unsigned int backlink_offset)
{
  assert(cmp != NULL) ;         /* or there will be tears */

  h->cmp   = cmp ;
  h->state = with_backlink ? Heap_Has_Backlink : 0 ;
  h->backlink_offset = backlink_offset ;

  if (new_vector)
    vector_init_new(&h->v, size) ;
  else
    vector_re_init(&h->v, size) ;

  return h ;
} ;

/* Initialize heap.
 *
 * Allocates heap structure if none given.
 * Does not allocate the underlying vector if the heap is initialised empty.
 *
 * eg:
 *
 *  ... = heap_new_init_simple(NULL, 0, (heap_cmp*)my_cmp)
 *
 * NB: when initialising an existing heap structure it is ESSENTIAL that
 *     any previous heap and its contents have been released, because this
 *     function simply discards whatever was there before.  (This function may
 *     be called to initialise a heap structure which has never been
 *     initialised.)
 *
 * Backlink:
 *
 *   The heap_delete_item and heap_update_item functions need the heap
 *   position of the item.  The default way of finding that is to scan the
 *   underlying heap array, looking for the address of the item.
 *
 *   If either of these functions is done often and on large heaps, it is
 *   possible to speed this up by implementing a 'backlink'.  This requires
 *   a field of type heap_backlink_t in the item structure, and it is the
 *   offset of that which must be initialised here, eg:
 *
 *     ... = heap_new_init_backlink(NULL, 0, (heap_cmp*)my_cmp,
 *     				offset_of(struct xxx_heap_item, backlink)) ;
 *
 *   This adds a little extra work to every change in the heap -- keeping the
 *   backlink of any moved item up to date.  But avoids a linear search for
 *   every heap_delete_item or heap_update_item.
 */
heap
heap_init_new(heap h, vector_index size, heap_cmp* cmp,
                                int with_backlink, unsigned int backlink_offset)
{
  if (h == NULL)
    h = XCALLOC(MTYPE_HEAP, sizeof(struct heap)) ;
  else
    memset(h, 0, sizeof(struct heap)) ;

  return heap_setup(h, 1, size, cmp, with_backlink, backlink_offset) ;
} ;

/* Re-Initialize heap (or create a new one, if h == NULL).
 *
 * Allocates heap structure if none given -- allocating vector if size != 0.
 * Otherwise, re-initialise the heap and any vector (reusing its memory).
 *
 * NB: when re-initialising an existing heap it is the caller's
 *     responsibility to release any item values *before* doing this.
 */
heap
heap_re_init(heap h, vector_index size, heap_cmp* cmp,
                                int with_backlink, unsigned int backlink_offset)
{
  if (h == NULL)
    return heap_init_new(h, size, cmp, with_backlink, backlink_offset) ;
  else
    return heap_setup(h, 0, size, cmp, with_backlink, backlink_offset) ;
} ;

/* Release heap contents (underlying vector), and (if required) release the
 * heap structure.
 *
 * Return NULL if releases heap, otherwise the address of the heap.
 *
 * If does not release the heap, it retains the comparison function and any
 * backlink setting -- so heap can be reused without reinitialising it.
 *
 * NB: it is the callers responsibility to release any heap item values
 *     *before* doing this.
 */
heap
heap_reset(heap h, int free_structure)
{
  vector_reset(&h->v, 0) ;

  if (free_structure)
    {
      XFREE(MTYPE_VECTOR, h) ;
      return NULL ;
    } ;

  return h ;
} ;

/* Remove last item from heap.
 *
 * If heap is empty, release the underlying vector, and (if required) release
 * the heap structure.
 *
 * Useful for emptying out and discarding a heap:
 *
 *     while ((p_v = heap_ream_out(v, 1)))
 *       ... do what's required to release the item p_v
 *
 * Returns NULL when heap is empty.
 *
 * If does not release the heap, it retains the comparison function and any
 * backlink setting -- so heap can be reused without reinitialising it.
 *
 * NB: by 'last' this does NOT mean last according to the heap ordering,
 *     it just means last in the underlying vector -- which won't be the
 *     first in heap order (unless heap contains only one item).
 */
p_vector_item
heap_ream(heap h, int free_structure)
{
  p_vector_item p_v ;

  if (h == NULL)
    return NULL ;

  if ((p_v = vector_ream_keep(&h->v)) == NULL)
    heap_reset(h, free_structure) ;

  return p_v ;
} ;

/*==============================================================================
 * Simple Heap Operations
 */

/* Push given item onto the heap                                          */
void
heap_push_item(heap h, p_vector_item p_v)
{
  assert(p_v) ;	 /* no NULLs, thank you.  */
  heap_bubble_up(h, vector_extend_by_1(&h->v), p_v) ;
} ;

/* Pop item off the heap.
 *
 * Returns NULL if heap is empty.
 */
p_vector_item
heap_pop_item(heap h)
{
  p_vector_item p_v ;
  p_vector_item p_x ;

  p_x = vector_pop_item(&h->v) ;  /* extract last item, if any 	        */
  if (h->v.end == 0)
    return p_x ;                  /* done if last was also first        */

  p_v = h->v.p_items[0] ;	  /* this is what we are popping	*/

  heap_bubble_down(h, 0, p_x) ;	  /* reposition what was the last item	*/
				  /* updating any backlink		*/
  return p_v ;
} ;

/* Get address of top item on heap.  Re-establish heap order if required.
 *
 * Returns NULL if heap is empty.
 */
p_vector_item
heap_top_item(heap h)
{
  return vector_get_first_item(&h->v) ;  /* if any */
} ;

/* Update heap to reflect new value of top item.        */
void
heap_update_top_item(heap h)
{
  heap_bubble_down(h, 0, vector_get_first_item(&h->v)) ;
} ;

/*==============================================================================
 * Heap Operations which use 'backlink', if implemented.
 */

static vector_index heap_find_item(heap h, p_vector_item) ;

/* Delete given item from the heap.
 *
 * See notes on backlink, above.
 *
 * NB: do NOT try this on items which are not in the given heap !
 */
void
heap_delete_item(heap h, p_vector_item p_v)
{
  p_vector_item p_x ;
  vector_index i ;

  i = heap_find_item(h, p_v) ;

  p_x = vector_pop_item(&h->v) ;  /* extract last item, if any 		*/

  if (i == h->v.end)
    return ;			  /* stop now if deleting last item	*/

  heap_bubble(h, i, p_x) ;	  /* move what was last into position	*/
				  /* updating any backlink		*/
} ;

/* Update heap to reflect new value of given item.
 *
 * See notes on backlink, above.
 */
void heap_update_item(heap h, p_vector_item p_v)
{
  heap_bubble(h, heap_find_item(h, p_v), p_v) ;
} ;

/*==============================================================================
 * Other Heap Operations.
 */

/* Push entire vector onto heap copying or moving items as required.
 *
 * Copy or move vector to end of heap's vector, then move each
 * (non-NULL) item into heap order.
 */
void
heap_push_vector(heap h, vector v, int move_vector)
{
  vector_index i = h->v.end ;
  vector_index e ;
  vector_index n = v->end ;

  if (move_vector)
    vector_move_append(&h->v, v) ;
  else
    vector_copy_append(&h->v, v) ;

  e = i ;                           /* old end of the heap.     */
  while (n--) {
    p_vector_item p_v = h->v.p_items[i++] ;
    if (p_v != NULL)
      heap_bubble_up(h, e++, p_v) ; /* move new item into position in heap  */
                                    /* setting any backlink                 */
  } ;

  h->v.end = e ;                    /* new end of heap          */
} ;

/* Pop given heap to vector -- creating vector if required (v == NULL).
 *
 * Resulting vector is fully sorted.
 *
 * Moves or copies the contents of the heap.
 *
 * NB: when creating new vector, will be exactly the required size.
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to release any existing items if that is required.
 *
 * NB: if re-initialising existing vector, it is the caller's responsibility
 *     to ensure the vector structure is currently valid.
*/

vector
heap_pop_vector(vector v, heap h, int move_heap)
{
  vector_index n = h->v.end ;
  vector_index i ;

  v = vector_re_init(v, n) ;
  v->end = n ;

  for (i = 0 ; i < n ; i++)
    v->p_items[i] = heap_pop_item(h) ;

  if (!move_heap)
    vector_copy_here(&h->v, v) ;  /* fully sorted is also heap ordered ! */

  return v ;
} ;

/*==============================================================================
 * The Heap internal mechanics.
 */

/* Returns pointer to backlink value in heap item: lvalue or rvalue	*/
#define HEAP_BACKLINK(h, p_v) \
  *(heap_backlink_t*)((char*)(p_v) + (h)->backlink_offset)
/* Sets backlink, if required.						*/
#define heap_set_backlink(h, p_v, i) \
  if ((h)->state & Heap_Has_Backlink) HEAP_BACKLINK(h, p_v) = (i)

/* Returns index of parent.		*/
#define HEAP_UP(i)   (((i) * 2) + 1)
/* Returns index of left child.		*/
#define HEAP_DOWN(i) (((i) - 1) / 2)

/* Insert given item in the required place in heap, given that there is now
 * a hole at the given position -- may move up or down the heap, or stay put.
 *
 * Bubbles up or down as required.
 *
 * Note that this sets the backlink on the given item.
 */
static void
heap_bubble(heap h, vector_index i, p_vector_item p_v)
{
  /* If this is < parent, we bubble upwards.	*/
  if ((i != 0) && (h->cmp(&p_v, &h->v.p_items[HEAP_UP(i)]) < 0))
    heap_bubble_up(h, i, p_v) ;
  /* Otherwise we try bubbling downwards.       */
  else
    heap_bubble_down(h, i, p_v) ;
} ;

/* Insert given item in the required place in heap, given that there is now
 * a hole at the given position -- where we know may *only* move up the heap.
 *
 * Note that this sets the backlink on the given item.
 *
 * NB: ignores anything in the heap beyond 'i' -- in particular does not use
 *     v.end at all.  So this can be used to work along a vector and bring
 *     items into heap order.
 */
static void
heap_bubble_up(heap h, vector_index i, p_vector_item p_v)
{
  p_vector_item* ha = h->v.p_items ;    /* underlying array     */

  while (i != 0)
    {
      vector_index ip = HEAP_UP(i) ;
      p_vector_item p_p = &ha[ip] ;

      if (h->cmp(&p_p, &p_v) <= 0)
	break ;				/* stop when parent is <= us	*/
      ha[i] = p_p ;		        /* move parent down...		*/
      heap_set_backlink(h, p_p, i) ;	/* ...updating any backlink	*/
      i = ip ;				/* move up the heap		*/
    }

  ha[i] = p_v ;		                /* place in new position...	*/
  heap_set_backlink(h, p_v, i) ;	/* ...updating any backlink	*/
} ;

/* Insert given item in the required place in heap, given that there is now
 * a hole at the given position -- where we know may *only* move down the heap.
 *
 * Note that this sets the backlink on the given item.
 */
static void
heap_bubble_down(heap h, vector_index i, p_vector_item p_v)
{
  vector_index   e  = h->v.end ;        /* end of heap          */
  p_vector_item* ha = h->v.p_items ;    /* underlying array     */

  while (1)
    {
      vector_index ic ;			/* index of child	*/
      vector_index is ;			/* index of sibling	*/
      p_vector_item p_c ;		/* pointer to child	*/
      p_vector_item p_s ;		/* pointer to sibling	*/

      ic = HEAP_DOWN(i) ;
      if (ic >= e)
	break ;				/* Quit if run out of heap ! */
      p_c = &ha[ic] ;

      is = ic + 1 ;
      if (is < e)
	{
	  p_s = &ha[is] ;
	  if (h->cmp(&p_s, &p_c) < 0)
	    {
	      ic  = is ;		/* select smaller sibling	*/
	      p_c = p_s ;
	    }
	}

      if (h->cmp(&p_v, &p_c) <= 0)
	break ;				/* stop when we are <= both children  */
      ha[i] = p_c ;		        /* move smaller child up	      */
      heap_set_backlink(h, p_c, i) ;	/* ...updating any backlink	      */
      i = ic ;				/* move down the heap		      */
    }

  ha[i] = p_v ;
  heap_set_backlink(h, p_v, i) ;
} ;

/* Find index of given item in the given heap.		*/
static vector_index
heap_find_item(heap h, p_vector_item p_v)
{
  vector_index i ;

  if (h->state & Heap_Has_Backlink)
    i = HEAP_BACKLINK(h, p_v) ;
  else
    {
      for (i = 0 ; i < h->v.end ; ++i)
	if (h->v.p_items[i] == p_v)
	  break ;
    } ;

  assert((i < h->v.end) && (h->v.p_items[i] == p_v)) ;

  return i ;
} ;

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

#include "misc.h"
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
 * An essential component of a heap is its comparison function.  So, a heap
 * CANNOT be used before it has been initialised and the comparison function
 * set.  (This is unlike a vector, which may be implicitly initialised empty by
 * zeroising the vector structure.)
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
 *   int heap_cmp(...** a, ...** b) ...
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

/*==============================================================================
 * Initialisation, allocation, reset etc.
 */

static heap
heap_setup(heap h, int new_vector, vector_length_t size, heap_cmp* cmp,
                              int with_backlink, unsigned int backlink_offset) ;

/* Initialize heap -- allocating heap structure if required.
 *
 * Does not allocate the underlying vector if the heap is initialised empty.
 *
 * eg:
 *
 *  ... = heap_new_init_simple(NULL, 0, (heap_cmp*)my_cmp)
 *
 * See:  #define heap_init_new_simple(h, size, cmp)
 *       #define heap_init_new_backlinked(h, size, cmp, offset)
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
 *     ... = heap_new_init_backlinked(NULL, 0, (heap_cmp*)my_cmp,
 *     				offset_of(struct xxx_heap_item, backlink)) ;
 *
 *   This adds a little extra work to every change in the heap -- keeping the
 *   backlink of any moved item up to date.  But avoids a linear search for
 *   every heap_delete_item or heap_update_item.
 *
 * Returns the heap which has been initialised.
 */
heap
heap_init_new(heap h, unsigned int size, heap_cmp* cmp,
                                int with_backlink, unsigned int backlink_offset)
{
  if (h == NULL)
    h = XCALLOC(MTYPE_HEAP, sizeof(struct heap)) ;
  else
    memset(h, 0, sizeof(struct heap)) ;

  return heap_setup(h, 1, size, cmp, with_backlink, backlink_offset) ;
} ;

/* Reinitialise heap (or create a new one, if h == NULL).
 *
 * Allocates heap structure if none given -- allocating vector if size != 0.
 * Otherwise, re-initialise the heap and any vector (reusing its memory).
 *
 * See:  #define heap_re_init_simple(h, size, cmp)
 *       #define heap_re_init_backlinked(h, size, cmp, offset)
 *
 * NB: when reinitialising an existing heap it is the caller's
 *     responsibility to release any item values *before* doing this.
 *
 * Returns the heap that has been reinitialised.
 */
heap
heap_re_init(heap h, unsigned int size, heap_cmp* cmp,
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
 * Returns NULL if releases heap, otherwise the reset heap.
 *
 * If does not release the heap, it retains the comparison function and any
 * backlink setting -- so heap can be reused without reinitialising it.
 *
 * NB: it is the callers responsibility to release any heap item values
 *     *before* doing this.
 */
heap
heap_reset(heap h, free_keep_b free_structure)
{
  vector_reset(h->v, keep_it) ;   /* vector structure is embedded in the heap */

  confirm(free_it == true) ;
  if (free_structure)
    XFREE(MTYPE_VECTOR, h) ;      /* sets h = NULL */

  return h ;
} ;

/* Common set-up for heap_init_new() & heap_reset().
 */
static heap
heap_setup(heap h, int new_vector, unsigned int size, heap_cmp* cmp,
                                int with_backlink, unsigned int backlink_offset)
{
  assert(cmp != NULL) ;         /* or there will be tears */

  h->cmp   = cmp ;
  h->state = with_backlink ? Heap_Has_Backlink : 0 ;
  h->backlink_offset = backlink_offset ;

  if (new_vector)
    vector_init_new(h->v, size) ;
  else
    vector_re_init(h->v, size) ;

  return h ;
} ;

/* Ream (another) item out of the given heap.
 *
 * If heap is empty, release the underlying vector, and (if required) release
 * the heap structure.
 *
 * See: #define heap_ream_free(h) heap_ream(h, 1)
 *      #define heap_ream_keep(h) heap_ream(h, 0)
 *
 * Useful for emptying out and resetting/discarding a heap:
 *
 *     while ((p_v = heap_ream_free(h)))
 *       ... do what's required to release the item p_v
 *
 * Returns NULL when heap is empty (and structure has been freed, if required).
 *
 * If does not release the heap, it retains the comparison function and any
 * backlink setting -- so heap can be reused without reinitialising it.
 *
 * NB: once the process of reaming a heap has started: (a) MUST NOT attempt to
 *     use the heap until process completes, and (b) MUST complete the process.
 *
 * NB: items are reamed out in no defined order.
 */
p_vector_item
heap_ream(heap h, free_keep_b free_structure)
{
  p_vector_item p_v ;

  if (h == NULL)
    return NULL ;

  if ((p_v = vector_ream(h->v, keep_it)) == NULL)
    heap_reset(h, free_structure) ;

  return p_v ;
} ;

/*==============================================================================
 * Simple Heap Operations -- see also the Inline functions.
 */

/* Pop item off the heap.
 *
 * Returns the popped value, which is NULL if the heap was (and still is) empty.
 */
p_vector_item
heap_pop_item(heap h)
{
  p_vector_item p_v ;
  p_vector_item p_x ;

  p_v = vector_pop_item(h->v) ;   /* extract last item, if any 	          */
  if ((p_v == NULL) || (h->v->end == 0))
    return p_v ;                  /* done if empty or last was also first */

  p_x = h->v->p_items[0] ;        /* this is what we are popping	  */

  heap_bubble_down(h, 0, p_v) ;	  /* reposition what was the last item	  */
				  /* updating any backlink		  */
  return p_x ;
} ;

/* Pop one item off the heap and promptly push another.
 *
 * In this combination, the pop is essentially free.
 *
 * Returns the popped value, which is NULL if the heap was (and still is) empty.
 */
p_vector_item
heap_pop_push_item(heap h, p_vector_item p_v)
{
  p_vector_item p_x ;

  dassert(p_v != NULL) ;           /* no NULLs, thank you.               */

  p_x = heap_top_item(h) ;         /* what we are popping                */

  if (p_x == NULL)
    heap_push_item(h, p_v) ;       /* for empty heap, this deals with    */
                                   /* extending heap etc.                */
  else
    heap_bubble_down(h, 0, p_v) ;  /* position the replacement           */
                                   /* setting any backlink               */
  return p_x ;
} ;

/*==============================================================================
 * Heap Operations which use 'backlink', if implemented.
 */

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
  vector_index_t i ;

  i = heap_find_item(h, p_v) ;    /* index of item to be deleted              */

  p_x = vector_pop_item(h->v) ;   /* extract last item, if any 	              */

  if (i < h->v->end)              /* if not deleting the last item...         */
    heap_bubble(h, i, p_x) ;      /* ...reinsert what was last, at the delete */
                                  /* position, updating any backlink          */
} ;

/*==============================================================================
 * Other Heap Operations.
 */

/* Push entire vector onto heap copying or moving items as required.
 *
 * Copy or move vector to end of heap's vector, then move each
 * (non-NULL) item into heap order (discarding any NULL items).
 *
 * See: #define heap_push_vector_copy(h, v)
 *      #define heap_push_vector_move(h, v)
 */
void
heap_push_vector(heap h, vector v, int move_vector)
{
  vector_index_t  i = h->v->end ;
  vector_index_t  e ;
  vector_length_t n = v->end ;
  p_vector_item p_v ;

  if (move_vector)
    vector_move_append(h->v, v) ;
  else
    vector_copy_append(h->v, v) ;

  e = i ;                           /* old end of the heap.     */
  while (n--) {
    p_v = h->v->p_items[i++] ;
    if (p_v != NULL)
      heap_bubble_up(h, e++, p_v) ; /* move new item into position in heap  */
                                    /* setting any backlink                 */
  } ;

  h->v->end = e ;                   /* new end of heap          */
} ;

/* Pop given heap to vector -- creating vector if required (v == NULL).
 *
 * Resulting vector is fully sorted.
 *
 * Moves or copies the contents of the heap.
 *
 * See: #define heap_pop_vector_copy(v, h)
 *      #define heap_pop_vector_move(v, h)
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
  vector_length_t n = h->v->end ;
  vector_index_t  i ;

  v = vector_re_init(v, n) ;      /* guarantees >= 'n' items in vector  */
  v->end = n ;

  for (i = 0 ; i < n ; i++)
    v->p_items[i] = heap_pop_item(h) ;

  if (!move_heap)
    vector_copy_here(h->v, v) ;  /* fully sorted is also heap ordered ! */

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
#define HEAP_UP(i)   (((i) - 1) / 2)
/* Returns index of left child.		*/
#define HEAP_DOWN(i) (((i) * 2) + 1)

/* Insert given item in the required place in heap, given that there is now
 * a hole at the given position -- may move up or down the heap, or stay put.
 *
 * Bubbles up or down as required.
 *
 * Note that this sets the backlink on the given item.
 */
Private void
heap_bubble(heap h, vector_index_t i, p_vector_item p_v)
{
  /* If this is < parent, we bubble upwards.	*/
  if ((i != 0) && (h->cmp(&p_v, h->v->p_items[HEAP_UP(i)]) < 0))
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
Private void
heap_bubble_up(heap h, vector_index_t i, p_vector_item p_v)
{
  p_vector_item* ha = h->v->p_items ; /* underlying array        */
  vector_index_t ip ;                 /* index of parent         */
  p_vector_item  p_p ;                /* pointer to parent item  */

  dassert(ha != NULL) ;

  while (i != 0)
    {
      ip = HEAP_UP(i) ;
      p_p = ha[ip] ;                    /* get parent                   */

      if (h->cmp(&p_v, &p_p) >= 0)
	break ;				/* stop when value >= parent	*/

      ha[i] = p_p ;		        /* move parent down...		*/
      heap_set_backlink(h, p_p, i) ;	/* ...updating any backlink	*/

      i = ip ;				/* move up the heap		*/
    } ;

  ha[i] = p_v ;		                /* place in new position...	*/
  heap_set_backlink(h, p_v, i) ;	/* ...updating any backlink	*/
} ;

/* Insert given item in the required place in heap, given that there is now
 * a hole at the given position -- where we know may *only* move down the heap.
 *
 * Note that this sets the backlink on the given item.
 */
Private void
heap_bubble_down(heap h, vector_index_t i, p_vector_item p_v)
{
  vector_length_t e  = h->v->end ;    /* end of heap          */
  vector_index_t  ic ;                /* index of child       */
  vector_index_t  is ;                /* index of sibling     */
  p_vector_item   p_c ;               /* pointer to child     */
  p_vector_item   p_s ;               /* pointer to sibling   */

  p_vector_item* ha = h->v->p_items ; /* underlying array     */
  dassert(ha != NULL) ;

  while (1)
    {
      ic = HEAP_DOWN(i) ;
      if (ic >= e)
	break ;                        /* Quit if run out of heap !     */
      p_c = ha[ic] ;                   /* get left hand child           */

      is = ic + 1 ;
      if (is < e)                      /* is there a right hand child ? */
	{
	  p_s = ha[is] ;               /* get right hand child          */
	  if (h->cmp(&p_s, &p_c) < 0)
	    {
	      ic  = is ;               /* select smaller sibling        */
	      p_c = p_s ;
	    } ;
	} ;

      if (h->cmp(&p_v, &p_c) <= 0)
        break ;                        /* stop when <= both children    */

      ha[i] = p_c ;                    /* move smaller child up         */
      heap_set_backlink(h, p_c, i) ;   /* ...updating any backlink	*/

      i = ic ;                         /* move down the heap		*/
    } ;

  ha[i] = p_v ;                         /* place in new position...     */
  heap_set_backlink(h, p_v, i) ;        /* ...updating any backlink     */
} ;

/* Find index of given item in the given heap.		*/
Private vector_index_t
heap_find_item(heap h, p_vector_item p_v)
{
  vector_index_t i ;

  if (h->state & Heap_Has_Backlink)
    i = HEAP_BACKLINK(h, p_v) ;
  else
    {
      for (i = 0 ; i < h->v->end ; ++i)
	if (h->v->p_items[i] == p_v)
	  return i ;
    } ;

  assert((i < h->v->end) && (h->v->p_items[i] == p_v)) ;

  return i ;
} ;

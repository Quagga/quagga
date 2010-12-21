/* Generic heap data structure -- header.
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *.
 * This file is part of GNU Zebra.
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

#ifndef _ZEBRA_HEAP_H
#define _ZEBRA_HEAP_H

#include "misc.h"
#include "vector.h"

/*==============================================================================
 * Data structures etc.
 */

typedef int heap_cmp(p_vector_item* a, p_vector_item*) ;

enum heap_state {
  Heap_Has_Backlink  = 0x01,    /* Set if backlink set  */
} ;

typedef vector_index_t heap_backlink_t ;

typedef struct heap* heap ;

struct heap
{
  heap_cmp* cmp ;

  enum heap_state state ;
  unsigned int backlink_offset ;

  vector_t v ;
} ;

/*==============================================================================
 * Prototypes.
 */

extern heap heap_init_new(heap h, unsigned int size, heap_cmp* cmp,
                            int with_backlink, unsigned int backlink_offset) ;
#define heap_init_new_simple(h, size, cmp) \
  heap_init_new(h, size, cmp, 0, 0)
#define heap_init_new_backlinked(h, size, cmp, offset) \
  heap_init_new(h, size, cmp, 1, offset)

extern heap heap_re_init(heap h, unsigned int size, heap_cmp* cmp,
			    int with_backlink, unsigned int backlink_offset) ;
#define heap_re_init_simple(h, size, cmp) \
  heap_re_init(h, size, cmp, 0, 0)
#define heap_re_init_backlinked(h, size, cmp, offset) \
  heap_re_init(h, size, cmp, 1, offset)

extern heap heap_reset(heap h, free_keep_b free_structure) ;
extern p_vector_item heap_ream(heap h, free_keep_b free_structure) ;

Inline void heap_push_item(heap h, p_vector_item p_v) ;
extern p_vector_item heap_pop_item(heap h) ;
extern p_vector_item heap_pop_push_item(heap h, p_vector_item p_v) ;
Inline p_vector_item heap_top_item(heap h) ;
Inline void heap_update_top_item(heap h) ;

extern void heap_delete_item(heap h, p_vector_item p_v) ;
Inline void heap_update_item(heap h, p_vector_item p_v) ;

extern void heap_push_vector(heap h, vector v, int move_vector) ;
#define heap_push_vector_copy(h, v) \
  heap_push_vector(h, v, 0)
#define heap_push_vector_move(h, v) \
  heap_push_vector(h, v, 1)
extern vector heap_pop_vector(vector v, heap h, int move_heap) ;
#define heap_pop_vector_copy(v, h)  \
  heap_pop_vector(v, h, 0)
#define heap_pop_vector_move(v, h)  \
  heap_pop_vector(v, h, 1)

/*==============================================================================
 * This are extern only for use in Inline and other friends
 */

Private void
heap_bubble(heap h, vector_index_t i, p_vector_item p_v) ;

Private void
heap_bubble_up(heap h, vector_index_t i, p_vector_item p_v) ;

Private void
heap_bubble_down(heap h, vector_index_t i, p_vector_item p_v) ;

Private vector_index_t
heap_find_item(heap h, p_vector_item p_v) ;

/*==============================================================================
 * Inline Functions
 */

/* Push given item onto the heap
 */
Inline void
heap_push_item(heap h, p_vector_item p_v)
{
  dassert(p_v != NULL) ;        /* no NULLs, thank you.  */
  heap_bubble_up(h, vector_extend_by_1(h->v), p_v) ;
} ;

/* Get copy of top heap item (does not pop).
 *
 * Returns NULL if heap is empty.
 */
Inline p_vector_item
heap_top_item(heap h)
{
  return vector_get_first_item(h->v) ;  /* if any */
} ;

/* Update heap to reflect new value of top item.
 */
Inline void
heap_update_top_item(heap h)
{
  heap_bubble_down(h, 0, heap_top_item(h)) ;
} ;

/* Update heap to reflect new value of given item.
 */
Inline void
heap_update_item(heap h, p_vector_item p_v)
{
  heap_bubble(h, heap_find_item(h, p_v), p_v) ;
} ;

#endif /* _ZEBRA_HEAP_H */

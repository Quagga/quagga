/* Generic vector interface header.
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
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

#ifndef _ZEBRA_VECTOR_H
#define _ZEBRA_VECTOR_H

#include "misc.h"

/*------------------------------------------------------------------------------
 * types and struct for vector
 *
 * NB: an entirely zero structure represents an entirely empty vector.
 */
typedef void*         p_vector_item ;
typedef unsigned int  vector_index_t ;
typedef unsigned int  vector_length_t ;

enum {
  VECTOR_LENGTH_MAX = (vector_length_t)INT_MAX + 1
} ;

struct vector
{
  p_vector_item*  p_items ;     /* pointer to array of vector item pointers */
  vector_length_t end ;         /* number of "active" item entries          */
  vector_length_t limit ;       /* number of allocated item entries         */
};

typedef struct vector  vector_t[1] ;    /* embedded vector structure    */
typedef struct vector* vector ;         /* pointer to vector structure  */

/* Setting a vector object to all zeros is enough to initialise it to
 * an empty vector.
 */
enum
{
  VECTOR_INIT_ALL_ZEROS = true
} ;

#define VECTOR_INIT_EMPTY  { .p_items = NULL, .end = 0, .limit = 0 }

/* Under very controlled circumstances, may access the vector body      */
typedef p_vector_item const* vector_body_t ;

/* Values that control the allocation of the vector body.                     */
/* NB: these must all be powers of 2.                                         */

/* The body, when allocated, will have at least this many entries.      */
#define VECTOR_LIMIT_MIN            8
/* When the body grows, it doubles in size, until it is this big.       */
/* After that it grows in units of this much.				*/
#define VECTOR_LIMIT_DOUBLE_MAX  2048
/* "Midway" between VECTOR_LIMIT_MIN and VECTOR_LIMIT_DOUBLE_MAX.       */
#define VECTOR_LIMIT_MID          128
/* When growing in units of VECTOR_LIMIT_DOUBLE_MAX, this is the        */
/* minimum slack space to leave after the logical end of the vector.    */
#define VECTOR_LIMIT_SLACK_MIN   ((VECTOR_LIMIT_DOUBLE_MAX) / 8)

/* (Sometimes) useful macros.						      */

/* Reference item at given index.                                       */
/* NB: does not guarantee the item is "active" (that is: within the     */
/*     (logical) vector) -- but see vector_ensure.                      */
/* Returns address of vector item.					*/
/* See: VECTOR_ITEMS() for walking items in a vector.			*/
/* See: vector_get_item(), which is preferable.                         */
#define vector_slot(V,I)  ((V)->p_items[(I)])

/* Number of "active" item entries -- (logical) end of the vector.      */
/* Note that this differs from vector_count() as this count will        */
/* include any NULL items.                                              */
#define vector_active(V) ((V)->end)

/* To walk all items in a vector:
 *
 *   vector_index_t i ;
 *   xxxxx* p_v ;
 *
 *   for (VECTOR_ITEMS(v, p_v, i))
 *     {
 *       ...  i    is index of current item
 *       ...  p_v  is address of current item value -- may be NULL
 *     } ;
 *
 *   ... i is number of items in vector (including any NULLs)
 */
#define VECTOR_ITEMS(v, p_v, i)\
  (i) = 0 ;\
  (i) < (v)->end ? (((p_v) = (void*)(v)->p_items[i]), 1) \
                 : (((p_v) = NULL),                   0) ;\
  ++(i)

/*==============================================================================
 *  Prototypes.
 */

extern vector vector_init (vector_length_t size);
Inline void vector_ensure(vector v, vector_index_t i) ;
extern int vector_set (vector v, void *val);
extern int vector_set_index (vector v, vector_index_t i, void *val);
#define vector_unset(v, i) (void)vector_unset_item(v, i)
extern vector_length_t vector_count (vector v);
extern void vector_free (vector v);
extern vector vector_copy (vector v);

extern void *vector_lookup (vector, vector_index_t);
extern void *vector_lookup_ensure (vector, vector_index_t);

extern vector vector_init_new(vector v, vector_length_t size) ;
extern vector vector_re_init(vector v, vector_length_t size) ;
extern vector vector_reset(vector v, free_keep_b free_structure) ;
extern p_vector_item vector_ream(vector v, free_keep_b free_structure) ;

Inline  void vector_set_min_length(vector v, vector_length_t len) ;
Private void vector_set_new_min_length(vector v, vector_length_t len) ;

Inline void vector_set_length(vector v, vector_length_t len) ;
#define     vector_set_end(v, l) vector_set_length(v, l)

Inline vector_length_t vector_length(vector v) ;
#define                vector_end(v) vector_length(v)
Inline bool vector_is_empty(vector v) ;

Inline vector_body_t vector_body(vector v) ;

Inline p_vector_item vector_get_item(vector v, vector_index_t i) ;
Inline p_vector_item vector_get_first_item(vector v) ;
Inline p_vector_item vector_get_last_item(vector v) ;
Inline void vector_set_item(vector v, vector_index_t i, p_vector_item p_v) ;
Inline void vector_assign_item(vector v, vector_index_t dst,
                                         vector_index_t src) ;
extern p_vector_item vector_unset_item(vector v, vector_index_t i) ;
extern vector_length_t vector_trim(vector v) ;
extern vector_length_t vector_condense(vector v) ;

extern void vector_insert_item(vector v, vector_index_t i, p_vector_item p_v) ;
extern void vector_insert_item_here(vector v, vector_index_t i, int rider,
							  p_vector_item p_v) ;
extern void vector_move_item(vector v, vector_index_t dst, vector_index_t src) ;
extern void vector_move_item_here(vector v, vector_index_t dst, int rider,
							   vector_index_t src) ;
extern p_vector_item vector_delete_item(vector v, vector_index_t i) ;
extern void vector_reverse(vector v) ;
extern void vector_part_reverse(vector v, vector_index_t i, vector_length_t n) ;

Inline void vector_push_item(vector v, p_vector_item p_v) ;
Inline p_vector_item vector_pop_item(vector v) ;
Inline void vector_unshift_item(vector v, p_vector_item p_v) ;
Inline p_vector_item vector_shift_item(vector v) ;

extern void vector_insert(vector v, vector_index_t i, vector_length_t n) ;
extern void vector_delete(vector v, vector_index_t i, vector_length_t n) ;

typedef int vector_bsearch_cmp(const cvp* pp_val, const cvp* item) ;
vector_index_t vector_bsearch(vector v, vector_bsearch_cmp* cmp,
					      const void* p_val, int* result) ;
typedef int vector_sort_cmp(const cvp* a, const cvp* b) ;
void vector_sort(vector v, vector_sort_cmp* cmp) ;

extern vector vector_copy_here(vector dst, vector src) ;
extern vector vector_move_here(vector dst, vector src) ;
extern vector vector_copy_append(vector dst, vector src) ;
extern vector vector_move_append(vector dst, vector src) ;

#define vector_copy_extract(to, src, i_src, n_src) \
  vector_sak(1, to,   NULL, 0,    0,     src, i_src, n_src, 0)
#define vector_move_extract(to, src, i_src, n_src) \
  vector_sak(1, to,   NULL, 0,    0,     src, i_src, n_src, 1)
#define vector_copy_splice(to, dst, i_dst, n_dst, src, i_src, n_src) \
  vector_sak(1, to,   dst, i_dst, n_dst, src, i_src, n_src, 0)
#define vector_move_splice(to, dst, i_dst, n_dst, src, i_src, n_src) \
  vector_sak(1, to,   dst, i_dst, n_dst, src, i_src, n_src, 1)
#define vector_copy_replace(dst, i_dst, n_dst, src, i_src, n_src) \
  vector_sak(0, NULL, dst, i_dst, n_dst, src, i_src, n_src, 0)
#define vector_move_replace(dst, i_dst, n_dst, src, i_src, n_src) \
  vector_sak(0, NULL, dst, i_dst, n_dst, src, i_src, n_src, 1)

extern vector vector_sak(int to_copy, vector to,
        vector dst, vector_index_t i_dst, vector_length_t n_dst,
        vector src, vector_index_t i_src, vector_length_t n_src, int src_move) ;

extern void vector_discard(vector v, vector_index_t i) ;
extern void vector_chop(vector v) ;
extern void vector_decant(vector v) ;

Inline vector_index_t vector_extend_by_1(vector v) ;
extern void vector_extend(vector v, vector_length_t new_end) ;

/*==============================================================================
 * The inline functions:
 */

/* Extend vector by one item at the end, which is about to be set.      */
/* Returns index of new least item in the vector.                       */
/* NB: if left unset, the item may be UNDEFINED.                        */
Inline vector_index_t
vector_extend_by_1(vector v)
{
  vector_index_t i = v->end ;

  if (i < v->limit)
    return v->end++ ;                   /* simple if we have room       */

  vector_extend(v, i + 1) ;             /* the hard way                 */
  return i ;
} ;

/* Ensure given index is "active".                                      */
/* Adjusts logical and physical end of the vector as required, filling  */
/* with NULLs upto any new logical end.                                 */
Inline void
vector_ensure(vector v, vector_index_t i)
{
  if (i < v->end)                       /* trivial if within vector     */
    return ;
  if ((i == v->end) && (i < v->limit))  /* simple if end and have room  */
    v->p_items[v->end++] = NULL ;       /* set NULL for complete safety */
  else
    vector_extend(v, i + 1) ;           /* do it the hard way           */
} ;

/* Want vector to be at least the given length.                         */
/*                                                                      */
/* Adjusts logical and physical end of the vector as required, filling  */
/* with NULLs upto any new logical end -- does not allocate any more    */
/* than is exactly necessary.                                           */
Inline void
vector_set_min_length(vector v, vector_length_t len)
{
  if (len > v->end)                     /* will not reduce the length   */
    vector_set_new_min_length(v, len) ;
} ;

/* Want vector to be the given length.                                  */
/*                                                                      */
/* If this is less than the current length, items are discarded.  It    */
/* is the caller's responsibility to have freed anything that needs it. */
/*                                                                      */
/* Adjusts logical and physical end of the vector as required, filling  */
/* with NULLs upto any new logical end -- does not allocate any more    */
/* than is exactly necessary.                                           */
Inline void
vector_set_length(vector v, vector_length_t len)
{
  if (len > v->end)
    vector_set_new_min_length(v, len) ; /* Extend if new length greater */
  else
    v->end = len ;                      /* chop                         */
} ;

/* Return index of end of vector (index of last item + 1)               */
Inline vector_length_t
vector_length(vector v)
{
  return v->end ;
} ;

/* Returns whether vector is empty or not.                              */
Inline bool
vector_is_empty(vector v)
{
  return (v->end == 0) ;
} ;

/* Returns highly restricted pointer to vector body                     */
//Inline const void* const*
//vector_body(vector v)
//{
//  return (const void* const*)v->p_items ;
//} ;

Inline vector_body_t
vector_body(vector v)
{
  return (vector_body_t)v->p_items ;
} ;

/* Access functions -- Inline for obvious reasons.			*/

/* Get pointer to item.  Returns NULL if accessing beyond end.		*/
Inline p_vector_item
vector_get_item(vector v, vector_index_t i)
{
  return (i < v->end) ? v->p_items[i] : NULL ;
} ;

/* Get pointer to first item.  Returns NULL if vector empty.		*/
Inline p_vector_item
vector_get_first_item(vector v)
{
  return (v->end != 0) ? v->p_items[0] : NULL ;
} ;

/* Get pointer to last item.  Returns NULL if vector empty.		*/
Inline p_vector_item
vector_get_last_item(vector v)
{
  return (v->end != 0) ? v->p_items[v->end - 1] : NULL ;
} ;

/* Set item value in vector.  Extend vector if required.		*/
/* NB: it is the caller's responsibility to release memory used by any  */
/*     current value of the item, if required.                          */
Inline void
vector_set_item(vector v, vector_index_t i, p_vector_item p_v)
{
  vector_ensure(v, i) ;
  v->p_items[i] = (p_vector_item)p_v ;
} ;

/* Set dst item to be a copy of the src item.  Extend vector if required.
 *
 * NB: it is the caller's responsibility to look after the memory being
 *     used by the current dst item or the new (duplicated) src item.
 */
Inline void
vector_assign_item(vector v, vector_index_t dst, vector_index_t src)
{
  vector_set_item(v, dst, vector_get_item(v, src)) ;
} ;

/* Push value onto vector, extending as required.			*/
Inline void
vector_push_item(vector v, p_vector_item p_v)
{
  vector_index_t i = vector_extend_by_1(v) ;
  v->p_items[i] = (p_vector_item)p_v ;
} ;

/* Pop value from vector.  Returns NULL if vector is empty.		*/
/* NB: does NOT change the size of the vector body.                     */
Inline p_vector_item
vector_pop_item(vector v)
{
  return (v->end > 0) ? v->p_items[--v->end] : NULL ;
} ;

/* Unshift value onto start of vector, extending as required.           */
Inline void
vector_unshift_item(vector v, p_vector_item p_v)
{
  vector_insert(v, 0, 1) ;
  v->p_items[0] = p_v ;
} ;

/* Pop value from vector.  Returns NULL if vector is empty.             */
/* NB: does NOT change the size of the vector body.                     */
Inline p_vector_item
vector_shift_item(vector v)
{
  p_vector_item p_v = vector_get_first_item(v) ;
  vector_delete(v, 0, 1) ;
  return p_v ;
} ;

#endif /* _ZEBRA_VECTOR_H */

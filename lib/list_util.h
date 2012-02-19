/* List Utilities -- header
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

#ifndef _ZEBRA_LIST_UTIL_H
#define _ZEBRA_LIST_UTIL_H

#include "misc.h"

/*------------------------------------------------------------------------------
 * Note that the following fell foul of "strict-aliasing":
 *
 *    #define ssl_del_head(base, next) \
 *      ssl_del_head_func_i((void**)&(base), _lu_off(base, next))
 *
 *    Inline void*
 *    ssl_del_head_func_i(void** p_base, size_t link_offset)
 *    {
 *      void* item = *p_base ;
 *
 *      if (item != NULL)
 *        *p_base = _sl_next(item, link_offset) ;
 *
 *      return item ;
 *    } ;
 *
 * the assignment to *p_base is, apparently, unacceptable.  This works
 * perfectly well as an ordinary function.  Using a GNUC extension it is
 * possible to avoid the function call... hence the ugly skips.
 */
#ifdef __GNUC__
#define __GNUC__LIST_UTIL
#endif

/*==============================================================================
 * These utilities provide for linked lists of items, where the list pointers
 * are fields in the items.
 *
 * This is a little less general that the linklist stuff, but carries less
 * overhead.
 *
 * The items will be structures of some sort, and are described here as being
 * of type "struct item".  Pointers to those items will be of type
 * "struct item*".
 *
 * Most of these utilities are implemented as macros.
 *
 *------------------------------------------------------------------------------
 * Links and Bases.
 *
 * For a singly linked list, the item declaration is straightforward:
 *
 *   struct item
 *   {
 *     ....
 *     struct item*  foo_next ;
 *     ....
 *   }
 *
 * The item can live on more than one list, all that is required is that each
 * list has its next pointer.
 *
 * For double linked lists, the item may be declared:
 *
 *   struct item
 *   {
 *     ....
 *     struct dl_list_pair(struct item*) foo_list ;
 *     ....
 *   } ;
 *
 * A single base is straighforward:
 *
 *   struct item* foo_base ;
 *
 * and that may be a variable or a structure field.
 *
 * A double base may be declared:
 *
 *   struct dl_base_pair(struct item*) foo_base ;
 *
 * Various ways to construct structures or structure types:
 *
 *   typedef struct dl_list_pair(struct foo*) foo_list ;
 *
 *   struct foo_list dl_list_pair(struct foo*) ;
 *
 *   struct foo_base dl_base_pair(struct foo*) ;
 */

#define dl_list_pair(ptr_t)  { ptr_t next ; ptr_t prev ; }

#define dl_base_pair(ptr_t)  { ptr_t head ; ptr_t tail ; }

#define INIT_DL_BASE_PAIR    { NULL, NULL }

struct dl_void_list_pair dl_list_pair(void*) ;
struct dl_void_base_pair dl_base_pair(void*) ;

#define _lu_off(obj, field) ((char*)&((obj)->field) - (char*)(obj))

/*==============================================================================
 * Single Base, Single Link
 *
 * To delete entry must chase down list to find it.  Cannot insert at tail.
 *
 * Supports:
 *
 *   ssl_init(base)                 -- initialise base
 *
 *     An empty list has a NULL base.
 *
 *   ssl_push(base, item, next)     -- add at head of list
 *
 *     Treat as void function.  The item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one).
 *
 *   ssl_insert(base, after, item, next)  -- insert after given item
 *
 *     Treat as void function.  The item may *not* be NULL.
 *
 *     Inserts at head of list if after is NULL.
 *
 *   ssl_del(base, item, next)      -- delete from list
 *
 *     Treat as function returning bool.  Does nothing if the item is NULL.
 *
 *     Returns: true  => removed item from list
 *              false => item not found on list (or item was NULL)
 *
 *   ssl_del_head(base, next)       -- delete head of list
 *
 *     Treat as void function.  Does nothing if the list is empty.
 *
 *   ssl_pop(&dst, base, next)      -- pop head of list, if any
 *
 *     Treat as function returning void*.
 *
 *     Returns old head in dst and as return from "function".
 *
 *     Returns NULL and sets dst == NULL if list is empty.
 *
 *   ssl_head(base)                 -- return head of list
 *
 *     Treat as function returning void*.
 *
 *   ssl_next(item, next)           -- step to next item, if any
 *
 *     Treat as function returning void*.  Returns NULL if item is NULL.
 *
 * Note that ssl_del() and ssl_pop() do NOT affect the item->next pointer.
 *
 * Where:
 *
 *   "base" to be an r-value of type struct item*
 *
 *   "item" to be an l-value of type struct item*
 *
 *   "dst"  to be an r-value of type struct item*
 *
 *   "next" to be the name of a field in struct item, with type struct item*
 *
 *------------------------------------------------------------------------------
 * For example:
 *
 *   struct item                       // definition for list items
 *   {
 *     ...
 *     struct item*  bar_next ;
 *     ...
 *   } ;
 *
 *   static struct item* bar_base ;    // declaration of the list base
 *
 *   // create item and add to list (adds at front)
 *   struct item* q = calloc(1, sizeof(struct item)) ;
 *   ssl_push(bar_base, q, bar_next) ;
 *
 *   // remove item from list
 *   ssl_del(bar_base, q, bar_next) ;
 *
 *   // walk a list
 *   struct item* t = ssl_head(bar_base) ;
 *   while (t != NULL)
 *     {
 *       ....
 *       t = ssl_next(t, bar_next) ;
 *     }
 *
 *   // walk and empty out a list -- removing item before processing
 *   struct item* t ;
 *   while (ssl_pop(&t, bar_base, bar_next) != NULL)
 *     {
 *       ....  // t points to old head of list
 *     }
 *
 *   // walk and empty out a list -- removing after processing
 *   struct item* t ;
 *   while ((t = ssl_head(bar_base) != NULL)
 *     {
 *       ....
 *       ssl_del_head(bar_base, bar_next) ;
 *     }
 *
 * And for example:
 *
 *   struct parent_item                 // parent structure containing list
 *   {
 *      ....
 *      struct item* bar_base ;
 *      ....
 *   }
 *
 *   void footle(struct parent_item* parent, struct item* item)
 *   {
 *     ....
 *     ssl_push(parent->bar_base, item, bar_next) ;
 *     ....
 *   }
 */

#define ssl_init(base)                                          \
  ((base) = NULL)

#define ssl_push(base, item, next)                              \
  do { (item)->next = (base) ;                                  \
       (base) = item ;                                          \
  } while (0)

Private bool ssl_del_func(void** p_this, void* obj, size_t link_offset)
                                                    __attribute__((noinline)) ;

#define ssl_insert(base, after, item, next)                     \
  do { if ((after) == NULL)                                     \
         ssl_push(base, item, next) ;                           \
       else                                                     \
         { (item)->next  = (after)->next ;                      \
           (after->next) = item ;  } ;                          \
  } while (0)

#define ssl_del(base, item, next)                               \
  ssl_del_func((void**)(&base), item, _lu_off(item, next))

#define ssl_del_head(base, next)                                \
  do { if ((base) != NULL)                                      \
       (base) = (base)->next ;                                  \
  } while (0)

#define ssl_pop(dst, base, next)                                \
  ((*(dst) = (base)) != NULL ? ((base) = (base)->next, *(dst)) : NULL)

#define ssl_head(base) (base)

#define ssl_next(item, next)                                    \
  ((item) != NULL ? (item)->next : NULL)

/*   _sl_p_next(item, off) -- pointer to next pointer at given offset
 *   _sl_next(item, off)   -- contents of next pointer at given offset
 */

#define _sl_p_next(item, off)                                   \
  ((void**)( (char*)(item) + (off) ))

#define _sl_next(item, off)                                     \
  *_sl_p_next(item, off)

/*==============================================================================
 * Single Base, Double Link
 *
 * Can delete entry directly.
 *
 * Supports:
 *
 *   sdl_init(base)                 -- initialise base
 *
 *     An empty list has a NULL base.
 *
 *   sdl_push(base, item, list)     -- add at head of list
 *
 *     Treat as void function.  The item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one).
 *
 *   sdl_del(base, item, list)      -- delete from list
 *
 *     Treat as void function.  Does nothing if the item is NULL.
 *
 *     Undefined if item is not on the list.
 *
 *   sdl_del_head(base, next)       -- delete head of list
 *
 *     Treat as void function.  Does nothing if the list is empty.
 *
 *   sdl_pop(&dst, base, next)      -- pop head of list, if any
 *
 *     Treat as function returning void*.
 *
 *     Returns old head in dst and as return from "function".
 *
 *     Returns NULL and sets dst == NULL if list is empty.
 *
 *   sdl_head(base)                 -- return head of list
 *
 *     Treat as function returning void*.
 *
 *   sdl_next(item, next)           -- step to next item, if any
 *
 *     Treat as function returning void*.  Returns NULL if the item is NULL.
 *
 *   sdl_prev(item, next)           -- step to prev item, if any
 *
 *     Treat as function returning void*.  Returns NULL if the item is NULL.
 *
 * Note that sdl_del() and sdl_pop() do NOT affect the item->list.next
 * or item->list.prev pointers.
 *
 * Where:
 *
 *   "base" to be an r-value of type: struct base_pair(struct item*)*
 *
 *          That is... a variable or field which is a pointer to
 *
 *   "item" to be an l-value of type struct item*
 *
 *   "dst"  to be an r-value of type struct item*
 *
 *   "list" to be the name of a field in struct item
 *          of type: struct list_pair(struct item*)
 *
 *------------------------------------------------------------------------------
 * For example:
 *
 *   struct item                       // definition for list items
 *   {
 *     ...
 *     struct list_pair(struct item*)  bar_list ;
 *     ...
 *   } ;
 *
 *   static struct base_pair(struct item*) bar_base ;
 *                                    // declaration of the list base
 *
 *   // create item and add to list (adds at front)
 *   struct item* q = calloc(1, sizeof(struct item)) ;
 *   sdl_push(bar_base, q, bar_list) ;
 *
 *   // remove item from list
 *   sdl_del(bar_base, q, bar_list) ;
 *
 *   // walk a list
 *   struct item* t = sdl_head(bar_base) ;
 *   while (t != NULL)
 *     {
 *       ....
 *       t = sdl_next(t, bar_list) ;
 *     }
 *
 *   // walk and empty out a list -- removing item before processing
 *   struct item* t ;
 *   while (sdl_pop(&t, bar_base, bar_list) != NULL)
 *     {
 *       ....  // t points to old head of list
 *     }
 *
 *   // walk and empty out a list -- removing after processing
 *   struct item* t ;
 *   while ((t = sdl_head(bar_base) != NULL)
 *     {
 *       ....
 *       sdl_del_head(bar_base, bar_list) ;
 *     }
 *
 * And for example:
 *
 *   struct parent_item                 // parent structure containing list
 *   {
 *      ....
 *      struct base_pair(struct item*) bar_base ;
 *      ....
 *   }
 *
 *   void footle(struct parent_item* parent, struct item* item)
 *   {
 *     ....
 *     sdl_push(parent->bar_base, item, bar_list) ;
 *     ....
 *   }
 */

#define sdl_init(base)                                                  \
  ((base) = NULL)

#define sdl_push(base, item, list)                                      \
  do { confirm(_lu_off(base, list.next) == _lu_off(item, list.next)) ;  \
       confirm(_lu_off(base, list.prev) == _lu_off(item, list.prev)) ;  \
       (item)->list.next = (base) ;                                     \
       (item)->list.prev = NULL ;                                       \
       if ((base) != NULL)                                              \
         (base)->list.prev = (item) ;                                   \
       (base) = (item) ;                                                \
  } while (0)

#define sdl_del(base, item, list)                                       \
  do { confirm(_lu_off(base, list.next) == _lu_off(item, list.next)) ;  \
       confirm(_lu_off(base, list.prev) == _lu_off(item, list.prev)) ;  \
       if ((item) != NULL)                                              \
         {                                                              \
           if ((item)->list.next != NULL)                               \
             (item)->list.next->list.prev = (item)->list.prev ;         \
           if ((item)->list.prev != NULL)                               \
             (item)->list.prev->list.next = (item)->list.next ;         \
           else                                                         \
             (base) = (item)->list.next ;                               \
         } ;                                                            \
  } while (0)

#define sdl_del_head(base, list)                                        \
  do { if ((base) != NULL)                                              \
         {                                                              \
           (base) = (base)->list.next ;                                 \
           if ((base) != NULL)                                          \
             (base)->list.prev = NULL ;                                 \
         }                                                              \
  } while (0)

#define sdl_pop(dst, base, list)                                        \
  ((*(dst) = (base)) != NULL                                            \
    ? ( ((base) = (base)->list.next) != NULL                            \
      ? ( (base)->list.prev = NULL, *(dst) ) : *(dst) ) : NULL)

#define sdl_head(base) (base)

#define sdl_next(item, list)                                            \
  ((item) != NULL ? (item)->list.next : NULL)

#define sdl_prev(item, list)                                            \
  ((item) != NULL ? (item)->list.prev : NULL)

/*   _dl_p_next(obj, off)   -- pointer to next pointer at given offset
 *   _dl_next(obj, off)     -- contents of next pointer at given offset
 *   _dl_p_prev(obj, off)   -- pointer to prev pointer at given offset
 *   _dl_prev(obj, off)     -- contents of prev pointer at given offset
 */
#define _dl_p_next(obj, off)                                            \
  ( (void**)( (char*)(obj) + (off) + 0 ) )

#define _dl_next(obj, off)                                              \
  *_dl_p_next(obj, off)

#define _dl_p_prev(obj, off)                                            \
  ( (void**)( (char*)(obj) + (off) _ sizeof(void*) ) )

#define _dl_prev(obj, off)                                              \
  *_dl_p_next(obj, off)

/*==============================================================================
 * Double Base, Double Link
 *
 * Can delete entry directly.  Can insert and remove at tail.
 *
 * Supports:
 *
 *   ddl_init(base)                 -- initialise base
 *
 *     An empty list has *both* head and tail pointers NULL.
 *
 *     NB: confusion will arise if only one of these pointers is NULL.
 *
 *   ddl_push(base, item, list)     -- insert at head of list
 *
 *     Treat as void function.  The item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one).
 *
 *   ddl_append(base, item, list)   -- insert at tail of list
 *
 *     Treat as void function.  The item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one).
 *
 *   ddl_in_after(after, base, item, list)   -- insert after
 *
 *     Treat as void function.  The after & item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one), or if
 *     after is not on the list.
 *
 *   ddl_in_before(before, base, item, list) -- insert before
 *
 *     Treat as void function.  The before & item may *not* be NULL.
 *
 *     Undefined if item is already on any list (including this one), or if
 *     before is not on the list.
 *
 *   ddl_pop(&dst, base, next)      -- pop head of list, if any
 *
 *     Treat as function returning void*.
 *
 *     Returns old head in dst and as return from "function".
 *
 *     Returns NULL and sets dst == NULL if list is empty.
 *
 *   ddl_crop(&dst, base, next)     -- crop tail of list, if any
 *
 *     Treat as function returning void*.
 *
 *     Returns old tail in dst and as return from "function".
 *
 *     Returns NULL and sets dst == NULL if list is empty.
 *
 *   ddl_del(base, item, list)      -- delete from list
 *
 *     Treat as void function.  Does nothing if the item is NULL.
 *
 *     Undefined if item is not on the list.
 *
 *   ddl_del_head(base, next)       -- delete head of list
 *
 *     Treat as void function.  Does nothing if the list is empty.
 *
 *   ddl_del_tail(base, next)       -- delete tail of list
 *
 *     Treat as void function.  Does nothing if the list is empty.
 *
 *   ddl_head(base)                 -- return head of list
 *
 *     Treat as function returning void*.
 *
 *   ddl_tail(base)                 -- return tail of list
 *
 *     Treat as function returning void*.
 *
 *   ddl_next(item, next)           -- step to next item, if any
 *
 *     Treat as function returning void*.  Returns NULL if the item is NULL.
 *
 *   ddl_prev(item, next)           -- step to prev item, if any
 *
 *     Treat as function returning void*.  Returns NULL if the item is NULL.
 *
 * Note that ddl_del() and ddl_pop() do NOT affect the item->list.next
 * or item->list.prev pointers.
 *
 * Where:
 *
 *   "base" to be an r-value of type: struct base_pair(struct item*)*
 *
 *          That is... a variable or field which is a pointer to
 *
 *   "item" to be an l-value of type struct item*
 *
 *   "dst"  to be an r-value of type struct item*
 *
 *   "list" to be the name of a field in struct item
 *          of type: struct list_pair(struct item*)
 *
 *
 *
 *------------------------------------------------------------------------------
 * For example:
 *
 *   struct item                       // definition for list items
 *   {
 *     ...
 *     struct list_pair(struct item*)  bar_list ;
 *     ...
 *   } ;
 *
 *   static struct base_pair(struct item*) bar_base ;
 *                                    // declaration of the list base
 *
 *   // create item and add to list (adds at front)
 *   struct item* q = calloc(1, sizeof(struct item)) ;
 *   ddl_push(bar_base, q, bar_list) ;
 *
 *   // remove item from list
 *   ddl_del(bar_base, q, bar_list) ;
 *
 *   // walk a list
 *   struct item* t = ddl_head(bar_base) ;
 *   while (t != NULL)
 *     {
 *       ....
 *       t = ddl_next(t, bar_list) ;
 *     }
 *
 *   // walk and empty out a list -- removing item before processing
 *   struct item* t ;
 *   while (ddl_pop(&t, bar_base, bar_list) != NULL)
 *     {
 *       ....  // t points to old head of list
 *     }
 *
 *   // walk and empty out a list -- removing after processing
 *   struct item* t ;
 *   while ((t = ddl_head(bar_base) != NULL)
 *     {
 *       ....
 *       ddl_del_head(bar_base, bar_list) ;
 *     }
 *
 * And for example:
 *
 *   struct parent_item                 // parent structure containing list
 *   {
 *      ....
 *      struct base_pair(struct item*) bar_base ;
 *      ....
 *   }
 *
 *   void footle(struct parent_item* parent, struct item* item)
 *   {
 *     ....
 *     ddl_push(parent->bar_base, item, bar_list) ;
 *     ....
 *   }
 */

#define ddl_init(base)                                                  \
  ((base).head = (base).tail = NULL)

#define ddl_push(base, item, list)                                      \
  do { (item)->list.next = (base).head ;                                \
       (item)->list.prev = NULL ;                                       \
       if ((base).head != NULL)                                         \
         (base).head->list.prev = (item) ;                              \
       else                                                             \
         (base).tail = (item) ;                                         \
       (base).head = (item) ;                                           \
  } while (0)

#define ddl_append(base, item, list)                                    \
  do { (item)->list.next = NULL ;                                       \
       (item)->list.prev = (base).tail ;                                \
       if ((base).tail != NULL)                                         \
         (base).tail->list.next = (item) ;                              \
       else                                                             \
         (base).head = (item) ;                                         \
       (base).tail = (item) ;                                           \
  } while (0)

#define ddl_in_after(after, base, item, list)                           \
  do { (item)->list.next = (after)->list.next ;                         \
       (item)->list.prev = (after) ;                                    \
       if ((after)->list.next != NULL)                                  \
         (after)->list.next->list.prev = (item) ;                       \
       else                                                             \
         (base).tail = (item) ;                                         \
       (after)->list.next = (item) ;                                    \
  } while (0)

#define ddl_in_before(before, base, item, list)                         \
  do { (item)->list.next = (before) ;                                   \
       (item)->list.prev = (before)->list.prev ;                        \
       if ((before)->list.prev != NULL)                                 \
         (before)->list.prev->list.next = (item) ;                      \
       else                                                             \
         (base).head = (item) ;                                         \
       (before)->list.prev = (item) ;                                   \
  } while (0)

#define ddl_del(base, item, list)                                       \
  do { if ((item) != NULL)                                              \
         {                                                              \
           if ((item)->list.next != NULL)                               \
             (item)->list.next->list.prev = (item)->list.prev ;         \
           else                                                         \
             (base).tail = (item)->list.prev ;                          \
           if ((item)->list.prev != NULL)                               \
             (item)->list.prev->list.next = (item)->list.next ;         \
           else                                                         \
             (base).head = (item)->list.next ;                          \
         } ;                                                            \
  } while (0)

#define ddl_del_head(base, list)                                        \
  do { if ((base).head != NULL)                                         \
         {                                                              \
           (base).head = (base).head->list.next ;                       \
           if ((base).head != NULL)                                     \
             (base).head->list.prev = NULL ;                            \
           else                                                         \
             (base).tail = NULL ;                                       \
         }                                                              \
  } while (0)

#define ddl_del_tail(base, list)                                        \
  do { if ((base).tail != NULL)                                         \
         {                                                              \
           (base).tail = (base).tail->list.prev ;                       \
           if ((base).tail != NULL)                                     \
             (base).tail->list.next = NULL ;                            \
           else                                                         \
             (base).head = NULL ;                                       \
         }                                                              \
  } while (0)

#define ddl_pop(dst, base, list)                                        \
  ((*(dst) = (base).head) != NULL                                       \
    ? ( ((base).head = (base).head->list.next) != NULL                  \
          ? ( (base).head->list.prev = NULL, *(dst) )                   \
          : ( (base).tail            = NULL, *(dst) ) )                 \
    : NULL)

#define ddl_crop(dst, base, list)                                       \
  ((*(dst) = (base).tail) != NULL                                       \
    ? ( ((base).tail = (base).tail->list.prev) != NULL                  \
          ? ( (base).tail->list.next = NULL, *(dst) )                   \
          : ( (base).head            = NULL, *(dst) ) )                 \
    : NULL)

#define ddl_head(base) ((base).head)

#define ddl_tail(base) ((base).tail)

#define ddl_next(item, list)                                            \
  ((item) != NULL ? (item)->list.next : NULL)

#define ddl_prev(item, list)                                            \
  ((item) != NULL ? (item)->list.prev : NULL)

 /*==============================================================================
  * Double Base, Single Link
  *
  * To delete entry must chase down list to find it.  Can insert at tail, but
  * not remove (except by chasing down list).
  *
  * Supports:
  *
  *   dsl_init(base)                 -- initialise base
  *
  *     An empty list has *both* head and tail pointers NULL.
  *
  *     NB: confusion will arise if only one of these pointers is NULL.
  *
  *   dsl_push(base, item, next)     -- insert at head of list
  *
  *     Treat as void function.  The item may *not* be NULL.
  *
  *     Undefined if item is already on any list (including this one).
  *
  *   dsl_append(base, item, next)   -- insert at tail of list
  *
  *     Treat as void function.  The item may *not* be NULL.
  *
  *     Undefined if item is already on any list (including this one).
  *
  *   dsl_in_after(after, base, item, next)   -- insert after
  *
  *     Treat as void function.  The after & item may *not* be NULL.
  *
  *     Undefined if item is already on any list (including this one), or if
  *     after is not on the list.
  *
  *   dsl_pop(&dst, base, next)      -- pop head of list, if any
  *
  *     Treat as function returning void*.
  *
  *     Returns old head in dst and as return from "function".
  *
  *     Returns NULL and sets dst == NULL if list is empty.
  *
  *   dsl_del(base, item, next)      -- delete from list
  *
  *     Treat as void function.  Does nothing if the item is NULL.
  *
  *     Undefined if item is not on the list.
  *
  *   dsl_del_head(base, next)       -- delete head of list
  *
  *     Treat as void function.  Does nothing if the list is empty.
  *
  *   dsl_del_tail(base, next)       -- delete tail of list
  *
  *     Treat as void function.  Does nothing if the list is empty.
  *
  *   dsl_head(base)                 -- return head of list
  *
  *     Treat as function returning void*.
  *
  *   dsl_tail(base)                 -- return tail of list
  *
  *     Treat as function returning void*.
  *
  *   dsl_next(item, next)           -- step to next item, if any
  *
  *     Treat as function returning void*.  Returns NULL if the item is NULL.
  *
  * Note that dsl_del() and dsl_pop() do NOT affect the item->next pointer.
  *
  * Where:
  *
  *   "base" to be an r-value of type: struct base_pair(struct item*)*
  *
  *          That is... a variable or field which is a pointer to
  *
  *   "item" to be an l-value of type struct item*
  *
  *   "dst"  to be an r-value of type struct item*
  *
  *   "next" to be the name of a field in struct item of type struct item*
  *
  *------------------------------------------------------------------------------
  * For example:
  *
  *   struct item                       // definition for list items
  *   {
  *     ...
  *     struct item*  bar_next ;
  *     ...
  *   } ;
  *
  *   static struct base_pair(struct item*) bar_base ;
  *                                    // declaration of the list base
  *
  *   // create item and add to list (adds at front)
  *   struct item* q = calloc(1, sizeof(struct item)) ;
  *   dsl_push(bar_base, q, bar_next) ;
  *
  *   // remove item from list
  *   dsl_del(bar_base, q, bar_next) ;
  *
  *   // walk a list
  *   struct item* t = dsl_head(bar_base) ;
  *   while (t != NULL)
  *     {
  *       ....
  *       t = dsl_next(t, bar_next) ;
  *     }
  *
  *   // walk and empty out a list -- removing item before processing
  *   struct item* t ;
  *   while (dsl_pop(&t, bar_base, bar_next) != NULL)
  *     {
  *       ....  // t points to old head of list
  *     }
  *
  *   // walk and empty out a list -- removing after processing
  *   struct item* t ;
  *   while ((t = dsl_head(bar_base) != NULL)
  *     {
  *       ....
  *       dsl_del_head(bar_base, bar_next) ;
  *     }
  *
  * And for example:
  *
  *   struct parent_item                 // parent structure containing list
  *   {
  *      ....
  *      struct base_pair(struct item*) bar_base ;
  *      ....
  *   }
  *
  *   void footle(struct parent_item* parent, struct item* item)
  *   {
  *     ....
  *     dsl_push(parent->bar_base, item, bar_next) ;
  *     ....
  *   }
  */

 #define dsl_init(base)                                                  \
   ((base).head = (base).tail = NULL)

 #define dsl_push(base, item, next)                                      \
   do { (item)->next = (base).head ;                                     \
        if ((base).tail == NULL)                                         \
          (base).tail = (item) ;                                         \
        (base).head = (item) ;                                           \
   } while (0)

 #define dsl_append(base, item, next)                                    \
   do { (item)->next = NULL ;                                            \
        if ((base).tail != NULL)                                         \
          (base).tail->next = (item) ;                                   \
        else                                                             \
          (base).head = (item) ;                                         \
        (base).tail = (item) ;                                           \
   } while (0)

 #define dsl_in_after(after, base, item, next)                           \
   do { (item)->next = (after)->next ;                                   \
        (after)->next = (item) ;                                         \
        if ((base).tail == (after))                                      \
          (base).tail = (item) ;                                         \
   } while (0)

 Private bool dsl_del_func(struct dl_void_base_pair* p_base,
                                                 void* obj, size_t link_offset)
                                                     __attribute__((noinline)) ;
 #define dsl_del(base, item, list)                                       \
   dsl_del_func((struct dl_void_base_pair*)(&base), item,                \
                                                   _lu_off(item, next))

 #define dsl_del_head(base, list)                                        \
   do { if ((base).head != NULL)                                         \
          {                                                              \
            (base).head = (base).head->next ;                            \
            if ((base).head == NULL)                                     \
              (base).tail = NULL ;                                       \
          }                                                              \
   } while (0)

 #define dsl_pop(dst, base, list)                                        \
   ((*(dst) = (base).head) != NULL                                       \
     ? ( ((base).head = (base).head->next) == NULL                       \
           ? ( (base).tail = NULL, *(dst) )                              \
           :                       *(dst)   )                            \
     : NULL)

 #define dsl_head(base) ((base).head)

 #define dsl_tail(base) ((base).tail)

 #define dsl_next(item, list)                                            \
   ((item) != NULL ? (item)->next : NULL)

#endif /* _ZEBRA_LIST_UTIL_H */

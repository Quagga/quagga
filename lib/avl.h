/* Generic AVL tree structure -- header.
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

#ifndef _ZEBRA_AVL_H
#define _ZEBRA_AVL_H

#include "misc.h"
#include "list_util.h"

/*==============================================================================
 * Data structures etc.
 *
 * The red-black tree provided here is designed so that the nodes may be
 * embedded in some other data structure.
 *
 * The avl_node structure contains two pointers and the red flag.  The pointers
 * point to the avl_nodes embedded in the child nodes.
 *
 * The avl_tree structure points to the root of the tree, and contains:
 *
 */

/*------------------------------------------------------------------------------
 * Sort out AVL_DEBUG.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if AVL_NO_DEBUG is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set AVL_DEBUG    == 0 to turn off debug
 *       *  or set AVL_DEBUG    != 0 to turn on debug
 *       *  or set AVL_NO_DEBUG != 0 to force debug off
 */

#ifdef AVL_DEBUG                /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(AVL_DEBUG)
#  undef  AVL_DEBUG
#  define AVL_DEBUG 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define AVL_DEBUG QDEBUG
#endif

#ifdef AVL_NO_DEBUG             /* Override, if defined                 */
# if IS_NOT_ZERO_OPTION(AVL_NO_DEBUG)
#  undef  AVL_DEBUG
#  define AVL_DEBUG 0
# endif
#endif

enum { avl_debug = AVL_DEBUG } ;

/*------------------------------------------------------------------------------
 * Structures and other definitions
 */
typedef void* avl_value ;
typedef void* avl_key ;
typedef const void* avl_key_c ;

typedef struct avl_node* avl_node ;
typedef struct avl_node  avl_node_t ;

typedef enum
{
  avl_left   = 0,
  avl_right  = 1,
} avl_dir_t ;

struct avl_node
{
  avl_node  child[2] ;  /* two children, avl_left and avl_right         */
  avl_node  next ;      /* when tree has been linked in some order      */
  int8_t    bal ;
  uint8_t   level ;     /* set by some linkages                         */
} ;

typedef int avl_cmp_func(avl_key_c key, avl_value value) ;
typedef avl_value avl_new_func(avl_key_c key) ;

typedef struct avl_tree* avl_tree ;
typedef struct avl_tree  avl_tree_t ;

typedef enum
{
  avl_unlinked  = 0,            /* tree nodes are not linked    */
  avl_in_order,                 /* linked in-order              */
  avl_depth_first,              /* linked depth first           */
  avl_breadth_first,            /* linked breadth first         */
  avl_reverse_order,            /* linked in-order, reversed    */
  avl_reaming,                  /* some order, undefined        */
} avl_link_t ;

struct avl_tree
{
  avl_node  root ;              /* address of root node         */

  uint      node_count ;        /* number of nodes in the tree  */

  uint      offset ;            /* offset of node in value      */

  avl_link_t linked ;           /* how (if at all) linked       */
  uint8_t   height ;            /* set by some linkages         */

  struct dl_base_pair(avl_node) base ;
                                /* of linked nodes              */

  avl_new_func* new ;           /* create new value             */
  avl_cmp_func* cmp ;           /* compare key and value        */
} ;

/* Stack structure for "recursing" around tree.
 *
 * Absolute worst case for AVL tree is 1.44 lg N + 2, so we arrange here to
 * cope with N = 2^32 in a *worst case* -- which is clearly bonkers.
 */
typedef struct
{
  struct entry
  {
    avl_node  node ;
    avl_dir_t dir ;
  }
    empty[49],                  /* impossibly huge !            */
    full[1] ;                   /* sentinal                     */

  struct entry* sp ;

} avl_stack_t ;

#if 0                           /* dropped the tree walker      */

typedef struct
{
  avl_tree      tree ;

  uint          count ;

  uint          level ;
  bool          more ;

  avl_stack_t   stack ;
} avl_walker_t ;

typedef avl_walker_t* avl_walker ;

#endif

/*==============================================================================
 * Prototypes.
 */

extern avl_tree avl_tree_init_new(avl_tree tree, avl_new_func* new,
                                                 avl_cmp_func* cmp,
                                                                  uint offset) ;
extern avl_value avl_tree_ream(avl_tree tree, free_keep_b free_structure) ;
extern avl_tree avl_tree_reset(avl_tree tree, free_keep_b free_structure) ;

Inline uint avl_tree_node_count(avl_tree tree) ;

extern avl_value avl_lookup_add(avl_tree tree, avl_key_c key, bool* add) ;
extern avl_value avl_lookup(avl_tree tree, avl_key_c key) ;
extern avl_value avl_delete(avl_tree tree, avl_key_c key) ;

extern avl_value avl_tree_link(avl_tree tree, avl_link_t how) ;

Inline avl_node avl_get_node(avl_tree tree, avl_value value) ;

Inline avl_value avl_get_value(avl_tree tree, avl_node node) ;
Inline avl_value avl_get_child_value(avl_tree tree, avl_value value,
                                                                avl_dir_t dir) ;
Inline avl_value avl_get_next_value(avl_tree tree, avl_value value) ;
Inline uint avl_get_level(avl_tree tree, avl_value value) ;
Inline uint avl_get_height(avl_tree tree) ;
Inline int avl_get_balance(avl_tree tree, avl_value value) ;

#if 0                   /* Dropped the tree walker              */
extern uint avl_tree_walk_start(avl_tree tree, avl_walker walk) ;
extern avl_value avl_tree_walk_next(avl_walker walk) ;
extern avl_value avl_tree_walk_depth_next(avl_walker walk) ;
extern avl_value avl_tree_walk_level_next(avl_walker walk) ;
extern uint avl_tree_walk_depth(avl_walker walk) ;
extern uint avl_tree_walk_level(avl_walker walk) ;
#endif

/*==============================================================================
 * The Inlines
 */

/*------------------------------------------------------------------------------
 * Get the node count for the tree
 */
Inline uint
avl_tree_node_count(avl_tree tree)
{
  return (tree != NULL) ? tree->node_count : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get height of tree -- after avl_depth_first or avl_breadth_first
 *
 * Empty tree has height == 0, just root has height == 1, etc.
 */
Inline uint
avl_get_height(avl_tree tree)
{
  return (tree != NULL) ? tree->height : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Given an avl_value, return the enclosed avl_node.
 */
Inline avl_node
avl_get_node(avl_tree tree, avl_value value)
{
  return (avl_node)((char*)value + tree->offset) ;
} ;

/*------------------------------------------------------------------------------
 * Given an avl_node, return the enclosing avl_value.
 */
Inline avl_value
avl_get_value(avl_tree tree, avl_node node)
{
  return (avl_value)((char*)node - tree->offset) ;
} ;

/*------------------------------------------------------------------------------
 * Step to next avl_value as currently linked.
 */
Inline avl_value
avl_get_next_value(avl_tree tree, avl_value value)
{
  if (value != NULL)
    {
      avl_node next ;

      next = avl_get_node(tree, value)->next ;

      if (next != NULL)
        return avl_get_value(tree, next) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Return value for left/right child of given value (if any).
 */
Inline avl_value
avl_get_child_value(avl_tree tree, avl_value value, avl_dir_t dir)
{
  if (value != NULL)
    {
      avl_node child ;

      child = avl_get_node(tree, value)->child[dir] ;

      if (child != NULL)
        return avl_get_value(tree, child) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Get level for given value -- after avl_depth_first or avl_breadth_first
 *
 * Root has level == 0.
 */
Inline uint
avl_get_level(avl_tree tree, avl_value value)
{
  return (value != NULL) ? avl_get_node(tree, value)->level : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Return "balance" state of given value (if any).
 */
Inline int
avl_get_balance(avl_tree tree, avl_value value)
{
  return (value != NULL) ? avl_get_node(tree, value)->bal : 0 ;
} ;

#endif /* _ZEBRA_AVL_H */

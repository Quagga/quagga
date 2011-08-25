/* Generic AVL tree structure -- functions.
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
#include "avl.h"
#include "memory.h"

/*==============================================================================
 * This implementation of AVL trees ...
 *
 * Note: lg N is log base 2 of N.
 *
 * Perfectly balanced binary tree will give a maximum height of ceiling(lg N),
 * and an average path length of ~ lg N - 1.  So 500,000 nodes give a tree of
 * maximum height 19, and an average path length of ~18.  (The path length
 * being the number of comparisons required to find a given value, and the
 * average being the sum of comparisons to find all all nodes, divided by the
 * number of nodes.)
 *
 * The AVL tree will give a maximum height of 1.4405 lg N+2.  Building a tree
 * from a sorted list (forwards or backwards) seems to produce a *perfectly*
 * balanced tree !
 *
 * Experiments with AVL tree, using random keys, give results similar to
 * this:
 *
 */

/*==============================================================================
 * Initialisation, allocation, reset etc.
 */

/*------------------------------------------------------------------------------
 * Initialize AVL tree -- allocating if required.
 *
 * Returns the AVL tree which has been initialised.
 */
extern avl_tree
avl_tree_init_new(avl_tree tree, avl_new_func* new, avl_cmp_func* cmp,
                                                                    uint offset)
{
  if (tree == NULL)
    tree = XCALLOC(MTYPE_RB_TREE, sizeof(avl_tree_t)) ;
  else
    memset(tree, 0, sizeof(avl_tree_t)) ;

  /* Zeroising the structure has set:
   *
   *   root        -- NULL, empty tree
   *
   *   node_count  -- 0, no nodes, yet
   *
   *   offset      -- 0,    set below
   *
   *   linked      -- 0 => avl_unlinked
   *   height      -- 0
   *
   *   base        -- NULL, no nodes linked
   *
   *   cmp         -- NULL, set below
   *   new         -- NULL, set below
   */
  confirm(avl_unlinked == 0) ;

  tree->cmp    = cmp ;
  tree->new    = new ;
  tree->offset = offset ;

  return tree ;
} ;

/*------------------------------------------------------------------------------
 * Ream out given AVL tree -- freeing structure if required.
 *
 * Removes each entry in the tree and returns same, in some order, for the
 * caller to deal with.
 *
 * Returns:  next value to deal with -- NULL if tree is empty.
 *
 * If does not free the structure, it retains the parameters set when the tree
 * was initialised -- so tree can be reused without reinitialising it.
 *
 * NB: once started, this process MUST be completed.
 */
extern avl_value
avl_tree_ream(avl_tree tree, free_keep_b free_structure)
{
  avl_node next ;

  if (tree == NULL)
    return NULL ;               /* easy if no tree !    */

  if (tree->linked != avl_reaming)
    avl_tree_link(tree, avl_reaming) ;

  next = dsl_pop(&next, tree->base, next) ;

  if (next == NULL)
    avl_tree_reset(tree, free_structure) ;

  return next ;
} ;

/*------------------------------------------------------------------------------
 * Reset given AVL tree -- freeing structure if required.
 *
 * Returns:  NULL if frees structure, otherwise returns reset structure.
 *
 * If does not free the structure, it retains the parameters set when the tree
 * was initialised -- so tree can be reused without reinitialising it.
 *
 * This is pretty trivial because takes no responsibility for the data in which
 * the 'avl_node's are embedded -- so can discard the tree structure, or simply
 * set the root NULL, and the count to zero.
 *
 * NB: it is the caller's responsibility to release any tree item values
 *     *before* doing this.
 */
extern avl_tree
avl_tree_reset(avl_tree tree, free_keep_b free_structure)
{
  confirm(free_it) ;    /* free_it == true      */

  if (tree == NULL)
    return NULL ;               /* easy if no tree !    */

  if (free_structure)
    XFREE(MTYPE_RB_TREE, tree) ; /* sets tree = NULL      */
  else
    {
      tree->root       = NULL ;
      tree->node_count = 0 ;
      tree->linked     = avl_unlinked ;
      dsl_init(tree->base) ;
    } ;

  return tree ;
} ;

/*==============================================================================
 * AVL lookup, lookup-add and delete.
 */
static inline void avl_set_tos_pointer(avl_tree tree, const avl_stack_t* stack,
                                                                avl_node node) ;
static avl_node avl_rebalance_left(avl_node b) ;
static avl_node avl_rebalance_right(avl_node d) ;
static avl_node avl_rebalance_centre(avl_node b, avl_node c, avl_node d) ;

static uint avl_tree_check(avl_tree tree) ;

/*------------------------------------------------------------------------------
 * Lookup item in the red-black tree -- does NOT add if not found.
 *
 * Returns:  address of item found
 *       or: NULL if not found
 */
extern avl_value
avl_lookup(avl_tree tree, avl_key_c key)
{
  avl_node  node ;

  node = tree->root ;
  while (node != NULL)
    {
      avl_value  value ;
      int cmp ;

      value = avl_get_value(tree, node) ;
      cmp   = tree->cmp(key, value) ;

      if (cmp == 0)
        return value ;          /* FOUND                        */

      if (cmp < 0)              /* key < node's key             */
        node = node->child[avl_left] ;
      else
        node = node->child[avl_right] ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Lookup item in the AVL tree, and add if not there.
 *
 * Returns:  address of item found or added
 *
 * Sets:     *add false/true <=> found/added
 *
 * Uses the avl_create_func() to create the entry to insert from the given key.
 */
extern avl_value
avl_lookup_add(avl_tree tree, avl_key_c key, bool* add)
{
  avl_stack_t stack ;
  avl_value value ;
  avl_node  node ;
  avl_node* cp ;

  if (avl_debug || qdebug)
    memset(&stack, 0, sizeof(avl_stack_t)) ;

  /* Go down tree.
   */
  stack.sp = stack.empty ;
  cp = &tree->root ;
  while (1)
    {
      int cmp ;

      node = *cp ;

      if (node == NULL)
        break ;                 /* Not found -- must insert     */

      value = avl_get_value(tree, node) ;
      cmp   = tree->cmp(key, value) ;

      if (cmp == 0)
        {
          *add = false ;        /* Have not had to add          */
          return value ;        /* FOUND -- easy !!             */
        } ;

      stack.sp->node = node ;

      if (cmp < 0)              /* key < node's key             */
        {
          stack.sp->dir = avl_left ;
          cp = &node->child[avl_left] ;
        }
      else
        {
          stack.sp->dir = avl_right ;
          cp = &node->child[avl_right] ;
        } ;

      ++stack.sp ;

      qassert((stack.sp > stack.empty) && (stack.sp <= stack.full));
    } ;

  /* Create value and insert.
   *
   * When we emerge from the down tree loop we have:
   *
   *   cp -> points to empty pointer to the child to come.
   */
  value = tree->new(key) ;      /* make (minimal) value         */
  node  = avl_get_node(tree, value) ;

  node->child[avl_left]  = NULL ;
  node->child[avl_right] = NULL ;
  node->next             = NULL ;       /* tidy */
  node->bal = 0 ;

  *cp = node ;                  /* insert                       */

  /* When we emerge from the down tree loop we have:
   *
   *   sp - 1 -> node which will be parent, and whether left/right
   *
   *   but if tree is empty, then stack will be empty.
   *
   * Proceed up the stack, rebalancing as required.
   */
  while (stack.sp != stack.empty)
    {
      int delta ;
      int bal ;

      --stack.sp ;                      /* pop                  */
      qassert(stack.sp >= stack.empty) ;

      node  = stack.sp->node ;
      delta = (stack.sp->dir == avl_left) ? -1 : +1 ;

      bal = node->bal ;
      qassert((bal >= -1) && (bal <= +1)) ;

      /* If the old balance == 0:
       *
       *   new balance = delta.
       *
       *   the change has tipped the node, increasing the height, which
       *   must be propagated up the tree.
       *
       * If the new balance == 0:
       *
       *   the change has rebalanced the node, so height has not changed,
       *   so can stop now.
       *
       * Otherwise:
       *
       *   the change has unbalanced the node, in the delta direction, must
       *   now rebalance and if that succeeds, propagate up the tree, otherwise
       *   stop.
       */
      if (bal == 0)
        {
          node->bal = delta ;
          continue ;            /* tipped => height increased           */
        } ;

      bal += delta ;

      if (bal == 0)
        {
          node->bal = bal ;
          break ;               /* rebalanced => height unchanged       */
        } ;

      /* Must now rebalance and then update parent down pointer.
       */
      if (node->bal < 0)
        node = avl_rebalance_right(node) ;
      else
        node = avl_rebalance_left(node) ;

      avl_set_tos_pointer(tree, &stack, node) ;

      qassert(node->bal == 0) ;
      break ;                   /* rebalanced => height unchanged       */
    } ;

  if (avl_debug)
    avl_tree_check(tree) ;      /* check the balance            */

  /* Count in the new node and return its value.                        */

  ++tree->node_count ;
  tree->linked = avl_unlinked ; /* no longer complete           */

  *add = true ;                 /* we have added                */
  return value ;
} ;

/*------------------------------------------------------------------------------
 * Delete item from the red-black tree, if finds it.
 *
 * Returns:  address of item deleted
 *       or: NULL if not found.
 */
extern avl_value
avl_delete(avl_tree tree, avl_key_c key)
{
  avl_stack_t stack ;
  avl_value value ;
  avl_node  node ;
  avl_node  down ;
  avl_node* cp ;

  if (qdebug || avl_debug)
    memset(&stack, 0, sizeof(avl_stack_t)) ;

  /* Go down tree, looking for the node to be deleted.
   */
  stack.sp = stack.empty ;
  cp = &tree->root ;
  while (1)
    {
      int cmp ;

      node = *cp ;

      if (node == NULL)
        return NULL ;                   /* quit if not found    */

      value = avl_get_value(tree, node) ;
      cmp   = tree->cmp(key, value) ;

      if (cmp == 0)
        break ;                         /* found node to delete */

      stack.sp->node = node ;

      if (cmp < 0)                      /* key < node's key     */
        {
          stack.sp->dir = avl_left ;
          cp = &node->child[avl_left] ;
        }
      else
        {
          stack.sp->dir = avl_right ;
          cp = &node->child[avl_right] ;
        } ;

      ++stack.sp ;

      qassert((stack.sp > stack.empty) && (stack.sp <= stack.full));
    } ;

  /* When we emerge from the down tree loop we have found the node to be
   * deleted:
   *
   *   sp - 1 -> node which is the parent, and whether is left/right
   *
   *   cp    == address of parent pointer to node
   *
   *   node  == address of node
   *
   *   value == address of node value -- to be returned
   *
   * If the node has no left child, or no right child, or no children at all,
   * then the node can be deleted directly.
   *
   * If the node has both left and right children, the we need to go down to
   * find the successor of this node -- collecting stuff on the stack as we go.
   * We then "delete" the successor -- moving it to replace the node to be
   * really deleted.
   */
  down = node->child[avl_right] ;
  if ((down == NULL) || (node->child[avl_left] == NULL))
    {
      /* Node to be deleted has no right child or no left child, or no
       * children at all.
       *
       * Deletion is straightforward -- point parent at the child, if any.
       *
       *   down == right child.
       *   cp   == pointer to parent's pointer to the node
       */
      if (down == NULL)
        {
          /* No right child, go left.
           */
          down = node->child[avl_left] ;

          if (down == NULL)
            {
              /* No left child -- we are deleting a leaf.       */
              qassert(node->bal == 0) ;
            }
          else
            {
              /* Left child must be a leaf.                     */
              qassert(node->bal == -1) ;
              qassert(down->bal ==  0) ;
              qassert( (down->child[avl_left]  == NULL) &&
                       (down->child[avl_right] == NULL) ) ;
            }
        }
      else
        {
          /* Have right child, but no left.
           *
           * The right child must be a leaf.
           */
          qassert(node->bal == +1) ;
          qassert(down->bal ==  0) ;
          qassert( (down->child[avl_left]  == NULL) &&
                   (down->child[avl_right] == NULL) ) ;
        } ;
    }
  else
    {
      /* Node to be deleted has both left and right children.
       *
       * Go find the successor and move it into the place occupied by the
       * node to be deleted.
       *
       *   down == right child.
       *   cp   == pointer to parent's pointer to the node
       *
       * Will then balance on the basis of the removal of the successor.
       */
      avl_node* sp ;
      avl_node* qp ;

      /* Proceed down to find the successor
       */
      sp = &stack.sp->node ;    /* So can fix stack on the way back     */

      stack.sp->node = node ;   /* place self on stack                  */
      stack.sp->dir  = avl_right ;
      ++stack.sp ;
      qassert((stack.sp > stack.empty) && (stack.sp <= stack.full)) ;

      qp = &node->child[avl_right] ;
      while (1)
        {
          avl_node* dp ;

          dp = &down->child[avl_left] ;
          if (*dp == NULL)
            break ;                     /* down is the successor        */

          stack.sp->node = down ;       /* place on stack               */
          stack.sp->dir  = avl_left ;
          ++stack.sp ;
          qassert((stack.sp > stack.empty) && (stack.sp <= stack.full)) ;

          down = *dp ;
          qp = dp ;
        } ;

      /* Remove the successor, which has no left children.
       *
       * NB: does this before copying the contents of the node being deleted.
       *
       *     So, if the successor is immediately to the right of the node
       *     being deleted, then that is updated, first.
       */
      *qp = down->child[avl_right] ;

      /* Transfer the successor, so that it occupies the place of the
       * node to actually be deleted.
       *
       * For this purpose we have kept, for the node to be deleted:
       *
       *   cp = pointer to the parent's child pointer
       *
       *   sp = pointer to its stack entry
       *
       * So that the node to be deleted can be replaced by the node to be
       * kept.
       */
      *down = *node ;           /* replace contents of node to be kept  */
      *sp = down ;              /* stack entry must point here, too     */
    } ;

  /* To delete the node we now update the pointer in the parent to point at
   * the selected node down from the node to delete.
   */
  *cp = down ;

  /* Have deleted the node.  Now need to rebalance, as required.
   *
   * We have:
   *
   *   sp - 1 -> node which as the parent of the node we just deleted, and
   *             whether just deleted left or right child.
   *
   *   but if tree is now empty, then stack will be empty.
   */
  while (stack.sp != stack.empty)
    {
      int delta ;
      int bal ;

      --stack.sp ;                      /* pop                  */
      qassert(stack.sp >= stack.empty) ;

      node  = stack.sp->node ;
      delta = (stack.sp->dir == avl_left) ? +1 : -1 ;

      bal = node->bal ;
      qassert((bal >= -1) && (bal <= +1)) ;

      /* If the old balance == 0:
       *
       *   new balance = delta.
       *
       *   the change has tipped the node, so height has not changed,
       *   so can stop now.
       *
       * If the new balance == 0:
       *
       *   the change has rebalanced the node, reducing the height, which
       *   must be propagated up the tree.
       *
       * Otherwise:
       *
       *   the change has unbalanced the node, in the delta direction, must
       *   now rebalance and if that succeeds, propagate up the tree, otherwise
       *   stop.
       */
      if (bal == 0)
        {
          node->bal = delta ;
          break ;               /* tipped => no height change   */
        } ;

      bal += delta ;

      if (bal == 0)
        {
          node->bal = bal ;
          continue ;            /* rebalanced => height reduced */
        } ;

      /* Must now rebalance and then update parent down pointer.
       */
      if (delta < 0)
        node = avl_rebalance_right(node) ;
      else
        node = avl_rebalance_left(node) ;

      avl_set_tos_pointer(tree, &stack, node) ;

      if (node->bal != 0)
        break ;
    } ;

  if (avl_debug)
    avl_tree_check(tree) ;      /* check the balance            */

  /* Count off the deleted node and return its value.                   */

  --tree->node_count ;
  tree->linked = avl_unlinked ; /* no longer complete           */

  return value ;
} ;

/*------------------------------------------------------------------------------
 * Set the
 */
static inline void
avl_set_tos_pointer(avl_tree tree, const avl_stack_t* stack, avl_node node)
{
  if (stack->sp == stack->empty)
    tree->root = node ;
  else
    (stack->sp-1)->node->child[(stack->sp-1)->dir] = node ;
} ;

/*------------------------------------------------------------------------------
 * Rebalance AVL tree, by rotating LEFTWARDS -- RL/RRL
 *
 * RL -- Rotate LEFT -- two cases:
 *
 *   Case B:    +2                RL:           -1 <--
 *          ____b____                       ____d____
 *         /         \0 <<<<         --> +1/         \
 *        a          _d_                 _b_          e
 *                  /   \               /   \
 *                 c     e             a     c
 *
 *   Case X:    +2                RL:           -1 <--
 *          ____b____                       ____d____
 *         /         \+1 <<<<        --> +1/         \
 *        a          _d_                  _b_         e
 *                  /   \                /   \
 *                 c     e              a     c
 *
 * RRL -- Rotate Right and then LEFT -- three cases:
 *
 *   Case L:    +2                RRL:          0 <--
 *          ____b____                       ____c____
 *         /         \-1 <<<<         --> 0/         \+1 <--
 *        a          _d_                 _b_        _d_
 *           >>>> -1/   \               /   \      /   \
 *                 c     e             a     x    y     e
 *                / \
 *               x   y
 *
 *   Case 0:    +2                RRL:          0 <--
 *          ____b____                       ____c____
 *         /         \-1 <<<<         --> 0/         \0 <--
 *        a          _d_                 _b_         _d_
 *            >>>> 0/   \               /   \       /   \
 *                 c     e             a     x     y     e
 *                / \
 *               x   y
 *
 *   Case R:    +2                RRL:          0 <--
 *          ____b____                       ____c____
 *         /         \-1 <<<<        --> -1/         \0 <--
 *        a          _d_                 _b_         _d_
 *           >>>> +1/   \               /   \       /   \
 *                 c     e             a     x     y     e
 *                / \
 *               x   y
 *
 * Note that case 'B' does not occur in insertion.
 *
 * Note that we don't really care that 'b' is nominally '+2', and we don't
 * actually set that, so don't need to be able to represent it.
 */
static avl_node
avl_rebalance_left(avl_node b)
{
  avl_node  d ;
  avl_node  c ;
  int bal ;

  qassert(b->bal > 0) ;
  qassert(b->child[avl_right] != NULL) ;

  d = b->child[avl_right] ;     /* RIGHT child                  */
  c = d->child[avl_left] ;      /* RIGHT child's LEFT child     */

  bal = d->bal ;

  if (bal >= 0)
    {
      /* RL: single rotate LEFT
       */
      qassert(bal <= +1) ;

      b->child[avl_right] = c ;
      d->child[avl_left]  = b ;

      bal -= 1 ;                /* case: 'B'  0 -> -1   */
                                /* case: 'X' +1 ->  0   */

      b->bal = - bal ;          /* case: 'B' -> +1      */
                                /* case: 'X' ->  0      */
      d->bal =   bal ;          /* case: 'B' -> -1      */
                                /* case: 'X' ->  0      */

      return d ;            /* return the rotated up node   */
    }
  else
    {
      /* RRL: double rotate RIGHT then LEFT.
       */
      qassert(bal == -1) ;

      return avl_rebalance_centre(b, c, d) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Rebalance AVL tree, by rotating RIGHTWARDS -- RR/RLR
 *
 * RR -- Rotate RIGHT -- two cases:
 *
 *   Case B:       -2                RR:           +1 <--
 *             ____d____                       ____b____
 *      >>>> 0/         \                     /         \-1 <--
 *          _b_          e                   a          _d_
 *         /   \                                       /   \
 *        a     c                                     c     e
 *
 *   Case X:       -2                RR:           0 <--
 *             ____d____                       ____b____
 *     >>>> -1/         \                     /         \0 <--
 *          _b_          e                   a          _d_
 *         /   \                                       /   \
 *        a     c                                     c     e
 *
 *
 * RLR -- Rotate Left and then RIGHT -- three cases:
 *
 *   Case L:       -2                RLR:          0 <--
 *             ____d____                       ____c____
 *     >>>> +1/         \                --> 0/         \+1 <--
 *          _b_          e                  _b_        _d_
 *         /   \-1 <<<<                    /   \      /   \
 *        a     c                         a     x    y     e
 *             / \
 *            x   y
 *
 *   Case 0:       -2                RLR:          0 <--
 *             ____d____                       ____c____
 *     >>>> +1/         \                --> 0/         \0 <--
 *          _b_          e                  _b_        _d_
 *         /   \0 <<<<                     /   \      /   \
 *        a     c                         a     x    y     e
 *             / \
 *            x   y
 *
 *   Case R:       -2                RLR:          0 <--
 *             ____d____                       ____c____
 *     >>>> +1/         \               --> -1/         \0 <--
 *          _b_          e                  _b_        _d_
 *         /   \+1 <<<<                    /   \      /   \
 *        a     c                         a     x    y     e
 *             / \
 *            x   y
 *
 * Note that case 'B' does not occur in insertion.
 *
 * Note that we don't really care that 'd' is nominally '-2', and we don't
 * actually set that, so don't need to be able to represent it.
 */
static avl_node
avl_rebalance_right(avl_node d)
{
  avl_node  b ;
  avl_node  c ;
  int bal ;

  qassert(d->bal < 0) ;
  qassert(d->child[avl_left] != NULL) ;

  b = d->child[avl_left] ;      /* LEFT child                   */
  c = b->child[avl_right] ;     /* LEFT child's RIGHT child     */

  bal = b->bal ;

  if (bal <= 0)
    {
      /* RR: single rotate RIGHT
       */
      qassert(bal >= -1) ;

      b->child[avl_right] = d ;
      d->child[avl_left]  = c ;

      bal += 1 ;                /* case: 'B'  0 -> +1   */
                                /* case: 'X' -1 ->  0   */

      b->bal =   bal ;          /* case: 'B' -> +1      */
                                /* case: 'X' ->  0      */
      d->bal = - bal ;          /* case: 'B' -> -1      */
                                /* case: 'X' ->  0      */

      return b ;            /* return the rotated up node   */
    }
  else
    {
      /* RLR: double rotate LEFT then RIGHT.
       */
     qassert(bal == +1) ;

     return avl_rebalance_centre(b, c, d) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Rebalance AVL tree -- double rotation.
 */
static avl_node
avl_rebalance_centre(avl_node b, avl_node c, avl_node d)
{
  int bal ;

  qassert(c != NULL) ;

  bal = c->bal ;                /* -1 => case: 'L'      */
                                /*  0 => case: '0'      */
                                /* +1 => case: 'R'      */
  qassert((bal >= -1) && (bal <= +1)) ;

  b->child[avl_right] = c->child[avl_left] ;
  d->child[avl_left]  = c->child[avl_right] ;

  c->child[avl_left]  = b ;
  c->child[avl_right] = d ;

  b->bal = (bal > 0) ? -1 : 0 ;
  c->bal = 0 ;
  d->bal = (bal < 0) ? +1 : 0 ;

  return c ;
} ;

/*==============================================================================
 * Tree linking
 */
static void avl_link_in_order(avl_tree tree, avl_node node) ;
static void avl_link_depth_first(avl_tree tree, avl_node node, uint level) ;
static void avl_link_breadth_first(avl_tree tree, avl_node node, uint level) ;
static void avl_link_reverse_order(avl_tree tree, avl_node node) ;

/*------------------------------------------------------------------------------
 * Link list in the required order, and return first value in that order.
 *
 * This is debounced if is already in the required order.
 *
 * Inserting or deleting nodes clears the trees link state, so a subsequent
 * link call will remake the required list.
 *
 * NB: any insertions made after the linkage is made will not affect the
 *     linkage, but will not be included in it.
 *
 *     Any deletions made after the linkage may or may not affect it.  However,
 *     while walking the linked list, can delete the current value, and
 *     continue.
 *
 * NB: linking depth_first or breadth_first sets the level on every node, and
 *     sets the height on the tree.  Any change to the tree may invalidate that.
 */
extern avl_value
avl_tree_link(avl_tree tree, avl_link_t how)
{
  avl_node node ;

  if (how != tree->linked)
    {
      dsl_init(tree->base) ;
      tree->linked = how ;
      tree->height = 0 ;

      switch (how)
        {
          default:
            qassert(false) ;
            fall_through ;

          case avl_unlinked:
            break ;

          case avl_in_order:
          case avl_reaming:
            avl_link_in_order(tree, tree->root) ;
            break ;

          case avl_depth_first:
            avl_link_depth_first(tree, tree->root, 0) ;
            break ;

          case avl_breadth_first:
            avl_link_breadth_first(tree, tree->root, 0) ;
            break ;

          case avl_reverse_order:
            avl_link_reverse_order(tree, tree->root) ;
            break ;
        } ;
    } ;

  node = dsl_head(tree->base) ;

  return (node != NULL) ? avl_get_value(tree, node) : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Add given subtree to tree list, in-order.
 */
static void
avl_link_in_order(avl_tree tree, avl_node node)
{
  if (node != NULL)
    {
      avl_link_in_order(tree, node->child[avl_left]) ;
      dsl_append(tree->base, node, next) ;
      avl_link_in_order(tree, node->child[avl_right]) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Add given subtree to tree list, depth first.
 */
static void
avl_link_depth_first(avl_tree tree, avl_node node, uint level)
{
  if (node != NULL)
    {
      avl_link_depth_first(tree, node->child[avl_left], level + 1) ;
      avl_link_depth_first(tree, node->child[avl_right], level + 1) ;
      dsl_append(tree->base, node, next) ;
      node->level = level ;
    }
  else
    {
      if (level > tree->height) /* If this is leaf, then level is its   */
        tree->height = level ;  /* height                               */
    } ;
} ;

/*------------------------------------------------------------------------------
 * Add given subtree to tree list, processing each level in turn.
 */
static void
avl_link_breadth_first(avl_tree tree, avl_node node, uint level)
{
  struct dl_base_pair(avl_node) queue ;
  uint height ;

  dsl_init(queue) ;
  height = 0 ;

  if (node != NULL)
    node->level = 0 ;           /* root is level 0      */

  while (node != NULL)
    {
      avl_node child ;

      level = node->level + 1 ;

      if (level > height)       /* If this is leaf, then level is its   */
        height = level ;        /* height                               */

      dsl_append(tree->base, node, next) ;

      if ((child = node->child[avl_left]) != NULL)
        {
          dsl_append(queue, child, next) ;
          child->level = level ;
        } ;

      if ((child = node->child[avl_right]) != NULL)
        {
          dsl_append(queue, child, next) ;
          child->level = level ;
        } ;

      node = dsl_pop(&node, queue, next) ;
    } ;

  tree->height = height ;
} ;

/*------------------------------------------------------------------------------
 * Add given subtree to tree list in reverse order.
 */
static void
avl_link_reverse_order(avl_tree tree, avl_node node)
{
  if (node != NULL)
    {
      avl_link_in_order(tree, node->child[avl_right]) ;
      dsl_append(tree->base, node, next) ;
      avl_link_in_order(tree, node->child[avl_left]) ;
    } ;
} ;

/*==============================================================================
 * Diagnostics
 */

static uint avl_get_node_height(avl_node node) ;

/*------------------------------------------------------------------------------
 * Get the height of the given tree.
 *
 * Returns the height of the longest branch.
 *
 * Returns zero if tree is empty.
 *
 * NB: if avl_debug, checks the balance of every node.
 */
static uint
avl_tree_check(avl_tree tree)
{
  return avl_get_node_height(tree->root) ;
} ;

/*------------------------------------------------------------------------------
 * Get the height of the given node.
 *
 * Gets the heights of the left and right sub-trees, and returns the max of
 * those + 1.
 *
 * If avl_debug, checks the balance on every node against the left and right
 * tree heights.
 *
 * Returns zero if node is NULL.
 */
static uint
avl_get_node_height(avl_node node)
{
  uint hl, hr ;

  if (node == NULL)
    return 0 ;

  hl = avl_get_node_height(node->child[avl_left]) ;
  hr = avl_get_node_height(node->child[avl_right]) ;

  if (avl_debug)
    assert(node->bal == ((int)hr - (int)hl)) ;

  return ((hl >= hr) ? hl : hr) + 1 ;
} ;

/*==============================================================================
 * Tree walking -- replaced by tree linking.
 */

#if 0

/*------------------------------------------------------------------------------
 * Start a tree walk.
 *
 * Returns:  number of nodes in the tree
 *
 * Note that if the tree is NULL, will set up a walk that will stop immediately.
 */
extern uint
avl_tree_walk_start(avl_tree tree, avl_walker walk)
{
  memset(walk, 0, sizeof(avl_walker_t)) ;

  /* Zeroising sets:
   *
   *   tree        -- NULL, set below
   *
   *   count       -- 0   => walk just started
   *
   *   level       -- 0      ) see avl_tree_walk_level_next
   *   more        -- false  )
   *
   *   stack       -- all zero.  Set by "next" when count == 0 and root != NULL
   */
  walk->tree   = tree ;

  return (tree != NULL) ? tree->node_count : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Step to next in-order node in given walk
 */
extern avl_value
avl_tree_walk_next(avl_walker walk)
{
  avl_node node, down ;

  if (walk->count == 0)
    {
      /* We are at the start of the process, which is the end if the tree
       * is empty.
       *
       * Need to head down to the left-most child.
       */
      if (walk->tree == NULL)
        return NULL ;

      node = walk->tree->root ;

      if (node == NULL)
        return NULL ;

      walk->stack.sp = walk->stack.empty ;
    }
  else
    {
      /* We are somewhere in the tree, top of the stack contains the node
       * whose value we just returned.  So:
       *
       *   a. if there is a right child:
       *
       *      step right and then head leftwards and return the left-most
       *      available (or self if none).
       *
       *   b. if no right child:
       *
       *      if this is the root, we are finished.
       *
       *      if this is the left child of the parent, return to parent and
       *      return its value.
       *
       *      if this is the right child of the parent, return to the parent,
       *      repeat....
       */
      node = walk->stack.sp->node ;
      down = node->child[avl_right] ;

      if (down != NULL)
        {
          ++walk->stack.sp ;            /* push current                 */
          node = down ;
        }
      else
        {
          while (1)
            {
              if (node == walk->tree->root)
                {
                  /* We have returned to the root -- we are done.       */
                  qassert(walk->stack.sp == walk->stack.empty) ;
                  return NULL ;
                } ;

              qassert(walk->stack.sp > walk->stack.empty) ;

              if (walk->stack.sp->dir == avl_left)
                {
                  /* We can step up the left-hand path, and return the
                   * parent's value.
                   */
                  --walk->stack.sp ;

                  ++walk->count ;
                  return avl_get_value(walk->tree, walk->stack.sp->node) ;
                } ;

              /* We step up the right-hand path, and keep going, depending
               * on how we arrived at the parent node.
               */
              --walk->stack.sp ;
              node = walk->stack.sp->node ;
            } ;
        } ;
    } ;

  /* If we get here we have just stepped rightwards to the current
   * node.
   *
   * Now head as far to the left as we can go.
   *
   * Note that the root is depth == 0.
   */
  walk->stack.sp->node = node ;
  walk->stack.sp->dir  = avl_right ;

  while ((down = node->child[avl_left]) != NULL)
    {
      ++walk->stack.sp ;    /* push current         */
      node = down ;

      walk->stack.sp->node = node ;
      walk->stack.sp->dir  = avl_left ;
    } ;

  qassert( (walk->stack.sp >= walk->stack.empty)
        && (walk->stack.sp <= walk->stack.full) ) ;

  ++walk->count ;
  return avl_get_value(walk->tree, node) ;
} ;

/*------------------------------------------------------------------------------
 * Step to next depth-first node in given walk
 */
extern avl_value
avl_tree_walk_depth_next(avl_walker walk)
{
  avl_node node, down ;

  if (walk->count == 0)
    {
      /* We are at the start of the process, which is the end if the tree
       * is empty.
       *
       * Need to head down to the left-most child.
       */

      down = walk->tree->root ;

      if (down == NULL)
        return NULL ;

      walk->stack.sp = walk->stack.empty ;
    }
  else
    {
      /* We are somewhere in the tree, top of the stack contains the node
       * whose value we just returned.  So:
       *
       *   a. if this is the root, we are finished.
       *
       *   b. if this is a left child, return to the parent, and then head
       *      right and down.
       *
       *   c. if this is a right child, return the parent value.
       */
      node = walk->stack.sp->node ;

      if (node == walk->tree->root)
        {
          /* We have returned to the root -- we are done.       */
          qassert(walk->stack.sp == walk->stack.empty) ;
          return NULL ;
        } ;

      node = (walk->stack.sp - 1)->node ;
      down = NULL ;

      if ( (walk->stack.sp->dir == avl_left)
                                  && ((down = node->child[avl_right]) != NULL) )
        ;                       /* Go to the right, and then down       */
      else
        --walk->stack.sp ;      /* pop                                  */
    } ;

  /* If down != NULL, we have just stepped rightwards to that node (or have
   * just started with the root node).  In which case head as far down as
   * we can go.
   *
   * If down == NULL, return the current node.
   *
   * Note that the root is depth == 0.
   */
  if (down != NULL)
    {
      node = down ;

      walk->stack.sp->node = node ;
      walk->stack.sp->dir  = avl_right ;

      while (1)
        {
          avl_dir_t dir ;

          if      ((down = node->child[avl_left]) != NULL)
            dir = avl_left ;
          else if ((down = node->child[avl_right]) != NULL)
            dir = avl_right ;
          else
            break ;

          ++walk->stack.sp ;    /* push current         */
          node = down ;

          walk->stack.sp->node = node ;
          walk->stack.sp->dir  = dir ;
        }
    } ;

  qassert( (walk->stack.sp >= walk->stack.empty)
        && (walk->stack.sp <= walk->stack.full) ) ;

  ++walk->count ;
  return avl_get_value(walk->tree, node) ;
} ;

/*------------------------------------------------------------------------------
 * Step to next level order node in given walk
 */
extern avl_value
avl_tree_walk_level_next(avl_walker walk)
{
  avl_node  node ;

  /* First: if we are at the beginning, place self on root, at proceed
   *        from there.
   *
   * Otherwise, we need to backtrack to find next branch to run down, or
   * restart the process with a new target level.
   */
  if (walk->count == 0)
    {
      /* We are at the start of the process, which is the end if the tree
       * is empty.
       *
       * Need to head down to the left-most child.
       */

      node = walk->tree->root ;

      if (node == NULL)
        return NULL ;

      walk->stack.sp = walk->stack.empty ;

      walk->stack.sp->node = node ;
      walk->stack.sp->dir  = avl_right ;
    }
  else
    {
      uint     level ;

      level = walk->level ;     /* current level == target level !      */
      qassert(avl_tree_walk_depth(walk) == level) ;

      node = walk->stack.sp->node ;

      /* The outer do loop manages the backtracking required when cannot reach
       * the current level along the current branch.
       */
      do
        {
          /* We are somewhere in the tree, top of the stack contains the node
           * whose value we just returned, or where we stopped last time,
           * because failed to reach depth.  So:
           *
           *   a. if we are at the root, and there is more to come, set the
           *      new target level and set off to find it.
           *
           *   b. backtrack.
           *
           *      if was left child of parent, try the right child, otherwise,
           *      go back to (a).
           *
           * The while loop backtracks until:
           *
           *   * hit root and either increases the level, or stops.
           *
           *     Note that we cope with the case of having just returned the
           *     root node.
           *
           *   * are able to step rightwards, from a node that we backtrack to.
           */
          while (1)
            {
              if (node == walk->tree->root)
                {
                  /* We have returned to the root -- we are done.       */
                  qassert(walk->stack.sp == walk->stack.empty) ;
                  qassert(level == 0) ;

                  if (!walk->more)
                    return NULL ;

                  walk->more = false ;
                  ++walk->level ;
                  break ;               /* proceed to find level        */
                }
              else
                {
                  avl_dir_t dir ;
                  avl_node  down ;

                  dir = walk->stack.sp->dir ;

                  --walk->stack.sp ;            /* pop          */
                  --level ;                     /* up a level   */

                  node = walk->stack.sp->node ;

                  if ( (dir == avl_left) &&
                       ((down = node->child[avl_right]) != NULL) )
                    {
                      /* We can step right and down             */
                      node = down ;

                      ++walk->stack.sp ;            /* push         */
                      ++level ;                     /* down a level */

                      break ;           /* proceed to find level        */
                    } ;
                } ;
            } ;

          /* If we get here we have just stepped rightwards to the current
           * node -- or have just started at the root.
           *
           * Now head as far to the down we can go, subject to stopping at the
           * current required level.
           *
           * Note that the root is depth == 0.
           */
          walk->stack.sp->node = node ;
          walk->stack.sp->dir  = avl_right ;

          while (level < walk->level)
            {
              avl_node  down ;
              avl_dir_t dir ;

              if      ((down = node->child[avl_left]) != NULL)
                dir = avl_left ;
              else if ((down = node->child[avl_right]) != NULL)
                dir = avl_right ;
              else
                break ;

              ++level ;
              ++walk->stack.sp ;    /* push current         */
              node = down ;

              walk->stack.sp->node = node ;
              walk->stack.sp->dir  = dir ;
            } ;

        /* This is the end of the outer do loop.
         *
         * If we have reached the required depth, then can exit the loop to
         * return the current node.
         *
         * Otherwise, there is nothing deep enough on the current branch, so
         * loops back to backtrack.
         */
        } while (level < walk->level) ;
    } ;

  /* When we get here we are ready to return the current node.          */
  if ((node->child[avl_left] != NULL) || (node->child[avl_right] != NULL))
    walk->more = true ;

  qassert( (walk->stack.sp >= walk->stack.empty)
        && (walk->stack.sp <= walk->stack.full) ) ;

  ++walk->count ;
  return avl_get_value(walk->tree, node) ;
} ;

/*------------------------------------------------------------------------------
 * How deep in the tree was the last value returned by the walk ?
 *
 * Note that the root is at depth == 0.
 */
extern uint
avl_tree_walk_depth(avl_walker walk)
{
  return (walk->stack.sp - walk->stack.empty) ;
} ;

/*------------------------------------------------------------------------------
 * For avl_tree_walk_level_next walk, what level are we at ?
 *
 * Note that the root is at level == 0.
 */
extern uint
avl_tree_walk_level(avl_walker walk)
{
  return walk->level ;
} ;
#endif

/*==============================================================================
 * AVL Tree torture tests
 *
 */
#include "misc.h"

#include <stdio.h>

#include "avl.h"
#include "symtab.h"
#include "qlib_init.h"
#include "thread.h"
#include "command.h"

/*==============================================================================
 * prototypes
 */

typedef enum
{
  forwards,
  backwards,
  random_order,
} order_t ;

int main(int argc, char **argv);

static void assert_true(int result, const char * message);

static void test_avl_init(void) ;
static void test_avl_tree_new(void) ;
static void test_avl_tree_lookup(const uint len, const order_t how,
                                                 const uint seed) ;
static void test_avl_tree_delete(const uint len, const order_t how,
                                                 const uint seed) ;

static void scan_avl_tree(avl_tree tree, bool compare) ;
static void show_tree(avl_tree tree) ;
static void shuffle(uint list[], uint n, uint seed) ;

/*------------------------------------------------------------------------------
 * Run all tests
 */
int
main(int argc, char **argv)
{
  int i ;
  uint s ;

  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  test_avl_init() ;

  test_avl_tree_new();

  test_avl_tree_lookup(75, forwards,     0) ;
  test_avl_tree_lookup(75, backwards,    0) ;
  test_avl_tree_lookup(75, random_order, 191229507) ;

  test_avl_tree_lookup(10000, forwards,     0) ;
  test_avl_tree_lookup(10000, backwards,    0) ;
  test_avl_tree_lookup(10000, random_order, 231690116) ;

  test_avl_tree_delete(75, forwards, 39219284) ;

  s = 39219283 ;
  for (i = 1 ; i <= 2000 ; ++i)
    {
      s *= 1234567 ;
      test_avl_tree_delete(10000, forwards, s) ;
    } ;

  return 0;
}

static void
assert_true(int result, const char * message)
{
  if (!result)
    {
      printf("Assert failed: %s\n", message);
    }
}

/*==============================================================================
 * Data structures and related functions
 */

/* The test avl_value is pretty simple.                                  */
struct test_value
{
  uint  val ;

  uint  visit ;

  uint  pos ;

  avl_node_t avl ;

  char  name[] ;
} ;

typedef char test_name_t[24] ;

typedef struct test_value  test_value_t ;
typedef struct test_value* test_value ;

static uint value_count = 0 ;   /* Keep track of values created/freed   */
static uint value_max   = 0 ;   /* Keep track of values created/freed   */
static uint value_visit = 0 ;   /* current visit number                 */

enum { max_value_count = 100 * 1000 } ;

static test_value values[max_value_count] ;
static uint order[max_value_count] ;

/*------------------------------------------------------------------------------
 * Initialise the test value handling
 *
 */
static void
test_avl_init(void)
{
  uint i ;

  for (i = 0 ; i < max_value_count ; ++i)
    {
      values[i] = NULL ;
      order[i]  = 0 ;
    } ;

  value_count = 0 ;
  value_max   = 0 ;
  value_visit = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set an ordering
 */
static char*
test_avl_set_order(uint len, order_t how, uint seed)
{
  uint i ;
  char* desc ;

  assert(len <= max_value_count) ;

  if (seed != 0)
    srand(seed) ;

  for (i = 0 ; i < len ; ++i)
    if (how == forwards)
      order[i] = i ;            /* forwards             */
    else
      order[i] = len - i - 1 ;  /* backwards or random  */

  if (how == random_order)
    shuffle(order, len, 0) ;

  desc = calloc(1, 60) ;
  switch (how)
    {
      case forwards:
        snprintf(desc, 60, "%d forwards", len) ;
        break ;

      case backwards:
        snprintf(desc, 60, "%d backards", len) ;
        break ;

      case random_order:
        snprintf(desc, 60, "%d random(%d)", len, seed) ;
        break ;
    } ;

  return desc ;
} ;

/*------------------------------------------------------------------------------
 * Shuffle array of uints
 */
static void
shuffle(uint list[], uint n, uint seed)
{
  if (seed != 0)
    srand(seed) ;

  while (n > 1)
    {
      uint v ;
      uint r ;

      r = rand() % n ;

      --n ;
            v = list[r] ;
      list[r] = list[n] ;
      list[n] = v ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set value name
 */
static const char*
test_avl_set_name(test_name_t name, uint val)
{
  snprintf(name, sizeof(test_name_t), "Name:%d", val) ;
  return name ;
} ;

/*------------------------------------------------------------------------------
 * Set new test value.
 *
 * Used when avl_insert_add signals that a new value has been added -- makes
 * sure that the value should not already exist, and replaces any existing
 * value.
 */
static void
test_avl_set_value(test_value value, uint val)
{
  test_name_t  name ;

  assert(val < max_value_count) ;

  assert(values[val] == NULL) ;
  assert(strcmp(test_avl_set_name(name, val), value->name) == 0) ;

  values[val] = value ;

  value->val = val ;
  value->visit = 0 ;

  if (val > value_max)
    value_max = val ;

  ++value_count ;
} ;

/*------------------------------------------------------------------------------
 * Unset test value.
 *
 * Used when avl_insert_add signals that a new value has been added -- makes
 * sure that the value should not already exist, and replaces any existing
 * value.
 */
static void
test_avl_unset_value(test_value value)
{
  if (value != NULL)
    {
      assert(value->val < max_value_count) ;
      assert(values[value->val] == value) ;

      values[value->val] = NULL ;
      free(value) ;

      --value_count ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Unset everything -- keep tidy between tests.
 */
static void
test_avl_unset_all(void)
{
  uint i ;

  for (i = 0 ; i <= value_max ; ++i)
    test_avl_unset_value(values[i]) ;

  assert(value_count == 0) ;

  value_max   = 0 ;
  value_visit = 0 ;
} ;

/*------------------------------------------------------------------------------
 * The comparison function -- avl_cmp_func
 */
static int
test_avl_cmp(avl_key_c name, avl_value value)
{
  return symbol_mixed_name_cmp(name, ((const struct test_value*)value)->name) ;
} ;

/*------------------------------------------------------------------------------
 * The value create function -- avl_new_func
 *
 * Creates a skeleton value -- enough for insertion in the tree.
 *
 * Updates the value_count.
 */
static avl_value
test_avl_new(avl_key_c name)
{
  struct test_value* value ;

  value = calloc(1, offsetof(test_value_t, name) + strlen(name) + 1) ;
  strcpy(value->name, name) ;

  return value ;
} ;

/*------------------------------------------------------------------------------
 * Test that can construct and destroy a tree.
 *
 * Test that can search an empty tree, and add a single item and find it
 * again.
 *
 * Test the combinations of lookup/lookup_add, before and after the item
 * is added.
 */
static void
test_avl_tree_new(void)
{
  avl_tree tree = NULL;
  test_name_t name ;
  struct test_value* value ;
  struct test_value* value2 ;
  bool  add ;

  printf("%s\n", __func__);
  tree = avl_tree_init_new(NULL, test_avl_new, test_avl_cmp,
                                                  offsetof(test_value_t, avl)) ;
  assert_true(tree != NULL, "tree == NULL");

  /* expect to not find                                 */
  value = avl_lookup(tree, name);
  assert_true(value == NULL, "value != NULL");

  /* add                                                */
  test_avl_set_name(name, 77) ;
  add = false ;
  value = avl_lookup_add(tree, name, &add);
  assert_true(value != NULL, "value == NULL");
  assert_true(add, "add == false") ;

  test_avl_set_value(value, 77) ;

  assert_true(avl_tree_node_count(tree) == 1, "node count != 1") ;
  assert(value_count == 1) ;

  /* find                                               */
  value2 = avl_lookup(tree, name);
  assert_true(value2 == value, "value2 != value") ;

  add = true ;
  value2 = avl_lookup_add(tree, name, &add) ;
  assert_true(value2 == value, "value2 != value") ;
  assert_true(!add, "add == true") ;

  /* delete                                             */
  value2 = avl_delete(tree, name) ;
  assert_true(value2 == value, "value2 != value") ;
  assert_true(avl_tree_node_count(tree) == 0, "node count != 0") ;

  test_avl_unset_value(value) ;
  assert(value_count == 0) ;

  /* delete and don't expect to find                    */
  value2 = avl_delete(tree, name) ;
  assert_true(value2 == NULL, "found non-existent value") ;

  /* tidy up and finish                                 */
  test_avl_unset_all() ;

  tree = avl_tree_reset(tree, free_it) ;
  assert_true(tree == NULL, "tree not freed") ;
} ;

#if 0
static int
test_symbol_sort(const symbol* a, const symbol* b)
{
  return symbol_mixed_name_cmp(
                 ((struct test_value*)symbol_get_value(*a))->name,
                 ((struct test_value*)symbol_get_value(*b))->name ) ;
} ;
#endif








/*------------------------------------------------------------------------------
 * Test that can construct a large tree and find things again.
 *
 * Does no deletions.
 */
static void
test_avl_tree_lookup(const uint len, const order_t how, const uint seed)
{
  avl_tree tree ;
  test_name_t name ;
  uint i ;

  test_value value ;

  const bool trace = false ;

  char* desc ;

  /* Set up the test and construct empty tree.                          */

  desc = test_avl_set_order(len, how, seed) ;
  printf("%s %s\n", __func__, desc) ;
  free(desc) ;

  tree = avl_tree_init_new(NULL, test_avl_new, test_avl_cmp,
                                                  offsetof(test_value_t, avl)) ;

  /* add                                                                */
  for (i = 0; i < len; ++i)
    {
      bool add ;
      uint v ;

      v = order[i] ;

      add = false ;
      value = avl_lookup_add(tree, test_avl_set_name(name, v), &add) ;
      assert_true(value != NULL, "add: value == NULL");
      assert_true(add, "add: not added") ;

      test_avl_set_value(value, v) ;

      assert_true(avl_tree_node_count(tree) == (i + 1), "node count != i + 1") ;
      assert_true(avl_tree_node_count(tree) == value_count,
                                                 "node count != value_count") ;
      if (trace)
        {
          if (i == 0)
            printf("\n") ;
          printf("Step %2d: insert %03d\n", i+1, v) ;
          show_tree(tree) ;
          printf("\n") ;
        } ;
    }

  /* try walking the entire tree -- in order                            */
  ++value_visit ;               /* new walk     */
  i = 0;
  value = avl_tree_link(tree, avl_in_order) ;
  while (value != NULL)
    {
      assert_true(value->visit != value_visit, "value seen already") ;
      assert_true(value->val == i, "value != i") ;

      value->visit = value_visit ;
      ++i;

      value = avl_get_next_value(tree, value) ;
    } ;
  assert_true(i == len, "i != len");

  /* try walking the entire tree -- depth first                         */
  ++value_visit ;               /* new walk     */
  i = 0;
  value = avl_tree_link(tree, avl_depth_first) ;
  while (value != NULL)
    {
      test_value child ;

      assert_true(value->visit != value_visit, "value seen already") ;
      value->visit = value_visit ;
      ++i;

      child = avl_get_child_value(tree, value, avl_left) ;
      if (child != NULL)
        assert_true(child->visit == value_visit, "left child not seen") ;

      child = avl_get_child_value(tree, value, avl_left) ;
      if (child != NULL)
        assert_true(child->visit == value_visit, "right child not seen") ;

      value = avl_get_next_value(tree, value) ;
    } ;
  assert_true(i == len, "i != len");

  /* See what we got                                                    */
  scan_avl_tree(tree, true) ;

  /* Tidy up                                                            */
  test_avl_unset_all() ;

  tree = avl_tree_reset(tree, free_it) ;
  assert_true(tree == NULL, "tree not freed") ;
} ;

/*------------------------------------------------------------------------------
 * Test that can construct a large tree, find things, delete things, insert
 * things and still find them... etc.
 */
static void
test_avl_tree_delete(const uint len, const order_t how, const uint seed)
{
  avl_tree tree ;
  test_name_t name ;
  uint i, q ;

  test_value value ;

  bool trace = false ;

  char* desc ;

  /* Set up the test and construct empty tree.                          */

  desc = test_avl_set_order(len, how, seed) ;
  printf("%s %s\n", __func__, desc) ;
  free(desc) ;

  tree = avl_tree_init_new(NULL, test_avl_new, test_avl_cmp,
                                                  offsetof(test_value_t, avl)) ;

  /* Fill tree for the first time                                       */
  for (i = 0; i < len; ++i)
    {
      bool add ;
      uint v ;

      v = order[i] ;

      add = false ;
      value = avl_lookup_add(tree, test_avl_set_name(name, v), &add) ;
      assert_true(value != NULL, "add: value == NULL");
      assert_true(add, "add: not added") ;

      test_avl_set_value(value, v) ;

      assert_true(avl_tree_node_count(tree) == (i + 1), "node count != i + 1") ;
      assert_true(avl_tree_node_count(tree) == value_count,
                                                 "node count != value_count") ;
      if (trace)
        {
          if (i == 0)
            printf("\n") ;
          printf("Step %2d: insert %03d\n", i+1, v) ;
          show_tree(tree) ;
          printf("\n") ;
        } ;
    } ;

  scan_avl_tree(tree, true) ;

  /* Now delete 25%, 50% 75% and add back in again.
   */
  for (q = 1 ; q < 4 ; ++q)
    {
      uint n = (len * q) / 4 ;
      uint c = len ;

      shuffle(order, len, 0) ;

      for (i = 0; i < n ; ++i)
        {
          uint v ;

          v = order[i] ;

          value = avl_delete(tree, test_avl_set_name(name, v)) ;
          assert_true(value != NULL, "delete: value == NULL");

          test_avl_unset_value(value) ;

          --c ;

          assert_true(avl_tree_node_count(tree) == c, "node count != c") ;
          assert_true(avl_tree_node_count(tree) == value_count,
                                                  "node count != value_count") ;
          if (trace)
            {
              if (i == 0)
                printf("\n") ;
              printf("Step %2d: delete %03d\n", i+1, v) ;
              show_tree(tree) ;
              printf("\n") ;
            } ;
        } ;

      scan_avl_tree(tree, true) ;

      shuffle(order, n, 0) ;

      for (i = 0; i < n ; ++i)
        {
          uint v ;
          bool add ;

          v = order[i] ;

          add = false ;
          value = avl_lookup_add(tree, test_avl_set_name(name, v), &add) ;
          assert_true(value != NULL, "add: value == NULL");
          assert_true(add, "add: not added") ;

          test_avl_set_value(value, v) ;
          ++c ;

          assert_true(avl_tree_node_count(tree) == c, "node count != c") ;
          assert_true(avl_tree_node_count(tree) == value_count,
                                                  "node count != value_count") ;
          if (trace)
            {
              if (i == 0)
                printf("\n") ;
              printf("Step %2d: insert %03d\n", i+1, v) ;
              show_tree(tree) ;
              printf("\n") ;
            } ;
        } ;
    } ;

  /* Now delete everything.
   */
  shuffle(order, len, 0) ;

  for (i = 0; i < len ; ++i)
    {
      uint v ;

      v = order[i] ;

      value = avl_delete(tree, test_avl_set_name(name, v)) ;
      assert_true(value != NULL, "delete: value == NULL");

      test_avl_unset_value(value) ;

      assert_true(avl_tree_node_count(tree) == (len - i - 1),
                                            "node count != (len - i - 1)") ;
      assert_true(avl_tree_node_count(tree) == value_count,
                                              "node count != value_count") ;
      if (trace)
        {
          if (i == 0)
            printf("\n") ;
          printf("Step %2d: delete %03d\n", i+1, v) ;
          show_tree(tree) ;
          printf("\n") ;
        } ;
    } ;

  scan_avl_tree(tree, true) ;

  /* Tidy up                                                            */
  test_avl_unset_all() ;

  tree = avl_tree_reset(tree, free_it) ;
  assert_true(tree == NULL, "tree not freed") ;
} ;

/*==============================================================================
 * Scanning AVL tree and showing properties.
 */
static void show_histogram(uint count[], uint max, uint n, uint t) ;
static char show_bal_char(int bal) ;

/*------------------------------------------------------------------------------
 * Scan AVL tree to...
 */
static void
scan_avl_tree(avl_tree tree, bool compare)
{
  uint   n = 24 ;               /* this is still nuts   */
  uint   depth[n + 2] ;
  uint   i ;
  uint   d ;
  uint   t ;
  uint   max_d ;
  uint   tpl ;
  uint   h ;

  test_value value ;

  /* Get number of nodes and height and report same.
   *
   * Under qdebug, getting the height also checks the node balance.
   */
  t = avl_tree_node_count(tree) ;

  printf("AVL Tree %'d entries:", t) ;

  /* Do a depth first walk to establish the depth of each and every node,
   * and report on the distribution etc. of node depths.
   */
  for (d = 0 ; d < (n + 2) ; ++d)
    depth[d] = 0 ;

  i = 0 ;
  max_d = 0 ;
  tpl = 0 ;
  value = avl_tree_link(tree, avl_depth_first) ;
  while (value != NULL)
    {
      d = avl_get_level(tree, value) + 1 ;

      if (d <= n)
        ++depth[d] ;
      else
        ++depth[n+1] ;

      if (d > max_d)
        max_d = d ;

      tpl += d ;

      ++i ;

      value = avl_get_next_value(tree, value) ;
    } ;

  h = avl_get_height(tree) ;

  assert_true(i == t, "i != tree_node_count");
  assert_true(max_d == h, "max depth and heigh mismatch") ;

  printf(" max depth: %d  av. path length %3.1f\n", max_d,
                             tpl != 0 ? (double)tpl / (double)t : (double)tpl) ;

  assert_true(max_d <= n, "maximum depth is BROKEN\n") ;

  if (t != 0)
    show_histogram(depth, max_d, n, t) ;

  /* For comparison, show distribution of a perfect tree -- if required.
   */
  if (compare && (t != 0))
    {
      for (d = 0 ; d < (n + 2) ; ++d)
        depth[d] = 0 ;

      i     = t ;
      d     = 1 ;
      max_d = 1 ;
      tpl   = 0 ;
      while (i != 0)
        {
          if (i < d)
            d = i ;

          depth[max_d] = d ;
          tpl += max_d * d ;

          i -= d ;

          if (i != 0)
            {
              ++max_d ;
              d += d ;
            } ;
        } ;

      printf("Perfect balance max depth: %d  av. path length %3.1f\n", max_d,
                                                      (double)tpl / (double)t) ;

      show_histogram(depth, max_d, n, t) ;
    } ;

  /* If small enough -- show the entire tree
   */
  if ((t != 0) && (t < 100))
    show_tree(tree) ;
} ;

static void
show_histogram(uint count[], uint max, uint n, uint t)
{
  uint   i ;
  uint   m ;
  uint   c ;
  uint   h = 30 ;

  m = 0 ;
  for (i = 0 ; i < (n + 2) ; ++i)
    if (count[i] > m)
      m = count[i] ;

  c = 0 ;
  for (i = 1 ; i < (n + 2) ; ++i)
    {
      uint j ;
      uint s ;

      if (i > max)
        break ;

      if (i <= n)
        printf("   %2d: ", i) ;
      else
        printf("  >%2d: ", n) ;

      s = (count[i] * h) / m ;
      for (j = 0 ; j <= h ; ++j)
        if (j < s)
          printf("=") ;
        else
          printf(" ") ;

      j  = count[i] ;
      c += count[i] ;
      printf(" %6d %4.1f%%  %6d %5.1f%% :%2d\n",
                                            j, ((double)j * 100.0)/(double)t,
                                            c, ((double)c * 100.0)/(double)t,
                                                                           i) ;
    } ;
} ;

static void
show_tree(avl_tree tree)
{
  uint level ;
  uint i ;
  uint pos ;

  char* buf ;
  uint  bp ;
  uint  bl ;

  struct test_value* value ;

  /* Walk to establish the node widths  */

  i = 0 ;
  value = avl_tree_link(tree, avl_in_order) ;
  while (value != NULL)
    {
      value->pos  = i ;
      ++i ;
      value = avl_get_next_value(tree, value) ;
    } ;
  assert_true(i == tree->node_count, "i != tree_node_count");

  bl  = 200 ;
  buf = malloc(bl) ;

  i = 0 ;
  pos = 0 ;
  level = UINT_MAX ;
  bp = 0 ;
  value = avl_tree_link(tree, avl_breadth_first) ;
  while (value != NULL)
    {
      uint old_level ;
      uint tpos ;
      struct test_value* child ;

      ++i ;

      tpos = value->pos * 2 + 1 ;

      old_level = level ;
      level = avl_get_level(tree, value) ;

      if (level != old_level)
        {
          if (level == 0)
            {
              qassert((pos == 0) && (bp == 0)) ;
              printf("  :") ;
              while (pos < (tpos + 1))
                {
                  printf(" ") ;
                  ++pos ;
                } ;

              buf[bp++] = show_bal_char(avl_get_balance(tree, value)) ;
            }
          else
            printf("\n  :") ;

          buf[bp] = '\0' ;
          printf("%s\n%2d:", buf, level) ;

          bp  = 0 ;
          pos = 0 ;
        } ;

      child = avl_get_child_value(tree, value, avl_left) ;
      if (child != NULL)
        {
          uint cpos ;

          cpos = (child->pos * 2) + 2 ;
          qassert(cpos < tpos) ;

          if ((cpos + 2 + 1) > bl)
            {
              bl *= 2 ;
              buf = realloc(buf, bl) ;
            } ;

          qassert(bp <= cpos) ;
          while (bp < cpos)
            buf[bp++] = ' ' ;

          buf[bp++] = show_bal_char(avl_get_balance(tree, child)) ;
          buf[bp++] = '/' ;
          cpos += 2 ;

          if (cpos < tpos)
            {
              qassert(pos < tpos) ;
              while (pos < cpos)
                {
                  printf(" ") ;
                  ++pos ;
                } ;
              while (pos < tpos)
                {
                  printf("_") ;
                  ++pos ;
                } ;
            } ;
        } ;

      qassert(pos <= tpos) ;
      while (pos < tpos)
        {
          printf(" ") ;
          ++pos ;
        } ;

      printf("%03d", value->val) ;
      pos += 3 ;

      child = avl_get_child_value(tree, value, avl_right) ;
      if (child != NULL)
        {
          uint cpos ;

          cpos = child->pos * 2 + 1 ;

          if ((cpos + 3 + 1) > bl)
            {
              bl *= 2 ;
              buf = realloc(buf, bl) ;
            } ;

          qassert(bp <= cpos) ;
          while (bp < cpos)
            buf[bp++] = ' ' ;

          buf[bp++] = '\\' ;
          buf[bp++] = show_bal_char(avl_get_balance(tree, child)) ;

          while (cpos > pos)
            {
              printf("_") ;
              ++pos ;
            } ;
        } ;

      value = avl_get_next_value(tree, value) ;
    } ;

  printf("\n") ;

  assert_true(i == tree->node_count, "i != tree_node_count");
} ;


static char
show_bal_char(int bal)
{
  switch(bal)
    {
      case -1:
        return '-' ;

      case  0:
        return '=' ;

      case +1:
        return '+' ;

      default:
        assert_true(false, "invalid balance") ;
        return '*' ;
    } ;
} ;

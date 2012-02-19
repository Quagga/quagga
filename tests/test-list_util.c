#include <zebra.h>
#include "misc.h"
#include "qlib_init.h"
#include "command.h"
#include "list_util.h"
#include <string.h>

/* List util torture tests
 *
 */

/* prototypes */
int main(int argc, char **argv);

static void test_ssl(void);
static void test_sdl(void);
static void test_ddl(void);

#define test_assert(true, message) \
  do { if (!(true)) test_assert_fail(#true, message, __func__, __LINE__) ; \
  } while (0)

static void
test_assert_fail(const char* truth, const char* message, const char* func,
                                                                       int line)
{
  printf("*** %s %d: (%s) not true: %s\n", func, line, truth, message) ;

} ;

/*==============================================================================
 * The tests.
 */
int
main(int argc, char **argv)
{
  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  printf("Starting list_util tests -- %s\n",
#ifdef __GNUC__LIST_UTIL
      "GNUC version"
#else
      "not GNUC version"
#endif
      " -- v0.01 26-Feb-2010") ;

  srand(22) ;                   /* Consistent testing required          */

  test_ssl() ;
  test_sdl() ;
  test_ddl() ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Construct a majic mark from two addresses
 */
static unsigned majic(void* a, void* b)
{
  uintptr_t z = (uintptr_t)a ^ (uintptr_t)b ^ (uintptr_t)&majic ;
  return z ;
} ;

/*==============================================================================
 * Single Base, Single Link
 *
 *   ssl_push(base, item, next)    -- add at head of list
 *   ssl_del(base, item, next)     -- delete from list
 *   ssl_del_head(base, next)      -- delete head of list
 *   ssl_pop(dst, base, next)      -- pop head of list, if any
 *   ssl_head(base)                -- return head of list
 *   ssl_next(item, next)          -- step to next item, if any
 *
 * Cases to cover:
 *
 *   a) adding when list is empty
 *   b) adding when list is not empty
 *   c) deletion of first item and more than one item on list
 *   d) deletion of first item when only one item on the list
 *   e) deletion of arbitrary item (list implicitly not empty)
 *   f) deletion when item not on list and list not empty
 *   g) deletion when item not on list and list is empty
 *   h) deletion of NULL item and list not empty
 *   i) deletion of NULL item and list empty
 *   j) pop of head when list not empty
 *   k) pop of head when list contains one item
 *   l) pop of head when list empty
 *   m) deletion of head when list not empty
 *   n) deletion of head when contains one item
 *   o) deletion of head when list empty
 *   p) head when list not empty
 *   q) head when list is empty
 *   r) next when not last
 *   s) next when last
 */

typedef struct ssl_test* ssl_test ;
struct ssl_test
{
  ssl_test   next ;            /* pointer at start of structure        */

  unsigned    majic ;
  char        dummy ;

  ssl_test   other_next ;      /* pointer elsewhere in structure       */
} ;

struct ssl_test_parent
{
  unsigned    rubbish ;
  char        fred ;

  ssl_test   base ;

  int         z[5] ;
} ;

static struct ssl_test_parent  ssl_parent ;

static void
test_ssl(void)
{
  ssl_test base       = NULL ;

  ssl_test del        = NULL ;
  ssl_test other_del  = NULL ;
  ssl_test last       = NULL ;
  ssl_test first      = NULL ;

  struct ssl_test  dummy ;

  int n = 47 ;

  int i_del       =  7 ;        /* NB: neither of these may be 0 or 1   */
  int i_other_del = 37 ;

  ssl_test prev ;
  ssl_test this ;
  ssl_test other_this ;
  ssl_test take ;
  ssl_test temp ;

  int i ;
  int ret ;

  static struct ssl_test_parent* other = &ssl_parent ;

  memset(other, 77, sizeof(struct ssl_test_parent)) ;
  other->base = NULL ;

  /* Repeated insertion, starting from empty list
   *
   *   a) adding when list is empty
   *   b) adding when list is not empty
   *
   * Creates lists for following tests.
   */
  printf("=== Testing ssl -- Single Base, Single Link -- stuff\n") ;

  printf("  Creating list of items") ;
  for (i = 1 ; i <= n ; ++i)
    {
      ssl_test this = calloc(1, sizeof(struct ssl_test)) ;

      if (last == NULL)
        last = this ;

      this->majic = majic(this, &base) ;

      this->dummy = i ;

      ssl_push(base, this, next) ;
      if (i & 1)
        ssl_push(other->base, this, other_next) ;
      else
        ssl_push(ssl_parent.base, this, other_next) ;

      if (i == i_del)
        del = this ;
      if (i == i_other_del)
        other_del = this ;

      first = this ;

      printf("+") ;
    } ;

  test_assert((base == first) && (other->base == first),
                                          "Failed to create consistent lists") ;
  printf("\n") ;

  passert((del != base)       && (del       != last)) ;
  passert((other_del != base) && (other_del != last)) ;

  /* Walk to check that have the expected items
   *
   *   l) head when list not empty
   *   r) next when not last
   *   s) next when last
   */
  printf("  Walking list of items") ;

  this = ssl_head(base) ;
  test_assert(this == first, "ssl_head failed") ;

  this = ssl_head(other->base) ;
  test_assert(this == first, "ssl_head failed") ;

  this = ssl_head(ssl_parent.base) ;
  test_assert(this == first, "ssl_head failed") ;

  this = ssl_next(del, next) ;
  test_assert((this == del->next) && (this != NULL), "ssl_next failed") ;
  this = ssl_next(last, next) ;
  test_assert((this == last->next) && (this == NULL),
                                                    "ssl_next failed at end") ;

  this = ssl_next(other_del, other_next) ;
  test_assert((this == other_del->other_next) && (this != NULL),
                                                           "ssl_next failed") ;
  this = ssl_next(last, other_next) ;
  test_assert((this == last->other_next) && (this == NULL),
                                                    "ssl_next failed at end") ;

  this = base ;
  other_this = other->base ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      test_assert(this == other_this, "this and other_this not in step") ;
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;

      printf(".") ;
      this = ssl_next(this, next) ;
      other_this = ssl_next(other_this, other_next) ;
    } ;
  printf("\n") ;

  /* Deletion specifically at the start of the list
   *
   *   c) deletion of first item and more than one item on list
   */
  printf("  Deleting the first item") ;

  this = base ;
  first = base->next ;
  ret = ssl_del(base, this, next) ;
  test_assert(ret == true, "ssl_del did not return true") ;
  test_assert(first == base, "ssl_del of first item failed") ;

  this = other->base ;
  ret = ssl_del(other->base, this, other_next) ;
  test_assert(ret == true, "ssl_del did not return true") ;
  test_assert(first == other->base, "ssl_del of first item failed") ;

  printf("\n") ;

  --n ;         /* one less on the lists !      */

  /* Deletion of items from arbitrary place in list
   *
   *   e) deletion of arbitrary item (list implicitly not empty)
   */
  printf("  Deleting arbitrary items") ;
  ret = ssl_del(base, del, next) ;
  test_assert(ret == true, "ssl_del did not return true") ;
  ret = ssl_del(ssl_parent.base, other_del, other_next) ;
  test_assert(ret == true, "ssl_del did not return true") ;
  printf("\n") ;

  /* Deletion of items from arbitrary place in list
   *
   *   f) deletion when item not on list and list not empty
   */
  printf("  Deleting non-existant items") ;
  ret = ssl_del(base, &dummy, next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  ret = ssl_del(other->base, &dummy, other_next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  printf("\n") ;

  /* Deletion of NULL items
   *
   *   h) deletion of NULL item and list not empty
   */
  printf("  Deleting NULL items") ;

  this = NULL ;

  ret = ssl_del(base, this, next) ;
  test_assert(ret == false, "ssl_del did not return false") ;

  ret = ssl_del(ssl_parent.base, this, other_next) ;
  test_assert(ret == false, "ssl_del did not return false") ;

  printf("\n") ;

  /* Scan lists to check after deletion                                 */
  printf("    Base list scan") ;

  this = base ;
  prev = NULL ;
  i = n ;
  while (1)
    {
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      if (i == i_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;

      printf("*") ;
      this = ssl_next(this, next) ;
    } ;
  printf("\n") ;

  printf("    Other list scan") ;

  other_this = other->base ;
  prev = NULL ;
  i = n ;
  while (1)
    {
      if (other_this == NULL)
        break ;

      test_assert(other_this != prev, "this is same as prev") ;
      prev = other_this ;

      if (i == i_other_del)
        --i ;

      test_assert(other_this->dummy == i--, "don't have the right dummy") ;
      test_assert(other_this->majic == majic(other_this, &base),
                                            "don't have the right majic") ;

      printf("*") ;
      other_this = ssl_next(other_this, other_next) ;
    } ;
  printf("\n") ;

  /* Dismantle lists
   *
   *   j) pop of head when list not empty
   *   k) pop of head when list contains one item
   *   l) pop of head when list empty
   *   p) head when list not empty
   *   q) head when list is empty
   */
  printf("  Popping the head until list is empty\n") ;
  printf("    Base list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = base ;
      test_assert(this == ssl_head(base), "this is not head !") ;

      temp = ssl_pop(&take, base, next) ;
      test_assert(this == take, "this is not same as deleted head !") ;
      test_assert(temp == take, "temp is not same as deleted head !") ;

      if (this == NULL)
        break ;

      test_assert(base == this->next, "ssl_pop broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      if (i == i_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(base == NULL, "Base list should be empty") ;

  this = ssl_head(base) ;
  test_assert(this == NULL, "ssl_head of empty list failed") ;

  printf("\n") ;

  printf("   Other list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = other->base ;
      test_assert(this == ssl_head(other->base), "this is not head !") ;

      if (i & 1)
        temp = ssl_pop(&take, other->base, other_next) ;
      else
        temp = ssl_pop(&take, ssl_parent.base, other_next) ;

      test_assert(this == take, "this is not same as deleted head !") ;
      test_assert(temp == take, "temp is not same as deleted head !") ;

      if (this == NULL)
        break ;

      test_assert(other->base == this->other_next, "ssl_pop broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      if (i == i_other_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(other->base == NULL, "Other list should be empty") ;

  this = ssl_head(other->base) ;
  test_assert(this == NULL, "ssl_head of empty list failed") ;

  printf("\n") ;

  /* Rebuild lists to do:
   *
   *   m) deletion of head when list not empty
   *   n) deletion of head when contains one item
   *   o) deletion of head when list empty
   */
  passert((base == NULL) && (other->base == NULL)) ;

  last  = NULL ;
  first = NULL ;
  prev  = NULL ;
  printf("  Building list of items again") ;
  for (i = 1 ; i <= n ; ++i)
    {
      ssl_test this = calloc(1, sizeof(struct ssl_test)) ;

      if (last == NULL)
        last = this ;

      this->majic = majic(this, &base) ;
      this->dummy = i ;

      ssl_push(base, this, next) ;
      if (i & 1)
        ssl_push(ssl_parent.base, this, other_next) ;
      else
        ssl_push(other->base, this, other_next) ;

      test_assert(this->next       == prev, "broken ssl_push") ;
      test_assert(this->other_next == prev, "broken ssl_push") ;

      test_assert(base       == this, "broken ssl_push") ;
      test_assert(other->base == this, "broken ssl_push") ;

      first = this ;
      prev  = this ;

      printf("+") ;
    } ;

  printf("\n") ;

  printf("  Deleting the head until list is empty\n") ;
  printf("    Base list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = base ;

      ssl_del_head(base, next) ;

      if (this == NULL)
        break ;

      test_assert(base == this->next, "ssl_del_head broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(base == NULL, "Base list should be empty") ;

  printf("\n") ;

  printf("   Other list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = other->base ;

      if (i & 1)
        ssl_del_head(ssl_parent.base, other_next) ;
      else
        ssl_del_head(other->base, other_next) ;

      if (this == NULL)
        break ;

      test_assert(other->base == this->other_next, "ssl_del_head broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(other->base == NULL, "Other list should be empty") ;

  printf("\n") ;

  /* Final few tests
   *
   *   d) deletion of first item when only one item on the list
   *   g) deletion when item not on list and list is empty
   *   i) deletion of NULL item and list empty
   */

  /* Deletion of items from arbitrary place in list                     */
  printf("  Miscellaneous") ;

  del->next = &dummy ;
  ssl_push(base, del, next) ;
  test_assert((base == del) && (del->next == NULL), "ssl_push failed ??") ;
  ssl_del(base, del, next) ;
  test_assert(base == NULL, "ssl_del of first and only item failed") ;

  other_del->other_next = &dummy ;
  ssl_push(other->base, other_del, other_next) ;
  test_assert((other->base == other_del) && (other_del->other_next == NULL),
                                                         "ssl_push failed ??") ;
  ssl_del(other->base, other_del, other_next) ;
  test_assert(other->base == NULL, "ssl_del of first and only item failed") ;

  ret = ssl_del(base, del, next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  test_assert(base == NULL, "ssl_del on empty list") ;

  ret = ssl_del(other->base, other_del, other_next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  test_assert(other->base == NULL, "ssl_del on empty list") ;

  this = NULL ;

  ret = ssl_del(base, this, next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  test_assert(base == NULL, "ssl_del on empty list") ;

  ret = ssl_del(other->base, this, other_next) ;
  test_assert(ret == false, "ssl_del did not return false") ;
  test_assert(other->base == NULL, "ssl_del on empty list") ;

  printf("\n") ;

} ;

/*==============================================================================
 * Single Base, Double Link
 *
 *   sdl_push(base, item, list)     -- add at head of list
 *   sdl_del(base, item, list)      -- delete from list
 *   sdl_pop(&dst, base, next)      -- pop head of list, if any
 *   sdl_head(base)                 -- return head of list
 *   sdl_next(item, next)           -- step to next item, if any
 *   sdl_prev(item, next)           -- step to prev item, if any
 *
 * Cases to cover:
 *
 *   a) adding when list is empty
 *   b) adding when list is not empty
 *   c) deletion of first item and more than one item on list
 *   d) deletion of first item when only one item on the list
 *   e) deletion of arbitrary item (list implicitly not empty)
 *   f) deletion of NULL item and list not empty
 *   g) deletion of NULL item and list empty
 *   h) pop of head when list not empty
 *   i) pop of head when list contains one item
 *   j) pop of head when list empty
 *   k) deletion of head when list not empty
 *   l) deletion of head when list contains one item
 *   m) deletion of head when list empty
 *   n) head when list not empty
 *   o) head when list is empty
 *   p) next when not last
 *   q) next when last
 *   r) prev when not first
 *   s) prev when first
 *
 * NB: unlike single link stuff, cannot attempt to remove item which is
 *     not on the list !
 */

typedef struct sdl_test* sdl_test ;
struct sdl_test
{
  struct dl_list_pair(sdl_test)   list ;

  unsigned    majic ;
  char        dummy ;

  struct dl_list_pair(sdl_test)   other_list ;
};

struct sdl_test_parent
{
  long        rubbish ;
  char        fred ;

  sdl_test   base ;

  int         z[7] ;
} ;

static struct sdl_test_parent  sdl_parent ;

static void
test_sdl(void)
{
  sdl_test base       = NULL ;

  sdl_test del        = NULL ;
  sdl_test other_del  = NULL ;
  sdl_test last       = NULL ;
  sdl_test first      = NULL ;

  struct sdl_test  dummy ;

  int n = 57 ;

  int i_del       =  9 ;        /* NB: neither of these may be 0 or 1   */
  int i_other_del = 49 ;

  sdl_test prev ;
  sdl_test this ;
  sdl_test other_this ;
  sdl_test take ;
  sdl_test temp ;

  int i ;

  static struct sdl_test_parent* other = &sdl_parent ;

  memset(other, 99, sizeof(struct sdl_test_parent)) ;
  other->base = NULL ;

  /* Repeated insertion, starting from empty list
   *
   *   a) adding when list is empty
   *   b) adding when list is not empty
   *
   * Creates lists for following tests.
   */
  printf("=== Testing sdl -- Single Base, Double Link -- stuff\n") ;

  printf("  Creating list of items") ;
  for (i = 1 ; i <= n ; ++i)
    {
      sdl_test this = calloc(1, sizeof(struct sdl_test)) ;

      if (last == NULL)
        last = this ;

      this->majic = majic(this, &base) ;

      this->dummy = i ;

      sdl_push(base, this, list) ;
      if (i & 1)
        sdl_push(other->base, this, other_list) ;
      else
        sdl_push(sdl_parent.base, this, other_list) ;

      if (i == i_del)
        del = this ;
      if (i == i_other_del)
        other_del = this ;

      first = this ;

      printf("+") ;
    } ;

  test_assert((base == first) && (other->base == first),
                                          "Failed to create consistent lists") ;

  printf("\n") ;

  passert((del != base)       && (del       != last)) ;
  passert((other_del != base) && (other_del != last)) ;

  /* Walk to check that have the expected items
   *
   *   n) head when list not empty
   *   p) next when not last
   *   q) next when last
   *   r) prev when not first
   *   s) prev when first
   */
  printf("  Walking list of items") ;

  this = sdl_head(base) ;
  test_assert(this == first, "sdl_head failed") ;

  this = sdl_head(other->base) ;
  test_assert(this == first, "sdl_head failed") ;

  this = sdl_head(sdl_parent.base) ;
  test_assert(this == first, "sdl_head failed") ;

  /* next on both lists                                         */
  this = sdl_next(first, list) ;
  test_assert((this == first->list.next) && (this != NULL),
                                                  "sdl_next failed at start") ;
  this = sdl_next(del, list) ;
  test_assert((this == del->list.next) && (this != NULL), "sdl_next failed") ;
  this = sdl_next(last, list) ;
  test_assert((this == last->list.next) && (this == NULL),
                                                    "sdl_next failed at end") ;

  this = sdl_next(first, other_list) ;
  test_assert((this == first->other_list.next) && (this != NULL),
                                                  "sdl_next failed at start") ;
  this = sdl_next(other_del, other_list) ;
  test_assert((this == other_del->other_list.next) && (this != NULL),
                                                           "sdl_next failed") ;
  this = sdl_next(last, other_list) ;
  test_assert((this == last->other_list.next) && (this == NULL),
                                                    "sdl_next failed at end") ;

  /* prev on both lists                                         */
  this = sdl_prev(first, list) ;
  test_assert((this == first->list.prev) && (this == NULL),
                                                  "sdl_prev failed at start") ;
  this = sdl_prev(del, list) ;
  test_assert((this == del->list.prev) && (this != NULL), "sdl_prev failed") ;
  this = sdl_prev(last, list) ;
  test_assert((this == last->list.prev) && (this != NULL),
                                                    "sdl_prev failed at end") ;

  this = sdl_prev(first, other_list) ;
  test_assert((this == first->other_list.prev) && (this == NULL),
                                                  "sdl_prev failed at start") ;
  this = sdl_prev(other_del, other_list) ;
  test_assert((this == other_del->other_list.prev) && (this != NULL),
                                                           "sdl_prev failed") ;
  this = sdl_prev(last, other_list) ;
  test_assert((this == last->other_list.prev) && (this != NULL),
                                                    "sdl_prev failed at end") ;

  this = base ;
  other_this = other->base ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      test_assert(this == other_this, "this and other_this not in step") ;
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;

      printf(".") ;
      this = sdl_next(this, list) ;
      other_this = sdl_next(other_this, other_list) ;
    } ;
  printf("\n") ;

  /* Deletion specifically at the start of the list
   *
   *   c) deletion of first item and more than one item on list
   */
  printf("  Deleting the first item") ;

  this = base ;
  first = base->list.next ;
  sdl_del(base, this, list) ;
  test_assert(first == base, "sdl_del of first item failed") ;
  test_assert((base == NULL) || (base->list.prev == NULL), "sdl_del failed") ;

  this = other->base ;
  sdl_del(sdl_parent.base, this, other_list) ;
  test_assert(first == other->base, "sdl_del of first item failed") ;
  test_assert((base == NULL) || (base->other_list.prev == NULL),
                                                            "sdl_del failed") ;

  printf("\n") ;

  --n ;         /* one less on the lists !      */

  /* Deletion of items from arbitrary place in list
   *
   *   e) deletion of arbitrary item (list implicitly not empty)
   */
  printf("  Deleting arbitrary items") ;
  sdl_del(base, del, list) ;
  test_assert((base == NULL) || (base->list.prev == NULL), "sdl_del failed") ;
  sdl_del(sdl_parent.base, other_del, other_list) ;
  test_assert((base == NULL) || (base->other_list.prev == NULL),
                                                            "sdl_del failed") ;
  printf("\n") ;

  /* Deletion of NULL items
   *
   *   f) deletion of NULL item and list not empty
   */
  printf("  Deleting NULL items") ;
  this = NULL ;
  sdl_del(base, this, list) ;
  test_assert((base == NULL) || (base->list.prev == NULL), "sdl_del failed") ;
  sdl_del(other->base, this, other_list) ;
  test_assert((base == NULL) || (base->other_list.prev == NULL),
                                                            "sdl_del failed") ;
  printf("\n") ;

  /* Scan lists to check after deletion                                 */
  printf("    Base list scan") ;

  this = base ;
  prev = NULL ;
  i = n ;
  while (1)
    {
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      test_assert(this->list.prev == prev, "broken prev pointer") ;

      if (i == i_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("*") ;

      prev = this ;
      this = sdl_next(this, list) ;

      test_assert(this == prev->list.next, "broken sdl_next") ;
      if (this != NULL)
        test_assert(prev == sdl_prev(this, list), "broken sdl_prev") ;
    } ;
  printf("\n") ;

  printf("    Other list scan") ;

  this = other->base ;
  prev = NULL ;
  i = n ;
  while (1)
    {
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      test_assert(this->other_list.prev == prev, "broken prev pointer") ;

      if (i == i_other_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                            "don't have the right majic") ;

      printf("*") ;

      prev = this ;
      this = sdl_next(this, other_list) ;

      test_assert(this == prev->other_list.next, "broken sdl_next") ;
      if (this != NULL)
        test_assert(prev == sdl_prev(this, other_list), "broken sdl_prev") ;
    } ;
  printf("\n") ;

  /* Dismantle lists
   *
   *   h) pop of head when list not empty
   *   i) pop of head when list contains one item
   *   j) pop of head when list empty
   *   o) head when list is empty
   */
  printf("  Popping the head until list is empty\n") ;
  printf("    Base list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = sdl_head(base) ;
      test_assert(this == base, "broken sdl_head !") ;

      temp = sdl_pop(&take, base, list) ;
      test_assert(this == take, "take is not same as deleted head !") ;
      test_assert(this == temp, "temp is not same as deleted head !") ;
      if (base != NULL)
        test_assert(base->list.prev == NULL, "sdl_pop failed") ;

      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      if (i == i_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(base == NULL, "Base list should be empty") ;

  this = sdl_head(base) ;
  test_assert(this == NULL, "sdl_head of empty list failed") ;

  printf("\n") ;

  printf("   Other list") ;

   prev = NULL ;
  i = n ;
  while (1)
    {
      this = sdl_head(other->base) ;
      test_assert(this == other->base, "broken sdl_head !") ;

      if (i & 1)
        temp = sdl_pop(&take, sdl_parent.base, other_list) ;
      else
        temp = sdl_pop(&take, other->base, other_list) ;

      test_assert(this == take, "take is not same as deleted head !") ;
      test_assert(this == temp, "temp is not same as deleted head !") ;
      if (other->base != NULL)
        test_assert(other->base->other_list.prev == NULL,
                                                       "sdl_pop failed") ;
      if (this == NULL)
        break ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      if (i == i_other_del)
        --i ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;

      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(other->base == NULL, "Other list should be empty") ;

  this = sdl_head(other->base) ;
  test_assert(this == NULL, "sdl_head of empty list failed") ;

  printf("\n") ;

  /* Rebuild lists to do:
   *
   *   k) deletion of head when list not empty
   *   l) deletion of head when list contains one item
   *   m) deletion of head when list empty
   */
  passert((base == NULL) && (other->base == NULL)) ;

  last  = NULL ;
  first = NULL ;
  prev  = NULL ;
  printf("  Building list of items again") ;
  for (i = 1 ; i <= n ; ++i)
    {
      sdl_test this = calloc(1, sizeof(struct sdl_test)) ;

      if (last == NULL)
        last = this ;

      this->majic = majic(this, &base) ;
      this->dummy = i ;

      sdl_push(base, this, list) ;
      if (i & 1)
        sdl_push(sdl_parent.base, this, other_list) ;
      else
        sdl_push(other->base, this, other_list) ;

      test_assert(this->list.next       == prev, "broken sdl_push") ;
      test_assert(this->other_list.next == prev, "broken sdl_push") ;

      test_assert(base       == this, "broken sdl_push") ;
      test_assert(other->base == this, "broken sdl_push") ;

      first = this ;
      prev  = this ;

      printf("+") ;
    } ;

  printf("\n") ;

  printf("  Deleting the head until list is empty\n") ;
  printf("    Base list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = base ;

      sdl_del_head(base, list) ;

      if (this == NULL)
        break ;

      test_assert(base == this->list.next, "sdl_del_head broken") ;
      if (base != NULL)
        test_assert(base->list.prev == NULL, "sdl_del_head broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(base == NULL, "Base list should be empty") ;

  printf("\n") ;

  printf("   Other list") ;

  prev = NULL ;
  i = n ;
  while (1)
    {
      this = other->base ;

      if (i & 1)
        sdl_del_head(other->base, other_list) ;
      else
        sdl_del_head(sdl_parent.base, other_list) ;

      if (this == NULL)
        break ;

      test_assert(other->base == this->other_list.next, "sdl_del_head broken") ;
      if (other->base != NULL)
        test_assert(other->base->other_list.prev == NULL,
                                                       "sdl_del_head broken") ;

      test_assert(this != prev, "this is same as prev") ;
      prev = this ;

      test_assert(this->dummy == i--, "don't have the right dummy") ;
      test_assert(this->majic == majic(this, &base),
                                      "don't have the right majic") ;
      printf("-") ;
    } ;
  test_assert(i == 0, "not the expected final value of 'i'") ;
  test_assert(other->base == NULL, "Other list should be empty") ;

  printf("\n") ;

  /* Final few tests
   *
   *   d) deletion of first item when only one item on the list
   *   g) deletion of NULL item and list empty
   */

  /* Deletion of items from arbitrary place in list                     */
  printf("  Miscellaneous") ;

  del->list.next = &dummy ;
  sdl_push(base, del, list) ;
  test_assert((base == del) && (del->list.next == NULL), "sdl_push failed ??") ;
  sdl_del(base, del, list) ;
  test_assert(base == NULL, "sdl_del of first and only item failed") ;

  other_del->other_list.next = &dummy ;
  sdl_push(other->base, other_del, other_list) ;
  test_assert((other->base == other_del) && (other_del->other_list.next == NULL),
                                                         "sdl_push failed ??") ;
  sdl_del(other->base, other_del, other_list) ;
  test_assert(other->base == NULL, "sdl_del of first and only item failed") ;

  this = NULL ;
  sdl_del(base, this, list) ;
  test_assert(base == NULL, "sdl_del of NULL item with empty list") ;

  sdl_del(other->base, this, other_list) ;
  test_assert(other->base == NULL, "sdl_del of NULL item with empty list") ;

  printf("\n") ;
} ;

/*==============================================================================
 * Double Base, Double Link
 *
 *   ddl_init(base)                 -- initialise base
 *   ddl_push(base, item, list)     -- insert at head of list
 *   ddl_append(base, item, list)   -- insert at tail of list
 *   ddl_in_after(after, base, item, list)   -- insert after
 *   ddl_in_before(before, base, item, list) -- insert before
 *   ddl_pop(&dst, base, next)      -- pop head of list, if any
 *   ddl_crop(&dst, base, next)     -- crop tail of list, if any
 *   ddl_del(base, item, list)      -- delete from list
 *   ddl_del_head(base, next)       -- delete head of list
 *   ddl_del_tail(base, next)       -- delete tail of list
 *   ddl_head(base)                 -- return head of list
 *   ddl_tail(base)                 -- return tail of list
 *   ddl_next(item, next)           -- step to next item, if any
 *   ddl_prev(item, next)           -- step to prev item, if any
 *
 * Cases to cover:
 */

/* Testing runs two lists through struct ddt_item objects.                    */

enum list { a_list, b_list, list_count } ;

typedef struct ddt_item* ddt_item ;

struct ddt_list_pair dl_list_pair(ddt_item) ;  /* Example struct constructor */
struct ddt_base_pair dl_base_pair(ddt_item) ;

typedef struct dl_base_pair(ddt_item) ddt_base_pair_t ;
                                            /* Example typedef constructor  */
typedef struct dl_base_pair(ddt_item)* p_ddt_base_pair ;
                                            /* Example typedef constructor  */

typedef struct ddt_list_pair* ddt_list_pair ;
typedef struct ddt_base_pair* ddt_base_pair ;

typedef struct ddt_rank* ddt_rank ;

struct ddt_rank                 /* information for each list    */
{
  struct ddt_list_pair  list ;  /* the thing we are testing     */

  int         lister ;
  int         list_found ;
  unsigned    majic ;

  int         ordinal ;
};

struct ddt_item                 /* the test items               */
{
  struct ddt_rank  a ;

  char  a_rubbish[21] ;

  struct ddt_rank b ;

  char  b_rubbish[19] ;
} ;

/* The test list base pairs, and pointers to the actual bases, for use in
 * the verification code.
 */
static ddt_base_pair ddt_bases[list_count] ;

/* For some tests want to construct lists and know the first, last and
 * somewhere between items.
 */

enum where { first, middle, last, where_count } ;

struct ddt_test_list_items
{
  struct
  {
    ddt_item  where[where_count] ;
  } list[list_count] ;
} ;

/*------------------------------------------------------------------------------
 * The test list items -- keep track here also for use in verification.
 */
enum { ddt_max_items = 1000 } ;

static unsigned ddt_item_count = 0 ;
static unsigned ddt_item_alloc = 0 ;
static ddt_item ddt_items[ddt_max_items] ;

static inline ddt_item
ddt_new_item(void)
{
  ddt_item item ;

  assert(ddt_item_count <= ddt_item_alloc) ;

  if (ddt_item_count == ddt_item_alloc)
    {
      assert(ddt_item_alloc < ddt_max_items) ;
      ddt_items[ddt_item_alloc++] = malloc(sizeof(struct ddt_item)) ;
    } ;

  item = ddt_items[ddt_item_count++] ;

  memset(item, ddt_item_count & 0xFF, sizeof(struct ddt_item)) ;

  item->a.lister = 0 ;
  item->b.lister = 0 ;

  item->a.ordinal = 0 ;
  item->b.ordinal = 0 ;

  return item ;
} ;

/*------------------------------------------------------------------------------
 * Given an item and a list ordinal, return pointer to "rank" for item.
 */
static inline ddt_rank
ddt_get_rank(ddt_item item, enum list l)
{
  if (item == NULL)
    return NULL ;

  if (l == a_list)
    return &item->a ;
  if (l == b_list)
    return &item->b ;

  assert(0) ;
} ;

/*------------------------------------------------------------------------------
 * Keep track of what should be found on the lists, and majic marker to check
 * that don't get lost and point into space.
 */
static inline unsigned
ddt_get_majic(ddt_item item, enum list l)
{
  return majic(item, ddt_bases[l]) ;
} ;

static void
ddt_test_list_add(ddt_item item, enum list l)
{
  ddt_rank rank = ddt_get_rank(item, l) ;

  test_assert(rank->lister == 0, "already on list") ;

  rank->lister  = 1 ;
  rank->majic   = ddt_get_majic(item, l) ;
  rank->ordinal = 0 ;   /* unknown ordinal      */
} ;

static void
ddt_test_list_del(ddt_item item, enum list l)
{
  ddt_rank rank ;

  if (item == NULL)
    return ;

  rank = ddt_get_rank(item, l) ;

  test_assert(rank->lister == 1, "not on list") ;

  rank->lister  = 0 ;
  rank->majic   = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Verification code.
 *
 * Blunt instrument to check that all known lists are valid.  Checks:
 *
 *   * bases are both NULL together, or both not NULL.
 *
 *   * first and last items on the list have suitable prev/next pointers.
 *
 *   * walk list to confirm, for each item:
 *
 *      -- prev pointer valid for each item
 *      -- item majic is correct (so not pointing somewhere invalid)
 *      -- item is supposed to be on the list
 *      -- item has not already been seen on list (list bent)
 *      -- ordinal, if not zero, is bigger than any previous non-zero ordinal
 *
 *   * last item visited on walk is what the tail points to
 *
 *   * for any items which are supposed to be on list, but were not found
 */
static void
ddt_verify_lists(void)
{
  ddt_base_pair base ;
  ddt_rank r ;
  ddt_item this ;
  ddt_item prev ;
  int l ;
  int i ;

  /* Wash the found flags                                       */
  for (l = 0 ; l < list_count ; ++l)
    for (i = 0 ; i < (int)ddt_item_count ; ++i)
      ddt_get_rank(ddt_items[i], l)->list_found = 0 ;

  /* Walk the lists                                             */
  for (l = 0 ; l < list_count ; ++l)
    {
      int ordinal = 0 ;

      base = ddt_bases[l] ;
      if (base == NULL)
        continue ;

      if ((base->head == NULL) || (base->tail == NULL))
        test_assert(base->head == base->tail, "broken list bases") ;
      else
        {
          r = ddt_get_rank(base->head, l) ;
          test_assert(r->list.prev == NULL, "broken list first item->prev") ;
          r = ddt_get_rank(base->tail, l) ;
          test_assert(r->list.next == NULL, "broken list last item->next") ;

          this = base->head ;
          prev = NULL ;

          while (this != NULL)
            {
              r = ddt_get_rank(this, l) ;

              test_assert(r->list.prev == prev, "broken item->prev") ;

              test_assert(r->lister, "should not be on this list") ;

              test_assert(!r->list_found, "circular list") ;
              r->list_found = 1 ;

              if (r->ordinal != 0)
                {
                  test_assert(r->ordinal > ordinal, "list out of order") ;
                  ordinal = r->ordinal ;
                }

              test_assert(r->majic == ddt_get_majic(this, l),
                                                        "wrong sort of majic") ;
              prev = this ;
              this = r->list.next ;
            } ;

          test_assert(base->tail == prev, "broken tail pointer") ;
        } ;
    } ;

  /* Verify that did not miss anything should have found                */
  /* Wash the found flags                                       */
  for (l = 0 ; l < list_count ; ++l)
    for (i = 0 ; i < (int)ddt_item_count ; ++i)
      {
        r = ddt_get_rank(ddt_items[i], l) ;

        if (r->lister)
          test_assert(r->list_found, "should have found this on list") ;
      } ;
} ;

/*------------------------------------------------------------------------------
 * Reset the test list handling
 *
 */
static void
ddt_reset_lists(void)
{
  int l ;

  for (l = 0 ; l < list_count ; ++l)
    ddl_init(*(ddt_bases[l])) ;

  ddt_item_count = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Make lists with 'n' items each.
 *
 * If 'n' 0..3, makes lists with exactly that many items.
 *
 * Otherwise, list length has +/- 25% jitter.
 */
static void
ddt_test_make_lists(struct ddt_test_list_items* test, int n)
{
  ddt_base_pair base ;
  ddt_item      item ;
  ddt_rank      rank ;
  enum list l ;
  int t ;
  int m ;

  int req[list_count] ;
  int mid[list_count] ;

  /* Capture the requirements                                   */
  t = 0 ;
  m = 1 ;
  for (l = 0 ; l < list_count ; ++l)
    {
      m += m ;
      int j = (n + 1) / 4 ;

      if (n <= 3)
        req[l] = n ;
      else
        req[l] = n - j + (rand() % (j + j + 1)) ;

      mid[l] = req[l] / 2 ;

      t += req[l] ;

      test->list[l].where[first]  = NULL ;
      test->list[l].where[middle] = NULL ;
      test->list[l].where[last]   = NULL ;
    } ;

  ddt_reset_lists() ;

  /* Have t = total number of items still required
   *      m = 2^n -- where there are 'n' lists
   */
  while (t != 0)
    {
      int b ;
      int r ;
      r = (rand() % (m - 1)) + 1 ;  /* bit pattern, at least one set   */

      item = ddt_new_item() ;

      b = 1 ;
      for (l = 0 ; l < list_count ; ++l)
        {
          if ((req[l] != 0) && ((r & b) != 0))
            {
              --req[l] ;
              --t ;

              ddt_test_list_add(item, l) ;

              if (test->list[l].where[first] == NULL)
                test->list[l].where[first] = item ;

              if (mid[l]-- == 0)
                test->list[l].where[middle] = item ;

              test->list[l].where[last]     = item ;

              base = ddt_bases[l] ;
              rank = ddt_get_rank(item, l) ;

              if (base->head == NULL)
                {
                  base->head = item ;
                  base->tail = item ;
                  rank->list.next = NULL ;
                  rank->list.prev = NULL ;
                }
              else if (rand() & 1)
                {
                  rank->list.next = base->head ;
                  rank->list.prev = NULL ;
                  ddt_get_rank(base->head, l)->list.prev = item ;
                  base->head = item ;
                }
              else
                {
                  rank->list.next = NULL ;
                  rank->list.prev = base->tail ;
                  ddt_get_rank(base->tail, l)->list.next = item ;
                  base->tail = item ;
                }
            } ;
          b <<= 1 ;
        }
    } ;

  /* Number the items                                                   */
  for (l = 0 ; l < list_count ; ++l)
    {
      int o = 0 ;

      base = ddt_bases[l] ;

      item = base->head ;
      while (item != NULL)
        {
          rank = ddt_get_rank(item, l) ;
          rank->ordinal = ++o ;                 /* first is 1   */
          item = rank->list.next ;
        } ;
    } ;

  ddt_verify_lists() ;
} ;

/*------------------------------------------------------------------------------
 *   ddl_init(base)                 -- initialise base
 *   ddl_push(base, item, list)     -- insert at head of list
 *   ddl_append(base, item, list)   -- insert at tail of list
 *   ddl_in_after(after, base, item, list)   -- insert after
 *   ddl_in_before(before, base, item, list) -- insert before
 *   ddl_pop(&dst, base, next)      -- pop head of list, if any
 *   ddl_crop(&dst, base, next)     -- crop tail of list, if any
 *   ddl_del(base, item, list)      -- delete from list
 *   ddl_del_head(base, next)       -- delete head of list
 *   ddl_del_tail(base, next)       -- delete tail of list
 *   ddl_head(base)                 -- return head of list
 *   ddl_tail(base)                 -- return tail of list
 *   ddl_next(item, next)           -- step to next item, if any
 *   ddl_prev(item, next)           -- step to prev item, if any
 */

static struct ddt_parent
{
  char  zlxq[37] ;

  struct ddt_base_pair base ;

  char  qxlz[45] ;
} ddt_parent ;

static void
test_ddl(void)
{
  struct ddt_base_pair  a_base ;
  struct ddt_parent*    b ;

  struct ddt_test_list_items test ;
  int n ;
  int o ;

  int base_n = 23 ;
  int rand_n = 17 ;

  printf("=== Testing ddl -- Double Base, Double Link -- stuff\n") ;

  /* Initialise the test support                                        */
  ddt_bases[a_list] = &a_base ;
  ddt_bases[b_list] = &ddt_parent.base ;

  ddt_item_count = 0 ;
  ddt_item_alloc = 0 ;

  /* Initialise the list bases                                          */
  b = &ddt_parent ;
  memset(b, 42, sizeof(struct ddt_parent)) ;

  ddl_init(a_base) ;
  ddl_init(b->base) ;

  ddt_verify_lists() ;          /* start as mean to go on               */


  /* ddl_push(base, item, list)     -- insert at head of list
   *
   * Cases: (a) empty list
   *        (b) list with one item
   *        (c) list with multiple items
   */
  printf("  ddl_push test") ;
  ddt_reset_lists() ;

  n = base_n + (rand() % rand_n) ;
  while (n)
    {
      ddt_item item ;
      int r ;

      printf(".") ;

      item = ddt_new_item() ;
      r = (rand() % 3) + 1 ;

      if (r & 1)
        {
          ddl_push(a_base, item, a.list) ;
          test_assert(a_base.head == item, "ddl_push broken") ;
          ddt_test_list_add(item, a_list) ;
          item->a.ordinal = n ;
        } ;
      ddt_verify_lists() ;

      if (r & 2)
        {
          ddl_push(b->base, item, b.list) ;
          test_assert(b->base.head == item, "ddl_push broken") ;
          ddt_test_list_add(item, b_list) ;
          item->b.ordinal = n ;
        } ;
      ddt_verify_lists() ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_append(base, item, list)   -- insert at tail of list
   *
   * Cases: (a) empty list
   *        (b) list with one item
   *        (c) list with multiple items
   */
  printf("  ddl_append test") ;
  ddt_reset_lists() ;

  n = base_n + (rand() % rand_n) ;
  o = 0 ;
  while (n)
    {
      ddt_item item ;
      int r ;

      printf(".") ;
      ++o ;

      item = ddt_new_item() ;
      r = (rand() % 3) + 1 ;

      if (r & 1)
        {
          ddl_append(a_base, item, a.list) ;
          test_assert(a_base.tail == item, "ddl_append broken") ;
          ddt_test_list_add(item, a_list) ;
          item->a.ordinal = o ;
        } ;
      ddt_verify_lists() ;

      if (r & 2)
        {
          ddl_append(b->base, item, b.list) ;
          test_assert(b->base.tail == item, "ddl_append broken") ;
          ddt_test_list_add(item, b_list) ;
          item->b.ordinal = o ;
        } ;
      ddt_verify_lists() ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_in_after(after, base, item, list)   -- insert after
   *
   * Cases: (a) after first and only (so is also last)
   *        (b) after first when more than one
   *        (c) after last when more than one
   *        (d) after something between
   */
  printf("  ddl_in_after test") ;

  n = base_n + (rand() % rand_n) ;
  while (n)
    {
      ddt_item item ;
      ddt_item after ;

      printf(".") ;
      ddt_test_make_lists(&test, n) ;

      item = ddt_new_item() ;
      after = test.list[a_list].where[n % where_count] ;
      ddl_in_after(after, a_base, item, a.list) ;
      test_assert(after->a.list.next == item, "ddl_in_after broken") ;
      test_assert(item->a.list.prev == after, "ddl_in_after broken") ;
      ddt_test_list_add(item, a_list) ;
      ddt_verify_lists() ;

      item = ddt_new_item() ;
      after = test.list[b_list].where[n % where_count] ;
      ddl_in_after(after, b->base, item, b.list) ;
      test_assert(after->b.list.next == item, "ddl_in_after broken") ;
      test_assert(item->b.list.prev == after, "ddl_in_after broken") ;
      ddt_test_list_add(item, b_list) ;
      ddt_verify_lists() ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_in_before(before, base, item, list) -- insert before
   *
   * Cases: (a) before first and only (so is also last)
   *        (b) before first when more than one
   *        (c) before last when more than one
   *        (d) before something between
   */
  printf("  ddl_in_before test") ;

  n = base_n + (rand() % rand_n) ;
  while (n)
    {
      ddt_item item ;
      ddt_item before ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      item = ddt_new_item() ;
      before = test.list[a_list].where[n % where_count] ;
      ddl_in_before(before, a_base, item, a.list) ;
      test_assert(before->a.list.prev == item, "ddl_in_before broken") ;
      test_assert(item->a.list.next == before, "ddl_in_before broken") ;
      ddt_test_list_add(item, a_list) ;
      ddt_verify_lists() ;

      item = ddt_new_item() ;
      before = test.list[b_list].where[n % where_count] ;
      ddl_in_before(before, b->base, item, b.list) ;
      test_assert(before->b.list.prev == item, "ddl_in_before broken") ;
      test_assert(item->b.list.next == before, "ddl_in_before broken") ;
      ddt_test_list_add(item, b_list) ;
      ddt_verify_lists() ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_pop(&dst, base, next)      -- pop head of list, if any
   *
   * Cases: (a) list with more than one item
   *        (b) list with one item
   *        (c) empty list
   */
  printf("  ddl_pop test") ;

  n = base_n + (rand() % rand_n) ;
  while (n >= 0)
    {
      ddt_item item ;
      ddt_item temp ;
      ddt_item peek ;
      int      ordinal ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      ordinal = 0 ;
      while (1)
        {
          peek = a_base.head ;
          temp = ddl_pop(&item, a_base, a.list) ;
          test_assert(temp == item, "ddl_pop broken") ;
          test_assert(peek == item, "ddl_pop broken") ;

          ddt_test_list_del(item, a_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;

          ++ordinal ;
          test_assert(item->a.ordinal == ordinal, "ddl_pop not in order") ;
        } ;

      ordinal = 0 ;
      while (1)
        {
          peek = b->base.head ;
          temp = ddl_pop(&item, b->base, b.list) ;
          test_assert(temp == item, "ddl_pop broken") ;
          test_assert(peek == item, "ddl_pop broken") ;

          ddt_test_list_del(item, b_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;

          ++ordinal ;
          test_assert(item->b.ordinal == ordinal, "ddl_pop not in order") ;
        } ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_crop(&dst, base, next)     -- crop tail of list, if any
   *
   * Cases: (a) list with more than one item
   *        (b) list with one item
   *        (c) empty list
   */

  printf("  ddl_crop test") ;

  n = base_n + (rand() % rand_n) ;
  while (n >= 0)
    {
      ddt_item item ;
      ddt_item temp ;
      ddt_item peek ;
      int      ordinal ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      ordinal = 0 ;
      while (1)
        {
          peek = a_base.tail ;
          temp = ddl_crop(&item, a_base, a.list) ;
          test_assert(temp == item, "ddl_crop broken") ;
          test_assert(peek == item, "ddl_crop broken") ;

          ddt_test_list_del(item, a_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;

          if (ordinal == 0)
            ordinal = item->a.ordinal ;
          else
            {
              test_assert(ordinal > 0, "ordinal out of whack") ;
              --ordinal ;
              test_assert(item->a.ordinal == ordinal, "ddl_crop not in order") ;
            } ;
        } ;

      ordinal = 0 ;
      while (1)
        {
          peek = b->base.tail ;
          temp = ddl_crop(&item, b->base, b.list) ;
          test_assert(temp == item, "ddl_crop broken") ;
          test_assert(peek == item, "ddl_crop broken") ;

          ddt_test_list_del(item, b_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;

          if (ordinal == 0)
            ordinal = item->b.ordinal ;
          else
            {
              test_assert(ordinal > 0, "ordinal out of whack") ;
              --ordinal ;
              test_assert(item->b.ordinal == ordinal, "ddl_crop not in order") ;
            } ;
        } ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_del(base, item, list)      -- delete from list
   *
   * Cases: (a) first and only (so is also last)
   *        (b) first when more than one
   *        (c) last when more than one
   *        (d) something between
   */
  printf("  ddl_del test") ;

  n = base_n + (rand() % rand_n) ;
  while (n)
    {
      ddt_item item ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      item = test.list[a_list].where[n % where_count] ;
      ddl_del(a_base, item, a.list) ;
      ddt_test_list_del(item, a_list) ;
      ddt_verify_lists() ;

      item = test.list[b_list].where[n % where_count] ;
      ddl_del(b->base, item, b.list) ;
      ddt_test_list_del(item, b_list) ;
      ddt_verify_lists() ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_del_head(base, next)       -- delete head of list
   *
   * Cases: (a) list with more than one item
   *        (b) list with one item
   *        (c) empty list
   */
  printf("  ddl_del_head test") ;

  n = base_n + (rand() % rand_n) ;
  while (n >= 0)
    {
      ddt_item item ;
      ddt_item peek ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      while (1)
        {
          item = a_base.head ;
          peek = (item != NULL) ? item->a.list.next : NULL ;

          ddl_del_head(a_base, a.list) ;

          test_assert(a_base.head == peek, "ddl_del_head broken") ;

          ddt_test_list_del(item, a_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;
        } ;

      while (1)
        {
          item = b->base.head ;
          peek = (item != NULL) ? item->b.list.next : NULL ;

          ddl_del_head(b->base, b.list) ;

          test_assert(b->base.head == peek, "ddl_del_head broken") ;

          ddt_test_list_del(item, b_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;
        } ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_del_tail(base, next)       -- delete tail of list
   *
   * Cases: (a) list with more than one item
   *        (b) list with one item
   *        (c) empty list
   */
  printf("  ddl_del_tail test") ;

  n = base_n + (rand() % rand_n) ;
  while (n >= 0)
    {
      ddt_item item ;
      ddt_item peek ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      while (1)
        {
          item = a_base.tail ;
          peek = (item != NULL) ? item->a.list.prev : NULL ;

          ddl_del_tail(a_base, a.list) ;

          test_assert(a_base.tail == peek, "ddl_del_tail broken") ;

          ddt_test_list_del(item, a_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;
        } ;

      while (1)
        {
          item = b->base.tail ;
          peek = (item != NULL) ? item->b.list.prev : NULL ;

          ddl_del_tail(b->base, b.list) ;

          test_assert(b->base.tail == peek, "ddl_del_tail broken") ;

          ddt_test_list_del(item, b_list) ;
          ddt_verify_lists() ;

          if (item == NULL)
            break ;
        } ;

      --n ;
    } ;
  printf("\n") ;

  /* ddl_head(base)                 -- return head of list
   * ddl_tail(base)                 -- return tail of list
   *
   * Cases: (a) list with more than one item
   *        (b) list with one item
   *        (c) empty list
   */
  printf("  ddl_head & ddl_tail test") ;

  n = base_n + (rand() % rand_n) ;
  while (n >= 0)
    {
      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      test_assert(ddl_head(a_base)  == a_base.head,  "ddl_head broken") ;
      test_assert(ddl_tail(a_base)  == a_base.tail,  "ddl_head broken") ;
      test_assert(ddl_head(b->base) == b->base.head, "ddl_head broken") ;
      test_assert(ddl_tail(b->base) == b->base.tail, "ddl_head broken") ;

      --n ;
    } ;
  printf("\n") ;


  /* ddl_next(item, next)           -- step to next item, if any
   * ddl_prev(item, next)           -- step to prev item, if any
   *
   * Cases: (a) at first and only (so is also last)
   *        (b) at first when more than one
   *        (c) at last when more than one
   *        (d) at something between
   */
  printf("  ddl_next and ddl_prev test") ;

  n = base_n + (rand() % rand_n) ;
  while (n)
    {
      ddt_item item ;
      ddt_item where ;

      printf(".") ;

      ddt_test_make_lists(&test, n) ;

      where = test.list[a_list].where[n % where_count] ;
      item = ddl_next(where, a.list) ;
      test_assert(item == where->a.list.next, "ddl_next broken") ;

      where = test.list[b_list].where[n % where_count] ;
      item = ddl_next(where, b.list) ;
      test_assert(item == where->b.list.next, "ddl_next broken") ;

      where = test.list[a_list].where[n % where_count] ;
      item = ddl_prev(where, a.list) ;
      test_assert(item == where->a.list.prev, "ddl_prev broken") ;

      where = test.list[b_list].where[n % where_count] ;
      item = ddl_prev(where, b.list) ;
      test_assert(item == where->b.list.prev, "ddl_prev broken") ;

      --n ;
    } ;
  printf("\n") ;


}

/*
 *  TODO
 *
 */

#include <zebra.h>
#include "misc.h"
#include "qlib_init.h"
#include "command.h"
#include "symtab.h"

/* Symtab torture tests
 *
 */

struct test_value
{
  int   val ;

  bool  defined ;
  bool  seen ;

  char  name[] ;
} ;

/* prototypes */
void assert_true(int result, const char * message);
int main(int argc, char **argv);
void test_symbol_table_new(void);
void test_symbol_table_lookup(void);
void call_back_function_set(symbol sym, void* value);
void call_back_function_change(symbol sym, void* value);
void call_back_function_unset(symbol sym, void* value);
void test_call_back(void);
void test_ref(void);
void test_ref_heavy(void);

static void scan_symbol_table(symbol_table table) ;

void
assert_true(int result, const char * message)
{
  if (!result)
    {
      printf("Assert failed: %s\n", message);
    }
}

int
main(int argc, char **argv)
{
  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  test_symbol_table_new();
  test_symbol_table_lookup();
#if 0
  test_call_back();
  test_ref();
  test_ref_heavy();
#endif

  return 0;
}

static int
test_symbol_cmp(const void* val, const void* name)
{
  return strcmp(((const struct test_value*)val)->name, name) ;
} ;

static int value_count = 0 ;

static struct test_value*
test_symbol_make(const char* name, int val)
{
  struct test_value* value ;

  value = calloc(1, sizeof(struct test_value) + strlen(name) + 1) ;
  strcpy(value->name, name) ;

  value->val     = val ;
  value->defined = true ;
  value->seen    = false ;

  ++value_count ;

  return value ;
} ;

static void
test_symbol_free(void* val)
{
  assert_true(!((struct test_value*)val)->defined, "freeing defined value") ;
  free(val) ;

  --value_count ;
} ;

static const symbol_funcs_t test_symbol_funcs =
{
  .hash   = symbol_hash_string,
  .cmp    = test_symbol_cmp,
  .free   = test_symbol_free,
} ;


void
test_symbol_table_new(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  struct test_value* value ;
  symbol sym = NULL;
  symbol sym2 = NULL;

  printf("test_symbol_table_init_new\n");
  table = symbol_table_new(NULL, 0, 0, &test_symbol_funcs);
  assert_true(table != NULL, "table == NULL");

  /* expect to not find */
  sym = symbol_lookup(table, name, no_add);
  assert_true(sym == NULL, "sym != NULL");

  /* add */
  sym = symbol_lookup(table, name, add);
  assert_true(sym != NULL, "sym == NULL");
  assert_true(symbol_get_body(sym) == NULL, "sym->value != NULL") ;

  value = test_symbol_make(name, 777) ;
  symbol_set_body(sym, value, true /* set */, free_it /* existing */) ;

  /* find */
  sym2 = symbol_lookup(table, name, no_add);
  assert_true(sym == sym2, "sym != sym2");
  assert_true(symbol_get_body(sym) == value, "symbol_get_body(sym) != value");

  value->defined = false ;
  symbol_delete(sym, free_it);

  assert_true(value_count == 0, "value_count != 0") ;

  sym = NULL ;
  while ((sym = symbol_table_ream(table, sym, free_it)) != NULL)
    assert_true(sym == NULL, "table not empty") ;
} ;

static int
test_symbol_sort(const symbol* a, const symbol* b)
{
  return symbol_mixed_name_cmp(
                 ((struct test_value*)symbol_get_body(*a))->name,
                 ((struct test_value*)symbol_get_body(*b))->name ) ;
} ;

void
test_symbol_table_lookup(void)
{
  symbol_table table = NULL;
  char name[20];
  symbol sym = NULL;
  int i ;
  uint j ;
  struct test_value* value = NULL;
  const int len = 100000;
  struct symbol_walker itr;
  vector v = NULL;

  printf("test_symbol_table_lookup\n");
  table = symbol_table_new(NULL, 0, 200, &test_symbol_funcs);

  /* add */
  for (i = 0; i < len; ++i)
    {
      sprintf(name, "%d-name", i);
      sym = symbol_lookup(table, name, add);
      assert_true(sym != NULL, "add: sym == NULL");
      assert_true(symbol_get_body(sym) == NULL, "sym->body != NULL") ;

      value = test_symbol_make(name, i) ;
      symbol_set_body(sym, value, true /* set */, free_it /* existing */);
      assert_true(symbol_get_body(sym) == value,
                                            "symbol_get_body(sym) != value");
    }

  scan_symbol_table(table) ;

  /* find */
  for (i = 0; i < len; ++i)
    {
      sprintf(name, "%d-name", i);
      sym = symbol_lookup(table, name, no_add);
      assert_true(sym != NULL, "find: sym == NULL");
      value = symbol_get_body(sym) ;
      assert_true(value != NULL, "symbol_get_body(sym) == NULL");

      assert_true(strcmp(value->name, name) == 0,
                                             "strcmp(value->name, name) != 0");
      assert_true(value->val == i, "value->val != i");
    }

  /* walk with symbol_walker */
  symbol_walk_start(table, &itr);
  i = 0;
  while ((sym = symbol_walk_next(&itr)) != NULL)
    {
      value = symbol_get_body(sym) ;
      assert_true(!value->seen, "value seen already") ;
      value->seen = true ;
      ++i;
    } while (sym != NULL);
  assert_true(i == len, "i != len");

  /* extract vector */
  v = symbol_table_extract(table, NULL, NULL, 1, test_symbol_sort);
  assert_true(vector_end(v) == (unsigned)len, "vector_get_end(v) != len");

  i = 0 ;
  for (VECTOR_ITEMS(v, sym, j))
    {
      value = symbol_get_body(sym) ;
      assert_true(value->val == i, "value->val != i") ;
      ++i ;
    }
  assert_true(i == len, "i != len");

  vector_free(v);

  /* Ream out                                                   */
  sym = NULL ;
  i = 0 ;
  while ((sym = symbol_table_ream(table, sym, free_it)) != NULL)
    {
      value = symbol_get_body(sym) ;
      value->defined = false ;
      ++i ;
    } ;
  assert_true(i == len, "i != len");

  assert_true(value_count == 0, "value_count != 0") ;
}

#if 0
void
test_call_back(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  char value[] = "value";
  char new_value[] = "new value";
  symbol sym = NULL;

  printf("test_call_back\n");
  table = symbol_table_init_new(table, NULL, 0, 0, NULL, NULL);
  assert_true(table != NULL, "table == NULL");

  /* add */
  symbol_table_set_call_back(table, call_back_function_set);
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, value);

  /* change */
  symbol_table_set_call_back(table, call_back_function_change);
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, new_value);

  /* delete */
  symbol_table_set_call_back(table, call_back_function_unset);
  symbol_unset_value(sym);

  while ((symbol_table_ream(table, 1)) != NULL)
    {
    }
}

void call_back_function_set(symbol sym, void* value)
{
  assert_true(symbol_get_body(sym) != NULL && value == NULL,
      "symbol_get_body(sym) == NULL || value != NULL");
}

void call_back_function_change(symbol sym, void* value)
{
  assert_true(symbol_get_body(sym) != NULL && value != NULL,
      "symbol_get_body(sym) == NULL || value == NULL");
}


void call_back_function_unset(symbol sym, void* value)
{
  assert_true(symbol_get_body(sym) == NULL && value != NULL,
      "symbol_get_body(sym) != NULL || value == NULL");
}

void
test_ref(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  char value[] = "value";
  symbol sym = NULL;
  symbol_nref ref = NULL;
  symbol_nref ref1 = NULL;
  symbol_nref ref2 = NULL;
  struct symbol_nref walk;
  const int num_refs = 2;
  long int itag = 0;

  printf("test_ref\n");
  table = symbol_table_init_new(table, NULL, 0, 0, NULL, NULL);

  /* add */
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, value);

  /* create references, in reverse order so that walk in order */
  ref2 = symbol_set_ref(NULL, sym);
  assert_true(ref2 != NULL, "ref2 == NULL");
  sym_ref_set_i_tag(ref2, 2);
  assert_true(sym_ref_i_tag(ref2) == 2, "sym_ref_i_tag(ref2) != 2");

  ref1 = symbol_set_ref(NULL, sym);
  assert_true(ref1 != NULL, "ref1 == NULL");
  sym_ref_set_i_tag(ref1, 1);
  assert_true(sym_ref_i_tag(ref1) == 1, "sym_ref_i_tag(ref1) != 1");

  /* walk references */
  itag = 1;
  symbol_nref_walk_start(sym, &walk) ;
  assert_true(sym->ref_list == &walk, "sym->ref_list != &walk");
  assert_true(walk.next == ref1, "walk.next != ref1");
  assert_true(ref1->next == ref2, "ref1->next != ref2");
  assert_true(ref2->next == NULL, "ref2->next != NULL");

  while ((ref = symbol_nref_walk_step(&walk)) != NULL)
    {
      assert_true(sym_ref_i_tag(ref) == itag, "sym_ref_i_tag(ref) != itag");
      ++itag;
    }
  assert_true(itag == num_refs + 1, "itag != num_refs + 1");

  symbol_nref_walk_end(&walk);

  /* clean up */
  symbol_unset_ref(ref1, 1);
  symbol_unset_ref(ref2, 1);

  while ((symbol_table_ream(table, 1)) != NULL)
    {
    }
}

void
test_ref_heavy(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  char value[] = "value";
  symbol sym = NULL;
  symbol_nref ref = NULL;
  struct symbol_nref walk;
  const long int num_refs = 100000;
  long int itag = 0;

  printf("test_ref_heavy\n");
  table = symbol_table_init_new(table, NULL, 0, 0, NULL, NULL);

  /* add */
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, value);

  /* create references, in reverse order so that walk in order */
  for (itag = num_refs; itag > 0; --itag)
    {
      ref = symbol_set_ref(NULL, sym);
      assert_true(ref != NULL, "ref == NULL");
      sym_ref_set_i_tag(ref, itag);
      assert_true(sym_ref_i_tag(ref) == itag, "sym_ref_i_tag(ref) != itag");
    }

  /* walk references */
  itag = 1;
  symbol_nref_walk_start(sym, &walk) ;
  assert_true(sym->ref_list == &walk, "sym->ref_list != &walk");

  while ((ref = symbol_nref_walk_step(&walk)) != NULL)
    {
      assert_true(sym_ref_i_tag(ref) == itag, "sym_ref_i_tag(ref) != itag");
      ++itag;
      symbol_unset_ref(ref, 1);
    }
  assert_true(itag == num_refs + 1, "itag != num_refs + 1");

  symbol_nref_walk_end(&walk);

  while ((symbol_table_ream(table, 1)) != NULL)
    {
    }
}

#endif


/*==============================================================================
 * Scanning symbol table and showing properties.
 */
static void show_histogram(uint length[], uint n, uint t) ;

static void
scan_symbol_table(symbol_table table)
{
  uint   n = 10 ;           /* 0..10 and >10        */
  uint   length[n+2] ;
  uint   i ;
  uint*  comp ;

  for (i = 0 ; i < (n + 2) ; ++i)
    length[i] = 0 ;

  fprintf(stderr, "Symbol Table %'d entries: %'d bases and %'d extend_thresh"
                                                         " @ density %0.2f\n",
                   table->entry_count, table->base_count,
                   table->extend_thresh, table->density) ;

  for (i = 0 ; i < table->base_count ; ++i)
    {
      symbol sym ;
      uint   l ;

      l = 0 ;
      sym = table->bases[i] ;

      while (sym != NULL)
        {
          ++l ;
          sym = sym->next ;
        } ;

      if (l <= n)
        ++length[l] ;
      else
        ++length[n + 1] ;
    } ;

  show_histogram(length, n, table->entry_count) ;

  for (i = 0 ; i < (n + 2) ; ++i)
    length[i] = 0 ;

  fprintf(stderr, "  RAND_MAX == 0x%x\n", RAND_MAX) ;

  comp = calloc(table->base_count, sizeof(uint)) ;
  for (i = 0 ; i < table->entry_count ; ++i)
    {
      uint q = rand() % table->base_count ;
      ++comp[q] ;
    } ;

  for (i = 0 ; i < table->base_count ; ++i)
    {
      uint   l ;

      l = comp[i] ;

      if (l <= n)
        ++length[l] ;
      else
        ++length[n + 1] ;
    } ;

  show_histogram(length, n, table->entry_count) ;
} ;



static void
show_histogram(uint length[], uint n, uint t)
{
  uint   i ;
  uint   m ;
  uint   c ;

  m = 0 ;
  for (i = 0 ; i < (n + 2) ; ++i)
    if (length[i] > m)
      m = length[i] ;

  c = 0 ;
  for (i = 0 ; i < (n + 2) ; ++i)
    {
      uint j ;
      uint s ;

      if (i <= n)
        fprintf(stderr, "   %2d: ", i) ;
      else
        fprintf(stderr, "  >%2d: ", n) ;

      s = (length[i] * 50) / m ;
      for (j = 0 ; j < 51 ; ++j)
        if (j < s)
          fprintf(stderr, "=") ;
        else
          fprintf(stderr, " ") ;

      j = i * length[i] ;
      c += j ;
      fprintf(stderr, "%'6d   %6d  %4.1f%%  %5.1f%% :%2d\n", length[i], j,
                                               ((double)j * 100.0)/(double)t,
                                               ((double)c * 100.0)/(double)t,
                                                                           i) ;
    } ;
} ;

/*
 *
 * TODO
 *

symbol_table_set_parent
symbol_table_get_parent
symbol_hash_string
symbol_hash_bytes
symbol_table_set_call_back
symbol_table_free
symbol_unset_value
symbol_select_cmp
symbol_sort_cmp
symbol_table_extract
symbol_sort_cmp
symbol_get_name_len
symbol_get_table
symbol_zero_ref
symbol_dec_ref
symbol_init_ref
symbol_set_ref
symbol_unset_ref
symbol_unset_ref_free
symbol_unset_ref_keep
sym_ref_symbol
sym_ref_value
sym_ref_name
sym_ref_name_len
sym_ref_parent
sym_ref_p_tag
sym_ref_u_tag
sym_ref_i_tag
sym_ref_set_parent
sym_ref_set_p_tag
sym_ref_set_i_tag
 */

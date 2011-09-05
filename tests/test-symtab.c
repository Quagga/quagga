#include <zebra.h>
#include <symtab.h>

/* Symtab torture tests
 *
 */

/* prototypes */
void assert_true(int result, const char * message);
int main(int argc, char **argv);
void test_symbol_table_init_new(void);
void test_symbol_table_lookup(void);
void call_back_function_set(symbol sym, void* value);
void call_back_function_change(symbol sym, void* value);
void call_back_function_unset(symbol sym, void* value);
void test_call_back(void);
void test_ref(void);
void test_ref_heavy(void);

struct thread_master *master;

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

  test_symbol_table_init_new();
  test_symbol_table_lookup();
  test_call_back();
  test_ref();
  test_ref_heavy();

  return 0;
}

void
test_symbol_table_init_new(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  char value[] = "value";
  symbol sym = NULL;
  symbol sym2 = NULL;
  void * old_value = NULL;
  const void* get_name ;

  printf("test_symbol_table_init_new\n");
  table = symbol_table_init_new(table, NULL, 0, 0, NULL, NULL);
  assert_true(table != NULL, "table == NULL");

  /* expect to not find */
  sym = symbol_lookup(table, name, no_add);
  assert_true(sym == NULL, "sym != NULL");

  /* add */
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, value);
  assert_true(sym != NULL, "sym == NULL");
  get_name = symbol_get_name(sym) ;
  assert_true(strcmp(get_name, name) == 0,
                                     "strcmp(symbol_get_name(sym), name) != 0");

  /* find */
  sym2 = symbol_lookup(table, name, no_add);
  assert_true(sym == sym2, "sym != sym2");
  assert_true(symbol_get_value(sym) == value, "symbol_get_value(sym) != value");

  old_value = symbol_delete(sym);
  assert_true(value == old_value, "value != old_value");

  while ((old_value = symbol_table_ream(table, keep_it)) != NULL)
    {
    }

}

void
test_symbol_table_lookup(void)
{
  symbol_table table = NULL;
  char buf[20];
  symbol sym = NULL;
  int i;
  char *value = NULL;
  const int len = 100000;
  struct symbol_walker itr;
  vector v = NULL;

  printf("test_symbol_table_lookup\n");
  table = symbol_table_init_new(table, NULL, 0, 0, NULL, NULL);

  /* add */
  for (i = 0; i < len; ++i)
    {
      const void* get_name ;

      sprintf(buf, "%d-name", i);
      sym = symbol_lookup(table, buf, add);
      assert_true(sym != NULL, "add: sym == NULL");
      get_name = symbol_get_name(sym) ;
      assert_true(strcmp(get_name, buf) == 0,
                                    "strcmp(symbol_get_name(sym), buf) != 0");

      sprintf(buf, "%d-value", i);
      value = strdup(buf);
      symbol_set_value(sym, value);
      assert_true(symbol_get_value(sym) == value,
          "symbol_get_value(sym) != value");
    }

  /* find */
  for (i = 0; i < len; ++i)
    {
      const void* get_name ;

      sprintf(buf, "%d-name", i);
      sym = symbol_lookup(table, buf, no_add);
      assert_true(sym != NULL, "find: sym == NULL");
      get_name = symbol_get_name(sym) ;
      assert_true(strcmp(get_name, buf) == 0,
                                    "strcmp(symbol_get_name(sym), buf) != 0");

      sprintf(buf, "%d-value", i);
      assert_true(strcmp(symbol_get_value(sym), buf) == 0,
          "strcmp(symbol_get_value(sym), buf) != 0");
    }

  /* walk with symbol_walker */
  symbol_walk_start(table, &itr);
  i = 0;
  do
    {
      sym = symbol_walk_next(&itr);
      if (sym != NULL) {
        ++i;
      }
    } while (sym != NULL);
  assert_true(i == len, "i != len");

  /* extract vector */
  v = symbol_table_extract(table, NULL, NULL, 1, NULL);
  assert_true(vector_end(v) == (unsigned)len, "vector_get_end(v) != len");
  vector_free(v);

  while ((value = symbol_table_ream(table, 1)) != NULL)
    {
      free(value);
    }
}

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
  symbol_table_set_value_call_back(table, call_back_function_set);
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, value);

  /* change */
  symbol_table_set_value_call_back(table, call_back_function_change);
  sym = symbol_lookup(table, name, add);
  symbol_set_value(sym, new_value);

  /* delete */
  symbol_table_set_value_call_back(table, call_back_function_unset);
  symbol_unset_value(sym);

  while ((symbol_table_ream(table, 1)) != NULL)
    {
    }
}

void call_back_function_set(symbol sym, void* value)
{
  assert_true(symbol_get_value(sym) != NULL && value == NULL,
      "symbol_get_value(sym) == NULL || value != NULL");
}

void call_back_function_change(symbol sym, void* value)
{
  assert_true(symbol_get_value(sym) != NULL && value != NULL,
      "symbol_get_value(sym) == NULL || value == NULL");
}


void call_back_function_unset(symbol sym, void* value)
{
  assert_true(symbol_get_value(sym) == NULL && value != NULL,
      "symbol_get_value(sym) != NULL || value == NULL");
}

void
test_ref(void)
{
  symbol_table table = NULL;
  char name[] = "name";
  char value[] = "value";
  symbol sym = NULL;
  symbol_ref ref = NULL;
  symbol_ref ref1 = NULL;
  symbol_ref ref2 = NULL;
  struct symbol_ref walk;
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
  symbol_ref_walk_start(sym, &walk) ;
  assert_true(sym->ref_list == &walk, "sym->ref_list != &walk");
  assert_true(walk.next == ref1, "walk.next != ref1");
  assert_true(ref1->next == ref2, "ref1->next != ref2");
  assert_true(ref2->next == NULL, "ref2->next != NULL");

  while ((ref = symbol_ref_walk_step(&walk)) != NULL)
    {
      assert_true(sym_ref_i_tag(ref) == itag, "sym_ref_i_tag(ref) != itag");
      ++itag;
    }
  assert_true(itag == num_refs + 1, "itag != num_refs + 1");

  symbol_ref_walk_end(&walk);

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
  symbol_ref ref = NULL;
  struct symbol_ref walk;
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
  symbol_ref_walk_start(sym, &walk) ;
  assert_true(sym->ref_list == &walk, "sym->ref_list != &walk");

  while ((ref = symbol_ref_walk_step(&walk)) != NULL)
    {
      assert_true(sym_ref_i_tag(ref) == itag, "sym_ref_i_tag(ref) != itag");
      ++itag;
      symbol_unset_ref(ref, 1);
    }
  assert_true(itag == num_refs + 1, "itag != num_refs + 1");

  symbol_ref_walk_end(&walk);

  while ((symbol_table_ream(table, 1)) != NULL)
    {
    }
}


/*
 *
 * TODO
 *

symbol_table_set_parent
symbol_table_get_parent
symbol_hash_string
symbol_hash_bytes
symbol_table_set_value_call_back
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

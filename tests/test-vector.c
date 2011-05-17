#include <zebra.h>
#include <vector.h>

/* Vector torture tests
 *
 */

/* prototypes */
void assert_true(int result, const char * message);
int main(int argc, char **argv);
void test_vector_init(void);
void test_vector_set_index(void);
void test_vector_lookup(void);
void test_vector_lookup_ensure(void);
void test_vector_unset(void);
void test_vector_set_index_null(void);
void test_vector_copy(void);
void test_vector_set(void);
void test_vector_growth(void);

/* GMCH interface */
void test_vector_init_new(void);
void test_vector_set_item(void);
void test_vector_insert_item(void);
void test_vector_insert_item_here(void);
void test_vector_delete_item(void);
void do_test_insert(const int rider);
int sort_cmp(const void* const* a, const void* const* b);
void test_vector_sort(void);
void test_vector_bsearch(void);
void test_vector_move_item_here(void);
void do_test_move_item_here(const int rider, vector_index ins, vector_index src,
                                   char strings[][10], const vector_index len) ;
void test_vector_part_reverse(void);
void test_vector_copy_here(void);
void test_vector_move_here(void);
void test_vector_copy_append(void);
void test_vector_move_append(void);
void test_vector_insert(void);
void test_vector_delete(void);
void test_vector_discard(void);
void test_vector_sak(void);

struct thread_master *master;

/* objects to put in the vectors */
static char s0[5];
static char s1000[5];
static char s2000[5];

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
  strcpy(s0, "0");
  strcpy(s1000, "1000");
  strcpy(s2000, "2000");

  test_vector_init();
  test_vector_set_index();
  test_vector_lookup();
  test_vector_lookup_ensure();
  test_vector_unset();
  test_vector_set_index_null();
  test_vector_copy();
  test_vector_set();
  test_vector_growth();

  /* GMCH interface */
  test_vector_init_new();
  test_vector_set_item();
  test_vector_insert_item();
  test_vector_insert_item_here();
  test_vector_delete_item();
  test_vector_sort();
  test_vector_bsearch();
  test_vector_move_item_here();
  test_vector_part_reverse();
  test_vector_copy_here();
  test_vector_move_here();
  test_vector_copy_append();
  test_vector_move_append();
  test_vector_insert();
  test_vector_delete();
  test_vector_discard();
  test_vector_sak();

  return 0;
}

void
test_vector_init(void)
{
  vector v = NULL;

  printf("test_vector_init\n");
  v = vector_init(10);

  assert_true(v != NULL,
      "v == NULL");
  assert_true(vector_count(v) == 0,
      "vector_count != 0");
  assert_true(vector_active(v) == 0,
      "vector_active != 0");

  vector_free(v);
}

void
test_vector_set_index(void)
{
  vector v = NULL;

  printf("test_vector_set_index\n");
  v = vector_init(0);

  vector_set_index(v, 1000, s1000);
  assert_true(vector_count(v) == 1,
      "vector_count != 1");
  assert_true(vector_active(v) == 1001,
      "vector_active != 1001");
  assert_true(vector_slot(v,1000) == s1000,
      "vector_slot != 1000");

  vector_free(v);
}

void
test_vector_lookup(void)
{
  vector v = NULL;

  printf("test_vector_lookup\n");
  v = vector_init(0);

  vector_set_index(v, 1000, s1000);

  assert_true(vector_lookup(v,1000) == s1000,
      "vector_lookup != 1000");
  assert_true(vector_lookup(v,1001) == NULL,
      "vector_lookup != NULL");

  vector_free(v);
}

void
test_vector_lookup_ensure(void)
{
  vector v = NULL;

  printf("test_vector_lookup_ensure\n");
  v = vector_init(0);

  vector_set_index(v, 1000, s1000);

  assert_true(vector_lookup_ensure(v,1000) == s1000,
      "vector_lookup_ensure != 1000");
  assert_true(vector_lookup_ensure(v,1001) == NULL,
      "vector_lookup_ensure != NULL");

  vector_free(v);
}

void
test_vector_unset(void)
{
  vector v = NULL;

  printf("test_vector_unset\n");
  v = vector_init(0);

  vector_set_index(v, 1000, s1000);
  vector_set_index(v, 2000, s2000);
  assert_true(vector_count(v) == 2,
      "vector_count != 2");
  assert_true(vector_active(v) == 2001,
      "vector_active != 2001");

  vector_unset(v, 2000);
  assert_true(vector_count(v) == 1,
      "vector_count != 1");
  assert_true(vector_active(v) == 1001,
      "vector_active != 1001");

  vector_free(v);
}

void
test_vector_set_index_null(void)
{
  vector v = NULL;

  printf("test_vector_set_index_null\n");
  v = vector_init(0);

  vector_set_index(v, 1000, s1000);
  vector_set_index(v, 2000, s2000);
  assert_true(vector_count(v) == 2,
      "vector_count != 2");
  assert_true(vector_active(v) == 2001,
      "vector_active != 2001");

  /* storing a NULL is not the same as vector_unset() */
  vector_set_index(v, 2000, NULL);
  assert_true(vector_count(v) == 1,
      "vector_count != 1");
  assert_true(vector_active(v) == 2001,
      "vector_active != 2001");

  /* GMCH change in semantics */
  vector_unset(v, 1000);
  assert_true(vector_count(v) == 0,
      "vector_count != 0");
  assert_true(vector_active(v) == 2001,
      "vector_active != 2001 ignore post GMCH");
  assert_true(vector_active(v) == 0,
      "vector_active != 0 ignore pre GMCH");

  vector_free(v);
}

void
test_vector_copy(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  int i;
  const int len = 100;
  char buf[10];

  printf("test_vector_copy\n");

  /* to help debug objects are strings of their index */
  v1 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%d", i);
    vector_set_index(v1, i, strdup(buf));
  }

  v2 = vector_copy(v1);
  assert_true(v2 != NULL, "v2 == NULL");
  assert_true(v1 != v2, "v1 == v2");

  /* check contents are the same */
  for (i = 0; i < len; ++i)
    {
      assert_true(vector_slot(v1, i) == vector_slot(v2, i), "v1 != v2");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_slot(v1, i));
    }

  vector_free(v1);
  vector_free(v2);
}

void
test_vector_set(void)
{
  vector v = NULL;
  int i;
  const int len = 1000;
  char buf[10];

  printf("test_vector_set\n");

  /* to help debug objects are strings of their index */
  v = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%d", i);
    vector_set_index(v, i, strdup(buf));
  }

  assert_true(vector_count(v) == (unsigned)len,
      "vector_count != 1000");
  assert_true(vector_active(v) == (unsigned)len,
      "vector_active != 1000");

  vector_set(v, s1000);
  assert_true(vector_count(v) == len + 1,
      "vector_count != 1001");
  assert_true(vector_active(v) == len + 1,
      "vector_active != 1001");
  assert_true(vector_slot(v,1000) == s1000,
      "vector_slot != 1000");

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}

void
test_vector_growth(void)
{
  vector v = NULL;
  int i;
  const int len = 150000;
  char buf[10];

  printf("test_vector_growth\n");

  /* to help debug objects are strings of their index */
  v = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%d", i);
    vector_set_index(v, i, strdup(buf));
  }

  assert_true(vector_count(v) == (unsigned)len,
      "vector_count != len");
  assert_true(vector_active(v) == (unsigned)len,
      "vector_active != len");

  /* check contents are correct */
  for (i = 0; i < len; ++i)
    {
      sprintf(buf, "%d", i);
      assert_true(strcmp(vector_slot(v, i), buf) == 0, "vector_slot(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}


/* post GMCH interface */

void
test_vector_init_new(void)
{
  vector v = NULL;
  void * item = NULL;

  printf("test_vector_init_new\n");

  v = vector_init_new(v, 0);
  assert_true(v != NULL, "v == NULL");
  assert_true(vector_is_empty(v), "!vector_is_empty(v)");
  assert_true(vector_end(v) == 0, "vector_end(v) != 0");

  vector_push_item(v, s0);
  assert_true(vector_end(v) == 1, "vector_end(v) != 1");

  item = vector_pop_item(v);
  assert_true(item == s0, "item != s0");
  assert_true(vector_is_empty(v), "!vector_is_empty(v)");
  assert_true(vector_end(v) == 0, "vector_end(v) != 0");

  vector_free(v);
}

void
test_vector_set_item(void)
{
  vector v = NULL;
  void * item = NULL;

  printf("test_vector_set_item\n");

  v = vector_init_new(v, 0);
  vector_set_item(v, 0, s0);
  vector_set_item(v, 1000, s1000);
  vector_set_item(v, 2000, s2000);
  assert_true(vector_end(v) == 2001, "vector_end(v) != 2001");

  item = vector_get_item(v, 0);
  assert_true(item == s0, "item != s0");
  item = vector_get_item(v, 1000);
  assert_true(item == s1000, "item != s1000");
  item = vector_get_item(v, 2000);
  assert_true(item == s2000, "item != s2000");

  item = vector_get_first_item(v);
  assert_true(item == s0, "item != s0");

  item = vector_get_last_item(v);
  assert_true(item == s2000, "item != s2000");

  vector_free(v);
}
void
test_vector_insert_item(void)
{
    do_test_insert(2);
}

void
test_vector_insert_item_here(void)
{
  do_test_insert(-1);
  do_test_insert(0);
  do_test_insert(1);
}

void
test_vector_delete_item(void)
{
  do_test_insert(3);
}

void
do_test_insert(const int rider)
{
  vector v = NULL;
  const vector_index len = 100;
  const vector_index ins = 50;
  vector_index i;
  char buf[10];
  vector_index check_end = len + 1;
  vector_index check_ins = ins;
  int check_shift = 1;

  switch(rider)
  {
  case -1:
    printf("test_vector_insert_here before\n");
    break;
  case 0:
    printf("test_vector_insert_here at\n");
    check_shift = 0;
    check_end = len;
    break;
  case 1:
    printf("test_vector_insert_here after\n");
    check_ins++;
    break;
  case 2:
    printf("test_vector_insert\n");
    break;
  case 3:
    printf("test_vector_delete\n");
    check_shift = -1;
    check_end = len - 1;
    break;
  }

  v = vector_init_new(v, 0);

  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%d", i);
    vector_set_item(v, i, strdup(buf));
  }

  switch(rider)
    {
    case -1:
    case 0:
    case 1:
      vector_insert_item_here(v, ins, rider, strdup(s1000));
      break;
    case 2:
      vector_insert_item(v, ins, strdup(s1000));
      break;
    case 3:
      free(vector_delete_item(v, ins));
      break;
    }

  assert_true(vector_end(v) == check_end, "vector_end(v) != check_end");

  /* check contents are correct */
  for (i = 0; i < check_ins; ++i)
    {
      sprintf(buf, "%d", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  if (rider != 3)
    {
      assert_true(strcmp(vector_get_item(v, check_ins), s1000) == 0,
          "vector_get_item(v, check_ins) != s1000");
    }

  for (i = check_ins + 1; i < check_end; ++i)
    {
      sprintf(buf, "%d", i - check_shift);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0, "vector_get_item(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < vector_end(v); ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}

void
test_vector_sort(void)
{
  vector v = NULL;
  int i;
  const int len = 100;
  char buf[10];

  printf("test_vector_sort\n");

  v = vector_init(0);

  vector_sort(v, sort_cmp);     /* null sort */

  /* initialize backwards, using width so that lexical = numerical order */
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%9d", i);
    vector_set_index(v, len - i - 1, strdup(buf));
  }

  vector_sort(v, sort_cmp);

  /* check contents are correct */
  for (i = 0; i < len; ++i)
    {
      sprintf(buf, "%9d", i);
      assert_true(strcmp(vector_slot(v, i), buf) == 0, "vector_slot(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}

int
sort_cmp(const void* const* a, const void* const* b)
{
  return strcmp(*a, *b);
}

void
test_vector_bsearch(void)
{
  vector v = NULL;
  int i;
  const int len = 2000;
  char buf[20];
  char target[20];
  vector_index target_index = 0;
  int result;
  vector_index index;

  printf("test_vector_bsearch\n");

  sprintf(target, "%9u", target_index);
  v = vector_init(0);

  index = vector_bsearch(v, sort_cmp, target, &result);     /* null search */
  assert_true(index == 0, "index != 0");
  assert_true(result == -1, "result != -1");

  /* initialize, using width so that lexical = numerical order */
  for (i = 0; i < len; ++i)
    {
      sprintf(buf, "%9d", i);
      vector_set_index(v, i, strdup(buf));
    }

  for (target_index = 0; target_index < (unsigned)len; ++target_index)
    {
      sprintf(target, "%9u", target_index);
      index = vector_bsearch(v, sort_cmp, target, &result);
      assert_true(index == target_index, "index != target_index");
      assert_true(result == 0, "result != 0");
      assert_true(strcmp(vector_get_item(v, index), target) == 0,
          "strcmp(vector_get_item(v, index), target)) != 0");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}

void
test_vector_move_item_here(void)
{
  const uint n = 20 ;
  const uint l = n - 1 ;
  char strings[n][10] ;
  uint i ;

  for (i = 0 ; i < n ; ++i)
    sprintf(strings[i], "item=%2u", i) ;

  do_test_move_item_here(-1,  8, 16, strings, n);
  do_test_move_item_here( 0,  8, 16, strings, n);
  do_test_move_item_here(+1,  8, 16, strings, n);

  do_test_move_item_here(-1, 16,  8, strings, n);
  do_test_move_item_here( 0, 16,  8, strings, n);
  do_test_move_item_here(+1, 16,  8, strings, n);

  do_test_move_item_here(-1,  9,  9, strings, n);
  do_test_move_item_here( 0,  9,  9, strings, n);
  do_test_move_item_here(+1,  9,  9, strings, n);

  do_test_move_item_here(-1, 10,  9, strings, n);
  do_test_move_item_here( 0, 10,  9, strings, n);
  do_test_move_item_here(+1, 10,  9, strings, n);

  do_test_move_item_here(-1,  9, 10, strings, n);
  do_test_move_item_here( 0,  9, 10, strings, n);
  do_test_move_item_here(+1,  9, 10, strings, n);

  do_test_move_item_here(-1, 11,  9, strings, n);
  do_test_move_item_here( 0, 11,  9, strings, n);
  do_test_move_item_here(+1, 11,  9, strings, n);

  do_test_move_item_here(-1,  9, 11, strings, n);
  do_test_move_item_here( 0,  9, 11, strings, n);
  do_test_move_item_here(+1,  9, 11, strings, n);

  do_test_move_item_here(-1,  0, 10, strings, n);
  do_test_move_item_here( 0,  0, 10, strings, n);
  do_test_move_item_here(+1,  0, 10, strings, n);

  do_test_move_item_here(-1, 10,  0, strings, n);
  do_test_move_item_here( 0, 10,  0, strings, n);
  do_test_move_item_here(+1, 10,  0, strings, n);

  do_test_move_item_here(-1,  0,  l, strings, n);
  do_test_move_item_here( 0,  0,  l, strings, n);
  do_test_move_item_here(+1,  0,  l, strings, n);

  do_test_move_item_here(-1,  l,  0, strings, n);
  do_test_move_item_here( 0,  l,  0, strings, n);
  do_test_move_item_here(+1,  l,  0, strings, n);

  do_test_move_item_here(-1, 10,  l, strings, n);
  do_test_move_item_here( 0, 10,  l, strings, n);
  do_test_move_item_here(+1, 10,  l, strings, n);

  do_test_move_item_here(-1,  l, 10, strings, n);
  do_test_move_item_here( 0,  l, 10, strings, n);
  do_test_move_item_here(+1,  l, 10, strings, n);

  do_test_move_item_here(-1,  0,  0, strings, n);
  do_test_move_item_here( 0,  0,  0, strings, n);
  do_test_move_item_here(+1,  0,  0, strings, n);

  do_test_move_item_here(-1,  1,  1, strings, n);
  do_test_move_item_here( 0,  1,  1, strings, n);
  do_test_move_item_here(+1,  1,  1, strings, n);

  do_test_move_item_here(-1,  0,  1, strings, n);
  do_test_move_item_here( 0,  0,  1, strings, n);
  do_test_move_item_here(+1,  0,  1, strings, n);

  do_test_move_item_here(-1,  1,  0, strings, n);
  do_test_move_item_here( 0,  1,  0, strings, n);
  do_test_move_item_here(+1,  1,  0, strings, n);

  do_test_move_item_here(-1,  0,  2, strings, n);
  do_test_move_item_here( 0,  0,  2, strings, n);
  do_test_move_item_here(+1,  0,  2, strings, n);

  do_test_move_item_here(-1,  2,  0, strings, n);
  do_test_move_item_here( 0,  2,  0, strings, n);
  do_test_move_item_here(+1,  2,  0, strings, n);

  do_test_move_item_here(-1,  l,  l, strings, n);
  do_test_move_item_here( 0,  l,  l, strings, n);
  do_test_move_item_here(+1,  l,  l, strings, n);

  do_test_move_item_here(-1,l-1,  l, strings, n);
  do_test_move_item_here( 0,l-1,  l, strings, n);
  do_test_move_item_here(+1,l-1,  l, strings, n);

  do_test_move_item_here(-1,  l,l-1, strings, n);
  do_test_move_item_here( 0,  l,l-1, strings, n);
  do_test_move_item_here(+1,  l,l-1, strings, n);
}

void
do_test_move_item_here(const int rider, vector_index ins, vector_index src,
                                     char strings[][10], const vector_index len)
{
  vector v = NULL;
  vector_index i, e, check_end ;
  vector_index hi, lo ;
  char* expect[len] ;

  p_vector_item dest_item = NULL;

  switch(rider)
  {
  case -1:
    printf("test_vector_move_here before dst=%2d, src=%2d\n", src, ins);
    break;

  case 0:
    printf("test_vector_move_here at     dst=%2d, src=%2d\n", src, ins);
    --check_end ;
    break;

  case 1:
    printf("test_vector_move_here after  dst=%2d, src=%2d\n", src, ins);
    break;
  } ;

  /* Build the test vector and perform action                           */

  v = vector_init_new(v, 0);

  for (i = 0; i < len; ++i)
    vector_set_item(v, i, strdup(strings[i]));

  dest_item = vector_get_item(v, ins); /* item to free if rider == 0 */

  vector_move_item_here(v, ins, rider, src);

  /* Build the expected result.                                         */

  if (ins <= src)
    {
      lo = ins ;
      hi = src ;
    }
  else
    {
      lo = src ;
      hi = ins ;
    } ;

  check_end = (rider != 0) ? len : len - 1 ;
  i = 0 ;
  e = 0 ;

  while (i < lo)
    expect[i++] = strings[e++] ;

  if      (lo == hi)
    {
      /* Special case -- do nothing if rider != 0
       *                 drop entry if rider == 0
       */
      if (rider == 0)
        ++e ;
    }
  else if (lo == (hi - 1))
    {
      /* Special case -- ins and src next to each other !
       *
       * If rider != 0 -- insert ins and src in the required order
       * If rider == 0 -- insert just src
       */
      if      (rider < 0)
        {
          expect[i++] = strings[src] ;
          expect[i++] = strings[ins] ;
        }
      else if (rider == 0)
        {
          expect[i++] = strings[src] ;
        }
      else
        {
          expect[i++] = strings[ins] ;
          expect[i++] = strings[src] ;
        } ;
      e += 2 ;
    }
  else
    {
      /* Now we know that ins and src are separated by at least 1 entry.
       */
      uint be ;

      be = hi - 1 ;

      if (ins < src)
        {
          /* At insertion point, so insert ins and src in the required order
           * or insert juist the src.
           */
          if      (rider < 0)
            {
              expect[i++] = strings[src] ;
              expect[i++] = strings[ins] ;
              ++be ;
            }
          else if (rider == 0)
            {
              expect[i++] = strings[src] ;
            }
          else
            {
              expect[i++] = strings[ins] ;
              expect[i++] = strings[src] ;
              ++be ;
            } ;

          ++be ;
        } ;

      e += 1 ;

      while (i < be)
        expect[i++] = strings[e++] ;

      if (ins > src)
        {
          /* At insertion point, so insert ins and src in the required order
           * or insert juist the src.
           */
          if      (rider < 0)
            {
              expect[i++] = strings[src] ;
              expect[i++] = strings[ins] ;
            }
          else if (rider == 0)
            {
              expect[i++] = strings[src] ;
            }
          else
            {
              expect[i++] = strings[ins] ;
              expect[i++] = strings[src] ;
            } ;
        } ;

      e = hi + 1 ;
    } ;

  while (i < check_end)
    expect[i++] = strings[e++] ;

  /* check contents are correct                                         */
  assert_true(vector_end(v) == check_end, "vector_end(v) != check_end");

  for (i = 0 ; i < check_end ; ++i)
    {
      if (strcmp(vector_get_item(v, i), expect[i]) != 0)
        {
          assert_true(0, "vector_get_item(v, i) != expected") ;
          break ;
        } ;
    } ;

  /* free contents                                                      */
  for (i = 0; i < vector_end(v); ++i)
    free(vector_slot(v, i));

  if (rider == 0)
    free(dest_item);

  vector_free(v);
}

void
test_vector_part_reverse(void)
{
  vector v = NULL;
  const vector_index len = 100;
  const vector_index rstart = 50;
  const vector_index rstop = 70;
  vector_index i;
  char buf[10];

  printf("test_vector_part_reverse\n");

  v = vector_init_new(v, 0);

  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_item(v, i, strdup(buf));
  }

  vector_part_reverse(v, rstart, rstop - rstart);
  assert_true(vector_end(v) == len, "vector_end(v) != len");

  /* check contents are correct */

  /* before reversed section */
  for (i = 0; i < rstart - 1; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "before reversed vector_get_item(v, i) != buf");
    }

  /* reversed section */
  for (i = rstart; i < rstop; ++i)
    {
      sprintf(buf, "%u", rstop - (i - rstart + 1));
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "reversed vector_get_item(v, i) != buf");
    }

  /* after reversed section */
  for (i = rstop; i < len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "after reversed vector_get_item(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < vector_end(v); ++i)
    {
      free(vector_slot(v, i));
    }

  vector_free(v);
}

void
test_vector_copy_here(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  vector_index i;
  const vector_index len = 100;
  char buf[10];

  printf("test_vector_copy_here\n");

  /* to help debug objects are strings of their index */
  v1 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v1, i, strdup(buf));
  }

  v2 = vector_copy_here(v2, v1);
  assert_true(v2 != NULL, "v2 == NULL");
  assert_true(v1 != v2, "v1 == v2");

  /* check contents are the same */
  for (i = 0; i < len; ++i)
    {
      assert_true(vector_get_item(v1, i) == vector_get_item(v2, i), "v1 != v2");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_get_item(v1, i));
    }

  vector_free(v1);
  vector_free(v2);
}

void
test_vector_move_here(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  vector_index i;
  const vector_index len = 100;
  char buf[10];

  printf("test_vector_move_here\n");

  /* to help debug objects are strings of their index */
  v1 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v1, i, strdup(buf));
  }

  v2 = vector_move_here(v2, v1);
  assert_true(v2 != NULL, "v2 == NULL");
  assert_true(v1 != v2, "v1 == v2");
  assert_true(vector_end(v1) == 0, "vector_end(v1) != 0");

  /* check contents are the same */
  for (i = 0; i < len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_get_item(v2, i));
    }

  vector_free(v1);
  vector_free(v2);
}

void
test_vector_copy_append(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  vector_index i;
  const vector_index len = 100;
  char buf[10];

  printf("test_vector_copy_append\n");

  /* to help debug objects are strings of their index */
  v2 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v2, i, strdup(buf));
  }

  v1 = vector_init(0);
  for (i = len; i < 2 * len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v1, i - len, strdup(buf));
  }

  v2 = vector_copy_append(v2, v1);
  assert_true(v2 != NULL, "v2 == NULL");
  assert_true(v1 != v2, "v1 == v2");
  assert_true(vector_end(v2) == 2 * len, "vector_end(v2) != 2 * len");

  /* check contents */
  for (i = 0; i < 2 * len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }

  /* free contents */
  for (i = 0; i < 2 * len; ++i)
    {
      free(vector_get_item(v2, i));
    }

  vector_free(v1);
  vector_free(v2);
}

void
test_vector_move_append(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  vector_index i;
  const vector_index len = 100;
  char buf[10];

  printf("test_vector_move_append\n");

  /* to help debug objects are strings of their index */
  v2 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v2, i, strdup(buf));
  }

  v1 = vector_init(0);
  for (i = len; i < 2 * len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v1, i - len, strdup(buf));
  }

  v2 = vector_move_append(v2, v1);
  assert_true(v2 != NULL, "v2 == NULL");
  assert_true(v1 != v2, "v1 == v2");
  assert_true(vector_end(v2) == 2 * len, "vector_end(v2) != 2 * len");
  assert_true(vector_end(v1) == 0, "vector_end(v1) != 0");

  /* check contents */
  for (i = 0; i < 2 * len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }

  /* free contents */
  for (i = 0; i < 2 * len; ++i)
    {
      free(vector_get_item(v2, i));
    }

  vector_free(v1);
  vector_free(v2);
}

void
test_vector_insert(void)
{
  vector v = NULL;
  vector_index i;
  const vector_index len = 100;
  const vector_index istart = 50;
  const vector_index istop = 70;
  char buf[10];

  printf("test_vector_insert\n");

  /* to help debug objects are strings of their index */

  v = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v, i, strdup(buf));
  }

  vector_insert(v, istart, istop - istart);
  assert_true(vector_end(v) == len + (istop - istart),
      "vector_end(v) != len + (istop - istart)");

  /* check contents */
  for (i = 0; i < istart; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  for (i = istart + 1; i < istop; ++i)
    {
      assert_true(vector_get_item(v, i) == NULL,
          "vector_get_item(v, i) != NULL");
    }

  for (i = istop; i < len + (istop - istart); ++i)
    {
      sprintf(buf, "%u", i - (istop - istart));
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len + (istop - istart); ++i)
    {
      free(vector_get_item(v, i));
    }

  vector_free(v);
}

void
test_vector_delete(void)
{
  vector v = NULL;
  vector_index i;
  const vector_index len = 100;
  const vector_index dstart = 50;
  const vector_index dstop = 70;
  char buf[10];

  printf("test_vector_delete\n");

  /* to help debug objects are strings of their index */

  v = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    if (i < dstart || i >= dstop)
      {
        sprintf(buf, "%u", i);
        vector_set_index(v, i, strdup(buf));
      }
    else
      {
        vector_set_index(v, i, s0);
      }
  }

  vector_delete(v, dstart, dstop - dstart);
  assert_true(vector_end(v) == len - (dstop - dstart),
      "vector_end(v) != len - (dstop - dstart)");

  /* check contents */
  for (i = 0; i < dstart; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  for (i = dstart; i < len - (dstop - dstart); ++i)
    {
      sprintf(buf, "%u", i + (dstop - dstart));
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len - (dstop - dstart); ++i)
    {
      free(vector_get_item(v, i));
    }

  vector_free(v);
}

void
test_vector_discard(void)
{
  vector v = NULL;
  vector_index i;
  const vector_index len = 100;
  const vector_index dstart = 50;
  char buf[10];

  printf("test_vector_discard\n");

  /* to help debug objects are strings of their index */

  v = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    if (i < dstart)
      {
        sprintf(buf, "%u", i);
        vector_set_index(v, i, strdup(buf));
      }
    else
      {
        vector_set_index(v, i, s0);
      }
  }

  vector_discard(v, dstart);
  assert_true(vector_end(v) == dstart,
      "vector_end(v) != dstart");

  /* check contents */
  for (i = 0; i < dstart; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v, i), buf) == 0,
          "vector_get_item(v, i) != buf");
    }

  /* free contents */
  for (i = 0; i < dstart; ++i)
    {
      free(vector_get_item(v, i));
    }

  vector_free(v);
}

void
test_vector_sak(void)
{
  vector v1 = NULL;
  vector v2 = NULL;
  vector v3 = NULL;
  vector_index i;
  const vector_index len = 100;
  const vector_index sstart = 60;
  const vector_index sstop = 70;
  const vector_index dstart = 40;
  const vector_index dstop = 50;
  char buf[10];

  printf("test_vector_sak\n");

  /* to help debug objects are strings of their index */

  v2 = vector_init(0);
  v3 = vector_init(0);
  for (i = 0; i < len; ++i)
  {
    sprintf(buf, "%u", i);
    vector_set_index(v2, i, strdup(buf));
    vector_set_index(v3, i, strdup(buf));
  }

  v1 = vector_sak(1, v1, v2, dstart, dstop - dstart,
      v3, sstart, sstop - sstart, 0);
  assert_true(v1 != NULL, "v1 == NULL");

  assert_true(vector_end(v1) == (dstop - dstart),
      "vector_end(v1) != (dstop - dstart)");
  assert_true(vector_end(v2) == len,
      "vector_end(v2) != len");
  assert_true(vector_end(v3) == len,
      "vector_end(v3) != len");

  /* check contents v1 */
  for (i = 0; i < dstop - dstart; ++i)
    {
      sprintf(buf, "%u", i + dstart);
      assert_true(vector_get_item(v1, i) != NULL,
          "vector_get_item(v1, i) == NULL");
      assert_true(strcmp(vector_get_item(v1, i), buf) == 0,
          "vector_get_item(v1, i) != buf");
    }

  /* check contents v2 */
  for (i = 0; i < dstart; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }
  for (i = dstart; i < dstop; ++i)
    {
      sprintf(buf, "%u", i - dstart + sstart);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }
  for (i = dstop; i < len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v2, i), buf) == 0,
          "vector_get_item(v2, i) != buf");
    }

  /* check contents v3 */
  for (i = 0; i < len; ++i)
    {
      sprintf(buf, "%u", i);
      assert_true(strcmp(vector_get_item(v3, i), buf) == 0,
          "vector_get_item(v3, i) != buf");
    }

  /* free contents */
  for (i = 0; i < len; ++i)
    {
      free(vector_get_item(v3, i));
    }

  vector_free(v1);
  vector_free(v2);
  vector_free(v3);
}
/*
 *  TODO
 *

vector_re_init
vector_reset
vector_ream
vector_sak
vector_chop
vector_decant
vector_extend_by_1

*/

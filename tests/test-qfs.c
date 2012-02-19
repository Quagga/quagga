/*==============================================================================
 * qfstring torture tests
 *
 */
#include "misc.h"

#include <stdio.h>
#include <ctype.h>

#include "qfstring.h"
#include "qlib_init.h"
#include "command.h"

/*==============================================================================
 * prototypes
 */

int main(int argc, char **argv);

static void assert_true(int result, const char * message);

static void test_qfs_init(uint seed) ;
static void test_qfs_dec_value(bool show) ;
static void test_qfs_bin_value(bool show) ;

/*------------------------------------------------------------------------------
 * Run all tests
 */
int
main(int argc, char **argv)
{
  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  test_qfs_init(1) ;

  test_qfs_dec_value(true);
  test_qfs_bin_value(true);

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

static ulong random_last = 31415926535ul ;

static void random_ulong_seed(uint seed) ;

/*------------------------------------------------------------------------------
 * Initialise the test value handling
 *
 */
static void
test_qfs_init(uint seed)
{
  random_ulong_seed(seed) ;
} ;

/*------------------------------------------------------------------------------
 * Set random ulong seed
 */
static void
random_ulong_seed(uint seed)
{
  random_last = (((ulong)seed ^ 31415926535ul) * 31415926535ul) + 31415926535ul;
} ;

/*------------------------------------------------------------------------------
 * Random ulong
 */
static ulong
random_ulong(void)
{
  uint32_t  q ;

  union
  {
    ulong    l ;
    uint32_t r[2] ;
  } u ;

  confirm(sizeof(u.l) == sizeof(u.r)) ;

  u.l = random_last ;

  q = u.r[0] ^ u.r[1] ;
  q ^= u.r[0] = (q * 2650845021) + 5 ;
       u.r[1] = (q * 2650845021) + 5 ;

  return (random_last = u.l) ;
} ;

/*------------------------------------------------------------------------------
 * Random ulong between two values l & h, inclusive
 */
static ulong
random_ulong_between(ulong l, ulong h)
{
  return l + (random_ulong() % (h - l + 1)) ;
} ;

/*==============================================================================
 * Testing the decimal and binary scaled string constructors.
 *
 * The scaling and rounding is deceptively complicated.
 */

struct encoded
{
  int   s ;     /* sign -- 0 => none or ' '             */
  ulong v ;     /* value ignoring sign, ',' and '.'     */
  int   d ;     /* number of digits after any '.'       */
  int   t ;     /* scaling: 1000^t or 1024^t            */
  int   m ;     /* magnitude... (t+1)*3 - d             */
} ;

static void encode_dec(struct encoded* enc, long value, pf_flags_t flags) ;
static void encode_bin(struct encoded* enc, long value, pf_flags_t flags) ;
static const char* decode(struct encoded* enc, const char* str,
                                                             pf_flags_t flags) ;
static void test_value(long val, bool dec) ;
static int test_magnitude(long val, bool dec) ;

static const pf_flags_t flag_list[] =
{
    pf_scale,
    pf_scale | pf_plus,
    pf_scale | pf_space,
    pf_scale | pf_plus_nz,
    pf_scale | pf_plus | pf_space,
    pf_scale | pf_plus_nz | pf_space,

    pf_scale | pf_commas,
    pf_scale | pf_commas | pf_plus,
    pf_scale | pf_commas | pf_space,
    pf_scale | pf_commas | pf_plus_nz,
    pf_scale | pf_commas | pf_plus | pf_space,
    pf_scale | pf_commas | pf_plus_nz | pf_space,

    pf_scale | pf_trailing,

    0,
    pf_plus,
    pf_space,
    pf_plus_nz,
    pf_plus | pf_space,
    pf_plus_nz | pf_space,

    pf_commas,
    pf_commas | pf_plus,
    pf_commas | pf_space,
    pf_commas | pf_plus_nz,
    pf_commas | pf_plus | pf_space,
    pf_commas | pf_plus_nz | pf_space,

    pf_trailing,
} ;

enum { flags_count = sizeof(flag_list) / sizeof(pf_flags_t) } ;

/*------------------------------------------------------------------------------
 * Test qfs_dec_value()
 */
static void
test_qfs_dec_value(bool show)
{
  enum { type = true } ;        /* qfs_dec_value()              */

  int decade, tests ;
  ulong v_min ;
  ulong v_max ;

  printf("%s\n", __func__) ;

  /* test the min and max possible values, and simple sign combinations.
   */
  test_value(0, type) ;
  test_value(1, type) ;
  test_value(-1, type) ;
  test_value(LONG_MAX, type) ;
  test_value(LONG_MIN, type) ;

  /* Work our way up the decades, testing minimum, maximum, random values
   * between and then hunt down the rounding point.
   */
  decade = 0 ;
  v_min = 1 ;
  v_max = 9 ;
  tests = 4 ;

  while (v_min < LONG_MAX)
    {
      int t, m_min, m_max;

      if (show)
        printf("  d=%2d %4d tests: %ld..%ld", decade, tests, v_min, v_max) ;

      test_value(v_min, type) ;
      test_value(v_max, type) ;

      test_value(-v_min, type) ;
      test_value(-v_max, type) ;

      for (t = 0 ; t < tests ; ++t)
        {
          long v ;
          v = random_ulong_between(v_min, v_max) ;

          test_value(+v, type) ;
          test_value(-v, type) ;
        } ;

      m_min = test_magnitude(v_min, type) ;
      m_max = test_magnitude(v_max, type) ;

      assert_true(m_min == decade, "not the expected decade") ;

      if (show)
        printf("  m=%d..%d", m_min, m_max) ;

      if (m_min < m_max)
        {
          long v, vl, vr ;

          assert_true(m_min == (m_max - 1), "expect round to magnitude +1") ;

          vl = v_max - v_min ;
          vr = v_max ;

          assert_true(test_magnitude(vl, type) == m_min,
                                                     "expect vl not to round") ;
          assert_true(test_magnitude(vr, type) == m_max,
                                                     "expect vr to round") ;

          while (vr > (vl + 1))
            {
              int m ;

              v = (vl + vr) / 2 ;
              m = test_magnitude(v, type) ;

              if (m == m_min)
                vl = v ;
              else
                vr = v ;
            } ;

          if (show)
            printf("  rounds at: %ld", vr) ;
        }
      else
        {
          if (v_max != LONG_MAX)
            assert_true(v_max <= 9999, "expect rounding for values > 9999") ;

          if (show)
            printf("  no rounding") ;
        } ;

      if (show)
        printf("\n") ;

      /* step to next decade                            */

      v_min = v_max + 1 ;

      if (v_max < (LONG_MAX / 10))
        v_max = (v_max * 10) + 9 ;
      else
        v_max = LONG_MAX ;

      tests += (tests / 2) ;
      ++decade ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Test qfs_bin_value()
 */
static void
test_qfs_bin_value(bool show)
{
  enum { type = false } ;       /* qfs_bin_value()              */

  int decade, tests ;
  ulong v_min ;
  ulong v_max ;
  ulong bd ;

  printf("%s\n", __func__) ;

  /* test the min and max possible values, and simple sign combinations.
   */
  test_value(0, type) ;
  test_value(1, type) ;
  test_value(-1, type) ;
  test_value(LONG_MAX, type) ;
  test_value(LONG_MIN, type) ;

  /* Work our way up the decades, testing minimum, maximum, random values
   * between and then hunt down the rounding point.
   */
  decade = 0 ;
  bd     = 1 ;
  v_min  = 1 ;
  v_max  = 9 ;
  tests  = 4 ;

  while (v_min < LONG_MAX)
    {
      int t, m_min, m_max;

      if (show)
        {
          printf("  d=%2d %4d tests: ", decade, tests) ;
          if (decade < 3)
            printf("%ld..%ld", v_min, v_max) ;
          else
            printf("%lX..%lX", v_min, v_max) ;
        } ;

      test_value(v_min, type) ;
      test_value(v_max, type) ;

      test_value(-v_min, type) ;
      test_value(-v_max, type) ;

      for (t = 0 ; t < tests ; ++t)
        {
          long v ;
          v = random_ulong_between(v_min, v_max) ;

          test_value(+v, type) ;
          test_value(-v, type) ;
        } ;

      m_min = test_magnitude(v_min, type) ;
      m_max = test_magnitude(v_max, type) ;

      assert_true(m_min == decade, "not the expected decade") ;

      if (show)
        printf("  m=%d..%d", m_min, m_max) ;

      if (m_min < m_max)
        {
          long v, vl, vr ;

          assert_true(m_min == (m_max - 1), "expect round to magnitude +1") ;

          vl = v_max - v_min ;
          vr = v_max ;

          assert_true(test_magnitude(vl, type) == m_min,
                                                     "expect vl not to round") ;
          assert_true(test_magnitude(vr, type) == m_max,
                                                     "expect vr to round") ;

          while (vr > (vl + 1))
            {
              int m ;

              v = (vl + vr) / 2 ;
              m = test_magnitude(v, type) ;

              if (m == m_min)
                vl = v ;
              else
                vr = v ;
            } ;

          if (show)
            printf("  rounds at: %lx", vr) ;
        }
      else
        {
          if (show)
            printf("  no rounding") ;
        } ;

      if (show)
        printf("\n") ;

      /* step to next decade                            */

      ++decade ;
      v_min = v_max + 1 ;

      if (v_max < (LONG_MAX / 10))
        {
          if ((decade % 3) < 2)
            v_max = (v_min * 10) - 1 ;
          else
            {
              bd *= 1024 ;
              v_max = bd - 1 ;
            } ;
        }
      else
        v_max = LONG_MAX ;

      tests += (tests / 2) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Run qfs_dec_value()/qfs_bin_value() and compare result with what we expect
 * to get.
 */
static void
test_value(long val, bool dec)
{
  const char* what ;
  int i ;

  if (dec)
    what = "qfs_dec_value" ;
  else
    what = "qfs_bin_value" ;

  for (i = 0 ; i < flags_count ; ++i)
    {
      qfs_num_str_t num ;
      struct encoded got, expect ;
      const char *fail ;
      pf_flags_t flags ;

      flags = flag_list[i] ;

      if (dec)
        num = qfs_dec_value(val, flags) ;
      else
        num = qfs_bin_value(val, flags) ;

      fail = decode(&got, num.str, flags) ;
      if (fail != NULL)
        {
          printf("%s(%ld, %d): failed in decode '%s': %s\n", what,
                                                    val, flags, num.str, fail) ;
          continue ;
        } ;

      if (dec)
        encode_dec(&expect, val, flags) ;
      else
        encode_bin(&expect, val, flags) ;

      if (got.s != expect.s)
        {
          printf("%s(%ld, %x): not got expected sign '%s'\n", what,
                                                          val, flags, num.str) ;
          continue ;
        } ;

      if (got.v != expect.v)
        {
          printf("%s(%ld, %x): not got expected value '%s'\n", what,
                                                          val, flags, num.str) ;
          continue ;
        } ;

      if (got.t != expect.t)
        {
          printf("%s(%ld, %x): not got expected scale '%s' (is %d, expect %d)\n",
                               what, val, flags, num.str, got.t, expect.t) ;
          continue ;
        } ;

      if (got.t != expect.t)
        {
          printf("%s(%ld, %x): not got expected scale '%s' (is %d, expect %d)\n",
                               what, val, flags, num.str, got.t, expect.t) ;
          continue ;
        } ;

      if (got.m != expect.m)
        {
          printf("%s(%ld, %x): not got expected magnitude '%s'"
                                                       " (is %d, expect %d)\n",
                                   what, val, flags, num.str, got.m, expect.m) ;
          continue ;
        } ;
    } ;
  return ;
} ;

/*------------------------------------------------------------------------------
 * Get the expected magnitude for the given value when scaled
 */
static int
test_magnitude(long val, bool dec)
{
  struct encoded expect ;

  if (dec)
    encode_dec(&expect, val, pf_scale) ;
  else
    encode_bin(&expect, val, pf_scale) ;

  return expect.m ;
} ;

#if 0

static void
bd_form_test(void)
{
  ulong s ;
  int   g = 0 ;

  fprintf(stderr, "Group %d\n", g) ;
  b_test(0    ) ;
  b_test(1    ) ;
  b_test(9    ) ;
  b_test(10   ) ;
  b_test(99   ) ;
  b_test(100  ) ;
  b_test(999  ) ;
  b_test(1023 ) ;
  b_test(1024 ) ;
  b_test(1025 ) ;
  b_test(9999 ) ;
  b_test(10000) ;
  b_test(10007) ;

  s = 1 ;
  for (g = 1 ; g <= 5 ; ++g)
    {
      fprintf(stderr, "Group %d\n", g) ;
      s *= 1024ul ;
      b_test(t_val(.31414, s)    ) ;
      b_test(t_val(.31415, s)    ) ;
      b_test(t_val(.31416, s)    ) ;
      b_test(t_val(.99995, s) - 1) ;
      b_test(t_val(.99995, s)    ) ;
      b_test(t_val(.99995, s) + 1) ;
      b_test(t_val(1.2345, s)    ) ;
      b_test(t_val(9.9995, s) - 1) ;
      b_test(t_val(9.9995, s)    ) ;
      b_test(t_val(9.9995, s) + 1) ;
      b_test(t_val(3.1415, s) - 1) ;
      b_test(t_val(3.1415, s)    ) ;
      b_test(t_val(3.1416, s)    ) ;
      b_test(t_val(99.995, s) - 1) ;
      b_test(t_val(99.995, s)    ) ;
      b_test(t_val(99.995, s) + 1) ;
      b_test(t_val(314.15, s) - 1) ;
      b_test(t_val(314.15, s)    ) ;
      b_test(t_val(314.16, s)    ) ;
      b_test(t_val(999.95, s) - 1) ;
      b_test(t_val(999.95, s)    ) ;
      b_test(t_val(999.95, s) + 1) ;
      b_test(t_val(1023.5, s) - 1) ;
      b_test(t_val(1023.5, s)    ) ;
      b_test(t_val(1023.5, s) + 1) ;
      b_test(t_val(3141.5, s) - 1) ;
      b_test(t_val(3141.5, s)    ) ;
      b_test(t_val(3141.6, s)    ) ;
      b_test(t_val(9999.5, s) - 1) ;
      b_test(t_val(9999.5, s)    ) ;
      b_test(t_val(9999.5, s) + 1) ;
    } ;

  g = 0 ;
  fprintf(stderr, "Group %d\n", g) ;
  d_test(0    ) ;
  d_test(1    ) ;
  d_test(9    ) ;
  d_test(10   ) ;
  d_test(11   ) ;
  d_test(99   ) ;
  d_test(100  ) ;
  d_test(999  ) ;
  d_test(1000 ) ;
  d_test(1001 ) ;
  d_test(9999 ) ;
  d_test(10000) ;
  d_test(10001) ;

  s = 1 ;
  for (g = 1 ; g <= 4 ; ++g)
    {
      fprintf(stderr, "Group %d\n", g) ;
      s *= 1000ul ;
      d_test(t_val(1.2345, s) - 1) ;
      d_test(t_val(1.2345, s)    ) ;
      d_test(t_val(9.9995, s) - 1) ;
      d_test(t_val(9.9995, s)    ) ;
      d_test(t_val(21.235, s) - 1) ;
      d_test(t_val(21.235, s)    ) ;
      d_test(t_val(99.995, s) - 1) ;
      d_test(t_val(99.995, s)    ) ;
      d_test(t_val(312.35, s) - 1) ;
      d_test(t_val(312.35, s)    ) ;
      d_test(t_val(999.95, s) - 1) ;
      d_test(t_val(999.95, s)    ) ;
      d_test(t_val(4321.5, s) - 1) ;
      d_test(t_val(4321.5, s)    ) ;
      d_test(t_val(9999.5, s) - 1) ;
      d_test(t_val(9999.5, s)    ) ;
    } ;

  g = 5 ;
  fprintf(stderr, "Group %d\n", g) ;
  s *= 1000ul ;
  d_test(t_val(1.2345, s) - 1) ;
  d_test(t_val(1.2345, s)    ) ;
  d_test(t_val(9.9995, s) - 1) ;
  d_test(t_val(9.9995, s)    ) ;
  d_test(t_val(21.2345, s) - 1) ;
  d_test(t_val(21.2345, s)    ) ;
  d_test(t_val(99.9995, s) - 1) ;
  d_test(t_val(99.9995, s)    ) ;
  d_test(t_val(312.6785, s) - 1) ;
  d_test(t_val(312.6785, s)    ) ;
  d_test(t_val(999.9995, s) - 1) ;
  d_test(t_val(999.9995, s)    ) ;
  d_test(t_val(4321.5675, s) - 1) ;
  d_test(t_val(4321.5675, s)    ) ;
  d_test(t_val(9999.9995, s) - 1) ;
  d_test(t_val(9999.9995, s)    ) ;
} ;

static ulong
t_val(double v, ulong s)
{
  return v * s ;
} ;

static void
b_test(ulong v)
{
  num_str_t g ;
  char      e[50] ;
  ulong     u ;
  uint      i ;
  uint      d ;

  d = 0 ;
  u = v ;
  i = 0 ;
  if (u >= 1024)
    {
      uint n ;

      while ((u >= 1024) && (i < scale_max))
        {
          u >>= 10 ;
          i  += 1 ;
        }
      n = i * 10 ;

      u = v ;
      while (u < (1000ul << n))
        {
          u *= 10 ;
          d += 1 ;
        } ;

      u = ((u >> (n - 1)) + 1) >> 1 ;

      if (d == 0)
        {
          if ((u == 1024) && (i < scale_max))
            {
              u  = 1000 ;
              i += 1 ;
              d  = 3 ;
            } ;
        }
      else
        {
          assert(u <= 10000) ;
          if (u == 10000)
            {
              u  = 1000 ;
              d -= 1 ;
            } ;
        } ;
    } ;

  c_val(e, u, d, scale_b_tags[i]) ;

  g = mem_form_byte_count(v) ;

  fprintf(stderr, "%16lx %'20lu -> '%7s'", v, v, g.str) ;

  if (strcmp(e, g.str) == 0)
    fputs(" -- OK\n", stderr) ;
  else
    fprintf(stderr, " *** expect: '%7s'\n", e) ;
} ;

static void
d_test(ulong v)
{
  num_str_t g ;
  char      e[50] ;
  ulong     u ;
  uint      i ;
  uint      n ;
  uint      d ;

  d = 0 ;
  i = 0 ;
  u = v ;
  n = (v == 0) ? 0 : 1 ;

  while (u >= 10)
    {
      u /= 10 ;
      n += 1 ;
    } ;

  u = v ;

  if (n > 4)
    {
      if (n > ((scale_max * 3) + 3))
        {
          u = (u + q10[scale_max * 3]) / p10[scale_max * 3] ;
          i = scale_max ;
        }
      else
        {
          u = (u + q10[n - 4]) / p10[n - 4] ;

          if (u > 9999)
            {
              u /= 10 ;
              n += 1 ;
            } ;

          i = (n - 2) / 3 ;
          d = (i * 3) + 4 - n ;
        } ;
    } ;

  c_val(e, u, d, scale_d_tags[i]) ;

  g = mem_form_count(v) ;

  fprintf(stderr, "%'20lu -> '%7s'", v, g.str) ;

  if (strcmp(e, g.str) == 0)
    fputs(" -- OK\n", stderr) ;
  else
    fprintf(stderr, " *** expect: '%7s'\n", e) ;
} ;

static void
c_val(char* e, ulong u, uint d, const char* t)
{
  if      (d != 0)
    sprintf(e, "%lu.%0*lu%s", u / p10[d], (int)d, u % p10[d], t) ;
  else
    {
      if      (u < 1000)
        sprintf(e, "%lu%s", u, t) ;
      else if (u < 1000000)
        sprintf(e, "%lu,%03lu%s", u / 1000, u % 1000, t) ;
      else
        sprintf(e, "%lu,%03lu,%03lu%s", u / 1000000, (u / 1000) % 1000,
                                                                 u % 1000, t) ;
    } ;
} ;
#endif

/*------------------------------------------------------------------------------
 * Work out scaled encoding for given value, for decimal numbers.
 *
 * See qfs_dec_value() -- which encodes as 4 digits, with 0, 1 or 2 after
 * decimal point -- scaling in 1000^n.
 *
 * Sets enc->s  == -1 if  < 0
 *              ==  0 if == 0
 *              == +1 if  > 0
 */
static void
encode_dec(struct encoded* enc, long val, pf_flags_t flags)
{
  ulong v ;
  int q, m ;

  enc->s = 0 ;

  if (val < 0)
    {
      enc->s = -1 ;
      v = (ulong)labs(val + 1) + 1 ;
    }
  else
    {
      v = val ;

      if ((flags & pf_plus) || ((flags & pf_plus_nz) && (val > 0)))
        enc->s = +1 ;
      else
        enc->s =  0 ;
    } ;

  q = 0 ;
  m = 0 ;

  if ((flags & pf_scale) && (v > 9999))
    {
      do                        /* more than 4 digits           */
        {
          ++q ;
          if (v <= 99999)       /* round when reach 5 digits    */
            v += 5 ;
          v /= 10 ;
        } while (v > 9999) ;
    }
  else
    {
      ulong q = v ;

      while (q > 9)
        {
          ++m ;
          q /= 10 ;
        } ;
    } ;

  /* When we get here we have q and the value (after rounding):
   *
   *   q = 0: v = 0..9,999          -- d = 0, t = 0, m = number of digits - 1
   *
   *   q = 1: v = 10.00k..99.99k    -- d = 2, t = 1, m = 4
   *   q = 2: v = 100.0k..999.9k    -- d = 1, t = 1, m = 5
   *   q = 3: v = 1,000k..9,999k    -- d = 0, t = 1, m = 6
   *
   *   q = 4: v = 10.00m..99.99m    -- d = 2, t = 2, m = 7
   *   q = 5: v = 100.0m..999.9m    -- d = 1, t = 2, m = 8
   *   q = 6: v = 1,000m..9,999m    -- d = 0, t = 2, m = 9
   *
   * If no scaling, returns t = 0 & d = 0
   */
  enc->v = v ;
  enc->t = (q + 2) / 3 ;
  enc->d = (enc->t * 3) - q ;
  enc->m = (q > 0) ? (enc->t * 3) + (3 - enc->d) : m ;
} ;

/*------------------------------------------------------------------------------
 * Work out scaled encoding for given value, for binary numbers.
 *
 * See qfs_bin_value() -- which encodes as 4 digits, with 1, 2 or 3 after
 * decimal point -- scaling in 1024^n.
 *
 * Sets enc->s  == -1 if  < 0
 *              ==  0 if == 0
 *              == +1 if  > 0
 */
static void
encode_bin(struct encoded* enc, long val, pf_flags_t flags)
{
  ulong v ;
  int t, d ;
  int m ;

  if (val < 0)
    {
      enc->s = -1 ;
      v = (ulong)labs(val + 1) + 1 ;
    }
  else
    {
      v = val ;

      if ((flags & pf_plus) || ((flags & pf_plus_nz) && (val > 0)))
        enc->s = +1 ;
      else
        enc->s =  0 ;
    } ;

  t  = 0 ;
  d  = 0 ;
  m  = 0 ;

  if ((flags & pf_scale) && (v > 999))
    {
      ulong bp, vv, f ;

      vv = v ;
      bp = 1 ;
      do                        /* find 0..999 * 1024^t */
        {
          v  >>= 10 ;
          bp <<= 10 ;
          t += 1 ;
        } while (v > 999) ;

      f = vv % bp ;

      do
        {
          v *= 10 ;
          f *= 10 ;
          v += f / bp ;
          f %= bp ;
          ++d ;
        } while ((v <= 999) && (d < 3)) ;

      v += (f + (bp / 2)) / bp ;

      if (v > 9999)
        {
          --d ;
          v /= 10 ;
          if (d == 0)
            {
              d = 3 ;
              t+= 1 ;
            } ;
        } ;
    }
  else
    {
      ulong q = v ;

      while (q > 9)
        {
          ++m ;
          q /= 10 ;
        } ;
    } ;

  /* When we get here we have q and the value (after rounding):
   *
   *   t = 0: v = 0..999            -- d = 0, m = number of digits - 1
   *
   *   t = 1: v = 0.976K..9.999K    -- d = 3, m = 3
   *   t = 1: v = 10.00K..99.99K    -- d = 2, m = 4
   *   t = 1: v = 100.0K..999.9K    -- d = 1, m = 5
   *
   *   t = 2: v = 0.976M..9.999M    -- d = 3, m = 6
   *   t = 2: v = 10.00M..99.99M    -- d = 2, m = 7
   *   t = 2: v = 100.0M..999.9M    -- d = 1, m = 8
   *
   * If no scaling, returns t = 0 & d = 0
   */
  enc->v = v ;
  enc->t = t ;
  enc->d = d ;
  enc->m = (d > 0) ? (t * 3) + (3 - d) : m ;
} ;

/*------------------------------------------------------------------------------
 * Take string returned by qfs_dec_value() or qfs_bin_value(), and extract
 *
 *   enc->s   --  0 => none
 *               -1 => '-'
 *               +1 => '+'
 *   enc->v   -- the value of the digits -- ignoring ',' and '.' and sign
 *   enc->d   -- number of digits after any '.'
 *   enc->t   -- scaling: 1000^t or 1024^t
 *   enc->m   -- if (enc->t == 0) = number of digits
 *                           else = (enc->t * 3) + (3 - enc->d)
 *
 * Returns NULL <=> appears well formed
 *         otherwise, indication of error.
 */
static const char*
decode(struct encoded* enc, const char* str, pf_flags_t flags)
{
  bool post ;
  bool pre ;
  int c ;
  int m ;

  enc->s = 0 ;

  if      (*str == '-')
    {
      enc->s = -1 ;
      ++str ;
    }
  else if (*str == '+')
    {
      if (((flags & pf_plus) == 0) && ((flags & pf_plus_nz) == 0))
        return "do not expect '+'" ;

      enc->s = +1 ;
      ++str ;
    }
  else if (*str == ' ')
    {
      if (flags & pf_plus)
        return "expect '+'/'-'" ;

      if ((flags & pf_space) == 0)
        return "do not expect ' ' 'sign'" ;

      ++str ;
    }
  else
    {
      if (flags & (pf_plus | pf_space))
        return "expect '+'/'-'/' '" ;
    } ;

  if (!isdigit((int)*str))
    return "does not start with digit (after any sign)" ;

  enc->v = 0 ;
  enc->d = 0 ;
  post = false ;
  pre  = false ;
  c    = 0 ;
  m    = 0 ;

  while (1)
    {
      if (isdigit((int)*str))
        {
          ulong d =  (*str++ - '0') ;

          if (enc->v > (LONG_MAX / 10))
            return "value > LONG_MAX" ;

          enc->v *= 10 ;

          if ((LONG_MAX - enc->v) < d)
            {
              if ((enc->s >= 0) || ((LONG_MAX - enc->v + 1) < d))
                return "value > LONG_MAX" ;
            } ;

          enc->v += d ;

          if (post)
            ++enc->d ;

          ++m ;
          ++c ;
        }
      else if ((*str == ',') || (*str == '.'))
        {
          if (post)
            return "'.' or ',' after '.'" ;

          if (pre)
            {
              if (c != 3)
                return "expect exactly 3 digits after ','" ;
            }
          else if (*str == ',')
            {
              if ((flags & pf_commas) == 0)
                return "do not expect ','" ;

              if ((c < 1) || (c > 3))
                return "expect 1..3 digits before first ','" ;
            } ;

          pre  = (*str == ',') ;
          post = (*str == '.') ;

          c = 0 ;
          ++str ;
        }
      else
        break ;
    } ;

  if (pre && (c != 3))
    return "expect exactly 3 digits after ','" ;

  if (post && (c == 0))
    return "expect at least one digit after '.'" ;

  if ((flags & pf_plus_nz) && (enc->s == 0) && (enc->v != 0))
    return "expect '+'" ;

  enc->t = 0 ;

  switch (*str++)       /* deal with scale character    */
    {
      case ' ':
        if ((flags & pf_trailing) ==0)
          return "do not expect trailing ' '" ;

        break ;

      case 'k':
      case 'K':
        enc->t =  1 ;
        break ;

      case 'm':
      case 'M':
        enc->t =  2 ;
        break ;

      case 'g':
      case 'G':
        enc->t =  3 ;
        break ;

      case 't':
      case 'T':
        enc->t =  4 ;
        break ;

      case 'p':
      case 'P':
        enc->t =  5 ;
        break ;

      case 'e':
      case 'E':
        enc->t =  6 ;
        break ;

      default:
        --str ;         /* not scale character, step back       */
        break ;
    } ;

  if (post && (enc->t == 0))
    return "expect scale after '.'" ;

  if (*str != '\0')
    return "unexpected character in number" ;

  if (enc->t != 0)
    enc->m = (enc->t * 3) + (3 - enc->d) ;
  else
    enc->m = m - 1 ;

  return NULL ;
} ;


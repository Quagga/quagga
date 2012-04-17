#include <zebra.h>
#include "misc.h"
#include "qlib_init.h"
#include "command.h"

#include "prefix.h"

/*------------------------------------------------------------------------------
 * Infrastructure
 */
typedef struct
{
  uint  errs ;
  uint  errs_total ;

  uint  good ;
  uint  total ;
} counts_t ;

typedef counts_t* counts ;

typedef void (*test_func_t)(counts ct) ;

typedef struct
{
  test_func_t  func ;

  uint  good ;
  uint  total ;
} test_t ;


static uint get_pbit(byte* v, uint bn) __attribute__((unused)) ;
static void set_pbit(byte* v, uint bn) ;
static void clear_pbit(byte* v, uint bn) ;
static void flip_pbit(byte* v, uint bn) ;

/*------------------------------------------------------------------------------
 * The actual testing
 */
static void test_masklen2ip(counts ct) ;
static void test_masklen2ip6(counts ct) ;
static void test_ip_masklen(counts ct) ;
static void test_ip6_masklen(counts ct) ;
static void test_apply_mask_ipv4(counts ct) ;
static void test_apply_mask_ipv6(counts ct) ;
static void test_prefix_raw_ipv4(counts ct) ;
static void test_prefix_raw_ipv6(counts ct) ;
static void test_prefix_match(counts ct) ;
static void test_prefix_same(counts ct) ;
static void test_prefix_cmp(counts ct) ;
static void test_prefix_common_bits(counts ct) ;
static void test_spfxtoa_str2prefix(counts ct) ;

static const test_t tests[] =
{
  { .func = test_masklen2ip,         .good =     33, .total =     256 },
  { .func = test_masklen2ip6,        .good =    129, .total =     256 },
  { .func = test_ip_masklen,         .good =     33, .total =     256 },
  { .func = test_ip6_masklen,        .good =    129, .total =     512 },
  { .func = test_apply_mask_ipv4,    .good =   1800, .total =    1800 },
  { .func = test_apply_mask_ipv6,    .good =   6800, .total =    6800 },
  { .func = test_prefix_raw_ipv4,    .good =     33, .total =     256 },
  { .func = test_prefix_raw_ipv6,    .good =    129, .total =     256 },
  { .func = test_prefix_match,       .good = 723905, .total = 1098177 },
  { .func = test_prefix_same,        .good =    512, .total =   36864 },
  { .func = test_prefix_cmp,         .good =   8385, .total =  133128 },
  { .func = test_prefix_common_bits, .good =  83592, .total =  650160 },
  { .func = test_spfxtoa_str2prefix, .good =    512, .total =     512 },

  { .func = NULL }
} ;

static byte mask_n[256][32] ;

int
main (int argc, char **argv)
{
  const test_t*  test ;
  counts_t ct[1] ;

  uint i ;

  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  for (i = 0 ; i <= 256 ; ++i)
    {
      uint b ;

      for (b = 0 ; b < i ; b++)
        set_pbit(mask_n[i], b) ;

      for (b = i ; b < 256 ; b++)
        clear_pbit(mask_n[i], b) ;
    } ;

#if 0
  for (i = 0 ; i <= 256 ; ++i)
    {
      uint b ;

      fprintf(stderr, "%4u:", i) ;
      for (b = 0 ; b < 32 ; ++b)
        fprintf(stderr, " %02x", mask_n[i][b]) ;
      fprintf(stderr, "\n") ;
    } ;
#endif

  test = tests ;
  ct->errs_total = 0 ;

  while (test->func != NULL)
    {
      ct->errs   = 0 ;
      ct->good   = 0 ;
      ct->total  = 0 ;

      test->func(ct) ;

      if (ct->errs == 0)
        {
          fprintf(stderr, " -- OK  %u/%u", ct->good, ct->total) ;

          if ((ct->good == test->good) && (ct->total == test->total))
            fprintf(stderr, " -- as expected\n") ;
          else
            {
              fprintf(stderr, " *** but expected %u/%u\n", test->good,
                                                           test->total) ;
              ++ct->errs ;
            } ;
        }
      else
        {
          fprintf(stderr, "\n === %u errors (%u/%u)\n", ct->errs, ct->good,
                                                                    ct->total) ;
         } ;

      ct->errs_total += ct->errs ;

      ++test ;
    } ;

  if (ct->errs_total == 0)
    fprintf(stderr, "All tests completed -- OK\n") ;
  else
    fprintf(stderr, "*** All tests completed, with a total of %u errors\n",
                                                               ct->errs_total) ;

  if (ct->errs_total == 0)
    return 0;
  else
    return 1 ;
} ;

/*------------------------------------------------------------------------------
 * masklen2ip() -- converts a mask length to a mask.
 */
static void
test_masklen2ip(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test masklen2ip()") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      struct in_addr get ;
      struct in_addr expect ;

      masklen2ip (plen, &get) ;

      /* For plen 0..32 we expect to get a mask 0x00000000..0xFFFFFFFF
       *
       * For all other plen we get the maximum mask 0xFFFFFFFF.
       *
       * We count 0..32 as "good" -- so final mark should be 33/256.
       */
      memcpy(&expect, mask_n[plen], 4) ;

      ++ct->total ;

      if (expect.s_addr == get.s_addr)
        {
          if (plen <= 32)
            ++ct->good ;
        }
      else
        {
          ++ct->errs ;
          fprintf(stderr, "\n *** for plen=%u expect 0x%08x got 0x%08x",
                                plen, ntohl(expect.s_addr), ntohl(get.s_addr)) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * masklen2ip6() -- converts a mask length to a mask.
 */
static void
test_masklen2ip6(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test masklen2ip6()") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      union ip6_test
      {
        struct
        {
          char dummy_0[7] ;
          struct in6_addr ip6 ;
          char dummy_1[5] ;
        } a ;
        char t[1] ;
      } ;

      union ip6_test get, expect ;

      memset(&get,    0xA5, sizeof(union ip6_test)) ;
      memset(&expect, 0xA5, sizeof(union ip6_test)) ;

      masklen2ip6 (plen, &get.a.ip6) ;

      /* For plen 0..128 we expect to get a mask 0x00...00..0xFF...FF
       *
       * For all other plen we get the maximum mask 0xFF...FF.
       *
       * We count 0..128 as "good" -- so final mark should be 129/256.
       */
      memcpy(&expect.a.ip6, mask_n[plen], 16) ;

      ++ct->total ;

      if (memcmp(&get, &expect, sizeof(union ip6_test)) == 0)
        {
          if (plen <= 128)
            ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** for plen=%u", plen) ;
          fprintf(stderr, "\n     exp:") ;
          for (i = 0 ; i < 16 ; ++i)
            fprintf(stderr, " %02x", expect.a.ip6.s6_addr[i]) ;
          fprintf(stderr, "\n     got:") ;
          for (i = 0 ; i < 16 ; ++i)
            fprintf(stderr, " %02x", get.a.ip6.s6_addr[i]) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * ip_masklen() & ip_mask_check() -- converts mask to prefix/checks mask
 */
static void
test_ip_masklen(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test ip_masklen() & ip_mask_check()") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      uint get, expect ;
      bool get_ok, expect_ok ;
      struct in_addr mask ;

      /* ip_masklen() will give a result for any mask.
       *
       * We use plen > 32 to construct masks of lengths 0..30, and perturb
       * one bit after the first '0'.
       *
       * Expect final score of 33/256.
       */
      if (plen <= 32)
        {
          masklen2ip (plen, &mask) ;
          expect    = plen ;
          expect_ok = true ;            /* good mask    */
        }
      else
        {
          uint px ;

          expect    = plen % 31 ;       /* 0..30        */
          expect_ok = false ;

          masklen2ip (expect, &mask) ;

          px = expect + 1 ;             /* bit after first '0'  */
          set_pbit((byte*)&mask, px + (rand() % (32 - px))) ;
        } ;

      get    = ip_masklen(mask) ;
      get_ok = ip_mask_check(mask) ;

      ++ct->total ;

      if ((get == expect) && (get_ok == expect_ok))
        {
          if (expect_ok)
            ++ct->good ;
        }
      else
        {
          ++ct->errs ;
          fprintf(stderr, "\n *** for mask=0x%08x expect %u got %u, "
                                                       "expect_ok %u got_ok %u",
                           ntohl(mask.s_addr), expect, get, expect_ok, get_ok) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * ip6_masklen() & ip6_mask_check() -- converts mask to prefix/checks mask
 */
static void
test_ip6_masklen(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test ip6_masklen() & ip6_mask_check()") ;
  for (plen = 0 ; plen <= 511 ; ++plen)
    {
      uint get, expect ;
      bool get_ok, expect_ok ;
      struct in6_addr mask ;

      /* ip_masklen() will give a result for any mask.
       *
       * We use plen > 128 to construct masks of lengths 0..126, and perturb
       * one bit after the first '0'.
       *
       * Expect final score of 129/512.
       */
      if (plen <= 128)
        {
          masklen2ip6 (plen, &mask) ;
          expect    = plen ;
          expect_ok = true ;
        }
      else
        {
          uint px ;

          expect    = plen % 127 ;              /* 0..126               */
          expect_ok = false ;

          masklen2ip6 (expect, &mask) ;
          px = expect + 1 ;                     /* bit after first '0'  */
          set_pbit(mask.s6_addr, px + (rand() % (128 - px))) ;
        } ;

      get    = ip6_masklen(&mask) ;
      get_ok = ip6_mask_check(&mask) ;

      ++ct->total ;
      if ((expect == get) && (expect_ok == get_ok))
        {
          if (expect_ok)
            ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** for mask=0x%02x", mask.s6_addr[0]) ;
          for (i = 1 ; i < 16 ; ++i)
            fprintf(stderr, " %02x", mask.s6_addr[i]) ;
          fprintf(stderr, " expect %u got %u, expect_ok %d got_ok %d",
                                               expect, get, expect_ok, get_ok) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * apply_mask_ipv4() -- takes body of prefix and applies mask to it
 */
static void
test_apply_mask_ipv4(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test apply_mask_ipv4()") ;
  for (plen = 0 ; plen < 36 ; ++plen)
    {
      uint it ;

      /* We use plen > 32 to construct masks of lengths 33..255
       *
       * For each plen generate 50 random prefix values.
       *
       * Expect final score of 1800/1800.
       */
      for (it = 0 ; it < 50 ; ++it)
        {
          uint i ;
          uint8_t* p ;
          struct prefix_ipv4 px ;
          struct prefix_ipv4 p1 ;
          struct prefix_ipv4 p2 ;

          /* Fill p1 with garbage and set prefix length
           */
          p = (uint8_t*)&px ;
          for (i = 0 ; i < sizeof(struct prefix_ipv4) ; ++i)
             *p++ = rand() & 0xFF ;

          if (plen <= 32)
            px.prefixlen = plen ;
          else
            px.prefixlen = 33 + (rand() % (256 - 33)) ;

          memcpy(&p1, &px, sizeof(struct prefix_ipv4)) ;
          memcpy(&p2, &px, sizeof(struct prefix_ipv4)) ;

          apply_mask_ipv4(&p1) ;

          if (plen < 32)
            p2.prefix.s_addr &= htonl(~(uint64_t)0 << (32 - plen)) ;

          ++ct->total ;

          if (memcmp(&p2, &p1, sizeof(struct prefix_ipv4)) == 0)
            {
              ++ct->good ;
            }
          else
            {
              ++ct->errs ;
              fprintf(stderr, "\n *** for 0x%08x/%d expect 0x%08x got 0x%08x",
                             ntohl(px.prefix.s_addr), px.prefixlen,
                             ntohl(p1.prefix.s_addr), ntohl(p2.prefix.s_addr)) ;
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * apply_mask_ipv6() -- takes body of prefix and applies mask to it
 */
static void
test_apply_mask_ipv6(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test apply_mask_ipv6()") ;
  for (plen = 0 ; plen < 136 ; ++plen)
    {
      uint it ;

      /* We use plen > 128 to construct masks of lengths 129..255
       *
       * For each plen generate 50 random prefix values.
       *
       * Expect final score of 6800/6800.
       */
      for (it = 0 ; it < 50 ; ++it)
        {
          uint i ;
          uint8_t* p ;
          struct prefix_ipv6 px ;
          struct prefix_ipv6 p1 ;
          struct prefix_ipv6 p2 ;

          /* Fill p1 with garbage and set prefix length
           */
          p = (uint8_t*)&px ;
          for (i = 0 ; i < sizeof(struct prefix_ipv4) ; ++i)
             *p++ = rand() & 0xFF ;

          if (plen <= 128)
            px.prefixlen = plen ;
          else
            px.prefixlen = 129 + (rand() % (256 - 129)) ;

          memcpy(&p1, &px, sizeof(struct prefix_ipv6)) ;
          memcpy(&p2, &px, sizeof(struct prefix_ipv6)) ;

          apply_mask_ipv6(&p1) ;

          if (plen < 128)
            {
              uint    i ;
              uint8_t m ;

              m = ~(uint16_t)0 << (8 - (plen % 8)) ;

              for (i = plen / 8 ; i < 16 ; ++i)
                {
                  p2.prefix.s6_addr[i] &= m ;
                  m = 0 ;
                } ;
            } ;

          ++ct->total ;

          if (memcmp(&p2, &p1, sizeof(struct prefix_ipv6)) == 0)
            {
              ++ct->good ;
            }
          else
            {
              ++ct->errs ;

              fprintf(stderr, "\n *** for %3u:", px.prefixlen) ;
              for (i = 0 ; i < 16 ; ++i)
                fprintf(stderr, " %02x", px.prefix.s6_addr[i]) ;

              fprintf(stderr, "\n      expect:") ;
              for (i = 0 ; i < 16 ; ++i)
                fprintf(stderr, " %02x", p2.prefix.s6_addr[i]) ;

              fprintf(stderr, "\n         got:") ;
              for (i = 0 ; i < 16 ; ++i)
                fprintf(stderr, " %02x", p1.prefix.s6_addr[i]) ;
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * prefix_to_raw() & prefix_from_raw() -- converts prefix to/from raw form
 */
static void
test_prefix_raw_ipv4(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_to/from_raw() IPv4") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      struct prefix p1[1] ;
      prefix_raw_t raw_in ;
      prefix_raw_t raw_out ;
      prefix_raw_t raw_exp ;
      const byte* pm ;
      uint i ;
      bool ok ;

      pm = mask_n[plen] ;

      /* We generate raw_in which are full to the brim (256) bits of rubbish,
       * and *ensure* that all bits beyond the plen are set '1' !
       *
       * Only plen 0..32 are kosher, so expect final score of 33/256.
       *
       * The raw_out is only significant for the prefix_len part, but we fill
       * that with the opposite of the raw-in, to make sure it is not set
       * by accident.
       *
       * The raw_exp is the bits of raw_in which are valid.
       */
      raw_in->prefix_len  = plen ;
      raw_out->prefix_len = ~plen ;
      raw_exp->prefix_len = (plen <= 32) ? plen : 32 ;

      for (i = 0 ; i < (256/8) ; ++i)
        {
          if (i < 16)
            {
              raw_in->prefix[i]  = (rand() & 0xFF) | ~ pm[i] ;
              raw_exp->prefix[i] = raw_in->prefix[i] & pm[i] ;
            }
          else
            {
              raw_in->prefix[i]  = 0xFF ;
              raw_exp->prefix[i] = 0 ;
            }

          raw_out->prefix[i] = ~raw_in->prefix[i] ;
        } ;

      prefix_from_raw(p1, raw_in, AF_INET) ;

      ++ct->total ;
      if ((p1->family == AF_INET) && (p1->prefixlen == raw_exp->prefix_len)
                                  && (memcmp(p1->u.b, raw_exp->prefix, 4) == 0))
        {
          ok = true ;
        }
      else
        {
          uint i ;

          ++ct->errs ;
          ok = false ;

          fprintf(stderr, "\n *** raw=%3u:", raw_in->prefix_len) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          if (p1->family != AF_INET)
            fprintf(stderr, "\n   expect AF=%u got %u", AF_INET, p1->family) ;

          fprintf(stderr, "\n     got=%3u:", p1->prefixlen) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", p1->u.b[i]) ;
        } ;

      prefix_to_raw(raw_out, p1) ;

      if ((raw_exp->prefix_len == raw_out->prefix_len)
          && ( (raw_exp->prefix_len == 0)
               || (memcmp(raw_exp->prefix, raw_exp->prefix,
                                        (raw_exp->prefix_len + 7u) / 8) == 0) ))
        {
          if (ok && (plen <= 32))
            ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** raw=%3u:", raw_in->prefix_len) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          fprintf(stderr, "\n     got=%3u:", raw_out->prefix_len) ;
          for (i = 0 ; i < (raw_out->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_out->prefix[i]) ;

          fprintf(stderr, "\n     exp=%3u:", raw_exp->prefix_len) ;
          for (i = 0 ; i < (raw_exp->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_exp->prefix[i]) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * prefix_to_raw() & prefix_from_raw() -- converts prefix to/from raw form
 */
static void
test_prefix_raw_ipv6(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_to/from_raw() IPv6") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      struct prefix p1[1] ;
      prefix_raw_t raw_in ;
      prefix_raw_t raw_out ;
      prefix_raw_t raw_exp ;
      const byte* pm ;
      uint i ;
      bool ok ;

      pm = mask_n[plen] ;

      /* We generate raw_in which are full to the brim (256) bits of rubbish,
       * and *ensure* that all bits beyond the plen are set '1' !
       *
       * Only plen 0..128 are kosher, so expect final score of 129/256.
       *
       * The raw_out is only significant for the prefix_len part, but we fill
       * that with the opposite of the raw-in, to make sure it is not set
       * by accident.
       *
       * The raw_exp is the bits of raw_in which are valid.
       */
      raw_in->prefix_len  = plen ;
      raw_out->prefix_len = ~plen ;
      raw_exp->prefix_len = (plen <= 128) ? plen : 128 ;

      for (i = 0 ; i < (256/8) ; ++i)
        {
          if (i < 16)
            {
              raw_in->prefix[i]  = (rand() & 0xFF) | ~ pm[i] ;
              raw_exp->prefix[i] = raw_in->prefix[i] & pm[i] ;
            }
          else
            {
              raw_in->prefix[i]  = 0xFF ;
              raw_exp->prefix[i] = 0 ;
            }

          raw_out->prefix[i] = ~raw_in->prefix[i] ;
        } ;

      prefix_from_raw(p1, raw_in, AF_INET6) ;

      ++ct->total ;
      if ((p1->family == AF_INET6) && (p1->prefixlen == raw_exp->prefix_len)
                                 && (memcmp(p1->u.b, raw_exp->prefix, 16) == 0))
        {
          ok = true ;
        }
      else
        {
          uint i ;

          ++ct->errs ;
          ok = false ;

          fprintf(stderr, "\n *** raw=%3u:", raw_in->prefix_len) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          if (p1->family != AF_INET6)
            fprintf(stderr, "\n   expect AF=%u got %u", AF_INET6, p1->family) ;

          fprintf(stderr, "\n     got=%3u:", p1->prefixlen) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", p1->u.b[i]) ;
        } ;

      prefix_to_raw(raw_out, p1) ;

      if ((raw_exp->prefix_len == raw_out->prefix_len)
          && ( (raw_exp->prefix_len == 0)
               || (memcmp(raw_exp->prefix, raw_exp->prefix,
                                        (raw_exp->prefix_len + 7u) / 8) == 0) ))
        {
          if (ok && (plen <= 128))
            ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** raw=%3u:", raw_in->prefix_len) ;
          for (i = 0 ; i < 4 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          fprintf(stderr, "\n     got=%3u:", raw_out->prefix_len) ;
          for (i = 0 ; i < (raw_out->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_out->prefix[i]) ;

          fprintf(stderr, "\n     exp=%3u:", raw_exp->prefix_len) ;
          for (i = 0 ; i < (raw_exp->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_exp->prefix[i]) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * prefix_match() tests whether prefix p1 includes (or is equal to) p2.
 *
 * NB: takes no notice of the prefixes' families.
 */
static void
test_prefix_match(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_match()") ;
  for (plen = 0 ; plen <= 128 ; ++plen)
    {
      struct prefix p1[1] ;
      struct prefix p2[1] ;
      uint i ;
      uint p2_len ;

      /* For prefix lengths 0..129 full p1 and p2 with rubbish, and the set
       * the two prefix addresses to be equal.
       */
      for (i = 0 ; i < sizeof(struct prefix) ; ++i)
        {
          ((byte*)p1)[i] = rand() & 0xFF ;
          ((byte*)p2)[i] = rand() & 0xFF ;
        } ;

      for (i = 0 ; i < 16 ; ++i)
        p1->u.b[i] = p2->u.b[i] ;

      /* We run a test for p1 taking plen (0..128), and p2 taking prefix
       * lengths plen-1..128 (or 0..128) -- 8513 cases.
       *
       * For each case we perturb zero out of 128 bits, and then one of the
       * 128 bits for all possible bits -- 129 tests.
       *
       * Total of 8513 * 129 = 1,098,177 tests.
       *
       * For plen == 0 everything matches: so 129 cases * 129 tests
       *          == 1 everything but 1       128 cases * 129 -   1
       *          == 2 everything but 2       127 cases * 129 -   2
       *          == 3                3       126 cases * 129 -   3
       *          ....
       *          == 127              127       2 cases * 129 - 127
       *          == 128              128       1 cases * 129 - 128
       *
       * So expect sum(n*n) dir n = 1..129 == 723,905 tests to be good !
       *
       * Score should be 723905/1098177
       */
      p1->prefixlen = plen ;

      for (p2_len = (plen == 0) ? 0 : plen -1 ; p2_len <= 128 ; ++p2_len)
        {
          uint px ;

          p2->prefixlen = p2_len ;

          for (px = 0 ; px <= 128 ; ++px)
            {
              int get, expect ;

              if (px < 128)
                flip_pbit(p1->u.b, px) ;        /* perturb bit just beyond
                                                 * prefix of length px. */
              get = prefix_match(p1, p2) ;

              expect = (p2_len >= plen) && (px >= plen) ? 1 : 0 ;

              ++ct->total ;
              if (get == expect)
                {
                  if (expect == 1)
                    ++ct->good ;
                }
              else
                {
                  uint i ;
                  ++ct->errs ;
                  fprintf(stderr, "\n *** expected %d but got %d (px = %u)",
                                                              expect, get, px) ;
                  fprintf(stderr, "\n         p1=%3u:", p1->prefixlen) ;
                  for (i = 0 ; i < 16 ; ++i)
                    fprintf(stderr, " %02x", p1->u.b[i]) ;

                  fprintf(stderr, "\n         p2=%3u:", p2->prefixlen) ;
                  for (i = 0 ; i < 16 ; ++i)
                    fprintf(stderr, " %02x", p2->u.b[i]) ;
                } ;

              if (px < 128)
                flip_pbit(p2->u.b, px) ;        /* keep p1 & p2 equal   */
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Test prefix_same()
 *
 * Family, Prefix Length and *entire* Prefix must be the same.
 *
 * And Family must be known.  But does not check Prefix Length is valid !
 */
static void
test_prefix_same(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_same()") ;
  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      struct prefix p1[1] ;
      struct prefix p2[1] ;
      uint i ;
      uint fi ;
      sa_family_t fam[] = { AF_UNSPEC, AF_INET, AF_INET6, 99 } ;
                                        /* NB: sa_family_t may be uint8_t ! */

      for (i = 0 ; i < sizeof(struct prefix) ; ++i)
        {
          ((byte*)p1)[i] = rand() & 0xFF ;
          ((byte*)p2)[i] = rand() & 0xFF ;
        } ;

      /* We set p1 to the above 4 families.
       *           prefix length to all possible 0..255.
       *           prefix to a random value
       *
       * We set p2 family, 3 cases: the same as p1
       *                             slightly different (AF_INET <=> AF_INET6)
       *                             random, but different
       *
       *        p2 prefix length, 4 cases: the same as p1
       *                                   one more (mod 256)
       *                                   one less (mod 256)
       *                                   random, but different
       *
       *        p2 address, 3 cases: the same as p1 -- for the family length
       *                             one bit different within family length
       *                             random, but different
       *
       * Score should be: 512/36864
       */
      for (fi = 0 ; fi < (sizeof(fam)/sizeof(fam[0])) ; ++fi)
        {
          sa_family_t fam1 ;
          bool fam_known ;
          uint fam_len ;
          uint ft, plt, pt ;

          fam1 = fam[fi] ;

          switch (fam1)
            {
              case AF_INET:
                fam_len = 32 ;
                fam_known = true ;
                break ;

              case AF_INET6:
                fam_len = 128 ;
                fam_known = true ;
                break ;

              default:
                fam_len   = 128 ;
                fam_known = false ;
                break ;
            } ;

          for (ft = 0 ; ft < 3 ; ++ft)
            {
              sa_family_t fam2 ;

              switch (ft)
                {
                  case 0:
                    fam2 = fam1 ;
                    break ;

                  case 1:
                    switch (fam1)
                      {
                        case AF_INET:
                          fam2 = AF_INET6 ;
                          break ;

                        case AF_INET6:
                          fam2 = AF_INET ;
                          break ;

                        default:
                          fam2 = fam1 + 1 ;
                          break ;
                      } ;
                    break ;

                  case 2:
                    fam1 = rand() % 31415 ;
                    do
                      fam2 = rand() % 31415 ;
                    while (fam2 == fam1) ;
                    break ;
                } ;

              for (plt = 0 ; plt < 4 ; ++plt)
                {
                  uint plen2 ;

                  switch (plt)
                    {
                      case 0:
                        plen2 = plen ;
                        break ;

                      case 1:
                        if (plen < 255)
                          plen2 = plen + 1 ;
                        else
                          plen2 = 0 ;
                        break ;

                      case 2:
                        if (plen > 0)
                          plen2 = plen - 1 ;
                        else
                          plen2 = 255 ;
                        break ;

                      case 3:
                        do
                          plen2 = rand() % 256 ;
                        while (plen2 == plen) ;
                    } ;

                  p1->family    = fam1 ;
                  p2->family    = fam2 ;
                  p1->prefixlen = plen ;
                  p2->prefixlen = plen2 ;

                  for (pt = 0 ; pt < 3 ; ++pt)
                    {
                      int expect, get ;

                      i = 0 ;

                      if (pt < 2)
                        {
                          for (i = 0 ; i < (fam_len / 8) ; ++i)
                            p1->u.b[i] = p2->u.b[i] ;

                        } ;

                      if (pt == 1)
                        p2->u.b[rand() % i] ^= (1 << (rand() % 8)) ;

                      for (; i < 16 ; ++i)
                        {
                          byte d ;
                          do
                            d = rand() % 256 ;
                          while (d == 0) ;

                          p1->u.b[i] = p2->u.b[i] ^ d ;
                        } ;

                      expect = fam_known && (ft == 0) && (plt == 0)
                                                      && (pt == 0) ? 1 : 0 ;
                      get = prefix_same(p1, p2) ;

                      ++ct->total ;
                      if (get == expect)
                        {
                          if (expect == 1)
                            ++ct->good ;
                        }
                      else
                        {
                          uint i ;
                          ++ct->errs ;
                          fprintf(stderr, "\n *** expected %d but got %d"""
                                             " (plen=%u, ft=%u, plt=%u, pt=%u)",
                                               expect, get, plen, ft, plt, pt) ;
                          fprintf(stderr, "\n         p1=%3u %3u:",
                                                    p1->family, p1->prefixlen) ;
                          for (i = 0 ; i < 16 ; ++i)
                            fprintf(stderr, " %02x", p1->u.b[i]) ;

                          fprintf(stderr, "\n         p2=%3u %3u:",
                                                    p2->family, p2->prefixlen) ;
                          for (i = 0 ; i < 16 ; ++i)
                            fprintf(stderr, " %02x", p2->u.b[i]) ;
                        } ;
                    } ;
                } ;
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Test prefix_cmp()
 *
 * Family, Prefix Length and Prefix under mask must be the same.
 *
 * Does not care what the Family is and does not check that Prefix Length is
 * feasible (either for the Family or for the size of the struct prefix !)
 */
static void
test_prefix_cmp(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_cmp()") ;
  for (plen = 0 ; plen <= 128 ; ++plen)
    {
      struct prefix p1[1] ;
      struct prefix p2[1] ;
      uint i ;
      uint ft, plt, pt ;

      for (i = 0 ; i < sizeof(struct prefix) ; ++i)
        {
          ((byte*)p1)[i] = rand() & 0xFF ;
          ((byte*)p2)[i] = rand() & 0xFF ;
        } ;

      /* For each prefix length 0..128
       *
       *   we try 2 family cases: same
       *                          randomly different
       *          4 prefix length cases: same
       *                                 one greater (modulo 129)
       *                                 one less    (modulo 129)
       *                                 randomly different
       *
       * Sets both prefix addresses equal, then perturbs 1 out of 129 bits.
       *
       * So 129 * 2 * 4 * 129 = 133128 tests
       *
       * Of which for prefix length 0, all 129 perturbed bits pass
       *                            1, 129 - 1 perturbed bits pass...
       *                            .....
       * so: score 8385/133128
       */
      for (ft = 0 ; ft < 2 ; ++ft)
        {
          sa_family_t fam1, fam2 ;

          switch (ft)
            {
              case 0:
                fam2 = fam1 = rand() % 65536 ;
                break ;

              case 1:
                fam1 = rand() % 65536 ;
                do
                  fam2 = fam1 ^ rand() % 65536 ;
                while (fam2 == fam1) ;
                break ;
            } ;

          for (plt = 0 ; plt < 4 ; ++plt)
            {
              uint plen2 ;

              switch (plt)
                {
                  case 0:
                    plen2 = plen ;
                    break ;

                  case 1:
                    if (plen < 128)
                      plen2 = plen + 1 ;
                    else
                      plen2 = 0 ;
                    break ;

                  case 2:
                    if (plen > 1)
                      plen2 = plen - 1 ;
                    else
                      plen2 = 128 ;
                    break ;

                  case 3:
                    do
                      plen2 = rand() % 129 ;
                    while (plen2 == plen) ;
                } ;

              p1->family    = fam1 ;
              p2->family    = fam2 ;
              p1->prefixlen = plen ;
              p2->prefixlen = plen2 ;

              for (i = 0 ; i < (128 / 8) ; ++i)
                p1->u.b[i] = p2->u.b[i] = rand() % 256 ;

              for (pt = 0 ; pt <= 128 ; ++pt)
                {
                  int expect, get ;

                  if (pt < 128)
                    flip_pbit(p1->u.b, pt) ;        /* perturb bit just beyond
                                                     * prefix of length px. */

                  expect = (ft == 0) && (plt == 0) && (pt >= plen) ? 0 : 1 ;
                  get = prefix_cmp(p1, p2) ;

                  ++ct->total ;
                  if (get == expect)
                    {
                      if (expect == 0)
                        ++ct->good ;
                    }
                  else
                    {
                      uint i ;
                      ++ct->errs ;
                      fprintf(stderr, "\n *** expected %d but got %d"""
                                         " (plen=%u, ft=%u, plt=%u, pt=%u)",
                                           expect, get, plen, ft, plt, pt) ;
                      fprintf(stderr, "\n         p1=%3u %3u:",
                                                p1->family, p1->prefixlen) ;
                      for (i = 0 ; i < 16 ; ++i)
                        fprintf(stderr, " %02x", p1->u.b[i]) ;

                      fprintf(stderr, "\n         p2=%3u %3u:",
                                                p2->family, p2->prefixlen) ;
                      for (i = 0 ; i < 16 ; ++i)
                        fprintf(stderr, " %02x", p2->u.b[i]) ;
                    } ;

                  if (pt < 128)
                    flip_pbit(p2->u.b, pt) ;        /* keep p1 & p2 equal   */
                } ;
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Test prefix_common_bits()
 *
 * Counts leading equal bits on the prefix address, ignoring the prefix
 * length... counting only within the known address family length.
 *
 * Does not use or check the Prefix Length.
 */
static void
test_prefix_common_bits(counts ct)
{
  uint plen ;

  fprintf(stderr, "Test prefix_common_bits()") ;
  for (plen = 0 ; plen <= 128 ; ++plen)
    {
      struct prefix p1[1] ;
      struct prefix p2[1] ;
      uint i ;
      uint fi ;
      sa_family_t fam[] = { AF_UNSPEC, AF_INET, AF_INET6, 77 } ;
                                        /* NB: sa_family_t may be uint8_t ! */

      for (i = 0 ; i < sizeof(struct prefix) ; ++i)
        {
          ((byte*)p1)[i] = rand() & 0xFF ;
          ((byte*)p2)[i] = rand() & 0xFF ;
        } ;

      /* We set p1 to the above 4 families.
       *           prefix length to all possible 0..128.
       *           prefix to a random value
       *
       * We set p2 family, 3 cases: the same as p1
       *                             slightly different (AF_INET <=> AF_INET6)
       *                             random, but different
       *
       *        p2 prefix length, 4 cases: the same as p1
       *                                   one more (mod 256)
       *                                   one less (mod 256)
       *                                   random, but different
       *
       *        p2 address, family length + 1 cases
       *                    set all bits beyond family length to some random
       *                    value.
       *
       *                    Then no bits different, and then 1 bit different
       *                    for all possible family length bits, and randomly
       *                    different thereafter.
       *
       * Score should be:  83592/650160
       */
      for (fi = 0 ; fi < (sizeof(fam)/sizeof(fam[0])) ; ++fi)
        {
          sa_family_t fam1 ;
          bool fam_known ;
          uint fam_len ;
          uint ft, plt, pt ;

          fam1 = fam[fi] ;

          switch (fam1)
            {
              case AF_INET:
                fam_len   = 32 ;
                fam_known = true ;
                break ;

              case AF_INET6:
                fam_len   = 128 ;
                fam_known = true ;
                break ;

              default:
                fam_len   = 128 ;
                fam_known = false ;
                break ;
            } ;

          for (ft = 0 ; ft < 3 ; ++ft)
            {
              sa_family_t fam2 ;

              switch (ft)
                {
                  case 0:
                    fam2 = fam1 ;
                    break ;

                  case 1:
                    switch (fam1)
                      {
                        case AF_INET:
                          fam2 = AF_INET6 ;
                          break ;

                        case AF_INET6:
                          fam2 = AF_INET ;
                          break ;

                        default:
                          fam2 = fam1 + 1 ;
                          break ;
                      } ;
                    break ;

                  case 2:
                    fam1 = rand() % 31415 ;
                    do
                      fam2 = rand() % 31415 ;
                    while (fam2 == fam1) ;
                    break ;
                } ;

              for (plt = 0 ; plt < 4 ; ++plt)
                {
                  uint plen2 ;

                  switch (plt)
                    {
                      case 0:
                        plen2 = plen ;
                        break ;

                      case 1:
                        if (plen < 255)
                          plen2 = plen + 1 ;
                        else
                          plen2 = 0 ;
                        break ;

                      case 2:
                        if (plen > 0)
                          plen2 = plen - 1 ;
                        else
                          plen2 = 255 ;
                        break ;

                      case 3:
                        do
                          plen2 = rand() % 256 ;
                        while (plen2 == plen) ;
                    } ;

                  p1->family    = fam1 ;
                  p2->family    = fam2 ;
                  p1->prefixlen = plen ;
                  p2->prefixlen = plen2 ;

                  for (i = 0 ; i < 16 ; ++i)
                    p1->u.b[i] = rand() & 0xFF ;

                  for (i = 0 ; i < (fam_len / 8) ; ++i)
                    p2->u.b[i] = p1->u.b[i] ;

                  for (; i < 16 ; ++i)
                    {
                      byte d ;
                      do
                        d = rand() % 256 ;
                      while (d == 0) ;

                      p2->u.b[i] = p1->u.b[i] ^ d ;
                    } ;

                  for (pt = 0 ; pt <= fam_len ; ++pt)
                    {
                      struct prefix px[1] ;
                      int expect, get ;
                      uint pr ;

                      memcpy(px->u.b, p2->u.b, 16) ;

                      if (pt < 128)
                        flip_pbit(p2->u.b, pt) ;    /* perturb bit just beyond
                                                     * prefix of length pt. */
                      for (pr = pt + 1 ; pr < 128 ; ++pr)
                        if (rand() & 1)
                          flip_pbit(p2->u.b, pr) ;  /* perturb bit beyond
                                                     * prefix of length pt. */

                      expect = fam_known && (ft == 0) ? (int)pt : -1 ;
                      get = prefix_common_bits(p1, p2) ;

                      ++ct->total ;
                      if (get == expect)
                        {
                          if (expect != -1)
                            ++ct->good ;
                        }
                      else
                        {
                          uint i ;
                          ++ct->errs ;
                          fprintf(stderr, "\n *** expected %d but got %d"""
                                             " (plen=%u, ft=%u, plt=%u, pt=%u)",
                                               expect, get, plen, ft, plt, pt) ;
                          fprintf(stderr, "\n         p1=%3u %3u:",
                                                    p1->family, p1->prefixlen) ;
                          for (i = 0 ; i < 16 ; ++i)
                            fprintf(stderr, " %02x", p1->u.b[i]) ;

                          fprintf(stderr, "\n         p2=%3u %3u:",
                                                    p2->family, p2->prefixlen) ;
                          for (i = 0 ; i < 16 ; ++i)
                            fprintf(stderr, " %02x", p2->u.b[i]) ;
                        } ;

                      memcpy(p2->u.b, px->u.b, 16) ;    /* recover       */
                    } ;
                } ;
            } ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Some tests of spfxtoa() str2prefix ()
 *
 * All 512 tests pass "good"
 */
static void
test_spfxtoa_str2prefix(counts ct)
{
  struct prefix p1[1] ;
  uint plen ;
  const char* unknown ;
  char  buffer[100] ;

  fprintf(stderr, "Test spfxtoa & str2prefix") ;

  memset(p1, 0xA5, sizeof(struct prefix)) ;

  snprintf(buffer, sizeof(buffer), "'%s'", spfxtoa(p1).str) ;

  unknown = "'?unknown address family=42405?'" ;
  if (strcmp(buffer, unknown))
    {
      ++ct->errs ;
      fprintf(stderr, "\n *** for unknown AF=0xA5 expect: %s", unknown) ;
      fprintf(stderr, "\n                            got: %s", buffer) ;
    } ;

  for (plen = 0 ; plen <= 255 ; ++plen)
    {
      uint i ;
      prefix_raw_t raw_in ;
      prefix_raw_t raw_out ;
      const byte* pm ;

      pm = mask_n[plen] ;

      memset(&raw_in,     0xA5, sizeof(prefix_raw_t)) ;
      memset(&raw_out,    0x5A, sizeof(prefix_raw_t)) ;

      raw_in->prefix_len = plen ;

      for (i = 0 ; i < 16 ; ++i)
        raw_in->prefix[i] = rand() & pm[i] ;

      /* AF_INET
       */
      raw_in->prefix_len = (plen <= 32) ? plen : 32 ;

      prefix_from_raw(p1, raw_in, AF_INET) ;
      snprintf(buffer, sizeof(buffer), "%s", spfxtoa(p1).str) ;
      str2prefix(buffer, p1) ;
      prefix_to_raw(raw_out, p1) ;

      ++ct->total ;
      if ((raw_in->prefix_len == raw_out->prefix_len)
          && ( (raw_in->prefix_len == 0)
              || (memcmp(raw_in->prefix, raw_out->prefix,
                                        (raw_out->prefix_len + 7u) / 8) == 0) ))
        {
          ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** for AF_INET raw=0x%02x", raw_in->prefix_len) ;
          for (i = 0 ; i < (raw_in->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          fprintf(stderr, "\n                 got=0x%02x",
                                                          raw_out->prefix_len) ;
          for (i = 0 ; i < (raw_out->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_out->prefix[i]) ;
        } ;

      /* AF_INET6
       */
      raw_in->prefix_len = (plen <= 128) ? plen : 128 ;

      prefix_from_raw(p1, raw_in, AF_INET6) ;
      snprintf(buffer, sizeof(buffer), "%s", spfxtoa(p1).str) ;
      str2prefix(buffer, p1) ;
      prefix_to_raw(raw_out, p1) ;

      ++ct->total ;
      if ((raw_in->prefix_len == raw_out->prefix_len)
          || ( (raw_in->prefix_len == 0)
            || (memcmp(raw_in->prefix, raw_out->prefix,
                                         (raw_out->prefix_len + 7u) / 8) ==0) ))
        {
          ++ct->good ;
        }
      else
        {
          uint i ;
          ++ct->errs ;
          fprintf(stderr, "\n *** for AF_INET6 raw=0x%02x", raw_in->prefix_len);
          for (i = 0 ; i < (raw_in->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_in->prefix[i]) ;

          fprintf(stderr, "\n                 got=0x%02x",
                                                          raw_out->prefix_len) ;
          for (i = 0 ; i < (raw_out->prefix_len + 7u) / 8 ; ++i)
            fprintf(stderr, " %02x", raw_out->prefix[i]) ;
        } ;
   } ;
} ;

/*==============================================================================
 * Utilities
 */

/*------------------------------------------------------------------------------
 * Return state of given prefix bit -- bits are numbered as per prefix length.
 *
 * Bit 0 is the ms bit -- the first bit *after* a prefix of length 0.
 */
static uint
get_pbit(byte* v, uint bn)
{
  byte b ;

  b = 0x80 >> (bn % 8) ;

  return (v[bn / 8] & b) ? 1 : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set given prefix bit -- bits are numbered as per prefix length.
 *
 * Bit 0 is the ms bit -- the first bit *after* a prefix of length 0.
 */
static void
set_pbit(byte* v, uint bn)
{
  byte b ;

  b = 0x80 >> (bn % 8) ;

  v[bn / 8] |= b ;
} ;

/*------------------------------------------------------------------------------
 * Clear given prefix bit -- bits are numbered as per prefix length.
 *
 * Bit 0 is the ms bit -- the first bit *after* a prefix of length 0.
 */
static void
clear_pbit(byte* v, uint bn)
{
  byte b ;

  b = 0x80 >> (bn % 8) ;

  v[bn / 8] &= ~b ;
} ;

/*------------------------------------------------------------------------------
 * Flip given prefix bit -- bits are numbered as per prefix length.
 *
 * Bit 0 is the ms bit -- the first bit *after* a prefix of length 0.
 */
static void
flip_pbit(byte* v, uint bn)
{
  byte b ;

  b = 0x80 >> (bn % 8) ;

  v[bn / 8] ^= b ;
} ;


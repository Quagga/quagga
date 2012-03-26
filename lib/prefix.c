/*
 * Prefix related functions.
 * Copyright (C) 1997, 98, 99 Kunihiro Ishiguro
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

#include <zebra.h>

#include "prefix.h"
#include "vty.h"
#include "sockunion.h"
#include "memory.h"
#include "log.h"
#include "tstring.h"
#include "string.h"

/* Some advantages with __GNUC__ !
 *
 * Can set __GNUC_LOCAL to 0 for testing
 */
#ifdef __GNUC__
#define __GNUC__LOCAL 1
#else
#define __GNUC__LOCAL 0
#endif

/* Maskbit -- mask for last significant byte of a prefix: maskbit[len % 8]
 */
static const uint8_t maskbit[] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0,
			                 0xf8, 0xfc, 0xfe, 0xff };

/*==============================================================================
 * "Macros" for banging 32 and 64 bits of masks
 */
              /* 0123456701234567 */
#define U32_1s (uint32_t)0xFFFFFFFF
#define U64_1s (uint64_t)0xFFFFFFFFFFFFFFFF

inline static uint32_t n32_mask(uint len)              Always_Inline ;
inline static u_char n32_mask_check (uint32_t mask_n)  Always_Inline ;
inline static u_char n64_mask_check (uint64_t mask_n)  Always_Inline ;

inline static uint8_t local_clz_n32(uint32_t n32)      Always_Inline ;
inline static uint8_t local_clz_u32(uint32_t u32)      Always_Inline ;
static uint8_t local_clz_u32_long(uint32_t u32) ;

inline static uint8_t local_clz_n64(uint64_t n64)      Always_Inline ;

/*------------------------------------------------------------------------------
 * Return 32 bit mask
 */
inline static uint32_t
n32_mask(uint len)
{
  return (len < 32) ? ~htonl((U32_1s >> len))
                    :         U32_1s ;
} ;

/*------------------------------------------------------------------------------
 * Check whether given uint32_t is valid as a netmask.
 *
 * Netmask is valid if there are no '1' bits after the LS '0' (if any)
 *
 * Argument should be network byte order.
 */
inline static u_char
n32_mask_check (uint32_t mask_n)
{
  uint32_t mask_h ;

  mask_h = ntohl(mask_n) ;

  return (mask_h | (mask_h - 1)) == U32_1s ;

  /* So: where ip_h has at some unknown MS bits, and then '1' followed by
   *     '0's we have:
   *
   *        'X..X10..0' - 1 -> 'X..X01..1'
   *
   *     so to be a valid mask: 'X..X10..0' | 'X..X01..1' == '1..1' !
   *
   *     ip_h is unsigned, so if ip_h == 0
   *
   *        '0..0' - 1 -> '1..1' and '0..0' | '1..1' == '1..1'
   *
   *     so that's fine too.
   */
} ;

/*------------------------------------------------------------------------------
 * Check whether given uint32_t is valid as a netmask.
 *
 * Netmask is valid if there are no '1' bits after the LS '0' (if any)
 *
 * Argument should be network byte order.
 */
inline static u_char
n64_mask_check (uint64_t mask_n)
{
  uint64_t mask_h ;

  mask_h = ntohq(mask_n) ;

  return (mask_h | (mask_h - 1)) == U64_1s ;
} ;

/*------------------------------------------------------------------------------
 * Wrapper for __builtin_clz() for 32-bit Network Order value
 *
 * NB: *undefined* result for n32 == 0
 */
inline static uint8_t
local_clz_n32(uint32_t n32)
{
  return local_clz_u32(ntohl(n32)) ;
} ;

/*------------------------------------------------------------------------------
 * Wrapper for __builtin_clz() for 32-bit Host Order value
 *
 * NB: *undefined* result for u32 == 0
 */
inline static uint8_t
local_clz_u32(uint32_t u32)
{
  /* Expect the compiler to reap the unused code here.
   *
   * Done this way to ensure that the obscure code is kept up to date !
   */
  if (__GNUC__LOCAL)
    {
      confirm(UINT_MAX == U32_1s) ;

#if __GNUC__LOCAL
      return __builtin_clz(u32) ;
#else
      assert(false) ;                   /* CANNOT reach here !! */
#endif
    }
  else
    {
      /* NB: we try to use ffs() if we can.  We want to count the leading
       *     zeros, so we can only do this if have '0's followed by '1's,
       *     which is the case for valid prefix masks, and is the case we
       *     want to handle most quickly.
       *
       *     If we give ffs() zero we get 0, which is completely wrong, so
       *     need to look out for u32 = 0xFFFFFFFF.  Since we have to do that,
       *     we deal with all the cases where the result is zero, and for
       *     good measure we deal with the one case where we would present
       *     ffs() with a value > 0x7FFFFFFF -- because ffs() technically
       *     takes an int !
       */
      if (u32 >= 0x7FFFFFFF)
        return (u32 > 0x7FFFFFFF) ? 0 : 1 ;

      if ((u32 & (u32 + 1)) == 0)
        return 33 - ffs(u32 + 1) ;

      return local_clz_u32_long(u32) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Long alternative to __builtin_clz() for 32-bit (Host Order) value
 *
 * For use where u32 is *not* '1's followed by '0's, only -- so cannot use
 * ffs() because
 *
 * NB: *undefined* result for u32 == 0
 */
static uint8_t
local_clz_u32_long(uint32_t u32)
{
  uint8_t n ;

  if (u32 > 0x0000FFFF)
    {
      if (u32 > 0x00FFFFFF)
        {
          if (u32 > 0x0FFFFFFF)
            {
              n   = (28    - 28) ;
              u32 = (u32 >> (28 - 1)) ;
            }
          else
            {
              n   = (28    - 24) ;
              u32 = (u32 >> (24 - 1)) ;
            } ;
        }
      else
        {
          if (u32 > 0x000FFFFF)
            {
              n   = (28    - 20) ;
              u32 = (u32 >> (20 - 1)) ;
            }
          else
            {
              n   = (28    - 16) ;
              u32 = (u32 >> (16 - 1)) ;
            } ;
        }
    }
  else
    {
      if (u32 > 0x000000FF)
        {
          if (u32 > 0x00000FFF)
            {
              n   = (28    - 12) ;
              u32 = (u32 >> (12 - 1)) ;
            }
          else
            {
              n   = (28    -  8) ;
              u32 = (u32 >> ( 8 - 1)) ;
            } ;
        }
      else
        {
          if (u32 > 0x0000000F)
            {
              n   = (28    -  4) ;
              u32 = (u32 >> ( 4 - 1)) ;
            }
          else
            {
              n   = (28    -  0) ;
              u32 = (u32 << 1) ;
            } ;
        }
    } ;

  return n + ((0x000055AC >> (u32 & 0x1E)) & 0x3) ;
} ;

/*------------------------------------------------------------------------------
 * Wrapper for __builtin_clz() for 64-bit Network Order value
 *
 * NB: *undefined* result for n64 == 0
 */
inline static uint8_t
local_clz_n64(uint64_t n64)
{
  uint64_t u64 ;

  u64 = ntohq(n64) ;

  /* Expect the compiler to reap the unused code here.
   *
   * Done this way to ensure that the obscure code is kept up to date !
   */
  if (__GNUC__LOCAL)
    {
      confirm(ULONG_MAX == U64_1s) ;

#if __GNUC__LOCAL
      return __builtin_clzl(u64) ;
#else
      assert(false) ;                   /* CANNOT reach here !! */
#endif
    }
  else
    {
      /* Break this down so that can operate of 32 bit parts.
       */
      if (u64 > U32_1s)
        return local_clz_u32(u64 >> 32) ;
      else
        return 32 + local_clz_u32(u64) ;
    } ;
} ;

/*==============================================================================
 */

/* Number of bits in prefix type. */
#ifndef PNBBY
#define PNBBY 8
#endif /* PNBBY */

#define MASKBIT(offset)  ((0xff << (PNBBY - (offset))) & 0xff)

unsigned int
prefix_bit (const u_char *prefix, const u_char prefixlen)
{
  uint offset = prefixlen / PNBBY;
  uint shift  = (PNBBY - 1) - (prefixlen % PNBBY);

  return (prefix[offset] >> shift) & 1;
}

#ifdef HAVE_IPV6

unsigned int
prefix6_bit (const struct in6_addr *prefix, const u_char prefixlen)
{
  return prefix_bit((const u_char *) &prefix->s6_addr, prefixlen);
}

#endif /* HAVE_IPV6 */

/* Address Famiy Identifier to Address Family converter. */
extern sa_family_t
afi2family (afi_t afi)
{
  switch (afi)
    {
      case AFI_IP:
        return AF_INET;

#ifdef HAVE_IPV6
      case AFI_IP6:
        return AF_INET6;
#endif /* HAVE_IPV6 */

      default:
        return AF_UNSPEC;

        confirm(AF_UNSPEC == 0) ;
    } ;
}

extern afi_t
family2afi (sa_family_t family)
{
  switch (family)
    {
      case AF_INET:
        return AFI_IP;

#ifdef HAVE_IPV6
      case AF_INET6:
        return AFI_IP6;
#endif /* HAVE_IPV6 */

      default:
        return AFI_RESERVED;

        confirm(AFI_RESERVED == 0) ;
    } ;
}

/* If n includes p prefix then return 1 else return 0.
 *
 * NB: this takes no notice of the prefixes' families.
 */
int
prefix_match (const struct prefix *n, const struct prefix *p)
{
  uint i, m ;

  if (n->prefixlen > p->prefixlen)
    return 0;

  i = n->prefixlen / 32 ;
  m = n->prefixlen % 32 ;

  if (m != 0)
    if (ntohl(n->u.n32[i] ^ p->u.n32[i]) > (0xFFFFFFFF >> m))
      return 0 ;

  while (i-- != 0)
    if (n->u.n32[i] != p->u.n32[i])
      return 0;

  return 1;             /* match        */
}

/* Copy prefix from src to dest. */
void
prefix_copy (struct prefix *dest, const struct prefix *src)
{
  dest->family    = src->family;
  dest->prefixlen = src->prefixlen;

  switch (src->family)
    {
      case AF_INET:
        dest->u.prefix4 = src->u.prefix4;
        break ;

#ifdef HAVE_IPV6
      case AF_INET6:
        dest->u.prefix6 = src->u.prefix6;
        break ;
#endif /* HAVE_IPV6 */

      case AF_UNSPEC:
        dest->u.lp.id         = src->u.lp.id;
        dest->u.lp.adv_router = src->u.lp.adv_router;
        break ;

      default:
        zlog (NULL, LOG_ERR, "prefix_copy(): Unknown address family %d",
	                                                           src->family);
        assert (0);
    } ;
}

/*
 * Return 1 if the address/netmask contained in the prefix structure
 * is the same, and else return 0.  For this routine, 'same' requires
 * that not only the prefix length and the network part be the same,
 * but also the host part.  Thus, 10.0.0.1/8 and 10.0.0.2/8 are not
 * the same.  Note that this routine has the same return value sense
 * as '==' (which is different from prefix_cmp).
 *
 * If the Families are the same, they must be either AF_INET or AF_INET6 --
 * otherwise returns 0.
 *
 * If the Prefix Lengths are different, will return 0.  Takes no notice of
 * what the Prefix Length is, however.  The check for "same" checks the
 * address part specified by the Family.
 */
int
prefix_same (const struct prefix *p1, const struct prefix *p2)
{
  if ((p1->family == p2->family) && (p1->prefixlen == p2->prefixlen))
    {
      switch (p1->family)
      {
        case AF_INET:
          return (IPV4_ADDR_SAME (&p1->u.prefix4.s_addr,
                                  &p2->u.prefix4.s_addr)) ? 1 : 0;
#ifdef HAVE_IPV6
        case AF_INET6:
          return (IPV6_ADDR_SAME (&p1->u.prefix6.s6_addr,
                                  &p2->u.prefix6.s6_addr)) ? 1 : 0;
#endif /* HAVE_IPV6 */

        default:
          break ;
      } ;
    }

  return 0;
}

/*
 * Return 0 if the network prefixes represented by the struct prefix
 * arguments are the same prefix, and 1 otherwise.  Network prefixes
 * are considered the same if the prefix lengths are equal and the
 * network parts are the same.  Host bits (which are considered masked
 * by the prefix length) are not significant.  Thus, 10.0.0.1/8 and
 * 10.0.0.2/8 are considered equivalent by this routine.  Note that
 * this routine has the same return sense as strcmp (which is different
 * from prefix_same).
 *
 * Does not care what the Family is and does not check that Prefix Length is
 * feasible (either for the Family or for the size of the struct prefix !)
 *
 * Whatever the prefix length is, requires the body of the prefix to be some
 * multiple of uint32_t (in future uint64_t and uint128_t !)
 */
int
prefix_cmp (const struct prefix *p1, const struct prefix *p2)
{
  uint i, m ;

  if ((p1->family != p2->family) || (p1->prefixlen != p2->prefixlen))
    return 1;

  i = p1->prefixlen / 32 ;
  m = p1->prefixlen % 32 ;

  if (m != 0)
    if (ntohl(p1->u.n32[i] ^ p2->u.n32[i]) > (U32_1s >> m))
      return 1;

  while (i--)
    if (p1->u.n32[i] != p2->u.n32[i])
      return 1;

  return 0;             /* equal        */
}

/*
 * Count the number of common bits in 2 prefixes. The prefix length is
 * ignored for this function; the whole prefix is compared. If the prefix
 * address families don't match, return -1; otherwise the return value is
 * in range 0 ... maximum prefix length for the address family.
 */
int
prefix_common_bits (const struct prefix *p1, const struct prefix *p2)
{
  uint32_t dn32 ;
#ifdef HAVE_IPV6
  uint64_t dn64 ;
#endif

  if (p1->family != p2->family)
    return -1;

  switch (p1->family)
    {
      case AF_INET:

        dn32 = p1->u.n32[0] ^ p2->u.n32[0] ;

        return (dn32 != 0) ? local_clz_n32(dn32) : 32 ;

#ifdef HAVE_IPV6
      case AF_INET6:
        dn64 = p1->u.n64[0] ^ p2->u.n64[0] ;

        if (dn64 != 0)
          return local_clz_n64(dn64) ;

        dn64 = p1->u.n64[1] ^ p2->u.n64[1] ;

        return (dn64 != 0) ? 64 + local_clz_n64(dn64) : 128 ;
#endif

      default:
        return -1 ;
    } ;
} ;

/* Return prefix family type string. */
const char *
prefix_family_str (const struct prefix *p)
{
  switch (p->family)
    {
      case AF_INET:
        return "inet";

#ifdef HAVE_IPV6
      case AF_INET6:
        return "inet6";
#endif /* HAVE_IPV6 */

      case AF_UNSPEC:
        return "unspec";

      default:
        return "unknown" ;
    } ;
}

/*==============================================================================
 * IPv4 Stuff
 */

/* Allocate new prefix_ipv4 structure. */
struct prefix_ipv4 *
prefix_ipv4_new ()
{
  struct prefix_ipv4 *p;

  /* Call prefix_new to allocate a full-size struct prefix to avoid problems
     where the struct prefix_ipv4 is cast to struct prefix and unallocated
     bytes were being referenced (e.g. in structure assignments). */
  p = (struct prefix_ipv4 *)prefix_new();
  p->family = AF_INET;
  return p;
}

/* Free prefix_ipv4 structure. */
void
prefix_ipv4_free (struct prefix_ipv4 *p)
{
  prefix_free((struct prefix *)p);
}

/* When string format is valid return 1 otherwise return 0.
 *
 * inet_aton() returns 1 <=> valid, 0 <=> invalid.
 * inet_pton() returns 1 <=> valid, 0 <=> invalid, -1 <=> error
 *                                where error => unknown address family argument
 *
 * Callers of this function vary in how they test the return:
 *
 *   1) some treat non-0 as OK and 0 as invalid -- consistent with inet_aton().
 *
 *   2) some treat > 0 as OK and <= 0 as invalid -- consistent with inet_pton().
 *
 * Since this function returns 1 <=> valid and 0 <=> invalid, both the above
 * work.
 */
int
str2prefix_ipv4 (const char *str, struct prefix_ipv4 *p)
{
  tstring_t(ipv4, 24) ;
  char*       pnt ;
  const char* cp ;
  int         ret ;
  unsigned    plen ;

  pnt = strchr (str, '/');

  if (pnt == NULL)
    {
      /* No / => simple address                 */
      plen = IPV4_MAX_BITLEN ;
      cp   = str ;
    }
  else
    {
      /* With / => prefix                       */
      plen = (unsigned)atoi (pnt + 1) ;
      if (plen > IPV4_MAX_PREFIXLEN)
        return 0;

      cp = tstring_set_n(ipv4, str, (pnt - str)) ;
    } ;

  ret = inet_aton (cp, &p->prefix);
  if (ret <= 0)   /* should not return < 0, but it would not be valid ! */
    return 0;

  p->family    = AF_INET;
  p->prefixlen = plen;

  tstring_free(ipv4) ;

  return 1 ;
}

/* Convert masklen into IP address's netmask (network byte order).
 */
extern void
masklen2ip (const uint masklen, struct in_addr *netmask)
{
  netmask->s_addr = n32_mask(masklen) ;

  confirm(IPV4_MAX_BITLEN == 32) ;
} ;

/* Convert IPv4 netmask to prefix length.
 *
 * If the netmask is invalid, all '1's after the first '0' are ignored.
 *
 * Argument netmask should be network byte order.
 */
u_char
ip_masklen (struct in_addr netmask)
{
  return (netmask.s_addr != U32_1s) ? local_clz_n32(~netmask.s_addr) : 32 ;
} ;

/* Check whether given IPv4 netmask is valid.
 *
 * Netmask is valid if there are no '1' bits after the LS '0' (if any)
 *
 * Argument netmask should be network byte order.
 */
bool
ip_mask_check (struct in_addr netmask)
{
  return n32_mask_check(netmask.s_addr) ;
} ;

/* Apply mask to IPv4 prefix (network byte order). */
void
apply_mask_ipv4 (struct prefix_ipv4 *p)
{
  p->prefix.s_addr &= n32_mask(p->prefixlen) ;
}

/* If prefix is 0.0.0.0/0 then return 1 else return 0. */
int
prefix_ipv4_any (const struct prefix_ipv4 *p)
{
  return (p->prefix.s_addr == 0 && p->prefixlen == 0);
}

/*==============================================================================
 * IPv6 Stuff
 */
#ifdef HAVE_IPV6

/* Allocate a new ip version 6 route */
struct prefix_ipv6 *
prefix_ipv6_new (void)
{
  struct prefix_ipv6 *p;

  /* Allocate a full-size struct prefix to avoid problems with structure
     size mismatches. */
  p = (struct prefix_ipv6 *)prefix_new();
  p->family = AF_INET6;
  return p;
}

/* Free prefix for IPv6. */
void
prefix_ipv6_free (struct prefix_ipv6 *p)
{
  prefix_free((struct prefix *)p);
}

/* If given string is valid IPv6 address or prefix return 1 else return 0
 *
 * inet_aton() returns 1 <=> valid, 0 <=> invalid.
 * inet_pton() returns 1 <=> valid, 0 <=> invalid, -1 <=> error
 *                                where error => unknown address family argument
 *
 * Any error returned by inet_pton() is reported as an invalid address or
 * prefix.  So best not to call this if IPv6 is not supported.
 *
 * Callers of this function vary in how they test the return:
 *
 *   1) some treat non-0 as OK and 0 as invalid -- consistent with inet_aton().
 *
 *   2) some treat > 0 as OK and <= 0 as invalid -- consistent with inet_pton().
 *
 * Since this function returns 1 <=> valid and 0 <=> invalid, both the above
 * work.
 */
int
str2prefix_ipv6 (const char *str, struct prefix_ipv6 *p)
{
  tstring_t(ipv6, 64) ;
  char*       pnt ;
  const char* cp ;
  int         ret ;
  unsigned    plen ;

  pnt = strchr (str, '/');

  if (pnt == NULL)
    {
      /* No / => simple address                 */
      plen = IPV6_MAX_BITLEN;
      cp   = str ;
    }
  else
    {
      /* With / => prefix                       */
      plen = (unsigned) atoi (pnt + 1) ;
      if (plen > IPV6_MAX_PREFIXLEN)
        return 0 ;

      cp = tstring_set_n(ipv6, str, (pnt - str)) ;
    } ;

  ret  = inet_pton (AF_INET6, cp, &p->prefix);
  if (ret <= 0)
    return 0 ;

  p->family    = AF_INET6;
  p->prefixlen = plen;

  tstring_free(ipv6) ;

  return 1 ;
}

/* Convert IPv6 netmask to prefix length.
 *
 * If the netmask is invalid, all '1's after the first '0' are ignored.
 *
 * Argument netmask should be network byte order.
 */
u_char
ip6_masklen (union in6_addr_u netmask)
{
  if (netmask.n64[0] != U64_1s)
    return local_clz_n64(~netmask.n64[0]) ;

  if (netmask.n64[1] != U64_1s)
    return local_clz_n64(~netmask.n64[1]) + 64 ;

  return 128 ;
}

/* Check whether given IPv6 netmask is valid.
 *
 * Netmask is valid if there are no '1' bits after the LS '0' (if any)
 *
 * Argument netmask should be network byte order.
 */
bool
ip6_mask_check (union in6_addr_u netmask)
{
  if (netmask.n64[1] == 0)
    return n64_mask_check(netmask.n64[0]) ;

  if (netmask.n64[0] == U64_1s)
    return n64_mask_check(netmask.n64[1]) ;

  return false ;
} ;

void
masklen2ip6 (uint masklen, struct in6_addr *netmask)
{
  uint64_t m0, m1 ;

  if      (masklen < 64)
    {
      m0 = ~htonq(U64_1s >> masklen) ;
      m1 = 0 ;
    }
  else if (masklen < 128)
    {
      m0 = U64_1s;
      m1 = ~htonq(U64_1s >> (masklen - 64)) ;
    }
  else
    {
      m0 = U64_1s ;
      m1 = U64_1s ;
    } ;

  memcpy((char*)netmask + 0, &m0, 8) ;
  memcpy((char*)netmask + 8, &m1, 8) ;
}

void
apply_mask_ipv6 (struct prefix_ipv6 *p)
{
  struct prefix* px ;

  px = (struct prefix*)p ;
  confirm(offsetof(struct prefix, u.n64) ==
                                         offsetof(struct prefix_ipv6, prefix)) ;

  if (p->prefixlen < 64)
    {
      px->u.n64[0] &= ~htonq(U64_1s >> p->prefixlen) ;
      px->u.n64[1] = 0 ;
    }
  else if (p->prefixlen < 128)
    {
      px->u.n64[1] &= ~htonq(U64_1s >> (p->prefixlen - 64)) ;
    } ;
}

void
str2in6_addr (const char *str, struct in6_addr *addr)
{
  int i;
  unsigned int x;

  /* %x must point to unsinged int */
  for (i = 0; i < 16; i++)
    {
      sscanf (str + (i * 2), "%02x", &x);
      addr->s6_addr[i] = x & 0xff;
    }
}
#endif /* HAVE_IPV6 */

/*==============================================================================
 * General prefix and sockunion stuff
 */
void
apply_mask (struct prefix *p)
{
  switch (p->family)
    {
      case AF_INET:
        apply_mask_ipv4 ((struct prefix_ipv4 *)p);
        break;
#ifdef HAVE_IPV6
      case AF_INET6:
        apply_mask_ipv6 ((struct prefix_ipv6 *)p);
        break;
#endif /* HAVE_IPV6 */
      default:
        break;
    }
  return;
}

/* Utility function of convert between struct prefix <=> union sockunion.
 * FIXME This function isn't used anywhere. */
struct prefix *
sockunion2prefix (const_sockunion dest,
		  const_sockunion mask)
{
  if (dest->sa.sa_family == AF_INET)
    {
      struct prefix_ipv4 *p;

      p = prefix_ipv4_new ();
      p->family    = AF_INET;
      p->prefix    = dest->sin.sin_addr;
      p->prefixlen = ip_masklen (mask->sin.sin_addr);
      return (struct prefix *) p;
    }
#ifdef HAVE_IPV6
  if (dest->sa.sa_family == AF_INET6)
    {
      struct prefix_ipv6 *p;

      p = prefix_ipv6_new ();
      p->family    = AF_INET6;
      p->prefixlen = ip6_masklen ((union in6_addr_u)mask->sin6.sin6_addr);
      memcpy (&p->prefix, &dest->sin6.sin6_addr, sizeof (struct in6_addr));
      return (struct prefix *) p;
    }
#endif /* HAVE_IPV6 */
  return NULL;
}

/* Utility function of convert between struct prefix <=> union sockunion. */
struct prefix *
sockunion2hostprefix (const_sockunion su)
{
  if (su->sa.sa_family == AF_INET)
    {
      struct prefix_ipv4 *p;

      p = prefix_ipv4_new ();
      p->family = AF_INET;
      p->prefix = su->sin.sin_addr;
      p->prefixlen = IPV4_MAX_BITLEN;
      return (struct prefix *) p;
    }
#ifdef HAVE_IPV6
  if (su->sa.sa_family == AF_INET6)
    {
      struct prefix_ipv6 *p;

      p = prefix_ipv6_new ();
      p->family = AF_INET6;
      p->prefixlen = IPV6_MAX_BITLEN;
      memcpy (&p->prefix, &su->sin6.sin6_addr, sizeof (struct in6_addr));
      return (struct prefix *) p;
    }
#endif /* HAVE_IPV6 */
  return NULL;
}

void
prefix2sockunion (const struct prefix *p, union sockunion *su)
{
  memset (su, 0, sizeof (*su));

  su->sa.sa_family = p->family;
  if (p->family == AF_INET)
    su->sin.sin_addr = p->u.prefix4;
#ifdef HAVE_IPV6
  if (p->family == AF_INET6)
    memcpy (&su->sin6.sin6_addr, &p->u.prefix6, sizeof (struct in6_addr));
#endif /* HAVE_IPV6 */
}

int
prefix_blen (const struct prefix *p)
{
  switch (p->family)
    {
    case AF_INET:
      return IPV4_MAX_BYTELEN;
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      return IPV6_MAX_BYTELEN;
      break;
#endif /* HAVE_IPV6 */
    }
  return 0;
}

/* Generic function for conversion string to struct prefix.
 *
 * Accepts addresses without '/' and prefixes with.
 *
 * Returns 1 <=> valid IPv4 or (if HAVE_IPV6) IPv6 address or prefix.
 *         0 <=> not a a valid address or prefix
 */
int
str2prefix (const char *str, struct prefix *p)
{
  int ret;

  /* First we try to convert string to struct prefix_ipv4.      */
  ret = str2prefix_ipv4 (str, (struct prefix_ipv4 *) p);

#ifdef HAVE_IPV6
  /* If not IPv4, try to convert to struct prefix_ipv6.         */
  if (ret == 0)
    ret = str2prefix_ipv6 (str, (struct prefix_ipv6 *) p);
#endif /* HAVE_IPV6 */

  return ret;
}

int
prefix2str (const struct prefix *p, char *str, int size)
{
  char buf[BUFSIZ];

  inet_ntop (p->family, &p->u.prefix, buf, BUFSIZ);
  snprintf (str, size, "%s/%d", buf, p->prefixlen);
  return 0;
}

/*------------------------------------------------------------------------------
 * Return str_pfxtoa_t structure containing string representation of given
 * prefix.
 */
extern str_pfxtoa_t
spfxtoa(const struct prefix* p)
{
  str_pfxtoa_t QFB_QFS(pfa, qfs) ;

  switch (p->family)
    {
      case AF_INET:
        confirm(sizeof(pfa.str) > (INET_ADDRSTRLEN + 3)) ;
#ifdef HAVE_IPV6
      case AF_INET6:
        confirm(sizeof(pfa.str) > (INET6_ADDRSTRLEN + 4)) ;
#endif
        inet_ntop(p->family, &p->u.prefix, pfa.str, sizeof(pfa.str)) ;

        qfs_init_as_is(qfs, pfa.str, sizeof(pfa.str)) ;
        qfs_printf(qfs, "/%u", p->prefixlen) ;
        break;

      default:
        qfs_printf(qfs, "?unknown address family=%u?", p->family) ;
        break ;
    } ;

  qfs_term(qfs) ;
  return pfa;
} ;

struct prefix *
prefix_new ()
{
  struct prefix *p;

  p = XCALLOC (MTYPE_PREFIX, sizeof *p);
  return p;
}

/* Free prefix structure. */
void
prefix_free (struct prefix *p)
{
  XFREE (MTYPE_PREFIX, p);
}

/* Utility function.  Check the string only contains digit
 * character.
 * FIXME str.[c|h] would be better place for this function. */
int
all_digit (const char *str)
{
  for (; *str != '\0'; str++)
    if (!isdigit ((int) *str))
      return 0;
  return 1;
}

/* Utility function to convert ipv4 prefixes to Classful prefixes */
void apply_classful_mask_ipv4 (struct prefix_ipv4 *p)
{

  u_int32_t destination;

  destination = ntohl (p->prefix.s_addr);

  if (p->prefixlen == IPV4_MAX_PREFIXLEN);
  /* do nothing for host routes */
  else if (IN_CLASSC (destination))
    {
      p->prefixlen=24;
      apply_mask_ipv4(p);
    }
  else if (IN_CLASSB(destination))
    {
      p->prefixlen=16;
      apply_mask_ipv4(p);
    }
  else
    {
      p->prefixlen=8;
      apply_mask_ipv4(p);
    }
}

in_addr_t
ipv4_network_addr (in_addr_t hostaddr, int masklen)
{
  struct in_addr mask;

  masklen2ip (masklen, &mask);
  return hostaddr & mask.s_addr;
}

in_addr_t
ipv4_broadcast_addr (in_addr_t hostaddr, int masklen)
{
  struct in_addr mask;

  masklen2ip (masklen, &mask);
  return (masklen != IPV4_MAX_PREFIXLEN-1) ?
	 /* normal case */
         (hostaddr | ~mask.s_addr) :
	 /* special case for /31 */
         (hostaddr ^ ~mask.s_addr);
}

/* Utility function to convert ipv4 netmask to prefixes
   ex.) "1.1.0.0" "255.255.0.0" => "1.1.0.0/16"
   ex.) "1.0.0.0" NULL => "1.0.0.0/8"                   */
int
netmask_str2prefix_str (const char *net_str, const char *mask_str,
			char *prefix_str)
{
  struct in_addr network;
  struct in_addr mask;
  u_char prefixlen;
  u_int32_t destination;
  int ret;

  ret = inet_aton (net_str, &network);
  if (! ret)
    return 0;

  if (mask_str)
    {
      ret = inet_aton (mask_str, &mask);
      if (! ret)
        return 0;

      prefixlen = ip_masklen (mask);
    }
  else
    {
      destination = ntohl (network.s_addr);

      if (network.s_addr == 0)
	prefixlen = 0;
      else if (IN_CLASSC (destination))
	prefixlen = 24;
      else if (IN_CLASSB (destination))
	prefixlen = 16;
      else if (IN_CLASSA (destination))
	prefixlen = 8;
      else
	return 0;
    }

  sprintf (prefix_str, "%s/%d", net_str, prefixlen);

  return 1;
}

#ifdef HAVE_IPV6
/* Utility function for making IPv6 address string. */
const char *
inet6_ntoa (struct in6_addr addr)
{
  static char buf[INET6_ADDRSTRLEN];

  inet_ntop (AF_INET6, &addr, buf, INET6_ADDRSTRLEN);
  return buf;
}
#endif /* HAVE_IPV6 */

/*==============================================================================
 * Raw prefix handling
 */
static const byte prefix_last_byte_mask[8] = { 0xFF, 0x80, 0xC0, 0xE0,
                                               0xF0, 0xF8, 0xFC, 0xFE } ;

/*------------------------------------------------------------------------------
 * Make raw form of prefix_len + prefix, and return total length.
 *
 * Silently enforces maximum prefix for known families, and masks last byte
 * of prefix to guarantee not sending any bits beyond the given length.
 */
extern ulen
prefix_to_raw(prefix_raw raw, const struct prefix * p)
{
  ulen len ;
  byte plen ;

  plen = p->prefixlen & 0xFF ;

  switch (p->family)
    {
      case AF_INET:
        if (plen > IPV4_MAX_PREFIXLEN)
          plen = IPV4_MAX_PREFIXLEN ;
        break ;

#if HAVE_IPV6
      case AF_INET6:
        if (plen > IPV6_MAX_PREFIXLEN)
          plen = IPV6_MAX_PREFIXLEN ;
        break ;
#endif

      default:
        plen = 0 ;
    } ;

  len = ((plen + 7) / 8) & 0xFF ;

  raw->prefix_len = plen ;
  if (len != 0)
    {
      memcpy(raw->prefix, p->u.b, len) ;

      raw->prefix[len - 1] &= prefix_last_byte_mask[plen & 0x7] ;
    } ;

  return (len + 1) ;
} ;

/*------------------------------------------------------------------------------
 * Set prefix from raw value and known family.
 *
 * Silently enforces maximum prefix for known families, and masks last byte
 * of prefix to guarantee not using any bits beyond the given length.
 */
extern void
prefix_from_raw(struct prefix * p, prefix_raw raw, sa_family_t family)
{
  ulen len ;
  byte plen ;

  memset(p, 0, sizeof(struct prefix)) ;

  plen = raw->prefix_len & 0xFF ;

  switch (family)
    {
      case AF_INET:
        if (plen > IPV4_MAX_PREFIXLEN)
          plen = IPV4_MAX_PREFIXLEN ;
        break ;

#if HAVE_IPV6
      case AF_INET6:
        if (plen > IPV6_MAX_PREFIXLEN)
          plen = IPV6_MAX_PREFIXLEN ;
        break ;
#endif

      default:
        plen = 0 ;
    } ;

  len = ((plen + 7) / 8) & 0xFF ;

  p->family    = family ;
  p->prefixlen = plen ;
  if (len != 0)
    {
      memcpy(p->u.b, raw->prefix, len) ;

      p->u.b[len - 1] &= prefix_last_byte_mask[plen & 0x7] ;
    } ;
} ;


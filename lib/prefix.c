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
#if defined(__GNUC__)
#define __GNUC__LOCAL 1
#else
#define __GNUC__LOCAL 0
#endif

/* Maskbit -- mask for last significant byte of a prefix: maskbit[len % 8]
 */
static const uint8_t maskbit[] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0,
			                 0xf8, 0xfc, 0xfe, 0xff };

/*==============================================================================
 * "Macros" for banging 32 bits of masks
 */

inline static u_char n32_masklen(uint32_t mask_n)      Always_Inline ;
inline static uint32_t n32_mask(uint len)              Always_Inline ;
inline static uint32_t n32_p_mask(uint len)            Always_Inline ;
inline static u_char n32_mask_check (uint32_t mask_n)  Always_Inline ;

/*------------------------------------------------------------------------------
 * Convert 32 bits of netmask to prefix length.
 *
 * If the netmask is invalid, all '1's after the first '0' are ignored.
 *
 * Argument netmask should be network byte order.
 */
inline static u_char
n32_masklen (uint32_t mask_n)
{
  uint32_t mask_h ;

  mask_h = ntohl(mask_n) ;

#if __GNUC__LOCAL

  return (mask_h != 0xFFFFFFFF) ? __builtin_clz(~mask_h) : 32 ;

#else

  while (1)
    {
      uint32_t t ;

      if (mask_h == 0)
        return 0 ;

      t = (mask_h | (mask_h - 1)) - 0xFFFFFFFF ;

      if (t == 0)
        return 33 - ffs(mask_h) ;

      mask_h &= t ;
    }
#endif
} ;

/*------------------------------------------------------------------------------
 * Return 32 bit mask
 */
inline static uint32_t
n32_mask(uint len)
{
  return (len < 32) ? htonl(~((uint32_t)0xFFFFFFFF >> len))
                    :         (uint32_t)0xFFFFFFFF ;
} ;

/*------------------------------------------------------------------------------
 * Return 32 bit part of mask -- take len % 32
 */
inline static uint32_t
n32_p_mask(uint len)
{
  return htonl(~((uint32_t)0xFFFFFFFF >> (len % 32))) ;
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

  return (mask_h | (mask_h - 1)) == 0xFFFFFFFF ;

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
    if (ntohl(p1->u.n32[i] ^ p2->u.n32[i]) > (0xFFFFFFFF >> m))
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
  uint i, len ;
  uint32_t d ;

  if (p1->family != p2->family)
    return -1;

  switch (p1->family)
    {
      case AF_INET:
        len = IPV4_MAX_BYTELEN / 4 ;
        confirm((IPV4_MAX_BYTELEN % 4) == 0) ;
        break ;

#ifdef HAVE_IPV6
      case AF_INET6:
        len = IPV6_MAX_BYTELEN / 4 ;
        confirm((IPV6_MAX_BYTELEN % 4) == 0) ;
        break ;
#endif

      default:
        return -1 ;
    } ;

  i = 0 ;

  while ((d = p1->u.n32[i] ^ p2->u.n32[i]) == 0)
    {
      ++i ;
      if (i == len)
        return len * 32 ;
    } ;

  d = ntohl(d) ;        /* NB d != 0            */

#if __GNUC__LOCAL
  return (i * 32) + __builtin_clz(d) ;

#else
  if (d > 0x0000FFFF)
    {
      if (d > 0x00FFFFFF)
        {
          i = (i * 32) + (24 - 24);
          d >>= 24 ;
        }
      else
        {
          i = (i * 32) + (24 - 16) ;
          d >>= 16 ;
        }
    }
  else
    {
      if (d > 0x000000FF)
        {
          i = (i * 32) + (24 -  8) ;
          d >>= 8 ;
        }
      else
        {
          i = (i * 32) + (24 -  0) ;
        }
    } ;

  if (d > 0x0F)
    d >>= 4 ;
  else
    i += 4 ;

  while ((d & 0x8) == 0)
    {
      ++i ;
      d <<= 1 ;
    } ;

  return i ;

#endif
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
  return n32_masklen (netmask.s_addr) ;
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
  uint     i ;
  uint32_t nm ;

  i = 0;
  while ((nm = netmask.n32[i]) == 0xFFFFFFFF)
    {
      ++i ;

      if (i == (IPV6_MAX_BYTELEN / 4))
        return IPV6_MAX_BITLEN ;

      confirm((IPV6_MAX_BYTELEN % 4) == 0) ;
      confirm(IPV6_MAX_BITLEN == (IPV6_MAX_BYTELEN * 8)) ;
    } ;

  return (i * 32) + n32_masklen (nm) ;
}

/* Check whether given IPv6 netmask is valid.
 *
 * Netmask is valid if there are no '1' bits after the LS '0' (if any)
 *
 * The check does all of the work required to establish the prefix length (plus
 * a little.  So, unlike ip4_mask_check() -- which returns a bool -- this
 * returns the prefix length if is a valid mask.
 *
 * Returns:  -1 <=> *not* valid
 *         >= 0 == the prefix length
 *
 * Argument netmask should be network byte order.
 */
bool
ip6_mask_check (union in6_addr_u netmask)
{
  uint     i ;
  uint32_t nm ;

  i = 0;
  while ((nm = netmask.n32[i++]) == 0xFFFFFFFF)
    {
      if (i == (IPV6_MAX_BYTELEN / 4))
        return true ;

      confirm((IPV6_MAX_BYTELEN % 4) == 0) ;
      confirm(IPV6_MAX_BITLEN == (IPV6_MAX_BYTELEN * 8)) ;
    } ;

  while (i < 4)
    if (netmask.n32[i++] != 0)
      return false ;

  return n32_mask_check(nm) ;
} ;

void
masklen2ip6 (uint masklen, struct in6_addr *netmask)
{
  uint32_t m ;
  uint i ;

  static const uint32_t in6_masks[7] =
    { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
      0x00000000, 0x00000000, 0x00000000, 0x000000000
    };

  if (masklen < 128)
    {
      i = masklen / 32 ;
      m = n32_p_mask(masklen) ;
    }
  else
    {
      i = 3 ;
      m = 0xFFFFFFFF ;
    } ;

  memcpy(netmask, &in6_masks[3 - i], 16) ;
  memcpy(netmask->s6_addr + (i * 4), &m, 4) ;
}

void
apply_mask_ipv6 (struct prefix_ipv6 *p)
{
  struct prefix* px ;
  uint     i ;

  px = (struct prefix*)p ;
  confirm(offsetof(struct prefix, u.n32) ==
                                         offsetof(struct prefix_ipv6, prefix)) ;
  i = p->prefixlen / 32 ;

  if (i < 4)
    {
      px->u.n32[i++] &= n32_p_mask(p->prefixlen) ;

      while (i < 4)
        px->u.n32[i++] = 0;
    }
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


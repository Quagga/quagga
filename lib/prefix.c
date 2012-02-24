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

/* Maskbit. */
static const u_char maskbit[] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0,
			                0xf8, 0xfc, 0xfe, 0xff };

/* Number of bits in prefix type. */
#ifndef PNBBY
#define PNBBY 8
#endif /* PNBBY */

#define MASKBIT(offset)  ((0xff << (PNBBY - (offset))) & 0xff)

/* Address Famiy Identifier to Address Family converter. */
int
afi2family (afi_t afi)
{
  if (afi == AFI_IP)
    return AF_INET;
#ifdef HAVE_IPV6
  else if (afi == AFI_IP6)
    return AF_INET6;
#endif /* HAVE_IPV6 */
  return 0;
}

afi_t
family2afi (int family)
{
  if (family == AF_INET)
    return AFI_IP;
#ifdef HAVE_IPV6
  else if (family == AF_INET6)
    return AFI_IP6;
#endif /* HAVE_IPV6 */
  return 0;
}

/* If n includes p prefix then return 1 else return 0. */
int
prefix_match (const struct prefix *n, const struct prefix *p)
{
  int offset;
  int shift;
  const u_char *np, *pp;

  /* If n's prefix is longer than p's one return 0. */
  if (n->prefixlen > p->prefixlen)
    return 0;

  /* Set both prefix's head pointer. */
  np = (const u_char *)&n->u.prefix;
  pp = (const u_char *)&p->u.prefix;

  offset = n->prefixlen / PNBBY;
  shift =  n->prefixlen % PNBBY;

  if (shift)
    if (maskbit[shift] & (np[offset] ^ pp[offset]))
      return 0;

  while (offset--)
    if (np[offset] != pp[offset])
      return 0;
  return 1;
}

/* Copy prefix from src to dest. */
void
prefix_copy (struct prefix *dest, const struct prefix *src)
{
  dest->family = src->family;
  dest->prefixlen = src->prefixlen;

  if (src->family == AF_INET)
    dest->u.prefix4 = src->u.prefix4;
#ifdef HAVE_IPV6
  else if (src->family == AF_INET6)
    dest->u.prefix6 = src->u.prefix6;
#endif /* HAVE_IPV6 */
  else if (src->family == AF_UNSPEC)
    {
      dest->u.lp.id = src->u.lp.id;
      dest->u.lp.adv_router = src->u.lp.adv_router;
    }
  else
    {
      zlog (NULL, LOG_ERR, "prefix_copy(): Unknown address family %d",
	      src->family);
      assert (0);
    }
}

/*
 * Return 1 if the address/netmask contained in the prefix structure
 * is the same, and else return 0.  For this routine, 'same' requires
 * that not only the prefix length and the network part be the same,
 * but also the host part.  Thus, 10.0.0.1/8 and 10.0.0.2/8 are not
 * the same.  Note that this routine has the same return value sense
 * as '==' (which is different from prefix_cmp).
 */
int
prefix_same (const struct prefix *p1, const struct prefix *p2)
{
  if (p1->family == p2->family && p1->prefixlen == p2->prefixlen)
    {
      if (p1->family == AF_INET)
	if (IPV4_ADDR_SAME (&p1->u.prefix, &p2->u.prefix))
	  return 1;
#ifdef HAVE_IPV6
      if (p1->family == AF_INET6 )
	if (IPV6_ADDR_SAME (&p1->u.prefix, &p2->u.prefix))
	  return 1;
#endif /* HAVE_IPV6 */
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
 */
int
prefix_cmp (const struct prefix *p1, const struct prefix *p2)
{
  int offset;
  int shift;

  /* Set both prefix's head pointer. */
  const u_char *pp1 = (const u_char *)&p1->u.prefix;
  const u_char *pp2 = (const u_char *)&p2->u.prefix;

  if (p1->family != p2->family || p1->prefixlen != p2->prefixlen)
    return 1;

  offset = p1->prefixlen / 8;
  shift = p1->prefixlen % 8;

  if (shift)
    if (maskbit[shift] & (pp1[offset] ^ pp2[offset]))
      return 1;

  while (offset--)
    if (pp1[offset] != pp2[offset])
      return 1;

  return 0;
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
  int pos, bit;
  int length = 0;
  u_char xor;

  /* Set both prefix's head pointer. */
  const u_char *pp1 = (const u_char *)&p1->u.prefix;
  const u_char *pp2 = (const u_char *)&p2->u.prefix;

  if (p1->family == AF_INET)
    length = IPV4_MAX_BYTELEN;
#ifdef HAVE_IPV6
  if (p1->family == AF_INET6)
    length = IPV6_MAX_BYTELEN;
#endif
  if (p1->family != p2->family || !length)
    return -1;

  for (pos = 0; pos < length; pos++)
    if (pp1[pos] != pp2[pos])
      break;
  if (pos == length)
    return pos * 8;

  xor = pp1[pos] ^ pp2[pos];
  for (bit = 0; bit < 8; bit++)
    if (xor & (1 << (7 - bit)))
      break;

  return pos * 8 + bit;
}

/* Return prefix family type string. */
const char *
prefix_family_str (const struct prefix *p)
{
  if (p->family == AF_INET)
    return "inet";
#ifdef HAVE_IPV6
  if (p->family == AF_INET6)
    return "inet6";
#endif /* HAVE_IPV6 */
  return "unspec";
}

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

/* Convert masklen into IP address's netmask. */
void
masklen2ip (int masklen, struct in_addr *netmask)
{
  u_char *pnt;
  int bit;
  int offset;

  memset (netmask, 0, sizeof (struct in_addr));
  pnt = (unsigned char *) netmask;

  offset = masklen / 8;
  bit = masklen % 8;

  while (offset--)
    *pnt++ = 0xff;

  if (bit)
    *pnt = maskbit[bit];
}

/* Convert IP address's netmask into integer. We assume netmask is
   sequential one. Argument netmask should be network byte order. */
u_char
ip_masklen (struct in_addr netmask)
{
  u_char len;
  u_char *pnt;
  u_char *end;
  u_char val;

  len = 0;
  pnt = (u_char *) &netmask;
  end = pnt + 4;

  while ((pnt < end) && (*pnt == 0xff))
    {
      len+= 8;
      pnt++;
    }

  if (pnt < end)
    {
      val = *pnt;
      while (val)
	{
	  len++;
	  val <<= 1;
	}
    }
  return len;
}

/* Apply mask to IPv4 prefix. */
void
apply_mask_ipv4 (struct prefix_ipv4 *p)
{
  u_char *pnt;
  int index;
  int offset;

  index = p->prefixlen / 8;

  if (index < 4)
    {
      pnt = (u_char *) &p->prefix;
      offset = p->prefixlen % 8;

      pnt[index] &= maskbit[offset];
      index++;

      while (index < 4)
	pnt[index++] = 0;
    }
}

/* If prefix is 0.0.0.0/0 then return 1 else return 0. */
int
prefix_ipv4_any (const struct prefix_ipv4 *p)
{
  return (p->prefix.s_addr == 0 && p->prefixlen == 0);
}

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

/* Convert struct in6_addr netmask into integer.
 * FIXME return u_char as ip_maskleni() does. */
int
ip6_masklen (struct in6_addr netmask)
{
  int len = 0;
  unsigned char val;
  unsigned char *pnt;

  pnt = (unsigned char *) & netmask;

  while ((*pnt == 0xff) && len < 128)
    {
      len += 8;
      pnt++;
    }

  if (len < 128)
    {
      val = *pnt;
      while (val)
	{
	  len++;
	  val <<= 1;
	}
    }
  return len;
}

void
masklen2ip6 (int masklen, struct in6_addr *netmask)
{
  unsigned char *pnt;
  int bit;
  int offset;

  memset (netmask, 0, sizeof (struct in6_addr));
  pnt = (unsigned char *) netmask;

  offset = masklen / 8;
  bit = masklen % 8;

  while (offset--)
    *pnt++ = 0xff;

  if (bit)
    *pnt = maskbit[bit];
}

void
apply_mask_ipv6 (struct prefix_ipv6 *p)
{
  u_char *pnt;
  int index;
  int offset;

  index = p->prefixlen / 8;

  if (index < 16)
    {
      pnt = (u_char *) &p->prefix;
      offset = p->prefixlen % 8;

      pnt[index] &= maskbit[offset];
      index++;

      while (index < 16)
	pnt[index++] = 0;
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
sockunion2prefix (const union sockunion *dest,
		  const union sockunion *mask)
{
  if (dest->sa.sa_family == AF_INET)
    {
      struct prefix_ipv4 *p;

      p = prefix_ipv4_new ();
      p->family = AF_INET;
      p->prefix = dest->sin.sin_addr;
      p->prefixlen = ip_masklen (mask->sin.sin_addr);
      return (struct prefix *) p;
    }
#ifdef HAVE_IPV6
  if (dest->sa.sa_family == AF_INET6)
    {
      struct prefix_ipv6 *p;

      p = prefix_ipv6_new ();
      p->family = AF_INET6;
      p->prefixlen = ip6_masklen (mask->sin6.sin6_addr);
      memcpy (&p->prefix, &dest->sin6.sin6_addr, sizeof (struct in6_addr));
      return (struct prefix *) p;
    }
#endif /* HAVE_IPV6 */
  return NULL;
}

/* Utility function of convert between struct prefix <=> union sockunion. */
struct prefix *
sockunion2hostprefix (const union sockunion *su)
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
prefix2sockunion (const struct prefix *p, union sockunion *su) {
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
      memcpy(raw->prefix, p->u.val, len) ;

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
      memcpy(p->u.val, raw->prefix, len) ;

      p->u.val[len - 1] &= prefix_last_byte_mask[plen & 0x7] ;
    } ;
} ;

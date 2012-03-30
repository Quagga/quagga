/*
 * Prefix structure.
 * Copyright (C) 1998 Kunihiro Ishiguro
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

#ifndef _ZEBRA_PREFIX_H
#define _ZEBRA_PREFIX_H

#include "misc.h"
#include "sockunion.h"
#include "qfstring.h"

typedef const union sockunion* const_sockunion ;

/*
 * A struct prefix contains an address family, a prefix length, and an
 * address.  This can represent either a 'network prefix' as defined
 * by CIDR, where the 'host bits' of the prefix are 0
 * (e.g. AF_INET:10.0.0.0/8), or an address and netmask
 * (e.g. AF_INET:10.0.0.9/8), such as might be configured on an
 * interface.
 */

/* IPv4 and IPv6 unified prefix structure. */
struct prefix
{
  sa_family_t   family;
  u_char        prefixlen;
  union
  {
    uint64_t        n64[2] ;
    uint32_t        n32[4] ;
    uint8_t         b[16] ;

    u_char          prefix ;

    struct in_addr  prefix4 ;
  #ifdef HAVE_IPV6
    struct in6_addr prefix6;
  #endif /* HAVE_IPV6 */
    struct
    {
      struct in_addr id;
      struct in_addr adv_router;
    } lp;
    u_char val[8];
  } u __attribute__ ((aligned (8)));
};

/* For when struct in6_addr does not have these elements
 */
union in6_addr_u
{
  struct in6_addr addr ;
  uint64_t        n64[2] ;
  uint32_t        n32[4] ;
  uint8_t         b[16] ;
};

CONFIRM(sizeof(union in6_addr_u) == sizeof(struct in6_addr)) ;

/* So we know that the AF_INET, IPv4 prefix address maps to *network order*
 * uint32_t.
 *
 * And that the AF_INET6, IPv6 prefix address maps to Big Endian array of four
 * *network order* uint32_t (or two *network order uint64_t !)
 */
CONFIRM(sizeof(struct in_addr) == sizeof(((struct prefix*)0)->u.n32[0])) ;

#ifdef HAVE_IPV6
CONFIRM(sizeof(struct in6_addr) == sizeof(((struct prefix*)0)->u.b)) ;
CONFIRM(sizeof(struct in6_addr) == sizeof(((struct prefix*)0)->u.n32)) ;
CONFIRM(sizeof(struct in6_addr) == sizeof(((struct prefix*)0)->u.n64)) ;
#endif

/* Prefix as carried in protocols
 */
struct prefix_raw
{
  byte prefix_len ;
  byte prefix[256 / 8] ;
} ;

CONFIRM(offsetof(struct prefix_raw, prefix_len) == 0) ;
CONFIRM(offsetof(struct prefix_raw, prefix)     == 1) ;

typedef struct prefix_raw* prefix_raw ;
typedef struct prefix_raw  prefix_raw_t[1] ;

/* IPv4 prefix structure. */
struct prefix_ipv4
{
  sa_family_t   family;
  u_char        prefixlen;
  struct in_addr prefix __attribute__ ((aligned (8)));
};
CONFIRM(offsetof(struct prefix_ipv4, family)
     == offsetof(struct prefix,      family)) ;
CONFIRM(offsetof(struct prefix_ipv4, prefixlen)
     == offsetof(struct prefix,      prefixlen)) ;
CONFIRM(offsetof(struct prefix_ipv4, prefix)
     == offsetof(struct prefix,      u.prefix4)) ;
CONFIRM(sizeof(struct prefix_ipv4) <= sizeof(struct prefix)) ;

/* IPv6 prefix structure. */
#ifdef HAVE_IPV6
struct prefix_ipv6
{
  sa_family_t   family;
  u_char        prefixlen;
  struct in6_addr prefix __attribute__ ((aligned (8)));
};
CONFIRM(offsetof(struct prefix_ipv6, family)
     == offsetof(struct prefix,      family)) ;
CONFIRM(offsetof(struct prefix_ipv6, prefixlen)
     == offsetof(struct prefix,      prefixlen)) ;
CONFIRM(offsetof(struct prefix_ipv6, prefix)
     == offsetof(struct prefix,      u.prefix6)) ;
CONFIRM(sizeof(struct prefix_ipv6) <= sizeof(struct prefix)) ;
#endif /* HAVE_IPV6 */

struct prefix_ls
{
  sa_family_t   family;
  u_char        prefixlen;
  struct in_addr id __attribute__ ((aligned (8)));
  struct in_addr adv_router;
};
CONFIRM(offsetof(struct prefix_ls,   family)
     == offsetof(struct prefix,      family)) ;
CONFIRM(offsetof(struct prefix_ls,   prefixlen)
     == offsetof(struct prefix,      prefixlen)) ;
CONFIRM(offsetof(struct prefix_ls,   id)
     == offsetof(struct prefix,      u.lp.id)) ;
CONFIRM(offsetof(struct prefix_ls,   adv_router)
     == offsetof(struct prefix,      u.lp.adv_router)) ;
CONFIRM(sizeof(struct prefix_ls)   <= sizeof(struct prefix)) ;

/* Prefix for routing distinguisher. */
struct prefix_rd
{
  sa_family_t   family;
  u_char        prefixlen;
  u_char        val[8] __attribute__ ((aligned (8)));
};
CONFIRM(offsetof(struct prefix_rd,   family)
     == offsetof(struct prefix,      family)) ;
CONFIRM(offsetof(struct prefix_rd,   prefixlen)
     == offsetof(struct prefix,      prefixlen)) ;
CONFIRM(offsetof(struct prefix_rd,   val)
     == offsetof(struct prefix,      u.val)) ;
CONFIRM(sizeof(struct prefix_rd)   <= sizeof(struct prefix)) ;

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif /* INET_ADDRSTRLEN */

#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif /* INET6_ADDRSTRLEN */

#ifndef INET6_BUFSIZ
#define INET6_BUFSIZ 51
#endif /* INET6_BUFSIZ */

/* Max bit/byte length of IPv4 address. */
#define IPV4_MAX_BYTELEN    4
#define IPV4_MAX_BITLEN    32
#define IPV4_MAX_PREFIXLEN 32
#define IPV4_ADDR_CMP(D,S)   memcmp ((D), (S), IPV4_MAX_BYTELEN)
#define IPV4_ADDR_SAME(D,S)  (memcmp ((D), (S), IPV4_MAX_BYTELEN) == 0)
#define IPV4_ADDR_COPY(D,S)  memcpy ((D), (S), IPV4_MAX_BYTELEN)

#define IPV4_NET0(a)    ((((u_int32_t) (a)) & 0xff000000) == 0x00000000)
#define IPV4_NET127(a)  ((((u_int32_t) (a)) & 0xff000000) == 0x7f000000)
#define IPV4_LINKLOCAL(a) ((((u_int32_t) (a)) & 0xffff0000) == 0xa9fe0000)
#define IPV4_CLASS_DE(a)  ((((u_int32_t) (a)) & 0xe0000000) == 0xe0000000)

/* Max bit/byte length of IPv6 address. */
#define IPV6_MAX_BYTELEN    16
#define IPV6_MAX_BITLEN    128
#define IPV6_MAX_PREFIXLEN 128
#define IPV6_ADDR_CMP(D,S)   memcmp ((D), (S), IPV6_MAX_BYTELEN)
#define IPV6_ADDR_SAME(D,S)  (memcmp ((D), (S), IPV6_MAX_BYTELEN) == 0)
#define IPV6_ADDR_COPY(D,S)  memcpy ((D), (S), IPV6_MAX_BYTELEN)

/* Count prefix size from mask length */
#define PSIZE(a) (((a) + 7) / (8))

/* Prefix's family member. */
#define PREFIX_FAMILY(p)  ((p)->family)

QFB_T(60) str_pfxtoa_t ;

/*==============================================================================
 * Prototypes.
 */
extern sa_family_t afi2family (afi_t);
extern afi_t family2afi (sa_family_t );

/* Check bit of the prefix. */
extern unsigned int prefix_bit (const u_char *prefix, const u_char prefixlen);
#ifdef HAVE_IPV6
extern unsigned int prefix6_bit (const struct in6_addr *prefix,
                                                        const u_char prefixlen);
#endif
extern struct prefix *prefix_new (void);
extern void prefix_free (struct prefix *);
extern const char *prefix_family_str (const struct prefix *);
extern int prefix_blen (const struct prefix *);

extern ulen prefix_to_raw(prefix_raw, const struct prefix *) ;
extern void prefix_from_raw(struct prefix *, prefix_raw, sa_family_t) ;

extern int str2prefix (const char *, struct prefix *);
extern int prefix2str (const struct prefix *, char *, int);
extern str_pfxtoa_t spfxtoa(const struct prefix* p) ;
extern int prefix_match (const struct prefix *, const struct prefix *);
extern int prefix_same (const struct prefix *, const struct prefix *);
extern int prefix_cmp (const struct prefix *, const struct prefix *);
extern int prefix_common_bits (const struct prefix *, const struct prefix *);
extern void prefix_copy (struct prefix *dest, const struct prefix *src);
extern void apply_mask (struct prefix *);

extern struct prefix *sockunion2prefix (const_sockunion dest,
                                        const_sockunion mask);
extern struct prefix *sockunion2hostprefix (const_sockunion src);
extern void prefix2sockunion (const struct prefix *, union sockunion *);

extern struct prefix_ipv4 *prefix_ipv4_new (void);
extern void prefix_ipv4_free (struct prefix_ipv4 *);
extern int str2prefix_ipv4 (const char *, struct prefix_ipv4 *);
extern void apply_mask_ipv4 (struct prefix_ipv4 *);

#define PREFIX_COPY_IPV4(DST, SRC)	\
	*((struct prefix_ipv4 *)(DST)) = *((const struct prefix_ipv4 *)(SRC));

Inline void
prefix_copy_ipv4(struct prefix* dst, struct prefix* src)
{
  *dst = *src ;
} ;

extern int prefix_ipv4_any (const struct prefix_ipv4 *);
extern void apply_classful_mask_ipv4 (struct prefix_ipv4 *);

extern u_char ip_masklen (struct in_addr);
extern bool ip_mask_check (struct in_addr netmask) ;
extern void masklen2ip (const uint, struct in_addr *);
/* returns the network portion of the host address */
extern in_addr_t ipv4_network_addr (in_addr_t hostaddr, int masklen);
/* given the address of a host on a network and the network mask length,
 * calculate the broadcast address for that network;
 * special treatment for /31: returns the address of the other host
 * on the network by flipping the host bit */
extern in_addr_t ipv4_broadcast_addr (in_addr_t hostaddr, int masklen);

extern int netmask_str2prefix_str (const char *, const char *, char *);

#ifdef HAVE_IPV6
extern struct prefix_ipv6 *prefix_ipv6_new (void);
extern void prefix_ipv6_free (struct prefix_ipv6 *);
extern int str2prefix_ipv6 (const char *, struct prefix_ipv6 *);
extern void apply_mask_ipv6 (struct prefix_ipv6 *);

#define PREFIX_COPY_IPV6(DST, SRC)	\
        *((struct prefix_ipv6 *)(DST)) = *((const struct prefix_ipv6 *)(SRC));

Inline void
prefix_copy_ipv6(struct prefix* dst, struct prefix* src)
{
  *dst = *src ;
} ;

extern u_char ip6_masklen (const struct in6_addr* p_s6_addr);
extern bool ip6_mask_check (const struct in6_addr* p_s6_addr) ;
extern void masklen2ip6 (uint, struct in6_addr *);

extern void str2in6_addr (const char *, struct in6_addr *);
extern const char *inet6_ntoa (struct in6_addr);

#endif /* HAVE_IPV6 */

extern int all_digit (const char *);

#endif /* _ZEBRA_PREFIX_H */

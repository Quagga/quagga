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

/* Maskbit -- mask for last significant byte of a prefix: maskbit[len % 8]
 */
static const uint8_t maskbit[] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0,
			                 0xf8, 0xfc, 0xfe, 0xff };

/* IPv4 masks -- address in *host* order
 */
static const uint32_t in_addr_mask_h[] =
{
  0x00000000, /* /0  0.0.0.0         */
  0x80000000, /* /1  128.0.0.0       */
  0xc0000000, /* /2  192.0.0.0       */
  0xe0000000, /* /3  224.0.0.0       */
  0xf0000000, /* /4  240.0.0.0       */
  0xf8000000, /* /5  248.0.0.0       */
  0xfc000000, /* /6  252.0.0.0       */
  0xfe000000, /* /7  254.0.0.0       */
  0xff000000, /* /8  255.0.0.0       */
  0xff800000, /* /9  255.128.0.0     */
  0xffc00000, /* /10 255.192.0.0     */
  0xffe00000, /* /11 255.224.0.0     */
  0xfff00000, /* /12 255.240.0.0     */
  0xfff80000, /* /13 255.248.0.0     */
  0xfffc0000, /* /14 255.252.0.0     */
  0xfffe0000, /* /15 255.254.0.0     */
  0xffff0000, /* /16 255.255.0.0     */
  0xffff8000, /* /17 255.255.128.0   */
  0xffffc000, /* /18 255.255.192.0   */
  0xffffe000, /* /19 255.255.224.0   */
  0xfffff000, /* /20 255.255.240.0   */
  0xfffff800, /* /21 255.255.248.0   */
  0xfffffc00, /* /22 255.255.252.0   */
  0xfffffe00, /* /23 255.255.254.0   */
  0xffffff00, /* /24 255.255.255.0   */
  0xffffff80, /* /25 255.255.255.128 */
  0xffffffc0, /* /26 255.255.255.192 */
  0xffffffe0, /* /27 255.255.255.224 */
  0xfffffff0, /* /28 255.255.255.240 */
  0xfffffff8, /* /29 255.255.255.248 */
  0xfffffffc, /* /30 255.255.255.252 */
  0xfffffffe, /* /31 255.255.255.254 */
  0xffffffff  /* /32 255.255.255.255 */
};

/* IPv4 masks -- address in *network* order
 */
#if BYTE_ORDER == BIG_ENDIAN

#define in_addr_mask_n in_addr_mask_h

#elif BYTE_ORDER == LITTLE_ENDIAN

static const u_int32_t in_addr_mask_n[] =
{
  0x00000000, /* /0  0.0.0.0         */
  0x00000080, /* /1  128.0.0.0       */
  0x000000c0, /* /2  192.0.0.0       */
  0x000000e0, /* /3  224.0.0.0       */
  0x000000f0, /* /4  240.0.0.0       */
  0x000000f8, /* /5  248.0.0.0       */
  0x000000fc, /* /6  252.0.0.0       */
  0x000000fe, /* /7  254.0.0.0       */
  0x000000ff, /* /8  255.0.0.0       */
  0x000080ff, /* /9  255.128.0.0     */
  0x0000c0ff, /* /10 255.192.0.0     */
  0x0000e0ff, /* /11 255.224.0.0     */
  0x0000f0ff, /* /12 255.240.0.0     */
  0x0000f8ff, /* /13 255.248.0.0     */
  0x0000fcff, /* /14 255.252.0.0     */
  0x0000feff, /* /15 255.254.0.0     */
  0x0000ffff, /* /16 255.255.0.0     */
  0x0080ffff, /* /17 255.255.128.0   */
  0x00c0ffff, /* /18 255.255.192.0   */
  0x00e0ffff, /* /19 255.255.224.0   */
  0x00f0ffff, /* /20 255.255.240.0   */
  0x00f8ffff, /* /21 255.255.248.0   */
  0x00fcffff, /* /22 255.255.252.0   */
  0x00feffff, /* /23 255.255.254.0   */
  0x00ffffff, /* /24 255.255.255.0   */
  0x80ffffff, /* /25 255.255.255.128 */
  0xc0ffffff, /* /26 255.255.255.192 */
  0xe0ffffff, /* /27 255.255.255.224 */
  0xf0ffffff, /* /28 255.255.255.240 */
  0xf8ffffff, /* /29 255.255.255.248 */
  0xfcffffff, /* /30 255.255.255.252 */
  0xfeffffff, /* /31 255.255.255.254 */
  0xffffffff  /* /32 255.255.255.255 */
};

#endif /* BYTE_ORDER == ZZZ     */

/* IPv6 masks -- address in *network* order
 */
#define FFx1  0xFF
#define FFx2  0xFF, 0xFF
#define FFx3  0xFF, 0xFF, 0xFF
#define FFx4  0xFF, 0xFF, 0xFF, 0xFF
#define FFx5  FFx4, FFx1
#define FFx6  FFx4, FFx2
#define FFx7  FFx4, FFx3
#define FFx8  FFx4, FFx4
#define FFx9  FFx4, FFx4, FFx1
#define FFx10 FFx4, FFx4, FFx2
#define FFx11 FFx4, FFx4, FFx3
#define FFx12 FFx4, FFx4, FFx4
#define FFx13 FFx4, FFx4, FFx4, FFx1
#define FFx14 FFx4, FFx4, FFx4, FFx2
#define FFx15 FFx4, FFx4, FFx4, FFx3
#define FFx16 FFx4, FFx4, FFx4, FFx4

static const byte in_addr6_mask_n[129][sizeof(struct in6_addr)] =
{
  [  0] = {        0x00 },
  [  1] = {        0x80 },
  [  2] = {        0xc0 },
  [  3] = {        0xe0 },
  [  4] = {        0xf0 },
  [  5] = {        0xf8 },
  [  6] = {        0xfc },
  [  7] = {        0xfe },

  [  8] = { FFx1,  0x00 },
  [  9] = { FFx1,  0x80 },
  [ 10] = { FFx1,  0xc0 },
  [ 11] = { FFx1,  0xe0 },
  [ 12] = { FFx1,  0xf0 },
  [ 13] = { FFx1,  0xf8 },
  [ 14] = { FFx1,  0xfc },
  [ 15] = { FFx1,  0xfe },

  [ 16] = { FFx2,  0x00 },
  [ 17] = { FFx2,  0x80 },
  [ 18] = { FFx2,  0xc0 },
  [ 19] = { FFx2,  0xe0 },
  [ 20] = { FFx2,  0xf0 },
  [ 21] = { FFx2,  0xf8 },
  [ 22] = { FFx2,  0xfc },
  [ 23] = { FFx2,  0xfe },
  [ 24] = { FFx3,  0x00 },

  [ 25] = { FFx3,  0x80 },
  [ 26] = { FFx3,  0xc0 },
  [ 27] = { FFx3,  0xe0 },
  [ 28] = { FFx3,  0xf0 },
  [ 29] = { FFx3,  0xf8 },
  [ 30] = { FFx3,  0xfc },
  [ 31] = { FFx3,  0xfe },

  [ 32] = { FFx4,  0x00 },
  [ 33] = { FFx4,  0x80 },
  [ 34] = { FFx4,  0xc0 },
  [ 35] = { FFx4,  0xe0 },
  [ 36] = { FFx4,  0xf0 },
  [ 37] = { FFx4,  0xf8 },
  [ 38] = { FFx4,  0xfc },
  [ 39] = { FFx4,  0xfe },

  [ 40] = { FFx5,  0x00 },
  [ 41] = { FFx5,  0x80 },
  [ 42] = { FFx5,  0xc0 },
  [ 43] = { FFx5,  0xe0 },
  [ 44] = { FFx5,  0xf0 },
  [ 45] = { FFx5,  0xf8 },
  [ 46] = { FFx5,  0xfc },
  [ 47] = { FFx5,  0xfe },

  [ 48] = { FFx6,  0x00 },
  [ 49] = { FFx6,  0x80 },
  [ 50] = { FFx6,  0xc0 },
  [ 51] = { FFx6,  0xe0 },
  [ 52] = { FFx6,  0xf0 },
  [ 53] = { FFx6,  0xf8 },
  [ 54] = { FFx6,  0xfc },
  [ 55] = { FFx6,  0xfe },

  [ 56] = { FFx7,  0x00 },
  [ 57] = { FFx7,  0x80 },
  [ 58] = { FFx7,  0xc0 },
  [ 59] = { FFx7,  0xe0 },
  [ 60] = { FFx7,  0xf0 },
  [ 61] = { FFx7,  0xf8 },
  [ 62] = { FFx7,  0xfc },
  [ 63] = { FFx7,  0xfe },

  [ 64] = { FFx8,  0x00 },
  [ 65] = { FFx8,  0x80 },
  [ 66] = { FFx8,  0xc0 },
  [ 67] = { FFx8,  0xe0 },
  [ 68] = { FFx8,  0xf0 },
  [ 69] = { FFx8,  0xf8 },
  [ 70] = { FFx8,  0xfc },
  [ 71] = { FFx8,  0xfe },

  [ 72] = { FFx9,  0x00 },
  [ 73] = { FFx9,  0x80 },
  [ 74] = { FFx9,  0xc0 },
  [ 75] = { FFx9,  0xe0 },
  [ 76] = { FFx9,  0xf0 },
  [ 77] = { FFx9,  0xf8 },
  [ 78] = { FFx9,  0xfc },
  [ 79] = { FFx9,  0xfe },

  [ 80] = { FFx10, 0x00 },
  [ 81] = { FFx10, 0x80 },
  [ 82] = { FFx10, 0xc0 },
  [ 83] = { FFx10, 0xe0 },
  [ 84] = { FFx10, 0xf0 },
  [ 85] = { FFx10, 0xf8 },
  [ 86] = { FFx10, 0xfc },
  [ 87] = { FFx10, 0xfe },

  [ 88] = { FFx11, 0x00 },
  [ 89] = { FFx11, 0x80 },
  [ 90] = { FFx11, 0xc0 },
  [ 91] = { FFx11, 0xe0 },
  [ 92] = { FFx11, 0xf0 },
  [ 93] = { FFx11, 0xf8 },
  [ 94] = { FFx11, 0xfc },
  [ 95] = { FFx11, 0xfe },

  [ 96] = { FFx12, 0x00 },
  [ 97] = { FFx12, 0x80 },
  [ 98] = { FFx12, 0xc0 },
  [ 99] = { FFx12, 0xe0 },
  [100] = { FFx12, 0xf0 },
  [101] = { FFx12, 0xf8 },
  [102] = { FFx12, 0xfc },
  [103] = { FFx12, 0xfe },

  [104] = { FFx13, 0x00 },
  [105] = { FFx13, 0x80 },
  [106] = { FFx13, 0xc0 },
  [107] = { FFx13, 0xe0 },
  [108] = { FFx13, 0xf0 },
  [109] = { FFx13, 0xf8 },
  [110] = { FFx13, 0xfc },
  [111] = { FFx13, 0xfe },

  [112] = { FFx14, 0x00 },
  [113] = { FFx14, 0x80 },
  [114] = { FFx14, 0xc0 },
  [115] = { FFx14, 0xe0 },
  [116] = { FFx14, 0xf0 },
  [117] = { FFx14, 0xf8 },
  [118] = { FFx14, 0xfc },
  [119] = { FFx14, 0xfe },

  [120] = { FFx15, 0x00 },
  [121] = { FFx15, 0x80 },
  [122] = { FFx15, 0xc0 },
  [123] = { FFx15, 0xe0 },
  [124] = { FFx15, 0xf0 },
  [125] = { FFx15, 0xf8 },
  [126] = { FFx15, 0xfc },
  [127] = { FFx15, 0xfe },

  [128] = { FFx16       },
};

/* Table to map last byte of mask to the number of bits that byte adds
 * to the prefix length.
 *
 * NB: this ignores all bits from the MS '0' bit onwards.
 */
static const u_char masklen_byte[256] =
{ /*       x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF */
  /* 0x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 1x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 2x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 3x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 4x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 5x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 6x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 7x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 8x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 9x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* Ax */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* Bx */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* Cx */  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  /* Dx */  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  /* Ex */  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  /* Fx */  4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8,
  /*       x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF */
} ;

/* Table to test for valid mask byte.
 *
 * Returns:  0 => not a valid prefix byte
 *         > 0 == number of prefix bits the byte contributes + 1
 */
static const u_char masklen_byte_valid[256] =
{
    [0x00] = 0 + 1,
    [0x80] = 1 + 1,
    [0xC0] = 2 + 1,
    [0xE0] = 3 + 1,
    [0xF0] = 4 + 1,
    [0xF8] = 5 + 1,
    [0xFC] = 6 + 1,
    [0xFE] = 7 + 1,
    [0xFF] = 8 + 1,
} ;

/* Table to count number of leading '0' bits before the ms '1'.
 *
 * This is the (an) inverse of masklen_byte -- which is the count of leanding
 * '1' bits before the ms '0'.
 */
static const u_char masklen_byte_inverse[256] =
{ /*       x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF */
  /* 0x */  8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
  /* 1x */  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  /* 2x */  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  /* 3x */  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  /* 4x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 5x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 6x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 7x */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  /* 8x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* 9x */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Ax */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Bx */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Cx */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Dx */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Ex */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /* Fx */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  /*       x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF */
} ;


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
    if (((n->u.n32[i] ^ p->u.n32[i]) & in_addr_mask_n[m]) != 0)
      return 0;

  while (i--)
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
    if (((p1->u.n32[i] ^ p2->u.n32[i]) & in_addr_mask_n[m]) != 0)
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

  d = ntohl(d) ;

  if (d > 0x0000FFFF)
    {
      if (d > 0x00FFFFFF)
        return (i * 32) +  0 + masklen_byte_inverse[(d >> 24) & 0xFF] ;
      else
        return (i * 32) +  8 + masklen_byte_inverse[(d >> 16) & 0xFF] ;
    }
  else
    {
      if (d > 0x000000FF)
        return (i * 32) + 16 + masklen_byte_inverse[(d >>  8) & 0xFF] ;
      else
        return (i * 32) + 24 + masklen_byte_inverse[(d >>  0) & 0xFF] ;
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
  netmask->s_addr = in_addr_mask_n[masklen <= IPV4_MAX_BITLEN
                                                  ? masklen : IPV4_MAX_BITLEN] ;
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
  uint32_t ip_h ;

  ip_h = ntohl(netmask.s_addr) ;

  if (ip_h > 0xFFFF0000)
    if (ip_h > 0xFFFFFF00)
      return 24 + masklen_byte[(ip_h >>  0) & 0xFF] ;
    else
      return 16 + masklen_byte[(ip_h >>  8) & 0xFF] ;
  else
    if (ip_h > 0xFF000000)
      return  8 + masklen_byte[(ip_h >> 16) & 0xFF] ;
    else
      return  0 + masklen_byte[(ip_h >> 24) & 0xFF] ;
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
  uint32_t ip_h ;

  ip_h = ntohl(netmask.s_addr) ;

  return (ip_h | (ip_h - 1)) == 0xFFFFFFFF ;

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

/* Apply mask to IPv4 prefix (network byte order). */
void
apply_mask_ipv4 (struct prefix_ipv4 *p)
{
  qassert (p->prefixlen >= 0 && p->prefixlen <= IPV4_MAX_BITLEN);
  p->prefix.s_addr &= in_addr_mask_n[ p->prefixlen <= IPV4_MAX_BITLEN
                                    ? p->prefixlen :  IPV4_MAX_BITLEN ] ;
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
ip6_masklen (struct in6_addr netmask)
{
  uint i ;
  byte b ;

  i = 0;
  while ((b = netmask.s6_addr[i]) == 0xff)
    {
      ++i ;

      if (i == IPV6_MAX_BYTELEN)
        return IPV6_MAX_BITLEN ;

      confirm(IPV6_MAX_BITLEN == (IPV6_MAX_BYTELEN * 8)) ;
    } ;

  return (i * 8) + masklen_byte[b] ;
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
int
ip6_mask_check (struct in6_addr netmask)
{
  uint i, l ;
  byte b ;

  i = 0;
  while ((b = netmask.s6_addr[i]) == 0xff)
    {
      ++i ;

      if (i == IPV6_MAX_BYTELEN)
        return IPV6_MAX_BITLEN ;

      confirm(IPV6_MAX_BITLEN == (IPV6_MAX_BYTELEN * 8)) ;
    } ;

  l = masklen_byte_valid[b] ;

  if (l == 0)
    return -1 ;

  l += (i * 8) - 1 ;

  while (i < (IPV6_MAX_BYTELEN - 1))
    {
      if (netmask.s6_addr[++i] != 0)
        return -1 ;
    } ;

  return l ;
} ;

void
masklen2ip6 (const uint masklen, struct in6_addr *netmask)
{
//assert (masklen >= 0 && masklen <= IPV6_MAX_BITLEN);
  memcpy (netmask,
            &in_addr6_mask_n[masklen <= IPV6_MAX_BITLEN ? masklen
                                                        : IPV6_MAX_BITLEN],
              sizeof (struct in6_addr));
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
      p->prefixlen = ip6_masklen (mask->sin6.sin6_addr);
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


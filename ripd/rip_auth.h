/*
 * This file, a part of Quagga, implements RIP packet authentication.
 *
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef QUAGGA_RIP_AUTH_H_
#define QUAGGA_RIP_AUTH_H_

#include "stream.h"       /* for struct stream              */
#include "ripd/ripd.h"    /* for struct rip_packet          */
#include "ripd/rip_interface.h"

/* RIPv2 special RTE family types */
#define RIP_FAMILY_AUTH                  0xffff

/* RIPv2 authentication types, for RIP_FAMILY_AUTH RTE's */
#define RIP_NO_AUTH                0
#define RIP_AUTH_DATA              1
#define RIP_AUTH_SIMPLE_PASSWORD   2
#define RIP_AUTH_HASH              3

#define RIP_AUTH_ALGO_MD5          1
#define RIP_AUTH_ALGO_SHA1         2
#define RIP_AUTH_ALGO_SHA256       3
#define RIP_AUTH_ALGO_SHA384       4
#define RIP_AUTH_ALGO_SHA512       5

/* RIPv2 Simple authentication */
#define RIP_AUTH_SIMPLE_SIZE		16

/* RIPv2 MD5 authentication. */
#define RIP_AUTH_MD5_SIZE               16
#define RIP_AUTH_MD5_COMPAT_SIZE        RIP_RTE_SIZE
#define RIP_AUTH_SHA1_SIZE              20
#define RIP_AUTH_SHA256_SIZE            32
#define RIP_AUTH_SHA384_SIZE            48
#define RIP_AUTH_SHA512_SIZE            64

#ifdef HAVE_LIBGCRYPT
#define RIP_AUTH_MAX_SIZE               64
#else
#define RIP_AUTH_MAX_SIZE               16
#endif /* HAVE_LIBGCRYPT */

struct rip_auth_rte
{
  u_int16_t family; /* 0xFFFF */
  u_int16_t type;   /* 0x0001/0x0002/0x0003 */
  union
  {
    char password[RIP_AUTH_SIMPLE_SIZE];   /* type == 0x0002 */
    struct
    {
      u_int16_t packet_len;
      u_int8_t  key_id;
      u_int8_t  auth_len;
      u_int32_t sequence;
      u_int32_t reserved1; /* MBZ */
      u_int32_t reserved2; /* MBZ */
    } hash_info;                           /* type == 0x0003 */
    u_char hash_digest[RIP_AUTH_MAX_SIZE]; /* type == 0x0001 */
  } u;
};

extern int rip_auth_check_packet (struct rip_interface *, struct sockaddr_in *, struct rip_packet *, const size_t);
extern int rip_auth_make_packet (struct rip_interface *, struct stream *, struct stream *, const u_int8_t, const u_int8_t);
extern void rip_auth_dump_ffff_rte (struct rip_auth_rte *);
extern unsigned rip_auth_allowed_inet_rtes (struct rip_interface *, const u_char);

#endif /* QUAGGA_RIP_AUTH_H_ */

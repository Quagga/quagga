/*
 * This file, a part of Quagga, implements RIP peer management.
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

#ifndef QUAGGA_RIP_PEER_H_
#define QUAGGA_RIP_PEER_H_

#include "command.h"
#include "thread.h"

/* RIP peer information. */
struct rip_peer
{
  /* Peer address. */
  struct in_addr addr;

  /* Peer RIP tag value. */
  int domain;

  /* Last update time. */
  time_t uptime;

  /* Peer RIP version. */
  u_char version;

  /* Statistics. */
  int recv_badpackets;
  int recv_badroutes;

  /* Timeout thread. */
  struct thread *t_timeout;

  /* crypto sequence number */
  u_int32_t seqno;
};

extern void rip_peer_init (void);
extern void rip_peer_update (struct sockaddr_in *, u_char);
extern void rip_peer_bad_route (struct sockaddr_in *);
extern void rip_peer_bad_packet (struct sockaddr_in *);
extern u_int32_t rip_peer_getseqno (struct in_addr *);
extern void rip_peer_setseqno (struct in_addr *, const u_int32_t);
extern void rip_peer_display (struct vty *);
extern struct rip_peer *rip_peer_lookup (struct in_addr *);
extern struct rip_peer *rip_peer_lookup_next (struct in_addr *);

#endif /* QUAGGA_RIP_PEER_H_ */

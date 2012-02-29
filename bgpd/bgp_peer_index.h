/* BGP Peer Index -- header
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _QUAGGA_BGP_PEER_INDEX_H
#define _QUAGGA_BGP_PEER_INDEX_H

#include "bgpd/bgp_common.h"

#include "lib/sockunion.h"

/*==============================================================================
 * The Peer Index maps:
 *
 *   * IP address (name of peer)
 *   * peer_id    (ordinal of peer)
 *
 * To the bgp_peer_index_entry.
 */
typedef struct bgp_peer_index_entry* bgp_peer_index_entry ;

typedef unsigned bgp_peer_id_t ;

struct bgp_peer_index_entry
{
  bgp_peer_index_entry  next_free ; /* for list of free peer_id's       */
                                /* points to self if entry is in use    */

  bgp_peer      peer ;          /* NULL if entry is not in use          */

  sockunion_t   su ;            /* The "name".                          */

  bgp_peer_id_t id ;            /* maps IP address to peer_id           */
} ;

enum { bgp_peer_id_null = 0 } ; /* no peer can have id == 0     */

/*==============================================================================
 * Functions
 */
extern void bgp_peer_index_init(void* parent) ;
extern void bgp_peer_index_mutex_init(void) ;
extern void bgp_peer_index_reset(void) ;
extern void bgp_peer_index_mutex_free(void) ;
extern void bgp_peer_index_register(bgp_peer peer, union sockunion* su) ;
extern void bgp_peer_index_deregister(bgp_peer peer, union sockunion* su) ;
extern bgp_peer bgp_peer_index_seek(sockunion su) ;
extern bgp_peer_index_entry bgp_peer_index_seek_entry(sockunion su) ;
extern void bgp_peer_index_set_session(bgp_peer peer, bgp_session session) ;
extern bgp_connection bgp_peer_index_seek_accept(sockunion su, bool* p_found) ;

#endif /* _QUAGGA_BGP_PEER_INDEX_H */


/* bgpd mapping protocol values to names
   Copyright (C) 1996, 97, 98 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef _QUAGGA_BGP_NAMES_H
#define _QUAGGA_BGP_NAMES_H

#include "name_map.h"

/*------------------------------------------------------------------------------
 * Mapping various BGP protocol items to their names
 *
 * e.g. map_direct(bgp_fsm_status_map, prev_state).str
 */
extern const map_direct_t bgp_fsm_status_map ;
extern const map_direct_t bgp_peer_status_map ;
extern const map_direct_t bgp_message_type_map ;
extern const map_direct_t bgp_notify_msg_map ;
extern const map_direct_t bgp_notify_head_msg_map ;
extern const map_direct_t bgp_notify_open_msg_map ;
extern const map_direct_t bgp_notify_update_msg_map ;
extern const map_direct_t bgp_notify_cease_msg_map ;
extern const map_direct_t bgp_notify_capability_msg_map ;
extern const map_direct_t bgp_notify_unspecific_msg_map ;
extern const map_direct_t bgp_notify_unknown_msg_map ;
extern const map_direct_t bgp_origin_short_map ;
extern const map_direct_t bgp_origin_long_map ;
extern const map_direct_t bgp_attr_name_map ;
extern const map_direct_t bgp_afi_name_map ;

#endif /* _QUAGGA_BGP_NAMES_H */

/* BGP message writing -- in BGP Engine -- header
 * Copyright (C) 1999 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
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

#ifndef _QUAGGA_BGP_MSG_WRITE_H
#define _QUAGGA_BGP_MSG_WRITE_H

#include "misc.h"

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_route_refresh.h"

#include "lib/stream.h"
#include "lib/sockunion.h"

/*==============================================================================
 * Functions for use in BGP_Engine for construction and sending of BGP
 * messages.
 */
extern int bgp_msg_write_notification(bgp_connection connection,
                                                      bgp_notify notification) ;
extern int bgp_msg_send_keepalive(bgp_connection connection, bool must_send) ;
extern int bgp_msg_send_open(bgp_connection connection,
                                                    bgp_open_state open_state) ;
extern int bgp_msg_send_route_refresh(bgp_connection connection,
                                                         bgp_route_refresh rr) ;
extern int bgp_msg_send_update(bgp_connection connection, struct stream* s) ;
extern int bgp_msg_send_end_of_rib(bgp_connection connection,
                                                      iAFI_t afi, iSAFI_t safi);

/*==============================================================================
 * Functions for the construction of BGP messages
 *
 * Pro tem some messages are constructed in the Routing Engine, and use these
 * when filling in the stream.
 */
extern void bgp_packet_set_marker(struct stream* s, uint8_t type) ;
extern uint bgp_packet_set_size (struct stream* s) ;

extern uint bgp_packet_check_size(struct stream* s, sockunion remote) ;

#endif /* _QUAGGA_BGP_MSG_WRITE_H */

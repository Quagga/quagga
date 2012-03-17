/* BGP-4 Finite State Machine
 * From RFC1771 [A Border Gateway Protocol 4 (BGP-4)]
 * Copyright (C) 1996, 97, 98 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) Chris Hall (GMCH), Highwayman
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

#ifndef _QUAGGA_BGP_FSM_H
#define _QUAGGA_BGP_FSM_H

#include "bgpd/bgp_common.h"

/* Prototypes. */

extern void
bgp_fsm_enable_session(bgp_session session) ;

extern void
bgp_fsm_disable_session(bgp_session session, bgp_notify notification) ;

extern int
bgp_fsm_pre_update(bgp_connection connection) ;

extern void
bgp_fsm_open_received(bgp_connection connection) ;

extern void
bgp_fsm_keepalive_received(bgp_connection connection) ;

extern void
bgp_fsm_notification_sent(bgp_connection connection) ;

extern void
bgp_fsm_exception(bgp_connection connection, bgp_session_event_t except,
                                                      bgp_notify notification) ;

extern void
bgp_fsm_io_fatal_error(bgp_connection connection, int err) ;

extern void
bgp_fsm_io_error(bgp_connection connection, int err) ;

extern void
bgp_fsm_connect_completed(bgp_connection connection, int err,
                                                   union sockunion* su_local,
                                                   union sockunion* su_remote) ;

extern void
bgp_fsm_notification_exception(bgp_connection connection,
                                                      bgp_notify notification) ;


#endif /* _QUAGGA_BGP_FSM_H */

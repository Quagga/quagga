/* BGP Message Read Handling -- header
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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


#ifndef BGP_MSG_READ_H_
#define BGP_MSG_READ_H_

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_notification.h"

extern bgp_size_t
bgp_msg_get_mlen(uint8_t* p, uint8_t* limit) ;

extern int
bgp_msg_check_header(bgp_connection connection);

typedef void bgp_msg_handler(bgp_connection connection, bgp_size_t size) ;

extern bgp_notify
bgp_msg_noms_o_bad_id(bgp_notify notification, bgp_id_t id) ;

#endif /* BGP_MSG_READ_H_ */

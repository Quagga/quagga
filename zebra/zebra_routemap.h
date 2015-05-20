/*
 * This file is part of Quagga.
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

#ifndef _QUAGGA_ZEBRA_ROUTEMAP_H
#define _QUAGGA_ZEBRA_ROUTEMAP_H
#include "routemap.h"

#define ZEBRA_RMAP_DEFAULT_UPDATE_TIMER 5 /* disabled by default */

route_map_result_t zebra_route_map_check (int family, int rib_type,
                                          struct prefix *,
                                          struct nexthop *,
                                          vrf_id_t);
void zebra_route_map_write_delay_timer(struct vty *);
route_map_result_t zebra_nht_route_map_check (int family,
                                                     int client_proto,
                                                     struct prefix *,
                                                     struct rib *,
                                                     struct nexthop *);
#endif /* _ZEBRA_ROUTEMAP_H */

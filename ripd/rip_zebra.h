/*
 * This file, a part of Quagga, implements ripd zclient-zserv interface.
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

#ifndef QUAGGA_RIP_ZEBRA_H_
#define QUAGGA_RIP_ZEBRA_H_

#include "command.h"
#include "prefix.h"

extern void rip_zebra_ipv4_add (struct prefix_ipv4 *, struct in_addr *, u_int32_t, u_char);
extern void rip_zebra_ipv4_delete (struct prefix_ipv4 *, struct in_addr *, u_int32_t);
extern void rip_zclient_reset (void);
extern void rip_zclient_init (void);
extern int rip_redistribute_check (int);
extern void rip_redistribute_clean (void);
extern int config_write_rip_redistribute (struct vty *, int);

#endif /* QUAGGA_RIP_ZEBRA_H_ */

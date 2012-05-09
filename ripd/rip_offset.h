/*
 * This file, a part of Quagga, implements RIP offset-list management.
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

#ifndef QUAGGA_RIP_OFFSET_H_
#define QUAGGA_RIP_OFFSET_H_

#include "command.h"
#include "prefix.h"

extern void rip_offset_init (void);
extern void rip_offset_clean (void);
extern int rip_offset_list_apply_in (struct prefix_ipv4 *, struct interface *, u_int32_t *);
extern int rip_offset_list_apply_out (struct prefix_ipv4 *, struct interface *, u_int32_t *);
extern int config_write_rip_offset_list (struct vty *);

#endif /* QUAGGA_RIP_OFFSET_H_ */

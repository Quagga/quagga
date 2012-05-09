/*
 * This file, a part of Quagga, implements RIP SNMP interface.
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

#ifndef QUAGGA_RIP_SNMP_H_
#define QUAGGA_RIP_SNMP_H_
#ifdef HAVE_SNMP

#include "if.h"

extern void rip_snmp_init (void);
extern void rip_ifaddr_add (struct interface *, struct connected *);
extern void rip_ifaddr_delete (struct interface *, struct connected *);

#endif /* HAVE_SNMP */
#endif /* QUAGGA_RIP_SNMP_H_ */

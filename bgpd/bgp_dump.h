/* BGP bd routine.
   Copyright (C) 1999 Kunihiro Ishiguro

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

#ifndef _QUAGGA_BGP_DUMP_H
#define _QUAGGA_BGP_DUMP_H

#include "bgp_connection.h"
#include <stdbool.h>

/*------------------------------------------------------------------------------
 */
extern void bgp_dump_init (void);
extern void bgp_dump_finish (void);

extern void bgp_dump_state (bgp_connection connection,
                                                    bgp_fsm_state_t new_state) ;
extern void bgp_dump_packet (bgp_connection connection) ;

/*------------------------------------------------------------------------------
 * These flags are set iff the respective dump function should be called.
 */
extern bool bgp_dump_state_flag ;
extern bool bgp_dump_packet_flag ;

#endif /* _QUAGGA_BGP_DUMP_H */

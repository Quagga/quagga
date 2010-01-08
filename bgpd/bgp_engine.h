/* BGP Engine pThread -- header
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

#ifndef _QUAGGA_BGP_ENGINE_H
#define _QUAGGA_BGP_ENGINE_H

#include "bgpd/bgp_common.h"

#include "lib/mqueue.h"
#include "lib/qpthreads.h"
#include "lib/qtimers.h"
#include "lib/qpselect.h"
#include "lib/qpnexus.h"

#include "lib/sockunion.h"

#ifndef Inline
#define Inline static inline
#endif




extern qpn_nexus p_bgp_engine ;

extern void
bgp_engine_start(void) ;



/*==============================================================================
 *
 */

/* Send given message to the BGP Engine -- ordinary
 */
Inline void
bgp_to_engine(mqueue_block mqb)
{
  mqueue_enqueue(p_bgp_engine->queue, mqb, 0) ;
} ;

/* Send given message to the BGP Engine -- priority
 */
Inline void
bgp_to_engine_priority(mqueue_block mqb)
{
  mqueue_enqueue(p_bgp_engine->queue, mqb, 1) ;
} ;

#endif /* QUAGGA_BGP_ENGINE_H */

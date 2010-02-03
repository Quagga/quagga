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

#include "bgpd/bgpd.h"

#include "lib/mqueue.h"
#include "lib/qpnexus.h"
#include "lib/log.h"

#ifndef Inline
#define Inline static inline
#endif



/*==============================================================================
 *
 */

static unsigned bgp_engine_queue_thresh_up   = 0 ;
static unsigned bgp_engine_queue_thresh_down = 0 ;
static unsigned peering_engine_queue_thresh_up   = 0 ;
static unsigned peering_engine_queue_thresh_down = 0 ;

Inline void
bgp_queue_logging(const char* name, unsigned count, unsigned* up,
                                                    unsigned* down)
{
  if (count > *up)
    {
      if (*up != 0)
        {
          zlog_debug("%s queue up to %u entries", name, count) ;

          *up  *= 2 ;
          *down = *up / 4 ;
        }
      else
        *up   = 32 ;
    } ;

  if (count < *down)
    {
      zlog_debug("%s queue down to %u entries", name, count) ;

      if (*up == 32)
        *down = 0 ;
      else
        {
          *up  /= 2 ;
          *down = *up / 2 ;
        } ;
    } ;  ;
} ;

/* Send given message to the BGP Engine -- ordinary
 */
Inline void
bgp_to_bgp_engine(mqueue_block mqb)
{
  mqueue_enqueue(bgp_nexus->queue, mqb, 0) ;
  bgp_queue_logging("BGP Engine", bgp_nexus->queue->count,
                   &bgp_engine_queue_thresh_up, &bgp_engine_queue_thresh_down) ;
} ;

/* Send given message to the BGP Engine -- priority
 */
Inline void
bgp_to_bgp_engine_priority(mqueue_block mqb)
{
  mqueue_enqueue(bgp_nexus->queue, mqb, 1) ;
  bgp_queue_logging("BGP Engine", bgp_nexus->queue->count,
                   &bgp_engine_queue_thresh_up, &bgp_engine_queue_thresh_down) ;
} ;

/*==============================================================================
 *
 */

/* Send given message to the Peering Engine -- ordinary
 */
Inline void
bgp_to_peering_engine(mqueue_block mqb)
{
  mqueue_enqueue(routing_nexus->queue, mqb, 0) ;
  bgp_queue_logging("Peering Engine", routing_nexus->queue->count,
           &peering_engine_queue_thresh_up, &peering_engine_queue_thresh_down) ;
} ;

/* Send given message to the Peering Engine -- priority
 */
Inline void
bgp_to_peering_engine_priority(mqueue_block mqb)
{
  mqueue_enqueue(routing_nexus->queue, mqb, 1) ;
  bgp_queue_logging("Peering Engine", routing_nexus->queue->count,
           &peering_engine_queue_thresh_up, &peering_engine_queue_thresh_down) ;
} ;

#endif /* QUAGGA_BGP_ENGINE_H */

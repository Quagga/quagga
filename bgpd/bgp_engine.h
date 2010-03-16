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

enum { qdebug =
#ifdef QDEBUG
  1
#else
  0
#endif
};

/*==============================================================================
 *
 */

struct queue_stats
{
  unsigned        count ;
  unsigned long   total ;
  unsigned        max ;
  unsigned        recent ;

  unsigned        xon ;
  unsigned        event ;
  unsigned        update ;
} ;

static struct queue_stats bgp_engine_queue_stats ;
static struct queue_stats routing_engine_queue_stats ;

Inline void
bgp_queue_logging(const char* name, mqueue_queue mq, struct queue_stats* stats)
{
  double average ;
  unsigned my_count ;
  mqueue_block mqb ;

  ++stats->count ;

  qpt_mutex_lock(&mq->mutex) ;

  if (mq->count > stats->max)
    stats->max    = mq->count ;
  if (mq->count > stats->recent)
    stats->recent = mq->count ;

  stats->total += mq->count ;

  if (stats->count < 1000)
    {
      qpt_mutex_unlock(&mq->mutex) ;
      return ;
    } ;

  my_count = 0 ;

  mqb = mq->head ;
  while (mqb != NULL)
    {
      ++my_count ;
      mqb = mqb->next ;
    } ;

  assert(my_count == mq->count) ;

  qpt_mutex_unlock(&mq->mutex) ;

  average = stats->total ;
  average /= stats->count ;

  zlog_debug("%s queue: max=%u  recent: max=%u av=%3.1f (%u) [x=%u e=%u u=%u]",
                    name, stats->max, stats->recent, average, stats->count,
                                      stats->xon, stats->event, stats->update) ;

  stats->recent = 0 ;
  stats->count  = 0 ;
  stats->total  = 0 ;

  stats->event  = 0 ;
  stats->update = 0 ;
  stats->xon    = 0 ;
} ;

/* Send given message to the BGP Engine -- ordinary
 */
Inline void
bgp_to_bgp_engine(mqueue_block mqb)
{
  mqueue_enqueue(bgp_nexus->queue, mqb, 0) ;
  if (qdebug)
    bgp_queue_logging("BGP Engine", bgp_nexus->queue, &bgp_engine_queue_stats) ;
} ;

/* Send given message to the BGP Engine -- priority
 */
Inline void
bgp_to_bgp_engine_priority(mqueue_block mqb)
{
  mqueue_enqueue(bgp_nexus->queue, mqb, 1) ;
  if (qdebug)
    bgp_queue_logging("BGP Engine", bgp_nexus->queue, &bgp_engine_queue_stats) ;
} ;

/*==============================================================================
 *
 */

/* Send given message to the Routing Engine -- ordinary
 */
Inline void
bgp_to_routing_engine(mqueue_block mqb)
{
  mqueue_enqueue(routing_nexus->queue, mqb, 0) ;
  if (qdebug)
    bgp_queue_logging("Routing Engine", routing_nexus->queue,
                                                 &routing_engine_queue_stats) ;
} ;

/* Send given message to the Routing Engine -- priority
 */
Inline void
bgp_to_routing_engine_priority(mqueue_block mqb)
{
  mqueue_enqueue(routing_nexus->queue, mqb, 1) ;
  if (qdebug)
    bgp_queue_logging("Routing Engine", routing_nexus->queue,
                                                  &routing_engine_queue_stats) ;
} ;

#endif /* QUAGGA_BGP_ENGINE_H */

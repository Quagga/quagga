/* BGP Engine pThread -- functions
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

#include <zebra.h>

#include "bgpd/bgp_engine.h"

#include "lib/memory.h"
#include "lib/symtab.h"

/*==============================================================================
 * The BGP Engine pThread contains:
 *
 *   * the BGP Finite State Machine (FSM) for BGP Sessions
 *   * the encoding and decoding of BGP protocol messages
 *   * all related socket handling and I/O
 *   * all related timers
 *
 * The BGP Engine communicates with the BGP Routeing pthread(s) via two Message
 * Queues.
 *
 * There are also shared pools of data:
 *
 *   * the BGP session structures, which ...
 *   * the Attributes Store
 *   * the Prefixes Store
 *   ...
 *
 * Other pthread-safe facilities used:
 *
 *   * logging
 *   * privilege raising/lowering
 *   ...
 *
 * At the heart of the BGP Engine pthread is a select for the socket I/O, a
 * Message Queue reader and a timer handler.  The Message Queue uses SIGUSR2 to
 * kick the pthread into action if it is stopped on the select.
 *
 */

/*==============================================================================
 * The qpnexus for the BGP Engine.
 *
 *
 */

qpn_nexus p_bgp_engine ;

static struct qpn_nexus  bgp_engine ;


/*==============================================================================
 * Start the BGP Engine Thread.
 *
 * Initialise the Engine Thread qpnexus
 *
 */

static void* bgp_engine_loop(void* arg) ;

extern void
bgp_engine_start(void)
{
  p_bgp_engine = qpn_init_new(&bgp_engine) ;

  p_bgp_engine->start     = bgp_engine_loop ;

  p_bgp_engine->thread_id = qpt_thread_self() ;

  p_bgp_engine->selection = qps_selection_init_new(NULL) ;
  p_bgp_engine->pile      = qtimer_pile_init_new(NULL) ;
  p_bgp_engine->queue     = mqueue_init_new(NULL, mqt_signal_broadcast) ;
  p_bgp_engine->mts       = mqueue_thread_signal_init(NULL,
                                           p_bgp_engine->thread_id, SIGMQUEUE) ;

  qpn_exec(p_bgp_engine) ;
} ;

/*==============================================================================
 * The BGP Engine Thread main loop
 *
 * Processes:
 *
 *   1) connections with pending work -- local queue.
 *
 *      When a connection fills its output buffers, any further messages
 *      requiring output are placed on the connection's pending queue.
 *
 *      When the output buffers empty sufficiently, some of those messages
 *      can (finally) be processed.
 *
 *      So this is done first.
 *
 *      [This is also where stopped connections are finally reaped.]
 *
 *   2) messages coming from the Routeing Engine -- mqueue_queue.
 *
 *      These will mostly be BGP UPDATE messages, which will either be
 *      processed into the relevant connection's output buffers, or end up
 *      on its pending queue.
 *
 *      There is a flow control mechanism to prevent the Routeing Engine from
 *      flooding the BGP Engine with UPDATE messages.
 *
 *      Other messages start/stop sessions and so on.
 *
 *   3) I/O -- qpselect
 *
 *      This deals with all active sockets for read/write/connect/accept.
 *
 *      Each time a socket is readable, one message is read and dispatched to
 *      the Routeing Engine (or otherwise dealt with by the FSM).
 *
 *      Each time a socket is writable, as much as possible is written to it
 *      from the connection's write buffer.  When the write buffer is drained,
 *      the connection goes back onto the local queue.
 *
 *   4) Timers -- qtimers
 *
 *      Which generate FSM events.
 *
 *
 */






/*==============================================================================
 * BGP Session Handling
 */


/* BGP Engine Action: enable given session
 *
 *   arg0   -- bgp_session to be enabled
 *
 *
 */

extern void
bgp_session_enable_action(mqueue_block mqb, mqb_flag_t flag)
{
  bgp_session session ;
  bgp_connection connection ;

  /* Construct bgp_connection for the new session.              */

  session    = mqb_get_arg0(mqb) ;
  connection = bgp_connection_init_new(NULL, session, 0) ;

  /*                                                            */

} ;


/*==============================================================================
 * The qpnexus for the BGP Engine.
 *
 *
 */



/*==============================================================================
 * The write queue for the BGP Engine
 *
 * Each connection has a single message buffer.  When that has yet to be
 *
 *
 */



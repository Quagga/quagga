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

extern qpn_nexus bgp_nexus ;

/*==============================================================================
 * Start the BGP Engine Thread.
 *
 * Initialise the Engine Thread qpnexus
 *
 */

/* BGP Engine side of bgp_engine_start() must call bgp_open_listeners()
 * for which it needs the port and address from command line.
 *
 * Implemented in bgp_main.c
 */

/*==============================================================================
 * Stop the BGP Engine Thread.
 *
 */

/* BGP Engine side of bgp_engine_stop() must call bgp_close_listeners()
 *
 * Implemented in bgp_main.c
 */

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
 * Implemented in qpnexus.c
 *
 */


/*==============================================================================
 * The write queue for the BGP Engine
 *
 * Each connection has a single message buffer.  When that has yet to be
 *
 *
 */



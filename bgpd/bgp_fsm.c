/* BGP-4 Finite State Machine
 * From RFC1771 [A Border Gateway Protocol 4 (BGP-4)]
 * Copyright (C) 1996, 97, 98 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
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
#include "bgpd/bgp.h"

#include "log.h"

#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_fsm.h"

#include "lib/qtimers.h"

#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_packet.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_dump.h"
#include "bgpd/bgp_open.h"
#ifdef HAVE_SNMP
#include "bgpd/bgp_snmp.h"
#endif /* HAVE_SNMP */

/*==============================================================================
 * The BGP Finite State Machine
 *
 * The state machine is represented as a table, indexed by [state, event],
 * giving an action to be performed to deal with the event and the state that
 * will advance to (or stay at).
 *
 * In some cases the action routine may override the the default new state.
 *
 * When a new state is entered, bgp_fsm_state_change() is called to complete
 * the transition (in particular to set/unset timers).
 *
 * The fsm action functions are called with the session locked.
 *
 *------------------------------------------------------------------------------
 * FSM "events"
 *
 * These are raised when:
 *
 *   * the BGP Engine receives instructions from the Routeing Engine
 *
 *   * some I/O operations complete
 *
 *   * timers go off
 *
 * and the mechanism is to call bgp_fsm_event().
 *
 * Note that the event is dealt with *immediately* -- there is no queueing of
 * events.  The problem with queueing events is that the state of the connection
 * is "out of date" until its event queue is emptied, so decisions made in the
 * meantime may be wrong.
 *
 * The downside is that need to deal with the possibility of events being
 * raised while the FSM is in some indeterminate state while processing an
 * event for the current connection.  This requires a little care.
 *
 * Timers, messages, and qpselect actions all occur outside the FSM.  So there
 * is no problem there.
 *
 * However, the FSM does some I/O operations -- notably write() and connect().
 * These may complete immediately, and may need to trigger a new event.  To
 * handle this, the connection can set a "post" event, to be processed at the
 * tail end of the current event processing.
 *
 * Note that there is only one level of "post" event.  The FSM only ever issues
 * one I/O operation per event.  (It's a RULE.)
 *
 *------------------------------------------------------------------------------
 * Primary and Secondary Connections
 *
 * To support BGP's "symmetrical" open strategy, this code allows for two
 * connections to be made for a session -- one connect() and one accept().
 * If two connections are made, only one will reach Established state.
 *
 * Up to Established state, the primary connection will be the out-bound
 * connect() connection (if allowed) and the secondary will be the in-bound
 * accept() connection (if allowed).
 *
 * The session->accept flag is set iff the secondary connection is prepared to
 * accept a connection.  The flag is cleared as soon as a connection is
 * accepted.
 *
 * When a session is enabled, the allowed connections are initialised and
 * a BGP_Start event issued for each one.
 *
 *------------------------------------------------------------------------------
 * Error Handling.
 *
 * I/O and other operations may fail.  When they do one of four events may
 * be generated:
 *
 *   a. BGP_Stop
 *
 *      The functions bgp_fsm_stop_connection() and bgp_fsm_stop_session()
 *      set the cause of stop and generate a BGP_Stop event for one or both
 *      connections.  The session is stopped if no connections remain.
 *
 *      (The FSM itself uses bgp_fsm_set_stopping() before moving to
 *       Stopping state.)
 *
 *   b. TCP_connection_closed ("soft" error)
 *
 *      A read or write operation finds that the connection has been closed.
 *
 *      This is raised when a read operation returns 0 bytes.
 *
 *      Is also raised when read or write see the errors:
 *
 *        ECONNRESET, ENETDOWN, ENETUNREACH, EPIPE or ETIMEDOUT
 *
 *      Other errors are reported as TCP_fatal_error.
 *
 *      The function bgp_fsm_io_error() is used by read and write operations to
 *      signal an error -- it decides which event to generate.  (Error == 0 is
 *      used to signal a read operation that has returned 0.)
 *
 *   c. TCP_connection_open_failed ("soft" error)
 *
 *      A connect() operation has failed:
 *
 *        ECONNREFUSED, ECONNRESET, EHOSTUNREACH or ETIMEDOUT
 *
 *      Other errors are reported as TCP_fatal_error.
 *
 *      The function bgp_fsm_connect_completed() decides what event to generate.
 *      (It will generate TCP_connection_open if there is no error.)
 *
 *      All errors that accept() may raise are fatal.
 *
 *   d. TCP_fatal_error ("hard" error)
 *
 *      Raised by unexpected errors in connect/accept/read/write
 *
 *      The function bgp_fsm_io_fatal_error() will generate a TCP_fatal_error.
 */


/*==============================================================================
 * Functions to enable, stop, signal I/O events to, etc.  the FSM
 */

static void
bgp_fsm_set_stopping(bgp_connection connection, bgp_stopped_cause_t cause,
                                            bgp_notify notification, int both) ;

/*------------------------------------------------------------------------------
 * Enable the given connection -- which must be newly initialised.
 *
 * This is the first step in the FSM, and the connection advances to Idle.
 */

extern void
bgp_fsm_enable_connection(bgp_connection connection)
{
  assert(connection->state == bgp_fsm_Initial) ;
  bgp_fsm_event(connection, bgp_fsm_BGP_Start) ;
} ;

/*------------------------------------------------------------------------------
 * Bring given connection to a stop.
 *
 * If is the only connection for the session, then the session is stopped.
 *
 * Is given the reasons for the stop.  This function looks after releasing the
 * notification once it is finished with it.
 *
 * Records the reasons for the stop, and then generates a BGP_Stop event.
 *
 * This may be used to stop:
 *
 *   * as requested by Routeing Engine.  The notification, if any, should be a
 *     Cease.
 *
 *   * because a problem with a BGP packet has arisen.  The notification, if
 *     any, will describe the problem.
 *
 * Note that I/O problems are signalled by bgp_fsm_io_error().
 *
 * NB: may NOT be used within the FSM.
 *
 * NB: locks and unlocks the session.
 */
extern void
bgp_fsm_stop_connection(bgp_connection connection, bgp_stopped_cause_t cause,
                                                        bgp_notify notification)
{
  bgp_fsm_set_stopping(connection, cause, notification, 0) ;
  bgp_fsm_event(connection, bgp_fsm_BGP_Stop) ;
} ;

/*------------------------------------------------------------------------------
 * Bring given connection to a stop, and the other connection (if any), and
 * then the session.
 *
 * See bgp_fsm_stop_connection, above.
 */
extern void
bgp_fsm_stop_session(bgp_connection connection, bgp_stopped_cause_t cause,
                                                        bgp_notify notification)
{
  bgp_fsm_set_stopping(connection, cause, notification, 1) ;
  bgp_fsm_event(connection, bgp_fsm_BGP_Stop) ;
} ;

/*------------------------------------------------------------------------------
 * Signal a fatal I/O error on the given connection.
 *
 * Error to be reported as "TCP_fatal_error".
 */
extern void
bgp_fsm_io_fatal_error(bgp_connection connection, int err)
{
  connection->err = err ;

  plog_err (connection->log, "%s [Error] bgp IO error: %s",
            connection->host, safe_strerror(err)) ;

  bgp_fsm_event(connection, bgp_fsm_TCP_fatal_error) ;
} ;

/*------------------------------------------------------------------------------
 * Signal an I/O error on the given connection.
 *
 * This is used by read/write operations -- so not until the TCP connection
 * is up (which implies OpenSent state or later).
 *
 * It is assumed that the read/write code deals with EAGAIN/EWOULDBLOCK/EINTR
 * itself -- so only real errors are reported here.
 *
 * A read operation that returns zero is reported here as err == 0.
 *
 * The following are reported as "TCP_connection_closed":
 *
 *   0, ECONNRESET, ENETDOWN, ENETUNREACH, EPIPE or ETIMEDOUT
 *
 * All other errors are reported as "TCP_fatal_error".
 */
extern void
bgp_fsm_io_error(bgp_connection connection, int err)
{
  connection->err = err ;

  if (   (err == 0)
      || (err == ECONNRESET)
      || (err == ENETDOWN)
      || (err == ENETUNREACH)
      || (err == EPIPE)
      || (err == ETIMEDOUT) )
    {
      if (BGP_DEBUG(events, EVENTS))
        if (err == 0)
          plog_debug(connection->log,
                       "%s [Event] BGP connection closed fd %d",
                               connection->host, qps_file_fd(&connection->qf)) ;
        else
          plog_debug(connection->log,
                       "%s [Event] BGP connection closed fd %d (%s)",
                               connection->host, qps_file_fd(&connection->qf),
                                                           safe_strerror(err)) ;

      bgp_fsm_event(connection, bgp_fsm_TCP_connection_closed) ;
    }
  else
    bgp_fsm_io_fatal_error(connection, err) ;
} ;

/*------------------------------------------------------------------------------
 * Signal completion of a connect() or an accept() for the given connection.
 *
 * This is used by the connect() and accept() qpselect actions.  It is also
 * used if a connect() attempt fails immediately.
 *
 * If err == 0, then all is well: generate TCP_connection_open event.
 *
 * If err is one of:
 *
 *   ECONNREFUSED, ECONNRESET, EHOSTUNREACH or ETIMEDOUT
 *
 * generate TCP_connection_open_failed event.  (accept() does not return any of
 * these errors.)
 *
 * Other errors are reported as TCP_fatal_error.
 */
extern void
bgp_fsm_connect_completed(bgp_connection connection, int err)
{
  connection->err = err ;

  if (err == 0)
    bgp_fsm_event(connection, bgp_fsm_TCP_connection_open) ;
  else if (   (err == ECONNREFUSED)
           || (err == ECONNRESET)
           || (err == EHOSTUNREACH)
           || (err == ETIMEDOUT) )
    bgp_fsm_event(connection, bgp_fsm_TCP_connection_open_failed) ;
  else
    bgp_fsm_io_fatal_error(connection, err) ;
} ;

/*==============================================================================
 * For debug...
 */
#define BGP_FSM_DEBUG(connection, message) \
  if (BGP_DEBUG (fsm, FSM)) \
    plog_debug (connection->log, "%s [FSM] " message, connection->host)

/*==============================================================================
 * The FSM table and the finite state machine actions.
 */

typedef bgp_fsm_state_t
bgp_fsm_action(bgp_connection connection, bgp_fsm_state_t next_state,
                                          bgp_fsm_event_t event) ;

struct bgp_fsm {
  bgp_fsm_action*  action ;
  bgp_fsm_state_t  next_state ;
} ;

static bgp_fsm_action bgp_fsm_null ;
static bgp_fsm_action bgp_fsm_invalid ;
static bgp_fsm_action bgp_fsm_ignore ;
static bgp_fsm_action bgp_fsm_enter ;
static bgp_fsm_action bgp_fsm_start ;
static bgp_fsm_action bgp_fsm_restart ;
static bgp_fsm_action bgp_fsm_stop ;
static bgp_fsm_action bgp_fsm_open ;
static bgp_fsm_action bgp_fsm_failed ;
static bgp_fsm_action bgp_fsm_fatal ;
static bgp_fsm_action bgp_fsm_retry ;
static bgp_fsm_action bgp_fsm_error ;
static bgp_fsm_action bgp_fsm_expire ;
static bgp_fsm_action bgp_fsm_opened ;
static bgp_fsm_action bgp_fsm_establish ;
static bgp_fsm_action bgp_fsm_closed ;
static bgp_fsm_action bgp_fsm_kal_send ;
static bgp_fsm_action bgp_fsm_kal_recv ;
static bgp_fsm_action bgp_fsm_update ;
static bgp_fsm_action bgp_fsm_notified ;
static bgp_fsm_action bgp_fsm_done ;

static void
bgp_fsm_state_change(bgp_connection connection, bgp_fsm_state_t new_state) ;

/*------------------------------------------------------------------------------
 * Finite State Machine events
 *
 *    0. null_event
 *
 *       Do nothing.  As quietly as possible.
 *
 *       Never generated, so should not be seen !
 *
 *    1. BGP_Start
 *
 *         a. in Initial state  (-> Idle)
 *
 *            raised immediately after creating the connection.
 *
 *         b. in Idle state
 *
 *            raised on expiry of IdleHoldTime.
 *
 *            primary connection:   proceed to Connect
 *
 *            secondary connection: proceed to Accept
 *
 *       Cannot happen at any other time.
 *
 *    2. BGP_Stop
 *
 *         a. in all states:
 *
 *              -- from Routeing Engine -- at any time.
 *
 *              -- internally in the event of collision resolution.
 *
 *              -- internally, in the event of some protocol error -- once
 *                 connection is up and packets are being transferred.
 *
 *       The reason for stopping is set in the connection before the event is
 *       generated.
 *
 *    3. TCP_connection_open
 *
 *         a. primary connection:   in Connect state  (-> OpenSent)
 *
 *            raised when a connect() connection succeeds
 *
 *         b. secondary connection: in Active state  (-> OpenSent)
 *
 *            raised when an accept() connection is accepted.
 *
 *       Cannot happen at any other time.
 *
 *    4. TCP_connection_closed
 *
 *       Raised by "EOF" on read or by EPIPE and some other errors.
 *
 *         a. in OpenSent and OpenConfirm states
 *
 *            This may be because the the other end has detected a collision.
 *            It may be because the other end is being vexatious.
 *
 *            Fall back to Idle.
 *
 *         b. and Established state
 *
 *            Stop the session.
 *
 *       NB: any errors generated when the OPEN message is sent (on exit from
 *           Connect or Active states) are not delivered until has entered
 *           OpenSent state.
 *
 *       Cannot happen at any other time.
 *
 *    5. TCP_connection_open_failed ("soft" error)
 *
 *         a. in Connect State
 *
 *            raised if connect() fails eg: ECONNREFUSED or ETIMEDOUT
  *
 *       Cannot happen at any other time.  In particular, any errors during an
 *       accept() are reported as TCP_fatal_error.
 *
 *    6. TCP_fatal_error ("hard" error)
 *
 *         a. in all states other than Initial
 *
 *            raised by unexpected errors in connect/accept/read/write
 *
 *            Stops the connection and disables the type of connection.  So,
 *            for the remains of this session, will not attempt to <<<<<<<<<<<<<<<<
 *
 *    7. ConnectRetry_timer_expired
 *
 *         a. in either Connect or Active states ONLY.
 *
 *            Time to give up current connection attempt(s), and start trying
 *            to connect all over again.
 *
 *       Cannot happen at any other time.
 *
 *    8. Hold_Timer_expired
 *
 *         a. in OpenSent state
 *
 *            Time to give up waiting for an OPEN (or NOTIFICATION) from the
 *            other end.
 *
 *            Fall back to Idle.
 *
 *            For this state the RFC recommends a "large" value for the hold
 *            time -- and suggests 4 minutes.
 *
 *         b. in OpenConfirm state
 *
 *            Time to give up waiting for a KEEPALIVE to confirm the connection.
 *
 *            Fall back to Idle.
 *
 *            In this state the hold time used is that negotiated in the OPEN
 *            messages that have been exchanged.
 *
 *         c. in Established state
 *
 *            The session has failed.  Stop.
 *
 *            In this state the hold time used is that negotiated in the OPEN
 *            messages that have been exchanged.
 *
 *         d. in Stopping state
 *
 *            Time to give up trying to send NOTIFICATION and terminate the
 *            connection.
 *
 *       Cannot happen at any other time.
 *
 *    9. KeepAlive_timer_expired
 *
 *         a. in OpenConfirm and Established states
 *
 *            Time to send a KEEPALIVE message.
 *
 *            In these states the keepalive time used is that which follows
 *            from the hold time negotiated in the OPEN messages that have been
 *            exchanged.
 *
 *       Cannot happen at any other time.
 *
 *   10. Receive_OPEN_message
 *
 *       Generated by read action.
 *
 *         a. in OpenSent state -- the expected response
 *
 *            Proceed (via collision resolution) to OpenConfirm or Stopping.
 *
 *         b. in OpenConfirm state -- FSM error
 *
 *            Send NOTIFICATION.  Fall back to Idle.
 *
 *         c. in Established state -- FSM error
 *
 *            Terminate session.
 *
 *       Cannot happen at any other time (connection not up).
 *
 *   11. Receive_KEEPALIVE_message
 *
 *       Generated by read action.
 *
 *         a. in OpenSent state -- FSM error
 *
 *            Fall
 *
 *         b. in OpenConfirm state -- the expected response
 *
 *         c. in Established state -- expected
 *
 *       Cannot happen at any other time (connection not up).
 *
 *   12. Receive_UPDATE_message
 *
 *       Generated by read action.
 *
 *         a. in OpenSent and OpenConfirm states -- FSM error
 *
 *         b. in Established state -- expected
 *
 *       Cannot happen at any other time (connection not up).
 *
 *   13. Receive_NOTIFICATION_message
 *
 *       Generated by read action.
 *
 *         a. in OpenSent, OpenConfirm and Established states -- give up
 *            on the session.
 *
 *       Cannot happen at any other time (connection not up).
 *
 *   14. Sent_NOTIFICATION_message
 *
 *       Generated by write action when completed sending the message.
 *
 *         a. in Stopping state -- the desired outcome
 *
 *            Terminate the connection.
 *
 *       Cannot happen at any other time.
 */

/*------------------------------------------------------------------------------
 *  Finite State Machine structure
 */

static const struct bgp_fsm
bgp_fsm[bgp_fsm_last_state + 1][bgp_fsm_last_event + 1] =
{
  {
    /* bgp_fsm_Initial: initialised in this state...............................
     *
     * Expect only a BGP_Start event, which arms the IdleHoldTimer and advances
     * to the Idle state.
     *
     * Could (just) get a bgp_fsm_Stop if other connection stops immediately !
     *
     * A connection should be in this state for a brief period between being
     * initialised and set going.
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Initial},     /* null event                   */
    {bgp_fsm_enter,     bgp_fsm_Idle},        /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_Idle: waiting for IdleHoldTimer..................................
     *
     * When a session is enabled both its connections start in this state.
     * (Noting that an accept() only session starts only the secondary
     * connection and a connect() only session starts only the primary.)
     *
     * While in this state is waiting for the IdleHoldTimer to expire.  This
     * timer becomes longer if the peer misbehaves.
     *
     * If a connection stops at OpenState or OpenConfirm, may loop back through
     * Idle, with an increased IdleHoldTimer.
     *
     * In Idle state the connection is dormant.  (While the secondary is Idle,
     * no connections will be accepted.)
     *
     * If the peer keeps making or accepting TCP connections, and then dropping
     * them, then the IdleHoldTimer will grow to slow down the rate of vexatious
     * connections.
     *
     * When a connection falls back to Idle it will have been closed.
     *
     * The expected events are:
     *
     *   * BGP_Start -- generated by IdleHoldTimer expired
     *
     *     For primary connection:
     *
     *       Causes a connect() to be attempted.
     *
     *         * Connect state   -- if connect() OK, or failed "soft"
     *
     *         * Stopping state  -- if connect() failed "hard"
     *
     *           Bring connection and session to a dead stop.
     *
     *     For secondary connection:
     *
     *       Enables session->accept, and goes to "Active" state.
     *
     *     Note that bgp_fsm_start() decides on the appropriate next state.
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Idle},        /* null event                   */
    {bgp_fsm_start,     bgp_fsm_Connect},     /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_Connect: waiting for connect (and listen)........................
     *
     * Only the primary connection can be in this state.
     *
     * While in this state is waiting for connection to succeed or fail, or for
     * the ConnectRetryTimer to expire.
     *
     * The expected events are:
     *
     *   * TCP_connection_open
     *
     *     Send BGP OPEN message, arm the HoldTimer ("large" value) and advance
     *     to OpenSent.
     *
     *   * TCP_connection_open_fail ("soft" error)
     *
     *     Shut down the connection.  Stay in Connect state.
     *
     *     The ConnectRetryTimer is left running.
     *
     *   * TCP_fatal_error ("hard" error)
     *
     *     Bring connection and session to a dead stop.
     *
     *   * ConnectRetry_timer_expired
     *
     *     Shut down the connection.  Retry opening a connection.  Stay in
     *     Connect state.  Refresh the ConnectRetryTimer.
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Connect},     /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_open,      bgp_fsm_OpenSent},    /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_failed,    bgp_fsm_Connect},     /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_retry,     bgp_fsm_Connect},     /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_Active: waiting for listen (only)................................
     *
     * Only the secondary connection can be in this state.
     *
     * While in this state is waiting for an incoming connection to succeed or
     * for the ConnectRetryTimer to expire.
     *
     * The expected events are:
     *
     *   * TCP_connection_open
     *
     *     Send BGP OPEN message, arm the HoldTimer ("large" value) and advance
     *     to OpenSent.
     *
     *   * TCP_fatal_error
     *
     *     Bring connection and session to a dead stop.
     *
     *   * ConnectRetry_timer_expired
     *
     *     Stay in Active state.  Refresh the ConnectRetryTimer.
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Active},      /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_open,      bgp_fsm_OpenSent},    /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_retry,     bgp_fsm_Active},      /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_OpenSent: waiting for Open from the other end....................
     *
     * Both primary and secondary connections can be in this state.
     *
     * While in this state is waiting for a BGP OPEN to arrive or for the
     * HoldTimer ("large" value) to expire.
     *
     * The expected events are:
     *
     *   * Receive_OPEN_message
     *
     *     This means has received a satisfactory BGP OPEN from the other end,
     *     so the session is very nearly up.
     *
     *     If there is another connection, and it is in OpenConfirm state,
     *     then must now choose between the two -- terminating one or the
     *     other with a "Connection Collision Resolution" NOTIFICATION message.
     *
     *     If proceeding, send a BGP KEEPALIVE message (effectively ACK), arm
     *     HoldTimer and KeepliveTimer (as per negotiated values) and advance
     *     to OpenConfirm state.
     *
     *   * Receive_UPDATE_message
     *
     *     FSM error -- bring connection to a dead stop.
     *
     *   * Receive_KEEPALIVE_message
     *
     *     FSM error -- bring connection to a dead stop.
     *
     *   * Receive_NOTIFICATION_message
     *
     *     Bring connection to a dead stop.
     *
     *   * TCP_connection_closed
     *
     *     Close connection,
     *
     *   * TCP_fatal_error
     *
     *     Bring connection and session to a dead stop.
     *
     *   * Hold_Timer_expired
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_OpenSent},    /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_restart,   bgp_fsm_Idle},        /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_restart,   bgp_fsm_Idle},        /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_opened,    bgp_fsm_OpenConfirm}, /* Receive_OPEN_message         */
    {bgp_fsm_error,     bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_error,     bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_notified,  bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_OpenConfirm: Opens sent and received, waiting for KeepAlive......
     *
     * Only one of the two connections can reach this state.
     *
     * While in this state is waiting for a BGP KEEPALIVE to arrive or for the
     * HoldTimer to expire, or for the KeepaliveTimer to prompt sending of
     * another KEEPALIVE message.
     *
     * The expected events are:
     *
     *   * Receive_KEEPALIVE_message
     *
     *     This means that the other end is acknowledging the OPEN, and the
     *     session is now Established.
     *
     *     If there is another connection, now is the time to kill it off.
     *
     *     This connection becomes the primary and only connection.
     *
     *     Arm HoldTimer and KeepliveTimer (as per negotiated values) and
     *     advance to Established state.
     *
     *     Pass a session established message to the Routeing Engine, complete
     *     with the bgp_open_state for the successful connection.
     *
     *   * Receive_OPEN_message
     *
     *     FSM error -- bring connection to a dead stop.
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * Receive_UPDATE_message
     *
     *     FSM error -- bring connection to a dead stop.
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * Receive_NOTIFICATION_message
     *
     *     Bring connection to a dead stop.
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * TCP_connection_closed
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * TCP_fatal_error
     *
     *     Bring connection and session to a dead stop.
     *
     *   * KeepAlive_Timer_expired
     *
     *     Send KEEPALIVE message and recharge KeepaliveTimer.
     *
     *   * Hold_Timer_expired
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_OpenConfirm}, /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_restart,   bgp_fsm_Idle},        /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_restart,   bgp_fsm_Idle},        /* Hold_Timer_expired           */
    {bgp_fsm_kal_send,  bgp_fsm_OpenConfirm}, /* KeepAlive_timer_expired      */
    {bgp_fsm_error,     bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_establish, bgp_fsm_Established}, /* Receive_KEEPALIVE_message    */
    {bgp_fsm_error,     bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_notified,  bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_Established: session is up and running...........................
     *
     * Only the primary connection exists in this state.
     *
     * While in this state is waiting for a BGP UPDATE (or KEEPALIVE) messages
     * to arrive or for the HoldTimer to expire, or for the KeepaliveTimer to
     * prompt sending of another KEEPALIVE message.
     *
     * The expected events are:
     *
     *   * Receive_OPEN_message
     *
     *     FSM error -- bring connection and session to a dead stop.
     *
     *   * Receive_UPDATE_message
     *
     *     Restart the HoldTimer.
     *
     *   * Receive_KEEPALIVE_message
     *
     *     Restart the HoldTimer.
     *
     *   * Receive_NOTIFICATION_message
     *
     *     Bring connection and session to a dead stop.
     *
     *   * TCP_connection_closed
     *
     *     Bring connection and session to a dead stop.
     *
     *   * TCP_fatal_error
     *
     *     Bring connection and session to a dead stop.
     *
     *   * KeepAlive_Timer_expired
     *
     *     Send KEEPALIVE message and recharge KeepaliveTimer.
     *
     *   * Hold_Timer_expired
     *
     *     If primary, promote the secondary.  If no secondary...
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Established}, /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_closed,    bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_expire,    bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_kal_send,  bgp_fsm_Established}, /* KeepAlive_timer_expired      */
    {bgp_fsm_error,     bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_kal_recv,  bgp_fsm_Established}, /* Receive_KEEPALIVE_message    */
    {bgp_fsm_update,    bgp_fsm_Established}, /* Receive_UPDATE_message       */
    {bgp_fsm_notified,  bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_Stopping: waiting (briefly) to send Notification.................
     *
     * Before a connection is sent to Stopping state the reasons for stopping
     * are set.  (See bgp_fsm_set_stopping.)
     *
     * There are two flavours of stop:
     *
     *   * stop-idle
     *
     *     Close the connection, then fall back to Idle state.
     *
     *   * stop-dead
     *
     *     Close and terminate the connection (cut loose from session).
     *
     *     If this is the only connection, stop the session.
     *
     * The complication is the possible need to send a NOTIFICATION message
     * before closing the connection.
     *
     * Once a connection has reached Established state, the TCP write buffers
     * may be full, so it may not be possible immediately to send the
     * NOTIFICATION.  Note that stopping from Established state is always
     * stop-dead.
     *
     * In other states there should be plenty of room in the TCP write buffers.
     *
     * On entry to Stopping:
     *
     *   1) if this is stop-dead -- unlink self from session.
     *
     *      NB: this clears the pointer from session to connection.
     *
     *          ....
     *
     *   2) if there is a NOTIFICATION message (notification_pending):
     *
     *        * close the connection for reading and purge read buffers
     *        * purge the write buffering and any pending writes
     *        * stop all timers
     *        * send the NOTIFICATION
     *
     *      if the NOTIFICATION immediately clears the buffers (or fails),
     *      clear the notification_pending flag.
     *
     *   3) if the notification_pending flag is still set:
     *
     *        * for stop-idle set a short time-out (5 seconds)
     *        * for stop-dead set a longer time-out (30 seconds)
     *
     *      stays in Stopping state, waiting for NOTIFICATION to be sent, or
     *      to fail, or for the timeout.
     *
     *      (Should not really need the time-out for stop-idle, but seems
     *       neater than crash closing the connection.)
     *
     *      While in Stopping state, any further event will clear the
     *      notification-pending flag.
     *
     * When the notification-pending flag is not set:
     *
     *   * close the connection
     *   * purge all buffering
     *   * stop all timers
     *
     *   * for stop-idle: proceed to Idle state





     * In this state the connection is no longer associated with a session.
     *
     * This state exists only to allow the TCP output buffer to drain
     * sufficiently to allow the tail end of one BGP message to be sent,
     * followed by a NOTIFICATION message.
     *
     * When entering this state, if there is no NOTIFICATION to send, then
     * will terminate the session.
     *
     * While in this state is waiting for the NOTIFICATION message to have been
     * sent, or for the HoldTimer to expire (does not wait indefinitely).
     *
     * The expected events are:
     *
     *   * Sent NOTIFICATION message
     *   * Hold_Timer_expired
     *   * TCP_fatal_error
     *   * TCP_connection_closed
     *
     *     Clear NOTIFICATION pending, so connection will then be terminated.
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_Stopping},    /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Start                    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open          */
    {bgp_fsm_done,      bgp_fsm_Stopping},    /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* TCP_connection_open_failed   */
    {bgp_fsm_done,      bgp_fsm_Stopping},    /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_done,      bgp_fsm_Stopping},    /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_Stopping},    /* Receive_NOTIFICATION_message */
    {bgp_fsm_done,      bgp_fsm_Stopping},    /* Sent NOTIFICATION message    */
  },
} ;

static const char *bgp_event_str[] =
{
  "NULL",
  "BGP_Start",
  "BGP_Stop",
  "TCP_connection_open",
  "TCP_connection_closed",
  "TCP_connection_open_failed",
  "TCP_fatal_error",
  "ConnectRetry_timer_expired",
  "Hold_Timer_expired",
  "KeepAlive_timer_expired",
  "Receive_OPEN_message",
  "Receive_KEEPALIVE_message",
  "Receive_UPDATE_message",
  "Receive_NOTIFICATION_message",
  "Sent_NOTIFICATION_message",
} ;

/*------------------------------------------------------------------------------
 * Deal with the given event for the given connection.
 */
extern void
bgp_fsm_event(bgp_connection connection, bgp_fsm_event_t event)
{
  bgp_fsm_state_t next_state ;
  const struct bgp_fsm* fsm ;

  dassert( (event >= bgp_fsm_null_event)
        && (event <= bgp_fsm_last_event)) ;
  dassert( (connection->state >= bgp_fsm_first_state)
        && (connection->state >= bgp_fsm_last_state) ) ;

  /* Watch out for recursing through the FSM for this connection.       */
  ++connection->fsm_active ;

  if (connection->fsm_active == 2)
    {
      connection->post = event ;
      return ;
    } ;

  /* Lock the session for the convenience of the event handlers.
   *
   * NB: if the current state is Stopping, then connection is no longer
   *     attached to session -- so connection->session is NULL -- BEWARE !
   *
   *     The session lock does nothing if no session is attached.
   */
  BGP_CONNECTION_SESSION_LOCK(connection) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  do
    {
      assert(connection->fsm_active == 1) ;

      fsm = &bgp_fsm[connection->state][event] ;
      next_state = fsm->next_state ;

      /* Call function. */
      next_state = fsm->action(connection, next_state, event) ;

      dassert( (next_state >= bgp_fsm_first_state)
            && (next_state <= bgp_fsm_last_state) ) ;

      /* If state is changed.                               */
      if (next_state != connection->state)
        {
          bgp_fsm_state_t prev_state  = connection->state ;

          /* Complete the state change                                        */
          bgp_fsm_state_change(connection, next_state) ;

          /* Log state change as required.                                    */
          if (BGP_DEBUG(fsm, FSM))
            plog_debug(connection->log,
                       "%s [FSM] %s (%s->%s)",
                         connection->host,
                         bgp_event_str[event],
                         LOOKUP (bgp_status_msg, prev_state),
                         LOOKUP (bgp_status_msg, next_state)) ;

          if (BGP_DEBUG(normal, NORMAL))
            zlog_debug ("%s on %s went from %s to %s",
                          connection->host,
                          bgp_event_str[event],
                          LOOKUP (bgp_status_msg, prev_state),
                          LOOKUP (bgp_status_msg, next_state));
        } ;

      /* Pick up post event -- if any                                   */
      event = connection->post ;
      connection->post = bgp_fsm_null_event ;

    } while (--connection->fsm_active != 0) ;

  BGP_CONNECTION_SESSION_UNLOCK(connection) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  /* Connections which are Stopping are no longer linked to a session.      */
  /* There is no way out of Stopping state

  if (connection->state == bgp_fsm_Stopping)
    {
      /* Sever link with session -- after mutex unlock the first time       */

      session->connections[connection->ordinal] = NULL ;

      connection->session = NULL ;
      connection->p_mutex = NULL ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Null action -- do nothing at all.
 */
static bgp_fsm_state_t
bgp_fsm_null(bgp_connection connection, bgp_fsm_state_t next_state,
                                        bgp_fsm_event_t event)
{
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Invalid event -- cannot occur in current state.
 *
 * Brings down the session -- next state is bgp_fsm_stopping.
 *
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_invalid(bgp_connection connection, bgp_fsm_state_t next_state,
                                           bgp_fsm_event_t event)
{
  if (BGP_DEBUG (fsm, FSM)) \
    plog_debug (connection->log, "%s [FSM] invalid event %d in state %d",
                                   connection->host, event, connection->state) ;

  bgp_fsm_set_stopping(connection, bgp_stopped_invalid,
                      bgp_notify_new(BGP_NOMC_FSM, BGP_NOMS_UNSPECIFIC, 0), 1) ;

  return bgp_fsm_Stopping ;
} ;

/*------------------------------------------------------------------------------
 * This is empty event -- should not really happen...
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_ignore(bgp_connection connection, bgp_fsm_state_t next_state,
                                          bgp_fsm_event_t event)
{
  BGP_FSM_DEBUG(connection, "bgp_ignore called") ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Entry point to FSM.
 *
 * This is the first thing to happen to the FSM, and takes it from Initial
 * state to Idle, with Idle Hold Timer running.
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_enter(bgp_connection connection, bgp_fsm_state_t next_state,
                                         bgp_fsm_event_t event)
{
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Start up BGP Connection
 *
 * This is used:
 *
 *   * to change from Idle to Connect or Active -- when the IdleHoldTimer
 *     expires.
 *
 *   * to loop back to Connect or Active -- when the ConnectRetryTimer expires.
 *
 * The state entered depends on whether this is the primary or secondary
 * connection.
 *
 * If this is the primary connection, then kicks a connect() into life,
 * before the state change.  Note that if that fails, then post an event to
 * be processed as soon as completes the state transition.
 *
 * If this is the secondary connection, enables the session for accept().
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_start(bgp_connection connection, bgp_fsm_state_t next_state,
                                         bgp_fsm_event_t event)
{
  if (connection->ordinal == bgp_connection_primary)
    {
      next_state = bgp_fsm_Connect ;
      connection->post = bgp_open_connect(connection) ;
    }
  else
    {
      next_state = bgp_fsm_Active ;
      connection->session->accept = 1 ;
    } ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Restart BGP Connection
 *
 * This is used when a TCP connection has come up, but has stopped -- for some
 * reason (such as the connection simply closed) which suggests that the other
 * end might still be prepared to make a connection.
 *
 * Extends the IdleHoldTimer for the session (up to a maximum of 120 secs) and
 * changes to Idle state.
 *
 * Note that this works equally for the primary and the secondary connection.
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_restart(bgp_connection connection, bgp_fsm_state_t next_state,
                                           bgp_fsm_event_t event)
{
  unsigned* p_interval = &connection->session->idle_hold_timer_interval ;

  *p_interval *= 2 ;

  if      (*p_interval < 4)
    *p_interval = 4 ;
  else if (*p_interval > 120)
    *p_interval = 120 ;

  bgp_connection_close(connection) ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Stop BGP Connection
 *
 * The reason should already have been set: bgp_fsm_set_stopping().
 * But, if not, set unknown reason.
 *
 * If no notification to be sent, close the connection now.
 * If notification to be sent, try to send it now.
 *
 * NB: the session is locked.
 */
static bgp_fsm_state_t
bgp_fsm_stop(bgp_connection connection, bgp_fsm_state_t next_state,
                                        bgp_fsm_event_t event)
{
  if (connection->stopped == bgp_stopped_not)
    bgp_fsm_set_stopping(connection, bgp_stopped_unknown, NULL, 0) ;

  /* */
  if (connection->notification_pending)
    {
      bgp_msg_write_notification(connection) ;


          nothing pending
        }
          *   * notification_pending     nothing pending
          *   * notification_written     not written
)

    } ;

  /* If are still waiting for the
  if (connection->notification_pending || connection->notification_written)
    bgp_connection_read_close(connection) ;
  else
    bgp_connection_close(connection) ;

  return next_state ;
}

/*------------------------------------------------------------------------------
 * TCP connection open.
 *
 * Send BGP Open Message to peer.
 */
static bgp_fsm_state_t
bgp_fsm_open(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  char buf1[BUFSIZ];

  if (connection->fd < 0)
    {
      zlog_err ("bgp_connect_success peer's fd is negative value %d",
                connection->fd);
      return -1;
    }

  bgp_getsockname(connection) ;

  if (BGP_DEBUG (normal, NORMAL))
    {
      if (! connection->listenerCHECK_FLAG (peer->sflags, PEER_STATUS_ACCEPT_PEER))
        zlog_debug ("%s open active, local address %s", peer->host,
                    sockunion2str (peer->su_local, buf1, SU_ADDRSTRLEN));
      else
        zlog_debug ("%s passive open", peer->host);
    }

  bgp_send_open(connection) ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Connect retry timer is expired when in Connect or Active states.
 *
 * For primary connection:
 *
 *   * close the existing connection (may already be closed if failed)
 *   * start the connect() attempt again
 *
 * For secondary connection:
 *
 *   * close the existing connection (easy, 'cos never opened !)
 *   * continue waiting to accept()
 *
 */
static bgp_fsm_state_t
bgp_fsm_retry(bgp_connection connection, bgp_fsm_state_t next_state,
                                         bgp_fsm_event_t event)
{
  bgp_connection_close(connection) ;
  return bgp_fsm_start(connection, next_state, event) ;
} ;

/*------------------------------------------------------------------------------
 * Error:
 *
 *
 */
static bgp_fsm_state_t
bgp_fsm_error(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  /* Double start timer. */
  peer->v_start *= 2;

  /* Overflow check. */
  if (peer->v_start >= (60 * 2))
    peer->v_start = (60 * 2);

  return bgp_stop(peer, next_state);
} ;

/*------------------------------------------------------------------------------
 * Hold timer expire.  This is error of BGP connection. So cut the
 * peer and change to Idle status.
 */
static bgp_fsm_state_t
bgp_fsm_expire(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  BGP_FSM_DEBUG(connection, "Hold timer expire") ;

  /* Send notify to remote peer. */
  bgp_notify_send (peer, BGP_NOTIFY_HOLD_ERR, 0);

  /* Sweep if it is temporary peer. */
  if (CHECK_FLAG (peer->sflags, PEER_STATUS_ACCEPT_PEER))
    {
      zlog_info ("%s [Event] Accepting BGP peer is deleted", peer->host);
      peer_delete (peer);
      return -1;
    }

  /* bgp_stop needs to be invoked while in Established state */
  return bgp_stop(peer, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * Received an acceptable Open Message
 *
 * The next state is OpenConfirm.
 *
 * However: this is where we do Collision Resolution.
 *
 * If the sibling connection has reached OpenConfirm before this one, then now
 * this one either closes its sibling, or itself.
 *
 * As soon as a connection reaches Established, it immediately kills off any
 * sibling -- so the farthest two connections can get is to OpenSent.
 *
 * The connection that is closed should send a Cease/Collision Resolution
 * NOTIFICATION.  The other end should do likewise.
 *
 * The connection that is closed is not stopped.  It falls
 *
 * Immediately respond with a keepalive......
 *
 */
static bgp_fsm_state_t
bgp_fsm_opened(bgp_connection connection, bgp_fsm_state_t next_state,
                                                         bgp_fsm_event_t event)
{
  bgp_send_keepalive(connection) ;

  return next_state ;
}

/*------------------------------------------------------------------------------
 * Status goes to Established.
 *
 * TODO: do we need to send a KEEPALIVE on entry to established ?
 *
 *
 * On transition
 */
static bgp_fsm_state_t
bgp_fsm_establish(bgp_connection connection, bgp_fsm_state_t next_state,
                                             bgp_fsm_event_t event)
{
  bgp_session    session = connection->session ;
  bgp_connection sibling = bgp_fsm_get_sibling(connection) ;

  assert(session != NULL) ;

  /* The first thing to do is to kill off any sibling and establish
   * self as the primary connection.
   */
  if (sibling != NULL)
    {
      bgp_fsm_stop_connection(sibling, bgp_stopped_collision,
                      bgp_notify_new(BGP_NOMC_CEASE, BGP_NOMS_C_COLLISION, 0)) ;

      if (connection->ordinal != bgp_connection_primary)
        {
          connection->ordinal = bgp_connection_primary ;
          session->connections[bgp_connection_primary]   = connection ;
          session->connections[bgp_connection_secondary] = NULL ;
        } ;
    } ;

  /* Whatever else happens, will no longer accept connections.          */
  /* TODO: now would be a good time to withdraw the password from listener ?  */
  session->accept = 0 ;

  /* Set the session state -- tell the Routeing Engine the news         */
  bgp_session_set_state(session, bgp_session_Established) ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Keepalive send to peer.
 */
static bgp_fsm_state_t
bgp_fsm_kal_send(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  bgp_keepalive_send(connection) ;
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 *  Keepalive packet is received.
 */
static bgp_fsm_state_t
bgp_fsm_kal_recv(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  /* peer count update */
  peer->keepalive_in++;

  bgp_hold_timer_recharge(connection) ;
  return next_state ;
}

/*------------------------------------------------------------------------------
 * Update packet is received.
 */
static bgp_fsm_state_t
bgp_fsm_update(bgp_connection connection, bgp_fsm_state_t next_state,
    bgp_fsm_event_t event)
{
  bgp_hold_timer_recharge(connection) ;
  return next_state ;
}

/*------------------------------------------------------------------------------
 * We're done with this connection.
 *
 * May have been waiting to get a NOTIFICATION message away, and that has
 * either succeeded or timed out.
 *
 * Shut the socket and tear down the connection.
 */
static bgp_fsm_state_t
bgp_fsm_done(bgp_connection connection, bgp_fsm_state_t next_state,
                                        bgp_fsm_event_t event)
{
  bgp_tear_down(connection);
  return next_state ;
}

/*==============================================================================
 * The FSM stopping management.
 *
 * There are many ways in which a connection may be required to stop.
 *
 * The stopping of one connection may mean that the entire session should be
 * stopped -- either because there is only one connection (eg when in
 * Established state), or because this is an administrative stop, or because
 * there has been a serious error, or because a notification received, or ...
 *
 *
 *
 */


static void
bgp_fsm_set_stopping(bgp_connection connection, bgp_stopped_cause_t cause,
                                              bgp_notify notification, int both)
{
  bgp_session    session ;
  bgp_connection sibling ;

  /* If not connected to session, then already stopping and can do no more.   */
  session = connection->session ;
  if (session == NULL)
    return ;

  BGP_CONNECTION_SESSION_LOCK(connection) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  /* Get "sibling" and set stopping same as us, if required.            */
  /* Won't be there if one-time sibling is already stopping/stopped.    */
  if (both && ((sibling = bgp_fsm_get_sibling(connection)) != NULL))
    bgp_fsm_stop_connection(sibling, cause, bgp_notify_dup(notification)) ;

  /* If have been passed a notification, then that should be sent to    */
  /* the other end, and reported with the session stop cause.           */

  connection->notification_pending = (notification != NULL) ;

  if (connection->notification_pending)
    bgp_notify_set(&connection->notification, notification) ;

  /* Set the session stopping cause and copy the notification there     */

  session->stopped      = cause ;
  bgp_notify_set_dup(&session->notification, connection->notification) ;

  BGP_CONNECTION_SESSION_UNLOCK(connection) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
} ;





static bgp_connection
bgp_fsm_get_sibling(bgp_connection connection)
{
  bgp_session session = connection->session ;

  if (session == NULL)
    return NULL ;               /* no sibling if no session             */

  confirm(bgp_connection_primary   == (bgp_connection_secondary ^ 1)) ;
  confirm(bgp_connection_secondary == (bgp_connection_primary   ^ 1)) ;

  return session->connections[connection->ordinal ^ 1] ;
} ;




/*==============================================================================
 * The BGP connections timers handling.
 *
 * The FSM has four timers:
 *
 *   * IdleHoldTimer -- uses connection.hold_timer with jitter
 *
 *     This runs while in Idle state, and is a period in which no connections
 *     are started, and none will be accepted.
 *
 *     The purpose of this timer is to slow down re-making connections with
 *     peers who are flapping or otherwise proving a nuisance.
 *
 *     This is a one shot timer, which generates a bgp_fsm_BGP_Start event.
 *
 *   * ConnectRetryTimer -- uses connection.hold_timer with jitter
 *
 *     This runs while in Connect or Active state, and is the period for which
 *     the connection is prepared to wait between attempts to connect.
 *
 *     When trying to make a connect connection:
 *
 *       The FSM will be in Connect state.
 *
 *       If listen connections are enabled, will be listening.
 *
 *       If nothing happens before the ConnectRetryTimer expires, then
 *       the connection attempt will be abandoned, and another started.
 *
 *       If the connection attempt fails, moves to Active state -- with the
 *       timer still running.
 *
 *       If nothing further happens before the ConnectRetryTimer expires,
 *       another connect will be started and the FSM returns to Connect state.
 *
 *     When only listening is enabled:
 *
 *       The FSM will be in Active state (!).
 *
 *       If nothing happens before the ConnectRetryTimer expires, then the
 *       FSM will loop round back into Active state.
 *
 *     This timer is recharged each time it goes off, and generates a
 *     bgp_fsm_ConnectRetry_timer_expired event.
 *
 *  * HoldTimer  -- uses connection.hold_timer *without* jitter
 *
 *    This timer is used in OpenSent state, and limits the time will wait for
 *    an Open to appear from the other end.  RFC4271 calls for this to be a
 *    "large value" -- suggesting 240 seconds.
 *
 *    This timer is also used in OpenConfirm and Established states, and limits
 *    the time the connection will be held if hear nothing from the other end.
 *    In these states the timer is set to the negotiated HoldTime.  If this is
 *    zero, then the HoldTime is infinite.
 *
 *    This is a one shot timer, which generates a bgp_fsm_Hold_Timer_expired
 *    event.
 *
 *  * KeepaliveTimer -- uses connection.keepalive_timer with jitter.
 *
 *    This timer is used in OpenConfirm and Established states only.
 *
 *    The default KeepalineTimer is 1/3 the HoldTimer, and is set from the
 *    negotiated HoldTime.  If that is zero, then the KeepaliveTime is also
 *    infinite, and no KEEPALIVE messages will be sent (other than the "ack"
 *    of the OPEN message).
 *
 *    This timer is recharged each time it goes off, and generates a
 *    bgp_fsm_KeepAlive_timer_expired event.
 */

/* Forward reference                                                    */
static inline void
bgp_timer_set(bgp_connection connection, qtimer timer, unsigned secs,
                                            int jitter, qtimer_action* action) ;

/* Forward reference the action functions                               */
static qtimer_action bgp_idle_hold_timer_action ;
static qtimer_action bgp_connect_retry_timer_action ;
static qtimer_action bgp_hold_timer_action ;
static qtimer_action bgp_keepalive_timer_action ;

/*==============================================================================
 * Completion of State Change
 *
 * This performs fixed changes associated with the entry to each state from
 * *another* state.
 *
 * Set and unset all the connection timers as required by the new state of
 * the connection.
 *
 *
 *
 * NB: requires the session to be LOCKED.
 */
static void
bgp_fsm_state_change(bgp_connection connection, bgp_fsm_state_t new_state)
{
  bgp_session session = connection->session ;

  switch (new_state)
    {
    /* Base state of connection's finite state machine -- when a session has
     * been enabled.  Falls back to Idle in the event of various errors.
     *
     * In Idle state the IdleHoldTimer is running, at the end of which the
     * BGP Engine will try to connect.
     *
     * Note that don't allow a zero (ie infinite) IdleHoldTimer -- by forcing
     * a 1 second minimum time.  As a side effect we make the time odd just
     * before we jitter it.
     *
     * In Idle state refuses connections.
     */
    case bgp_fsm_Idle:
      bgp_timer_set(connection, &connection->hold_timer,
                              (session->idle_hold_timer_interval | 1), 1,
                                                   bgp_idle_hold_timer_action) ;
      qtimer_unset(&connection->keepalive_timer) ;

      break;

    /* In Connect state the BGP Engine is attempting to make a connection
     * with the peer and may be listening for a connection.
     *
     * In Active state the BGP Engine is only listening (!).
     *
     * In both cases, waits for the connect_hold_timer_interval.
     *
     * The ConnectRetryTimer automatically recharges, because will loop back
     * round into the same state.
     */
    case bgp_fsm_Connect:
    case bgp_fsm_Active:
      bgp_timer_set(connection, &connection->hold_timer,
                           session->connect_retry_timer_interval, 1,
                                               bgp_connect_retry_timer_action) ;
      qtimer_unset(&connection->keepalive_timer) ;
      break;

    /* In OpenSent state is waiting for an OPEN from the other end, before
     * proceeding to OpenConfirm state.
     *
     * Prepared to wait for quite a long time for this.
     *
     * Note that session->accept is left as it is.  If have reached OpenSent
     * on:
     *
     *   * a connect() connection, then session->accept will be true and will
     *     still accept in-bound connections.
     *
     *   * an accept() connection, then session->accept will be false.
     */
    case bgp_fsm_OpenSent:
      bgp_timer_set(connection, &connection->hold_timer,
                               session->open_hold_timer_interval, 0,
                                                        bgp_hold_timer_action) ;
      qtimer_unset(&connection->keepalive_timer) ;
      break;

    /* In OpenConfirm state is waiting for an "ack" before proceeding to
     * Established.  Session->accept is left as it is.  If have reached
     * OpenConfirm on:
     *
     *   * a connect() connection, then session->accept may still be true and
     *     will still accept in-bound connections.  (Collision detection may
     *     have discarded an accept() connection already.)
     *
     *   * an accept() connection, then session->accept will be false.
     *
     * There is only one way into Established, and that is from OpenConfirm.
     * OpenConfirm starts the KeepaliveTimer.  It would be wrong to reset the
     * timer on entry to Established.
     *
     * In both cases have just received a message, so can restart the HoldTimer.
     *
     * Both use the negotiated Hold Time and Keepalive Time.  May send further
     * KEEPALIVE messages in OpenConfirm.
     *
     * If the negotiated Hold Time value is zero, then the Keepalive Time
     * value will also be zero, and this will unset both timers.
     */
    case bgp_fsm_OpenConfirm:
      bgp_timer_set(connection, &connection->keepalive_timer,
                                 connection->keepalive_timer_interval, 1,
                                                   bgp_keepalive_timer_action) ;
    case bgp_fsm_Established:
      bgp_timer_set(connection, &connection->hold_timer,
                                 connection->hold_timer_interval, 0,
                                                        bgp_hold_timer_action) ;
      break;

    /* The connection is coming to an dead stop.
     *
     */

    case bgp_fsm_Stopping:
      if (connection->notification_pending)
        bgp_timer_set(connection, &connection->hold_timer,
                                                 60, 1, bgp_hold_timer_action) ;
      else
        qtimer_unset(&connection->hold_timer) ;

      qtimer_unset(&connection->keepalive_timer) ;

      break ;

    default:
      zabort("Unknown bgp_fsm_state") ;
    } ;

  /* Finally: set the new state                                         */
  connection->state = new_state ;
} ;

/*==============================================================================
 * Timer set and Timer Action Functions
 */

/*------------------------------------------------------------------------------
 * Start or reset given qtimer with given interval, in seconds.
 *
 * If the interval is zero, unset the timer.
 */
static inline void
bgp_timer_set(bgp_connection connection, qtimer timer, unsigned secs,
                                              int jitter, qtimer_action* action)
{
  if (secs == 0)
    qtimer_unset(timer) ;
  else
    {
      if (jitter)
        secs -= ((rand() % ((int)secs + 1)) / 4) ;
      qtimer_set_interval(timer, QTIME(secs), action) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * BGP start timer action => bgp_fsm_BGP_Start event
 *
 * The timer is automatically unset, which is fine.
 */
static void
bgp_idle_hold_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (start timer expire)") ;

  bgp_fsm_event(connection, bgp_fsm_BGP_Start) ;
} ;

/*------------------------------------------------------------------------------
 * BGP connect retry timer => bgp_fsm_ConnectRetry_timer_expired event
 *
 * The timer is recharged here, applying a new "jitter", but that may be
 * overridden by the bgp_event() handling.
 */
static void
bgp_connect_retry_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (connect timer expire)") ;

  bgp_timer_set(connection, &connection->hold_timer,
           connection->session->connect_retry_timer_interval, 1, NULL) ;

  bgp_fsm_event(connection, bgp_fsm_ConnectRetry_timer_expired) ;
} ;

/*------------------------------------------------------------------------------
 * BGP hold timer => bgp_fsm_Hold_Timer_expired event
 *
 * The timer is automatically unset, which is fine.
 */
static void
bgp_hold_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (holdtime timer expire)") ;

  bgp_fsm_event(connection, bgp_fsm_Hold_Timer_expired) ;
} ;

/*------------------------------------------------------------------------------
 * BGP keepalive fire => bgp_fsm_KeepAlive_timer_expired
 *
 * The timer is recharged here, applying a new "jitter", but that may be
 * overridden by the bgp_event() handling.
 */
static void
bgp_keepalive_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (keepalive timer expire)") ;

  bgp_timer_set(connection, &connection->keepalive_timer,
                    connection->session->keepalive_timer_interval, 1, NULL) ;

  bgp_fsm_event(connection, bgp_fsm_KeepAlive_timer_expired) ;
} ;

/*============================================================================*/
/* BGP Peer Down Cause */
const char *peer_down_str[] =
{
  "",
  "Router ID changed",
  "Remote AS changed",
  "Local AS change",
  "Cluster ID changed",
  "Confederation identifier changed",
  "Confederation peer changed",
  "RR client config change",
  "RS client config change",
  "Update source change",
  "Address family activated",
  "Admin. shutdown",
  "User reset",
  "BGP Notification received",
  "BGP Notification send",
  "Peer closed the session",
  "Neighbor deleted",
  "Peer-group add member",
  "Peer-group delete member",
  "Capability changed",
  "Passive config change",
  "Multihop config change",
  "NSF peer closed the session"
};

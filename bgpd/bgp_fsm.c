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

#include "log.h"

#include "bgpd/bgp_session.h"
#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_msg_write.h"
#include "bgpd/bgp_msg_read.h"
#include "bgpd/bgp_dump.h"

#include "lib/qtimers.h"
#include "lib/sockunion.h"

#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_dump.h"

/*==============================================================================
 * The BGP Finite State Machine
 *
 * The working of BGP is described in the RFC (4271) in terms of a finite
 * state machine.
 *
 * This code follows the older Quagga code, which appears to be based on the
 * earlier RFC 1771.
 *
 * The state machine is represented as a table, indexed by [state, event],
 * giving an action to be performed to deal with the event and the state that
 * will advance to (or stay at).
 *
 * In some cases the action routine may override the the default new state.
 *
 * In general the FSM manages connections, but there is some interaction with
 * the session.  In particular, exceptions are expressed as session_eXXX
 * values -- which are passed to the Routing Engine as session events.  The
 * handling of FSM events is depends mostly on the FSM state, but any
 * exception influences that too.
 *
 * When a new state is entered, bgp_fsm_state_change() is called to complete
 * the transition (in particular to set/unset timers).
 *
 * The fsm action functions are called with the session locked.
 *
 * TODO: restore NSF
 * TODO: track incoming connections while established
 *
 *       To do this need to accept connection while established, then wait for
 *       OPEN.  While waiting, the established connection may drop, and
 *       so the session restart, but with an incoming connection already
 *       in place.
 *
 *       When OPEN arrives, for NSF should close any established connection
 *       and restart with the new one.
 *
 *       Otherwise, if CollisionDetectEstablishedState option is set, should
 *       go through collision detection !
 *
 *------------------------------------------------------------------------------
 * FSM "events" and Session "exceptions".
 *
 * These are raised when:
 *
 *   * the BGP Engine receives instructions from the Routeing Engine
 *
 *   * some I/O operations complete
 *
 *   * some I/O operations fail
 *
 *   * timers go off
 *
 * FSM events are raised by calling bgp_fsm_event().  A number of events are
 * associated with session exceptions -- the exception is posted in the
 * connection and then a suitable FSM event is raised.
 *
 * The most general FSM event is fsm_eBGP_Stop -- which MUST have a posted
 * exception to tell it why the stop has been been raised.
 *
 * However, nothing external calls bgp_fsm_event() directly -- functions
 * defined here will raise the appropriate event.
 *
 * Note that the event is dealt with *immediately* -- there is no queueing of
 * events.  This avoids the problem of the state of the connection being
 * "out of date" until its event queue is emptied, and the problem of the state
 * changing between the raising of an event and dealing with same.
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
 * handle this, the connection can set a "follow on" event, to be processed at
 * immediately after the current event and any state change that makes.
 *
 * Also, some things within the FSM are most consistently dealt with by
 * raising follow on events.
 *
 * Note that there is only one level of "follow on" event.  The FSM never
 * issues more than one I/O operation per event or more than one follow on
 * event, and never both at the same time.  (It's a RULE.)
 *
 *------------------------------------------------------------------------------
 * Primary and Secondary Connections
 *
 * To support BGP's "symmetrical" open strategy, this code allows for two
 * connections to be made for a session -- one connect() and one accept().
 * If two connections are made, only one will reach sOpenConfirm state and
 * hence sEstablished.
 *
 * When a session is enabled, the allowed connections are initialised and
 * a BGP_Start event issued for each one.
 *
 * The listeners (in bgp_network) will only accept connections from addresses
 * known to be peer addresses, and then only when the FSM is ready for them.
 * The accept field in the peer_index entry (see bgp_peer_index) is maintained
 * by this code -- the accept field (when set) points to the secondary
 * connection of the session.
 *
 * Up to sEstablished state, the primary connection will be the out-bound
 * connect() connection (if allowed) and the secondary will be the in-bound
 * accept() connection (if allowed).  In sEstablished state, the primary
 * connection is the one that won the race -- any other connection has been
 * snuffed out.
 *
 * As per the RFC, collision detection/resolution is performed when an OPEN
 * message is received -- that is, as the connection attempts to advance to
 * sOpenConfirm state.  At that point, if the sibling is in sOpenConfirm state,
 * then one of the two connections is closed (and will go sIdle once the
 * NOTIFICATION has been sent).
 *
 * See below for a discussion of the fall back to sIdle -- the losing connection
 * will remain comatose until the winner either reaches sEstablished (when the
 * loser is snuffed out) or the winner falls back to sIdle (when the
 * IdleHoldTimer for the loser is set, and it will wake up in due course).
 *
 * NB: the RFC talks of matching source/destination and destination/source
 *     addresses of connections in order to detect collisions.  This code
 *     uses only the far end address to detect collisions.  It does so
 *     implicitly because the in-bound connection is matched with the out-
 *     bound one using the peer's known IP address -- effectively its name.
 *
 *     [It is not deemed relevant if the local addresses for the in- and out-
 *      bound connections are different.]
 *
 *     The RFC further says "the local system MUST examine all of its
 *     connections that are in OpenConfirm state" ... "If, among these
 *     connections, there is a connection to a remote BGP speaker whose BGP
 *     identifier equals the one in the OPEN message, and this connection
 *     collides with [it]" ... then must resolve the collision.
 *
 *     This code almost does this, but:
 *
 *       * there can only be one connection that collides (i.e. only one other
 *         which has the same remote end address), and that is the sibling
 *         connection.
 *
 *         So there's not a lot of "examining" to be done.
 *
 *       * the RFC seems to accept that there could be two distinct connections
 *         with the same remote end address, but *different* BGP Identifiers.
 *
 *         As far as Quagga is concerned, that is impossible.  The remote end
 *         IP address is the name of the peering session, and there cannot
 *         be two peering sessions with the same name.  It follows that Quagga
 *         requires that the "My AS" and the "BGP Identifier" entries in the
 *         OPEN messages from a given remote end IP address MUST MATCH !
 *
 *------------------------------------------------------------------------------
 * The FSM proceeds in three basic phases:
 *
 *   1) attempting to establish a TCP connection: sIdle/sActive/sConnect
 *
 *      In this phase there is no connection for the other end to close !
 *
 *      sIdle is a "stutter step" which becomes longer each time the FSM falls
 *      back to sIdle, which it does if the process fails in sOpenSent or
 *      sOpenConfirm.
 *
 *      Cannot fail in sIdle !
 *
 *      In sActive/sConnect any failure causes the FSM to stop trying to
 *      connect.  It does nothing further until the end of the ConnectRetryTimer
 *      interval -- at which point it will try again, re-charging the timer.
 *      (That is usually 120 seconds (less jitter) -- so in the worst case, it
 *      will try to do something impossible every 90-120 seconds.)
 *
 *      A connection may fall back to sIdle from sOpenSent/sOpenConfirm (see
 *      below).  While one connection is sOpenSent or sOpenConfirm don't really
 *      want to start another TCP connection in competition.  So, on entry
 *      to sIdle:
 *
 *        * if a sibling exists and is in sOpenSent or sOpenConfirm:
 *
 *            - do not change the IdleHoldTimer interval.
 *            - unset the IdleHoldTimer.
 *            - set self "comatose".
 *
 *        * otherwise:
 *
 *            - increase the IdleHoldTimer interval.
 *            - set the IdleHoldTimer (with jitter).
 *
 *          and if a sibling exists and is comatose:
 *
 *            - set *its* IdleHoldTimer (with jitter).
 *            - clear *its* comatose flag.
 *
 *      The effect is that if both connections make it to sOpenSent, then only
 *      when *both* fall back to sIdle will the FSM try to make any new TCP
 *      connections.
 *
 *      The IdleHoldTimer increases up to 120 seconds.  In the worst case, the
 *      far end repeatedly makes outgoing connection attempts, and immediately
 *      drops them.  In which case, the IdleHoldTimer grows, and the disruption
 *      reduces to once every 90-120 seconds !
 *
 *   2) attempting to establish a BGP session: sOpenSent/sOpenConfirm
 *
 *      If something goes wrong, or the other end closes the connection (with
 *      or without notification) the FSM will loop back to sIdle state.  Also,
 *      when collision resolution closes one connection it too loops back to
 *      sIdle (see above).
 *
 *      Both connections may reach sOpenSent.  Only one at once can reach
 *      sOpenConfirm -- collision resolution sees to that.
 *
 *      Note that while a NOTIFICATION is being sent the connection stays
 *      in sOpenSent/sOpenConfirm state.
 *
 *   3) BGP session established
 *
 *      If something goes wrong, or the other end closes the connection
 *      (with or without notification) will stop the session.
 *
 * Only three things bring the FSM to a dead stop, and stop the session:
 *
 *   1) the Routeing Engine disabling the session.
 *
 *   2) invalid events -- which are assumed to be bugs, really.
 *
 *   3) anything that stops the session while in sEstablished state.
 *
 * This means that the FSM will plough on trying to establish connections with
 * configured peers, even in circumstances when the likelihood of success
 * appears slim to vanishing.  However, the Routeing Engine and the operator
 * are responsible for the decision to start and to stop trying to connect.
 *
 *------------------------------------------------------------------------------
 * Exception handling.
 *
 * The basic mechanism is:
 *
 *   * exceptions may the "thrown" -- which posts a given exception in the
 *     connection then kicks the FSM with a given fsm_eXxxxx event.
 *
 *     Information posted is:
 *
 *       sesssion_eXxxxx    -- what the exception is
 *       notification       -- any NOTIFICATION message
 *       err                -- any I/O or other error
 *
 *     on exit from the FSM this information is passed to the Routing Engine.
 *
 *     Can throw exceptions within the FSM, as discussed above.
 *
 *   * within the FSM exceptions are "caught".
 *
 *     Which deals with the exception as thrown, depending on the next state.
 *
 * See the various exception functions below for what exceptions are posted and
 * what fsm_eXxxx events are generated.
 *
 * The following fsm events require an exception:
 *
 *   bgp_fsm_eBGP_Stop                      -- bgp_fsm_exception()
 *   bgp_fsm_eTCP_connection_closed         -- bgp_fsm_io_error()
 *   bgp_fsm_eTCP_connection_open_failed    -- bgp_fsm_connect_completed()
 *   bgp_fsm_eTCP_fatal_error               -- bgp_fsm_io_fatal_error()
 *   bgp_fsm_eReceive_NOTIFICATION_message  -- bgp_fsm_notification_exception()
 *
 *------------------------------------------------------------------------------
 * FSM errors
 *
 * Invalid events: if the FSM receives an event that cannot be raised in the
 * current state, it will terminate the session, sending an FSM Error
 * NOTIFICATION (if a TCP connection is up).  See bgp_fsm_invalid().
 *
 * If the FSM receives a message type that is not expected in the current,
 * state, it will close the connection (if sOpenSent or sOpenConfirm) or stop
 * the session (if sEstablished), also sending an FSM Error NOTIFICATION.
 * See bgp_fsm_error().
 *
 *------------------------------------------------------------------------------
 * Sending NOTIFICATION message
 *
 * In sOpenSent, sOpenConfirm and sEstablished states may send a NOTIFICATION
 * message.
 *
 * The procedure for sending a NOTIFICATION is:
 *
 *   -- close the connection for reading and clear read buffers.
 *
 *      This ensures that no further read I/O can occur and no related events.
 *
 *      Note that anything sent from the other end is acknowledged, but
 *      quietly discarded.
 *
 *   -- purge the write buffer of all output except any partly sent message.
 *
 *      This ensures there is room in the write buffer at the very least.
 *
 *      For sOpenSent and sOpenConfirm states there should be zero chance of
 *      there being anything to purge.
 *
 *   -- purge any pending write messages for the connection (for sEstablished).
 *
 *   -- set notification_pending = 1 (write pending)
 *
 *   -- write the NOTIFICATION message.
 *
 *      For sEstablished, the message will at the very least be written to the
 *      write buffer.  For sOpenSent and sOpenConfirm expect it to go directly
 *      to the TCP buffer.
 *
 *   -- stop the KeepaliveTimer
 *
 *   -- set HoldTimer to a waiting to clear buffer time -- say 20 secs.
 *
 *      Don't expect to need to wait at all in sOpenSent/sOpenConfirm states.
 *
 *   -- when the NOTIFICATION message clears the write buffer, that will
 *      generate a Sent_NOTIFICATION_message event.
 *
 * After sending the NOTIFICATION, sOpenSent & sOpenConfirm stay in their
 * respective states.  sEstablished goes to sStopping State.
 *
 * When the Sent_NOTIFICATION_message event occurs, set the HoldTimer to
 * a "courtesy" time of 5 seconds.  Remain in the current state.
 *
 * During the "courtesy" time the socket will continue to acknowledge, but
 * discard input.  In the case of Collision Resolution this gives a little time
 * for the other end to send its NOTIFICATION message.  In all cases, it gives
 * a little time before the connection is slammed shut.
 *
 * When the HoldTimer expires close the connection completely (whether or not
 * the NOTIFICATION has cleared the write buffer).
 *
 *------------------------------------------------------------------------------
 * Communication with the Routeing Engine
 *
 * The FSM sends bgp_session_event messages to the Routeing Engine which keep
 * it up to date with the progress and state of the FSM.
 *
 * In particular, these event messages tell the Routeing Engine when the
 * session enters and leaves sEstablished, when the session stops for any
 * reason and when the session has been disabled -- which are what really
 * matters to it !
 */

static void
bgp_fsm_event(bgp_connection connection, bgp_fsm_event_t event) ;

/*==============================================================================
 * Recharge the HoldTimer
 *
 * Defined here for the convenience of bgp_fsm_pre_update(), which is called
 * once for every incoming update.
 *
 * NB: no jitter.
 *
 * NB: do nothing if connection->hold_timer_interval == 0
 *
 * NB: if connection->hold_timer_interval != 0, timer MUST be set
 */
static inline void
bgp_hold_timer_recharge(bgp_connection connection)
{
  if (connection->hold_timer_interval != 0)
    qtimer_set_interval(connection->hold_timer,
                                 QTIME(connection->hold_timer_interval), NULL) ;
} ;

/*==============================================================================
 * Enable the given session -- which must be newly initialised.
 *
 * This is the first step in the FSM, and the connection advances to Idle.
 *
 * Returns in something of a hurry if not enabled for connect() or for accept().
 *
 * NB: requires the session LOCKED
 */
extern void
bgp_fsm_enable_session(bgp_session session)
{
  bgp_connection connection ;

  /* Proceed instantly to a dead stop if neither connect nor listen !   */
  if (!(session->connect || session->listen))
    {
      bgp_session_event(session, bgp_session_eInvalid, NULL, 0, 0, 1) ;
      return ;
    } ;

  /* Primary connection -- if connect allowed
   *
   * NB: the start event for the primary connection is guaranteed to succeed,
   *     and nothing further will happen until the initial IdleHoldTimer
   *     expires -- always has a small, non-zero time.
   *
   *     This ensures that the secondary connection can be started before
   *     there's any change of the session being torn down !!
   */
  if (session->connect)
    {
      connection = bgp_connection_init_new(NULL, session,
                                                       bgp_connection_primary) ;
      bgp_fsm_event(connection, bgp_fsm_eBGP_Start) ;
    } ;

  /* Secondary connection -- if listen allowed
   */
  if (session->listen)
    {
      connection = bgp_connection_init_new(NULL, session,
                                                     bgp_connection_secondary) ;
      bgp_fsm_event(connection, bgp_fsm_eBGP_Start) ;
    } ;
} ;

 /*=============================================================================
 * Signalling events and throwing exceptions.
 *
 */
static void
bgp_fsm_throw(bgp_connection connection, bgp_session_event_t exception,
                      bgp_notify notification, int err, bgp_fsm_event_t event) ;

static bgp_fsm_state_t
bgp_fsm_catch(bgp_connection connection, bgp_fsm_state_t next_state) ;

/*------------------------------------------------------------------------------
 * Signal that valid OPEN message has been received and processed into the
 * connection->open_recv.
 */
extern void
bgp_fsm_open_received(bgp_connection connection)
{
  bgp_fsm_event(connection, bgp_fsm_eReceive_OPEN_message) ;
} ;

/*------------------------------------------------------------------------------
 * Signal that valid KEEPALIVE message has been received.
 */
extern void
bgp_fsm_keepalive_received(bgp_connection connection)
{
  bgp_fsm_event(connection, bgp_fsm_eReceive_KEEPALIVE_message);
} ;

/*------------------------------------------------------------------------------
 * Signal that have received a message that is some form of "update".
 *
 * If is sEstablished: re-charge the HoldTimer.
 *
 *          otherwise: raise bfp_fsm_eUpdate event, which will most likely
 *                     throw an error.
 *
 * Avoids going through the full FSM process for update events (of which there
 * may be many) in the simple case -- where only need to re-charge HoldTimer.
 *
 * Deals, via the FSM, with unexpected "update" events -- for example an
 * UPDATE (or ROUTE-REFRESH) before reaching sEstablished !
 */
extern int
bgp_fsm_pre_update(bgp_connection connection)
{
  if (connection->state == bgp_fsm_sEstablished)
    {
      bgp_hold_timer_recharge(connection) ;
      return 0 ;
    } ;

  bgp_fsm_event(connection, bgp_fsm_eReceive_UPDATE_message) ;
  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Signal that notification message has cleared buffers into TCP.
 */
extern void
bgp_fsm_notification_sent(bgp_connection connection)
{
  bgp_fsm_event(connection, bgp_fsm_eSent_NOTIFICATION_message) ;
} ;

/*------------------------------------------------------------------------------
 * Ultimate exception -- disable the session
 *
 * Does nothing if neither connection exists (which implies the session has
 * already been disabled, or never got off the ground).
 *
 * NB: takes responsibility for the notification structure.
 *
 * NB: requires the session LOCKED
 */
extern void
bgp_fsm_disable_session(bgp_session session, bgp_notify notification)
{
  bgp_connection connection ;

  connection = session->connections[bgp_connection_primary] ;
  if (connection == NULL)
    connection = session->connections[bgp_connection_secondary] ;

  if (connection != NULL)
    bgp_fsm_exception(connection, bgp_session_eDisabled, notification) ;
  else
    {
      /* Acknowledge the disable -- session is stopped.                 */
      bgp_session_event(session, bgp_session_eDisabled, NULL, 0, 0, 1) ;

      bgp_notify_free(notification) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Throw a general exception -- not I/O related.
 *
 * Note that I/O problems are signalled by bgp_fsm_io_error().
 *
 * NB: can throw an exception for other connections while in the FSM.
 *
 *     Can throw an exception for the current connection while in the FSM, the
 *     fsm_active/follow_on mechanism looks after this.
 */
extern void
bgp_fsm_exception(bgp_connection connection, bgp_session_event_t exception,
                                                        bgp_notify notification)
{
  bgp_fsm_throw(connection, exception, notification, 0, bgp_fsm_eBGP_Stop) ;
} ;

/*------------------------------------------------------------------------------
 * Raise a discard exception for sibling.
 *
 * A connection will discard any sibling if:
 *
 *   * the session is being disabled (by the Routing Engine)
 *
 *   * an invalid event is bringing down the session
 *
 *   * it has reached Established state, and is snuffing out the sibling.
 *
 * NB: requires the session LOCKED
 */
static void
bgp_fsm_discard_sibling(bgp_connection sibling, bgp_notify notification)
{
  bgp_fsm_throw(sibling, bgp_session_eDiscard,
                                           notification, 0, bgp_fsm_eBGP_Stop) ;
} ;

/*------------------------------------------------------------------------------
 * Raise a NOTIFICATION received exception
 */
extern void
bgp_fsm_notification_exception(bgp_connection connection,
                                                        bgp_notify notification)
{
  bgp_fsm_throw(connection, bgp_session_eNOM_recv, notification, 0,
                                        bgp_fsm_eReceive_NOTIFICATION_message) ;
} ;

/*------------------------------------------------------------------------------
 * Raise a "fatal I/O error" exception on the given connection.
 *
 * Error to be reported as "TCP_fatal_error".
 */
extern void
bgp_fsm_io_fatal_error(bgp_connection connection, int err)
{
  plog_err (connection->log, "%s [Error] bgp IO error: %s",
            connection->host, errtoa(err, 0).str) ;

  assert(err != EFAULT) ;

  bgp_fsm_throw(connection, bgp_session_eTCP_error, NULL, err,
                                                     bgp_fsm_eTCP_fatal_error) ;
} ;

/*------------------------------------------------------------------------------
 * Raise an "I/O error" exception on the given connection.
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
  if (   (err == 0)
      || (err == ECONNRESET)
      || (err == ENETDOWN)
      || (err == ENETUNREACH)
      || (err == EPIPE)
      || (err == ETIMEDOUT) )
    {
      if (BGP_DEBUG(events, EVENTS))
        {
          if (err == 0)
            plog_debug(connection->log,
                       "%s [Event] BGP connection closed fd %d",
                               connection->host, qps_file_fd(connection->qf)) ;
          else
            plog_debug(connection->log,
                       "%s [Event] BGP connection closed fd %d (%s)",
                               connection->host, qps_file_fd(connection->qf),
                                                           errtoa(err, 0).str) ;
        } ;

      bgp_fsm_throw(connection, bgp_session_eTCP_dropped, NULL, err,
                                               bgp_fsm_eTCP_connection_closed) ;
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
 * If err == 0, then all is well: start the connection (can now write to it)
 *                            and generate TCP_connection_open event
 *
 * If err is one of:
 *
 *   ECONNREFUSED, ECONNRESET, EHOSTUNREACH or ETIMEDOUT
 *
 * generate TCP_connection_open_failed event.  (accept() does not return any of
 * these errors.)
 *
 * Other errors are reported as TCP_fatal_error.
 *
 * NB: in any case on entry to this function the file is *disabled* in all
 *     modes.
 */
extern void
bgp_fsm_connect_completed(bgp_connection connection, int err,
                                                   union sockunion* su_local,
                                                   union sockunion* su_remote)
{
  if (err == 0)
    {
      bgp_connection_start(connection, su_local, su_remote) ;
      bgp_fsm_event(connection, bgp_fsm_eTCP_connection_open) ;
    }
  else if (   (err == ECONNREFUSED)
           || (err == ECONNRESET)
           || (err == EHOSTUNREACH)
           || (err == ETIMEDOUT) )
    bgp_fsm_throw(connection, bgp_session_eTCP_failed, NULL, err,
                                          bgp_fsm_eTCP_connection_open_failed) ;
  else
    bgp_fsm_io_fatal_error(connection, err) ;
} ;

/*------------------------------------------------------------------------------
 * Post the given exception and raise the given event.
 *
 * NB: takes responsibility for the notification structure.
 */
static void
bgp_fsm_throw(bgp_connection connection, bgp_session_event_t exception,
                        bgp_notify notification, int err, bgp_fsm_event_t event)
{
  connection->exception = exception ;
  bgp_notify_set(&connection->notification, notification) ;
  connection->err       = err ;

  bgp_fsm_event(connection, event) ;
} ;

/*------------------------------------------------------------------------------
 * Post the given exception and raise a follow-on fsm_eBGP_Stop event
 *
 * This is for use *within* the FSM.
 *
 * Returns the current connection state, so the follow-on fsm_eBGP_Stop uses
 * action from the current state.
 *
 * In most cases the effect is to translate one event into a suitable general
 * exception -- to be handled same like any other, in the current state.
 *
 * NB: takes responsibility for the notification structure.
 */
static bgp_fsm_state_t
bgp_fsm_throw_stop(bgp_connection connection, bgp_session_event_t exception,
                                                        bgp_notify notification)
{
  bgp_fsm_throw(connection, exception, notification, 0, bgp_fsm_eBGP_Stop) ;
  return connection->state ;
} ;

/*==============================================================================
 * For debug...
 */
#define BGP_FSM_DEBUG(connection, message) \
  if (BGP_DEBUG (fsm, FSM)) \
    plog_debug (connection->log, "%s [FSM] " message, connection->host)

/*==============================================================================
 * The FSM table
 */

#define bgp_fsm_action(FUNCNAME) \
  bgp_fsm_state_t FUNCNAME(bgp_connection connection, \
                    bgp_fsm_state_t next_state, bgp_fsm_event_t event)

typedef bgp_fsm_action(bgp_fsm_action_func) ;

struct bgp_fsm
{
  bgp_fsm_action_func*  action ;
  bgp_fsm_state_t       next_state ;
} ;

static bgp_fsm_action(bgp_fsm_null) ;
static bgp_fsm_action(bgp_fsm_enter) ;
static bgp_fsm_action(bgp_fsm_stop) ;
static bgp_fsm_action(bgp_fsm_invalid) ;
static bgp_fsm_action(bgp_fsm_start) ;
static bgp_fsm_action(bgp_fsm_half_open) ;
static bgp_fsm_action(bgp_fsm_connect) ;
static bgp_fsm_action(bgp_fsm_accept) ;
static bgp_fsm_action(bgp_fsm_send_open) ;
static bgp_fsm_action(bgp_fsm_failed) ;
static bgp_fsm_action(bgp_fsm_fatal) ;
static bgp_fsm_action(bgp_fsm_retry) ;
static bgp_fsm_action(bgp_fsm_closed) ;
static bgp_fsm_action(bgp_fsm_expire) ;
static bgp_fsm_action(bgp_fsm_recv_open) ;
static bgp_fsm_action(bgp_fsm_error) ;
static bgp_fsm_action(bgp_fsm_recv_nom) ;
static bgp_fsm_action(bgp_fsm_sent_nom) ;
static bgp_fsm_action(bgp_fsm_send_kal) ;
static bgp_fsm_action(bgp_fsm_establish) ;
static bgp_fsm_action(bgp_fsm_recv_kal) ;
static bgp_fsm_action(bgp_fsm_update) ;
static bgp_fsm_action(bgp_fsm_exit) ;

/*------------------------------------------------------------------------------
 *  Finite State Machine events
 *
 *    0. null_event
 *
 *       Do nothing.  As quietly as possible.
 *
 *       Never generated, so should not be seen !
 *
 *    1. BGP_Start
 *
 *         a. in sInitial state  (-> sIdle)
 *
 *            raised immediately after creating the connection.
 *
 *         b. in sIdle state
 *
 *            raised on expiry of IdleHoldTime.
 *
 *            primary connection:   proceed to sConnect
 *
 *            secondary connection: proceed to sAccept
 *
 *         c. in sConnect
 *
 *            raised on expiry of ConnectRetryTime.
 *
 *            Start a new connect() attempt.
 *
 *         d. in sActive
 *
 *            raised on expiry of ConnectRetryTime.
 *
 *            Start a new accept() attempt.
 *
 *    2. BGP_Stop -- process the posted exception (invalid if none !)
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
 *              -- in the event of any general exception.
 *
 *    3. TCP_connection_open
 *
 *         a. primary connection:   in sConnect state  (-> OpenSent)
 *
 *            raised when a connect() connection succeeds
 *
 *         b. secondary connection: in sIdle state     (stay sIdle)
 *
 *            raised when an accept() connection is accepted.
 *
 *            The connection is not refused, but it is ignored until the
 *            IdleHoldTimer expires.
 *
 *         c. secondary connection: in sActive state  (-> OpenSent)
 *
 *            raised when an accept() connection is accepted.
 *
 *       Cannot happen at any other time.
 *
 *    4. TCP_connection_closed -- process the posted exception (MUST be there)
 *
 *       Raised by "EOF" on read or by EPIPE and some other errors.
 *
 *         a. in sOpenSent and sOpenConfirm states
 *
 *            This may be because the the other end has detected a collision.
 *            It may be because the other end is being vexatious.
 *
 *            Fall back to sIdle.
 *
 *         b. and sEstablished state
 *
 *            Stop the session.
 *
 *       NB: any errors generated when the OPEN message is sent (on exit from
 *           sConnect or sActive states) are not delivered until has entered
 *           sOpenSent state.
 *
 *       Cannot happen at any other time.
 *
 *    5. TCP_connection_open_failed ("soft" error)
 *                               -- process the posted exception (MUST be there)
 *
 *         a. in sConnect or sActive states
 *
 *            Close the connection.  For sActive state, disable accept.
 *
 *            Stay in sConnect/sActive (until ConnectRetryTimer expires).
 *
 *       Cannot happen at any other time.
 *
 *    6. TCP_fatal_error ("hard" error)
 *                               -- process the posted exception (MUST be there)
 *
 *         a. in sConnect or sActive states
 *
 *            Close the connection.  For sActive state, disable accept.
 *
 *            Stay in sConnect/sActive (until ConnectRetryTimer expires).
 *
 *         b. in sOpenSent/sOpenConfirm states
 *
 *            Close the connection.
 *
 *            Fall back to sIdle.
 *
 *         c. in sEstablished state
 *
 *            Close the the session -- go to sStopping state.
 *
 *         d. in sStopping state.
 *
 *            Terminate the connection.
 *
 *       Cannot happen at any other time (ie sIdle).
 *
 *    7. ConnectRetry_timer_expired
 *
 *         a. in either sConnect or sActive states ONLY.
 *
 *            Time to give up current connection attempt(s), and start trying
 *            to connect all over again.
 *
 *       Cannot happen at any other time.
 *
 *    8. Hold_Timer_expired
 *
 *         a. in sOpenSent state
 *
 *            Time to give up waiting for an OPEN (or NOTIFICATION) from the
 *            other end.  For this wait the RFC recommends a "large" value for
 *            the hold time -- and suggests 4 minutes.  Send NOTIFICATION.
 *
 *            Or, if the connection has sent a notification message that
 *            process is now complete.
 *
 *            Close the connection.  Fall back to sIdle.
 *
 *         b. in sOpenConfirm state
 *
 *            Time to give up waiting for a KEEPALIVE to confirm the connection.
 *            For this wait the hold time used is that negotiated in the OPEN
 *            messages that have been exchanged.  Send NOTIFICATION.
 *
 *            Or, if the connection has sent a notification message that
 *            process is now complete.
 *
 *            Close the connection.  Fall back to sIdle.
 *
 *         c. in sEstablished state
 *
 *            In this state the hold time used is that negotiated in the OPEN
 *            messages that have been exchanged.
 *
 *            The session has failed.  Send NOTIFICATION.
 *
 *            Close the session -- go to sStopping state.
 *
 *         d. in sStopping state
 *
 *            Time to give up trying to send NOTIFICATION and terminate the
 *            connection.
 *
 *       Cannot happen at any other time.
 *
 *    9. KeepAlive_timer_expired
 *
 *         a. in sOpenConfirm and sEstablished states
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
 *         a. in sOpenSent state -- the expected response
 *
 *            Proceed (via collision resolution) to sOpenConfirm or sIdle.
 *
 *         b. in sOpenConfirm state -- FSM error
 *
 *            Send NOTIFICATION.  Fall back to sIdle.
 *
 *         c. in Established state -- FSM error
 *
 *            Send NOTIFICATION.  Close session.
 *
 *       Cannot happen at any other time (connection not up or read closed).
 *
 *   11. Receive_KEEPALIVE_message
 *
 *       Generated by read action.
 *
 *         a. in sOpenSent state -- FSM error
 *
 *            Send NOTIFICATION.  Fall back to Idle.
 *
 *         b. in sOpenConfirm state -- the expected response
 *
 *         c. in sEstablished state -- expected
 *
 *       Cannot happen at any other time (connection not up or read closed).
 *
 *   12. Receive_UPDATE_message
 *
 *       Generated by read action.
 *
 *         a. in sOpenSent and sOpenConfirm states -- FSM error
 *
 *            Send NOTIFICATION.  Fall back to sIdle.
 *
 *         b. in sEstablished state -- expected
 *
 *       Cannot happen at any other time (connection not up or read closed).
 *
 *   13. Receive_NOTIFICATION_message
 *                               -- process the posted exception (MUST be there)
 *
 *       Generated by read action.
 *
 *         a. in sOpenSent and sOpenConfirm close the connection and fall back
 *            to sIdle.
 *
 *         b. in sEstablished state -- close the session.
 *
 *       Cannot happen at any other time (connection not up or read closed).
 *
 *   14. Sent_NOTIFICATION_message
 *
 *       Generated by write action when completed sending the message.
 *
 *         a. in sOpenSent, sOpenConfirm, and sStopping states
 *
 *            Set the "courtesy" hold time.
 *
 *       Cannot happen at any other time.
 */

/*------------------------------------------------------------------------------
 *  Finite State Machine Table
 */

static const struct bgp_fsm
bgp_fsm[bgp_fsm_last_state + 1][bgp_fsm_last_event + 1] =
{
  {
    /* bgp_fsm_sInitial: initialised in this state..............................
     *
     * Expect only a eBGP_Start event, which arms the IdleHoldTimer and
     * advances to the sIdle state.
     *
     * Could (just) get a bgp_fsm_eStop if other connection stops immediately !
     *
     * A connection should be in this state for a brief period between being
     * initialised and set going.
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sInitial},    /* null event                   */
    {bgp_fsm_enter,     bgp_fsm_sIdle},       /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sInitial},    /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sIdle: waiting for IdleHoldTimer.................................
     *
     * When a session is enabled both its connections start in this state.
     * (Noting that an accept() only session starts only the secondary
     * connection and a connect() only session starts only the primary.)
     *
     * While in this state is waiting for the IdleHoldTimer to expire.  This
     * timer becomes longer if the peer misbehaves.
     *
     * If a connection stops at sOpenState or sOpenConfirm, may loop back
     * through sIdle, with an increased IdleHoldTimer.
     *
     * In sIdle state the connection is dormant.  (While the secondary is sIdle,
     * no connections will be accepted.)
     *
     * If the peer keeps making or accepting TCP connections, and then dropping
     * them, then the IdleHoldTimer will grow to slow down the rate of vexatious
     * connections.
     *
     * When a connection falls back to sIdle it will have been closed.
     *
     * The expected events are:
     *
     *   * eBGP_Start -- generated by IdleHoldTimer expired
     *
     *   * eBGP_Stop -- for whatever reason
     *
     *   * eTCP_connection_open -- generated if connection is accepted
     *
     *     This set the connection "half_open", but stays in sIdle until
     *     the IdleHoldTimer expires.  This avoids rejecting inbound
     *     connections, but also enforces the IdleHoldTimer if the other end
     *     is being vexatious.
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sIdle},       /* null event                   */
    {bgp_fsm_start,     bgp_fsm_sConnect},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sIdle},       /* BGP_Stop                     */
    {bgp_fsm_half_open, bgp_fsm_sIdle},       /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sConnect: waiting for connect (and listen).......................
     *
     * Only the primary connection can be in this state.
     *
     * While in this state is waiting for connection to succeed or fail, or for
     * the ConnectRetryTimer to expire.
     *
     * The expected events are:
     *
     *   * eBGP_Start -- follow-on event from ConnectRetryTimer expired
     *
     *   * TCP_connection_open -- good news !
     *
     *   * TCP_connection_open_fail ("soft" error)
     *
     *   * TCP_fatal_error ("hard" error)
     *
     *   * ConnectRetryTimer expired
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sConnect},    /* null event                   */
    {bgp_fsm_connect,   bgp_fsm_sConnect},    /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sConnect},    /* BGP_Stop                     */
    {bgp_fsm_send_open, bgp_fsm_sOpenSent},   /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_failed,    bgp_fsm_sConnect},    /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_sConnect},    /* TCP_fatal_error              */
    {bgp_fsm_retry,     bgp_fsm_sConnect},    /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sActive: waiting for listen (only)...............................
     *
     * Only the secondary connection can be in this state.
     *
     * While in this state is waiting for an incoming connection to succeed or
     * for the ConnectRetryTimer to expire.
     *
     * The expected events are:
     *
     *   * eBGP_Start -- follow-on event from ConnectRetryTimer expired
     *
     *   * TCP_connection_open -- good news
     *
     *   * TCP_connection_open_fail ("soft" error)
     *
     *   * TCP_fatal_error
     *
     *   * ConnectRetry_timer_expired
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sActive},     /* null event                   */
    {bgp_fsm_accept,    bgp_fsm_sActive},     /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sActive},     /* BGP_Stop                     */
    {bgp_fsm_send_open, bgp_fsm_sOpenSent},   /* TCP_connection_open          */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_failed,    bgp_fsm_sActive},     /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_sActive},     /* TCP_fatal_error              */
    {bgp_fsm_retry,     bgp_fsm_sActive},     /* ConnectRetry_timer_expired   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sOpenSent: waiting for Open from the other end...................
     *
     * Both primary and secondary connections can be in this state.
     *
     * While in this state is waiting for a BGP OPEN to arrive or for the
     * HoldTimer ("large" value) to expire.
     *
     * The expected events are:
     *
     *   * Receive_OPEN_message -- good news
     *
     *   * Receive_UPDATE_message  -- FSM error !
     *
     *   * Receive_KEEPALIVE_message -- FSM error !
     *
     *   * Receive_NOTIFICATION_message -- bad news
     *
     *   * TCP_connection_closed
     *
     *   * TCP_fatal_error
     *
     *   * Hold_Timer_expired
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sOpenSent},   /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sIdle},       /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open          */
    {bgp_fsm_closed,    bgp_fsm_sIdle},       /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_sIdle},       /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_expire,    bgp_fsm_sIdle},       /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_recv_open, bgp_fsm_sOpenConfirm},/* Receive_OPEN_message         */
    {bgp_fsm_error,     bgp_fsm_sOpenSent},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_error,     bgp_fsm_sOpenSent},   /* Receive_UPDATE_message       */
    {bgp_fsm_recv_nom,  bgp_fsm_sIdle},       /* Receive_NOTIFICATION_message */
    {bgp_fsm_sent_nom,  bgp_fsm_sOpenSent},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sOpenConfirm: Opens sent and received, waiting for KeepAlive.....
     *
     * Only one of the two connections can reach this state.
     *
     * While in this state is waiting for a BGP KEEPALIVE to arrive or for the
     * HoldTimer to expire, or for the KeepaliveTimer to prompt sending of
     * another KEEPALIVE message.
     *
     * The expected events are:
     *
     *   * Receive_KEEPALIVE_message -- effectively 'ACK' for OPEN.
     *
     *   * Receive_OPEN_message -- FSM error
     *
     *   * Receive_UPDATE_message -- FSM error
     *
     *   * Receive_NOTIFICATION_message -- bad news
     *
     *   * TCP_connection_closed
     *
     *   * TCP_fatal_error
     *
     *   * KeepAlive_Timer_expired -- keep trying
     *
     *   * Hold_Timer_expired
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sOpenConfirm},/* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sIdle},       /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open          */
    {bgp_fsm_closed,    bgp_fsm_sIdle},       /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_sIdle},       /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_expire,    bgp_fsm_sIdle},       /* Hold_Timer_expired           */
    {bgp_fsm_send_kal,  bgp_fsm_sOpenConfirm},/* KeepAlive_timer_expired      */
    {bgp_fsm_error,     bgp_fsm_sOpenConfirm},/* Receive_OPEN_message         */
    {bgp_fsm_establish, bgp_fsm_sEstablished},/* Receive_KEEPALIVE_message    */
    {bgp_fsm_error,     bgp_fsm_sOpenConfirm},/* Receive_UPDATE_message       */
    {bgp_fsm_recv_nom,  bgp_fsm_sIdle},       /* Receive_NOTIFICATION_message */
    {bgp_fsm_sent_nom,  bgp_fsm_sOpenConfirm},/* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sEstablished: session is up and running..........................
     *
     * Only the primary connection exists in this state.
     *
     * While in this state is waiting for a BGP UPDATE (or KEEPALIVE) messages
     * to arrive or for the HoldTimer to expire, or for the KeepaliveTimer to
     * prompt sending of another KEEPALIVE message.
     *
     * The expected events are:
     *
     *   * Receive_OPEN_message -- FSM error
     *
     *   * Receive_UPDATE_message -- the real business
     *
     *     In fact this is dealt with elsewhere, to avoid going through the
     *     full FSM process -- see above.
     *
     *   * Receive_KEEPALIVE_message
     *
     *   * Receive_NOTIFICATION_message
     *
     *   * TCP_connection_closed
     *
     *   * TCP_fatal_error
     *
     *   * KeepAlive_Timer_expired
     *
     *   * Hold_Timer_expired
     *
     *   * BGP_Stop -- for whatever reason
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sEstablished},/* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* BGP_Start                    */
    {bgp_fsm_stop,      bgp_fsm_sStopping},   /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open          */
    {bgp_fsm_closed,    bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_fatal,     bgp_fsm_sStopping},   /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_expire,    bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_send_kal,  bgp_fsm_sEstablished},/* KeepAlive_timer_expired      */
    {bgp_fsm_error,     bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_recv_kal,  bgp_fsm_sEstablished},/* Receive_KEEPALIVE_message    */
    {bgp_fsm_update,    bgp_fsm_sEstablished},/* Receive_UPDATE_message       */
    {bgp_fsm_recv_nom,  bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
  },
  {
    /* bgp_fsm_sStopping: waiting (briefly) to send Notification................
     *
     * Before a connection is sent to sStopping state it may have sent a
     * NOTIFICATION message.  If so, it will stay in sStopping state until
     * that process completes -- which will be signalled by the HoldTimer
     * expiring, or some sort of I/O failure.
     *
     * Other exceptions and any invalid fsm events will raise an fsm_eBGP_Stop.
     *
     * On exit from the FSM a connection that has just entered sStopping state
     * will be unlinked from the session.
     *
     * The only way out of sStopping is bgp_fsm_exit(), which terminates the
     * connection which will later be reaped.
     *
     * The expected events are:
     *
     *   * BGP_Stop -- exit
     *
     *   * Sent NOTIFICATION message -- set "courtesy" timer
     *
     *   * Hold_Timer_expired -- exit
     *
     *   * TCP_fatal_error -- exit
     *
     *   * TCP_connection_closed -- exit
     *
     * All other events (other than null) are invalid (should not happen).
     */
    {bgp_fsm_null,      bgp_fsm_sStopping},   /* null event                   */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* BGP_Start                    */
    {bgp_fsm_exit,      bgp_fsm_sStopping},   /* BGP_Stop                     */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open          */
    {bgp_fsm_exit,      bgp_fsm_sStopping},   /* TCP_connection_closed        */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* TCP_connection_open_failed   */
    {bgp_fsm_exit,      bgp_fsm_sStopping},   /* TCP_fatal_error              */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* ConnectRetry_timer_expired   */
    {bgp_fsm_exit,      bgp_fsm_sStopping},   /* Hold_Timer_expired           */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* KeepAlive_timer_expired      */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_OPEN_message         */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_KEEPALIVE_message    */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_UPDATE_message       */
    {bgp_fsm_invalid,   bgp_fsm_sStopping},   /* Receive_NOTIFICATION_message */
    {bgp_fsm_sent_nom,  bgp_fsm_sStopping},   /* Sent NOTIFICATION message    */
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

/*==============================================================================
 * Signal FSM event.
 */

static void
bgp_fsm_state_change(bgp_connection connection, bgp_fsm_state_t new_state) ;

/*------------------------------------------------------------------------------
 * Signal event to FSM for the given connection.
 */
static void
bgp_fsm_event(bgp_connection connection, bgp_fsm_event_t event)
{
  bgp_fsm_state_t next_state ;
  const struct bgp_fsm* fsm ;

  dassert( (event >= bgp_fsm_null_event)
        && (event <= bgp_fsm_last_event)) ;
  dassert( (connection->state >= bgp_fsm_first_state)
        && (connection->state <= bgp_fsm_last_state) ) ;

  /* Watch out for recursing through the FSM for this connection.       */
  ++connection->fsm_active ;

  if (connection->fsm_active == 2)
    {
      connection->follow_on = event ;
      return ;
    } ;

  /* Lock the session for the convenience of the event handlers.
   *
   * NB: if the current state is sStopping, then connection is no longer
   *     attached to session -- so connection->session is NULL -- BEWARE !
   *
   *     The session lock does nothing if no session is attached.
   */

  BGP_CONNECTION_SESSION_LOCK(connection) ; /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

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

      /* Pick up follow_on event -- if any                              */
      event = connection->follow_on ;
      connection->follow_on = bgp_fsm_null_event ;

    } while (--connection->fsm_active != 0) ;

  /* If required, post session event.                                   */
  if (connection->exception != bgp_session_null_event)
    {
      int stopped     = (connection->state == bgp_fsm_sStopping) ;
      int has_session = (connection->session != NULL) ;

      /* Some exceptions are not reported to the Routeing Engine
       *
       * In particular: eDiscard and eCollision -- so the only time the
       * connection->state will be Stopping is when the session is being
       * stopped.  (eDiscard and eCollision go quietly to Stopping !)
       */
      if ((connection->exception <= bgp_session_max_event) && has_session)
        bgp_session_event(connection->session, connection->exception,
                                   bgp_notify_take(&connection->notification),
                                   connection->err,
                                   connection->ordinal,
                                   stopped) ;

      /* Tidy up -- notification already cleared                        */
      connection->exception = bgp_session_null_event ;
      connection->err       = 0 ;
      bgp_notify_unset(&connection->notification) ;     /* if any       */

      if (stopped && has_session)
        BGP_CONNECTION_SESSION_CUT_LOOSE(connection) ;
    } ;

  BGP_CONNECTION_SESSION_UNLOCK(connection) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
} ;

/*==============================================================================
 * The BGP FSM Action Functions
 */

static void
bgp_hold_timer_set(bgp_connection connection, unsigned secs) ;

/*------------------------------------------------------------------------------
 * Null action -- do nothing at all.
 */
static bgp_fsm_action(bgp_fsm_null)
{
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Entry point to FSM.
 *
 * This is the first thing to happen to the FSM, and takes it from sInitial
 * state to sIdle, with IdleHoldTimer running.
 *
 * NB: the IdleHoldTimer is always a finite time.  So the start up event for
 *     the primary connection *cannot* fail.
 *
 * NB: the session is locked.
 */
static bgp_fsm_action(bgp_fsm_enter)
{
  if (connection->ordinal == bgp_connection_secondary)
    {
      bgp_prepare_to_accept(connection) ;
      bgp_connection_enable_accept(connection) ;
    } ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Stop BGP Connection -- general exception event.
 *
 * An exception should have been posted, treat as invalid if not.
 *
 * For some exceptions the next state is forced to be sStopping.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_stop)
{
  if (connection->exception == bgp_session_null_event)
    return bgp_fsm_invalid(connection, bgp_fsm_sStopping, event) ;

  if (   (connection->exception == bgp_session_eDisabled)
      || (connection->exception == bgp_session_eDiscard)
      || (connection->exception == bgp_session_eInvalid)  )
    next_state = bgp_fsm_sStopping ;

  return bgp_fsm_catch(connection, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * Invalid event -- cannot occur in current state.
 *
 * Throws a general bgp_session_eInvalid exception -- staying in the current
 * state.  The follow-on eBGP_Stop event bounces the entire session into
 * sStopped.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_invalid)
{
  if (BGP_DEBUG(fsm, FSM)) \
    plog_debug(connection->log, "%s [FSM] invalid event %d in state %d",
                                   connection->host, event, connection->state) ;

  return bgp_fsm_throw_stop(connection, bgp_session_eInvalid,
                            bgp_notify_new(BGP_NOMC_FSM, BGP_NOMS_UNSPECIFIC)) ;
} ;

/*------------------------------------------------------------------------------
 * Half open a BGP Connection
 *
 * Used during sIdle when a connection is made before the IdleHoldTimer
 * expires.
 *
 * Expected only for the secondary connection.
 *
 * Sets the connection half_open, and remains in sIdle.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_half_open)
{
  assert( (connection->ordinal == bgp_connection_secondary)
       && !connection->half_open ) ;

  connection->half_open = 1 ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Start up BGP Connection
 *
 * Used on exit from sIdle to sConnect or sActive -- when the IdleHoldTimer
 * expires.
 *
 * If not half open:
 *
 *   Enters either sConnect or sActive, depending on primary/secondary.
 *
 *   Throws a session_eStart exception so the Routing Engine gets to see this,
 *   and a follow-on fsm_eBGP_Start event to kick the connect() or accept()
 *   into life.
 *
 * If is half open:
 *
 *   Must be secondary.  Enters sActive.
 *
 *   Throws a session_eStart exception so the Routing Engine gets to see this,
 *   and a follow-on bgp_fsm_eTCP_connection_open event to kick sActive into
 *   processing the already open connection.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_start)
{
  bgp_fsm_event_t fsm_event ;

  if (!connection->half_open)
    /* Straightforward -- set eBGP_Start follow-on event                */
    fsm_event = bgp_fsm_eBGP_Start ;
  else
    {
      /* Is half open -- so set a eTCP_connection_open follow-on event, then
       * change to sActive where the event will be collected.
       */
      assert(connection->ordinal == bgp_connection_secondary) ;

      connection->half_open = 0 ;

      fsm_event = bgp_fsm_eTCP_connection_open ;
    } ;

  bgp_fsm_throw(connection, bgp_session_eStart, NULL, 0, fsm_event) ;

  return (connection->ordinal == bgp_connection_primary) ? bgp_fsm_sConnect
                                                         : bgp_fsm_sActive ;
} ;

/*------------------------------------------------------------------------------
 * Start up BGP connect()
 *
 *   * to change from sIdle to sConnect -- when the IdleHoldTimer
 *
 *   * to loop back to sConnect         -- when the ConnectRetryTimer expires.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_connect)
{
  assert((connection->ordinal == bgp_connection_primary)
                                           && (next_state = bgp_fsm_sConnect)) ;
  bgp_open_connect(connection) ;

  return next_state ;
} ;
/*------------------------------------------------------------------------------
 * Start up BGP accept()
 *
 *   * to change from sIdle to sActive -- when the IdleHoldTimer expires.
 *
 *   * to loop back to sActive         -- when the ConnectRetryTimer expires.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_accept)
{
  assert((connection->ordinal == bgp_connection_secondary)
                                            && (next_state = bgp_fsm_sActive)) ;
  bgp_connection_enable_accept(connection) ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * TCP connection open has come up -- connect() or accept()
 *
 * Send BGP Open Message to peer.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_send_open)
{
  if (BGP_DEBUG (normal, NORMAL))
    {
      const char* how ;

      if (connection->ordinal == bgp_connection_primary)
        how = "connect" ;
      else
        how = "accept" ;

      zlog_debug("%s open %s(), local address %s",
                                sutoa(connection->su_remote).str,
                                how,
                                sutoa(connection->su_local).str) ;
    } ;

  bgp_connection_read_enable(connection) ;

  bgp_msg_send_open(connection, connection->session->open_send) ;

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * TCP connection has failed to come up -- sConnect/sActive states.
 *
 * This is in response to TCP_connection_open_failed, which has posted the
 * exception -- so now need to deal with it.
 *
 * Close the connection -- if secondary connection, disable accept.
 *
 * Will stay in sConnect/sActive states.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_failed)
{
  return bgp_fsm_catch(connection, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * Fatal I/O error -- any state (other than sIdle).
 *
 * Close the connection (if any) -- if secondary connection, disable accept.
 *
 * This is in response to TCP_fatal_error, which has posted the
 * exception -- so now need to deal with it.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_fatal)
{
  return bgp_fsm_catch(connection, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * ConnectRetryTimer expired -- sConnect/sActive states.
 *
 * If the connection failed, the connection will have been closed.  For the
 * secondary connection accept() will have been disabled.
 *
 * In any case, close the connection (but leave timers running) and then
 * throw a session_eRetry event and an fsm_eBGP_Start follow-on.
 *
 * NB: the connection remains in the current state, and the retry timer will
 *     still be running, because it automatically recharges.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_retry)
{
  bgp_connection_close(connection, true) ;      /* true => keep timers, the
                                                 * FSM handles them.    */

  bgp_fsm_throw(connection, bgp_session_eRetry, NULL, 0,
                                                           bgp_fsm_eBGP_Start) ;
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * TCP connection has closed -- sOpenSent/sOpenConfirm/sEstablished states
 *
 * This is in response to TCP_connection_closed, which has posted the
 * exception -- so now need to deal with it.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_closed)
{
  return bgp_fsm_catch(connection, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * Hold timer expire -- sOpenSent/sOpenConfirm/sEstablished/sStopping
 *
 * This means either: have finished sending NOTIFICATION (end of "courtesy"
 *                    wait time)
 *
 *                or: can wait no longer for something from the other end.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_expire)
{
  /* The process of sending a NOTIFICATION comes to an end here.        */
  if (connection->notification_pending)
    {
      bgp_connection_close(connection, true) ;  /* true => keep timers, the
                                                 * FSM handles them.    */
      return next_state ;
    } ;

  /* Otherwise: treat as a general exception.                           */
  return bgp_fsm_throw_stop(connection, bgp_session_eExpired,
                       bgp_notify_new(BGP_NOMC_HOLD_EXP, BGP_NOMS_UNSPECIFIC)) ;
} ;

/*------------------------------------------------------------------------------
 * Received an acceptable OPEN Message
 *
 * The next state is expected to be sOpenConfirm.
 *
 * However: this is where we do Collision Resolution.
 *
 * If the sibling connection has reached OpenConfirm before this one, then now
 * this one either closes its sibling, or itself.
 *
 * As soon as a connection reaches sEstablished, it immediately kills off any
 * sibling -- so the farthest two connections can get is to sOpenSent.
 *
 * The connection that is closed should send a Cease/Collision Resolution
 * NOTIFICATION.  The other end should do likewise.
 *
 * The connection that is closed will fall back to sIdle -- so that if the
 * connection that wins the race to sOpenConfirm fails there, then both will be
 * back in sIdle state.
 *
 * If makes it past Collision Resolution, respond with a KEEPALIVE (to "ack"
 * the OPEN message).
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_recv_open)
{
  bgp_session    session = connection->session ;
  bgp_connection sibling = bgp_connection_get_sibling(connection) ;

  assert(session != NULL) ;

  /* If there is a sibling, and it is in sOpenConfirm state, then now must do
   * collision resolution.
   */
  if ((sibling != NULL) && (sibling->state == bgp_fsm_sOpenConfirm))
    {
      bgp_connection loser ;

      /* Before choosing a winner, check that both have the same BGP Id.
       *
       * The two connections are to an address which is configured for the
       * given peer, and both have given the right ASN.  It would be
       * astonishing (but also disturbing) to find that they had different
       * BGP Ids !!!
       */
      if (connection->open_recv->bgp_id != sibling->open_recv->bgp_id)
        {
          bgp_fsm_exception(        sibling,    bgp_session_eOpen_reject,
                   bgp_msg_noms_o_bad_id(NULL, sibling->open_recv->bgp_id)) ;

          return bgp_fsm_throw_stop(connection, bgp_session_eOpen_reject,
                   bgp_msg_noms_o_bad_id(NULL, connection->open_recv->bgp_id)) ;
        } ;

      /* NB: bgp_id in open_state is in *network* order                 */
      loser = (ntohl(session->open_send->bgp_id) <
                                              ntohl(sibling->open_recv->bgp_id))
                ? connection
                : sibling ;

      /* Throw exception                                                */
      bgp_fsm_exception(loser, bgp_session_eCollision,
                      bgp_notify_new(BGP_NOMC_CEASE, BGP_NOMS_C_COLLISION)) ;

      /* If self is the loser, exit now to process the eBGP_Stop        */
      if (loser == connection)
        return connection->state ;      /* sBGP_Stop deals with state   */
    } ;

  /* All is well: send a KEEPALIVE message to acknowledge the OPEN      */
  bgp_msg_send_keepalive(connection, 1) ;

  /* Transition to OpenConfirm state                                    */
  return next_state ;
}

/*------------------------------------------------------------------------------
 * FSM error -- received wrong type of message !
 *
 * For example, an OPEN message while in sEstablished state.
 *
 * For use in: sOpenSent, sOpenConfirm and sEstablished states.
 *
 * Sends NOTIFICATION.
 *
 * Next state will be sIdle, except if is sEstablished, when will be
 * sStopping.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_error)
{
  return bgp_fsm_throw_stop(connection, bgp_session_eFSM_error,
                            bgp_notify_new(BGP_NOMC_FSM, BGP_NOMS_UNSPECIFIC)) ;
} ;

/*------------------------------------------------------------------------------
 * Receive NOTIFICATION from far end -- sOpenSent/sOpenConfirm/sEstablished
 *
 * This is in response to Receive_NOTIFICATION_message, which has posted the
 * exception -- so now need to deal with it.
 *
 * Next state will be sIdle, except if is sEstablished, when will be sStopping.
 *
 * Per RFC 5492: if get a NOTIFICATION: "Open/Unsupported Optional Parameter"
 *               then suppress sending of capabilities.
 *
 *               If didn't send capabilities the last time, doesn't make any
 *               difference if suppress them from now on !
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_recv_nom)
{
  if (connection->state != bgp_fsm_sEstablished)
    {
      if (   (connection->notification->code    == BGP_NOMC_OPEN)
          && (connection->notification->subcode == BGP_NOMS_O_OPTION) )
        {
          if (!connection->cap_suppress)
            BGP_FSM_DEBUG(connection, "Suppressing Capabilities") ;

          connection->cap_suppress = true ;
        } ;
    } ;

  return bgp_fsm_catch(connection, next_state) ;
} ;

/*------------------------------------------------------------------------------
 * Pending NOTIFICATION has cleared write buffers
 *                                          -- sOpenSent/sOpenConfirm/sStopping
 *
 * Set the "courtesy" HoldTimer.  Expect to stay in current state.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_sent_nom)
{
  bgp_hold_timer_set(connection, 5) ;
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Send Keepalive to peer.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_send_kal)
{
  bgp_msg_send_keepalive(connection, 0) ;
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Session Established !
 *
 * If there is another connection, that is now snuffed out and this connection
 * becomes the primary.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_establish)
{
  bgp_session    session = connection->session ;
  bgp_connection sibling = bgp_connection_get_sibling(connection) ;

  assert(session != NULL) ;

  /* The first thing to do is to snuff out any sibling                  */
  if (sibling != NULL)
    bgp_fsm_discard_sibling(sibling,
                      bgp_notify_new(BGP_NOMC_CEASE, BGP_NOMS_C_COLLISION)) ;

  /* Establish self as primary and copy state up to session             */
  bgp_connection_make_primary(connection) ;

  /* Signal exciting session event                                      */
  bgp_session_event(session, bgp_session_eEstablished, NULL, 0, 0, 0) ;

  /* TODO: now would be a good time to withdraw the password from listener ?  */

  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Keepalive packet is received -- sOpenConfirm/sEstablished
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_recv_kal)
{
  bgp_hold_timer_recharge(connection) ;
  return next_state ;
} ;

/*------------------------------------------------------------------------------
 * Update packet is received.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_update)
{
  bgp_hold_timer_recharge(connection) ;
  return next_state ;
}

/*------------------------------------------------------------------------------
 * Connection exit
 *
 * Ring down the curtain.  Connection structure will be freed by the BGP Engine.
 *
 * NB: requires the session LOCKED
 */
static bgp_fsm_action(bgp_fsm_exit)
{
  assert(connection->state == bgp_fsm_sStopping) ;

  bgp_connection_exit(connection) ;

  return bgp_fsm_sStopping ;
} ;

/*==============================================================================
 * Catching FSM Exceptions.
 *
 * Uses the posted information and the expected next_state to deal with some
 * exception.  Proceeds:
 *
 *  1a) if have notification & not eNOM_recv & is in a suitable state
 *
 *      Suitable states are sOpenSent/sOpenConfirm/sEstablished.
 *
 *      Send NOTIFICATION -- see notes above on the process.
 *
 *      For sOpenSent/sOpenConfirm, override the next_state to stay where it is
 *      until NOTIFICATION process completes.
 *
 *      For sEstablished, the next_state will be sStopping.
 *
 *  1b) otherwise: close the connection file.
 *
 *      If the next_state is sStopping, there is nothing else to do, so
 *      raise an eBGP_Stop event, so that the connection exits.
 *
 *   2) if next state is sStopping, and exception is not session_eDiscard
 *
 *      This means we bring down the session, so discard any sibling.
 *
 *      The sibling will send any notification, and proceed immediately to
 *      sStopping.
 *
 *      (The sibling will be session_eDiscard -- so no deadly embrace here.)
 *
 * So: proceeds to the given next_state unless has started notification process,
 * and next_state was not sStopping.
 *
 * Issues follow-on events:
 *
 *   * bgp_fsm_eSent_NOTIFICATION_message if notification clears to TCP buffers
 *     immediately.
 *
 *   * bgp_fsm_eTCP_fatal_error (or other such) if fails trying to send
 *     notification.
 *
 *   * bgp_fsm_eBGP_Stop if next_state is sStopping and no notification to
 *     send.
 *
 * The state machine takes care of the rest:
 *
 *   * complete entry to new state
 *
 *   * send message to Routeing Engine
 *
 *   * cutting the connection loose if ends up sStopping.
 *
 * NB: requires the session LOCKED -- connection-wise
 */
static bgp_fsm_state_t
bgp_fsm_catch(bgp_connection connection, bgp_fsm_state_t next_state)
{
  bgp_notify send_notification ;

  assert(connection->exception != bgp_session_null_event) ;

  /* Have a notification to send iff not just received one, and is in a
   * suitable state to send one at all.
   */
  if (connection->exception == bgp_session_eNOM_recv)
    send_notification = NULL ;
  else
    {
      if (   (connection->state != bgp_fsm_sOpenSent)
          && (connection->state != bgp_fsm_sOpenConfirm)
          && (connection->state != bgp_fsm_sEstablished) )
        bgp_notify_unset(&connection->notification) ;

      send_notification = connection->notification ;
    } ;

  /* If there is a NOTIFICATION to send, send it if possible.
   * Otherwise, close the connection but leave the timers.
   *
   * The state transition stuff looks after timers.  In particular an error
   * in Connect/Active states leaves the ConnectRetryTimer running.
   *
   * However, in any event, no longer require any Keepalive.
   */
  qtimer_unset(connection->keepalive_timer) ;

  if ((send_notification != NULL) && bgp_connection_part_close(connection))
    {
      /* If not changing to stopping, we hold in the current state until
       * the NOTIFICATION process is complete.
       */
      if (next_state != bgp_fsm_sStopping)
        next_state = connection->state ;

      /* Write the message                                                  */
      bgp_msg_write_notification(connection, send_notification) ;

      /* notification is sitting in the write buffer
       *
       * notification_pending is set, so write action will raise the required
       * event in due course.
       *
       * Set the HoldTimer to something suitable.  Don't really expect this
       * to happen in anything except sEstablished state -- but copes.  (Is
       * ready to wait 20 seconds in sStopping state and 5 otherwise.)
       */
      bgp_hold_timer_set(connection,
                                   (next_state == bgp_fsm_sStopping) ? 20 : 5) ;
    }
  else
    {
      bgp_connection_close(connection, true) ;  /* true => keep timers, the
                                                 * FSM handles them.    */
      if (next_state == bgp_fsm_sStopping)      /* can exit if sStopping  */
        bgp_fsm_event(connection, bgp_fsm_eBGP_Stop) ;
    } ;

  /* If sStopping and not eDiscard, do in any sibling                   */
  if (   (next_state == bgp_fsm_sStopping)
      && (connection->exception != bgp_session_eDiscard) )
    {
      bgp_connection sibling ;

      sibling = bgp_connection_get_sibling(connection) ;  /* ... if any */

      if (sibling != NULL)
        bgp_fsm_discard_sibling(sibling, bgp_notify_dup(send_notification)) ;
    } ;

  /* Return the (possibly adjusted) next_state                          */
  return next_state ;
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
 *     This is a one shot timer, which generates a bgp_fsm_eBGP_Start event.
 *
 *   * ConnectRetryTimer -- uses connection.hold_timer with jitter
 *
 *     This runs while in sConnect or sActive state, and is the period for which
 *     the connection is prepared to wait between attempts to connect.
 *
 *     When trying to make a connect connection:
 *
 *       The primary connection (if any) will be in sConnect state.
 *
 *       The secondary connection (if any) will be in sActive state.
 *
 *       If nothing happens before the ConnectRetryTimer expires, then
 *       the connection attempt will be abandoned, and another started.
 *
 *       If the connection attempt fails, closes any connect() socket, but then
 *       waits for the ConnectRetryTimer to expire.
 *
 *     The ConnectRetryTimer recharges itself (with new jitter each time).  It
 *     generates a bgp_fsm_eConnectRetry_timer_expired event.
 *
 *  * HoldTimer  -- uses connection.hold_timer *without* jitter
 *
 *    This timer is used in sOpenSent state, and limits the time will wait for
 *    an Open to appear from the other end.  RFC4271 calls for this to be a
 *    "large value" -- suggesting 240 seconds.
 *
 *    This timer is also used in sOpenConfirm and sEstablished states, and
 *    limits the time the connection will be held if hear nothing from the
 *    other end.  In these states the timer is set to the negotiated HoldTime.
 *    If this is zero, then the HoldTime is infinite.
 *
 *    This is a one shot timer, which generates a bgp_fsm_eHold_Timer_expired
 *    event.
 *
 *  * KeepaliveTimer -- uses connection.keepalive_timer with jitter.
 *
 *    This timer is used in sOpenConfirm and sEstablished states only.
 *
 *    The default KeepalineTimer is 1/3 the HoldTimer, and is set from the
 *    negotiated HoldTime.  If that is zero, then the KeepaliveTime is also
 *    zero and treated as infinite, and no KEEPALIVE messages will be sent
 *    (other than the "ack" of the OPEN message).
 *
 *    This timer is recharged each time it goes off, and generates a
 *    bgp_fsm_eKeepAlive_timer_expired event.
 */

/* Forward reference the action functions                               */
static qtimer_action bgp_idle_hold_timer_action ;
static qtimer_action bgp_connect_retry_timer_action ;
static qtimer_action bgp_hold_timer_action ;
static qtimer_action bgp_keepalive_timer_action ;

/*==============================================================================
 * Timer set functions -- general and HoldTimer specific.
 */
enum
{
  no_jitter   = 0,
  with_jitter = 1,
} ;

/*------------------------------------------------------------------------------
 * Start or reset given qtimer with given interval, in seconds.
 *
 * If the interval is zero, unset the timer.
 */
static void
bgp_timer_set(bgp_connection connection, qtimer timer, unsigned secs,
                                              int jitter, qtimer_action* action)
{
  if (secs == 0)
    qtimer_unset(timer) ;
  else
    {
      secs *= 40 ;      /* a bit of resolution for jitter       */
      if (jitter != no_jitter)
        secs -= ((rand() % ((int)secs + 1)) / 4) ;
      qtimer_set_interval(timer, QTIME(secs) / 40, action) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set HoldTimer with given time (without jitter) so will generate a
 * Hold_Timer_expired event.
 *
 * Setting 0 will unset the HoldTimer.
 */
static void
bgp_hold_timer_set(bgp_connection connection, unsigned secs)
{
  bgp_timer_set(connection, connection->hold_timer, secs, no_jitter,
                                                        bgp_hold_timer_action) ;
} ;

/*==============================================================================
 * Completion of State Change
 *
 * This performs fixed changes associated with the entry to each state from
 * *another* state.
 *
 * connection->state == current (soon to be old) state
 *
 * Set and unset all the connection timers as required by the new state of
 * the connection -- which may depend on the current state.
 *
 * NB: requires the session LOCKED
 */
static void
bgp_fsm_state_change(bgp_connection connection, bgp_fsm_state_t new_state)
{
  bgp_connection sibling ;
  uint interval ;

  bgp_session    session = connection->session ;

  if (bgp_dump_state_flag)
    bgp_dump_state(connection, new_state) ;

  switch (new_state)
    {
    /* Base state of connection's finite state machine -- when a session has
     * been enabled.  Falls back to Idle in the event of various errors.
     *
     * In sIdle state:
     *
     *   either: the IdleHoldTimer is running, at the end of which the
     *           BGP Engine will try to connect.
     *
     *       or: the connection is comatose, in which case will stay that way
     *           until sibling connection also falls back to Idle (from
     *           OpenSent/OpenConfirm.
     *
     * When entering sIdle from anything other than Initial state, and not
     * falling into a coma, extend the IdleHoldTimer.
     *
     * In sIdle state doesn't refuse connections (unless comatose), but won't
     * act on them until the IdleHoldTimer expires.
     */
    case bgp_fsm_sIdle:
      interval = session->idle_hold_timer_interval ;
      sibling  = bgp_connection_get_sibling(connection) ;

      if (connection->state == bgp_fsm_sInitial)
        interval = (interval > 0) ? interval : 1 ;  /* may not be zero  */
      else
        {
          if ( (sibling != NULL)
                && (   (sibling->state == bgp_fsm_sOpenSent)
                    || (sibling->state == bgp_fsm_sOpenConfirm) ) )
            {
              interval = 0 ;              /* unset the HoldTimer        */
              connection->comatose = 1 ;  /* so now comatose            */
            }
          else
            {
              /* increase the IdleHoldTimer interval                    */
              interval *= 2 ;

              if      (interval < 4)      /* enforce this minimum       */
                interval = 4 ;
              else if (interval > 120)
                interval = 120 ;

              session->idle_hold_timer_interval = interval ;

              if (connection->ordinal == bgp_connection_secondary)
                bgp_connection_enable_accept(connection) ;

              /* if sibling is comatose, set time for it to come round  */

              if ((sibling != NULL) && (sibling->comatose))
                {
                  sibling->comatose = 0 ;       /* no longer comatose   */

                  if (sibling->ordinal == bgp_connection_secondary)
                    bgp_connection_enable_accept(sibling) ;

                  bgp_timer_set(sibling, sibling->hold_timer, interval,
                                      with_jitter, bgp_idle_hold_timer_action) ;
                } ;
            } ;
        } ;

      bgp_timer_set(connection, connection->hold_timer, interval,
                                      with_jitter, bgp_idle_hold_timer_action) ;

      qtimer_unset(connection->keepalive_timer) ;

      break;

    /* In sConnect state the primary connection is attempting connect().
     *
     * In sActive state the secondary connection is prepared to accept().
     *
     * The ConnectRetryTimer automatically recharges, because will loop back
     * round into the same state.
     */
    case bgp_fsm_sConnect:
    case bgp_fsm_sActive:
      bgp_timer_set(connection, connection->hold_timer,
                           session->connect_retry_timer_interval, with_jitter,
                                               bgp_connect_retry_timer_action) ;
      qtimer_unset(connection->keepalive_timer) ;
      break;

    /* In sOpenSent state is waiting for an OPEN from the other end, before
     * proceeding to sOpenConfirm state.
     *
     * Prepared to wait for quite a long time for this.
     */
    case bgp_fsm_sOpenSent:
      bgp_hold_timer_set(connection, session->open_hold_timer_interval) ;
      qtimer_unset(connection->keepalive_timer) ;
      break;

    /* In sOpenConfirm state is waiting for an "ack" before proceeding to
     * sEstablished.
     *
     * There is only one way into sEstablished, and that is from sOpenConfirm.
     * sOpenConfirm starts the KeepaliveTimer.  It would be wrong to reset the
     * timer on entry to sEstablished.
     *
     * In both cases have just received a message, so can restart the HoldTimer.
     *
     * Both use the negotiated Hold Time and Keepalive Time.  May send further
     * KEEPALIVE messages in sOpenConfirm.
     *
     * If the negotiated Hold Time value is zero, then the Keepalive Time
     * value will also be zero, and this will unset both timers.
     */
    case bgp_fsm_sOpenConfirm:
      bgp_timer_set(connection, connection->keepalive_timer,
                                 connection->keepalive_timer_interval,
                                      with_jitter, bgp_keepalive_timer_action) ;
    case bgp_fsm_sEstablished:
      bgp_hold_timer_set(connection, connection->hold_timer_interval) ;
      break;

    /* The connection is coming to an dead stop.
     *
     * Leave the HoldTimer running -- may be waiting for NOTIFICATION to clear,
     * or for the "courtesy" time to expire.
     */
    case bgp_fsm_sStopping:
      qtimer_unset(connection->keepalive_timer) ;

      break ;

    default:
      zabort("Unknown bgp_fsm_state") ;
    } ;

  /* Finally: set the new state                                         */
  connection->state = new_state ;
} ;

/*==============================================================================
 * Timer Action Functions
 */

/*------------------------------------------------------------------------------
 * BGP start timer action => bgp_fsm_eBGP_Start event
 *
 * The timer is automatically unset, which is fine.
 */
static void
bgp_idle_hold_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (start timer expire)") ;

  bgp_fsm_event(connection, bgp_fsm_eBGP_Start) ;
} ;

/*------------------------------------------------------------------------------
 * BGP connect retry timer => bgp_fsm_eConnectRetry_timer_expired event
 *
 * The timer is recharged here, applying a new "jitter", but that may be
 * overridden by the bgp_event() handling.
 */
static void
bgp_connect_retry_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (connect timer expire)") ;

  bgp_timer_set(connection, connection->hold_timer,
         connection->session->connect_retry_timer_interval, with_jitter, NULL) ;

  bgp_fsm_event(connection, bgp_fsm_eConnectRetry_timer_expired) ;
} ;

/*------------------------------------------------------------------------------
 * BGP hold timer => bgp_fsm_eHold_Timer_expired event
 *
 * The timer is automatically unset, which is fine.
 */
static void
bgp_hold_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (holdtime timer expire)") ;

  bgp_fsm_event(connection, bgp_fsm_eHold_Timer_expired) ;
} ;

/*------------------------------------------------------------------------------
 * BGP keepalive fire => bgp_fsm_eKeepAlive_timer_expired
 *
 * The timer is recharged here, applying a new "jitter", but that may be
 * overridden by the bgp_event() handling.
 */
static void
bgp_keepalive_timer_action(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_connection connection = timer_info ;

  BGP_FSM_DEBUG(connection, "Timer (keepalive timer expire)") ;

  bgp_timer_set(connection, connection->keepalive_timer,
                    connection->session->keepalive_timer_interval,
                                                            with_jitter, NULL) ;

  bgp_fsm_event(connection, bgp_fsm_eKeepAlive_timer_expired) ;
} ;

/* BGP Session -- functions
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

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_peer_index.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_route_refresh.h"
#include "bgpd/bgp_msg_write.h"

#include "bgpd/bgp_packet.h"

#include "lib/memory.h"
#include "lib/sockunion.h"
#include "lib/mqueue.h"
#include "lib/zassert.h"

/* prototypes */
static int bgp_session_defer_if_stopping(bgp_session session);
static void bgp_session_do_enable(mqueue_block mqb, mqb_flag_t flag) ;
static void bgp_session_do_update_recv(mqueue_block mqb, mqb_flag_t flag);
static void bgp_session_do_update_send(mqueue_block mqb, mqb_flag_t flag);
static void bgp_session_do_end_of_rib_send(mqueue_block mqb, mqb_flag_t flag);
static void bgp_session_do_route_refresh_send(mqueue_block mqb,
                                                               mqb_flag_t flag);
static void bgp_session_do_disable(mqueue_block mqb, mqb_flag_t flag) ;
static void bgp_session_XON(bgp_session session);
static void bgp_session_do_XON(mqueue_block mqb, mqb_flag_t flag);

/*==============================================================================
 * BGP Session.
 *
 * Every bgp_peer has (at most) one bgp_session associated with it.
 *
 * A session is shared by the Routeing Engine and the BGP Engine -- so there
 * is a mutex to coordinate access.
 *
 * A session is created some time before it is enabled, and may be destroyed
 * when the peer is disabled, or once the session has stopped.
 *
 * A session may be in one of four states:
 *
 *   * bgp_session_Idle         -- not doing anything
 *   * bgp_session_Enabled      -- the BGP Engine is trying to connect
 *   * bgp_session_Established  -- the BGP Engine is exchanging updates etc
 *   * bgp_session_Stopping     -- the BGP Engine is stopping the session
 *   * bgp_session_Stopped      -- a session has come to a dead stop
 *
 * NB: in Idle and Stopped states the BGP Engine has no interest in the session.
 *     These are known as the "inactive" states.
 *
 * NB: in Enabled, Established and Stopping states the BGP Engine is running
 *     connection(s) for the session.  These are known as the "active" states.
 *
 *     While the session is active the Routeing Engine should not attempt to
 *     change any shared item in the session, except under the mutex.  And
 *     even then it may make no sense !
 *
 * The BGP Engine's primary interest is in its (private) bgp_connection
 * structure(s), which (while a session is Enabled, Established or Stopping)
 * are pointed to by their associated session.
 */

/*==============================================================================
 * BGP Session handling.
 *
 */

/*------------------------------------------------------------------------------
 * Initialise new session structure -- allocate if required.
 *
 * Ties peer and session together.  Sets session sIdle, initialises mutex.
 *
 * Unsets everything else -- mostly by zeroising it.
 *
 * NB: if not allocating, the existing session MUST be sIdle/sStopped OR never
 *     been kissed.
 *
 * NB: in any event, the peer's peer index entry MUST have a NULL session
 *     pointer.
 */
extern bgp_session
bgp_session_init_new(bgp_session session, bgp_peer peer)
{
  assert(peer->session == NULL) ;
  assert(peer->index_entry->accept == NULL) ;

  if (session == NULL)
    session = XCALLOC(MTYPE_BGP_SESSION, sizeof(struct bgp_session)) ;
  else
    memset(session, 0, sizeof(struct bgp_session)) ;

  qpt_mutex_init_new(&session->mutex, qpt_mutex_recursive) ;

  peer->session  = session ;
  session->peer  = peer ;
  session->state = bgp_session_sIdle ;

  session->index_entry = peer->index_entry ;

  /* Zeroising the structure has set:
   *
   *   made           -- false, not yet sEstablished
   *
   *   event          -- bgp_session_null_event
   *   notification   -- NULL -- none
   *   err            -- 0    -- none
   *   ordinal        -- 0    -- unset
   *
   *   open_send      -- NULL -- none
   *   open_recv      -- NULL -- none
   *
   *   connect        -- unset, false
   *   listen         -- unset, false
   *
   *   cap_override   -- unset, false
   *   cap_strict     -- unset, false
   *
   *   ttl            -- unset
   *   port           -- unset
   *   su_peer        -- NULL -- none
   *
   *   log            -- NULL -- none
   *   host           -- NULL -- none
   *   password       -- NULL -- none
   *
   *   idle_hold_timer_interval      )
   *   connect_retry_timer_interval  )
   *   open_hold_timer_interval      ) unset
   *   hold_timer_interval           )
   *   keepalive_timer_interval      )
   *
   *   as4            -- unset, false
   *   route_refresh_pre -- unset, false
   *
   *   su_local       -- NULL -- none
   *   su_remote      -- NULL -- none
   *
   *   connections[]  -- NULL -- none
   */
  confirm(bgp_session_null_event == 0) ;

  return session ;
} ;

/* Free session structure
 *
 */
extern bgp_session
bgp_session_free(bgp_session session)
{
  if (session == NULL)
    return NULL;

  assert(!bgp_session_is_active(session)) ;

  qpt_mutex_destroy(&session->mutex, 0) ;

  bgp_notify_free(&session->notification);
  bgp_open_state_free(session->open_send);
  bgp_open_state_free(session->open_recv);
  if (session->host != NULL)
    XFREE(MTYPE_BGP_SESSION, session->host);
  if (session->password != NULL)
    XFREE(MTYPE_BGP_SESSION, session->password);

  /* Zeroize to catch dangling references asap */
  memset(session, 0, sizeof(struct bgp_session)) ;
  XFREE(MTYPE_BGP_SESSION, session);

  return NULL;
}

/*==============================================================================
 * Enable session for given peer -- allocate session if required.
 *
 * Sets up the session given the current state of the peer.  If the state
 * changes, then....
 *
 *
 */
extern void
bgp_session_enable(bgp_peer peer)
{
  bgp_session    session ;
  mqueue_block   mqb ;

  /* Set up session if required.  Check session if already exists.
   *
   * Only the Peering Engine creates sessions, so it is safe to pick up the
   * peer->session pointer and test it.
   *
   * If session exists, it MUST be inactive.
   *
   * Peering Engine does not require the mutex while the session is inactive.
   */
  session = peer->session ;

  if (session == NULL)
    session = bgp_session_init_new(NULL, peer) ;
  else
    {
      assert(session->peer == peer) ;
      /* if session is stopping then defer the enable */
      if (bgp_session_defer_if_stopping(session))
        return;
      assert(!bgp_session_is_active(session)) ;
    } ;

  /* Initialise what we need to make and run connections                */
  session->state    = bgp_session_sIdle;
  session->made     = 0;
  session->defer_enable = 0;
  session->event    = bgp_session_null_event;
  bgp_notify_free(&session->notification);
  session->err      = 0;
  session->ordinal  = 0;

  session->open_send = bgp_peer_open_state_init_new(session->open_send, peer);
  session->open_recv = bgp_open_state_free(session->open_recv);

  session->connect  = (peer->flags & PEER_FLAG_PASSIVE) != 0 ;
  session->listen   = 1 ;

  session->ttl      = peer->ttl ;
  session->port     = peer->port ;

  session->su_peer  = sockunion_dup(&peer->su) ;

  session->log      = peer->log ;

  /* take copies of host and password */
  XFREE(MTYPE_BGP_SESSION, session->host);
  session->host     = XSTRDUP(MTYPE_BGP_SESSION, peer->host);
  XFREE(MTYPE_BGP_SESSION, session->password);
  session->password = XSTRDUP(MTYPE_BGP_SESSION, peer->password);

  session->idle_hold_timer_interval     = peer->v_start ;
  session->connect_retry_timer_interval = peer->v_connect ;
  /* TODO: proper value for open_hold_timer_interval    */
  session->open_hold_timer_interval     = 4 * 60;
  session->hold_timer_interval          = peer->v_holdtime ;
  session->keepalive_timer_interval     = peer->v_keepalive ;

  session->as4       = 0;
  session->route_refresh_pre = 0;

  /* su_local set when session Established */
  /* su_remote  set when session Established */

  /* Routeing Engine does the state change now.                         */
  session->state    = bgp_session_sEnabled ;

  /* Now pass the session to the BGP Engine, which will set about       */
  /* making and running a connection to the peer.                       */

  mqb = mqb_init_new(NULL, bgp_session_do_enable, session) ;

  confirm(sizeof(struct bgp_session_enable_args) == 0) ;

  bgp_to_bgp_engine(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine: session enable message action
 */
static void
bgp_session_do_enable(mqueue_block mqb, mqb_flag_t flag)
{
  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;

      BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

      assert(session->state == bgp_session_sEnabled) ;
      bgp_fsm_enable_session(session) ;

      BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
    } ;

  mqb_free(mqb) ;
} ;

/*==============================================================================
 * Disable session for given peer -- if enabled (!).
 *
 * Passes any bgp_notify to the BGP Engine, which will dispose of it in due
 * course.
 *
 * If no bgp_notify provided, will send Cease/Administrative Shutdown (2).
 *
 * When session has been brought to a stop, BGP Engine will respond with an
 * eDisabled event (unless session stopped of its own accord first).
 */

extern void
bgp_session_disable(bgp_peer peer, bgp_notify notification)
{
  bgp_session    session ;
  mqueue_block   mqb ;
  struct bgp_session_disable_args* args ;

  session = peer->session ;
  assert((session != NULL) && (session->peer == peer)) ;

  /* Do nothing if session is not active, or is already stopping.       */

  if ( (session->state != bgp_session_sEnabled) &&
       (session->state != bgp_session_sEstablished) ) ;
    {
      bgp_notify_free(&notification) ;  /* discard any bgp_notify       */
      return ;
    } ;

  /* Now change to stopping state                                       */
  session->state = bgp_session_sStopping;

  /* Ask the BGP engine to disable the session.
   *
   * NB: it is, of course, possible that the session will stop between
   *     issuing the disable and it being processed by the BGP Engine.
   *
   *     The BGP Engine quietly discards disable messages for sessions which
   *     are not active.
   *
   * NB: The BGP Engine will discard any outstanding work for the session.
   *
   *     The Peering Engine should discard all further messages for this
   *     session up to the event message that tells it the session has
   *     stopped.
   */
  mqb = mqb_init_new(NULL, bgp_session_do_disable, session) ;

  args = mqb_get_args(mqb) ;
  args->notification = notification ;

  bgp_to_bgp_engine_priority(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine: session disable message action
 */
static void
bgp_session_do_disable(mqueue_block mqb, mqb_flag_t flag)
{
  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;
      struct bgp_session_disable_args* args = mqb_get_args(mqb) ;

      /* Immediately discard any other messages for this session.       */
      mqueue_revoke(p_bgp_engine->queue, session) ;

      /* Get the FSM to send any notification and close connections     */
      bgp_fsm_disable_session(session, args->notification) ;
    } ;

  mqb_free(mqb) ;
}

/*==============================================================================
 * Send session event signal from BGP Engine to Routeing Engine
 */
extern void
bgp_session_event(bgp_session session, bgp_session_event_t  event,
                                       bgp_notify           notification,
                                       int                  err,
                                       bgp_connection_ord_t ordinal,
                                       int                  stopped)
{
  struct bgp_session_event_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_event, session) ;

  args = mqb_get_args(mqb) ;

  args->event        = event ;
  args->notification = bgp_notify_dup(notification) ;
  args->err          = err ;
  args->ordinal      = ordinal ;
  args->stopped      = stopped,

  bgp_to_peering_engine(mqb) ;
}

/*==============================================================================
 * Dispatch update to peer -- Peering Engine -> BGP Engine
 *
 * PRO TEM -- this is being passed the pre-packaged BGP message.
 *
 * The BGP Engine takes care of discarding the stream block once it's been
 * dealt with.
 */

extern void
bgp_session_update_send(bgp_session session, struct stream* upd)
{
  struct bgp_session_update_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_update_send, session) ;

  args = mqb_get_args(mqb) ;
  args->buf     = stream_dup(upd) ;
  args->pending = NULL ;

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  session->flow_control++;      /* count them in ... */
  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  bgp_to_bgp_engine(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine -- mqb action function -- write given BGP update message.
 *
 * Each connection has a pending queue associated with it, onto which messages
 * are put if the connection's write buffer is unable to absorb any further
 * messages.
 *
 * This function is called both when the mqb is received from the Peering
 * Engine, and when the BGP Engine is trying to empty the connection's pending
 * queue.
 *
 * When the mqb is received from the Peering Engine, then:
 *
 *   -- if the connection's pending queue is empty, try to send the message.
 *
 *      If cannot send the message (and not encountered any error), add it to
 *      the connection's pending queue.
 *
 *   -- otherwise, add mqb to the pending queue.
 *
 * When the mqb is on the connection's pending queue it must be the head of
 * that queue -- and still on the queue.  Then:
 *
 *   -- if the message is sent (or is now redundant), remove the mqb from
 *      the connection's pending queue.
 *
 *   -- otherwise: leave the mqb on the connection's pending queue for later,
 *      but remove the connection from the connection queue, because unable to
 *      proceed any further.
 *
 * If the mqb has been dealt with (is not on the pending queue), it is freed,
 * along with the stream buffer.
 *
 * NB: when not called "mqb_action", the mqb MUST NOT be on the connection's
 *     pending queue.
 */
static void
bgp_session_do_update_send(mqueue_block mqb, mqb_flag_t flag)
{
  struct bgp_session_update_args* args = mqb_get_args(mqb) ;

  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;

      bgp_connection connection = session->connections[bgp_connection_primary] ;

      mqueue_block head = mqueue_local_head(&connection->pending_queue) ;

      int is_pending = (args->pending != NULL) ;
      if (is_pending)
        assert( (args->pending == connection) && (mqb == head) ) ;

      /* If established, try and send.                                  */
      if (connection->state == bgp_fsm_Established)
        {
          int ret = 0 ;

          if ((head == NULL) || is_pending)
            ret = bgp_msg_send_update(connection, args->buf) ;

          if (ret == 0)
            {
              /* Did not fail, but could not write the message.         */
              if (!is_pending)
                {
                  mqueue_local_enqueue(&connection->pending_queue, mqb) ;
                  args->pending = connection ;
                }
              else
                bgp_connection_queue_del(connection) ;

              return ;  /* **** Quit now, with message intact.          */

            }
          else if (ret > 0)
            {
              /* Successfully wrote the message.                        */
              int xon ;
              BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
              xon = --session->flow_control ;   /* ... count them out */
              BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

              if (xon == 0)
                bgp_session_XON(session);
            } ;
        } ;

      /* Have dealt with the message -- if was pending, it's done.      */
      if (is_pending)
        mqueue_local_dequeue(&connection->pending_queue) ;
    } ;

  stream_free(args->buf) ;
  mqb_free(mqb) ;
} ;

/* Are we in XON state ? */
extern int
bgp_session_is_XON(bgp_peer peer)
{
  int result = 0;
  bgp_session session = peer->session;

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/
  result = session->flow_control < BGP_XON_THRESHOLD ;
  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return result;
} ;

/*==============================================================================
 * Dispatch Route Refresh to peer -- Peering Engine -> BGP Engine
 *
 * The BGP Engine takes care of discarding the bgp_route_refresh once it's been
 * dealt with.
 */
extern void
bgp_session_route_refresh_send(bgp_session session, bgp_route_refresh rr)
{
  struct bgp_session_route_refresh_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_route_refresh_send, session) ;

  args = mqb_get_args(mqb) ;
  args->rr      = rr ;
  args->pending = NULL ;

  bgp_to_bgp_engine(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine -- mqb action function -- write given BGP route refresh message.
 *
 * The logic here is the same as for bgp_session_do_update_send -- except that
 * there is no flow control (!).
 */
static void
bgp_session_do_route_refresh_send(mqueue_block mqb, mqb_flag_t flag)
{
  struct bgp_session_route_refresh_args* args = mqb_get_args(mqb) ;

  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;

      bgp_connection connection = session->connections[bgp_connection_primary] ;

      mqueue_block head = mqueue_local_head(&connection->pending_queue) ;

      int is_pending = (args->pending != NULL) ;
      if (is_pending)
        assert( (args->pending == connection) && (mqb == head) ) ;

      /* If established, try and send.                                  */
      if (connection->state == bgp_fsm_Established)
        {
          int ret = 0 ;

          if ((head == NULL) || is_pending)
            ret = bgp_msg_send_route_refresh(connection, args->rr) ;

          if (ret == 0)
            {
              /* Did not fail, but could not write everything.
               *
               * If this is not on the connection's pending queue, put it there.
               *
               * Otherwise leave it there, and take the connection off the
               * connection queue -- nothing further can be done for this
               * connection.
               */
              if (!is_pending)
                {
                  mqueue_local_enqueue(&connection->pending_queue, mqb) ;
                  args->pending = connection ;
                }
              else
                bgp_connection_queue_del(connection) ;

              return ;  /* Quit now, with message intact.       */
            } ;
        } ;

      /* Have dealt with the message -- if was pending, it's done.      */
      if (is_pending)
        mqueue_local_dequeue(&connection->pending_queue) ;
    } ;

  bgp_route_refresh_free(args->rr) ;
  mqb_free(mqb) ;
} ;

/*==============================================================================
 * Dispatch End-of-RIB to peer -- Peering Engine -> BGP Engine
 */
extern void
bgp_session_end_of_rib_send(bgp_session session, qAFI_t afi, qSAFI_t safi)
{
  struct bgp_session_end_of_rib_args* args ;
  mqueue_block   mqb ;
  qafx_num_t     qafx ;

  qafx = qafx_num_from_qAFI_qSAFI(afi, safi) ;

  mqb = mqb_init_new(NULL, bgp_session_do_end_of_rib_send, session) ;

  args = mqb_get_args(mqb) ;
  args->afi     = get_iAFI(qafx) ;
  args->safi    = get_iSAFI(qafx) ;
  args->pending = NULL ;

  bgp_to_bgp_engine(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine -- mqb action function -- write given BGP end-of-RIB message.
 *
 * The logic here is the same as for bgp_session_do_update_send -- except that
 * there is no flow control (!).
 */
static void
bgp_session_do_end_of_rib_send(mqueue_block mqb, mqb_flag_t flag)
{
  struct bgp_session_end_of_rib_args* args = mqb_get_args(mqb) ;

  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;

      bgp_connection connection = session->connections[bgp_connection_primary] ;

      mqueue_block head = mqueue_local_head(&connection->pending_queue) ;

      int is_pending = (args->pending != NULL) ;
      if (is_pending)
        assert( (args->pending == connection) && (mqb == head) ) ;

      /* If established, try and send.                                  */
      if (connection->state == bgp_fsm_Established)
        {
          int ret = 0 ;

          if ((head == NULL) || is_pending)
            ret = bgp_msg_send_end_of_rib(connection, args->afi, args->safi) ;

          if (ret == 0)
            {
              /* Did not fail, but could not write everything.
               *
               * If this is not on the connection's pending queue, put it there.
               *
               * Otherwise leave it there, and take the connection off the
               * connection queue -- nothing further can be done for this
               * connection.
               */
              if (!is_pending)
                {
                  mqueue_local_enqueue(&connection->pending_queue, mqb) ;
                  args->pending = connection ;
                }
              else
                bgp_connection_queue_del(connection) ;

              return ;  /* Quit now, with message intact.       */
            } ;
        } ;

      /* Have dealt with the message -- if was pending, it's done.      */
      if (is_pending)
        mqueue_local_dequeue(&connection->pending_queue) ;
    } ;

  mqb_free(mqb) ;
} ;

/*==============================================================================
 * Forward incoming update -- BGP Engine -> Peering Engine
 *
 * PRO TEM -- this is being passed the raw BGP message.
 *
 * The Peering Engine takes care of discarding the stream block once it's been
 * dealt with.
 */

void
bgp_session_update_recv(bgp_session session, struct stream* buf, bgp_size_t size)
{
  struct bgp_session_update_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_update_recv, session) ;

  args = mqb_get_args(mqb) ;
  args->buf = stream_dup(buf) ;
  args->size = size;

  bgp_to_peering_engine(mqb) ;
}

static void
bgp_session_do_update_recv(mqueue_block mqb, mqb_flag_t flag)
{

  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;
      bgp_peer peer = session->peer;
      struct bgp_session_update_args* args = mqb_get_args(mqb) ;

      stream_free(peer->ibuf);
      peer->ibuf = args->buf;
      bgp_update_receive (peer, args->size);
    }

  mqb_free(mqb) ;
}

/*==============================================================================
 * XON -- BGP Engine -> Peering Engine
 *
 * Can be sent more packets now
 */

static void
bgp_session_XON(bgp_session session)
{
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_XON, session) ;

  confirm(sizeof(struct bgp_session_XON_args) == 0) ;

  bgp_to_peering_engine(mqb) ;
}

static void
bgp_session_do_XON(mqueue_block mqb, mqb_flag_t flag)
{

  if (flag == mqb_action)
    {
      bgp_session session = mqb_get_arg0(mqb) ;
      bgp_peer peer = session->peer;

      bgp_write (peer);
    }

  mqb_free(mqb) ;
}

/*==============================================================================
 * Session data access functions.
 *
 *
 */

/*------------------------------------------------------------------------------
 * See if session exists and is active.
 *
 * Ensure that if exists and is not active, that the peer index entry accept
 * pointer is NULL -- this is largely paranoia, but it would be a grave
 * mistake for the listening socket(s) to find a session which is not active !
 */
extern int
bgp_session_is_active(bgp_session session)
{
  int active ;

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  if (session == NULL)
    active = 0 ;
  else
    {
      active =  (   (session->state == bgp_session_sEnabled)
                 || (session->state == bgp_session_sEstablished)
                 || (session->state == bgp_session_sStopping) ) ;

      if (!active)
        assert(session->index_entry->accept == NULL) ;
    } ;

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return active ;
} ;

/*------------------------------------------------------------------------------
 * If session is stopping we defer re-enabling the session until it has stopped.
 *
 * returns 1 if stopping and defer
 * returns 0 if not stopping
  */
static int
bgp_session_defer_if_stopping(bgp_session session)
{
  int defer_enable = 0 ;

  if (session == NULL)
      return defer_enable;

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  session->defer_enable = defer_enable = (session->state == bgp_session_sStopping);

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return defer_enable ;
} ;

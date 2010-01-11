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

#include "bgpd/bgp_session.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_peer_index.h"

#include "lib/memory.h"
#include "lib/sockunion.h"

/*==============================================================================
 * BGP Session.
 *
 * Every bgp_peer has (at most) one bgp_session associated with it.
 *
 * A session is shared by the Routeing Engine and the BGP Engine -- so there
 * is a mutex to coordinate access.
 *
 * NB: to avoid deadlock, do NOT attempt to lookup a peer or session while
 *     a session mutex !
 *
 * A session is created when the bgp peer is first enabled, and may be destroyed
 * when the peer is disabled, or once the session has stopped.
 *
 * A session may be in one of four states:
 *
 *   * bgp_session_Idle         -- not doing anything
 *   * bgp_session_Enabled      -- the BGP Engine is trying to connect
 *   * bgp_session_Established  -- the BGP Engine is exchanging updates etc
 *   * bgp_session_Stopped      -- a session has come to a dead stop
 *
 * NB: in Idle and Stopped states the BGP Engine has no interest in the session.
 *     These are known as the "inactive" states.
 *
 * NB: in Enabled and Established states the Routeing Engine it may be too late
 *     to change items in the session !  These are known as the "active" states.
 *
 * NB: once a session is enabled the BGP_Engine looks after the state, up to
 *     and including setting the Stopped state.
 *
 * The BGP Engine's primary interest is in its (private) bgp_connection
 * structure(s), which (while a session is Enabled or Established) are pointed
 * to by their associated session.
 *
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
  assert(peer->index_entry->session == NULL) ;

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
bgp_session
bgp_session_free(bgp_session session)
{
  if (session == NULL)
    return NULL;

  qpt_mutex_destroy(&session->mutex, 0) ;

  bgp_notify_free(session->notification);
  bgp_open_state_free(session->open_send);
  bgp_open_state_free(session->open_recv);
  XFREE(MTYPE_BGP_SESSION, session->host);
  XFREE(MTYPE_BGP_SESSION, session->password);

  /* Zeroize to catch dangling references asap */
  memset(session, 0, sizeof(struct bgp_session)) ;
  XFREE(MTYPE_BGP_SESSION, session);

  return NULL;
}


/* Look up session
 *
 */
extern bgp_session
bgp_session_lookup(union sockunion* su, int* exists) ;



/*==============================================================================
 * Enable session for given peer -- allocate session if required.
 *
 * Sets up the session given the current state of the peer.  If the state
 * changes, then....
 *
 *
 */
static void
bgp_session_do_enable(mqueue_block mqb, mqb_flag_t flag) ;

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
      assert(!bgp_session_is_active(session)) ;
    } ;

  /* Initialise what we need to make and run connections                */
  session->state    = bgp_session_sIdle;
  session->made     = 0;
  session->event    = bgp_session_null_event;
  session->notification = bgp_notify_free(session->notification);
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

  bgp_to_engine(mqb) ;
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
static void
bgp_session_do_disable(mqueue_block mqb, mqb_flag_t flag) ;

extern void
bgp_session_disable(bgp_peer peer, bgp_notify notification)
{
  bgp_session    session ;
  mqueue_block   mqb ;

  session = peer->session ;
  assert((session != NULL) && (session->peer == peer)) ;

  /* Should do nothing if session is not active.                        */

  if (!bgp_session_is_active(session))
    {
      bgp_notify_free(&notification) ;  /* discard any bgp_notify       */
      return ;
    } ;

  /* Now ask the BGP engine to disable the session.
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

  confirm(sizeof(struct bgp_session_enable_args) == 0) ;

  bgp_to_engine_priority(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Engine: session enable message action
 */
static void
bgp_session_do_disable(mqueue_block mqb, mqb_flag_t flag)
{
  if (flag == mqb_action)
    {


    } ;

  mqb_free(mqb) ;
} ;

/*==============================================================================
 * Send session event signal from BGP Engine to Routeing Engine
 *
 * The event has been posted to the session, and now is dispatched to the
 * Routeing Engine.
 */
extern void
bgp_session_event(bgp_session session)
{
  struct bgp_session_event_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_event, session) ;

  args = mqb_get_args(mqb) ;

  args->event        = session->event ;
  args->notification = bgp_notify_dup(session->notification) ;
  args->state        = session->state ;
  args->ordinal      = session->ordinal ;

  bgp_to_peering(mqb) ;
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

  args->buf   = upd ;

  bgp_to_engine(mqb) ;
} ;

/*==============================================================================
 * Forward incoming update -- BGP Engine -> Peering Engine
 *
 * PRO TEM -- this is being passed the raw BGP message.
 *
 * The Peering Engine takes care of discarding the stream block once it's been
 * dealt with.
 */
extern void
bgp_session_update_recv(bgp_session session, struct stream* upd)
{
  struct bgp_session_update_args* args ;
  mqueue_block   mqb ;

  mqb = mqb_init_new(NULL, bgp_session_do_update_recv, session) ;

  args = mqb_get_args(mqb) ;

  args->buf   = upd ;

  bgp_to_peering(mqb) ;
} ;



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
                 || (session->state == bgp_session_sEstablished) ) ;

      if (!active)
        assert(session->index_entry->accept == NULL) ;
    } ;

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return active ;
} ;

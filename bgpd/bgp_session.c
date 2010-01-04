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

#include "lib/memory.h"

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

/* Initialise new session structure -- allocate if required.
 *
 */
extern bgp_session
bgp_session_init_new(bgp_session session)
{
  if (session == NULL)
    session = XCALLOC(MTYPE_BGP_SESSION, sizeof(struct bgp_session)) ;
  else
    memset(session, 0, sizeof(struct bgp_session)) ;

  qpt_mutex_init_new(&session->mutex, qpt_mutex_recursive) ;



} ;

/* Look up session
 *
 */
extern bgp_session
bgp_session_lookup(union sockunion* su, int* exists) ;




/* Enable session for given peer -- allocate session if required.
 *
 * Sets up the session given the current state of the peer.  If the state
 * changes, then....
 *
 *
 */

extern bgp_session
bgp_session_enable(bgp_session session, bgp_peer peer)
{
  if (session == NULL)
    session = bgp_session_init_new(session) ;

  /* Tie back to peer and set state of session.                         */

  assert((session->peer == NULL) || (session->peer == peer)) ;
  assert((session->state != bgp_session_Enabled) &&
         (session->state != bgp_session_Established)) ;

  session->peer = peer ;
  session->state = bgp_session_Enabled ;

  /* Initialise what we need to make and run connections                */

  session->connect  = (peer->flags & PEER_FLAG_PASSIVE) != 0 ;
  session->listen   = 1 ;
  session->accept   = 0 ;

  session->ttl      = peer->ttl ;
  session->port     = peer->port ;

//session->su       = peer->su ;

  session->log      = peer->log ;
  session->host     = peer->host ;

  session->idle_hold_timer_interval     = peer->v_start ;
  session->connect_retry_timer_interval = peer->v_connect ;
  /* TODO: proper value for open_hold_timer_interval    */
  session->open_hold_timer_interval     = peer->v_connect ;
  session->hold_timer_interval          = peer->v_holdtime ;
  session->keepalive_timer_interval     = peer->v_keepalive ;

  /* Initialise the BGP Open negotiating position                       */

  session->router_id = peer->local_id ;

  /* Now pass the session to the BGP Engine, which will set about       */
  /* making and running a connection to the peer.                       */



} ;


/*==============================================================================
 * Session data access functions.
 *
 *
 */

extern int
bgp_session_is_active(bgp_session session)
{
  int ret ;

  if (session == NULL)
    return 0 ;                  /* NULL session is implicitly not active    */

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  ret = (  (session->state == bgp_session_Enabled)
        || (session->state == bgp_session_Established) ) ;

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return ret ;
} ;

extern int
bgp_session_is_accepting(bgp_session session)
{
  int ret ;

  if (session == NULL)
    return 0 ;                  /* NULL session is implicitly not accepting */

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  ret = session->accept ;

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return ret ;
} ;

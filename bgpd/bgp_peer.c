/* BGP Peer Handling
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

#include "bgpd/bgp_peer.h"

#include <zebra.h>

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_peer_index.h"
#include "bgpd/bgpd.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_packet.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_dump.h"
#include "bgpd/bgp_open.h"
#include "bgpd/bgp_advertise.h"

#include "linklist.h"
#include "prefix.h"
#include "vty.h"
#include "sockunion.h"
#include "prefix.h"
#include "thread.h"
#include "log.h"
#include "stream.h"
#include "memory.h"
#include "plist.h"
#include "mqueue.h"
#include "workqueue.h"
#include "if.h"

#ifdef HAVE_SNMP
#include "bgpd/bgp_snmp.h"
#endif /* HAVE_SNMP */

/*==============================================================================
 * This is the high level management of BGP Peers and peering conversations.
 *
 * The BGP Engine looks after the opening, running and closing of BGP sessions.
 *
 * Here we look after...
 *
 *   * the peer state and the effects of changes in that state
 *
 *   * timers for advertisements, graceful restart, ...
 *
 * The naming of peers/sessions and bgp_session_index
 * --------------------------------------------------
 *
 * Each peer/session is known by its IP address (IPv4 or IPv6) -- see the
 * "neighbor" commands.
 *
 * No matter how many bgp instances there may be, only one peer/session can
 * exist with a given IP address.
 *
 * The bgp_peer_index maps IP addresses to the peer, and hence to the session
 * (if any exists and is active).
 *
 * [To support multi-instance BGP, might be nice to use the "update source"
 *  address as part of the name of a peer.  But that is another story.]
 *
 *------------------------------------------------------------------------------
 * The Peer State.
 *
 * The peer has a small number of states.  Where there is a session, its state
 * is a sub-state of the main peer state.
 *
 * The states of a peer are as follows:
 *
 *   1. bgp_peer_pIdle
 *
 *      All peers start in this state.
 *
 *      This is the case when:
 *
 *        a. peer is administratively down/disabled/deactivated
 *
 *        b. waiting for route flap or other such timer before reawakening.
 *
 *        c. waiting for previous session to be closed -- in BGP Engine
 *
 *        d. waiting for session to establish -- in BGP Engine
 *
 *      The session states relate to the above as follows:
 *
 *        sIdle, sDisabled or no session at all -- (a) or (b)
 *
 *        sEnabled -- (d)
 *
 *        sEstablished -- IMPOSSIBLE
 *
 *        sLimping -- (c)
 *
 *      Only when the peer is none of (a), (b) or (c) can a session be enabled
 *      and the session move to (d), sEnabled.  So changes either in the state
 *      of the peer or in the state of the session have to check whether it
 *      is time to:
 *
 *        1. enable a new session
 *
 *        2. disable a session which has yet to reach sEstablished, but is
 *           now no longer correctly configured, or the like.
 *
 *   2. bgp_peer_pEstablished
 *
 *      Reaches this state from pIdle when a session becomes established.
 *
 *      This can only be the case when the session is sEstablished.
 *
 *      If the session is stopped for any reason, issues a Disable request to
 *      the BGP Engine:
 *
 *         peer    -> pClearing
 *         session -> sLimping
 *
 *   3. bgp_peer_pClearing
 *
 *      Reaches this state from pEstablished *only*, as above.
 *
 *      In this state the session can only be:
 *
 *        sLimping    -- session disable has been sent to the BGP Engine.
 *        sDisabled   -- session has been disabled by the BGP Engine
 *
 *      Tidies up the peer, including clearing routes etc.  Once the peer is
 *      completely tidy:
 *
 *         peer    -> pIdle
 *
 *      On entry to pClearing the session will be sLimping, on exit it may be
 *      still sLimping, or have advanced to sDisabled.
 *
 *      NB: while in pClearing the peer's routes and RIBs are being processed.
 *          All other parts of the peer may be modified... but mindful of the
 *          "background" task which is yet to complete.
 *
 *   4. bgp_peer_pDeleted
 *
 *      This is an exotic state, reached only when a peer is being completely
 *      deleted.
 *
 *      This state may be reached from any of the above.
 *
 *      If there is an active session, it will be sLimping.  When advances to
 *      sDisabled it will be deleted.
 *
 *      The remaining tasks are to clear out routes, dismantle the peer
 *      structure and delete it.  While that is happening, the peer is in this
 *      state.
 */

/* prototypes */

static void bgp_session_has_established(bgp_session session);
static void bgp_session_has_stopped(bgp_session session);
static void bgp_session_has_disabled(bgp_session session);
static void bgp_uptime_reset (struct peer *peer);
static void bgp_peer_stop (struct peer *peer, bool nsf) ;
static void bgp_peer_reset_idle(struct peer *peer) ;
static void bgp_peer_down_notify(bgp_peer peer, peer_down_t why_down,
                                                      bgp_notify notification) ;
static void bgp_peer_shutdown(bgp_peer peer) ;
static bgp_notify bgp_peer_map_peer_down(peer_down_t why_down) ;
static peer_down_t bgp_peer_map_notification(bgp_notify notification) ;
static void bgp_peer_free (struct peer *peer) ;
static void bgp_peer_change_status (bgp_peer peer, bgp_peer_state_t new_state);
static void bgp_peer_timers_set (struct peer *peer) ;
static int bgp_routeadv_timer (struct thread *thread);
static int bgp_graceful_restart_timer_expire (struct thread *thread);
static void bgp_graceful_restart_timer_cancel (struct peer* peer) ;
static int bgp_graceful_stale_timer_expire (struct thread *thread);
static void bgp_graceful_stale_timer_cancel (struct peer* peer) ;


/*==============================================================================
 * Deal with change in session state -- mqueue_action function.
 *
 * Receives notifications from the BGP Engine a session event occurs.
 *
 * -- arg0  = session
 *    args  =  bgp_session_event_args
 */
void
bgp_session_do_event(mqueue_block mqb, mqb_flag_t flag)
{

  struct bgp_session_event_args * args = mqb_get_args(mqb) ;
  bgp_session session                  = mqb_get_arg0(mqb) ;

  if (flag == mqb_action)
    {
      /* Pull stuff into Routing Engine *private* fields in the session     */

      session->event = args->event ;      /* last event                     */
      bgp_notify_set(&session->notification, args->notification) ;
                                          /* if any sent/received           */
      session->err     = args->err ;      /* errno, if any                  */
      session->ordinal = args->ordinal ;  /* primary/secondary connection   */

      switch(args->event)
      {
        /* If now Established, then the BGP Engine has exchanged BGP Open
         * messages, and received the KeepAlive that acknowledges our Open.
         *
         * Ignore this, however, if the session is sLimping -- which can
         * happen when the session has been disabled, but it became established
         * before the BGP Engine had seen the disable message.
         */
        case bgp_session_eEstablished:
          if (session->state == bgp_session_sLimping)
            break ;

          bgp_session_has_established(session);
          break ;

        /* If now Disabled, then the BGP Engine is acknowledging the a
         * session disable, and the session is now disabled.
         *
         * BEWARE: this may be the last thing that happens to the session
         *         and/or the related peer -- which may be deleted inside
         *         bgp_session_has_disabled().
         */
        case bgp_session_eDisabled:
          bgp_session_has_disabled(session);
          break ;

        /* If now Stopped, then for some reason the BGP Engine has either
         * stopped trying to connect, or the session has been stopped.
         *
         * Again we ignore this in sLimping.
         */
        default:
          if (session->state == bgp_session_sLimping)
            break ;

          if (args->stopped)
            bgp_session_has_stopped(session) ;
          break ;
      } ;
    }
  else
    bgp_notify_free(args->notification) ;

  mqb_free(mqb) ;
}

/*------------------------------------------------------------------------------
 * BGP Session has been Established.
 */
static void
bgp_session_has_established(bgp_session session)
{
  afi_t afi;
  safi_t safi;
  int nsf_af_count ;

  bgp_peer peer  = session->peer ;
  assert(peer->session == session) ;            /* Safety first         */

  /* Session state change -- Routing Engine private fields              */
  assert(session->state == bgp_session_sEnabled) ;

  session->state = bgp_session_sEstablished ;
  session->flow_control = BGP_XON_REFRESH; /* updates can be sent */

  /* Peer state change.
   *
   * This stops all timers other than the Graceful Stale Timer.
   */
  bgp_peer_change_status (peer, bgp_peer_pEstablished);

  /* Extracting information from shared fields.                         */
  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  bgp_peer_open_state_receive(peer);

  sockunion_set_dup(&peer->su_local,  session->su_local) ;
  sockunion_set_dup(&peer->su_remote, session->su_remote) ;

  BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  /* Install next hop, as required.                                     */
  bgp_nexthop_set(peer->su_local, peer->su_remote, &peer->nexthop, peer) ;

  /* Reset capability open status flag. */
  if (! CHECK_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN))
    SET_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN);

  /* Clear last notification data -- Routing Engine private field       */
  bgp_notify_unset(&(peer->session->notification));

  /* Clear start timer value to default. */
  /* TODO: figure out where to increase the IdleHoldTimer               */
  peer->v_start = BGP_INIT_START_TIMER;

  /* Increment established count. */
  peer->established++;

  /* bgp log-neighbor-changes of neighbor Up */
  if (bgp_flag_check (peer->bgp, BGP_FLAG_LOG_NEIGHBOR_CHANGES))
    zlog_info ("%%ADJCHANGE: neighbor %s Up", peer->host);

  /* graceful restart                                                   */
  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT) ;

  nsf_af_count = 0 ;
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      {
        /* If the afi/safi has been negotiated, and have received Graceful
         * Restart capability, and is Restarting, and will Gracefully Restart
         * the afi/safi, then....
         */
        if (peer->afc_nego[afi][safi]
            && CHECK_FLAG (peer->cap, PEER_CAP_RESTART_ADV)
            && CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_RESTART_AF_RCV))
          {
            /* If have held onto routes for this afi/safi but forwarding has
             * not been preserved, then clean out the stale routes.
             *
             * Set NSF for this address family for next time.
             */
            if (peer->nsf[afi][safi]
                && ! CHECK_FLAG (peer->af_cap[afi][safi],
                                              PEER_CAP_RESTART_AF_PRESERVE_RCV))
              bgp_clear_stale_route (peer, afi, safi);

            peer->nsf[afi][safi] = 1;
            nsf_af_count++;
          }
        else
          {
            /* Remove stale routes, if any for this afi/safi            */
            if (peer->nsf[afi][safi])
              bgp_clear_stale_route (peer, afi, safi);
            peer->nsf[afi][safi] = 0;
          }
      }

  if (nsf_af_count)
    SET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE);
  else
    {
      UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE);
      bgp_graceful_stale_timer_cancel(peer) ;
    }

  /* Send route-refresh when ORF is enabled */
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_MAX ; safi++)
      if (CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_SM_ADV))
        {
          if (CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_RM_RCV))
            bgp_route_refresh_send (peer, afi, safi, ORF_TYPE_PREFIX,
                                    REFRESH_IMMEDIATE, 0);
          else if (CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_RM_OLD_RCV))
            bgp_route_refresh_send (peer, afi, safi, ORF_TYPE_PREFIX_OLD,
                                    REFRESH_IMMEDIATE, 0);
        }

  /* First update is deferred until ORF or ROUTE-REFRESH is received */
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_MAX ; safi++)
      if (CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_RM_ADV))
        if (CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_SM_RCV)
            || CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_SM_OLD_RCV))
          SET_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_ORF_WAIT_REFRESH);

  /* Reset uptime, send current table.                          */
  bgp_uptime_reset (peer);

  bgp_announce_route_all (peer);

  BGP_TIMER_ON (peer->t_routeadv, bgp_routeadv_timer, 1);

#ifdef HAVE_SNMP
  bgpTrapEstablished (peer);
#endif /* HAVE_SNMP */
}

/*------------------------------------------------------------------------------
 * BGP Session has stopped of its own accord -> disable the peer & session.
 *
 * The BGP Engine has signalled that it has stopped the session.  Response to
 * that is to tell it to disable the session, and then wait in sLimping state
 * until the BGP Engine completes the disable request and signals that.
 *
 * TODO: session stopped because we stopped it or because the other end did ?
 * TODO: restore NSF !!
 */
static void
bgp_session_has_stopped(bgp_session session)
{
  peer_down_t why_down ;

  bgp_peer peer  = session->peer ;
  assert(peer->session == session) ;            /* Safety first         */

  assert(bgp_session_is_active(session)) ;      /* "confused" if not    */

  if (session->state == bgp_session_sEstablished)
    {
      /* This code has been moved from where it was, in bgp_write       */
      /* TODO: not clear whether v_start handling is still correct      */
      peer->v_start *= 2;
      if (peer->v_start >= (60 * 2))
        peer->v_start = (60 * 2);
    } ;

  if (session->notification == NULL)
    why_down = PEER_DOWN_CLOSE_SESSION ;
  else
    {
      if (session->notification->received)
        why_down = PEER_DOWN_NOTIFY_RECEIVED ;
      else
        why_down = bgp_peer_map_notification(session->notification) ;
    } ;

  bgp_peer_down_notify(peer, why_down, session->notification) ;
} ;

/*------------------------------------------------------------------------------
 * BGP Session is now disabled -> can now move peer on to next state.
 *
 * The BGP Engine has closed the session in response to a disable request, and
 * no longer has an interest in it, and has signalled that.
 */
static void
bgp_session_has_disabled(bgp_session session)
{
  bgp_peer peer  = session->peer ;
  assert(peer->session == session) ;            /* Safety first         */

  assert(session->state == bgp_session_sLimping) ;

  session->state = bgp_session_sDisabled ;

  /* Immediately discard any other messages for this session.           */
  mqueue_revoke(routing_nexus->queue, session) ;

  /* If the session is marked "delete_me", do that.
   *
   * Otherwise, Old session now gone, so re-enable peer if now possible.
   */
  if (session->delete_me)
    bgp_session_delete(peer) ;  /* NB: this may also delete the peer.   */
  else
    bgp_peer_enable(peer);
} ;

/*------------------------------------------------------------------------------
 * Administrative BGP peer stop event -- stop pEstablished peer.
 *
 * MUST be pEstablished.
 *
 * Changes to pClearing and sets off to clear down all routes etc, subject to
 * the required NSF.
 *
 * On exit the peer will be:
 *
 *   - pIdle      if the clearing of routes etc completed immediately.
 *
 *   - pClearing  if further work for clearing of routes has been scheduled
 *                for later.
 *
 * NB: Leaves any Max Prefix Timer running.
 *
 *     Starts Graceful Restart and Stale Route timers iff NSF and at least one
 *     afi/safi is enabled for NSF.
 */
static void
bgp_peer_stop (struct peer *peer, bool nsf)
{
  bool cleared ;

  assert(peer->state == bgp_peer_pEstablished) ;

  /* Change state to pClearing.
   *
   * Turns off all timers.
   */
  bgp_peer_change_status(peer, bgp_peer_pClearing) ;

  peer->dropped++ ;
  peer->resettime = time (NULL) ;

  /* bgp log-neighbor-changes of neighbor Down                          */
  if (bgp_flag_check (peer->bgp, BGP_FLAG_LOG_NEIGHBOR_CHANGES))
    zlog_info ("%%ADJCHANGE: neighbor %s Down %s", peer->host,
               peer_down_str [(int) peer->last_reset]);

  /* Clear out routes, with NSF if required.
   *
   * Sets PEER_STATUS_NSF_WAIT iff NSF and at least one afi/safi is enabled
   * for NSF.  Clears PEER_STATUS_NSF_WAIT otherwise.
   */
  cleared = bgp_clear_all_routes (peer, nsf) ;

  /* graceful restart                                                   */
  if (CHECK_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT))
    {
      if (BGP_DEBUG (events, EVENTS))
        {
          zlog_debug ("%s graceful restart timer started for %d sec",
                      peer->host, peer->v_gr_restart);
          zlog_debug ("%s graceful restart stalepath timer started for %d sec",
                      peer->host, peer->bgp->stalepath_time);
        }
      BGP_TIMER_ON (peer->t_gr_restart, bgp_graceful_restart_timer_expire,
                    peer->v_gr_restart);
      BGP_TIMER_ON (peer->t_gr_stale, bgp_graceful_stale_timer_expire,
                    peer->bgp->stalepath_time);
    } ;

  /* Reset uptime. */
  bgp_uptime_reset (peer);

#ifdef HAVE_SNMP
  bgpTrapBackwardTransition (peer);
#endif /* HAVE_SNMP */

  /* Reset peer synctime */
  peer->synctime = 0;

  /* If completed the clearing of routes, then can now go pIdle         */
  if (cleared)
    bgp_peer_change_status(peer, bgp_peer_pIdle) ;
} ;

/*------------------------------------------------------------------------------
 * Clear out any stale routes, cancel any Graceful Restart timers.
 *
 * NB: may still be pClearing from when peer went down leaving these stale
 *     routes.
 *
 * NB: assumes clearing stale routes will complete immediately !
 */
static void
bgp_peer_clear_all_stale_routes (struct peer *peer)
{
  afi_t  afi;
  safi_t safi;

  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      if (peer->nsf[afi][safi])
        bgp_clear_stale_route (peer, afi, safi);

  bgp_graceful_restart_timer_cancel(peer) ;
  bgp_graceful_stale_timer_cancel(peer) ;

  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT);
} ;

/*------------------------------------------------------------------------------
 * If waiting for NSF peer to come back, stop now.
 *
 * When a session stops -- see bgp_peer_stop(), above -- the peer is set
 * PEER_STATUS_NSF_WAIT iff there are now stale routes in the table, waiting
 * for peer to come back.
 *
 * This function terminates that wait and clears out any stale routes, and
 * cancels any timers.
 *
 * Also clears down all NSF flags.
 *
 * If is PEER_STATUS_NSF_WAIT, MUST be pIdle or pClearing.
 *
 * NB: may still be pClearing from when peer went down leaving these stale
 *     routes.
 *
 * NB: assumes clearing stale routes will complete immediately !
 */
static void
bgp_peer_nsf_stop (struct peer *peer)
{
  afi_t  afi;
  safi_t safi;

  if (CHECK_FLAG(peer->sflags, PEER_STATUS_NSF_WAIT))
    {
      assert( (peer->state == bgp_peer_pIdle)
           || (peer->state == bgp_peer_pClearing) ) ;

      bgp_peer_clear_all_stale_routes (peer) ;
    } ;

  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE);

  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_MAX ; safi++)
      peer->nsf[afi][safi] = 0;
} ;

/*------------------------------------------------------------------------------
 * Set the PEER_FLAG_SHUTDOWN flag and also:
 *
 *   - turn off any NSF and related timers.
 *
 *   - turn off any Max Prefix overflow and related timers.
 */
static void
bgp_peer_shutdown(struct peer *peer)
{
  SET_FLAG (peer->flags, PEER_FLAG_SHUTDOWN) ;

  bgp_maximum_prefix_cancel_timer(peer) ;

  bgp_peer_nsf_stop (peer) ;
} ;

/*------------------------------------------------------------------------------
 * Reset state as peer goes to pIdle.
 *
 * This tidies things up, ready for session to be enabled again.
 *
 * NB: can be called any number of times... either to tidy up or to prepare
 *     for session to be enabled.
 */
static void
bgp_peer_reset_idle(struct peer *peer)
{
  afi_t  afi;
  safi_t safi;
  char orf_name[BUFSIZ];

  assert(peer->state == bgp_peer_pIdle) ;

  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE) ;

  peer->cap = 0 ;
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_MAX ; safi++)
      {
        /* Reset all negotiated variables */
        peer->afc_nego[afi][safi] = 0;
        peer->afc_adv[afi][safi]  = 0;
        peer->afc_recv[afi][safi] = 0;
        peer->nsf[afi][safi]      = 0;

        /* peer address family capability flags*/
        peer->af_cap[afi][safi] = 0;

        /* peer address family status flags*/
        peer->af_sflags[afi][safi] = 0;

        /* Received ORF prefix-filter */
        peer->orf_plist[afi][safi] = NULL;

        /* ORF received prefix-filter pnt */
        sprintf (orf_name, "%s.%d.%d", peer->host, afi, safi);
        prefix_bgp_orf_remove_all (orf_name);
      } ;

  /* Reset keepalive and holdtime */
  if (CHECK_FLAG (peer->config, PEER_CONFIG_TIMER))
    {
      peer->v_keepalive = peer->keepalive;
      peer->v_holdtime  = peer->holdtime;
    }
  else
    {
      peer->v_keepalive = peer->bgp->default_keepalive;
      peer->v_holdtime  = peer->bgp->default_holdtime;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Allocate new peer object, implicitly locked.
 */
struct peer *
bgp_peer_new (struct bgp *bgp)
{
  afi_t afi;
  safi_t safi;
  struct peer *peer;
  struct servent *sp;

  /* bgp argument is absolutely required                        */
  assert (bgp);
  bgp_lock (bgp);

  /* Allocate new peer.                                         */
  peer = XCALLOC (MTYPE_BGP_PEER, sizeof (struct peer));

  /* Set default value. */
  peer->v_start   = BGP_INIT_START_TIMER;
  peer->v_connect = BGP_DEFAULT_CONNECT_RETRY;
  peer->v_asorig  = BGP_DEFAULT_ASORIGINATE;
  peer->state     = bgp_peer_pIdle;
  peer->ostate    = bgp_peer_pIdle;
  peer->weight    = 0;
  peer->password  = NULL;
  peer->bgp       = bgp;

  peer = bgp_peer_lock (peer); /* initial reference */

  /* Set default flags.  */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      {
        if (! bgp_option_check (BGP_OPT_CONFIG_CISCO))
          {
            SET_FLAG (peer->af_flags[afi][safi], PEER_FLAG_SEND_COMMUNITY);
            SET_FLAG (peer->af_flags[afi][safi], PEER_FLAG_SEND_EXT_COMMUNITY);
          }
        peer->orf_plist[afi][safi] = NULL;
      }
  SET_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN);

  /* Create buffers.  */
  peer->ibuf = stream_new (BGP_MAX_PACKET_SIZE);
  peer->obuf = stream_fifo_new ();
  peer->work = stream_new (BGP_MAX_PACKET_SIZE);

  bgp_sync_init (peer);

  /* Get service port number.  */
  sp = getservbyname ("bgp", "tcp");
  peer->port = (sp == NULL) ? BGP_PORT_DEFAULT : ntohs (sp->s_port);

  return peer;
}

/*------------------------------------------------------------------------------
 * Create new BGP peer -- if an AFI/SAFI is given, activate & enable for that.
 *
 * Peer starts in pIdle state.
 *
 * This is creating a PEER_STATUS_REAL_PEER, which is placed on the bgp->peer
 * list.
 */
struct peer *
bgp_peer_create (union sockunion *su, struct bgp *bgp, as_t local_as,
                                        as_t remote_as, afi_t afi, safi_t safi)
{
  struct peer *peer;
  bool   enable = (afi != 0) && (safi != 0) ;

  /* TODO: should bgp_peer_new() set state pNull ??                     */
  peer = bgp_peer_new (bgp);    /* sets peer->state == pIdle            */

  peer->su          = *su;
  peer->local_as    = local_as;
  peer->as          = remote_as;
  peer->local_id    = bgp->router_id;
  peer->v_holdtime  = bgp->default_holdtime;
  peer->v_keepalive = bgp->default_keepalive;
  if (peer_sort (peer) == BGP_PEER_IBGP)
    peer->v_routeadv = BGP_DEFAULT_IBGP_ROUTEADV;
  else
    peer->v_routeadv = BGP_DEFAULT_EBGP_ROUTEADV;

  SET_FLAG(peer->sflags, PEER_STATUS_REAL_PEER) ;
  peer = bgp_peer_lock (peer);          /* bgp peer list reference */
  listnode_add_sort (bgp->peer, peer);

  /* If required, activate given AFI/SAFI -- eg "default ipv4-unicast"  */
  if (enable)
    peer->afc[afi][safi] = 1;

  /* Last read time set */
  peer->readtime = time (NULL);

  /* Last reset time set */
  peer->resettime = time (NULL);

  /* Default TTL set. */
  peer->ttl = (peer_sort (peer) == BGP_PEER_IBGP ? 255 : 1);

  /* Make peer's address string. */
  peer->host = sockunion_su2str (su, MTYPE_BGP_PEER_HOST) ;

  /* session -- NB: *before* peer is registered, so before any possible
   *                lookup up by accept() in the BGP Engine
   */
  bgp_session_init_new(peer);

  /* register -- NB: *after* peer->session set, so safe                 */
  bgp_peer_index_register(peer, &peer->su);

  /* Fire up any timers that should run during pIdle                    */
  bgp_peer_timers_set (peer);

  /* If require, enable now all is ready                                */
  if (enable)
    bgp_peer_enable(peer) ;

  return peer;
}

/*------------------------------------------------------------------------------
 * Delete peer from configuration.
 *
 * The peer is moved to a dead-end "Deleted" neighbour-state, to allow
 * it to "cool off" and refcounts to hit 0, at which state it is freed.
 *
 */
int
bgp_peer_delete (struct peer *peer)
{
  int i;
  afi_t afi;
  safi_t safi;
  struct bgp *bgp;
  struct bgp_filter *filter;

  bgp = peer->bgp ;

  /* Once peer is pDeleting it should be impossible to find in order to
   * bgp_peer_delete() it !
   */
  assert (peer->state != bgp_peer_pDeleting);

  /* If the peer is active, then need to shut it down now.  If there are any
   * stale routes, flush them now.
   *
   * If the peer ends up in pClearing state, that implies that some background
   * work is required to completely clear this peer.  So increment the
   * reference count to hold the peer in existence.  That will be decremented
   * again in bgp_peer_clearing_completed().
   *
   * If the peer ends up in sIdle state, that implies that any routes that
   * needed clearing have been dealt with.
   *
   * There may be a session in existence.  If so, it must either be sLimping or
   * sDisabled.
   *
   * Changing to pDeleting state turns off all timers.
   */
  bgp_peer_down(peer, PEER_DOWN_NEIGHBOR_DELETE) ;

  if (peer->state == bgp_peer_pClearing)
    bgp_peer_lock (peer) ;
  else
    assert(peer->state == bgp_peer_pIdle) ;

  bgp_peer_change_status (peer, bgp_peer_pDeleting);

  /* Increment count of peers lingering in pDeleting
   *
   * The count is used while terminating bgpd -- keeps all the nexuses running
   * until this count drops to zero.
   */
  bm->peer_linger_count++ ;

  /* If is an rsclient in its own right, remove from rsclient list      */
  if (peer_rsclient_active (peer))
    {
      struct listnode *pn;
      pn = listnode_lookup (bgp->rsclient, peer) ;

      bgp_peer_unlock (peer);   /* rsclient list reference */
      list_delete_node (bgp->rsclient, pn);
    } ;

  /* If this peer belongs to peer group, clear up the relationship.     */
  if (peer->group != NULL)
    {
      struct listnode *pn;
      pn = listnode_lookup (peer->group->peer, peer) ;

      assert(pn != NULL) ;

      bgp_peer_unlock (peer);   /* group->peer list reference   */
      list_delete_node (peer->group->peer, pn);

      peer->group = NULL;
    } ;

  /* Password configuration */
  if (peer->password)
    {
      XFREE (MTYPE_PEER_PASSWORD, peer->password);
      peer->password = NULL;
    }

  /* Delete from bgp->peer list, if required.                           */
  if (CHECK_FLAG (peer->sflags, PEER_STATUS_REAL_PEER))
    {
      struct listnode *pn;
      pn = listnode_lookup (bgp->peer, peer) ;

      assert(pn != NULL) ;

      bgp_peer_unlock (peer);   /* bgp peer list reference      */
      list_delete_node (bgp->peer, pn);
    } ;

  /* Discard rsclient ribs which are owned by group
   * and cross-check rib pointer and PEER_FLAG_RSERVER_CLIENT.
   */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      if (CHECK_FLAG(peer->af_flags[afi][safi], PEER_FLAG_RSERVER_CLIENT))
        {
          assert(peer->rib[afi][safi] != NULL) ;
          if (peer->af_group[afi][safi])
            {
              peer->rib[afi][safi] = NULL ;
              UNSET_FLAG(peer->af_flags[afi][safi], PEER_FLAG_RSERVER_CLIENT) ;
            } ;
        }
      else
        assert(peer->rib[afi][safi] == NULL) ;

  /* Can now clear any rsclient ribs.                                   */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      if (peer->rib[afi][safi] != NULL)
        {
          bgp_clear_rsclient_rib(peer, afi, safi) ;
          UNSET_FLAG(peer->af_flags[afi][safi], PEER_FLAG_RSERVER_CLIENT) ;
        } ;

  /* Have now finished with any rsclient ribs                           */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      bgp_table_finish (&peer->rib[afi][safi]) ;

  /* Buffers.  */
  if (peer->ibuf)
    stream_free (peer->ibuf);
  if (peer->obuf)
    stream_fifo_free (peer->obuf);
  if (peer->work)
    stream_free (peer->work);
  peer->obuf = NULL;
  peer->work = peer->ibuf = NULL;

  /* Local and remote addresses. */
  if (peer->su_local)
    sockunion_free (peer->su_local);
  if (peer->su_remote)
    sockunion_free (peer->su_remote);
  peer->su_local = peer->su_remote = NULL;

  /* Free filter related memory.  */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      {
        filter = &peer->filter[afi][safi];

        for (i = FILTER_IN; i < FILTER_MAX; i++)
          {
            if (filter->dlist[i].name)
              free (filter->dlist[i].name);
            if (filter->aslist[i].name)
              free (filter->aslist[i].name);
            filter->dlist[i].name = NULL;
            prefix_list_unset_ref(&filter->plist[i].ref) ;
            filter->aslist[i].name = NULL;
          }
        for (i = RMAP_IN; i < RMAP_MAX; i++)
          {
            if (filter->map[i].name)
              free (filter->map[i].name);
            filter->map[i].name = NULL;
          }

        if (filter->usmap.name)
          free (filter->usmap.name);

        if (peer->default_rmap[afi][safi].name)
          free (peer->default_rmap[afi][safi].name);

        filter->usmap.name = NULL;
        peer->default_rmap[afi][safi].name = NULL;
      }

  /* Unregister the peer.
   *
   * NB: the peer can no longer be looked up by its 'name'.
   *
   *     In particular this means that the accept() logic in the BGP Engine
   *     will conclude that the session should not be accepting connections.
   *
   * NB: also (currently) releases the peer_id -- which may not be so clever ?
   */
  if (peer->index_entry != NULL)
    bgp_peer_index_deregister(peer, &peer->su);

  /* Tear down session, if any and if possible.                         */
  bgp_session_delete(peer) ;

  /* Finally: count down the initial reference, which will delete the peer
   * iff everything else has finished with it.
   */
  bgp_peer_unlock (peer); /* initial reference */

  return 0;
} ;

/*------------------------------------------------------------------------------
 * increase reference count on a struct peer
 */
struct peer *
bgp_peer_lock (struct peer *peer)
{
  assert (peer && (peer->lock >= 0));

  peer->lock++;

  return peer;
}

/*------------------------------------------------------------------------------
 * decrease reference count on a struct peer
 *
 * If is last reference, the structure is freed and NULL returned
 */
struct peer *
bgp_peer_unlock (struct peer *peer)
{
  assert (peer && (peer->lock > 0));

  peer->lock--;

  if (peer->lock == 0)
    {
#if 0
      zlog_debug ("unlocked and freeing");
      zlog_backtrace (LOG_DEBUG);
#endif
      bgp_peer_free (peer);
      return NULL;
    }

#if 0
  if (peer->lock == 1)
    {
      zlog_debug ("unlocked to 1");
      zlog_backtrace (LOG_DEBUG);
    }
#endif

  return peer;
}

/*------------------------------------------------------------------------------
 * Dismantle and free peer data structure.
 */
static void
bgp_peer_free (struct peer *peer)
{
  struct bgp* bgp ;

  assert (peer->state   == bgp_peer_pDeleting);
  assert (peer->session == NULL) ;      /* session holds a lock on peer */

  bm->peer_linger_count-- ;

  bgp = peer->bgp ;

  if (peer->desc)
    XFREE (MTYPE_PEER_DESC, peer->desc);

  /* Free allocated host character. */
  if (peer->host)
    XFREE (MTYPE_BGP_PEER_HOST, peer->host);

  /* Update source configuration.  */
  if (peer->update_source)
    sockunion_free (peer->update_source);

  if (peer->update_if)
    XFREE (MTYPE_PEER_UPDATE_SOURCE, peer->update_if);

  bgp_sync_delete (peer);

  memset (peer, 0, sizeof (struct peer));
  peer->lock = -54321 ;
  XFREE (MTYPE_BGP_PEER, peer);

  bgp_unlock(bgp);
} ;

/*------------------------------------------------------------------------------
 * Enable Peer
 *
 * This means that something has changed, and session can be started, if no
 * session has already started.
 *
 * So does nothing unless in pIdle, and expects the session state to be:
 *
 *   - sIdle, sDisabled or no session at all -- session enabled if possible
 *
 *   - sEnabled -- session already enabled, so do nothing
 *
 *   - sEstablished -- IMPOSSIBLE
 *
 *   - sLimping -- cannot, yet, start a new session -- that will happen in due
 *                 course.
 *
 * This means that any change which requires the session to be taken down and
 * restarted needs to call bgp_peer_disable().
 *
 * The other peer states:
 *
 *   - pEstablished == sEstablished
 *
 *   - pClearing -- cannot restart the peer yet, that will happen in due course.
 *
 *   - pDeleted  -- Enable peer makes no sense...  asserts invalid.
 *
 * TODO: assert !pEstablished in bgp_peer_enable ?
 */
void
bgp_peer_enable(bgp_peer peer)
{
  switch (peer->state)
  {
    case bgp_peer_pIdle:

      /* Enable the session unless:
       * 1) session is active -- ie sEnabled/sLimping (see above)
       * 2) no address family is activated
       * 3) the peer has been shutdown
       * 4) is dealing with prefix overflow (waiting for timer)
       */

      if (bgp_session_is_active(peer->session))
        assert(peer->session->state != bgp_session_sEstablished) ;
      else
        {
          if (peer_active (peer)
              && !CHECK_FLAG (peer->flags,  PEER_FLAG_SHUTDOWN)
              && !CHECK_FLAG (peer->sflags, PEER_STATUS_PREFIX_OVERFLOW))
            {
              /* enable the session */
              zlog_err ("%s: enabling peer %s:", __func__, peer->host) ;

              bgp_peer_reset_idle(peer) ;       /* tidy up      */
              bgp_session_enable(peer);
            } ;
        } ;
      break ;

    case bgp_peer_pEstablished:
      break ;

    case bgp_peer_pClearing:
      break ;

    case bgp_peer_pDeleting:
      zabort("cannot enable a pDeleting peer") ;
      break ;

    default:
      zabort("unknown peer->state") ;
      break ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Down Peer -- bring down any existing session and restart if possible.
 *
 * The following "why_down" values are special:
 *
 *   - PEER_DOWN_NSF_CLOSE_SESSION
 *
 *     causes NSF to be turned on as the peer is stopped and routes are cleared.
 *
 *   - PEER_DOWN_USER_SHUTDOWN
 *
 *     causes the peer to be shutdown -- so won't restart.
 *
 *   - PEER_DOWN_NEIGHBOR_DELETE
 *
 *     prevents the peer from restarting.
 *
 * If there is an active session, then it must be disabled, sending the given
 * notification, or one based on the reason for downing the peer.
 *
 * If there is no active session, any stale NSF routes will be cleared.
 *
 * So any session ends up as:
 *
 *   sIdle     -- wasn't active and still isn't
 *
 *   sLimping  -- was sEnabled or sEstablished, we now wait for BGP Engine
 *                to complete the disable action and signal when done.
 *
 *   sDisabled -- has previously been disabled.
 *
 * The result depends on the initial peer state:
 *
 *   1. bgp_peer_pIdle
 *
 *      Any session that was sEnabled will have been disabled -- and will now
 *      be sLimping.
 *
 *      The peer will have automatically restarted, if possible.
 *
 *      Noting that PEER_DOWN_USER_SHUTDOWN and PEER_DOWN_NEIGHBOR_DELETE both
 *      prevent any restart.
 *
 *   2. bgp_peer_pEstablished
 *
 *      The session will have been disabled -- and will now be sLimping.
 *
 *      See bgp_peer_stop() for the state of the peer.
 *
 *   3. bgp_peer_pClearing
 *
 *      In this state the session can only be:
 *
 *        sLimping    -- session disable has been sent to the BGP Engine.
 *        sDisabled   -- session has been disabled by the BGP Engine
 *
 *      because peer must have been pEstablished immediately before.
 *
 *      Do nothing -- will proceed to pIdle in due course.
 *
 *   4. bgp_peer_pDeleting
 *
 *      In this state there may be no session at all, or the session can
 *      only be:
 *
 *        sIdle       -- session never got going
 *        sLimping    -- session disable has been sent to the BGP Engine.
 *        sDisabled   -- session has been disabled by the BGP Engine
 *
 *      Do nothing -- peer will be deleted in due course.
 */
void
bgp_peer_down(bgp_peer peer, peer_down_t why_down)
{
  bgp_peer_down_notify(peer, why_down, NULL) ;
} ;

static void
bgp_peer_down_notify(bgp_peer peer, peer_down_t why_down,
                                                        bgp_notify notification)
{
  /* Deal with session (if any).                                        */

  if (bgp_session_is_active(peer->session))
    {
      if (notification == NULL)
        notification = bgp_peer_map_peer_down(why_down) ;

      bgp_notify_set_dup(&peer->session->notification, notification) ;

      bgp_session_disable(peer, notification) ;
    }
  else
    bgp_notify_free(notification) ;     /* Discard unused notification  */

  /* Now worry about the state of the peer                              */

  if (why_down == PEER_DOWN_USER_SHUTDOWN)
    bgp_peer_shutdown(peer) ;

  if (why_down != PEER_DOWN_NULL)
    peer->last_reset = why_down ;

  switch (peer->state)
  {
    case bgp_peer_pIdle:
      assert(!bgp_session_is_active(peer->session)
                  || (peer->session->state == bgp_session_sLimping)) ;

      bgp_peer_nsf_stop (peer) ;        /* flush stale routes, if any   */

      if (why_down != PEER_DOWN_NEIGHBOR_DELETE)
        bgp_peer_enable(peer) ;         /* Restart if possible.         */

      break ;

    case bgp_peer_pEstablished:
      assert(peer->session->state == bgp_session_sLimping) ;

      bgp_peer_stop(peer, why_down == PEER_DOWN_NSF_CLOSE_SESSION) ;

      break ;

    case bgp_peer_pClearing:
      assert(   (peer->session->state == bgp_session_sLimping)
             || (peer->session->state == bgp_session_sDisabled) ) ;

      bgp_peer_nsf_stop (peer) ;        /* flush stale routes, if any   */

      break ;

    case bgp_peer_pDeleting:
      assert(   (peer->session == NULL)
             || (peer->session->state == bgp_session_sIdle)
             || (peer->session->state == bgp_session_sLimping)
             || (peer->session->state == bgp_session_sDisabled) ) ;
      break ;

    default:
      zabort("unknown peer->state") ;
      break ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Notify the far end that an error has been detected, and close down the
 * session.
 *
 * The session will have been established, so the IdleHoldTime will be extended.
 *
 * Because it knows no better, the session will be restarted.
 */
extern void
bgp_peer_down_error(struct peer* peer,
                                bgp_nom_code_t code, bgp_nom_subcode_t subcode)
{
  bgp_peer_down_error_with_data (peer, code, subcode, NULL, 0);
}

/*------------------------------------------------------------------------------
 * Notify the far end that an error has been detected, and close down the
 * session.
 *
 * Same as above, except that this accepts a data part for the notification
 * message.
 */
extern void
bgp_peer_down_error_with_data (struct peer* peer,
                               bgp_nom_code_t code, bgp_nom_subcode_t subcode,
                                           const u_int8_t* data, size_t datalen)
{
  bgp_notify notification;
  notification = bgp_notify_new_with_data(code, subcode, data, datalen);

  bgp_peer_down_notify(peer, bgp_peer_map_notification(notification),
                                                                 notification) ;
} ;

/*------------------------------------------------------------------------------
 * Construct notification based on the reason for bringing down the session
 *
 * Where the session is brought down by the other end, returns NULL.
 */
static bgp_notify
bgp_peer_map_peer_down(peer_down_t why_down)
{
  bgp_nom_code_t    code ;
  bgp_nom_subcode_t subcode ;

  assert((why_down >= PEER_DOWN_first) && (why_down < PEER_DOWN_count)) ;

  code    = BGP_NOMC_CEASE ;            /* Default values       */
  subcode = BGP_NOMS_UNSPECIFIC ;

  switch(why_down)
  {
    case PEER_DOWN_NULL:
      return NULL ;

    /* Session taken down at this end for some unspecified reason         */

    case PEER_DOWN_UNSPECIFIED:
      break ;

    /* Configuration changes that cause a session to be reset.            */

    case PEER_DOWN_CONFIG_CHANGE:
    case PEER_DOWN_RID_CHANGE:
    case PEER_DOWN_REMOTE_AS_CHANGE:
    case PEER_DOWN_LOCAL_AS_CHANGE:
    case PEER_DOWN_CLID_CHANGE:
    case PEER_DOWN_CONFED_ID_CHANGE:
    case PEER_DOWN_CONFED_PEER_CHANGE:
    case PEER_DOWN_RR_CLIENT_CHANGE:
    case PEER_DOWN_RS_CLIENT_CHANGE:
    case PEER_DOWN_UPDATE_SOURCE_CHANGE:
    case PEER_DOWN_AF_ACTIVATE:
    case PEER_DOWN_RMAP_BIND:
    case PEER_DOWN_RMAP_UNBIND:
    case PEER_DOWN_CAPABILITY_CHANGE:
    case PEER_DOWN_PASSIVE_CHANGE:
    case PEER_DOWN_MULTIHOP_CHANGE:
    case PEER_DOWN_AF_DEACTIVATE:
    case PEER_DOWN_PASSWORD_CHANGE:
    case PEER_DOWN_ALLOWAS_IN_CHANGE:
      subcode = BGP_NOMS_C_CONFIG ;
      break ;

    /* Other actions that cause a session to be reset                     */

    case PEER_DOWN_USER_SHUTDOWN:
      subcode = BGP_NOMS_C_SHUTDOWN ;
      break ;

    case PEER_DOWN_USER_RESET:
      subcode = BGP_NOMS_C_RESET ;
      break ;

    case PEER_DOWN_NEIGHBOR_DELETE:
      subcode = BGP_NOMS_C_DECONFIG ;
      break ;

    case PEER_DOWN_INTERFACE_DOWN:
      return NULL ;             /* nowhere to send a notification !     */

    /* Errors and problems that cause a session to be reset               */
    /*                                                                    */
    /* SHOULD really have a notification constructed for these, but for   */
    /* completeness construct an "unspecified" for these.                 */

    case PEER_DOWN_MAX_PREFIX:
      subcode = BGP_NOMS_C_MAX_PREF ;
      break ;

    case PEER_DOWN_HEADER_ERROR:
      code = BGP_NOMC_HEADER ;
      break ;

    case PEER_DOWN_OPEN_ERROR:
      code = BGP_NOMC_OPEN ;
      break ;

    case PEER_DOWN_UPDATE_ERROR:
      code = BGP_NOMC_UPDATE ;
      break ;

    case PEER_DOWN_HOLD_TIMER:
      code = BGP_NOMC_HOLD_EXP ;
      break ;

    case PEER_DOWN_FSM_ERROR:
      code = BGP_NOMC_FSM ;
      break ;

    case PEER_DOWN_DYN_CAP_ERROR:
      code = BGP_NOMC_DYN_CAP ;
      break ;

    /* Things the far end can do to cause a session to be reset           */

    case PEER_DOWN_NOTIFY_RECEIVED:
      return NULL ;             /* should not get here !                */

    case PEER_DOWN_CLOSE_SESSION:
    case PEER_DOWN_NSF_CLOSE_SESSION:
      return NULL ;             /* nowhere to send a notification !     */

    /* To keep the compiler happy.      */
    case PEER_DOWN_count:
      break ;                   /* should have asserted already         */
  } ;

  return bgp_notify_new(code, subcode) ;
} ;

/*------------------------------------------------------------------------------
 * Construct reason for bringing down the session based on the notification
 */
static peer_down_t
bgp_peer_map_notification(bgp_notify notification)
{
  switch (bgp_notify_get_code(notification))    /* deals with NULL      */
  {
    case BGP_NOMC_UNDEF:
      break ;

    case BGP_NOMC_HEADER:
      return PEER_DOWN_HEADER_ERROR ;

    case BGP_NOMC_OPEN:
      return PEER_DOWN_OPEN_ERROR ;

    case BGP_NOMC_UPDATE:
      return PEER_DOWN_UPDATE_ERROR ;

    case BGP_NOMC_HOLD_EXP:
      return PEER_DOWN_HOLD_TIMER ;

    case BGP_NOMC_FSM:
      return PEER_DOWN_FSM_ERROR ;

    case BGP_NOMC_CEASE:
      switch (notification->subcode)
      {
        case BGP_NOMS_C_MAX_PREF:
          return PEER_DOWN_MAX_PREFIX ;

        case BGP_NOMS_C_SHUTDOWN:
          return PEER_DOWN_USER_SHUTDOWN ;

        case BGP_NOMS_C_DECONFIG:
          return PEER_DOWN_NEIGHBOR_DELETE ;

        case BGP_NOMS_C_RESET:
          return PEER_DOWN_USER_RESET ;

        case BGP_NOMS_C_REJECTED:       /* should not get here  */
          return PEER_DOWN_NULL ;

        case BGP_NOMS_C_CONFIG:
          return PEER_DOWN_CONFIG_CHANGE ;

        case BGP_NOMS_C_COLLISION:      /* should not get here  */
          return PEER_DOWN_NULL ;

        case BGP_NOMS_C_RESOURCES:      /* not used             */
          return PEER_DOWN_NULL ;

        default:
          break ;
      } ;
      break ;

    case BGP_NOMC_DYN_CAP:
      return PEER_DOWN_DYN_CAP_ERROR ;

    default:
      break ;
  } ;

  return PEER_DOWN_UNSPECIFIED ;
} ;

/*------------------------------------------------------------------------------
 * Background route clearing has completed.
 *
 * If there is any background work required to clear routes for a given peer,
 * then when that completes, this is called to move the state of the peer
 * forwards.
 */
extern void
bgp_peer_clearing_completed(struct peer *peer)
{
  switch (peer->state)
  {
    case bgp_peer_pIdle:
    case bgp_peer_pEstablished:
      zabort("invalid peer->state") ;
      break ;

    case bgp_peer_pClearing:
      bgp_peer_change_status (peer, bgp_peer_pIdle);
      bgp_peer_enable(peer);
      break ;

    case bgp_peer_pDeleting:
      bgp_peer_unlock (peer);       /* route clearing "reference"   */
      break ;

    default:
      zabort("unknown peer->state") ;
      break ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Set new peer state.
 *
 * If state changes, do dump new state and log state change if required.
 *
 * In any case, set timers for the new state -- so if state hasn't changed,
 * will restart those timers.
 */
static void
bgp_peer_change_status (bgp_peer peer, bgp_peer_state_t new_state)
{
  if (peer->state != new_state)
    {
      bgp_dump_state (peer, peer->state, new_state);

      /* Preserve old status and change into new status. */
      peer->ostate = peer->state ;
      peer->state  = new_state ;

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s went from %s to %s", peer->host,
                    LOOKUP (bgp_peer_status_msg, peer->ostate),
                    LOOKUP (bgp_peer_status_msg, peer->state)) ;

      if (new_state == bgp_peer_pIdle)
        bgp_peer_reset_idle(peer) ;       /* tidy up      */
    } ;

  bgp_peer_timers_set (peer);
} ;

/*==============================================================================
 * Timer handling
 */

static void
bgp_peer_timers_set (struct peer *peer)
{
  switch (peer->state)
  {
    case bgp_peer_pIdle:
      /* On entry to pIdle the Graceful Restart Timers are left running:
       *
       *   - if no connection is established within the Graceful Restart time,
       *     then things are no longer graceful, and the stale routes have to
       *     be thrown away.
       *
       *   - if routes do not thereafter arrive quickly enough, then the
       *     Graceful Stale time kicks in and stale routes will be thrown away.
       */
      BGP_TIMER_OFF (peer->t_routeadv) ;
      BGP_TIMER_OFF (peer->t_pmax_restart) ;
      break;

    case bgp_peer_pEstablished:
      /* On entry to pEstablished only the the Graceful Stale Timer is left
       * running.
       *
       * Any Graceful Restart Timer can be cancelled -- have established in
       * time.
       */
      BGP_TIMER_OFF (peer->t_routeadv) ;
      BGP_TIMER_OFF (peer->t_pmax_restart) ;

      bgp_graceful_restart_timer_cancel(peer) ;
      break;

    case bgp_peer_pClearing:
      /* On entry to pClearing, turn off all timers.
       *
       * The Graceful Restart timer should not be running in any case.
       *
       * If the session is brought down quickly enough, the Graceful Stale
       * timer may be running.
       */
      BGP_TIMER_OFF (peer->t_routeadv);
      BGP_TIMER_OFF (peer->t_pmax_restart);
      BGP_TIMER_OFF (peer->t_gr_restart);

      bgp_graceful_stale_timer_cancel(peer) ;
      break ;

    case bgp_peer_pDeleting:
      /* On entry to pDeleting, turn off all timers.
       */
      BGP_TIMER_OFF (peer->t_routeadv);
      BGP_TIMER_OFF (peer->t_pmax_restart);
      BGP_TIMER_OFF (peer->t_gr_restart);
      BGP_TIMER_OFF (peer->t_gr_stale);
      break;

    default:
      assert(0);
  } ;
} ;

static int
bgp_routeadv_timer (struct thread *thread)
{
  struct   peer *peer;
  uint32_t jittered ;
  uint32_t jitter ;

  peer = THREAD_ARG (thread);
  peer->t_routeadv = NULL;

  if (BGP_DEBUG (fsm, FSM))
    zlog (peer->log, LOG_DEBUG,
          "%s [FSM] Timer (routeadv timer expire)",
          peer->host);

  peer->synctime = time (NULL);

  bgp_write(peer, NULL);

  /* Apply +/- 10% jitter to the route advertise timer.
   *
   * The time is in seconds, so for anything less than 10 seconds this forced
   * to be +/- 1 second.
   */
  jittered = jitter = peer->v_routeadv ;
  if (jitter < 10)
    jitter = 10 ;
  jittered = (jittered * 90) + (rand() % (jitter * 20)) ; /* jitter is +/-10% */
  jittered = (jittered + 50) / 100 ;

  /* TODO: move this to the Routeing Engine qtimer pile.                */
  BGP_TIMER_ON (peer->t_routeadv, bgp_routeadv_timer, jittered) ;

  return 0;
}

/* Reset bgp update timer */
static void
bgp_uptime_reset (struct peer *peer)
{
  peer->uptime = time (NULL);
}

/*------------------------------------------------------------------------------
 * BGP Peer Down Causes mapped to strings
 */
const char *peer_down_str[] =
{
  [PEER_DOWN_NULL]                 = "",

  [PEER_DOWN_UNSPECIFIED]          = "Unspecified reason",

  [PEER_DOWN_CONFIG_CHANGE]        = "Unspecified config change",

  [PEER_DOWN_RID_CHANGE]           = "Router ID changed",
  [PEER_DOWN_REMOTE_AS_CHANGE]     = "Remote AS changed",
  [PEER_DOWN_LOCAL_AS_CHANGE]      = "Local AS change",
  [PEER_DOWN_CLID_CHANGE]          = "Cluster ID changed",
  [PEER_DOWN_CONFED_ID_CHANGE]     = "Confederation identifier changed",
  [PEER_DOWN_CONFED_PEER_CHANGE]   = "Confederation peer changed",
  [PEER_DOWN_RR_CLIENT_CHANGE]     = "RR client config change",
  [PEER_DOWN_RS_CLIENT_CHANGE]     = "RS client config change",
  [PEER_DOWN_UPDATE_SOURCE_CHANGE] = "Update source change",
  [PEER_DOWN_AF_ACTIVATE]          = "Address family activated",
  [PEER_DOWN_RMAP_BIND]            = "Peer-group add member",
  [PEER_DOWN_RMAP_UNBIND]          = "Peer-group delete member",
  [PEER_DOWN_CAPABILITY_CHANGE]    = "Capability changed",
  [PEER_DOWN_PASSIVE_CHANGE]       = "Passive config change",
  [PEER_DOWN_MULTIHOP_CHANGE]      = "Multihop config change",
  [PEER_DOWN_AF_DEACTIVATE]        = "Address family deactivated",
  [PEER_DOWN_PASSWORD_CHANGE]      = "MD5 Password changed",
  [PEER_DOWN_ALLOWAS_IN_CHANGE]    = "Allow AS in changed",

  [PEER_DOWN_USER_SHUTDOWN]        = "Admin. shutdown",
  [PEER_DOWN_USER_RESET]           = "User reset",
  [PEER_DOWN_NEIGHBOR_DELETE]      = "Neighbor deleted",

  [PEER_DOWN_INTERFACE_DOWN]       = "Interface down",

  [PEER_DOWN_MAX_PREFIX]           = "Max Prefix Limit exceeded",

  [PEER_DOWN_HEADER_ERROR]         = "Error in message header",
  [PEER_DOWN_OPEN_ERROR]           = "Error in BGP OPEN message",
  [PEER_DOWN_UPDATE_ERROR]         = "Error in BGP UPDATE message",
  [PEER_DOWN_HOLD_TIMER]           = "HoldTimer expired",
  [PEER_DOWN_FSM_ERROR]            = "Error in FSM sequence",
  [PEER_DOWN_DYN_CAP_ERROR]        = "Error in Dynamic Capability",

  [PEER_DOWN_NOTIFY_RECEIVED]      = "Notification received",
  [PEER_DOWN_NSF_CLOSE_SESSION]    = "NSF peer closed the session",
  [PEER_DOWN_CLOSE_SESSION]        = "Peer closed the session",
} ;

CONFIRM(sizeof(peer_down_str) == (PEER_DOWN_count * sizeof(const char*))) ;

/*------------------------------------------------------------------------------
 * Graceful Restart timer has expired.
 *
 * MUST be pIdle or pClearing -- transition to pEstablished cancels this timer.
 *
 * Clears out stale routes and stops the Graceful Restart Stale timer.
 *
 * Clears down PEER_STATUS_NSF_MODE & PEER_STATUS_NSF_WAIT.
 */
static int
bgp_graceful_restart_timer_expire (struct thread *thread)
{
  struct peer *peer;

  peer = THREAD_ARG (thread);
  peer->t_gr_restart = NULL;

  if (BGP_DEBUG (events, EVENTS))
    zlog_debug ("%s graceful restart timer expired", peer->host) ;

  bgp_peer_nsf_stop (peer) ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Cancel any Graceful Restart timer
 *
 * NB: does NOT do anything about any stale routes or about any stale timer !
 */
static void
bgp_graceful_restart_timer_cancel (struct peer* peer)
{
  if (peer->t_gr_restart)
    {
      BGP_TIMER_OFF (peer->t_gr_restart);
      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s graceful restart timer stopped", peer->host);
    }
} ;

/*------------------------------------------------------------------------------
 * Graceful Restart Stale timer has expired.
 *
 * SHOULD be pEstablished, because otherwise the Graceful Restart timer should
 * have gone off before this does, and cancelled this.
 *
 * To be safe, if not pEstablished, then MUST be pIdle or pClearing, so can do
 * bgp_peer_nsf_stop (peer).
 *
 * Clears out stale routes and stops the Graceful Restart Stale timer.
 *
 * Clears down PEER_STATUS_NSF_MODE & PEER_STATUS_NSF_WAIT.
 */
static int
bgp_graceful_stale_timer_expire (struct thread *thread)
{
  struct peer *peer;

  peer = THREAD_ARG (thread);
  peer->t_gr_stale = NULL;

  if (BGP_DEBUG (events, EVENTS))
    zlog_debug ("%s graceful restart stalepath timer expired", peer->host);

  if (peer->state == bgp_peer_pEstablished)
    bgp_peer_clear_all_stale_routes(peer) ;
  else
    bgp_peer_nsf_stop(peer) ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Cancel any Graceful Restart Stale timer
 *
 * NB: does NOT do anything about any stale routes !
 */
static void
bgp_graceful_stale_timer_cancel (struct peer* peer)
{
  if (peer->t_gr_stale)
    {
      BGP_TIMER_OFF (peer->t_gr_stale);
      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s graceful restart stalepath timer stopped", peer->host);
    }
} ;

#if 0
/* BGP peer is stopped by the error. */
static int
bgp_stop_with_error (struct peer *peer)
{
  /* Double start timer. */
  peer->v_start *= 2;

  /* Overflow check. */
  if (peer->v_start >= (60 * 2))
    peer->v_start = (60 * 2);

  bgp_stop (peer);

  return 0;
}
#endif


/*==============================================================================
 * For the given interface name, get a suitable address so can bind() before
 * connect() so that we use the required interface.
 *
 * If has a choice, uses address that best matches the peer's address.
 */
extern sockunion
bgp_peer_get_ifaddress(bgp_peer peer, const char* ifname, pAF_t paf)
{
  struct interface* ifp ;
  struct connected* connected;
  struct listnode*  node;
  struct prefix*    best_prefix ;
  struct prefix*    peer_prefix ;
  int   best, this ;

  if (ifname == NULL)
    return NULL ;

  ifp = if_lookup_by_name (peer->update_if) ;
  if (ifp == NULL)
    {
      zlog_err("Peer %s interface %s is not known", peer->host, ifname) ;
      return NULL ;
    } ;

  peer_prefix = sockunion2hostprefix(&peer->su) ;
  best_prefix = NULL ;
  best = -1 ;

  for (ALL_LIST_ELEMENTS_RO (ifp->connected, node, connected))
    {
      if (connected->address->family != paf)
        continue ;

      this = prefix_common_bits (connected->address, peer_prefix) ;
      if (this > best)
        {
          best_prefix = connected->address ;
          best = this ;
        } ;
    } ;

  prefix_free(peer_prefix) ;

  if (best_prefix != NULL)
    return sockunion_new_prefix(NULL, best_prefix) ;

  zlog_err("Peer %s interface %s has no suitable address", peer->host, ifname);

  return NULL ;
} ;

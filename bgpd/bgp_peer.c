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

/* prototypes */

static int bgp_session_has_established(bgp_peer peer);
static int bgp_session_has_stopped(bgp_peer peer);
static int bgp_session_has_disabled(bgp_peer peer);
static void bgp_uptime_reset (struct peer *peer);
static int bgp_routeadv_timer (struct thread *thread);
static int bgp_graceful_restart_timer_expire (struct thread *thread);
static int bgp_graceful_stale_timer_expire (struct thread *thread);

/*==============================================================================
 * This is the high level management of BGP Peers and peering conversations.
 *
 * The BGP Engine looks after the opening, running and closing of BGP sessions.
 *
 * Here we look after...
 *
 *   * the peer state and the effects of changes in that state
 *   *
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
 */

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
  bgp_session session = mqb_get_arg0(mqb) ;
  bgp_peer peer = session->peer ;

  if (flag == mqb_action)
    {
      BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

      session->event = args->event ;      /* last event                     */
      bgp_notify_set(&session->notification, args->notification) ;
                                          /* if any sent/received           */
      session->err   = args->err ;        /* errno, if any                  */
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

          bgp_session_has_established(peer);
          break ;

        /* If now Disabled, then the BGP Engine is acknowledging the a
         * session disable, and the session is now disabled.
         */
        case bgp_session_eDisabled:
          bgp_session_has_disabled(peer);
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
            bgp_session_has_stopped(peer);
          break ;
      } ;

      BGP_SESSION_UNLOCK(session) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
    }
  else
    bgp_notify_free(args->notification) ;

  mqb_free(mqb) ;
}

/* BGP Session has been Established.
 *
 * NB: holding the session structure mutex.
 *
 * Send keepalive packet then make first update information.
 */
static int
bgp_session_has_established(bgp_peer peer)
{
  afi_t afi;
  safi_t safi;
  int nsf_af_count = 0;

  bgp_session session = peer->session ;

  assert(session->state == bgp_session_sEnabled) ;

  session->state = bgp_session_sEstablished ;
  peer_change_status (peer, bgp_peer_sEstablished);

  /* update peer state from received open */
  bgp_peer_open_state_receive(peer);

  /* get the local and remote addresses, and set the nexthop.   */

  sockunion_set_dup(&peer->su_local,  session->su_local) ;
  sockunion_set_dup(&peer->su_remote, session->su_remote) ;
  bgp_nexthop_set(peer->su_local, peer->su_remote, &peer->nexthop, peer) ;

  /* Reset capability open status flag. */
  if (! CHECK_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN))
    SET_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN);

  /* Clear last notification data. */
  bgp_notify_unset(&(peer->session->notification));

  /* Clear start timer value to default. */
  peer->v_start = BGP_INIT_START_TIMER;

  /* Increment established count. */
  peer->established++;

  /* bgp log-neighbor-changes of neighbor Up */
  if (bgp_flag_check (peer->bgp, BGP_FLAG_LOG_NEIGHBOR_CHANGES))
    zlog_info ("%%ADJCHANGE: neighbor %s Up", peer->host);

  /* graceful restart */
  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT);
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      {
        if (peer->afc_nego[afi][safi]
            && CHECK_FLAG (peer->cap, PEER_CAP_RESTART_ADV)
            && CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_RESTART_AF_RCV))
          {
            if (peer->nsf[afi][safi]
                && ! CHECK_FLAG (peer->af_cap[afi][safi], PEER_CAP_RESTART_AF_PRESERVE_RCV))
              bgp_clear_stale_route (peer, afi, safi);

            peer->nsf[afi][safi] = 1;
            nsf_af_count++;
          }
        else
          {
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
      if (peer->t_gr_stale)
        {
          BGP_TIMER_OFF (peer->t_gr_stale);
          if (BGP_DEBUG (events, EVENTS))
            zlog_debug ("%s graceful restart stalepath timer stopped", peer->host);
        }
    }

  if (peer->t_gr_restart)
    {
      BGP_TIMER_OFF (peer->t_gr_restart);
      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s graceful restart timer stopped", peer->host);
    }

#ifdef HAVE_SNMP
  bgpTrapEstablished (peer);
#endif /* HAVE_SNMP */

  /* Reset uptime, send keepalive, send current table. */
  bgp_uptime_reset (peer);

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

  bgp_announce_route_all (peer);

  BGP_TIMER_ON (peer->t_routeadv, bgp_routeadv_timer, 1);

  return 0;
}

/*------------------------------------------------------------------------------
 * State change to sLimping, session mutex locked
 *
 * The BGP Engine has signalled that it has stopped the session.  Response to
 * that is to tell it to disable the session, and then wait in sLimping state
 * until the BGP Engine acknowledges the disable request.
 */
static int
bgp_session_has_stopped(bgp_peer peer)
{
  bgp_session session = peer->session ;

  assert(bgp_session_is_active(session)) ;
  bgp_peer_disable(peer, NULL);
  /* TODO: needs to deal with NOTIFICATION, if any ??                   */

  return 0;
}

/*------------------------------------------------------------------------------
 * State change to sDisabled, session mutex locked
 *
 * The BGP Engine has acknowledged the disable request.
 */
static int
bgp_session_has_disabled(bgp_peer peer)
{
  bgp_session session = peer->session;

  assert(session->state == bgp_session_sLimping) ;

  session->state = bgp_session_sDisabled ;

  /* Immediately discard any other messages for this session.       */
  mqueue_revoke(routing_nexus->queue, session) ;

  /* does the peer need to be re-enabled? */
  if (session->defer_enable || peer->state == bgp_peer_sIdle)
    {
      session->defer_enable = 0;
      bgp_peer_enable(peer);
    }
  else if (peer->state == bgp_peer_sEstablished)
    {
      /* disable the peer */
      bgp_peer_stop(peer);
      bgp_clear_route_all(peer);
      peer_change_status(peer, bgp_peer_sClearing);
    }

  /* if the program is terminating then see if this was the last session
   * and if so ... die ....
   */
  program_terminate_if_all_disabled();
  return 0;
}

/* Administrative BGP peer stop event. */
/* May be called multiple times for the same peer */
int
bgp_peer_stop (struct peer *peer)
{
  afi_t afi;
  safi_t safi;
  char orf_name[BUFSIZ];

  /* Can't do this in Clearing; events are used for state transitions */
  if (peer->state != bgp_peer_sClearing)
    {
      /* Delete all existing events of the peer */
      BGP_EVENT_FLUSH (peer);
    }

  /* Increment Dropped count. */
  if (peer->state == bgp_peer_sEstablished)
    {
      peer->dropped++;

      /* bgp log-neighbor-changes of neighbor Down */
      if (bgp_flag_check (peer->bgp, BGP_FLAG_LOG_NEIGHBOR_CHANGES))
        zlog_info ("%%ADJCHANGE: neighbor %s Down %s", peer->host,
                   peer_down_str [(int) peer->last_reset]);

      /* graceful restart */
      if (peer->t_gr_stale)
        {
          BGP_TIMER_OFF (peer->t_gr_stale);
          if (BGP_DEBUG (events, EVENTS))
            zlog_debug ("%s graceful restart stalepath timer stopped", peer->host);
        }
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
        }
      else
        {
          UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE);

          for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
            for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
              peer->nsf[afi][safi] = 0;
        }

      /* set last reset time */
      peer->resettime = time (NULL);
      /* Reset uptime. */
      bgp_uptime_reset (peer);

#ifdef HAVE_SNMP
      bgpTrapBackwardTransition (peer);
#endif /* HAVE_SNMP */

      /* Reset uptime. */
      bgp_uptime_reset (peer);

      /* Reset peer synctime */
      peer->synctime = 0;
    }

  /* Stop all timers. */
  BGP_TIMER_OFF (peer->t_asorig);
  BGP_TIMER_OFF (peer->t_routeadv);

  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_MAX ; safi++)
      {
        /* Reset all negotiated variables */
        peer->afc_nego[afi][safi] = 0;
        peer->afc_adv[afi][safi] = 0;
        peer->afc_recv[afi][safi] = 0;

        /* peer address family capability flags*/
        peer->af_cap[afi][safi] = 0;

        /* peer address family status flags*/
        peer->af_sflags[afi][safi] = 0;

        /* Received ORF prefix-filter */
        peer->orf_plist[afi][safi] = NULL;

        /* ORF received prefix-filter pnt */
        sprintf (orf_name, "%s.%d.%d", peer->host, afi, safi);
        prefix_bgp_orf_remove_all (orf_name);
      }

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
    }

#if 0
  /* Reset prefix count */
  peer->pcount[AFI_IP][SAFI_UNICAST] = 0;
  peer->pcount[AFI_IP][SAFI_MULTICAST] = 0;
  peer->pcount[AFI_IP][SAFI_MPLS_VPN] = 0;
  peer->pcount[AFI_IP6][SAFI_UNICAST] = 0;
  peer->pcount[AFI_IP6][SAFI_MULTICAST] = 0;
#endif

  return 0;
}

#if 0
/* Stop all timers for the given peer
 */
static void
bgp_peer_timers_stop(bgp_peer peer)
{
  BGP_TIMER_OFF(peer->t_asorig) ;
  BGP_TIMER_OFF(peer->t_routeadv) ;

  BGP_TIMER_OFF (peer->t_gr_restart);
  BGP_TIMER_OFF (peer->t_gr_stale);
  BGP_TIMER_OFF (peer->t_pmax_restart);
} ;
#endif

static void
bgp_timer_set (struct peer *peer)
{
  switch (peer->state)
    {
    case bgp_peer_sIdle:
      /* First entry point of peer's finite state machine.  In Idle
	 status start timer is on unless peer is shutdown or peer is
	 inactive.  All other timer must be turned off */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;

#if 0
    case Connect:
      /* After start timer is expired, the peer moves to Connnect
         status.  Make sure start timer is off and connect timer is
         on. */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;

    case Active:
      /* Active is waiting connection from remote peer.  And if
         connect timer is expired, change status to Connect. */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;

    case OpenSent:
      /* OpenSent status. */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;

    case OpenConfirm:
      /* OpenConfirm status. */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;
#endif

    case bgp_peer_sEstablished:
      /* In Established status start and connect timer is turned
         off. */
     BGP_TIMER_OFF (peer->t_asorig);
      break;
    case bgp_peer_sDeleted:
      BGP_TIMER_OFF (peer->t_gr_restart);
      BGP_TIMER_OFF (peer->t_gr_stale);
      BGP_TIMER_OFF (peer->t_pmax_restart);
    case bgp_peer_sClearing:
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;
    default:
      assert(0);
    }
}

static int
bgp_routeadv_timer (struct thread *thread)
{
  struct peer *peer;

  peer = THREAD_ARG (thread);
  peer->t_routeadv = NULL;

  if (BGP_DEBUG (fsm, FSM))
    zlog (peer->log, LOG_DEBUG,
	  "%s [FSM] Timer (routeadv timer expire)",
	  peer->host);

  peer->synctime = time (NULL);

  bgp_write(peer);

  BGP_TIMER_ON (peer->t_routeadv, bgp_routeadv_timer,
		peer->v_routeadv);

  return 0;
}

/* Reset bgp update timer */
static void
bgp_uptime_reset (struct peer *peer)
{
  peer->uptime = time (NULL);
}

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

static int
bgp_graceful_restart_timer_expire (struct thread *thread)
{
  struct peer *peer;
  afi_t afi;
  safi_t safi;

  peer = THREAD_ARG (thread);
  peer->t_gr_restart = NULL;

  /* NSF delete stale route */
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      if (peer->nsf[afi][safi])
	bgp_clear_stale_route (peer, afi, safi);

  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT);
  BGP_TIMER_OFF (peer->t_gr_stale);

  if (BGP_DEBUG (events, EVENTS))
    {
      zlog_debug ("%s graceful restart timer expired", peer->host);
      zlog_debug ("%s graceful restart stalepath timer stopped", peer->host);
    }

  bgp_timer_set (peer);

  return 0;
}

static int
bgp_graceful_stale_timer_expire (struct thread *thread)
{
  struct peer *peer;
  afi_t afi;
  safi_t safi;

  peer = THREAD_ARG (thread);
  peer->t_gr_stale = NULL;

  if (BGP_DEBUG (events, EVENTS))
    zlog_debug ("%s graceful restart stalepath timer expired", peer->host);

  /* NSF delete stale route */
  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      if (peer->nsf[afi][safi])
	bgp_clear_stale_route (peer, afi, safi);

  return 0;
}

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

/* Allocate new peer object, implicitly locked.  */
struct peer *
peer_new (struct bgp *bgp)
{
  afi_t afi;
  safi_t safi;
  struct peer *peer;
  struct servent *sp;

  /* bgp argument is absolutely required */
  assert (bgp);
  if (!bgp)
    return NULL;

  /* Allocate new peer. */
  peer = XCALLOC (MTYPE_BGP_PEER, sizeof (struct peer));

  /* Set default value. */
  peer->v_start = BGP_INIT_START_TIMER;
  peer->v_connect = BGP_DEFAULT_CONNECT_RETRY;
  peer->v_asorig = BGP_DEFAULT_ASORIGINATE;
  peer->state = bgp_peer_sIdle;
  peer->ostate = bgp_peer_sIdle;
  peer->weight = 0;
  peer->password = NULL;
  peer->bgp = bgp;
  peer = peer_lock (peer); /* initial reference */
  bgp_lock (bgp);

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

/* Create new BGP peer.  */
struct peer *
peer_create (union sockunion *su, struct bgp *bgp, as_t local_as,
             as_t remote_as, afi_t afi, safi_t safi)
{
  int active;
  struct peer *peer;
  char buf[SU_ADDRSTRLEN];

  peer = peer_new (bgp);
  peer->su = *su;
  peer->local_as = local_as;
  peer->as = remote_as;
  peer->local_id = bgp->router_id;
  peer->v_holdtime = bgp->default_holdtime;
  peer->v_keepalive = bgp->default_keepalive;
  if (peer_sort (peer) == BGP_PEER_IBGP)
    peer->v_routeadv = BGP_DEFAULT_IBGP_ROUTEADV;
  else
    peer->v_routeadv = BGP_DEFAULT_EBGP_ROUTEADV;

  peer = peer_lock (peer); /* bgp peer list reference */
  listnode_add_sort (bgp->peer, peer);

  active = peer_active (peer);

  if (afi && safi)
    peer->afc[afi][safi] = 1;

  /* Last read time set */
  peer->readtime = time (NULL);

  /* Last reset time set */
  peer->resettime = time (NULL);

  /* Default TTL set. */
  peer->ttl = (peer_sort (peer) == BGP_PEER_IBGP ? 255 : 1);

  /* Make peer's address string. */
  sockunion2str (su, buf, SU_ADDRSTRLEN);
  peer->host = XSTRDUP (MTYPE_BGP_PEER_HOST, buf);

  /* Set up peer's events and timers. */
  if (! active && peer_active (peer))
    bgp_timer_set (peer);

  /* register */
  bgp_peer_index_register(peer, &peer->su);

  /* session */
  peer->session = bgp_session_init_new(peer->session, peer);

  return peer;
}

/* Delete peer from configuration.
 *
 * The peer is moved to a dead-end "Deleted" neighbour-state, to allow
 * it to "cool off" and refcounts to hit 0, at which state it is freed.
 *
 * This function /should/ take care to be idempotent, to guard against
 * it being called multiple times through stray events that come in
 * that happen to result in this function being called again.  That
 * said, getting here for a "Deleted" peer is a bug in the neighbour
 * FSM.
 */
int
peer_delete (struct peer *peer)
{
  int i;
  afi_t afi;
  safi_t safi;
  struct bgp *bgp;
  struct bgp_filter *filter;
  struct listnode *pn;

  assert (peer->state != bgp_peer_sDeleted);

  bgp = peer->bgp;

  if (CHECK_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT))
    peer_nsf_stop (peer);

  /* If this peer belongs to peer group, clear up the
     relationship.  */
  if (peer->group)
    {
      if ((pn = listnode_lookup (peer->group->peer, peer)))
        {
          peer = peer_unlock (peer); /* group->peer list reference */
          list_delete_node (peer->group->peer, pn);
        }
      peer->group = NULL;
    }

  /* Withdraw all information from routing table.  We can not use
   * BGP_EVENT_ADD (peer, BGP_Stop) at here.  Because the event is
   * executed after peer structure is deleted.
   */
  peer->last_reset = PEER_DOWN_NEIGHBOR_DELETE;
  bgp_peer_stop (peer);
  bgp_clear_route_all(peer);
  peer_change_status (peer, bgp_peer_sDeleted);

  /* Password configuration */
  if (peer->password)
    {
      XFREE (MTYPE_PEER_PASSWORD, peer->password);
      peer->password = NULL;
    }

  bgp_timer_set (peer); /* stops all timers for Deleted */

  /* Delete from all peer list. */
  if (! CHECK_FLAG (peer->sflags, PEER_STATUS_GROUP)
      && (pn = listnode_lookup (bgp->peer, peer)))
    {
      peer_unlock (peer); /* bgp peer list reference */
      list_delete_node (bgp->peer, pn);
    }

  if (peer_rsclient_active (peer)
      && (pn = listnode_lookup (bgp->rsclient, peer)))
    {
      peer_unlock (peer); /* rsclient list reference */
      list_delete_node (bgp->rsclient, pn);

      /* Clear our own rsclient ribs. */
      for (afi = AFI_IP; afi < AFI_MAX; afi++)
        for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
          if (CHECK_FLAG(peer->af_flags[afi][safi],
                         PEER_FLAG_RSERVER_CLIENT))
            bgp_clear_route (peer, afi, safi, BGP_CLEAR_ROUTE_MY_RSCLIENT);
    }

  /* Free RIB for any family in which peer is RSERVER_CLIENT, and is not
      member of a peer_group. */
  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      if (peer->rib[afi][safi] && ! peer->af_group[afi][safi])
        bgp_table_finish (&peer->rib[afi][safi]);

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

  peer_unlock (peer); /* initial reference */

  return 0;
}

void
peer_free (struct peer *peer)
{
  assert (peer->state == bgp_peer_sDeleted);

  bgp_unlock(peer->bgp);

  /* this /ought/ to have been done already through bgp_stop earlier,
   * but just to be sure..
   */
  bgp_timer_set (peer);
  BGP_EVENT_FLUSH (peer);

  /* unregister */
  if (peer->index_entry != NULL)
    bgp_peer_index_deregister(peer, &peer->su);

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

  if (peer->clear_node_queue)
    work_queue_free (peer->clear_node_queue);

  /* session */
  if (peer->session)
    bgp_session_free(peer->session);

  bgp_sync_delete (peer);
  memset (peer, 0, sizeof (struct peer));

  XFREE (MTYPE_BGP_PEER, peer);
}

void
peer_nsf_stop (struct peer *peer)
{
  afi_t afi;
  safi_t safi;

  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_WAIT);
  UNSET_FLAG (peer->sflags, PEER_STATUS_NSF_MODE);

  for (afi = AFI_IP ; afi < AFI_MAX ; afi++)
    for (safi = SAFI_UNICAST ; safi < SAFI_UNICAST_MULTICAST ; safi++)
      peer->nsf[afi][safi] = 0;

  if (peer->t_gr_restart)
    {
      BGP_TIMER_OFF (peer->t_gr_restart);
      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s graceful restart timer stopped", peer->host);
    }
  if (peer->t_gr_stale)
    {
      BGP_TIMER_OFF (peer->t_gr_stale);
      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s graceful restart stalepath timer stopped", peer->host);
    }
  bgp_clear_route_all (peer);
}

/* enable the peer */
void
bgp_peer_enable(bgp_peer peer)
{
  /* Don't enable the session if:
   * 1) Peer not idle, means we're not ready yet, clearing, deleting or waiting
   *    for disable.
   * 2) In shutdown, never want to enable ever again
   * 3) Dealing with prefix overflow, its timer will enable peer when ready
   */
  if ((peer->state == bgp_peer_sIdle)
      && !CHECK_FLAG (peer->flags, PEER_FLAG_SHUTDOWN)
      && !CHECK_FLAG (peer->sflags, PEER_STATUS_PREFIX_OVERFLOW))
    {
      /* enable the session */
      bgp_session_enable(peer);
    }
}

/* disable the peer
 * sent notification, disable session
 */
void
bgp_peer_disable(bgp_peer peer, bgp_notify notification)
{
  if (bgp_session_is_active(peer->session))
      bgp_session_disable(peer, notification);
  else
    {
      bgp_notify_free(notification) ;
      bgp_peer_stop(peer);
    }
}

/* Called after event occurred, this function changes status */
void
peer_change_status (bgp_peer peer, int status)
{
  bgp_dump_state (peer, peer->state, status);

  /* Preserve old status and change into new status. */
  peer->ostate = peer->state;
  peer->state = status;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s went from %s to %s",
                peer->host,
                LOOKUP (bgp_peer_status_msg, peer->ostate),
                LOOKUP (bgp_peer_status_msg, peer->state));
}

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
    return sockunion_new(best_prefix) ;

  zlog_err("Peer %s interface %s has no suitable address", peer->host, ifname);

  return NULL ;
} ;

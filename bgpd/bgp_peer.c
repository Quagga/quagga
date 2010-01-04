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

#include <zebra.h>

#include "linklist.h"
#include "prefix.h"
#include "vty.h"
#include "sockunion.h"
#include "thread.h"
#include "log.h"
#include "stream.h"
#include "memory.h"
#include "plist.h"
#include "mqueue.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_peer.h"

#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_packet.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_dump.h"
#include "bgpd/bgp_open.h"

#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_session.h"

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
 * BGP Peer Index
 *
 * When peers are created, they are registered in the bgp_peer_index.  When
 * they are destroyed, they are removed.  This is done by the Routeing Engine.
 *
 * The peer index is used by the Routeing Engine to lookup peers.
 *
 * The BGP Engine needs to lookup sessions when a listening socket accepts a
 * connection -- first, to decide whether to continue with the connection, and
 * second, to tie the connection to the right session.  It uses the peer index
 * to do this.
 *
 * A mutex is used to coordinate access to the index.
 *
 * NB: the bgp_peer points to its bgp_session (if any).  The session pointer
 *     MUST only be changed while holding the index mutex.
 *
 *     See bgp_peer_set_session().
 *
 * NB: to avoid deadlock, MUST NOT do anything using the index while holding
 *     any session mutex.
 *
 *     But may acquire a session mutex while doing things to the index.
 */

static struct symbol_table bgp_peer_index ;
static qpt_mutex_t         bgp_peer_index_mutex ;

inline static void BGP_PEER_INDEX_LOCK(void)
{
  qpt_mutex_lock(&bgp_peer_index_mutex) ;
} ;

inline static void BGP_PEER_INDEX_UNLOCK(void)
{
  qpt_mutex_unlock(&bgp_peer_index_mutex) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index.
 *
 * This must be done before any peers are configured !
 */
extern void
bgp_peer_index_init(void* parent)
{
  symbol_table_init_new(
          &bgp_peer_index,
          parent,
          10,                     /* start ready for a few sessions   */
          200,                    /* allow to be quite dense          */
          sockunion_symbol_hash,  /* "name" is an IP Address          */
          NULL) ;                 /* no value change call-back        */
} ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index_mutex.
 *
 * This must be done before the BGP Engine is started.
 */
extern void
bgp_peer_index_mutex_init(void* parent)
{
  qpt_mutex_init(&bgp_peer_index_mutex, qpt_mutex_recursive) ;
} ;

/*------------------------------------------------------------------------------
 * Lookup a peer -- do nothing if does not exist
 *
 * For use by the Routeing Engine.
 *
 * Returns the bgp_peer -- NULL if not found.
 */
extern bgp_peer
bgp_peer_index_seek(union sockunion* su)
{
  bgp_peer peer ;

  BGP_PEER_INDEX_LOCK() ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  peer = symbol_get_value(symbol_seek(&bgp_peer_index, su)) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return peer ;
} ;

/*------------------------------------------------------------------------------
 * Register a peer in the peer index.
 *
 * For use by the Routeing Engine.
 *
 * NB: it is a FATAL error to register a peer for an address which is already
 *     registered.
 */
extern void
bgp_peer_index_register(bgp_peer peer, union sockunion* su)
{
  BGP_PEER_INDEX_LOCK() ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  peer = symbol_set_value(symbol_find(&bgp_peer_index, su), peer) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  passert(peer != NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Lookup a session -- do nothing if does not exist
 *
 * For use by the BGP Engine.
 *
 * Returns the bgp_session, or NULL if not found OR not active
 * Sets *p_found <=> found.
 *
 * NB: the BGP Engine may not access the bgp_session structure if it is not
 *     in either of the active states (Enabled or Established).
 *
 *     So this function does not return the bgp_session unless it is active.
 *
 *     For callers who care, the p_found return indicates whether the session
 *     exists, or not.
 */
extern bgp_session
bgp_session_index_seek(union sockunion* su, int* p_found)
{
  bgp_session session ;
  bgp_peer    peer ;

  BGP_PEER_INDEX_LOCK() ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  peer = symbol_get_value(symbol_seek(&bgp_peer_index, su)) ;
  *p_found = (peer != NULL) ;
  if (*p_found && bgp_session_is_active(peer->session))
                                /* NULL peer->session is not active     */
    session = peer->session ;
  else
    session = NULL ;

  BGP_PEER_INDEX_UNLOCK() ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return session ;
} ;

/*------------------------------------------------------------------------------
 * Set peer's session pointer.
 *
 * For use by the Routeing Engine.  Locks the bgp_peer_index mutex so that the
 * BGP Engine is not fooled when it looks up the session.
 *
 * Returns the old session pointer value.
 *
 * NB: it is a FATAL error to change the pointer if the current session is
 *     "active".
 *
 */

extern bgp_session
bgp_session_index_seek(bgp_peer peer, bgp_session new_session)
{
  bgp_session old_session ;

  BGP_PEER_INDEX_LOCK() ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  old_session   = peer->session ;
  peer->session = new_session ;

  passert(!bgp_session_is_active(old_session)) ;

  BGP_PEER_INDEX_UNLOCK() ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return old_session ;
} ;



/*==============================================================================
 * Deal with change in session state -- mqueue_action function.
 *
 * Receives notifications from the BGP Engine when a BGP Session state changes,
 * which may be:
 *
 *    Enabled     -> Established  -- connection up and Opens exchanged
 *    Enabled     -> Stopped      -- stopped trying to connect (for some reason)
 *    Established -> Stopped      -- for some reason
 *
 * -- arg0  = session
 *    arg1  = new state
 *
 */
void
bgp_peer_new_session_state(mqueue_block mqb, mqb_flag_t flag)
{
  bgp_session         session = mqb_get_arg0(mqb) ;
  bgp_session_state_t state   = mqb_get_arg1_i(mqb) ;
  bgp_peer            peer    = session->peer ;

  if (flag == mqb_action)
    {
      bgp_session_lock(session) ;



      switch(new_state)
      {
      /* If now Enabled, then the BGP Engine is attempting to connect     */
      /* (may be waiting for the Idle Hold Time to expire.                */
      case bgp_session_Enabled:
        break ;

      /* If now Established, then the BGP Engine has exchanged BGP Open   */
      /* messages, and received the KeepAlive that acknowledges our Open. */
      case bgp_session_Established:
        break ;

      /* If now Stopped, then for some reason the BGP Engine has either   */
      /* stopped trying to connect, or the session has been stopped.      */
      case bgp_session_Stopped:
        break ;

      default:
      } ;

      bgp_session_unlock(session) ;
    } ;

  mqb_free(mqb) ;
} ;

/* BGP Session has been Established.
 *
 * NB: holding the session structure mutex.
 *
 * Send keepalive packet then make first update information.
 */
static int
bgp_session_has_established(bgp_peer peer)
{
  struct bgp_notify *notify;
  afi_t afi;
  safi_t safi;
  int nsf_af_count = 0;

  /* Reset capability open status flag. */
  if (! CHECK_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN))
    SET_FLAG (peer->sflags, PEER_STATUS_CAPABILITY_OPEN);

  /* Clear last notification data. */
  notify = &peer->notify;
  if (notify->data)
    XFREE (MTYPE_TMP, notify->data);
  memset (notify, 0, sizeof (struct bgp_notify));

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

  if (peer->v_keepalive)
    bgp_keepalive_send (peer);

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

/* Administrative BGP peer stop event. */
/* May be called multiple times for the same peer */
static int
bgp_session_has_stopped(bgp_peer *peer)
{
  afi_t afi;
  safi_t safi;
  char orf_name[BUFSIZ];

  /* Can't do this in Clearing; events are used for state transitions */
  if (peer->status != Clearing)
    {
      /* Delete all existing events of the peer */
      BGP_EVENT_FLUSH (peer);
    }

  /* Increment Dropped count. */
  if (peer->status == Established)
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

  /* Stop read and write threads when exists. */
  BGP_READ_OFF (peer->t_read);
  BGP_WRITE_OFF (peer->t_write);

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

  peer->update_time = 0;

  /* Until we are sure that there is no problem about prefix count
     this should be commented out.*/
#if 0
  /* Reset prefix count */
  peer->pcount[AFI_IP][SAFI_UNICAST] = 0;
  peer->pcount[AFI_IP][SAFI_MULTICAST] = 0;
  peer->pcount[AFI_IP][SAFI_MPLS_VPN] = 0;
  peer->pcount[AFI_IP6][SAFI_UNICAST] = 0;
  peer->pcount[AFI_IP6][SAFI_MULTICAST] = 0;
#endif /* 0 */

  return 0;
}



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






void
bgp_timer_set (struct peer *peer)
{
  int jitter = 0;

  switch (peer->status)
    {
    case Idle:
      /* First entry point of peer's finite state machine.  In Idle
	 status start timer is on unless peer is shutdown or peer is
	 inactive.  All other timer must be turned off */
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
      break;

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

    case Established:
      /* In Established status start and connect timer is turned
         off. */
     BGP_TIMER_OFF (peer->t_asorig);
      break;
    case Deleted:
      BGP_TIMER_OFF (peer->t_gr_restart);
      BGP_TIMER_OFF (peer->t_gr_stale);
      BGP_TIMER_OFF (peer->t_pmax_restart);
    case Clearing:
      BGP_TIMER_OFF (peer->t_asorig);
      BGP_TIMER_OFF (peer->t_routeadv);
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

  BGP_WRITE_ON (peer->t_write, bgp_write, peer->fd);

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


/* BGP peer is stoped by the error. */
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


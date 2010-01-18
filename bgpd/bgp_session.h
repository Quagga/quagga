/* BGP Session -- header
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

#ifndef _QUAGGA_BGP_SESSION_H
#define _QUAGGA_BGP_SESSION_H

#include <zebra.h>

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_route_refresh.h"
#include "bgpd/bgp_peer_index.h"

#include "lib/qtimers.h"
#include "lib/qpthreads.h"
#include "lib/sockunion.h"
#include "lib/mqueue.h"

#ifndef Inline
#define Inline static inline
#endif


/*==============================================================================
 * BGP Session data structure.
 *
 * The bgp_session structure encapsulates a BGP session from the perspective
 * of the Routeing Engine, and that is shared with the BGP Engine.
 *
 * The session may have up to two BGP connections associated with it, managed
 * by the BGP Engine.
 *
 * The session includes the "negotiating position" for the BGP Open exchange,
 * which is managed by the BGP Engine.  Changes to that negotiating position
 * may require any existing session to be terminated.
 *
 * NB: the session structure is shared by the Routeing Engine and the BGP
 *     Engine, so there is a mutex to coordinate access.
 *
 *     The information in this shared structure is only required every now and
 *     then, so the overhead of a mutex operation for every access is not an
 *     issue.
 *
 * NB: the connections associated with a BGP session are private to the BGP
 *     Engine when sessions are disabled or have failed, there will be no
 *     connections.
 *
 */


struct bgp_session
{
  bgp_peer          peer ;              /* peer whose session this is     */

  bgp_peer_index_entry  index_entry ;   /* and its index entry            */

  qpt_mutex_t       mutex ;             /* for access to the rest         */

  /* While sIdle and sStopped:
   *
   *   the session belongs to the Peering Engine.
   *
   *   The BGP Engine will not touch a session in these states and the
   *   Peering Engine may do what it likes with it.
   *
   * While sEnabled, sEstablished and sStopping:
   *
   *   the session belongs to the BGP Engine.
   *
   *   A (very) few items in the session may be accessed by the Peering Engine,
   *   as noted below.  (Subject to the mutex.)
   *
   * Only the Peering Engine creates and destroys sessions.  The BGP Engine
   * assumes that a session will not be destroyed while it is sEnabled,
   * sEstablished or sStopping.
   *
   * Only the Peering Engine touches the state and defer_enable items.
   *
   * The made flag is cleared by the Peering Engine before enabling a session,
   * and is set by the BGP Engine when the session becomes sEstablished.
   *
   * The Peering Engine may use this flag in sStopped state to see if the
   * session was ever established.
   */
  bgp_session_state_t   state ;
  int                   defer_enable ;  /* set when waiting for stop      */

  flag_t                made ;          /* set when -> sEstablished       */

  /* Flow control. Incremented when an update packet is sent
   * from peering to BGP engine.  Decremented when packet processed
   * by BGP engine.  On transition to 0 BGP engine should send an XON.
   */

  int flow_control;

  /* These belong to the Peering Engine, and may be set when a session
   * event message is received from the BGP Engine.
   */
  bgp_session_event_t   event ;         /* last event                     */
  bgp_notify            notification ;  /* if any sent/received           */
  int                   err ;           /* errno, if any                  */
  bgp_connection_ord_t  ordinal ;       /* primary/secondary connection   */

  /* The Routeing Engine sets open_send and clears open_recv before enabling
   * the session, and may not change them while sEnabled/sEstablished.
   *
   * The BGP Engine sets open_recv before setting the session sEstablished,
   * and will not touch it thereafter.
   *
   * So: the Routeing Engine may use open_recv once the session is
   *     sEstablished.
   */
  bgp_open_state    open_send ;         /* how to open the session        */
  bgp_open_state    open_recv ;         /* set when session Established   */

  /* The following are set by the Routeing Engine before a session is
   * enabled, and not changed at any other time by either engine.
   */
  flag_t            connect ;           /* initiate connections           */
  flag_t            listen ;            /* listen for connections         */

  flag_t            cap_override ;      /* override ... TODO: what ?      */
  flag_t            cap_strict ;        /* strict...    TODO: what ?      */

  int               ttl ;               /* TTL to set, if not zero        */
  unsigned short    port ;              /* destination port for peer      */
  union sockunion*  su_peer ;           /* Sockunion address of the peer  */

  struct zlog*      log ;               /* where to log to                */
  char*             host ;              /* copy of printable peer's addr  */

  char*             password ;          /* copy of MD5 password           */

  unsigned  idle_hold_timer_interval ;  /* in seconds                     */
  unsigned  connect_retry_timer_interval ;
  unsigned  open_hold_timer_interval ;

  /* These are set by the Routeing Engine before a session is enabled,
   * but are affected by the capabilities received in the OPEN message.
   *
   * The Routeing Engine may read these once sEstablished (under mutex).
   *
   * In sStopped state these reflect the last state of the session.
   */
  unsigned  hold_timer_interval ;       /* subject to negotiation         */
  unsigned  keepalive_timer_interval ;  /* subject to negotiation         */

  flag_t            as4 ;               /* set by OPEN                    */
  flag_t            route_refresh_pre ; /* use pre-RFC version            */

  /* These are cleared by the Routeing Engine before a session is enabled,
   * and set by the BGP Engine when the session is established.
   *
   * In sStopped state these reflect the last state of the session.
   */
  union sockunion*  su_local ;          /* set when session Established   */
  union sockunion*  su_remote ;         /* set when session Established   */

  /* These values are are private to the BGP Engine.
   *
   * They must be cleared before the session is enabled, but may not be
   * touched by the Routeing Engine at any other time.
   *
   * Before stopping a session the BGP Engine unlinks any connections from
   * the session, and sets the stopped flag.
   *
   * The active flag is set when one or more connections are activated, and
   * cleared when either the BGP Engine stops the session or the Peering
   * Engine disables it.  When not "active" all messages other than disable
   * and enable are ignored.  This deals with the hiatus that exists between
   * the BGP Engine signalling that it has stopped (because of some exception)
   * and the Peering Engine acknowledging that (by disabling the session).
   */
  bgp_connection    connections[bgp_connection_count] ;

  flag_t        active ;
} ;

/*==============================================================================
 * Mqueue messages related to sessions
 *
 * In all these messages arg0 is the session.
 */

struct bgp_session_enable_args          /* to BGP Engine                */
{
                                        /* no further arguments         */
} ;
MQB_ARGS_SIZE_OK(bgp_session_enable_args) ;

struct bgp_session_disable_args         /* to BGP Engine                */
{
  bgp_notify    notification ;          /* NOTIFICATION to send         */
} ;
MQB_ARGS_SIZE_OK(bgp_session_enable_args) ;

struct bgp_session_update_args          /* to and from BGP Engine       */
{
  struct stream*  buf ;
  bgp_size_t size ;

  bgp_connection  pending ;             /* used inside the BGP Engine   */
                                        /* set NULL on message creation */
} ;
MQB_ARGS_SIZE_OK(bgp_session_update_args) ;

struct bgp_session_route_refresh_args   /* to and from BGP Engine       */
{
  bgp_route_refresh  rr ;

  bgp_connection  pending ;             /* used inside the BGP Engine   */
                                        /* set NULL on message creation */
} ;
MQB_ARGS_SIZE_OK(bgp_session_route_refresh_args) ;

struct bgp_session_end_of_rib_args      /* to and from BGP Engine       */
{
  iAFI_t    afi ;
  iSAFI_t   safi ;

  bgp_connection  pending ;             /* used inside the BGP Engine   */
                                        /* set NULL on message creation */
} ;
MQB_ARGS_SIZE_OK(bgp_session_end_of_rib_args) ;

struct bgp_session_event_args           /* to Routeing Engine           */
{
  bgp_session_event_t  event ;
  bgp_notify           notification ;   /* sent or received (if any)    */
  int                  err ;            /* errno if any                 */
  bgp_connection_ord_t ordinal ;        /* primary/secondary connection */
  int                  stopped ;        /* session has stopped          */
} ;
MQB_ARGS_SIZE_OK(bgp_session_event_args) ;

struct bgp_session_XON_args             /* to Routeing Engine           */
{
                                        /* no further arguments         */
} ;
MQB_ARGS_SIZE_OK(bgp_session_XON_args) ;



enum { BGP_XON_THRESHOLD = 7 } ;

/*==============================================================================
 * Session mutex lock/unlock
 */

inline static void BGP_SESSION_LOCK(bgp_session session)
{
  qpt_mutex_lock(&session->mutex) ;
} ;

inline static void BGP_SESSION_UNLOCK(bgp_session session)
{
  qpt_mutex_unlock(&session->mutex) ;
} ;

/*==============================================================================
 * Functions
 */

extern bgp_session
bgp_session_init_new(bgp_session session, bgp_peer peer) ;

extern bgp_session
bgp_session_free(bgp_session session);

extern void
bgp_session_enable(bgp_peer peer) ;

extern void
bgp_session_disable(bgp_peer peer, bgp_notify notification) ;

extern void
bgp_session_event(bgp_session session, bgp_session_event_t  event,
                                       bgp_notify           notification,
                                       int                  err,
                                       bgp_connection_ord_t ordinal,
                                       int                  stopped) ;

extern void
bgp_session_update_send(bgp_session session, struct stream* upd) ;

extern void
bgp_session_route_refresh_send(bgp_session session, bgp_route_refresh rr) ;

extern void
bgp_session_end_of_rib_send(bgp_session session, qAFI_t afi, qSAFI_t) ;

extern void
bgp_session_update_recv(bgp_session session, struct stream* buf,
                                                              bgp_size_t size) ;

extern int
bgp_session_is_XON(bgp_peer peer);

/*==============================================================================
 * Session data access functions.
 *
 *
 */

extern int
bgp_session_is_active(bgp_session session) ;


#endif /* QUAGGA_BGP_SESSION_H */

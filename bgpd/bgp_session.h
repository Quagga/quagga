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

#include "lib/qtimers.h"
#include "lib/qpthreads.h"
#include "lib/sockunion.h"

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

typedef enum bgp_session_states bgp_session_state_t ;
enum bgp_session_states
{
  bgp_session_min_state     = 0,

  bgp_session_Idle          = 0,

  bgp_session_Enabled       = 1,  /* attempting to connect                  */
  bgp_session_Established   = 2,

  bgp_session_Stopped       = 3,  /* for whatever reason                    */

  bgp_session_max_state     = 3
} ;

struct bgp_session
{
  bgp_peer          peer ;              /* peer whose session this is     */

  qpt_mutex_t       mutex ;             /* for access to the rest         */

  bgp_session_state_t   state ;         /* as above                       */

  bgp_stopped_cause_t   stopped ;       /* why stopped                    */
  bgp_notify            notification ;  /* if any sent/received           */

  bgp_open_state    open_send ;         /* how to open the session        */
  bgp_open_state    open_recv ;         /* how session was opened         */

  int               connect ;           /* initiate connections           */
  int               listen ;            /* listen for connections         */

  int               accept ;            /* accept connections             */

  int               ttl ;               /* TTL to set, if not zero        */
  unsigned short    port ;              /* destination port for peer      */
  union sockunion   su_peer ;           /* Sockunion address of the peer  */

  struct in_addr    router_id ;

  struct zlog*      log ;               /* where to log to                */
  char*             host ;              /* copy of printable peer's addr  */

  char*             password ;          /* copy of MD5 password           */

  unsigned  idle_hold_timer_interval ;  /* in seconds                     */
  unsigned  connect_retry_timer_interval ;
  unsigned  open_hold_timer_interval ;
  unsigned  hold_timer_interval ;       /* subject to negotiation         */
  unsigned  keepalive_timer_interval ;  /* subject to negotiation         */

  /* NB: these values are private to the BGP Engine, which accesses them  */
  /*     *without* worrying about the mutex.                              */
  /*                                                                      */
  /*     The BGP Engine uses these while the session is Enabled or        */
  /*     Established -- so the session must not be freed in these states. */

  bgp_connection    connections[bgp_connection_count] ;
} ;

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
 *
 */

extern bgp_session
bgp_session_init_new(bgp_session session) ;

extern bgp_session
bgp_session_lookup(union sockunion* su, int* exists) ;

/*==============================================================================
 * Session data access functions.
 *
 *
 */

extern int
bgp_session_is_active(bgp_session session) ;

extern int
bgp_session_is_accepting(bgp_session session) ;

#endif /* QUAGGA_BGP_SESSION_H */

/* BGP Connection Handling -- header
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

#ifndef _QUAGGA_BGP_CONNECTION_H
#define _QUAGGA_BGP_CONNECTION_H

#include <stdbool.h>

#include "lib/mqueue.h"
#include "lib/qpthreads.h"
#include "lib/qtimers.h"
#include "lib/qpselect.h"

#include "lib/sockunion.h"
#include "lib/stream.h"

//#include "bgpd/bgp.h"

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_msg_read.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * The BGP Finite State Machine: states and events
 *
 */

typedef enum bgp_fsm_states bgp_fsm_state_t ;
enum bgp_fsm_states
{
  bgp_fsm_first_state     = 0,

  bgp_fsm_sInitial        = 0,  /* extra: connection initialised            */

  bgp_fsm_sIdle           = 1,  /* waiting for Idle Hold time               */
  bgp_fsm_sConnect        = 2,  /* waiting for connect (may be listening)   */
  bgp_fsm_sActive         = 3,  /* listening only                           */
  bgp_fsm_sOpenSent       = 4,  /* sent Open -- awaits Open                 */
  bgp_fsm_sOpenConfirm    = 5,  /* sent & received Open -- awaits keepalive */
  bgp_fsm_sEstablished    = 6,  /* running connection                       */

  bgp_fsm_sStopping       = 7,  /* extra: connection shutting down          */

  bgp_fsm_last_state      = 7,
} ;

typedef enum bgp_fsm_events bgp_fsm_event_t ;
enum bgp_fsm_events
{
  bgp_fsm_null_event                      =  0,

  bgp_fsm_eBGP_Start                      =  1,
  bgp_fsm_eBGP_Stop                       =  2,
  bgp_fsm_eTCP_connection_open            =  3,
  bgp_fsm_eTCP_connection_closed          =  4,
  bgp_fsm_eTCP_connection_open_failed     =  5,
  bgp_fsm_eTCP_fatal_error                =  6,
  bgp_fsm_eConnectRetry_timer_expired     =  7,
  bgp_fsm_eHold_Timer_expired             =  8,
  bgp_fsm_eKeepAlive_timer_expired        =  9,
  bgp_fsm_eReceive_OPEN_message           = 10,
  bgp_fsm_eReceive_KEEPALIVE_message      = 11,
  bgp_fsm_eReceive_UPDATE_message         = 12,
  bgp_fsm_eReceive_NOTIFICATION_message   = 13,
  bgp_fsm_eSent_NOTIFICATION_message      = 14,

  bgp_fsm_last_event                      = 15,
} ;

/*==============================================================================
 * BGP Connection Structures
 *
 *------------------------------------------------------------------------------
 * Write buffer for connection.
 *
 *  NB: when connection is initialised all the pointers are set NULL.
 *
 *      The buffer is not allocated until the TCP connection comes up.
 *
 *  NB: p_out == p_in => buffer is empty
 *
 *      BUT: p_out == limit => buffer is not writable.
 *
 *      When connection is first initialised all pointers are NULL, so the
 *      buffer is "empty but not writable".
 *
 *      When connection is opened, closed or fails, buffer is set into this
 *      "empty but not writable" state.
 */
typedef struct bgp_wbuffer* bgp_wbuffer ;
struct bgp_wbuffer
{
  uint8_t*    p_out ;
  uint8_t*    p_in ;

  uint8_t*    base ;
  uint8_t*    limit ;
} ;

/* Buffer is allocated for a number of maximum size BGP messages.       */
enum { bgp_wbuff_size = BGP_MSG_MAX_L * 10 } ;

/*==============================================================================
 * BGP Connection Structure
 *
 * The BGP Connection is the main data structure for the BGP Engine.
 *
 * When a session terminates, or a connection is shut it may have a short
 * independent life, if a NOTIFICATION message is pending.
 */
struct bgp_connection
{
  struct dl_list_pair(bgp_connection) exist ;
                                        /* list of existing connections   */

  bgp_session       session ;           /* session connection belongs to  */
                                        /* NULL if connection stopping    */
  qpt_mutex         p_mutex ;           /* session mutex*                 */
                                        /* (avoids incomplete type issue) */
  unsigned          lock_count ;        /* session mutex lock count       */

  bgp_connection_ord_t ordinal ;        /* primary/secondary connection   */
  bool              accepted ;          /* came via accept()              */

  bgp_fsm_state_t   state ;             /* FSM state of connection        */
  bool              comatose ;          /* Idle and no timer set          */
  bool              half_open ;         /* Idle but accepted connection   */

  bgp_connection    next ;              /* for the connection queue       */
  bgp_connection    prev ;              /* NULL <=> not on the queue      */

  int               fsm_active ;        /* active in FSM counter          */
  bgp_fsm_event_t   follow_on ;         /* event raised within FSM        */

  bgp_session_event_t exception;        /* exception posted here          */
  bgp_notify        notification ;      /* if any sent/received           */
  int               err ;               /* erno, if any                   */

  bool              cap_suppress ;      /* capability send suppress
                                           always set false when connection
                                           initialised.  Set if get
                                           NOTIFICATION that other end does
                                           not do capabilities.  Copied to
                                           session when established.      */

  bgp_open_state    open_recv ;         /* the open received.             */

  qps_file          qf ;                /* qpselect file structure        */
  pAF_t             paf ;               /* address family                 */

  union sockunion*  su_local ;          /* address of the near end        */
  union sockunion*  su_remote ;         /* address of the far end         */

  char*             host ;              /* peer "name" + Connect/Listen   */
  struct zlog*      log ;               /* where to log to                */

  unsigned  hold_timer_interval ;       /* subject to negotiation         */
  unsigned  keepalive_timer_interval ;  /* subject to negotiation         */

  bool              as4 ;               /* subject to negotiation         */
  bool              route_refresh ;     /* subject to negotiation         */
  bool              orf_prefix ;        /* subject to negotiation         */

  qtimer            hold_timer ;
  qtimer            keepalive_timer ;

  struct stream*    ibuf ;              /* a single input "stream"        */
  unsigned          read_pending ;      /* how much input waiting for     */

  bool              read_header ;       /* reading message header         */
  uint8_t           msg_type ;          /* copy of message type           */
  bgp_size_t        msg_body_size ;     /* size of message *body*         */
  bgp_msg_handler*  msg_func ;          /* function to handle message     */

  struct stream*    obuf ;              /* a single output "stream"       */

  int          notification_pending ;   /* waiting to write NOTIFICATION  */

  struct mqueue_local_queue
                    pending_queue ;     /* pending write messages         */

  struct bgp_wbuffer wbuff ;            /* write buffer                   */
} ;

/*==============================================================================
 * The functions
 */

extern bgp_connection
bgp_connection_init_new(bgp_connection connection, bgp_session session,
                                                 bgp_connection_ord_t ordinal) ;
extern void
bgp_connection_open(bgp_connection connection, int fd, int family) ;

extern void
bgp_connection_start(bgp_connection connection, union sockunion* su_local,
                                                union sockunion* su_remote) ;
extern void
bgp_connection_enable_accept(bgp_connection connection) ;

extern void
bgp_connection_disable_accept(bgp_connection connection) ;

extern bgp_connection
bgp_connection_query_accept(bgp_session session) ;

extern bgp_connection
bgp_connection_get_sibling(bgp_connection connection) ;

extern void
bgp_connection_make_primary(bgp_connection connection) ;

extern void
bgp_connection_full_close(bgp_connection connection, int unset_timers) ;

#define bgp_connection_close(conn) bgp_connection_full_close(conn, false)
#define bgp_connection_close_down(conn) bgp_connection_full_close(conn, true)

extern bool
bgp_connection_part_close(bgp_connection connection) ;

extern void
bgp_connection_exit(bgp_connection connection) ;

extern void
bgp_connection_read_enable(bgp_connection connection) ;

extern int
bgp_connection_write(bgp_connection connection, struct stream* s) ;

extern void
bgp_connection_queue_add(bgp_connection connection) ;

extern void
bgp_connection_queue_del(bgp_connection connection) ;

extern int
bgp_connection_queue_process(void) ;

Inline bool
bgp_connection_no_pending(bgp_connection connection, bgp_connection* is_pending)
{
  return (   (mqueue_local_head(&connection->pending_queue) == NULL)
          || (*is_pending != NULL) ) ;
} ;

extern void
bgp_connection_add_pending(bgp_connection connection, mqueue_block mqb,
                                                   bgp_connection* is_pending) ;

/*------------------------------------------------------------------------------
 * Set buffer *unwritable* (buffer appears full, but nothing pending).
 */
Inline void
bgp_write_buffer_unwritable(bgp_wbuffer wb)
{
  wb->p_in = wb->p_out = wb->limit ;
} ;

/*------------------------------------------------------------------------------
 * If allocated:   set buffer empty
 * If unallocated: buffer remains *unwritable*
 */
Inline void
bgp_write_buffer_reset(bgp_wbuffer wb)
{
  wb->p_in = wb->p_out = wb->base ;
} ;

/*------------------------------------------------------------------------------
 * See if do NOT have enough room for what want to write PLUS 1.
 *
 * NB: there is never any room in an unallocated buffer.
 */
Inline bool
bgp_write_buffer_cannot(bgp_wbuffer wb, size_t want)
{
  return ((size_t)(wb->limit - wb->p_in) <= want) ;
} ;

/*------------------------------------------------------------------------------
 * Full if NOT enough room for a maximum size BGP message + 1
 *
 * NB: there is never any room in an unallocated buffer.
 */
enum { bgp_write_buffer_full_threshold = BGP_MSG_MAX_L + 1 } ;

Inline bool
bgp_write_buffer_cannot_max(bgp_wbuffer wb)
{
  return bgp_write_buffer_cannot(wb, BGP_MSG_MAX_L) ;
} ;

/*------------------------------------------------------------------------------
 * See if buffer has anything in it.
 *
 * If empty, ensures that the buffer has been allocated, and sets the pointers
 * to the start of the buffer -- so all set to go.
 */
Inline bool
bgp_write_buffer_empty(bgp_wbuffer wb)
{
  if (wb->p_out < wb->p_in)
    return false ;              /* not empty => has buffer      */

  dassert(wb->p_out == wb->p_in) ;

  passert(wb->base != NULL) ;   /* must have buffer             */

  bgp_write_buffer_reset(wb) ;  /* pointers to start of buffer  */

  return true ;                 /* empty and all ready to go    */
} ;

/*------------------------------------------------------------------------------
 * Return how much the write buffer still has to write.
 *
 * NB: if returns 0, may not yet have been allocated.
 *
 *     > 0 => allocated.
 */
Inline int
bgp_write_buffer_has(bgp_wbuffer wb)
{
  dassert(wb->p_out <= wb->p_in) ;
  return (wb->p_in - wb->p_out) ;
} ;

/*------------------------------------------------------------------------------
 * As above, for connection
 */
Inline bool
bgp_connection_write_cannot_max(bgp_connection connection)
{
  return bgp_write_buffer_cannot_max(&connection->wbuff) ;
} ;

/*------------------------------------------------------------------------------
 * As above, for connection
 */
Inline bool
bgp_connection_write_empty(bgp_connection connection)
{
  return bgp_write_buffer_empty(&connection->wbuff) ;
} ;

/*==============================================================================
 * Locking the session associated with the connection.
 *
 * This is slightly complicated by the fact that when the connection is in
 * sStopping, it is no longer attached to the session.
 *
 * To facilitate that, the connection maintains its own "recursive" count, so
 * that when the connection is cut loose from the session, the session's mutex
 * can be released.
 *
 * Further -- when the connection is cut loose, a big number is added to the
 * count, so when the session is "unlocked" nothing will happen !
 *
 * Also -- this mechanism means that the session lock can be called even after
 * the connection has been cut loose, without requiring any other tests.
 */

Inline void
BGP_CONNECTION_SESSION_LOCK(bgp_connection connection)
{
  if (connection->lock_count++ == 0)
    qpt_mutex_lock(connection->p_mutex) ;
} ;

Inline void
BGP_CONNECTION_SESSION_UNLOCK(bgp_connection connection)
{
  if (--connection->lock_count == 0)
    qpt_mutex_unlock(connection->p_mutex) ;
} ;

extern void
BGP_CONNECTION_SESSION_CUT_LOOSE(bgp_connection connection) ;

#endif /* QUAGGA_BGP_CONNECTION_H */

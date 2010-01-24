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
 * BGP Connection Structure
 *
 * The BGP Connection is the main data structure for the BGP Engine.
 *
 * When a session terminates, or a connection is shut it may have a short
 * independent life, if a NOTIFICATION message is pending.
 *
 */

/* NB: p_out == p_in => buffer is empty
 *
 *     BUT: buffer is not allocated until required, and until then
 *          p_out == p_in == NULL  -- empty does NOT imply usable !
 *
 *     AND: when buffer is emptied, p_out and p_in will be some way down the
 *          buffer.
 *
 *     SO:  before writing, check for base != NULL and set p_out = p_in = base.
 *
 * NB: before buffer is allocated base == NULL, but limit is set to NULL + n,
 *     so that buffer does not appear full.
 *
 *     SO:  not full does NOT imply that p_out/p_in/base are set, either !
 */
typedef struct bgp_wbuffer* bgp_wbuffer ;
struct bgp_wbuffer
{
  uint8_t*    p_out ;
  uint8_t*    p_in ;

  uint8_t*    base ;
  uint8_t*    limit ;
} ;


struct bgp_connection
{
  bgp_session       session ;           /* session connection belongs to  */
                                        /* NULL if connection stopping    */
  qpt_mutex         p_mutex ;           /* session mutex*                 */
                                        /* (avoids incomplete type issue) */

  bgp_connection_ord_t ordinal ;        /* primary/secondary connection   */
  int               accepted ;          /* came via accept()              */

  bgp_fsm_state_t   state ;             /* FSM state of connection        */
  int               comatose ;          /* Idle and no timer set          */

  bgp_connection    next ;              /* for the connection queue       */
  bgp_connection    prev ;              /* NULL <=> not on the queue      */

  int               fsm_active ;        /* active in fsm count            */
  bgp_fsm_event_t   post ;              /* event raised within FSM        */

  bgp_session_event_t except ;          /* exception                      */
  bgp_notify        notification ;      /* if any sent/received           */
  int               err ;               /* erno, if any                   */

  bgp_open_state    open_recv ;         /* the open received.             */

  struct qps_file   qf ;                /* qpselect file structure        */

  union sockunion*  su_local ;          /* address of the near end        */
  union sockunion*  su_remote ;         /* address of the far end         */

  char*             host ;              /* peer "name" + Connect/Listen   */
  struct zlog*      log ;               /* where to log to                */

  unsigned  hold_timer_interval ;       /* subject to negotiation         */
  unsigned  keepalive_timer_interval ;  /* subject to negotiation         */

  flag_t            as4 ;               /* subject to negotiation         */
  flag_t            route_refresh_pre ; /* subject to negotiation         */
  flag_t            orf_prefix_pre ;    /* subject to negotiation         */

  struct qtimer     hold_timer ;
  struct qtimer     keepalive_timer ;

  struct stream*    ibuf ;              /* a single input "stream"        */
  unsigned          read_pending ;      /* how much input waiting for     */

  flag_t            read_header ;       /* reading message header         */
  uint8_t           msg_type ;          /* copy of message type           */
  bgp_size_t        msg_size ;          /* size of message *body*         */

  struct stream*    obuf ;              /* a single output "stream"       */

  int          notification_pending ;   /* waiting to write NOTIFICATION  */

  struct mqueue_local_queue
                    pending_queue ;     /* pending write messages         */

  struct bgp_wbuffer wbuff ;            /* write buffer                   */
} ;

/*==============================================================================
 *
 */

extern bgp_connection
bgp_connection_init_new(bgp_connection connection, bgp_session session,
                                                 bgp_connection_ord_t ordinal) ;
extern void
bgp_connection_open(bgp_connection connection, int fd) ;

extern void
bgp_connection_enable_accept(bgp_connection connection) ;

extern void
bgp_connection_disable_accept(bgp_connection connection) ;

extern bgp_connection
bgp_connection_get_sibling(bgp_connection connection) ;

extern void
bgp_connection_make_primary(bgp_connection connection) ;

extern void
bgp_connection_close(bgp_connection connection, int unset_timers) ;

extern void
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

extern void
bgp_connection_queue_process(void) ;


/*------------------------------------------------------------------------------
 * See if have enough room for what want to write PLUS 1.
 *
 * NB: caller must ensure buffer has been allocated, which will be true if
 *     has found that the buffer is not empty !
 */
Inline int
bgp_write_buffer_can(bgp_wbuffer wb, size_t want)
{
  return ((size_t)(wb->limit - wb->p_in) <= want) ;
} ;

/*------------------------------------------------------------------------------
 * Full if not enough room for a maximum size BGP message + 1
 *
 * NB: this will be true even if the buffer has not been allocated (!).
 */
enum { bgp_write_buffer_full_threshold = BGP_MSG_MAX_L + 1 } ;

Inline int
bgp_write_buffer_full(bgp_wbuffer wb)
{
  return bgp_write_buffer_can(wb, BGP_MSG_MAX_L) ;
} ;

/*------------------------------------------------------------------------------
 * Empty if in and out pointers are equal (but may need to be reset !)
 */
Inline int
bgp_write_buffer_empty(bgp_wbuffer wb)
{
  return (wb->p_out == wb->p_in) ;
} ;

/*------------------------------------------------------------------------------
 * As above, for connection
 */
Inline int
bgp_connection_write_full(bgp_connection connection)
{
  return bgp_write_buffer_full(&connection->wbuff) ;
} ;

/*------------------------------------------------------------------------------
 * As above, for connection
 */
Inline int
bgp_connection_write_empty(bgp_connection connection)
{
  return bgp_write_buffer_empty(&connection->wbuff) ;
} ;

/*==============================================================================
 * Access functions via bgp_connection for bgp_session attributes.
 *
 *
 *
 */

Inline void
BGP_CONNECTION_SESSION_LOCK(bgp_connection connection)
{
  if (connection->session != NULL)
    qpt_mutex_lock(connection->p_mutex) ;
} ;

Inline void
BGP_CONNECTION_SESSION_UNLOCK(bgp_connection connection)
{
  if (connection->session != NULL)
    qpt_mutex_unlock(connection->p_mutex) ;
} ;


#endif /* QUAGGA_BGP_CONNECTION_H */

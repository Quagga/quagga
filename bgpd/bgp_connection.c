/* BGP Connection Handling -- functions
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

#include "bgpd/bgp_connection.h"

#include <zebra.h>

#include "bgpd/bgpd.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_msg_read.h"

#include "lib/memory.h"
#include "lib/mqueue.h"
#include "lib/symtab.h"
#include "lib/stream.h"

/*==============================================================================
 * BGP Connections.
 *
 * Each BGP Connection has its own:
 *
 *   * BGP Finite State Machine (FSM)
 *   * socket and related qpselect file
 *   * input/output buffers and I/O management
 *   * timers to support the above
 *
 * Each BGP Session is associated with at most two BGP Connections.  The second
 * connection exists only if a connect and a listen connection is made while
 * a session is starting up, and one will be dropped before either connection
 * reaches Established state.
 *
 * The bgp_connection structure is private to the BGP Engine, and is accessed
 * directly, without the need for any mutex.
 *
 * Each connection is closely tied to its parent bgp_session.  The bgp_session
 * is shared between the Routeing Engine and the BGP Engine, and therefore
 * access is subject to the bgp_session's mutex.
 *
 */

/*==============================================================================
 * The connection queue.
 *
 * When the connection's write buffer empties, the connection is placed on the
 * connection queue.
 *
 * The connection queue is processed as the highest priority action in the
 * BGP Engine, at which point as many of the items on the connection's
 * pending queue as possible will be processed.
 *
 * The connection_queue is managed as a circular list of connections.  The
 * connection_queue variable points at the next to be processed.
 *
 */

static bgp_connection bgp_connection_queue ;

/*==============================================================================
 * Managing bgp_connection stuctures.
 */
static const char* bgp_connection_tags[] =
  {
      [bgp_connection_primary]   = "(primary)",
      [bgp_connection_secondary] = "(secondary)",
  } ;

static void bgp_connection_init_host(bgp_connection connection,
                                                              const char* tag) ;
static void bgp_write_buffer_init_new(bgp_wbuffer wb, size_t size) ;
static void bgp_write_buffer_free(bgp_wbuffer wb) ;

/*------------------------------------------------------------------------------
 * Initialise connection structure -- allocate if required.
 *
 *
 *
 * NB: requires the session LOCKED
 */
extern bgp_connection
bgp_connection_init_new(bgp_connection connection, bgp_session session,
                                                   bgp_connection_ord_t ordinal)
{
  assert( (ordinal == bgp_connection_primary)
       || (ordinal == bgp_connection_secondary) ) ;
  assert(session->connections[ordinal] == NULL) ;

  if (connection == NULL)
    connection = XCALLOC(MTYPE_BGP_CONNECTION, sizeof(struct bgp_connection)) ;
  else
    memset(connection, 0, sizeof(struct bgp_connection)) ;

  /* Structure is zeroised, so the following are implictly initialised:
   *
   *   * state                    bgp_fsm_Initial
   *   * comatose                 not comatose
   *   * next                     NULL -- not on the connection queue
   *   * prev                     NULL -- not on the connection queue
   *   * post                     bgp_fsm_null_event
   *   * fsm_active               not active
   *   * notification             NULL -- none received or sent
   *   * err                      no error, so far
   *   * su_local                 NULL -- no address, yet
   *   * su_remote                NULL -- no address, yet
   *   * hold_timer_interval      none -- set when connection is opened
   *   * keepalive_timer_interval none -- set when connection is opened
   *   * as4                      not AS4 conversation
   *   * route_refresh_pre        not pre-RFC ROUTE-REFRESH
   *   * read_pending             nothing pending
   *   * read_header              not reading header
   *   * notification_pending     nothing pending
   *   * wbuff_full               not full
   *   * wbuff                    all pointers NULL -- empty buffer
   */

  confirm(bgp_fsm_Initial    == 0) ;
  confirm(bgp_fsm_null_event == 0) ;

  /* Link back to session, point at its mutex and point session here        */
  connection->session  = session ;
  connection->p_mutex  = &session->mutex ;

  connection->ordinal  = ordinal ;
  connection->accepted = (ordinal == bgp_connection_secondary) ;

  session->connections[ordinal] = connection ;

  /* qps_file structure                                                 */
  qps_file_init_new(&connection->qf, NULL) ;

  /* Initialise all the timers                                          */
  qtimer_init_new(&connection->hold_timer,      bgp_nexus->pile,
                                                             NULL, connection) ;
  qtimer_init_new(&connection->keepalive_timer, bgp_nexus->pile,
                                                             NULL, connection) ;

  /* Copy log destination and make host name + (primary)/(secondary)    */
  /* Makes complete copies so that connection may continue to run, even */
  /* after the session has stopped, and may have been destroyed.        */
  connection->log  = session->log ;
  bgp_connection_init_host(connection, bgp_connection_tags[ordinal]) ;

  /* Need two empty "stream" buffers                                    */
  connection->ibuf = stream_new(BGP_MSG_MAX_L) ;
  connection->obuf = stream_new(BGP_MSG_MAX_L) ;

  /* Ensure mqueue_local_queue is empty.                                */
  mqueue_local_init_new(&connection->pending_queue) ;

  return connection ;
} ;

/*------------------------------------------------------------------------------
 * Set the host field for the connection to session->host + given tag.
 *
 * NB: requires the session to be LOCKED.
 */
static void
bgp_connection_init_host(bgp_connection connection, const char* tag)
{
  const char* host = connection->session->host ;

  connection->host = XMALLOC(MTYPE_BGP_PEER_HOST, strlen(host)
                                                + strlen(tag) + 1) ;
  strcpy(connection->host, host) ;
  strcat(connection->host, tag) ;
} ;

/*------------------------------------------------------------------------------
 * Get sibling (if any) for given connection.
 *
 * NB: requires the session to be LOCKED.
 */
extern bgp_connection
bgp_connection_get_sibling(bgp_connection connection)
{
  bgp_session session = connection->session ;

  if (session == NULL)
    return NULL ;               /* no sibling if no session             */

  confirm(bgp_connection_primary   == (bgp_connection_secondary ^ 1)) ;
  confirm(bgp_connection_secondary == (bgp_connection_primary   ^ 1)) ;

  return session->connections[connection->ordinal ^ 1] ;
} ;

/*------------------------------------------------------------------------------
 * Make given connection the primary.
 *
 * Expects the given connection to be the only remaining connection.
 *
 * NB: requires the session to be LOCKED.
 */
extern void
bgp_connection_make_primary(bgp_connection connection)
{
  bgp_session session = connection->session ;

  /* Deal with the connection ordinal.                          */
  if (connection->ordinal != bgp_connection_primary)
    {
      connection->ordinal = bgp_connection_primary ;
      session->connections[bgp_connection_primary] = connection ;
    } ;

  session->connections[bgp_connection_secondary] = NULL ;

  /* Move the open_state to the session.
   * Change the connection host to drop the primary/secondary distinction.
   * Copy the negotiated hold_timer_interval and keepalive_timer_interval
   * Copy the su_local and su_remote
   */

  session->open_recv = connection->open_recv ;
  connection->open_recv = NULL ;        /* no longer interested in this  */

  XFREE(MTYPE_BGP_PEER_HOST, connection->host) ;
  bgp_connection_init_host(connection, "") ;

  session->hold_timer_interval        = connection->hold_timer_interval ;
  session->keepalive_timer_interval   = connection->keepalive_timer_interval ;

  session->as4                        = connection->as4 ;
  session->route_refresh_pre          = connection->route_refresh_pre ;

  session->su_local  = connection->su_local ;
  connection->su_local  = NULL ;
  session->su_remote = connection->su_remote ;
  connection->su_remote = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Exit connection
 *
 * Make sure the connection is closed, then queue it to be reaped.
 */
extern void
bgp_connection_exit(bgp_connection connection)
{
  bgp_connection_close(connection) ;    /* make sure    */

  assert(   (connection->state == bgp_fsm_Stopping)
         && (connection->session == NULL) ) ;

  /* Add the connection to the connection queue, in Stopped state.
   *
   * When BGP Engine gets round to it, will free the structure.  This avoids
   * freeing the connection structure somewhere inside the FSM, and having to
   * cope with the possibility of having dangling references to it.
   */
  bgp_connection_queue_add(connection) ;
} ;

/*------------------------------------------------------------------------------
 * Free connection.
 *
 * Connection must be Stopping -- no longer attached to a session.
 *
 *
 *
 *
 */
static void
bgp_connection_free(bgp_connection connection)
{
  assert( (connection->state == bgp_fsm_Stopping) &&
          (connection->session == NULL) ) ;

  /* Make sure is closed, so no active file, no active timers, pending queue
   * is empty, not on the connection queue, etc.
   */
  bgp_connection_close(connection) ;

  /* Free any components which still exist                              */
  bgp_notify_free(&connection->notification) ;
  bgp_open_state_free(connection->open_recv) ;
  if (connection->su_local != NULL)
    sockunion_free(connection->su_local) ;
  if (connection->su_remote != NULL)
    sockunion_free(connection->su_remote) ;
  if (connection->host != NULL)
    XFREE(MTYPE_BGP_PEER_HOST, connection->host) ;
  stream_free(connection->ibuf) ;
  stream_free(connection->obuf) ;
  bgp_write_buffer_free(&connection->wbuff) ;

  /* Free the body                                                      */
  XFREE(MTYPE_BGP_CONNECTION, connection) ;
} ;

/*------------------------------------------------------------------------------
 * Allocate new write buffer and initialise pointers
 *
 * NB: assumes structure has been zeroised by the initialisation of the
 *     enclosing connection.
 */
static void
bgp_write_buffer_init_new(bgp_wbuffer wb, size_t size)
{
  assert(wb->base == NULL) ;

  wb->base  = XMALLOC(MTYPE_STREAM_DATA, size) ;
  wb->limit = wb->base + size ;

  wb->p_in  = wb->p_out = wb->base ;
} ;

/*------------------------------------------------------------------------------
 * Free any write buffer
 */
static void
bgp_write_buffer_free(bgp_wbuffer wb)
{
  if (wb->base != NULL)
    XFREE(MTYPE_STREAM_DATA, wb->base) ;
} ;

/*==============================================================================
 * Connection queue management.
 *
 * Connections appear on this queue when their write buffer becomes empty, or
 * they are finally stopped.
 */

/*------------------------------------------------------------------------------
 * Add connection to connection queue -- if not already on it
 */
extern void
bgp_connection_queue_add(bgp_connection connection)
{
  if (connection->next == NULL)
    {
      if (bgp_connection_queue == NULL)
        {
          /* adding to empty queue              */
          bgp_connection_queue = connection ;
          connection->next = connection ;
          connection->prev = connection ;
        }
      else
        {
          /* add behind the current entry       */
          connection->next       = bgp_connection_queue ;
          connection->prev       = bgp_connection_queue->prev ;

          connection->next->prev = connection ;
          connection->prev->next = connection ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Delete connection from connection queue -- if on it
 */
extern void
bgp_connection_queue_del(bgp_connection connection)
{
  if (connection->next != NULL)
    {
      if (connection == connection->next)
        {
          /* deleting the only item on the queue                */
          assert((connection == connection->prev)
                                      && (connection == bgp_connection_queue)) ;
          bgp_connection_queue = NULL ;
        }
      else
        {
          if (connection == bgp_connection_queue)
            bgp_connection_queue = connection->next ;

          connection->next->prev = connection->prev ;
          connection->prev->next = connection->next ;
        } ;

      connection->next = connection->prev = NULL ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Process the connection queue until it becomes empty.
 *
 * Process each item until its pending queue becomes empty, or its write
 * buffer becomes full, or it is stopped.
 *
 * TODO: link bgp_connection_queue_process() into the bgp_engine loop.
 */
extern void
bgp_connection_queue_process(void)
{
  mqueue_block mqb ;

  while (bgp_connection_queue != NULL)
    {
      /* select the first in the queue, and step to the next            */
      bgp_connection connection = bgp_connection_queue ;
      bgp_connection_queue = connection->next ;

      /* Reap the connection if it is now stopped.                      */
      if (connection->state == bgp_fsm_Stopping)
        {
          bgp_connection_free(connection) ; /* removes from connection queue */
          continue ;
        } ;

      /* Process next item on connection's pending queue                */
      mqb = mqueue_local_head(&connection->pending_queue) ;
      if (mqb != NULL)
        /* The action will either remove the mqb from the pending queue,
         * or remove the connection from the connection queue.
         */
        {
          bgp_session session = mqb_get_arg0(mqb) ;
          assert(  (session == connection->session)
                && (connection
                            == session->connections[bgp_connection_primary]) ) ;
          mqb_dispatch_action(mqb) ;
        }
      else
        bgp_connection_queue_del(connection) ;
    } ;
} ;

/*==============================================================================
 * Opening and closing Connections
 */

/*------------------------------------------------------------------------------
 * Open connection.
 *
 * Expects connection to either be newly created or recently closed.
 *
 * Sets:
 *
 *   * if accept() clears the session accept flag
 *   * sets the qfile and fd ready for use
 *   * clears except, notification and err
 *   * discards any open_state and notification
 *   * copies hold_timer_interval and keep_alive_timer_interval from session
 *
 * Expects:
 *
 *   * links to/from session to be set up (including ordinal)
 *   * timers to be initialised and unset
 *   * log and host to be set up
 *   * buffers to exist and all buffering to be set empty
 *   * pending queue to be empty
 *
 * Does not touch:
 *
 *   * state of the connection
 *
 * NB: requires the session to be LOCKED.
 */
extern void
bgp_connection_open(bgp_connection connection, int fd)
{
  bgp_session session = connection->session ;

  /* If this is the secondary connection, do not accept any more.       */
  if (connection->ordinal == bgp_connection_secondary)
    bgp_connection_disable_accept(connection) ;

  /* Set the file going                                                 */
  qps_add_file(bgp_nexus->selection, &connection->qf, fd, connection) ;

  /* Clear sundry state is clear                                        */
  connection->post    = bgp_fsm_null_event ;    /* no post event event  */

  connection->except  = bgp_session_null_event ;
  bgp_notify_free(&connection->notification) ;
  connection->err     = 0 ;                     /* so far, so good      */

  /* These accept NULL arguments                                        */
  connection->open_recv    = bgp_open_state_free(connection->open_recv) ;
  bgp_notify_free(&connection->notification) ;

  /* Copy the original hold_timer_interval and keepalive_timer_interval
   * Assume these have sensible initial values.
   *
   * These may be changed during the exchange of BGP OPEN messages.
   */
  connection->hold_timer_interval      = session->hold_timer_interval ;
  connection->keepalive_timer_interval = session->keepalive_timer_interval ;
} ;

/*------------------------------------------------------------------------------
 * Enable connection for accept()
 *
 */
extern void
bgp_connection_enable_accept(bgp_connection connection)
{
  connection->session->index_entry->accept = connection->session ;
} ;

/*------------------------------------------------------------------------------
 * Disable connection for accept()
 *
 */
extern void
bgp_connection_disable_accept(bgp_connection connection)
{
  connection->session->index_entry->accept = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Close connection.
 *
 *   * if there is an fd, close it
 *   * if qfile is active, remove it
 *   * forget any addresses
 *   * unset any timers
 *   * reset all buffering to empty
 *   * empties the pending queue -- destroying all messages
 *   * for secondary connection: disable accept
 *
 * The following remain:
 *
 *   * state of the connection
 *   * links to and from the session
 *   * the timers remain initialised (but unset)
 *   * the buffers remain (but reset)
 *   * logging and host string
 *   * any open_state that has been received
 *   * any notification sent/received
 *   * the exception state and any error
 *
 * Once closed, the only further possible actions are:
 *
 *   * bgp_connection_open()     -- to retry connection
 *
 *   * bgp_connection_free()     -- to finally discard
 *
 *   * bgp_connection_close()    -- can do this again
 *
 */
extern void
bgp_connection_close(bgp_connection connection)
{
  int fd ;

  /* close the qfile and any associate file descriptor                  */
  qps_remove_file(&connection->qf) ;
  fd = qps_file_unset_fd(&connection->qf) ;
  if (fd != fd_undef)
    shutdown(fd, SHUT_RDWR) ;

  /* If this is the secondary connection, do not accept any more.       */
  if (connection->ordinal == bgp_connection_secondary)
    bgp_connection_disable_accept(connection) ;

  /* forget any addresses                                               */
  if (connection->su_local != NULL)
    {
      sockunion_clear(connection->su_local) ;
      connection->su_local = NULL;
    }
  if (connection->su_remote != NULL)
    {
      sockunion_clear(connection->su_remote) ;
      connection->su_remote = NULL;
    }

  /* Unset all the timers                                               */
  qtimer_unset(&connection->hold_timer) ;
  qtimer_unset(&connection->keepalive_timer) ;

  /* Reset all buffering empty.                                         */
  stream_reset(connection->ibuf) ;
  stream_reset(connection->obuf) ;

  connection->read_pending  = 0 ;
  connection->read_header   = 0 ;
  connection->notification_pending = 0 ;

  connection->wbuff.p_in  = connection->wbuff.base ;
  connection->wbuff.p_out = connection->wbuff.base ;

  /* Empty out the pending queue and remove from connection queue       */
  mqueue_local_reset_keep(&connection->pending_queue) ;
  bgp_connection_queue_del(connection) ;
} ;

/*------------------------------------------------------------------------------
 * Close connection for reading and purge the write buffers.
 *
 * This is done when the connection is about to be fully closed, but need to
 * send a NOTIFICATION message before finally closing.
 *
 *   * if there is an fd, shutdown(, SHUT_RD) and disable the qfile for reading
 *   * reset all read buffering to empty
 *   * discard all output except any partially written message
 *   * empty the pending queue
 *
 * Can do this because the write buffer contains only complete BGP messages.
 *
 * This ensures the write buffer is not full, so NOTIFICATION message can
 * be written (at least as far as the write buffer).
 *
 * Everything else is left untouched.
 */
extern void
bgp_connection_part_close(bgp_connection connection)
{
  bgp_wbuffer wb = &connection->wbuff ;
  int         fd ;
  uint8_t*    p ;
  bgp_size_t  mlen ;

  /* close the qfile and any associate file descriptor                  */
  fd = qps_file_fd(&connection->qf) ;
  if (fd != fd_undef)
    {
      shutdown(fd, SHUT_RD) ;
      qps_disable_modes(&connection->qf, qps_read_mbit) ;
    } ;

  /* Reset all input buffering.                                        */
  stream_reset(connection->ibuf) ;

  connection->read_pending  = 0 ;
  connection->read_header   = 0 ;

  /* Reset obuf and purge wbuff.                                        */
  stream_reset(connection->obuf) ;

  connection->notification_pending = 0 ;

  if (wb->p_in != wb->p_out)    /* will be equal if buffer is empty     */
    {
      mlen = 0 ;
      p    = wb->base ;
      do                        /* Advance p until p + mlen > wb->p_out */
        {
          p += mlen ;
          mlen = bgp_msg_get_mlen(p) ;
        } while ((p + mlen) <= wb->p_out) ;

      if (p == wb->p_out)
        mlen = 0 ;              /* wb->p_out points at start of message */
      else
        memcpy(wb->base, p, mlen) ;

      wb->p_out = wb->base + (wb->p_out - p) ;
      wb->p_in  = wb->base + mlen ;
    }
  else
    wb->p_in = wb->p_out = wb->base ;

  /* Empty out the pending queue and remove from connection queue       */
  mqueue_local_reset_keep(&connection->pending_queue) ;
  bgp_connection_queue_del(connection) ;
} ;

/*==============================================================================
 * Writing to BGP connection -- once TCP connection has come up.
 *
 * All writing is done by preparing a BGP message in the "obuf" buffer,
 * and then calling bgp_connection_write().
 *
 * If possible, that is written away immediately.  If not, then no further
 * messages may be prepared until the buffer has been cleared.
 *
 * Write the contents of the "work" buffer.
 *
 * Returns true <=> able to write the entire buffer without blocking.
 */

static int bgp_connection_write_direct(bgp_connection connection,
                                                             struct stream* s) ;
static void bgp_connection_write_action(qps_file qf, void* file_info) ;

/*------------------------------------------------------------------------------
 * Write the contents of the given stream, if possible
 *
 * Writes everything or nothing.
 *
 * If the write buffer is empty, then will attempt to write directly to the
 * socket, buffering anything that cannot be sent immediately.  Any errors
 * encountered in this process generate an FSM event.
 *
 * In case it is relevant, identifies when the data has been written all the
 * way into the TCP buffer.
 *
 * Returns: 2 => written to TCP   -- it's gone          -- stream reset, empty
 *          1 => written to wbuff -- waiting for socket -- stream reset, empty
 *          0 => nothing written  -- insufficient space in wbuff
 *         -1 => failed           -- error event generated
 */
extern int
bgp_connection_write(bgp_connection connection, struct stream* s)
{
  bgp_wbuffer wb = &connection->wbuff ;

  if (bgp_write_buffer_empty(wb))
    {
      /* write buffer is empty -- attempt to write directly     */
      return bgp_connection_write_direct(connection, s) ;
    } ;

  /* Write nothing if cannot write everything                   */
  if (!bgp_write_buffer_can(wb, stream_pending(s)))
    return 0 ;

  /* Transfer the obuf contents to the write buffer.            */
  wb->p_in = stream_transfer(wb->p_in, s, wb->limit) ;

  return 1 ;    /* written as far as the write buffer           */
} ;

/*------------------------------------------------------------------------------
 * The write buffer is empty -- so try to write stream directly.
 *
 * If cannot empty the stream directly to the TCP buffers, transfer it to to
 * the write buffer, and enable the qpselect action.
 * (This is where the write buffer is allocated, if it hasn't yet been.)
 *
 * Either way, the stream is cleared and can be reused (unless failed).
 *
 * Returns: 2 => written to TCP   -- it's gone          -- stream reset, empty
 *          1 => written to wbuff -- waiting for socket -- stream reset, empty
 *         -1 => failed           -- error event generated
 */
enum { bgp_wbuff_size = BGP_MSG_MAX_L * 10 } ;

static int
bgp_connection_write_direct(bgp_connection connection, struct stream* s)
{
  int ret ;

  ret = stream_flush_try(s, qps_file_fd(&connection->qf)) ;

  if (ret == 0)
    return 2 ;          /* Done: wbuff and stream are empty         */

  else if (ret > 0)
    {
      bgp_wbuffer wb = &connection->wbuff ;

      /* Partial write -- set up buffering, if required.            */
      if (wb->base == NULL)
        bgp_write_buffer_init_new(wb, bgp_wbuff_size) ;

      /* Transfer *entire* message to staging buffer                */
      wb->p_in = stream_transfer(wb->base, s, wb->limit) ;

      wb->p_out = wb->p_in - ret ;          /* output from here     */

      /* Must now be enabled to write                               */
      qps_enable_mode(&connection->qf, qps_write_mnum,
                                        bgp_connection_write_action) ;

      return 1 ;        /* Done: wbuff is not empty -- stream is    */
    } ;

  /* write failed -- signal error and return failed                 */
  bgp_fsm_io_error(connection, errno) ;

  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Write Action for bgp connection.
 *
 * Empty the write buffer if we can.
 *
 * If empties that, disable write mode, then:
 *
 *   -- if notification is pending, then generate a notification sent event
 *
 *   -- otherwise: place connection on the connection queue, so can start to
 *      flush out anything on the connection's pending queue and/or send an
 *      XON message to the Peering Engine.
 *
 * If empty out everything, disable write mode.
 *
 * If encounter an error, generate TCP_fatal_error event.
 */
static void
bgp_connection_write_action(qps_file qf, void* file_info)
{
  bgp_connection connection = file_info ;
  bgp_wbuffer wb = &connection->wbuff ;
  int have ;
  int ret ;

  /* Try to empty the write buffer.                                     */
  have = wb->p_out - wb->p_in ;
  while (have != 0)
    {
      ret = write(qps_file_fd(qf), wb->p_out, have) ;
      if      (ret > 0)
        {
          wb->p_out += ret ;
          have      -= ret ;
        }
      else if (ret < 0)
        {
          ret = errno ;
          if (ret == EINTR)
            continue ;

          if ((ret != EAGAIN) && (ret != EWOULDBLOCK))
            bgp_fsm_io_error(connection, errno) ;

          return ;
        } ;
    } ;

  /* Buffer is empty -- reset it and disable write mode                 */
  wb->p_out = wb->p_in = wb->base ;

  qps_disable_modes(&connection->qf, qps_write_mbit) ;

  /* If waiting to send NOTIFICATION, just did it.                      */
  /* Otherwise: is writable again -- so add to connection_queue         */
  if (connection->notification_pending)
    bgp_fsm_event(connection, bgp_fsm_Sent_NOTIFICATION_message) ;
  else
    bgp_connection_queue_add(connection) ;
} ;

/*==============================================================================
 * Reading from BGP connection -- once the TCP connection has come up.
 *
 * Nothing is read directly -- all reading is qpselect driven.
 *
 * Sets the qfile readable -- and leaves it there for the duration.
 *
 * TODO: implement some read flow control ??
 */

static void
bgp_connection_read_action(qps_file qf, void* file_info) ;

/*------------------------------------------------------------------------------
 * Enable reading on the given connection.
 */
extern void
bgp_connection_read_enable(bgp_connection connection)
{
  qps_enable_mode(&connection->qf, qps_read_mnum, bgp_connection_read_action) ;
} ;

/*------------------------------------------------------------------------------
 * Read Action for BGP connection
 *
 * Reads one BGP message into the ibuf and dispatches it.
 *
 * Performs the checks on the BGP message header:
 *
 *   * Marker is all '1's
 *   * Length is <= BGP_MSG_MAX_L
 *   * Type   is OPEN/UPDATE/NOTIFICATION/KEEPALIVE
 *
 */
static void
bgp_connection_read_action(qps_file qf, void* file_info)
{
  bgp_connection connection = file_info ;
  int want ;
  int ret ;

  /* If nothing pending for partial packet, start reading new one.      */

  want = connection->read_pending ;
  if (want == 0)
    {
      want = BGP_MH_HEAD_L ;
      stream_reset(connection->ibuf) ;
      connection->read_header = 1 ;
    } ;

  /* Loop to read entire BGP message into ibuf.
   *
   * On error or "EOF", raises suitable FSM events and returns.
   *
   * If cannot read entire message, sets new pending count and returns.
   *
   * Exits loop iff completes a BGP message.
   */
  while (1)
    {
      ret = stream_read_unblock(connection->ibuf, qps_file_fd(&connection->qf),
                                                                         want) ;
      if (ret >= 0)
        {
          want -= ret ;
          if (want != 0)
            {
              connection->read_pending = want ;
              return ;                  /* must wait for the rest       */
            } ;

          if (!connection->read_header)
            break ;                     /* got complete message         */

          connection->read_header = 0 ; /* got complete header          */

          want = bgp_msg_check_header(connection) ;
                                        /* returns balance of message   */
          if (want < 0)
            return ;                    /* failed in header check       */
        }
      else
        {
          bgp_fsm_io_error(connection, (ret == -1) ? errno : 0) ;
          return ;
        } ;
    } ;

  /* Deal with the BGP message.  MUST remove from ibuf before returns ! */
  bgp_msg_dispatch(connection) ;

  /* Ready to read another message                                      */
  connection->read_pending = 0 ;
} ;


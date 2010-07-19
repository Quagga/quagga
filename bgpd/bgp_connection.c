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
#include "lib/sockunion.h"
#include "lib/list_util.h"

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
 * Each BGP Session is associated with at most two BGP Connections, a primary
 * and a secondary.  The primary starts as the connect() connection, and the
 * secondary as the acccept().  One will be dropped before either connection
 * reaches sEstablished state, and the remaining connection becomes the primary.
 *
 * The bgp_connection structure is private to the BGP Engine, and is accessed
 * directly, without the need for any mutex.
 *
 * Each connection is closely tied to its parent bgp_session.  The bgp_session
 * is shared between the Routeing Engine and the BGP Engine, and therefore
 * access is subject to the bgp_session's mutex.
 */

/*==============================================================================
 * The connection queue and the connection's pending queue.
 *
 * When it is no longer possible to write the the connection's write buffer,
 * any mqueue messages that cannot be dealt with are queued on the connection's
 * pending queue.  So when the BGP Engine's mqueue is processed, the messages
 * are either dealt with, or queued in the relevant connection.
 *
 * When the connection's write buffer empties, the connection is placed on the
 * BGP Engine's connection queue.
 *
 * The connection queue is processed as the highest priority action in the
 * BGP Engine, at which point as many of the items on each connection's
 * pending queue as possible will be processed.
 *
 * The connection_queue is managed as a circular list of connections.  The
 * connection_queue variable points at the next to be processed.
 */

static bgp_connection bgp_connection_queue ;  /* BGP Engine connection queue */

static bgp_connection bgp_connection_list ;   /* list of known connections   */

enum { CUT_LOOSE_LOCK_COUNT = 1000 } ;

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
static void bgp_write_buffer_free(bgp_wbuffer wb) ;

/*------------------------------------------------------------------------------
 * Initialise connection structure -- allocate if required.
 *
 * Copies information required by the connection from the parent session.
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
   *   * half_open                not half open
   *   * next                     NULL -- not on the connection queue
   *   * prev                     NULL -- not on the connection queue
   *   * follow_on                bgp_fsm_null_event
   *   * exception                bgp_session_null_event
   *   * fsm_active               not active
   *   * notification             NULL -- none received or sent
   *   * err                      no error, so far
   *   * su_local                 NULL -- no address, yet
   *   * su_remote                NULL -- no address, yet
   *   * hold_timer_interval      none -- set when connection is opened
   *   * keepalive_timer_interval none -- set when connection is opened
   *   * as4                      not AS4 conversation
   *   * route_refresh_pre        not pre-RFC ROUTE-REFRESH
   *   * orf_prefix_pre           not pre-RFC ORF by prefix
   *   * read_pending             nothing pending
   *   * read_header              not reading header
   *   * msg_type                 none -- set when reading message
   *   * msg_size                 none -- set when reading message
   *   * notification_pending     nothing pending
   *   * wbuff                    all pointers NULL -- empty but not writable
   */
  confirm(bgp_fsm_sInitial       == 0) ;
  confirm(bgp_fsm_null_event     == 0) ;
  confirm(bgp_session_null_event == 0) ;

  /* Put on the connections that exist list                             */
  sdl_push(bgp_connection_list, connection, exist) ;

  /* Link back to session, point at its mutex and point session here    */
  connection->session    = session ;
  connection->p_mutex    = &session->mutex ;
  connection->lock_count = 0 ;  /* no question about it         */

  connection->paf = AF_UNSPEC ;

  connection->ordinal  = ordinal ;
  connection->accepted = (ordinal == bgp_connection_secondary) ;

  session->connections[ordinal] = connection ;

  /* qps_file structure                                                 */
  connection->qf = qps_file_init_new(NULL, NULL) ;

  /* Initialise all the timers                                          */
  connection->hold_timer      = qtimer_init_new(NULL, bgp_nexus->pile,
                                                             NULL, connection) ;
  connection->keepalive_timer = qtimer_init_new(NULL, bgp_nexus->pile,
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
 * Cut connection free from session.
 *
 * NB: will release any lock on the session.  The bumps the lock count up
 *     so that will neither lock nor unlock the session again.
 *
 *     It's only necessary to bump the count by 1, because all locks must be
 *     exactly balanced by unlocks.  However, adding a big number makes this
 *     stand out.
 */
extern void
BGP_CONNECTION_SESSION_CUT_LOOSE(bgp_connection connection)
{
  if (connection->session != NULL)
    {
      if (connection->lock_count != 0)
        qpt_mutex_unlock(connection->p_mutex) ;

      connection->lock_count += CUT_LOOSE_LOCK_COUNT ;

      connection->session->connections[connection->ordinal] = NULL ;
      connection->session = NULL ;
      connection->p_mutex = NULL ;
    } ;
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

  if (connection->host != NULL)
    XFREE(MTYPE_BGP_PEER_HOST, connection->host) ;
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
      assert(session->connections[bgp_connection_primary] == NULL) ;
      session->connections[connection->ordinal] = NULL ;
      connection->ordinal = bgp_connection_primary ;
      session->connections[connection->ordinal] = connection ;
    } ;

  assert(session->connections[bgp_connection_secondary] == NULL) ;

  /* Move the open_state to the session.
   * Change the connection host to drop the primary/secondary distinction.
   * Copy the negotiated hold_timer_interval and keepalive_timer_interval
   * Copy the su_local and su_remote
   */
  bgp_open_state_set_mov(&session->open_recv, &connection->open_recv) ;

  if (connection->host != NULL)
    XFREE(MTYPE_BGP_PEER_HOST, connection->host) ;
  bgp_connection_init_host(connection, "") ;

  session->hold_timer_interval        = connection->hold_timer_interval ;
  session->keepalive_timer_interval   = connection->keepalive_timer_interval ;

  session->as4                        = connection->as4 ;
  session->route_refresh_pre          = connection->route_refresh ;
  session->orf_prefix_pre             = connection->orf_prefix ;

  sockunion_set_dup(&session->su_local,  connection->su_local) ;
  sockunion_set_dup(&session->su_remote, connection->su_remote) ;
} ;

/*------------------------------------------------------------------------------
 * Exit connection
 *
 * Make sure the connection is closed, then queue it to be reaped.
 *
 * When BGP Engine gets round to it, will free the structure.  This avoids
 * freeing the connection structure somewhere inside the FSM, and having to
 * cope with the possibility of having dangling references to it.
 *
 * In fact, the connection may be set to be reaped before the FSM has cut it
 * loose from the session -- so the connection may still be active inside the
 * FSM when this is called.
 */
extern void
bgp_connection_exit(bgp_connection connection)
{
  bgp_connection_close_down(connection) ;       /* make sure    */

  assert(connection->state == bgp_fsm_sStopping) ;

  bgp_connection_queue_add(connection) ;
} ;

/*------------------------------------------------------------------------------
 * Free connection.
 *
 * Connection must be Stopping -- no longer attached to a session.
 *
 * This is done in the BGP Engine connection queue handling -- so that the
 * structure is reaped once there is no chance of any dangling pointers to it.
 */
static void
bgp_connection_free(bgp_connection connection)
{
  assert(   (connection->state    == bgp_fsm_sStopping)
         && (connection->session  == NULL)
         && ( (connection->lock_count == 0) ||
              (connection->lock_count == CUT_LOOSE_LOCK_COUNT) )   ) ;

  /* Make sure is closed, so no active file, no timers, pending queue is empty,
   * not on the connection queue, etc.
   */
  bgp_connection_close_down(connection) ;

  /* Free any components which still exist                              */
  connection->qf              = qps_file_free(connection->qf) ;
  connection->hold_timer      = qtimer_free(connection->hold_timer) ;
  connection->keepalive_timer = qtimer_free(connection->hold_timer) ;

  bgp_notify_unset(&connection->notification) ;
  bgp_open_state_unset(&connection->open_recv) ;
  sockunion_unset(&connection->su_local) ;
  sockunion_unset(&connection->su_remote) ;
  if (connection->host != NULL)
    XFREE(MTYPE_BGP_PEER_HOST, connection->host) ;
  stream_free(connection->ibuf) ;
  stream_free(connection->obuf) ;
  bgp_write_buffer_free(&connection->wbuff) ;

  /* Free the body                                                      */
  sdl_del(bgp_connection_list, connection, exist) ;

  XFREE(MTYPE_BGP_CONNECTION, connection) ;
} ;

/*------------------------------------------------------------------------------
 * Terminate all known connections.
 *
 * TODO: for bringing the BGP Engine to a dead halt.
 *
 * Problem: can it be assumed that all sessions have been closed ?
 *
 *          if not... how are all the connections to be pursuaded to adopt
 *          an appropriate posture ?
 */


/*------------------------------------------------------------------------------
 * If required, allocate new write buffer.
 * Initialise pointers empty and writable.
 *
 * NB: structure was zeroised the enclosing connection was initialised.
 *     Buffer may have been allocated since then.
 */
static void
bgp_write_buffer_init(bgp_wbuffer wb, size_t size)
{
  if (wb->base == NULL)
    {
      wb->base  = XMALLOC(MTYPE_STREAM_DATA, size) ;
      wb->limit = wb->base + size ;
    } ;

  bgp_write_buffer_reset(wb) ;
} ;

/*------------------------------------------------------------------------------
 * Free any write buffer
 */
static void
bgp_write_buffer_free(bgp_wbuffer wb)
{
  if (wb->base != NULL)
    XFREE(MTYPE_STREAM_DATA, wb->base) ;        /* sets wb->base = NULL */

  wb->p_in = wb->p_out = wb->limit = wb->base;
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
 * Process each connection in turn, dealing with one item on each one's pending
 * queue.  Dealing with the item will either remove it from the connection's
 * pending queue (success) or remove connection from the pending queue.
 *
 * This is also where connections come to die.
 *
 * Returns: 0 => nothing to do
 *          1 => dealt with one or more queued bits of work
 */
extern int
bgp_connection_queue_process(void)
{
  mqueue_block mqb ;

  if (bgp_connection_queue == NULL)
    return 0 ;

  while (bgp_connection_queue != NULL)
    {
      /* select the first in the queue, and step to the next            */
      bgp_connection connection = bgp_connection_queue ;
      bgp_connection_queue = connection->next ;

      /* Reap the connection if it is now stopped.                      */
      if (connection->state == bgp_fsm_sStopping)
        {
          bgp_connection_free(connection) ; /* removes from connection queue */
          continue ;
        } ;

      /* Process next item on connection's pending queue                */
      mqb = mqueue_local_dequeue(&connection->pending_queue) ;
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
        } ;

      /* If head is unchanged, then no more to do now.                  */
      if (mqb == mqueue_local_head(&connection->pending_queue))
        bgp_connection_queue_del(connection) ;
    } ;

  return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Add given message block to the given connection's pending queue
 *
 * If mqb is not already pending, add it at the tail and mark it pending.
 *
 * If is already pending, then is being put back onto the queue, so put it
 * at the head.
 *
 * In any case, remove the connection from the BGP Engine connection queue (if
 * there) -- there is nothing more to be done for the connection for the time
 * being.
 */
extern void
bgp_connection_add_pending(bgp_connection connection, mqueue_block mqb,
                                                     bgp_connection* is_pending)
{
  if (*is_pending == NULL)
    {
      mqueue_local_enqueue(&connection->pending_queue, mqb) ;
      *is_pending = connection ;
    }
  else
    {
      dassert(*is_pending == connection) ;
      mqueue_local_enqueue_head(&connection->pending_queue, mqb) ;
    } ;

  bgp_connection_queue_del(connection) ;
} ;

/*==============================================================================
 * Opening and closing Connections
 */

/*------------------------------------------------------------------------------
 * Open connection.
 *
 * Expects connection to either be newly created or recently closed.
 *
 * For connect() connections this is done at connect() time, so before any
 * connection comes up.
 *
 * For accept() connections this is done at accept() time, so when the
 * connection comes up.
 *
 * The file is disabled in all modes.
 *
 * To complete the process must bgp_connection_start(), which resets the write
 * buffer (allocating if required), and ensures that all is ready to read/write.
 *
 * Resets:
 *
 *   * closes any file that may be lingering   (should never be)
 *   * reset all stream buffers to empty       (should already be)
 *   * set write buffer unwritable
 *   * clears half_open
 *
 * Sets:
 *
 *   * if secondary connection, turn off accept()
 *   * sets the qfile and fd ready for use -- disabled in all modes
 *   * clears err -- must be OK so far
 *   * discards any open_state
 *   * copies hold_timer_interval and keep_alive_timer_interval from session
 *
 * Expects:
 *
 *   * links to/from session to be set up (including ordinal)
 *   * timers to be initialised
 *   * log and host to be set up
 *   * stream buffers to exist
 *
 * Does not touch:
 *
 *   * state of the connection (including exception and follow-on event)
 *   * timers -- FSM looks after those
 *
 * NB: nothing can be written until bgp_connection_start() has been called.
 *
 * NB: requires the session to be LOCKED.
 */
extern void
bgp_connection_open(bgp_connection connection, int fd, int family)
{
  bgp_session session = connection->session ;

  /* Make sure that there is no file and that buffers are clear, etc.   */
  /* If this is the secondary connection, do not accept any more.       */
  bgp_connection_close(connection) ;    /* FSM deals with timers        */

  /* Set the file going                                                 */
  qps_add_file(bgp_nexus->selection, connection->qf, fd, connection) ;

  connection->err     = 0 ;                     /* so far, so good      */

  bgp_open_state_unset(&connection->open_recv) ;

  /* Note the address family for the socket.
   *
   * This is the real family -- so is IPv6 independent of whether one or
   * both addresses are actually mapped IPv4.
   */
  connection->paf = family ;

  /* Copy the original hold_timer_interval and keepalive_timer_interval
   * Assume these have sensible initial values.
   *
   * These may be changed during the exchange of BGP OPEN messages.
   */
  connection->hold_timer_interval      = session->hold_timer_interval ;
  connection->keepalive_timer_interval = session->keepalive_timer_interval ;
} ;

/*------------------------------------------------------------------------------
 * Start connection which has just come up -- connect() or accept()
 *
 * Copy the local and remote addresses (IPv4 mapped IPv6 addresses appear as
 * IPv4 addresses).
 *
 * Make sure now have a write buffer, and set it empty and writable.
 */
extern void
bgp_connection_start(bgp_connection connection, union sockunion* su_local,
                                                union sockunion* su_remote)
{
  sockunion_set_dup(&connection->su_local,  su_local) ;
  sockunion_set_dup(&connection->su_remote, su_remote) ;

  bgp_write_buffer_init(&connection->wbuff, bgp_wbuff_size) ;
} ;

/*------------------------------------------------------------------------------
 * Stop connection
 *
 *   * reset stream buffers
 *   * empty out any pending queue
 *   * remove from the BGP Engine connection queue, if there
 *   * clear session->active flag, so will not process any more messages
 *     that expect some message to be sent.
 *   * no notification pending (yet)
 *
 * If required:
 *
 *   * set write buffer unwritable
 *   * disable file in write mode
 *
 * NB: requires the session to be LOCKED.
 */
static void
bgp_connection_stop(bgp_connection connection, int stop_writer)
{
  /* Reset all stream buffering empty.                                  */
  stream_reset(connection->ibuf) ;
  stream_reset(connection->obuf) ;

  connection->read_pending  = 0 ;
  connection->read_header   = 0 ;
  connection->notification_pending = 0 ;

  /* Empty out the pending queue and remove from connection queue       */
  mqueue_local_reset_keep(&connection->pending_queue) ;
  bgp_connection_queue_del(connection) ;

  /* If required: set write buffer *unwritable* (and empty).            */
  if (stop_writer)
    bgp_write_buffer_unwritable(&connection->wbuff) ;
} ;

/*------------------------------------------------------------------------------
 * Enable connection/session for accept()
 *
 * NB: requires the session to be LOCKED
 */
extern void
bgp_connection_enable_accept(bgp_connection connection)
{
  bgp_session session = connection->session ;

  assert(connection->ordinal == bgp_connection_secondary) ;
  assert((session != NULL) && (session->active)) ;

  session->accept = true ;
} ;

/*------------------------------------------------------------------------------
 * Disable connection for accept() -- assuming still have session !
 *
 * NB: requires the session to be LOCKED
 */
extern void
bgp_connection_disable_accept(bgp_connection connection)
{
  if (connection->session != NULL)
    connection->session->accept = false ;
} ;

/*------------------------------------------------------------------------------
 * See if there is a connection which is ready to accept()
 *
 * Note that if there *is* a connection, then the session is active, and is not
 * subject to the whim of the Routing Engine -- in particular it cannot be
 * deleted !
 *
 * NB: this is *only* used in the BGP Engine.  The session->active and
 *     session->accept flags are private variables, only set by the BGP Engine.
 *
 * NB: this is called under the Peer Index Mutex.  The Routing Engine never
 *     deletes sessions while it holds the Peer Index Mutex, nor when the
 *     session->active is true.
 *
 *     Only returns a connection if session->active -- so safe.
 */
extern bgp_connection
bgp_connection_query_accept(bgp_session session)
{
  bgp_connection connection ;

  if ((session != NULL) && session->active && session->accept)
    {
      connection = session->connections[bgp_connection_secondary] ;
      assert(connection != NULL) ;
    }
  else
    {
      connection = NULL ;
    } ;

  return connection ;
} ;

/*------------------------------------------------------------------------------
 * Close connection.
 *
 *   * if there is an fd, close it
 *   * if qfile is active, remove it
 *   * forget any addresses
 *   * reset all stream buffers to empty
 *   * reset write buffer to unwritable
 *   * empties the pending queue -- destroying all messages
 *
 *   * for secondary connection: disable accept
 *   * clear half_open
 *
 *   * if required: unset all timers
 *
 * The following remain:
 *
 *   * state of the connection
 *   * links to and from the session
 *   * the timers remain initialised (but may have been unset)
 *   * the buffers remain (but reset)
 *   * logging and host string
 *   * any open_state that has been received
 *   * any notification sent/received
 *   * the exception state and any error
 *
 * Once closed, the only further possible actions are:
 *
 *   * bgp_connection_open()       -- to retry connection
 *
 *   * bgp_connection_free()       -- to finally discard
 *
 *   * bgp_connection_full_close() -- can do this again
 *
 * NB: requires the session to be LOCKED.
 */
extern void
bgp_connection_full_close(bgp_connection connection, int unset_timers)
{
  int fd ;

  /* Close connection's file, if any.                                   */
  qps_remove_file(connection->qf) ;

  fd = qps_file_unset_fd(connection->qf) ;
  if (fd != fd_undef)
    close(fd) ;

  /* If required, unset the timers.                                     */
  if (unset_timers)
    {
      qtimer_unset(connection->hold_timer) ;
      qtimer_unset(connection->keepalive_timer) ;
    } ;

  /* If this is the secondary connection, do not accept any more.       */
  if (connection->ordinal == bgp_connection_secondary)
    bgp_connection_disable_accept(connection) ;

  connection->half_open = 0 ;

  /* forget any addresses                                               */
  sockunion_unset(&connection->su_local) ;
  sockunion_unset(&connection->su_remote) ;

  /* Bring connection to a stop.                                        */
  bgp_connection_stop(connection, 1) ;
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
 *
 * Returns:  1 => OK, ready to send NOTIFICATION now
 *           0 => no file descriptor => no chance of sending NOTIFICATION
 *
 * NB: requires the session to be LOCKED.
 */
extern int
bgp_connection_part_close(bgp_connection connection)
{
  bgp_session session = connection->session ;
  bgp_wbuffer wb = &connection->wbuff ;
  int         fd ;
  uint8_t*    p ;
  bgp_size_t  mlen ;

  /* Check that have a usable file descriptor                           */
  fd = qps_file_fd(connection->qf) ;

  if (fd == fd_undef)
    return 0 ;

  /* Shutdown the read side of this connection                          */
  shutdown(fd, SHUT_RD) ;
  qps_disable_modes(connection->qf, qps_read_mbit) ;

  /* Stop all buffering activity, except for write buffer.              */
  bgp_connection_stop(connection, 0) ;

  /* Turn off session->active (if still attached).                      */
  if (session != NULL)
    session->active = false ;

  /* Purge wbuff of all but current partly written message (if any)     */
  if (wb->p_in != wb->p_out)    /* will be equal if buffer is empty     */
    {
      passert(wb->p_out < wb->p_in) ;
      mlen = 0 ;
      p    = wb->base ;
      do                        /* Advance p until p + mlen > wb->p_out */
        {
          p += mlen ;
          mlen = bgp_msg_get_mlen(p, wb->p_in) ;    /* checks pointers  */
        } while ((p + mlen) <= wb->p_out) ;

      if (p == wb->p_out)
        mlen = 0 ;              /* wb->p_out points at start of message */
      else
        memcpy(wb->base, p, mlen) ;

      wb->p_out = wb->base + (wb->p_out - p) ;
      wb->p_in  = wb->base + mlen ;
    }
  else
    bgp_write_buffer_reset(wb) ;

  /* OK -- part closed, ready to send NOTIFICATION              */
  return 1 ;
} ;

/*==============================================================================
 * Writing to BGP connection -- once TCP connection has come up.
 *
 * Nothing is written directly -- all writing is qpselect driven.
 *
 * All writing is done by preparing a BGP message in a stream buffer,
 * and then calling bgp_connection_write().  The contents of the stream buffer
 * are transferred to the connection's write buffer.
 *
 * Returns true <=> able to write the entire buffer without blocking.
 */

static void bgp_connection_write_action(qps_file qf, void* file_info) ;

/*------------------------------------------------------------------------------
 * Write the contents of the given stream
 *
 * Writes everything or FATAL error.
 *
 * Returns: 1 => written to wbuff -- stream reset, empty
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 */
extern int
bgp_connection_write(bgp_connection connection, struct stream* s)
{
  bgp_wbuffer wb = &connection->wbuff ;

  /* FATAL error if cannot write everything.                    */
  if (bgp_write_buffer_cannot(wb, stream_pending(s)))
    zabort("Write buffer does not have enough room") ;

  /* If buffer is empty, enable write mode                      */
  if (bgp_write_buffer_empty(wb))
    qps_enable_mode(connection->qf, qps_write_mnum,
                                                bgp_connection_write_action) ;

  /* Transfer the obuf contents to the write buffer.            */
  wb->p_in = stream_transfer(wb->p_in, s, wb->limit) ;

  return 1 ;    /* written as far as the write buffer           */
} ;

/*------------------------------------------------------------------------------
 * Write Action for bgp connection.
 *
 * Empty the write buffer if we can.
 *
 * If empties that, disable write mode, then:
 *
 *   -- if notification is pending, generate a notification sent event
 *
 *   -- otherwise: place connection on the connection queue, so can start to
 *      flush out anything on the connection's pending queue.
 *
 * If encounter an error, generate TCP_fatal_error event, forcing buffer
 * empty but unwritable.
 */
static void
bgp_connection_write_action(qps_file qf, void* file_info)
{
  bgp_connection connection = file_info ;
  bgp_wbuffer wb = &connection->wbuff ;
  int have ;
  int ret ;

  /* Try to empty the write buffer.                                     */
  have = bgp_write_buffer_pending(wb) ;
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
            {
              bgp_write_buffer_unwritable(wb) ;
              bgp_fsm_io_error(connection, errno) ;
            } ;
          return ;
        } ;
    } ;

  /* Buffer is empty -- reset it and disable write mode                 */
  bgp_write_buffer_reset(wb) ;

  qps_disable_modes(connection->qf, qps_write_mbit) ;

  /* If waiting to send NOTIFICATION, just did it.                      */
  /* Otherwise: is writable again -- so add to connection_queue         */
  if (connection->notification_pending)
    bgp_fsm_notification_sent(connection) ;
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
  qps_enable_mode(connection->qf, qps_read_mnum, bgp_connection_read_action) ;
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
      ret = stream_read_nonblock(connection->ibuf, qps_file_fd(connection->qf),
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
          if (want == 0)
            break ;
        }
      else
        {
          bgp_fsm_io_error(connection, (ret == -1) ? errno : 0) ;
          return ;
        } ;
    } ;

  /* Deal with the BGP message.  MUST remove from ibuf before returns !
   *
   * NB: if the session pointer is NULL, that means the connection has been
   *     cut from the session, so no point dealing with the message.
   *
   * NB: if something goes wrong while processing the message,
   *
   * NB: size passed is the size of the *body* of the message.
   */
   if (connection->session != NULL)      /* don't bother if session gone ! */
     {
       BGP_CONNECTION_SESSION_LOCK(connection) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<*/

       connection->msg_func(connection, connection->msg_body_size) ;

       BGP_CONNECTION_SESSION_UNLOCK(connection) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>*/
    } ;

  /* Ready to read another message                                      */
  connection->read_pending = 0 ;
} ;

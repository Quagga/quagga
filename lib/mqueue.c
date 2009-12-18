/* Message Queue data structure -- functions
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <string.h>

#include "memory.h"
#include "mqueue.h"
#include "zassert.h"

/*==============================================================================
 * These message queues are designed for inter-qpthread communication.
 *
 * A message queue carries messages from one or more qpthreads to one or more
 * other qpthreads.
 *
 * If !qpthreads_enabled, then a message queue hold messages for the program
 * to consume later.  There are never any waiters.  Timeouts are ignored.
 *
 * A message queue has one ordinary priority queue and one high priority
 * queue.
 *
 * There are four types of queue, depending on how qpthreads wait and how they
 * are woken up:
 *
 *   mqt_cond_unicast     -- wait on condition variable, one waiter kicked
 *   mqt_cond_broadcast   -- wait on condition variable, all waiters kicked
 *   mqt_signal_unicast   -- wait for signal,            one waiter kicked
 *   mqt_signal_broadcast -- wait for signal,            all waiters kicked
 *
 * For condition variables there is a timeout mechanism so that waiters
 * are woken up at least every now and then.  The message queue maintains
 * a timeout time and a timeout interval.  The timeout time is a qtime_mono_t
 * time -- so is monotonic.
 *
 * When waiting, an explicit timeout may be given, otherwise the stored timeout
 * will be used:
 *
 *   wait until explicit/stored timeout
 *   if times out and there is a stored interval:
 *     new stored timeout = stored timeout + stored interval
 *     if new stored timeout < time now
 *       new stored timeout = time now + stored interval
 *
 * Left to its own devices, this will produce a regular timeout every interval,
 * assuming that the queue is waited on within the interval.  Otherwise the
 * "clock" will slip.
 *
 * There is a default timeout period.  The period may be set "infinite".
 *
 * For waiters kicked by signal, the wait does not occur within the message
 * queue code, but the need for a signal is recorded in the message queue.
 *
 * Messages take the form of a small block of information which contains:
 *
 *   * flags     -- used by the message handler
  *
 *   * action    -- void action(mqueue_block)   message dispatch
 *   * arg0      -- *void                    )  standard arguments
 *   * arg1      -- *void/uintptr_t/intptr_t )
 *
 * There are set/get functions for action/arg0/arg1 -- users should not poke
 * around inside the structure.
 *
 * To send a message, first allocate a message block (see mqb_init_new),
 * then fill in the arguments and enqueue it.
 *
 * For specific revoke, arg0 is assumed to identify the messages to be
 * revoked.
 *
 */

/*==============================================================================
 * Initialisation etc. for Message Queues.
 *
 * TODO: how to shut down a message queue... for reset/exit ?
 */

/* Initialise new Message Queue, if required (mq == NULL) allocating it.
 *
 * For mqt_cond_xxx type queues, sets the default timeout interval and the
 * initial timeout time to now + that interval.
 *
 * NB: once any message queue has been enabled, it is TOO LATE to enable
 *     qpthreads.
 */

mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type)
{
  if (mq == NULL)
    mq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(struct mqueue_queue)) ;
  else
    memset(mq, 0, sizeof(struct mqueue_queue)) ;

  if (qpt_freeze_qpthreads_enabled())
    qpt_mutex_init_new(&mq->mutex, qpt_mutex_quagga) ;

  /* head, tail and tail_priority set NULL already              */
  /* waiters set zero already                                   */

  mq->type = type ;
  switch (type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
        if (qpthreads_enabled)
          qpt_cond_init_new(&mq->kick.cond.wait_here, qpt_cond_quagga) ;
        if (MQUEUE_DEFAULT_INTERVAL != 0)
          {
            mq->kick.cond.interval = MQUEUE_DEFAULT_INTERVAL ;
            mq->kick.cond.timeout  = qt_get_monotonic()
                                                     + MQUEUE_DEFAULT_INTERVAL ;
          } ;
        break;

      case mqt_signal_unicast:
      case mqt_signal_broadcast:
        /* head/tail pointers set NULL already  */
        break;

      default:
        zabort("Invalid mqueue queue type") ;
    } ;

  return mq ;
} ;

/* Set new timeout interval (or unset by setting <= 0)
 *
 * Sets the next timeout to be the time now + new interval (or never).
 *
 * This is a waste of time if !qpthreads_enabled, but does no harm.  The
 * timeout is ignored.
 */
void
mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval)
{
  qpt_mutex_lock(&mq->mutex) ;

  dassert( (mq->type == mqt_cond_unicast) ||
           (mq->type == mqt_cond_broadcast) ) ;

  mq->kick.cond.interval = interval ;
  mq->kick.cond.timeout  = (interval > 0) ? qt_add_monotonic(interval)
                                          : 0 ;
  qpt_mutex_unlock(&mq->mutex) ;
} ;

/*==============================================================================
 * Message Block memory management.
 *
 * Allocates message_block structures in lots of 256.  Uses first message_block
 * in each lot to keep track of the lots.
 *
 * mqueue_initialise MUST be called before the first message block is allocated.
 */

static pthread_mutex_t  mqb_mutex ;

#define MQB_LOT_SIZE  256

static mqueue_block mqb_lot_list  = NULL ;
static mqueue_block mqb_free_list = NULL ;

static mqueue_block mqueue_block_new_lot(void) ;

/* Initialise message block (allocate if required) and set action and arg0.
 */
mqueue_block
mqb_init_new(mqueue_block mqb, mqueue_action action, void* arg0)
{
  if (mqb == NULL)
    {
      qpt_mutex_lock(&mqb_mutex) ;

      mqb = mqb_free_list ;
      if (mqb == NULL)
        mqb = mqueue_block_new_lot() ;

      mqb_free_list = mqb->next ;

      qpt_mutex_unlock(&mqb_mutex) ;
    } ;

  memset(mqb, 0, sizeof(struct mqueue_block)) ;

  mqb->action  = action ;
  mqb->arg0    = arg0 ;

  return mqb ;
} ;

/* Free message block when done with it.
 */
void
mqb_free(mqueue_block mqb)
{
  qpt_mutex_lock(&mqb_mutex) ;

  mqb->next = mqb_free_list ;
  mqb_free_list = mqb ;

  qpt_mutex_unlock(&mqb_mutex) ;
} ;

/* Make a new lot of empty message_block structures.
 *
 * NB: caller MUST hold the mqb_mutex.
 */
static mqueue_block
mqueue_block_new_lot(void)
{
  mqueue_block first, last, this ;

  mqueue_block new = XCALLOC(MTYPE_MQUEUE_BLOCKS,
                                      SIZE(struct mqueue_block, MQB_LOT_SIZE)) ;
  first = &new[1] ;
  last  = &new[MQB_LOT_SIZE - 1] ;

  new->next = mqb_lot_list ;            /* add to list of lots             */
  mqb_lot_list = new ;

  /* String all the new message_blocks together.        */
  this = last ;
  while (this > first)
    {
      mqueue_block prev = this-- ;
      this->next = prev ;
    } ;
  assert(this == first) ;

  last->next = mqb_free_list ;          /* point last at old free list     */
  mqb_free_list = first ;               /* new blocks at head of free list */

  return mqb_free_list ;
} ;

/*==============================================================================
 * Enqueue and dequeue messages.
 */

static void mqueue_kick_signal(mqueue_queue mq, unsigned n) ;
static void mqueue_dequeue_signal(mqueue_queue mq, mqueue_thread_signal mtsig) ;

/* Enqueue message.
 *
 * If priority != 0, will enqueue after any previously enqueued priority
 * messages.
 *
 * If there are any waiters, then we kick one or all of them.
 *
 * Note that we decrement or zero the waiters count here -- because if the
 * waiter did it, they might not run before something else is enqueued.
 * Similarly, if the kick uses a signal, the signal block is dequeued here.
 *
 * The waiter count is only incremented when a dequeue is attempted and the
 * queue is empty, then:
 *
 *   for a broadcast type message queue, the first message that arrives will
 *   kick all the waiters into action.
 *
 *   for a signal type message queue, each message that arrives will kick one
 *   waiter.
 *
 * NB: this works perfectly well if !qpthreads enabled.  Of course, there can
 *     never be any waiters... so no kicking is ever done.
 */

void
mqueue_enqueue(mqueue_queue mq, mqueue_block mqb, int priority)
{
  qpt_mutex_lock(&mq->mutex) ;

  if (mq->head == NULL)
    {
      mqb->next = NULL ;
      mq->head  = mqb ;
      mq->tail_priority = priority ? mqb : NULL ;
      mq->tail  = mqb ;
    }
  else if (priority)
    {
      mqueue_block after = mq->tail_priority ;
      if (after == NULL)
        {
          mqb->next = mq->head ;
          mq->head  = mqb ;
        }
      else
        {
          mqb->next   = after->next ;
          after->next = mqb ;
        }
      mq->tail_priority = mqb ;
    }
  else
    {
      dassert(mq->tail != NULL) ;
      mqb->next = NULL ;
      mq->tail->next = mqb ;
      mq->tail = mqb ;
    } ;

  if (mq->waiters != 0)
    {
      dassert(qpthreads_enabled) ;  /* waiters == 0 if !qpthreads_enabled */

      switch (mq->type)
      {
        case mqt_cond_unicast:
          qpt_cond_signal(&mq->kick.cond.wait_here) ;
          --mq->waiters ;
          break ;

        case mqt_cond_broadcast:
          qpt_cond_broadcast(&mq->kick.cond.wait_here) ;
          mq->waiters = 0 ;
          break ;

        case mqt_signal_unicast:
          mqueue_kick_signal(mq, 1) ; /* pick off first and kick it (MUST be  */
                                      /* one) and decrement the waiters count */
          break ;

        case mqt_signal_broadcast:
          mqueue_kick_signal(mq, mq->waiters) ;
          dassert(mq->kick.signal.head == NULL) ;
          break;

        default:
          zabort("Invalid mqueue queue type") ;
      } ;
    } ;

  qpt_mutex_unlock(&mq->mutex) ;
} ;

/* Dequeue message.
 *
 * If the queue is empty and wait != 0 (and qpthreads_enabled), will wait for a
 * message.  In which case for:
 *
 *   * mqt_cond_xxxx type message queues, will wait on the condition variable,
 *     and may timeout.
 *
 *     If the argument is NULL, uses the already set up timeout, if there is
 *     one.
 *
 *     If the argument is not NULL, it is a pointer to a qtime_mono_t time,
 *     to be used as the new timeout time.
 *
 *   * mqt_signal_xxxx type message queues, will register the given signal
 *     (mtsig argument MUST be provided), and return immediately.
 *
 * NB: if !qpthreads_enabled, will not wait on the queue.  No how.
 *
 *     Note this means that waiters == 0 all the time if !qpthreads_enabled !
 *
 * NB: the argument is ignored if !wait or !qpthreads_enabled, so may be NULL.
 *
 * Returns a message block if one is available.  (And not otherwise.)
 */

mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, void* arg)
{
  mqueue_block mqb ;
  mqueue_thread_signal last ;

  mqueue_thread_signal mtsig ;
  qtime_mono_t         timeout_time ;

  qpt_mutex_lock(&mq->mutex) ;

  while (1)
    {
      mqb = mq->head ;
      if (mqb != NULL)
        break ;                   /* Easy if queue not empty              */

      if (!wait || !qpthreads_enabled)
        goto done ;               /* Easy if not waiting !  mqb == NULL   */
                                  /* Short circuit if !qpthreads_enabled  */

      ++mq->waiters ;             /* Another waiter                       */

      switch (mq->type)
      {
        case mqt_cond_unicast:    /* Now wait here                        */
        case mqt_cond_broadcast:
          if ((arg == NULL) && (mq->kick.cond.interval <= 0))
            qpt_cond_wait(&mq->kick.cond.wait_here, &mq->mutex) ;
          else
            {
              timeout_time = (arg != NULL) ? *(qtime_mono_t*)arg
                                           : mq->kick.cond.timeout ;

              if (qpt_cond_timedwait(&mq->kick.cond.wait_here, &mq->mutex,
                                                             timeout_time) == 0)
                {
                  /* Timed out -- update timeout time, if required      */
                  if (mq->kick.cond.interval > 0)
                    {
                      qtime_mono_t now = qt_get_monotonic() ;
                      timeout_time = mq->kick.cond.timeout
                                                      + mq->kick.cond.interval ;
                      if (timeout_time < now)
                        timeout_time = now + mq->kick.cond.interval ;

                      mq->kick.cond.timeout = timeout_time ;
                    } ;

                  goto done ;    /* immediate return.  mqb == NULL       */
                } ;
            } ;
          break ;

        case mqt_signal_unicast:  /* Register desire for signal           */
        case mqt_signal_broadcast:
          mtsig = arg ;
          dassert(mtsig != NULL) ;

          if (mq->kick.signal.head == NULL)
            {
              mq->kick.signal.head = mtsig ;
              mtsig->prev = (void*)mq ;
            }
          else
            {
              last = mq->kick.signal.tail ;
              last->next  = mtsig ;
              mtsig->prev = last ;
            }
          mtsig->next = NULL ;
          mq->kick.signal.tail = mtsig ;

          goto done ;           /* BUT do not wait !   mqb == NULL      */

        default:
          zabort("Invalid mqueue queue type") ;
      } ;
    } ;

  /* Have something to pull off the queue       */

  mq->head = mqb->next ;
  if (mqb == mq->tail_priority)
    mq->tail_priority = NULL ;

done:
  qpt_mutex_unlock(&mq->mutex) ;

  return mqb ;
} ;

/* No longer waiting for a signal  -- does nothing if !qpthreads_enabled.
 *
 * Returns true <=> signal has been kicked
 *
 * (Signal will never be kicked if !qpthreads_enabled.)
 */
int
mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig)
{
  int kicked ;

  if (!qpthreads_enabled)
    return 0 ;

  qpt_mutex_lock(&mq->mutex) ;

  dassert( (mq->type == mqt_signal_unicast) ||
           (mq->type == mqt_signal_broadcast) ) ;
  dassert(mtsig != NULL) ;

  /* When the thread is signalled, the prev entry is set NULL and the   */
  /* waiters count is decremented.                                      */
  /*                                                                    */
  /* So, only need to do something here if the prev is not NULL (ie the */
  /* mqueue_thread_signal is still on the list.                         */

  kicked = (mtsig->prev == NULL) ;

  if (!kicked)
    mqueue_dequeue_signal(mq, mtsig) ;

  qpt_mutex_unlock(&mq->mutex) ;

  return kicked ;
} ;

/*==============================================================================
 * Message queue signal handling
 */

/* Initialise a message queue signal structure (struct mqueue_thread_signal).
 * Allocate one if required.
 *
 * If !pthreads_enabled, then this structure is entirely redundant, but there
 * is no harm in creating it -- but the signal will never be used.
 *
 * Returns address of the structure.
 */
mqueue_thread_signal
mqueue_thread_signal_init(mqueue_thread_signal mqt, qpt_thread_t thread,
                                                                     int signum)
{
  if (mqt == NULL)
    mqt = XCALLOC(MTYPE_MQUEUE_THREAD_SIGNAL,
                   sizeof(struct mqueue_thread_signal)) ;
  else
    memset(mqt, 0, sizeof(struct mqueue_thread_signal)) ;

  /* next and prev fields set to NULL already.  */

  mqt->qpthread = thread ;
  mqt->signum   = signum ;

  return mqt ;
} ;

/* Signal the first 'n' threads on the to be signalled list.
 *
 * Removes the threads from the list and reduces the waiters count.
 *
 * NB: must be qpthreads_enabled with at least 'n' waiters.
 *
 * NB: sets the prev entry in the mqueue_thread_signal block to NULL, so that
 *     the thread can tell that its signal has been kicked.
 *
 * NB: *** MUST own the mqueue_queue mutex. ***
 */
static void
mqueue_kick_signal(mqueue_queue mq, unsigned n)
{
  mqueue_thread_signal mtsig ;

  dassert( (qpthreads_enabled) && (mq->waiters >= n) ) ;
  while (n--)
    {
      mqueue_dequeue_signal(mq, mtsig = mq->kick.signal.head) ;
      qpt_thread_signal(mtsig->qpthread, mtsig->signum) ;
    } ;
} ;

/* Remove given signal from given message queue.
 *
 * NB: sets the prev entry in the mqueue_thread_signal block to NULL, so that
 *     the thread can tell that its signal has been kicked.
 *
 * NB: *** MUST own the mqueue_queue mutex. ***
 */
static void
mqueue_dequeue_signal(mqueue_queue mq, mqueue_thread_signal mtsig)
{
  mqueue_thread_signal next ;
  mqueue_thread_signal prev ;

  next = mtsig->next ;
  prev = mtsig->prev ;

  if (prev == (void*)mq)    /* marker for head of list      */
    {
      dassert(mq->kick.signal.head == mtsig) ;
      mq->kick.signal.head = next ;
    }
  else
    {
      dassert((prev != NULL) && (prev->next == mtsig)) ;
      prev->next = next ;
    } ;

  if (next != NULL)
    next->prev = prev ;

  mtsig->next = NULL ;
  mtsig->prev = NULL ;          /* essential to show signal kicked      */
  --mq->waiters ;               /* one fewer waiter                     */

  dassert( ((mq->kick.signal.head == NULL) && (mq->waiters == 0)) ||
           ((mq->kick.signal.head != NULL) && (mq->waiters != 0)) ) ;
} ;

/*==============================================================================
 * Initialise Message Queue handling
 *
 * Must be called before any qpt_threads are started.
 *
 * Freezes qpthreads_enabled.
 *
 * TODO: how do we shut down message queue handling ?
 */
void
mqueue_initialise(void)
{
  if (qpthreads_enabled_freeze)
    qpt_mutex_init_new(&mqb_mutex, qpt_mutex_quagga) ;
} ;

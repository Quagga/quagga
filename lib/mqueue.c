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
 * current timeout time and timeout interval variables.  Each time a waiter
 * waits it will do:
 *
 *   next = now + interval
 *   if (now > current) || (next <= current)
 *     current = next and return (don't wait)
 *   else
 *     wait until current
 *
 * If the wait times out, the current is set to current + interval.
 *
 * The effect of this is to return at least at regular intervals from the wait,
 * provided the queue is waited on within the timeout period.  If the queue is
 * waited on after the current timeout time, it returns immediately, updating
 * the current timeout time -- the "clock" slips.
 *
 * There is a default timeout period.  The period may be set "infinite".
 *
 * For waiters kicked by signal, the wait does not occur within the message
 * queue code, but the need for a signal is recorded in the message queue.
 *
 * Messages take the form of a small block of information which contains:
 *
 *   * flags     -- used by the message handler
 *   * context   -- identifies the context of the message (see revoke)
 *
 *   * action    -- void action(mqueue_block)   message dispatch
 *   * arg_0     -- *void/uintptr_t/intptr_t )  standard arguments
 *   * arg_1     -- *void/uintptr_t/intptr_t )
 *
 * (see struct mqueue_block).
 *
 * To send a message, first allocate a message block (see mqueue_block_new),
 * then fill in the arguments and enqueue it.
 *
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
 */

mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type)
{
  if (mq == NULL)
    mq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(struct mqueue_queue)) ;
  else
    memset(mq, 0, sizeof(struct mqueue_queue)) ;

  qpt_mutex_init(&mq->mutex, qpt_mutex_quagga) ;

  /* head, tail and tail_priority set NULL already              */
  /* waiters set zero already                                   */

  mq->type = type ;
  switch (type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
        qpt_cond_init(&mq->kick.cond.wait_here, qpt_cond_quagga) ;
        if (MQUEUE_DEFAULT_INTERVAL != 0)
          {
            mq->kick.cond.interval = MQUEUE_DEFAULT_INTERVAL ;
            mq->kick.cond.timeout  =
                            qpt_cond_get_timeout_time(MQUEUE_DEFAULT_INTERVAL) ;
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
 */
void
mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval)
{
  qpt_mutex_lock(&mq->mutex) ;

  dassert( (mq->type == mqt_cond_unicast) ||
           (mq->type == mqt_cond_broadcast) ) ;

  mq->kick.cond.interval = interval ;
  mq->kick.cond.timeout  = (interval > 0) ? qpt_cond_get_timeout_time(interval)
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

static pthread_mutex_t* p_mb_mutex ;    /* NULL => no mutex (yet)       */
static pthread_mutex_t  mb_mutex ;

#define MB_LOT_SIZE  256

static mqueue_block mb_lot_list  = NULL ;
static mqueue_block mb_free_list = NULL ;

static mqueue_block mqueue_block_new_lot(void) ;

/* Get an empty message block
 */
mqueue_block
mqueue_block_new(void)
{
  mqueue_block mb ;

  qpt_mutex_lock(&mb_mutex) ;

  mb = mb_free_list ;
  if (mb == NULL)
    mb = mqueue_block_new_lot() ;

  mb_free_list = mb->next ;

  qpt_mutex_unlock(&mb_mutex) ;

  memset(mb, 0, sizeof(struct mqueue_block)) ;

  return mb ;
} ;

/* Free message block when done with it.
 */
void
mqueue_block_free(mqueue_block mb)
{
  qpt_mutex_lock(&mb_mutex) ;

  mb->next = mb_free_list ;
  mb_free_list = mb ;

  qpt_mutex_unlock(&mb_mutex) ;
} ;

/* Make a new lot of empty message_block structures.
 *
 * NB: caller MUST hold the mb_mutex.
 *
 */
static mqueue_block
mqueue_block_new_lot(void)
{
  mqueue_block first, last, this ;

  mqueue_block new = XCALLOC(MTYPE_MQUEUE_BLOCKS,
                                      SIZE(struct mqueue_block, MB_LOT_SIZE)) ;
  first = &new[1] ;
  last  = &new[MB_LOT_SIZE - 1] ;

  new->next = mb_lot_list ;             /* add to list of lots             */
  mb_lot_list = new ;

  /* String all the new message_blocks together.        */
  this = last ;
  while (this > first)
    {
      mqueue_block prev = this-- ;
      this->next = prev ;
    } ;
  assert(this == first) ;

  last->next = mb_free_list ;           /* point last at old free list     */
  mb_free_list = first ;                /* new blocks at head of free list */

  return mb_free_list ;
} ;

/*==============================================================================
 * Enqueue and dequeue messages.
 */

static void mqueue_kick_signal(mqueue_queue mq, int n) ;
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
 */

void
mqueue_enqueue(mqueue_queue mq, mqueue_block mb, int priority)
{
  qpt_mutex_lock(&mq->mutex) ;

  if (mq->head == NULL)
    {
      mb->next = NULL ;
      mq->head = mb ;
      mq->tail_priority = priority ? mb : NULL ;
      mq->tail = mb ;
    }
  else if (priority)
    {
      mqueue_block after = mq->tail_priority ;
      if (after == NULL)
        {
          mb->next = mq->head ;
          mq->head = mb ;
        }
      else
        {
          mb->next = after->next ;
          after->next = mb ;
        }
      mq->tail_priority = mb ;
    }
  else
    {
      dassert(mq->tail != NULL) ;
      mb->next = NULL ;
      mq->tail->next = mb ;
      mq->tail = mb ;
    } ;

  if (mq->waiters != 0)
    {
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
 * If the queue is empty and wait != 0, will wait for a message.  In which
 * case for:
 *
 *   * mqt_cond_xxxx type message queues, will wait on the condition variable,
 *     and may time-out.  (mtsig argument MUST be NULL.)
 *
 *   * mqt_signal_xxxx type message queues, will register the given signal
 *     (mtsig argument MUST be provided), and return immediately.
 *
 * Returns a message block if one is available.  (And not otherwise.)
 */

mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, mqueue_thread_signal mtsig)
{
  mqueue_block mb ;
  mqueue_thread_signal last ;

  qpt_mutex_lock(&mq->mutex) ;

  while (1)
    {
      mb = mq->head ;
      if (mb != NULL)
        break ;                   /* Easy if queue not empty              */

      if (!wait)
        goto done ;               /* Easy if not waiting !  mb == NULL    */

      ++mq->waiters ;             /* Another waiter                       */

      switch (mq->type)
      {
        case mqt_cond_unicast:    /* Now wait here                        */
        case mqt_cond_broadcast:
          dassert(mtsig == NULL) ;

          if (mq->kick.cond.interval <= 0)
            qpt_cond_wait(&mq->kick.cond.wait_here, &mq->mutex) ;
          else
            {
              qtime_t now = qpt_cond_get_timeout_time(0) ;

#if QPT_COND_CLOCK_MONOTONIC
              dassert(now >= (mq->kick.cond.timeout - mq->kick.cond.interval)) ;
              if (now > mq->kick.cond.timeout)
#else
              if ( (now > mq->kick.cond.timeout)
                  || (now < (mq->kick.cond.timeout - mq->kick.cond.interval)) )
#endif
                {
                  /* the "clock" has slipped.  Reset it and return now. */
                  mq->kick.cond.timeout = now + mq->kick.cond.interval ;
                  goto done ;    /* immediate return.  mb == NULL        */
                }
              else
                {
                  if (qpt_cond_timedwait(&mq->kick.cond.wait_here, &mq->mutex,
                                                    mq->kick.cond.timeout) == 0)
                    mq->kick.cond.timeout += mq->kick.cond.interval ;
                } ;
            } ;
          break ;

        case mqt_signal_unicast:  /* Register desire for signal           */
        case mqt_signal_broadcast:
          dassert(mtsig != NULL) ;

          last = mq->kick.signal.tail ;
          if (last == NULL)
            {
              mq->kick.signal.head = mtsig ;
              mtsig->prev = (void*)mq ;
            }
          else
            {
              last->next = mtsig ;
              mtsig->prev = last ;
            }
          mtsig->next = NULL ;
          mq->kick.signal.tail = mtsig ;

          goto done ;           /* BUT do not wait !   mb == NULL       */

        default:
          zabort("Invalid mqueue queue type") ;
      } ;
    } ;

  /* Have something to pull off the queue       */

  mq->head = mb->next ;
  if (mb == mq->tail_priority)
    mq->tail_priority = NULL ;

done:
  qpt_mutex_unlock(&mq->mutex) ;

  return mb ;
} ;

/* No longer waiting for a signal.
 *
 * Returns true <=> signal has been kicked.
 */
int
mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig)
{
  int kicked ;

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
 * NB: sets the prev entry in the mqueue_thread_signal block to NULL, so that
 *     the thread can tell that its signal has been kicked.
 *
 * NB: *** MUST own the mqueue_queue mutex. ***
 */
static void
mqueue_kick_signal(mqueue_queue mq, int n)
{
  mqueue_thread_signal mtsig ;

  while (n--)
    {
      mqueue_dequeue_signal(mq, mtsig = mq->kick.signal.head) ;
      qpt_thread_signal(mtsig->qpthread, mtsig->signum) ;
    } ;
} ;

/* Remove given signal from given message queue.
 *
 * NB: *** MUST own the mqueue_queue mutex. ***
 */
static void
mqueue_dequeue_signal(mqueue_queue mq, mqueue_thread_signal mtsig)
{
  mqueue_thread_signal next ;
  mqueue_thread_signal prev ;

  dassert((mq->kick.signal.head != NULL) && (mq->waiters != 0)) ;

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
 * TODO: how do we shut down message queue handling ?
 */
void
mqueue_initialise(int qpthreads)
{
  if (qpthreads)
    p_mb_mutex = qpt_mutex_init(&mb_mutex, qpt_mutex_quagga) ;
} ;

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
 * If !qpthreads_enabled, then a message queue holds messages for the program
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
 *------------------------------------------------------------------------------
 * Message Blocks and Arguments
 *
 * Messages take the form of a small block of information which contain:
 *
 *   * action      -- void action(mqueue_block)   message dispatch
 *   * arg0        -- void* argument
 *   * struct args -- embedded argument structure
 *   * argv        -- optional array of union of: *void/uintptr_t/intptr_t
 *
 * There are set/get functions for action/arguments -- users should not poke
 * around inside the structure.
 *
 * To send a message, first initialise/allocate a message block
 * (see mqb_init_new), then fill in the arguments and enqueue it.
 *
 * NB: arg0 is expected to be used as the "context" for the message -- to
 *     point to some data common to both ends of the conversation.
 *
 *     For specific revoke, arg0 is assumed to identify the messages to be
 *     revoked.
 *
 * NB: the struct args is expected to be a modest sized structure, carrying
 *     the key elements of the message.
 *
 *     Some other structure must be overlaid on this, in the same way by sender
 *     and receiver of the message.  So:
 *
 *        mqueue_block mqb = mqb_init_new(NULL, arg0, action_func) ;
 *
 *        struct my_message* args = mqb_get_args(mqb) ;
 *
 *     allocates mqueue block, filling in arg0 and the action func.  Then
 *     args can be used to fill in a "struct my_message" form of args.
 *
 * NB: the sizeof(struct my_message) MUST BE <= mqb_args_size_max !!!
 *
 * The argv is an optional, flexible list/array of optional array of
 * union of: *void/uintptr_t/intptr_t -- see mqb_arg_t et al.
 *
 * May set any number of arguments in argv.
 *
 * A count of arguments is maintained, and is the highest index set + 1.  That
 * count can be fetched.  (So there is no need to maintain it separately.)
 *
 * May get any argument by its index -- but it is a fatal error to attempt to
 * access a non-existent argument (one beyond the known count).
 *
 * There is support for pushing values onto the argv "list" and for iterating
 * along the "list".  May also push and pop entire arrays of items.
 *
 *==============================================================================
 * Local Queues
 *
 * A local queue may be used within a thread to requeue messages for later
 * processing.
 *
 * Local queues are simple FIFO queues.
 */

/*==============================================================================
 * Message Block allocation statics
 *
 * Once a message block is allocated it is not deallocated, but kept ready
 * for future use.
 *
 * Keeps a count of free message blocks.  (Could at some later date reduce the
 * number of free message blocks if it is known that some burst of messages has
 * now passed.)
 */

static pthread_mutex_t  mqb_mutex ;     /* for allocation of mqueue blocks  */

static mqueue_block mqb_free_list  = NULL ;
static unsigned     mqb_free_count = 0 ;

/*==============================================================================
 * Initialise and shut down Message Queue and Message Block handling
 */

/*------------------------------------------------------------------------------
 * Initialise Message Queue handling.
 *
 * Must be called before any qpt_threads are started.
 *
 * Freezes qpthreads_enabled.
 */
extern void
mqueue_initialise(void)
{
  if (qpthreads_enabled_freeze)
    qpt_mutex_init_new(&mqb_mutex, qpt_mutex_quagga) ;
} ;

/*------------------------------------------------------------------------------
 * Shut down Message Queue handling.
 *
 * Release all resources used.
 *
 * NB: all pthreads must have stopped -- mutex must be free and no further
 *     uses may be made.
 */
extern void
mqueue_finish(void)
{
  mqueue_block mqb ;

  while ((mqb = mqb_free_list) != NULL)
    {
      assert(mqb_free_count != 0) ;
      mqb_free_count-- ;
      mqb_free_list = mqb->next ;
      XFREE(MTYPE_MQUEUE_BLOCK, mqb) ;
    } ;

  assert(mqb_free_count == 0) ;

  qpt_mutex_destroy_keep(&mqb_mutex) ;
} ;

/*==============================================================================
 * Initialisation etc. for Message Queue
 *
 */

/*------------------------------------------------------------------------------
 * Initialise new Message Queue, if required (mq == NULL) allocating it.
 *
 * For mqt_cond_xxx type queues, sets the default timeout interval and the
 * initial timeout time to now + that interval.
 *
 * NB: once any message queue has been initialised, it is TOO LATE to enable
 *     qpthreads.
 */
extern mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type)
{
  if (mq == NULL)
    mq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(struct mqueue_queue)) ;
  else
    memset(mq, 0, sizeof(struct mqueue_queue)) ;

  if (qpt_freeze_qpthreads_enabled())
    qpt_mutex_init_new(&mq->mutex, qpt_mutex_quagga) ;

  /* head, tail and tail_priority set NULL already              */
  /* count set zero already                                     */
  /* waiters set zero already                                   */

  mq->type = type ;
  switch (type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
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

/*------------------------------------------------------------------------------
 * Empty message queue -- by revoking everything.
 *
 * Leaves queue ready for continued use with all existing settings.
 *
 * If there were any waiters, they are still waiting.
 */
extern void
mqueue_empty(mqueue_queue mq)
{
  mqueue_revoke(mq, NULL) ;

  assert((mq->head == NULL) && (mq->count == 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Reset message queue -- empty it out by revoking everything.
 *
 * Frees the structure if required, and returns NULL.
 * Otherwise zeroises the structure, and returns address of same.
 *
 * NB: there MUST NOT be ANY waiters !
 */
extern mqueue_queue
mqueue_reset(mqueue_queue mq, int free_structure)
{
  mqueue_empty(mq) ;

  passert(mq->waiters == 0) ;

  qpt_mutex_destroy_keep(&mq->mutex) ;

  switch (mq->type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
        qpt_cond_destroy_keep(&mq->kick.cond.wait_here) ;
        break;

      case mqt_signal_unicast:
      case mqt_signal_broadcast:
        passert(mq->kick.signal.head == NULL) ;
        break;

      default:
        zabort("Invalid mqueue queue type") ;
    } ;

  if (free_structure)
    XFREE(MTYPE_MQUEUE_QUEUE, mq) ;     /* sets mq == NULL      */
  else
    memset(mq, 0, sizeof(struct mqueue_queue)) ;

  return mq ;
} ;

/*------------------------------------------------------------------------------
 * Initialise new Local Message Queue, if required (lmq == NULL) allocating it.
 *
 * Returns address of Local Message Queue
 */
extern mqueue_local_queue
mqueue_local_init_new(mqueue_local_queue lmq)
{
  if (lmq == NULL)
    lmq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(struct mqueue_local_queue)) ;
  else
    memset(lmq, 0, sizeof(struct mqueue_local_queue)) ;

  /* Zeroising the structure is enough to initialise:
   *
   *   * head   -- NULL
   *   * tail   -- NULL
   */

  return lmq ;
} ;

/*------------------------------------------------------------------------------
 * Reset Local Message Queue, and if required free it.
 *
 * Dequeues entries and dispatches them "mqb_destroy", to empty the queue.
 *
 * See: mqueue_local_reset_keep(lmq)
 *      mqueue_local_reset_free(lmq)
 *
 * Returns address of Local Message Queue
 */
extern mqueue_local_queue
mqueue_local_reset(mqueue_local_queue lmq, int free_structure)
{
  mqueue_block mqb ;

  while ((mqb = lmq->head) != NULL)
    {
      lmq->head = mqb->next ;
      mqb_dispatch_destroy(mqb) ;
    } ;

  if (free_structure)
    XFREE(MTYPE_MQUEUE_QUEUE, lmq) ;    /* sets lmq = NULL      */
  else
    memset(lmq, 0, sizeof(struct mqueue_local_queue)) ;

  return lmq ;
} ;

/*------------------------------------------------------------------------------
 * Set new timeout interval (or unset by setting <= 0)
 *
 * Sets the next timeout to be the time now + new interval (or never).
 *
 * This is a waste of time if !qpthreads_enabled, but does no harm.  The
 * timeout is ignored.
 */
extern void
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
 * Allocates message block structures when required.
 *
 * Places those structures on the free list when they are freed.
 *
 * Keeps a count of free structures.  (Could at some later date reduce the
 * number of free structures if it is known that some burst of messages has
 * now passed.)
 *
 * mqueue_initialise MUST be called before the first message block is allocated.
 */

inline static size_t mqb_argv_size(mqb_index_t alloc)
{
  return alloc * sizeof(mqb_arg_t) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise message block (allocate if required) and set action & arg0.
 *
 * Zeroises the struct args.
 *
 * Returns address of message block.
 */
extern mqueue_block
mqb_init_new(mqueue_block mqb, mqueue_action action, void* arg0)
{
  if (mqb == NULL)
    {
      qpt_mutex_lock(&mqb_mutex) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

      mqb = mqb_free_list ;
      if (mqb == NULL)
        {
          dassert(mqb_free_count == 0) ;
          mqb = XMALLOC(MTYPE_MQUEUE_BLOCK, sizeof(struct mqueue_block)) ;
        }
      else
        {
          dassert(mqb_free_count >= 0) ;
          mqb_free_list = mqb->next ;
          --mqb_free_count ;
        } ;

      qpt_mutex_unlock(&mqb_mutex) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
    } ;

  memset(mqb, 0, sizeof(struct mqueue_block)) ;

  mqb->action = action ;
  mqb->arg0   = arg0 ;

  /* Zeroising the mqb sets:
   *
   *    next           -- NULL
   *
   *    args           -- zeroised
   *
   *    argv           -- NULL -- empty list/array
   *
   *    argv_count     -- 0 -- empty
   *    argv_alloc     -- 0 -- nothing allocated
   *    argv_next      -- 0 -- iterator reset
   */

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Re-initialise message block (or allocate if required) and set action & arg0.
 *
 * NB: preserves any existing argv, but empties it.
 *
 * NB: it is the caller's responsibility to free the value of any argument that
 *     requires it.
 */
extern mqueue_block
mqb_re_init(mqueue_block mqb, mqueue_action action, void* arg0)
{
  mqb_index_t argv_alloc ;
  mqb_arg_t*  argv ;

  /* Exactly mqb_init_new if mqb is NULL                        */
  if (mqb == NULL)
    return mqb_init_new(NULL, action, arg0) ;

  /* Otherwise, need to put argv to one side first              */
  argv        = mqb->argv ;
  argv_alloc  = mqb->argv_alloc ;

  mqb_init_new(mqb, action, arg0) ;

  /* Now zeroize the argv, and restore it                       */
  memset(argv, 0, mqb_argv_size(argv_alloc)) ;

  mqb->argv       = argv ;
  mqb->argv_alloc = argv_alloc ;

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Free message block when done with it.
 *
 * Frees any argv argument vector.
 *
 * NB: it is the caller's responsibility to free the value of any argument that
 *     requires it.
 */
extern void
mqb_free(mqueue_block mqb)
{
  if (mqb->argv != NULL)
    XFREE(MTYPE_MQUEUE_BLOCK_ARGV, mqb->argv) ;

  qpt_mutex_lock(&mqb_mutex) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  mqb->next = mqb_free_list ;
  mqb_free_list = mqb ;
  ++mqb_free_count ;

  qpt_mutex_unlock(&mqb_mutex) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
} ;

/*==============================================================================
 * Enqueue and dequeue messages.
 */

static void mqueue_kick_signal(mqueue_queue mq, unsigned n) ;
static void mqueue_dequeue_signal(mqueue_queue mq, mqueue_thread_signal mtsig) ;

/*------------------------------------------------------------------------------
 * Enqueue message.
 *
 * If priority, will enqueue after any previously enqueued priority
 * messages.  (See enum: mqb_priority and mqb_ordinary.)
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
 * If mq is NULL, the message is not queued but is immediately destroyed.
 *
 * NB: this works perfectly well if !qpthreads enabled.  Of course, there can
 *     never be any waiters... so no kicking is ever done.
 */
extern void
mqueue_enqueue(mqueue_queue mq, mqueue_block mqb, enum mqb_rank priority)
{
  if (mq == NULL)
    return mqb_dispatch_destroy(mqb) ;

  qpt_mutex_lock(&mq->mutex) ;

  if (mq->head == NULL)
    {
      assert(mq->count == 0) ;
      mqb->next = NULL ;
      mq->head  = mqb ;
      mq->tail_priority = priority ? mqb : NULL ;
      mq->tail  = mqb ;
    }
  else
    {
      assert(mq->count > 0) ;
      if (priority)
      {
        mqueue_block after = mq->tail_priority ;
        if (after == NULL)
          {
            mqb->next = mq->head ;
            mq->head  = mqb ;
            /* mq non-empty, enchain at head, therefore tail unaffected */
          }
        else
          {
            mqb->next   = after->next ;
            after->next = mqb ;
            /* if only have priority messages then fix tail */
            if (mq->tail == after)
              mq->tail = mqb;
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
    } ;

  ++mq->count ;

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

/*------------------------------------------------------------------------------
 * Dequeue message.
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
 *
 * NB: if mq is NULL, returns NULL -- nothing available
 */
extern mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, void* arg)
{
  mqueue_block mqb ;
  mqueue_thread_signal last ;

  mqueue_thread_signal mtsig ;
  qtime_mono_t         timeout_time ;

  if (mq == NULL)
    return NULL ;

  qpt_mutex_lock(&mq->mutex) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  while (1)
    {
      mqb = mq->head ;
      if (mqb != NULL)
        break ;                   /* Easy if queue not empty              */

      assert(mq->count == 0) ;

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

  assert(mq->count > 0) ;
  --mq->count ;

  mq->head = mqb->next ;

  /* fix tails if at tail */
  if (mqb == mq->tail)
    mq->tail = NULL ;
  if (mqb == mq->tail_priority)
    mq->tail_priority = NULL ;

done:
  qpt_mutex_unlock(&mq->mutex) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Revoke message(s)
 *
 * Revokes all messages, or only messages whose arg0 matches the given value.
 * (If the given value is NULL revokes everything.)
 *
 * Revokes by calling mqb_dispatch_destroy().
 *
 * During a revoke() operation more items may be enqueued, but no other mqueue
 * operations may be performed.  Enqueued items may promptly be revoked, except
 * for priority items if the revoke operation has already moved past the last
 * priority item.
 *
 * If mq is NULL, does nothing.
 */
extern void
mqueue_revoke(mqueue_queue mq, void* arg0)
{
  mqueue_block mqb ;
  mqueue_block prev ;

  if (mq == NULL)
    return ;

  qpt_mutex_lock(&mq->mutex) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  prev = NULL ;
  while (1)
    {
      if (prev == NULL)
        mqb = mq->head ;
      else
        mqb = prev->next ;

      if (mqb == NULL)
        break ;

      if ((arg0 == NULL) || (arg0 == mqb->arg0))
        {
          assert(mq->count > 0) ;

          if (prev == NULL)
            mq->head   = mqb->next ;
          else
            prev->next = mqb->next ;

          if (mqb == mq->tail)
            mq->tail = prev ;

          if (mqb == mq->tail_priority)
            mq->tail_priority = prev ;

          --mq->count ;

          qpt_mutex_unlock(&mq->mutex) ;
            mqb_dispatch_destroy(mqb) ;
          qpt_mutex_lock(&mq->mutex) ;
        }
      else
        prev = mqb ;
    } ;

  qpt_mutex_unlock(&mq->mutex) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
} ;

/*------------------------------------------------------------------------------
 * No longer waiting for a signal  -- does nothing if !qpthreads_enabled.
 *
 * Returns true <=> signal has been kicked
 *
 * (Signal will never be kicked if !qpthreads_enabled.)
 */
extern int
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

/*------------------------------------------------------------------------------
 * Enqueue message on local queue -- at tail
 */
extern void
mqueue_local_enqueue(mqueue_local_queue lmq, mqueue_block mqb)
{
  if (lmq->head == NULL)
    lmq->head       = mqb ;
  else
    lmq->tail->next = mqb ;
  lmq->tail = mqb ;
  mqb->next = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue message on local queue -- at head
 */
extern void
mqueue_local_enqueue_head(mqueue_local_queue lmq, mqueue_block mqb)
{
  if (lmq->head == NULL)
    lmq->tail = mqb ;

  mqb->next = lmq->head ;
  lmq->head = mqb ;
} ;

/*------------------------------------------------------------------------------
 * Dequeue message from local queue -- returns NULL if empty
 */
extern mqueue_block
mqueue_local_dequeue(mqueue_local_queue lmq)
{
  mqueue_block mqb = lmq->head ;

  if (mqb != NULL)
    lmq->head = mqb->next ;

  return mqb ;
} ;

/*==============================================================================
 * Message queue signal handling
 */

/*------------------------------------------------------------------------------
 * Initialise a message queue signal structure (struct mqueue_thread_signal).
 * Allocate one if required.
 *
 * If !pthreads_enabled, then this structure is entirely redundant, but there
 * is no harm in creating it -- but the signal will never be used.
 *
 * Returns address of the structure.
 */
extern mqueue_thread_signal
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

/*------------------------------------------------------------------------------
 * Reset a message queue signal structure and release if required.
 *
 * NB: MUST NOT be queued as a waiter anywhere !!
 *
 * Frees the structure if required, and returns NULL.
 * Otherwise zeroises the structure, and returns address of same.
 */
extern mqueue_thread_signal
mqueue_thread_signal_reset(mqueue_thread_signal mqt, int free_structure)
{
  passert(mqt->prev == NULL) ;

  if (free_structure)
    XFREE(MTYPE_MQUEUE_THREAD_SIGNAL, mqt) ;    /* sets mqt = NULL      */
  else
    memset(mqt, 0, sizeof(struct mqueue_thread_signal)) ;

  return mqt ;
} ;

/*------------------------------------------------------------------------------
 * Signal the first 'n' threads on the to be signalled list.
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

/*------------------------------------------------------------------------------
 * Remove given signal from given message queue.
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
 * Message Queue Block Argument Handling
 */

static void mqb_argv_extend(mqueue_block mqb, mqb_index_t iv) ;

/*------------------------------------------------------------------------------
 * Get pointer to argv[iv] -- extending if required
 */
inline static mqb_arg_t*
mqb_p_arg_set(mqueue_block mqb, mqb_index_t iv)
{
  if (iv >= mqb->argv_count)
    {
      if (iv >= mqb->argv_alloc)
        mqb_argv_extend(mqb, iv) ;
      mqb->argv_count = iv + 1 ;
    } ;

  return &mqb->argv[iv] ;
} ;

/*------------------------------------------------------------------------------
 * Set pointer argv[iv] to given value.
 */
extern void
mqb_set_argv_p(mqueue_block mqb, mqb_index_t iv, mqb_ptr_t  p)
{
  mqb_arg_t* p_arg = mqb_p_arg_set(mqb, iv) ;
  p_arg->p = p ;
} ;

/*------------------------------------------------------------------------------
 * Set integer argv[iv] to given value.
 */
extern void
mqb_set_argv_i(mqueue_block mqb, mqb_index_t iv, mqb_int_t  i)
{
  mqb_arg_t* p_arg = mqb_p_arg_set(mqb, iv) ;
  p_arg->i = i ;
} ;

/*------------------------------------------------------------------------------
 * Set unsigned integer argv[iv] to given value.
 */
extern void
mqb_set_argv_u(mqueue_block mqb, mqb_index_t iv, mqb_uint_t u)
{
  mqb_arg_t* p_arg = mqb_p_arg_set(mqb, iv) ;
  p_arg->u = u ;
} ;

/*------------------------------------------------------------------------------
 * Set size of argv[].
 *
 * This is entirely optional, and may be used to ensure that at least the given
 * number of elements have been allocated.
 *
 * Does not change the "count".  Will not reduce the allocated size.
 *
 * Just avoids repeated extensions of argv if it is known that it will become
 * large.
 */
extern void
mqb_set_argv_size(mqueue_block mqb, unsigned n)
{
  if (n > mqb->argv_alloc)
    mqb_argv_extend(mqb, n - 1) ;
} ;

/*------------------------------------------------------------------------------
 * Push a pointer onto the argv "list"
 */
extern void
mqb_push_argv_p(mqueue_block mqb, mqb_ptr_t  p)
{
  mqb_arg_t* p_arg ;

  p_arg = mqb_p_arg_set(mqb, mqb->argv_count) ;
  p_arg->p = p ;
} ;

/*------------------------------------------------------------------------------
 * Push an integer onto the argv "list"
 */
extern void
mqb_push_argv_i(mqueue_block mqb, mqb_int_t  i)
{
  mqb_arg_t* p_arg ;

  p_arg = mqb_p_arg_set(mqb, mqb->argv_count) ;
  p_arg->i = i ;
} ;

/*------------------------------------------------------------------------------
 * Push an unsigned integer onto the argv "list"
 */
extern void
mqb_push_argv_u(mqueue_block mqb, mqb_uint_t u)
{
  mqb_arg_t* p_arg ;

  p_arg = mqb_p_arg_set(mqb, mqb->argv_count) ;
  p_arg->u = u ;
} ;

/*------------------------------------------------------------------------------
 * Push an array of 'n' void* pointers onto the argv "list"
 */
extern void
mqb_push_argv_array(mqueue_block mqb, unsigned n, void** array)
{
  mqb_index_t iv ;

  /* need do nothing if n == 0, get out now to avoid edge cases         */
  if (n == 0)
    return ;

  /* make sure we are allocated upto and including the last array item  */
  iv = mqb->argv_count ;
  mqb_set_argv_size(mqb, iv + n - 1) ;

  /* require that mqb_ptr_t values exactly fill mqb_arg_t entries       */
  /*     and that mqb_ptr_t values are exactly same as void* values     */
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(mqb_arg_t)) ;
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(void*)) ;

  /* copy the pointers                                                  */
  memcpy(&mqb->argv[iv], array, sizeof(void*) * n) ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to argv[iv] -- which MUST exist
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 */
inline static mqb_arg_t*
mqb_p_arg_get(mqueue_block mqb, mqb_index_t iv)
{
  if (iv >= mqb->argv_count)
    zabort("invalid message block argument index") ;

  return &mqb->argv[iv] ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer value of argv[iv]
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 *
 *     mqb_get_argv_count() returns the number of arguments set in argv.
 */
extern mqb_ptr_t
mqb_get_argv_p(mqueue_block mqb, mqb_index_t iv)
{
  mqb_arg_t* p_arg = mqb_p_arg_get(mqb, iv) ;
  return p_arg->p ;
} ;

/*------------------------------------------------------------------------------
 * Get integer value of argv[iv]
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 *
 *     mqb_get_argv_count() returns the number of arguments set in argv.
 */
extern mqb_int_t
mqb_get_argv_i(mqueue_block mqb, mqb_index_t iv)
{
  mqb_arg_t* p_arg = mqb_p_arg_get(mqb, iv) ;
  return p_arg->i ;
} ;

/*------------------------------------------------------------------------------
 * Get unsigned integer value of argv[iv]
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 *
 *     mqb_get_argv_count() returns the number of arguments set in argv.
 */
extern mqb_uint_t
mqb_get_argv_u(mqueue_block mqb, mqb_index_t iv)
{
  mqb_arg_t* p_arg = mqb_p_arg_get(mqb, iv) ;
  return p_arg->u ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer value of next argv "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and by mqb_reset_argv_next().
 *
 * NB: returns NULL if there is no "list" or if already at the end of same.
 */
extern mqb_ptr_t
mqb_next_argv_p(mqueue_block mqb)
{
  if (mqb->argv_next >= mqb->argv_count)
    return NULL ;

  return mqb_get_argv_p(mqb, mqb->argv_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Get integer value of next argv "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and by mqb_reset_argv_next().
 *
 * NB: returns 0 if there is no "list" or if already at the end of same.
 */
extern mqb_int_t
mqb_next_argv_i(mqueue_block mqb)
{
  if (mqb->argv_next >= mqb->argv_count)
    return 0 ;

  return mqb_get_argv_i(mqb, mqb->argv_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Get unsigned integer value of next argv "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and by mqb_reset_argv_next().
 *
 * NB: returns 0 if there is no "list" or if already at the end of same.
 */
extern mqb_uint_t
mqb_next_argv_u(mqueue_block mqb)
{
  if (mqb->argv_next >= mqb->argv_count)
    return 0 ;

  return mqb_get_argv_u(mqb, mqb->argv_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Pop an array of 'n' void* pointers from the argv "list"
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and by mqb_reset_argv_next().
 *
 * Treats from "next" to the end of the "list" as an array of void* pointers.
 *
 * Creates a temporary void* [] array (MTYPE_TMP), which caller must free.
 *
 * NB: returns NULL if there is no "list" or if already at the end of same.
 */
extern void**
mqb_pop_argv_array(mqueue_block mqb)
{
  void**      array ;
  unsigned    n ;

  mqb_index_t iv = mqb->argv_next ;

  /* worry about state of "next" and get out if nothing to do.          */
  if (iv >= mqb->argv_count)
    return NULL ;

  /* work out how much to pop and update "next"                         */
  n  = mqb->argv_count - iv ;

  mqb->argv_next = mqb->argv_count ;

  /* construct target array                                             */
  array = XMALLOC(MTYPE_TMP, sizeof(void*) * n) ;

  /* require that mqb_ptr_t values exactly fill mqb_arg_t entries       */
  /*     and that mqb_ptr_t values are exactly same as void* values     */
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(mqb_arg_t)) ;
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(void*)) ;

  /* now transfer pointers to the array                                 */
  memcpy(array, mqb->argv + iv, sizeof(void*) * n) ;

  return array ;
} ;

/*------------------------------------------------------------------------------
 * Extend the argv to include at least given iv.
 *
 * The number of argv slots allocated is arranged to be a multiple of
 * mqb_argv_size_unit.
 *
 * Ensures that newly created slots are zeroised.
 */
static void
mqb_argv_extend(mqueue_block mqb, mqb_index_t iv)
{
  mqb_index_t need ;                    /* total slots required         */
  mqb_index_t have ;

  have = mqb->argv_alloc ;
  assert(have <= iv) ;

  need = ((iv / mqb_argv_size_unit) + 1) * mqb_argv_size_unit ;

  if (mqb->argv == NULL)
    mqb->argv = XCALLOC(MTYPE_MQUEUE_BLOCK_ARGV, mqb_argv_size(need)) ;
  else
    {
      mqb->argv = XREALLOC(MTYPE_MQUEUE_BLOCK_ARGV, mqb->argv,
                                                 mqb_argv_size(need)) ;
      memset(&mqb->argv[have], 0, mqb_argv_size(need - have)) ;
    } ;

  mqb->argv_alloc = need ;
} ;

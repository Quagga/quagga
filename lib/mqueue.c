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
#include "misc.h"

#include "memory.h"
#include "mqueue.h"

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
 *   * struct args -- embedded argument structure
 *   * arg0        -- void* argument
 *   * action      -- void action(mqueue_block)   message dispatch
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
 * NB: the sizeof(struct my_message) MUST BE <= sizeof(mqb_args_t) !!!
 *
 *     The macro MQB_ARGS_SIZE_OK(s) is a CONFIRM for this, eg:
 *
 *       struct my_args { ... } ;
 *       MQB_ARGS_SIZE_OK(struct my_args) ;
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

static qpt_mutex    mqb_mutex ;         /* for allocation of mqueue blocks  */

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
  mqb_mutex = qpt_mutex_new(qpt_mutex_quagga, "mqb alloc") ;
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

  mqb_mutex = qpt_mutex_destroy(mqb_mutex) ;
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
mqueue_init_new(mqueue_queue mq, mqueue_queue_type_t type, const char* name)
{
  if (mq == NULL)
    mq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(mqueue_queue_t)) ;
  else
    memset(mq, 0, sizeof(mqueue_queue_t)) ;

  /* Zeroising has set:
   *
   *    mutex           -- NULL     -- set below, if required
   *
   *    head            -- NULL
   *    tail_priority   -- NULL
   *    tail            -- NULL
   *    count           -- 0
   *
   *    type            -- X        -- set below
   *
   *    revoking        -- false    -- not revoking !
   *
   *    waiters         -- 0
   *
   *    kick.cond       -- X        -- set below, if required
   *    kick.signal     -- all zero -- see below
   */

  if (qpt_freeze_qpthreads_enabled())
    mq->mutex = qpt_mutex_new(qpt_mutex_quagga, name) ;

  mq->type = type ;
  switch (type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
        qpt_cond_init_new(mq->kick.cond.wait_here, qpt_cond_quagga) ;

        if (MQUEUE_DEFAULT_INTERVAL != 0)
          {
            mq->kick.cond.interval = MQUEUE_DEFAULT_INTERVAL ;
            mq->kick.cond.timeout  = qt_get_monotonic()
                                                     + MQUEUE_DEFAULT_INTERVAL ;
          } ;
        break;

      case mqt_signal_unicast:
      case mqt_signal_broadcast:
        /* kick.signal.head ) set to NULL already
         * kick.signal.tail )
         */
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
  mqueue_revoke(mq, NULL, 0) ;

  assert((mq->head == NULL) && (mq->count == 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Reset message queue -- empty it out by revoking everything.
 *
 * Frees the structure if required, and returns NULL.
 * Otherwise zeroises the structure, and returns address of same.
 *
 * NB: there MUST NOT be ANY waiters !
 *
 * NB: assumes caller has good reason to believe they have sole control !
 */
extern mqueue_queue
mqueue_reset(mqueue_queue mq, free_keep_b free_structure)
{
  if (mq == NULL)
    return NULL ;

  mqueue_empty(mq) ;

  passert(mq->waiters == 0) ;

  mq->mutex = qpt_mutex_destroy(mq->mutex) ;

  switch (mq->type)
    {
      case mqt_cond_unicast:
      case mqt_cond_broadcast:
        qpt_cond_destroy(mq->kick.cond.wait_here, keep_it) ;
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
    memset(mq, 0, sizeof(mqueue_queue_t)) ;

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
    lmq = XCALLOC(MTYPE_MQUEUE_QUEUE, sizeof(mqueue_local_queue_t)) ;
  else
    memset(lmq, 0, sizeof(mqueue_local_queue_t)) ;

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
 * Returns address of Local Message Queue
 */
extern mqueue_local_queue
mqueue_local_reset(mqueue_local_queue lmq, free_keep_b free_structure)
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
    memset(lmq, 0, sizeof(mqueue_local_queue_t)) ;

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
  MQUEUE_LOCK(mq) ;

  qassert( (mq->type == mqt_cond_unicast) ||
           (mq->type == mqt_cond_broadcast) ) ;

  mq->kick.cond.interval = interval ;
  mq->kick.cond.timeout  = (interval > 0) ? qt_add_monotonic(interval)
                                          : 0 ;
  MQUEUE_UNLOCK(mq) ;
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
      qpt_mutex_lock(mqb_mutex) ;     /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-*/

      mqb = mqb_free_list ;
      if (mqb == NULL)
        {
          qassert(mqb_free_count == 0) ;
          mqb = XMALLOC(MTYPE_MQUEUE_BLOCK, sizeof(struct mqueue_block)) ;
        }
      else
        {
          qassert(mqb_free_count >= 0) ;
          mqb_free_list = mqb->next ;
          --mqb_free_count ;
        } ;

      qpt_mutex_unlock(mqb_mutex) ;   /*->->->->->->->->->->->->->->->->->->->*/
    } ;

  memset(mqb, 0, sizeof(mqueue_block_t)) ;

  /* Zeroising the mqb sets:
   *
   *    args           -- zeroised
   *
   *    arg0           -- X       -- set below
   *
   *    next           -- NULL
   *
   *    action         -- X       -- set below
   *
   *    state          -- 0       -- mqb_s_undef
   */
  confirm(mqb_s_undef == 0) ;

  mqb->action = action ;
  mqb->arg0   = arg0 ;

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Free message block when done with it.
 *
 * NB: it is the caller's responsibility to free the value of any argument that
 *     requires it.
 */
extern mqueue_block
mqb_free(mqueue_block mqb)
{
  if (mqb == NULL)
    return NULL ;

  qpt_mutex_lock(mqb_mutex) ;     /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-*/

  mqb->next = mqb_free_list ;
  mqb_free_list = mqb ;
  ++mqb_free_count ;

  qpt_mutex_unlock(mqb_mutex) ;   /*->->->->->->->->->->->->->->->->->->->->->*/

  return NULL ;
} ;

/*==============================================================================
 * Enqueue and dequeue messages.
 */

static void mqueue_kick_signal(mqueue_queue mq, uint n) ;
static void mqueue_dequeue_signal(mqueue_queue mq, mqueue_thread_signal mtsig) ;
static mqueue_block mqb_revoke_this(mqueue_block this, mqueue_queue mq,
                                                            mqueue_block prev) ;

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
 * When queues message, sets mqb->state == mqb_s_queued.
 *
 * If mq is NULL, the message is not queued but is immediately destroyed.  The
 *
 *
 * NB: this works perfectly well if !qpthreads enabled.  Of course, there can
 *     never be any waiters... so no kicking is ever done.
 */
extern void
mqueue_enqueue(mqueue_queue mq, mqueue_block mqb, mqb_rank_b priority)
{
  qassert(mqb->state != mqb_s_queued) ;

  if (mq == NULL)
    {
      /* Trying to queue on a non-existent list is daft... but if a queue once
       * existed, but has been destroyed, then messages which were on the queue
       * at the time would have been revoked... so we treat this as if it had
       * made it to the queue before the queue was destroyed !
       */
      mqb->state = mqb_s_revoked ;
      return mqb_dispatch_destroy(mqb) ;
    } ;

  MQUEUE_LOCK(mq) ;

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
        qassert(mq->tail != NULL) ;
        mqb->next = NULL ;
        mq->tail->next = mqb ;
        mq->tail = mqb ;
      } ;
    } ;

  mqb->state = mqb_s_queued ;

  ++mq->count ;

  if (mq->waiters != 0)
    {
      qassert(qpthreads_enabled) ;  /* waiters == 0 if !qpthreads_enabled */

      switch (mq->type)
      {
        case mqt_cond_unicast:
          qpt_cond_signal(mq->kick.cond.wait_here) ;
          --mq->waiters ;
          break ;

        case mqt_cond_broadcast:
          qpt_cond_broadcast(mq->kick.cond.wait_here) ;
          mq->waiters = 0 ;
          break ;

        case mqt_signal_unicast:
          mqueue_kick_signal(mq, 1) ; /* pick off first and kick it (MUST be  */
                                      /* one) and decrement the waiters count */
          break ;

        case mqt_signal_broadcast:
          mqueue_kick_signal(mq, mq->waiters) ;
          qassert(mq->kick.signal.head == NULL) ;
          break;

        default:
          zabort("Invalid mqueue queue type") ;
      } ;
    } ;

  MQUEUE_UNLOCK(mq) ;
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
 * When dequeues message, sets mqb->state == mqb_s_undef.
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

  MQUEUE_LOCK(mq) ;

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
            qpt_cond_wait(mq->kick.cond.wait_here, mq->mutex) ;
          else
            {
              timeout_time = (arg != NULL) ? *(qtime_mono_t*)arg
                                           : mq->kick.cond.timeout ;

              if (!qpt_cond_timedwait(mq->kick.cond.wait_here, mq->mutex,
                                                               timeout_time))
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
          qassert(mtsig != NULL) ;

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

  /* Have something to pull off the queue
   */
  assert(mq->count > 0) ;
  --mq->count ;

  mq->head = mqb->next ;

  mqb->state = mqb_s_undef ;

  /* fix tails if at tail
   */
  if (mqb == mq->tail)
    mq->tail = NULL ;
  if (mqb == mq->tail_priority)
    mq->tail_priority = NULL ;

done:
  MQUEUE_UNLOCK(mq) ;

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
 * NB: for safety, holds the queue locked for the duration of the revoke
 *     operation.
 *
 *     If the destroy code can handle it, this means that can revoke stuff
 *     from one thread even though it is usually only dequeued by another.
 *
 *     The danger is that if queues get very long, and many revokes happen,
 *     may (a) spend a lot of time scanning the message queue, which stops
 *     other threads as soon as they try to enqueue anything, and (b) if this
 *     happens a lot, could end up in an O(n^2) thing scanning the message
 *     queue once for each revoked object type.
 *
 *     ALSO: mqb_dispatch_destroy() MUST NOT attempt to fiddle with the
 *           queue !!
 *
 *     AND:  mqb_dispatch_destroy() MUST avoid deadlocking on other mutexes !!
 *
 *           Simplest is to avoid all locking, with the exception of memory
 *           management or other "deep" stuff which definitely won't use this
 *           message queue's lock !
 *
 * If mq is NULL, does nothing.
 *
 * If num > 0, stops after revoking that many messages.
 *
 * Returns: number of messages revoked.
 */
extern int
mqueue_revoke(mqueue_queue mq, void* arg0, uint num)
{
  mqueue_block mqb ;
  mqueue_block prev ;
  uint  did ;

  if (mq == NULL)
    return 0 ;

  MQUEUE_LOCK(mq) ;
  mq->revoking = true ;

  did  = 0 ;
  prev = NULL ;
  mqb  = mq->head ;
  while (mqb != NULL)
    {
      if ((arg0 == NULL) || (arg0 == mqb->arg0))
        {
          mqb = mqb_revoke_this(mqb, mq, prev) ;

          ++did ;

          if (num == 1)
            break ;

          if (num > 1)
            --num ;
        }
      else
        {
          prev = mqb ;
          mqb  = mqb->next ;
        } ;
    } ;

  mq->revoking = false ;
  MQUEUE_UNLOCK(mq) ;

  return did ;
} ;

/*------------------------------------------------------------------------------
 * Revoke given mqb from given queue.
 *
 * There is some deep magic here.  The problem is that where a message queue is
 * used by more than one pthread, it is possible to become confused if more
 * than one pthread may revoke a given message.
 *
 * To avoid confusion, the message queue in question could have a higher level
 * lock, so that the state of a given message can be managed under that lock.
 *
 * But, it is possible for a message queue to be revoked, wholesale, which
 * makes a higher level lock more problematic, particularly where a message
 * queue may contain a number of quite different sorts of message.
 *
 * So, if we have a message that might be revoked by more than one pthread,
 * then (provided, of course, that the mqb_destroy operation does not free
 * the mqb) we can use this function to:
 *
 *   (a) revoke a given mqb, if it is on the queue -- in the usual way,
 *       calling mqb_dispatch_destroy() under the message queue lock.
 *
 * or:
 *
 *   (b) discover that the mqb has already been revoked.
 *
 * Once revoked, the mqb stays in that state until it is queued again.
 *
 * So, an mqb can be revoked by mqueue_revoke() or by mqb_revoke(), and
 * mqb_revoke() can be used to revoke or test the revocation state of a given
 * mqb.
 *
 * NB: this is only really useful if one pthread is responsible for enqueuing
 *     messages, or there is some other interlock to avoid being confused by
 *     learning that an mqb has been revoked, but then it being requeued by
 *     some other pthread !
 *
 * Returns: true <=> is now, or was already, revoked.
 *          false => not revoked
 */
extern bool
mqb_revoke(mqueue_block mqb, mqueue_queue mq)
{
  mqb_state_t mst ;

  if (mq == NULL)
    return true ;

  MQUEUE_LOCK(mq) ;
  mq->revoking = true ;

  mst = mqb->state ;

  if (mst == mqb_s_queued)
    {
      mqueue_block prev, this ;

      prev = NULL ;
      this = mq->head ;

      while ((mqb != this) && (this != NULL))
        {
          prev = this ;
          this = this->next ;
        } ;

      if (mqb == this)
        {
          /* Possible (if unlikely for this application) that the mqb will be
           * freed, so we do not depend on "this" hereafter.
           */
          mqb_revoke_this(this, mq, prev) ;
          mst = mqb_s_revoked ;
        }
      else
        qassert(false) ;
    } ;

  mq->revoking = false ;
  MQUEUE_UNLOCK(mq) ;

  return (mst == mqb_s_revoked) ;
} ;

/*------------------------------------------------------------------------------
 * Revoke given mqb from given mqueue.
 *
 * Must have the mqueue locked.
 *
 * Sets mqb_s_revoked.
 *
 * May free the given mqb !
 *
 * Returns: the next mqb
 */
static mqueue_block
mqb_revoke_this(mqueue_block this, mqueue_queue mq, mqueue_block prev)
{
  mqueue_block next ;

  assert(mq->count > 0) ;
  qassert(mq->revoking) ;

  next = this->next ;

  if (prev == NULL)
    mq->head   = next ;
  else
    prev->next = next ;

  if (this == mq->tail)
    mq->tail = prev ;

  if (this == mq->tail_priority)
    mq->tail_priority = prev ;

  --mq->count ;

  this->state = mqb_s_revoked ;

  mqb_dispatch_destroy(this) ;

  return next ;
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

  MQUEUE_LOCK(mq) ;

  qassert( (mq->type == mqt_signal_unicast) ||
           (mq->type == mqt_signal_broadcast) ) ;
  qassert(mtsig != NULL) ;

  /* When the thread is signalled, the prev entry is set NULL and the   */
  /* waiters count is decremented.                                      */
  /*                                                                    */
  /* So, only need to do something here if the prev is not NULL (ie the */
  /* mqueue_thread_signal is still on the list.                         */

  kicked = (mtsig->prev == NULL) ;

  if (!kicked)
    mqueue_dequeue_signal(mq, mtsig) ;

  MQUEUE_UNLOCK(mq) ;

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
mqueue_thread_signal_reset(mqueue_thread_signal mqt, free_keep_b free_structure)
{
  if (mqt == NULL)
    return NULL ;

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

  qassert( (qpthreads_enabled) && (mq->waiters >= n) ) ;
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
      qassert(mq->kick.signal.head == mtsig) ;
      mq->kick.signal.head = next ;
    }
  else
    {
      qassert((prev != NULL) && (prev->next == mtsig)) ;
      prev->next = next ;
    } ;

  if (next != NULL)
    next->prev = prev ;

  mtsig->next = NULL ;
  mtsig->prev = NULL ;          /* essential to show signal kicked      */
  --mq->waiters ;               /* one fewer waiter                     */

  qassert( ((mq->kick.signal.head == NULL) && (mq->waiters == 0)) ||
           ((mq->kick.signal.head != NULL) && (mq->waiters != 0)) ) ;
} ;

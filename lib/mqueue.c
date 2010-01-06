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
 *------------------------------------------------------------------------------
 * Message Blocks and Arguments
 *
 * Messages take the form of a small block of information which contain:
 *
 *   * action    -- void action(mqueue_block)   message dispatch
 *   * arguments -- each a union of: *void/uintptr_t/intptr_t
 *
 * There are set/get functions for action/arguments -- users should not poke
 * around inside the structure.
 *
 * To send a message, first allocate a message block (see mqb_init_new),
 * then fill in the arguments and enqueue it.
 *
 * The number of arguments is flexible, and the mqb handling looks after
 * the array of same.  Note:
 *
 *   * arg0 (aka argv[0]) implicitly exists and is implicitly a pointer.
 *
 *     This is expected to be used as the "context" for the message.
 *
 *     For specific revoke, arg0 is assumed to identify the messages to be
 *     revoked.
 *
 *   * arg1 (aka argv[1]) implicitly exists.
 *
 * The count of known arguments is, then, always at least 2.
 *
 * May set any number of arguments, and the count is extended to include the
 * highest index set.
 *
 * May get any argument by its index -- but it is a fatal error to attempt to
 * access a non-existent argument (one beyond the known count).
 *
 * May treat arguments from some index forward as a "list".  There is support
 * for pushing values onto the "list" and for iterating along the "list".
 * (But note that there is only one argv[] -- the "list" is not separate and
 * does not have separate indexes.)
 *
 *==============================================================================
 * Local Queues
 *
 * A local queue may be used within a thread to requeue messages for later
 * processing.
 *
 * Local queues are very simple FIFO queues.
 */

/*==============================================================================
 * Initialisation etc. for Message Queues.
 *
 * TODO: how to shut down a message queue... for reset/exit ?
 */

/*------------------------------------------------------------------------------
 * Initialise new Message Queue, if required (mq == NULL) allocating it.
 *
 * For mqt_cond_xxx type queues, sets the default timeout interval and the
 * initial timeout time to now + that interval.
 *
 * NB: once any message queue has been enabled, it is TOO LATE to enable
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
 * Dequeues entries and dispatches them "mqb_revoke", to empty the queue.
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

/*------------------------------------------------------------------------------
 * Size of argv_extension assuming the given *total* number of arguments.
 *
 * NB: expect there to always be >= mqb_argv_static_len arguments allocated.
 */
static inline size_t
mqb_extension_size(mqb_index_t arg_have)
{
  return sizeof(mqb_arg_t) * (arg_have - mqb_argv_static_len) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise message block (allocate if required) and set action & arg0.
 */
extern mqueue_block
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

  mqb->action    = action ;
  mqb->argv[0].p = arg0 ;

  mqb->arg_count = 2 ;  /* Always arg0 and arg1 (aka argv[0] and argv[1])   */
  mqb->arg_have  = mqb_argv_static_len ;

  /* Zeroising the mqb sets:
   *
   *    next           -- NULL
   *
   *    argv           -- everything zero or NULL
   *
   *    arg_list_base  -- no list
   *    arg_list_next  -- reset
   *
   *    argv_extension -- NULL -- no extension
   */

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Re-initialise message block (or allocate if required) and set action & arg0.
 *
 * NB: preserves any existing extension.
 *
 * NB: it is the caller's responsibility to free the value of any argument that
 *     requires it.
 */
extern mqueue_block
mqb_re_init(mqueue_block mqb, mqueue_action action, void* arg0)
{
  mqb_index_t arg_have ;
  mqb_arg_t*  argv_extension ;

  /* Exactly mqb_init_new if mqb is NULL                        */
  if (mqb == NULL)
    return mqb_init_new(NULL, action, arg0) ;

  /* Otherwise, need to put extension to one side first         */
  argv_extension = mqb->argv_extension ;
  arg_have       = mqb->arg_have ;

  mqb_init_new(mqb, action, arg0) ;

  /* Now zeroize the extension, and restore it                  */
  memset(argv_extension, 0, mqb_extension_size(arg_have)) ;

  mqb->argv_extension = argv_extension ;
  mqb->arg_have       = arg_have ;

  return mqb ;
} ;

/*------------------------------------------------------------------------------
 * Free message block when done with it.
 *
 * Frees an extension argument vector.
 *
 * NB: it is the caller's responsibility to free the value of any argument that
 *     requires it.
 */
extern void
mqb_free(mqueue_block mqb)
{
  if (mqb->argv_extension != NULL)
    XFREE(MTYPE_MQUEUE_BLOCK_EXT, mqb->argv_extension) ;

  qpt_mutex_lock(&mqb_mutex) ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  mqb->next = mqb_free_list ;
  mqb_free_list = mqb ;

  qpt_mutex_unlock(&mqb_mutex) ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
} ;

/*------------------------------------------------------------------------------
 * Make a new lot of empty message_block structures.
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

/*------------------------------------------------------------------------------
 * Enqueue message.
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

/*------------------------------------------------------------------------------
 * No longer waiting for a signal  -- does nothing if !qpthreads_enabled.
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

/*------------------------------------------------------------------------------
 * Enqueue message on local queue
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
 * Message Queue Block Argument Handling
 */

static void mqb_argv_extend(mqueue_block mqb, mqb_index_t iv) ;

/*------------------------------------------------------------------------------
 * Get pointer to argv[iv] -- extending if required
 */
inline static mqb_arg_t*
mqb_p_arg_set(mqueue_block mqb, mqb_index_t iv)
{
  if (iv >= mqb->arg_count)
    {
      if (iv >= mqb->arg_have)
        mqb_argv_extend(mqb, iv) ;
      mqb->arg_count = iv + 1 ;
    } ;

  if (iv < mqb_argv_static_len)
    return &mqb->argv[iv] ;
  else
    return &mqb->argv_extension[iv - mqb_argv_static_len] ;
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
 * Set start of "list" part of argv[] to be the given iv (must be > 0 !).
 */
extern void
mqb_set_argv_list(mqueue_block mqb, mqb_index_t iv)
{
  assert(iv > 0) ;

  mqb->arg_list_base = iv ;

  /* If there is a gap between the existing items and the start of the  */
  /* "list", then fill in upto the start of the "list"                  */
  if (iv > mqb->arg_count)
    mqb_set_argv_p(mqb, iv - 1, NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Push a pointer onto the "list"
 *
 * Implicitly starts list if not set by mqb_set_argv_list(), setting to
 * just past the last argument set -- noting that arg0 and arg1 are implicitly
 * set.
 */
extern void
mqb_push_argv_p(mqueue_block mqb, mqb_ptr_t  p)
{
  mqb_arg_t* p_arg ;

  if (mqb->arg_list_base == 0)
    mqb->arg_list_base = mqb->arg_count ;

  p_arg = mqb_p_arg_set(mqb, mqb->arg_count) ;
  p_arg->p = p ;
} ;

/*------------------------------------------------------------------------------
 * Push an integer onto the "list"
 *
 * Implicitly starts list if not set by mqb_set_argv_list(), setting to
 * just past the last argument set -- noting that arg0 and arg1 are implicitly
 * set.
 */
extern void
mqb_push_argv_i(mqueue_block mqb, mqb_int_t  i)
{
  mqb_arg_t* p_arg ;

  if (mqb->arg_list_base == 0)
    mqb->arg_list_base = mqb->arg_count ;

  p_arg = mqb_p_arg_set(mqb, mqb->arg_count) ;
  p_arg->i = i ;
} ;

/*------------------------------------------------------------------------------
 * Push an unsigned integer onto the "list"
 *
 * Implicitly starts list if not set by mqb_set_argv_list(), setting to
 * just past the last argument set -- noting that arg0 and arg1 are implicitly
 * set.
 */
extern void
mqb_push_argv_u(mqueue_block mqb, mqb_uint_t u)
{
  mqb_arg_t* p_arg ;

  if (mqb->arg_list_base == 0)
    mqb->arg_list_base = mqb->arg_count ;

  p_arg = mqb_p_arg_set(mqb, mqb->arg_count) ;
  p_arg->u = u ;
} ;

/*------------------------------------------------------------------------------
 * Push an array of 'n' void* pointers
 *
 * Implicitly starts list if not set by mqb_set_argv_list(), setting to
 * just past the last argument set -- noting that arg0 and arg1 are implicitly
 * set.
 */
extern void
mqb_push_argv_array(mqueue_block mqb, unsigned n, void** array)
{
  mqb_arg_t*  p_arg ;
  unsigned    m ;

  mqb_index_t iv = mqb->arg_count ;

  if (mqb->arg_list_base == 0)
    mqb->arg_list_base = iv ;

  /* need do nothing more if n == 0, get out now to avoid edge cases    */
  if (n == 0)
    return ;

  /* make sure we are allocated upto and including the last array item  */
  p_arg = mqb_p_arg_set(mqb, iv + n - 1) ;

  /* require that mqb_ptr_t values exactly fill mqb_arg_t entries       */
  /*     and that mqb_ptr_t values are exactly same as void* values     */
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(mqb_arg_t)) ;
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(void*)) ;

  /* copy the pointers                                                  */
  p_arg = mqb_p_arg_set(mqb, iv) ;      /* get address for first item   */

  if (iv < mqb_argv_static_len)
    {
      m = mqb_argv_static_len - iv ;    /* deal with static part        */
      if (n < m)
        m = n ;

      memcpy(p_arg, array, sizeof(void*) * m) ;

      n -= m ;
      if (n == 0)
        return ;                        /* quit now if done everything  */

      array += m ;                      /* advance past stuff copied    */
      p_arg  = mqb->argv_extension ;    /* rest goes in the extension   */
    } ;

  memcpy(p_arg, array, sizeof(void*) * n) ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to argv[iv] -- which MUST exist
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 */
inline static mqb_arg_t*
mqb_p_arg_get(mqueue_block mqb, mqb_index_t iv)
{
  if (iv >= mqb->arg_count)
    zabort("invalid message block argument index") ;

  if (iv < mqb_argv_static_len)
    return &mqb->argv[iv] ;
  else
    return &mqb->argv_extension[iv - mqb_argv_static_len] ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer value of argv[iv]
 *
 * NB: it is a FATAL error to reference an argument beyond the last one set.
 *
 *     arg0 and arg1 are implicitly set.
 *
 *     mqb_get_arg_count() returns the number of arguments set, including any
 *     "list" part.
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
 *     arg0 and arg1 are implicitly set.
 *
 *     mqb_get_arg_count() returns the number of arguments set, including any
 *     "list" part.
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
 *     arg0 and arg1 are implicitly set.
 *
 *     mqb_get_arg_count() returns the number of arguments set, including any
 *     "list" part.
 */
extern mqb_uint_t
mqb_get_argv_u(mqueue_block mqb, mqb_index_t iv)
{
  mqb_arg_t* p_arg = mqb_p_arg_get(mqb, iv) ;
  return p_arg->u ;
} ;

/*------------------------------------------------------------------------------
 * Get iv for the first argument in the "list" portion (if any).
 *
 * Returns:  0 => no list portion
 *          iv    1..n: can be used to access the "list" portion using
 *                      mqb_get_argv_x().  BUT, watch out for empty lists !
 */
extern mqb_index_t
mqb_get_argv_list_base(mqueue_block mqb)
{
  return mqb->arg_list_base ;
} ;

/*------------------------------------------------------------------------------
 * Get the number of arguments in the "list" portion (if any).
 *
 * Returns:  0 => no or empty list portion
 *
 * Resets the "next" counter -- see mqb_next_argv_x() below.
 */
extern mqb_index_t
mqb_get_argv_list_count(mqueue_block mqb)
{
  mqb->arg_list_next = 0 ;
  return (mqb->arg_list_base == 0) ? 0 : mqb->arg_count - mqb->arg_list_base ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer value of next "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and when mqb_get_list_count() is
 * called.
 *
 * NB: returns NULL if there is no "list" or if already at the end of same.
 */
extern mqb_ptr_t
mqb_next_argv_p(mqueue_block mqb)
{
  if (mqb->arg_list_next == 0)
    mqb->arg_list_next = mqb->arg_list_base ;

  if ((mqb->arg_list_base == 0) || (mqb->arg_list_next >= mqb->arg_count))
    return NULL ;

  return mqb_get_argv_p(mqb, mqb->arg_list_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Get integer value of next "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and when mqb_get_list_count() is
 * called.
 *
 * NB: returns 0 if there is no "list" or if already at the end of same.
 */
extern mqb_int_t
mqb_next_argv_i(mqueue_block mqb)
{
  if (mqb->arg_list_next == 0)
    mqb->arg_list_next = mqb->arg_list_base ;

  if ((mqb->arg_list_base == 0) || (mqb->arg_list_next >= mqb->arg_count))
    return 0 ;

  return mqb_get_argv_i(mqb, mqb->arg_list_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Get unsigned integer value of next "list" argument -- if any.
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and when mqb_get_list_count() is
 * called.
 *
 * NB: returns 0 if there is no "list" or if already at the end of same.
 */
extern mqb_uint_t
mqb_next_argv_u(mqueue_block mqb)
{
  if (mqb->arg_list_next == 0)
    mqb->arg_list_next = mqb->arg_list_base ;

  if ((mqb->arg_list_base == 0) || (mqb->arg_list_next >= mqb->arg_count))
    return 0 ;

  return mqb_get_argv_u(mqb, mqb->arg_list_next++) ;
} ;

/*------------------------------------------------------------------------------
 * Pop an array of 'n' void* pointers
 *
 * There is a "next" counter in the message queue block, which is reset when
 * the mqb is initialised or re-initialised, and when mqb_get_list_count() is
 * called.
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
  void**      p_item ;
  unsigned    n, m ;
  mqb_index_t iv ;
  mqb_arg_t*  p_arg ;

  /* worry about state of "next" and get out if nothing to do.          */
  if (mqb->arg_list_next == 0)
    mqb->arg_list_next = mqb->arg_list_base ;

  if ((mqb->arg_list_base == 0) || (mqb->arg_list_next >= mqb->arg_count))
    return NULL ;

  /* work out what we are popping and update "next"                     */
  iv = mqb->arg_list_next ;
  n  = mqb->arg_count - iv ;

  mqb->arg_list_next = mqb->arg_count ;

  /* construct target array                                             */
  array = XMALLOC(MTYPE_TMP, sizeof(void*) * n) ;

  /* require that mqb_ptr_t values exactly fill mqb_arg_t entries       */
  /*     and that mqb_ptr_t values are exactly same as void* values     */
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(mqb_arg_t)) ;
  CONFIRM(sizeof(mqb_ptr_t) == sizeof(void*)) ;

  /* now transfer pointers to the array                                 */
  p_item = array ;                      /* address for first item       */
  p_arg  = mqb_p_arg_get(mqb, iv) ;     /* get address of first item    */

  if (iv < mqb_argv_static_len)
    {
      m = mqb_argv_static_len - iv ;
      if (n < m)
        m = n ;
      memcpy(p_item, p_arg, sizeof(void*) * m) ;

      n -= m ;                          /* count down                   */
      if (n == 0)
        return array ;                  /* all done                     */

      p_item += m ;                     /* step past copied stuff       */
      p_arg  = mqb->argv_extension ;    /* rest from the extension      */
    } ;

  memcpy(p_item, p_arg, sizeof(void*) * n) ;

  return array ;
} ;

/*------------------------------------------------------------------------------
 * Extend the argv to include at least given iv.
 *
 * The number of argv slots available is arranged to be a multiple of
 * mqb_argv_static_len.
 *
 * Ensures that newly created slots are zeroised.
 */
static void
mqb_argv_extend(mqueue_block mqb, mqb_index_t iv)
{
  mqb_index_t need ;                    /* total slots required         */
  size_t      new_size, old_size ;      /* sizes of the extension part  */

  assert(mqb->arg_have >= mqb_argv_static_len) ;
  assert(mqb->arg_have <= iv) ;

  need = ((iv / mqb_argv_static_len) + 1) * mqb_argv_static_len ;
  new_size = mqb_extension_size(need) ;

  if (mqb->argv_extension == NULL)
    mqb->argv_extension = XCALLOC(MTYPE_MQUEUE_BLOCK_EXT, new_size) ;
  else
    {
      mqb->argv_extension = XREALLOC(MTYPE_MQUEUE_BLOCK_EXT,
                                     mqb->argv_extension, new_size) ;

      old_size = mqb_extension_size(mqb->arg_have) ;
      memset(mqb->argv_extension + old_size, 0, new_size - old_size) ;
    } ;

  mqb->arg_have = need ;
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

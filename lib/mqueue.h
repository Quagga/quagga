/* Message Queue data structure -- header
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

#ifndef _ZEBRA_MQUEUE_H
#define _ZEBRA_MQUEUE_H

#include "misc.h"

#include "qpthreads.h"
#include "qtime.h"

/*==============================================================================
 * Message Queue Blocks -- mqb
 *
 * NB: mqueue_block structures are malloced, which guarantees maximum alignment.
 *     To guarantee maximum alignment for the user specified "struct args", it
 *     is the first item !
 */

typedef struct mqueue_block  mqueue_block_t ;
typedef struct mqueue_block* mqueue_block ;

typedef enum
{
  mqb_destroy   = 0,
  mqb_action    = 1
} mqb_flag_t ;

typedef enum
{
  mqb_s_undef    = 0,
  mqb_s_queued   = 1,
  mqb_s_revoked  = 2,
} mqb_state_t ;

typedef void mqueue_action(mqueue_block mqb, mqb_flag_t flag) ;

enum { mqb_args_size_max  = 64 } ;      /* maximum size of struct args  */

typedef struct mqb_args
{
  char bytes[mqb_args_size_max] ;       /* empty space                  */
} mqb_args_t ;

#define MQB_ARGS_SIZE_OK(s) CONFIRM(sizeof(s) <= sizeof(mqb_args_t))

struct mqueue_block
{
  mqb_args_t      args ;        /* user structure               */
  void*           arg0 ;

  mqueue_block    next ;        /* single linked list           */

  mqueue_action*  action ;      /* for message dispatch         */

  mqb_state_t     state ;       /* see...                       */
} ;

CONFIRM(offsetof(mqueue_block_t, args) == 0) ;

/*==============================================================================
 * The Message Queue itself
 */
typedef struct mqueue_thread_signal  mqueue_thread_signal_t ;
typedef struct mqueue_thread_signal* mqueue_thread_signal ;

struct mqueue_thread_signal {
  mqueue_thread_signal  next ;  /* NULL => last on list                 */
  mqueue_thread_signal  prev ;  /* NULL => NOT on list -- vital !       */

  qpt_thread_t  qpthread ;      /* qpthread to kick             */
  int           signum ;        /* signal to kick with          */
} ;

typedef enum {
  mqt_cond_unicast,     /* use qpt_cond_signal to kick the queue        */
  mqt_cond_broadcast,   /* use qpt_cond_broadcast to kick the queue     */
  mqt_signal_unicast,   /* use single qpt_signal to kick the queue      */
  mqt_signal_broadcast, /* use multiple qpt_signal to kick the queue    */
} mqueue_queue_type_t ;

#ifndef  MQUEUE_DEFAULT_INTERVAL
# define MQUEUE_DEFAULT_INTERVAL QTIME(5)
#endif

typedef struct mqueue_queue_cond  mqueue_queue_cond_t ;
typedef struct mqueue_queue_cond* mqueue_queue_cond ;

struct mqueue_queue_cond {
  qpt_cond_t   wait_here ;
  qtime_mono_t timeout ;
  qtime_t      interval ;
} ;

typedef struct mqueue_queue_signal  mqueue_queue_signal_t ;
typedef struct mqueue_queue_signal* mqueue_queue_signal ;

struct mqueue_queue_signal {
  mqueue_thread_signal head ;   /* NULL => list is empty        */
  mqueue_thread_signal tail ;
};

typedef struct mqueue_queue  mqueue_queue_t ; /* Forward reference    */
typedef struct mqueue_queue* mqueue_queue ;

struct mqueue_queue
{
  qpt_mutex     mutex ;

  mqueue_block  head ;          /* NULL => list is empty                      */
  mqueue_block  tail_priority ; /* last priority message (if any & not empty) */
  mqueue_block  tail ;          /* last message (if not empty)                */

  uint          count ;         /* of items on the queue                      */

  mqueue_queue_type_t  type ;
  bool          revoking ;

  uint          waiters ;

  union {
    mqueue_queue_cond_t   cond ;
    mqueue_queue_signal_t signal ;
  } kick ;
} ;

typedef struct mqueue_local_queue  mqueue_local_queue_t ;
typedef struct mqueue_local_queue* mqueue_local_queue ;

struct mqueue_local_queue
{
  mqueue_block head ;           /* NULL => list is empty                      */
  mqueue_block tail ;           /* last message (if not empty)                */
} ;

/*==============================================================================
 * Locking
 */
Inline void
MQUEUE_LOCK(mqueue_queue mq)
{
  qpt_mutex_lock(mq->mutex) ;
  qassert(!mq->revoking) ;
} ;

Inline void
MQUEUE_UNLOCK(mqueue_queue mq)
{
  qpt_mutex_unlock(mq->mutex) ;
} ;

/*==============================================================================
 * Functions
 */

extern void mqueue_initialise(void) ;
extern void mqueue_finish(void) ;

extern mqueue_queue mqueue_init_new(mqueue_queue mq, mqueue_queue_type_t type,
                                                             const char* name) ;
extern void mqueue_empty(mqueue_queue mq) ;
extern mqueue_queue mqueue_reset(mqueue_queue mq, free_keep_b free_structure) ;

extern mqueue_local_queue mqueue_local_init_new(mqueue_local_queue lmq) ;
extern mqueue_local_queue mqueue_local_reset(mqueue_local_queue lmq,
                                                   free_keep_b free_structure) ;

extern void mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval) ;
extern mqueue_thread_signal mqueue_thread_signal_init(mqueue_thread_signal mqt,
                                              qpt_thread_t thread, int signum) ;
mqueue_thread_signal mqueue_thread_signal_reset(mqueue_thread_signal mqt,
                                                   free_keep_b free_structure) ;

extern mqueue_block mqb_init_new(mqueue_block mqb, mqueue_action action,
                                                                   void* arg0) ;
extern mqueue_block mqb_free(mqueue_block mqb) ;

typedef enum
{
  mqb_priority  = true,
  mqb_ordinary  = false
} mqb_rank_b ;

extern void mqueue_enqueue(mqueue_queue mq, mqueue_block mqb,
                                                          mqb_rank_b priority) ;
extern mqueue_block mqueue_dequeue(mqueue_queue mq, int wait, void* arg) ;
extern int mqueue_revoke(mqueue_queue mq, void* arg0, uint num) ;

extern bool mqb_revoke(mqueue_block mqb, mqueue_queue mq) ;

extern int mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig) ;

extern void mqueue_local_enqueue(mqueue_local_queue lmq, mqueue_block mqb) ;
extern void mqueue_local_enqueue_head(mqueue_local_queue lmq, mqueue_block mqb) ;
Inline mqueue_block mqueue_local_head(mqueue_local_queue lmq) ;
extern mqueue_block mqueue_local_dequeue(mqueue_local_queue lmq) ;

/*==============================================================================
 * Access functions for mqueue_block fields -- mqb_set_xxx/mqb_get_xxx
 *
 * Users should not poke around inside the mqueue_block structure.
 */
Inline void mqb_set_action(mqueue_block mqb, mqueue_action action) ;

Inline void mqb_set_arg0(mqueue_block mqb, void* p) ;

Inline void* mqb_get_arg0(mqueue_block mqb) ;
Inline void* mqb_get_args(mqueue_block mqb) ;

/* NB: the following require that the mqb is *not* mqb_s_queued.
 */
Inline void mqb_dispatch(mqueue_block mqb, mqb_flag_t flag) ;
Inline void mqb_dispatch_action(mqueue_block mqb) ;
Inline void mqb_dispatch_destroy(mqueue_block mqb) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Get head of given local queue -- returns NULL if no queue (!)
 */
Inline mqueue_block
mqueue_local_head(mqueue_local_queue lmq)
{
  return (lmq != NULL) ? lmq->head : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set operations.
 */
Inline void
mqb_set_action(mqueue_block mqb, mqueue_action action)
{
  mqb->action = action ;
} ;

Inline void
mqb_set_arg0(mqueue_block mqb, void* arg0)
{
  mqb->arg0 = arg0 ;
} ;

/*------------------------------------------------------------------------------
 * Get operations -- NB: mqb MUST exist !
 */

Inline void*
mqb_get_arg0(mqueue_block mqb)
{
  return mqb->arg0 ;
} ;

Inline void*
mqb_get_args(mqueue_block mqb)
{
  return &mqb->args ;
} ;

/*------------------------------------------------------------------------------
 * It would be a BAD mistake to dispatch a message that was on a queue.
 *
 * Of course, while can check the mqb->state, it is possible for another thread
 * to dequeue the mqb while this is going on.  So, the check is really just a
 * reminder of required use.
 *
 * There might be a need to dispatch a message that has been revoked... but
 * that is not something that is legislated for or against here.
 */

Inline void
mqb_dispatch(mqueue_block mqb, mqb_flag_t flag)
{
  qassert(mqb->state != mqb_s_queued) ;
  mqb->action(mqb, flag) ;
} ;

Inline void
mqb_dispatch_action(mqueue_block mqb)
{
  qassert(mqb->state != mqb_s_queued) ;
  mqb->action(mqb, mqb_action) ;
} ;

Inline void
mqb_dispatch_destroy(mqueue_block mqb)
{
  qassert(mqb->state != mqb_s_queued) ;
  mqb->action(mqb, mqb_destroy) ;
} ;

#endif /* _ZEBRA_MQUEUE_H */

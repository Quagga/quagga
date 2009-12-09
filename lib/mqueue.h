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

#include "qpthreads.h"
#include "qtime.h"

/*==============================================================================
 */

typedef struct mqueue_block* mqueue_block ;

typedef uint32_t  mqueue_flags_t ;
typedef uint32_t  mqueue_context_t ;
typedef union
{
  void*     p ;
  uintptr_t u ;
  intptr_t  i ;
} mqueue_arg_t ;

typedef void mqueue_action(mqueue_block) ;

struct mqueue_block
{
  mqueue_block   next ;         /* single linked list -- see ...        */

  mqueue_action* action ;       /* for message dispatch                 */

  mqueue_flags_t flags ;        /* for message handler                  */

  mqueue_context_t context ;    /* for message revoke                   */

  mqueue_arg_t   arg0 ;  /* may be pointer to more data or integer      */
  mqueue_arg_t   arg1 ;  /* may be pointer to more data or integer      */
} ;

typedef struct mqueue_thread_signal* mqueue_thread_signal ;

struct mqueue_thread_signal {
  mqueue_thread_signal  next ;  /* NULL => last on list                 */
  mqueue_thread_signal  prev ;  /* NULL => NOT on list -- vital !       */

  qpt_thread_t  qpthread ;      /* qpthread to kick             */
  int           signum ;        /* signal to kick with          */
} ;

enum mqueue_queue_type {
  mqt_cond_unicast,     /* use qpt_cond_signal to kick the queue        */
  mqt_cond_broadcast,   /* use qpt_cond_broadcast to kick the queue     */
  mqt_signal_unicast,   /* use single qpt_signal to kick the queue      */
  mqt_signal_broadcast, /* use multiple qpt_signal to kick the queue    */
};

#ifndef  MQUEUE_DEFAULT_INTERVAL
# define MQUEUE_DEFAULT_INTERVAL QTIME(5)
#endif

struct mqueue_queue_cond {
  qpt_cond_t   wait_here ;
  qtime_mono_t timeout ;
  qtime_t      interval ;
} ;

struct mqueue_queue_signal {
  mqueue_thread_signal head ;   /* NULL => list is empty        */
  mqueue_thread_signal tail ;
};

typedef struct mqueue_queue* mqueue_queue ;

struct mqueue_queue
{
  qpt_mutex_t   mutex ;

  mqueue_block head ;          /* NULL => list is empty                      */
  mqueue_block tail_priority ; /* last priority message (if any & not empty) */
  mqueue_block tail ;          /* last message (if not empty)                */

  enum mqueue_queue_type    type ;

  unsigned      waiters ;

  union {
    struct mqueue_queue_cond   cond ;
    struct mqueue_queue_signal signal ;
  } kick ;
} ;

/*==============================================================================
 * Functions
 */

void
mqueue_initialise(int qpthreads) ;

mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type) ;

void
mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval) ;

mqueue_thread_signal
mqueue_thread_signal_init(mqueue_thread_signal mqt, qpt_thread_t thread,
                                                                   int signum) ;
mqueue_block
mqueue_block_new(void) ;

void
mqueue_block_free(mqueue_block mb) ;

void
mqueue_enqueue(mqueue_queue mq, mqueue_block mb, int priority) ;

mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, void* arg) ;

int
mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig) ;

#endif /* _ZEBRA_MQUEUE_H */

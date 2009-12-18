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

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 */

typedef struct mqueue_block* mqueue_block ;

typedef uint32_t  mqb_flags_t ;
typedef uint32_t  mqb_context_t ;

typedef void*     mqb_ptr_t ;
typedef intptr_t  mqb_int_t ;
typedef uintptr_t mqb_uint_t ;

typedef union
{
  mqb_ptr_t  p ;
  mqb_int_t  i ;
  mqb_uint_t u ;
} mqb_arg_t ;

enum mqb_flag
{
  mqb_revoke    = 0,
  mqb_action    = 1
} ;

typedef enum mqb_flag mqb_flag_t ;

typedef void mqueue_action(mqueue_block mqb, mqb_flag_t flag) ;

struct mqueue_block
{
  mqueue_block   next ;         /* single linked list                   */

  mqueue_action* action ;       /* for message dispatch                 */

  mqb_flags_t    flags ;        /* for message handler                  */

  void*          arg0 ;         /* NB: used for specific revoke         */
  mqb_arg_t      arg1 ;         /* may be pointer or integer            */
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
  qpt_mutex_t  mutex ;

  mqueue_block head ;           /* NULL => list is empty                      */
  mqueue_block tail_priority ;  /* last priority message (if any & not empty) */
  mqueue_block tail ;           /* last message (if not empty)                */

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
mqueue_initialise(void) ;

mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type) ;

void
mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval) ;

mqueue_thread_signal
mqueue_thread_signal_init(mqueue_thread_signal mqt, qpt_thread_t thread,
                                                                   int signum) ;
mqueue_block
mqb_init_new(mqueue_block mqb, mqueue_action action, void* arg0) ;

void
mqb_free(mqueue_block mqb) ;

void
mqueue_enqueue(mqueue_queue mq, mqueue_block mqb, int priority) ;

mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, void* arg) ;

int
mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig) ;

/*==============================================================================
 * Access functions for mqueue_block fields -- mqb_set_xxx/mqb_get_xxx
 *
 * Users should not poke around inside the mqueue_block structure.
 */

Inline void mqb_set_action(mqueue_block mqb, mqueue_action action) ;

Inline void mqb_set_arg0(mqueue_block mqb, void* p) ;
Inline void mqb_set_arg1_p(mqueue_block mqb, mqb_ptr_t  p) ;
Inline void mqb_set_arg1_i(mqueue_block mqb, mqb_int_t  i) ;
Inline void mqb_set_arg1_u(mqueue_block mqb, mqb_uint_t u) ;

Inline void mqb_dispatch(mqueue_block mqb, mqb_flag_t flag) ;

Inline void*  mqb_get_arg0(mqueue_block mqb) ;
Inline mqb_ptr_t  mqb_get_arg1_p(mqueue_block mqb) ;
Inline mqb_int_t  mqb_get_arg1_i(mqueue_block mqb) ;
Inline mqb_uint_t mqb_get_arg1_u(mqueue_block mqb) ;

/*==============================================================================
 * The Inline functions.
 */

/* Set operations.      */

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

Inline void
mqb_set_arg1_p(mqueue_block mqb, mqb_ptr_t p)
{
  mqb->arg1.p = p ;
} ;

Inline void
mqb_set_arg1_i(mqueue_block mqb, mqb_int_t i)
{
  mqb->arg1.i = i ;
} ;

Inline void
mqb_set_arg1_u(mqueue_block mqb, mqb_uint_t u)
{
  mqb->arg1.u = u ;
} ;

/* Get operations       */

Inline void
mqb_dispatch(mqueue_block mqb, mqb_flag_t flag)
{
  mqb->action(mqb, flag) ;
} ;

Inline void*
mqb_get_arg0(mqueue_block mqb)
{
  return mqb->arg0 ;
} ;

Inline mqb_ptr_t
mqb_get_arg1_p(mqueue_block mqb)
{
  return mqb->arg1.p ;
} ;

Inline mqb_int_t
mqb_get_arg1_i(mqueue_block mqb)
{
  return mqb->arg1.i ;
} ;

Inline mqb_uint_t
mqb_get_arg1_u(mqueue_block mqb)
{
  return mqb->arg1.u ;
} ;

#endif /* _ZEBRA_MQUEUE_H */

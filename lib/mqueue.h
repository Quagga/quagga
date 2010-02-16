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

#include <stddef.h>

#include "qpthreads.h"
#include "qtime.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Message Queue Blocks -- mqb
 *
 * Messages in a message queue are held as Message Queue Blocks:
 *
 *   * action -- function to call when message is dispatched
 *
 *   * arg0   -- always a pointer -- used in specific revoke
 *
 *   * args   -- embedded structure -- to be overlaid by user structure
 *
 *   * argv   -- pointer to: list/array of pointer/integer/unsigned
 *
 * NB: the elements of argv are all exactly the same size and alignment.
 *
 *     So, as well as using the access functions, it is possible to use the
 *     argv array directly, as any of:
 *
 *       mqb_arg_t*  argv = mqb_get_argv(mqb) ;
 *
 *       void**      argv = mqb_get_argv(mqb) ;
 *       char**      argv = mqb_get_argv(mqb) ;
 *
 *       mqb_ptr_t*  argv = mqb_get_argv(mqb) ;
 *       mqb_int_t*  argv = mqb_get_argv(mqb) ;
 *       mqb_uint_t* argv = mqb_get_argv(mqb) ;
 */

typedef struct mqueue_block* mqueue_block ;

typedef void*     mqb_ptr_t ;
typedef intptr_t  mqb_int_t ;
typedef uintptr_t mqb_uint_t ;

typedef unsigned short mqb_index_t ;

typedef union
{
  mqb_ptr_t  p ;
  mqb_int_t  i ;
  mqb_uint_t u ;
} mqb_arg_t ;

/* argv is an array of mqb_arg_t, which is the same as an array of....        */
CONFIRM(sizeof(mqb_arg_t) == sizeof(void*)) ;       /* ... pointers           */
CONFIRM(sizeof(mqb_arg_t) == sizeof(mqb_ptr_t)) ;   /* ... mqb_ptr_t          */
CONFIRM(sizeof(mqb_arg_t) == sizeof(mqb_int_t)) ;   /* ... mqb_int_t          */
CONFIRM(sizeof(mqb_arg_t) == sizeof(mqb_uint_t)) ;  /* ... mqb_uint_t         */
                                                    /* ... or any combination */
enum mqb_flag
{
  mqb_destroy   = 0,
  mqb_action    = 1
} ;

typedef enum mqb_flag mqb_flag_t ;

typedef void mqueue_action(mqueue_block mqb, mqb_flag_t flag) ;

enum { mqb_args_size_max  = 64 } ;      /* maximum size of struct args  */
enum { mqb_argv_size_unit = 16 } ;      /* allocate argv in these units */

struct mqb_args
{
  char bytes[mqb_args_size_max] ;       /* empty space                  */
} ;

#define MQB_ARGS_SIZE_OK(s) CONFIRM(sizeof(struct s) <= mqb_args_size_max)

struct mqueue_block
{
  struct mqb_args args ;                /* user structure               */

  mqueue_block    next ;                /* single linked list           */

  mqueue_action*  action ;              /* for message dispatch         */

  void*           arg0 ;
  mqb_arg_t*      argv ;                /* argv, if any                 */

  mqb_index_t argv_count ;              /* count of elements in argv    */
  mqb_index_t argv_alloc ;              /* count of elements allocated  */
  mqb_index_t argv_next ;               /* iterator                     */
} ;

/* mqueue_block structures are malloced.  That guarantees maximum alignment.
 * To guarantee maximum alignment for "struct args", it must be first item !
 *
 * (The typedef is required to stop Eclipse (3.4.2 with CDT 5.0) whining
 *  about first argument of offsetof().)
 */
typedef struct mqueue_block mqueue_block_t ;
CONFIRM(offsetof(mqueue_block_t, args) == 0) ;

/*==============================================================================
 * The Message Queue itself
 */

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

  mqueue_block  head ;          /* NULL => list is empty                      */
  mqueue_block  tail_priority ; /* last priority message (if any & not empty) */
  mqueue_block  tail ;          /* last message (if not empty)                */

  unsigned      count ;         /* of items on the queue                      */

  enum mqueue_queue_type    type ;

  unsigned      waiters ;

  union {
    struct mqueue_queue_cond   cond ;
    struct mqueue_queue_signal signal ;
  } kick ;
} ;

typedef struct mqueue_local_queue* mqueue_local_queue ;

struct mqueue_local_queue
{
  mqueue_block head ;           /* NULL => list is empty                      */
  mqueue_block tail ;           /* last message (if not empty)                */
} ;

/*==============================================================================
 * Functions
 */

extern void
mqueue_initialise(void) ;

extern mqueue_queue
mqueue_init_new(mqueue_queue mq, enum mqueue_queue_type type) ;

extern void
mqueue_empty(mqueue_queue mq) ;

extern mqueue_queue
mqueue_reset(mqueue_queue mq, int free_structure) ;

#define mqueue_reset_keep(mq) mqueue_reset(mq, 0)
#define mqueue_reset_free(mq) mqueue_reset(mq, 1)

extern mqueue_local_queue
mqueue_local_init_new(mqueue_local_queue lmq) ;

extern mqueue_local_queue
mqueue_local_reset(mqueue_local_queue lmq, int free_structure) ;

#define mqueue_local_reset_keep(lmq) mqueue_local_reset(lmq, 0)
#define mqueue_local_reset_free(lmq) mqueue_local_reset(lmq, 1)

extern void
mqueue_set_timeout_interval(mqueue_queue mq, qtime_t interval) ;

extern mqueue_thread_signal
mqueue_thread_signal_init(mqueue_thread_signal mqt, qpt_thread_t thread,
                                                                   int signum) ;
mqueue_thread_signal
mqueue_thread_signal_reset(mqueue_thread_signal mqt, int free_structure) ;

#define mqueue_thread_signal_reset_keep(mqt) mqueue_thread_signal_reset(mqt, 0)
#define mqueue_thread_signal_reset_free(mqt) mqueue_thread_signal_reset(mqt, 1)

extern mqueue_block
mqb_init_new(mqueue_block mqb, mqueue_action action, void* arg0) ;

extern mqueue_block
mqb_re_init(mqueue_block mqb, mqueue_action action, void* arg0) ;

extern void
mqb_free(mqueue_block mqb) ;

extern void
mqueue_enqueue(mqueue_queue mq, mqueue_block mqb, int priority) ;

extern mqueue_block
mqueue_dequeue(mqueue_queue mq, int wait, void* arg) ;

extern void
mqueue_revoke(mqueue_queue mq, void* arg0) ;

extern int
mqueue_done_waiting(mqueue_queue mq, mqueue_thread_signal mtsig) ;

extern void
mqueue_local_enqueue(mqueue_local_queue lmq, mqueue_block mqb) ;

extern void
mqueue_local_enqueue_head(mqueue_local_queue lmq, mqueue_block mqb) ;

Inline mqueue_block
mqueue_local_head(mqueue_local_queue lmq) ;

extern mqueue_block
mqueue_local_dequeue(mqueue_local_queue lmq) ;

/*==============================================================================
 * Access functions for mqueue_block fields -- mqb_set_xxx/mqb_get_xxx
 *
 * Users should not poke around inside the mqueue_block structure.
 */

Inline void mqb_set_action(mqueue_block mqb, mqueue_action action) ;

Inline void mqb_set_arg0(mqueue_block mqb, void* p) ;

extern void mqb_set_argv_size(mqueue_block mqb, unsigned n) ;

extern void mqb_set_argv_p(mqueue_block mqb, mqb_index_t iv, mqb_ptr_t  p) ;
extern void mqb_set_argv_i(mqueue_block mqb, mqb_index_t iv, mqb_int_t  i) ;
extern void mqb_set_argv_u(mqueue_block mqb, mqb_index_t iv, mqb_uint_t u) ;

extern void mqb_push_argv_p(mqueue_block mqb, mqb_ptr_t  p) ;
extern void mqb_push_argv_i(mqueue_block mqb, mqb_int_t  i) ;
extern void mqb_push_argv_u(mqueue_block mqb, mqb_uint_t u) ;

extern void mqb_push_argv_array(mqueue_block mqb, unsigned n, void** array) ;

Inline void mqb_dispatch(mqueue_block mqb, mqb_flag_t flag) ;
Inline void mqb_dispatch_action(mqueue_block mqb) ;
Inline void mqb_dispatch_destroy(mqueue_block mqb) ;

Inline void* mqb_get_arg0(mqueue_block mqb) ;
Inline void* mqb_get_args(mqueue_block mqb) ;
Inline void* mqb_get_argv(mqueue_block mqb) ;

Inline mqb_index_t mqb_get_argv_count(mqueue_block mqb) ;

extern mqb_ptr_t  mqb_get_argv_p(mqueue_block mqb, mqb_index_t iv) ;
extern mqb_int_t  mqb_get_argv_i(mqueue_block mqb, mqb_index_t iv) ;
extern mqb_uint_t mqb_get_argv_u(mqueue_block mqb, mqb_index_t iv) ;

extern mqb_ptr_t  mqb_next_argv_p(mqueue_block mqb) ;
extern mqb_int_t  mqb_next_argv_i(mqueue_block mqb) ;
extern mqb_uint_t mqb_next_argv_u(mqueue_block mqb) ;

extern void** mqb_pop_argv_array(mqueue_block mqb) ;

/*==============================================================================
 * The Inline functions.
 */

Inline mqueue_block
mqueue_local_head(mqueue_local_queue lmq)
{
  return lmq->head ;
} ;

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

/* Get operations       */

Inline void
mqb_dispatch(mqueue_block mqb, mqb_flag_t flag)
{
  mqb->action(mqb, flag) ;
} ;

Inline void
mqb_dispatch_action(mqueue_block mqb)
{
  mqb->action(mqb, mqb_action) ;
} ;

Inline void
mqb_dispatch_destroy(mqueue_block mqb)
{
  mqb->action(mqb, mqb_destroy) ;
} ;

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

Inline void*
mqb_get_argv(mqueue_block mqb)
{
  return mqb->argv ;
} ;

Inline mqb_index_t
mqb_get_argv_count(mqueue_block mqb)
{
  return mqb->argv_count ;
} ;

Inline void
mqb_reset_argv_next(mqueue_block mqb)
{
  mqb->argv_next = 0 ;
} ;

#endif /* _ZEBRA_MQUEUE_H */

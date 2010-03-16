/* Quagga Pthreads support -- header
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

#ifndef _ZEBRA_QPNEXUS_H
#define _ZEBRA_QPNEXUS_H

#include <stdint.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>

#include "zassert.h"
#include "qpthreads.h"
#include "qtimers.h"
#include "mqueue.h"
#include "qpselect.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Quagga Nexus Interface -- qpn_xxxx
 *
 * Object to hold, a qpthread, a qps_selection, a qtimer_pile, a mqueue_queue
 * together with the thread routine to poll and dispatch their respective
 * action routines.
 *
 */

/* maximum time in seconds to sit in a pselect  */
#define MAX_PSELECT_WAIT 10

/* signal for message queues                    */
#define SIGMQUEUE SIGUSR2

/* number of hooks per hook list                */
enum { qpn_hooks_max = 4 } ;

/*==============================================================================
 * Data Structures.
 */

typedef int qpn_hook_function(void) ;   /* dispatch of tasks    */
typedef int qpn_init_function(void) ;   /* start/stop work      */

typedef struct qpn_hook_list* qpn_hook_list ;
struct qpn_hook_list
{
  void*     hooks[qpn_hooks_max] ;
  unsigned  count ;
} ;

typedef struct qpn_nexus* qpn_nexus ;

struct qpn_nexus
{
  /* set true to terminate the thread (eventually) */
  int terminate;

  /* true if this is the main thread */
  int main_thread;

  /* thread ID */
  qpt_thread_t thread_id;

  /* pselect handler */
  qps_selection selection;

  /* timer pile */
  qtimer_pile pile;

  /* message queue */
  mqueue_queue queue;
  mqueue_thread_signal mts;

  /* qpthread routine, can override */
  void* (*start)(void*);

  /* in-thread initialise, can override.  Called within the thread after all
   * other initialisation just before thread loop
   *
   * These are typedef int qpn_init_function(void).
   *
   * These are executed in the order given.
   */
  struct qpn_hook_list in_thread_init ;

  /* in-thread finalise, can override.  Called within thread just before
   * thread dies.  Nexus components all exist but thread loop is no longer
   * executed
   *
   * These are typedef int qpn_init_function(void).
   *
   * These are executed in the reverse of the order given.
   */
  struct qpn_hook_list in_thread_final ;

  /* in-thread queue(s) of events or other work.
   *
   * The hook function(s) are called in the qpnexus loop, at the top of the
   * loop.  So in addition to the mqueue, I/O, timers and any background stuff,
   * the thread may have other queue(s) of things to be done.
   *
   * These are typedef int qpn_hook_function(void).
   *
   * Hook function can process some queue(s) of things to be done.  It does not
   * have to empty its queues, but it MUST only return 0 if all queues are now
   * empty.
   */
  struct qpn_hook_list foreground ;

  /* in-thread background queue(s) of events or other work.
   *
   * The hook functions are called at the bottom of the qpnexus loop, but only
   * when there is absolutely nothing else to do.
   *
   * These are typedef int qpn_hook_function(void).
   *
   * The hook function should do some unit of background work (if there is any)
   * and return.  MUST return 0 iff there is no more work to do.
   */
  struct qpn_hook_list background ;
};

/*==============================================================================
 * Functions
 */

extern qpn_nexus qpn_init_new(qpn_nexus qpn, int main_thread);
extern void qpn_add_hook_function(qpn_hook_list list, void* hook) ;
extern void qpn_exec(qpn_nexus qpn);
extern void qpn_terminate(qpn_nexus qpn);
extern qpn_nexus qpn_free(qpn_nexus qpn);

#endif /* _ZEBRA_QPNEXUS_H */

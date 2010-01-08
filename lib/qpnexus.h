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

/* maximum time in seconds to sit in a pselect */
#define MAX_PSELECT_TIMOUT 10

/* signal for message queues */
#define SIGMQUEUE SIGUSR2

/*==============================================================================
 * Data Structures.
 */

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

  /* qpthread routine */
  void* (*start)(void*);

};

/*==============================================================================
 * Functions
 */

extern qpn_nexus qpn_init_new(qpn_nexus qtn);
extern qpn_nexus qpn_init_main(qpn_nexus qtn);
extern qpn_nexus qpn_init_bgp(qpn_nexus qtn);
extern qpn_nexus qpn_init_routing(qpn_nexus qtn);
extern void qpn_exec(qpn_nexus qtn);
extern void qpn_terminate(qpn_nexus qpn);
extern qpn_nexus qpn_free(qpn_nexus qpn);

#endif /* _ZEBRA_QPNEXUS_H */

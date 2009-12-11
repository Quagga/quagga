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

#include <zebra.h>

#include "qpnexus.h"
#include "memory.h"
#include "thread.h"

/* prototypes */
static void qpn_dispatch_queue(qpn_nexus qpn);
static void* qpn_start(void* arg);
static void* qpn_start_legacy(void* arg);
static void qpn_thread_prep(qpn_nexus qpn);

/* Master of the threads. */
extern struct thread_master *master;

/*==============================================================================
 * Quagga Nexus Interface -- qpt_xxxx
 *

 */

/* Initialise a nexus -- allocating it if required.
 *
 * If main_thread is set then no new thread will be created
 * when qpn_exec() is called, instead the finite state machine will be
 * run in the calling thread.  The main thread will only block the
 * message queue's signal.  Non main threads will block all signals.
 *
 * Returns the qtn_nexus.
 */
qpn_nexus
qpn_init_new(qpn_nexus qpn, int main)
{
  if (qpn == NULL)
    qpn = XCALLOC(MTYPE_QPN_NEXUS, sizeof(struct qpn_nexus)) ;
  else
    memset(qpn, 0,  sizeof(struct qpn_nexus)) ;

   /* will change if we start new thread */
  qpn->selection = qps_selection_init_new(qpn->selection);
  qpn->pile = qtimer_pile_init_new(qpn->pile);
  qpn->queue = mqueue_init_new(qpn->queue, mqt_signal_unicast);
  qpn->main_thread = main;

  return qpn;
}

/* free timers, selection, message queue and nexus */
static void
qpn_free(qpn_nexus qpn)
{
  qps_file qf;
  qtimer qtr;

  /* timers and the pile */
  while ((qtr = qtimer_pile_ream(qpn->pile, 1)))
    {
      qtimer_free(qtr);
    }

  /* files and selection */
  while ((qf = qps_selection_ream(qpn->selection, 1)))
    {
      qps_file_free(qf);
    }

  /* TODO: free qtn->queue */

  XFREE(MTYPE_QPN_NEXUS, qpn) ;
}

/* If not main thread create new qpthread.
 * Execute the state machine */
void
qpn_exec(qpn_nexus qpn)
{
  if (qpn->main_thread)
    {
      /* Run the state machine in calling thread */
      qpn->thread_id = qpt_thread_self();
      qpn_start(qpn);
    }
  else
    {
      /* create a qpthread and run the state machine in it */
      qpn->thread_id = qpt_thread_create(qpn_start, qpn, NULL) ;
    }
}

/* Prep thread and signals, then run finite state machine
 * using qps_selection and qtimer
*/
static void*
qpn_start(void* arg)
{
  qpn_nexus qpn = arg;
  int actions;

  qpn_thread_prep(qpn);

  while (!qpn->terminate)
    {
      qtime_mono_t now;

      /* Signals are highest priority.
       * only execute on the main thread */
      if (qpn->main_thread)
        quagga_sigevent_process ();

      /* process timers */
      now = qt_get_monotonic();
      while (qtimer_pile_dispatch_next(qpn->pile, now))
        {
        }

      /* block for some input, output or timeout */
      actions = qps_pselect( qpn->selection,
          qtimer_pile_top_time(qpn->pile, now + QTIME(MAX_PSELECT_TIMOUT)) );

      /* process I/O actions */
      while (actions)
        {
          actions = qps_dispatch_next(qpn->selection) ;
        }

      /* process message queue */
      qpn_dispatch_queue(qpn);
    }

  qpn_free(qpn);

  return NULL;
}

/* Create new qpthread and execute the thread state machine in it */
void
qpn_exec_legacy(qpn_nexus qpn)
{
  qpn->thread_id = qpt_thread_create(qpn_start_legacy, qpn, NULL) ;
}

/* Prep thread and signals, then run finite state machine
 * using legacy threads
*/
static void*
qpn_start_legacy(void* arg)
{
  qpn_nexus qpn = arg;
  struct thread thread;

  qpn_thread_prep(qpn);

  while (!qpn->terminate)
    {
      if (thread_fetch (master, &thread))
        thread_call (&thread);

      /* process message queue, if any */
      qpn_dispatch_queue(qpn);
    }

  qpn_free(qpn);

  return NULL;
}

/* dispatch any messages on our message queue */
static void
qpn_dispatch_queue(qpn_nexus qpn)
{
  mqueue_block mqb;

  for (;;)
    {
      mqb = mqueue_dequeue(qpn->queue, 1, qpn->mts) ;
      if (mqb == NULL)
        return;

      mqb_dispatch(mqb);
    }
}

/* Init code to be run within the thread */
static void
qpn_thread_prep(qpn_nexus qpn)
{
  sigset_t newmask;

  if (qpn->main_thread)
    {
      /* Main thread, block the message queue's signal */
      sigemptyset (&newmask);
      sigaddset (&newmask, SIGMQUEUE);
    }
  else
    {
      /*
       * Not main thread.  Block most signals, but be careful not to
       * defer SIGTRAP because doing so breaks gdb, at least on
       * NetBSD 2.0.  Avoid asking to block SIGKILL, just because
       * we shouldn't be able to do so.
        */
      sigfillset (&newmask);
      sigdelset (&newmask, SIGTRAP);
      sigdelset (&newmask, SIGKILL);
  }

  qpt_thread_sigmask(SIG_BLOCK, &newmask, NULL);
  qps_set_signal(qpn->selection, SIGMQUEUE, newmask);

  /* init mqueue_thread_signal here now we know our thread-id */
  qpn->mts = mqueue_thread_signal_init(qpn->mts, qpn->thread_id, SIGMQUEUE);
}



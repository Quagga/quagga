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

/* This MUST come first...  otherwise we don't get __USE_UNIX98, which is     */
/* essential if glibc is to allow pthread_mutexattr_settype() to be used.     */
#include "config.h"

#include <signal.h>
#include <string.h>

#include "qpnexus.h"
#include "memory.h"

/* prototypes */
static void* qpn_start(void* arg);

/*==============================================================================
 * Quagga Nexus Interface -- qpt_xxxx
 *

 */

/* Initialise a nexus -- allocating it if required.
 *
 * Returns the qtn_nexus.
 */
qpn_nexus
qpn_init_new(qpn_nexus qpn)
{
  if (qpn == NULL)
    qpn = XCALLOC(MTYPE_QPN_NEXUS, sizeof(struct qpn_nexus)) ;
  else
    memset(qpn, 0,  sizeof(struct qpn_nexus)) ;

  qpn->selection = qps_selection_init_new(qpn->selection);
  qpn->pile = qtimer_pile_init_new(qpn->pile);

  /* TODO mqueue initialisation */

  return qpn;
}

void
qpn_free(qpn_nexus qpn)
{
  /* timers and the pile */
  qtimer qtr;
  while ((qtr = qtimer_pile_ream(qpn->pile, 1)))
    {
      qtimer_free(qtr);
    }

  /* TODO: free qtn->selection */

  /* TODO: free qtn->queue */

  XFREE(MTYPE_QPN_NEXUS, qpn) ;
}

/* Create and execute the qpthread */
void
qpn_exec(qpn_nexus qpn)
{
  qpn->thread_id = qpt_thread_create(qpn_start, qpn, NULL) ;
}

static void*
qpn_start(void* arg)
{
  qpn_nexus qpn = arg;
  int actions;

  while (!qpn->terminate)
    {
      qtime_t now = qtimer_time_now();

      /* process timers */
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

      /* TODO process message queue */
    }

  qpn_free(qpn);

  return NULL;
}





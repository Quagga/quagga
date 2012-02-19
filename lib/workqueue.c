/*
 * Quagga Work Queue Support.
 *
 * Copyright (C) 2005 Sun Microsystems, Inc.
 *
 * This file is part of GNU Zebra.
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <lib/zebra.h>
#include "thread.h"
#include "memory.h"
#include "workqueue.h"
#include "command.h"
#include "log.h"
#include "linklist.h"

/* master list of work_queues */
static struct list work_queues;

enum {
  WQ_MIN_GRANULARITY   = 1,
  WQ_HYSTERESIS_FACTOR = 4,
} ;

static void
work_queue_item_free (struct work_queue *wq, struct work_queue_item *item)
{
  /* call private data deletion callback if needed              */
  if (wq->spec.del_item_data != NULL)
    wq->spec.del_item_data (wq, item) ;

  XFREE (MTYPE_WORK_QUEUE_ITEM, item) ;
  return;
}

/* create new work queue */
struct work_queue *
work_queue_new (struct thread_master *m, const char *queue_name)
{
  struct work_queue *new;

  new = XCALLOC (MTYPE_WORK_QUEUE, sizeof (struct work_queue));

  if (new == NULL)
    return new;

  new->name   = XSTRDUP (MTYPE_WORK_QUEUE_NAME, queue_name);
  new->master = m;
  SET_FLAG (new->flags, WQ_UNPLUGGED);

  listnode_add (&work_queues, new);

  new->cycles.granularity = WQ_MIN_GRANULARITY;

  /* Default values, can be overriden by caller */
  new->spec.hold = WORK_QUEUE_DEFAULT_HOLD;

  return new;
}

void
work_queue_free (struct work_queue *wq)
{
  work_queue_item item ;

  if (wq->thread != NULL)
    thread_cancel(wq->thread);

  while ((item = wq->head) != NULL)
    {
      wq->head = item->next ;
      work_queue_item_free(wq, item) ;
    } ;

  XFREE (MTYPE_WORK_QUEUE_NAME, wq->name);
  XFREE (MTYPE_WORK_QUEUE, wq);
  return;
}

static inline int
work_queue_schedule (struct work_queue *wq, unsigned int delay)
{
  /* if appropriate, schedule work queue thread */
  if ( CHECK_FLAG (wq->flags, WQ_UNPLUGGED)
       && (wq->thread == NULL)
       && (wq->head != NULL) )
    {
      wq->thread = thread_add_background (wq->master, work_queue_run,
                                          wq, delay);
      return 1;
    }
  else
    return 0;
}

/*------------------------------------------------------------------------------
 * Create new work queue item and place on the end of the given work queue.
 *
 * Schedules the work queue if there were no items (unless already scheduled
 * or plugged).
 *
 * Returns the address of the args area in the new item.
 */
extern void*
work_queue_item_add (struct work_queue *wq)
{
  work_queue_item item ;

  assert (wq);

  item = XCALLOC (MTYPE_WORK_QUEUE_ITEM, sizeof (struct work_queue_item));

  if (item == NULL)
    {
      zlog_err ("%s: unable to get new queue item", __func__);
      return NULL ;
    }

  item->next = NULL ;
  if (wq->head == NULL)
    {
      assert(wq->list_count == 0) ;
      wq->head = item ;
      item->prev = NULL ;
    }
  else
    {
      assert((wq->tail != NULL) && (wq->list_count > 0)) ;
      wq->tail->next = item ;
      item->prev = wq->tail ;
    } ;
  wq->tail = item ;

  ++wq->list_count ;
  work_queue_schedule (wq, wq->spec.hold);

  return work_queue_item_args(item) ;
}

static void
work_queue_item_remove (struct work_queue *wq, work_queue_item item)
{
  assert ((wq != NULL) && (item != NULL)) ;

  if (wq->head == item)
    {
      /* Removing the first item                */
      assert(item->prev == NULL) ;

      wq->head = item->next ;

      if (wq->tail == item)
        {
          /* Removing the only item             */
          assert((item->next == NULL) && (wq->list_count == 1)) ;
          wq->tail = NULL ;
        }
      else
        {
          /* First, but not the only item       */
          assert((item->next != NULL) && (wq->list_count > 1)) ;
          wq->head->prev = NULL ;
        } ;
    }
  else if (wq->tail == item)
    {
      /* Removing last, but not only item       */
      assert(item->next == NULL) ;
      assert((item->prev != NULL) && (wq->list_count > 1)) ;

      wq->tail = item->prev ;
      wq->tail->next = NULL ;
    }
  else
    {
      /* Removing from somewhere in middle      */
      assert(item->next != NULL) ;
      assert((item->prev != NULL) && (wq->list_count > 2)) ;

      item->prev->next = item->next ;
      item->next->prev = item->prev ;
    } ;

  --wq->list_count ;
  work_queue_item_free (wq, item);

  return;
}

static work_queue_item
work_queue_item_requeue (struct work_queue *wq, work_queue_item item)
{
  work_queue_item next = item->next ;
  work_queue_item last = wq->tail ;

  assert(last != NULL) ;

  if (last == item)
    {
      /* Requeuing last item -- easy !          */
      assert(next == NULL) ;
      return item ;
    } ;

  assert(next != NULL) ;

  if (wq->head == item)
    {
      /* Requeuing first, but not only item     */
      assert(item->prev == NULL) ;

      wq->head       = next ;
      next->prev     = NULL ;
    }
  else
    {
      /* Requeuing something in middle          */
      assert(item->prev != NULL) ;

      item->prev->next = item->next ;
      item->next->prev = item->prev ;
    } ;

  item->next   = NULL ;
  item->prev   = last ;

  last->next   = item ;
  wq->tail     = item ;

  return next ;
}

/* 'plug' a queue: Stop it from being scheduled,
 * ie: prevent the queue from draining.
 */
void
work_queue_plug (struct work_queue *wq)
{
  if (wq->thread)
    thread_cancel (wq->thread);

  wq->thread = NULL;

  UNSET_FLAG (wq->flags, WQ_UNPLUGGED);
}

/* unplug queue, schedule it again, if appropriate
 * Ie: Allow the queue to be drained again
 */
void
work_queue_unplug (struct work_queue *wq)
{
  SET_FLAG (wq->flags, WQ_UNPLUGGED);

  /* if thread isnt already waiting, add one */
  work_queue_schedule (wq, wq->spec.hold);
}

/* timer thread to process a work queue
 * will reschedule itself if required,
 * otherwise work_queue_item_add
 */
int
work_queue_run (struct thread *thread)
{
  struct work_queue *wq;
  work_queue_item next, item ;
  wq_item_status ret;
  unsigned int cycles = 0;
  char yielded = 0;

  wq = THREAD_ARG (thread);
  wq->thread = NULL;

  assert (wq != NULL) ;

  /* calculate cycle granularity:
   * list iteration == 1 cycle
   * granularity == # cycles between checks whether we should yield.
   *
   * granularity should be > 0, and can increase slowly after each run to
   * provide some hysteris, but not past cycles.best or 2*cycles.
   *
   * Best: starts low, can only increase
   *
   * Granularity: starts at WQ_MIN_GRANULARITY, can be decreased
   *              if we run to end of time slot, can increase otherwise
   *              by a small factor.
   *
   * We could use just the average and save some work, however we want to be
   * able to adjust quickly to CPU pressure. Average wont shift much if
   * daemon has been running a long time.
   */
  if (wq->cycles.granularity == 0)
    wq->cycles.granularity = WQ_MIN_GRANULARITY;

  next = wq->head ;
  while (next != NULL)
  {
    item = next ;
    next = item->next ;         /* default next item    */

    /* dont run items which are past their allowed retries */
    if (item->ran > wq->spec.max_retries)
      {
        /* run error handler, if any */
	if (wq->spec.errorfunc != NULL)
	  wq->spec.errorfunc (wq, item);
	work_queue_item_remove (wq, item);
	continue;
      }

    /* run and take care of items that want to be retried immediately */
    do
      {
        ret = wq->spec.workfunc (wq, item);
        item->ran++;
      }
    while ((ret == WQ_RETRY_NOW)
           && (item->ran < wq->spec.max_retries));

    switch (ret)
      {
      case WQ_QUEUE_BLOCKED:
        {
          /* decrement item->ran again, cause this isn't an item
           * specific error, and fall through to WQ_RETRY_LATER
           */
          item->ran--;
        }
      case WQ_RETRY_LATER:
	{
	  goto stats;
	}
      case WQ_REQUEUE:
	{
	  item->ran--;
	  next = work_queue_item_requeue (wq, item);
	  break;
	}
      case WQ_RETRY_NOW:
        /* a RETRY_NOW that gets here has exceeded max_tries, same as ERROR */
      case WQ_ERROR:
	{
	  if (wq->spec.errorfunc != NULL)
	    wq->spec.errorfunc (wq, item);
	}
	/* fall through here is deliberate */
      case WQ_SUCCESS:
      default:
	{
	  work_queue_item_remove (wq, item);
	  break;
	}
      }

    /* completed cycle */
    cycles++;

    /* test if we should yield */
    if ( !(cycles % wq->cycles.granularity)
        && thread_should_yield (thread))
      {
        yielded = 1;
        goto stats;
      }
  }

stats:

  /* we yielded, check whether granularity should be reduced */
  if (yielded && (cycles < wq->cycles.granularity))
    {
      wq->cycles.granularity = ((cycles > 0) ? cycles
                                             : WQ_MIN_GRANULARITY);
    }
  /* otherwise, should granularity increase? */
  else if (cycles >= (wq->cycles.granularity))
    {
      if (cycles > wq->cycles.best)
        wq->cycles.best = cycles;

      /* along with yielded check, provides hysteresis for granularity */
      if (cycles > (wq->cycles.granularity * WQ_HYSTERESIS_FACTOR
                                           * WQ_HYSTERESIS_FACTOR))
        wq->cycles.granularity *= WQ_HYSTERESIS_FACTOR; /* quick ramp-up */
      else if (cycles > (wq->cycles.granularity * WQ_HYSTERESIS_FACTOR))
        wq->cycles.granularity += WQ_HYSTERESIS_FACTOR;
    }

  wq->runs++;
  wq->cycles.total += cycles;

#if 0
  printf ("%s: cycles %d, new: best %d, worst %d\n",
            __func__, cycles, wq->cycles.best, wq->cycles.granularity);
#endif

  /* Is the queue done yet? If it is, call the completion callback. */
  if (wq->head != NULL)
    work_queue_schedule (wq, 0);
  else if (wq->spec.completion_func)
    wq->spec.completion_func (wq);

  return 0;
}

/*------------------------------------------------------------------------------
 * Reporting command(s)
 */
DEFUN(show_work_queues,
      show_work_queues_cmd,
      "show work-queues",
      SHOW_STR
      "Work Queue information\n")
{
  struct listnode *node;
  struct work_queue *wq;

  vty_out (vty,
           "%c %8s %5s %8s %21s%s",
           ' ', "List","(ms) ","Q. Runs","Cycle Counts   ",
           VTY_NEWLINE);
  vty_out (vty,
           "%c %8s %5s %8s %7s %6s %6s %s%s",
           'P',
           "Items",
           "Hold",
           "Total",
           "Best","Gran.","Avg.",
           "Name",
           VTY_NEWLINE);

  for (ALL_LIST_ELEMENTS_RO ((&work_queues), node, wq))
    {
      vty_out (vty,"%c %8d %5d %8ld %7d %6d %6u %s%s",
               (CHECK_FLAG (wq->flags, WQ_UNPLUGGED) ? ' ' : 'P'),
               wq->list_count,
               wq->spec.hold,
               wq->runs,
               wq->cycles.best, wq->cycles.granularity,
                 (wq->runs) ?
                   (unsigned int) (wq->cycles.total / wq->runs) : 0,
               wq->name,
               VTY_NEWLINE);
    }

  return CMD_SUCCESS;
} ;

CMD_INSTALL_TABLE(extern, workqueue_cmd_table, ALL_RDS) =
{
  { RESTRICTED_NODE, &show_work_queues_cmd                              },
  { VIEW_NODE,       &show_work_queues_cmd                              },
  { ENABLE_NODE,     &show_work_queues_cmd                              },

  CMD_INSTALL_END
} ;

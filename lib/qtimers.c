/* Quagga timers support -- functions
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

#include <stddef.h>
#include <string.h>

#include "zassert.h"
#include "qtimers.h"
#include "memory.h"
#include "heap.h"

/*==============================================================================
 * Quagga Timers -- qtimer_xxxx
 *
 * Here and in qtimers.h is a data structure for managing multiple timers
 * each with an action to be executed when the timer expires.
 *
 * The qtime_pile structure manages a "pile" of qtimer structures which are
 * waiting for the right time to go off.
 *
 * NB: it is ASSUMED that a qtime_pile will be private to the thread in which
 *     it is created and used.
 *
 *     There is NO mutex handling here.
 *
 * Timers are triggered by calling qtimer_dispatch_next().  This is given the
 * current qtimer time (see below), and it dispatches the first timer whose
 * time has come (or been passed).  Dispatching a timer means calling its
 * action function (see below).  Each call of qtimer_dispatch_next() triggers
 * at most one timer.
 *
 * Time Base
 * ---------
 *
 * The time base for qtimers is the monotonic time provided in qtime.c/.h.
 *
 * Interval
 * --------
 *
 * There is an optional interval associated with each timer.
 *
 * The timer may be set to "now + interval", and the interval is stored with
 * the timer.
 *
 * The timer may be set to its current time + stored interval (to provide a
 * "steady" clock).
 *
 * Action Functions
 * ----------------
 *
 * There is a separate action function for each timer.
 *
 * When the action function is called it is passed the qtimer structure, the
 * timer_info pointer from that structure and the time which triggered the
 * timer (which may, or may not, be the current qtimer time).
 *
 * During an action function timers may be set/unset, actions changed, and so
 * on... there are no restrictions EXCEPT that the qtimer structure may NOT be
 * freed.
 */

static int
qtimer_cmp(qtimer* a, qtimer* b)        /* the heap discipline  */
{
  if ((**a).time < (**b).time)
    return -1 ;
  if ((**a).time > (**b).time)
    return +1 ;
  return 0 ;
} ;

/* Kill this */
qtimer_pile our_pile;

/*==============================================================================
 * qtimer_pile handling
 */

/* Initialise a timer pile -- allocating it if required.
 *
 * Returns the qtimer_pile.
 */
qtimer_pile
qtimer_pile_init_new(qtimer_pile qtp)
{
  if (qtp == NULL)
    qtp = XCALLOC(MTYPE_QTIMER_PILE, sizeof(struct qtimer_pile)) ;
  else
    memset(qtp, 0, sizeof(struct qtimer_pile)) ;

  /* Zeroising has initialised:
   *
   *   timers        -- invalid heap -- need to properly initialise
   */

  /* Eclipse flags offsetof(struct qtimer, backlink) as a syntax error :-(  */
  typedef struct qtimer qtimer_t ;

  heap_init_new_backlinked(&qtp->timers, 0, (heap_cmp*)qtimer_cmp,
                                                 offsetof(qtimer_t, backlink)) ;

  /* TODO: kill this */
  our_pile = qtp;
  return qtp ;
} ;

/* Get the timer time for the first timer due to go off in the given pile.
 *
 * The caller must provide a maximum acceptable time.  If the qtimer pile is
 * empty, or the top entry times out after the maximum time, then the maximum
 * is returned.
 */
qtime_mono_t
qtimer_pile_top_time(qtimer_pile qtp, qtime_mono_t max_time)
{
  qtimer  qtr = heap_top_item(&qtp->timers) ;

  if ((qtr == NULL) || (qtr->time >= max_time))
    return max_time ;
  else
    return qtr->time ;
} ;

/* Dispatch the next timer whose time is <= the given "upto" time.
 *
 * The upto time must be a qtimer time (!) -- see qtimer_time_now().
 *
 * The upto argument allows the caller to get a qtimer_time_now() value, and
 * then process all timers upto that time.
 *
 * Returns true  <=> dispatched a timer, and there may be more to do.
 *         false <=> nothing to do (and nothing done).
 */
int
qtimer_pile_dispatch_next(qtimer_pile qtp, qtime_mono_t upto)
{
  qtimer   qtr ;

  qtimer_pile_verify(qtp) ;     /* TODO: remove after debuggery */

  qtr = heap_top_item(&qtp->timers) ;
  if ((qtr != NULL) && (qtr->time <= upto))
    {
      passert(qtp == qtr->pile);
      qtr->state = qtr_state_unset_pending ;

      qtr->action(qtr, qtr->timer_info, upto) ;
      assert(qtp == qtr->pile);

      if (qtr->state == qtr_state_unset_pending)
        qtimer_unset(qtr) ;

      return 1 ;
    }
  else
    return 0 ;
} ;

/* Ream out (another) item from qtimer_pile.
 *
 * If pile is empty, release the qtimer_pile structure, if required.
 *
 * See: #define qtimer_pile_ream_free(qtp)
 *      #define qtimer_pile_ream_keep(qtp)
 *
 * Useful for emptying out and discarding a pile of timers:
 *
 *     while ((p_qtr = qtimer_pile_ream_free(qtp)))
 *       ... do what's required to release the item p_qtr
 *
 * Returns NULL when timer pile is empty (and has been released, if required).
 *
 * If the timer pile is not released, it may be reused without reinitialisation.
 *
 * NB: once reaming has started, the timer pile MUST NOT be used for anything,
 *     and the process MUST be run to completion.
 */
qtimer
qtimer_pile_ream(qtimer_pile qtp, int free_structure)
{
  qtimer qtr ;

  qtr = heap_ream_keep(&qtp->timers) ;  /* ream, keeping the heap structure   */
  if (qtr != NULL)
    qtr->state = qtr_state_inactive ;   /* has been removed from pile         */
  else
    if (free_structure)                 /* pile is empty, may now free it     */
      XFREE(MTYPE_QTIMER_PILE, qtp) ;

  return qtr ;
} ;

/*==============================================================================
 * qtimer handling
 */

/* Initialise qtimer structure -- allocating one if required.
 *
 * Associates qtimer with the given pile of timers, and sets up the action and
 * the timer_info.
 *
 * Once initialised, the timer may be set.
 *
 * Returns the qtimer.
 */
qtimer
qtimer_init_new(qtimer qtr, qtimer_pile qtp,
                                        qtimer_action* action, void* timer_info)
{
  if (qtr == NULL)
    qtr = XCALLOC(MTYPE_QTIMER, sizeof(struct qtimer)) ;
  else
    memset(qtr, 0, sizeof(struct qtimer)) ;

  /* Zeroising has initialised:
   *
   *   pile        -- NULL -- not in any pile (yet)
   *   backlink    -- unset
   *
   *   state       -- not active
   *
   *   time        -- unset
   *   action      -- NULL -- no action set (yet)
   *   timer_info  -- NULL -- no timer info set (yet)
   *
   *   interval    -- unset
   */

  confirm(qtr_state_inactive == 0) ;

  qtr->pile       = qtp ;
  qtr->action     = action ;
  qtr->timer_info = timer_info ;

  return qtr ;
} ;

/* Free given timer.
 *
 * Unsets it first if it is active.
 *
 * The timer MAY NOT be currently the subject of qtimer_pile_dispatch_next().
 */
void
qtimer_free(qtimer qtr)
{
  assert(qtr->state != qtr_state_unset_pending) ;

  if (qtr->state != qtr_state_inactive)
    qtimer_unset(qtr) ;

  XFREE(MTYPE_QTIMER, qtr) ;
} ;

/* Set pile in which given timer belongs.
 *
 * Unsets the timer if active in another pile.
 * (Does nothing if active in the "new" pile.)
 */
void
qtimer_set_pile(qtimer qtr, qtimer_pile qtp)
{
  if (qtr_is_active(qtr) && (qtr->pile != qtp))
    qtimer_unset(qtr) ;
  qtr->pile = qtp ;
}

/* Set action for given timer.
 */
void
qtimer_set_action(qtimer qtr, qtimer_action* action)
{
  qtr->action = action ;
} ;

/* Set timer_info for given timer.
 */
void
qtimer_set_info(qtimer qtr, void* timer_info)
{
  qtr->timer_info = timer_info ;
} ;

/* Set given timer.
 *
 * Setting a -ve time => qtimer_unset.
 *
 * Sets any given action -- if the action given is NULL, retains previously set
 * action.
 *
 * If the timer is already active, sets the new time & updates pile.
 *
 * Otherwise, sets the time and adds to pile -- making timer active.
 *
 * It is an error to set a timer which has a NULL action.
 */
void
qtimer_set(qtimer qtr, qtime_mono_t when, qtimer_action* action)
{
  qtimer_pile qtp ;

  if (when < 0)
    return qtimer_unset(qtr) ;

  qtp = qtr->pile ;
  dassert(qtp != NULL) ;
  assert(qtp == our_pile);
  qtimer_pile_verify(qtp) ;     /* TODO: remove after debuggery */

  qtr->time = when ;

  if (qtr_is_active(qtr))
    heap_update_item(&qtp->timers, qtr) ; /* update in heap               */
  else
    heap_push_item(&qtp->timers, qtr) ;   /* add to heap                  */

  assert(qtp == qtr->pile);

  qtr->state = qtr_state_active ;         /* overrides any unset pending  */

  if (action != NULL)
    qtr->action = action ;
  else
    dassert(qtr->action != NULL) ;

  qtimer_pile_verify(qtp) ;     /* TODO: remove after debuggery */
} ;

/* Unset given timer
 *
 * If the timer is active, removes from pile and sets inactive.
 */
void
qtimer_unset(qtimer qtr)
{
  if (qtr_is_active(qtr))
    {
      qtimer_pile qtp = qtr->pile ;
      dassert(qtp != NULL) ;

      assert(qtp == our_pile);
      qtimer_pile_verify(qtp) ;     /* TODO: remove after debuggery */

      heap_delete_item(&qtp->timers, qtr) ;

      assert(qtp == qtr->pile);
      qtimer_pile_verify(qtp) ;     /* TODO: remove after debuggery */

      qtr->state = qtr_state_inactive ; /* overrides any unset pending  */
    } ;
} ;

/*==============================================================================
 * Verification code for debug purposes.
 */
extern void
qtimer_pile_verify(qtimer_pile qtp)
{
  heap   th = &qtp->timers ;
  vector v ;
  vector_index i ;
  vector_index e ;
  qtimer qtr ;

  /* Eclipse flags offsetof(struct qtimer, backlink) as a syntax error :-(  */
  typedef struct qtimer qtimer_t ;

  assert(th->cmp             == (heap_cmp*)qtimer_cmp) ;
  assert(th->state           == Heap_Has_Backlink) ;
  assert(th->backlink_offset == offsetof(qtimer_t, backlink)) ;

  v = &th->v ;
  e = vector_end(v) ;
  for (i = 0 ; i < e ; ++i)
    {
      qtr = vector_get_item(v, i) ;

      assert(qtr->pile     == qtp) ;
      assert(qtr->backlink == i) ;
      assert(qtr->action   != NULL) ;
    } ;
} ;

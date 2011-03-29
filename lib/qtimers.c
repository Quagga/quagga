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
#include "misc.h"

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
 * on... there are no restrictions EXCEPT that may NOT recurse into the
 * dispatch function.
 *
 * If nothing is done with the time during the action function, the timer is
 * implicitly unset when the action function returns.
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

/*==============================================================================
 * qtimer_pile handling
 */

/*------------------------------------------------------------------------------
 * Initialise a timer pile -- allocating it if required.
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
   *   current       = NULL -- no current timer
   */

  /* (The typedef is required to stop Eclipse (3.4.2 with CDT 5.0) whining
   *  about first argument of offsetof().)
   */
  typedef struct qtimer qtimer_t ;

  heap_init_new_backlinked(&qtp->timers, 0, (heap_cmp*)qtimer_cmp,
                                                 offsetof(qtimer_t, backlink)) ;
  return qtp ;
} ;

/*------------------------------------------------------------------------------
 * Get the timer time for the first timer due to go off in the given pile.
 *
 * The caller must provide a maximum acceptable time.  If the qtimer pile is
 * empty, or the top entry times out after the maximum time, then the maximum
 * is returned.
 */
extern qtime_t
qtimer_pile_top_wait(qtimer_pile qtp, qtime_t max_wait)
{
  qtime_t top_wait ;
  qtimer  qtr = heap_top_item(&qtp->timers) ;

  if (qtr == NULL)
    return max_wait ;

  top_wait = qtr->time - qt_get_monotonic() ;

  return (top_wait < max_wait) ? top_wait : max_wait ;
} ;

/*------------------------------------------------------------------------------
 * Dispatch the next timer whose time is <= the given "upto" time.
 *
 * The upto time must be a qtimer time (!) -- see qtimer_time_now().
 *
 * The upto argument allows the caller to get a qtimer_time_now() value, and
 * then process all timers upto that time.
 *
 * Returns true  <=> dispatched a timer, and there may be more to do.
 *         false <=> nothing to do (and nothing done).
 *
 * NB: it is a sad, very sad, mistake to recurse into this !
 */
extern bool
qtimer_pile_dispatch_next(qtimer_pile qtp, qtime_mono_t upto)
{
  qtimer   qtr ;

  if (qtimers_debug)
    qtimer_pile_verify(qtp) ;

  qtr = heap_top_item(&qtp->timers) ;

  if ((qtr == NULL) || (qtr->time > upto))
    return 0 ;

  passert((qtp == qtr->pile) && (qtr->active)) ;

  qtp->implicit_unset = qtr ;   /* Timer must be unset if is still here
                                   when the action function returns       */
  qtr->action(qtr, qtr->timer_info, upto) ;

  if (qtp->implicit_unset == qtr)
    qtimer_unset(qtr) ;
  else
    assert(qtp->implicit_unset == NULL) ;   /* check for tidy-ness      */

  return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Ream out (another) item from qtimer_pile.
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
qtimer_pile_ream(qtimer_pile qtp, free_keep_b free_structure)
{
  qtimer qtr ;
  confirm(free_it == true) ;

  qtr = heap_ream(&qtp->timers, keep_it) ; /* ream, keeping the heap    */
  if (qtr != NULL)
    qtr->active = false ;               /* has been removed from pile         */
  else
    if (free_structure)                 /* pile is empty, may now free it     */
      XFREE(MTYPE_QTIMER_PILE, qtp) ;

  return qtr ;
} ;

/*==============================================================================
 * qtimer handling
 */

/*------------------------------------------------------------------------------
 * Initialise qtimer structure -- allocating one if required.
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
   *   active      -- false
   *
   *   time        -- unset
   *   action      -- NULL -- no action set (yet)
   *   timer_info  -- NULL -- no timer info set (yet)
   *
   *   interval    -- unset
   */

  qtr->pile       = qtp ;
  qtr->action     = action ;
  qtr->timer_info = timer_info ;

  return qtr ;
} ;

/*------------------------------------------------------------------------------
 * Free given timer -- if any.
 *
 * Unsets it first if it is active or pending unset.
 *
 * Returns: NULL
 */
extern qtimer
qtimer_free(qtimer qtr)
{
  /* Note that if is the current dispatched timer and an unset is still
   * pending, then it must still be active.
   */
  if (qtr != NULL)
    {
      if (qtr->active)
        qtimer_unset(qtr) ;

      XFREE(MTYPE_QTIMER, qtr) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set pile in which given timer belongs.
 *
 * Does nothing if timer already belongs to the given pile.
 *
 * Unsets the timer if active in another pile, before reassigning it.
 */
extern void
qtimer_set_pile(qtimer qtr, qtimer_pile qtp)
{
  if (qtr->pile == qtp)
    return ;

  /* Note that if is the current dispatched timer and an unset is still
   * pending, then it must still be active.
   */
  if (qtr->active)
    qtimer_unset(qtr) ;

  qtr->pile = qtp ;
}

/*------------------------------------------------------------------------------
 * Set given timer.
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
extern void
qtimer_set(qtimer qtr, qtime_mono_t when, qtimer_action* action)
{
  qtimer_pile qtp ;

  if (when < 0)
    return qtimer_unset(qtr) ;

  qtp = qtr->pile ;
  assert(qtp != NULL) ;

  if (qtimers_debug)
    qtimer_pile_verify(qtp) ;

  qtr->time = when ;

  if (qtr->active)
    {
      /* Is active, so update the timer in the pile.                    */
      heap_update_item(&qtp->timers, qtr) ;

      if (qtr == qtp->implicit_unset)
        qtp->implicit_unset = NULL ;        /* no unset required, now   */
    }
  else
    {
      /* Is not active, so insert the timer into the pile.              */
      heap_push_item(&qtp->timers, qtr) ;

      assert(qtr != qtp->implicit_unset) ;  /* because it's not active  */

      qtr->active = true ;
    } ;

  if (action != NULL)
    qtr->action = action ;
  else
    assert(qtr->action != NULL) ;

  if (qtimers_debug)
    qtimer_pile_verify(qtp) ;
} ;

/*------------------------------------------------------------------------------
 * Unset given timer
 *
 * If the timer is active, removes from pile and sets inactive.
 */
extern void
qtimer_unset(qtimer qtr)
{
  qtimer_pile qtp = qtr->pile ;

  assert(qtp != NULL) ;

  if (qtimers_debug)
    qtimer_pile_verify(qtp) ;

  if (qtr->active)
    {
      if (qtr == qtp->implicit_unset)
        qtp->implicit_unset = NULL ;        /* no unset required, now       */

      heap_delete_item(&qtp->timers, qtr) ;

      if (qtimers_debug)
        qtimer_pile_verify(qtp) ;

      qtr->active = false ;
    }
  else
    assert(qtr != qtp->implicit_unset) ;
} ;

/*==============================================================================
 * Verification code for debug purposes.
 */
extern void
qtimer_pile_verify(qtimer_pile qtp)
{
  heap   th = &qtp->timers ;
  vector v ;
  vector_index_t  i ;
  vector_length_t e ;
  qtimer qtr ;
  bool seen ;

  assert(qtp != NULL) ;

  /* (The typedef is required to stop Eclipse (3.4.2 with CDT 5.0) whining
   *  about first argument of offsetof().)
   */
  typedef struct qtimer qtimer_t ;

  assert(th->cmp             == (heap_cmp*)qtimer_cmp) ;
  assert(th->state           == Heap_Has_Backlink) ;
  assert(th->backlink_offset == offsetof(qtimer_t, backlink)) ;

  v = th->v ;
  e = vector_end(v) ;
  for (i = 0 ; i < e ; ++i)
    {
      qtr = vector_get_item(v, i) ;
      assert(qtr != NULL) ;

      if (qtr == qtp->implicit_unset)
        seen = 1 ;

      assert(qtr->active) ;

      assert(qtr->pile     == qtp) ;
      assert(qtr->backlink == i) ;
      assert(qtr->action   != NULL) ;
    } ;

  assert(seen || (qtp->implicit_unset == NULL)) ;
} ;

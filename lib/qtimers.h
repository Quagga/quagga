/* Quagga timers support -- header
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

#ifndef _ZEBRA_QTIMERS_H
#define _ZEBRA_QTIMERS_H

#include "zassert.h"
#include "qtime.h"
#include "heap.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Quagga Timers -- qtimer_xxxx
 *
 * Here and in qtimers.c is a data structure for managing multiple timers
 * each with an action to be executed when the timer expires.
 */

/*==============================================================================
 * Data Structures.
 */

typedef struct qtimer*       qtimer ;
typedef struct qtimer_pile*  qtimer_pile ;

typedef void (qtimer_action)(qtimer qtr, void* timer_info, qtime_mono_t when) ;

enum qtimer_state {
  qtr_state_inactive      = 0,
  qtr_state_active        = 1,  /* timer is active in its pile            */
  qtr_state_unset_pending = 3   /* timer is active, but unset is pending  */
} ;

#define qtr_is_active(qtr) ((qtr)->state != qtr_state_inactive)

typedef enum qtimer_state qtimer_state_t ;

struct qtimer
{
  qtimer_pile     pile ;        /* pile currently allocated to          */
  heap_backlink_t backlink ;

  qtimer_state_t  state ;

  qtime_mono_t    time ;        /* current time to trigger action       */
  qtimer_action*  action ;
  void*           timer_info ;

  qtime_t         interval ;    /* optional timer interval              */
} ;

struct qtimer_pile
{
  struct heap   timers ;
} ;

/*==============================================================================
 * Functions
 */

qtimer_pile
qtimer_pile_init_new(qtimer_pile qtp) ;

int
qtimer_pile_dispatch_next(qtimer_pile qtp, qtime_mono_t upto) ;

qtime_mono_t
qtimer_pile_top_time(qtimer_pile qtp, qtime_mono_t max_time) ;

qtimer
qtimer_pile_ream(qtimer_pile qtp, int free_structure) ;

/* Ream out qtimer pile and free the qtimer structure.   */
#define qtimer_pile_ream_free(qtp) qtimer_pile_ream(qtp, 1)
/* Ream out qtimer pile but keep the qtimer structure.   */
#define qtimer_pile_ream_keep(qtp) qtimer_pile_ream(qtp, 0)

qtimer
qtimer_init_new(qtimer qtr, qtimer_pile qtp,
                                      qtimer_action* action, void* timer_info) ;
void
qtimer_set_pile(qtimer qtr, qtimer_pile qtp) ;

void
qtimer_set_action(qtimer qtr, qtimer_action* action) ;

void
qtimer_set_info(qtimer qtr, void* timer_info) ;

void
qtimer_free(qtimer qtr) ;

void
qtimer_set(qtimer qtr, qtime_mono_t when, qtimer_action* action) ;

void
qtimer_unset(qtimer qtr) ;

Inline void
qtimer_add(qtimer qtr, qtime_t interval, qtimer_action* action) ;

Inline qtime_mono_t
qtimer_get(qtimer qtr) ;

Inline void
qtimer_set_interval(qtimer qtr, qtime_t interval, qtimer_action* action) ;

Inline void
qtimer_add_interval(qtimer qtr, qtimer_action* action) ;

Inline qtime_t
qtimer_get_interval(qtimer qtr) ;

/*==============================================================================
 * Inline functions
 */

/* Set given timer to given time later than *its* current time.
 */
Inline void
qtimer_add(qtimer qtr, qtime_t interval, qtimer_action* action)
{
  qtimer_set(qtr, qtimer_get(qtr) + interval, action);
} ;

/* Get the given timer's time.
 */
Inline qtime_mono_t
qtimer_get(qtimer qtr)
{
  return qtr->time ;
} ;

/* Interval handling ---------------------------------------------------------*/

/* Set the interval field
 */

Inline void
qtimer_set_interval(qtimer qtr, qtime_t interval, qtimer_action* action)
{
  qtr->interval = interval ;
  qtimer_set(qtr, qt_add_monotonic(interval), action) ;
} ;

Inline void
qtimer_add_interval(qtimer qtr, qtimer_action* action)
{
  qtimer_add(qtr, qtr->interval, action) ;
} ;

/* Get the current value of the interval field
 */
Inline qtime_t
qtimer_get_interval(qtimer qtr)
{
  return qtr->interval ;
} ;

#endif /* _ZEBRA_QTIMERS_H */

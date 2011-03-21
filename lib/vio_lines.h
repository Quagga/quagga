/* Line Control for VTY Terminal output -- header
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

#ifndef _ZEBRA_VIO_LINES_H
#define _ZEBRA_VIO_LINES_H

#include "misc.h"
#include "qiovec.h"
#include "qstring.h"

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Line control -- collecting lines of a given width for output.
 *
 * NB: need to explicitly initialise line control in order to set the required
 *     new line, the qiovec vectors and a qstring buffer !
 */
struct vio_line_control
{
  uint      width ;         /* console width       -- 0 => HUGE         */
  uint      height ;        /* console height      -- 0 => indefinite   */

  int       counter ;       /* number of lines to next pause
                                                    <= 0 => paused.     */

  bool      incomplete ;    /* fragments in hand are an incomplete line */
  qiovec    fragments ;
  qstring   here ;          /* any fragments after write                */

  qiovec    qiov ;          /* output screen lines                      */

  qiov_item_t   newline ;   /* the required sequence                    */
} ;

typedef struct vio_line_control* vio_line_control ;
typedef struct vio_line_control  vio_line_control_t[1] ;

enum
{
  VIO_LINE_CONTROL_INIT_ALL_ZEROS = false
} ;

/*==============================================================================
 * Functions
 */
extern vio_line_control vio_lc_init_new(vio_line_control lc, int width,
                                                             int height,
                                                     const char* newline) ;
Inline vio_line_control vio_lc_new(int width, int height,
                                                     const char* newline) ;
extern vio_line_control vio_lc_reset(vio_line_control lc,
                                                   free_keep_b free_structure) ;
Inline vio_line_control vio_lc_free(vio_line_control lc) ;

extern void vio_lc_clear(vio_line_control lc) ;
extern void vio_lc_set_window(vio_line_control lc, int width, int height) ;

Inline void vio_lc_counter_reset(vio_line_control lc) ;
Inline void vio_lc_clear_pause(vio_line_control lc) ;

Inline bool vio_lc_counter_is_exhausted(vio_line_control lc) ;
Inline bool vio_lc_have_complete_line_in_hand(vio_line_control lc) ;
Inline bool vio_lc_is_empty(vio_line_control lc) ;

extern size_t vio_lc_append(vio_line_control lc, const void* buf, size_t len) ;
extern bool vio_lc_flush(vio_line_control lc) ;
extern int vio_lc_write_nb(int fd, vio_line_control lc) ;

/*------------------------------------------------------------------------------
 * Create new line control.
 *
 * Returns:  address of new line control.
 */
Inline vio_line_control
vio_lc_new(int width, int height, const char* newline)
{
  return vio_lc_init_new(NULL, width, height, newline) ;
} ;

/*------------------------------------------------------------------------------
 * Free given line control (if any).
 *
 * It is the caller's responsibility to free anything that the line control may
 * point to.
 *
 * Returns:  NULL
 */
Inline vio_line_control
vio_lc_free(vio_line_control lc)
{
  return vio_lc_reset(lc, free_it) ;
} ;

/*------------------------------------------------------------------------------
 * Counter reset.
 */
Inline void
vio_lc_counter_reset(vio_line_control lc)
{
  lc->counter = (lc->height > 0) ? lc->height : 100 ;
} ;

/*------------------------------------------------------------------------------
 * If no height is set, set counter to large number -- to do a tranche of
 * output.
 *
 * Otherwise, if the line control is paused (or would pause as soon as any
 * output is sent), reset the counter.
 */
Inline void
vio_lc_clear_pause(vio_line_control lc)
{
  if ((lc->counter <= 0) || (lc->height == 0))
    vio_lc_counter_reset(lc) ;
} ;

/*------------------------------------------------------------------------------
 * Is the given line control counter exhausted ?
 *
 * Any attempt to output more stuff will be prevented -- except for
 * vio_lc_flush() which will succeed if height is indefinite.
 */
Inline bool
vio_lc_counter_is_exhausted(vio_line_control lc)
{
  return (lc->counter <= 0) ;
} ;

/*------------------------------------------------------------------------------
 * Do we have a complete line "in hand" ?
 *
 * This will only be the case if have a definite height, and the line counter
 * has expired.
 */
Inline bool
vio_lc_have_complete_line_in_hand(vio_line_control lc)
{
  return !lc->incomplete && !qiovec_empty(lc->fragments) ;
} ;

/*------------------------------------------------------------------------------
 * Is given line control empty ?
 *
 * Is empty if the qiov is empty and there is nothing in hand.
 *
 * NB: if there is something in hand, it may be complete or incomplete.
 */
Inline bool
vio_lc_is_empty(vio_line_control lc)
{
  return qiovec_empty(lc->qiov) && qiovec_empty(lc->fragments) ;
} ;

#endif /* _ZEBRA_VIO_LINES_H */

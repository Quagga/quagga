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

#include "zebra.h"

#include <stddef.h>
#include <stdint.h>

#include "qiovec.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Line control -- collecting lines of a given width for output.
 *
 * NB: a completely zero structure is a valid, clear vio_line_control.
 */

typedef struct vio_line_control* vio_line_control ;
typedef struct vio_line_control  vio_line_control_t ;
struct vio_line_control
{
  unsigned      width ;   /* console width       -- 0 => HUGE           */
  unsigned      height ;  /* console height      -- 0 => indefinite     */

  unsigned      pause ;   /* number of lines to next pause
                                                    0 => indefinite     */
  bool          paused ;  /* true <=> last append stopped on pause      */

  unsigned      col ;     /* current column position                    */
  unsigned      line ;    /* line number of last complete line          */

  struct qiovec qiov ;    /* iovec control                              */
  bool          writing ; /* write started, but not completed           */
} ;

/*==============================================================================
 * Functions
 */
extern vio_line_control vio_lc_init_new(vio_line_control lc, int width,
                                                                   int height) ;
extern vio_line_control vio_lc_reset(vio_line_control lc, bool free_structure) ;

#define vio_lc_reset_keep(lc) vio_lc_reset(lc, 0)
#define vio_lc_reset_free(lc) vio_lc_reset(lc, 1)

Inline bool vio_lc_empty(vio_line_control lc) ;
extern void vio_lc_clear(vio_line_control lc) ;
extern void vio_lc_set_window(vio_line_control lc, int width, int height) ;

extern void vio_lc_set_pause(vio_line_control lc) ;
extern size_t vio_lc_append(vio_line_control lc, const void* buf, size_t len) ;
extern int vio_lc_write_nb(int fd, vio_line_control lc) ;

/*------------------------------------------------------------------------------
 * Is given line control empty ?
 */
Inline bool
vio_lc_empty(vio_line_control lc)
{
  return qiovec_empty(&lc->qiov) ;
} ;

#endif /* _ZEBRA_VIO_LINES_H */

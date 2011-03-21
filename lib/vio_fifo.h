/* VTY I/O FIFO -- header
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#ifndef _ZEBRA_VIO_FIFO_H
#define _ZEBRA_VIO_FIFO_H

#include "misc.h"
#include "vargs.h"
#include <stdio.h>

#include "list_util.h"

/*==============================================================================
 * VTY I/O FIFO -- buffering of arbitrary amounts of I/O.
 */

/*------------------------------------------------------------------------------
 * Sort out VIO_FIFO_DEBUG.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if VIO_FIFO_NO_DEBUG is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set VIO_FIFO_DEBUG    == 0 to turn off debug
 *       *  or set VIO_FIFO_DEBUG    != 0 to turn on debug
 *       *  or set VIO_FIFO_NO_DEBUG != 0 to force debug off
 */

#ifdef VIO_FIFO_DEBUG           /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(VIO_FIFO_DEBUG)
#  undef  VIO_FIFO_DEBUG
#  define VIO_FIFO_DEBUG 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define VIO_FIFO_DEBUG QDEBUG
#endif

#ifdef VIO_FIFO_NO_DEBUG        /* Override, if defined                 */
# if IS_NOT_ZERO_OPTION(VIO_FIFO_NO_DEBUG)
#  undef  VIO_FIFO_DEBUG
#  define VIO_FIFO_DEBUG 0
# endif
#endif

enum { vio_fifo_debug = VIO_FIFO_DEBUG } ;

/*==============================================================================
 * Data Structures
 */
typedef struct vio_fifo_lump  vio_fifo_lump_t ;
typedef struct vio_fifo_lump* vio_fifo_lump ;

struct vio_fifo
{
  struct dl_base_pair(vio_fifo_lump) base ;

  bool    set ;                 /* have at least one lump               */

  bool    as_one ;              /* get_lump == tail && !end_mark
                                   => get_end may not be up to date     */

  bool    hold_mark ;           /* hold stuff while getting             */
  bool    end_mark ;            /* do not get beyond end                */

  char*   hold_ptr ;            /* implicitly in the head lump          */
                                /* used only if "hold_mark"             */

  vio_fifo_lump get_lump ;      /* head lump unless "hold_mark"         */
  char*   get_ptr ;
  char*   get_end ;

  vio_fifo_lump end_lump ;      /* tail lump unless "end_mark"          */
  char*   end_end ;             /* used only if "end_mark"              */

  char*   put_ptr ;             /* implicitly in the tail lump          */
  char*   put_end ;

  size_t  size ;

  vio_fifo_lump spare ;
} ;

typedef struct vio_fifo  vio_fifo_t[1] ;        /* embedded     */
typedef struct vio_fifo* vio_fifo ;

/* Setting a FIFO object to all zeros is enough to initialise it to an
 * empty FIFO (with default lump sizes).
 */
enum
{
  VIO_FIFO_INIT_ALL_ZEROS     = true,
  VIO_FIFO_DEFAULT_LUMP_SIZE  = 4 * 1024
} ;

#define VIO_FIFO_INIT_EMPTY  { 0 }

struct vio_fifo_lump
{
  struct dl_list_pair(vio_fifo_lump) list ;

  char*   end ;         /* end of this particular lump  */
  size_t  size ;        /* size of lump when allocated  */
  char    data[] ;
} ;

/*==============================================================================
 * Functions
 */

extern vio_fifo vio_fifo_init_new(vio_fifo vff, size_t size) ;
extern vio_fifo vio_fifo_reset(vio_fifo vff, free_keep_b free_structure) ;

extern void vio_fifo_clear(vio_fifo vff, bool clear_marks) ;
Inline bool vio_fifo_empty(vio_fifo vff) ;
Inline bool vio_fifo_tail_empty(vio_fifo vff) ;
extern size_t vio_fifo_room(vio_fifo vff) ;

extern void vio_fifo_put_bytes(vio_fifo vff, const char* src, size_t n) ;
Inline void vio_fifo_put_byte(vio_fifo vff, char b) ;

extern int vio_fifo_printf(vio_fifo vff, const char* format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern int vio_fifo_vprintf(vio_fifo vff, const char *format, va_list args) ;
extern int vio_fifo_read_nb(vio_fifo vff, int fd, uint request) ;

extern size_t vio_fifo_get_bytes(vio_fifo vff, void* dst, size_t n) ;
Inline int vio_fifo_get_byte(vio_fifo vff) ;
extern void* vio_fifo_get(vio_fifo vff, size_t* have) ;
extern void vio_fifo_step(vio_fifo vff, size_t step) ;
extern void* vio_fifo_step_get(vio_fifo vff, size_t* p_have, size_t step) ;

extern vio_fifo vio_fifo_copy(vio_fifo dst, vio_fifo src) ;
extern vio_fifo vio_fifo_copy_tail(vio_fifo dst, vio_fifo src) ;

Inline bool vio_fifo_full_lump(vio_fifo vff) ;
extern int vio_fifo_write_nb(vio_fifo vff, int fd, bool all) ;
extern int vio_fifo_fwrite(vio_fifo vff, FILE* file) ;

extern void vio_fifo_skip_to_end(vio_fifo vff) ;
extern void vio_fifo_set_end_mark(vio_fifo vff) ;
extern void vio_fifo_step_end_mark(vio_fifo vff) ;
extern void vio_fifo_clear_end_mark(vio_fifo vff) ;
extern void vio_fifo_back_to_end_mark(vio_fifo vff, bool keep) ;
extern void vio_fifo_set_hold_mark(vio_fifo vff) ;
extern void vio_fifo_clear_hold_mark(vio_fifo vff) ;
extern void vio_fifo_back_to_hold_mark(vio_fifo vff, bool keep) ;
extern void vio_fifo_set_get_wrt_hold(vio_fifo vff, size_t hold_offset) ;

/*==============================================================================
 * Debug -- verification function
 */

Private void vio_fifo_verify(vio_fifo vff) ;

#define VIO_FIFO_DEBUG_VERIFY(vff) if (vio_fifo_debug) vio_fifo_verify(vff)

/*==============================================================================
 * Inline Functions
 */

Private void vio_fifo_lump_new(vio_fifo vff, size_t size) ;
Private int vio_fifo_get_next_byte(vio_fifo vff) ;
Private bool vio_fifo_do_empty(vio_fifo vff) ;

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is empty -- at least: get_ptr == end_end (if any)
 *                                         or: get_ptr == put_ptr.
 */
Inline bool
vio_fifo_empty(vio_fifo vff)
{
  /* if vff is NULL, treat as empty !
   * if !set, then all pointers should be NULL    (so get_ptr == put_ptr)
   * if !end_mark, then vff->end_end will be NULL (so get_ptr != end_end,
   *                                                               unless !set)
   * Is definitely empty if any of the following is true.
   */
  if ((vff == NULL) || (vff->get_ptr == vff->put_ptr)
                    || (vff->get_ptr == vff->end_end))
    return true ;

  /* If get_ptr < get_end is NOT empty                                  */
  if (vff->get_ptr < vff->get_end)
    return false ;

  /* See if can advance get_ptr, and if so whether is then empty.       */
  return vio_fifo_do_empty(vff) ;
} ;

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is empty beyond end_end (if any).
 */
Inline bool
vio_fifo_tail_empty(vio_fifo vff)
{
  /* if vff is NULL, treat as empty !
   * if !set, then all pointers should be NULL (so end_end == put_ptr)
   * if !end_mark, then tail is empty !
   *               else tail is empty iff end_end == put_ptr.
   */
  return (vff == NULL) || !vff->end_mark || (vff->end_end == vff->put_ptr) ;
} ;

/*------------------------------------------------------------------------------
 * Put one byte to the FIFO
 */
Inline void
vio_fifo_put_byte(vio_fifo vff, char b)
{
  if (vff->put_ptr >= vff->put_end)
    vio_fifo_lump_new(vff, 0) ;         /* traps put_ptr > put_end      */

  VIO_FIFO_DEBUG_VERIFY(vff) ;

  *vff->put_ptr++ = b ;
} ;

/*------------------------------------------------------------------------------
 * Get one byte from the FIFO.
 *
 * Returns: 0x00..0xFF -- byte value     (as an int)
 *          -1         => FIFO is empty.
 */
Inline int
vio_fifo_get_byte(vio_fifo vff)
{
  if (vff->get_ptr < vff->get_end)
    return (uchar)*vff->get_ptr++ ;

  return vio_fifo_get_next_byte(vff) ;
} ;

/*------------------------------------------------------------------------------
 * See if have at least one full lump.
 *
 * This may be used with vio_fifo_write_nb(..., false) to use FIFO as a sort of
 * double buffer.
 *
 * Returns: true <=> there is at least one full lump in the FIFO
 *                   (excluding the last lump if it happens to be full)
 */
Inline bool
vio_fifo_full_lump(vio_fifo vff)
{
  return (ddl_head(vff->base) != ddl_tail(vff->base)) ;
} ;

#endif /* _ZEBRA_VIO_FIFO_H */

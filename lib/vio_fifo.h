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

#include "qstring.h"
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
 *
 * Note that we do not support an embedded fifo.
 */
typedef struct vio_fifo_lump  vio_fifo_lump_t ;
typedef struct vio_fifo_lump* vio_fifo_lump ;

struct vio_fifo_lump
{
  struct dl_list_pair(vio_fifo_lump) list ;

  char*   end ;         /* end of this particular lump  */
  char    data[] ;
} ;

struct vio_fifo
{
  struct dl_base_pair(vio_fifo_lump) base ;

  char**  p_start ;             /* -> hold_ptr/get_ptr                    */

  char*   hold_ptr ;            /* p_start == &hold_ptr <=> hold mark set */
                                /* implicitly in the head lump, if set    */

  vio_fifo_lump get_lump ;      /* head lump unless "hold_ptr"            */
  char*   get_ptr ;

  char**  p_get_end ;           /* -> lump->end/end_ptr/put_ptr           */
  char**  p_end ;               /* -> end_ptr/put_ptr                     */

  vio_fifo_lump end_lump ;      /* tail lump unless "end_ptr"             */
  char*   end_ptr ;             /* p_end == &end_ptr <=> end mark set     */

  char*   put_ptr ;             /* implicitly in the tail lump            */
  char*   put_end ;

  ulen    size ;                /* set when initialised                   */

  vio_fifo_lump   spare ;       /* keep back one spare when clearing      */
} ;

typedef struct vio_fifo  vio_fifo_t ;
typedef struct vio_fifo* vio_fifo ;

enum
{
  VIO_FIFO_DEFAULT_LUMP_SIZE  = 4 * 1024
} ;

/*==============================================================================
 * Functions
 */
extern vio_fifo vio_fifo_new(ulen size) ;
extern vio_fifo vio_fifo_free(vio_fifo vff) ;

extern void vio_fifo_clear(vio_fifo vff) ;
extern void vio_fifo_reset(vio_fifo vff) ;

Inline bool vio_fifo_is_empty(vio_fifo vff) ;
Inline bool vio_fifo_is_quite_empty(vio_fifo vff) ;
Inline bool vio_fifo_is_tail_empty(vio_fifo vff) ;
Inline bool vio_fifo_have_hold_mark(vio_fifo vff) ;
Inline bool vio_fifo_have_end_mark(vio_fifo vff) ;

extern void vio_fifo_put_string(vio_fifo vff, const char* src) ;
extern void vio_fifo_put_bytes(vio_fifo vff, const char* src, ulen n) ;
Inline void vio_fifo_put_byte(vio_fifo vff, char b) ;

extern int vio_fifo_printf(vio_fifo vff, const char* format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern int vio_fifo_vprintf(vio_fifo vff, const char *format, va_list args) ;
extern int vio_fifo_read_nb(vio_fifo vff, int fd, ulen require, ulen request) ;
extern void vio_fifo_trim(vio_fifo vff, bool term) ;

extern ulen vio_fifo_get_bytes(vio_fifo vff, void* dst, ulen n) ;
Inline int vio_fifo_get_byte(vio_fifo vff) ;
Inline ulen vio_fifo_get(vio_fifo vff) ;
Inline void* vio_fifo_get_ptr(vio_fifo vff) ;
Inline void vio_fifo_step(vio_fifo vff, ulen step) ;
Inline ulen vio_fifo_step_get(vio_fifo vff, ulen step) ;

extern vio_fifo vio_fifo_copy(vio_fifo dst, vio_fifo src) ;
extern vio_fifo vio_fifo_copy_tail(vio_fifo dst, vio_fifo src) ;
extern vio_fifo vio_fifo_move(vio_fifo dst, vio_fifo src) ;

extern qstring vio_fifo_to_qstring(qstring dst, vio_fifo src) ;

extern int vio_fifo_write_nb(vio_fifo vff, int fd, bool all) ;
extern int vio_fifo_fwrite(vio_fifo vff, FILE* file) ;

extern void vio_fifo_skip_to_end(vio_fifo vff) ;
extern void vio_fifo_set_end_mark(vio_fifo vff) ;
extern void vio_fifo_step_end_mark(vio_fifo vff) ;
extern void vio_fifo_clear_end_mark(vio_fifo vff) ;
extern void vio_fifo_back_to_end_mark(vio_fifo vff) ;
extern void vio_fifo_set_hold_mark(vio_fifo vff) ;
extern void vio_fifo_clear_hold_mark(vio_fifo vff) ;
extern void vio_fifo_back_to_hold_mark(vio_fifo vff, on_off_b on) ;

/*==============================================================================
 * Debug -- verification function
 */

Private void vio_fifo_verify(vio_fifo vff) ;

#define VIO_FIFO_DEBUG_VERIFY(vff) if (vio_fifo_debug) vio_fifo_verify(vff)

/*==============================================================================
 * Inline Functions
 */

Private void vio_fifo_add_lump(vio_fifo vff) ;
Private void vio_fifo_sync_get(vio_fifo vff) ;

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is empty -- at least up to the end_ptr (if any)
 *
 * If there is an end marker, then there may be stuff beyond there.
 *
 * This test of emptiness is consistent with the various get operations, which
 * do not read past the end marker if there is one.
 */
Inline bool
vio_fifo_is_empty(vio_fifo vff)
{
  return (vff == NULL) || (vff->get_ptr == *vff->p_end) ;
} ;

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is completely empty.
 *
 * So, ignoring the end marker, if there is one, see if the FIFO is empty.
 *
 * This is equivalent to: vio_fifo_is_empty(vff) && vio_fifo_is_tail_empty(vff)
 */
Inline bool
vio_fifo_is_quite_empty(vio_fifo vff)
{
  return (vff == NULL) || (vff->get_ptr == vff->put_ptr) ;
} ;

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is empty beyond end_ptr (if any).
 */
Inline bool
vio_fifo_is_tail_empty(vio_fifo vff)
{
  /* if vff is NULL, treat as empty !
   * if end_ptr is NULL, then tail is empty !
   *                     else tail is empty iff end_ptr == put_ptr.
   */
  return (vff == NULL) || (vff->put_ptr == *vff->p_end) ;
} ;

/*------------------------------------------------------------------------------
 * Test whether there is a hold mark.
 */
Inline bool
vio_fifo_have_hold_mark(vio_fifo vff)
{
  return vff->p_start == &vff->hold_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Test whether there is an end mark.
 */
Inline bool
vio_fifo_have_end_mark(vio_fifo vff)
{
  return vff->p_end == &vff->end_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Put one byte to the FIFO
 */
Inline void
vio_fifo_put_byte(vio_fifo vff, char b)
{
  if (vff->put_ptr >= vff->put_end)
    vio_fifo_add_lump(vff) ;             /* traps put_ptr > put_end      */

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
  if (vff->get_ptr < *vff->p_get_end)
    {
      int ch ;
      ch = (uchar)*vff->get_ptr ;

      vio_fifo_step(vff, 1) ;

      return ch ;
    }

  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Get count of bytes available in the current lump.
 *
 * There will always be at least 1 byte available in the current lump, unless
 * the FIFO is empty, or at the end mark.
 *
 * Returns: number of bytes available at vio_fifo_get_ptr().
 */
Inline ulen
vio_fifo_get(vio_fifo vff)
{
  return *vff->p_get_end - vff->get_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to bytes in the current lump.
 *
 * NB: to be called only after vio_fifo_get(), or vio_fifo_steo_get() have
 *     returned a non-zero value.
 */
Inline void*
vio_fifo_get_ptr(vio_fifo vff)
{
  return vff->get_ptr ;
} ;

/*------------------------------------------------------------------------------
 * Step FIFO past bytes used.
 *
 * Can be called after a vio_fifo_get() or vio_fifo_step_get().
 *
 * NB: the "step" argument MUST not exceed the "have" previously returned.
 */
Inline void
vio_fifo_step(vio_fifo vff, ulen step)
{
  vff->get_ptr += step ;

  if (vff->get_ptr >= *vff->p_get_end)
    vio_fifo_sync_get(vff) ;

  VIO_FIFO_DEBUG_VERIFY(vff) ;
} ;

/*------------------------------------------------------------------------------
 * vio_fifo_step() followed by vio_fifo_get() !
 *
 * Can start with a step == 0.
 */
Inline ulen
vio_fifo_step_get(vio_fifo vff, ulen step)
{
  vio_fifo_step(vff, step) ;
  return vio_fifo_get(vff) ;
} ;

#endif /* _ZEBRA_VIO_FIFO_H */

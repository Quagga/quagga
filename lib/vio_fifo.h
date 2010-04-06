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

#include "zebra.h"
#include <stdint.h>
#include <stdbool.h>

#include "list_util.h"
#include "zassert.h"

#ifndef Inline                  /* in case of compiler issues           */
#define Inline static inline
#endif

#ifndef Private                 /* extern, but for "friends" only       */
#define Private extern
#endif

/* GCC have printf type attribute check.  */
#ifdef __GNUC__
#define PRINTF_ATTRIBUTE(a,b) __attribute__ ((__format__ (__printf__, a, b)))
#else
#define PRINTF_ATTRIBUTE(a,b)
#endif /* __GNUC__ */

/*==============================================================================
 * VTY I/O FIFO -- buffering of arbitrary amounts of I/O.
 */

#ifdef  NDEBUG
# define  VIO_FIFO_DEBUG 0      /* NDEBUG override                      */
#else
# ifndef VIO_FIFO_DEBUG
#  define VIO_FIFO_DEBUG 1      /* Set to 1 to turn on debug checks     */
# endif
#endif

/*==============================================================================
 * Data Structures
 */
typedef struct vio_fifo  vio_fifo_t ;
typedef struct vio_fifo* vio_fifo ;

typedef struct vio_fifo_lump  vio_fifo_lump_t ;
typedef struct vio_fifo_lump* vio_fifo_lump ;

struct vio_fifo
{
  struct dl_base_pair(vio_fifo_lump) base ;

  bool    one ;

  char*   put_ptr ;
  char*   put_end ;

  char*   get_ptr ;
  char*   get_end ;

  vio_fifo_lump rdr_lump ;
  char*         rdr_ptr ;

  size_t  size ;

  vio_fifo_lump spare ;
} ;

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

extern vio_fifo vio_fifo_init_new(vio_fifo vf, size_t size) ;
extern vio_fifo vio_fifo_reset(vio_fifo vf, int free_structure) ;

#define vio_fifo_reset_keep(vf) vio_fifo_reset(vf, 0)
#define vio_fifo_reset_free(vf) vio_fifo_reset(vf, 1)

extern void vio_fifo_clear(vio_fifo vf) ;
Inline bool vio_fifo_empty(vio_fifo vf) ;
extern size_t vio_fifo_room(vio_fifo vf) ;

extern void vio_fifo_put(vio_fifo vf, const char* src, size_t n) ;
Inline void vio_fifo_put_byte(vio_fifo vf, char b) ;

extern int vio_fifo_printf(vio_fifo vf, const char* format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern int vio_fifo_vprintf(vio_fifo vf, const char *format, va_list args) ;

extern size_t vio_fifo_get(vio_fifo vf, void* dst, size_t n) ;
Inline int vio_fifo_get_byte(vio_fifo vf) ;
extern void* vio_fifo_get_lump(vio_fifo vf, size_t* have) ;
extern void vio_fifo_got_upto(vio_fifo vf, void* here) ;

Inline bool vio_fifo_full_lump(vio_fifo vf) ;
extern int vio_fifo_write_nb(vio_fifo vf, int fd, bool all) ;

extern void* vio_fifo_get_rdr(vio_fifo vf, size_t* have) ;
extern void* vio_fifo_step_rdr(vio_fifo vf, size_t* have, size_t step) ;
extern void vio_fifo_sync_rdr(vio_fifo vf) ;
extern void vio_fifo_drop_rdr(vio_fifo vf) ;

Private void vio_fifo_lump_new(vio_fifo vf, size_t size) ;
Private int vio_fifo_get_next_byte(vio_fifo vf) ;

/*==============================================================================
 * Debug -- verification function
 */

Private void vio_fifo_verify(vio_fifo vf) ;

#if VIO_FIFO_DEBUG
# define VIO_FIFO_DEBUG_VERIFY(vf) vio_fifo_verify(vf)
#else
# define VIO_FIFO_DEBUG_VERIFY(vf)
#endif

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Returns true <=> FIFO is empty
 */
Inline bool
vio_fifo_empty(vio_fifo vf)
{
  return (vf->get_ptr == vf->put_ptr) ;
}

/*------------------------------------------------------------------------------
 * Put one byte to the FIFO
 */
Inline void
vio_fifo_put_byte(vio_fifo vf, char b)
{
  if (vf->put_ptr >= vf->put_end)
    vio_fifo_lump_new(vf, vf->size) ;   /* traps put_ptr > put_end      */

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  *vf->put_ptr++ = b ;
} ;

/*------------------------------------------------------------------------------
 * Get one byte from the FIFO.
 *
 * Returns: 0x00..0xFF -- byte value     (as an int)
 *          -1         => FIFO is empty.
 */
Inline int
vio_fifo_get_byte(vio_fifo vf)
{
  if (vf->get_end <= (vf->get_ptr + 1))
    return vio_fifo_get_next_byte(vf) ;

  VIO_FIFO_DEBUG_VERIFY(vf) ;

  return (unsigned char)*vf->get_ptr++ ;
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
vio_fifo_full_lump(vio_fifo vf)
{
  return (!vf->one && (vf->put_ptr != NULL)) ;
} ;

#endif /* _ZEBRA_VIO_FIFO_H */

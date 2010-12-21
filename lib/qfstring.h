/* Some string handling -- header
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

#ifndef _ZEBRA_QFSTRING_H
#define _ZEBRA_QFSTRING_H

#include "misc.h"
#include "vargs.h"

/*==============================================================================
 * These "qfstrings" address the issues of dealing with *fixed* length
 * strings, particularly where the string handling must be async-signal-safe.
 */

typedef struct qf_str  qf_str_t ;
typedef struct qf_str* qf_str ;

/* When initialised a qf_string is set:
 *
 *   str    = start of string -- and this is never changed
 *   ptr    = start of string -- this is moved as stuff is appended
 *   end    = last possible position for terminating '\0'
 *                            -- and this is never changed
 */
struct qf_str
{
  char* str ;           /* start of string      */
  char* ptr ;           /* current position     */
  char* end ;           /* end of string        */
} ;

/*------------------------------------------------------------------------------
 * Print format flags for number printing
 */
enum pf_flags
{
  pf_none       = 0,

  /* The following correspond to the "flags"                    */
  pf_commas     = BIT( 0),      /* "'" seen             */
  pf_plus       = BIT( 1),      /* "+" seen             */
  pf_space      = BIT( 2),      /* " " seen             */
  pf_zeros      = BIT( 3),      /* "0" seen             */
  pf_alt        = BIT( 4),      /* "#" seen             */

  pf_precision  = BIT( 7),      /* '.' seen             */

  /* The following signal how to render the value               */
  pf_hex        = BIT( 8),      /* hex                  */
  pf_uc         = BIT( 9),      /* upper-case           */

  /* The following signal the type of value                     */
  pf_ptr        = BIT(14),      /* is a pointer         */
  pf_unsigned   = BIT(15),      /* unsigned value       */

  /* Common combination                                         */
  pf_hex_x      = pf_unsigned | pf_hex,
  pf_hex_X      = pf_unsigned | pf_hex | pf_uc,

  pf_void_p     = pf_ptr | pf_hex_x,
} ;

/*==============================================================================
 * Functions
 */

extern void qfs_init(qf_str qfs, char* str, size_t size) ;
extern void qfs_init_as_is(qf_str qfs, char* str, size_t size) ;

extern void qfs_term(qf_str qfs, const char* src) ;

Inline int   qfs_len(qf_str qfs) ;
Inline void* qfs_ptr(qf_str qfs) ;
Inline int   qfs_left(qf_str qfs) ;

extern void qfs_append(qf_str qfs, const char* src) ;
extern void qfs_append_n(qf_str qfs, const char* src, size_t n) ;

extern void qfs_append_ch_x_n(qf_str qfs, char ch, size_t n) ;
extern void qfs_append_justified(qf_str qfs, const char* src, int width) ;
extern void qfs_append_justified_n(qf_str qfs, const char* src,
                                                          size_t n, int width) ;

extern void qfs_signed(qf_str qfs, intmax_t s_val, enum pf_flags flags,
                                                     int width, int precision) ;
extern void qfs_unsigned(qf_str qfs, uintmax_t u_val, enum pf_flags flags,
                                                     int width, int precision) ;
extern void qfs_pointer(qf_str qfs, void* p_val, enum pf_flags flags,
                                                     int width, int precision) ;

extern void qfs_printf(qf_str qfs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern void qfs_vprintf(qf_str qfs, const char *format, va_list args) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Current length of qf_str, not counting the terminating '\0'.
 */
Inline int
qfs_len(qf_str qfs)
{
  return qfs->ptr - qfs->str ;
} ;

/*------------------------------------------------------------------------------
 * Address of the terminating '\0'.
 */
Inline void*
qfs_ptr(qf_str qfs)
{
  return qfs->ptr ;
} ;

/*------------------------------------------------------------------------------
 * Current space left in the qstr, given what has been reserved for terminating
 * '\0' and any other reservation.
 */
Inline int
qfs_left(qf_str qfs)
{
  return qfs->end - qfs->ptr ;
} ;


#endif /* _ZEBRA_QSTRING_H */

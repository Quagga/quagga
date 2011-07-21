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
 *
 * Are also used to support snprintf() style printing, but to one or more
 * fixed length buffers.
 */

/* When initialised a qf_string is set:
 *
 *   str      = start of string -- and this is never changed
 *   ptr      = start of string -- this is moved as stuff is appended
 *   end      = end of string   -- and this is never changed
 *   offset   = number of characters left to omit -- used to collect a long
 *              string in more than one section
 *   overflow = number of characters that had to leave out, because string
 *              was too short.
 *
 * When filling the string, will work right up to the byte just before the
 * end.  Does not terminate the string -- that must be done explicitly.
 */
struct qf_str
{
  char*  str ;          /* start of string              */
  char*  ptr ;          /* current position             */
  char*  end ;          /* end of string                */
  uint   offset ;       /* number of chars to omit      */
  uint   overflow ;     /* number of excess chars       */
} ;

typedef struct qf_str  qf_str_t[1] ;
typedef struct qf_str* qf_str ;

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

extern void qfs_init(qf_str qfs, char* str, uint size) ;
extern void qfs_init_offset(qf_str qfs, char* str, uint size, uint offset) ;
extern void qfs_reset_offset(qf_str qfs, uint offset) ;
extern void qfs_init_as_is(qf_str qfs, char* str, uint size) ;

Inline uint qfs_overflow(qf_str qfs) ;
Inline uint qfs_term(qf_str qfs) ;
extern void qfs_term_string(qf_str qfs, const char* src, uint n) ;

Inline uint  qfs_len(qf_str qfs) ;
Inline void* qfs_ptr(qf_str qfs) ;
Inline uint  qfs_left(qf_str qfs) ;

extern void qfs_append(qf_str qfs, const char* src) ;
extern void qfs_append_n(qf_str qfs, const char* src, uint n) ;

extern void qfs_append_ch_x_n(qf_str qfs, char ch, uint n) ;
extern void qfs_append_justified(qf_str qfs, const char* src, int width) ;
extern void qfs_append_justified_n(qf_str qfs, const char* src,
                                                            uint n, int width) ;

extern void qfs_signed(qf_str qfs, intmax_t s_val, enum pf_flags flags,
                                                     int width, int precision) ;
extern void qfs_unsigned(qf_str qfs, uintmax_t u_val, enum pf_flags flags,
                                                     int width, int precision) ;
extern void qfs_pointer(qf_str qfs, void* p_val, enum pf_flags flags,
                                                     int width, int precision) ;

extern uint qfs_printf(qf_str qfs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern uint qfs_vprintf(qf_str qfs, const char *format, va_list args) ;

Inline uint qfs_strlen(const char* str) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Current length of qf_str.
 */
Inline uint
qfs_len(qf_str qfs)
{
  return qfs->ptr - qfs->str ;
} ;

/*------------------------------------------------------------------------------
 * Address for next byte -- assuming zero offset -- may be the end.
 */
Inline void*
qfs_ptr(qf_str qfs)
{
  return qfs->ptr ;
} ;

/*------------------------------------------------------------------------------
 * Current space left in the qstr -- assuming zero offset.
 */
Inline uint
qfs_left(qf_str qfs)
{
  return qfs->end - qfs->ptr ;
} ;

/*------------------------------------------------------------------------------
 * Did everything we put in the qfs, fit ?.
 *
 * Returns:  number of chars that did *not* fit.
 */
Inline uint
qfs_overflow(qf_str qfs)
{
  return qfs->overflow ;
} ;

/*------------------------------------------------------------------------------
 * Insert '\0' terminator -- overwrites the last byte, if required.
 *
 * Assumes the qfs is not zero length !
 *
 * Returns:  number of chars that did *not* fit (after using one for '\0').
 *
 * NB: does not advance pointer -- so length does not include the '\0'
 */
Inline uint
qfs_term(qf_str qfs)
{
  if (qfs->ptr >= qfs->end)
    {
      assert((qfs->ptr == qfs->end) && (qfs->ptr > qfs->str)) ;
      --qfs->ptr ;
      ++qfs->overflow ;
    } ;

  *qfs->ptr = '\0' ;
  return qfs->overflow ;
} ;

/*------------------------------------------------------------------------------
 * async-signal-safe strlen
 */
Inline uint
qfs_strlen(const char* str)
{
  const char* s ;

  s = str ;

  if (s != NULL)
    while (*s != '\0')
      ++s ;

  return s - str ;
}

/*==============================================================================
 * Fixed Size String Buffers
 *
 * This supports the common case of a function whose task is to construct a
 * (small) string of known maximum length, which will promptly be output
 * or something similar.
 *
 * This scheme removes the need for the caller to construct a small buffer
 * and pass it to the string constructor.  The "magic" is to make the callee
 * return a struct containing the result.  So the callee is, for example:
 *
 *   foo_t make_foo(...) { ... } ;
 *
 * where foo_t is a struct, with a "str" element large enough for all known
 * foo.  So the caller can, for example:
 *
 *   printf("...%s...", ..., make_foo(...).str, ...) ;
 *
 * All the fiddling around with buffers and buffer sizes is hidden from the
 * caller.  And, since the buffer is implicitly on the stack, this is thread
 * safe (and async-signal-safe, provided make_foo() is).
 *
 * The macro: qfb_t(name, len) declares a fixed length buffer type.  So:
 *
 *   QFB_T(foo, 79) ;
 *
 * declares:
 *
 *   typedef struct { char str[79 + 1] ; } foo_t ;
 *
 * NB: the length given *excludes* the terminating '\0' ;
 *
 * NB: the type declared has the "_t" added *automatically*.
 *
 * Having declared a suitable type, function(s) can be declared to return
 * a string in a value of that type.
 *
 * A string generating function can use the buffer directly, for example:
 *
 *   foo_t make_foo(...)
 *   {
 *     foo_t foo ;
 *
 *       ...  foo.str          is the address of the string buffer
 *       ...  sizeof(foo.str)  is its length *including* the '\0'
 *
 *     return foo ;
 *   } ;
 *
 * The qfstring facilities may be used to construct the string, and to
 * facilitate that, the macro: qfb_qfs(buf, qfs) declares the buffer and a
 * qf_str_t and initialises same, thus:
 *
 *   QFB_QFS(foo, foo_qfs) ;
 *
 * declares:
 *
 *   foo_t    foo ;
 *   qf_str_t foo_qfs = { ...initialised for empty foo... } ;
 *
 * So the string generator can use foo_qfs and qfstring facilities to fill in
 * the string in foo, and then return foo (having terminated it) as above.
 *
 * So... with two macros we reduce the amount of fiddling about required to
 * do something reasonably simple.
 *
 * NB: it is quite possible that the compiler will allocate two buffers, one
 *     in the caller's stack frame and one in the callee's, and returning the
 *     value will involve copying from one to the other.
 */
#define QFB_T(name, len) \
  typedef struct { char str[((len) | 7) + 1] ; } name##_t

#define QFB_QFS(qfb, qfs) \
  qfb##_t     qfb ;       \
  qf_str_t  qfs = { { .str      = qfb.str,                   \
                      .ptr      = qfb.str,                   \
                      .end      = qfb.str + sizeof(qfb.str), \
                      .offset   = 0,                         \
                      .overflow = 0  } }

/* And, finally, a "standard" qfb for general use: qfb_gen_t !          */

enum { qfb_gen_len = 200 } ;    /* More than enough for most purposes ! */
QFB_T(qfb_gen, qfb_gen_len) ;

#endif /* _ZEBRA_QFSTRING_H */

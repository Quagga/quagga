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

#include <stddef.h>
#include <stdint.h>

#include "qtime.h"

/*==============================================================================
 * These "qfstring" address the issues of dealing with *fixed* length
 * strings, particularly where the string handling must be async-signal-safe.
 *
 * Are also used to support snprintf() style printing, but to one or more
 * fixed length buffers.
 *
 * All operations that can possibly be async-signal-safe, are.  Notable
 * exception is anything involving floating point values -- because of the
 * state contain in floating point status/option registers !
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

  /* The following correspond to the standard "flags"
   *
   * Note that C standard specifies that ' ' is ignored if '+' seen.
   */
  pf_commas     = BIT( 0),      /* "'": *non-standard*          */
  pf_plus       = BIT( 1),      /* '+': add '+' if >= 0         */
  pf_space      = BIT( 2),      /* ' ': add ' ' if >= 0         */
  pf_zeros      = BIT( 3),      /* '0': add leading 0's         */
  pf_alt        = BIT( 4),      /* '#': "alternative" form      */

  /* Non-standard flags                                         */
  pf_plus_nz    = BIT( 5),      /* add '+' if > 0               */

  /* A precision part (empty or otherwise) has been seen        */
  pf_precision  = BIT( 7),      /* '.' seen                     */

  /* The following signal how to render the value               */
  pf_oct        = BIT( 8),      /* octal                */
  pf_hex        = BIT( 9),      /* hex                  */
  pf_uc         = BIT(10),      /* upper-case           */

  /* For scaled formatting of decimals and byte counts          */
  pf_scale      = BIT(11),      /* scale and add scale tag      */
  pf_trailing   = BIT(12),      /* add blank scale if required  */

  /* The following signal the type of value                     */
  pf_ptr        = BIT(14),      /* is a pointer         */
  pf_unsigned   = BIT(15),      /* unsigned value       */

  /* Common combination                                         */
  pf_hex_x      = pf_unsigned | pf_hex,
  pf_hex_X      = pf_unsigned | pf_hex | pf_uc,

  pf_void_p     = pf_ptr | pf_hex_x,
} ;

typedef enum pf_flags pf_flags_t ;

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
 * The macro: QFB_T(name, len) declares a fixed length buffer type.  So:
 *
 *   QFB_T(79) foo_t ;
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
 *     foo_t foo_buf ;
 *
 *       ...  foo_buf.str          is the address of the string buffer
 *       ...  sizeof(foo_buf.str)  is its length *including* the '\0'
 *
 *     return foo_buf ;
 *   } ;
 *
 * The qfstring facilities may be used to construct the string, and to
 * facilitate that, the macro QFB_QFS declares the buffer and a qf_str_t and
 * initialises same, thus:
 *
 *   foo_t QFB_QFS(foo_buf, foo_qfs) ;
 *
 * declares:
 *
 *   foo_t    foo_buf ;
 *   qf_str_t foo_qfs = { ...initialised for empty foo... } ;
 *
 * So the string generator can use foo_qfs and qfstring facilities to fill in
 * the string in foo_buf, and then return foo_buf (having terminated it) as
 * above.
 *
 * So... with two macros we reduce the amount of fiddling about required to
 * do something reasonably simple.
 *
 * NB: it is quite possible that the compiler will allocate two buffers, one
 *     in the caller's stack frame and one in the callee's, and returning the
 *     value will involve copying from one to the other.
 */
#define QFB_T(len) \
  typedef struct { char str[((len) | 7) + 1] ; }

#define QFB_QFS(qfb, qfs) \
   qfb ; \
  qf_str_t qfs = { { .str      = qfb.str,                   \
                     .ptr      = qfb.str,                   \
                     .end      = qfb.str + sizeof(qfb.str), \
                     .offset   = 0,                         \
                     .overflow = 0  } }

/* And, finally, a "standard" qfb for general use: qfb_gen_t !          */

enum { qfb_gen_len = 200 } ;    /* More than enough for most purposes ! */
QFB_T(qfb_gen_len) qfb_gen_t ;

/*==============================================================================
 * Simple keyword support
 *
 * A "keyword table" is an array of qfs_keyword_t, the last entry of which
 * has a NULL word, eg:
 *
 *   static qfs_keyword_t deny_permit_table[] =
 *   {
 *     { .word = "deny",    .val = 0 },
 *     { .word = "permit",  .val = 1 },
 *     { .word = NULL }
 *   } ;
 */
typedef struct qfs_keyword
{
  const char* word ;
  uint        val ;             /* NB: <= MAX_INT       */
} qfs_keyword_t ;

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

extern void qfs_signed(qf_str qfs, intmax_t s_val, pf_flags_t flags,
                                                     int width, int precision) ;
extern void qfs_unsigned(qf_str qfs, uintmax_t u_val, pf_flags_t flags,
                                                     int width, int precision) ;
extern void qfs_pointer(qf_str qfs, void* p_val, pf_flags_t flags,
                                                     int width, int precision) ;

extern uint qfs_printf(qf_str qfs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern qfb_gen_t qfs_gen(const char* format, ...)      PRINTF_ATTRIBUTE(1, 2) ;

extern uint qfs_vprintf(qf_str qfs, const char *format, va_list args) ;

Inline uint qfs_strlen(const char* str) ;

extern int qfs_keyword_lookup(qfs_keyword_t* table, const char* str,
                                                                  bool strict) ;
extern int qfs_keyword_lookup_nocase(qfs_keyword_t* table, const char* str,
                                                                  bool strict) ;
extern int qfs_keyword_lookup_abstract(void* a_array, const char* str,
                                                                  bool strict,
                           const char* (*a_lookup)(void* a_array, uint index)) ;

/*------------------------------------------------------------------------------
 * Construction of numbers from long and other stuff.
 *
 * Need enough space for sign, then groups of 3 decimal digits plus ',' or '\0'.
 * For 64 bits comes out at 29 bytes !
 */
enum { qfs_number_len = 1 + (((64 + 9) / 10) * (3 + 1)) } ;

CONFIRM((sizeof(long) * 8) <= 64) ;

QFB_T(qfs_number_len) qfs_num_str_t ;

extern qfs_num_str_t qfs_dec_value(long val, pf_flags_t flags) ;
extern qfs_num_str_t qfs_bin_value(long val, pf_flags_t flags) ;

/* Time period expressed as +999,999d99h99m99.999s (22 characters !)
 */
CONFIRM(qfs_number_len > (1+4+4+3+3+3+4)) ;

extern qfs_num_str_t qfs_time_period(qtime_t val, pf_flags_t flags) ;

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

#endif /* _ZEBRA_QFSTRING_H */

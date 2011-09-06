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

#ifndef _ZEBRA_QSTRING_H
#define _ZEBRA_QSTRING_H

#include "misc.h"
#include "vargs.h"
#include "zassert.h"
#include "memory.h"
#include "elstring.h"

/*==============================================================================
 * These "qstrings" address address the lack of a flexible length string in 'C'.
 *
 * This is not a general purpose strings module, but provides a limited number
 * of useful string operations such that the caller does not need to worry
 * about the length of the string, and allocating space and so on.
 *
 * The caller does, however, have to explicitly release the contents of a
 * qstring when it is done with.
 *
 * The mechanics of a qstring work on a length/pointer basis.  The body of a
 * qstring is not guaranteed to be '\0' terminated, but when memory is
 * allocated provision is always made for a terminator beyond the 'len'.
 * The qs_string() function will add a '\0' at the 'len' position.
 *
 * The body of a qstring is allocated and extended automatically as the length
 * or the size is set.  The address of the body can, therefore, change -- so
 * should be careful when handling pointers to the body.  The qstring handling
 * tends to hold on to any body that has been allocated -- only qs_reset() will
 * free the body.
 *
 * The qstring supports an "alias" state.  A qstring can be set to be an
 * "alias" for another length/pointer string, and at some later date that
 * can be copied to the qstring body -- to be changed or for any other
 * reason.
 */
struct qstring
{
  elstring_t  els ;             /* *embedded*                   */

  usize       size ;            /* of the els body              */

  ulen        cp ;

  void*       b_body ;
  usize       b_size ;

  bool        alias ;
  char*       empty ;
} ;

typedef struct qstring  qstring_t[1] ;
typedef struct qstring* qstring ;

/* Setting an qstring object to all zeros is enough to initialise it to
 * an empty string -- including the embedded elstring.
 */
CONFIRM(ELSTRING_INIT_ALL_ZEROS) ;
enum
{
  QSTRING_INIT_ALL_ZEROS = true
} ;

/*------------------------------------------------------------------------------
 * Access functions for body of qstring -- to take care of casting pointers
 *
 * There are generally two versions of each function:
 *
 *   xxxx_nn    -- where the argument may NOT be NULL
 *
 *   xxxx       -- where a NULL or zero value is returned if the argument
 *                 is NULL
 *
 * NB: the various 'cp', 'len' and 'at' functions do not guarantee that these
 *     positions exist within the current body of the string.
 *
 */

Inline elstring qs_els(qstring qs) ;
Inline elstring qs_els_nn(qstring qs) ;
Inline void qs_els_copy(elstring els, qstring qs) ;
Inline void qs_els_copy_nn(elstring els, qstring qs) ;

Inline char* qs_char(qstring qs) ;
Inline char* qs_char_nn(qstring qs) ;

Inline char* qs_cp_char(qstring qs) ;
Inline char* qs_cp_char_nn(qstring qs) ;

Inline char* qs_ep_char(qstring qs) ;
Inline char* qs_ep_char_nn(qstring qs) ;

Inline char* qs_char_at(qstring qs, usize off) ;
Inline char* qs_char_at_nn(qstring qs, usize off) ;

Inline void* qs_body(qstring qs) ;
Inline void* qs_body_nn(qstring qs) ;
Inline void qs_set_body_nn(qstring qs, const void* body) ;

Inline usize qs_size(qstring qs) ;
Inline usize qs_size_nn(qstring qs) ;

Inline ulen qs_len(qstring qs) ;
Inline ulen qs_len_nn(qstring qs) ;
Inline void qs_set_len_nn(qstring qs, ulen len) ;
Inline void qs_set_strlen_nn(qstring qs) ;

Inline ulen qs_cp(qstring qs) ;
Inline ulen qs_cp_nn(qstring qs) ;
Inline void qs_set_cp_nn(qstring qs, usize cp) ;
Inline void qs_move_cp_nn(qstring qs, int delta) ;

Inline ulen qs_after_cp(qstring qs) ;
Inline ulen qs_after_cp_nn(qstring qs) ;

Inline void qs_pp(pp p, qstring qs) ;
Inline void qs_pp_nn(pp p, qstring qs) ;

Inline void qs_cpp(cpp p, qstring qs) ;
Inline void qs_cpp_nn(cpp p, qstring qs) ;

/*------------------------------------------------------------------------------
 * Functions to fetch the elstring body of the qstring.
 */

/* Pointer to elstring of qstring -- returns NULL if qstring is NULL    */
Inline elstring
qs_els(qstring qs)
{
  return (qs != NULL) ? qs->els : NULL ;
} ;

/* Pointer to elstring of qstring (not NULL)                            */
Inline elstring
qs_els_nn(qstring qs)
{
  return qs->els ;
} ;

/* Copy elstring of qstring to another elstring (elstring not NULL)     */
Inline void
qs_els_copy(elstring els, qstring qs)
{
  if (qs != NULL)
    qs_els_copy_nn(els, qs) ;
  else
    els_null(els) ;
} ;

/* Copy elstring of qstring to another elstring (neither NULL)          */
Inline void
qs_els_copy_nn(elstring els, qstring qs)
{
  *els = *(qs->els) ;
} ;

/*------------------------------------------------------------------------------
 * Functions to fetch pointers to or into the string body.
 *
 * NB: these pointers must be treated with care -- and change to the string
 *     may invalidate the pointer.  Where the qstring is an alias, the pointer
 *     returned is the alias -- which could disappear !
 */

/* Start of qstring -- returns NULL if qstring is NULL, or body is.     */
Inline char*
qs_char(qstring qs)
{
  return qs_body(qs) ;
} ;

/* Start of qstring (not NULL)-- returns NULL if body is NULL.          */
Inline char*
qs_char_nn(qstring qs)
{
  return qs_body_nn(qs) ;
} ;

/* Offset in qstring -- returns NULL if qstring is NULL
 *                      returns *nonsense if body is NULL               */
Inline char*
qs_char_at(qstring qs, usize off)
{
  return (qs != NULL) ? qs_char_at_nn(qs, off) : NULL ;
} ;

/* Offset in qstring (not NULL) -- returns *nonsense if body is NULL    */
Inline char*
qs_char_at_nn(qstring qs, usize off)
{
  return qs_char_nn(qs) + off ;
} ;

/* 'cp' in qstring -- returns NULL if qstring is NULL
 *                    returns *nonsense if body is NULL                 */
Inline char*
qs_cp_char(qstring qs)
{
  return (qs != NULL) ? qs_cp_char_nn(qs) : NULL ;
} ;

/* 'cp' in qstring (not NULL) -- returns *nonsense if body is NULL      */
Inline char*
qs_cp_char_nn(qstring qs)
{
  return qs_char_at_nn(qs, qs_cp_nn(qs)) ;
} ;

/* 'len' in qstring -- returns NULL if qstring is NULL
 *                     returns *nonsense if body is NULL                */
Inline char*
qs_ep_char(qstring qs)
{
  return (qs != NULL) ? qs_ep_char_nn(qs) : NULL ;
} ;

/* 'len' in qstring (not NULL) -- returns *nonsense if body is NULL     */
Inline char*
qs_ep_char_nn(qstring qs)
{
  return qs_char_at_nn(qs, qs_len_nn(qs)) ;
} ;

/* Start of qstring -- returns NULL if qstring is NULL, or body is.     */
Inline void*
qs_body(qstring qs)
{
  return (qs != NULL) ? qs_body_nn(qs) : NULL ;
} ;

/* Start of qstring (not NULL)-- returns NULL if body is NULL.          */
Inline void*
qs_body_nn(qstring qs)
{
  return els_body_nn(qs->els) ;
} ;

/* Set new body of qstring (not NULL).
 *
 * Caller must ensure that 'size', 'len' & 'cp' are all valid !         */
Inline void
qs_set_body_nn(qstring qs, const void* body)
{
  els_set_body_nn(qs->els, body) ;
} ;

/* Size of qstring body -- zero if qstring is NULL, or is alias.        */
Inline usize
qs_size(qstring qs)
{
  return (qs != NULL) ? qs_size_nn(qs) : 0 ;
} ;

/* Size of qstring (not NULL).                                          */
Inline usize
qs_size_nn(qstring qs)
{
  return (qs->size) ;
} ;

/*----------------------------------------------------------------------------*/

/* 'len' of qstring -- returns 0 if qstring is NULL                     */
Inline ulen
qs_len(qstring qs)
{
  return (qs != NULL) ? qs_len_nn(qs) : 0 ;
} ;

/* 'len' of qstring (not NULL)                                          */
Inline ulen
qs_len_nn(qstring qs)
{
  return els_len_nn(qs->els) ;
} ;

/* set 'len' of qstring (not NULL) -- caller responsible for validity   */
Inline void
qs_set_len_nn(qstring qs, ulen len)
{
  els_set_len_nn(qs->els, len) ;
} ;

/* set 'len' of qstring according to strlen(body) -- nothing NULL !     */
Inline void
qs_set_strlen_nn(qstring qs)
{
  els_set_len_nn(qs->els, strlen(qs_body_nn(qs))) ;
} ;

/*----------------------------------------------------------------------------*/

/* 'cp' of qstring -- returns 0 if qstring is NULL                      */
Inline ulen
qs_cp(qstring qs)
{
  return (qs != NULL) ? qs_cp_nn(qs) : 0 ;
} ;

/* 'cp' of qstring (not NULL)                                           */
Inline ulen
qs_cp_nn(qstring qs)
{
  return qs->cp ;
} ;

/* set 'cp' of qstring (not NULL) -- caller responsible for validity    */
Inline void
qs_set_cp_nn(qstring qs, usize cp)
{
  qs->cp = cp ;
} ;

/* move 'cp' of qstring (not NULL) -- caller responsible for validity   */
Inline void
qs_move_cp_nn(qstring qs, int delta)
{
  qs->cp += delta ;
} ;

/*----------------------------------------------------------------------------*/

/* 'len' - 'cp' of qstring (not NULL) -- zero if 'len' < 'cp'           */
Inline ulen
qs_after_cp_nn(qstring qs)
{
  return (qs_len_nn(qs) > qs_cp_nn(qs)) ? qs_len_nn(qs) - qs_cp_nn(qs) : 0 ;
} ;

/* 'len' - 'cp' of qstring -- zero if NULL or 'len' < 'cp'              */
Inline ulen
qs_after_cp(qstring qs)
{
  return (qs != NULL) ? qs_after_cp_nn(qs) : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Functions to fetch various pointer pairs.
 */

Inline void
qs_pp(pp p, qstring qs)
{
  if (qs != NULL)
    qs_pp_nn(p, qs) ;
  else
    pp_null(p) ;
} ;

Inline void
qs_pp_nn(pp p, qstring qs)
{
  els_pp_nn(p, qs->els) ;
} ;

Inline void
qs_cpp(cpp p, qstring qs)
{
  if (qs != NULL)
    qs_cpp_nn(p, qs) ;
  else
    cpp_null(p) ;
} ;

Inline void
qs_cpp_nn(cpp p, qstring qs)
{
  els_cpp_nn(p, qs->els) ;
} ;

/*------------------------------------------------------------------------------
 * Set real body -- discarding any alias.
 *
 * NB: does not affect 'len' or 'cp'
 */
Inline void qs_set_real_body_nn(qstring qs)
{
  qs->size = qs->b_size ;
  qs_set_body_nn(qs, qs->b_body) ;
  qs->alias = false ;
} ;

/*==============================================================================
 * Functions
 */

extern qstring qs_new(usize slen) ;
extern qstring qs_new_with_body(usize slen) ;
extern qstring qs_init_new(qstring qs, usize len) ;
extern qstring qs_reset(qstring qs, free_keep_b free_structure) ;
Inline qstring qs_free(qstring qs) ;

extern char* qs_make_string(qstring qs) ;
extern const char* qs_string(qstring qs) ;
extern void qs_clear(qstring qs) ;
extern qstring qs_new_size(qstring qs, usize slen) ;
extern qstring qs_extend(qstring qs, usize elen) ;
extern void qs_chop(qstring qs, usize clen) ;

Private void qs_make_to_size(qstring qs, usize len, usize alen) ;

extern qstring qs_set(qstring qs, const char* src) ;
extern qstring qs_set_n(qstring qs, const char* src, usize n) ;
extern qstring qs_set_qs(qstring qs, qstring src) ;
extern qstring qs_set_els(qstring qs, elstring src) ;
extern qstring qs_set_fill(qstring qs, usize len, const char* src) ;
extern qstring qs_set_fill_n(qstring qs, usize len, const char* src,
                                                                   usize flen) ;

extern qstring qs_append_str(qstring qs, const char* src) ;
extern qstring qs_append_str_n(qstring qs, const char* src, usize n) ;
extern qstring qs_append(qstring qs, qstring src) ;
extern qstring qs_append_els(qstring qs, elstring src) ;

extern qstring qs_set_alias(qstring qs, const char* src) ;
extern qstring qs_set_alias_n(qstring qs, const char* src, usize len) ;
extern qstring qs_set_alias_qs(qstring qs, qstring src) ;
extern qstring qs_set_alias_els(qstring qs, elstring src) ;

extern qstring qs_copy(qstring dst, qstring src) ;

extern qstring qs_printf(qstring qs, const char* format, ...)
                                                       PRINTF_ATTRIBUTE(2, 3) ;
extern qstring qs_vprintf(qstring qs, const char *format, va_list args) ;

extern usize qs_replace(qstring qs, usize r, const void* src, usize n) ;
Inline usize qs_insert(qstring qs, const void* src, usize n)
{
  return qs_replace(qs, 0, src, n) ;
} ;
Inline usize qs_delete(qstring qs, usize n)
{
  return qs_replace(qs, n, NULL, 0) ;
} ;

Inline int qs_cmp(qstring a, qstring b) ;
Inline int qs_cmp_word(qstring a, qstring w) ;
Inline int qs_cmp_sig(qstring a, qstring b) ;
Inline bool qs_equal(qstring a, qstring b) ;
Inline bool qs_substring(qstring a, qstring b) ;

/*==============================================================================
 * The Inline functions.
 */

/*------------------------------------------------------------------------------
 * Free given qstring
 */
Inline qstring
qs_free(qstring qs)
{
  return qs_reset(qs, free_it) ;
} ;

/*------------------------------------------------------------------------------
 * Compare two qstrings -- returns the usual -ve, 0, +ve cmp result.
 *
 * NULL qstring is treated as empty.
 */
Inline int
qs_cmp(qstring a, qstring b)
{
  return els_cmp(qs_els(a), qs_els(b)) ;
} ;

/*------------------------------------------------------------------------------
 * Compare qstrings to given word -- see els_cmp_word
 */
Inline int
qs_cmp_word(qstring a, qstring w)
{
  return els_cmp_word(qs_els(a), qs_els(w)) ;
} ;

/*------------------------------------------------------------------------------
 * Compare significant parts of two qstrings -- returns the usual -ve, 0, +ve
 * cmp result.
 *
 * By significant, mean excluding leading/trailing isspace() and treating
 * multiple isspace() as single isspace().
 *
 * NULL qstring is treated as empty.
 */
Inline int
qs_cmp_sig(qstring a, qstring b)
{
  return els_cmp_sig(qs_els(a), qs_els(b)) ;
} ;

/*------------------------------------------------------------------------------
 * Are two qstrings equal ?  -- returns true if they are.
 */
Inline bool
qs_equal(qstring a, qstring b)
{
  return els_equal(qs_els(a), qs_els(b)) ;
} ;

/*------------------------------------------------------------------------------
 * Is 'b' a leading substring of 'a' ?  -- returns true if it is.
 *
 * If 'b' is empty it is always a leading substring.
 */
Inline bool
qs_substring(qstring a, qstring b)
{
  return els_substring(qs_els(a), qs_els(b)) ;
} ;

#endif /* _ZEBRA_QSTRING_H */

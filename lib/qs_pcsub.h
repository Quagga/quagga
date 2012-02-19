/* Substitution of percent escapes in qstring -- header
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

#ifndef _ZEBRA_QS_PCSUB_H
#define _ZEBRA_QS_PCSUB_H

#include "misc.h"
#include "qstring.h"

/*==============================================================================
 * Fairly general purpose engine to substitute "%...x" escapes in a qstring.
 *
 * A "%...x" escape is up to 22 characters starting '%' and ending 'a'..'z' or
 * 'A'..'Z'.  The (up to 20) characters between may be anything ' '..'\x7E',
 * except for '%', 'a'..'z' and 'A'..'Z' and are escape specific modifiers.
 * A pair of "%%" is also an escape.
 *
 * Anything which is not an escape according to the above is ignored, including
 * escapes which are too long.
 *
 * The substitution process is driven by a table, whose entries take the
 * form:
 *
 *   'x'    -- the escape "action" char (alphabetic)
 *   parser -- address of escape parsing function
 *   int    -- extra parameter for parser
 *
 * The process is also given an "argument array", which is an array of void*
 * pointers, also passed to the parser.
 *
 * The parser function verifies the escape, and returns either a qstring
 * containing the substitution text, or NULL indicating that the escape is
 * not to be substituted (perhaps because is invalid).
 *
 * There is a default parser which expects the escape to have no modifiers, and
 * for the extra parameter to be the index into the argument vector, which is
 * assumed to give the address of a '\0' terminated string.
 *
 * The table is terminated by the required '%' entry -- the parser and extra
 * fields of which are ignored.
 */

typedef struct
{
  uint  count ;
  void* args[] ;

} qs_pcsub_args_t ;

typedef qs_pcsub_args_t* qs_pcsub_args ;

enum { qs_pcsub_esc_max = 20 } ;
typedef char qs_pcsub_esc_t[qs_pcsub_esc_max + 1] ;     /* '\0' terminated */

typedef qstring qs_pcsub_parser_f(qstring qs, char action, qs_pcsub_esc_t esc,
                                                qs_pcsub_args args, int extra) ;
typedef struct
{
  char  action ;
  qs_pcsub_parser_f*
        parser ;
  int   extra ;

} qs_pcsub_entry_t ;

typedef qs_pcsub_entry_t  qs_pcsub_table_t[] ;
typedef qs_pcsub_entry_t* qs_pcsub_table ;

/*==============================================================================
 * Functions
 */

extern void qs_pcsub(qstring qs, const qs_pcsub_table table, ...) ;
extern qs_pcsub_parser_f qs_pcsub_default ;
extern void* qs_pcsub_arg(qs_pcsub_args args, int index) ;

#endif /* _ZEBRA_QS_PCSUB_H */

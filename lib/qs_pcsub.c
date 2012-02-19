/* Substitution of percent escapes in qstring
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
#include "misc.h"

#include "qs_pcsub.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Perform percent escape substitution on the given qstring.
 *
 * Does nothing if qs == NULL.
 */
extern void
qs_pcsub(qstring qs, qs_pcsub_table table, ...)
{
  qs_pcsub_esc_t  esc ;
  qs_pcsub_args   args ;
  va_list va ;
  qstring sub ;
  usize   cp ;
  usize   count ;

  if (qs == NULL)
    return ;

  /* Construct and fill in the argument vector.
   */
  count = 10 ;
  args = XCALLOC(MTYPE_TMP, sizeof(qs_pcsub_args_t) + (count * sizeof(void*))) ;
  args->count = 0 ;

  va_start (va, table);
  while (1)
    {
      void* ap ;

      ap = va_arg(va, void*) ;
      if (ap == NULL)
        break ;

      if (args->count == count)
        {
          count += 10 ;

          args = XREALLOC(MTYPE_TMP, args,
                            sizeof(qs_pcsub_args_t) + (count * sizeof(void*))) ;
        } ;

      args->args[args->count++] = ap ;
    } ;

  va_end(va) ;

  /* Now process the qstring against the table and the arguments
   */
  sub = NULL ;
  cp  = 0 ;

  while (1)
    {
      char* p, * q, * e ;
      char  action ;

      /* Find next "%...x", if any
       */
      qs_set_cp_nn(qs, cp) ;

      if (qs_find_n(qs, "%", 1) == 0)
        break ;

      p = qs_cp_char_nn(qs) + 1 ;       /* past the '%'         */
      e = qs_ep_char_nn(qs) ;

      cp = qs_cp_nn(qs) + 1 ;           /* default continuation */

      if (p == e)
        break ;                         /* found '%' at end     */

      action = '\0' ;
      q  = esc ;

      if (*p == '%')
        action = '%' ;
      else
        {
          while (1)
            {
              char ch ;

              ch = *p++ ;

              if (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z')))
                {
                  action = ch ;
                  break ;
                } ;

              if ((ch < ' ') || (ch > 0x7E))
                break ;

              if (ch == '%')
                break ;

              if ((q - esc) == qs_pcsub_esc_max)
                break ;

              *q++ = ch ;
            } ;
        } ;

      if (action != '\0')
        {
          if (action == '%')
            {
              /* For "%%" we delete the second '%', and proceed from after
               * the first, which is the default continuation position.
               */
              qs_delete_n(qs, 1) ;
            }
          else
            {
              qs_pcsub_entry_t* se ;
              qstring s ;

              *q = '\0' ;               /* terminate modifiers  */
              s  = NULL ;               /* no substitution, yet */

              se = table ;
              while (se->action != '%')
                {
                  if (se->action != action)
                    {
                      ++se ;
                      continue ;
                    } ;

                  s = se->parser(sub, action, esc, args, se->extra) ;
                  break ;
                } ;

              if (s != NULL)
                {
                  /* Going to make a substitution !
                   *
                   * The qs is positioned at the start of the escape (on '%').
                   *
                   * Our cp points just after the '%', and want to continue
                   * searching after the substitution.
                   */
                  qs_replace(qs, strlen(esc) + 2, s) ;

                  cp += qs_len_nn(s) - 1 ;
                } ;
            } ;
        } ;
    } ;

  /* Arrives here when cannot find (another) percent escape.
   */
  XFREE(MTYPE_TMP, args) ;      /* release argument vector      */
  qs_free(sub) ;                /* release temporary qstring    */

  qs_set_cp_nn(qs, 0) ;         /* to be tidy                   */
} ;

/*------------------------------------------------------------------------------
 * Default pcsub parser -- accepts escape iff there are no modifiers.
 *
 * If accepts escape, return the argument given by the extra parameter as a
 * qstring -- if the extra parameter is out of range, return "???".
 *
 * Returns:  qstring  => OK, substitute
 *           NULL     => not valid, no substitution.
 */
extern qstring
qs_pcsub_default(qstring qs, char action, qs_pcsub_esc_t esc,
                                                  qs_pcsub_args args, int extra)
{
  const char* s ;

  if (*esc != '\0')
    return NULL ;

  s = qs_pcsub_arg(args, extra) ;

  return qs_set_str(qs, (s != NULL) ? s : "???") ;
} ;

/*------------------------------------------------------------------------------
 * Get given entry of given pcsub arguments -- or NULL if index invalid.
 */
extern void*
qs_pcsub_arg(qs_pcsub_args args, int index)
{
  if ((index < 0) || ((uint)index >= args->count))
    return NULL ;
  else
    return args->args[index] ;
} ;


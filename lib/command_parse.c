/* Quagga command line parsing
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Recast and extended: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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
#include <ctype.h>
#include <stdio.h>
#include <arpa/inet.h>

#include "command_local.h"
#include "command_parse.h"
#include "memory.h"

/*==============================================================================
 * Command Description objects.
 *
 */
static void cmd_fail_item(cmd_command cmd, const char* msg) ;
static char* cmd_item_brackets(cmd_command cmd, char* cp) ;
static cmd_item cmd_make_item(cmd_command cmd, char* cp, char* dp) ;
static void cmd_make_item_inner(cmd_command cmd, cmd_item n, char* cp) ;
static char* cmd_make_item_numeric(cmd_command cmd, cmd_item n, char* cp) ;
static long cmd_make_item_number(cmd_command cmd, cmd_item n, char** p_cp) ;
static int cmd_cmp_item(const cmd_item* a, const cmd_item* b) ;
static int cmd_cmp_range_items(const cmd_item a, const cmd_item b) ;
static bool cmd_item_is_option(cmd_item_type_t it) ;
static bool cmd_item_is_vararg(cmd_item_type_t it) ;

/*------------------------------------------------------------------------------
 * Dummy eol_item
 */
static struct cmd_item eol_item =
{
  .str    = "<cr>",
  .doc    = "",

  .next   = NULL,

  .type   = item_eol,
  .arg    = false,

  .range_sign_allowed   = false,
  .range_sign_required  = false,
  .range_min  = 0,
  .range_max  = 0
} ;

/*------------------------------------------------------------------------------
 * Parse cmd_command string and doc to create the items for the cmd_command,
 * and fill in:
 *
 *   cmd->items     -- vector of cmd_item(s).
 *
 *                      Where a given item may have more than one possible
 *                      value, thet are arranged as a list.
 *
 *   cmd->nt_min     -- count of items up to first [option]
 *                         where (...) counts as 1
 *                           and .vararg counts as 1
 *
 *                      Is the minimum number of tokens required to match to
 *                      this command.
 *
 *   cmd->nt        -- count of all items
 *                         where (...) counts as 1
 *                           and [option] counts as 1
 *                           and .vararg counts as 1
 *
 *   cmd->nt_max     -- count of all items as nt_var,
 *                        except .vararg forces to UINT_MAX
 *
 *                      Is the maximum number of tokens which can be matched
 *                      to this command.
 *
 *   cmd->r_string   -- copy of cmd->string, chopped up and referred to by
 *                      the cmd_items.
 *
 *   cmd->d_string   -- copy of the cmd->doc, chopped up and referred to by
 *                      the cmd_items.
 *
 * Note that the cmd_items point into the r_string and the d_string, and
 * do not have further copies of their fragments of the original.
 *
 * Note that the t_string and d_string have all extraneous spaces, tabs and
 * control characters removed.
 *
 * Stops dead if not valid !
 *
 * Accepts: items separated by one or more spaces.  Early on in the process,
 * will discard spaces within a bracketed item (and checks for balanced
 * brackets).  Rejects any control character other than '\t', which is
 * converted to ' '.  Multiple spaces are reduced to single spaces.
 *
 *   - single item is one of:
 *
 *      - keyword    -- anything starting a-z or 0-9, followed by any
 *                      alphanumeric, '-', '_' or ':'.
 *                      or *
 *      - <0-9>      -- decimal range
 *      - WORD       -- anything at least starting A-Z, followed by any
 *                      alphanumeric, '-', '_' or ':'.
 *      - A.B.C.D    -- ipv4 address
 *      - A.B.C.D/M  -- ipv4 prefix
 *      - X:X::X:X   -- ipv6 address
 *      - X:X::X:X/M -- ipv6 prefix
 *      - .vararg    -- anything starting '.', followed by any alphanumeric,
 *                      '-', '_' or ':'.
 *      - [item]     -- optional item any of the above  TODO
 *
 *  - multiple item is: '(' item '|' item .... ')'
 *
 *    where spaces around items are discarded.  The items may be any of the
 *    above, except:
 *
 *      - must all be different types of item, or for keywords and ranges,
 *        different values.
 *
 *      - cannot have [item]
 *
 *      - cannot have .var
 *
 *    may have a single item -- whose value is sent out as an argument.
 *
 * An [item] may only be followed by other [item](s).  An [item] matches a
 * token or end of line.
 *
 * A .vararg must be the last item.  A .vararg matches one or more tokens.
 *
 *
 *
 */
extern void
cmd_compile(cmd_command cmd)
{
  vector multvec ;
  char* cp ;
  char* qp ;
  char* dp ;
  bool  opt ;
  bool  vararg ;

  /* Initialise the compiled version of the command                     */

  assert((cmd->r_doc  == NULL) && (cmd->r_string == NULL)) ;

  cmd->items    = vector_init_new(NULL, 10) ; /* plenty !       */
  cmd->nt_min   = 0 ;
  cmd->nt       = 0 ;
  cmd->nt_max   = 0 ;
  cmd->vararg   = NULL ;
  cmd->r_doc    = XSTRDUP(MTYPE_CMD_STRING, cmd->doc) ;  /* NULL => "" */
  cmd->r_string = XSTRDUP(MTYPE_CMD_STRING, cmd->string) ;

  /* Simplify the command line string by replacing TABs by spaces, and barfing
   * on control characters.  Strip leading and trailing spaces and any spaces
   * between brackets... checking for matching brackets.
   */
  cp = cmd->r_string ;
  while (*cp != '\0')
    {
      if      (!iscntrl(*cp))
        ++cp ;
      else if (*cp == '\t')
        *cp++ = ' ' ;
      else
        cmd_fail_item(cmd, "improper control character in string") ;
    } ;

  cp = cmd->r_string ;
  while (*cp == ' ')
    ++cp ;

  qp = cmd->r_string ;
  while (*cp != '\0')
    {
      if (*cp != ' ')
        {
          if ((*cp == '(') || (*cp == '[') || (*cp == '<') || (*cp == '{'))
            {
              /* Check for balanced brackets and remove any spaces between.
               *
               * Checks for enclosed brackets being balanced as well.
               *
               * Leaves cp pointing at the trailing bracket.
               */
              char* sp = cp ;
              cp = cmd_item_brackets(cmd, cp) ;
              while (sp < cp)
                {
                  if (*sp != ' ')
                    *qp++ = *sp++ ;
                  else
                    ++sp ;
                } ;
            } ;
        }
      else
        {
          while (*(cp + 1) == ' ')
            ++cp ;
          if (*(cp + 1) == '\0')
            break ;
        } ;

      *qp++ = *cp++ ;
    } ;

  *qp++ = '\0' ;        /* terminate reduced string             */

  /* Simplify the documentation string by replacing TABs by spaces, and barfing
   * on control characters other than '\n'.
   *
   * Strips leading spaces and any spaces before or after '\n'.
   */

  qp = dp = cmd->r_doc ;
  while (*dp != '\0')
    {
      /* Strip leading                                          */
      while (*dp == ' ')
        ++dp ;

      /* Eat documentation section.                             */
      while ((*dp != '\n') && (*dp != '\0'))
        {
          if      (!iscntrl(*dp))
            *qp++ = *dp++ ;
          else if (*dp == '\t')
            {
              *qp++ = ' ' ;
              ++dp ;
            }
          else
            cmd_fail_item(cmd, "improper control character in documentation") ;
        } ;

      /* Get here with *dp == '\n' or '\0'
       *
       * Strip trailing spaces (any before '\n' or '\0'
       */
      while ((qp != cmd->r_doc) && (*(qp - 1) == ' '))
        --qp ;

      /* copy '\n', if required.                                */
      if (*dp == '\n')
        *qp++ = *dp++ ;
    } ;

  *qp++ = '\0' ;        /* terminate reduced string             */

  /* Processing loop                                                    */

  cp = cmd->r_string ;
  dp = cmd->r_doc ;

  opt    = false ;
  vararg = false ;

  multvec = NULL ;

  while (*cp != '\0')
    {
      uint   multiple ;

      /* Deal with single or multiple item.                             */
      multiple = 0 ;
      do
        {
          cmd_item n ;
          char* c_sp ;
          char* d_sp ;

          /* step to the next documentation section                     */

          d_sp = dp ;               /* start of documentation       */

          while (*dp != '\0')
            {
              if (*dp == '\n')
                {
                  *dp++ = '\0' ;
                  break ;
                } ;
              ++dp ;
            } ;

          /* Deal with '(' if we have one.                              */

          if (*cp == '(')       /* change up to multiple        */
            {
              if (multiple != 0)
                cmd_fail_item(cmd, "unexpected '('") ;

              multiple = 1 ;    /* seen '('                     */
              ++cp ;            /* step past it                 */

              multvec = vector_re_init(multvec, 10) ; /* plenty !       */
            } ;

          /* Find end of current item & '\0' terminate it.              */
          c_sp = cp ;
          while (1)
            {
              if (*cp == '|')       /* eat '|'                          */
                {
                  if ((c_sp == cp) || (multiple < 1))
                    cmd_fail_item(cmd, "unexpected '|'") ;
                  *cp++ = '\0' ;
                  break ;
                } ;

              if (*cp == ')')  /* eat ')'                          */
                {
                  if ((c_sp == cp) || (multiple < 1))
                    cmd_fail_item(cmd, "unexpected ')'") ;
                  *cp++ = '\0' ;
                  multiple = 2 ;

                  if ((*cp != ' ') && (*cp != '\0'))
                    cmd_fail_item(cmd, "expect ' ' or nothing after ')'") ;
                } ;

              if (*cp == ' ')
                {
                  *cp++ = '\0' ;
                  break ;
                } ;

              if (*cp == '\0')
                break ;

              ++cp ;
            } ;

          /* Create the next item and push                              */

          n = cmd_make_item(cmd, c_sp, d_sp) ;

          if (multiple == 0)
            vector_push_item(cmd->items, n) ;
          else
            vector_push_item(multvec, n) ;

          /* Extra checks for multiple item.                            */
          if (multiple > 0)
            {
              n->arg = true ;           /* always                       */

              if (cmd_item_is_option(n->type))
                cmd_fail_item(cmd, "cannot have [option] inside (..)") ;

              /* could lift this restriction, but need to check that
               * do not have a WORD|.VAR together, because that is tautologous.
               */

              if (cmd_item_is_vararg(n->type))
                cmd_fail_item(cmd, "cannot have .vararg inside (..)") ;
            } ;

          /* Check optional item state -- can only be trailing          */
          if      (cmd_item_is_option(n->type))
            opt = true ;
          else if (opt)
              cmd_fail_item(cmd, "can only have [option] after [option]") ;

          /* Check vararg item state -- can only be trailing            */
          if      (vararg)
            cmd_fail_item(cmd, "cannot have anything after .vararg") ;
          else if (cmd_item_is_vararg(n->type))
            {
              vararg = true ;
              cmd->vararg = n ;         /* remember for parsing */
            } ;

        } while (multiple == 1) ;

      /* count the item                                                 */
      if (!opt)
        ++cmd->nt_min ;
      ++cmd->nt ;
      if (!vararg)
        ++cmd->nt_max ;
      else
        cmd->nt_max = UINT_MAX ;

      /* Complete the multiple item.
       *
       * Sort the items so that are always used and presented in the same
       * order.  Check that the items are unique.
       *
       * We must have at least one item.
       */
      if (multiple == 2)
        {
          cmd_item n, p ;
          uint i ;

          assert(vector_length(multvec) >= 1) ;

          vector_sort(multvec, (vector_sort_cmp*)cmd_cmp_item) ;

          n = vector_get_item(multvec, 0) ;
          vector_push_item(cmd->items, n) ;

          for (i = 1 ; i < vector_length(multvec) ; ++i)
            {
              p = n ;
              n = vector_get_item(multvec, i) ;

              p->next = n ;
              n->next = NULL ;

              if (p->type == n->type)
                {
                  bool repeat ;

                  if      (n->type == item_keyword)
                    repeat = strcmp(n->str, p->str) == 0 ;
                  else if (n->type == item_range)
                    repeat = cmd_cmp_range_items(n, p) == 0 ;
                  else
                    repeat = true ;

                  if (repeat)
                    cmd_fail_item(cmd, "repeated items in (...)") ;
                } ;
            } ;
        }
      else
        assert(multiple == 0) ;
    } ;

  vector_reset(multvec, free_it) ;

  /* Reduce the vector to the minimum size required             */
  vector_decant(cmd->items) ;
} ;

/*------------------------------------------------------------------------------
 * Validate a compiled item
 *
 * Checks that the contents of the cmd_command are consistent with the
 * contents of the srcvec.
 *
 */
extern void
cmd_compile_check(cmd_command cmd)
{
  bool ok ;

  uint nt_min = 0 ;
  uint nt     = 0 ;
  uint nt_max = 0 ;
  cmd_item vararg = NULL ;

  ok = true ;

  /* Require the following to be set to something               */
  if (   (cmd->string    == NULL)
      || (cmd->func      == NULL)
      || (cmd->items     == NULL)
      || (cmd->r_string  == NULL)
      || (cmd->r_doc     == NULL) )
    ok = false ;

  /* cmd->nt must match the vector_length and be non-zero.      */
  ok = ok && ((nt = vector_length(cmd->items)) == cmd->nt) ;
  ok = ok && (nt != 0) ;

  /* Walk the vector of items, and check that those are OK.     */
  if (ok)
    {
      uint ii  = 0 ;
      bool opt = false ;

      for (ii = 0 ; ok && (ii < nt) ; ++ii)
        {
          cmd_item item ;
          cmd_item first_item ;

          item = vector_get_item(cmd->items, ii) ;
          if (item == NULL)
            {
              ok = false ;
              break ;
            } ;

          if (vararg != NULL)           /* nothing after vararg         */
            {
              ok = false ;
              break ;
            } ;

          first_item = item ;
          while (ok && (item != NULL))
            {
              /* If this is an option, may only be a single item
               *
               * Otherwise, after an option must all be options.
               */
              if      (cmd_item_is_option(item->type))
                {
                  /* option must be a single item.                      */
                  opt = true ;
                  if ((item != first_item) || (item->next != NULL))
                    {
                      ok = false ;
                      break ;
                    } ;
                }
              else if (opt)
                {
                  /* once we have an option, must all be options        */
                  ok = false ;
                  break ;
                } ;

              /* If this is a vararg, must be the last of this item.
               *
               * Note that allow for [.varg] and (...., .varg) -- the second
               * should be sorted to the back !
               */
              if      (cmd_item_is_vararg(item->type))
                {
                  /* vararg must be last item & only vararg             */
                  if ((item->next != NULL) || (vararg != NULL))
                    {
                      ok = false ;
                      break ;
                    } ;

                  vararg = item ;
                } ;


              /* If there is a next, this and the next MUST be arg      */
              if (item->next != NULL)
                {
                  if (!((item->arg) && (item->next->arg)))
                    {
                      ok = false ;
                      break ;
                    } ;
                } ;

              item = item->next ;
            } ;

          /* Advance the nt_min and nt_max as required.                 */
          if (!opt)
            ++nt_min ;

          if (vararg == NULL)
            ++nt_max ;
          else
            nt_max = UINT_MAX ;
        } ;
    } ;

  /* Final checks                                                       */

  ok = ok && (cmd->nt_min  == nt_min)
          && (cmd->nt      == nt)
          && (cmd->nt_max  == nt_max)
          && (cmd->vararg  == vararg) ;

  if (!ok)
    cmd_fail_item(cmd, "some compile error") ;
} ;

/*------------------------------------------------------------------------------
 * Reject the cmd_item string or doc.
 */
static void
cmd_fail_item(cmd_command cmd, const char* msg)
{
  fprintf (stderr, "Command parse error!: %s\n", msg) ;
  fprintf (stderr, "  in command: '%s'\n", cmd->string) ;
  exit(2) ;
}

/*------------------------------------------------------------------------------
 * Advance to matching bracket -- fail if not found.  Recurse as required.
 *
 * Returns:  address of matching bracket.
 */
static char*
cmd_item_brackets(cmd_command cmd, char* cp)
{
  char seek ;

  switch (*cp)
  {
    case '(':
      seek = ')' ;
      break ;

    case '[':
      seek = ']' ;
      break ;

    case '<':
      seek = '>' ;
      break ;

    case '{':
      seek = '}' ;
      break ;

    default:
      return cp ;
  } ;

  do
    {
      ++cp ;

      if      (*cp == seek)
        return cp ;
      else if ((*cp == '(') || (*cp == '[') || (*cp == '<') || (*cp == '{'))
        cp = cmd_item_brackets(cmd, cp) ;
    }
  while (*cp != '\0') ;

  cmd_fail_item(cmd, "unbalanced brackets of some sort") ;

  return cp ;
} ;

/*------------------------------------------------------------------------------
 * Make descriptor for current item.
 *
 * cp points at start of '\0' terminated item, which has no spaces and no
 * control characters in or around it.  Also, if there are brackets, they are
 * balanced.
 *
 * Returns:  new descriptor, filled in as required.
 */

static cmd_item
cmd_make_item(cmd_command cmd, char* cp, char* dp)
{
  char* inner ;
  cmd_item  n ;

  n = XCALLOC(MTYPE_CMD_ITEM, sizeof(struct cmd_item)) ;

  /* Zeroising has set:
   *
   *   * cmd     = NULL        -- set below
   *   * doc     = NULL        -- set below
   *
   *   * next    = NULL        -- set elsewhere if multiple.
   *
   *   * type    = item_null
   *   * arg     = false       -- set if required, below
   *
   *   * range_sign_allowed  )
   *   * range_sign_required ) -- set elsewhere if required
   *   * range_min           )
   *   * range_max           )
   */
  confirm(item_null == 0) ;

  n->str  = cp ;
  n->doc  = dp ;

  /* Worry about option state                                           */
  inner = NULL ;
  if (*cp == '[')
    {
      n->arg = true ;                   /* always true for option       */

      inner = XSTRDUP(MTYPE_TMP, cp) ;
      cp = inner + 1 ;                  /* strip leading  '['   */
      *(cp + strlen(cp) - 1) = '\0' ;   /* strip trailing ']'   */

      if (*cp == '\0')
        cmd_fail_item(cmd, "empty [option]") ;
    } ;

  /* Deal with the inner item                                           */
  cmd_make_item_inner(cmd, n, cp) ;

  /* Worry about the option state, again.                               */
  if (inner != NULL)
    {
      XFREE(MTYPE_TMP, inner) ;

      if (n->type == item_vararg)
        cmd_fail_item(cmd, "cannot have [.vararg]") ;

      n->type = item_option_word ;      /* TODO other option types ?    */
    } ;

  /* return newly minted cmd_item item                                      */
  assert(n->type != item_null) ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Make inner part of cmd_item -- so can have [...] anything (in principle).
 *
 * Require '\0' terminated inner part.
 */
static void
cmd_make_item_inner(cmd_command cmd, cmd_item n, char* cp)
{
  bool  eat_name_chars ;        /* alphanumeric + '-', '_', '.' and ':' */

  eat_name_chars = false ;

  if      (islower(*cp)                 /* 'a'..'z'                     */
            || isdigit(*cp))            /* '0'..'9'                     */
    {
      /* item_keyword -- lowercase alpha numeric + '_' and '-'  */
      n->type = item_keyword ;
      eat_name_chars = true ;
    }
  else if (*cp == '*')                  /* '*'                          */
    {
      /* special item_keyword '*'                               */
      n->type = item_keyword ;
      ++cp ;
    }
  else if (isupper(*cp))                /* 'A'..'Z'                     */
    {
      n->arg = true ;

      /* WORD or other variable                                 */
      if      (strcmp(cp, "A.B.C.D")    == 0)
        n->type = item_ipv4_address ;
      else if (strcmp(cp, "A.B.C.D/M")  == 0)
        n->type = item_ipv4_prefix ;
      else if (strcmp(cp, "X:X::X:X")   == 0)
        n->type = item_ipv6_address ;
      else if (strcmp(cp, "X:X::X:X/M") == 0)
        n->type = item_ipv6_prefix ;
      else
        {
          n->type = item_word ;
          eat_name_chars = true ;
        } ;

      if (n->type != item_word)
        cp += strlen(cp) ;              /* step past "A.B.C.D" et al    */
    }
  else if (*cp == '.')                  /* '.'                          */
    {
      n->arg  = true ;
      n->type = item_vararg ;
      eat_name_chars = true ;
    }
  else if (*cp == '<')                  /* '<'                          */
    {
      n->arg  = true ;

      cp = cmd_make_item_numeric(cmd, n, ++cp) ;

      if (*cp != '>')
        cmd_fail_item(cmd, "badly formed <...>") ;
      else
        ++cp ;
    }
  else if (*cp == '\0')
    cmd_fail_item(cmd, "cannot have an empty item") ;

  if (eat_name_chars)
    {
      do ++cp ; while (   isalnum(*cp)
                       || (*cp == '-')
                       || (*cp == '_')
                       || (*cp == ':')
                       || (*cp == '.') ) ;

    } ;

  if (*cp != '\0')
    cmd_fail_item(cmd, "invalid item") ;
} ;

/*------------------------------------------------------------------------------
 * Make <...> types
 *
 * Require '\0' terminated <...>, pointing after the '<'.
 *
 * Assumes the '>' is present.
 *
 * Returns: where processed up to -- pointing at '>' iff OK.
 *
 * Supports ranges:
 *
 *    9-10   => unsigned values in 32b range -- NO sign
 *
 *   +9-10   => unsigned value, where '+' is optional
 *    9-+10  => unsigned value, where '+' is *required*
 *   +9-+10  => same as above
 *
 *   -9-10   => signed value, where '+' is optional
 *   -9-+10  => signed value, where '+' is *required*
 *
 *   -9--8   => signed value, where '-' is required (!)
 *
 *
 *   +/-9    => -9-+9 -- sign is required.
 *
 * In place of decimal number can use 9b -- giving 2^9 - 1.
 */
static char*
cmd_make_item_numeric(cmd_command cmd, cmd_item n, char* cp)
{
  if (isdigit(*cp) || (*cp == '+') || (*cp == '-'))
    {
      long  m ;
      bool  pm ;

      confirm((LONG_MAX > item_max_number) && (LONG_MIN < -item_max_number)) ;

      n->type                = item_range ;
      n->range_sign_allowed  = false ;
      n->range_sign_required = false ;

      if (strncmp(cp, "+/-", 3) == 0)
        {
          pm = true ;
          n->range_sign_required = true ;
          cp += 2 ;     /* step to '-' to get -ve range_min.    */
        }
      else
        {
          pm = false ;
          n->range_sign_allowed  = (*cp == '+') ;
        } ;

      m = cmd_make_item_number(cmd, n, &cp) ;
      n->range_min = m ;

      if (pm)
        m = -m ;                /* for range_max        */

      else if (*cp == '-')
        {
          ++cp ;                /* past the '-'         */

          n->range_sign_required = (*cp == '+') ;

          m = cmd_make_item_number(cmd, n, &cp) ;
        }

      else
        cmd_fail_item(cmd, "badly formed <0-1>") ;

      n->range_max = m ;

      if (n->range_min > n->range_max)
        cmd_fail_item(cmd, "badly formed <0-1> min > max !") ;

      if ((n->range_sign_required) || (n->range_min < 0))
        n->range_sign_allowed  = true ; /* allowed if required !        */
    } ;

  return cp ;
} ;

/*------------------------------------------------------------------------------
 * Get signed or unsigned value -- process the '9b' form.
 *
 */
static long
cmd_make_item_number(cmd_command cmd, cmd_item n, char** p_cp)
{
  long  m ;
  char* cp ;

  cp = *p_cp ;
  m = strtol(cp, p_cp, 10) ;

  if ((*p_cp == cp) || (m > item_max_number))
    cmd_fail_item(cmd, "badly formed or out of range number in <...>") ;

  if (**p_cp == 'b')
    {
      long s ;

      ++(*p_cp) ;       /* step past 'b'        */
      s = m ;
      m = labs(m) ;
      if ((m == 0) || (m > 32))
        cmd_fail_item(cmd, "out of range number in 9b form in <...>") ;

      m = ((long)1 << m) - 1 ;
      if (s < 0)
        m = -m ;
    } ;

  return m ;
} ;

/*------------------------------------------------------------------------------
 * Compare cmd_item items
 *
 * Note that command types sort with the larger type value before the smaller.
 */
static int
cmd_cmp_item(const cmd_item* a, const cmd_item* b)
{
  if ((*a)->type != (*b)->type)
    return ((*a)->type > (*b)->type) ? -1 : +1 ;  /* descending order   */
  else
    {
      if ((*a)->type == item_range)
        return cmd_cmp_range_items(*a, *b) ;
      else
        return strcmp ((*a)->str, (*b)->str);
    } ;
} ;

/*------------------------------------------------------------------------------
 * Compare cmd_item item_range items
 */
static int
cmd_cmp_range_items(const cmd_item a, const cmd_item b)
{
  int as, bs ;

  if (a->range_min != b->range_min)
    return (a->range_min < b->range_min) ? -1 : +1 ;

  if (a->range_max != b->range_max)
    return (a->range_max < b->range_max) ? -1 : +1 ;

  as = a->range_sign_required ? 2 : (a->range_sign_allowed ? 1 : 0) ;
  bs = b->range_sign_required ? 2 : (b->range_sign_allowed ? 1 : 0) ;

  if (as != bs)
    return (as < bs) ? -1 : +1 ;

  return 0 ;
} ;

/*==============================================================================
 * Token objects
 */

/*------------------------------------------------------------------------------
 * Make a brand new token object
 */
Private cmd_token
cmd_token_new(void)
{
  return XCALLOC(MTYPE_TOKEN, sizeof(struct cmd_token)) ;

  /* Zeroising the new structure sets:
   *
   *   type       = 0    -- cmd_tok_eol
   *   qs         = zeroised qstring  -- empty string
   *   complete   = 0 -- false
   *
   *   tp         = 0
   *   lp         = zeroised elstring -- empty string
   */
  confirm(cmd_tok_eol == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;
  confirm(ELSTRING_INIT_ALL_ZEROS) ;
} ;

/*------------------------------------------------------------------------------
 * Empty token object and free it.
 */
static void
cmd_token_free(cmd_token t)
{
  qs_reset(t->qs, keep_it) ;            /* discard body of qstring      */
  XFREE(MTYPE_TOKEN, t) ;
} ;

/*==============================================================================
 * Parser object
 */

/*------------------------------------------------------------------------------
 * Initialise a new cmd_parsed object, allocating if required
 */
extern cmd_parsed
cmd_parsed_init_new(cmd_parsed parsed)
{
  if (parsed == NULL)
    parsed = XCALLOC(MTYPE_CMD_PARSED, sizeof(*parsed)) ;
  else
    memset(parsed, 0, sizeof(*parsed)) ;

  /* Zeroising the structure has set:
   *
   *   parts       = 0          -- cleared by cmd_tokenise()
   *   tok_total   = 0          -- set by cmd_tokenise()
   *
   *   elen        = 0          -- set by cmd_tokenise()
   *   tsp         = 0          -- set by cmd_tokenise()
   *
   *   cmd         = NULL       -- no command yet
   *   cnode       = 0          -- not set
   *
   *   num_tokens  = 0          -- set by cmd_tokenise()
   *   tokens      = all zeros  -- empty token vector
   *
   *   args        = all zeros  -- empty vector of arguments
   *
   *   emess       = NULL       -- no error yet
   *   eloc        = 0          -- no error location
   *
   *   in_pipe     = cmd_pipe_none
   *   out_pipe    = cmd_pipe_none
   *
   *   first_in_pipe  )
   *   num_in_pipe    )
   *   first_do       )
   *   num_do         )
   *   first_command  )  none
   *   num_command    )
   *   first_out_pipe )
   *   num_out_pipe   )
   *   first_comment  )
   *   num_comment    )
   *
   *   cti            ) set by cmd_token_position()
   *   rp             )
   *
   *   cmd_v       = all zeros  -- empty vector of filtered commands
   *   item_v      = all zeros  -- empty vector of filtered items
   *
   *   strongest      )
   *   best_complete  ) set by cmd_filter_prepare()
   *   min_strength   )
   *   strict         )
   */
  confirm(cmd_pipe_none == 0) ;
  confirm(TOKEN_VECTOR_INIT_ALL_ZEROS) ;
  confirm(ARG_VECTOR_INIT_ALL_ZEROS) ;

  return parsed ;
} ;

/*------------------------------------------------------------------------------
 * Empty out and (if required) free a cmd_parsed object
 */
extern cmd_parsed
cmd_parsed_reset(cmd_parsed parsed, free_keep_b free_structure)
{
  if (parsed != NULL)
    {
      cmd_token t ;

      /* Give back all the token objects and release vector body        */
      while ((t = vector_ream(parsed->tokens->body, keep_it)) != NULL)
        cmd_token_free(t) ;

      vector_reset(parsed->args->body, keep_it) ;   /* embedded */
      vector_reset(parsed->cmd_v, keep_it) ;        /* embedded */
      vector_reset(parsed->item_v, keep_it) ;       /* embedded */

      if (free_structure)
        XFREE(MTYPE_CMD_PARSED, parsed) ;   /* sets parsed = NULL       */
      else
        cmd_parsed_init_new(parsed) ;
    } ;

  return parsed ;
} ;

/*==============================================================================
 * Parsing error handling.
 *
 *
 */

/*------------------------------------------------------------------------------
 * Register a parsing error.
 *
 * Takes token in which parsing error was detected, and an offset from the
 * start of that, for the location of the error.  If the offset is not zero,
 * it must be an offset in the original token (!).
 *
 * The message is a simple constant string (!).
 *
 * The message will be output as: ..........^ pointing to the location
 *                   followed by: % <mess>\n
 *
 * (The mess does not need to include the '%' or the '\n'.)
 *
 * Sets:  parsed->emess
 *        parsed->eloc
 *
 * Returns: CMD_ERR_PARSING -- which MUST only be returned if p
 */
static cmd_return_code_t
cmd_parse_error(cmd_parsed parsed, cmd_token t, usize off, const char* mess)
{
  parsed->emess  = mess ;
  parsed->eloc   = t->tp + off ;

  return CMD_ERR_PARSING ;
} ;

/*==============================================================================
 * Lexical level stuff
 */

/*------------------------------------------------------------------------------
 * Take elstring and see if it is empty -- only whitespace and/or comment
 */
extern bool
cmd_is_empty(elstring line)
{
  cpp_t lp ;

  els_cpp(lp, line) ;                   /* NULL -> NULL                 */

  while (lp->p < lp->e)
    {
      if ((*lp->p == ' ') || (*lp->p == '\t'))
        ++lp->p ;
      else
        return ((*lp->p == '!') || (*lp->p == '#')) ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Reserved characters in a pipe token.
 *
 * The characters reserved allow for a large number of possible options to
 * be attached to the pipe token.
 *
 * Wish to allow the pipe token to be followed by file name or command name
 * without requiring whitespace separation, also do not want to intrude into
 * quoted or escaped stuff.  So limit the characters that are reserved.
 */
static inline bool
cmd_pipe_reserved_char(char ch)
{
  return strchr("<|>%&*+-=?", ch) != NULL ;
} ;

/*------------------------------------------------------------------------------
 * Take elstring and break it into tokens.
 *
 * Discards leading and trailing ' ' or '\t'.
 *
 * Expects string to have been preprocessed, if required, to ensure that any
 * unwanted control characters have been removed.  This code only recognises
 * '\t' and treats it as whitespace.
 *
 * Anything between '....' is ignored by the tokenizer.  NB: this follows the
 * shell convention, so '\' is also ignored and there is no way to include "'"
 * in a single quoted string.
 *
 * Anything immediately preceded by '\' is ignored by the tokenizer.  This
 * includes blanks and quotes.
 *
 * Anything inside "...." is ignored by the tokenizer, including '\"' escapes.
 *
 * Unbalanced "'" or '"' are treated as if eol was a "'" or '"'.
 *
 * Of the things which are not ignored by the tokenizer:
 *
 *   * tokens are separated by whitespace -- one ' ' or '\t' characters
 *     The whitespace is discarded.
 *
 *   * tokens which start with any of:
 *
 *       '!', '#', '<' and '>'
 *
 *     terminate themselves, as follows:
 *
 *       - from '!' or '#' to end of line is a comment token.
 *
 *       - '<' followed by pipe_reserved_chars is a token (in_pipe)
 *
 *       - '>' followed by pipe_reserved_chars is a token (out_pipe).
 *
 *     See above for pipe_reserved_
 *
 * NB: this means that for '!', '#', '<' and '>' to be significant, they
 *     MUST be preceeded by whitespace (or start of line).  This ever so
 *     slightly reduces the impact of the new lexical conventions.
 *
 * NB: the tokenization roughly mimics the (POSIX) standard shell.  The
 *     differences are:
 *
 *        '|' is *not* a pipe ('>|' is), because '|' is a character in the
 *        regex repertoire.
 *
 *        '<' and '>' do not terminate a token -- so are only significant
 *        at the start of a token.
 *
 *        '!' is not a comment for the shell.
 *
 *     The requirement for whitespace (or start of line) before '#' is
 *     consistent with the shell.
 *
 *     The handling of '...' follows the standard shell.
 *
 *     The tokenization does not remove any " ' or \ characters, that is left
 *     for a later stage, where context may affect the handling.
 *
 * NB: any control characters other than '\t' are accepted as part of the
 *     current token !
 *
 * The tokens returned contain all the original characters of the line, except
 * for the removal of ' ' and '\t' between tokens and at the end of the line.
 *
 * Note: all the tokens in the vector have at least one character, and no
 *       entries are NULL.
 *
 * NB: it is the callers responsibility to release the token objects in due
 *     course.
 *
 * NB: the elstring containing the line to be tokenised MUST NOT change
 *     until the parsed object is finished with.
 *
 * Returns: the types of all tokens or'd together.
 *          returns cmd_tok_null if the line is empty (apart from ' ' and '\t')
 *                               or if the elstring was or contained NULL.
 *
 * Initialises the parsed object, ready for further parsing:
 *
 * Sets: parsed->parts      = cmd_parts_none
 *       parsed->num_tokens )
 *       parsed->elen       ) per the results
 *       parsed->tsp        )
 *       parsed->tokens     )
 *
 * Note that the num_tokens does not include the cmd_tok_eol on the end.
 */
extern void
cmd_tokenise(cmd_parsed parsed, qstring line)
{
  cpp_t lp ;
  const char *cp, *tp ;
  cmd_token_type_t total ;
  uint nt ;

  total = 0 ;                           /* nothing yet                  */
  nt    = 0 ;

  qs_cpp(lp, line) ;                    /* NULL -> NULL                 */

  cp = lp->p ;
  tp = cp ;
  while (cp < lp->e)                     /* process to end               */
    {
      const char* sp ;
      bool end ;
      cmd_token_type_t type ;

      if ((*cp == ' ') || (*cp == '\t'))
        {
          /* skip white-space                                           */
          do
            {
              end = (++cp == lp->e) ;
            } while (!end && ((*cp == ' ') || (*cp == '\t'))) ;

          if (end)
            break ;
        } ;

      end  = false ;
      sp   = cp ;
      type = cmd_tok_simple ;
      do
        {
          switch (*cp)
          {
            case '\t':          /* whitespace at end of token           */
            case ' ':
              end = true ;
              break ;

            case '\'':          /* proceed to matching ' or end         */
              ++cp ;
              type |= cmd_tok_sq ;
              while (cp < lp->e)
                {
                  if (*cp++ == '\'')
                    break ;
                } ;
              break ;

            case '\\':          /* step past escaped character, if any  */
              ++cp ;
              type |= cmd_tok_esc ;
              if (cp < lp->e)
                ++cp ;
              break ;

            case '"':           /* proceed to matching " or end...      */
              ++cp ;
              type |= cmd_tok_dq ;
              while (cp < lp->e)  /* NB: do not register \ separately   */
                {
                  if (*cp++ == '"')
                    if (*(cp - 2) != '\\')    /* ignore escaped "       */
                      break ;
                } ;
              break ;

            case '>':           /* '>' special at start                 */
              end = (cp == sp) ;
              ++cp ;
              if (end)          /* if special                           */
                {
                  type = cmd_tok_out_pipe ;
                  while ((cp < lp->e) && cmd_pipe_reserved_char(*cp))
                    ++cp ;
                } ;
              break ;

            case '<':           /* '<' special at start                 */
              end = (cp == sp) ;
              ++cp ;
              if (end)          /* if special                           */
                {
                  type = cmd_tok_in_pipe ;
                  while ((cp < lp->e) && cmd_pipe_reserved_char(*cp))
                    ++cp ;
                } ;
              break ;

            case '!':           /* '!' and '#' special at start         */
            case '#':
              if ((cp == sp) && (nt == 0))
                {
                  end = true ;
                  type = cmd_tok_comment ;
                  cp = lp->e ;
                }
              else
                ++cp ;
              break ;

            default:
              ++cp ;
              break ;
          } ;
        } while (!end && (cp < lp->e)) ;

      cmd_token_set(parsed->tokens, nt, type, sp, cp - sp, sp - lp->p) ;
      ++nt ;
      total |= type ;

      tp = cp ;
    } ;

  /* When we get here, tp points just after last character of last token,
   * or at start of line if the line is blank (other than whitespace).
   *
   * If line is empty (apart from whitespace):
   *
   *  - the effective length is zero.
   *  - the trailing space count is the number of (leading) spaces.
   *
   * If line is not empty:
   *
   *  - the start position of the first token is the number of leading spaces.
   *  - the trailing space count is the number of spaces after the last
   *    token.  (If the last token is comment, this will be zero.)
   */
  parsed->parts      = cmd_parts_none ;
  parsed->tok_total  = total ;
  parsed->num_tokens = nt ;
  parsed->elen       = tp - lp->p ;
  parsed->tsp        = lp->e - tp ;

  /* Append an empty end of line token.                                 */
  cmd_token_set(parsed->tokens, parsed->num_tokens, cmd_tok_eol,
                                                      lp->e, 0, lp->e - lp->p) ;
} ;

/*------------------------------------------------------------------------------
 * Process in-pipe token and set the required bits in the pipe type word
 *
 * Known tokens are:  <  <|  <+  <|+
 */
static cmd_return_code_t
cmd_parse_in_pipe(cmd_parsed parsed, cmd_token t)
{
  cpp_t p ;
  bool  ok ;

  els_cpp(p, t->ot) ;

  ok = ((p->p < p->e) && (*p->p++ == '<')) ;

  if (ok)
    {
      /* First character after '<' may qualify the type of the pipe     */
      parsed->in_pipe = cmd_pipe_file ;

      if (p->p < p->e)
        {
          switch (*p->p++)
          {
            case '|':
              parsed->in_pipe  = cmd_pipe_shell ;
              break ;

            default:
              --p->p ;          /* put back     */
              break ;
          }
        } ;

      /* Deal with option characters                                    */
      while (ok && (p->p < p->e))
        {
          /* Eat option character, if recognise it.                     */
          switch (*p->p++)
          {
            case '+':                   /* reflect command lines        */
              parsed->in_pipe |= cmd_pipe_reflect ;
              break ;

            default:
              --p->p ;
              ok = false ;
              break ;
          } ;
        } ;
    } ;

  if (!ok)
    return cmd_parse_error(parsed, t, 0, "invalid 'pipe in'") ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Process out-pipe token and set the required bits in the pipe type word
 *
 * Known tokens are:  >  >>  >| >*
 */
static cmd_return_code_t
cmd_parse_out_pipe(cmd_parsed parsed, cmd_token t)
{
  cpp_t p ;
  bool  ok ;

  els_cpp(p, t->ot) ;

  ok = ((p->p < p->e) && (*p->p++ == '>')) ;

  if (ok)
    {
      /* First character after '>' may qualify the type of the pipe     */
      parsed->out_pipe = cmd_pipe_file ;

      if (p->p < p->e)
        {
          switch (*p->p++)
          {
            case '>':
              parsed->out_pipe |= cmd_pipe_append ;
              break ;

            case '|':
              parsed->out_pipe  = cmd_pipe_shell ;
              break ;

            case '*':
              parsed->out_pipe  = cmd_pipe_dev_null ;
              break ;

            default:
              --p->p ;          /* put back     */
              break ;
          }
        } ;

      /* Could now have options, but presently do not                   */
      if (p->p < p->e)
        {
          ok = false ;
        } ;
    } ;

  if (!ok)
    return cmd_parse_error(parsed, t, 0, "invalid 'pipe out'") ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * If token is incomplete make a copy of it and '\0' terminate.
 *
 * If if contains quotes or escapes process those down.
 *
 * For Quagga purposes, the following is done:
 *
 *   inside '...':   all characters stand for themselves, except '\t' -> ' ',
 *                   and, of course, the terminating '.
 *
 *                   This is just like the shell.
 *
 *   inside "...":   all characters stand for themselves, except '\t' -> ' ',
 *                   and, of course, the terminating ", plus the following
 *                   \x escapes are processed:
 *
 *                     \"   -> "   -- so can have " in "...."
 *                     \\   -> \   -- so can have \ in "...."
 *                     \$   -> $   -- so can have $ in "...."
 *
 *   outside quotes: all characters stand for themselves, except:
 *
 *                     \sp  -> sp  -- so can escape the odd space (cf shell)
 *                     \tab -> sp  -- ditto of tab, but map to space
 *                     \?   -> ?   )
 *                     \'   -> '   )
 *                     \"   -> "   )  so can escape the odd meta character
 *                     \<   -> <   )  where required or to taste.
 *                     \>   -> >   )
 *                     \!   -> !   )
 *                     \#   -> \#  )
 *
 * NB: with the exception of $ inside of "..." the carefully avoids the
 *     regex meta characters: .*+?^$_  and (|)[-]
 *     and the regex escaped forms of those and the back reference \1 etc.
 *
 *     $ in "..." is reserved for future use as a variable or other
 *     substitution start.
 *
 * Returns: CMD_SUCCESS   <=> OK
 *          CMD_ERR_PARSE <=> invalid escape or incomplete quotes
 *
 * NB: if fails, returns token completed as far as possible.
 */
static cmd_return_code_t
cmd_token_complete(cmd_parsed parsed, cmd_token t)
{
  cpp_t p ;
  pp_t  q ;
  bool  dq ;
  cmd_return_code_t ret ;

  if ((t->type & cmd_tok_incomplete) == 0)
    return CMD_SUCCESS ;        /* Quick out if nothing to do   */

  /* To process quotes etc works from the elstring that points to
   * the original line, to the qstring.
   *
   * For quotes and escapes, the result is always no longer than the
   * original.
   */
  els_cpp_nn(p, t->ot) ;                /* original token               */

  qs_new_size(t->qs, p->e - p->p) ;     /* discard alias & set qs to be
                                           big enough for original      */
  qs_pp_nn(q, t->qs) ;                  /* where to complete token to   */

  ret = CMD_SUCCESS ;
  dq = false ;
  while (p->p < p->e)
    {
      switch (*p->p)
      {
        case '\t':
          *q->p++ = ' ' ;               /* '\t' -> ' '          */
          ++p->p ;
          break ;

        case '\'':
          ++p->p ;                      /* skip leading '       */
          while (1)
            {
              if (p->p == p->e)
                {
                  ret = cmd_parse_error(parsed, t, 0, "missing closing '") ;
                  break ;
                } ;

              if (*p->p == '\'')
                {
                  ++p->p ;              /* skip trailing '      */
                  break ;               /* done '....'          */
                } ;

              if (*p->p == '\t')
                {
                  *q->p++ = ' ' ;       /* '\t' -> ' '          */
                  ++p->p ;
                }
              else
                *q->p++ = *p->p++ ;     /* rest as is           */
            } ;
          break ;

        case '"':
          ++p->p ;                      /* skip "               */
          dq = !dq ;                    /* switch state         */
          break ;

        case '\\':
          *q->p++ = *p->p++ ;           /* copy the \           */
          if (p->p == p->e)
            ret = cmd_parse_error(parsed, t, 0, "trailing \\") ;
          else
            {
              if (dq)
                {
                  /* inside "...": \", \$ and \\ only       */
                  if ((*p->p == '"') || (*p->p == '$') || (*p->p == '\\'))
                    --q->p ;            /* strip the \          */
                }
              else
                {
                  /* outside quotes: \sp \tab \< \> \! \#   */
                  if ( (*p->p == '\t') || (*p->p == ' ') ||
                       (*p->p == '\'') || (*p->p == '"') ||
                       (*p->p == '<')  || (*p->p == '>') ||
                       (*p->p == '!')  || (*p->p == '#') )
                    --q->p ;            /* strip the \          */
                } ;

              if (*p->p != '\t')
                *q->p++ = *p->p++ ;
              else
                {
                  *q->p++ = ' ' ;
                  ++p->p ;
                } ;
            } ;
            break ;

        default:
          *q->p++ = *p->p++ ;
          break ;
      } ;
    } ;

  if (dq)
    ret = cmd_parse_error(parsed, t, 0, "missing closing \"") ;

  if (ret == CMD_SUCCESS)
    {
      *q->p = '\0' ;                    /* '\0' terminate       */
      qs_set_len_nn(t->qs, q->p - qs_char_nn(t->qs)) ;
      t->term = true ;
    }
  else
    {
      qs_set_alias_els(t->qs, t->ot) ;
    } ;

  return ret ;
}

/*------------------------------------------------------------------------------
 * Tokenise the given line and work out where cursor is wrt tokens.
 *
 * Looks for first token whose end is at or beyond the cursor position. Note
 * that:
 *
 *   * where the cursor is just after the last character of a token, it is at
 *     the end of that token.
 *
 *   * where there is more than one space between tokens, and the cursor is
 *     after the first space, then it is deemed to be "on" the second token.
 *
 *       - if there are spaces at the start of the line and the cursor is on
 *         one of those spaces, the it is "on" the first token.
 *
 *       - if the line is blank, with zero or more spaces, the cursor is "on"
 *         the eol token.
 *
 * Note that where a line ends in a comment, there are no trailing spaces.
 *
 * Returns: true <=> "in a special place"
 *
 * Is "in a special place" if the cursor is:
 *
 *   a. in a quoted string of any type
 *   b. in an escape (so immediately after a '\')
 *   c. after the '!' or '#' on a line which consists only of a comment
 *   d. after the first '<' or '>' of the first pipe token on the line
 *
 * If NOT "in a special place", will set:
 *
 *   * parsed->cti -- cursor token index
 *   * parsed->rp  -- cursor position relative to the start of token
 *
 * Note that the cursor token may be a comment or pipe token, or the eol token
 * at the end of the line.
 */
extern bool
cmd_token_position(cmd_parsed parsed, qstring line)
{
  cmd_token t ;
  uint      ti ;
  uint      cp, ep ;

  cpp_t       p ;
  const char* q ;
  const char* e ;
  bool sq, dq, bs ;

  /* Re-initialise parsed object and tokenise the given line            */
  cmd_tokenise(parsed, line) ;

  /* Get the cursor position                                            */
  cp = qs_cp_nn(line) ;

  /* Look for the last token whose end is <= cp
   *
   * Will position on last, "eol" token -- which is not counted in
   * parsed->num_tokens -- if is beyond the last real token on the line.
   */
  t  = NULL ;
  ti = 0 ;
  while (1)
    {
      t  = cmd_token_get(parsed->tokens, ti) ;

      if (cp > t->tp)
        {
          /* As soon as we are past '<', '>', '!' or '#' -- return "special" */
          if ((t->type & ( cmd_tok_in_pipe
                         | cmd_tok_out_pipe | cmd_tok_comment) ) != 0)
            return true ;
        } ;

      ep = t->tp + els_len_nn(t->ot) ;

      if ((cp <= ep) || (ti == parsed->num_tokens))
        break ;         /* stop when found token (or at eol)    */

      ++ti ;
    } ;

  parsed->cti = ti ;
  parsed->ctl = els_len_nn(t->ot) ;
  parsed->rp  = (int)cp - (int)t->tp ;

  /* Arrive with t = token in which cp belongs
   *
   * If the token is incomplete, then need to check for "ins" -- unless is
   * already "ins".
   */
  if ((t->type & cmd_tok_incomplete) == 0)
    return false ;

  /* Scan to see if in '...' or "..."  or after '\'.            */

  els_cpp_nn(p, t->ot) ;        /* original token               */

  q = p->p + (cp - t->tp) ;     /* position interested in       */
  assert(q > p->p) ;

  dq = false ;
  sq = false ;
  bs = false ;

  e = (q <= p->e) ? q : p->e ;  /* stop at q or end of token    */

  while (p->p < e)
    {
      switch (*p->p)
      {
        case '\'':
          if (!dq && !bs)       /* ignore ' inside "..." & after \      */
            sq = !sq ;
          break ;

        case '\"':              /* ignore " inside '...' & after \      */
          if (!sq && !bs)
            dq = !dq ;
          break ;

        case '\\':
          if (!sq)              /* ignore \ inside '...'                */
            bs = !bs ;          /* cope with \\                         */
          break ;

        default:
          break ;
      } ;

      ++p->p ;
    } ;

  /* Have scanned to but excluding current character or end of token, whichever
   * came first.
   *
   * If in '...' or "...", then is "ins"
   * If is immediately after \, then is "ins".
   */
  return sq || dq || (bs && (p->e <= q)) ;
} ;

/*==============================================================================
 * Match functions.
 *
 * Is the given string a, possibly incomplete, value of the required kind ?
 */

static match_strength_t cmd_ipv4_match(const char* cp, uint prefix) ;
static match_strength_t cmd_prefix_match(const char* cp, uint prefix) ;

/*------------------------------------------------------------------------------
 * Is this an IPv4 Address
 *
 *   999.999.999.999  -- each 0..255, no leading zeros, decimal only.
 *
 * Returns: mt_no_match              -- improperly formed
 *          mt_ipv4_address_partial  -- OK as far as it goes (or empty)
 *          mt_ipv4_address_complete -- syntactically complete
 */
static match_type_t
cmd_ipv4_address_match(cmd_token t)
{
  match_strength_t ms ;

  ms = cmd_ipv4_match(cmd_token_make_string(t), 0) ;

  if (ms == ms_var_complete)
    return mt_ipv4_address_complete ;
  if (ms == ms_partial)
    return mt_ipv4_address_partial ;

  return mt_no_match ;
} ;

/*------------------------------------------------------------------------------
 * Is this an IPv4 Prefix
 *
 *   999.999.999.999/99  -- each 0..255, no leading zeros, decimal only.
 *                          and prefix length must be <= 32
 *
 * Returns: mt_no_match              -- improperly formed
 *          mt_ipv4_prefix_partial   -- OK as far as it goes (or empty)
 *          mt_ipv4_prefix_complete  -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static match_type_t
cmd_ipv4_prefix_match(cmd_token t)
{
  match_strength_t ms ;

  ms = cmd_ipv4_match(cmd_token_make_string(t), 32) ;

  if (ms == ms_var_complete)
    return mt_ipv4_prefix_complete ;
  if (ms == ms_partial)
    return mt_ipv4_prefix_partial ;

  return mt_no_match ;
} ;

/*------------------------------------------------------------------------------
 * Is this an IPv4 Address or Prefix:
 *
 *   999.999.999.999[/99] -- each 0..255, no leading zeros, decimal only.
 *                           and prefix length must be <= n
 *
 * Returns: ms_no_match     -- improperly formed
 *          ms_partial      -- OK as far as it goes (or empty)
 *          ms_var_complete -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static match_strength_t
cmd_ipv4_match(const char* cp, uint prefix)
{
  uint nums ;

  for (nums = 0 ; nums < 4 ; ++nums)
    {
      if (*cp == '.')           /* need a '.' except at start   */
        {
          if (nums == 0)
            return ms_no_match ;

          ++cp ;                /* step past '.'                */
        }
      else
        {
          if (nums != 0)
            return (*cp == '\0') ? ms_partial : ms_no_match ;
        } ;

      /* Collect a decimal number 0..255, no leading zeros.
       *
       * Rejects anything other than digits -- including '/' and '.'.
       *
       * Accepts '\0' as partial -- which accepts empty strings.
       */
      if (*cp == '0')
        {
          ++cp ;
          if (isdigit(*cp))
            return ms_no_match ;      /* reject leading zeros   */
        }
      else
        {
          char* ep ;

          if (isdigit(*cp))
            {
              if (strtoul(cp, &ep, 10) <= 255)
                cp = ep ;
              else
                return ms_no_match ;  /* reject invalid number  */
            }
          else
            return (*cp == '\0') ? ms_partial : ms_no_match ;
        } ;
   } ;

  /* Arrive here with 4 numbers                                 */

  if (prefix == 0)
    return (*cp == '\0') ? ms_var_complete : ms_no_match ;
  else
    return cmd_prefix_match(cp, prefix) ;
} ;

/*------------------------------------------------------------------------------
 * Is this a Prefix:
 *
 *   /99   no leading zeros, decimal only, value must be <= n.
 *
 * Arrives here with *cp pointing at where there should be a '/'.
 *
 * Returns: ms_no_match     -- improperly formed
 *          ms_partial      -- OK as far as it goes (or empty).
 *          ms_var_complete -- syntactically complete
 */
static match_strength_t
cmd_prefix_match(const char* cp, uint prefix)
{
  if (*cp != '/')
    return (*cp == '\0') ? ms_partial : ms_no_match ;

  /* OK have '/' and a prefix is now expected.                          */

  ++cp ;                        /* step past '/'                */

  if (*cp == '\0')
    return ms_partial ;         /* if nothing after '/' */

  if (*cp == '0')
    ++cp ;
  else
    {
      char* ep ;
      if (isdigit(*cp) && (strtoul(cp, &ep, 10) <= prefix))
        cp = ep ;
      else
        return ms_no_match ;    /* reject invalid number    */
    } ;

  if (*cp != '\0')
    return ms_no_match ;  /* something other than digits after the '/',
                             or leading zero, or number too big         */

  return ms_var_complete ;
} ;

/*------------------------------------------------------------------------------
 * IPv6 Address and Prefix matching.
 */

#ifdef HAVE_IPV6

static match_strength_t cmd_ipv6_match(const char* cp, uint prefix) ;



/*------------------------------------------------------------------------------
 * Is this an IPv6 Address
 *
 *   h:h:...   -- RFC 4291 rules & prefix length must be <= n
 *
 * Returns: mt_no_match              -- improperly formed
 *          mt_ipv6_address_partial  -- OK as far as it goes (or empty)
 *          mt_ipv6_address_complete -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static match_type_t
cmd_ipv6_address_match (cmd_token t)
{
  match_strength_t ms ;

  ms = cmd_ipv6_match(cmd_token_make_string(t), 0) ;

  if (ms == ms_var_complete)
    return mt_ipv6_address_complete ;
  if (ms == ms_partial)
    return mt_ipv6_address_partial ;

  return mt_no_match ;
} ;

/*------------------------------------------------------------------------------
 * Is this an IPv6 Prefix
 *
 *   h:h:...[/99]   -- RFC 4291 rules & prefix length must be <= 128
 *
 * Returns: mt_no_match              -- improperly formed
 *          mt_ipv6_prefix_partial   -- OK as far as it goes (or empty)
 *          mt_ipv6_prefix_complete  -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static match_type_t
cmd_ipv6_prefix_match(cmd_token t)
{
  match_strength_t ms ;

  ms = cmd_ipv6_match(cmd_token_make_string(t), 128) ;

  if (ms == ms_var_complete)
    return mt_ipv6_prefix_complete ;
  if (ms == ms_partial)
    return mt_ipv6_prefix_partial ;

  return mt_no_match ;
} ;

/*------------------------------------------------------------------------------
 * Is this an IPv6 Address or Prefix:
 *
 *   h:h:...[/99]   -- RFC 4291 rules & prefix length must be <= n
 *
 * Returns: ms_no_match     -- improperly formed
 *          ms_partial      -- OK as far as it goes (or empty)
 *          ms_var_complete -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static match_strength_t
cmd_ipv6_match(const char* cp, uint prefix)
{
  bool  double_colon ;
  uint  nums ;

  double_colon = false ;
  nums         = 0 ;

  /* At start, the first time around the loop...                        */

  for (nums = 0 ; nums < 8 ; ++nums)
    {
      const char* sp, * ep ;

      /* After number (nums != 0), or at start.
       *
       * Deal with (a) ':', '::' and '::/'.
       *           (b) '/'  -- valid only if had '::'.
       *           (c) '\0' -- partial unless have had '::'
       *           (d) if not at start, must have one of the above.
       */
      if      (*cp == ':')
        {
          /* (a) ':', '::' and '::/'.
           *
           * At start can accept '::', but not ':' (unless '\0' follows).
           *
           * If not at start, accept ':' and accept '::' if not already seen.
           *
           * After '::' can have the full complement of numbers, or '/' or
           * '\0' which bring the number part to an end.
           *
           * After ':' we accept '\0' but explicitly reject '/' (and we
           * reject a ':' at the start if not followed by '\0').
           */
          ++cp ;                        /* step past ':'                */

          if      (*cp == ':')
            {
              /* '::' -- counts as number, can be followed by '/'       */

              if (double_colon)
                return ms_no_match ;    /* at most one                  */

              ++cp ;                    /* step past '::'               */

              double_colon = true ;
              ++nums ;                  /* counts as a number           */

              if ((nums == 8) || (*cp == '/') || (*cp == '\0'))
                break ;                 /* no more numbers              */
            }
          else if (*cp == '\0')
            return ms_partial ;         /* accepts bare ':', inter alia */

          else if ((*cp == '/') || (nums == 0))
            return ms_no_match ;
        }
      else if (*cp == '/')
        {
          /* (b) '/'  -- valid only if had '::'.                        */
          if (double_colon)
            break ;
          else
            return ms_no_match ;
        }
      else if (*cp == '\0')
        {
          /* (c) '\0' -- partial unless have had '::'                   */
          if (double_colon)
            break ;
          else
            return ms_partial ;     /* accept empty string, inter alia  */
        }
      else if (nums != 0)
        /* (d) if not at start, must have one of the above.             */
        return ms_no_match ;

      assert(*cp != '\0') ;

      /* Is now at start, or after ':' and is not '\0'.
       *
       * Require 1..4 hex digits -- will also accept 1..3 decimals !
       *
       * Rejects anything else, including '/' at this stage.
       */
      sp = cp ;
      ep = cp + 4 ;
      do
        {
          if (((*cp >= '0') && (*cp <= '9')) || ((*cp >= 'A') && (*cp <= 'F'))
                                             || ((*cp >= 'a') && (*cp <= 'f')))
            ++cp ;
          else
            {
              if (cp > sp)
                break ;
              else
                return ms_no_match ;    /* no digits    */
            }
        }
      while (cp < ep) ;

      /* Watch out for '.' !                                            */

      if (*cp == '.')
        {
          /* Can have IPv4 trailing part, if that would account for the
           * last two number parts of the IPv6.
           *
           * Note that a '.' after something which is not simple decimal
           * 0..255 will be rejected by cmd_ipv4_match().
           *
           * Note also that we pass through the prefix requirement.
           */
          if ((nums == 6) || (double_colon && (nums < 6)))
            return cmd_ipv4_match(sp, prefix) ;
          else
            return ms_no_match ;
        } ;
    } ;

  /* Arrives here either because nums == 8, or met '/' or '\0' after '::
   *
   * So only get here if have a valid end of the digits part of the IPv6
   */

  assert((nums == 8) || double_colon) ;

  if (prefix == 0)
    return (*cp == '\0') ? ms_var_complete : ms_no_match ;
  else
    return cmd_prefix_match(cp, prefix) ;
} ;


#endif /* HAVE_IPV6  */

/*------------------------------------------------------------------------------
 * Is this a decimal number in the allowed range:
 *
 * Returns: mt_no_match       -- improperly formed or empty
 *          mt_range_partial  -- OK as far as it went (or empty string)
 *          mt_range_complete -- syntactically complete
 */
static match_type_t
cmd_range_match (cmd_item item, cmd_token t)
{
  const char* cp, * dp ;
  char  *ep ;
  int   base ;
  long  val;

  confirm((LONG_MAX > item_max_number) && (LONG_MIN < -item_max_number)) ;

  cp = cmd_token_make_string(t) ;

  /* Worry about any sign                                               */

  dp = cp ;

  if ((*cp == '-') || (*cp == '+'))
    {
      if (!item->range_sign_allowed)
        return mt_no_match ;            /* reject '-' or '+'    */

      ++dp ;                            /* step to digit        */
    }
  else
    {
      if (item->range_sign_required)
        return mt_no_match ;
    } ;

  /* Worry about leading digits and hex, and no digits at all           */

  base = 10 ;                   /* by default.  */

  if      (*dp == '\0')
    return mt_range_partial ;   /* accepts empty string, inter alia     */

  else if (*dp == '0')
    {
      ++dp ;                    /* step past zero               */

      if (*dp != '\0')
        {
          /* No leading zeros and no stinking octal -- but allow hex    */
          if ((*dp != 'x') && (*dp != 'X'))
            return mt_no_match ;

          ++dp ;                /* step past 'x' or 'X'         */
          base = 16 ;

          if (*dp == '\0')
            return mt_range_partial ;
        } ;
    }

  else if (!isdigit(*dp))
    return mt_no_match ;

  /* The string starts with digit, possibly preceded by sign, and possibly
   * an 'x' or 'X' with at least 1 further character.
   */
  val = strtol(cp, &ep, base) ;
  if (*ep != '\0')
    return mt_no_match ;

  /* Is the result in range ?                                           */

  if (val >= item->range_min && val <= item->range_max)
    return mt_range_complete ;          /* on the money         */

  /* Want to return mt_range_partial iff adding digits might make
   * an in range value.
   *
   *   If val is  < 0, then adding digits makes it smaller.
   *   If val is == 0, not allowed to add digits.
   *   If val is  > 0, then adding digits makes it bigger.
   */
  if (val < item->range_min)
    {
      /* Is less than minimum, so partial match if can get bigger.      */
      return (val > 0) ? mt_range_partial : mt_no_match ;
    }
  else
    {
      /* Is more than maximum, so partial match if can get smaller.     */
      return (val < 0) ? mt_range_partial : mt_no_match ;
    } ;
} ;

/*==============================================================================
 * Command "filtering".
 *
 * The command parsing process starts with a (shallow) copy of the cmd_vector
 * entry for the current "node".
 *
 * So cmd_v contains pointers to struct cmd_command values.  When match fails,
 * the pointer is set NULL -- so parsing is a process of reducing the cmd_v
 * down to just the entries that match.
 *
 * Each cmd_command has a vector "items", which contains an entry for each
 * "token" position.  That entry is a vector containing the possible values at
 * that position.
 */

static int cmd_item_filter(cmd_parsed parsed, cmd_item item, cmd_token t) ;

/*------------------------------------------------------------------------------
 * Prepare to filter commands in the node being parsed in.
 *
 * The execute option turns off all partial matching -- so will not match, say,
 * 222 as a possible IP address !  This means that the result of the filter
 * operation will be executable command(s), only.
 *
 * The execute option also pre-filters the command vector to discard all
 * commands which are too short or too long to match the current line.
 *
 * The strict option turns off partial matching of keywords, so only complete
 * keywords will do.  This is used for the configuration file, so that new
 * commands can be added !
 *
 * Returns:  number of commands which may match to.
 */
static uint
cmd_filter_prepare(cmd_parsed parsed, cmd_parse_type_t type)
{
  vector src_v ;
  uint ci ;
  uint nct ;

  bool  execution = (type & cmd_parse_execution) != 0 ;
  bool  strict    = (type & cmd_parse_strict)    != 0 ;

  /* get commands for the current node                                  */
  src_v = ((cmd_node)vector_get_item(node_vector, parsed->cnode))
                                                                 ->cmd_vector ;

  assert(src_v != NULL) ;       /* invalid parsed->cnode ??     */

  /* empty the working commands vector, making sure big enough for the current
   * node's commands.
   *
   * Note that the cmd_v lives for as long as the parsed object, so will
   * grow over time to accommodate what is required.
   */
  vector_re_init(parsed->cmd_v,  vector_length(src_v)) ;

  /* Filter out commands which are too short.
   *
   * For execution, can filter out commands which are too long.
   */
  nct = parsed->num_command ;

  for (ci = 0 ; ci < vector_length(src_v) ; ++ci)
  {
    cmd_command cmd ;

    cmd = vector_get_item(src_v, ci) ;

    if ( cmd->nt_max < nct)
      continue ;                /* ignore if too short  */

    if ((cmd->nt_min > nct) && execution)
      continue ;                /* ignore if too long   */

    vector_push_item(parsed->cmd_v, cmd) ;
  } ;

  /* Other preparation for filtering
   *
   * The min_strength is set according to whether is for execution.  The
   * strongest match is set from that.  This means that any match which does
   * not meet the minimum criterion is automatically excluded.
   */

  parsed->min_strength = execution ? ms_min_execute  : ms_min_parse ;
  parsed->strict       = strict ;

  parsed->strongest     = parsed->min_strength ;
  parsed->best_complete = mt_no_match ;

  return vector_length(parsed->cmd_v) ;
} ;

/*------------------------------------------------------------------------------
 * Filter set commands by matching items against the given token.
 *
 * Takes:   parsed     -- cmd_parsed object, previously prepared by
 *                        cmd_prepare_fiter().
 *          ii         -- item index (0 == first)
 *          keep_items -- how to filter
 *
 * The item index is the index wrt the start of the commands being matched,
 * not the index of the token in the command line.
 *
 * Keywords must match strictly or partly, depending on the filtering state
 * in the parsed object.
 *
 * Variables must match completely if filtering for execution, otherwise may
 * match partially.
 *
 * Returns: the number of items matched.
 *
 * NB: when filtering for execution, will not accept any partial matches for
 *     variables (but may accept partial matches for keywords).  So, once the
 *     last token has been filtered against, the number of items matched gives
 *     the number of unambiguous commands, with complete values, that the line
 *     matches.
 */
static uint
cmd_filter(cmd_parsed parsed, uint ii, bool keep_items)
{
  uint ci ;             /* command index -- in cmd_v    */
  uint ti ;             /* token index in command line  */
  uint c_keep ;
  uint i_keep ;
  cmd_token t ;

  /* Reset the filtering state                                          */

  parsed->strongest     = parsed->min_strength ;
  parsed->best_complete = mt_no_match ;

  /* If we are keeping the items which are matched, set the item_v
   * empty and guess that may get one item per command.
   *
   * Note that the item_v lives for as long as the parsed object, so will
   * grow over time to accommodate what is required.
   */
  if (keep_items)
    vector_re_init(parsed->item_v, vector_length(parsed->cmd_v)) ;

  /* Work down the cmd_v, attempting to match cmd_items against cmd_tokens.
   *
   * Keep in the cmd_v the commands for which we get a match.
   *
   * At the same time, if required, keep in the item_v all the items which have
   * matched.
   */
  if (ii < parsed->num_command)
    ti = parsed->first_command + ii ;   /* map to token index   */
  else
    {
      /* special case of filtering against the empty token at the end of
       * the command line -- this is for command line help stuff.
       *
       * Note that there may be out_pipe or even comment tokens in between,
       * so we here set the ti to the trailing eol token.
       */
      assert(ii == parsed->num_command) ;
      ti = parsed->num_tokens ;
    }
  t = cmd_token_get(parsed->tokens, ti) ;

  c_keep = 0 ;
  i_keep = 0 ;

  if (keep_items)
    vector_re_init(parsed->item_v, vector_length(parsed->cmd_v)) ;

  for (ci = 0; ci < vector_length(parsed->cmd_v); ci++)
    {
      cmd_command cmd ;
      cmd_item    item ;
      int         best ;

      cmd = vector_get_item(parsed->cmd_v, ci) ;

      if      (ii < cmd->nt)
        item = vector_get_item(cmd->items, ii) ;

      else if (ii == cmd->nt_max)
        item = &eol_item ;      /* match at end of line                 */

      else if (ii >  cmd->nt_max)
        continue ;              /* discard commands we are beyond, now  */

      else                      /* cmd->nt < cmd->nt_max <=> vararg.    */
        {
          /* It is elsewhere arranged that a vararg is always the last
           * item for a given command.
           *
           * We cope with all tokens from the first vararg onwards by matching
           * them all against the vararg.  Inter alia this allows for a
           * vararg to check each argument in turn -- iff feel like doing
           * that.
           */
          item = cmd->vararg ;

          /* Must have something and must now only check against the varag.
           * (Not anything else in the (...) if vararg was in one !)
           */
          assert((item != NULL) && (item->next == NULL)) ;
        } ;

      /* See if get any sort of match at current position               */
      best = -1 ;
      while (item != NULL)
        {
          int ret ;
          ret = cmd_item_filter(parsed, item, t) ;

          if (ret >= 0)
            {
              if (ret > 0)
                i_keep = 0 ;

              if (keep_items)
                vector_set_item(parsed->item_v, i_keep++, item) ;
              else
                ++i_keep ;
            } ;

          if (ret > best)
            best = ret ;

          item = item->next ;
        } ;

      /* Keep if had a match                                            */
      if (best >= 0)
        {
          if (best > 0)
            c_keep = 0 ;        /* better than all the rest             */

          vector_set_item(parsed->cmd_v, c_keep++, cmd) ;
        } ;
    } ;

  if (keep_items)
    vector_set_length(parsed->item_v, i_keep) ; /* discard what did not keep */

  vector_set_length(parsed->cmd_v, c_keep) ;    /* discard what did not keep */

  return i_keep ;
} ;

/*------------------------------------------------------------------------------
 * Filter given item, in given parsed state, attempting to match given token.
 *
 * Update the parsed state if get a better match.
 *
 * Returns:  < 0 => is not as good  )
 *          == 0 => equally good    ) compared to best match to date.
 *           > 0 => better than     )
 *
 * NB: the matching functions will partially match an empty string.
 *
 *     The ms_anything types of item, will match an empty string.
 */
static int
cmd_item_filter(cmd_parsed parsed, cmd_item item, cmd_token t)
{
  match_strength_t  ms ;
  match_type_t      mt ;
  int  cw ;

  /* Ignore item if the best we can hope for is not as strong as what we
   * have already.
   */
  mt = item_best_match(item->type) ;
  ms = match_match_strength(mt) ;

  if ((mt < parsed->best_complete) || (ms < parsed->strongest))
    return -1 ;                 /* cannot even be as good               */

  /* Bang the rocks together to get match type
   *
   * TODO: do we need mt_xxx_partial etc ?
   */
  if (t->type == cmd_tok_eol)
    {
//    mt = (item->type == item_eol) ? mt_eol : mt_eol_partial ;
      mt = mt_eol_partial ;
    }
  else
    {
      switch (item->type)
      {
        case item_null:
          zabort("invalid item_null") ;
          break ;

        case item_eol:
          break ;

        case item_keyword:
          cw = els_cmp_word(t->ot, item->str) ;

          if      (cw  > 0)             /* nope                 */
            mt = mt_no_match ;
          else if (cw == 0)             /* exact match          */
            mt = mt_keyword_complete ;
          else                          /* partial match        */
            mt = parsed->strict ? mt_no_match
                                : mt_keyword_incomplete ;
          break ;

        case item_range:
          mt = cmd_range_match(item, t) ;
          break ;

        case item_ipv4_address:
          mt = cmd_ipv4_address_match(t) ;
          break ;

        case item_ipv4_prefix:
          mt = cmd_ipv4_prefix_match(t) ;
          break ;

        case item_ipv6_address:
    #ifdef HAVE_IPV6
          mt = cmd_ipv6_address_match(t) ;
    #endif /* HAVE_IPV6  */
          break ;

        case item_ipv6_prefix:
    #ifdef HAVE_IPV6
          mt = cmd_ipv6_prefix_match(t) ;
    #endif /* HAVE_IPV6  */
          break ;

        case item_word:
          mt = mt_word_match ;
          break ;

        case item_vararg:
          mt = mt_vararg_match ;
          break ;

        case item_option_word:
          mt = mt_option_word_match ;
          break ;

        default:
          zabort("unknown item type") ;
      } ;
    } ;

  /* Easy if did not match at all                                       */
  if (mt == mt_no_match)
    return -1 ;

  /* Is what we got worse, as good or better ?
   *
   * Update parsed to suit and return the news.
   *
   * Note that parsed->best_complete will be ms_no_match until parsed->strongest
   * is set to ms_var_complete.
   */
  ms = match_match_strength(mt) ;

  if (ms  < parsed->strongest)
    return -1 ;

  if (ms == parsed->strongest)
    return 0 ;

  parsed->strongest = ms ;
  return +1 ;
} ;

/*==============================================================================
 * Parsing of command lines
 */

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", if possible, ready for execution.
 *
 * If 'exact':    use cmd_filter_by_string()
 *   otherwise:   use cmd_filter_by_completion()
 *
 * If 'do':       see if there is a 'do' at the front and proceed accordingly.
 *
 * If 'tree':     move up the node tree to find command if not found in the
 *                current node.
 */

static cmd_return_code_t cmd_parse_phase_one(cmd_parsed parsed,
                                      cmd_parse_type_t type, node_type_t node) ;
static cmd_return_code_t cmd_parse_phase_one_b(cmd_parsed parsed, uint nt) ;
static cmd_return_code_t cmd_parse_phase_two(cmd_parsed parsed,
                                                   cmd_parse_type_t type) ;

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", or (if required) any of its ancestors.
 *
 * Requires command line to have been tokenised.
 *
 * Returns:  CMD_SUCCESS => successfully parsed command, and the result is
 *                          in the given parsed structure, ready for execution.
 *
 *                          NB: parsed->cnode may not be the incoming node.
 *
 *                          NB: parsed->parts is what was found
 *
 *                          NB: parsed->cmd->daemon => daemon
 *
 *           CMD_EMPTY   => line is empty, except perhaps for comment
 *                          (iff parsing for execution)
 *
 *           CMD_ERR_INCOMPLETE => "do" and nothing more
 *                          (iff parsing for execution)
 *
 *           CMD_SUCCESS_DAEMON => parsed successfully.  Something for vtysh ??
 *
 *           CMD_ERR_NO_MATCH   => could find nothing to match the command
 *                                 line.
 *
 *           CMD_ERR_AMBIGUOUS  => found 2 or more possible matches.
 *                                 Or, if not parsing for execution, there
 *                                 were no command tokens.
 *
 *           CMD_ERR_PARSING    => something wrong !  See parsed->emess.
 *
 * NB: unless cmd_parse_no_tree, may have tried any ancestor nodes.  Returns
 *     with parsed->cnode with node last tried.
 *
 * NB: unless cmd_parse_no_do, will have taken a leading "do", and pushed the
 *     parsed->cnode to ENABLE_NODE (if in MIN_DO_SHORTCUT_NODE or higher).
 *
 * NB: the command line MUST be preserved until the parsed command is no
 *     longer required -- no copy is made.
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 *
 * See elsewhere for description of parsed structure.
 */
extern cmd_return_code_t
cmd_parse_command(cmd_parsed parsed, node_type_t node, cmd_parse_type_t type)
{
  cmd_return_code_t ret ;

  /* Level 1 parsing
   *
   * Break down the tokens into:
   *
   *   1) in-pipe or command (with or without "do")
   *   2) possible out-pipe
   *   3) possible comment
   *
   * Complete any tokens which contain quotes and/or escapes.
   *
   * If there is a command then:
   *
   *   - sort out any "do" and cut from start of command.
   *   - set parsed->cnode (from given node or according to "do")
   *   - set parsed->cmd = NULL
   *   - empty the argv vector
   *
   * Note that cmd_parse_phase_one only returns CMD_SUCCESS or CMD_ERR_PARSING.
   */
  ret = cmd_parse_phase_one(parsed, type, node) ;
  if (ret != CMD_SUCCESS)
    {
      assert(ret == CMD_ERR_PARSING) ;  /* no other error at this point */
      return ret ;
    } ;

  /* If no command tokens, and is parsing for execution, then we are done...
   * but watch out for bare "do"
   */
  if (((parsed->parts & cmd_part_command) == 0) &&
                                           ((type & cmd_parse_execution) != 0))
    {
      if ((parsed->parts & ~cmd_part_comment) == cmd_parts_none)
        return CMD_EMPTY ;              /* accept empty         */

      if ((parsed->parts & cmd_part_do) != 0)
        return CMD_ERR_INCOMPLETE ;     /* reject "do" alone    */

      return CMD_SUCCESS ;              /* accept pipes         */
    } ;

  /* Level 2 parsing
   *
   * Try in the current node and then in parent nodes, if can.
   *
   * Cannot move up the node tree if is already at CONFIG_NODE or below.
   * Note that "do" pushes us to the ENABLE_NODE -- which is below the
   * CONFIG_NODE.
   *
   * Note that when not parsing for execution, may get here with no command
   * tokens at all -- in which case cmd_parse_phase_two() will return
   * CMD_ERR_AMBIGUOUS.
   *
   * Note that cmd_parse_phase_two only returns CMD_SUCCESS, CMD_ERR_NO_MATCH
   * or CMD_ERR_AMBIGUOUS.
   */
  while (1)
    {
      node_type_t pnode ;

      ret = cmd_parse_phase_two(parsed, type) ;

      if (ret == CMD_SUCCESS)
        break ;

      if (ret != CMD_ERR_NO_MATCH)
        return ret ;

      confirm(ENABLE_NODE < CONFIG_NODE) ;

      if (((type & cmd_parse_no_tree) != 0) || (parsed->cnode <= CONFIG_NODE))
        return CMD_ERR_NO_MATCH ;

      pnode = cmd_node_parent(parsed->cnode) ;

      if (pnode == parsed->cnode)
        return CMD_ERR_NO_MATCH ; /* done if no parent node.            */

      parsed->cnode = pnode ;
    } ;

  /* Parsed successfully.
   *
   * If for execution, fill the arg_vector
   *
   * The arg_vector is an array of pointers to '\0' terminated strings, which
   * are pointers to the relevant tokens' qstring bodies.
   */

  if ((type & cmd_parse_execution) != 0)
    {
      uint ti ;

      cmd_arg_vector_empty(parsed) ;

      for (ti = 0; ti < parsed->num_command ; ti++)
        {
          bool take ;

          if (ti < parsed->cmd->nt_min)
            {
              cmd_item item = vector_get_item (parsed->cmd->items, ti);

              take = item->arg ;    /* follow item                          */
            }
          else
            take = true ;           /* option or vararg token               */

          if (take)
            {
              cmd_token t ;
              t = cmd_token_get(parsed->tokens, parsed->first_command + ti) ;
              cmd_arg_vector_push(parsed, cmd_token_make_string(t)) ;
            } ;
        } ;
    } ;

  /* Return appropriate form of success                                 */
  return parsed->cmd->daemon ? CMD_SUCCESS_DAEMON
                             : CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 1 of command parsing
 *
 * Scan the tokens to break them up into the sections:
 *
 *    - in-pipe  -- '<' etc up to '>' or comment
 *    - do       -- leading 'do' on the command (empty if in-pipe)
 *    - command  -- actual command              (empty if in-pipe)
 *    - out-pipe -- '>' etc up to comment
 *    - comment  -- '!' or '#' onwards
 *
 * Requires line to have been tokenised -- cmd_tokenise().
 *
 * Returns: CMD_SUCCESS     -- all is well
 *          CMD_ERR_PARSING -- parsing error -- malformed or misplaced pipe
 *                                           -- malformed quotes/escapes
 */
static cmd_return_code_t
cmd_parse_phase_one(cmd_parsed parsed, cmd_parse_type_t type, node_type_t node)
{
  uint  nt = parsed->num_tokens ;

  /* Set command and parsing entries                            */
  parsed->cnode   = node ;
  parsed->cmd     = NULL ;

  /* pick off any comment                                       */
  if ((parsed->tok_total & cmd_tok_comment) != 0)
    {
      parsed->num_comment   = 1 ;
      parsed->first_comment = --nt ;    /* implicitly the last  */
      parsed->tok_total   ^= cmd_tok_comment ;
      parsed->parts       |= cmd_part_comment ;
    } ;

  /* If this is not a simple line need to do some extra work:
   *
   *   - identify and check any pipe items
   *   -
   */
  if      (parsed->tok_total == cmd_tok_simple)
    {
      /* All tokens are simple and there is at least one                */
      parsed->first_command = 0 ;
      parsed->num_command   = nt ;
      parsed->parts        |= cmd_part_command ;
    }
  else if (parsed->tok_total != 0)
    {
      /* There is at least one not-simple cmd_token.                        */
      cmd_return_code_t ret ;
      ret = cmd_parse_phase_one_b(parsed, nt) ;

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* If have a  have a 'do' at the front, account for it                */
  if ((parsed->parts & cmd_part_command) != 0)
    {
      /* Have a command -- worry about "do" if allowed                  */
      if (((type & cmd_parse_no_do) == 0) && (node >= MIN_DO_SHORTCUT_NODE))
        {
          cmd_token t ;
          t = cmd_token_get(parsed->tokens, parsed->first_command) ;
          if (els_len_nn(t->ot) == 2)
            {
              const char* p = els_body_nn(t->ot) ;
              if ((*p == 'd') && (*(p+1) == 'o'))
                {
                  node = ENABLE_NODE ;          /* change to this node  */

                  parsed->num_do   = 1 ;
                  parsed->first_do = parsed->first_command ;
                  --parsed->num_command ;
                  ++parsed->first_command ;
                  parsed->parts |= cmd_part_do ;
                  /* If have *only* the "do", we don't have a command   */
                  if (parsed->num_command == 0)
                    parsed->parts ^= cmd_part_command ;
                } ;
            } ;
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 1b of command parsing
 *
 * Tokeniser found at least one of:
 *
 *   - in pipe token
 *   - out pipe token
 *   - token with quotes or escapes
 *
 * Deal with all of those, verifying the syntax of any pipes and completing
 * any tokens with quotes etc.
 *
 * Update the "parts" and the relevant first/num values.
 *
 * Returns: CMD_SUCCESS     -- all is well
 *          CMD_ERR_PARSING -- parsing error -- malformed or misplaced pipe
 *                                           -- malformed quotes/escapes
 */
static cmd_return_code_t
cmd_parse_phase_one_b(cmd_parsed parsed, uint nt)
{
  cmd_return_code_t ret ;
  cmd_token     t ;
  cmd_parts_t   parts ;
  uint i, n ;
  uint* pn ;

  parts = cmd_parts_none ;      /* no parts yet                 */
  n     = 0 ;                   /* no tokens in current part    */
  pn    = NULL ;                /* no current part              */

  for (i = 0 ; i < nt ; ++i)
    {
      t = cmd_token_get(parsed->tokens, i) ;

      if ((t->type & cmd_tok_simple) != 0)
        {
          if (parts == cmd_parts_none)
            {
              parts = cmd_part_command ;
              parsed->first_command  = i ;
              pn = &parsed->num_command ;
              n  = 0 ;
            } ;
        } ;

      if ((t->type & cmd_tok_in_pipe) != 0)
        {
          if (parts != cmd_parts_none)
            return cmd_parse_error(parsed, t, 0, "unexpected 'pipe in'") ;

          ret = cmd_parse_in_pipe(parsed, t) ;
          if (ret != CMD_SUCCESS)
            return ret ;

          parts = cmd_part_in_pipe ;
          parsed->first_in_pipe  = i ;
          pn = &parsed->num_in_pipe ;
          n  = 0 ;
        } ;

      if ((t->type & cmd_tok_out_pipe) != 0)
        {
          if ((parts == cmd_parts_none) || ((parts & cmd_part_out_pipe) != 0))
            return cmd_parse_error(parsed, t, 0, "unexpected 'pipe out'") ;

          ret = cmd_parse_out_pipe(parsed, t) ;
          if (ret != CMD_SUCCESS)
            return ret ;

          *pn = n ;                     /* set number of in-pipe/cmd    */

          parts |= cmd_part_out_pipe ;
          parsed->first_out_pipe  = i ;
          pn = &parsed->num_out_pipe ;
          n  = 0 ;
        } ;

      assert(parts != cmd_parts_none) ; /* dealt with all token types   */

      if ((t->type & cmd_tok_incomplete) != 0)
        {
          ret = cmd_token_complete(parsed, t) ;
          if (ret != CMD_SUCCESS)
            return ret ;
        } ;

      ++n ;                         /* count up tokens              */
    } ;

  if (pn != NULL)
    *pn = n ;                       /* number in last phase         */

  /* If have an in-pipe or an out-pipe, worry about the number of
  * arguments
  */
  if ((parts & cmd_parts_pipe) != 0)
    {
      const char*  msg  = NULL ;
      uint i ;
      bool e ;

      /* If there is an in_pipe part, check number of arguments         */
      if ((msg == NULL) && ((parts & cmd_part_in_pipe) != 0))
        {
          assert(parsed->num_in_pipe > 0) ;

          if (((parsed->in_pipe & cmd_pipe_file) != 0)
                                                && (parsed->num_in_pipe != 2))
            {
              if (parsed->num_in_pipe == 1)
                {
                  i    = parsed->first_in_pipe ;
                  e    = true ;
                  msg  = "requires file" ;
                }
              else
                {
                  i    = parsed->first_in_pipe + 2 ;
                  e    = false ;
                  msg  = "expects file only" ;
                } ;
            } ;

          if (((parsed->in_pipe & cmd_pipe_shell) != 0)
                                                 && (parsed->num_in_pipe < 2))
            {
              i    = parsed->first_in_pipe ;
              e    = true ;
              msg  = "requires shell command" ;
            } ;
        } ;

      /* If there is an out_pipe part, check the number of arguments    */
      if ((msg == NULL) && ((parts & cmd_part_out_pipe) != 0))
        {
          assert(parsed->num_out_pipe > 0) ;

          if (((parsed->out_pipe & cmd_pipe_file) != 0)
                                               && (parsed->num_out_pipe != 2))
            {
              if (parsed->num_out_pipe == 1)
                {
                  i    = parsed->first_out_pipe ;
                  e    = true ;
                  msg  = "requires file" ;
                }
              else
                {
                  i    = parsed->first_out_pipe + 2 ;
                  e    = false ;
                  msg  = "expects file only" ;
                } ;
            } ;

          if (((parsed->out_pipe & cmd_pipe_shell) != 0)
                                                && (parsed->num_out_pipe < 2))
            {
              assert(parsed->num_out_pipe > 0) ;

              i    = parsed->first_out_pipe ;
              e    = true ;
              msg  = "requires shell command" ;
            } ;
        } ;

      if (msg != NULL)
        {
          t = cmd_token_get(parsed->tokens, i) ;
          return cmd_parse_error(parsed, t, e ? els_len_nn(t->ot) : 0, msg) ;
        } ;
    } ;

  /* It's OK -- so record the parts found and return success            */
  parsed->parts |= parts ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 2 of command parsing -- parsing for execution.
 *
 * Assumes phase 1 completed successfully.
 *
 * Note that if parsed->num_command == 0, will have constructed a cmd_v, with
 * all possible commands in it (depending on cmd_parse_execution).
 *
 * Returns:  CMD_SUCCESS        -- parsed successfully
 *           CMD_ERR_NO_MATCH   -- could find nothing that matches
 *           CMD_ERR_AMBIGUOUS  -- found more than one match
 *                                   or parsed->num_command == 0
 */
static cmd_return_code_t
cmd_parse_phase_two(cmd_parsed parsed, cmd_parse_type_t type)
{
  uint ii ;
  uint match ;

  /* Prepare to filter commands                                         */

  cmd_filter_prepare(parsed, type) ;

  match = 2 ;   /* in case parsed->num_command == 0 !   */

  for (ii = 0 ; ii < parsed->num_command ; ii++)
    match = cmd_filter(parsed, ii, false) ;

  /* Should end up with one command to execute.                         */
  if (match == 0)
    return CMD_ERR_NO_MATCH ;

  if (match >  1)
    return CMD_ERR_AMBIGUOUS ;

  parsed->cmd = vector_get_item(parsed->cmd_v, 0) ;

  return CMD_SUCCESS ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * If src matches dst return dst string, otherwise return NULL
 *
 * Returns NULL if dst is an option, variable of vararg.
 *
 * NULL or empty src are deemed to match.
 */
static const char *
cmd_entry_function (const char *src, const char *dst)
{
  if (CMD_OPTION (dst) || CMD_VARIABLE (dst) || CMD_VARARG (dst))
    return NULL;

  if ((src == NULL) || (*src == '\0'))
    return dst;

  if (strncmp (src, dst, strlen (src)) == 0)
    return dst;

  return NULL;
}

/* If src matches dst return dst string, otherwise return NULL */
/* This version will return the dst string always if it is
   CMD_VARIABLE for '?' key processing */
static const char *
cmd_entry_function_item (const char *src, const char *dst)
{
  if (CMD_VARARG (dst))
    return dst;

  if (CMD_RANGE (dst))
    {
      if (cmd_range_match (dst, src))
        return dst;
      else
        return NULL;
    }

#ifdef HAVE_IPV6
  if (CMD_IPV6 (dst))
    {
      if (cmd_ipv6_match (src))
        return dst;
      else
        return NULL;
    }

  if (CMD_IPV6_PREFIX (dst))
    {
      if (cmd_ipv6_prefix_match (src))
        return dst;
      else
        return NULL;
    }
#endif /* HAVE_IPV6 */

  if (CMD_IPV4 (dst))
    {
      if (cmd_ipv4_match (src))
        return dst;
      else
        return NULL;
    }

  if (CMD_IPV4_PREFIX (dst))
    {
      if (cmd_ipv4_prefix_match (src))
        return dst;
      else
        return NULL;
    }

  /* Optional or variable commands always match on '?' */
  if (CMD_OPTION (dst) || CMD_VARIABLE (dst))
    return dst;

  /* In case of 'command \t', given src is NULL string. */
  if (src == NULL)
    return dst;

  if (strncmp (src, dst, strlen (src)) == 0)
    return dst;
  else
    return NULL;
}

/*------------------------------------------------------------------------------
 * Check same string element existence.
 *
 * Returns: 0 => found same string in the vector
 *          1 => NOT found same string in the vector
 */
static bool
cmd_unique_string (vector v, const char *str)
{
  unsigned int i;
  char *match;

  for (i = 0; i < vector_length (v); i++)
    if ((match = vector_get_item (v, i)) != NULL)
      if (strcmp (match, str) == 0)
        return 0;
  return 1;
}

/* Compare string to description vector.  If there is same string
   return 1 else return 0. */
static bool
item_unique_string (vector v, const char *str)
{
  unsigned int i;
  struct cmd_item *item;

  for (i = 0; i < vector_length (v); i++)
    if ((item = vector_get_item (v, i)) != NULL)
      if (strcmp (item->cmd, str) == 0)
        return 1;
  return 0;
}
#endif
/*==============================================================================
 * '?' describe command support.
 */

/*------------------------------------------------------------------------------
 * Get description of current (partial) command
 *
 * Returns: NULL => no description available
 *
 *                  status set to CMD_ERR_NO_MATCH or CMD_ERR_AMBIGUOUS
 *
 *      or: address of vector of "struct cmd_item" values available.
 *
 * NB: when a vector is returned it is the caller's responsibility to
 *     vector_free() it.  (The contents are all effectively const, so do not
 *     themselves need to be freed.)
 */
extern vector
cmd_describe_command (const char* line, node_type_t node,
                                                      cmd_return_code_t* status)
{
#if 0
  vector ret ;
  struct cmd_parsed parsed_s ;
  cmd_parsed        parsed ;
  cmd_token_type_t  tok_total ;

  /* Set up a parser object and tokenise the command line               */
  parsed = cmd_parse_init_new(&parsed_s) ;
  tok_total = cmd_tokenise(parsed, line, node) ;







  /* Level 1 parsing
   *
   * Strip quotes and escapes from all the tokens.
   */
  if (tok_total != cmd_tok_simple)
    {
      ret = cmd_parse_phase_one(parsed) ;
      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* If allowed to 'do', see if there.
   *
   * 'do' forces command to be parsed in ENABLE_NODE (if allowed)
   */
  if ((type & cmd_parse_no_do) == 0)
    cmd_try_do_shortcut(parsed) ;




  return cmd_describe_command_real (tokens, node, status);



  static vector
  cmd_describe_command_real (vector tokens, int node, int *status)
  {
    unsigned int i;
    vector cmd_vector;
  #define INIT_MATCHVEC_SIZE 10
    vector matchvec;
    struct cmd_command *cmd_command;
    unsigned int index;
    int ret;
    enum match_type match;
    char *command;

    /* Set index. */
    if (vector_length (tokens) == 0)
      {
        *status = CMD_ERR_NO_MATCH;
        return NULL;
      }
    else
      index = vector_length (tokens) - 1;

    /* Make copy vector of current node's command vector. */
    cmd_vector = vector_copy (cmd_node_vector (cmdvec, node));

    /* Prepare match vector */
    matchvec = vector_init (INIT_MATCHVEC_SIZE);

    /* Filter commands. */
    /* Only words precedes current word will be checked in this loop. */
    for (i = 0; i < index; i++)
      if ((command = vector_get_item (tokens, i)))
        {
          match = cmd_filter(command, cmd_vector, i, any_match) ;

          if (match == vararg_match)
            {
              struct cmd_command *cmd_command;
              vector descvec;
              unsigned int j, k;

              for (j = 0; j < vector_length (cmd_vector); j++)
                if ((cmd_command = vector_get_item (cmd_vector, j)) != NULL
                    && (vector_length (cmd_command->items)))
                  {
                    descvec = vector_get_item (cmd_command->items,
                                           vector_length (cmd_command->items) - 1);
                    for (k = 0; k < vector_length (descvec); k++)
                      {
                        struct cmd_item *item = vector_get_item (descvec, k);
                        vector_set (matchvec, item);
                      }
                  }

              vector_set (matchvec, &item_cr);
              vector_free (cmd_vector);

              return matchvec;
            } ;

          ret = is_cmd_ambiguous (command, cmd_vector, i, match) ;
          if (ret != 0)
            {
              vector_free (cmd_vector);
              vector_free (matchvec);
              *status = (ret == 1) ? CMD_ERR_AMBIGUOUS
                                   : CMD_ERR_NO_MATCH ;
              return NULL ;
            } ;
        }

    /* Prepare match vector */
    /*  matchvec = vector_init (INIT_MATCHVEC_SIZE); */

    /* Make sure that cmd_vector is filtered based on current word        */
    command = vector_get_item (tokens, index);
    if (command)
      match = cmd_filter(command, cmd_vector, index, any_match);

    /* Make description vector. */
    for (i = 0; i < vector_length (cmd_vector); i++)
      {
        vector items ;

        cmd_command = vector_get_item (cmd_vector, i) ;
        if (cmd_command == NULL)
          continue ;

        /* Ignore cmd_command if no tokens at index position.
         *
         * Deal with special case of possible <cr> completion.
         */
        items = cmd_command->items;
        if (index >= vector_length (items))
          {
            if (command == NULL && index == vector_length (items))
              {
                if (!item_unique_string (matchvec, cr_string))
                  vector_push_item(matchvec, &item_cr);
              }
            continue ;
          } ;

        /* Check if command is completed.                                 */
        unsigned int j;
        vector descvec = vector_get_item (items, index);
        struct cmd_item *item;

        for (j = 0; j < vector_length (descvec); j++)
          if ((item = vector_get_item (descvec, j)))
            {
              const char *string;

              string = cmd_entry_function_desc (command, item->cmd);
              if (string)
                {
                  /* Uniqueness check */
                  if (!item_unique_string (matchvec, string))
                    vector_push_item(matchvec, item);
                }
            } ;
        } ;

    vector_free (cmd_vector);

    if (vector_length(matchvec) == 0)
      {
        vector_free (matchvec);
        *status = CMD_ERR_NO_MATCH;
        return NULL;
      }

    *status = CMD_SUCCESS;
    return matchvec;
  }

  cmd_parse_reset(parsed, false) ;

  return
#endif

  return NULL ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * Check LCD of matched command.
 *
 * Scan list of matched keywords, and by comparing them pair-wise, find the
 * longest common leading substring.
 *
 * Returns: 0 if zero or one matched keywords
 *          length of longest common leading substring, otherwise.
 */
static int
cmd_lcd (vector matchvec)
{
  int n ;
  int i ;
  int lcd ;
  char *sp, *sq, *ss ;

  n = vector_end(matchvec) ;
  if (n < 2)
    return 0 ;

  ss = vector_get_item(matchvec, 0) ;
  lcd = strlen(ss) ;

  for (i = 1 ; i < n ; i++)
    {
      sq = ss ;
      ss = vector_get_item(matchvec, i) ;
      sp = ss ;

      while ((*sp == *sq) && (*sp != '\0'))
        {
          ++sp ;
          ++sq ;
        } ;

      if (lcd > (sp - ss))
        lcd = (sp - ss) ;
    }
  return lcd;
}

#endif

/*==============================================================================
 * Command line help support.
 *
 *
 */

static bool cmd_post_last(cmd_parsed parsed) ;


/*------------------------------------------------------------------------------
 * Look out for the following special cases:
 *
 *   a) before or on '#' or '!'
 *
 *      '#' and '!' are only recognised as the start of a comment at the
 *      start of a line...  So can insert something in front, and what
 *      currently looks like a comment, suddenly needs to be reconsidered
 *      as tokens... the first of which happens to start with '!' or '#'.
 *
 *      To save work, and leave open the possibility of trailing comments,
 *      we treat this case as an "error".
 *
 *   b) before or on '<'
 *
 *      currently, pipe-in must be the first token on the command line.
 *
 *      So if there is anything prior to the '<', treat that as an error.
 *      If is just spaces before the '<', say that nothing may be placed
 *      before the '<' (cf nothing may be placed before '!' or '#').
 *
 * in these cases no help is available -- return message explaining.
 *
 * NB: assumes that have already done cmd_token_position(), and therefore that
 *     if there is a '#' or '<' that cannot be positioned *after* it, or
 *     indeed that can be positioned after a '>'.
 */
extern const char*
cmd_help_preflight(cmd_parsed parsed)
{
  if ((parsed->tok_total & cmd_tok_comment) != 0)
    {
      return "cannot have a command on a comment line";
    } ;

  if ((parsed->tok_total & cmd_tok_in_pipe) != 0)
    {
      return "cannot have a command and an '<' pipe together" ;
    } ;

  return NULL ;         /* OK !                 */
} ;




/*------------------------------------------------------------------------------
 * See if current command line can be completed, and if so, how.
 *
 * NB: must already have done cmd_token_position().
 *
 *     Must not be called if cmd_token_position() reported a "special".
 *
 * Returns:  CMD_ERR_PARSING   -- the parser could not make sense of the line.
 *
 */
extern cmd_return_code_t
cmd_completion(cmd_parsed parsed, node_type_t node)
{
  cmd_return_code_t ret ;

  /* Parse the line -- allow completion, allow do, allow backing up the tree,
   *                   but do not parse for execution.
   */
  ret = cmd_parse_command(parsed, node, cmd_parse_standard) ;

  if (ret == CMD_ERR_PARSING)
    return ret ;                        /* nothing more possible        */

  /* Expect now to have a cmd_v set with the result of the filtering,
   * with 0, 1 or more possible commands in it.
   *
   * Now establish what the alternatives are at the current token position.
   *
   * We do this by filtering again, on the current token position, and
   * asking the filter to collect all the possible items.  Note that if the
   * current token position is beyond the last actual command token, then
   * will be filtering on the empty eol token -- which we arrange to match
   * anything, including eol -- which gives the dummy item_cr of type
   * item_eol.
   */
  assert(parsed->cti >= parsed->first_command) ;
  assert(parsed->cti <= parsed->first_command + parsed->num_command) ;

  cmd_filter(parsed, parsed->cti - parsed->first_command, true) ;

  /* Reduce the list of items available at the current position to
   * eliminate duplicates and have the possibilities sorted by type and
   * by value.
   */
  if (vector_length(parsed->item_v) > 1)
    {
      cmd_item prev ;
      uint     ii ;
      uint     i_keep ;

      vector_sort(parsed->item_v, (vector_sort_cmp*)cmd_cmp_item) ;

      i_keep = 1 ;

      prev = vector_get_item(parsed->item_v, 0) ;
      for (ii = 1 ; ii < vector_length(parsed->item_v) ; ++ii)
        {
          cmd_item item ;

          item = vector_get_item(parsed->item_v, ii) ;
          if (cmd_cmp_item(&item, &prev) != 0)
            {
              vector_set_item(parsed->item_v, i_keep++, item) ;
              prev = item ;
            } ;
        } ;

      vector_set_length(parsed->item_v, i_keep) ;
                                          /* discard what did not keep  */
    } ;

  return CMD_SUCCESS ;
} ;




/*------------------------------------------------------------------------------
 * How to insert a newly completed keyword ?
 *
 * There are a number of cases:
 *
 *   1) the cti is for the token to be completed.
 *
 *      May be positioned one or more spaces in front of the keyword.
 *
 *      Will replace any leading spaces and the keyword by the new keyword.
 *
 *   2) the cti is for the empty eol token
 *
 *      Will replace any leading spaces to the eol keyword, by the new
 *      keyword.
 *
 *   3) the cti is for a '>' token, or any other !cmd_tok_simple.
 *
 *      If cti is not exactly on the token, then insert the keyword exactly
 *      where it is -- no spaces need to be removed or added.
 *
 *      If the cti is on the token, insert the keyword and one space.
 *
 * The second issue is where to leave the cursor...  if the command is now
 * complete, wish to leave the cursor at the end of the newly completed
 * keyword.  Otherwise:
 *
 *   1) if the next token is a cmd_tok_simple, move to it.
 *
 *   2) otherwise if there is already a space after the newly completed
 *      keyword, move past it.
 *
 *      Otherwise, insert a space.
 *
 * Returns:  n = number of characters to move the cursor before
 *               starting to replace characters (may be -ve)
 *        *rep = number of characters to replace
 *        *ins = number of spaces to insert
 *        *mov = number of spaces to move afterwards (may be -ve)
 */
extern void
cmd_complete_keyword(cmd_parsed parsed, int* pre, int* rep, int* ins, int* mov)
{
  cmd_token   t, nt ;
  bool last ;
  int  gap ;

  /* Is this the last token possible on this command line ?             */
  last = cmd_post_last(parsed) ;

  /* Get the token we are operating and the following one (if any).
   *
   * Calculate gap between end of token to be replaced and start of next,
   * if any.
   *
   *   gap == 0 => is at eol or on !cmd_tok_simple.
   */
  t  = cmd_token_get(parsed->tokens, parsed->cti) ;
  if (parsed->cti < parsed->num_tokens)
    {
      nt = cmd_token_get(parsed->tokens, parsed->cti + 1) ;
      gap = nt->tp - (t->tp + els_len_nn(t->ot)) ;
    }
  else
    {
      nt = NULL ;
      gap = (parsed->rp < 0) ? -parsed->rp : 0 ;
    } ;

  /* Now work out what we need to do.                                   */

  *pre = 0 ;            /* preset values        */
  *rep = 0 ;
  *ins = 0 ;
  *mov = 0 ;

  if      ((t->type & cmd_tok_simple) != 0)
    {
      /* Replacing an existing simple token ------------------------------
       *
       * Move to start of token and replace it.
       */
      *pre = -parsed->rp ;
      *rep =  parsed->ctl  ;

      /* now what do we do after the token ?                    */
      assert(nt != NULL) ;

      if      ((nt->type & cmd_tok_simple) != 0)
        {
          /* Next token is simple -- step to it                 */
          assert(!last) ;

          *mov  = gap ;
        }
      else if (nt->type == cmd_tok_eol)
        {
          /* Next token is eol
           *
           * gap is the number of spaces there will be after the
           * newly replaced token.
           */
          if (!last)
            {
              if (gap == 0)
                *ins = 1 ;      /* need a trailing space        */
              else
                *mov = 1 ;      /* step over existing space     */
            } ;
        }
      else
        {
          /* Next token is something special.
           *
           * gap is the number of spaces there will be after the
           * newly replaced token.
           */
          if      (gap == 0)
            {
              *ins = last ? 1 : 2 ;
              *mov = -1 ;
            }
          else if (gap == 1)
            {
              *ins = last ? 0 : 1 ;
            }
          else
            {
              *mov = last ? 0 : 1 ;
            }
        } ;
    }

  else if (t->type == cmd_tok_eol)
    {
      /* Inserting at or before eol --------------------------------------
       *
       * Insert exactly where we are -- *pre == 0
       */

       /* If last, replace any spaces between cursor and end of line,
        * otherwise, keep one of them.
        */
       *rep = last ? gap : (gap > 0) ? gap - 1 : gap ;

      /* now what do we do after the token ?                    */
      assert(nt == NULL) ;

      if (!last)
        {
          if (gap == 0)
            *ins = 1 ;          /* need space after             */
          else
            *mov = 1 ;          /* step over one                */
        } ;
    }
  else
    {
      /* Inserting at or before something special ------------------------
       *
       * Insert exactly where we are -- *pre == 0
       *             replace nothing -- *rep == 0
       */
      if      (gap == 0)
        {
          *ins = last ? 1 : 2 ;
          *mov = -1 ;
        }
      else if (gap == 1)
        {
          *ins = last ? 0 : 1 ;
        }
      else
        {
          *mov = last ? 0 : 1 ;
        }
    } ;
} ;

/*------------------------------------------------------------------------------
 * Do we have just one command left, and have we just filtered on the last
 * possible command item & token ?
 *
 * If so, then there is no point filtering any further, and there is nothing
 * more that could be added to this command line.
 */
static bool
cmd_post_last(cmd_parsed parsed)
{
  if (vector_length(parsed->cmd_v) == 1)
    {
      cmd_command cmd ;
      cmd = vector_get_item(parsed->cmd_v, 0) ;

      return ((parsed->cti - parsed->first_command + 1) == cmd->nt_max) ;
    } ;

  return false ;
} ;

















#if 0
static vector
cmd_complete_command_real (vector tokens, int node, int *status)
{
  unsigned int i;
  unsigned int ivl ;
  unsigned int last_ivl ;
  vector cmd_v ;
#define INIT_MATCHVEC_SIZE 10
  vector matchvec;
  struct cmd_command *cmd_command;
  unsigned int index;
  struct cmd_item *cmd_item;
  vector descvec;
  char *token;
  int n ;

  /* Stop immediately if the tokens is empty.                           */
  if (vector_length (tokens) == 0)
    {
      *status = CMD_ERR_NO_MATCH;
      return NULL;
    }

  /* Take (shallow) copy of cmdvec for given node.                      */
  cmd_v = vector_copy (cmd_node_vector (cmdvec, node));

  /* First, filter upto, but excluding last token                       */
  last_ivl = vector_length (tokens) - 1;

  for (ivl = 0; ivl < last_ivl; ivl++)
    {
      enum match_type match;
      int ret;

      /* TODO: does this test make any sense ?                          */
      if ((token = vector_get_item (tokens, ivl)) == NULL)
        continue ;

      /* First try completion match, return best kind of match          */
      index = ivl ;
      match = cmd_filter_by_completion (token, cmd_v, index) ;

      /* Eliminate all but the selected kind of match                   */
      ret = is_cmd_ambiguous (token, cmd_v, index, match) ;

      if (ret == 1)
        {
          /* ret == 1 => either token matches more than one keyword
           *                 or token matches more than one number range
           */
          vector_free (cmd_v);
          *status = CMD_ERR_AMBIGUOUS;
          return NULL;
        }
#if 0
      /* For command completion purposes do not appear to care about
       * incomplete ipv4 or ipv6 prefixes (missing '/' or digits after).
       */
      else if (ret == 2)
        {
          vector_free (cmd_v);
           *status = CMD_ERR_NO_MATCH;
           return NULL;
        }
#endif
      }

  /* Prepare match vector.                                              */
  matchvec = vector_init (INIT_MATCHVEC_SIZE);

  /* Now we got into completion                                         */
  index = last_ivl ;
  token = vector_get_item(tokens, last_ivl) ;  /* is now the last token  */

  for (i = 0; i < vector_length (cmd_v); i++)
    {
      unsigned int j;
      const char *string;

      if ((cmd_command = vector_get_item (cmd_v, i)) == NULL)
        continue ;

      descvec = vector_get_item (cmd_command->items, index);
      if (descvec == NULL)
        continue ;

      for (j = 0; j < vector_length (descvec); j++)
        {
          item = vector_get_item (descvec, j) ;
          if (item == NULL)
            continue ;

          string = cmd_entry_function(token, item->str) ;
          if ((string != NULL) && cmd_unique_string(matchvec, string))
            cmd_add_to_strvec (matchvec, string) ;
        } ;
    } ;

  n = vector_length(matchvec) ; /* number of entries in the matchvec    */

  /* We don't need cmd_v any more.                                      */
  vector_free (cmd_v);

  /* No matched command */
  if (n == 0)
    {
      vector_free (matchvec);

      /* In case of 'command \t' pattern.  Do you need '?' command at
         the end of the line. */
      if (*token == '\0')
        *status = CMD_COMPLETE_ALREADY;
      else
        *status = CMD_ERR_NO_MATCH;
      return NULL;
    }

  /* Only one matched                                                   */
  if (n == 1)
    {
      *status = CMD_COMPLETE_FULL_MATCH;
      return matchvec ;
    }

  /* Check LCD of matched strings.                                      */
  if (token != NULL)
    {
      unsigned lcd = cmd_lcd (matchvec) ;

      if (lcd != 0)
        {
          if (strlen(token) < lcd)
            {
              char *lcdstr;

              lcdstr = XMALLOC (MTYPE_STRVEC, lcd + 1);
              memcpy (lcdstr, vector_get_item(matchvec, 0), lcd) ;
              lcdstr[lcd] = '\0';

              cmd_free_strvec(matchvec) ;      /* discard the match vector */

              matchvec = vector_init (1);
              vector_push_item(matchvec, lcdstr) ;

              *status = CMD_COMPLETE_MATCH;
              return matchvec ;
            }
        }
    }

  *status = CMD_COMPLETE_LIST_MATCH;
  return matchvec ;
}
#endif

/*------------------------------------------------------------------------------
 * Can the current command be completed ?
 */
extern vector
cmd_complete_command (vector tokens, int node, int *status)
{
#if 0
  vector ret;

  if ( cmd_try_do_shortcut(node, vector_get_item(tokens, 0) ) )
    {
      vector shifted_tokens;
      unsigned int index;

      /* We can try it on enable node, cos' the vty is authenticated */

      shifted_tokens = vector_init (vector_count(tokens));
      /* use memcpy? */
      for (index = 1; index < vector_length (tokens); index++)
        {
          vector_set_index (shifted_tokens, index-1,
                                                 vector_lookup(tokens, index)) ;
        }

      ret = cmd_complete_command_real (shifted_tokens, ENABLE_NODE, status);

      vector_free(shifted_tokens);
      return ret;
  }

  return cmd_complete_command_real (tokens, node, status);
#endif

  *status = CMD_ERR_NO_MATCH;
  return NULL;
} ;







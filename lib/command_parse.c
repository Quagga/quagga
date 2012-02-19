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
#include "list_util.h"
#include "elstring.h"

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
static void cmd_set_str(cmd_item n, const char* str) ;

/*------------------------------------------------------------------------------
 * Table of known "words" -- so that a word can be represented by its
 * address in this table and its length.
 *
 * See: cmd_set_str()
 */
enum { word_lump_length = 500 * 8 } ;           /* plenty ?     */
enum { command_specification_length = 500 } ;   /* plenty !!    */

typedef struct word_lump* word_lump ;

struct word_lump
{
  word_lump  next ;

  char*      end ;   /* points at the terminating '\0'       */
  char       words[word_lump_length] ;
} ;

static struct dl_base_pair(word_lump) word_lumps = INIT_DL_BASE_PAIR ;

/*------------------------------------------------------------------------------
 * Dummy eol_item -- completed in cmd_parse_init()
 */
static struct cmd_item eol_item =
{
  .str    = ELSTRING_INIT,      /* see cmd_parse_init()         */
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
  const char* p ;
  char* cp ;
  char* qp ;
  char* dp ;
  bool  opt ;
  bool  vararg ;

  char  spec[command_specification_length + 1] ;

  /* Initialise the compiled version of the command                     */

  assert(cmd->r_doc  == NULL) ;

  cmd->items    = vector_init_new(NULL, 10) ; /* plenty !       */
  cmd->nt_min   = 0 ;
  cmd->nt       = 0 ;
  cmd->nt_max   = 0 ;
  cmd->vararg   = NULL ;
  cmd->r_doc    = XSTRDUP(MTYPE_CMD_STRING, cmd->doc) ;  /* NULL => "" */

  if (strlen(cmd->string) > command_specification_length)
    cmd_fail_item(cmd, "command specification *too* long") ;

  /* Simplify the command line string by replacing TABs by spaces, and barfing
   * on control characters.  Strip leading and trailing spaces and any spaces
   * between brackets -- checking for matching brackets -- and squeeze out
   * multiple spaces.
   */
  qp = spec ;
  p  = cmd->string ;

  while ((*p == ' ') || (*p == '\t'))
    ++p ;                       /* squeeze out leading spaces           */

  while (*p != '\0')            /* starts with not ' ' and not '\t'     */
    {
      if      (!iscntrl(*p))
        *qp = *p ;
      else if (*p == '\t')
        *qp = ' ' ;
      else
        cmd_fail_item(cmd, "improper control character in string") ;

      if ((*qp != ' ') || (*(qp - 1) != ' '))
        ++qp ;                  /* squeeze out multiple spaces          */

      ++p ;
    } ;

  while ((qp > spec) && (*(qp - 1) == ' '))
    --qp ;                      /* squeeze out trailing spaces          */

  *qp = '\0' ;

  cp = spec ;
  qp = spec ;
  while (*cp != '\0')
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

      *qp++ = *cp++ ;
    } ;

  *qp = '\0' ;          /* terminate reduced string             */

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

  *qp  = '\0' ;         /* terminate reduced string             */

  /* Processing loop                                                    */

  cp = spec ;
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
                    repeat = (els_body_nn(n->str) == els_body_nn(p->str))
                                 && (els_len_nn(n->str) == els_len_nn(p->str)) ;
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

  /* Require the following to be set to something
   *
   * cmd->func may be NULL if this is a command entry for parsing only -- as
   * in the complete table of commands collected for vtysh.
   */
  if (   (cmd->string    == NULL)
      || (cmd->items     == NULL)
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
   *   * str     = NULL        -- set below
   *   * str_len = 0           -- set below
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

  cmd_set_str(n, cp) ;
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
                       || (*cp == '/')          // TODO pro tem ???
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
        return els_cmp((*a)->str, (*b)->str) ;
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

/*------------------------------------------------------------------------------
 * Set the str and str_len entries for a given item.
 *
 * The str entry of a cmd_item is set to point at a pool of known values.
 * This means that where two str entries are the same, they will have the
 * same address !
 */
static void
cmd_set_str(cmd_item n, const char* str)
{
  uint      len ;
  word_lump lump ;
  char*     word ;

  len = strlen(str) ;

  lump = dsl_head(word_lumps) ;
  while (1)
    {
      if (lump == NULL)
        {
          lump = XCALLOC(MTYPE_CMD_STRING, sizeof(struct word_lump)) ;
          lump->end = lump->words ;

          dsl_append(word_lumps, lump, next) ;
        } ;

      word = strstr(lump->words, str) ;
      if (word != NULL)
        break ;

      if (dsl_next(lump, next) != NULL)
        {
          lump = dsl_next(lump, next) ;
          continue ;
        } ;

      if (len >= (&lump->words[word_lump_length] - lump->end))
        {
          lump = NULL ;
          continue ;
        } ;

      word = lump->end ;
      memcpy(word, str, len) ;
      lump->end += len ;
      *lump->end = '\0' ;
      break ;
    } ;

  els_set_n_nn(n->str, word, len) ;
} ;

/*==============================================================================
 * Token objects
 */

/*------------------------------------------------------------------------------
 * Create a new, empty token_vector with room for a dozen arguments (initially).
 */
static token_vector
cmd_token_vector_new(void)
{
  token_vector tv ;

  tv = XCALLOC(MTYPE_CMD_PARSED, sizeof(token_vector_t)) ;

  vector_init_new(tv->body, 12) ;

  return tv ;
} ;

/*------------------------------------------------------------------------------
 * Empty token_vector and release all memory if required.
 */
static token_vector
cmd_token_vector_free(token_vector tv)
{
  if (tv != NULL)
    {
      cmd_token t ;

      /* Give back all the token objects and release vector body        */
      while ((t = vector_ream(tv->body, keep_it)) != NULL)
        {
          qs_reset(t->qs, keep_it) ;    /* discard body of qstring      */
          XFREE(MTYPE_TOKEN, t) ;
        } ;

      XFREE(MTYPE_CMD_PARSED, tv) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Get string value of given token.
 *
 * Returns an empty (not NULL) string if token NULL or no string.
 */
inline static char*
cmd_token_make_string(cmd_token t)
{
  qassert((t != NULL) && (t->qs != NULL)) ;

  if (t->term)
    return qs_char_nn(t->qs) ;
  else
    {
      t->term = true ;
      return qs_make_string(t->qs) ;
    }
} ;

/*------------------------------------------------------------------------------
 * Set i'th token from given token vector -- zero origin
 */
inline static void
cmd_token_set(token_vector tv, vector_index_t i,
                      cmd_token_type_t type, const char* p, usize len, usize tp)
{
  cmd_token t = cmd_token_get(tv, i) ;

  if (t == NULL)
    {
      /* Make a brand new token object                                  */
      t = XCALLOC(MTYPE_TOKEN, sizeof(struct cmd_token)) ;

      /* Zeroising the new structure sets:
       *
       *   type       = 0                 -- cmd_tok_eol
       *   qs         = zeroised qstring  -- empty string
       *   complete   = 0 -- false
       *
       *   tp         = 0
       *   lp         = zeroised elstring -- empty string
       */
      confirm(cmd_tok_eol == 0) ;
      confirm(QSTRING_INIT_ALL_ZEROS) ;
      confirm(ELSTRING_INIT_ALL_ZEROS) ;

      vector_set_item(tv->body, i, t) ;
    } ;

  t->type     = type ;
  t->term     = false ;
  t->tp       = tp ;

  qs_set_alias_n(t->qs, p, len) ;
  qs_els_copy_nn(t->ot, t->qs) ;

  t->w_len    = 0 ;             /* no words matched to, yet     */
  t->seen     = 0 ;             /* no matches attempted, yet    */
} ;


/*------------------------------------------------------------------------------
 * Get one or more original token values, concatenated with space between each.
 *
 * Returns a brand new qstring that must be discarded after use.
 */
extern qstring
cmd_tokens_concat(cmd_parsed parsed, uint ti, uint nt)
{
  cmd_token t ;
  qstring   qs ;

  assert(nt >= 2) ;

  t  = cmd_token_get(parsed->tokens, ++ti) ;
  qs = qs_set_els(NULL, t->ot) ;

  while (--nt >= 2)
    {
      t  = cmd_token_get(parsed->tokens, ++ti) ;
      qs_append_str(qs, " ") ;
      qs_append_els(qs, t->ot) ;
    } ;

  return qs ;
} ;

/*==============================================================================
 * Argument vector
 */

/*------------------------------------------------------------------------------
 * Create a new, empty arg_vector with room for a dozen arguments (initially).
 */
static arg_vector
cmd_arg_vector_new(void)
{
  arg_vector args ;

  args = XCALLOC(MTYPE_CMD_PARSED, sizeof(arg_vector_t)) ;

  vector_init_new(args->body, 12) ;

  return args ;
} ;

/*------------------------------------------------------------------------------
 * Empty arg_vector and release all memory if required.
 */
static arg_vector
cmd_arg_vector_free(arg_vector args)
{
  if (args != NULL)
    {
      vector_reset(args->body, keep_it) ;
      XFREE(MTYPE_CMD_PARSED, args) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Empty the body of the arg_vector object in cmd_parsed.
 */
Inline void
cmd_arg_vector_empty(cmd_parsed parsed)
{
  vector_set_length(parsed->args->body, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Push argument to the argument vector.
 */
Inline void
cmd_arg_vector_push(cmd_parsed parsed, char* arg)
{
  vector_push_item(parsed->args->body, arg) ;
} ;

/*==============================================================================
 * Parser object
 */

/*------------------------------------------------------------------------------
 * Allocate and initialise a new cmd_parsed object
 */
extern cmd_parsed
cmd_parsed_new(void)
{
  cmd_parsed parsed ;

  parsed = XCALLOC(MTYPE_CMD_PARSED, sizeof(cmd_parsed_t)) ;

  /* Zeroising the structure has set:
   *
   *   tok_total   = 0          -- set by cmd_tokenize()
   *   num_tokens  = 0          -- set by cmd_tokenize()
   *   tokens      = NULL       -- filled in by cmd_tokenize() -- see below
   *
   *   parts       = 0          -- set by cmd_parse_command()
   *
   *   cmd         = NULL       -- no command yet
   *   xnode       = 0          -- not set
   *   cnode       = 0          -- not set
   *   nnode       = 0          -- not set
   *
   *   args        = NULL       -- see below
   *
   *   emess       = NULL       -- no error yet
   *   eloc        = 0          -- no error location
   *
   *   in_pipe     = cmd_pipe_none
   *   out_pipe    = cmd_pipe_none
   *
   *   first_action   )  none
   *   num_action     )
   *   first_out_pipe )
   *   num_out_pipe   )
   *   first_comment  )
   *   num_comment    )
   *
   *   cti            ) set by cmd_token_position()
   *   rp             )
   *
   *   cmd_v       = NULL       -- no vector of filtered commands
   *   item_v      = NULL       -- no vector of filtered items
   *
   *   strongest      )
   *   best_complete  ) set by cmd_filter_prepare()
   *   min_strength   )
   *   strict         )
   */
  confirm(cmd_pipe_none == 0) ;

  parsed->tokens = cmd_token_vector_new() ;
  parsed->args   = cmd_arg_vector_new() ;

  return parsed ;
} ;

/*------------------------------------------------------------------------------
 * Empty out and free a cmd_parsed object
 */
extern cmd_parsed
cmd_parsed_free(cmd_parsed parsed)
{
  if (parsed != NULL)
    {
      parsed->tokens = cmd_token_vector_free(parsed->tokens) ;
      parsed->args   = cmd_arg_vector_free(parsed->args) ;

      parsed->cmd_v  = vector_reset(parsed->cmd_v, free_it) ;
      parsed->item_v = vector_reset(parsed->item_v, free_it) ;

      parsed->emess  = qs_reset(parsed->emess, free_it) ;

      XFREE(MTYPE_CMD_PARSED, parsed) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 * Parsing error handling.
 *
 *
 *
 */
static cmd_ret_t
cmd_set_parse_error(cmd_parsed parsed, cmd_token t, usize off,
                               const char* format, ...) PRINTF_ATTRIBUTE(4, 5) ;

/*------------------------------------------------------------------------------
 * Register a parsing error.
 *
 * Takes token in which parsing error was detected, and an offset from the
 * start of that, for the location of the error.  If the offset is not zero,
 * it must be an offset in the original token (!).
 *
 * The error message is constructed as if by sprintf(), and stored in the
 * parsed->emess.
 *
 * The message will be output as: ..........^ pointing to the location
 *                   followed by: % <emess>\n
 *
 * Note that the '%' and the trailing '\n' are added automatically.
 *
 * Sets:  parsed->emess
 *        parsed->eloc    -- set -ve if no known location
 *
 * Returns: CMD_ERR_PARSING
 */
static cmd_ret_t
cmd_set_parse_error(cmd_parsed parsed, cmd_token t, usize off,
                                                        const char* format, ...)
{
  va_list args ;

  qs_clear(parsed->emess) ;

  va_start (args, format);
  parsed->emess = qs_vprintf(parsed->emess, format, args);
  va_end (args) ;

  if (t != NULL)
    parsed->eloc   = t->tp + off ;
  else
    parsed->eloc   = -1 ;

  return CMD_ERR_PARSING ;
} ;

/*------------------------------------------------------------------------------
 * Output parsing error message to given fifo.
 *
 * The message will be output as: ..........^ pointing to the location
 *                   followed by: % <emess>\n
 *
 * The ".....^" is omitted if no location is known, or if the indent is zero.
 *
 * Uses:  parsed->emess
 *        parsed->eloc
 */
extern void
cmd_get_parse_error(vio_fifo ebuf, cmd_parsed parsed, uint indent)
{
  if ((parsed->eloc >= 0) && (indent > 0))
    {
      qstring here ;

      here = qs_set_fill(NULL, indent + parsed->eloc, "....") ;
      vio_fifo_put_bytes(ebuf, qs_body_nn(here), qs_len_nn(here)) ;
      qs_free(here) ;
      vio_fifo_printf(ebuf, "^\n") ;
    } ;

  vio_fifo_printf(ebuf, "%% %s\n", qs_string(parsed->emess)) ;
} ;

/*==============================================================================
 * Lexical level stuff
 */

/*------------------------------------------------------------------------------
 * Take qstring and see if it is empty -- only whitespace and/or comment
 */
extern bool
cmd_is_empty(qstring line)
{
  cpp_t lp ;

  qs_cpp(lp, line) ;            /* NULL -> NULL         */

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
static bool
cmd_pipe_reserved_char(char ch)
{
  return strchr("<|>%&*+-=?#!", ch) != NULL ;
} ;

/*------------------------------------------------------------------------------
 * Take qstring and break it into tokens.
 *
 * Expects string to have been preprocessed, if required, to ensure that any
 * unwanted control characters have been removed.  This code only recognises
 * '\t' and treats it as whitespace (so any other control characters will
 * end up as part of a token).
 *
 * Ignores leading and trailing ' ' or '\t' (whitespace).
 *
 * Have "full_lex" flag -- to distinguish old and new.
 *
 * If not full_lex, we have this simple tokenization:
 *
 *   * tokens are separated by one or more whitespace characters.
 *
 *   * if the first non-whitespace character is '!', this is a comment line.
 *
 *   * if the first non-whitespace character is '#', this is a meta command
 *     or a comment line.
 *
 *     This is an extension to previous lex rules -- where '#' would be an
 *     error -- required to implement the vtysh #daemon etc. meta commands.
 *
 * For the "full_lex" we more or less follow the usual shell conventions,
 * except:
 *
 *   * if '#' is the first non-whitespace character, then if it is
 *     *immediately* followed by an alphabetic or a '_', then that is the
 *     start of a meta command.
 *
 *   * can have '!' as well as '#' to start comments (at start of token)
 *
 *   * may only have '|' at the start of a line.  '>|' must be used for
 *     piping command output to shell command.  So '|' is, effectively, a
 *     "shell command prefix".
 *
 *     Limiting '|' in this way removes problems with regular expressions.
 *
 *     Also allows a <| shell to use pipes !!
 *
 * So the "full_lex" will:
 *
 *   * ignore anything between '....' -- as per shell convention, '\' is
 *     ignored and there is no way to include "'" in a single quoted string.
 *
 *   * ignore anything immediately preceded by '\' -- including space
 *     (or tab converted to a space) and double quote characters.
 *
 *   * ignore anything between "...." -- including '\"' escapes.
 *
 *   * unbalanced "'" or '"' are treated as if eol was a "'" or '"'.
 *
 * Except when ignored by the rules above, the "full lex" will recognise
 * the following:
 *
 *   * tokens are separated by whitespace -- one ' ' or '\t' characters.
 *     Whitespace before, after or between tokens is not part of the tokens.
 *
 *   * the character '|' is significant at the start of a line (after any
 *     leading whitespace) only.  It is then the start of an out_pipe
 *     token -- so self terminates after any pipe_reserved_chars.
 *
 *   * the character '#' at the start of a line (after any leading whitespace),
 *     followed by zero or more '_' and one or more alphabetic characters,
 *     signals the start of a meta command.  The '#' and any '_' are treated
 *     as an (insignificant) leading token, and the rest of the line is parsed
 *     as a command line.
 *
 *   * the characters '!', '#' are significant only at the start of a token,
 *     and then from there to end of line is comment (except as above for '#').
 *
 *     Note that this means that comment after a token must be separated by
 *     at least one space from that token.
 *
 *   * the characters '<' and '>' are separators -- they terminate any
 *     preceding token.  They are in_pipe or out_pipe tokens, and self
 *     terminate after any pipe_reserved_chars.
 *
 * The tokenization does not remove any " ' or \ characters, that is left for
 * a later stage, where context may affect the handling.
 *
 * The tokens returned contain all the original characters of the line, except
 * for the removal of ' ' and '\t' between tokens and at the end of the line.
 *
 * NB: the elstring containing the line to be tokenized MUST NOT change
 *     until the parsed object is finished with.
 *
 * Sets the following parsed object values, ready for further parsing:
 *
 *   parsed->tokens      -- the tokens, including an eol token
 *   parsed->num_tokens  -- number of tokens, *excluding* the eol token
 *   parsed->tok_total   -- all the token types seen (excluding eol)
 */
extern void
cmd_tokenize(cmd_parsed parsed, qstring line, bool full_lex)
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
              if (full_lex)
                {
                  type |= cmd_tok_sq ;
                  while (cp < lp->e)
                    {
                      if (*cp++ == '\'')
                        break ;
                    } ;
                } ;
              break ;

            case '\\':          /* step past escaped character, if any  */
              ++cp ;
              if (full_lex)
                {
                  type |= cmd_tok_esc ;
                  if (cp < lp->e)
                    ++cp ;
                } ;
              break ;

            case '"':           /* proceed to matching " or end...      */
              ++cp ;
              if (full_lex)
                {
                  type |= cmd_tok_dq ;
                  while (cp < lp->e)  /* NB: do not register \ separately   */
                    {
                      if (*cp++ == '"')
                        if (*(cp - 2) != '\\')    /* ignore escaped "       */
                          break ;
                    } ;
                } ;
              break ;

            case '|':           /* only at start of line                */
              if (full_lex && (nt == 0) && (cp == sp))
                {
                  type = cmd_tok_out_shell ;
                  do ++cp ;
                    while ((cp < lp->e) && cmd_pipe_reserved_char(*cp)) ;
                  end = true ;
                }
              else
                ++cp ;
              break ;

            case '>':           /* '>' is a separator                   */
              if (full_lex)
                {
                  if (cp == sp)
                    {
                      type = cmd_tok_out_pipe ;
                      do ++cp ;
                        while ((cp < lp->e) && cmd_pipe_reserved_char(*cp)) ;
                    } ;
                  end = true ;
                }
              else
                ++cp ;
              break ;

            case '<':           /* '<' is a separator                   */
              if (full_lex)
                {
                  if (cp == sp)
                    {
                      type = cmd_tok_in_pipe ;
                      do ++cp ;
                        while ((cp < lp->e) && cmd_pipe_reserved_char(*cp)) ;
                    } ;
                  end = true ;
                }
              else
                ++cp ;
              break ;

            case '#':
              if ((nt == 0) && (cp == sp))
                {
                  const char* p ;

                  p = cp + 1 ;
                  while (*p == '_')
                    ++p ;

                  if (isalpha(*p))
                    {
                      type = cmd_tok_meta_prefix ;
                      cp  = p ;
                      end = true ;
                      break ;
                    } ;
                } ;

              if (!full_lex)
                {
                  ++cp ;
                  break ;
                } ;
              fall_through ;    /* treat as '!'.                        */

            case '!':           /* '!' and '#' special at token start   */
              if ((cp == sp) && (full_lex || (nt == 0)))
                {
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
   * Record the result of tokenisation and insert trailing eol token.  The
   * eol token contains any whitespace between the end of the last token and
   * the end of line.
   */
  parsed->tok_total  = total ;
  parsed->num_tokens = nt ;

  /* Append an empty end of line token.                                 */
  cmd_token_set(parsed->tokens, parsed->num_tokens, cmd_tok_eol,
                                                      lp->e, 0, lp->e - lp->p) ;
} ;

/*==============================================================================
 * VTY command line special token handling.
 */

/*------------------------------------------------------------------------------
 * Get next cpp char & step -- unless at end of cpp.
 */
inline static char
cpp_getch(cpp p)
{
  if (p->p < p->e)
    return *p->p++ ;
  else
    return '\0' ;
}

/*------------------------------------------------------------------------------
 * Process in-pipe token and set the required bits in the pipe type word
 *
 * Known tokens are:   <  <| << <<|
 *
 * where << and <<| force lexical level 0 and requires strict keywords.
 *
 * Known options are:
 *
 *    +  -- enable  reflect command line
 *    -  -- disable reflect command line
 *    !  -- enable  warning output  & enable  stop on warning
 *    %  -- enable  warning output  & disable stop on warning
 *    =  -- enable  ordinary output
 *    #  -- disable ordinary output
 *    *  -- disable reflect command line
 *        & disable warning output & disable stop on warning
 *        & disable ordinary output  (quiet running)
 *
 * Rejects: more than one of either of '+' or '-'
 *          more than one of either of '!' or '%'
 *          more than one of either of '=' or '#'
 *          more than one '*'
 *
 * Can combine '*' with '+', '!', '%' or '=' -- which has the meaning of
 * disabling everything, then adding back enabled features.
 *
 * Can combine '*' with '-' or '#', but the latter are redundant.
 */
static cmd_ret_t
cmd_parse_in_pipe(cmd_parsed parsed, cmd_token t)
{
  cpp_t p ;
  bool ok ;

  els_cpp(p, t->ot) ;

  parsed->in_pipe = cmd_pipe_none ;

  ok = (cpp_getch(p) == '<') ;  /* must start with '<'  */

  if (ok)
    switch (cpp_getch(p))
      {
        default:                /* "<" not followed by '\0', '<' or '|' */
          --p->p ;
          fall_through ;

         case '\0':             /* "<" all on its own                   */
           parsed->in_pipe = cmd_pipe_file ;
           break ;

         case '|':              /* "<|..."                              */
           parsed->in_pipe = cmd_pipe_shell ;
           break ;

         case '<':              /* "<<..."                              */
           switch (cpp_getch(p))
             {
               default:         /* "<<" not followed by '|'             */
                 --p->p ;
                 fall_through ;

               case '\0':      /* "<<..."                              */
                 parsed->in_pipe = cmd_pipe_file  | cmd_pipe_strict ;
                 break ;

               case '|':       /* "<<|..."                             */
                 parsed->in_pipe = cmd_pipe_shell | cmd_pipe_strict ;
                 break ;
             } ;
           break ;
       } ;

  while (ok && (p->p < p->e))
    switch (*p->p++)
      {
        case '+':
          ok = (parsed->in_pipe &
                 (cmd_pipe_reflect_enable  | cmd_pipe_reflect_disable)) == 0 ;

          parsed->in_pipe |= cmd_pipe_reflect_enable ;
          break ;

        case '-':
          ok = (parsed->in_pipe &
                   (cmd_pipe_reflect_enable  | cmd_pipe_reflect_disable)) == 0 ;

          parsed->in_pipe |= cmd_pipe_reflect_disable ;
          break ;

        case '!':
          ok = (parsed->in_pipe &
                        (cmd_pipe_warn_stop  | cmd_pipe_warn_continue)) == 0 ;

          parsed->in_pipe |= cmd_pipe_warn_stop ;
          break ;

        case '%':
          ok = (parsed->in_pipe &
                        (cmd_pipe_warn_stop  | cmd_pipe_warn_continue)) == 0 ;

          parsed->in_pipe |= cmd_pipe_warn_continue ;
          break ;

        case '=':
          ok = (parsed->in_pipe &
                        (cmd_pipe_out_enable | cmd_pipe_out_disable)) == 0 ;

          parsed->in_pipe |= cmd_pipe_out_enable ;
          break ;

        case '#':
          ok = (parsed->in_pipe &
                        (cmd_pipe_out_enable | cmd_pipe_out_disable)) == 0 ;

          parsed->in_pipe |= cmd_pipe_out_disable ;
          break ;

        case '*':
          ok = (parsed->in_pipe & cmd_pipe_quiet) == 0 ;

          parsed->in_pipe |= cmd_pipe_quiet ;
          break ;

        default:
          ok = false ;
          break ;
      } ;

  if (ok)
    return CMD_SUCCESS ;

  return cmd_set_parse_error(parsed, t, 0, "invalid 'pipe in'") ;
} ;

/*------------------------------------------------------------------------------
 * Process out-pipe token and set the required bits in the pipe type word
 *
 * Known tokens are:  | >  >>  >| >*
 *
 * Known options for | and >| are:
 *
 *    &  -- enable  stderr return
 *    ~  -- disable stderr return
 *    =  -- enable  stdout return
 *    #  -- disable stdout return
 *    *  -- disable stdout and stderr returns
 *
 * Rejects: more than one of either of '&' or '~'
 *          more than one of either of '=' or '#'
 *          more than one '*'
 *
 * Can combine '*' with '&' or '=' -- which has the meaning of disabling
 * everything, then adding back enabled features.
 *
 * Can combine '*' with '~' or '#', but the latter are redundant.
 *
 * No options for > >> >*
 */
static cmd_ret_t
cmd_parse_out_pipe(cmd_parsed parsed, cmd_token t)
{
  cpp_t p ;
  bool  ok ;

  els_cpp(p, t->ot) ;

  ok = true ;

  switch (cpp_getch(p))
  {
    case '|':
      parsed->out_pipe = cmd_pipe_shell | cmd_pipe_shell_cmd ;
      break ;

    case '>':
      switch (cpp_getch(p))
      {
        default:
          --p->p ;
          fall_through ;

        case '\0':
          parsed->out_pipe = cmd_pipe_file ;
          break ;

        case '>':
          parsed->out_pipe = cmd_pipe_file | cmd_pipe_append ;
          break ;

        case '|':
          parsed->out_pipe = cmd_pipe_shell ;
          break ;

        case '*':
          parsed->out_pipe = cmd_pipe_dev_null ;
          break ;
      } ;
      break ;

    default:
      ok = false ;
      break ;
  } ;

  /* only the pipe shells have any options.
   */
  ok = (p->p == p->e) || (parsed->out_pipe & cmd_pipe_shell) ;

  while (ok && (p->p < p->e))
    {
      switch (*p->p++)
        {
          case '&':
            ok = (parsed->out_pipe &
                     (cmd_pipe_stderr_enable  | cmd_pipe_stderr_disable)) == 0 ;

            parsed->out_pipe |= cmd_pipe_stderr_enable ;
            break ;

          case '~':
            ok = (parsed->out_pipe &
                     (cmd_pipe_stderr_enable  | cmd_pipe_stderr_disable)) == 0 ;

            parsed->out_pipe |= cmd_pipe_stderr_disable ;
            break ;

          case '=':
            ok = (parsed->out_pipe &
                     (cmd_pipe_stdout_enable  | cmd_pipe_stdout_disable)) == 0 ;

            parsed->out_pipe |= cmd_pipe_stdout_enable ;
            break ;

          case '#':
            ok = (parsed->out_pipe &
                     (cmd_pipe_stdout_enable  | cmd_pipe_stdout_disable)) == 0 ;

            parsed->out_pipe |= cmd_pipe_stdout_disable ;
            break ;

          case '*':
            ok = (parsed->out_pipe & cmd_pipe_std_quiet) == 0 ;

            parsed->out_pipe |= cmd_pipe_std_quiet ;
            break ;

          default:
            ok = false ;
            break ;
        } ;
    } ;

  if (ok)
    return CMD_SUCCESS ;

  return cmd_set_parse_error(parsed, t, 0, "invalid 'pipe out'") ;
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
 * NB: with the exception of $ inside of "..." this carefully avoids the
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
static cmd_ret_t
cmd_token_complete(cmd_parsed parsed, cmd_token t)
{
  cpp_t     p ;
  pp_t      q ;
  bool      dq ;
  cmd_ret_t ret ;

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
                  ret = cmd_set_parse_error(parsed, t, 0, "missing closing '") ;
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
            ret = cmd_set_parse_error(parsed, t, 0, "trailing \\") ;
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
    ret = cmd_set_parse_error(parsed, t, 0, "missing closing \"") ;

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
 * Tokenize the given line and work out where cursor is wrt tokens.
 *
 * Note that this is implicitly "full lex".
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

  /* Re-initialise parsed object and tokenize the given line
   *
   * Resets to no parts at all.  Generally, should first look at parsed->parts
   * to see if any of the other values are valid.  Here we make life a touch
   * easier.
   */
  cmd_tokenize(parsed, line, true) ;

  parsed->parts    = cmd_parts_none ;

  parsed->in_pipe  = cmd_pipe_none ;
  parsed->out_pipe = cmd_pipe_none ;

  parsed->first_action   = 0 ;
  parsed->num_action     = 0 ;
  parsed->first_out_pipe = 0 ;
  parsed->first_comment  = 0 ;
  parsed->num_comment    = 0 ;

  /* Look for the last token whose end is <= cp
   *
   * Will position on last, "eol" token -- which is not counted in
   * parsed->num_tokens -- if is beyond the last real token on the line.
   */
  cp = qs_cp_nn(line) ;

  t  = NULL ;
  ti = 0 ;
  while (1)
    {
      t  = cmd_token_get(parsed->tokens, ti) ;

      if (cp > t->tp)
        {
          /* As soon as we are past '|', '<', '>', '!' or '#'
           * return "special"
           */
          if ((t->type & ( cmd_tok_in_pipe
                         | cmd_tok_out_pipe
                         | cmd_tok_out_shell
                         | cmd_tok_comment) ) != 0)
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
  match_type_t     mt ;

  if ((t->seen & item_ipv4_address_bit) != 0)
    return t->match[item_ipv4_address] ;

  ms = cmd_ipv4_match(cmd_token_make_string(t), 0) ;

  if      (ms == ms_var_complete)
    mt = mt_ipv4_address_complete ;
  else if (ms == ms_partial)
    mt = mt_ipv4_address_partial ;
  else
    mt = mt_no_match ;

  t->seen |= item_ipv4_address_bit ;

  return t->match[item_ipv4_address] = mt ;
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
  match_type_t     mt ;

  if ((t->seen & item_ipv4_prefix_bit) != 0)
    return t->match[item_ipv4_prefix] ;

  ms = cmd_ipv4_match(cmd_token_make_string(t), 32) ;

  if      (ms == ms_var_complete)
    mt = mt_ipv4_prefix_complete ;
  else if (ms == ms_partial)
    mt = mt_ipv4_prefix_partial ;
  else
    mt = mt_no_match ;

  t->seen |= item_ipv4_prefix_bit ;

  return t->match[item_ipv4_prefix] = mt ;
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
  match_type_t     mt ;

  if ((t->seen & item_ipv6_address_bit) != 0)
    return t->match[item_ipv6_address] ;

  ms = cmd_ipv6_match(cmd_token_make_string(t), 0) ;

  if      (ms == ms_var_complete)
    mt = mt_ipv6_address_complete ;
  else if (ms == ms_partial)
    mt = mt_ipv6_address_partial ;
  else
    mt = mt_no_match ;

  t->seen |= item_ipv6_address_bit ;

  return t->match[item_ipv6_address] = mt ;
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
  match_type_t     mt ;

  if ((t->seen & item_ipv6_prefix_bit) != 0)
    return t->match[item_ipv6_prefix] ;

  ms = cmd_ipv6_match(cmd_token_make_string(t), 128) ;

  if      (ms == ms_var_complete)
    mt = mt_ipv6_prefix_complete ;
  else if (ms == ms_partial)
    mt = mt_ipv6_prefix_partial ;
  else
    mt = mt_no_match ;

  t->seen |= item_ipv6_prefix_bit ;

  return t->match[item_ipv6_prefix] = mt ;
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
cmd_filter_prepare(cmd_parsed parsed, cmd_context context)
{
  cmd_node cn ;
  vector   src_v ;
  uint     ci ;
  uint     nct ;
  ulen     src_v_len ;

  /* Get the current node -- MUST be complete
   */
  cn = cmd_get_cmd_node(parsed->cnode) ;

  /* get commands for the current node
   */
  src_v = cn->cmd_vector ;
  src_v_len = (src_v != NULL) ? vector_length(src_v) : 0 ;

  /* Empty the working commands vector, making sure big enough for the current
   * node's commands -- creates vector if required.
   *
   * Note that the cmd_v lives for as long as the parsed object, so will
   * grow over time to accommodate what is required.
   */
  parsed->cmd_v = vector_re_init(parsed->cmd_v, src_v_len) ;

  /* Filter out commands which are too short.
   *
   * For execution, can filter out commands which are too long.
   */
  nct = parsed->num_action ;

  for (ci = 0 ; ci < src_v_len ; ++ci)
  {
    cmd_command cmd ;

    cmd = vector_get_item(src_v, ci) ;

    if ( cmd->nt_max < nct)
      continue ;                /* ignore if too short  */

    if ((cmd->nt_min > nct) && context->parse_execution)
      continue ;                /* ignore if too long   */

    vector_push_item(parsed->cmd_v, cmd) ;
  } ;

  /* Other preparation for filtering
   *
   * The min_strength is set according to whether is for execution.  The
   * strongest match is set from that.  This means that any match which does
   * not meet the minimum criterion is automatically excluded.
   */
  parsed->min_strength  = context->parse_execution ? ms_min_execute
                                                   : ms_min_parse ;
  parsed->strict        = context->parse_strict ||
                                (context->parse_execution && cn->parse_strict) ;

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

  /* Decide which token we are trying to match.                         */
  if (ii < parsed->num_action)
    ti = parsed->first_action + ii ;   /* map to token index   */
  else
    {
      /* special case of filtering against the empty token at the end of
       * the command line -- this is for command line help stuff.
       *
       * Note that there may be out_pipe or even comment tokens in between,
       * so we here set the ti to the trailing eol token.
       */
      assert(ii == parsed->num_action) ;
      ti = parsed->num_tokens ;
    }
  t = cmd_token_get(parsed->tokens, ti) ;

  /* If we are keeping the items which are matched, set the item_v
   * empty and guess that may get one item per command -- creates the item_v
   * vector if required..
   *
   * Note that the item_v lives for as long as the parsed object, so will
   * grow over time to accommodate what is required.
   */
  if (keep_items)
    parsed->item_v = vector_re_init(parsed->item_v,
                                                 vector_length(parsed->cmd_v)) ;

  /* Work down the cmd_v, attempting to match cmd_items against the cmd_token.
   *
   * Keep in the cmd_v the commands for which we get a match.
   *
   * At the same time, if required, keep in the item_v all the items which have
   * matched.
   */
  c_keep = 0 ;
  i_keep = 0 ;

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
      mt = (item->type == item_eol) ? mt_eol : mt_eol_partial ;
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
          if ((t->word == els_body_nn(item->str))
                                        && (t->w_len == els_len_nn(item->str)))
            cw = t->cw ;
          else
            {
              cw = els_cmp_word(t->ot, item->str) ;
              t->word  = els_body_nn(item->str) ;
              t->w_len = els_len_nn(item->str) ;
              t->cw    = cw ;
            } ;

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
static cmd_ret_t cmd_parse_phase_one(cmd_parsed parsed, cmd_context context) ;
static cmd_ret_t cmd_parse_phase_one_b(cmd_parsed parsed) ;
static cmd_ret_t cmd_parse_phase_two(cmd_parsed parsed, cmd_context context) ;
static cmd_ret_t cmd_parse_specials(cmd_parsed parsed, cmd_context context) ;
static node_type_t cmd_auth_specials(cmd_context context, node_type_t target) ;

/*------------------------------------------------------------------------------
 * Parse a command in the given context.
 *
 * Requires command line to have been tokenized.  Fills in the rest of the
 * given struct cmd_parsed -- see its declaration -- in particular the "parts"
 * entry.
 *
 * The context has these node settings as we start to parse:
 *
 *   context->node   -- this is the node the command handler is in, and to
 *                      which all other context applies.
 *
 *   context->cnode  -- this is the mode in which to start parsing.
 *
 *                      This generally follows the context->node, but for
 *                      vtysh server, may be set to an ancestor of that.
 *
 * The parsing operation will (if successful) set:
 *
 *   parsed->xnode   -- "execution node": the node to set vty->node to while
 *                      executing the command.
 *
 *                      Generally this will be the same as context->node, but
 *                      where there is an implicit node transfer, this will
 *                      be the same as parsed->nnode.
 *
 *   parsed->nnode   -- "next node": the node to set context->node to iff
 *                      the command succeeds.
 *
 *                      This may be an explicit node transfer (eg. "enable",
 *                      "router bgp", "end", etc), or an implicit node
 *                      transfer (eg. "route-map" in RMAP_NODE).
 *
 *                      Note that all implicit node transfers are up to an
 *                      ancestor.
 *
 *   parsed->cnode   -- "command node": where the command comes from.
 *
 *                      Generally this will be the same as parsed->xnode, but
 *                      where a "do", an implicit "do" or a meta command is
 *                      being executed, this is an ENABLE node or META_NODE.
 *
 *   context->onode  )  these are especially for AUTH_ENABLE_NODE
 *   context->tnode  )  see cmd_parse_specials()
 *
 * Node transfers have to follow quite strict rules, to avoid tripping up in
 * general and in vtysh in particular:
 *
 *   * implicit node transfers
 *
 *     This is where a command is found in an ancestor of the current node.
 *     Successful execution of the command implicitly causes a node change up
 *     to the ancestor.
 *
 *     When the command executes, vty->node will be the ancestor -- copied from
 *     parsed->xnode.
 *
 *     The execution context of the ancestor is a subset of the descendant's
 *     context, so context does not need to be modified before executing the
 *     command.
 *
 *     If the command succeeds, it may change the execution context to whatever
 *     is valid for the vty->node.
 *
 *     If the command fails, it MUST NOT change the execution context, because
 *     will fall back to the original child node.
 *
 *   * explicit node transfers
 *
 *     This is where the successful execution of a command in the current node
 *     explicitly causes a node change, up or down.
 *
 *     When the command executes, vty->node will be the current node.  The
 *     parsed->nnode will be the node to which will transfer if succeeds.
 *
 *     If the command succeeds, it may change the execution context to whatever
 *     is valid for the parsed->nnode, and may change the vty->node to that.
 *
 *     If the command fails, it MUST NOT change the execution context, because
 *     will fall back to the original child node.
 *
 *   * consistent node transfers
 *
 *     Commands which cause node transfers must cause the same node transfer
 *     in whatever daemons they run in.
 *
 *     If this were not the case, the vtysh could get hopelessly lost.
 *
 *   * "do", implicit "do" and meta commands
 *
 *     These execute with vty->node and context set to the current node.  This
 *     means that ENABLE node and META_NODE commands must be capable of
 *     executing in any execution context !  If the command results in a node
 *     transfer, if successful, the above rules apply.  If the command does not
 *     change node, then it MUST NOT change the execution context.
 *
 * Returns:  CMD_SUCCESS => successfully parsed command, and the result is
 *                          in the given parsed structure, ready for execution.
 *
 *                           - parsed->xnode may not be the incoming node.
 *
 *                           - parsed->cnode may not be the incoming node.
 *
 *                           - parsed->nnode is set for when command succeeds.
 *
 *                           - parsed->parts is what was found
 *
 *                             NB: may be empty or comment only !
 *
 *                           - parsed->cmd->daemon => daemon
 *
 *           CMD_ERR_INCOMPLETE => "do" and nothing more
 *                          (iff parsing for execution)
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
 *     parsed->cnode to an ENABLE (if available).
 *
 * NB: the command line MUST be preserved until the parsed command is no
 *     longer required -- no copy is made.
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 *
 * See elsewhere for description of parsed structure.
 */
extern cmd_ret_t
cmd_parse_command(cmd_parsed parsed, cmd_context context)
{
  cmd_ret_t ret ;

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
   * Unless CMD_ERR_PARSING:
   *
   *   - sort out any "do" and cut from start of command
   *   - deal with any meta command prefix
   *   - set parsed->cnode (from context->cnode or according to "do" or "meta")
   *   - set parsed->xnode (from context->node)
   *   - set parsed->nnode (from context->node)
   *   - set parsed->cmd = NULL
   *   - empty the argv vector
   *
   * Note that cmd_parse_phase_one only returns CMD_SUCCESS or CMD_ERR_PARSING.
   */
  ret = cmd_parse_phase_one(parsed, context) ;
  if (ret != CMD_SUCCESS)
    {
      qassert(ret == CMD_ERR_PARSING) ; /* no other error at this point */
      return CMD_ERR_PARSING ;
    } ;

  /* If no command tokens, and is parsing for execution, then we are done...
   * but watch out for bare "do"
   */
  if (((parsed->parts & cmd_part_command) == 0) && context->parse_execution)
    {
      if ((parsed->parts & cmd_part_do) != 0)
        return CMD_ERR_INCOMPLETE ;     /* reject "do" alone            */

      return CMD_SUCCESS ;              /* accept pipes and empty       */
    } ;

  /* Level 2 parsing
   *
   * Try in the parsed->cnode and then in parent nodes, if allowed.
   *
   * Will stop moving up the tree when hits a node which is its own parent, so
   * will stop on an ENABLE node (or immediately if is VIEW or SPECIAL node).
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

      ret = cmd_parse_phase_two(parsed, context) ;

      if (ret == CMD_SUCCESS)
        break ;

      if ((ret != CMD_ERR_NO_MATCH) || context->parse_no_tree)
        return ret ;

      pnode = cmd_node_parent(parsed->cnode) ;

      qassert(pnode != NULL_NODE) ;

      if ((pnode == parsed->cnode) || (pnode == NULL_NODE))
        return CMD_ERR_NO_MATCH ; /* done if no parent node.            */

      parsed->cnode = pnode ;
    } ;

  /* Parsed successfully -- worry about parsed->xnode and parsed->nnode.
   *
   * Here we deal with implicit node transfers and "do", implicit "do" and
   * meta commands.
   *
   * Currently both parsed->xnode and parsed->nnode are set to the node we came
   * in on (context->node).  If the node we have found the command in
   * (parsed->cnode) is not the same, then we have two choices:
   *
   *   (a) parsed->cnode is an ENABLE node -- either because have 'do' or by
   *       tree walk -- in which case we leave both parsed->xnode and
   *       parsed->nnode as they are;
   *
   *   (b) parsed->cnode is META_NODE -- in which case we also leave both
   *       parsed->xnode and parsed->nnode as they are;
   *
   *   (c) otherwise: this command, if successful will implicitly change
   *       context, in which case we need to set both parsed->xnode and
   *       parsed->nnode.
   */
  if (parsed->nnode != parsed->cnode)
    {
      if (!cmd_node_is_enable(parsed->cnode) && (parsed->cnode != META_NODE))
        parsed->xnode = parsed->nnode = parsed->cnode ;
    } ;

  /* Worry about "special" commands and those that set the next node.
   */
  if ((parsed->cmd->attr & (CMD_ATTR_NODE | CMD_ATTR_MASK)) != 0)
    {
      if ((parsed->cmd->attr & CMD_ATTR_NODE) != 0)
        parsed->nnode = parsed->cmd->attr & CMD_ATTR_MASK ;
      else
        {
          /* This is a "special" command, which may have some (limited)
           * semantic restrictions.
           *
           * The main thing we are interested in is commands which have
           * special effects on parsed->nnode (in particular exit and end).
           */
          ret = cmd_parse_specials(parsed, context) ;

          if (ret != CMD_SUCCESS)
            return ret ;
        } ;
    } ;

  /* If for execution, fill the arg_vector
   *
   * The arg_vector is an array of pointers to '\0' terminated strings, which
   * are pointers to the relevant tokens' qstring bodies.
   */
  if (context->parse_execution)
    {
      uint ti ;

      cmd_arg_vector_empty(parsed) ;

      for (ti = 0; ti < parsed->num_action ; ti++)
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
              t = cmd_token_get(parsed->tokens, parsed->first_action + ti) ;
              cmd_arg_vector_push(parsed, cmd_token_make_string(t)) ;
            } ;
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 1 of command parsing -- get ready to search the command system.
 *
 * Sets parsed->cnode to the context->cnode, or to an ancestor ENABLE if have
 * 'do' (and the original node allows it), or to META_NODE for meta commands.
 *
 * Sets parsed->xnode to the context->node -- so default is execute in current
 *
 * Sets parsed->nnode to the context->node -- so default is no change.
 *
 * Scan the tokens to break them up into the parts:
 *
 *    - do/meta  -- leading 'do' or '#' on the command (empty if in-pipe)
 *    - in-pipe  -- '<' etc           ) one of these
 *    - command  -- actual command    )
 *    - out-pipe -- '>' etc up to comment
 *    - comment  -- '!' or '#' onwards
 *                  or entire line if forced by context->daemons
 *
 * Sets parsed->parts and the required values out of:
 *
 *    parsed->in_pipe
 *    parsed->out_pipe
 *
 *    parsed->first_action
 *    parsed->num_action
 *    parsed->first_out_pipe
 *    parsed->num_out_pipe
 *    parsed->first_comment
 *    parsed->num_comment
 *
 * Requires line to have been tokenized -- cmd_tokenize().
 *
 * Returns: CMD_SUCCESS     -- all is well
 *          CMD_ERR_PARSING -- parsing error -- malformed or misplaced pipe
 *                                           -- malformed quotes/escapes
 */
static cmd_ret_t
cmd_parse_phase_one(cmd_parsed parsed, cmd_context context)
{
  uint  nt ;

  /* Set command and parsing entries to base state and prepare to process
   */
  parsed->nnode = parsed->xnode = context->node ;
  parsed->cnode = context->cnode ;
  parsed->cmd   = NULL ;

  nt = parsed->num_tokens ;

  /* If context->daemons is zero, then we treat all command lines as
   * comment, except for meta commands.
   *
   * Exit if nothing here to be parsed -- setting parsed->parts.
   */
  if ((context->daemons == DAEMON_NONE)
                            && ((parsed->tok_total & cmd_tok_meta_prefix) == 0))
    {
      /* Force entire line to be insignificant.
       *
       * Note that all tokens are deemed to be in a "cmd_part_comment", unless
       * there are no tokens.
       */
      parsed->first_comment = 0 ;
      parsed->num_comment   = parsed->num_tokens ;

      if (parsed->num_comment == 0)
        parsed->parts = cmd_parts_none ;
      else
        parsed->parts = cmd_part_comment ;

      return CMD_SUCCESS ;
    } ;

  /* If this is a command line with one or more simple tokens, then record
   * same -- this is a short cut for the 99% case !
   *
   * Otherwise, deal with everything else.
   */
  if (parsed->tok_total == cmd_tok_simple)
    {
      /* All tokens are simple
       */
      qassert(nt != 0) ;                /* must be one          */

      parsed->first_action = 0 ;
      parsed->num_action   = nt ;

      if (parsed->num_action == 0)      /* for completeness     */
        parsed->parts      = cmd_parts_none ;
      else
        parsed->parts      = cmd_part_command ;
    }
  else
    {
      /* Not simple only, may be empty, all comment etc.
       *
       * Sets parsed->parts
       */
      cmd_ret_t ret ;

      parsed->parts = cmd_parts_none ;          /* nothing, yet */
      ret = cmd_parse_phase_one_b(parsed) ;

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* Finally: if have a command part and have a "do" at the front deal with it.
   *
   *      or: if is a meta command, set META_NODE.
   *
   * Note that "do" is acted on in CONFIG and SPECIFIC nodes, ignored in any
   * ENABLE node, but is left as is for all other nodes (most likely to be
   * rejected as an error).
   *
   * Note that "do" does not apply to meta commands.
   */
  if ((parsed->parts & cmd_part_command) != 0)
    {
      /* Have a command -- worry about meta command or possible "do"
       */
      if      (parsed->parts & cmd_part_meta)
        {
          parsed->cnode = META_NODE ;   /* change to this node  */
        }
      else
        {
          cmd_token t ;
          t = cmd_token_get(parsed->tokens, parsed->first_action) ;

          if (els_len_nn(t->ot) == 2)
            {
              const char* p = els_body_nn(t->ot) ;
              if ((*p == 'd') && (*(p+1) == 'o'))
                {
                  node_type_t cnode ;

                  cnode = parsed->cnode ;

                  /* Do something with the 'do' if is ENABLE, CONFIG or
                   * SPECIFIC node
                   */
                  if (!context->parse_no_do && cmd_node_is_ecs(cnode))
                    {
                      /* For CONFIG and SPECIFIC nodes the end_to node is
                       * the parent ENABLE node
                       */
                      if (cmd_node_is_cs(cnode))
                        cnode = cmd_node_end_to(cnode) ;

                      qassert(cmd_node_is_enable(cnode)) ;

                      if (cmd_node_is_enable(cnode))
                        {
                          parsed->cnode = cnode ;       /* parse from here */

                          --parsed->num_action ;
                          ++parsed->first_action ;
                          parsed->parts |= cmd_part_do ;

                          /* If have *only* the "do", we don't have a command
                           */
                          if (parsed->num_action == 0)
                            parsed->parts ^= cmd_part_command ;
                        } ;
                    } ;
                } ;
            } ;
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 1b of command parsing
 *
 * tokenizer found at least one of:
 *
 *   - meta command prefix
 *   - in pipe token
 *   - out pipe token
 *   - token with quotes or escapes
 *
 * Deal with all of those, verifying the syntax of any pipes and completing
 * any tokens with quotes etc.
 *
 * Update the "parts" and the relevant first/num values.
 *
 * Note that the number of tokens (nt) *excludes* any comment token.
 *
 * Returns: CMD_SUCCESS     -- all is well
 *          CMD_ERR_PARSING -- parsing error -- malformed or misplaced pipe
 *                                           -- malformed quotes/escapes
 */
static cmd_ret_t
cmd_parse_phase_one_b(cmd_parsed parsed)
{
  cmd_ret_t   ret ;
  cmd_parts_t cur_part ;
  cmd_token   t ;
  uint i, nt ;
  uint* p_part_num ;

  i  = 0 ;                      /* starting at the beginning    */
  t  = cmd_token_get(parsed->tokens, i) ;

  nt = parsed->num_tokens ;

  /* Pick off any comment and quit now if empty or comment only
   *
   * Initialise parsed->parts
   */
  if ((parsed->tok_total & cmd_tok_comment) == 0)
    {
      parsed->parts = cmd_parts_none ;  /* no parts yet         */

      if (nt == 0)
        {
          qassert(t->type == cmd_tok_eol) ;
          return CMD_SUCCESS ;
        } ;
    }
  else
    {
      parsed->num_comment   = 1 ;
      parsed->first_comment = --nt ;    /* implicitly the last  */

      parsed->parts = cmd_part_comment ;

      if (nt == 0)
        {
          qassert(t->type == cmd_tok_comment) ;
          return CMD_SUCCESS ;
        } ;
    } ;

  /* Check for meta command prefix
   *
   * If we have one, then expect to have at least two tokens the second of
   * which is a simple token.
   */
  if ((t->type & cmd_tok_meta_prefix) != 0)
    {
      parsed->parts |= cmd_part_meta ;          /* note the meta part   */

      t = cmd_token_get(parsed->tokens, ++i) ;  /* next token           */

      if ((t->type & cmd_tok_simple) == 0)
        {
          qassert(false) ;
          return cmd_set_parse_error(parsed, t, 0, "broken meta command") ;
        } ;
    } ;

  /* Walk the tokens, establishing the start and end of:
   *
   *   - command part   )
   *   - in pipe part   ) exclusive, at start
   *   - out shell part )
   *   - out pipe part    which may only follow command or in pipe
   *
   * Update parsed->parts as each part is recognised.
   */
  cur_part = cmd_parts_none ;   /* current part -- nothing, yet */
  while (i < nt)
    {
      /* Simple tokens may appear anywhere.
       *
       * If this is the first token, start a cmd_part_command.
       */
      if      ((t->type & cmd_tok_simple) != 0)
        {
          if (cur_part == cmd_parts_none)
            {
              cur_part = cmd_part_command ;
              parsed->first_action  = i ;
              p_part_num  = &parsed->num_action ;
              *p_part_num = 0 ;
            } ;
        }

      /* '<' tokens may appear only as the first token.
       */
      else if ((t->type & cmd_tok_in_pipe) != 0)
        {
          if (cur_part == cmd_parts_none)
            {
              /* Parse the '<' token and set parsed->in_pipe
               */
              ret = cmd_parse_in_pipe(parsed, t) ;
              if (ret != CMD_SUCCESS)
                return ret ;

              cur_part = cmd_part_in_pipe ;
              p_part_num  = &parsed->num_action ;
              *p_part_num = 0 ;
            }
          else
            return cmd_set_parse_error(parsed, t, 0, "unexpected 'pipe in'") ;
        }

      /* '|' tokens may appear only as the first token.
       *
       * If this is the first token, start a cmd_part_out_pipe.
       */
      else if ((t->type & cmd_tok_out_shell) != 0)
        {
         if (cur_part == cmd_parts_none)
            {
             /* Parse the '|' token and set parsed->out_pipe
              */
              ret = cmd_parse_out_pipe(parsed, t) ;
              if (ret != CMD_SUCCESS)
                return ret ;

              cur_part = cmd_part_out_pipe ;
              parsed->first_out_pipe  = i ;
              p_part_num  = &parsed->num_out_pipe ;
              *p_part_num = 0 ;
            }
         else
           return cmd_set_parse_error(parsed, t, 0, "unexpected 'shell pipe'") ;
        }

      /* '>' types of token may appear once, but not at the start and not
       *     after a '|' token
       */
      else if ((t->type & cmd_tok_out_pipe) != 0)
        {
          if ((cur_part != cmd_parts_none) && (cur_part != cmd_part_out_pipe))
            {
              /* Parse the '>' token and set parsed->out_pipe
               */
              ret = cmd_parse_out_pipe(parsed, t) ;
              if (ret != CMD_SUCCESS)
                return ret ;

              cur_part = cmd_part_out_pipe ;
              parsed->first_out_pipe  = i ;
              p_part_num  = &parsed->num_out_pipe ;
              *p_part_num = 0 ;
            }
          else
            return cmd_set_parse_error(parsed, t, 0, "unexpected 'pipe out'") ;
        }

      /* Didn't recognise the token type !
       */
      else
        {
          qassert(false) ;
          return cmd_set_parse_error(parsed, t, 0, "unexpected token") ;
        } ;

      /* Token recognised and accepted.  Update parts and part count.
       */
      parsed->parts |= cur_part ;
      ++*p_part_num ;

      /* Now deal with any "incompleteness"
       */
      if ((t->type & cmd_tok_incomplete) != 0)
        {
          ret = cmd_token_complete(parsed, t) ;
          if (ret != CMD_SUCCESS)
            return ret ;
        } ;

      /* Next token, please
       */
      t = cmd_token_get(parsed->tokens, ++i) ;
    } ;

  /* If have an in-pipe or an out-pipe, worry about the number of
   * arguments
   */
  if ((parsed->parts & cmd_parts_pipe) != 0)
    {
      const char*  msg  = NULL ;
      uint i ;
      bool e ;

      /* If there is an action part, ie in pipe, check number of arguments
       */
      if ((parsed->parts & cmd_part_in_pipe) != 0)
        {
          assert(parsed->num_action > 0) ;

          if (((parsed->in_pipe & cmd_pipe_file) != 0)
                                                && (parsed->num_action != 2))
            {
              if (parsed->num_action == 1)
                {
                  i    = parsed->first_action ;
                  e    = true ;
                  msg  = "requires file" ;
                }
              else
                {
                  i    = parsed->first_action + 2 ;
                  e    = false ;
                  msg  = "expects file only" ;
                } ;
            } ;

          if (((parsed->in_pipe & cmd_pipe_shell) != 0)
                                                 && (parsed->num_action < 2))
            {
              i    = parsed->first_action ;
              e    = true ;
              msg  = "requires shell command" ;
            } ;
        } ;

      /* If there is an out_pipe part, check the number of arguments (unless
       * already failed)
       */
      if (((parsed->parts & cmd_part_out_pipe) != 0) && (msg == NULL))
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

      /* If failed, report
       */
      if (msg != NULL)
        {
          t = cmd_token_get(parsed->tokens, i) ;
          return cmd_set_parse_error(parsed, t, e ? els_len_nn(t->ot) : 0, msg) ;
        } ;
    } ;

  /* It's OK !
   */
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 2 of command parsing -- parsing for execution.
 *
 * Assumes phase 1 completed successfully.
 *
 * Note that if parsed->num_action == 0, will have constructed a cmd_v, with
 * all possible commands in it (depending on cmd_parse_execution).
 *
 * Returns:  CMD_SUCCESS        -- parsed successfully
 *           CMD_ERR_NO_MATCH   -- could find nothing that matches
 *           CMD_ERR_AMBIGUOUS  -- found more than one match
 *                                   or parsed->num_action == 0
 */
static cmd_ret_t
cmd_parse_phase_two(cmd_parsed parsed, cmd_context context)
{
  uint ii ;
  uint match ;

  /* Prepare to filter commands                                         */

  cmd_filter_prepare(parsed, context) ;

  match = 2 ;   /* in case parsed->num_action == 0 !   */

  for (ii = 0 ; ii < parsed->num_action ; ii++)
    match = cmd_filter(parsed, ii, false) ;

  /* Should end up with one command to execute.                         */
  if (match == 0)
    return CMD_ERR_NO_MATCH ;

  if (match >  1)
    return CMD_ERR_AMBIGUOUS ;

  parsed->cmd = vector_get_item(parsed->cmd_v, 0) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Perform the special handling required for the command that has parsed
 * successfully so far.
 *
 * This mechanism is used to deal with commands which have a context sensitive
 * effect on parsed->nnode.
 *
 * Can be used to do any other semantic checks, subject to the information
 * being available.
 *
 * Returns:  CMD_SUCCESS     -- OK
 *           CMD_ERR_PARSING -- failed some check, see parsed->emess
 *
 * Dealt with here are:
 *
 *   * "exit" and "quit"
 *
 *     The next node depends on the current node.
 *
 *   * "end"
 *
 *     The next node depends on the current node.
 *
 *   * "enable"
 *
 *     This command is really only intended for interactive use, but need to
 *     do something with it in other contexts.
 *
 *     Can go to ENABLE_NODE directly if can_enable is set in the context.
 *
 *     The can_enable is set when the vty starts if it does not require any
 *     authentication to enter ENABLE_NODE (eg when reading configuration) or
 *     because it started in ENABLE_NODE or greater (eg VTY_TERMINAL with no
 *     password and advanced mode !).
 *
 *     Once can_enable is set it is not unset.  So getting to enable once is
 *     sufficient for a given VTY.
 *
 *     A pipe will inherit can_enable, provided that the parent is in
 *     ENABLE_NODE or better.
 *
 *     A pipe cannot run AUTH_ENABLE_NODE -- this means that a pipe can either
 *     immediately enable, or cannot enable at all.
 *
 *     The effect of all this is that "enable" is straightforward, except for
 *     VTY_TERMINAL.  For VTY_TERMINAL:
 *
 *       - if the VTY starts in any node >= ENABLE_NODE, then can_enable
 *         is set from the beginning !
 *
 *         If has ever reached ENABLE_NODE, then can_enable will be set.
 *
 *       - otherwise: when enable command is seen, must authenticate.
 *
 *          - if there is an enable password, then must get and accept the
 *            password, which can only happen at vin_depth == vout_depth == 0
 *            -- see vty_cmd_can_auth_enable().
 *
 *          - if there is no enable password, then is implicitly authenticated
 *            if is in VIEW_NODE.
 *
 *            Note that will not accept enable with no password if is in
 *            RESTRICTED_NODE.  Can only be in RESTRICTED_NODE if started with
 *            no password, but host.restricted_mode is set.  Doesn't seem much
 *            point having a restricted_mode if you can go straight to
 *            ENABLE_NODE just because a password has not been set !
 *
 *   * "config terminal" and aliases
 *
 *     This is similar to "enable" above -- so can go direct from VIEW_NODE
 *     or RESTRICTED_NODE to CONFIG_NODE, without touching ENABLE_NODE.
 *
 *   * "daemon" command, which if parsed we want to execute.
 */
static cmd_ret_t
cmd_parse_specials(cmd_parsed parsed, cmd_context context)
{
  cmd_ret_t ret ;

  ret = CMD_SUCCESS ;

  switch (parsed->cmd->attr & CMD_ATTR_MASK)
    {
      case cmd_sp_simple:
        zabort("invalid cmd_sp_simple") ;
        break ;

      case cmd_sp_end:
        parsed->nnode = cmd_node_end_to(parsed->cnode) ;
        break ;

      case cmd_sp_exit:
        parsed->nnode = cmd_node_exit_to(parsed->cnode) ;
        break ;

      case cmd_sp_enable:
        parsed->nnode = cmd_auth_specials(context, ENABLE_NODE) ;
        break ;

      case cmd_sp_configure:
        parsed->nnode = cmd_auth_specials(context, CONFIG_NODE) ;
        break ;

      default:
        zabort("unknown cmd_sp_xxx") ;
        break ;
    } ;

  qassert(parsed->nnode != NULL_NODE) ;

  if (parsed->nnode == NULL_NODE)
    parsed->nnode = EXIT_NODE ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Deal with commands which may require AUTH_ENABLE_NODE authentication.
 *
 * The rule is that the parser must set what node a given command *will* change
 * to iff it succeeds.
 *
 * So if we can go directly to the target node, must establish that now.
 *
 * If cannot go to directly, we set that should go to AUTH_ENABLE_NODE.  That
 * may fail if not in a suitable state to do that -- and issue suitable
 * message.
 *
 * Returns:  the target node, if does not need to authenticate.
 *           otherwise: AUTH_ENABLE_NODE, and has set: context->onode
 *                                                     context->tnode
 */
static node_type_t
cmd_auth_specials(cmd_context context, node_type_t target)
{
  node_type_t nnode ;

  nnode = target ;              /* Assume can enable directly.  */

  if (!context->can_enable)
    {
      bool no_pw ;

      VTY_LOCK() ;
        no_pw = (host.enable.text == NULL) ;
      VTY_UNLOCK() ;

      /* Can enable if is already ENABLE_NODE or better (this should
       * not be required -- but does no harm).
       *
       * If is in VIEW_NODE and there is no password, then we can, in fact
       * enable.
       *
       * Otherwise, we need to authenticate before stepping up to the target
       * node, so return AUTH_ENABLE_NODE as the next node -- having set
       * the context->onode and tnode, which the AUTH_ENABLE_NODE needs.
       *
       * Note that setting AUTH_ENABLE_NODE does not guarantee we will reach
       * that !  When the command actually executes, it must check that
       * is allowed to proceed, and trigger suitable error/warning and
       * messages.
       *
       * Note that if we are in RESTRICTED_NODE, then must authenticate,
       * which will later be refused if no password is set at the time !
       */
      if      (context->node >= ENABLE_NODE)
        context->can_enable = true ;
      else if ((context->node == VIEW_NODE) && no_pw)
        context->can_enable = true ;
      else
        {
          nnode = AUTH_ENABLE_NODE ;

          context->onode = context->node ;   /* see vty_cmd_auth()   */
          context->tnode = target ;          /* see vty_cmd_auth()   */
        } ;
    } ;

  return nnode ;
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
cmd_describe_command (const char* line, node_type_t node, cmd_ret_t* status)
{
#if 0
  vector ret ;
  struct cmd_parsed parsed_s ;
  cmd_parsed        parsed ;
  cmd_token_type_t  tok_total ;

  /* Set up a parser object and tokenize the command line               */
  parsed = cmd_parse_init_new(&parsed_s) ;
  tok_total = cmd_tokenize(parsed, line, node) ;







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
extern cmd_ret_t
cmd_completion(cmd_parsed parsed, cmd_context context)
{
  cmd_ret_t ret ;

  /* Parse the line -- use given context, but do not parse for execution.
   */
  context->parse_execution = false ;
  ret = cmd_parse_command(parsed, context) ;

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
   *
   * TODO ... does not work for 'do' when on the 'do' !
   * TODO ... what about the automatic ENABLE_NODE stuff ?
   */
  qassert(parsed->cti >= parsed->first_action) ;
  qassert(parsed->cti <= parsed->first_action + parsed->num_action) ;

  cmd_filter(parsed, parsed->cti - parsed->first_action, true) ;

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
 * Returns: *pre = number of characters to move the cursor before
 *                 starting to replace characters (may be -ve)
 *          *rep = number of characters to replace
 *          *ins = number of spaces to insert : 0..2
 *          *mov = number of spaces to move afterwards (may be -ve)
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

      return ((parsed->cti - parsed->first_action + 1) == cmd->nt_max) ;
    } ;

  return false ;
} ;




/*------------------------------------------------------------------------------
 * Should we partly complete a token ?
 *
 * If cmd_completion() has returned two or more alternative completions, then
 * we have two interesting cases:
 *
 *   1) the alternatives are all keywords, and the longest leading substring
 *      of the alternatives is longer than the current token, then extend the
 *      current token to that longest leading substring -- leaving the
 *      cursor at the end of same.
 *
 *   2) in any case, if the cursor is not at the end of the token, move to the
 *      end of the token.
 *
 * Returns: true  => yes, should partly complete the token
 *                   sets the given "els" to be the what should replace token
 *
 *          false => no, should not
 *                   does not set the els.
 */
extern bool
cmd_part_complete(cmd_parsed parsed, elstring els)
{
  vector_index_t i ;
  cmd_token      t ;

  ulen           ep, pos ;

  qassert(vector_length(parsed->item_v) > 1) ;

  /* Do nothing if this is not a simple token.
   */
  t  = cmd_token_get(parsed->tokens, parsed->cti) ;

  if ((t->type & cmd_tok_simple) == 0)
    return false ;

  /* Scan the item_v, and if they are all keywords, set els to be the longest
   * common leading substring.
   *
   * Otherwise set els to be the keyword itself.
   */
  els_set_len_nn(els, 0) ;

  for (i = 0 ; i < vector_length(parsed->item_v) ; ++i)
    {
      cmd_item item ;

      item = vector_get_item(parsed->item_v, i) ;

      if (item->type != item_keyword)
        {
          *els = *(t->ot) ;
          break ;
        } ;

      if (els_len_nn(els) == 0)
        *els = *(item->str) ;
      else
        els_set_len_nn(els, els_sub_len(els, item->str)) ;
    } ;

  /* If the els is longer than the current token, then need to replace
   * the token with that -- moving to the end of the partial token.
   *
   * If the cursor is not at the end of the current token, then need to move
   * to the end of the token.
   */
  ep  = t->tp + els_len_nn(t->ot) ;     /* end of current token         */
  pos = t->tp + parsed->rp ;            /* current cursor position      */

  qassert(ep >= pos) ;

  return (els_len_nn(els) > els_len_nn(t->ot)) || (ep > pos) ;
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

/*==============================================================================
 * Initialise command parsing -- done before first command is installed.
 *
 * Complete the (much used) eol_item.
 */
extern void
cmd_parser_init(void)
{
  dsl_init(word_lumps) ;
  cmd_set_str(&eol_item, "<cr>") ;
} ;

/* Quagga command line parsing -- header
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

#include <zebra.h>

#include "command_parse.h"
#include "memory.h"

/*==============================================================================
 * Token handling
 */

/* Store of qstrings, used for parsing.         */
token_vector_t spare_tokens ;

static char cmd_token_escape(char e) ;

/*------------------------------------------------------------------------------
 * Initialise a brand new token vector -- empty.
 */
static inline void
cmd_token_vector_init(token_vector tokens)
{
  vector_init_new(tokens->body, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise empty spare tokens vector
 */
extern void
cmd_spare_tokens_init(void)
{
  cmd_token_vector_init(spare_tokens) ;
} ;

/*------------------------------------------------------------------------------
 * Empty out the spare_tokens vector and release all memory
 */
extern void
cmd_spare_tokens_free(void)
{
  token tok ;

  while ((tok = vector_ream(spare_tokens->body, keep_it)) != NULL)
    qs_reset(tok->qs, keep_it) ;
} ;

/*------------------------------------------------------------------------------
 * Take string and break it into tokens.
 *
 * Discards leading and trailing ' ' or '\t'.
 *
 * Expects string to have been preprocessed, if required, to ensure that any
 * unwanted control characters have been removed.  This code only recognises
 * '\t'.
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
 *   * tokens are separated by "separators", which start with any of:
 *
 *       '!', '#', '<', '>' and '|'
 *
 *     which may be followed by one or more characters to form a separator
 *     token.
 *
 *       - from '!' or '#' to end of line is a comment token.
 *
 *       - '<' opt and '<|' opt are separators, where opt is any combination
 *         of '+', '*' and '-'.
 *
 *       - '>', '>>' and '|' are separators.
 *
 * NB: the tokenization mimics the standard shell which makes the piping stuff
 *     straightforward.  It's also well known.  Apart from the "'" rule, it
 *     also seems fine !
 *
 * NB: any control characters other than those spotted by isspace() are accepted
 *     as part of the current token !
 *
 * The tokens returned contain all the original characters of the line, except
 * for the removal of '\t' between tokens.
 *
 * Returns: the types of all tokens or'd together.
 *          returns cmd_tok_null if the line is empty (apart from ' ' and '\t')
 *                               or if the pointer was NULL.
 *
 * Note: all the tokens in the vector have at least one character, and no
 *       entries are NULL.
 *
 * NB: it is the callers responsibility to release the token objects in due
 *     course.
 */
extern cmd_token_type_t
cmd_tokenise(cmd_parsed parsed, const char *line, node_type_t node)
{
  const char *cp, *ep ;
  cmd_token_type_t total ;

  cmd_empty_token_vector(parsed->tokens) ; /* Empty the token vector   */

  parsed->line  = line ;
  parsed->onode = parsed->cnode = node ;

  total = cmd_tok_null ;                /* nothing yet                  */

  if (line == NULL)                     /* tolerate NULL                */
    return total ;

  cp = line ;
  ep = cp + strlen(cp) ;

  while (cp < ep)                       /* process to end               */
    {
      const char* sp ;
      bool end ;
      cmd_token_type_t type ;

      if ((*cp == ' ') || (*cp == '\t'))
        {
          /* skip white-space                                           */
          do { ++cp ; } while ((*cp == ' ') || (*cp == '\t')) ;

          if (cp == ep)
            {
              if (total != cmd_tok_null)
                total |= cmd_tok_trailing ;
              break ;
            } ;
        } ;

      sp   = cp ;
      end  = false ;
      type = cmd_tok_simple ;
      do
        {
          switch (*cp)
          {
            case '\t':          /* whitespace at end of token           */
            case ' ':
              end = true ;
              break ;

            case '\'':          /* proceed to matching '\'' or end      */
              type |= cmd_tok_sq ;
              ++cp ;
              while (cp < ep)
                {
                  if (*cp++ == '\'')
                    break ;
                } ;
              break ;

            case '\\':          /* step past escaped character, if any  */
              type |= cmd_tok_esc ;
              ++cp ;
              if (cp < ep)
                ++cp ;
              break ;

            case '"':           /* proceed to matching '"' or end...    */
              type |= cmd_tok_dq ;
              ++cp ;
              while (cp < ep)   /* NB: do not register '\\' separately  */
                {
                  if (*cp++ == '"')
                    if (*(cp - 2) != '\\')    /* ignore escaped '"'   */
                      break ;
                } ;
              break ;

            case '>':           /* '>' or '>>' separators.              */
              end = true ;
              if (cp == sp)     /* if at start of token */
                {
                  type = cmd_tok_pipe_out ;
                  ++cp ;
                  if ((cp < ep) && (*cp == '>'))
                    ++cp ;
                } ;
              break ;

            case '|':           /* '|' separator.                       */
              end = true ;
              if (cp == sp)
                type = cmd_tok_pipe_out ;
                ++cp ;
              break ;

            case '<':           /* '<' or '<|' separators.              */
              end = true ;
              if (cp == sp)
                {
                type = cmd_tok_pipe_in ;
                  ++cp ;
                  if ((cp < ep) && (*cp == '|'))
                    ++cp ;
                  if ( (cp < ep) &&
                          ((*cp == '+') || (*cp == '-') || (*cp == '*')) )
                    ++cp ;
                } ;
              break ;

            case '!':           /* '!' and '#' separators.              */
            case '#':
              end = true ;
              if (cp == sp)
                {
                  type = cmd_tok_comment ;
                  cp = ep ;
                } ;
              break ;

            default:
              ++cp ;
              break ;
          } ;
        } while (!end && (cp < ep)) ;

      cmd_token_push(parsed->tokens,
                                  cmd_token_new(type, sp, cp - sp, sp - line)) ;
    } ;

  return total ;
} ;

/*------------------------------------------------------------------------------
 * Process token to remove quotes and escapes (if any).
 *
 * Returns: true <=> OK
 *          false => invalid escape or incomplete quotes
 *
 * NB: if fails, returns token completed as far as possible.
 */
extern bool
cmd_token_do_complete(token t)
{
  char *s, *p, *q ;
  char ch ;
  bool ok = true ;
  bool dq = false ;

  p = s = cmd_token_value(t) ;
  q = p ;
  while (*p != '\0')
    {
      switch (*p)
      {
        case '\'':
          ++p ;                         /* skip leading '\''    */
          while (1)
            {
              if (*p == '\0')
                {
                  ok = false ;          /* broken '...'         */
                  break ;
                } ;

              if (*p == '\'')
                {
                  ++p ;                 /* skip trailing '\''   */
                  break ;               /* done '....'          */
                } ;

              *q++ = *p++ ;
            } ;
          break ;

        case '"':
          ++p ;                         /* skip '"'             */
          dq = !dq ;
          break ;

        case '\\':
          ++p ;                         /* step past '\\'       */
          ch = cmd_token_escape(*p) ;
          if (ch == '\0')
            {
              ok = false ;
              ch = *p ;
              if (ch == '\0')
                ch = '\\' ;             /* \ at end is kept     */
              else
                *q++ = '\\' ;           /* otherwise keep \x    */
            } ;
          *q++ = ch ;
          break ;

        default:
          *q++ = *p++ ;
      } ;
    } ;

  qs_term_here(t->qs, q) ;

  return ok && !dq ;
}

/*------------------------------------------------------------------------------
 * Return escaped value of \e.
 *
 * Everything except '0'..'9', 'A'..'Z', 'a'..'z' can be escaped -- these are
 * reserved for future actual escapes !
 *
 * Returns '\0' if e == '\0' or if \e is an invalid escape.
 */
static char
cmd_token_escape(char e)
{
  if ((e < '0') || (e > 'z'))
    return e ;
  return isalpha(e) ? '\0' : e ;
} ;

/*==============================================================================
 * Parser object
 */

/*------------------------------------------------------------------------------
 * Initialise a new cmd_parsed object, allocating if required
 */
extern cmd_parsed
cmd_parse_init_new(cmd_parsed parsed)
{
  if (parsed == NULL)
    parsed = XCALLOC(MTYPE_CMD_PARSED, sizeof(*parsed)) ;
  else
    memset(parsed, 0, sizeof(*parsed)) ;

  /* Zeroising the structure has set:
   *
   *   cmd          = NULL -- no command parsed, yet
   *   cnode               -- no node set, yet
   *
   *   do_shortcut         -- false
   *   onode               -- not material (do_shortcut is false)
   *
   *   pipes             = 0  -- cmd_pipe_none
   */
  confirm(cmd_pipe_none == 0) ;

  cmd_token_vector_init(parsed->tokens) ;
  cmd_token_vector_init(parsed->read_pipe_tokens) ;
  cmd_token_vector_init(parsed->write_pipe_tokens) ;
  cmd_arg_vector_init(parsed) ;

  return parsed ;
} ;

/*------------------------------------------------------------------------------
 * Empty out and (if required) free a cmd_parsed object
 */
extern cmd_parsed
cmd_parse_reset(cmd_parsed parsed, bool free_structure)
{
  if (parsed != NULL)
    {
      cmd_empty_parsed_tokens(parsed) ;     /* give back tokens         */
      cmd_arg_vector_free(parsed) ;         /* give back vector body    */

      if (free_structure)
        XFREE(MTYPE_CMD_PARSED, parsed) ;   /* sets parsed = NULL       */
      else
        cmd_parse_init_new(parsed) ;
    } ;

  return parsed ;
} ;

/*==============================================================================
 * Match functions.
 *
 * Is the given string a, possibly incomplete, value of the required kind ?
 */


/*------------------------------------------------------------------------------
 * Is this an IPv4 Address:
 *
 *   999.999.999.999    -- where no part may be > 255
 *
 * TODO: cmd_ipv4_match() seems to accept leading '.' ?
 * TODO: cmd_ipv4_match() seems to accept leading zeros ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 */
extern match_type_t
cmd_ipv4_match (const char *str)
{
  const char *sp;
  int dots = 0, nums = 0;
  char buf[4];

  if (str == NULL)
    return partly_match;

  for (;;)
    {
      memset (buf, 0, sizeof (buf));
      sp = str;
      while (*str != '\0')
        {
          if (*str == '.')
            {
              if (dots >= 3)
                return no_match;

              if (*(str + 1) == '.')
                return no_match;

              if (*(str + 1) == '\0')
                return partly_match;

              dots++;
              break;
            }
          if (!isdigit ((int) *str))
            return no_match;

          str++;
        }

      if (str - sp > 3)
        return no_match;

      strncpy (buf, sp, str - sp);
      if (atoi (buf) > 255)
        return no_match;

      nums++;

      if (*str == '\0')
        break;

      str++;
    }

  if (nums < 4)
    return partly_match;

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv4 Prefix:
 *
 *   999.999.999.999/99  -- where no part may be > 255,
 *                          and prefix length may not be > 32
 *
 * TODO: cmd_ipv4_prefix_match() seems to accept leading '.' ?
 * TODO: cmd_ipv4_prefix_match() seems to accept leading zeros ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
extern match_type_t
cmd_ipv4_prefix_match (const char *str)
{
  const char *sp;
  int dots = 0;
  char buf[4];

  if (str == NULL)
    return partly_match;

  for (;;)
    {
      memset (buf, 0, sizeof (buf));
      sp = str;
      while (*str != '\0' && *str != '/')
        {
          if (*str == '.')
            {
              if (dots == 3)
                return no_match;

              if (*(str + 1) == '.' || *(str + 1) == '/')
                return no_match;

              if (*(str + 1) == '\0')
                return partly_match;

              dots++;
              break;
            }

          if (!isdigit ((int) *str))
            return no_match;

          str++;
        }

      if (str - sp > 3)
        return no_match;

      strncpy (buf, sp, str - sp);
      if (atoi (buf) > 255)
        return no_match;

      if (dots == 3)
        {
          if (*str == '/')
            {
              if (*(str + 1) == '\0')
                return partly_match;

              str++;
              break;
            }
          else if (*str == '\0')
            return partly_match;
        }

      if (*str == '\0')
        return partly_match;

      str++;
    }

  sp = str;
  while (*str != '\0')
    {
      if (!isdigit ((int) *str))
        return no_match;

      str++;
    }

  if (atoi (sp) > 32)
    return no_match;

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv6 Address:
 *
 * TODO: cmd_ipv6_match() only returns "partly_match" for empty string ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 */

#define IPV6_ADDR_STR           "0123456789abcdefABCDEF:.%"
#define IPV6_PREFIX_STR         "0123456789abcdefABCDEF:.%/"
#define STATE_START             1
#define STATE_COLON             2
#define STATE_DOUBLE            3
#define STATE_ADDR              4
#define STATE_DOT               5
#define STATE_SLASH             6
#define STATE_MASK              7

#ifdef HAVE_IPV6

extern match_type_t
cmd_ipv6_match (const char *str)
{
  int state = STATE_START;
  int colons = 0, nums = 0, double_colon = 0;
  const char *sp = NULL;
  struct sockaddr_in6 sin6_dummy;
  int ret;

  if (str == NULL)
    return partly_match;

  if (strspn (str, IPV6_ADDR_STR) != strlen (str))
    return no_match;

  /* use inet_pton that has a better support,
   * for example inet_pton can support the automatic addresses:
   *  ::1.2.3.4
   */
  ret = inet_pton(AF_INET6, str, &sin6_dummy.sin6_addr);

  if (ret == 1)
    return exact_match;

  while (*str != '\0')
    {
      switch (state)
        {
        case STATE_START:
          if (*str == ':')
            {
              if (*(str + 1) != ':' && *(str + 1) != '\0')
                return no_match;
              colons--;
              state = STATE_COLON;
            }
          else
            {
              sp = str;
              state = STATE_ADDR;
            }

          continue;
        case STATE_COLON:
          colons++;
          if (*(str + 1) == ':')
            state = STATE_DOUBLE;
          else
            {
              sp = str + 1;
              state = STATE_ADDR;
            }
          break;
        case STATE_DOUBLE:
          if (double_colon)
            return no_match;

          if (*(str + 1) == ':')
            return no_match;
          else
            {
              if (*(str + 1) != '\0')
                colons++;
              sp = str + 1;
              state = STATE_ADDR;
            }

          double_colon++;
          nums++;
          break;
        case STATE_ADDR:
          if (*(str + 1) == ':' || *(str + 1) == '\0')
            {
              if (str - sp > 3)
                return no_match;

              nums++;
              state = STATE_COLON;
            }
          if (*(str + 1) == '.')
            state = STATE_DOT;
          break;
        case STATE_DOT:
          state = STATE_ADDR;
          break;
        default:
          break;
        }

      if (nums > 8)
        return no_match;

      if (colons > 7)
        return no_match;

      str++;
    }

#if 0
  if (nums < 11)
    return partly_match;
#endif /* 0 */

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv6 Prefix:
 *
 * TODO: cmd_ipv6_prefix_match() hardly returns "partly_match" ?
 * TODO: cmd_ipv6_prefix_match() possibly accepts invalid address before '/' ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
extern match_type_t
cmd_ipv6_prefix_match (const char *str)
{
  int state = STATE_START;
  int colons = 0, nums = 0, double_colon = 0;
  int mask;
  const char *sp = NULL;
  char *endptr = NULL;

  if (str == NULL)
    return partly_match;

  if (strspn (str, IPV6_PREFIX_STR) != strlen (str))
    return no_match;

  while (*str != '\0' && state != STATE_MASK)
    {
      switch (state)
        {
        case STATE_START:
          if (*str == ':')
            {
              if (*(str + 1) != ':' && *(str + 1) != '\0')
                return no_match;
              colons--;
              state = STATE_COLON;
            }
          else
            {
              sp = str;
              state = STATE_ADDR;
            }

          continue;
        case STATE_COLON:
          colons++;
          if (*(str + 1) == '/')
            return no_match;
          else if (*(str + 1) == ':')
            state = STATE_DOUBLE;
          else
            {
              sp = str + 1;
              state = STATE_ADDR;
            }
          break;
        case STATE_DOUBLE:
          if (double_colon)
            return no_match;

          if (*(str + 1) == ':')
            return no_match;
          else
            {
              if (*(str + 1) != '\0' && *(str + 1) != '/')
                colons++;
              sp = str + 1;

              if (*(str + 1) == '/')
                state = STATE_SLASH;
              else
                state = STATE_ADDR;
            }

          double_colon++;
          nums += 1;
          break;
        case STATE_ADDR:
          if (*(str + 1) == ':' || *(str + 1) == '.'
              || *(str + 1) == '\0' || *(str + 1) == '/')
            {
              if (str - sp > 3)
                return no_match;

              for (; sp <= str; sp++)
                if (*sp == '/')
                  return no_match;

              nums++;

              if (*(str + 1) == ':')
                state = STATE_COLON;
              else if (*(str + 1) == '.')
                state = STATE_DOT;
              else if (*(str + 1) == '/')
                state = STATE_SLASH;
            }
          break;
        case STATE_DOT:
          state = STATE_ADDR;
          break;
        case STATE_SLASH:
          if (*(str + 1) == '\0')
            return partly_match;

          state = STATE_MASK;
          break;
        default:
          break;
        }

      if (nums > 11)
        return no_match;

      if (colons > 7)
        return no_match;

      str++;
    }

  if (state < STATE_MASK)
    return partly_match;

  mask = strtol (str, &endptr, 10);
  if (*endptr != '\0')
    return no_match;

  if (mask < 0 || mask > 128)
    return no_match;

/* I don't know why mask < 13 makes command match partly.
   Forgive me to make this comments. I Want to set static default route
   because of lack of function to originate default in ospf6d; sorry
       yasu
  if (mask < 13)
    return partly_match;
*/

  return exact_match;
}

#endif /* HAVE_IPV6  */

/*------------------------------------------------------------------------------
 * Is this a decimal number in the allowed range:
 *
 * Returns: true <=> OK -- *including* empty string
 *          false => not a valid number, or not in required range
 *                   (or invalid range !!)
 */

#define DECIMAL_STRLEN_MAX 10

extern bool
cmd_range_match (const char *range, const char *str)
{
  char *p;
  char buf[DECIMAL_STRLEN_MAX + 1];
  char *endptr = NULL;
  unsigned long min, max, val;

  if (str == NULL)
    return true ;

  val = strtoul (str, &endptr, 10);
  if (*endptr != '\0')
    return false ;

  range++;
  p = strchr (range, '-');
  if (p == NULL)
    return false ;
  if (p - range > DECIMAL_STRLEN_MAX)
    return false ;
  strncpy (buf, range, p - range);
  buf[p - range] = '\0';
  min = strtoul (buf, &endptr, 10);
  if (*endptr != '\0')
    return false ;

  range = p + 1;
  p = strchr (range, '>');
  if (p == NULL)
    return false ;
  if (p - range > DECIMAL_STRLEN_MAX)
    return false ;
  strncpy (buf, range, p - range);
  buf[p - range] = '\0';
  max = strtoul (buf, &endptr, 10);
  if (*endptr != '\0')
    return false ;

  if (val < min || val > max)
    return false ;

  return true ;
}



/* Miscellaneous extended functions
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "misc.h"

#include "errno.h"
#include "ctype.h"
#include "miyagi.h"

/*==============================================================================
 * Miscellaneous extended functions
 *
 * To qualify for inclusion here a function must be of general use and depend
 * only on standard system functions -- typically overcoming some clunky
 * standard interface.
 *
 */

/*==============================================================================
 * strtol and strtoul extensions.
 *
 * These extensions:
 *
 *   * deal with the error handling issues
 *
 *   * allow complete numbers only -- disallow empty numbers
 *
 *   * deal with trailing whitespace
 *
 *   * allow decimal or 0x/0X prefixed hex -- but NOT octal.
 *     (allows leading zeros).
 *
 *   * allow "_" in numbers (inside -- not at start or end.
 *
 *   * signal presence of sign character
 *
 * Syntax is: \s*([+-]\s*_*)?<number>\s*  -- sign only if signed
 *                                        -- followed by '\0' if required
 *
 * where <number> is:  0(_*0+)*
 *                     (0[_0]*)?[1-9](_*[0-9])*
 *                     0[xX](_*[0-9a-fA-F])+
 *
 * In essence number must contain at least one digit, may contain '_' but may
 * not start or end in '_'.  Leading and trailing whitespace is allowed, as is
 * whitespace after any sign.
 *
 * If the follower character is asked for, will return pointer to whatever
 * strtol()/stroul() stops on.  Otherwise, strips trailing whitespace and
 * rejects number if not the at end of string.
 */

/*------------------------------------------------------------------------------
 * Extended strtol()
 *
 * If p_end == NULL, requires string (ignoring leading and trailing whitespace)
 * to contain just a number.  Note that trailing '_' are not part of a number,
 * so if present will trigger strtox_invalid error.
 *
 * If p_end != NULL, if leading part of string is a valid number, returns
 * pointer to first character after the number.  Since trailing '_' are not part
 * of a number, the pointer may point at a '_'.  If the leading part of the
 * string is not a valid number, returns pointer to first non-whitespace
 * character in the string.  Note that number can be valid and still overflow !
 *
 * Returns:  *signed* long -- LONG_MIN if failed
 *
 *           *p_tox  ==  strtox_signed  => OK, sign seen (+ or -)
 *                   ==  strtox_ok      => OK, no sign
 *                   ==  strtox_invalid => badly formed number
 *                   ==  strtox_range   => out of range for signed long
 */
extern long
strtol_x(const char* restrict str, strtox_t* p_tox, char** p_end)
{
  const char* start ;
  ulong uval ;
  int   sign ;

  /* Establish sign if any -- eats leading whitespace, then if finds sign,
   * any following whitespace/'_'.
   */
  while (isspace((int)*str))
    ++str ;

  start = str ;

  sign = 0 ;
  if ((*str == '-') || (*str == '+'))
    {
      sign = (*str == '-') ? -1 : +1 ;

      do
        ++str ;
      while (isspace((int)*str) || (*str == '_')) ;
    } ;

  /* Now get unsigned value.
   */
  uval = strtoul_x(str, p_tox, p_end) ;

  if (*p_tox < 0)
    {
      /* Number no good -- if is badly formed, set any p_end back to the
       *                   start of the number (before any sign).
       */
      if ((*p_tox == strtox_invalid) && (p_end != NULL))
        *p_end = miyagi(start) ;

      return LONG_MIN ;
    } ;

  /* Worry about sign and range.
   *
   * By C99 DEFINITION: an unsigned integer type is: 0..((2**N)-1)
   *
   *                       the signed equivalent is:  -(2**M)   ..((2**M)-1)
   *                                             or: -((2**M)-1)..((2**M)-1)
   *
   *             where: M <= N
   *
   * Conversion signed -> unsigned of the same type is trivial for +ve values.
   * For -ve values, adds 2**N.
   *
   * Conversion unsigned -> signed is "implementation defined" for values
   * greater than ((2**M)-1)
   *
   * Usually M == N-1 -- but that is not guaranteed.
   */
  if (sign >= 0)
    {
      qassert((sign == strtox_signed) || (sign == strtox_ok)) ;
      qassert(strtox_ok == 0) ;         /* as promised for *p_tox */
      *p_tox = sign ;

      /* +ve is easy -- is OK unless > LONG_MAX
       */
      if (uval <= (ulong)LONG_MAX)
        return uval ;                   /* +ve success          */
    }
  else
    {
      *p_tox = strtox_signed ;

      /* -ve is not so easy -- is OK if <= LONG_MAX
       *                       is OK if == -LONG_MIN
       *
       * If OK, need to negate.
       *
       * Getting -LONG_MIN is tricky -- (theoretically) possible M != N-1
       *
       * Negating value > LONG_MAX is also tricky !
       */
      confirm(((LONG_MIN + LONG_MAX) ==  0) ||  /* -LONG_MIN == LONG_MAX     */
              ((LONG_MIN + LONG_MAX) == -1)) ;  /* -LONG_MIN == LONG_MAX + 1 */

      if (uval <= (ulong)LONG_MAX)
        return -((long)uval) ;          /* -ve success          */

      if ((LONG_MIN + LONG_MAX) == -1)
        if (uval == ((ulong)LONG_MAX + 1))
          return LONG_MIN ;             /* extreme -ve success  */
    } ;

  /* Range error
   */
  *p_tox = strtox_range ;

  return LONG_MIN ;
} ;

/*------------------------------------------------------------------------------
 * Extended strtoul()
 *
 * See strtol_x() for description of p_end.
 *
 * Returns:  *unsigned* long -- 0 if failed
 *
 *           *p_tox  ==  strtox_ok      => OK, no sign
 *                   ==  strtox_invalid => badly formed number
 *                   ==  strtox_range   => out of range for signed long
 */
extern ulong
strtoul_x(const char* restrict str, strtox_t* p_tox, char** p_end)
{
  ulong uval ;
  int   base ;
  char  stripped[sizeof(ulong) * 3] ;
  const char* start ;
  char* q, * e ;
  int   (* isd)(int c) ;
  bool  expect_overflow ;

  /* We want stripped buffer which is long enough so that a digit string which
   * is truncated to fit will overflow, even if the first digit is '1' (and
   * assuming the first digit is not '0' !).
   *
   * The estimate of the number of digits to achieve this is based on 3 bits
   * per digit, which is an overestimate, since we don't do octal !
   */
  confirm((((sizeof(ulong) * 8) + 5) / 3) < sizeof(stripped)) ;

  /* Establish base -- eats leading whitespace, base marker and leading zeros
   *                   with any '_'.
   *
   * Ends up pointing at first character which is either the first significant
   * digit, or some invalid character (possibly a leading '_' !)
   *
   * We set start to point at the first non-whitespace.
   */
  while (isspace((int)*str))
    ++str ;

  start = str ;                 /* for invalid return   */
  base  = 10 ;
  isd   = isdigit ;

  if (*str == '0')
    {
      /* Leading base marker and/or zeros
       */
      if ((*(str+1) == 'x') || (*(str+1) == 'X'))
        {
          /* Switch to base 16 and step past "0x"/"0X" and any following '_'.
           *
           * If next character is not a digit, then whole thing is invalid, and
           * we return the "start" pointer.
           */
          base = 16 ;
          isd  = isxdigit ;
          str += 2 ;

          while (*str == '_')
            ++str ;
        } ;

      /* Now, strip leading zeros and any '_', until find a non-zero digit.
       *
       * Note that if we hit something which is not a digit, we backtrack to
       * the last '0' -- which mat or may not have trailing '_'.
       */
      while (*str == '0')
        {
          const char* s ;

          s = str ;

          ++str ;
          while (*str == '_')
            ++str ;

          if (!isd((int)*str))
            {
              str = s ;
              break ;
            } ;
        } ;
    } ;

  /* Must now have a digit -- otherwise is invalid.
   */
  if (!isd((int)*str))
    goto invalid ;              /* require at least one digit   */

  /* Have at least one significant digit -- move all significant digits to the
   * stripped buffer, removing any embedded '_'.
   *
   * Sets expect_overflow if exhaust the stripped buffer.
   *
   * Note that from now on, if p_end != NULL the result is a valid number,
   * even if overflows, so p_end will be set to the first character after the
   * number.
   */
  q = stripped ;
  e = stripped + sizeof(stripped) - 1 ;

  expect_overflow = false ;     /* so far so good               */

  while (1)
    {
      const char* s ;

      if (q < e)
        *q++ = *str ;
      else
        expect_overflow = true ;

      ++str ;
      if (isd((int)*str))
        continue ;              /* easy decision                */

      if (*str != '_')
        break ;                 /* ditto                        */

      /* Found a '_' -- skip across and continue if then get a digit, otherwise
       *                backtrack to first '_' and exit.
       */
      s = str ;

      do
        ++str ;
      while (*str == '_') ;

      if (isd((int)*str))
        continue ;

      str = s ;
      break ;
    } ;

  *q = '\0' ;                   /* complete stripped    */

  /* Worry about p_end or trailing whitespace or '_'.
   */
  if (p_end == NULL)
    {
      /* Require number and only number, but allow trailing whitespace.
       */
      if (*str != '\0')
        {
          while (isspace((int)*str))
            ++str ;

          if (*str != '\0')
            goto invalid ;
        } ;
    }
  else
    *p_end = miyagi(str) ;

  /* Let strtoul() rip on the digits we have stripped out of the given
   * string -- it should read them all, and if we expect_overflow, return
   * that.
   */
  errno = 0 ;
  uval = strtoul(stripped, &e, base) ;

  qassert(*e == '\0') ;
  if (*e != '\0')
    {
      /* Rats... the string we gave to strtoul contains a character which
       * is not part of the number according to strtoul !
       *
       * This is something of a puzzle, because have only given it decimal or
       * hex digits !
       */
      goto invalid ;            /* treat entire number as invalid !     */
    } ;

  if ((errno == 0) && (!expect_overflow))
    {
      *p_tox = strtox_ok ;
      return uval ;             /* Success !                            */
    } ;

  /* Reject the number -- set p_end to current str, errno already set.
   *
   * NB: we expect only ERANGE here... force it in any case.
   *
   * NB: if we expect_overflow, but do not get it, the qassert should spring,
   *     but otherwise we force a range error.
   */
  qassert(errno == ERANGE) ;

  *p_tox = strtox_range ;
  if (p_end != NULL)
    *p_end = miyagi(str) ;
  return 0 ;

  /* Invalid -- number is not well formed, or is not the only thing in the
   *            given string.
   */
invalid:
  *p_tox = strtox_invalid ;
  if (p_end != NULL)
    *p_end = miyagi(start) ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Extended strtol() with check against given range.
 *
 * See strtol_x() for description of p_end.
 *
 * Returns:  *signed* long -- LONG_MIN if failed
 *
 *           *p_tox  ==  strtox_signed  => OK, sign seen (+ or -)
 *                   ==  strtox_ok      => OK, no sign
 *                   ==  strtox_invalid => badly formed number
 *                   ==  strtox_range   => out of range for signed long
 */
extern long
strtol_xr(const char* restrict str, int* p_tox, char** p_end, long min,
                                                              long max)
{
  long val ;

  val = strtol_x(str, p_tox, p_end) ;

  if (*p_tox != strtox_ok)
    return min ;

  if ((val >= min) && (val <= max))
    return val ;

  *p_tox = strtox_range ;
  return min ;
} ;

/*------------------------------------------------------------------------------
 * Extended strtoul() with check against given range.
 *
 * See strtol_x() for description of p_end.
 *
 * Returns:  *unsigned* long -- given min if failed
 *
 *           *p_tox  ==  strtox_ok      => OK, no sign
 *                   ==  strtox_invalid => badly formed number
 *                   ==  strtox_range   => out of range for signed long
 */
extern ulong
strtoul_xr(const char* restrict str, int* p_tox, char** p_end, ulong min,
                                                               ulong max)
{
  ulong uval ;

  uval = strtoul_x(str, p_tox, p_end) ;

  if (*p_tox != strtox_ok)
    return min ;

  if ((uval >= min) && (uval <= max))
    return uval ;

  *p_tox = strtox_range ;
  return min ;
} ;

/* Some string handling
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

#include "qfstring.h"
#include "zassert.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Initialise qf_str -- to given size (which includes the '\n')
 *
 * Sets pointers and terminates an empty string with one byte reserved for the
 * terminating '\n'.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_init(qf_str qfs, char* str, size_t size)
{
  assert(size > 0) ;

  qfs->str = str ;
  qfs->end = str + size - 1 ;

  *str = '\0' ;
  qfs->ptr = str ;
} ;

/*------------------------------------------------------------------------------
 * Initialise qf_str which already contains string -- to given size (which
 * includes the '\n')
 *
 * This may be used to prepare for appending to a buffer which already contains
 * something.
 *
 * Sets pointers, setting the write pointer to the existing terminating '\n'.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_init_as_is(qf_str qfs, char* str, size_t size)
{
  assert(size > 0) ;

  qfs->str = str ;
  qfs->end = str + size - 1 ;

  qfs->ptr = strchr(str, '\0') ;
} ;

/*------------------------------------------------------------------------------
 * Terminate string with the given string.
 *
 * If necessary, characters are discarded from the end of the string in order
 * to fit in the terminating stuff.
 *
 * If the terminating stuff won't fit, as much of the end if the terminating
 * stuff as possible is copied to the string -- displacing any existing
 * contents.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_term(qf_str qfs, const char* src)
{
  int len ;
  int excess ;

  if ((src == NULL) || (*src == '\0'))
    {
      *qfs->ptr = '\0' ;        /* should be true anyway        */
      return ;
    } ;

  len = strlen(src) ;
  excess = qfs_len(qfs) - len ;
  if (excess > 0)
    {
      if (excess <= (qfs->ptr - qfs->str))
        qfs->ptr -= excess ;
      else
        {
          int want = len ;
          len = qfs->end - qfs->str ;   /* take what can...     */
          src += (want - len) ;         /* ... from the end     */
          qfs->ptr = qfs->str ;
        } ;
    } ;

  memcpy(qfs->ptr, src, len + 1) ;      /* include the '\0'     */
  qfs->ptr += len ;
} ;

/*==============================================================================
 * Appending to the string
 */

/*------------------------------------------------------------------------------
 * Append as much as possible of the source string to the given qf_str.
 *
 * May append nothing at all !
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_append(qf_str qfs, const char* src)
{
  int n ;

  if ((src == NULL) || (*src == '\0'))
    return ;

  n = strlen(src) ;

  if (n > qfs_left(qfs))
    n = qfs_left(qfs) ;

  if (n == 0)
    return ;

  memcpy(qfs->ptr, src, n + 1) ;
  qfs->ptr += n ;
} ;

/*------------------------------------------------------------------------------
 * Append as much as possible of the first 'n' bytes of the source string to
 * the given qf_str.
 *
 * May append nothing at all !
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_append_n(qf_str qfs, const char* src, size_t n)
{
  if ((int)n > qfs_left(qfs))
    n = qfs_left(qfs) ;

  if (n <= 0)
    return ;

  memcpy(qfs->ptr, src, n) ;
  qfs->ptr += n ;

  *qfs->ptr = '\0' ;
} ;

/*------------------------------------------------------------------------------
 * Append upto 'n' copies of the given character to the qf_str
 *
 * May append nothing at all !
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_append_ch_x_n(qf_str qfs, char ch, size_t n)
{
  if ((int)n > qfs_left(qfs))
    n = qfs_left(qfs) ;

  if (n <= 0)
    return ;

  while (n--)
    *qfs->ptr++ = ch ;

  *qfs->ptr = '\0' ;
} ;

/*------------------------------------------------------------------------------
 * Append as much as possible of the source string to the given qf_str, left or
 * right justified to the given width.
 *
 * Ignores the width if the string is longer than it.
 *
 * Negative width => left justify.
 *
 * May append nothing at all !
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_append_justified(qf_str qfs, const char* src, int width)
{
  size_t n ;

  if ((src == NULL) || (*src == '\0'))
    n = 0 ;
  else
    n = strlen(src) ;

  qfs_append_justified_n(qfs, src, n, width) ;
} ;

/*------------------------------------------------------------------------------
 * Append as much as possible of the first 'n' bytes of the source string to
 * the given qf_str, left or right justified to the given width.
 *
 * Ignores the width if the string is longer than it.
 *
 * Negative width => left justify.
 *
 * May append nothing at all !
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_append_justified_n(qf_str qfs, const char* src, size_t n, int width)
{
  if ((int)n >= abs(width))
    width = 0 ;

  if (width > 0)
    qfs_append_ch_x_n(qfs, ' ', width - n) ;

  qfs_append_n(qfs, src, n) ;

  if (width < 0)
    qfs_append_ch_x_n(qfs, ' ', - width - n) ;
} ;

/*==============================================================================
 * Number conversion
 */

static void
qfs_number(qf_str qfs, uintmax_t val, int sign, enum pf_flags flags,
                                                     int width, int precision) ;

/*------------------------------------------------------------------------------
 * Signed integer -- converted as per flags, width and precision.
 *
 * Result is appended to the given qf_str.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_signed(qf_str qfs, intmax_t s_val, enum pf_flags flags,
                                                       int width, int precision)
{
  uintmax_t u_val ;
  int       sign ;

  if (s_val < 0)
    {
      sign  = -1 ;
      u_val = (uintmax_t)(-(s_val + 1)) + 1 ;
    }
  else
    {
      sign  = +1 ;
      u_val = s_val ;
    } ;

  qfs_number(qfs, u_val, sign, flags & ~pf_unsigned, width, precision) ;
} ;

/*------------------------------------------------------------------------------
 * Unsigned integer -- converted as per flags, width and precision.
 *
 * Result is appended to the given qf_str.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_unsigned(qf_str qfs, uintmax_t u_val, enum pf_flags flags,
                                                       int width, int precision)
{
  qfs_number(qfs, u_val, 0, flags | pf_unsigned, width, precision) ;
} ;

/*------------------------------------------------------------------------------
 * Address -- converted as per flags, width and precision.
 *
 * Result is appended to the given qf_str.
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_pointer(qf_str qfs, void* p_val, enum pf_flags flags,
                                                       int width, int precision)
{
  confirm(sizeof(uintmax_t) >= sizeof(uintptr_t)) ;
  qfs_number(qfs, (uintptr_t)p_val, 0, flags | pf_unsigned, width, precision) ;
} ;

/*------------------------------------------------------------------------------
 * Number conversion function.
 *
 * All number conversion ends up here.
 *
 * Accepts: pf_commas     -- format with commas
 *          pf_plus       -- requires '+' or '-'
 *          pf_space      -- requires space or '-'
 *          pf_zeros      -- zero fill to width
 *          pf_alt        -- add '0x' or '0X' if hex (no effect on decimal)
 *
 *          pf_precision  -- explicit precision (needed if precision == 0)
 *
 *          pf_hex        -- render in hex
 *          pf_uc         -- render in upper case
 *
 *          pf_unsigned   -- value is unsigned
 *          pf_ptr        -- value is a void* pointer
 *
 * NB: pf_hex does NOT imply pf_unsigned.
 *     pf_uc  does NOT imply pf_hex
 *
 * If the width is < 0  -- left justify in abs(width) -- zero fill ignored
 *                == 0  -- no width                   -- zero fill ignored
 *                 > 0  -- right justify in width     -- zero filling if req.
 *
 * If the precision is < 0 it is ignored (unless pf_hex, see below).
 *
 * If the precision is 0 it is ignored unless bf_precision is set.
 *
 * Precedence issues:
 *
 *   * precision comes first.  Disables zero fill.
 *
 *   * commas come before zero fill.
 *
 *   * signs and prefixes come before zero fill
 *
 *   * pf_plus takes precedence over pf_space
 *
 *   * pf_unsigned or sign == 0 takes precedence over pf_plus and pf_space.
 *
 * For hex output, pf_commas groups digits in 4's, separated by '_'.
 *
 * For hex output if precision is:
 *
 *   -1 set precision to multiple of 2, just long enough for the value
 *   -2 set precision to multiple of 4, just long enough for the value
 *
 * (under all other conditions, -ve precision is ignored).
 *
 * Note: if the precision is explicitly 0, and the value is 0, and no other
 *       characters are to be generated -- ie no: pf_plus, pf_space, pf_zeros,
 *       or pf_alt (with pf_hex) -- then nothing is generated.
 *
 * This operation is async-signal-safe.
 */
static void
qfs_number(qf_str qfs, uintmax_t val, int sign, enum pf_flags flags,
                                                       int width, int precision)
{
  enum
  {
    max_bits   = 256,   /* size of number can convert           */
    max_digits =  90,   /* could do octal !                     */
    buf_size   = 128,   /* buffer to use for that               */
  } ;

  confirm((sizeof(uintmax_t) * 8) <= max_bits) ;  /* check max_bits     */
  confirm((max_digits * 3) >= max_bits) ;         /* check max_digits   */

  /* Buffer requires space for sign, '0x', digits, '00', commas, '\0'
   *
   * The '00' is for zero fill will commas, and is enough to extend the
   * number to "000,...." -- that is, a full leading triple.
   */
  confirm(buf_size > (1 + 2 + max_digits + (2 + (max_digits / 3)) + 1)) ;

  /* For hex commas the sum is similar, but smaller.                    */
  confirm((3 + (max_digits / 4)) < (2 + (max_digits / 3))) ;

  unsigned    base ;
  const char* digits ;
  const char* radix_str ;
  const char* sign_str ;
  char  num[buf_size] ;
  char* p ;
  char* e ;
  int len ;
  int radix_len ;
  int sign_len ;
  uintmax_t v ;

  char  comma ;
  int   interval ;

  int   zeros ;

  static const char lc[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                             'a', 'b', 'c', 'd', 'e', 'f' } ;
  static const char uc[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                             'A', 'B', 'C', 'D', 'E', 'F' } ;

  /* Tidy up the options                                                */
  if (precision < 0)
    {
      if ((flags & pf_hex) && (precision >= -2))
        {
          /* special precision for hex output           */
          int unit = (precision == -1) ? 2 : 4 ;
          v = val | 1 ;
          precision = 0 ;
          while (v != 0)
            {
              precision += unit ;
              v >>= (unit * 4) ;
            } ;
        }
      else
        {
          /* mostly, -ve precision is ignored           */
          precision = 0 ;
          flags &= ~pf_precision ;      /* ignore precision < 0 */
        } ;
    } ;

  if (precision > 0)
    flags |= pf_precision ;     /* act on precision > 0 */

  if ((flags & pf_precision) || (width <= 0))
    flags &= ~pf_zeros ;        /* turn off zero fill           */

  /* Set up any required sign and radix prefix                          */
  if ((flags & pf_unsigned) || (sign == 0))
    sign_str = "" ;
  else if (sign < 0)
    sign_str = "-" ;
  else if (flags & pf_plus)
    sign_str = "+" ;
  else if (flags & pf_space)
    sign_str = " " ;
  else
    sign_str = "" ;

  sign_len = strlen(sign_str) ;

  radix_str = "" ;
  if ((flags & (pf_hex | pf_alt)) == (pf_hex | pf_alt))
    radix_str = (flags & pf_uc) ? "0X" : "0x" ;

  radix_len = strlen(radix_str) ;

  /* Turn off zero fill if left justify (width < 0)                     */
  if (width < 0)
    flags &= ~pf_zeros ;

  /* Special case of explicit zero precision and value == 0             */
  if ((flags & pf_precision) && (precision == 0) && (val == 0))
    {
      if (((flags & pf_zeros) == 0) && (sign_len == 0) && (radix_len == 0))
        {
          qfs_append_justified_n(qfs, NULL, 0, width) ;
          return ;
        } ;
    } ;

  /* Start with the basic digit conversion.                             */
  base   = (flags & pf_hex) ? 16 : 10 ;
  digits = (flags & pf_uc)  ? uc : lc ;

  e = p = num + sizeof(num) - 1 ;
  *p = '\0' ;
  v = val ;
  do
    {
      *--p = digits[v % base] ;
      v /= base ;
    } while ((v > 0) && (p > num)) ;

  assert(v == 0) ;

  len = e - p ;

  /* Worry about the precision          */
  while ((precision > len) && (len < max_digits))
    {
      *--p = '0' ;
      ++len ;
    } ;

  /* Worry about commas                 */
  comma    = (flags & pf_hex) ? '_' : ',' ;
  interval = (flags & pf_hex) ?  4  :  3  ;

  if (flags & pf_commas)
    {
      int   c ;
      int   t ;
      char* cq ;
      char* cp ;

      c = (len - 1) / interval ;        /* number of commas to insert   */
      t = len % interval ;              /* digits before first comma    */
      if (t == 0)
        t = interval ;

      len += c ;                        /* account for the commas       */

      cq = p ;
      p -= c ;
      cp = p ;

      assert(p > num) ;

      while (c--)
        {
          while (t--)
            *cp++ = *cq++ ;
          *cp++ = comma ;
        } ;

      assert(len == (e - p)) ;

      /* commas and zero fill interact.  Here fill the leading group.   */
      zeros = width - (sign_len + radix_len + len) ;
      if ((flags & pf_zeros) && (zeros > 0))
        {
          int group_fill = interval - (len % (interval + 1)) ;
          assert(group_fill < interval) ;
          if (group_fill > zeros)
            group_fill = zeros ;

          len += group_fill ;
          while (group_fill--)
            {
              assert(p > num) ;
              *--p = '0' ;
            } ;
        } ;
    } ;

  assert(len == (e - p)) ;

  /* See if still need to worry about zero fill                         */
  zeros = width - (sign_len + radix_len + len) ;
  if ((flags & pf_zeros) && (zeros > 0))
    {
      /* Need to insert zeros and possible commas between sign and radix
       * and the start of the number.
       *
       * Note that for commas the number has been arranged to have a full
       * leading group.
       *
       * The width can be large... so do this by appending any sign and
       * radix to the qf_str, and then the required leading zeros (with or
       * without commas).
       */
      if (sign_len != 0)
        qfs_append_n(qfs, sign_str, sign_len) ;

      if (radix_len != 0)
        qfs_append_n(qfs, radix_str, radix_len) ;

      if (flags & pf_commas)
        {
          /* Leading zeros with commas !
           *
           * Start with ',', '0,', '00,' etc to complete the first group.
           * Thereafter add complete groups.
           */
          int g ;
          int r ;
          g = (zeros + interval - 1) / (interval + 1) ;
          r = (zeros - 1)            % (interval + 1) ;

          if (r == 0)
            {
              qfs_append_ch_x_n(qfs, comma, 1) ;
              r = interval ;
            }

          while (g--)
            {
              qfs_append_ch_x_n(qfs, '0', r) ;
              qfs_append_ch_x_n(qfs, comma, 1) ;
              r = interval ;
            } ;
        }
      else
        qfs_append_ch_x_n(qfs, '0', zeros) ;

      width = 0 ;               /* have dealt with the width.   */
    }
  else
    {
      /* No leading zeros, so complete the number by adding any sign
       * and radix.
       */
      char* cp ;

      p   -= sign_len + radix_len ;
      len += sign_len + radix_len ;
      assert(p >= num) ;

      cp = p ;
      while (sign_len--)
        *cp++ = *sign_str++ ;
      while (radix_len--)
        *cp++ = *radix_str++ ;
    } ;

  /* Finally, can append the number -- respecting any remaining width   */
  assert(len == (e - p)) ;

  qfs_append_justified_n(qfs, p, len, width) ;
} ;

/*==============================================================================
 * printf() and vprintf() type functions
 */

enum pf_phase
{
  pfp_null,             /* in ascending order   */
  pfp_flags,
  pfp_width,
  pfp_precision,
  pfp_num_type,

  pfp_done,
  pfp_failed
} ;

/* Number types for printing                                    */
enum arg_num_type
{
  ant_char,             /* hh           */
  ant_short,            /* h            */
  ant_int,              /* default      */
  ant_long,             /* l            */
  ant_long_long,        /* ll           */
  ant_intmax_t,         /* j            */
  ant_size_t,           /* z            */
  ant_ptr_t,            /* void*        */

  ant_default    = ant_int,
};

static enum pf_phase qfs_arg_string(qf_str qfs, va_list args,
                                enum pf_flags flags, int width, int precision) ;
static enum pf_phase qfs_arg_char(qf_str qfs, va_list args,
                                enum pf_flags flags, int width, int precision) ;
static enum pf_phase qfs_arg_number(qf_str qfs, va_list args,
         enum pf_flags flags, int width, int precision, enum arg_num_type ant) ;

/*------------------------------------------------------------------------------
 * Formatted print to qf_str -- cf printf()
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_printf(qf_str qfs, const char* format, ...)
{
  va_list args;

  va_start (args, format);
  qfs_vprintf(qfs, format, args);
  va_end (args);
} ;

/*------------------------------------------------------------------------------
 * Formatted print to qf_str -- cf vprintf()
 *
 * This operation is async-signal-safe.
 */
extern void
qfs_vprintf(qf_str qfs, const char *format, va_list args)
{
  if (format == NULL)
    return ;

  while ((qfs->ptr < qfs->end) && (*format != '\0'))
    {
      /* Have space for one byte and current format byte is not '\0'    */
      if (*format != '%')
        *qfs->ptr++ = *format++ ;
      else
        {
          const char* start = format++ ;  /* start points at the '%' ...
                                             ... step past it now       */
          bool star      = false ;
          bool digit     = false ;
          int d          = 0 ;
          int width_sign = +1 ;
          int width      = 0 ;
          int precision  = 0 ;
          enum arg_num_type ant = ant_default ;
          enum pf_flags flags   = pf_none ;
          enum pf_phase phase   = pfp_null ;

          while (phase < pfp_done)
            {
              switch (*format++)        /* get next and step past it    */
              {
                case '%':       /* %% only                              */
                  if (phase == pfp_null)
                    *qfs->ptr++ = '%' ;
                  phase = (phase == pfp_null) ? pfp_done : pfp_failed ;
                  break ;

                case '\'':
                  flags |= pf_commas ;
                  phase = (phase <= pfp_flags) ? pfp_flags : pfp_failed ;
                  break ;

                case '-':
                  width_sign = -1 ;
                  phase = (phase <= pfp_flags) ? pfp_flags : pfp_failed ;
                  break ;

                case '+':
                  flags |= pf_plus ;
                  phase = (phase <= pfp_flags) ? pfp_flags : pfp_failed ;
                  break ;

                case ' ':
                  flags |= pf_space ;
                  phase = (phase <= pfp_flags) ? pfp_flags : pfp_failed ;
                  break ;

                case '0':
                  if (phase <= pfp_flags)
                    {
                      flags |= pf_zeros ;
                      phase = pfp_flags ;
                      break ;
                    } ;
                  /* fall through       */
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                  d = *(format - 1) - '0' ;
                  if      (!star && (phase <= pfp_width))
                    {
                      phase = pfp_width ;
                      width = (width * 10) + (d * width_sign) ;
                    }
                  else if (!star && (phase == pfp_precision))
                    precision = (precision * 10) + d ;
                  else
                    phase = pfp_failed ;

                  digit = true ;
                  break ;

                case '*':
                  if      (!star && !digit && (phase <= pfp_width))
                    {
                      phase = pfp_width ;
                      width = va_arg(args, int) ;
                    }
                  else if (!star && !digit && (phase == pfp_precision))
                    {
                      precision = va_arg(args, int) ;
                      if (precision < 0)
                        {
                          precision = 0 ;
                          flags &= ~pf_precision ;
                        } ;
                    }
                  else
                    phase = pfp_failed ;

                  star = true ;
                  break ;

                case '.':
                  phase = (phase <= pfp_precision) ? pfp_precision : pfp_failed;
                  flags |= pf_precision ;
                  precision = 0 ;
                  break ;

                case 'l':       /* 1 or 2 'l', not 'h', 'j' or 'z'      */
                  phase = pfp_num_type ;
                  if      (ant == ant_default)
                    ant = ant_long ;
                  else if (ant == ant_long)
                    ant = ant_long_long ;
                  else
                    phase = pfp_failed ;
                  break ;

                case 'h':       /* 1 or 2 'h', not 'l', 'j' or 'z'      */
                  phase = pfp_num_type ;
                  if      (ant == ant_default)
                    ant = ant_short ;
                  else if (ant == ant_short)
                    ant = ant_char ;
                  else
                    phase = pfp_failed ;
                  break ;

                case 'j':       /* 1 'j', not 'h', 'l' or 'z'           */
                  phase = (phase <= pfp_num_type) ? pfp_num_type : pfp_failed ;
                  ant = ant_intmax_t ;
                  break ;

                case 'z':       /* 1 'z', not 'h', 'l' or 'j'           */
                  phase = (phase <= pfp_num_type) ? pfp_num_type : pfp_failed ;
                  ant = ant_size_t ;
                  break ;

                case 's':
                  if (phase == pfp_num_type)
                    phase = pfp_failed ;        /* don't do 'l' etc.    */
                  else
                    phase = qfs_arg_string(qfs, args, flags, width, precision) ;
                  break ;

                case 'c':
                  if (phase == pfp_num_type)
                    phase = pfp_failed ;        /* don't do 'l' etc.    */
                  else
                    phase = qfs_arg_char(qfs, args, flags, width, precision) ;
                  break ;

                case 'd':
                case 'i':
                  phase = qfs_arg_number(qfs, args, flags, width, precision,
                                                                          ant) ;
                  break ;

                case 'u':
                  phase = qfs_arg_number(qfs, args, flags | pf_unsigned, width,
                                                               precision, ant) ;
                  break ;

                case 'x':
                  phase = qfs_arg_number(qfs, args, flags | pf_hex_x, width,
                                                               precision, ant) ;
                  break ;

                case 'X':
                  phase = qfs_arg_number(qfs, args, flags | pf_hex_X, width,
                                                               precision, ant) ;
                  break ;

                case 'p':
                  if (phase == pfp_num_type)
                    phase = pfp_failed ;
                  else
                    phase = qfs_arg_number(qfs, args, flags | pf_void_p, width,
                                                         precision, ant_ptr_t) ;
                  break ;

                default:                /* unrecognised format          */
                  phase = pfp_failed ;
                  break ;
              } ;
            } ;

          if (phase == pfp_failed)
            {
              format = start ;          /* back to the start            */
              *qfs->ptr++ = *format++ ; /* copy the '%'                 */
            } ;
        } ;
    } ;

  *qfs->ptr = '\0' ;
} ;

/*------------------------------------------------------------------------------
 * %s handler -- tolerates NULL pointer
 *
 * Accepts:    width
 *             precision
 *             pf_precision -- explicit precision
 *
 * Rejects:    pf_commas    -- "'" seen
 *             pf_plus      -- "+" seen
 *             pf_space     -- " " seen
 *             pf_zeros     -- "0" seen
 *             pf_alt       -- "#" seen
 *
 * Won't get:  pf_hex
 *             pf_uc
 *             pf_unsigned
 *             pf_ptr
 *
 * This operation is async-signal-safe.
 */
static enum pf_phase
qfs_arg_string(qf_str qfs, va_list args, enum pf_flags flags,
                                                       int width, int precision)
{
  const char* src ;
  int len ;

  src = va_arg(args, char*) ;

  if (flags != (flags & pf_precision))
    return pfp_failed ;

  len = (src != NULL) ? strlen(src) : 0 ;
  if (((precision > 0) || (flags & pf_precision)) && (len > precision))
    len = precision ;

  qfs_append_justified_n(qfs, src, len, width) ;

  return pfp_done ;
} ;

/*------------------------------------------------------------------------------
 * %c handler
 *
 * Accepts:    width
 *
 * Rejects:    precision
 *             pf_precision -- explicit precision
 *             pf_commas    -- "'" seen
 *             pf_plus      -- "+" seen
 *             pf_space     -- " " seen
 *             pf_zeros     -- "0" seen
 *             pf_alt       -- "#" seen
 *
 * Won't get:  pf_hex
 *             pf_uc
 *             pf_unsigned
 *             pf_ptr
 *
 * This operation is async-signal-safe.
 */
static enum pf_phase
qfs_arg_char(qf_str qfs, va_list args, enum pf_flags flags,
                                                       int width, int precision)
{
  unsigned char ch ;

  ch = va_arg(args, int) ;

  if ((flags != 0) || (precision != 0))
    return pfp_failed ;

  qfs_append_justified_n(qfs, (char*)&ch, 1, width) ;

  return pfp_done ;
} ;

/*------------------------------------------------------------------------------
 * %d, %i, %u, %x, %X and %p handler
 *
 * Accepts: pf_commas     -- format with commas
 *          pf_minus      -- left justify (any width will be -ve)
 *          pf_plus       -- requires sign
 *          pf_space      -- requires space or '-'
 *          pf_zeros      -- zero fill to width
 *          pf_alt        -- '0x' or '0X' for hex
 *
 *          pf_precision  -- precision specified
 *
 *          pf_unsigned   -- value is unsigned
 *          pf_ptr        -- value is a void* pointer
 *          pf_hex        -- render in hex
 *          pf_uc         -- render hex in upper case
 *
 *     and: all the number argument types.
 *
 * This operation is async-signal-safe.
 */
static enum pf_phase
qfs_arg_number(qf_str qfs, va_list args, enum pf_flags flags,
                                int width, int precision, enum arg_num_type ant)
{
  uintmax_t     u_val ;
  intmax_t      s_val ;

  /* Special for hex with '0...  if no explicit precision, set -1 for byte
   * and -2 for everything else -- see qfs_number().
   */
  if (((flags & pf_precision) == 0) && (flags & pf_hex))
    {
      if ((flags & (pf_commas | pf_zeros)) == (pf_commas | pf_zeros))
        {
          precision = (ant == ant_char) ? -1 : -2 ;
          flags |= pf_precision ;
        } ;
    } ;

  /* It is assumed that all values can be mapped to a uintmax_t         */
  confirm(sizeof(uintmax_t) >= sizeof(uintptr_t)) ;

  if (flags & pf_unsigned)
    {
      switch (ant)
      {
        case ant_char:
        case ant_short:
          u_val = va_arg(args, int) ;
          break ;

        case ant_int:
          u_val = va_arg(args, unsigned int) ;
          break ;

        case ant_long:
          u_val = va_arg(args, unsigned long) ;
          break ;

        case ant_long_long:
          u_val = va_arg(args, unsigned long long) ;
          break ;

        case ant_intmax_t:
          u_val = va_arg(args, uintmax_t) ;
          break ;

        case ant_size_t:
          u_val = va_arg(args, size_t) ;
          break ;

        case ant_ptr_t:
          u_val = va_arg(args, uintptr_t) ;
          break ;

        default:
          zabort("impossible integer size") ;
      } ;

      qfs_unsigned(qfs, u_val, flags, width, precision) ;
    }
  else
    {
      switch (ant)
      {
        case ant_char:
        case ant_short:
          s_val = va_arg(args, int) ;
          break ;

        case ant_int:
          s_val = va_arg(args, signed int) ;
          break ;

        case ant_long:
          s_val = va_arg(args, signed long) ;
          break ;

        case ant_long_long:
          s_val = va_arg(args, signed long long) ;
          break ;

        case ant_intmax_t:
          s_val = va_arg(args, intmax_t) ;
          break ;

        case ant_size_t:
          s_val = va_arg(args, ssize_t) ;
          break ;

        case ant_ptr_t:
          s_val = va_arg(args, intptr_t) ;
          break ;

        default:
          zabort("impossible integer size") ;
      } ;

      qfs_signed(qfs, s_val, flags, width, precision) ;
    } ;

  /* construct a digit string, the hard way                     */

  return pfp_done ;
} ;


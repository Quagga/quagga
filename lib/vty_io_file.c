/* VTY I/O for Files
 *
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

#include "command_local.h"

#include "vty_io_file.h"
#include "vty_io_basic.h"

/*==============================================================================
 * VTY File Output
 *
 * This is for input and output of configuration files and piped stuff.
 *
 * When reading the configuration (and piped stuff in the configuration) I/O
 * is blocking... nothing else can run while this is going on.  Otherwise,
 * all I/O is non-blocking.
 */



/*==============================================================================
 * Prototypes.
 */

/*------------------------------------------------------------------------------
 *
 *
 */
extern cmd_return_code_t
uty_file_read_open(vty_io vio, qstring name, bool reflect)
{
  const char* name_str ;
  int   fd ;
  vio_vf vf ;
  vfd_io_type_t iot ;

  name_str = qs_make_string(name) ;

  iot =  vfd_io_read | vfd_io_blocking ;        /* TODO blocking        */

  /* Do the basic file open.                                            */
  fd = uty_vfd_file_open(name_str, iot) ;

  if (fd < 0)
    {
      uty_out(vio, "%% Could not open input file %s\n", name_str) ;

      return CMD_WARNING ;
    }

  /* OK -- now push the new input onto the vin_stack.                   */
  vf = uty_vf_new(vio, name_str, fd, vfd_file, iot) ;
  uty_vin_open(vio, vf, VIN_FILE, NULL, NULL, 16 * 1024) ;

  vf->parse_type      = cmd_parse_strict ;
  vf->reflect_enabled = reflect ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 *
 *
 */
extern cmd_return_code_t
uty_file_write_open(vty_io vio, qstring name, bool append)
{
  const char* name_str ;
  int   fd ;
  vio_vf vf ;
  vfd_io_type_t iot ;

  iot =  vfd_io_write | vfd_io_blocking ;       /* TODO blocking        */

  if (append)
    iot |= vfd_io_append ;

  name_str = qs_make_string(name) ;

  /* Do the basic file open.                                            */
  fd = uty_vfd_file_open(name_str, iot) ;

  if (fd < 0)
    {
      uty_out(vio, "%% Could not open output file %s\n", name_str) ;

      return CMD_WARNING ;
    }

  /* OK -- now push the new input onto the vin_stack.                   */
  vf = uty_vf_new(vio, name_str, fd, vfd_file, iot) ;
  uty_vout_open(vio, vf, VOUT_FILE, NULL, NULL, 16 * 1024) ;

  vf->out_enabled = true ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * Command line fetch from a file or pipe.
 *
 * Returns: CMD_SUCCESS  -- have another command line ready to go
 *          CMD_WAITING  -- do not have a command line at the moment
 *          CMD_EOF      -- ran into EOF
 *          CMD_IO_ERROR -- ran into an I/O error
 */
extern cmd_return_code_t
uty_file_fetch_command_line(vio_vf vf, qstring* line)
{
  assert(vf->vin_state == vf_open) ;

  if (vf->line_complete)
    {
      vio_fifo_set_hold_mark(vf->ibuf) ;        /* advance hold         */

      vf->line_complete = false ;
      vf->line_number  += vf->line_step ;

      qs_set_len_nn(vf->cl, 0) ;
      vf->line_step = 0 ;
    } ;

  while (1)
    {
      char* s, * p, * q, * e ;
      size_t  have ;
      ulen    len ;

      s = vio_fifo_get(vf->ibuf, &have) ;

      /* If nothing in hand, try and get some more.
       *
       * Either exits or loops back to set s & have.
       */
      if (have == 0)
        {
          int get ;

          get = vio_fifo_read_nb(vf->ibuf, vio_vfd_fd(vf->vfd), 100) ;

          if (get > 0)
            continue ;                  /* loop back                    */

          if (get == 0)
            return CMD_WAITING ;        /* need to set read ready !     */

          if (get == -1)
            ;                           /* register error               */

          if (get == -2)
            return (qs_len_nn(vf->cl) > 0) ? CMD_SUCCESS : CMD_EOF ;
        } ;

      /* Try to find a '\n' -- converting all other control chars to ' '
       *
       * When we find '\n' step back across any trailing ' ' (which includes
       * any control chars before the '\n').
       *
       * This means that we cope with "\r\n" line terminators.  But not anything
       * more exotic.
       */
      p = s ;
      e = s + have ;       /* have != 0    */
      q = NULL ;

      while (p < e)
        {
          if (*p++ < 0x20)
            {
              if (*(p-1) != '\n')
                {
                  *(p-1) = ' ' ;        /* everything other than '\n'   */
                  continue ;
                } ;

              ++vf->line_step ;         /* got a '\n'                   */

              q = p ;                   /* point just past '\n'         */
              do --q ; while ((q > s) && (*(q-1) == ' ')) ;
                                        /* discard trailing "spaces"    */
              break ;
            } ;
        } ;

      /* Step past what have just consumed -- we have a hold_mark, so
       * stuff is still in the fifo.
       */
      vio_fifo_step(vf->ibuf, p - s) ;

      /* If not found '\n', then we have a line fragment that needs to be
       * appended to any previous line fragments.
       *
       * Loops back to try to get some more form the fifo.
       */
      if (q == NULL)
        {
          qs_append_n(vf->cl, s, p - s) ;
          continue ;
        } ;

      /* If we have nothing so far, set alias to point at what we have in
       * the fifo.  Otherwise, append to what we have.
       *
       * End up with: s = start of entire line, so far.
       *              p = end of entire line so far.
       */
      len = q - s ;                     /* length to add        */
      if (qs_len_nn(vf->cl) == 0)
        {
          qs_set_alias_n(vf->cl, s, len) ;
          p = q ;
        }
      else
        {
          if (len != 0)
            qs_append_n(vf->cl, s, len) ;

          s = qs_char_nn(vf->cl) ;
          p = s + qs_len_nn(vf->cl) ;

          if ((len == 0) && (p > s) && (*(p-1) == ' '))
            {
              /* Have an empty end of line section, and the last character
               * of what we have so far is ' ', so need now to trim trailing
               * spaces off the stored stuff.
               */
              do --p ; while ((p > s) && (*(p-1) == ' ')) ;

              qs_set_len_nn(vf->cl, p - s) ;
            } ;
        } ;

      /* Now worry about we have a trailing '\'.                        */

      if ((p == s) || (*(p-1) != '\\'))
        break ;                 /* no \ => no continuation => success   */

      /* Have a trailing '\'.
       *
       * If there are an odd number of '\', strip the last one and loop
       * round to collect the continuation.
       *
       * If there are an even number of '\', then this is not a continuation.
       *
       * Note that this rule deals with the case of the continuation line
       * being empty... e.g. ....\\\     n     n  -- where n is '\n'
       */
      q = p ;
      do --q ; while ((q > s) && (*(q-1) == '\\')) ;

      if (((p - q) & 1) == 0)
        break ;                 /* even => no continuation => success   */

      qs_set_len_nn(vf->cl, p - s - 1) ;        /* strip odd '\'        */

      continue ;                /* loop back to fetch more              */
    } ;

  /* Success have a line in hand                                        */

  vf->line_complete = true ;
  *line = vf->cl ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to a file or pipe.
 *
 * Returns: CMD_SUCCESS  -- done
 *          CMD_IO_ERROR -- ran into an I/O error
 */
extern cmd_return_code_t
uty_file_out_push(vio_vf vf)
{
  assert(vf->vout_state == vf_open) ;

  if (vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), false) >= 0)
    return CMD_SUCCESS ;
  else
    return CMD_IO_ERROR ;
} ;

/*------------------------------------------------------------------------------
 * Tidy up after input file has been closed
 */
extern void
uty_file_read_close(vio_vf vf)
{
  return ;
} ;

/*------------------------------------------------------------------------------
 * Flush output buffer and close.
 *
 * Returns: true <=> buffer (now) empty
 */
extern bool
uty_file_write_close(vio_vf vf, bool final)
{
  return vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), true) == 0 ;
} ;

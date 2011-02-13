/* Line Control for VTY Terminal output
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
#include "memory.h"
#include "zassert.h"

#include "vio_lines.h"
#include "qiovec.h"

/*==============================================================================
 * Line control handles the output of simple text to a telnet connection,
 * folding and counting lines (for "--more--" purposes) if required.
 *
 * LIMITATIONS:
 *
 *   1) does not handle '\r' except as part of '\r''\n' pairs.
 *
 *      Telnet requires that bare '\r' be sent as '\r''\0'.  That is not
 *      implemented.
 *
 *      The handling of '\r' which is not part of '\r''\n' is UNDEFINED.
 *      (In particular, the '\r' may be sent as is, or not sent at all.)
 *
 *   2) does not worry about '\t' or '\b' or any other control character.
 *
 *      Apart from '\r' and '\n' all characters are deemed to be printing
 *      characters -- and to have width == 1.
 *
 *   3) has no idea about escape sequences or telnet commands.
 *
 *      In particular: when looking for '\n' (and '\r') has no way of telling
 *      if those are part of an escape sequence.
 *
 *   4) DOES NOT handle 0xFF character value.
 *
 *      For Telnet this should be escaped.  It isn't.
 *
 * Current use of VTY command output will not be troubled by these limitations.
 * To do more would cost code and cpu unnecessarily.
 *
 * WHAT IT DOES DO:
 *
 *   1) maps bare '\n' to '\r''\n'.
 *
 *      Swallows '\r' immediately before '\n' if present.
 *
 *   2) if required, breaks output into screen width chunks, and counts
 *      down the height of a "screen full".
 *
 */

/*==============================================================================
 * Initialise, allocate, reset etc.
 */

/*------------------------------------------------------------------------------
 * Initialise new vio_line_control -- allocate if required.
 *
 * This is for initialising a new structure.  Any current contents are lost.
 *
 * A width of  <= 0 => very large width indeed.
 * A height of <= 0 => indefinite height
 *
 * Pause is unset.  vio_lc_append will collect an indefinite number of lines.
 *
 * Column and line position set to zero.
 *
 * Returns: address of vio_line_control
 */
extern vio_line_control
vio_lc_init_new(vio_line_control lc, int width, int height)
{
  if (lc == NULL)
    lc = XCALLOC(MTYPE_VIO_LC, sizeof(struct vio_line_control)) ;
  else
    memset(lc, 0, sizeof(struct vio_line_control)) ;

  /* Zeroising has set:
   *
   *   pause   = 0  -- no limit on the number of lines to append
   *   paused  = 0  -- not paused
   *
   *   col     = 0  -- at column 0
   *   lines   = 0  -- no lines collected, yet
   *
   *   iov     = all 0   -- empty
   *   writing = 0  -- not writing
   */

  lc->width  = width  >= 0 ? width  : 0 ;
  lc->height = height >= 0 ? height : 0 ;

  return lc ;
} ;

/*------------------------------------------------------------------------------
 * Reset vio_line_control (if any) -- release body and (if required) the
 * structure.
 *
 * Returns: address of vio_line_control (if any) -- NULL if structure released
 */
extern vio_line_control
vio_lc_reset(vio_line_control lc, free_keep_b free_structure)
{
  if (lc != NULL)
    {
      if (free_structure)
        XFREE(MTYPE_VIO_LC, lc) ;       /* sets lc = NULL       */
      else
        vio_lc_init_new(lc, lc->width, lc->height) ;
                                        /* re-initialise        */
    } ;

  return lc ;
} ;

/*------------------------------------------------------------------------------
 * Clear given vio_line_control.
 *
 * Sets:  pause   = 0
 *        paused  = 0
 *        col     = 0
 *        writing = 0
 *
 * NB: it is the callers responsibility to release anything buffered because
 *     it was earlier appended.
 */
extern void
vio_lc_clear(vio_line_control lc)
{
  if (lc == NULL)
    return ;

  qiovec_clear(&lc->qiov) ;

  lc->pause   = 0 ;
  lc->paused  = 0 ;
  lc->col     = 0 ;
  lc->writing = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Sets width and height for line control
 *
 * A width of  <= 0 => very large width indeed.
 * A height of <= 0 => indefinite height
 *
 * Pause is adjusted if it is not zero, and may become zero and set paused.
 */
extern void
vio_lc_set_window(vio_line_control lc, int width, int height)
{
  unsigned old_height ;

  old_height = lc->height ;

  lc->width  = width  >= 0 ? width  : 0 ;
  lc->height = height >= 0 ? height : 0 ;

  if (lc->pause != 0)
    {
      if (lc->height > old_height)
        lc->pause += lc->height - old_height ;
      else
        {
          if (lc->pause >= (old_height - lc->height))
            lc->pause = 0 ;
          else
            lc->pause -= old_height - lc->height ;
        } ;
      lc->paused = (lc->pause == 0) ;
    } ;
} ;

/*==============================================================================
 * Appending and writing
 */

/*------------------------------------------------------------------------------
 * Sets pause to the current height and clear paused.
 */
extern void
vio_lc_set_pause(vio_line_control lc)
{
  lc->pause  = lc->height ;
  lc->paused = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Put newline (if required) and account for it
 */
static inline void
vio_lc_newline(vio_line_control lc, bool required)
{
  if (required)
    qiovec_push(&lc->qiov, "\r\n", 2) ;

  lc->col   = 0 ;
  lc->line += 1 ;
  if (lc->pause != 0)
    {
      lc->pause -= 1 ;
      lc->paused = (lc->pause == 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Append a lump of output to the given line control's buffers.
 *
 * Breaks the output into lines which are no longer than the lc->width.
 *
 * Maps '\n' to '\r''\n'.
 *
 * Discards '\r' if found before '\n', and possibly at other times.
 *
 * If lc->width == 0, use a very large width indeed.
 *
 * If lc->pause == 0, append an indefinite number of lines
 *
 * NB: the buffer presented MUST be retained until the contents of the
 *     line control's buffers have been written.
 *
 * Returns: number of bytes able to append
 */
extern size_t
vio_lc_append(vio_line_control lc, const void* buf, size_t len)
{
  const char* p ;
  const char* end ;

  unsigned  width ;
  unsigned  pause ;

  /* Prepare local width and pause                                      */
  if (lc->width > 0)
    width = lc->width ;
  else
    width = UINT_MAX ;

  if (lc->pause > 0)
    pause = 0 ;
  else
    pause = 1 ;

  lc->paused = 0 ;

  /* Append: stop when run out of data or run out of lines      */
  end = (const char*)buf + len ;
  p   = buf ;

  while ((p < end) && (lc->pause != pause))
    {
      const char* e ;
      bool nl ;
      int  nlx ;

      nlx = 0 ;         /* no line ending chars yet     */

      /* scan for '\n'.                                         */
      e  = memchr(p, '\n', (end - p)) ;
      nl = (e != NULL) ;
      if (nl)
        ++nlx ;         /* account for the '\n'         */
      else
        e = end ;       /* use all there is             */

      /* peel off trailing '\r'.
       *
       * NB: if have not got a '\n', then this may discard a bare
       *     '\r' -- but bare '\r' are undefined in any case.
       */
      if ((e > p) && (*(e - 1) == '\r'))
        {
          --e ;         /* strip the '\r'       */
          ++nlx ;       /* but account for it   */
        }

      /* have p..e characters and possibly nl to add to the output.
       *
       * Note that if enters the while, (e - p) > 0.  So there is at least one
       * character to add.  This avoids generating a spurious line ending if
       * the width has been reduced, and the next thing output is a line end.
       */
      while ((p < e) && (lc->pause != pause))
        {
          const char* t ;
          unsigned col ;

          col = lc->col + (e - p) ;     /* NB: e > p                    */

          if (col > width)
            {
              /* can use only part of what there is                     */
              if (width > lc->col)
                t = p + (width - lc->col) ;
                                        /* take to edge of screen       */
              else
                t = p ;
              assert(t < e) ;           /* if not need to deal with nl  */
            }
          else
            {
              /* can use all of what there is                           */
              if (nlx == 2)             /* if have crlf, use it         */
                {
                  e += nlx ;            /* use the crlf that's there    */
                  nlx = 0 ;             /* used it                      */
                } ;

              t  = e ;                  /* take it all                  */
            } ;

          assert(t >= p) ;
          if (t != p)
            qiovec_push(&lc->qiov, p, (t - p)) ;

          /* advance.  If not taken all the line, need a crlf           */
          p = t ;

          if (p < e)
            vio_lc_newline(lc, 1) ;
        } ;

      /* If taken all of line, deal with any outstanding nl and nlx     */
      if (p == e)
        {
          if (nl)
            vio_lc_newline(lc, (nlx != 0)) ;

          p += nlx ;    /* step past '\r' or '\n'       */
        } ;
    } ;

  /* Exhausted the available data or the line count                     */
  assert(p <= end) ;

  return (p - (const char*)buf) ;       /* what have taken      */
} ;

/*------------------------------------------------------------------------------
 * Write away any collected output -- assuming NON-BLOCKING.
 *
 * Does nothing if the line control is empty.
 *
 * Loops internally if gets EINTR.
 *
 * Returns: > 0 => one or more bytes left to output
 *            0 => all done -- zero bytes left to output
 *           -1 => failed -- see errno
 *
 * Sets lc->writing if write does not complete
 */
extern int
vio_lc_write_nb(int fd, vio_line_control lc)
{
  int ret ;

  ret = qiovec_write_nb(fd, &lc->qiov) ;

  lc->writing = (ret > 0) ;

  return ret ;
} ;

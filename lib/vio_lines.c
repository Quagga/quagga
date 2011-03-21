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
 *   1) maps bare '\n' to '\r''\n' -- if required.
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
 * Returns:  address of vio_line_control
 */
extern vio_line_control
vio_lc_init_new(vio_line_control lc, int width, int height, const char* newline)
{
  if (lc == NULL)
    lc = XCALLOC(MTYPE_VIO_LC, sizeof(struct vio_line_control)) ;
  else
    memset(lc, 0, sizeof(vio_line_control_t)) ;

  /* Zeroising has set:
   *
   *   width       = X       -- set below.
   *   height      = X       -- set below
   *
   *   counter     = X       -- set below
   *
   *   incomplete  = false   -- no incomplete line in hand
   *   fragments   = NULL    -- set below
   *   here        = NULL    -- set below
   *
   *   qiov        = NULL    -- set below
   *
   *   newline     = zeros   -- set below
   */

  lc->width  = width  >= 0 ? width  : 0 ;
  lc->height = height >= 0 ? height : 0 ;
  vio_lc_counter_reset(lc) ;

  lc->fragments = qiovec_init_new(NULL) ;
  lc->here      = qs_new(120) ;

  lc->qiov      = qiovec_init_new(NULL) ;

  lc->newline->base  = newline ;
  lc->newline->len   = strlen(newline) ;

  return lc ;
} ;

/*------------------------------------------------------------------------------
 * Reset vio_line_control (if any) -- release body and (if required) free the
 * structure.
 *
 * Returns: address of vio_line_control (if any) -- NULL if structure released
 *
 * NB: if the line control is not freed, it MUST be reinitialised by
 *     vio_lc_init_new() before it is used again !
 *
 * NB: it is the callers responsibility to release anything which was buffered
 *     for the line control to look after.
 *
 *     It is also the callers responsibility to release the newline string,
 *     if required.
 */
extern vio_line_control
vio_lc_reset(vio_line_control lc, free_keep_b free_structure)
{
  if (lc != NULL)
    {
      lc->fragments = qiovec_free(lc->fragments) ;
      lc->here      = qs_free(lc->here) ;

      lc->qiov      = qiovec_free(lc->qiov) ;

      if (free_structure)
        XFREE(MTYPE_VIO_LC, lc) ;       /* sets lc = NULL       */
      else
        memset(lc, 0, sizeof(vio_line_control_t)) ;
    } ;

  return lc ;
} ;

/*------------------------------------------------------------------------------
 * Clear given vio_line_control -- discard all buffered lines and reset the
 * counter.
 *
 * NB: it is the callers responsibility to release anything buffered because
 *     it was earlier appended.
 */
extern void
vio_lc_clear(vio_line_control lc)
{
  if (lc == NULL)
    return ;

  vio_lc_counter_reset(lc) ;

  lc->incomplete = false ;

  qiovec_clear(lc->fragments) ;
  qs_clear(lc->here) ;

  qiovec_clear(lc->qiov) ;
} ;

/*------------------------------------------------------------------------------
 * Sets width and height for line control
 *
 * A width of  <= 0 => very large width indeed.
 * A height of <= 0 => indefinite height
 *
 * This may happen at almost any time when a Telnet terminal is resized.
 * From line control perspective, can happen between calls of vio_lc_append(),
 * vio_lc_flush() and vio_lc_write_nb().
 *
 * If happens while the line control has stuff buffered, then it is too late to
 * change anything, unless was an incomplete line.
 *
 * Tries to sort out the counter... but is is not possible to do this perfectly.
 */
extern void
vio_lc_set_window(vio_line_control lc, int width, int height)
{
  unsigned depth ;

  depth = lc->height ;

  lc->width  = width  >= 0 ? width  : 0 ;
  lc->height = height >= 0 ? height : 0 ;

  /* If counter already exhausted, or just set indefinite height, need do
   * nothing more.
   */
  if ((lc->counter <= 0) || (lc->height == 0))
    return ;

  /* Counter not already exhausted, and is setting a definite height.
   *
   * If no height was set before, start from scratch, now.
   */
  if (depth == 0)
    return vio_lc_counter_reset(lc) ;

  /* Had a definite height and setting a new one.
   *
   * Now calculate the depth, which is how far down the old height have
   * got to.  (The counter should be less than the old height, but if it
   * isn't we end up with a huge depth here...)
   */
  depth -= lc->counter ;

  /* If the new height is > depth set the counter to be the new height - depth.
   *
   * Otherwise, set the counter to 0, so is immediately exhausted.
   *
   * Cannot solve the problem of what to do if the width has changed !
   */
  lc->counter = (lc->height > depth) ? lc->height - depth : 0 ;
} ;

/*==============================================================================
 * Appending and writing
 */

static uint vio_lc_trim(vio_line_control lc, qiov_item item, const char* e) ;
static void vio_lc_append_line(vio_line_control lc, qiov_item item) ;

/*------------------------------------------------------------------------------
 * Append a lump of output to the given line control's buffers.
 *
 * Breaks the output into lines which are no longer than the lc->width.  If
 * that is zero (unset) we use a very large width indeed.
 *
 * Breaks the output into a number of screen lines, limited by the line counter.
 *
 * Effect of line counter:
 *
 *   * if definite height, will output only as many screen lines as it can,
 *     including none at all.
 *
 *   * if indefinite height, when the line counter is reset it is set to some
 *     limit on the number of screen lines to buffer in the line control.
 *
 *     Note that this is not exact, in particular will *not* stop in the
 *     middle of a complete line, just because the limit of screen lines has
 *     been exceeded.
 *
 *   * in any case, the counter may reach exactly 0 if the number of screen
 *     lines required is exactly the number of screen lines allowed.
 *
 * Trims trailing whitespace from each line (but not from screen lines), before
 * breaking up into screen lines.
 *
 * NB: this means that does not output anything until gets a '\n'.
 *
 *     See vio_lc_flush().
 *
 * Maps '\n' to '\r''\n'.
 *
 * Discards '\r' amongst trailing whitespace, but not otherwise.
 *
 * If lc->width == 0, use a very large width indeed.
 *
 * Returns: number of bytes able to append to the line control.
 *
 * NB: the buffer presented MUST be retained until the contents of the line
 *     control's buffers have been written -- see vio_lc_write_nb().
 *
 * NB: the line control may buffer stuff "in hand", before it reaches the
 *     output iovec, either because:
 *
 *       a) the current line is incomplete -- nothing will be output until
 *          a '\n' is appended, or the line control is flushed.
 *
 *       b) the line counter was exhausted while outputting a line -- this will
 *          be output on the next call of vio_lc_append(), or when the line
 *          control is flushed.
 *
 *          So even when the buffers that feed the line control are empty,
 *          a call of vio_lc_append() may push out some lines buffered in
 *          the line control.
 *
 * NB: all output from the line control ends with a newline.
 *
 *     Incomplete lines are held in the line control until they are completed,
 *     or until the line control is flushed.  When the line control is flushed,
 *     unless the incomplete line is all whitespace, a newline is appended.
 */
extern size_t
vio_lc_append(vio_line_control lc, const void* buf, size_t len)
{
  qiov_item_t item ;
  const char* bp ;
  const char* be ;

  /* If we have a line which was being output, but was interrupted part
   * way through by line counter expiry... try to empty that out, first.
   *
   * This may be prevented by the line counter (partly or completely), or may
   * exhaust it, which will be noticed, shortly, below.
   */
  if (!qiovec_empty(lc->fragments) && !lc->incomplete)
    {
      qiovec_shift(lc->fragments, item) ;  /* want first fragment  */
      vio_lc_append_line(lc, item) ;
    } ;

  /* Append: stop when run out of data or run out of lines      */
  be = (const char*)buf + len ;
  bp = buf ;

  /* If is counter exhausted, stop appending now.
   *
   * In vio_lc_append_line() updates the counter, and may set it -ve.
   */
  while ((bp < be) && (lc->counter > 0))
    {
      item->base = bp ;             /* start of current fragment        */

      /* scan for '\n' -- note that we look for a complete line,
       * before worrying about the line     */
      bp = memchr(bp, '\n', (be - bp)) ;

      if (bp == NULL)
        {
          /* We do not have a '\n' -- rats                              */
          item->len = be - item->base ;
          qiovec_push(lc->fragments, item) ;

          lc->incomplete = true ;   /* have incomplete line in hand     */

          bp = be ;
          break ;               /* done                         */
        } ;

      /* We have a '\n' -- hurrah !
       *
       * Have a complete line ready to be added to the qiov -- the
       * current fragment and any buffered ones.
       *
       * Trim off any trailing whitespace and establish the length of the
       * last fragment of the current (complete) line.  If the current
       * fragment is all whitespace, works its way up any in hand fragments.
       */
      lc->incomplete = false ;  /* definitely not       */

      item->len = vio_lc_trim(lc, item, bp) ;

      ++bp ;                    /* step past the '\n'   */

      /* If have fragments in hand, then want to have the first fragment
       * as the current fragment (currently the current fragment is the
       * last fragment).
       */
      if (!qiovec_empty(lc->fragments))
        {
          qiovec_push(lc->fragments, item) ;
          qiovec_shift(lc->fragments, item) ;
        } ;

      /* We are now ready to break the current line up into width
       * sections, if required -- to the extend allowed by the counter.
       *
       * Have trimmed off any trailing whitespace, including back across
       * any fragments.  So:
       *
       *   item        = first fragment of the line
       */
      vio_lc_append_line(lc, item) ;
    } ;

  /* Exhausted the available data or the line count                     */

  return (bp - (const char*)buf) ;      /* what have taken      */
} ;

/*------------------------------------------------------------------------------
 * Flush anything which the line control has buffered.
 *
 * There are two possible cases:
 *
 *   1) had collected a complete line in vio_lc_append(), but when putting to
 *      the qiovec was stopped by the line counter.
 *
 *      So have one or more screen lines buffered up, ready to go.
 *
 *   2) had collected an incomplete line in vio_lc_append().
 *
 *      That will now be flushed as if a newline had been appended, except that
 *      if the result would be a newline *only*, then all the whitespace is
 *      discarded and nothing is output.
 *
 * If there is nothing to flush, or an incomplete line turns out to be
 * entirely whitespace, return false.
 *
 * Otherwise we have something to flush, and will attempt to do so.
 *
 * Effect of line counter:
 *
 *   * if definite height, will output only as many screen lines as it can,
 *     including none at all.
 *
 *   * if indefinite height, will output as many screen lines as it takes,
 *     whatever the state of the line counter.
 *
 *   * in any case, the counter may reach exactly 0 if the number of screen
 *     lines required is exactly the number of screen lines allowed.
 *
 * Returns: true  <=> have something to flush (but counter may be exhausted)
 *          false <=> nothing to be flushed
 */
extern bool
vio_lc_flush(vio_line_control lc)
{
  qiov_item_t item ;

  if (qiovec_count(lc->fragments) == 0)
    return false ;

  /* We have something in hand.
   *
   * If was an incomplete line, now is the time to trim off any trailing
   * whitespace -- more or less as if there had been a '\n' at this point.
   */
  if (lc->incomplete)
    {
      lc->incomplete = false ;

      qiovec_pop(lc->fragments, item) ;

      item->len = vio_lc_trim(lc, item, item->base + item->len) ;

      if (item->len == 0)
        {
          assert(qiovec_empty(lc->fragments)) ;
          return false ;        /* it was all whitespace which has all
                                   been discarded.                      */
        } ;

      qiovec_push(lc->fragments, item) ;
    } ;

  /* Have something in hand, so now attempt to output it.               */
  qiovec_shift(lc->fragments, item) ;
  vio_lc_append_line(lc, item) ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Trim trailing whitespace from given fragment -- ' ', '\t' and '\r'.
 *
 * If required, pops fragments until finds some non-whitespace, or runs out
 * of fragments.
 *
 * Returns:  resulting length.
 */
static uint
vio_lc_trim(vio_line_control lc, qiov_item item, const char* e)
{
  const char* p ;

  while (1)
    {
      p = item->base ;

      while (e > p)
        {
          char ch ;

          ch = *(e - 1) ;
          if ( (ch != '\r') && (ch != ' ') && (ch != '\t') )
            return e - p ;      /* <<< found non-whitespace  <<< exit   */

          --e ;
        } ;

      qiovec_pop(lc->fragments, item) ; /* pops a NULL if empty */

      if (item->len == 0)
        return 0 ;

      e = item->base + item->len ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Append line to the output qiov -- breaking it up into width sections,
 * inserting newlines and looking out for and updating the line counter.
 *
 * If definite height, may stop immediately, without outputting anything.
 * If indefinite height, will output the current line, whatever the state of
 * the line counter.
 *
 * Note that do not trim whitespace from screen lines.
 *
 * At this point all trailing whitespace will have been removed from the
 * original line.
 *
 * Requires that have previously trimmed off any trailing whitespace, including
 * back across any fragments.  So:
 *
 *   item        = first fragment of the line -- NOT in lc->fragments.
 *
 * It is possible for the first fragment to be empty (empty line !).
 */
static void
vio_lc_append_line(vio_line_control lc, qiov_item item)
{
  bool done = false ;

  while ((lc->counter > 0) || (lc->height == 0))
    {
      uint take ;

      /* Take fragments or parts thereof until we have run out of output
       * (which sets done == true) or reach the width.
       */
      take = (lc->width > 0) ? lc->width : UINT_MAX ;
      while (1)
        {
          if (item->len <= take)
            {
              /* Use entire fragment, and step to next (if any)
               *
               * Note that qiovec_push() ignores zero length items, so if
               * this is an empty line, will push no fragments and will stop
               * here.
               */
              qiovec_push(lc->qiov, item) ;

              if (qiovec_empty(lc->fragments))
                {
                  done = true ;         /* signal all done      */
                  break ;
                }
              else
                {
                  take -= item->len ;
                  qiovec_shift(lc->fragments, item) ;
                }
            }
          else
            {
              /* Use leading part of fragment, and reduce same  */
              qiovec_push_this(lc->qiov, item->base, take) ;
              item->base += take ;
              item->len  -= take ;

              break ;
            } ;
        } ;

      qiovec_push(lc->qiov, lc->newline) ;

      /* Count down & exit if all done.                         */
      --lc->counter ;

      if (done)
        return ;
    } ;

  /* Counter exhausted, but we are not done with the current line.
   *
   * unshift the current item onto the (front of) the fragments qiovec.
   * (We know we can do this straightforwardly, because either the qiovec is
   * empty, or what was the first item has previously been shifted off.)
   */
  qiovec_unshift(lc->fragments, item) ;
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
 *
 * NB: when says "all done" (or failed) the caller may release all buffered
 *     material that has been accepted by vio_lc_append() -- and not before.
 */
extern int
vio_lc_write_nb(int fd, vio_line_control lc)
{
  int ret ;

  ret = qiovec_write_nb(fd, lc->qiov) ;

  if (ret <= 0)
    {
      /* About to promise that all buffered material previously accepted by
       * vio_lc_append() may now be released.
       *
       * Unfortunately, if we have any line fragments in hand, have to buffer
       * those locally, now.
       *
       * NB: if have a particularly long original line, or a particularly
       *     narrow or short screen, it is possible to pass through here
       *     more than once for the same original line.
       *
       *     In this obscure case, the line will already be buffered in
       *     lc->here.  Happily qs_clear() does not disturb the body of the
       *     qstring, and qs_append_str_n will append from within its own
       *     body !
       */
      if (!qiovec_empty(lc->fragments))
        {
          qs_set_len_nn(lc->here, 0) ;  /* ready to append stuff        */
          do
            {
              qiov_item_t item ;

              qiovec_shift(lc->fragments, item) ;

              qs_append_str_n(lc->here, item->base, item->len) ;
            }
          while (!qiovec_empty(lc->fragments)) ;

          /* One fragment in hand, collected from all previous fragments
           */
          qiovec_push_this(lc->fragments, qs_char(lc->here), qs_len(lc->here)) ;
        } ;
    } ;

  return ret ;
} ;

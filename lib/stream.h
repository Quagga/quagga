/*
 * Packet interface
 * Copyright (C) 1999 Kunihiro Ishiguro
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

#ifndef _ZEBRA_STREAM_H
#define _ZEBRA_STREAM_H

#include "misc.h"
#include "prefix.h"

/*==============================================================================
 * A stream is an arbitrary buffer, whose contents generally are assumed to
 * be in network order.
 *
 * The principle function of a stream is to support the reading and writing of
 * PDU.  A PDU may be read into a stream buffer, and then that may be read as
 * some sequence of values (byte, word, long word, etc.).  A PDU may be
 * constructed in a stream by writing a sequence of values (byte, word, long
 * word, etc.).
 *
 * A stream has a fixed size -- it does not automatically adjust itself as
 * stuff is read/written.
 *
 * Attempting to read beyond the current end of a stream will return zeros and
 * set the "overrun" flag.  So readers may plough on without having to
 * constantly check for running off the end of the stream, and check for
 * overrun at convenient moments.
 *
 * Similarly, attempting to write beyond the end of the stream buffer will
 * discard the excess, and set the "overflow" flag.  So writers can plough on
 * and check for overflow at some convenient moment.
 *
 * A stream has the following attributes associated with it:
 *
 * - size: the allocated, invariant size of the buffer.
 *
 * - getp: the get position marker, denoting the offset in the stream where
 *         the next read (or 'get') will be from.
 *
 * - endp: the end position marker, denoting the offset in the stream where
 *         valid data ends, and where any data would be written (or 'put') to.
 *
 * These attributes are all size_t values.
 *
 * Constraints:
 *
 * 1. endp can never exceed size
 *
 *    hence, if endp == size, then the stream is full, and no more data can be
 *    written to the stream.
 *
 *    Whenever data is written to the stream the endp is clamped to size, and
 *    in the event of overflow the overflow flag is set.
 *
 *    Whenever the endp is set, it is clamped.
 *
 * 2. getp can never exceed endp (which can never exceed size)
 *
 *    hence if getp == endp, there is no more valid data that can read from the
 *    stream -- though, the user may reposition getp to earlier in the stream,
 *    if they wish.
 *
 *    Whenever data is written to the stream the endp is clamped to size, and
 *    in the event of overflow the overflow flag is set.
 *
 * So, the following will be true (unless something has gone wrong):
 *
 *    getp <= endp <= size
 *
 * but the stream may overflow or overrun, which the caller may wish to check.
 *
 * A stream therefore can be thought of like this:
 *
 * 	---------------------------------------------------
 * 	|XXXXXXXXXXXXXXXXXXXXXXXX                         |
 * 	---------------------------------------------------
 *               ^               ^                        ^
 *               getp            endp                     size
 *
 * This shows a stream containing data (shown as 'X') up to the endp offset.
 * The stream is empty from endp to size. Without adjusting getp, there are
 * still endp-getp bytes of valid data to be read from the stream.
 *
 * Methods are provided to get and put to/from the stream, as well as
 * retrieve the values of the 3 markers and manipulate the getp marker.
 */

/* Stream buffer. */
struct stream
{
  struct stream *next;

  /* Remainder is ***private*** to stream
   * direct access is frowned upon!
   * Use the appropriate functions/macros
   */
  size_t getp;          /* next get position                    */
  size_t endp;		/* last valid data position             */
  size_t size;		/* size of data segment                 */

  bool   overflow ;     /* set if attempts to put beyond size   */
  bool   overrun ;      /* set if attempts to get beyond endp   */

  byte*  data ;         /* data pointer                 */
};

/* First in first out queue structure. */
struct stream_fifo
{
  size_t count;

  struct stream *head;
  struct stream *tail;
};

/*==============================================================================
 * Utility macros -- deprecated -- do not use in new code
 */
#define STREAM_PNT(S)    stream_get_pnt(S)
#define STREAM_DATA(S)   stream_get_data(S)
#define STREAM_REMAIN(S) stream_get_write_left(S)

/*==============================================================================
 * Stream prototypes.
 * For stream_{put,get}S, the S suffix mean:
 *
 * c: character (unsigned byte)
 * w: word (two bytes)
 * l: long (two words)
 * q: quad (four words)
 */
extern struct stream *stream_new (size_t);
extern void stream_free (struct stream *);
extern struct stream * stream_copy (struct stream *, struct stream *src);
extern struct stream *stream_dup (struct stream *);
extern size_t stream_resize (struct stream *, size_t);
Inline void stream_reset (struct stream *s) ;
Inline bool stream_is_empty (struct stream *s) ;

Inline size_t stream_get_getp(struct stream* s);
Inline size_t stream_get_endp(struct stream* s);
Inline size_t stream_get_len(struct stream* s) ;
Inline size_t stream_get_size(struct stream* s) ;
Inline byte*  stream_get_data(struct stream* s) ;
Inline byte*  stream_get_pnt(struct stream* s) ;

Inline size_t stream_get_read_left(struct stream* s) ;
Inline size_t stream_get_read_left_from(struct stream* s, size_t from) ;
Inline bool stream_has_read_left(struct stream* s, size_t len) ;
Inline size_t stream_get_write_left(struct stream* s) ;
Inline size_t stream_get_write_left_at(struct stream* s, size_t at) ;
Inline bool stream_has_write_left(struct stream* s, size_t len) ;
Inline bool stream_has_overrun(struct stream* s) ;
Inline bool stream_has_overflowed(struct stream* s) ;

Inline void stream_set_getp(struct stream *, size_t);
Inline void stream_set_endp(struct stream *, size_t);
Inline void stream_forward_getp(struct stream *, size_t);
Inline void stream_forward_endp(struct stream *, size_t);

Inline bool stream_push_endp(struct stream* s, size_t len, size_t* old_endp) ;
Inline bool stream_pop_endp(struct stream* s, size_t old_endp) ;

extern void stream_put (struct stream *, const void *, size_t);
extern void stream_putc (struct stream *, byte);
extern void stream_putc_at (struct stream *, size_t, byte);
extern void stream_putw (struct stream *, uint16_t);
extern void stream_putw_at (struct stream *, size_t, uint16_t);
extern void stream_putl (struct stream *, uint32_t);
extern void stream_putl_at (struct stream *, size_t, uint32_t);
extern void stream_putq (struct stream *, uint64_t);
extern void stream_putq_at (struct stream *, size_t, uint64_t);
extern void stream_put_ipv4 (struct stream *, uint32_t);
extern void stream_put_in_addr (struct stream *, struct in_addr *);
extern void stream_put_prefix (struct stream *, struct prefix *);

extern void stream_get (void *, struct stream *, size_t) ;
extern void* stream_get_bytes(struct stream *s, size_t want, size_t* have) ;
extern void* stream_get_bytes_left(struct stream *s, size_t* have) ;
extern byte stream_getc (struct stream *);
extern byte stream_getc_from (struct stream *, size_t);
extern uint16_t stream_getw (struct stream *);
extern uint16_t stream_getw_from (struct stream *, size_t);
extern uint32_t stream_getl (struct stream *);
extern uint32_t stream_getl_from (struct stream *, size_t);
extern uint64_t stream_getq (struct stream *);
extern uint64_t stream_getq_from (struct stream *, size_t);
extern in_addr_t stream_get_ipv4 (struct stream *);
extern void stream_get_prefix(struct stream *s, struct prefix* p,
                                                           sa_family_t family) ;

/* Deprecated: assumes blocking I/O.  Will be removed.
   Use stream_read_try instead.  */
extern int stream_read (struct stream *, int, size_t);

extern int stream_readn (struct stream* s, int fd, size_t size) ;

extern ssize_t stream_read_try(struct stream *s, int fd, size_t size);

extern ssize_t stream_recvmsg (struct stream *s, int fd, struct msghdr *,
                               int flags, size_t size);
extern ssize_t stream_recvfrom (struct stream *s, int fd, size_t len,
                                int flags, struct sockaddr *from,
                                socklen_t *fromlen);

extern int stream_flush_try(struct stream* s, int fd) ;
extern void* stream_transfer(void* p, struct stream* s, void* limit) ;

/* Stream fifo. */
extern struct stream_fifo *stream_fifo_new (void);
extern void stream_fifo_push (struct stream_fifo *fifo, struct stream *s);
extern struct stream *stream_fifo_pop (struct stream_fifo *fifo);
extern struct stream *stream_fifo_head (struct stream_fifo *fifo);
extern void stream_fifo_reset (struct stream_fifo *fifo);
extern void stream_fifo_clean (struct stream_fifo *fifo);
extern void stream_fifo_free (struct stream_fifo *fifo);

/*==============================================================================
 * The Inlines
 */
Private void stream_set_overs(struct stream* s) ;

/*------------------------------------------------------------------------------
 * qassert that the s->getp, s->endp and s->size are consistent.
 */
Inline void
qassert_stream(struct stream* s)
{
  qassert((s->getp <= s->endp) && (s->endp <= s->size)) ;
} ;

/*------------------------------------------------------------------------------
 * Is there anything in this stream ?
 */
Inline bool
stream_is_empty (struct stream *s)
{
  qassert_stream(s) ;
  return (s->endp == 0);
}

/*------------------------------------------------------------------------------
 * Reset to empty and not overflow or overrun.
 *
 * NB: contents of body are untouched.
 */
Inline void
stream_reset (struct stream *s)
{
  s->getp     = s->endp    = 0 ;
  s->overflow = s->overrun = false ;
}

/*------------------------------------------------------------------------------
 * The current s->getp.
 */
Inline size_t
stream_get_getp(struct stream* s)
{
  qassert_stream(s) ;
  return s->getp ;
} ;

/*------------------------------------------------------------------------------
 * The current s->endp.
 *
 * This is also the current total length of the stream data.
 *
 * This generally used for recording the position of some field which will be
 * updated later by stream_putX_at() -- typically a length field.
 */
Inline size_t
stream_get_endp(struct stream* s)
{
  qassert_stream(s) ;
  return s->endp ;
} ;

/*------------------------------------------------------------------------------
 * The current total length of the stream data -- from start to s->endp
 *
 * This is the same as stream_get_endp() -- but clearer in some contexts.
 *
 * NB: if the stream has overflowed, the length is same as the size of the
 *     stream -- it is NOT the length that the stream would have been had there
 *     been enough room.
 */
Inline size_t
stream_get_len(struct stream *s)
{
  qassert_stream(s) ;
  return s->endp ;
} ;

/*------------------------------------------------------------------------------
 * The current size of the stream data body
 *
 * May be zero !  (In which case stream_get_data() will return NULL.)
 */
Inline size_t
stream_get_size(struct stream* s)
{
  qassert_stream(s) ;
  return s->size ;
} ;

/*------------------------------------------------------------------------------
 * The current stream data body
 *
 * May be NULL -- if size is zero.
 *
 * NB: if the stream size is changed, the address returned here may become out
 *     of date.
 *
 * NB: for ordinary processing of the contents of a stream, the various
 *     get/put functions are *recommended* !
 */
Inline byte*
stream_get_data(struct stream* s)
{
  qassert_stream(s) ;
  return s->data ;
} ;

/*------------------------------------------------------------------------------
 * Return pointer to byte at current s->getp.
 *
 * NB: if the stream size is changed, the address returned here may become out
 *     of date.
 *
 * NB: for ordinary processing of the contents of a stream, the various
 *     get/put functions are *recommended* !
 */
Inline byte*
stream_get_pnt (struct stream *s)
{
  qassert_stream(s) ;
  return s->data + s->getp;
}

/*------------------------------------------------------------------------------
 * Count of bytes between s->getp and s->endp.
 */
Inline size_t
stream_get_read_left(struct stream* s)
{
  qassert_stream(s) ;
  return s->endp - s->getp ;
} ;

/*------------------------------------------------------------------------------
 * Count of bytes between given position and s->endp.
 *
 * If the given position is > s->endp, returns 0.
 */
Inline size_t
stream_get_read_left_from(struct stream* s, size_t from)
{
  qassert_stream(s) ;
  return (from < s->endp) ? s->endp - from : 0 ;
} ;

/*------------------------------------------------------------------------------
 * See if has at least len bytes between s->getp and s->lenp
 */
Inline bool
stream_has_read_left(struct stream* s, size_t len)
{
  return len <= stream_get_read_left(s) ;
} ;

/*------------------------------------------------------------------------------
 * Count of bytes between s->endp and s->size.
 */
Inline size_t
stream_get_write_left(struct stream* s)
{
  qassert_stream(s) ;
  return s->size - s->endp ;
}

/*------------------------------------------------------------------------------
 * Count of bytes between given position and s->size.
 *
 * If the given position is > s->size, returns 0.
 */
Inline size_t
stream_get_write_left_at(struct stream* s, size_t at)
{
  qassert_stream(s) ;
  return (at < s->size) ? s->size - at : 0 ;
}

/*------------------------------------------------------------------------------
 * See if has at least len bytes between s->endp and s->size
 */
Inline bool
stream_has_write_left(struct stream* s, size_t len)
{
  return len <= stream_get_write_left(s) ;
} ;

/*------------------------------------------------------------------------------
 * Return the overrun flag
 */
Inline bool
stream_has_overrun(struct stream* s)
{
  qassert_stream(s) ;
  return s->overrun ;
} ;

/*------------------------------------------------------------------------------
 * Return the overflow flag
 */
Inline bool
stream_has_overflowed(struct stream* s)
{
  qassert_stream(s) ;
  return s->overflow ;
} ;

/*------------------------------------------------------------------------------
 * Set s->getp to given value.
 *
 * If value > s->endp will force to s->endp and set s->overrun.
 */
Inline void
stream_set_getp (struct stream *s, size_t pos)
{
  qassert_stream(s) ;

  s->getp = pos ;

  if (s->getp > s->endp)
    stream_set_overs(s) ;
} ;

/*------------------------------------------------------------------------------
 * Set s->endp to given value.
 *
 * If value > s->size will force to s->size and set s->overflow
 *
 * If value < s->getp will force s->getp to new value and set s->overrun.
 *
 * NB: moving the s->endp around is unusual
 *
 *     Returning to a position previously returned by stream_get_endp() is
 *     plausible.  Otherwise, the user must beware !  In particular, moving
 *     s->endp forwards adds whatever is currently in the stream body
 *     beyond s->endp to the stream.
 *
 *     But see stream_push_endp()/stream_pop_endp().
 */
Inline void
stream_set_endp (struct stream *s, size_t pos)
{
  qassert_stream(s) ;

  s->endp = pos;

  if ((s->endp > s->size) || (s->endp < s->getp))
    stream_set_overs(s) ;
} ;

/*------------------------------------------------------------------------------
 * Move s->getp forwards by given step.
 *
 * If result > s->endp will force to s->endp and set s->overrun.
 */
Inline void
stream_forward_getp (struct stream *s, size_t step)
{
  qassert_stream(s) ;

  s->getp += step ;

  if (s->getp > s->endp)
    stream_set_overs(s) ;
} ;

/*------------------------------------------------------------------------------
 * Move s->endp forwards by given step.
 *
 * If value > s->size will force to s->size and set s->overflow
 *
 * NB: moving the s->endp around is unusual -- see stream_set_endp().
 */
Inline void
stream_forward_endp (struct stream *s, size_t step)
{
  qassert_stream(s) ;

  s->endp += step;

  if ((s->endp > s->size) || (s->endp < s->getp))
    stream_set_overs(s) ;
} ;

/*------------------------------------------------------------------------------
 * Push current s->endp and set a new value -- for reading !
 *
 * This may be used when reader knows that is about to read some unit of data
 * which is expected to be len bytes long.
 *
 * Fails if there are fewer than len bytes between s->getp and s->endp.
 *
 * Returns:  true  <=> OK -- new s->endp set.
 *           false <=> failed -- s->endp unchanged and overrun set
 *
 * If during reading the s->getp would move beyond the current s->endp, will
 * return zeros and set overrun in the usual way.
 *
 * Caller can ignore the return code and continue, overrun is already set and
 * will, presumably, hit overrun again.  Popping the saved value will work
 * fine.
 */
Inline bool
stream_push_endp(struct stream* s, size_t len, size_t* old_endp)
{
  size_t new_endp ;

  qassert_stream(s) ;

  *old_endp = s->endp ;
  new_endp  = s->getp + len ;

  if (new_endp <= *old_endp)
    {
      s->endp = new_endp ;
      return true ;
    }
  else
    {
      s->overrun = true ;
      return false ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Pop saved value for s->endp -- restore to as before stream_push_endp()
 *
 * This deemed to be OK if s->getp == s->endp -- ie the s->getp has reached the
 * end of the unit of data was about to process when did stream_push_endp().
 *
 * If not OK will force s->getp and set s->overrun.
 *
 * Expects the value being restored to be valid -- but checks for overflow and
 * overrun just in case !
 *
 * Returns:  OK or not
 */
Inline bool
stream_pop_endp(struct stream* s, size_t old_endp)
{
  bool ok ;

  qassert_stream(s) ;
  qassert((old_endp <= s->size) && (s->endp <= old_endp)) ;

  ok = s->getp == s->endp ;

  if (!ok)
    {
      s->getp = s->endp ;
      s->overrun = true ;
    } ;

  s->endp = old_endp ;

  if ((s->endp > s->size) || (s->getp > s->endp))
    stream_set_overs(s) ;

  return ok ;
} ;

#endif /* _ZEBRA_STREAM_H */

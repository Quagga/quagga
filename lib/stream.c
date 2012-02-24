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

#include "zebra.h"
#include "misc.h"

#include "stream.h"
#include "memory.h"
#include "network.h"
#include "prefix.h"
#include "log.h"

/*------------------------------------------------------------------------------
 * Make stream object
 *
 * NB: can make a stream of size == 0.
 *
 *     Will not allocate a body for the stream -- pointer will be NULL,
 *     but since the size is zero, that is OK.
 *
 * NB: the body of the stream is *NOT* zeroised.
 */
extern struct stream *
stream_new (size_t size)
{
  struct stream *s;

  s = XCALLOC(MTYPE_STREAM, sizeof (struct stream));

  /* Zeroising sets:
   *
   *   next       -- NULL   -- not on any list
   *
   *   getp       -- 0      -- start at the beginning
   *   endp       -- 0      -- empty
   *   size       -- X      -- see below
   *
   *   overrun    -- false  -- OK so far !
   *   overflow   -- false  -- ditto
   *
   *   data       -- NULL   -- set if size != 0
   */

  if (size != 0)
    s->data = XMALLOC(MTYPE_STREAM_DATA, size) ;

  s->size = size;

  return s;
} ;

/*------------------------------------------------------------------------------
 * Free it now.
 */
extern void
stream_free (struct stream *s)
{
  if (s == NULL)
    return;

  XFREE (MTYPE_STREAM_DATA, s->data);
  XFREE (MTYPE_STREAM, s);
}

/*------------------------------------------------------------------------------
 * Set overflow and/or overrun (if required)
 *
 *  If s->endp > s->size: force to s->size and set s->overflow
 *
 *  If s->getp > s->endp: force to s->endp and set s->overrun
 *
 * Clamping s->endp may induce overrun !
 *
 * Not expected to be called if neither overflow nor overrun have been detected,
 * but will do nothing if s->getp <= s->endp <= s->size !
 */
extern void
stream_set_overs(struct stream* s)
{
  if (s->endp > s->size)
    {
      s->endp     = s->size ;
      s->overflow = true ;
    } ;

  if (s->getp > s->endp)
    {
      s->getp    = s->endp ;
      s->overrun = true ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Copy contents of one stream to another
 *
 * Copies current pointers and overrun and overflow state.
 *
 * Copies as much as possible, given the dst->size.  Ensures that the new endp
 * and getp are valid, and sets overrun and/or overflow as required.
 */
extern struct stream *
stream_copy (struct stream *dst, struct stream *src)
{
  qassert ((dst != NULL) && (src != NULL)) ;
  qassert_stream(src) ;

  dst->endp = src->endp ;
  dst->getp = src->getp ;       /* dst->getp <= dst->endp       */

  dst->overflow = src->overflow ;
  dst->overrun  = src->overrun ;

  if (dst->endp > dst->size)    /* dst may be smaller           */
    stream_set_overs(dst) ;

  if (dst->endp > 0)
    memcpy (dst->data, src->data, dst->endp);

  return dst;
} ;

/*------------------------------------------------------------------------------
 * Duplicate given stream -- including overflow and/or overrun state.
 *
 * NB: result size is just big enough for the current stream.
 *
 * NB: if the src is empty, will create a stream with no body.
 */
extern struct stream *
stream_dup (struct stream* src)
{
  return stream_copy(stream_new(stream_get_len(src)), src) ;
} ;

/*------------------------------------------------------------------------------
 * Make stream data to new size.
 *
 * If the new size if less than the old size, then will adjust s->endp and/or
 * s->getp and set s->overflow and/or s->overrun as required.
 *
 * Checks s->getp <= s->endp for safety.
 */
extern size_t
stream_resize (struct stream *s, size_t newsize)
{
  s->data = XREALLOC (MTYPE_STREAM_DATA, s->data, newsize) ;
  s->size = newsize;

  if ((s->getp > s->endp) || (s->endp > s->size))
    stream_set_overs(s) ;

  return s->size;
} ;

/*==============================================================================
 * Getting stuff from a stream
 */
static void stream_get_partial(byte* b, struct stream* s, size_t n,
                                                       size_t from, bool step) ;

/*------------------------------------------------------------------------------
 * Get 'n' bytes to the given buffer and advance s->getp.
 *
 * If there aren't enough bytes between s->getp and s->endp, get as many as
 * can and fill the rest with zeros.  Sets s->getp == s->endp and s->overrun !
 */
inline static void
stream_get_this(byte* b, struct stream* s, size_t n)
{
  size_t getp, getp_n ;

  qassert_stream(s) ;

  getp   = s->getp ;
  getp_n = getp + n ;

  if (getp_n <= s->endp)
    {
      s->getp = getp_n ;
      memcpy(b, s->data + getp, n) ;
    }
  else
    stream_get_partial(b, s, n, getp, true) ;
} ;

/*------------------------------------------------------------------------------
 * Get 'n' bytes to the given buffer from the given position in the stream.
 *
 * If there aren't enough bytes between "from" and s->endp, get as many as
 * can and fill the rest with zeros.  Sets s->overrun !
 */
inline static void
stream_get_this_from(byte* b, struct stream* s, size_t n, size_t from)
{
  qassert_stream(s) ;

  if ((from + n) <= s->endp)
    memcpy(b, s->data + from, n) ;
  else
    stream_get_partial(b, s, n, from, false) ;
} ;

/*------------------------------------------------------------------------------
 * Read as many bytes as can of what we want to given buffer, and zero fill
 * the rest.
 *
 * Sets s->overrun if cannot get everything we want.
 *
 * Step s->getp if required.
 *
 * NB: expect that has already tried, but failed to get everything that was
 *     wanted -- however, will cope if can get everything.
 */
static void
stream_get_partial(byte* b, struct stream* s, size_t n, size_t from, bool step)
{
  size_t have ;

  have = stream_get_read_left_from(s, from) ;

  if (have < n)
    s->overrun = true ;         /* what we expect       */
  else
    have = n ;                  /* this is safe !       */

  if (have > 0)
    memcpy(b, s->data + from, have) ;

  if (n > have)
    memset(b + have, 0, n - have) ;

  if (step)
    {
      s->getp += have ;
      qassert(s->getp <= s->endp) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Copy from stream to destination
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern void
stream_get (void *dst, struct stream *s, size_t n)
{
  stream_get_this(dst, s, n) ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to given number of bytes in stream and step past.
 *
 * Returns:  address of first byte, sets *have = number of bytes wanted, or
 *                                               number of bytes available.
 *
 * Sets overrun if number of bytes wanted is greater than the number available.
 *
 * NB: the address will remain valid until the stream is resized or freed.
 */
extern void*
stream_get_bytes(struct stream *s, size_t want, size_t* have)
{
  byte*  ptr ;
  size_t avail ;

  qassert_stream(s) ;

  ptr   = s->data + s->getp ;
  avail = s->endp - s->getp ;

  if (want > avail)
    {
      want = avail ;
      s->overrun = true ;
    }

  *have = want ;
  s->getp += want ;

  return ptr ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to remaining bytes in stream and step past.
 *
 * Returns:  address of first byte, sets *have = number of bytes available.
 *
 * NB: the address will remain valid until the stream is resized or freed.
 */
extern void*
stream_get_bytes_left(struct stream *s, size_t* have)
{
  byte* ptr ;

  qassert_stream(s) ;

  ptr   = s->data + s->getp ;
  *have = s->endp - s->getp ;

  s->getp = s->endp ;

  return ptr ;
} ;

/*------------------------------------------------------------------------------
 * Get next character from the stream.
 *
 * Returns 0 if overruns (or already overrun)
 */
extern u_char
stream_getc (struct stream *s)
{
  qassert_stream(s) ;

  if (s->getp < s->endp)
    return *(s->data + (s->getp++)) ;

  s->overrun = true ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get next character from given position the stream.
 *
 * Returns 0 if overruns (or already overrun)
 */
extern u_char
stream_getc_from (struct stream *s, size_t from)
{
  qassert_stream(s) ;

  if (from < s->endp)
    return *(s->data + from) ;

  s->overrun = true ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Get next word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint16_t
stream_getw(struct stream *s)
{
  uint16_t w ;

  stream_get_this((byte*)&w, s, 2) ;

  return ntohs(w) ;
} ;

/*------------------------------------------------------------------------------
 * Get word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint16_t
stream_getw_from (struct stream *s, size_t from)
{
  uint16_t w ;

  stream_get_this_from((byte*)&w, s, 2, from) ;

  return ntohs(w) ;
} ;

/*------------------------------------------------------------------------------
 * Get next long word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint32_t
stream_getl (struct stream *s)
{
  uint32_t l ;

  stream_get_this((byte*)&l, s, 4) ;

  return ntohl(l) ;
} ;

/*------------------------------------------------------------------------------
 * Get long word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern u_int32_t
stream_getl_from (struct stream *s, size_t from)
{
  uint32_t l ;

  stream_get_this_from((byte*)&l, s, 4, from) ;

  return ntohl(l) ;
} ;

/*------------------------------------------------------------------------------
 * Get next quad word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint64_t
stream_getq(struct stream *s)
{
  uint64_t q ;

  stream_get_this((byte*)&q, s, 8) ;

  return ntohq(q) ;
} ;

/*------------------------------------------------------------------------------
 * Get quad word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint64_t
stream_getq_from (struct stream *s, size_t from)
{
  uint64_t q ;

  stream_get_this_from((byte*)&q, s, 8, from) ;

  return ntohq(q) ;
} ;

/*------------------------------------------------------------------------------
 * Get next ipv4 address -- returns in_addr_t (which is in NETWORK order).
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern in_addr_t
stream_get_ipv4 (struct stream *s)
{
  in_addr_t ip ;

  stream_get_this((byte*)&ip, s, sizeof(in_addr_t)) ;

  return ip ;
} ;

/*------------------------------------------------------------------------------
 * Get next ipv4 address -- returns in_addr_t (which is in NETWORK order).
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern void
stream_get_prefix(struct stream *s, struct prefix* p, sa_family_t family)
{
  prefix_raw_t raw ;
  size_t       psize;

  raw->prefix_len = stream_getc(s) ;
  psize = (raw->prefix_len + 7) / 8 ;
  if (psize != 0)
    stream_get_this(raw->prefix, s, psize) ;

  prefix_from_raw(p, raw, family) ;
} ;

/*==============================================================================
 * Putting stuff to a stream
 */
static void stream_put_partial(struct stream* s, const byte* src, size_t n,
                                                         size_t at, bool step) ;

/*------------------------------------------------------------------------------
 * Put 'n' bytes at the s->endp and advance s->endp
 *
 * If hit s->size, put as many bytes as possible (may be zero) and
 * set s->overflow.
 */
inline static void
stream_put_this(struct stream* s, const byte* src, size_t n)
{
  size_t endp, endp_n ;

  qassert_stream(s) ;

  endp   = s->endp ;
  endp_n = endp + n ;

  if (endp_n <= s->size)
    {
      s->endp = endp_n ;
      memcpy(s->data + endp, src, n) ;
    }
  else
    stream_put_partial(s, src, n, endp, true) ;
} ;

/*------------------------------------------------------------------------------
 * Put 'n' bytes at the given position.
 *
 * If hit s->size, put as many bytes as possible (may be zero) and
 * set s->overflow.
 *
 * NB: if (at + n) > s->endp, the last byte(s) put to the buffer may be lost
 *     or overwritten !!
 */
inline static void
stream_put_this_at(struct stream* s, const byte* src, size_t n, size_t at)
{
  size_t endp, endp_n ;

  qassert_stream(s) ;

  endp   = s->endp ;
  endp_n = endp + n ;

  if ((at + n) <= s->size)
    memcpy(s->data + at, src, n) ;
  else
    stream_put_partial(s, src, n, at, false) ;
} ;

/*------------------------------------------------------------------------------
 * Put as many bytes as can of what we want to given buffer, and zero fill
 * the rest.
 *
 * Sets s->overflow if cannot put everything we want.
 *
 * Step the s->endp if required.
 *
 * NB: if not stepping s->endp, if (to + n) > s->endp, then the last byte(s)
 *     put to the buffer may be lost or ovewritten !!
 *
 * NB: expect that has already tried, but failed to put everything that was
 *     wanted -- however, will cope if can put everything.
 */
static void
stream_put_partial(struct stream* s, const byte* src, size_t n,
                                                           size_t at, bool step)
{
  size_t have ;

  have = stream_get_write_left_at(s, at) ;

  if (have < n)
    s->overflow = true ;        /* as expected          */
  else
    have = n ;                  /* this is safe !       */

  if (have > 0)
    {
      if (src != NULL)
        memcpy(s->data + at, src, have) ;
      else
        memset(s->data + at, 0, have) ;
    } ;

  if (step)
    {
      s->endp += have ;
      qassert(s->endp <= s->size) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Copy as much of source as possible to stream.
 *
 * If src == NULL, then copy zeros.
 */
extern void
stream_put (struct stream *s, const void *src, size_t n)
{
  stream_put_this(s, src, n) ;
} ;

/*------------------------------------------------------------------------------
 * Put byte to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putc (struct stream *s, byte c)
{
  if (s->endp < s->size)
    *(s->data + (s->endp++)) = c ;
  else
    s->overflow = true ;
} ;

/*------------------------------------------------------------------------------
 * Put byte to the stream at given position -- does not affect s->endp.
 *
 * Puts as much as can, truncating anything truncating anything beyond s->size,
 * setting s->overflow.
 *
 * NB: If the byte written is at or beyond s->endp, it will be lost, or may be
 *     overwritten, unless the s->endp is moved suitably !
 */
extern void
stream_putc_at (struct stream *s, size_t at, u_char c)
{
  if (at < s->size)
    *(s->data + at) = c ;
  else
    s->overflow = true ;
} ;

/*------------------------------------------------------------------------------
 * Put word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putw (struct stream *s, uint16_t w)
{
  w = htons(w) ;
  stream_put_this(s, (byte*)&w, 2) ;
} ;

/*------------------------------------------------------------------------------
 * Put word to the stream at given position -- does not affect s->endp.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 *
 * NB: If the word crosses or is at or beyond s->endp, it will be lost,
 *     or may be overwritten, partly or completely, unless the s->endp is moved
 *     suitably !
 */
extern void
stream_putw_at(struct stream *s, size_t at, uint16_t w)
{
  w = htons(w) ;
  stream_put_this_at(s, (byte*)&w, 2, at) ;
} ;

/*------------------------------------------------------------------------------
 * Put long word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putl (struct stream *s, uint32_t l)
{
  l = htonl(l) ;
  stream_put_this(s, (byte*)&l, 4) ;
} ;

/*------------------------------------------------------------------------------
 * Put long word to the stream at given position -- does not affect s->endp.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 *
 * NB: If the long word crosses or is at or beyond s->endp, it will be lost,
 *     or may be overwritten, partly or completely, unless the s->endp is moved
 *     suitably !
 */
extern void
stream_putl_at (struct stream *s, size_t at, uint32_t l)
{
  l = htonl(l) ;
  stream_put_this_at(s, (byte*)&l, 4, at) ;
} ;

/*------------------------------------------------------------------------------
 * Put quad word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putq (struct stream *s, uint64_t q)
{
  q = htonq(q) ;
  stream_put_this(s, (byte*)&q, 8) ;
} ;

/*------------------------------------------------------------------------------
 * Put quad word to the stream at given position -- does not affect s->endp.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 *
 * NB: If the quad word crosses or is at or beyond s->endp, it will be lost,
 *     or may be overwritten, partly or completely, unless the s->endp is moved
 *     suitably !
 */
extern void
stream_putq_at (struct stream *s, size_t at, uint64_t q)
{
  q = htonq(q) ;
  stream_put_this_at(s, (byte*)&q, 8, at) ;
} ;

/*------------------------------------------------------------------------------
 * Put ipv4 address -- requires in_addr_t (which is in NETWORK order).
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_ipv4 (struct stream *s, in_addr_t ip)
{
  stream_put_this(s, (byte*)&ip, sizeof(ip)) ;
} ;

/*------------------------------------------------------------------------------
 * Put ipv4 address -- requires in_addr !
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_in_addr (struct stream *s, struct in_addr *addr)
{
  stream_put_this(s, (byte*)&(addr->s_addr), sizeof(addr->s_addr)) ;
} ;

/*------------------------------------------------------------------------------
 * Put prefix in nlri type format
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_prefix (struct stream *s, struct prefix *p)
{
  prefix_raw_t raw ;
  size_t       psize;

  psize = prefix_to_raw(raw, p) ;

  stream_put_this(s, (byte*)&raw, psize) ;
} ;

/*==============================================================================
 * Reading to streams
 */

/*------------------------------------------------------------------------------
 * Read bytes from fd to current s->endp
 *
 * Reads the requested amount, or up to the s->size, whichever is smaller.
 */
extern int
stream_read (struct stream *s, int fd, size_t size)
{
  size_t have ;
  int nbytes;

  have = stream_get_write_left(s) ;

  if (have < size)
    size = have ;

  nbytes = readn (fd, s->data + s->endp, size);

  if (nbytes > 0)
    s->endp += nbytes;

  return nbytes;
}

/*------------------------------------------------------------------------------
 * Read up to size bytes into stream -- assuming non-blocking socket.
 *
 * Loops internally if gets EINTR -- so if does not read everything asked for,
 * that must be because the read would otherwise block.
 *
 * Returns: 0..size -- number of bytes read
 *         -1 => failed -- see errno
 *         -2 => EOF met
 *
 * NB: if asks for zero bytes, will return 0 or error (if any).
 *
 * NB: if the stream has no space available, and request is not zero, will
 *     return -1 with errno == ENOMEM !
 */
extern int
stream_readn(struct stream *s, int fd, size_t size)
{
  size_t have ;
  int ret ;
  int want ;

  have = stream_get_write_left(s) ;

  if (have < size)
    {
      size = have ;             /* clamp        */

      if (have == 0)
        {
          errno = ENOMEM ;
          return -1 ;
        } ;
    } ;

  want = size ;
  do
    {
      ret = read(fd, s->data + s->endp, want);

      if (ret > 0)
        {
          s->endp += ret ;
          want    -= ret ;
        }
      else if (ret == 0)
        return (want == 0) ? 0 : -2 ;
      else
        {
          int err = errno ;
          if ((err == EAGAIN) || (err == EWOULDBLOCK))
            break ;
          if (err != EINTR)
            return -1 ;
        } ;
    } while (want > 0) ;

  return size - want ;
} ;

/*------------------------------------------------------------------------------
 * Read up to size bytes into stream -- assuming non-blocking socket.
 *
 * Returns: 0..size -- number of bytes read
 *         -1 => failed, hard -- see errno
 *         -2 => failed, soft -- can retry
 *
 * NB: if asks for zero bytes, will return 0 or error (if any).
 *
 * NB: if the stream has no space available, and request is not zero, will
 *     return -1 with errno == ENOMEM !
 */
extern ssize_t
stream_read_try(struct stream *s, int fd, size_t size)
{
  ssize_t nbytes;
  size_t  have ;

  have = stream_get_write_left(s) ;

  if (have < size)
    {
      size = have ;             /* clamp        */

      if (have == 0)
        {
          errno = ENOMEM ;

          zlog_warn("%s: read failed on fd %d: buffer full", __func__, fd) ;
          return -1 ;
        } ;
    } ;

  if ((nbytes = read(fd, s->data + s->endp, size)) >= 0)
    {
      s->endp += nbytes;
      return nbytes;
    }
  /* Error: was it transient (return -2) or fatal (return -1)? */
  if (ERRNO_IO_RETRY(errno))
    return -2;

  zlog_warn("%s: read failed on fd %d: %s", __func__, fd, errtoa(errno, 0).str);
  return -1;
}

/*------------------------------------------------------------------------------
 * Read up to size bytes into the stream from the fd, using recvmsgfrom
 * whose arguments match the remaining arguments to this function
 *
 * Limits bytes read to the space available in the stream (after s->endp).
 *
 * Returns: 0..size -- number of bytes read
 *         -1 => failed, hard -- see errno
 *         -2 => failed, soft -- can retry
 *
 * NB: if asks for zero bytes, will return 0 or error (if any).
 *
 * NB: if the stream has no space available, and request is not zero, will
 *     return -1 with errno == ENOMEM !
 */
extern ssize_t
stream_recvfrom (struct stream *s, int fd, size_t size, int flags,
                 struct sockaddr *from, socklen_t *fromlen)
{
  ssize_t nbytes;
  size_t  have ;

  have = stream_get_write_left(s) ;

  if (have < size)
    {
      size = have ;             /* clamp        */

      if (have == 0)
        {
          errno = ENOMEM ;

          zlog_warn("%s: failed on fd %d: buffer full", __func__, fd) ;
          return -1 ;
        } ;
    } ;

  if ((nbytes = recvfrom (fd, s->data + s->endp, size,
                          flags, from, fromlen)) >= 0)
    {
      s->endp += nbytes;
      return nbytes;
    }
  /* Error: was it transient (return -2) or fatal (return -1)? */
  if (ERRNO_IO_RETRY(errno))
    return -2;

  zlog_warn("%s: read failed on fd %d: %s", __func__, fd, errtoa(errno, 0).str);
  return -1;
}

/*------------------------------------------------------------------------------
 * Read up to size bytes into the stream from the fd, using recvmsg(), using
 * the given struct msghdr.
 *
 * Limits bytes read to the space available in the stream (after s->endp).
 *
 * Returns: 0..size -- number of bytes read
 *         -1 => failed, hard -- see errno
 *         -2 => failed, soft -- can retry
 *
 * NB: if asks for zero bytes, will return 0 or error (if any).
 *
 * NB: if the stream has no space available, and request is not zero, will
 *     return -1 with errno == ENOMEM !
 */
extern ssize_t
stream_recvmsg (struct stream *s, int fd, struct msghdr *msgh, int flags,
                size_t size)
{
  int nbytes;
  struct iovec *iov;

  size_t  have ;

  have = stream_get_write_left(s) ;

  if (have < size)
    {
      size = have ;             /* clamp        */

      if (have == 0)
        {
          errno = ENOMEM ;

          zlog_warn("%s: failed on fd %d: buffer full", __func__, fd) ;
          return -1 ;
        } ;
    } ;

  assert (msgh->msg_iovlen > 0);

  iov = &(msgh->msg_iov[0]);
  iov->iov_base = (s->data + s->endp);
  iov->iov_len = size;

  nbytes = recvmsg (fd, msgh, flags);

  if (nbytes > 0)
    s->endp += nbytes;

  return nbytes;
} ;

/*------------------------------------------------------------------------------
 * Transfer contents of stream to given buffer and reset stream.
 *
 * Transfers *entire* stream buffer -- but only up to s->size if has OVERFLOWED.
 *
 * Returns pointer to next byte in given buffer
 */
extern void*
stream_transfer(void* p, struct stream* s, void* limit)
{
  size_t have ;

  have = stream_get_len(s) ;

  qassert(((uint8_t*)p + have) <= (uint8_t*)limit) ;

  memcpy(p, s->data, have) ;

  stream_reset(s) ;

  return (uint8_t*)p + have ;
} ;

/*==============================================================================
 * Stream fifo stuff
 */

/*------------------------------------------------------------------------------
 * Make base of fifo of steams.
 */
extern struct stream_fifo *
stream_fifo_new (void)
{
  struct stream_fifo *new;

  new = XCALLOC (MTYPE_STREAM_FIFO, sizeof (struct stream_fifo));
  return new;
}

/*------------------------------------------------------------------------------
 * Append given stream to given fifo
 */
extern void
stream_fifo_push (struct stream_fifo *fifo, struct stream *s)
{
  if (fifo->tail)
    fifo->tail->next = s;
  else
    fifo->head = s;

  fifo->tail = s;

  fifo->count++;
}

/*------------------------------------------------------------------------------
 * Remove first stream from fifo.
 */
extern struct stream *
stream_fifo_pop (struct stream_fifo *fifo)
{
  struct stream *s;

  s = fifo->head;

  if (s)
    {
      fifo->head = s->next;

      if (fifo->head == NULL)
	fifo->tail = NULL;

      fifo->count--;
    }

  return s;
}

/*------------------------------------------------------------------------------
 * Return first fifo entry -- without removing it.
 */
extern struct stream *
stream_fifo_head (struct stream_fifo *fifo)
{
  return fifo->head;
}

/*------------------------------------------------------------------------------
 * Empty given fifo
 *
 * NB: assumes that any streams in the fifo will be dealt with separately,
 */
extern void
stream_fifo_reset (struct stream_fifo *fifo)
{
  fifo->head = fifo->tail = NULL;
  fifo->count = 0;
}

/*------------------------------------------------------------------------------
 * Empty given fifo and free off any streams it contains.
 */
extern void
stream_fifo_clean (struct stream_fifo *fifo)
{
  struct stream *s;
  struct stream *next;

  for (s = fifo->head; s; s = next)
    {
      next = s->next;
      stream_free (s);
    }
  fifo->head = fifo->tail = NULL;
  fifo->count = 0;
}

/*------------------------------------------------------------------------------
 * Empty given fifo, freeing any streams it contains, and then free fifo.
 */
extern void
stream_fifo_free (struct stream_fifo *fifo)
{
  stream_fifo_clean (fifo);
  XFREE (MTYPE_STREAM_FIFO, fifo);
}

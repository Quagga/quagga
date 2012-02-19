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
#include <stddef.h>

#include "stream.h"
#include "memory.h"
#include "network.h"
#include "prefix.h"
#include "log.h"

#if 0
/* Tests whether a position is valid */
#define GETP_VALID(S,G) \
  ((G) <= (S)->endp)
#define PUT_AT_VALID(S,G) GETP_VALID(S,G)
#define ENDP_VALID(S,E) \
  ((E) <= (S)->size)

/* asserting sanity checks. Following must be true before
 * stream functions are called:
 *
 * Following must always be true of stream elements
 * before and after calls to stream functions:
 *
 * getp <= endp <= size
 *
 * Note that after a stream function is called following may be true:
 * if (getp == endp) then stream is no longer readable
 * if (endp == size) then stream is no longer writeable
 *
 * It is valid to put to anywhere within the size of the stream, but only
 * using stream_put..._at() functions.
 */
#define STREAM_WARN_OFFSETS(S) \
  zlog_warn ("&(struct stream): %p, size: %lu, endp: %lu, getp: %lu\n", \
             (S), \
             (unsigned long) (S)->size, \
             (unsigned long) (S)->getp, \
             (unsigned long) (S)->endp)\

#define STREAM_VERIFY_SANE(S) \
  do { \
    if ( !(GETP_VALID(S, (S)->getp)) && ENDP_VALID(S, (S)->endp) ) \
      STREAM_WARN_OFFSETS(S); \
    assert ( GETP_VALID(S, (S)->getp) ); \
    assert ( ENDP_VALID(S, (S)->endp) ); \
  } while (0)

#define STREAM_BOUND_WARN(S, WHAT) \
  do { \
    zlog_warn ("%s: Attempt to %s out of bounds", __func__, (WHAT)); \
    STREAM_WARN_OFFSETS(S); \
    assert (0); \
  } while (0)

/* XXX: Deprecated macro: do not use */
#define CHECK_SIZE(S, Z) \
  do { \
    if (((S)->endp + (Z)) > (S)->size) \
      { \
        zlog_warn ("CHECK_SIZE: truncating requested size %lu\n", \
                   (unsigned long) (Z)); \
        STREAM_WARN_OFFSETS(S); \
        (Z) = (S)->size - (S)->endp; \
      } \
  } while (0);
#endif

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

/*------------------------------------------------------------------------------
 * Test to see if can get 'n' bytes without worrying about overrun
 *
 * If can, return pointer to first byte, and advance s->getp.
 *
 * If not, return NULL and do not change s->getp.
 */
inline static byte*
stream_get_this(struct stream* s, size_t n)
{
  size_t getp, getp_n ;

  qassert_stream(s) ;

  getp   = s->getp ;
  getp_n = getp + n ;

  if (getp_n > s->endp)
    return NULL ;               /* no can do    */

  s->getp = getp_n ;
  return s->data + getp ;
} ;

/*------------------------------------------------------------------------------
 * Test to see if can get 'n' bytes from given position without worrying about
 * overrun
 *
 * If can, return pointer to first byte
 *
 * If not, return NULL
 */
inline static byte*
stream_get_this_from(struct stream* s, size_t from, size_t n)
{
  qassert_stream(s) ;

  if ((from + n) > s->endp)
    return NULL ;               /* no can do    */

  return s->data + from ;
} ;

/*------------------------------------------------------------------------------
 * Read as many bytes as can of what we want to given buffer, and zero fill
 * the rest.
 *
 * Sets s->overrun if cannot get everything we want.
 *
 * Step s->getp if required.
 *
 * Return address of buffer.
 *
 * NB: expect that has already tried, but failed to get everything that was
 *     wanted -- however, will cope if can get everything.
 */
static byte*
stream_get_partial(byte* buf, size_t want,
                                       struct stream* s, size_t from, bool step)
{
  size_t have ;

  have = stream_get_read_left_from(s, from) ;

  if (have < want)
    s->overrun = true ;         /* what we expect       */
  else
    have = want ;               /* this is safe !       */

  if (have > 0)
    memcpy(buf, s->data + from, have) ;

  if (want > have)
    memset(buf + have, 0, want - have) ;

  if (step)
    {
      s->getp += have ;
      qassert(s->getp <= s->endp) ;
    } ;

  return buf ;
} ;

/*------------------------------------------------------------------------------
 * Copy from stream to destination
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern void
stream_get (void *dst, struct stream *s, size_t n)
{
  byte* src ;

  src = stream_get_this(s, n) ;

  if (src != NULL)
    memcpy (dst, src, n);
  else
    stream_get_partial(dst, n, s, s->endp, true) ;
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
  byte* src ;

  src = stream_get_this(s, 1) ;

  if (src != NULL)
    return *src ;
  else
    {
      s->overrun = true ;
      return 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Get next character from given position the stream.
 *
 * Returns 0 if overruns (or already overrun)
 */
extern u_char
stream_getc_from (struct stream *s, size_t from)
{
  byte* src ;

  src = stream_get_this_from(s, from, 1) ;

  if (src != NULL)
    return *src ;
  else
    {
      s->overrun = true ;
      return 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * s_getw -- get 16bit word from network order buffer
 */
inline static uint16_t
s_getw(const byte* src)
{
  uint16_t w ;

  w  = (uint16_t)src[0] << (8 * 1) ;    /* 1    */
  w |= (uint16_t)src[1] ;               /* 0    */

  return w ;
} ;

/*------------------------------------------------------------------------------
 * Get next word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint16_t
stream_getw (struct stream *s)
{
  byte* src ;
  byte  t[2] ;

  src = stream_get_this(s, 2) ;

  if (src == NULL)
    src = stream_get_partial(t, 2, s, s->getp, true) ;

  return s_getw(src) ;
} ;

/*------------------------------------------------------------------------------
 * Get word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint16_t
stream_getw_from (struct stream *s, size_t from)
{
  byte* src ;
  byte  t[2] ;

  src = stream_get_this_from(s, from, 2) ;

  if (src == NULL)
    src = stream_get_partial(t, 2, s, from, false) ;

  return s_getw(src) ;
} ;

/*------------------------------------------------------------------------------
 * s_getl -- get 32bit long word from network order buffer
 */
inline static uint32_t
s_getl(const byte* src)
{
  uint32_t l ;

  l  = (uint32_t)src[0] << (8 * 3) ;    /* 3    */
  l |= (uint32_t)src[1] << (8 * 2) ;    /* 2    */
  l |= (uint32_t)src[2] << (8 * 1) ;    /* 1    */
  l |= (uint32_t)src[3] ;               /* 0    */

  return l ;
} ;

/*------------------------------------------------------------------------------
 * Get next long word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint32_t
stream_getl (struct stream *s)
{
  byte* src ;
  byte  t[4] ;

  src = stream_get_this(s, 4) ;

  if (src == NULL)
    src = stream_get_partial(t, 4, s, s->getp, true) ;

  return s_getl(src) ;
} ;

/*------------------------------------------------------------------------------
 * Get long word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern u_int32_t
stream_getl_from (struct stream *s, size_t from)
{
  byte* src ;
  byte  t[4] ;

  src = stream_get_this_from(s, from, 4) ;

  if (src == NULL)
    src = stream_get_partial(t, 4, s, from, false) ;

  return s_getl(src) ;
} ;

/*------------------------------------------------------------------------------
 * s_getq -- get 64bit long word from network order buffer
 */
inline static uint64_t
s_getq(const byte* src)
{
  uint64_t q ;

  q  = (uint64_t)src[0] << (8 * 7) ;    /* 7    */
  q |= (uint64_t)src[1] << (8 * 6) ;    /* 6    */
  q |= (uint64_t)src[2] << (8 * 5) ;    /* 5    */
  q |= (uint64_t)src[3] << (8 * 4) ;    /* 4    */
  q |= (uint64_t)src[4] << (8 * 3) ;    /* 3    */
  q |= (uint64_t)src[5] << (8 * 2) ;    /* 2    */
  q |= (uint64_t)src[6] << (8 * 1) ;    /* 1    */
  q |= (uint64_t)src[7] ;               /* 0    */

  return q ;
} ;

/*------------------------------------------------------------------------------
 * Get next quad word from the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint64_t
stream_getq(struct stream *s)
{
  byte* src ;
  byte  t[8] ;

  src = stream_get_this(s, 8) ;

  if (src == NULL)
    src = stream_get_partial(t, 8, s, s->getp, true) ;

  return s_getq(src) ;
} ;

/*------------------------------------------------------------------------------
 * Get quad word from given position in the stream.
 *
 * Gets 0 for any byte(s) beyond s->endp and sets s->overrun
 */
extern uint64_t
stream_getq_from (struct stream *s, size_t from)
{
  byte* src ;
  byte  t[8] ;

  src = stream_get_this_from(s, from, 8) ;

  if (src == NULL)
    src = stream_get_partial(t, 8, s, from, false) ;

  return s_getq(src) ;
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
  byte*     src ;

  confirm(sizeof(in_addr_t) == 4) ;

  src = stream_get_this(s, 4) ;

  if (src != NULL)
    memcpy(&ip, src, 4) ;
  else
    stream_get_partial((byte*)&ip, 4, s, s->getp, true) ;

  return ip ;
} ;

/*------------------------------------------------------------------------------
 * Test to see if can put 'n' bytes without worrying about overflow
 *
 * If can, return pointer to first byte, and advance s->endp.
 *
 * If not, return NULL and do not change s->endp.
 */
inline static byte*
stream_put_this(struct stream* s, size_t n)
{
  size_t endp, endp_n ;

  qassert_stream(s) ;

  endp   = s->endp ;
  endp_n = endp + n ;

  if (endp_n > s->size)
    return NULL ;               /* no can do    */

  s->endp = endp_n ;
  return s->data + endp ;
} ;

/*------------------------------------------------------------------------------
 * Test to see if can put 'n' bytes at given position without worrying about
 * overflow
 *
 * If can, return pointer to first byte -- does not advance s->endp.
 *
 * NB: if (to + n) > s->endp, the last byte(s) put to the buffer may be lost
 *     or overwritten !!
 *
 * If not, return NULL
 */
inline static byte*
stream_put_this_at(struct stream* s, size_t at, size_t n)
{
  qassert_stream(s) ;

  if ((at + n) <= s->size)
    return NULL ;               /* no can do    */

  return s->data + at ;
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
stream_put_partial(const byte* src, size_t want,
                                         struct stream* s, size_t at, bool step)
{
  size_t have ;

  have = stream_get_write_left_at(s, at) ;

  if (have < want)
    s->overflow = true ;        /* as expected          */
  else
    have = want ;               /* this is safe !       */

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
  byte* dst ;

  dst = stream_put_this(s, n) ;

  if (dst != NULL)
    {
      if (src != NULL)
        memcpy (dst, src, n);
      else
        memset (dst, 0, n);
    }
  else
    stream_put_partial(src, n, s, s->endp, true) ;
} ;

/*------------------------------------------------------------------------------
 * Put byte to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putc (struct stream *s, byte c)
{
  byte* dst ;

  dst = stream_put_this(s, 1) ;

  if (dst != NULL)
    *dst = c ;
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
  byte* dst ;

  dst = stream_put_this_at(s, at, 1) ;

  if (dst != NULL)
    *dst = c ;
  else
    s->overflow = true ;
} ;

/*------------------------------------------------------------------------------
 * s_putw -- put 16bit word to network order buffer
 */
inline static byte*
s_putw(byte* dst, uint16_t w)
{
  *(dst + 1) =  w             & 0xFF ;
  *(dst + 0) = (w >> (8 * 1)) & 0xFF ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Put word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putw (struct stream *s, uint16_t w)
{
  byte* dst ;
  byte  t[2] ;

  dst = stream_put_this(s, 2) ;

  if (dst != NULL)
    s_putw(dst, w) ;
  else
    stream_put_partial(s_putw(t, w), 2, s, s->endp, true) ;
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
  byte* dst ;
  byte  t[2] ;

  dst = stream_put_this_at(s, at, 2) ;

  if (dst != NULL)
    s_putw(dst, w) ;
  else
    stream_put_partial(s_putw(t, w), 2, s, at, false) ;
} ;

/*------------------------------------------------------------------------------
 * s_putl -- put 32bit long word to network order buffer
 */
inline static byte*
s_putl(byte* dst, uint32_t l)
{
  *(dst + 3) =  l             & 0xFF ;
  *(dst + 2) = (l >> (8 * 1)) & 0xFF ;
  *(dst + 1) = (l >> (8 * 2)) & 0xFF ;
  *(dst + 0) = (l >> (8 * 3)) & 0xFF ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Put long word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putl (struct stream *s, uint32_t l)
{
  byte* dst ;
  byte  t[4] ;

  dst = stream_put_this(s, 4) ;

  if (dst != NULL)
    s_putl(dst, l) ;
  else
    stream_put_partial(s_putl(t, l), 4, s, s->endp, true) ;
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
  byte* dst ;
  byte  t[4] ;

  dst = stream_put_this_at(s, at, 4) ;

  if (dst != NULL)
    s_putl(dst, l) ;
  else
    stream_put_partial(s_putl(t, l), 4, s, at, false) ;
} ;

/*------------------------------------------------------------------------------
 * s_putq -- put 64bit quad word to network order buffer
 */
inline static byte*
s_putq(byte* dst, uint64_t q)
{
  *(dst + 7) =  q             & 0xFF ;
  *(dst + 6) = (q >> (8 * 1)) & 0xFF ;
  *(dst + 5) = (q >> (8 * 2)) & 0xFF ;
  *(dst + 4) = (q >> (8 * 3)) & 0xFF ;
  *(dst + 3) = (q >> (8 * 4)) & 0xFF ;
  *(dst + 2) = (q >> (8 * 5)) & 0xFF ;
  *(dst + 1) = (q >> (8 * 6)) & 0xFF ;
  *(dst + 0) = (q >> (8 * 7)) & 0xFF ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Put quad word to the stream.
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_putq (struct stream *s, uint64_t q)
{
  byte* dst ;
  byte  t[8] ;

  dst = stream_put_this(s, 8) ;

  if (dst != NULL)
    s_putq(dst, q) ;
  else
    stream_put_partial(s_putq(t, q), 8, s, s->endp, true) ;
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
  byte* dst ;
  byte  t[8] ;

  dst = stream_put_this_at(s, at, 8) ;

  if (dst != NULL)
    s_putq(dst, q) ;
  else
    stream_put_partial(s_putq(t, q), 8, s, at, false) ;
} ;

/*------------------------------------------------------------------------------
 * Put ipv4 address -- requires in_addr_t (which is in NETWORK order).
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_ipv4 (struct stream *s, in_addr_t ip)
{
  byte* dst ;

  confirm(sizeof(in_addr_t) == 4) ;

  dst = stream_put_this(s, 4) ;

  if (dst != NULL)
    s_putl(dst, ip) ;
  else
    stream_put_partial((byte*)&ip, 4, s, s->endp, true) ;
} ;

/*------------------------------------------------------------------------------
 * Put ipv4 address -- requires in_addr !
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_in_addr (struct stream *s, struct in_addr *addr)
{
  byte* dst ;

  confirm(sizeof(in_addr_t) == 4) ;

  dst = stream_put_this(s, 4) ;

  if (dst != NULL)
    s_putl(dst, addr->s_addr) ;
  else
    stream_put_partial((byte*)&(addr->s_addr), 4, s, s->endp, true) ;
} ;

/*------------------------------------------------------------------------------
 * Put prefix in nlri type format
 *
 * Puts as much as can: truncates anything beyond s->size and sets s->overflow.
 */
extern void
stream_put_prefix (struct stream *s, struct prefix *p)
{
  size_t psize;
  byte* dst ;

  psize = PSIZE(p->prefixlen) ;         /* number of bytes for prefix   */

  dst = stream_put_this(s, psize + 1) ;

  if (dst != NULL)
    {
      *dst++ = p->prefixlen ;
      if (psize != 0)
        memcpy (dst, &p->u.prefix, psize) ;
    }
  else
    {
      stream_putc(s, psize) ;
      if (psize != 0)
        stream_put(s, &p->u.prefix, psize) ;
    } ;
} ;

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

/*
 * Network library.
 * Copyright (C) 1997 Kunihiro Ishiguro
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
#include "qdebug_nb.h"

#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include "log.h"
#include "network.h"
#include "memory.h"

/*==============================================================================
 * Read and write loops -- assuming blocking or not-blocking.
 *
 *
 *
 */

static ssize_t read_qdebug_nb(int fd, void* buf, size_t nbyte) ;
static ssize_t write_qdebug_nb(int fd, const void* buf, size_t nbyte) ;

/*------------------------------------------------------------------------------
 * Read nbytes from fd and store into ptr -- BLOCKING
 *
 * Loops internally if gets EINTR.
 *
 * Returns: >= 0 -- number of bytes read -- < request => EOF met
 *            -1 => error
 *
 * NB: if applied to a NON-BLOCKING fd, may return EAGAIN or EWOULDBLOCK
 */
extern int
readn (int fd, void* buf, int nbytes)
{
  int nleft;
  int nread;

  nleft = nbytes;

  while (nleft > 0)
    {
      nread = read (fd, buf, nleft);

      if (nread > 0)
        {
          nleft -= nread;
          buf    = (char*)buf + nread;
        }

      else if (nread == 0)
	break;

      else if (errno != EINTR)
        return -1 ;
    } ;

  return nbytes - nleft;
}

/*------------------------------------------------------------------------------
 * Read up to nbyte bytes into buf -- assuming NON-BLOCKING.
 *
 * Loops internally if gets EINTR -- so if does not read everything asked for,
 * that must be because the read would otherwise block.
 *
 * Returns: 0..n -- number of bytes read
 *         -1 => failed -- see errno
 *         -2 => EOF met immediately
 *
 * NB: if asked to read zero bytes, does nothing and will return 0.
 *
 *     Reading zero bytes is defined for all types of files, and may be used
 *     to probe for error state.
 */
extern int
read_nb(int fd, void* buf, size_t nbyte)
{
  size_t nleft ;
  char*  p ;

  enum { read_nb_debug = false } ;

  if (read_nb_debug)
    p = buf ;

  nleft = nbyte ;

  do
    {
      ssize_t ret ;

      if (qdebug_nb)
        ret = read_qdebug_nb(fd, buf, nleft) ;
      else
        ret = read(fd, buf, nleft) ;

      if (ret > 0)
        {
          buf    = (char*)buf + ret ;
          nleft -= ret ;
        }

      else if (ret == 0)
        {
          if (nleft < nbyte)
            break ;             /* if read something before EOF */

// fprintf(stderr, "[read (%d) %s]\n", fd, (nbyte == 0) ? "OK" : "EOF") ;

          return (nbyte == 0) ? 0 : -2 ;        /* OK or EOF    */
        }

      else if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
        break ;

      else if (errno != EINTR)
        return -1 ;             /* failed                       */

    } while (nleft > 0) ;

  if (read_nb_debug)
    {
      int n ;
      char buffer[100] ;
      char* q, * e ;
      e = buffer + 95 ;

      n = nbyte - nleft ;

      fprintf(stderr, "[read (%d) %d: '", fd, n) ;
      while (n > 0)
        {
          q = buffer ;
          while ((q < e) && (n > 0))
            {
              char ch = *p++ ;
              --n ;

              if (ch < 0x20)
                {
                  *q++ = '\\' ;
                  switch (ch)
                    {
                      case '\n':
                        ch = 'n' ;
                        break ;

                      case '\r':
                        ch = 'r' ;
                        break ;

                      case '\t':
                        ch = 't' ;
                        break ;

                      default:
                        ch += '@' ;
                        break ;
                    } ;
                } ;

              *q++ = ch ;
            } ;
          *q++ = '\0' ;

          fprintf(stderr, "%s", buffer) ;
        }
      fprintf(stderr, "']\n") ;
    } ;

  return (nbyte - nleft) ;
} ;

/*------------------------------------------------------------------------------
 * Write nbytes to fd from buf -- BLOCKING
 *
 * Loops internally if gets EINTR.
 *
 * Returns: >= 0 -- number of bytes written
 *            -1 => error
 *
 * NB: if applied to a NON-BLOCKING fd, may return EAGAIN or EWOULDBLOCK
 */
extern int
writen(int fd, const void* buf, int nbytes)
{
  int nleft;
  int nwritten;

  nleft = nbytes;

  while (nleft > 0)
    {
      nwritten = write(fd, buf, nleft);

      if (nwritten >= 0)
        {
          /* write() is not expected to return 0 unless the request is 0,
           * which in this case it isn't.  So cannot happen -- and if it did,
           * wouldn't know what else to do with it !
           */
          nleft -= nwritten;
          buf    = (const char*)buf + nwritten;
        }
      else if (errno != EINTR)
        return -1 ;
    } ;

  return nbytes - nleft;
}

/*------------------------------------------------------------------------------
 * Write up to nbyte bytes from buf -- assuming NON-BLOCKING.
 *
 * Loops internally if gets EINTR.
 *
 * Returns: 0..n -- number of bytes written
 *         -1 => failed -- see errno
 *
 * NB: if asked to write zero bytes, does nothing and will return 0.
 *
 *     Writing zero bytes is defined for "regular files", but not for anything
 *     else.
 */
extern int
write_nb(int fd, const void* buf, size_t nbyte)
{
  size_t nleft ;

  nleft = nbyte ;

  while (nleft > 0)
    {
      ssize_t ret ;

      if (qdebug_nb)
        ret = write_qdebug_nb(fd, buf, nleft) ;
      else
        ret = write(fd, buf, nleft) ;

      if      (ret > 0)
        {
          buf    = (const char*)buf + ret ;
          nleft -= ret ;
        }

      else if (ret == 0)
        /* write() is not expected to return 0 unless the request is 0,
         * which in this case it isn't.  So cannot happen -- but if it were
         * to happen, this treats it as another form of "EAGAIN" !
         */
        break ;

      else if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
        break ;

      else if (errno != EINTR)
        return -1 ;             /* failed                       */
    } ;

  return (nbyte - nleft) ;
} ;

/*------------------------------------------------------------------------------
 * Copy from one fd to another -- blocking.
 *
 * NB: if applied to a NON-BLOCKING fd, may return EAGAIN or EWOULDBLOCK
 */
extern int
copyn (int dst_fd, int src_fd)
{
  void* buffer ;
  int   r, err ;

  enum { buffer_size = 64 * 1024 } ;
  buffer = XMALLOC(MTYPE_TMP, buffer_size) ;

  do
    {
      r = readn(src_fd, buffer, buffer_size) ;
      if (r > 0)
        r = writen(dst_fd, buffer, r) ;
    }
  while (r > 0) ;

  err = errno ;

  XFREE(MTYPE_TMP, buffer) ;

  errno = err ;
  return r ;
} ;

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Set fd to non-blocking
 *
 * Returns:  0 => OK
 *          -1 => failed -- logged a warning, but errno preserved
 */
extern int
set_nonblocking(int fd)
{
  int flags;

  /* According to the Single UNIX Spec, the return value for F_GETFL should
     never be negative. */
  if ((flags = fcntl(fd, F_GETFL)) < 0)
    {
      int err = errno ;

      zlog_warn("%s: fcntl(%d, F_GETFL) failed: %s", __func__,
      		                                       fd, errtoa(err, 0).str) ;
      errno = err ;
      return -1;
    } ;

  if ((flags & O_NONBLOCK) == 0)
    {
      flags |= O_NONBLOCK ;

      if (fcntl(fd, F_SETFL, flags) < 0)
        {
          int err = errno ;

          zlog_warn("%s: fcntl(%d, F_SETFL, 0x%x) failed: %s", __func__,
      		                                fd, flags, errtoa(err, 0).str) ;
          errno = err ;
          return -1;
        } ;
    } ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Set fd to close on exec
 *
 * Returns:  0 => OK
 *          -1 => failed -- logged a warning, but errno preserved
 */
extern int
set_close_on_exec(int fd)
{
  int flags;

  /* According to the Single UNIX Spec, the return value for F_GETFD should
     never be negative. */
  if ((flags = fcntl(fd, F_GETFD)) < 0)
    {
      int err = errno ;

      zlog_warn("%s: fcntl(%d, F_GETFD) failed: %s", __func__,
                                                       fd, errtoa(err, 0).str) ;
      errno = err ;
      return -1;
    } ;

  if ((flags & FD_CLOEXEC) == 0)
    {
      flags |= FD_CLOEXEC ;

      if (fcntl(fd, F_SETFD, flags) < 0)
        {
          int err = errno ;

          zlog_warn("%s: fcntl(%d, F_SETFD, 0x%x) failed: %s", __func__,
                                                fd, flags, errtoa(err, 0).str) ;
          errno = err ;
          return -1;
        } ;
    } ;

  return 0;
}

/*==============================================================================
 * Simulate read/write with tiny buffers and a lot of blocking.
 */

static qrand_seq_t rseq = QRAND_SEQ_INIT(2001) ;
static qrand_seq_t wseq = QRAND_SEQ_INIT(3001) ;

static const int blocking_errs[] = { EAGAIN, EWOULDBLOCK, EINTR } ;

/*------------------------------------------------------------------------------
 * Simulate read(), with tiny input buffer and lots of blocking.
 */
static ssize_t
read_qdebug_nb(int fd, void* buf, size_t nbyte)
{
  if (nbyte > 0)
    {
      if (qrand(rseq, 3) == 0)  /* 1/3 chance of blocking       */
        {
          errno = blocking_errs[qrand(rseq, 3)] ;
          return -1 ;
        } ;

      nbyte = qrand(rseq, (nbyte < 200) ? nbyte : 200) + 1 ;
    } ;


  return read(fd, buf, nbyte) ;
} ;

/*------------------------------------------------------------------------------
 * Simulate write(), with tiny input buffer and lots of blocking.
 */
static ssize_t
write_qdebug_nb(int fd, const void* buf, size_t nbyte)
{
  assert(nbyte > 0) ;

  if (qrand(wseq, 3) == 0)      /* 1/3 chance of blocking       */
    {
      errno = blocking_errs[qrand(wseq, 3)] ;
      return -1 ;
    } ;

  nbyte = qrand(wseq, (nbyte < 200) ? nbyte : 200) + 1 ;

  return write(fd, buf, nbyte) ;
} ;

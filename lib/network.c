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

#include <zebra.h>
#include "log.h"
#include "network.h"

/*------------------------------------------------------------------------------
 * Read nbytes from fd and store into ptr -- BLOCKING
 *
 * Loops internally if gets EINTR.
 *
 * Returns: >= 0 -- number of bytes read
 *           < 0 => error
 *
 * NB: if applied to a NON-BLOCKING fd, may return EAGAIN or EWOULDBLOCK
 */
int
readn (int fd, u_char *ptr, int nbytes)
{
  int nleft;
  int nread;

  nleft = nbytes;

  while (nleft > 0)
    {
      nread = read (fd, ptr, nleft);

      if (nread > 0)
        {
          nleft -= nread;
          ptr   += nread;
        }
      else if (nread == 0)
	break;
      else
        {
          if (errno != EINTR)
            return (nread);
        }
    }

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
 * NB: if asked to write zero bytes, does nothing and will return 0.
 *
 *     Reading zero bytes is defined for all types of files, and may be used
 *     to probe for error state.
 */
int
read_nb(int fd, void* buf, size_t nbyte)
{
  size_t nleft = nbyte ;

  do
    {
      int ret = read(fd, buf, nleft);

      if (ret > 0)
        {
          buf    = (char*)buf + ret ;
          nleft -= ret ;
        }
      else if (ret == 0)
        {
          if (nleft < nbyte)
            break ;             /* if read something before EOF */

          return -2 ;           /* hit EOF immediately          */
        }
      else
        {
          int err = errno ;
          if ((err == EAGAIN) || (err == EWOULDBLOCK))
            break ;
          if (err != EINTR)
            return -1 ;         /* failed                       */
        } ;
    } while (nleft > 0) ;

  return (nbyte - nleft) ;
} ;

/*------------------------------------------------------------------------------
 * Write nbytes to fd from ptr -- BLOCKING
 *
 * Loops internally if gets EINTR.
 *
 * Returns: >= 0 -- number of bytes written
 *           < 0 => error
 *
 * NB: if applied to a NON-BLOCKING fd, may return EAGAIN or EWOULDBLOCK
 */
int
writen(int fd, const u_char *ptr, int nbytes)
{
  int nleft;
  int nwritten;

  nleft = nbytes;

  while (nleft > 0)
    {
      nwritten = write(fd, ptr, nleft);

      if (nwritten > 0)
        {
          nleft -= nwritten;
          ptr   += nwritten;
        }
      else if (nwritten == 0)
        break ;
      else
        {
          if (errno != EINTR)
            return (nwritten);
        }
    }
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
int
write_nb(int fd, void* buf, size_t nbyte)
{
  size_t nleft = nbyte ;

  while (nleft > 0)
    {
      int ret = write(fd, buf, nleft);

      if (ret > 0)
        {
          buf    = (char*)buf + ret ;
          nleft -= ret ;
        }
      else if (ret == 0)
        break ;                 /* not sure can happen... but
                                   cannot assume will go away   */
      else
        {
          int err = errno ;
          if ((err == EAGAIN) || (err == EWOULDBLOCK))
            break ;
          if (err != EINTR)
            return -1 ;         /* failed                       */
        } ;
    } ;

  return (nbyte - nleft) ;
} ;

/*------------------------------------------------------------------------------
 * Set fd to non-blocking
 *
 * Returns:  0 => OK
 *          -1 => failed
 */
int
set_nonblocking(int fd)
{
  int flags;

  /* According to the Single UNIX Spec, the return value for F_GETFL should
     never be negative. */
  if ((flags = fcntl(fd, F_GETFL)) < 0)
    {
      zlog_warn("fcntl(F_GETFL) failed for fd %d: %s",
      		fd, safe_strerror(errno));
      return -1;
    }
  if (fcntl(fd, F_SETFL, (flags | O_NONBLOCK)) < 0)
    {
      zlog_warn("fcntl failed setting fd %d non-blocking: %s",
      		fd, safe_strerror(errno));
      return -1;
    }
  return 0;
}

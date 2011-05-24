/*
 * Process id output.
 * Copyright (C) 1998, 1999 Kunihiro Ishiguro
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
#include <fcntl.h>
#include <log.h>
#include "version.h"

#define PIDFILE_MASK 0644
#ifndef HAVE_FCNTL

pid_t
pid_output (const char *path)
{
  FILE *fp;
  pid_t pid;
  mode_t oldumask;

  pid = getpid();

  oldumask = umask(0777 & ~PIDFILE_MASK);
  fp = fopen (path, "w");
  if (fp != NULL)
    {
      fprintf (fp, "%d\n", (int) pid);
      fclose (fp);
      umask(oldumask);
      return pid;
    }
  /* XXX Why do we continue instead of exiting?  This seems incompatible
     with the behavior of the fcntl version below. */
  zlog_warn("Can't fopen pid lock file %s (%s), continuing",
	    path, errtoa(errno, 0).str);
  umask(oldumask);
  return -1;
}

#else /* HAVE_FCNTL */

pid_t
pid_output (const char *path)
{
  const char* fail ;
  int         err ;

  pid_t  pid ;
  mode_t oldumask ;
  int    fd ;
  struct flock lock ;
  char   buf[32] ;
  size_t pidsize ;

  pid = getpid ();

  oldumask = umask(0777 & ~PIDFILE_MASK);

  fail = "Failed to open pid lock file '%s' for pid %d (%s)" ;
  fd   = open (path, O_RDWR | O_CREAT, PIDFILE_MASK) ;
  err  = errno ;

  umask(oldumask);

  if (fd < 0)
    goto failed_err ;

  memset (&lock, 0, sizeof(lock));
  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;

  if (fcntl(fd, F_SETLK, &lock) < 0)
    {
      fail = "Failed to write lock pid lock file '%s' for pid %d (%s)" ;
      err  = errno ;

      if ((err == EACCES) || (err == EAGAIN))
        {
          fail = "Failed to write lock pid lock file '%s', "
                                                      "blocked by pid %d (%s)" ;
          fcntl(fd, F_GETLK, &lock) ;
          pid = lock.l_pid ;
        } ;

      goto failed_err ;
    } ;

  pidsize = sprintf (buf, "%d\n", (int)pid) ;

  fail = "Failed to write pid to pid lock file '%s' for pid %d (%s)" ;
  if (write(fd, buf, pidsize) != (ssize_t)pidsize)
    goto failed ;

  fail = "Failed to truncate pid lock file '%s' to length for pid %d (%s)" ;
  if (ftruncate(fd, pidsize) < 0)
    goto failed ;

  fail = "Failed to fsync pid lock file '%s' for pid %d (%s)" ;
  if (fsync(fd) < 0)
    goto failed ;

  return pid;

failed:
  err = errno ;

failed_err:
  zlog_err(fail, path, (int)pid, errtoa(err, 0).str) ;
  fprintf(stderr, fail, path, (int)pid, errtoa(err, 0).str) ;
  fprintf(stderr, "\n") ;

  exit(1) ;
}

#endif /* HAVE_FCNTL */

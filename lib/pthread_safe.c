/* Quagga Pthreads support -- thread safe versions of standard functions
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

/* We use thread specific storeage to provide buufers for the _r versions
 * of standard functions, so that the callers don't need to provide
 * their own.  The contents of a buffer will remain intact until another
 * safe_ function is called on the same thread
 */

#include "pthread_safe.h"
#include "qpthreads.h"
#include "memory.h"

#include <pthread.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>

#include "qfstring.h"
#include "errno_names.h"

/* prototypes */
static void destructor(void* data);
static char * thread_buff(void);

static pthread_key_t tsd_key;
static const int buff_size = 1024;

/* Module initialization, before any threads have been created */
void
safe_init_r(void)
{
  if (qpthreads_enabled)
    {
      int status;
      status = pthread_key_create(&tsd_key, destructor);
      if (status != 0)
        zabort("Can't create thread specific data key");
    }
}

/* Clean up */
void
safe_finish(void)
{
  if (qpthreads_enabled)
    pthread_key_delete(tsd_key);
}

/* called when thread terminates, clean up */
static void
destructor(void* data)
{
  XFREE(MTYPE_TSD, data);
}

/* Thread safe version of strerror.  Never returns NULL.
 * Contents of result remains intact until another call of
 * a safe_ function.
 */
const char *
safe_strerror(int errnum)
{
  static const char * unknown = "Unknown error";
  if (qpthreads_enabled)
    {
      char * buff = thread_buff();
      int ret = strerror_r(errnum, buff, buff_size);

      return (ret >= 0)
          ? buff
          : unknown;
    }
  else
    {
      const char *s = strerror(errnum);
      return (s != NULL)
          ? s
          : unknown;
    }
}

/*------------------------------------------------------------------------------
 * Construct string to describe the given error of the form:
 *
 *   ENAME '<strerror>'
 *
 * where <strerror> is given by strerror() or, if pthreads_enabled, strerror_r()
 *
 * Truncates the result to the given len (0 => no truncation).  But silently
 * imposes the maximum length allowed by the strerror_t.
 *
 * If has to truncate, places "..." at the end of the message to show this has
 * happened.
 *
 * If a name is not known then (assuming strerror() won't know it either):
 *
 *   ERRNO=999 *unknown error*
 *
 * For err==0 returns:
 *
 *   EOK 'no error'
 *
 * Thread safe replacement for strerror.  Never returns NULL.
 */
static void
errtox(strerror_t* st, int err, int len, int want)
{
  qf_str_t qfs ;

  const char* q ;
  int ql ;

  /* Prepare.                                                   */
  int errno_saved = errno ;

  if ((len <= 0) || (len >= (int)sizeof(st->str)))
    len = sizeof(st->str) - 1 ;
  qfs_init(&qfs, st->str, len + 1) ;

  q  = "" ;
  ql = 0 ;

  /* If want the error name, do that now.                       */
  if (want & 1)
    {
      const char* name = errno_name_lookup(err) ;

      if (name != NULL)
        qfs_append(&qfs, name) ;
      else
        qfs_printf(&qfs, "ERRNO=%d", err) ;
    } ;

  /* name and string ?                                          */
  if (want == 3)
    {
      qfs_append(&qfs, " ") ;
      q  = "'" ;
      ql = 2 ;
    } ;

  /* If want the error string, do that now                      */
  if (want & 2)
    {
      char  buf[400] ;      /* impossibly vast      */
      int   ret ;
      const char* errm ;

      if (err == 0)
        errm = "no error" ;
      else
        {
          if (qpthreads_enabled)
            {
              /* POSIX is not explicit about what happens if the buffer is not
               * big enough to accommodate the message, except that an ERANGE
               * error may be raised.
               *
               * By experiment: glibc-2.10.2-1.x86_64 returns -1, with errno
               * set to ERANGE and no string at all if the buffer is too small.
               *
               * A huge buffer is used to get the message, and that is later
               * truncated, if necessary, to fit in the strerror_t structure.
               */

              buf[0] = '\0' ;           /* make sure starts empty       */
              ret  = strerror_r(err, buf, sizeof(buf)) ;
              errm = buf ;
              if (ret != 0)
                ret = errno ;
            }
          else
            {
              /* POSIX says that strerror *will* return something, but it is
               * known that it may return NULL if the error number is not
               * recognised.
               */
              errno = 0 ;
              errm = strerror(err) ;
              ret  = errno ;
              if ((ret == 0) && ((errm == NULL) || (*errm == '\0')))
                ret = EINVAL ;
            } ;

          /* Deal with errors, however exotic.                          */
          if (ret != 0)
            {
              q  = "*" ;
              ql = 2 ;          /* force "*" "quotes"   */
              if      (ret == EINVAL)
                errm = "unknown error" ;
              else if (ret == ERANGE)
                {
                  if (*errm == '\0')
                    errm = "vast error message" ;
                }
              else
                {
                  qf_str_t qfs_b ;
                  qfs_init(&qfs_b, buf, sizeof(buf)) ;
                  qfs_printf(&qfs_b, "strerror%s(%d) returned error %d",
                                      qpthreads_enabled ? "_r" : "", err, ret) ;
                  errm = buf ;
                } ;
            } ;
        } ;

      /* Add strerror to the result... looking out for overflow.        */
      len = strlen(errm) ;

      if ((len + ql) <= qfs_left(&qfs)) /* accounting for "quotes"      */
        qfs_printf(&qfs, "%s%s%s", q, errm, q) ;
      else
        qfs_printf(&qfs, "%s%.*s...%s", q, qfs_left(&qfs) - ql - 3, errm, q) ;
                                        /* -ve precision is ignored !   */
    } ;

  /* Put back errno                                                     */
  errno = errno_saved ;
} ;

/*------------------------------------------------------------------------------
 * Construct string to describe the given error of the form:
 *
 *   ENAME '<strerror>'
 *
 * where <strerror> is given by strerror() or, if pthreads_enabled, strerror_r()
 *
 * Truncates the result to the given len (0 => no truncation).  But silently
 * imposes the maximum length allowed by the strerror_t.
 *
 * If has to truncate, places "..." at the end of the message to show this has
 * happened.
 *
 * If a name is not known then (assuming strerror() won't know it either):
 *
 *   ERRNO=999 *unknown error*
 *
 * For err==0 returns:
 *
 *   EOK 'no error'
 *
 * Thread safe replacement for strerror.  Never returns NULL.
 */
extern strerror_t
errtoa(int err, int len)
{
  strerror_t  st ;

  errtox(&st, err, len, 3) ;  /* name and message     */

  return st ;
} ;

/*------------------------------------------------------------------------------
 * Convert error number to its name

 * If a name is not known then:
 *
 *    ERRNO=999
 *
 * For err==0 returns:
 *
 *   EOK
 *
 * Truncates the result to the given len (0 => no truncation).  But silently
 * imposes the maximum length allowed by the strerror_t.
 *
 * This is thread-safe.
 */
extern strerror_t
errtoname(int err, int len)
{
  strerror_t  st ;

  errtox(&st, err, len, 1) ;  /* name                 */

  return st ;
} ;

/*------------------------------------------------------------------------------
 * Alternative thread-safe safe_strerror()
 *
 * Truncates the result to the given len (0 => no truncation).  But silently
 * imposes the maximum length allowed by the strerror_t.
 *
 * If the <strerror> will not fit in the strerror_t, it is truncated and
 * '...' placed at the end to show this has happened.
 *
 * Thread safe replacement for strerror.  Never returns NULL.
 */
extern strerror_t
errtostr(int err, int len)
{
  strerror_t  st ;

  errtox(&st, err, len, 2) ;  /* message              */

  return st ;
} ;

/* Thread safe version of inet_ntoa.  Never returns NULL.
 * Contents of result remains intact until another call of
 * a safe_ function.
 */
const char *
safe_inet_ntoa (struct in_addr in)
{
  static const char * unknown = "Unknown address";
  const char * buff;

  buff = (qpthreads_enabled)
            ? inet_ntop(AF_INET, &in, thread_buff(), buff_size)
            : inet_ntoa(in);

  return buff != NULL
      ? buff
      : unknown;
}

/* Return the thread's buffer, create it if necessary.
 * (pthread Thread Specific Data)
 */
static char *
thread_buff(void)
{
  int ret;
  char * buff = pthread_getspecific(tsd_key);
  if (buff == NULL)
    {
      buff = XMALLOC(MTYPE_TSD, buff_size);
      ret = pthread_setspecific(tsd_key, buff);
      if (ret != 0)
        zabort("Can't set thread specific data");
    }

  return buff;
}








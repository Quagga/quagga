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

const char *
safe_inet_ntoa (struct in_addr in)
{
  if (qpthreads_enabled)
    return inet_ntop(AF_INET, &in, thread_buff(), buff_size);
  else
    return inet_ntoa(in);
}

/* Return the thread's buffer */
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

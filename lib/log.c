/*
 * $Id$
 *
 * Logging of zebra
 * Copyright (C) 1997, 1998, 1999 Kunihiro Ishiguro
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
#include "vty.h"
#include "uty.h"
#include "memory.h"
#include "command.h"
#ifndef SUNOS_5
#include <sys/un.h>
#endif
/* for printstack on solaris */
#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif
#include "qpthreads.h"
#include "qfstring.h"

/* log is protected by the same mutext as vty, see comments in vty.c */

/* prototypes */
static int uzlog_reset_file (struct zlog *zl);
static void zlog_abort (const char *mess) __attribute__ ((noreturn));
static void vzlog (struct zlog *zl, int priority, const char *format, va_list args);
static void uzlog_backtrace(int priority);
static void uvzlog (struct zlog *zl, int priority, const char *format, va_list args);

static int logfile_fd = -1;	/* Used in signal handler. */

struct zlog *zlog_default = NULL;

const char *zlog_proto_names[] =
{
  "NONE",
  "DEFAULT",
  "ZEBRA",
  "RIP",
  "BGP",
  "OSPF",
  "RIPNG",
  "OSPF6",
  "ISIS",
  "MASC",
  NULL,
};

const char *zlog_priority[] =
{
  "emergencies",
  "alerts",
  "critical",
  "errors",
  "warnings",
  "notifications",
  "informational",
  "debugging",
  NULL,
};

/*==============================================================================
 * Time stamp handling -- gettimeofday(), localtime() and strftime().
 *
 * Maintains a cached form of the current time (under the vty/log mutex), so
 * that can avoid multiple calls of localtime() and strftime() per second.
 *
 * The value from gettimeofday() is in micro-seconds.  Can provide timestamp
 * with any number of decimal digits, but at most 6 will be significant.
 */

static void uquagga_timestamp(qf_str qfs, int timestamp_precision) ;

/*------------------------------------------------------------------------------
 * Fill buffer with current time, to given number of decimal digits.
 *
 * If given buffer is too small, provides as many characters as possible.
 *
 * Returns: number of characters in buffer, not including trailing '\0'.
 *
 * NB: does no rounding.
 *
 * NB: buflen MUST be > 1 and buf MUST NOT be NULL.
 */
extern size_t
quagga_timestamp(int timestamp_precision, char *buf, size_t buflen)
{
  qf_str_t qfs ;

  VTY_LOCK() ;

  qfs_init(&qfs, buf, buflen) ;
  uquagga_timestamp(&qfs, timestamp_precision) ;

  VTY_UNLOCK() ;
  return qfs_len(&qfs) ;
}

/*------------------------------------------------------------------------------
 * unprotected version for when mutex already held
 */
static void
uquagga_timestamp(qf_str qfs, int timestamp_precision)
{
  static struct {
    time_t last;
    size_t len;
    char buf[timestamp_buffer_len];
  } cache;

  struct timeval clock;

  /* would it be sufficient to use global 'recent_time' here?  I fear not... */
  gettimeofday(&clock, NULL);

  /* first, we update the cache if the time has changed */
  if (cache.last != clock.tv_sec)
    {
      struct tm tm;
      cache.last = clock.tv_sec;
      localtime_r(&cache.last, &tm);
      cache.len = strftime(cache.buf, sizeof(cache.buf), TIMESTAMP_FORM, &tm) ;
      assert(cache.len > 0) ;
    }

  /* note: it's not worth caching the subsecond part, because
   * chances are that back-to-back calls are not sufficiently close together
   * for the clock not to have ticked forward
   */

  qfs_append_n(qfs, cache.buf, cache.len) ;

  /* Add decimal part as required.                                      */
  if (timestamp_precision > 0)
    {
      /* should we worry about locale issues?              */
      static const int divisor[] = {     1,                     /* 0        */
                                    100000, 10000, 1000,        /* 1, 2, 3  */
                                       100,    10,    1};       /* 4, 5, 6  */
      int prec;

      prec = timestamp_precision ;
      if (prec > 6)
        prec = 6 ;

      qfs_append_n(qfs, ".", 1) ;
      qfs_unsigned(qfs, clock.tv_usec / divisor[prec], 0, 0, prec) ;

      if (prec < timestamp_precision)
        qfs_append_ch_x_n(qfs, '0', timestamp_precision - prec) ;
    } ;
} ;

/*==============================================================================
 * va_list version of zlog
 */
static void
vzlog (struct zlog *zl, int priority, const char *format, va_list args)
{
  VTY_LOCK() ;
  uvzlog(zl, priority, format, args);
  VTY_UNLOCK() ;
}

/* va_list version of zlog. Unprotected assumes mutex already held*/
static void
uvzlog (struct zlog *zl, int priority, const char *format, va_list va)
{
  struct logline ll ;           /* prepares line for output, here       */

  VTY_ASSERT_LOCKED() ;

  ll.p_nl = NULL ;              /* Nothing generated, yet               */

  /* If zlog is not specified, use default one.                 */
  if (zl == NULL)
    zl = zlog_default ;

  /* When zlog_default is also NULL, use stderr for logging.    */
  if (zl == NULL)
    {
      uvzlog_line(&ll, zl, priority, format, va, llt_lf) ;
      write(fileno(stderr), ll.line, ll.len) ;
    }
  else
    {
      /* Syslog output                                          */
      if (priority <= zl->maxlvl[ZLOG_DEST_SYSLOG])
        {
          va_list ac;
          va_copy(ac, va);
          vsyslog (priority|zlog_default->facility, format, ac);
          va_end(ac);
        }

      /* File output.                                           */
      if ((priority <= zl->maxlvl[ZLOG_DEST_FILE]) && zl->fp)
        {
          uvzlog_line(&ll, zl, priority, format, va, llt_lf) ;
          write(fileno(zl->fp), ll.line, ll.len) ;
        }

      /* stdout output. */
      if (priority <= zl->maxlvl[ZLOG_DEST_STDOUT])
        {
          uvzlog_line(&ll, zl, priority, format, va, llt_lf) ;
          write(fileno(zl->fp), ll.line, ll.len) ;
        }

      /* Terminal monitor. */
      if (priority <= zl->maxlvl[ZLOG_DEST_MONITOR])
        uty_log(&ll, zl, priority, format, va) ;
    }
}

/*------------------------------------------------------------------------------
 * Preparation of line to send to logging: file, stdout or "monitor" terminals.
 *
 * Takes copy of va_list before using it, so the va_list is unchanged.
 */
extern void
uvzlog_line(struct logline* ll, struct zlog *zl, int priority,
                              const char *format, va_list va, enum ll_term term)
{
  char*       p ;
  const char* q ;

  p = ll->p_nl ;

  if (p != NULL)
    {
      /* we have the line -- just need to worry about the crlf state    */
      if (term == ll->term)
        return ;                /* exit here if all set */
    }
  else
    {
      /* must construct the line                                        */
      qf_str_t qfs ;
      va_list vac ;

      qfs_init(&qfs, ll->line, sizeof(ll->line) - 2) ;
                                      /* leave space for '\n' or '\r''\n'   */
      /* "<time stamp>"                                                     */
      uquagga_timestamp(&qfs, (zl != NULL) ? zl->timestamp_precision : 0) ;

      qfs_append_n(&qfs, " ", 1) ;

      /* "<priority>: " if required                                     */
      if ((zl != NULL) && (zl->record_priority))
        {
          qfs_append(&qfs, zlog_priority[priority]) ;
          qfs_append(&qfs, ": ") ;
        } ;

      /* "<protocol>: " or "unknown: "                                  */
      if (zl != NULL)
        q = zlog_proto_names[zl->protocol] ;
      else
        q = "unknown" ;

      qfs_append(&qfs, q) ;
      qfs_append(&qfs, ": ") ;

      /* Now the log line itself                                        */
      va_copy(vac, va);
      qfs_vprintf(&qfs, format, vac) ;
      va_end(vac);

      /* Set pointer to where the '\0' is.                              */
      p = ll->p_nl = qfs_end(&qfs) ;
    } ;

  /* finish off with '\r''\n''\0' or '\n''\0' as required               */
  if (term == llt_crlf)
    *p++ = '\r' ;

  if (term != llt_nul)
    *p++ = '\n' ;

  *p = '\0' ;

  ll->len = p - ll->line ;
  ll->term = term ;
} ;

/*============================================================================*/

static char *
str_append(char *dst, int len, const char *src)
{
  while ((len-- > 0) && *src)
    *dst++ = *src++;
  return dst;
}

static char *
num_append(char *s, int len, u_long x)
{
  char buf[30];
  char *t;

  if (!x)
    return str_append(s,len,"0");
  *(t = &buf[sizeof(buf)-1]) = '\0';
  while (x && (t > buf))
    {
      *--t = '0'+(x % 10);
      x /= 10;
    }
  return str_append(s,len,t);
}

#if defined(SA_SIGINFO) || defined(HAVE_STACK_TRACE)
static char *
hex_append(char *s, int len, u_long x)
{
  char buf[30];
  char *t;

  if (!x)
    return str_append(s,len,"0");
  *(t = &buf[sizeof(buf)-1]) = '\0';
  while (x && (t > buf))
    {
      u_int cc = (x % 16);
      *--t = ((cc < 10) ? ('0'+cc) : ('a'+cc-10));
      x /= 16;
    }
  return str_append(s,len,t);
}
#endif

/* Needs to be enhanced to support Solaris. */
static int
syslog_connect(void)
{
#ifdef SUNOS_5
  return -1;
#else
  int fd;
  char *s;
  struct sockaddr_un addr;

  if ((fd = socket(AF_UNIX,SOCK_DGRAM,0)) < 0)
    return -1;
  addr.sun_family = AF_UNIX;
#ifdef _PATH_LOG
#define SYSLOG_SOCKET_PATH _PATH_LOG
#else
#define SYSLOG_SOCKET_PATH "/dev/log"
#endif
  s = str_append(addr.sun_path,sizeof(addr.sun_path),SYSLOG_SOCKET_PATH);
#undef SYSLOG_SOCKET_PATH
  *s = '\0';
  if (connect(fd,(struct sockaddr *)&addr,sizeof(addr)) < 0)
    {
      close(fd);
      return -1;
    }
  return fd;
#endif
}

static void
syslog_sigsafe(int priority, const char *msg, size_t msglen)
{
  static int syslog_fd = -1;
  char buf[sizeof("<1234567890>ripngd[1234567890]: ")+msglen+50];
  char *s;

  if ((syslog_fd < 0) && ((syslog_fd = syslog_connect()) < 0))
    return;

#define LOC s,buf+sizeof(buf)-s
  s = buf;
  s = str_append(LOC,"<");
  s = num_append(LOC,priority);
  s = str_append(LOC,">");
  /* forget about the timestamp, too difficult in a signal handler */
  s = str_append(LOC,zlog_default->ident);
  if (zlog_default->syslog_options & LOG_PID)
    {
      s = str_append(LOC,"[");
      s = num_append(LOC,getpid());
      s = str_append(LOC,"]");
    }
  s = str_append(LOC,": ");
  s = str_append(LOC,msg);
  write(syslog_fd,buf,s-buf);
#undef LOC
}

static int
open_crashlog(void)
{
#define CRASHLOG_PREFIX "/var/tmp/quagga."
#define CRASHLOG_SUFFIX "crashlog"
  if (zlog_default && zlog_default->ident)
    {
      /* Avoid strlen since it is not async-signal-safe. */
      const char *p;
      size_t ilen;

      for (p = zlog_default->ident, ilen = 0; *p; p++)
	ilen++;
      {
	char buf[sizeof(CRASHLOG_PREFIX)+ilen+sizeof(CRASHLOG_SUFFIX)+3];
	char *s = buf;
#define LOC s,buf+sizeof(buf)-s
	s = str_append(LOC, CRASHLOG_PREFIX);
	s = str_append(LOC, zlog_default->ident);
	s = str_append(LOC, ".");
	s = str_append(LOC, CRASHLOG_SUFFIX);
#undef LOC
	*s = '\0';
	return open(buf, O_WRONLY|O_CREAT|O_EXCL, LOGFILE_MASK);
      }
    }
  return open(CRASHLOG_PREFIX CRASHLOG_SUFFIX, O_WRONLY|O_CREAT|O_EXCL,
	      LOGFILE_MASK);
#undef CRASHLOG_SUFFIX
#undef CRASHLOG_PREFIX
}

/* Note: the goal here is to use only async-signal-safe functions. */
void
zlog_signal(int signo, const char *action
#ifdef SA_SIGINFO
	    , siginfo_t *siginfo, void *program_counter
#endif
	   )
{
  time_t now;
  char buf[sizeof("DEFAULT: Received signal S at T (si_addr 0xP, PC 0xP); aborting...")+100];
  char *s = buf;
  char *msgstart = buf;
#define LOC s,buf+sizeof(buf)-s

  time(&now);
  if (zlog_default)
    {
      s = str_append(LOC,zlog_proto_names[zlog_default->protocol]);
      *s++ = ':';
      *s++ = ' ';
      msgstart = s;
    }
  s = str_append(LOC,"Received signal ");
  s = num_append(LOC,signo);
  s = str_append(LOC," at ");
  s = num_append(LOC,now);
#ifdef SA_SIGINFO
  s = str_append(LOC," (si_addr 0x");
  s = hex_append(LOC,(u_long)(siginfo->si_addr));
  if (program_counter)
    {
      s = str_append(LOC,", PC 0x");
      s = hex_append(LOC,(u_long)program_counter);
    }
  s = str_append(LOC,"); ");
#else /* SA_SIGINFO */
  s = str_append(LOC,"; ");
#endif /* SA_SIGINFO */
  s = str_append(LOC,action);
  if (s < buf+sizeof(buf))
    *s++ = '\n';

  /* N.B. implicit priority is most severe */
#define PRI LOG_CRIT

#define DUMP(FD) write(FD, buf, s-buf);
  /* If no file logging configured, try to write to fallback log file. */
  if ((logfile_fd >= 0) || ((logfile_fd = open_crashlog()) >= 0))
    DUMP(logfile_fd)
  if (!zlog_default)
    DUMP(STDERR_FILENO)
  else
    {
      if (PRI <= zlog_default->maxlvl[ZLOG_DEST_STDOUT])
        DUMP(STDOUT_FILENO)
      /* Remove trailing '\n' for monitor and syslog */
      *--s = '\0';
      if (PRI <= zlog_default->maxlvl[ZLOG_DEST_MONITOR])
        vty_log_fixed(buf,s-buf);
      if (PRI <= zlog_default->maxlvl[ZLOG_DEST_SYSLOG])
	syslog_sigsafe(PRI|zlog_default->facility,msgstart,s-msgstart);
    }
#undef DUMP

  zlog_backtrace_sigsafe(PRI,
#ifdef SA_SIGINFO
  			 program_counter
#else
			 NULL
#endif
			);
#undef PRI
#undef LOC
}

/* Log a backtrace using only async-signal-safe functions.
   Needs to be enhanced to support syslog logging. */
void
zlog_backtrace_sigsafe(int priority, void *program_counter)
{
#ifdef HAVE_STACK_TRACE
  static const char pclabel[] = "Program counter: ";
  void *array[64];
  int size;
  char buf[100];
  char *s, **bt = NULL;
#define LOC s,buf+sizeof(buf)-s

#ifdef HAVE_GLIBC_BACKTRACE
  if (((size = backtrace(array,sizeof(array)/sizeof(array[0]))) <= 0) ||
      ((size_t)size > sizeof(array)/sizeof(array[0])))
    return;

#define DUMP(FD) { \
  if (program_counter) \
    { \
      write(FD, pclabel, sizeof(pclabel)-1); \
      backtrace_symbols_fd(&program_counter, 1, FD); \
    } \
  write(FD, buf, s-buf);	\
  backtrace_symbols_fd(array, size, FD); \
}
#elif defined(HAVE_PRINTSTACK)
#define DUMP(FD) { \
  if (program_counter) \
    write((FD), pclabel, sizeof(pclabel)-1); \
  write((FD), buf, s-buf); \
  printstack((FD)); \
}
#endif /* HAVE_GLIBC_BACKTRACE, HAVE_PRINTSTACK */

  s = buf;
  s = str_append(LOC,"Backtrace for ");
  s = num_append(LOC,size);
  s = str_append(LOC," stack frames:\n");

  if ((logfile_fd >= 0) || ((logfile_fd = open_crashlog()) >= 0))
    DUMP(logfile_fd)
  if (!zlog_default)
    DUMP(STDERR_FILENO)
  else
    {
      if (priority <= zlog_default->maxlvl[ZLOG_DEST_STDOUT])
	DUMP(STDOUT_FILENO)
      /* Remove trailing '\n' for monitor and syslog */
      *--s = '\0';
      if (priority <= zlog_default->maxlvl[ZLOG_DEST_MONITOR])
	vty_log_fixed(buf,s-buf);
      if (priority <= zlog_default->maxlvl[ZLOG_DEST_SYSLOG])
	syslog_sigsafe(priority|zlog_default->facility,buf,s-buf);
      {
	int i;
#ifdef HAVE_GLIBC_BACKTRACE
        bt = backtrace_symbols(array, size);
#endif
	/* Just print the function addresses. */
	for (i = 0; i < size; i++)
	  {
	    s = buf;
	    if (bt)
	      s = str_append(LOC, bt[i]);
	    else {
	      s = str_append(LOC,"[bt ");
	      s = num_append(LOC,i);
	      s = str_append(LOC,"] 0x");
	      s = hex_append(LOC,(u_long)(array[i]));
	    }
	    *s = '\0';
	    if (priority <= zlog_default->maxlvl[ZLOG_DEST_MONITOR])
	      vty_log_fixed(buf,s-buf);
	    if (priority <= zlog_default->maxlvl[ZLOG_DEST_SYSLOG])
	      syslog_sigsafe(priority|zlog_default->facility,buf,s-buf);
	  }
	  if (bt)
	    free(bt);
      }
    }
#undef DUMP
#undef LOC
#endif /* HAVE_STRACK_TRACE */
}

void
zlog_backtrace(int priority)
{
  VTY_LOCK() ;
  uzlog_backtrace(priority);
  VTY_UNLOCK() ;
}

static void
uzlog_backtrace(int priority)
{
#ifndef HAVE_GLIBC_BACKTRACE
  uzlog(NULL, priority, "No backtrace available on this platform.");
#else
  void *array[20];
  int size, i;
  char **strings;

  if (((size = backtrace(array,sizeof(array)/sizeof(array[0]))) <= 0) ||
      ((size_t)size > sizeof(array)/sizeof(array[0])))
    {
      uzlog(NULL, LOG_ERR, "Cannot get backtrace, returned invalid # of frames %d "
	       "(valid range is between 1 and %lu)",
	       size, (unsigned long)(sizeof(array)/sizeof(array[0])));
      return;
    }
  uzlog(NULL, priority, "Backtrace for %d stack frames:", size);
  if (!(strings = backtrace_symbols(array, size)))
    {
      uzlog(NULL, LOG_ERR, "Cannot get backtrace symbols (out of memory?)");
      for (i = 0; i < size; i++)
	uzlog(NULL, priority, "[bt %d] %p",i,array[i]);
    }
  else
    {
      for (i = 0; i < size; i++)
	uzlog(NULL, priority, "[bt %d] %s",i,strings[i]);
      free(strings);
    }
#endif /* HAVE_GLIBC_BACKTRACE */
}

/* unlocked version */
void
uzlog (struct zlog *zl, int priority, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  uvzlog (zl, priority, format, args);
  va_end (args);
}

void
zlog (struct zlog *zl, int priority, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vzlog (zl, priority, format, args);
  va_end (args);
}

#define ZLOG_FUNC(FUNCNAME,PRIORITY) \
void \
FUNCNAME(const char *format, ...) \
{ \
  va_list args; \
  va_start(args, format); \
  vzlog (NULL, PRIORITY, format, args); \
  va_end(args); \
}

ZLOG_FUNC(zlog_err, LOG_ERR)

ZLOG_FUNC(zlog_warn, LOG_WARNING)

ZLOG_FUNC(zlog_info, LOG_INFO)

ZLOG_FUNC(zlog_notice, LOG_NOTICE)

ZLOG_FUNC(zlog_debug, LOG_DEBUG)

#undef ZLOG_FUNC

#define PLOG_FUNC(FUNCNAME,PRIORITY) \
void \
FUNCNAME(struct zlog *zl, const char *format, ...) \
{ \
  va_list args; \
  va_start(args, format); \
  vzlog (zl, PRIORITY, format, args); \
  va_end(args); \
}

PLOG_FUNC(plog_err, LOG_ERR)

PLOG_FUNC(plog_warn, LOG_WARNING)

PLOG_FUNC(plog_info, LOG_INFO)

PLOG_FUNC(plog_notice, LOG_NOTICE)

PLOG_FUNC(plog_debug, LOG_DEBUG)

#undef PLOG_FUNC

void
_zlog_assert_failed (const char *assertion, const char *file,
    unsigned int line, const char *function)
{
  const static size_t buff_size = 1024;
  char buff[buff_size];
  snprintf(buff, buff_size,
          "Assertion `%s' failed in file %s, line %u, function %s",
          assertion, file, line, (function ? function : "?"));
  zlog_abort(buff);
}

/* Abort with message                                           */
void
_zlog_abort_mess (const char *mess, const char *file,
    unsigned int line, const char *function)
{
  const static size_t buff_size = 1024;
  char buff[buff_size];
  snprintf(buff, buff_size, "%s, in file %s, line %u, function %s",
          mess, file, line, (function ? function : "?"));
  zlog_abort(buff);
}

/* Abort with message and errno and strerror() thereof          */
void
_zlog_abort_errno (const char *mess, const char *file,
    unsigned int line, const char *function)
{
  _zlog_abort_err(mess, errno, file, line, function);
}

/* Abort with message and given error and strerror() thereof    */
void
_zlog_abort_err (const char *mess, int err, const char *file,
    unsigned int line, const char *function)
{
  const static size_t buff_size = 1024;
  char buff[buff_size];
  snprintf(buff, buff_size,
          "%s, in file %s, line %u, function %s, error %d \"%s\"",
          mess, file, line, (function ? function : "?"),
          err, safe_strerror(err));
  zlog_abort(buff);
}


static void
zlog_abort (const char *mess)
{
#if VTY_DEBUG
  /* May not be locked -- but that doesn't matter any more              */
  ++vty_lock_count ;
#endif

  /* Force fallback file logging? */
  if (zlog_default && !zlog_default->fp &&
      ((logfile_fd = open_crashlog()) >= 0) &&
      ((zlog_default->fp = fdopen(logfile_fd, "w")) != NULL))
    zlog_default->maxlvl[ZLOG_DEST_FILE] = LOG_ERR;

  uzlog(NULL, LOG_CRIT, "%s", mess);
  uzlog_backtrace(LOG_CRIT);
  abort();
}


/* Open log stream */
struct zlog *
openzlog (const char *progname, zlog_proto_t protocol,
	  int syslog_flags, int syslog_facility)
{
  struct zlog *zl;
  u_int i;

  zl = XCALLOC(MTYPE_ZLOG, sizeof (struct zlog));

  zl->ident = progname;
  zl->protocol = protocol;
  zl->facility = syslog_facility;
  zl->syslog_options = syslog_flags;

  /* Set default logging levels. */
  for (i = 0; i < sizeof(zl->maxlvl)/sizeof(zl->maxlvl[0]); i++)
    zl->maxlvl[i] = ZLOG_DISABLED;
  zl->maxlvl[ZLOG_DEST_MONITOR] = LOG_DEBUG;
  zl->default_lvl = LOG_DEBUG;

  openlog (progname, syslog_flags, zl->facility);

  return zl;
}

void
closezlog (struct zlog *zl)
{
  closelog();

  if (zl->fp != NULL)
    fclose (zl->fp);

  XFREE (MTYPE_ZLOG, zl);
}

/* Called from command.c. */
void
zlog_set_level (struct zlog *zl, zlog_dest_t dest, int log_level)
{
  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->maxlvl[dest] = log_level;
    }

  VTY_UNLOCK() ;
}

int
zlog_set_file (struct zlog *zl, const char *filename, int log_level)
{
  FILE *fp;
  mode_t oldumask;
  int result = 1;

  VTY_LOCK() ;

  /* There is opend file.  */
  uzlog_reset_file (zl);

  /* Set default zl. */
  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      /* Open file. */
      oldumask = umask (0777 & ~LOGFILE_MASK);
      fp = fopen (filename, "a");
      umask(oldumask);
      if (fp == NULL)
        result = 0;
      else
        {
        /* Set flags. */
        zl->filename = strdup (filename);
        zl->maxlvl[ZLOG_DEST_FILE] = log_level;
        zl->fp = fp;
        logfile_fd = fileno(fp);
        }
    }

  VTY_UNLOCK() ;
  return result;
}

/* Reset opend file. */
int
zlog_reset_file (struct zlog *zl)
{
  int result;
  VTY_LOCK() ;
  result = uzlog_reset_file(zl);
  VTY_UNLOCK() ;
  return result;
}

static int
uzlog_reset_file (struct zlog *zl)
  {
  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      if (zl->fp)
        fclose (zl->fp);
      zl->fp = NULL;
      logfile_fd = -1;
      zl->maxlvl[ZLOG_DEST_FILE] = ZLOG_DISABLED;

      if (zl->filename)
        free (zl->filename);
      zl->filename = NULL;
    }

  return 1;
}

/* Reopen log file. */
int
zlog_rotate (struct zlog *zl)
{
  int level;
  int result = 1;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      if (zl->fp)
        fclose (zl->fp);
      zl->fp = NULL;
      logfile_fd = -1;
      level = zl->maxlvl[ZLOG_DEST_FILE];
      zl->maxlvl[ZLOG_DEST_FILE] = ZLOG_DISABLED;

      if (zl->filename)
        {
          mode_t oldumask;
          int save_errno;

          oldumask = umask (0777 & ~LOGFILE_MASK);
          zl->fp = fopen (zl->filename, "a");
          save_errno = errno;
          umask(oldumask);
          if (zl->fp == NULL)
            {
              /* can't call logging while locked */
              char *fname = strdup(zl->filename);
              uzlog(NULL, LOG_ERR, "Log rotate failed: cannot open file %s for append: %s",
	  	   fname, safe_strerror(save_errno));
              free(fname);
              result = -1;
            }
          else
            {
              logfile_fd = fileno(zl->fp);
              zl->maxlvl[ZLOG_DEST_FILE] = level;
            }
        }
    }
  VTY_UNLOCK() ;
  return result;
}

int
zlog_get_default_lvl (struct zlog *zl)
{
  int result = LOG_DEBUG;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->default_lvl;
    }

  VTY_UNLOCK() ;
  return result;
}

void
zlog_set_default_lvl (struct zlog *zl, int level)
{
  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->default_lvl = level;
    }

  VTY_UNLOCK() ;
}

/* Set logging level and default for all destinations */
void
zlog_set_default_lvl_dest (struct zlog *zl, int level)
{
  int i;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->default_lvl = level;

      for (i = 0; i < ZLOG_NUM_DESTS; i++)
        if (zl->maxlvl[i] != ZLOG_DISABLED)
          zl->maxlvl[i] = level;
    }

  VTY_UNLOCK() ;
}

int
zlog_get_maxlvl (struct zlog *zl, zlog_dest_t dest)
{
  int result = ZLOG_DISABLED;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->maxlvl[dest];
    }

  VTY_UNLOCK() ;
  return result;
}

int
zlog_get_facility (struct zlog *zl)
{
  int result = LOG_DAEMON;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->facility;
    }

  VTY_UNLOCK() ;
  return result;
}

void
zlog_set_facility (struct zlog *zl, int facility)
{
  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->facility = facility;
    }

  VTY_UNLOCK() ;
}

int
zlog_get_record_priority (struct zlog *zl)
{
  int result = 0;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->record_priority;
    }

  VTY_UNLOCK() ;
  return result;
}

void
zlog_set_record_priority (struct zlog *zl, int record_priority)
{
  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->record_priority = record_priority;
    }
  VTY_UNLOCK() ;
}

int
zlog_get_timestamp_precision (struct zlog *zl)
{
  int result = 0;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->timestamp_precision;
    }
  VTY_UNLOCK() ;
  return result;
}

void
zlog_set_timestamp_precision (struct zlog *zl, int timestamp_precision)
{
  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      zl->timestamp_precision = timestamp_precision;
    }

  VTY_UNLOCK() ;
}

/* returns name of ZLOG_NONE if no zlog given and no default set */
const char *
zlog_get_proto_name (struct zlog *zl)
{
  const char * result;
  VTY_LOCK() ;
  result = uzlog_get_proto_name(zl);
  VTY_UNLOCK() ;
  return result;
}

/* unprotected version, assumes mutex held */
const char *
uzlog_get_proto_name (struct zlog *zl)
{
  zlog_proto_t protocol = ZLOG_NONE;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      protocol = zl->protocol;
    }

  return zlog_proto_names[protocol];
}

/* caller must free result */
char *
zlog_get_filename (struct zlog *zl)
{
  char * result = NULL;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL && zl->filename != NULL)
    {
      result = strdup(zl->filename);
    }

  VTY_UNLOCK() ;
  return result;
}

const char *
zlog_get_ident (struct zlog *zl)
{
  const char * result = NULL;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = zl->ident;
    }

  VTY_UNLOCK() ;
  return result;
}

/* logging to a file? */
int
zlog_is_file (struct zlog *zl)
{
  int result = 0;

  VTY_LOCK() ;

  if (zl == NULL)
    zl = zlog_default;

  if (zl != NULL)
    {
      result = (zl->fp != NULL);
    }

  VTY_UNLOCK() ;;
  return result;
}

/* Message lookup function. */
const char *
lookup (const struct message *mes, int key)
{
  const struct message *pnt;

  for (pnt = mes; pnt->key != 0; pnt++)
    if (pnt->key == key)
      return pnt->str;

  return "";
}

/* Older/faster version of message lookup function, but requires caller to pass
 * in the array size (instead of relying on a 0 key to terminate the search).
 *
 * The return value is the message string if found, or the 'none' pointer
 * provided otherwise.
 */
const char *
mes_lookup (const struct message *meslist, int max, int index, const char *none)
{
  int pos = index - meslist[0].key;

  /* first check for best case: index is in range and matches the key
   * value in that slot.
   * NB: key numbering might be offset from 0. E.g. protocol constants
   * often start at 1.
   */
  if ((pos >= 0) && (pos < max)
      && (meslist[pos].key == index))
    return meslist[pos].str;

  /* fall back to linear search */
  {
    int i;

    for (i = 0; i < max; i++, meslist++)
      {
	if (meslist->key == index)
	  {
	    const char *str = (meslist->str ? meslist->str : none);

	    zlog_debug ("message index %d [%s] found in position %d (max is %d)",
		      index, str, i, max);
	    return str;
	  }
      }
  }
  zlog_err("message index %d not found (max is %d)", index, max);
  assert (none);
  return none;
}

struct zebra_desc_table
{
  unsigned int type;
  const char *string;
  char chr;
};

#define DESC_ENTRY(T,S,C) [(T)] = { (T), (S), (C) }
static const struct zebra_desc_table route_types[] = {
  DESC_ENTRY	(ZEBRA_ROUTE_SYSTEM,	"system",	'X' ),
  DESC_ENTRY	(ZEBRA_ROUTE_KERNEL,	"kernel",	'K' ),
  DESC_ENTRY	(ZEBRA_ROUTE_CONNECT,	"connected",	'C' ),
  DESC_ENTRY	(ZEBRA_ROUTE_STATIC,	"static",	'S' ),
  DESC_ENTRY	(ZEBRA_ROUTE_RIP,	"rip",		'R' ),
  DESC_ENTRY	(ZEBRA_ROUTE_RIPNG,	"ripng",	'R' ),
  DESC_ENTRY	(ZEBRA_ROUTE_OSPF,	"ospf",		'O' ),
  DESC_ENTRY	(ZEBRA_ROUTE_OSPF6,	"ospf6",	'O' ),
  DESC_ENTRY	(ZEBRA_ROUTE_ISIS,	"isis",		'I' ),
  DESC_ENTRY	(ZEBRA_ROUTE_BGP,	"bgp",		'B' ),
  DESC_ENTRY	(ZEBRA_ROUTE_HSLS,	"hsls",		'H' ),
};
#undef DESC_ENTRY

#define DESC_ENTRY(T) [(T)] = { (T), (#T), '\0' }
static const struct zebra_desc_table command_types[] = {
  DESC_ENTRY	(ZEBRA_INTERFACE_ADD),
  DESC_ENTRY	(ZEBRA_INTERFACE_DELETE),
  DESC_ENTRY	(ZEBRA_INTERFACE_ADDRESS_ADD),
  DESC_ENTRY	(ZEBRA_INTERFACE_ADDRESS_DELETE),
  DESC_ENTRY	(ZEBRA_INTERFACE_UP),
  DESC_ENTRY	(ZEBRA_INTERFACE_DOWN),
  DESC_ENTRY	(ZEBRA_IPV4_ROUTE_ADD),
  DESC_ENTRY	(ZEBRA_IPV4_ROUTE_DELETE),
  DESC_ENTRY	(ZEBRA_IPV6_ROUTE_ADD),
  DESC_ENTRY	(ZEBRA_IPV6_ROUTE_DELETE),
  DESC_ENTRY	(ZEBRA_REDISTRIBUTE_ADD),
  DESC_ENTRY	(ZEBRA_REDISTRIBUTE_DELETE),
  DESC_ENTRY	(ZEBRA_REDISTRIBUTE_DEFAULT_ADD),
  DESC_ENTRY	(ZEBRA_REDISTRIBUTE_DEFAULT_DELETE),
  DESC_ENTRY	(ZEBRA_IPV4_NEXTHOP_LOOKUP),
  DESC_ENTRY	(ZEBRA_IPV6_NEXTHOP_LOOKUP),
  DESC_ENTRY	(ZEBRA_IPV4_IMPORT_LOOKUP),
  DESC_ENTRY	(ZEBRA_IPV6_IMPORT_LOOKUP),
  DESC_ENTRY	(ZEBRA_INTERFACE_RENAME),
  DESC_ENTRY	(ZEBRA_ROUTER_ID_ADD),
  DESC_ENTRY	(ZEBRA_ROUTER_ID_DELETE),
  DESC_ENTRY	(ZEBRA_ROUTER_ID_UPDATE),
};
#undef DESC_ENTRY

static const struct zebra_desc_table unknown = { 0, "unknown", '?' };

static const struct zebra_desc_table *
zroute_lookup(u_int zroute)
{
  u_int i;

  if (zroute >= sizeof(route_types)/sizeof(route_types[0]))
    {
      zlog_err("unknown zebra route type: %u", zroute);
      return &unknown;
    }
  if (zroute == route_types[zroute].type)
    return &route_types[zroute];
  for (i = 0; i < sizeof(route_types)/sizeof(route_types[0]); i++)
    {
      if (zroute == route_types[i].type)
        {
	  zlog_warn("internal error: route type table out of order "
		    "while searching for %u, please notify developers", zroute);
	  return &route_types[i];
        }
    }
  zlog_err("internal error: cannot find route type %u in table!", zroute);
  return &unknown;
}

const char *
zebra_route_string(u_int zroute)
{
  return zroute_lookup(zroute)->string;
}

char
zebra_route_char(u_int zroute)
{
  return zroute_lookup(zroute)->chr;
}

const char *
zserv_command_string (unsigned int command)
{
  if (command >= sizeof(command_types)/sizeof(command_types[0]))
    {
      zlog_err ("unknown zserv command type: %u", command);
      return unknown.string;
    }
  return command_types[command].string;
}

#define RTSIZE	(sizeof(route_types)/sizeof(route_types[0]))

int
proto_name2num(const char *s)
{
   unsigned i;

   for (i=0; i<RTSIZE; ++i)
     if (strcasecmp(s, route_types[i].string) == 0)
       return route_types[i].type;
   return -1;
}
#undef RTSIZE

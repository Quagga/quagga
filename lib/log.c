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
#include "log_local.h"
#include "vty_log.h"
#include "memory.h"

#ifndef SUNOS_5
#include <sys/un.h>
#endif
/* for printstack on solaris    */
#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif
#include "qpthreads.h"
#include "qfstring.h"
#include "sigevent.h"

/*------------------------------------------------------------------------------
 * Logging is managed using struct zlog, which represents a set of logging
 * destinations, which are output to in parallel, each with its own log
 * priority.
 *
 * Notionally, there may be more than one strct zlog, but in practice there is
 * only one and that is the one pointed at by zlog_default.
 */
struct zlog
{
  struct zlog* next ;           /* To support multiple logging streams  */

  const char *ident;            /* daemon name (first arg to openlog)   */
  zlog_proto_t protocol ;

  int maxlvl[ZLOG_DEST_COUNT];  /* maximum priority set                 */
  int monitor_lvl ;             /* last monitor level specified         */
  int default_lvl ;             /* maxlvl to use if none is specified   */

  int emaxlvl[ZLOG_DEST_COUNT]; /* effective maximum priority           */

  bool syslog ;                 /* have active syslog                   */
  int  file_fd ;                /* fd for ZLOG_DEST_FILE (if any)       */
  int  stdout_fd ;              /* fd for ZLOG_DEST_STDOUT              */

  char *filename;               /* for ZLOG_DEST_FILE                   */

  int facility;                 /* as per syslog facility               */
  int syslog_options;           /* 2nd arg to openlog                   */

  int  timestamp_precision;     /* # of digits of subsecond precision   */
  bool record_priority;         /* should messages logged through stdio
                                   include the priority of the message? */
} ;

/*------------------------------------------------------------------------------
 * Tables of protocol and log priority names.
 */
const char *zlog_proto_names[] =
{
  [ZLOG_NONE]    = "NONE",
  [ZLOG_DEFAULT] = "DEFAULT",
  [ZLOG_ZEBRA]   = "ZEBRA",
  [ZLOG_RIP]     = "RIP",
  [ZLOG_BGP]     = "BGP",
  [ZLOG_OSPF]    = "OSPF",
  [ZLOG_RIPNG]   = "RIPNG",
  [ZLOG_OSPF6]   = "OSPF6",
  [ZLOG_ISIS]    = "ISIS",
  [ZLOG_MASC]    = "MASC",
  NULL,
} ;

const char *zlog_priority[] =
{
  [LOG_EMERG]    = "emergencies",
  [LOG_ALERT]    = "alerts",
  [LOG_CRIT]     = "critical",
  [LOG_ERR]      = "errors",
  [LOG_WARNING]  = "warnings",
  [LOG_NOTICE]   = "notifications",
  [LOG_INFO]     = "informational",
  [LOG_DEBUG]    = "debugging",
  NULL,
};

/*------------------------------------------------------------------------------
 * Static variables
 */
struct zlog* zlog_default = NULL;

struct zlog* zlog_list    = NULL ;

static volatile sig_atomic_t max_maxlvl = ZLOG_DISABLED ;

qpt_mutex_t log_mutex ;

int log_lock_count    = 0 ;
int log_assert_fail   = 0 ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void zlog_abort (const char *mess) __attribute__ ((noreturn)) ;
static void uzlog_backtrace(int priority);

/*==============================================================================
 * Log locking and relationship with VTY.
 *
 * For pthreads purposes there is a LOG_LOCK() mutex.
 *
 * To support interaction with the VTY:
 *
 *   a. setting logging configuration.
 *
 *   b. issuing log messages within the VTY
 *
 * the LOG_LOCK() may be collected while VTY_LOCK() -- BUT NOT vice versa !
 * This is not too difficult, because the logging has no need to call VTY
 * functions...  except for "vty monitor" VTY, so, for that purpose:
 *
 *   a. for each monitor VTY there is a "monitor_fifo", into which log messages
 *      are place if required.  This buffer and some related flags must be
 *      handled under the LOG_LOCK().
 *
 *      The vty_monitor_log() function takes care of this, and takes care of
 *      prompting the vty(s) to output the contents of the fifo.
 *
 *      This will be called under LOG_LOCK().  It will NOT VTY_LOCK().  It
 *      will return promptly.
 *
 *   b. once any logging has been handed to the VTY, the logging can forget
 *      about it.
 *
 *   c. the interface from logging to VTY is in vty_log.h
 *
 * The logging system supports some logging once a signal (in particular,
 * the hard exceptions, such as SIGSEGV) has gone off and the system is being
 * brought to a dead stop.  A separate mechanism is provided for outputting
 * directly to any vty monitor VTY -- at a file descriptor level !
 *
 * The VTY may call logging functions in the same was as any other part of the
 * system.  When interacting with configuration etc, may need to acquire the
 * LOG_LOCK, etc.  The interface from VTY to logging is in log_local.h.
 */

/*------------------------------------------------------------------------------
 * Further initialisation for qpthreads.
 *
 * This is done during "second stage" initialisation, when all nexuses have
 * been set up and the qpthread_enabled state established.
 *
 * Initialise mutex.
 *
 * NB: may be called once and once only.
 */
extern void
log_init_r(void)
{
  qpt_mutex_init_new(log_mutex, qpt_mutex_recursive);
} ;

/*------------------------------------------------------------------------------
 * Close down for qpthreads.
 */
extern void
log_finish(void)
{
  qpt_mutex_destroy(log_mutex, 0);
} ;

/*==============================================================================
 * The actual logging function and creation of the logging lines.
 */
/* Structure used to hold line for log output -- so that need be generated
 * just once even if output to multiple destinations.
 *
 * Note that the buffer length is a hard limit (including terminating "\n")
 * Do not wish to malloc any larger buffer while logging.
 */
enum { logline_buffer_len = 1008 } ;

struct logline {
  size_t  len ;         /* length including either '\r''\n' or '\n'     */

  char line[logline_buffer_len]; /* buffer                              */
} ;

typedef struct logline  logline_t[1] ;
typedef struct logline* logline ;

static void uvzlog_line(logline ll, struct zlog *zl, int priority,
                                               const char *format, va_list va) ;
static void uquagga_timestamp(qf_str qfs, int timestamp_precision) ;

/*==============================================================================
 * The main logging function.
 *
 * This writes the logging information directly to all logging destinations,
 * except the vty_monitor_log(), so that it is externally visible as soon as
 * possible.
 *
 * This assumes that syslog and file output is essentially instantaneous, and
 * will not block.
 *
 * Does not attempt to pick up or report any I/O errors.
 *
 * For vty_monitor_log(), any logging is buffered and dealt with by the vty.
 *
 * So, having acquired the LOG_LOCK() the logging will proceed without
 * requiring any further locks.  Indeed, apart from vty_monitor_log() and
 * uquagga_timestamp() (TODO) the process is async_signal_safe, even.
 */
extern void
zlog (struct zlog *zl, int priority, const char *format, ...)
{
  logline_t     ll ;

  /* Decide whether any logging at all is required.
   *
   * NB: the max_maxlvl is established every time any change is made to
   *     logging facilities.  Those changes are made under LOG_LOCK(), so
   *     only one thread will write to max_max_lvl at any time, and that
   *     will be consistent with the state of all known logging at the
   *     time.
   *
   *     The max_maxlvl is sig_atomic_t and volatile, and we assume that:
   *
   *       writing to max_maxlvl is atomic wrt all forms of interrupt,
   *       and wrt any processor cache invalidation.
   *
   *     That is:
   *
   *       the variable cannot be read in a partly written state (with,
   *       say, some bytes of the old value and some of the new).
   *
   *     Hence, reading max_maxlvl will either collect the state before some
   *     change to the logging levels, or after.
   *
   *     If the given priority > max_maxlvl, this function exits, otherwise it
   *     immediately acquires the LOG_LOCK().
   *
   *     So, if the logging facilities are being changed, then:
   *
   *       a) if the level is being increased, so the current priority would
   *          would pass, then that change is just too late for this logging
   *          operation.
   *
   *       b) if the level is about to be reduced, then will get past the
   *          initial test, but after acquiring the LOG_LOCK(), will find that
   *          there is no logging to be done after all.
   *
   * NB: max_maxlvl is statically initialised to ZLOG_DISABLED.
   */
  if (priority > max_maxlvl)
    return ;

  /* Decide where we are logging to.                                    */

  LOG_LOCK() ;

  if (zl == NULL)
    {
      zl = zlog_default ;

      if (zl == NULL)
        {
          /* Have to get up very early in the morning to get to here, because
           * zlog_default should be initialised -- by openzlog() -- very early
           * indeed.
           *
           * So... "log" to stderr.
           */
          va_list va;
          va_start(va, format);
          uvzlog_line(ll, zl, priority, format, va) ;
          va_end (va);

          write(fileno(stderr), ll->line, ll->len) ;

          return ;
        } ;
    } ;

  /* Step through the logging destinations and log as required.         */

  ll->len = 0 ;                 /* Nothing generated, yet               */

  /* Syslog output                                          */
  if (priority <= zl->emaxlvl[ZLOG_DEST_SYSLOG])
    {
      va_list va;
      va_start(va, format);
      vsyslog (priority|zlog_default->facility, format, va);
      va_end(va);
    }

  /* File output.                                           */
  if (priority <= zl->emaxlvl[ZLOG_DEST_FILE])
    {
      if (ll->len == 0)
        {
          va_list va;
          va_start(va, format);
          uvzlog_line(ll, zl, priority, format, va) ;
          va_end (va);
        } ;
      write(zl->file_fd, ll->line, ll->len) ;
    }

  /* stdout output.                                         */
  if (priority <= zl->emaxlvl[ZLOG_DEST_STDOUT])
    {
      if (ll->len == 0)
        {
          va_list va;
          va_start(va, format);
          uvzlog_line(ll, zl, priority, format, va) ;
          va_end (va);
        } ;
      write(zl->stdout_fd, ll->line, ll->len) ;
    }

  /* Terminal monitor.                                      */
  if (priority <= zl->emaxlvl[ZLOG_DEST_MONITOR])
    {
      if (ll->len == 0)
        {
          va_list va;
          va_start(va, format);
          uvzlog_line(ll, zl, priority, format, va) ;
          va_end (va);
        } ;
      vty_monitor_log(priority, ll->line, ll->len - 1) ; /* less the '\n' */
    } ;

  LOG_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Preparation of line to send to logging: file, stdout or "monitor" terminals.
 *
 * Line ends in '\n', but no terminating '\0'.
 *
 * TODO: apart from uquagga_timestamp, this is all async_signal_safe.
 *
 */
static void
uvzlog_line(logline ll, struct zlog *zl, int priority,
                                               const char *format, va_list va)
{
  const char* q ;
  qf_str_t    qfs ;

  qfs_init(qfs, ll->line, sizeof(ll->line)) ;
  /* "<time stamp>"                                                     */
  uquagga_timestamp(qfs, (zl != NULL) ? zl->timestamp_precision : 0) ;

  qfs_append_n(qfs, " ", 1) ;

  /* "<priority>: " if required                                         */
  if ((zl != NULL) && zl->record_priority)
    {
      qfs_append(qfs, zlog_priority[priority]) ;
      qfs_append(qfs, ": ") ;
    } ;

  /* "<protocol>: " or "unknown: "                                      */
  if (zl != NULL)
    q = zlog_proto_names[zl->protocol] ;
  else
    q = "unknown" ;

  qfs_append(qfs, q) ;
  qfs_append(qfs, ": ") ;

  /* Now the log line itself (uses a *copy* of the va_list)             */
  qfs_vprintf(qfs, format, va) ;

  /* Stick '\n' on the end                                              */
  qfs_append_n(qfs, "\n", 1) ;

  /* Worry about overflow of message                                    */
  if (qfs_overflow(qfs) != 0)
    qfs_term_string(qfs, "...\n", sizeof("...\n") - 1) ;

  ll->len = qfs_len(qfs) ;
} ;

/*------------------------------------------------------------------------------
 * Return timestamp_str_t buffer current time, to given number of decimals.
 *
 * See uquagga_timestamp(), below.
 */
extern timestamp_str_t
quagga_timestamp(int timestamp_precision)
{
  timestamp_str_t QFB_QFS(str, qfs) ;

  LOG_LOCK() ;
  uquagga_timestamp(qfs, timestamp_precision) ;
  LOG_UNLOCK() ;

  qfs_term(qfs) ;
  return str ;
} ;

/*------------------------------------------------------------------------------
 * unprotected version for when mutex already held
 *
 * Append timestamp to the given qf_str -- assumes LOG_LOCK() is held, because
 * keeps local cache of the text form of the timestamp, to the second !
 *
 * Time stamp handling: gettimeofday(), localtime() and strftime().
 *
 * NB: this is NOT async signal safe :-(
 *
 * The value from gettimeofday() is in micro-seconds.  Can provide timestamp
 * with any number of decimal digits, but at most 6 will be significant.
 *
 * NB: does no rounding.
 */
static void
uquagga_timestamp(qf_str qfs, int timestamp_precision)
{
  /* This is the cached form of the current time (under the log mutex).
   */
  static struct {
    time_t last;
    size_t len;
    char buf[timestamp_buffer_len];
  } cache ;

  struct timeval clock;

  /* would it be sufficient to use global 'recent_time' here?  I fear not...
   */
  gettimeofday(&clock, NULL);

  /* first, we update the cache if the seconds time has changed             */
  if (cache.last != clock.tv_sec)
    {
      struct tm tm;

      cache.last = clock.tv_sec;
      localtime_r(&cache.last, &tm);
      cache.len = strftime(cache.buf, sizeof(cache.buf), TIMESTAMP_FORM, &tm) ;

      qassert(cache.len > 0) ;
    } ;

  qfs_append_n(qfs, cache.buf, cache.len) ;

  /* Add decimal part as required.
   *
   * note: it's not worth caching the subsecond part, because chances are that
   * back-to-back calls are not sufficiently close together for the clock not
   * to have ticked forward
   */
  if (timestamp_precision > 0)
    {
      /* should we worry about locale issues?
       */
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
 *
 */

void
_zlog_assert_failed (const char *assertion, const char *file,
    unsigned int line, const char *function)
{
  char buff[1024];
  snprintf(buff, sizeof(buff),
          "Assertion `%s' failed in file %s, line %u, function %s",
          assertion, file, line, (function ? function : "?"));
  zlog_abort(buff);
}

/* Abort with message                                           */
void
_zlog_abort_mess (const char *mess, const char *file,
    unsigned int line, const char *function)
{
  char buff[1024];
  snprintf(buff, sizeof(buff),
          "%s, in file %s, line %u, function %s",
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
  char buff[1024];
  snprintf(buff, sizeof(buff),
          "%s, in file %s, line %u, function %s, %s",
          mess, file, line, (function ? function : "?"),
          errtoa(err, 0).str);
  zlog_abort(buff);
}

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

/*------------------------------------------------------------------------------
 * Note: the goal here is to use only async-signal-safe functions.
 */
extern void
zlog_signal(int signo, const char *action, siginfo_t *siginfo,
                                                          void *program_counter)
{
  time_t now;
  char buf[sizeof("DEFAULT: Received signal S at T (si_addr 0xP, PC 0xP); aborting...")+100];
  char *s = buf;
  char *msgstart = buf;
  int  file_fd ;
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

#define DUMP(FD) write(FD, buf, s - buf);

  /* If no file logging configured, try to write to fallback log file. */
  file_fd = (zlog_default != NULL) ? zlog_default->file_fd : -1 ;
  if (file_fd < 0)
    file_fd = open_crashlog() ;

  if (file_fd >= 0)
    DUMP(file_fd)

  if (!zlog_default)
    DUMP(STDERR_FILENO)
  else
    {
      if (PRI <= zlog_default->maxlvl[ZLOG_DEST_STDOUT])
        DUMP(STDOUT_FILENO)
      /* Remove trailing '\n' for monitor and syslog */
      *--s = '\0';
      if (PRI <= zlog_default->maxlvl[ZLOG_DEST_MONITOR])
        vty_monitor_log_fixed(buf, s - buf);
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

/* Ring down the curtain -- turn of SIGABRT handler and abort()         */
void zabort_abort(void)
{
  quagga_sigabrt_no_trap() ;
  abort() ;
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
  int  file_fd ;
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

  file_fd = (zlog_default != NULL) ? zlog_default->file_fd : -1 ;
  if (file_fd < 0)
    file_fd = open_crashlog() ;

  if (file_fd >= 0)
    DUMP(file_fd)

  if (!zlog_default)
    DUMP(STDERR_FILENO)
  else
    {
      if (priority <= zlog_default->maxlvl[ZLOG_DEST_STDOUT])
	DUMP(STDOUT_FILENO)
      /* Remove trailing '\n' for monitor and syslog */
      *--s = '\0';
      if (priority <= zlog_default->maxlvl[ZLOG_DEST_MONITOR])
	vty_monitor_log_fixed(buf,s-buf);
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
	      vty_monitor_log_fixed(buf,s-buf);
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
  LOG_LOCK() ;
  uzlog_backtrace(priority);
  LOG_UNLOCK() ;
}

static void
uzlog_backtrace(int priority)
{
#ifndef HAVE_GLIBC_BACKTRACE
  zlog(NULL, priority, "No backtrace available on this platform.");
#else
  void *array[20];
  int size, i;
  char **strings;

  if (((size = backtrace(array,sizeof(array)/sizeof(array[0]))) <= 0) ||
      ((size_t)size > sizeof(array)/sizeof(array[0])))
    {
      zlog(NULL, LOG_ERR, "Cannot get backtrace, returned invalid # of frames %d "
	       "(valid range is between 1 and %lu)",
	       size, (unsigned long)(sizeof(array)/sizeof(array[0])));
      return;
    }
  zlog(NULL, priority, "Backtrace for %d stack frames:", size);
  if (!(strings = backtrace_symbols(array, size)))
    {
      zlog(NULL, LOG_ERR, "Cannot get backtrace symbols (out of memory?)");
      for (i = 0; i < size; i++)
	zlog(NULL, priority, "[bt %d] %p",i,array[i]);
    }
  else
    {
      for (i = 0; i < size; i++)
	zlog(NULL, priority, "[bt %d] %s",i,strings[i]);
      free(strings);
    }
#endif /* HAVE_GLIBC_BACKTRACE */
}

static void uzlog_set_effective_level(struct zlog* zl) ;


static void
zlog_abort (const char *mess)
{
#if LOG_DEBUG
  /* May not be locked -- but that doesn't matter any more              */
  ++log_lock_count ;
#endif

  if (zlog_default != NULL)
    {
      if (zlog_default->file_fd < 0)
        zlog_default->file_fd = open_crashlog() ;

      if (zlog_default->file_fd >= 0)
        {
          zlog_default->maxlvl[ZLOG_DEST_FILE] = LOG_ERR;
          uzlog_set_effective_level(zlog_default) ;
        } ;
    } ;

  zlog(NULL, LOG_CRIT, "%s", mess);
  uzlog_backtrace(LOG_CRIT);
  zabort_abort();
}

/*==============================================================================
 * Opening, closing, setting log levels etc.
 *
 */
static int uzlog_set_file(struct zlog *zl, const char *filename, int level) ;
static int uzlog_reset_file(struct zlog *zl) ;
static void uzlog_set_effective_level(struct zlog* zl) ;

/*------------------------------------------------------------------------------
 * Get the effective zlog stream.
 */
static inline struct zlog* zlog_actual(struct zlog* zl)
{
  return zl != NULL ? zl : zlog_default ;
} ;

/*------------------------------------------------------------------------------
 * Open logging -- create and initialise a struct zlog object.
 *
 * Opens a connection to syslog.
 *
 * This must be done very early in the morning, before any pthreading starts.
 */
extern struct zlog *
openzlog (const char *progname, zlog_proto_t protocol,
	                                  int syslog_flags, int syslog_facility)
{
  struct zlog *zl;
  u_int i;

  zl = XCALLOC(MTYPE_ZLOG, sizeof (struct zlog));

  zl->ident          = progname;
  zl->protocol       = protocol;
  zl->facility       = syslog_facility;
  zl->syslog_options = syslog_flags;

  /* Set initial logging levels.
   */
  for (i = 0 ; i < ZLOG_DEST_COUNT ; ++i)
    zl->maxlvl[i]    = ZLOG_DISABLED ;

  zl->monitor_lvl    = LOG_DEBUG ;
  zl->default_lvl    = LOG_DEBUG ;

  openlog (progname, syslog_flags, zl->facility);

  zl->syslog    = true ;                /* have syslog          */
  zl->stdout_fd = fileno(stdout) ;      /* assume have stdout   */
  zl->file_fd   = -1 ;                  /* no file, yet         */

  assert(zlog_list == NULL) ;           /* can do this once !   */
  zlog_list = zl ;

  uzlog_set_effective_level(zl) ;

  return zl;
} ;

/*------------------------------------------------------------------------------
 * Close logging -- destroy struct zlog object.
 *
 * Closes connection to syslog and any log file.
 */
extern void
closezlog (struct zlog *zl)
{
  assert((zl == zlog_list) && (zl->next == NULL)) ;     /* pro tem      */

  LOG_LOCK() ;

  closelog();
  if (zl->file_fd >= 0)
    close (zl->file_fd) ;

  if (zl->filename != NULL)
    free (zl->filename) ;

  zl->syslog    = false ;
  zl->file_fd   = -1 ;
  zl->stdout_fd = -1 ;

  uzlog_set_effective_level(zl) ;

  XFREE (MTYPE_ZLOG, zl);

  LOG_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Set new logging level for the given destination.
 *
 * If the log_level is ZLOG_DISABLED, then the destination is, effectively,
 * disabled.
 *
 * Note that for file logging need to use zlog_set_file() to set a file in
 * the first place and zlog_reset_file() to actually close a file destination.
 *
 * Update the effective maxlvl for this zlog, and the max_maxlvl for all zlog.
 *
 * Note that for monitor the sets the separate zl->monitor_lvl.  The entry
 * in the zl->maxlvl[] vector is the maximum of all *active* monitors, not
 * the current configured level.
 */
extern void
zlog_set_level (struct zlog *zl, zlog_dest_t dest, int level)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    {
      if (dest != ZLOG_DEST_MONITOR)
        {
          zl->maxlvl[dest] = level ;
          uzlog_set_effective_level(zl) ;
        }
      else
        zl->monitor_lvl = level ;
    }

  LOG_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Set new log file: name and level.
 *
 * Note that this closes any existing file
 *
 * Returns:  0 => OK
 *           errno otherwise.
 */
extern int
zlog_set_file(struct zlog *zl, const char *filename, int level)
{
  int err ;

  LOG_LOCK() ;

  err = uzlog_set_file(zl, filename, level) ;

  LOG_UNLOCK() ;
  return err ;
}

static int
uzlog_set_file(struct zlog *zl, const char *filename, int level)
{
  int err ;

  LOG_ASSERT_LOCKED() ;

  err = 0 ;
  if ((zl = zlog_actual(zl)) != NULL)
    {
      mode_t oldumask;
      int fd ;

      /* Close any existing file                                        */
      uzlog_reset_file(zl);

      /* Open file making damn sure we get the mode we want !           */
      oldumask = umask (0777 & ~LOGFILE_MASK);
      fd = open(filename, O_WRONLY | O_APPEND | O_CREAT, LOGFILE_MASK);
      if (fd < 0)
        err = errno ;
      else
        {
          /* Set flags. */
          zl->filename = strdup (filename) ;
          zl->file_fd  = fd ;
          zl->maxlvl[ZLOG_DEST_FILE] = level;

          uzlog_set_effective_level(zl) ;
        } ;

       umask(oldumask);
    } ;

  return err ;
}

/*------------------------------------------------------------------------------
 * Close any existing log file.  Set level to ZLOG_DISABLED.
 *
 * Note that this closes any existing file
 *
 * Returns:  1
 */
extern int
zlog_reset_file(struct zlog *zl)
{
  int ret ;
  LOG_LOCK() ;

  ret = uzlog_reset_file(zl) ;

  LOG_UNLOCK() ;
  return ret ;
}

static int
uzlog_reset_file(struct zlog *zl)
{
  LOG_ASSERT_LOCKED() ;

  if ((zl = zlog_actual(zl)) != NULL)
    {
      if (zl->file_fd >= 0)
        close (zl->file_fd) ;

      zl->file_fd = -1 ;
      zl->maxlvl[ZLOG_DEST_FILE] = ZLOG_DISABLED;

      if (zl->filename != NULL)
        free (zl->filename) ;

      zl->filename = NULL ;

      uzlog_set_effective_level(zl) ;
    } ;

  return 1;
}

/*------------------------------------------------------------------------------
 * Close and reopen log file  -- TODO and the point ??
 */
extern int
zlog_rotate (struct zlog *zl)
{
  int   err ;
  char* filename ;

  filename = NULL ;
  err      = 0 ;

  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    {
      if (zl->file_fd >= 0)
        {
          filename = zl->filename ;
          zl->filename = NULL ;         /* co-opt the name      */

          err = uzlog_set_file(zl, filename, zl->maxlvl[ZLOG_DEST_FILE]) ;
        } ;
    } ;

  LOG_UNLOCK() ;

  if (err != 0)
    zlog(NULL, LOG_ERR,
           "Log rotate failed: cannot open file %s for append: %s",
                                                 filename, errtoa(err, 0).str) ;
  if (filename != NULL)
    free(filename) ;                    /* discard old copy     */

  return (err == 0) ? 1 : -1 ;
}

/*------------------------------------------------------------------------------
 * Set the current maximum monitor level and the effective level (the same in
 * this case).
 */
extern void
uzlog_set_monitor(struct zlog *zl, int level)
{
  if ((zl = zlog_actual(zl)) != NULL)
    {
      zl->maxlvl[ZLOG_DEST_MONITOR] = level ;
      uzlog_set_effective_level(zl) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * get the current default level
 */
extern int
zlog_get_default_lvl (struct zlog *zl)
{
  int level ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  level = (zl != NULL) ? zl->default_lvl : LOG_DEBUG ;

  LOG_UNLOCK() ;
  return level ;
}

/*------------------------------------------------------------------------------
 * set the default level
 */
extern void
zlog_set_default_lvl (struct zlog *zl, int level)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    zl->default_lvl = level;

  LOG_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Set default logging level and the level for any destination not ZLOG_DISABLED
 *
 * Note that on openzlog(), all destinations are set ZLOG_DISABLED.
 */
extern void
zlog_set_default_lvl_dest (struct zlog *zl, int level)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    {
      int i;

      zl->default_lvl = level;

      for (i = 0; i < ZLOG_DEST_COUNT; i++)
        if (zl->maxlvl[i] != ZLOG_DISABLED)
          zl->maxlvl[i] = level ;

      uzlog_set_effective_level(zl) ;
    } ;

  LOG_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Get the current level for the given destination.
 *
 * Note that for monitor the gets the separate zl->monitor_lvl.  The entry
 * in the zl->maxlvl[] vector is the maximum of all *active* monitors, not
 * the current configured level.
 */
extern int
zlog_get_maxlvl (struct zlog *zl, zlog_dest_t dest)
{
  int level ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  if (zl == NULL)
    level = ZLOG_DISABLED ;
  else
    level = (dest != ZLOG_DEST_MONITOR) ? zl->maxlvl[dest] : zl->monitor_lvl ;

  LOG_UNLOCK() ;
  return level;
} ;

/*------------------------------------------------------------------------------
 * Get the current monitor level
 */
extern int
uzlog_get_monitor_lvl(struct zlog *zl)
{
  LOG_ASSERT_LOCKED() ;

  zl = zlog_actual(zl) ;
  return (zl == NULL) ? ZLOG_DISABLED : zl->monitor_lvl ;
} ;

/*------------------------------------------------------------------------------
 * Get the current facility setting for syslog
 */
extern int
zlog_get_facility (struct zlog *zl)
{
  int facility ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  facility = (zl != NULL) ? zl->facility : LOG_DAEMON ;

  LOG_UNLOCK() ;
  return facility ;
} ;

/*------------------------------------------------------------------------------
 * Set the current facility setting for syslog
 */
extern void
zlog_set_facility (struct zlog *zl, int facility)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    zl->facility = facility ;

  LOG_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Get the record priority setting
 */
extern bool
zlog_get_record_priority (struct zlog *zl)
{
  bool priority ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  priority = (zl != NULL) ? zl->record_priority : false ;

  LOG_UNLOCK() ;
  return priority ;
}

/*------------------------------------------------------------------------------
 * Set the record priority setting
 */
extern void
zlog_set_record_priority (struct zlog *zl, bool record_priority)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    zl->record_priority = record_priority ;

  LOG_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Get the timestamp precision setting
 */
extern int
zlog_get_timestamp_precision (struct zlog *zl)
{
  int precision = 0;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  precision = (zl != NULL) ? zl->timestamp_precision : 0 ;

  LOG_UNLOCK() ;
  return precision ;
}

/*------------------------------------------------------------------------------
 * Get the timestamp precision setting
 */
extern void
zlog_set_timestamp_precision (struct zlog *zl, int timestamp_precision)
{
  LOG_LOCK() ;

  if ((zl = zlog_actual(zl)) != NULL)
    zl->timestamp_precision = timestamp_precision;

  LOG_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Get name of the protocol -- name of ZLOG_NONE if no zlog set up yet.
 */
extern const char *
zlog_get_proto_name (struct zlog *zl)
{
  const char* name ;
  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  name = zlog_proto_names[(zl != NULL) ? zl->protocol : ZLOG_NONE] ;

  LOG_UNLOCK() ;
  return name ;
}

/*------------------------------------------------------------------------------
 * caller must free result
 */
extern char *
zlog_get_filename (struct zlog *zl)
{
  char* name = NULL;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  name = ((zl != NULL) && (zl->filename != NULL)) ? strdup(zl->filename)
                                                  : NULL ;
  LOG_UNLOCK() ;
  return name ;
}

/*------------------------------------------------------------------------------
 * Get address of ident string
 */
extern const char *
zlog_get_ident (struct zlog *zl)
{
  const char* ident ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  ident = (zl != NULL) ? zl->ident : NULL ;

  LOG_UNLOCK() ;
  return ident ;
}

/*------------------------------------------------------------------------------
 * logging to a file?
 */
extern bool
zlog_is_file (struct zlog *zl)
{
  bool is_file ;

  LOG_LOCK() ;

  zl = zlog_actual(zl) ;
  is_file = (zl != NULL) ? (zl->file_fd >= 0) : false ;

  LOG_UNLOCK() ;;
  return is_file ;
}

/*------------------------------------------------------------------------------
 * Setting of emaxlvl for given destination.
 */
inline static void
uzlog_set_emaxlvl(struct zlog *zl, zlog_dest_t dest, bool test)
{
  zl->emaxlvl[dest] = test ? zl->maxlvl[dest] : ZLOG_DISABLED ;
};

/*------------------------------------------------------------------------------
 * Update effective logging level for the given destination, and max_maxlvl.
 *
 * The effective logging level takes into account whether the destination is
 * enabled or not.
 */
static void
uzlog_set_effective_level(struct zlog *zl)
{
  int emaxlvl ;

  LOG_ASSERT_LOCKED() ;

  /* Re-establish the emaxlvl for this logging stream.                  */
  uzlog_set_emaxlvl(zl, ZLOG_DEST_SYSLOG,  zl->syslog        ) ;
  uzlog_set_emaxlvl(zl, ZLOG_DEST_FILE,    zl->file_fd   >= 0) ;
  uzlog_set_emaxlvl(zl, ZLOG_DEST_STDOUT,  zl->stdout_fd >= 0) ;
  uzlog_set_emaxlvl(zl, ZLOG_DEST_MONITOR, true) ;
  confirm(ZLOG_DEST_COUNT == 4) ;

  /* Scan all known logging streams, and re-establish max_maxlvl.
   *
   * Do not expect there to be many of these, or that we will change them very
   * often -- so nothing clever, here.
   */
  emaxlvl = ZLOG_DISABLED ;
  zl = zlog_list ;
  while (zl != NULL)
    {
      uint i ;
      for (i = 0 ; i < ZLOG_DEST_COUNT ; ++i)
        if (emaxlvl < zl->emaxlvl[i])
          emaxlvl = zl->emaxlvl[i] ;
      zl = zl->next ;
    } ;

  max_maxlvl = emaxlvl ;
} ;

/*==============================================================================
 *
 */

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

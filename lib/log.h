/*
 * $Id$
 *
 * Zebra logging funcions.
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

#ifndef _ZEBRA_LOG_H
#define _ZEBRA_LOG_H

#include "misc.h"
#include <signal.h>
#include <syslog.h>
#include <stdio.h>
#include "log_common.h"
#include "vargs.h"
#include "pthread_safe.h"

/* Here is some guidance on logging levels to use:
 *
 * LOG_DEBUG	- For all messages that are enabled by optional debugging
 *		  features, typically preceded by "if (IS...DEBUG...)"
 * LOG_INFO	- Information that may be of interest, but everything seems
 *		  to be working properly.
 * LOG_NOTICE	- Only for message pertaining to daemon startup or shutdown.
 * LOG_WARNING	- Warning conditions: unexpected events, but the daemon believes
 *		  it can continue to operate correctly.
 * LOG_ERR	- Error situations indicating malfunctions.  Probably require
 *		  attention.
 *
 * Note: LOG_CRIT, LOG_ALERT, and LOG_EMERG are currently not used anywhere,
 * please use LOG_ERR instead.
 */

/*------------------------------------------------------------------------------
 * Pointer to default logging structure.
 *
 * Each daemon initialises this very early in the morning (before processing
 * command line arguments) by:
 *
 *   zlog_default = openzlog(....) ;
 */
extern struct zlog* zlog_default;

/*------------------------------------------------------------------------------
 * zlog opening etc.
 */
extern struct zlog* openzlog (const char *progname, zlog_proto_t protocol,
		              int syslog_options, int syslog_facility);
extern void closezlog (struct zlog *zl);
extern void zlog_set_level(struct zlog *zl, zlog_dest_t, int log_level);
extern int zlog_rotate (struct zlog *);

/*------------------------------------------------------------------------------
 * Functions and macros for logging.
 */
extern void zlog (struct zlog *zl, int priority, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(3, 4);
/* Handy zlog functions.
 */
#define zlog_thus(thus, ...) zlog(zlog_default, thus,        __VA_ARGS__)
#define zlog_err(...)        zlog(zlog_default, LOG_ERR,     __VA_ARGS__)
#define zlog_warn(...)       zlog(zlog_default, LOG_WARNING, __VA_ARGS__)
#define zlog_info(...)       zlog(zlog_default, LOG_INFO,    __VA_ARGS__)
#define zlog_notice(...)     zlog(zlog_default, LOG_NOTICE,  __VA_ARGS__)
#define zlog_debug(...)      zlog(zlog_default, LOG_DEBUG,   __VA_ARGS__)

/* For bgpd's peer oriented log.
 */
#define plog_thus(zl, thus, ...) zlog(zl, thus,        __VA_ARGS__)
#define plog_err(zl, ...)        zlog(zl, LOG_ERR,     __VA_ARGS__)
#define plog_warn(zl, ...)       zlog(zl, LOG_WARNING, __VA_ARGS__)
#define plog_info(zl, ...)       zlog(zl, LOG_INFO,    __VA_ARGS__)
#define plog_notice(zl, ...)     zlog(zl, LOG_NOTICE,  __VA_ARGS__)
#define plog_debug(zl, ...)      zlog(zl, LOG_DEBUG,   __VA_ARGS__)

/*------------------------------------------------------------------------------
 * Message and other lookups.
 */
struct message                  /* For message lookup.          */
{
  int key;
  const char *str;
};

extern const char * zlog_get_proto_name (struct zlog *zl);

#define LOOKUP(x, y) mes_lookup(x, x ## _max, y, "(no item found)")

extern const char *lookup (const struct message *, int);
extern const char *mes_lookup (const struct message *meslist,
                               int max, int index,
                               const char *no_item);

extern const char *zlog_priority[];
extern const char *zlog_proto_names[];

extern const char *safe_strerror(int errnum);

/*------------------------------------------------------------------------------
 *
 */

/* To be called when a fatal signal is caught. */
extern void zlog_signal(int signo, const char *action, siginfo_t *siginfo,
                                                        void *program_counter) ;

/* Ring down the curtain -- turn of SIGABRT handler and abort()         */
extern void zabort_abort(void)  __attribute__ ((noreturn)) ;

/* Log a backtrace. */
extern void zlog_backtrace(int priority);

/* Log a backtrace, but in an async-signal-safe way.  Should not be
   called unless the program is about to exit or abort, since it messes
   up the state of zlog file pointers.  If program_counter is non-NULL,
   that is logged in addition to the current backtrace. */
extern void zlog_backtrace_sigsafe(int priority, void *program_counter);

/*------------------------------------------------------------------------------
 * Defines for use in command construction
 */
#define LOG_LEVELS "(emergencies|alerts|critical|errors|" \
                              "warnings|notifications|informational|debugging)"

#define LOG_LEVEL_DESC \
  "System is unusable\n" \
  "Immediate action needed\n" \
  "Critical conditions\n" \
  "Error conditions\n" \
  "Warning conditions\n" \
  "Normal but significant conditions\n" \
  "Informational messages\n" \
  "Debugging messages\n"

#define LOG_FACILITIES "(kern|user|mail|daemon|auth|syslog|lpr|news|uucp|" \
                 "cron|local0|local1|local2|local3|local4|local5|local6|local7)"

#define LOG_FACILITY_DESC \
  "Kernel\n" \
  "User process\n" \
  "Mail system\n" \
  "System daemons\n" \
  "Authorization system\n" \
  "Syslog itself\n" \
  "Line printer system\n" \
  "USENET news\n" \
  "Unix-to-Unix copy system\n" \
  "Cron/at facility\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n" \
  "Local use\n"

#endif /* _ZEBRA_LOG_H */

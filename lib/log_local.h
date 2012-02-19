/* Logging locking and interface presented to vty et al
 * Copyright (C) 2011 Chris Hall (GMCH), Highwayman
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

#ifndef _ZEBRA_LOG_LOCAL_H
#define _ZEBRA_LOG_LOCAL_H

#include "misc.h"

#include "log_common.h"
#include "qpthreads.h"

/*==============================================================================
 * This is for access to some things in log.c which are not required
 * by external users, who use log.h.
 *
 * This is for use within the log/command/vty family.
 *
 * Should not be used with log.h !  (Except in log.c itself.)
 *
 * This may duplicate things published in log.h, but also includes things
 * which are not intended for "external" use.
 */

/*==============================================================================
 * To make logging qpthread safe we use a single mutex.
 *
 * A normal mutex is used -- recursion is not expected or required.
 *
 * There are some "ulog" functions which assume the mutex is locked.
 *
 * It is assumed that while the log_mutex is owned, NO OTHER SIGNIFICANT lock
 * will be acquired -- in particular, NOT the VTY_LOCK() !!
 */

extern qpt_mutex_t log_mutex ;

/*------------------------------------------------------------------------------
 * Sort out LOG_DEBUG.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if LOG_NO_DEBUG is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set LOG_DEBUG    == 0 to turn off debug
 *       *  or set LOG_DEBUG    != 0 to turn on debug
 *       *  or set LOG_NO_DEBUG != 0 to force debug off
 */

#ifdef LOG_DEBUG                /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(LOG_DEBUG)
#  undef  LOG_DEBUG
#  define LOG_DEBUG 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define LOG_DEBUG QDEBUG
#endif

#ifdef LOG_NO_DEBUG             /* Override, if defined                 */
# if IS_NOT_ZERO_OPTION(LOG_NO_DEBUG)
#  undef  LOG_DEBUG
#  define LOG_DEBUG 0
# endif
#endif

enum { log_debug = LOG_DEBUG } ;

/*------------------------------------------------------------------------------
 * Locking and related functions.
 */
extern int log_lock_count ;

Inline void
LOG_LOCK(void)          /* if is qpthreads_active, lock log_mutex       */
{
  qpt_mutex_lock(log_mutex) ;
  if (log_debug)
    ++log_lock_count ;
} ;

Inline void
LOG_UNLOCK(void)        /* if is qpthreads_active, unlock log_mutex     */
{
  if (log_debug)
    --log_lock_count ;
  qpt_mutex_unlock(log_mutex) ;
} ;

/* For debug (and documentation) purposes, will LOG_ASSERT_LOCKED where that
 * is required.
 *
 * Note that both these checks will pass if !qpthreads_active.  So can have
 * code which is called before qpthreads are started up, or which will never
 * run qpthreaded, or which is active during shutdown !
 */

extern int log_assert_fail ;

Inline void
LOG_ASSERT_FAILED(void)
{
  if (log_assert_fail == 0)
    {
      log_assert_fail = 1 ;
      assert(0) ;
    } ;
} ;

Inline void
LOG_ASSERT_LOCKED(void)
{
  if (log_debug)
    if ((log_lock_count == 0) && (qpthreads_active))
      LOG_ASSERT_FAILED() ;
} ;

/*==============================================================================
 * Functions in log.c -- used in any of the log/command/vty
 */
extern void log_init_r(void) ;
extern void log_finish(void);

extern void uzlog_set_monitor(struct zlog *zl, int level) ;
extern int uzlog_get_monitor_lvl(struct zlog *zl) ;

extern int zlog_set_file(struct zlog *zl, const char *filename, int log_level);
extern int zlog_reset_file(struct zlog *zl);

extern int  zlog_get_default_lvl (struct zlog *zl);
extern void zlog_set_default_lvl (struct zlog *zl, int level);
extern void zlog_set_default_lvl_dest (struct zlog *zl, int level);
extern int  zlog_get_maxlvl (struct zlog *zl, zlog_dest_t dest);
extern int  zlog_get_facility (struct zlog *zl);
extern void zlog_set_facility (struct zlog *zl, int facility);
extern bool zlog_get_record_priority (struct zlog *zl);
extern void zlog_set_record_priority (struct zlog *zl, bool record_priority);
extern int  zlog_get_timestamp_precision (struct zlog *zl);
extern void zlog_set_timestamp_precision (struct zlog *zl, int timestamp_precision);
extern const char * zlog_get_ident (struct zlog *zl);
extern char * zlog_get_filename (struct zlog *zl);
extern bool zlog_is_file (struct zlog *zl);

#endif /* _ZEBRA_LOG_LOCAL_H */

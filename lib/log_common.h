/* Logging definitions used in log.h and log_local.h
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

#ifndef _ZEBRA_LOG_COMMON_H
#define _ZEBRA_LOG_COMMON_H

#include "misc.h"
#include <syslog.h>

/*==============================================================================
 * This is for things which are required in log.h for external use, and in
 * log_local.h for use within the log/command/vty family.
 */

/*------------------------------------------------------------------------------
 * The protocols known to the logging system.
 */
typedef enum
{
  ZLOG_NONE,
  ZLOG_DEFAULT,
  ZLOG_ZEBRA,
  ZLOG_RIP,
  ZLOG_BGP,
  ZLOG_OSPF,
  ZLOG_RIPNG,
  ZLOG_OSPF6,
  ZLOG_ISIS,
  ZLOG_MASC
} zlog_proto_t;

/*------------------------------------------------------------------------------
 * If maxlvl is set to ZLOG_DISABLED, then no messages will be sent
 * to that logging destination.
 *
 * Note that logging levels with higher priority have lower numbers.  So, this
 * may (well) be -ve.
 */
enum { ZLOG_DISABLED = LOG_EMERG - 1 } ;

/*------------------------------------------------------------------------------
 * The logging destinations supported.
 */
typedef enum
{
  ZLOG_DEST_SYSLOG = 0,
  ZLOG_DEST_FILE,
  ZLOG_DEST_STDOUT,
  ZLOG_DEST_MONITOR,

  ZLOG_DEST_COUNT               /* Number of destinations       */
} zlog_dest_t;

/*------------------------------------------------------------------------------
 *
 */
struct zlog ;

enum { timestamp_buffer_len = 32 } ;

#endif /* _ZEBRA_LOG_COMMON_H */

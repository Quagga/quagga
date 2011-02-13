/* VTY top level
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Revisions: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#ifndef _ZEBRA_VTY_LOCAL_H
#define _ZEBRA_VTY_LOCAL_H

#include "vty_common.h"         /* First and foremost                   */
#include "command_common.h"

#include "vargs.h"

#include "qpthreads.h"
#include "qpnexus.h"
#include "thread.h"

/*==============================================================================
 * This is for access to some things in vty.c which are not required
 * by external users, who use vty.h.
 *
 * This is for use within the command/vty family.
 *
 * Should not be used with vty.h !  (Except in vty.c itself.)
 *
 * This may duplicate things published in vty.h, but also includes things
 * which are not intended for "external" use.
 */

/*==============================================================================
 * The VTYSH uses a unix socket to talk to the daemon.
 *
 * The ability to respond to a connection from VTYSH appears to be a *compile*
 * time option.  In the interests of keeping the code up to date, the VTYSH
 * option is turned into a testable constant.
 */
#ifdef VTYSH
  enum { VTYSH_ENABLED = true  } ;
#else
  enum { VTYSH_ENABLED = false } ;
#endif

/*==============================================================================
 * Variables in vty.c -- used in any of the family
 */
extern struct vty_io* vio_list_base ;
extern struct vty_io* vio_monitors_base ;
extern struct vty_io* vio_death_watch ;

extern struct thread_master* vty_master ;

extern bool vty_nexus ;

extern qpn_nexus vty_cli_nexus ;
extern qpn_nexus vty_cmd_nexus ;

/*==============================================================================
 * To make vty qpthread safe we use a single mutex.
 *
 * vty and log recurse through each other, so the same mutex is used
 * for both, i.e. they are treated as being part of the same monitor.
 *
 * A recursive mutex is used.  This simplifies the calling from log to vty and
 * back again.  It also allows for the vty internals to call each other.
 *
 * There are some "uty" functions which assume the mutex is locked.
 *
 * vty is closely bound to the command handling -- the main vty structure
 * contains the context in which commands are parsed and executed.
 */

extern qpt_mutex_t vty_mutex ;

#ifdef VTY_DEBUG                /* Can be forced from outside           */
# if VTY_DEBUG
#  define VTY_DEBUG 1           /* Force 1 or 0                         */
#else
#  define VTY_DEBUG 0
# endif
#else
# ifdef  QDEBUG
#  define VTY_DEBUG 1           /* Follow QDEBUG                        */
#else
#  define VTY_DEBUG 0
# endif
#endif

enum { vty_debug = VTY_DEBUG } ;

extern int vty_lock_count ;

Inline void
VTY_LOCK(void)          /* if is qpthreads_enabled, lock vty_mutex      */
{
  qpt_mutex_lock(&vty_mutex) ;
  if (vty_debug)
    ++vty_lock_count ;
} ;

Inline void
VTY_UNLOCK(void)        /* if is qpthreads_enabled, unlock vty_mutex    */
{
  if (vty_debug)
    --vty_lock_count ;
  qpt_mutex_unlock(&vty_mutex) ;
} ;

Inline bool             /* true => is (effectively) cli thread          */
vty_is_cli_thread(void)
{
  return !qpthreads_enabled || qpt_thread_is_self(vty_cli_nexus->thread_id) ;
} ;

/* For debug (and documentation) purposes, will VTY_ASSERT_LOCKED where that
 * is required.
 *
 * In some cases, need also to be running in the CLI thread as well.
 *
 * Note that both these checks will pass if !qpthreads_enabled.  So can have
 * code which is called before qpthreads are started up, or which will never
 * run qpthreaded !
 */
#if VTY_DEBUG

extern int vty_assert_fail ;

Inline void
VTY_ASSERT_FAILED(void)
{
  if (vty_assert_fail == 0) ;
    {
      vty_assert_fail = 1 ;
      assert(0) ;
    } ;
} ;

Inline void
VTY_ASSERT_LOCKED(void)
{
  if ((vty_lock_count == 0) && (qpthreads_enabled))
    VTY_ASSERT_FAILED() ;
} ;

Inline void
VTY_ASSERT_CLI_THREAD(void)
{
  if (!vty_is_cli_thread())
    VTY_ASSERT_FAILED() ;
} ;

#else

#define VTY_ASSERT_LOCKED()
#define VTY_ASSERT_CLI_THREAD()

#endif

/*==============================================================================
 * Functions in vty.c -- used in any of the family
 */
extern int vty_out (vty vty, const char* format, ...) PRINTF_ATTRIBUTE(2, 3);
extern int vty_out_indent(vty vty, int indent) ;
extern void vty_out_clear(vty vty) ;

extern void vty_set_lines(vty vty, int lines);

extern bool vty_close(vty vty, bool final, qstring reason) ;

extern void vty_open_config_write(vty vty, int fd) ;
extern int vty_close_config_write(vty vty) ;

extern void vty_log_fixed (const char *buf, size_t len);

struct logline ;        /* forward reference    */
struct zlog ;           /* forward reference    */

extern void uty_log (struct logline* ll, struct zlog *zl, int priority,
                                                const char *format, va_list va);

#endif /* _ZEBRA_VTY_LOCAL_H */

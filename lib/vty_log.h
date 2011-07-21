/* VTY interface to logging -- header
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

#ifndef _ZEBRA_VTY_LOG_H
#define _ZEBRA_VTY_LOG_H

#include "misc.h"
#include "vty_io.h"

/*==============================================================================
 */

extern void uty_init_monitor(void) ;
extern void uty_terminate_monitor(void) ;

extern void uty_set_monitor(vty_io vio, bool on) ;
extern void vty_set_monitor_level(vty vty, int level) ;

extern void vty_log(int priority, const char* line, uint len) ;
extern void vty_log_fixed(const char* buf, uint len) ;

#endif /* _ZEBRA_VTY_LOG_H */

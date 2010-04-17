/*
 * kernel routing table reading prototype.
 * Copyright (C) 1998 Kunihiro Ishiguro
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

#ifndef _ZEBRA_RTREAD_H
#define _ZEBRA_RTREAD_H

/* There are (as at 17-Apr-2010) the following rtread methods:
 *
 *   rtread_getmsg.c
 *   rtread_netlink.c
 *   rtread_proc.c
 *   rtread_sysctl.c
 *
 * one of which is selected at "configure" time, see: RTREAD_METHOD
 */

extern void route_read (void) ;

#endif /* _ZEBRA_RTREAD_H */

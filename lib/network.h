/*
 * Network library header.
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

#ifndef _ZEBRA_NETWORK_H
#define _ZEBRA_NETWORK_H

extern int readn (int, void*, int);
extern int writen (int, const void*, int);
extern int copyn (int dst_fd, int src_fd) ;

extern int set_nonblocking(int fd);
extern int set_close_on_exec(int fd) ;

int read_nb(int fd, void* buf, size_t nbyte) ;
int write_nb(int fd, const void* buf, size_t nbyte) ;

#define ERRNO_IO_RETRY(EN) \
	(((EN) == EAGAIN) || ((EN) == EWOULDBLOCK) || ((EN) == EINTR))

#endif /* _ZEBRA_NETWORK_H */

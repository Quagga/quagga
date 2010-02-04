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

#ifndef PTHREAD_SAFE_H_
#define PTHREAD_SAFE_H_

#include <netinet/in.h>

extern void safe_init_r(void);
extern void safe_finish(void);
extern const char * safe_strerror(int errnum);
extern const char * safe_inet_ntoa (struct in_addr in);

#endif /* PTHREAD_SAFE_H_ */

/* Quagga library initialise/closedown -- header
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

#ifndef _ZEBRA_QLIB_INIT_H
#define _ZEBRA_QLIB_INIT_H

#include "misc.h"
#include <sys/stat.h>

/*==============================================================================
 * Quagga Library Initialise/Closedown
 *
 * This gathers together the essential initialisation and closedown for the
 * library.
 *
 * See qlib_init.c.
 */

extern void qlib_init_first_stage(mode_t cmask) ;

extern void qlib_init_second_stage(bool pthreaded) ;

extern void qexit(int exit_code, bool mem_stats) ;

/*==============================================================================
 * System parameters, set at qlib_init_first_stage() time.
 */

extern int qlib_pagesize ;              /* _SC_PAGE_SIZE        */
extern int qlib_iov_max ;               /* _SC_IOV_MAX          */
extern int qlib_open_max ;              /* _SC_OPEN_MAX         */
extern int qlib_thread_cputime ;        /* _SC_THREAD_CPUTIME   */

#endif /* _ZEBRA_QLIB_INIT_H */

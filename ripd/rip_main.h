/*
 * This file, a part of Quagga, implements ripd main() function.
 *
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef QUAGGA_RIP_MAIN_H_
#define QUAGGA_RIP_MAIN_H_

#include "thread.h"
#include "privs.h"

/* privileges global */
extern struct zebra_privs_t ripd_privs;

/* Default configuration file name. */
#define RIPD_DEFAULT_CONFIG    "ripd.conf"

/* Master thread strucutre. */
extern struct thread_master *master;

#endif /* QUAGGA_RIP_MAIN_H_ */

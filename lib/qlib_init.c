/* Quagga library initialise/closedown -- functions
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

#include "qlib_init.h"
#include "zassert.h"
#include "memory.h"
#include "qpthreads.h"
#include "qpselect.h"
#include "thread.h"
#include "privs.h"
#include "mqueue.h"
#include "pthread_safe.h"

/*==============================================================================
 * Quagga Library Initialise/Closedown
 *
 * This gathers together the essential initialisation and closedown for the
 * library.  This ensures that any changes in the library are contained here,
 * and do not require changes in all users of the library.
 *
 * There are two stages of initialisation:
 *
 *   1) first stage
 *
 *      this is expected to be called before the program does anything at all.
 *
 *      This performs all initialisation required to support asserts, logging,
 *      basic I/O (but not the remote console), trap signals... and so on.
 *
 *      After this has been done, the system is in good shape to deal with
 *      command line options, configuration files and so on.
 *
 *   2) second stage
 *
 *      this is expected to be called before the program does any serious work.
 *
 *      This performs all initialisation required to support socket I/O,
 *      thread handling, timers, and so on.
 *
 *      In particular, at this stage the system is set into Pthread Mode, if
 *      required.  No pthreads may be started before this.  Up to this point
 *      the system operates in non-Pthread Mode -- all mutexes are implicitly
 *      free.
 *
 * There is one stage of closedown.  This is expected to be called last, and
 * is passed the exit code.
 *
 *
 */

void
qlib_init_first_stage(void)
{
  qps_start_up() ;
}

void
qlib_init_second_stage(int pthreads)
{
  qpt_set_qpthreads_enabled(pthreads);
  memory_init_r();
  thread_init_r();
  zprivs_init_r();
  mqueue_initialise();
  safe_init_r();
}


void
qexit(int exit_code)
{
  safe_finish();
  mqueue_finish();
  zprivs_finish();
  thread_finish();
  memory_finish();
  exit (exit_code);
}



/*
 * Quagga Signal handling header.
 *
 * Copyright (C) 2004 Paul Jakma.
 *
 * This file is part of Quagga.
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

#ifndef _QUAGGA_SIGNAL_H
#define _QUAGGA_SIGNAL_H

#include <thread.h>
#include <signal.h>

#define QUAGGA_SIGNAL_TIMER_INTERVAL 2L
#define Q_SIGC(sig) (sizeof(sig)/sizeof(sig[0]))

typedef void qsig_event(void) ;

struct quagga_signal_t
{
  int           signal ;        /* signal number        */
  qsig_event*   handler ;       /* event function      */
} ;

extern void signal_init (struct thread_master *m, int sigc,
                                             struct quagga_signal_t *signals);

extern int quagga_sigevent_process (void);

extern const sigset_t* signal_get_hard_set(void) ;
extern void quagga_sigabrt_no_trap(void) ;
extern void quagga_signal_reset(void) ;

extern void sigmakeset(sigset_t* set, ...) ;
extern void sigcopyset(sigset_t* dst, const sigset_t* src) ;
extern void sigaddsets(sigset_t* a, const sigset_t* b) ;
extern void sigsubsets(sigset_t* a, const sigset_t* b) ;
extern void siginvset(sigset_t* a, const sigset_t* b) ;
extern int sigincommon(const sigset_t* a, const sigset_t* b) ;
extern int sighasmember(const sigset_t* a) ;

#endif /* _QUAGGA_SIGNAL_H */

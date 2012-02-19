/* VTY IO TERM -- Telnet Terminal I/O -- header
 * Virtual terminal [aka TeletYpe] interface routine.
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

#ifndef _ZEBRA_VTY_IO_TERM_H
#define _ZEBRA_VTY_IO_TERM_H

#include "misc.h"

#include "vty_io.h"
#include "vty_command.h"

#include "vio_fifo.h"
#include "keystroke.h"
#include "thread.h"

/*==============================================================================
 * Here are structures and other definitions for I/O to VTY_TERMINAL.
 */

/*==============================================================================
 * Functions
 */

extern void uty_term_new(vty_io vio, int sock_fd) ;

extern cmd_ret_t uty_term_cmd_line_fetch(vio_vf vf) ;
extern cmd_ret_t uty_term_cmd_complete(vio_vf vf, cmd_context context,
                                                                 bool closing) ;
extern cmd_ret_t uty_term_out_push(vio_vf vf) ;
extern bool uty_term_out_cancelled(vio_vf vf) ;
extern cmd_ret_t uty_term_read_close(vio_vf vf) ;
extern void uty_term_close_reason(vio_vf vf, qstring wrapped) ;
extern cmd_ret_t uty_term_write_close(vio_vf vf);

extern void uty_term_read(vio_vf vf) ;
extern cmd_ret_t uty_term_set_readiness(vio_vf vf, cmd_ret_t ret) ;

extern qtimer_action vty_term_pause_timeout ;

extern void uty_term_mon_write(vio_vf vf) ;

extern bool uty_telnet_command(vio_vf, keystroke stroke, bool callback) ;


extern void uty_term_open_listeners(const char *addr, unsigned short port) ;


#endif /* _ZEBRA_VTY_IO_TERM_H */

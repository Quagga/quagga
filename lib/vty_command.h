/* VTY for command execution
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

#ifndef _ZEBRA_VTY_COMMAND_H
#define _ZEBRA_VTY_COMMAND_H

#include "misc.h"

#include "command_local.h"
#include "vty_local.h"
#include "vty_io.h"

extern bool vty_cmd_config_loop_prepare(vty vty) ;
extern void uty_cmd_queue_loop_enter(vty_io vio) ;
extern void uty_cmd_signal(vty_io vio, cmd_return_code_t ret) ;
extern bool uty_cmd_loop_stop(vty_io vio, bool curtains) ;

extern void vty_cmd_loop_exit(vty vty) ;
extern void vty_cmd_set_stopped(vty vty) ;
extern void vty_cmd_check_stop(vty vty, cmd_return_code_t ret) ;

extern cmd_return_code_t vty_cmd_fetch_line(vty vty) ;
extern cmd_return_code_t vty_cmd_hiatus(vty vty, cmd_return_code_t ret) ;
extern cmd_return_code_t vty_cmd_special(vty vty) ;
extern cmd_return_code_t vty_cmd_can_auth_enable(vty vty) ;
extern cmd_return_code_t vty_cmd_reflect_line(vty vty) ;
extern cmd_return_code_t vty_cmd_out_push(vty vty) ;
extern cmd_return_code_t uty_cmd_out_push(vio_vf vf, bool final) ;

extern cmd_return_code_t uty_cmd_open_in_pipe_file(vty_io vio,
                   cmd_context context, qstring name,    cmd_pipe_type_t type) ;
extern cmd_return_code_t uty_cmd_open_in_pipe_shell(vty_io vio,
                   cmd_context context, qstring command, cmd_pipe_type_t type) ;
extern cmd_return_code_t uty_cmd_open_out_pipe_file(vty_io vio,
                   cmd_context context, qstring name,    cmd_pipe_type_t type,
                                                                   bool after) ;
extern cmd_return_code_t uty_cmd_open_out_dev_null(vty_io vio, bool after) ;
extern cmd_return_code_t uty_cmd_open_out_pipe_shell(vty_io vio,
                   cmd_context context, qstring command, cmd_pipe_type_t type,
                                                                   bool after) ;
extern qpath uty_cmd_path_name_complete(qpath dst, const char* name,
                                                         cmd_context context) ;

extern cmd_return_code_t vty_cmd_success(vty vty) ;
extern vio_fifo uty_cmd_get_ebuf(vty_io vio) ;

extern cmd_return_code_t vty_cmd_config_lock (vty vty) ;
extern void vty_cmd_config_lock_check(struct vty *vty, node_type_t node) ;

#endif /* _ZEBRA_VTY_COMMAND_H */

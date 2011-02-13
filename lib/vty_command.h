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

extern cmd_return_code_t uty_cmd_dispatch(vty_io vio, cmd_do_t to_do,
                                                                 qstring line) ;
extern cmd_return_code_t vty_cmd_fetch_line(vty vty) ;
extern cmd_return_code_t vty_cmd_special(vty vty) ;
extern void vty_cmd_reflect_line(vty vty) ;
extern void vty_cmd_out_push(vty vty) ;
extern void uty_cmd_out_push(vty_io vio) ;

extern cmd_return_code_t vty_cmd_open_in_pipe_file(vty vty, qstring name,
                                                                 bool reflect) ;
extern cmd_return_code_t vty_cmd_open_in_pipe_shell(vty vty) ;
extern cmd_return_code_t vty_cmd_open_out_pipe_file(vty vty, qstring name,
                                                                  bool append) ;
extern cmd_return_code_t vty_cmd_open_out_dev_null(vty vty) ;
extern cmd_return_code_t vty_cmd_open_out_pipe_shell(vty vty) ;
extern cmd_return_code_t vty_cmd_success(vty vty) ;
extern cmd_return_code_t vty_cmd_loop_exit(vty vty, cmd_return_code_t ret) ;

extern void uty_cmd_prepare(vty_io vio) ;

extern cmd_return_code_t uty_cmd_close(vty_io vio, const char* reason) ;

extern bool vty_config_lock (vty vty, node_type_t node);
extern void vty_config_unlock (vty vty, node_type_t node);
extern void uty_config_unlock (vty vty, node_type_t node) ;



#endif /* _ZEBRA_VTY_COMMAND_H */

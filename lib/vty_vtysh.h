/* VTY IO SHELL -- VTY Shell I/O -- header
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

#ifndef _ZEBRA_VTY_SHELL_H
#define _ZEBRA_VTY_SHELL_H

#include "misc.h"
#include "list_util.h"

#include "vty_common.h"
#include "vty_io_vtysh.h"
#include "command_common.h"

/*==============================================================================
 * Handling of commands in the vtysh
 */

/*------------------------------------------------------------------------------
 * A few common commands in the lib require special handling in vtysh.
 * Here is the structure which contains all the call-back functions which
 * implement that.
 */
typedef struct
{
  cmd_ret_t (*terminal_length)(vty vty, int length) ;
  cmd_ret_t (*show_history)(vty vty) ;
  cmd_ret_t (*show_integrated_config)(vty vty) ;
  cmd_ret_t (*write_integrated_config)(vty vty) ;

} vtysh_cmd_call_backs_t ;

/*==============================================================================
 * Functions
 */
extern vty vty_vtysh_open(vtysh_cmd_call_backs_t* call_backs, bool no_prefix) ;
extern void vty_vtysh_promote(vty vty) ;
extern daemon_set_t vty_vtysh_open_clients(vty vty, daemon_set_t daemons) ;
extern daemon_set_t vty_vtysh_check_clients(vty vty) ;
extern cmd_ret_t vty_vtysh_command_loop(vty vty, const char* line,
                                            bool interactive, ulen prompt_len) ;
extern qstring vty_vtysh_prep_line(vty vty, const char* line, ulen len,
                                                                     ulen pos) ;
extern cmd_ret_t vty_vtysh_fetch_config(vty vty,
                         void (*collect)(daemon_set_t daemon, vio_fifo buf),
                                                                    bool show) ;

extern void uty_vtysh_out_prep(vty_io vio, const char* pager_name) ;
extern void uty_vtysh_out_close_pager(vio_vf vf) ;
extern cmd_ret_t uty_vtysh_out_push(vio_vf vf) ;
extern cmd_ret_t uty_vtysh_out_close(vio_vf vf) ;

/*==============================================================================
 * Globals for handling connected clients in vtysh
 */
extern vtysh_cmd_call_backs_t* vtysh_cmd_call_backs ;

#endif /* _ZEBRA_VTY_SHELL_H */

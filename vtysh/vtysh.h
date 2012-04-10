/* Virtual terminal interface shell.
 * Copyright (C) 2000 Kunihiro Ishiguro
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

#ifndef VTYSH_H
#define VTYSH_H

#include "lib/misc.h"
#include "lib/vty.h"

#include <signal.h>

/* vtysh local configuration file.
 */
#define VTYSH_DEFAULT_CONFIG "vtysh.conf"

/* The interactive command handling uses some longjumps to cope with
 * ^Z and ^C.
 */
enum
{
  vtysh_lj_init  = 0,   /* by definition        */

  vtysh_lj_ctrl_c,
  vtysh_lj_ctrl_z,
} ;

/*------------------------------------------------------------------------------
 * See vtysh.c
 */
extern void vtysh_cmd_init(void) ;
extern void vtysh_vty_init(bool no_prefix) ;
extern bool vtysh_daemons_connect(vty vtysh, daemon_set_t daemons,
                                             daemon_set_t required) ;
extern daemon_set_t vtysh_daemons_list_ok(vty vtysh, qstring daemon_list) ;
extern bool vtysh_connected_ok(vty vtysh, bool quiet, bool fail) ;
extern void vtysh_readline_init(void) ;
extern cmd_ret_t vtysh_set_integrated_config(on_off_b integrated) ;

extern int vtysh_pager_set(int lines) ;
extern cmd_ret_t vtysh_execute(vty vtysh, const char* line, ulen prompt_len) ;

extern vty vtysh_vty ;

/* Child process execution flag.
 */
extern volatile sig_atomic_t execute_flag;

/* Using integrated config from Quagga.conf. Default is no.
 */
extern bool vtysh_integrated_vtysh_config;

/*------------------------------------------------------------------------------
 * See vtysh_config.c
 */
extern bool vtysh_read_config (vty boot_vty, qpath config_file, bool required,
                                             bool ignore_warnings, bool quiet) ;
extern cmd_ret_t vtysh_config_collect_integrated(vty vtysh, bool show) ;
extern int vtysh_config_write_config_node(vty vtysh, node_type_t node) ;
extern void vtysh_config_reset_integrated(void) ;

#endif /* VTYSH_H */

/* VTY Command Line Handler
 * Copyright (C) 1997 Kunihiro Ishiguro
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

#ifndef _ZEBRA_VTY_CLI_H
#define _ZEBRA_VTY_CLI_H

#include "vty_io.h"
#include "keystroke.h"

extern void uty_cli_init(vty_io vio) ;
extern enum vty_readiness uty_cli_start(vty_io vio) ;
extern void uty_cli_close(vty_io vio) ;

extern enum vty_readiness uty_cli(vty_io vio) ;
extern keystroke_callback uty_cli_iac_callback ;

extern void uty_cli_hist_add (vty_io vio, const char* cmd_line) ;
extern void uty_cli_go_more_wait(vty_io vio) ;
extern void uty_free_host_name(void) ;
extern void uty_check_host_name(void) ;

extern bool uty_cli_draw_if_required(vty_io vio) ;

extern void uty_cli_pre_monitor(vty_io vio, size_t len) ;
extern int uty_cli_post_monitor(vty_io vio, const char* buf, size_t len) ;

#endif /* _ZEBRA_VTY_CLI_H */

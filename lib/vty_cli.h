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

extern void uty_cli(vty_io vio) ;
extern void uty_cli_hist_add (vty_io vio, const char* cmd_line) ;
extern void uty_cli_want_more(vty_io vio) ;
extern void uty_cli_wait_more(vty_io vio) ;
extern void uty_cli_wipe(vty_io vio) ;
extern void uty_free_host_name(void) ;
extern void uty_check_host_name(void) ;

extern void uty_will_echo (vty_io vio) ;
extern void uty_will_suppress_go_ahead (vty_io vio) ;
extern void uty_dont_linemode (vty_io vio) ;
extern void uty_do_window_size (vty_io vio) ;
extern void uty_dont_lflow_ahead (vty_io vio) ;

#endif /* _ZEBRA_VTY_CLI_H */

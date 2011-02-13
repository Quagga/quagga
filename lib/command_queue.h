/* Command Message Queue -- header
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

#ifndef COMMAND_QUEUE_H_
#define COMMAND_QUEUE_H_

#include "vty_local.h"
#include "command_execute.h"
#include "qpnexus.h"

extern void cq_dispatch(vty vty, cmd_do_t to_do, qstring line) ;
extern void cq_go_fetch(vty vty) ;
extern void cq_revoke(vty vty) ;

#endif /* COMMAND_QUEUE_H_ */

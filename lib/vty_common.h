/* VTY top level
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

#ifndef _ZEBRA_VTY_COMMON_H
#define _ZEBRA_VTY_COMMON_H

#include "misc.h"
#include "qstring.h"
#include "command_common.h"

/*==============================================================================
 * These are things required by:
 *
 *   vty.h            -- which is used by all "external" code.
 *
 *   command_local.h  -- which is used by all "internal" code.
 *
 * This allows some things not to be published to "external" code.
 */

/*==============================================================================
 * VTY Types and the VTY structure.
 *
 * The "struct vty" is used extensively across the Quagga daemons, where it
 * has two functions relating to command handling as:
 *
 *   1) a "file handle" for output produced by commands
 *
 *   2) the holder of some context -- notably the current command "node" -- for
 *      command execution to use
 *
 * The bulk of "struct vty" is, therefore, private to vty.c et al and is
 * factored out into the "struct vty_io" -- opaque to users of the struct vty.
 *
 * There is also context used when parsing and executing commands which is
 * private to command.c et al, and is factored out into the "struct execution"
 * -- also opaque to users of the struct vty.
 */
enum vty_type           /* Command output                               */
{
  VTY_TERMINAL,         /* a telnet terminal server                     */
  VTY_SHELL_SERVER,     /* a vty_shell server                           */

  VTY_SHELL_CLIENT,     /* a vty_shell client                           */

  VTY_CONFIG_READ,      /* configuration file reader                    */

  VTY_STDOUT,           /* stdout                                       */
  VTY_STDERR,           /* stderr                                       */
} ;
typedef enum vty_type vty_type_t ;

/* Most of the contents of the vty structure live in two opaque structures,
 * which are forward referenced here.
 */
struct vty_io ;
struct cmd_execution ;

/* All command execution functions take a vty argument, and this is it.
 */
typedef struct vty* vty ;
struct vty
{
  vty_type_t    type ;

  /*----------------------------------------------------------------------
   * The following are the context in which commands are executed.
   *
   * While a command has the vty in its hands, it can access and change these
   * because they are not touched by the CLI thread until the command has
   * completed.
   */

  /* Node status of this vty.                                           */
  node_type_t   node ;

  /* For current referencing point of interface, route-map, access-list
   * etc...
   *
   * NB: this value is private to the command execution, which is assumed
   *     to all be in the one thread... so no lock required.
   */
  void* index ;

  /* For multiple level index treatment such as key chain and key.
   *
   * NB: this value is private to the command execution, which is assumed
   *     to all be in the one thread... so no lock required.
   */
  void* index_sub ;

  /* In configure mode (owner of the symbol of power)                   */
  bool  config;

  /*----------------------------------------------------------------------------
   * The current cmd_exec environment -- used in command_execute.c et al
   *
   * This is accessed freely by the command handling code while
   * vio->cmd_running.
   */
  struct cmd_exec* exec ;               /* one per vty          */

  /*----------------------------------------------------------------------
   * The following is used inside vty.c etc only -- under VTY_LOCK.
   */
  struct vty_io* vio ;                  /* one per vty          */
} ;

#endif /* _ZEBRA_VTY_COMMON_H */

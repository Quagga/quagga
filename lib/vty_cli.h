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

#include "misc.h"
#include "vargs.h"

#include "command_local.h"
#include "vty_io.h"
#include "vty_io_basic.h"
#include "vio_fifo.h"
#include "vio_lines.h"
#include "qstring.h"
#include "qtimers.h"
#include "keystroke.h"

/*------------------------------------------------------------------------------
 * States of the CLI and related output.
 */
typedef enum cli_state
{
  cst_active      = 0,
  cst_dispatched  = 1,
  cst_in_progress = 2,
  cst_complete    = 3,

} cli_state_t ;

typedef enum cli_out_state
{
  cos_idle        = 0,

  cos_active      = 1,
  cos_cancel      = 2,

  cos_more_enter  = 3,
  cos_more_wait   = 4,

  cos_mask        = BIT(4) - 1,

  cos_monitor     = BIT(6),
  cos_paused      = BIT(7),

} cli_out_state_t ;

/*------------------------------------------------------------------------------
 * The vty_cli structure pointed to by the vty_io structure.
 */
typedef struct vty_cli* vty_cli ;

struct vty_cli
{
  vio_vf        vf ;            /* parent                               */

  /* History of commands                                                */
  vector        hist ;          /* embedded                     */
  uint          hp ;            /* current place in history     */
  uint          h_now ;         /* the present moment           */
  bool          h_repeat ;      /* latest entry is repeat       */

  /* Window width/height as reported by Telnet.  0 => unknown           */
  int           width;
  int           height;

  /* Configure lines.                                                   */
  int           lines;
  bool          lines_set ;     /* true <=> explicitly set              */

  /* Terminal monitor.                                                  */
  bool          monitor ;
  bool          monitor_busy ;

  /* The incoming stuff                                                 */
  keystroke_stream key_stream ;

  /* drawn           <=> the current prompt and user input occupy the current
   *                     line on the screen.
   *
   *                     This flag <=> the CLI "owns" the screen.  This flag
   *                     must be cleared -- by wiping the command line -- before
   *                     any other output can use the screen.
   *
   *                     In particular, must be cleared before setting
   *                     cos_active -- see below.
   *
   * more_drawn      <=> what is drawn is the "--more--" prompt.
   *                  => drawn
   *
   * If drawn is true, the following are valid:
   *
   *   prompt_len    -- the length of the prompt part.
   *                    (will be the "--more--" prompt in cli_more_wait)
   *
   *   echo_suppress -- the user part of the command line is suppressed
   *
   * NB: echo_suppress is only used for password entry.
   */
  bool          drawn ;
  bool          more_drawn ;
  int           prompt_len ;

  /* "cache" for prompt -- when node or host name changes, prompt does  */
  node_type_t   prompt_node ;
  name_gen_t    prompt_gen ;
  qstring       prompt_for_node ;

  /* State of the CLI and its output
   */
  cli_state_t      state ;
  cli_out_state_t  out_state ;

  /* This is set only if the "--more--" handling is enabled             */
  bool          more_enabled ;

  /* Timer for paused state -- multi-threaded only                      */
  qtimer        pause_timer ;

  /* Command Line(s)
   *
   * context  -- the node etc. that the CLI is in.  This may be some way behind
   *             the VTY, but is updated when the CLI level command completes.
   *
   *             Note that this is a copy of the state of exec->context when
   *             uty_want_command() was last called.
   *
   * auth_context  -- true <=> context->node is AUTH_NODE or AUTH_ENABLE_NODE
   *
   * parsed   -- the parsed object used to parse command for cli help
   *
   * to_do    -- when current command being prepared is completed (by
   *             CR/LF or otherwise) this says what there now is to be done.
   *
   * cl       -- current command line being prepared.
   * cls      -- current command line on the screen
   *
   * clx      -- current command line being executed.
   *
   * dispatch -- the last action dispatched.
   *
   * NB: during command execution vty->buf is set to point at the '\0'
   *     terminated current command line being executed.
   */
  cmd_context   context ;
  bool          auth_context ;

  cmd_parsed    parsed ;

  cmd_do_t      to_do ;
  qstring       cl ;
  qstring       cls ;

  qstring       clx ;

  cmd_action_t  dispatch ;

  /* CLI line buffering and line control                                */
  vio_fifo          cbuf ;
  vio_line_control  olc ;
} ;

/*------------------------------------------------------------------------------
 * Functions
 */
extern vty_cli uty_cli_new(vio_vf vf) ;
extern vty_cli uty_cli_close(vty_cli cli, bool final) ;

extern void uty_cli_hist_show(vty_cli cli) ;
extern ulen uty_cli_prompt_len(vty_cli cli) ;

extern vty_readiness_t uty_cli(vty_cli cli) ;

extern cmd_return_code_t uty_cli_want_command(vty_cli cli, cmd_action action) ;
extern void uty_cli_out(vty_cli cli, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern void uty_cli_out_newline(vty_cli cli) ;
extern void uty_cli_write(vty_cli cli, const char *this, int len) ;
extern void uty_cli_wipe(vty_cli cli, int len) ;
extern void uty_cli_cancel(vty_cli cli, bool cntrl_c) ;

extern void uty_cli_set_lines(vty_cli cli, int lines, bool explicit) ;
extern void uty_cli_set_window(vty_cli cli, int width, int height) ;
extern void uty_cli_enter_more_wait(vty_cli cli) ;
extern void uty_cli_exit_more_wait(vty_cli cli) ;

extern void uty_cli_pre_monitor(vty_cli cli) ;
extern void uty_cli_post_monitor(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Pro tem -- "\r\n" string
 */
extern const char* uty_cli_newline ;

#endif /* _ZEBRA_VTY_CLI_H */

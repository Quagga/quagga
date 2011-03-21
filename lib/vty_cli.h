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

  /* Terminal timeout in seconds -- 0 => none                           */
  vty_timer_time v_timeout ;

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
   *                     out_active -- see below.
   *
   * dirty           <=> the last command output did not end with a newline.
   *
   * tilde_enabled  <=> do not do the "~ " one command line ahead.
   *
   * If drawn is true, the following are valid:
   *
   *   tilde_prompt  -- the prompt is the "~ "
   *
   *   prompt_len    -- the length of the prompt part.
   *                    (will be the "--more--" prompt in cli_more_wait)
   *
   *   extra_len     -- the length of any ^X at the cursor position
   *                    (for when blocked waiting for queued command)
   *
   *   echo_suppress -- the user part of the command line is suppressed
   *
   * NB: echo_suppress is only used for password entry.
   */
  bool          drawn ;
  bool          dirty ;

  bool          tilde_prompt ;
  bool          tilde_enabled ;

  int           prompt_len ;
  int           extra_len ;

  bool          echo_suppress ;

  /* "cache" for prompt -- when node or host name changes, prompt does  */
  node_type_t   prompt_node ;
  name_gen_t    prompt_gen ;
  qstring       prompt_for_node ;

  /* State of the CLI
   *
   *   dispatched   -- command dispatched by CLI
   *   in_progress  -- command taken by the command loop
   *   blocked      -- blocked until current command completes
   *   paused       -- command dispatched and nothing else happened
   *
   *   mon_active   -- there is stuff in the logging monitor buffer
   *
   *   out_active   -- contents of the obuf FIFO are being written away
   *                   though may be blocked in more_wait
   *
   *                   This flag <=> that the command output "owns" the screen.
   *
   *                   While this flag is set, the CLI may not write to the
   *                   screen.
   *
   *   flush        -- this flag => out_active.
   *
   *                   When the CLI is ready to read the next CLI command, it
   *                   must wait for all command output to complete.  This
   *                   flag is set, so that (a) any final but incomplete
   *                   line of command output will be flushed, and (b) to
   *                   signal that out_active must be cleared when all output
   *                   has completed.
   *
   *   more_wait    -- is in "--more--" wait state
   *   more_enter   -- more_wait and waiting for "--more--" prompt to be
   *                             written away and keystrokes to be consumed.
   */
  bool          dispatched ;
  bool          in_progress ;
  bool          blocked ;
  bool          paused ;

  bool          mon_active ;
  bool          out_active ;
  bool          flush ;

  bool          more_wait ;
  bool          more_enter ;

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

extern cmd_return_code_t uty_cli_want_command(vty_cli cli, cmd_action action,
                                                          cmd_context context) ;
extern void uty_cli_out(vty_cli cli, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern void uty_cli_out_newline(vty_cli cli) ;
extern void uty_cli_out_clear(vty_cli cli) ;
extern void uty_cli_write(vty_cli cli, const char *this, int len) ;
extern void uty_cli_wipe(vty_cli cli, int len) ;

extern void uty_cli_set_lines(vty_cli cli, int lines, bool explicit) ;
extern void uty_cli_set_window(vty_cli cli, int width, int height) ;
extern void uty_cli_enter_more_wait(vty_cli cli) ;
extern void uty_cli_exit_more_wait(vty_cli cli) ;

extern bool uty_cli_draw_if_required(vty_cli cli) ;

extern void uty_cli_pre_monitor(vty_cli cli) ;
extern void uty_cli_post_monitor(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Pro tem -- "\r\n" string
 */
extern const char* uty_cli_newline ;

#endif /* _ZEBRA_VTY_CLI_H */

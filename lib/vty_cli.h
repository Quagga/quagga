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
#include "keystroke.h"

/*------------------------------------------------------------------------------
 * The vty_cli structure pointed to by the vty_io structure.
 */
typedef struct vty_cli* vty_cli ;

struct vty_cli
{
  vio_vf        vf ;            /* parent                               */

  /* History of commands                                                */
  vector_t      hist ;          /* embedded                     */
  int           hp ;            /* current place in history     */
  int           h_now ;         /* the present moment           */

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

  /* drawn <=> the current prompt and user input occupy the current
   *           line on the screen.
   *
   * dirty <=> the last command output did not end with a newline.
   *
   * If drawn is true, the following are valid:
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

  int           prompt_len ;
  int           extra_len ;

  bool          echo_suppress ;

  /* "cache" for prompt -- when node or host name changes, prompt does  */
  node_type_t   prompt_node ;
  name_gen_t    prompt_gen ;
  qstring_t     prompt_for_node ;

  /* password failure count -- main login or enable login.              */
  int           password_fail ;

  /* State of the CLI
   *
   *   in_progress  -- command dispatched
   *   blocked      -- blocked until current command completes
   *   out_active   -- contents of the command FIFO are being written away
   *
   *   more_wait    -- is in "--more--" wait state
   *   more_active  -- more_wait and waiting for "--more--" prompt to be
   *                                 written away.
   */
  bool          in_progress ;
  bool          blocked ;
  bool          out_active ;

  bool          more_wait ;
  bool          more_active ;

  /* This is used to control command output, so that each write_ready event
   * generates at most one tranche of output.
   */
  bool          out_done ;

  /* This is set only if the "--more--" handling is enabled             */
  bool          more_enabled ;

  /* Command Line(s)
   *
   * node    -- the node that the CLI is in.  This may be some way behind
   *            the VTY, but is updated when the CLI level command completes.
   *
   * to_do   -- when current command being prepared is completed (by
   *            CR/LF or otherwise) this says what there now is to be done.
   *
   * cl      -- current command line being prepared.
   *
   * clx     -- current command line being executed.
   *
   * NB: during command execution vty->buf is set to point at the '\0'
   *     terminated current command line being executed.
   */
  node_type_t   node ;

  cmd_do_t      to_do ;

  qstring       cl ;
  qstring       clx ;

  cmd_parsed_t  parsed ;        /* embedded             */

  /* CLI line buffering                                                 */
  vio_fifo_t    cbuf ;          /* embedded             */

  /* CLI line control for command output & "--more--" stuff             */
  vio_line_control_t    olc ;   /* embedded             */
} ;

extern vty_cli uty_cli_new(vio_vf vf) ;
extern void uty_cli_start(vty_cli cli, node_type_t node) ;

extern vty_cli uty_cli_close(vty_cli cli, bool final) ;

extern cmd_return_code_t uty_cli_auth(vty_cli) ;
extern void uty_cli_hist_show(vty_cli cli) ;
extern ulen uty_cli_prompt_len(vty_cli cli) ;

extern vty_readiness_t uty_cli(vty_cli cli) ;
extern void uty_cli_out_push(vty_cli cli) ;
extern void uty_cli_done_command(vty_cli cli, node_type_t node) ;

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

extern void uty_cli_pre_monitor(vty_cli cli, size_t len) ;
extern int uty_cli_post_monitor(vty_cli cli, const char* buf, size_t len) ;

#endif /* _ZEBRA_VTY_CLI_H */

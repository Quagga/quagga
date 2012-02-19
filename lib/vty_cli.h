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
#include "vector.h"

/*------------------------------------------------------------------------------
 * The vty_cli structure pointed to by the vty_io structure.
 */
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
   *                     Will be true only when is vst_cmd_fetch, vst_cmd_dispatched
   *                     or vst_cmd_more.  Changes to the state must ensure that
   *                     the ownership of the current line is kept up to date.
   *
   * drawn_to_do     <=> what ^Z if any should be drawn after the command.
   *
   *                     Valid only if cli->state == vst_cmd_dispatched.
   */
  bool          drawn ;
  const char*   drawn_to_do ;

  /* State of the CLI and its output
   *
   * The state specifies whether a CLI or the output side has "ownership"
   * if the screen.  When the cli has ownership, the "drawn" states, above
   * specify whether the current prompt, command line etc. is drawn on the
   * screen.
   *
   * Note that if either vst_mon_active/vst_mon_blocked or vst_mon_paused is set,
   * then the screen belongs to the output side.
   */
//cli_state_t      state ;

  /* ready  <=> must call uty_cli() ASAP -- to redraw prompt, or fetch from
   *            keystroke buffer, etc.
   *
   *            The effect is to force write-ready in uty_term_set_readiness(),
   *            provided the cli is in a suitable state.
   */
  bool          ready ;

  /* This is set only if the "--more--" handling is enabled
   */
  bool          more_enabled ;

  /* Timer for paused state -- multi-threaded only
   */
  qtimer        pause_timer ;

  /* Command Line(s)
   *
   * auth_node -- true <=> current node is AUTH_NODE or AUTH_ENABLE_NODE
   *
   * help_parsed  -- parsed object used to parse command for cli help
   * help_context -- context object used during cli help
   *
   * to_do        -- used for current command line, when dispatched
   * to_do_next   -- used for "follow-on" cmd_do_ctrl_z
   *
   * cl           -- current command line being prepared/executed.
   * cls          -- current command line on the screen
   *
   * NB: during command execution vty->buf is set to point at the '\0'
   *     terminated current command line being executed.
   */
  bool          auth_node ;

  cmd_parsed    help_parsed ;
  cmd_context   help_context ;

  cmd_do_t      to_do ;
  cmd_do_t      to_do_next ;
  qstring       cl ;
  qstring       cls ;

  /* CLI line buffering and line control                                */
  vio_fifo          cbuf ;
  vio_line_control  olc ;
} ;

/*------------------------------------------------------------------------------
 * How active is the cli ?
 */
typedef enum cli_active
{
  cli_not_active,
  cli_standard,
  cli_more,

} cli_active_t;

/*------------------------------------------------------------------------------
 * Functions
 */
extern vty_cli uty_cli_new(vio_vf vf) ;
extern vty_cli uty_cli_free(vty_cli cli) ;
extern void uty_cli_close(vty_cli cli) ;

extern void uty_cli_hist_show(vty_cli cli) ;

extern void uty_cli(vty_cli cli) ;
extern cli_active_t uty_cli_is_active(vty_cli cli) ;

extern cmd_ret_t uty_cli_want_command(vio_vf vf);
extern void uty_cli_out(vty_cli cli, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern void uty_cli_out_newline(vty_cli cli) ;
extern void uty_cli_write(vty_cli cli, const char *this, ulen len) ;
extern void uty_cli_wipe(vty_cli cli) ;
extern bool uty_cli_out_cancel(vty_cli cli) ;

extern void uty_cli_set_lines(vty_cli cli, int lines, bool explicit) ;
extern void uty_cli_set_window(vty_cli cli, int width, int height) ;

extern vector uty_cli_make_describe_list(vector item_v, uint width) ;

/*------------------------------------------------------------------------------
 * Pro tem -- "\r\n" string
 */
extern const char* uty_cli_newline ;

#endif /* _ZEBRA_VTY_CLI_H */

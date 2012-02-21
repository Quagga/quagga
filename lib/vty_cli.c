/* VTY Command Line Handler
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
#include "misc.h"
#include <ctype.h>

#include "vty_local.h"
#include "vty_command.h"
#include "vty_cli.h"
#include "vty_io_term.h"
#include "vty_io.h"
#include "vio_lines.h"
#include "vio_fifo.h"
#include "log.h"

#include "keystroke.h"

#include "command_common.h"
#include "command_parse.h"
#include "command_execute.h"
#include "command_queue.h"

#include "memory.h"

/*==============================================================================
 * This is the CLI which is part of a VIN_TERM vf.
 *
 * With a few exceptions, the externs here are called by vty_io_term.
 */

enum { VTY_HIST_COUNT  = 99 } ;

/*------------------------------------------------------------------------------
 * Essential stuff.
 */
#define TELNET_NEWLINE "\r\n"
static const char* telnet_newline  = TELNET_NEWLINE ;
       const char* uty_cli_newline = TELNET_NEWLINE ;

/*------------------------------------------------------------------------------
 * The "--more--" prompt
 */
static const char* more_prompt = "--more--" ;

/*==============================================================================
 * Construct and destroy CLI object
 */

static bool uty_cli_callback(keystroke_callback_args) ;
static void uty_cli_update_more(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Construct and initialise a new CLI object -- never embedded.
 *
 * The CLI is started up in the state it is in waiting for a command to
 * complete.  This means that all start-up messages etc. are handled as the
 * output from an implied start command.  When the command loop is entered,
 * will find its way to uty_cli_want_command().
 *
 * Note that the vty_io_term stuff sends out a bunch of telnet commands, which
 * will be buffered in the CLI buffer.  That will be output, before the
 * start-up messages etc. on uty_cli_start().
 */
extern vty_cli
uty_cli_new(vio_vf vf)
{
  vty_cli cli ;

  cli = XCALLOC(MTYPE_VTY_CLI, sizeof(struct vty_cli)) ;

  /* Zeroising has initialised:
   *
   *   vf             = NULL           -- set below
   *
   *   hist           = X              -- set up below
   *   hp             = 0              -- hp == h_now => in the present ...
   *   h_now          = 0              --    ... see uty_cli_hist_add()
   *   h_repeat       = false          --    ... see uty_cli_hist_add()
   *
   *   width          = 0              -- unknown width  ) Telnet window size
   *   height         = 0              -- unknown height )
   *
   *   lines          = 0              -- set below
   *   lines_set      = false          -- set below
   *
   *   monitor        = false          -- not a "monitor"
   *   monitor_busy   = false          -- so not busy either
   *
   *   key_stream     = X              -- set below
   *
   *   drawn          = false
   *   drawn_to_do    = NULL
   *
   *   state          = X              -- set below
   *   ready          = false          -- no need to force write ready
   *
   *   more_enabled   = false          -- not in "--More--" state
   *
   *   pause_timer    = NULL           -- set below if multi-threaded
   *
   *   auth_node      = false          -- set by uty_cli_want_command()
   *
   *   help_parsed    = NULL           -- see below
   *   help_context   = NULL           -- see below
   *
   *   to_do          = cmd_do_nothing
   *   to_do_next     = cmd_do_nothing
   *   cl             = NULL qstring   -- set below
   *   cls            = NULL qstring   -- set below
   *
   *   cbuf           = NULL           -- see below
   *
   *   olc            = NULL           -- see below
   */
  confirm(NULL_NODE == 0) ;             /* node                 */
  confirm(cmd_do_nothing == 0) ;        /* to_do & to_do_next   */

  cli->vf = vf ;

  /* create empty cli->hist.
   */
  cli->hist = vector_init_new(cli->hist, VTY_HIST_COUNT) ;
  vector_set_min_length(cli->hist, VTY_HIST_COUNT) ;

  /* Allocate and initialise a keystroke stream     TODO: CSI ??
   */
  cli->key_stream = keystroke_stream_new('\0', "\x03", uty_cli_callback, cli) ;

  /* Set up cl and cls qstrings, the command line output fifo and the output
   * line control.
   */
  cli->cl   = qs_new(120) ;     /* reasonable line length       */
  cli->cls  = qs_new(120) ;

  cli->cbuf = vio_fifo_new(1000) ;

  cli->olc  = vio_lc_new(0 , 0, telnet_newline) ;

  /* Make an empty parsed object and context object
   */
  cli->help_parsed  = cmd_parsed_new() ;
  cli->help_context = cmd_context_new() ;

  /* Use global 'lines' setting, as default -- but 'lines_set' false.
   */
  uty_cli_set_lines(cli, host.lines, false) ;
  uty_cli_update_more(cli) ;    /* and update the olc etc to suit.      */

  /* Enable pause timer if multi-threaded
   */
  if (vty_nexus)
    cli->pause_timer = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                 vty_term_pause_timeout, cli) ;

  /* Ready to go: start as if a "start" command has been executed, which will
   *              generate some output.
   */
  cli->vf->vio->state = vst_cmd_running ;

  return cli ;
} ;

/*------------------------------------------------------------------------------
 * Free CLI object, if any, and all its contents.
 */
extern vty_cli
uty_cli_free(vty_cli cli)
{
  qstring line ;

  if (cli == NULL)
    return NULL ;

  while ((line = vector_ream(cli->hist, free_it)) != NULL)
    qs_reset(line, free_it) ;

  cli->hist = NULL ;
  cli->key_stream = keystroke_stream_free(cli->key_stream) ;

  cli->cl   = qs_free(cli->cl) ;
  cli->cls  = qs_free(cli->cls) ;

  cli->cbuf = vio_fifo_free(cli->cbuf) ;
  cli->olc  = vio_lc_free(cli->olc) ;

  cli->help_parsed  = cmd_parsed_free(cli->help_parsed) ;
  cli->help_context = cmd_context_free(cli->help_context) ;

  cli->pause_timer = qtimer_free(cli->pause_timer) ;

  XFREE(MTYPE_VTY_CLI, cli) ;               /* sets cli = NULL      */

  return cli ;
} ;

/*------------------------------------------------------------------------------
 * Close the CLI -- no further input is going to arrive, so stop anything to
 * do with interacting with the user.
 *
 * Expects to be called once only for the VTY_TERMINAL.
 *
 * There is nothing special for vst_final in this case.
 *
 * If is vst_cancel, then will eventually arrive at uty_cli_out_cancel() via
 * the hiatus... so we don't do that here.  If is not vst_cancel, then we do
 * the uty_cli_cancel() here, to tidy up the command line (if any) and to
 * ensure we exit vst_cmd_fetch/vst_cmd_dispatched/vst_cmd_more.
 */
extern void
uty_cli_close(vty_cli cli)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  qassert(cli->vf->vin_state & vf_cease) ;

  /* If is vst_cmd_fetch or vst_cmd_dispatched, cancel the current command line
   * -- drawing it if required -- and set vst_cmd_complete.
   *
   * If is vst_cmd_more, cancel the "--more--" -- drawing it if required -- and
   * set vst_cmd_running, again -- clearing vst_cmd_execute, which cannot be
   * the case !
   *
   * If is vst_cmd_complete, nothing more is required.
   *
   * The result is that we are then either vst_cmd_running or vst_cmd_complete,
   * and control is in the hands of the command loop and output side.
   *
   * Note that the output will be pushed later to bring the screen up to date.
   */
  if ((cli->vf->vio->state & (vst_cancel | vst_notify)) == 0)
    uty_cli_out_cancel(cli) ;

  /* Turn off "---more--" etc. so that output can proceed.
   */
  uty_cli_set_lines(cli, 0, true) ;     /* stop "--more--"      */

  return ;
} ;

/*------------------------------------------------------------------------------
 * The keystroke callback function.
 *
 * This deals with interrupts and IAC sequences that should be dealt with as
 * soon as they are read -- not stored in the keystroke stream for later
 * processing.
 *
 * Returns:  true <=> dealt with keystroke, do not store in keystroke stream
 */
static bool
uty_cli_callback(keystroke_callback_args /* void* context, keystroke stroke */)
{
  vty_cli cli   = context ;
  vty_io  vio ;

  vio = cli->vf->vio ;

  switch (stroke->type)
    {
      case ks_char:             /* character -- uint32_t                */
        /* We only intercept ^C.
         *
         * Whatever happens, a ^C discards everything that precedes it,
         * including any ^C accepted into the keystroke stream or stolen.
         */
        keystroke_stream_clear(cli->key_stream) ;

        /* If we are in the middle of processing a vst_cancel, then there
         * there is no point issuing another.
         *
         * If we are suspended, there is no point issuing a cancel at the
         * moment.
         *
         * In both cases, when (or if) processing continues, will re-enter the
         * cli.  So we leave the ^C for then.
         */
        if (vio->state & (vst_cancel | vst_notify | vst_suspended))
          return false ;

        /* If input has ceased, we are beyond caring.
         */
        if (cli->vf->vin_state & vf_cease)
          return true ;

        /* If is vst_cmd_running -- signal the cancel to the command loop and
         * output, and eat the ^C.
         *
         * Otherwise, will leave the ^C for cli to pick up.
         *
         * If is vst_cmd_dispatched, then we are part way between vst_cmd_fetch
         * and vst_cmd_running.  Because we clear everything out of the
         * keystroke stream, we can fall back to vst_cmd_fetch and the cli will
         * immediately find the ^C.
         *
         * If is vst_cmd_complete, then we are part way between vst_cmd_running
         * and vst_cmd_fetch.  Unless the input is closed, the ^C will be picked
         * up by the cli when it goes vst_cmd_fetch.  If the input is closed,
         * then the ^C is too late, and will get discarded -- in particular,
         * will not appear as if some output has been terminated early.
         */
        switch(vio->state & vst_cmd_mask)
          {
            case vst_cmd_fetch:
              break ;

            case vst_cmd_dispatched:
              vio->state = (vio->state & ~vst_cmd_mask) | vst_cmd_fetch ;
              break ;

            case vst_cmd_running:
            case vst_cmd_running_executing:
              uty_vio_exception(vio, vx_cancel) ;
              return true ;

            case vst_cmd_more:
            case vst_cmd_more_executing:
            case vst_cmd_complete:
              break ;

            default:
              qassert(false) ;
              break ;
          } ;

        return false ;

      case ks_iac:              /* Telnet command                       */
        return uty_telnet_command(cli->vf, stroke, true) ;

      case ks_esc:              /* ESC xx                               */
      case ks_csi:              /* ESC [ ... or CSI ...                 */
        zabort("invalid interrupt keystroke type") ;

      default:
        zabort("unknown keystroke type") ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set the cli->lines, explicitly or implicitly, and update the "--more--"
 * state to suit.
 *
 *   lines < 0 => use whatever telnet has said, or no "--more--"
 *   lines = 0 => no "--more--"
 *   lines > 0 => use
 */
extern void
uty_cli_set_lines(vty_cli cli, int lines, bool explicit)
{
  cli->lines     = lines ;
  cli->lines_set = explicit ;

  uty_cli_update_more(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Set the cli->lines, explicitly or implicitly, and update the "--more--"
 * state to suit.
 *
 *   lines < 0 => use whatever telnet has said, or no "--more--"
 *   lines = 0 => no "--more--"
 *   lines > 0 =>
 */
extern void
uty_cli_set_window(vty_cli cli, int width, int height)
{
  cli->width  = width ;
  cli->height = height ;

  uty_cli_update_more(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Update the "--more--" state, after changing one or more of:
 *
 *   cli->lines
 *   cli->lines_set
 *   cli->width
 *   cli->height
 *
 * Enable or disable "--more--" as required.
 *
 * Set the effective height for line control.  If "--more--" is disabled, then
 * a large height is set, which limits the amount of stuff output at a time.
 *
 * Sets the line control window width and height.
 *
 * NB: will disable "--more--" if the vin is not open or is ceased.
 */
static void
uty_cli_update_more(vty_cli cli)
{
  int height ;

  cli->more_enabled = false ;           /* default state        */

  height = 0 ;      /* default state: no "--more--"                 */

  if ((cli->width) != 0)
    {
      /* If window size is known, use lines or given height         */
      if (cli->lines >= 0)
        height = cli->lines ;
      else
        {
          /* Window height, leaving one line from previous "page"
           * and one line for the "--more--" -- if at all possible
           */
          height = cli->height - 2 ;
          if (height < 1)
            height = 1 ;
        } ;
    }
  else
    {
      /* If window size not known, use lines if that has been set
       * explicitly for this terminal.
       */
      if (cli->lines_set)
        height = cli->lines ;
    } ;

  if ((height > 0) && ((cli->vf->vin_state & (vf_open | vf_cease)) == vf_open))
    cli->more_enabled = true ;      /* have a defined height        */
  else
    height = 200 ;                  /* but no "--more--"            */

  vio_lc_set_window(cli->olc, cli->width, height) ;
} ;

/*==============================================================================
 * General mechanism for command execution.
 *
 * The CLI is driven by select/pselect.  Output is to various FIFOs, which may
 * be written directly or moved forward by select/pselect.
 *
 * The command loop calls uty_term_cmd_line_fetch() when it is ready for the
 * next command line.  That effectively passes control to the CLI until it
 * dispatches a command, which signals the command loop, which calls
 * uty_term_cmd_line_fetch() again, to pick up the dispatched command line.
 * Until that command has completed, and all its output (if any) has been
 * written away, the CLI is inactive.
 *
 * Signalling the command loop uses a message queue or (for legacy threads) the
 * event queue.  [If the event queue is too slow, a priority event queue will
 * have to be invented.]
 *
 * The CLI input and output interact quite strongly.  The basic vst_cmd_fetch,
 * vst_cmd_dispatched, vst_cmd_running, vst_cmd_more and vst_cmd_complete states reflect
 * the progress of gathering a command, dispatching and executing it,
 * outputting the results (possibly pausing with "--more--") and finally
 * having completed the process.
 *
 * On top of the basic vst_cmd_xxx states come the temporary state bits for log
 * monitor output -- vst_mon_active/vst_mon_blocked/vst_paused.  The key
 * interaction between the CLI and the monitor output is the "command line
 * drawn" state.  When monitor output is written to the screen, if the CLI has
 * drawn a command line on the screen, it is wiped, then when monitor output
 * completes, the command line is restored.
 *
 * State of the CLI -- under vst_cmd_mask:
 *
 *   vst_cmd_fetch    -- is busy drawing prompt, fetching input, processing
 *                       same, etc. -- preparing the next command.
 *
 *   vst_cmd_dispatched -- a command has been dispatched, and is waiting for
 *                       command loop to fetch it.
 *
 *                       uty_term_ready() will signal success to the command
 *                       loop, once any pending output has completed.
 *
 *                       Note that command line is drawn, and cursor is at
 *                       the end of the line.
 *
 *   vst_cmd_running  -- a command has been taken by the command loop, and is
 *                       still running -- or at least no further command has
 *                       been fetched by the command loop.
 *
 *                       The command may be an in_pipe, so there may be many
 *                       commands to be completed before the CLI level command
 *                       completes.
 *
 *   vst_cmd_more     -- during the output of results, is in the "--more--"
 *                       cli.
 *
 *                       Will return to vst_cmd_running -- possibly via cancel.
 *
 *   vst_cmd_complete -- this follows vst_cmd_running, and signals that the
 *                       last CLI level command has completed, and all output
 *                       has been written away.
 *
 *                       Is ready to go vst_cmd_fetch.
 *
 *                       Will end up vst_cmd_complete when the input is closed.
 *
 * Plus, the extra bit:
 *
 *   vst_cmd_execute  -- this is set when the command line is picked up, ie
 *                       when vst_cmd_running is set.
 *
 *                       This is cleared when a command finishes, and we are
 *                       back at the base vin level.  This means that once
 *                       all output has been written away, can move from
 *                       vst_cmd_running to vst_cmd_complete.
 *
 * Note well that when a CLI level command line is dispatched and picked up by
 * the command loop, it goes vst_cmd_running.  If the command line is a pipe in
 * or out operation, will not go to vst_cmd_complete until all pipes are
 * closed, and all output to the base vout has been written away.
 *
 * And those output states may be qualified by:
 *
 *   vst_mon_active  -- has log monitor stuff to output.
 *
 *                      This is set when monitor output is ready, and when it
 *                      is set any current command line is wiped.
 *
 *   vst_mon_blocked -- blocked while writing log monitor stuff.
 *
 *                      Will only appear with vst_mon_active.
 *
 *   vst_mon_paused  -- is pausing after monitor output, before continuing
 *                      to output stuff, or redraw CLI line -- in case more
 *                      monitor stuff rolls up.
 *
 *                      If running threaded, this is set and the associated
 *                      timer is started, when monitor output completes.
 *
 *                      NB: vst_mon_active/vst_mon_blocked and vst_mon_paused
 *                          are generally mutually exclusive.
 *
 *                      NB: vst_mon_active/vst_mon_blocked and vst_mon_paused
 *                          take priority over other output state.
 *
 * There are three output FIFOs:
 *
 *   1. for the CLI itself               -- uty_cli_out and friends
 *
 *   2. for monitor output, which is written/read under LOG_LOCK
 *
 *   3. for output generated by commands -- vty_out and friends.
 *
 * Plus the line control.  If there is anything pending to be written from
 * the line control, then that takes precedence.
 *
 * The CLI FIFO is emptied whenever possible, in preference to the other
 * FIFOs -- except for when is vst_mon_blocked, in which case the monitor
 * FIFO takes precedence.
 *
 * The monitor FIFO is emptied in preference to the command FIFO.
 *
 * The command FIFO is emptied while vst_cmd_running.  While a command is
 * executed all its output is collected in the command output buffer, to be
 * written away when the command completes, or if it pushes the output
 * explicitly.
 *
 * It is expected that each command's output will end with a newline.  However,
 * the line control imposes some discipline, and does not output incomplete
 * lines until the CLI level command completes.  This means that while a
 * command prompt/line is not drawn, the cursor is always at the start of a
 * blank line (unless a previous output operation is waiting to complete).
 *
 * Note that is read ready until the keystroke stream hits eof.  So any input
 * will be hoovered up as soon as it is available.  The CLI process is driven
 * mostly by write_ready, except for when all output is complete and the
 * input keystroke buffer has been emptied.
 *
 * Note the CLI dispatches blank or cancelled lines -- so does one command line
 * at a time, yielding the processor after each one.
 *
 *------------------------------------------------------------------------------
 * The "--more--" handling.
 *
 * The output process used the line_control structure to manage the output, and
 * occasionally enter the trivial "--more--" CLI.  This is invisible to the
 * main CLI.  See the vst_cmd_more handling.
 *
 * If the user decides to abandon output at the "--more--" prompt, then the
 * then contents of the command output FIFO are discarded.
 *
 *------------------------------------------------------------------------------
 * Command line drawn state.
 *
 * This is valid while is vst_cmd_fetch, vst_cmd_dispatched or vst_cmd_more.
 *
 * When the cli->drawn flag is set, the current console line contains the
 * current prompt and the user input to date.  The cursor is positioned where
 * the user last placed it -- or at the end when vst_cmd_dispatched.
 *
 * The command line can be "wiped" -- see uty_cli_wipe() -- which removes all
 * output and prompt, and leaves the console at the start of an empty line
 * where the command line used to be.  On entry to the CLI, it will draw the
 * command line again if it has been wiped.
 *
 * This is used for the command help/completion system and for the "monitor"
 * output.
 */

/*==============================================================================
 * The CLI
 */

#define CONTROL(X)  ((X) & 0x1F)

static bool uty_cli_standard(vty_cli cli) ;
static bool uty_cli_more(vty_cli cli) ;
static void uty_cli_draw(vty_cli cli, bool to_end) ;

/*------------------------------------------------------------------------------
 * CLI for VTY_TERMINAL
 *
 * Called by uty_term_ready(), so driven by read/write ready.
 *
 * Must be uty_cli_is_active() to get in !  Only if is active will cli->ready
 * be cleared -- so it is important not to act on cli->ready (and set
 * write-ready on the strength of it) *unless* uty_cli_is_active() !
 *
 * NB: the caller *must* attempt to flush the CLI buffer, and set write ready
 *     as required if cannot complete that.
 */
extern void
uty_cli(vty_cli cli)
{
  cli_active_t act ;

  VTY_ASSERT_LOCKED() ;

  switch (act = uty_cli_is_active(cli))
    {
      case cli_standard:
        cli->ready = uty_cli_standard(cli) ;
        break ;

      case cli_more:
        cli->ready = uty_cli_more(cli) ;
        break ;

      default:
        qassert(act == cli_not_active) ;
        break ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Is the cli active ?  That is to say, are we waiting for cli !
 *
 * If the cli is active, a call of uty_cli() will enter and run one of the cli
 * (and attend to the cli->ready flag).
 *
 * Called by uty_term_set_readiness() to establish whether should set
 * vin_waiting and whether to take notice of cli->ready.
 *
 * Returns:  cli_not_active
 *           cli_standard
 *           cli_more
 */
extern cli_active_t
uty_cli_is_active(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  if ((cli->vf->vin_state | cli->vf->vout_state) & (vf_cease | vf_cancel))
    return cli_not_active ;

  /* Note that, inter alia, vst_mon_active/vst_mon_blocked and
   * vst_mon_paused preclude activity in the CLI.
   */
  switch (cli->vf->vio->state)
    {
      case vst_cmd_fetch:
        return cli_standard ;

      case vst_cmd_dispatched:
        break ;

      case vst_cmd_running:
        break ;

      case vst_cmd_more:
      case vst_cmd_more_executing:
        return cli_more ;

      case vst_cmd_complete:
      default:
        break ;
    } ;

  return cli_not_active ;
} ;

/*==============================================================================
 * The Standard CLI
 */

static cmd_do_t uty_cli_process(vty_cli cli) ;
static cmd_do_t uty_cli_auth(vty_cli cli) ;
static void uty_cli_hist_add (vty_cli cli, qstring cl) ;
static void uty_cli_write_s(vty_cli cli, const char *str) ;

/*------------------------------------------------------------------------------
 * Standard CLI for VTY_TERM
 *
 * Entered only if vst_cmd_fetch.  Runs until:
 *
 *   * runs out of keystrokes, but not hit eof or time-out
 *
 *   * dispatches a command -- noting that eof and time-out are treated as
 *                             special commands, as are (eg) ^C and ^Z.
 *
 *   * something stops the CLI altogether
 *
 * Note that this executes at most one command each time it is called.  This
 * is to allow for a modicum of sharing of the system.  For real keyboard input
 * this will make no difference at all !
 *
 * NB: the caller *must* attempt to flush the CLI buffer, and set write ready
 *     as required if cannot complete that.
 *
 * Returns:  true  => set cli->ready
 *           false => clear cli->ready
 */
static bool
uty_cli_standard(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  qassert(cli->vf->vio->state == vst_cmd_fetch) ;

  /* Make sure that the command line is drawn.
   *
   * If there is no "follow-on" to_do, then run the required CLI.
   * Otherwise, adopt and clear the "follow-on" -- NB: command line empty !
   */
  if (!cli->drawn)
    uty_cli_draw(cli, false /* cursor unchanged */) ;

  if (cli->to_do_next == cmd_do_nothing)
    {
      cli->to_do = cli->auth_node ? uty_cli_auth(cli)
                                  : uty_cli_process(cli) ;

      if (cli->to_do == cmd_do_nothing)
        return false ;
    }
  else
    {
      cli->to_do      = cli->to_do_next ;
      cli->to_do_next = cmd_do_nothing ;

      qassert(cli->to_do == cmd_do_ctrl_z) ;    /* all this for ^Z :-(  */
    } ;

  /* Dispatch the "to_do".
   *
   *   * set vst_cmd_dispatched
   *
   *   * set history to the present
   *
   *   * uty_cli_auth() and uty_cli_progress() will have left the cursor at the
   *     end of the command line, with the screen up to date, and "drawn" is
   *     true.
   *
   *     Depending on the "to_do", complete the command by appending required
   *     marker, and note it in case need to redraw the dispatched line.
   *
   *     When the command line is picked up, then a newline is issued.
   *
   *     If the command is cancelled before it is picked up (!), then will
   *     return to the cli, vst_cmd_fetch, with the ^C in the keystroke stream.
   *
   *   * if is cli->auth_node, modify the to_do to cmd_do_auth version.
   *
   *   * for cmd_do_command and cmd_do_ctrl_z, update history as required.
   *
   *     And for cmd_do_ctrl_z set to_do_next as required.
   *
   *   * note that the command loop will be signalled at the end of
   *     uty_term_ready().
   */
  qassert(cli->drawn && (qs_cp_nn(cli->cl) == qs_len_nn(cli->cl))) ;

  cli->vf->vio->state = vst_cmd_dispatched ;

  static const char* cli_to_do_marker [cmd_do_count] =
  {
      [cmd_do_command]   = NULL,
      [cmd_do_ctrl_c]    = "^C",
      [cmd_do_ctrl_d]    = "^D",
      [cmd_do_ctrl_z]    = "^Z",
      [cmd_do_eof]       = "^*",
      [cmd_do_timed_out] = "^!",
  } ;

  cli->drawn_to_do = (cli->to_do < cmd_do_count) ? cli_to_do_marker[cli->to_do]
                                                 : NULL ;
  if (cli->drawn_to_do != NULL)
    uty_cli_write_s(cli, cli->drawn_to_do) ;

  if      (cli->auth_node)
    {
      cli->to_do |= cmd_do_auth ;
    }
  else if ((cli->to_do == cmd_do_command) || (cli->to_do == cmd_do_ctrl_z))
    {
      /* cmd_do_command => we have a simple command line on our hands.
       *
       * cmd_do_ctrl_z  => if command line is empty, do the ^Z action, now.
       *
       *                   if command line is not empty, treat it as a simple
       *                   command, and set a follow-on ^Z action.
       *
       * In both cases, if the command line is not empty, add to the history.
       */
      if (!cmd_is_empty(cli->cl))
        {
          if (cli->to_do == cmd_do_ctrl_z)
            {
              cli->to_do      = cmd_do_command ;
              cli->to_do_next = cmd_do_ctrl_z ;
            } ;

          uty_cli_hist_add(cli, cli->cl) ;
        } ;
    } ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Want another command line from the CLI -- called by command loop.
 *
 * Does something if is:
 *
 *   * vst_cmd_complete -- absolutely on its own, with no vst_hiatus_mask and
 *                                                     no vst_mon_mask bits.
 *
 *     This means that a previous command has completed, and all output has
 *     finished, so it is now time to go vst_cmd_fetch.
 *
 *     Returns CMD_WAITING.
 *
 *   * vst_cmd_dispatched -- also absolutely on its own
 *
 *     This means that the CLI has collected and dispatched a command, which
 *     can now be executed.
 *
 *     Puts out the newline to signal that the command has just been picked
 *     up, and sets vst_cmd_running | vst_cmd_executing.
 *
 *     Returns CMD_SUCCESS.
 *
 * Otherwise, does nothing and returns CMD_WAITING, because is:
 *
 *   * vst_cmd_fetch    -- currently fetching a command
 *
 * or returns CMD_HIATUS, because:
 *
 *   * vst_cmd_running           )
 *   * vst_cmd_running_executing ) -- previous command still running
 *   * vst_cmd_more              )    output side responsible for this.
 *   * vst_cmd_more_executing    )
 *
 *   * something in vst_hiatus_mask -- which the hiatus will take care of.
 *
 *     Note that should not have been called if this is the case -- tant pis.
 *
 *   * something in vst_mon_mask -- which the output side will take care of.
 *
 * Returns:  CMD_SUCCESS  -- has a command line, ready to go
 *           CMD_WAITING  -- for whatever reason, does not have a command line
 *           CMD_HIATUS   -- no command line, hiatus attention required
 *           CMD_IO_ERROR -- failed to set write-ready as required !
 *
 * NB: CMD_WAITING does *not* imply that the cli is currently active trying
 *     to read a command line.  May be waiting for output to complete, or
 *     anything else that must happen before can enter the cli !
 */
extern cmd_ret_t
uty_cli_want_command(vio_vf vf)
{
  vty_io       vio ;
  vty_cli      cli ;
  cmd_context  context ;
  cmd_ret_t    ret ;

  VTY_ASSERT_LOCKED() ;

  vio     = vf->vio ;
  cli     = vf->cli ;
  context = vf->context ;

  qassert(vio->vin_depth == 1) ;        /* so vst_cmd_xxx is ours !     */
  qassert((vio->state & vst_hiatus_mask) == 0) ;

  switch (vio->state)
    {
      case vst_cmd_fetch:
        ret = CMD_WAITING ;
        break ;

      case vst_cmd_complete:
        /* Ready to fetch another command line from the CLI.
         *
         * Sets "write ready" so that enters the CLI to output the prompt and
         * to process anything in the keystroke input buffer.
         *
         * Sets cli->ready, so that stays write ready until reaches the cli.
         */
        qs_clear(cli->cl) ;             /* finished with previous line  */

        cli->auth_node = false ;        /* by default                   */

        switch (context->node)
          {
            case NULL_NODE:             /* should not happen            */
            case EXIT_NODE:
            case META_NODE:
              uty_vf_read_stop(vf, vfs_stop_cease) ;
              ret = CMD_HIATUS ;        /* looks like eof               */
              break ;

            case AUTH_NODE:
            case AUTH_ENABLE_NODE:
              cli->auth_node = true ;
              fall_through ;

            default:
              vio->state = vst_cmd_fetch ;
              cli->ready = true ;       /* must re-enter uty_cli()      */

              ret = uty_term_set_readiness(vf, CMD_SUCCESS) ;

              if (ret == CMD_SUCCESS)
                ret = CMD_WAITING ;     /* OK, waiting for CLI          */

              break ;
          } ;
        break ;

      case vst_cmd_dispatched:
        /* New command has been dispatched -- can now pass that to the
         * command loop -- setting it in_progress.
         *
         * If the command line has been cleared by monitor output, restore
         * it.  Then issue newline to signal that command has been dispatched.
         *
         * Make sure the output buffers are reset as we dispatch the command.
         */
        if (!cli->drawn)
          uty_cli_draw(cli, false /* cursor unchanged */) ;
        uty_cli_out_newline(cli) ;      /* clears "drawn"               */

        vio_fifo_clear(vf->obuf) ;      /* make sure                    */
        vio_lc_clear(cli->olc) ;

        vio->state = vst_cmd_running | vst_cmd_executing ;

        qassert(cli->to_do != cmd_do_nothing) ;

        context->to_do = cli->to_do ;
        context->line  = cli->cl ;

        ret = uty_term_set_readiness(vf, CMD_WAITING) ;

        if (ret == CMD_WAITING)
          ret = CMD_SUCCESS ;           /* OK, have command line        */

        break ;

      /* We are waiting either for input or output before can do anything here.
       */
      default:
        ret = CMD_HIATUS ;
        break ;
    } ;

  return ret ;
} ;

/*==============================================================================
 * The "--more--" CLI
 *
 * When the output side finds that "--more--" is required, it changes from
 * vst_cmd_running to vst_cmd_more, and the uty_cli_more() cli will be entered.
 */

/*------------------------------------------------------------------------------
 * Handle the "--more--" state.
 *
 * Entered only if vst_cmd_more.  So does not get here if vst_mon_active/
 * vst_mon_blocked or vst_mon_paused are set.
 *
 * First, need to make sure the "--more--" prompt is written.  If not, suck up
 * any keystrokes, draw the prompt as required and then exit, cli->ready, to
 * allow the output side to write away the prompt.
 *
 * Note that log monitor output can interrupt the "--more--" cli, in which
 * case it will undraw the prompt.  When log monitor output is done, will
 * return here, and start the process again.
 *
 * We try not to steal a keystroke before the prompt has been written.  Hence
 * sucking up keystrokes before drawing the prompt.  The reading of keystrokes
 * is entirely automatic, under pselect(), but we here make completely sure
 * that we have read everything available, before starting to steal.
 *
 * If the log monitor interrupts the process, the first keystroke after the
 * "--more--" prompt is fully written is the one which will count.
 *
 * If the "--more--" prompt is written, try to steal a keystroke.  When
 * collects a stolen keystroke, will either wipe the "--more--" prompt and
 * return to vst_cmd_running, or signal a CMD_CANCEL.
 *
 * EOF on input causes immediate exit from "--more--", without cancelling
 * output.
 *
 * Returns:  true  => set cli->ready
 *           false => clear cli->ready
 */
static bool
uty_cli_more(vty_cli cli)
{
  keystroke_t steal ;
  bool  cancel, stolen ;

  VTY_ASSERT_LOCKED() ;

  qassert( (cli->vf->vio->state == vst_cmd_more)
        || (cli->vf->vio->state == vst_cmd_more_executing) ) ;

  /* Deal with the first stage of "--more--"
   *
   * If the "--more--" is not drawn, draw it.  If this or a previous output
   * of the prompt has not finished exit with cli->ready.
   *
   * We set cli->ready so that returns here as soon as all output has been
   * written away, so can start the process of stealing a keystoke.
   */
  if (!cli->drawn)
    uty_cli_draw(cli, false /* cursor unchanged */) ;

  if (!vio_fifo_is_empty(cli->cbuf))
    return true ;               /* must come back as soon as
                                 * the prompt is written away   */

  /* Now that the prompt has definitely been written away...
   *
   * ...if we have not yet started to steal a keystroke, make sure that any
   * pending input has been hoovered up.  Probably redundant -- but tidy.
   */
  if (!keystroke_steal_state(cli->key_stream))
    uty_term_read(cli->vf) ;    /* hoover up.                   */

  /* Try to get a stolen keystroke.  If the keystroke stream has hit
   * EOF (for any reason, including error or timed-out), will get a ks_null
   * stolen keystroke.
   *
   * If nothing to be stolen exit.
   */
  stolen = keystroke_steal(cli->key_stream, steal) ;

  if (!stolen)
    return false ;              /* waiting for keystroke        */

  /* Something has been stolen, so exit "--more--" state, and continue
   * or cancel.
   */
  cancel = false ;
  switch(steal->type)
    {
      case ks_null:             /* at EOF for whatever reason   */
        cancel = true ;
        break ;

      case ks_char:
        switch (steal->value)
          {
            case CONTROL('C'):
            case CONTROL('Z'):
            case 'q':
            case 'Q':
              cancel = true ;
              break;

            default:
              break ;
          } ;
        break ;

      case ks_esc:
        cancel = steal->value == '\x1B' ;
        break ;

      default:
        break ;
    } ;

  /* End of "--more--" process
   *
   * If cancelling signal the command loop et al.  The hiatus will deal with
   * the cancel, issue the ^C and revert to vst_cmd_running on the way to
   * vst_cmd_complete.
   *
   * If not cancelling, wipe out the prompt and revert to vst_cmd_running,
   * to continue output.
   */
  if (cancel)
    uty_vio_exception(cli->vf->vio, vx_cancel) ;
  else
    {
      uty_cli_wipe(cli) ;               /* clears cli->drawn    */
      vio_lc_counter_reset(cli->olc) ;

      cli->vf->vio->state = vst_cmd_running ;    /* revert to            */
    } ;

  return false ;                /* all done                     */
} ;

/*==============================================================================
 * CLI VTY output
 *
 * This is buffered separately from the general (command) VTY output above.
 *
 * Has a dedicated buffer in the struct vty_cli, which is flushed regularly
 * during command processing.
 *
 * It is expected that can flush straight to the file, since this is running at
 * CLI speed.  However, if the CLI is being driven by something other than a
 * keyboard then may need to have intermediate buffering.
 *
 * No actual I/O takes place here-- all "output" is to cli->cbuf
 */
enum { cli_rep_count = 32 } ;

typedef const char cli_rep_char[cli_rep_count] ;
typedef const char cli_rep[] ;

static cli_rep telnet_backspaces =
                          { 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08
                          } ;
CONFIRM(sizeof(telnet_backspaces) == sizeof(cli_rep_char)) ;

/*                              12345678901234567890123456789012        */
static cli_rep telnet_spaces = "                                " ;
static cli_rep telnet_dots   = "................................" ;
static cli_rep telnet_stars  = "********************************" ;

CONFIRM(sizeof(telnet_spaces)  == (sizeof(cli_rep_char) + 1)) ;
CONFIRM(sizeof(telnet_dots)    == (sizeof(cli_rep_char) + 1)) ;
CONFIRM(sizeof(telnet_stars)   == (sizeof(cli_rep_char) + 1)) ;

static void uty_cli_write_n(vty_cli cli, cli_rep_char chars, ulen n) ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf fprintf()
 *
 * NB: caller is responsible for issuing "\n\r" when newline is required.
 */
extern void
uty_cli_out(vty_cli cli, const char *format, ...)
{
  va_list args ;

  VTY_ASSERT_LOCKED() ;

  va_start (args, format);
  vio_fifo_vprintf(cli->cbuf, format, args) ;
  va_end(args);
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf write()
 *
 * NB: caller is responsible for issuing "\n\r" when newline is required.
 */
extern void
uty_cli_write(vty_cli cli, const char *this, ulen len)
{
  vio_fifo_put_bytes(cli->cbuf, this, len) ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- write 'n' characters using a cli_rep_char string
 */
static void
uty_cli_write_n(vty_cli cli, cli_rep_char chars, ulen n)
{
  ulen len ;

  len = sizeof(cli_rep_char) ;
  while (n > 0)
    {
      if (n < len)
        len = n ;
      uty_cli_write(cli, chars, len) ;
      n -= len ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- write given string
 *
 * NB: caller is responsible for issuing "\n\r" when newline is required.
 */
static void
uty_cli_write_s(vty_cli cli, const char *str)
{
  uty_cli_write(cli, str, strlen(str)) ;
} ;

/*==============================================================================
 * Prompts and responses
 */

/*------------------------------------------------------------------------------
 * Send newline to the console -- clears the cli_drawn flag.
 */
extern void
uty_cli_out_newline(vty_cli cli)
{
  uty_cli_write(cli, telnet_newline, 2) ;

  cli->drawn = false ;
} ;

/*------------------------------------------------------------------------------
 * If is vst_cmd_fetch, vst_cmd_dispatched or vst_cmd_more, then want to show that has
 * been cancelled.
 *
 * If "draw", make sure the command line or --more-- prompt is drawn and add
 * " ^C\n" to show cancelled.
 *
 * If is already drawn, make sure is at end of line and then add the " ^C\n".
 *
 * If was vst_cmd_complete, we know there is nothing to be cancelled -- all output
 * has completed and no new command line has been started -- so is already
 * "done".
 *
 * In any event:  if was vst_cmd_fetch or vst_cmd_dispatched, is now vst_cmd_complete.
 *
 *                if was vst_cmd_more, is now vst_cmd_running.
 *
 *                cli->drawn is false.
 *
 * So, when returns, is either vst_cmd_complete or vst_cmd_running.
 *
 * NB: ignores vst_mon_active/vst_mon_blocked and vst_mon_paused.
 *
 *     Those should not be set if we are here, however it is not a disaster if
 *     they are.  Setting vst_mon_active wipes the command line.  What this is
 *     about to do is redraw the command line, and promptly output terminating
 *     "\n", and clear cli->drawn.  The result will be output the first time
 *     uty_term_write() is called, after any vst_mon_blocked state is cleared.
 *     That may be earlier than vst_mon_paused would otherwise allow, but that is
 *     not a problem.
 *
 * Returns:  true <=> done, ie either: issued " ^C\n"
 *                                 or: was vst_cmd_complete, trivially done.
 */
extern bool
uty_cli_out_cancel(vty_cli cli)
{
  vst_state_t state ;
  bool done ;

  state = cli->vf->vio->state & vst_cmd_mask ;

  done = (state == vst_cmd_complete) ;

  if ( (state == vst_cmd_fetch) || (state == vst_cmd_dispatched)
                                || (state == vst_cmd_more)
                                || (state == vst_cmd_more_executing))
    {
      /* If not drawn -- draw it now, with cursor at end of line.
       *
       * If drawn     -- move cursor to end of line, if required.
       */
      uty_cli_draw(cli, true /* cursor to end of line */) ;

      /* Issue the ^C and now the line is no longer drawn !
       */
      uty_cli_write_s(cli, "^C" TELNET_NEWLINE) ;

      /* Finally:
       *
       *    if was vst_cmd_fetch or vst_cmd_dispatched, then current command line is
       *    done with, so is vst_cmd_complete.  If the vin is still vf_open, the
       *    next time that calls uty_cli_want_command(), will re-enter the
       *    command loop, vst_cmd_fetch.
       *
       *    if was vst_cmd_more then is now vst_cmd_running, and is no longer
       *    interested in any stealing of keystrokes.
       */
      switch (state)
        {
          case vst_cmd_fetch:
          case vst_cmd_dispatched:
            state = vst_cmd_complete ;
            break ;

          case vst_cmd_more:
          case vst_cmd_more_executing:
            keystroke_steal_clear(cli->key_stream) ;
            state = vst_cmd_running | (state & vst_cmd_executing);
            break ;

          default:
            assert(false) ;
        } ;

      cli->vf->vio->state = (cli->vf->vio->state & ~vst_cmd_mask) | state ;

      done = true ;
    } ;

  qassert((state == vst_cmd_running) || (state == vst_cmd_running_executing)
                                     || (state == vst_cmd_complete)) ;

  cli->drawn = false ;
  return done ;
} ;

/*------------------------------------------------------------------------------
 * Wipe 'n' characters.
 *
 * If 'n' < 0, wipes characters backwards and moves cursor back.
 *    'n' > 0, wipes characters forwards, leaving cursor where it is
 */
static void
uty_cli_out_wipe_n(vty_cli cli, int n)
{
  if (n < 0)
    {
      n = abs(n) ;
      uty_cli_write_n(cli, telnet_backspaces, n);
    } ;

  if (n > 0)
    {
      uty_cli_write_n(cli, telnet_spaces,     n) ;
      uty_cli_write_n(cli, telnet_backspaces, n) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Wipe the current console line -- if any.
 *
 * Does nothing unless is cst_idle, vst_cmd_dispatched or vst_cmd_more.
 * Does nothing if not drawn.
 *
 * Note: ignores any vst_mon_active etc.  While any of that is set, should not
 *       be cli->drawn -- but if is, best wiped !
 */
extern void
uty_cli_wipe(vty_cli cli)
{
  ulen b ;              /* characters before the cursor */

  if (!cli->drawn)
    return ;

  switch(cli->vf->vio->state & vst_cmd_mask)
    {
      case vst_cmd_fetch:
        /* Wipe anything ahead of the current cursor
         */
        uty_cli_out_wipe_n(cli, (int)(qs_len_nn(cli->cl) - qs_cp_nn(cli->cl))) ;

        b = cli->vf->vio->prompt_len + qs_cp_nn(cli->cl) ;
        break ;

      case vst_cmd_dispatched:
        /* Implicitly at end of line, after any to_do marker
         */
        b = cli->vf->vio->prompt_len + qs_len_nn(cli->cl) ;

        if (cli->drawn_to_do != NULL)
          b += strlen(cli->drawn_to_do) ;

        break ;

      case vst_cmd_more:
      case vst_cmd_more_executing:
        /* implicitly at end of line
         */
        b = strlen(more_prompt) ;
        break ;

      case vst_cmd_running:
      case vst_cmd_complete:
      default:
        qassert(!cli->drawn) ;
        b = 0 ;
        break ;
    } ;

  uty_cli_out_wipe_n(cli, -(int)b) ;
  uty_cli_write_n(cli, telnet_backspaces, b) ;

  cli->drawn = false ;
} ;

/*------------------------------------------------------------------------------
 * Draw prompt and entire command line, leaving current position where it
 * should be.
 *
 * Must be: vst_cmd_fetch, vst_cmd_dispatched or vst_cmd_more, ignoring vst_mon_active etc.
 *
 * Should not be vst_mon_active etc unless is "to_end".  If is "to_end", then
 * that implies is about to output "\n" and clear cli->drawn -- which is fine.
 * The issue here is that vst_mon_active etc. wipe the command line when
 * vst_mon_active is set, and expect it to stay that way.  If it doesn't,
 * the screen will be messed up.
 *
 * If not already drawn, assumes is positioned at start of an empty line, and
 * draws as required -- leaving cursor at the end if "to_end" or vst_cmd_dispatched
 * or vst_cmd_more, otherwise (vst_cmd_fetch not "to_end") leave cursor at the cursor
 * position.
 *
 * If is already drawn, will be at the end if vst_cmd_dispatched or vst_cmd_more.  If
 * "to_end", moves cursor as required if vst_cmd_fetch.
 */
static void
uty_cli_draw(vty_cli cli, bool to_end)
{
  vty_io vio ;
  vst_state_t state ;
  const char* prompt ;
  ulen   p_len ;

  vio = cli->vf->vio ;

  state = vio->state & vst_cmd_mask ;

  qassert( (state == vst_cmd_fetch) || (state == vst_cmd_dispatched)
                                    || (state == vst_cmd_more)
                                    || (state == vst_cmd_more_executing)) ;

  if (state == vst_cmd_dispatched)
    /* Will be at the end of the line, and after any to_do marker       */
    qassert(qs_cp_nn(cli->cl) == qs_len_nn(cli->cl)) ;

  if ( (state != vst_cmd_fetch) && (state != vst_cmd_dispatched)
                                && (state != vst_cmd_more)
                                && (state != vst_cmd_more_executing) )
    {
      cli->drawn = false ;      /* cannot be drawn      */
      return ;
    } ;

  if (!to_end)
    /* Should not be trying to draw if vst_mon_active etc.              */
    qassert(state == vio->state) ;

  /* If is already drawn, then if "to_end" move to the end, if required.
   */
  if (cli->drawn)
    {
      /* Should not be drawn if is vst_mon_active etc.
       */
      qassert((vio->state & vst_mon_mask) == 0) ;

      /* If is vst_cmd_fetch, make sure we are at the end of the current line.
       *
       * If is vst_cmd_dispatched, will already be at the end of the line, and
       * after any "^Z" or other.
       *
       * If is vst_cmd_more, is always at the end of the line.
       */
      if (to_end && (state == vst_cmd_fetch))
        uty_cli_write(cli, qs_cp_char_nn(cli->cl), qs_after_cp_nn(cli->cl)) ;

      return ;
    } ;

  /* Sort out the prompt and draw it.
   *
   * Note: vio->prompt_len is the length of the prompt written for the current
   *       command line -- is used when showing parsing and such like issues.
   *       So we clear this if writing a vst_cmd_more prompt.
   */
  if ((state == vst_cmd_more) || (state == vst_cmd_more_executing))
    {
      prompt  = more_prompt ;
      p_len   = strlen(prompt) ;

      vio->prompt_len = 0 ;
    }
  else
    {
      prompt  = uty_cmd_prompt(vio, cli->vf->context->node) ;
      p_len   = qs_len_nn(vio->prompt) ;

      vio->prompt_len = p_len ;
    } ;

  uty_cli_write(cli, prompt, p_len) ;

  /* If there is a command line, draw that, and for vst_cmd_dispatched draw any
   * "to_do" marker.
   *
   * If "to_end", and vst_cmd_fetch, forces cursor to the end of the line.
   *
   * Otherwise, leaves where it is.  vst_cmd_dispatched and vst_cmd_more will have the
   * cursor at the end already.
   */
  if ((state == vst_cmd_fetch) || (state == vst_cmd_dispatched))
    {
      ulen   l_len ;

      l_len = qs_len_nn(cli->cl) ;

      if (l_len != 0)
        {
          if (cli->auth_node)
            uty_cli_write_n(cli, telnet_stars, l_len) ;
          else
            uty_cli_write(cli, qs_char(cli->cl), l_len) ;
        } ;

      if (state == vst_cmd_dispatched)
        {
          if (cli->drawn_to_do != NULL)
            uty_cli_write_s(cli, cli->drawn_to_do) ;
        }
      else if (to_end)
        {
          qs_set_cp_nn(cli->cl, l_len) ;
        }
      else
        {
          ulen cp = qs_cp_nn(cli->cl) ;

          if (cp < l_len)
            uty_cli_write_n(cli, telnet_backspaces, l_len - cp) ;
        } ;
    } ;

  cli->drawn = true ;
} ;

/*==============================================================================
 * Command line processing loop
 */
static cmd_do_t uty_cli_get_keystroke(vty_cli cli, keystroke stroke) ;
static void uty_cli_update_line(vty_cli cli, uint rc) ;

static void uty_cli_insert (qstring cl, const char* chars, int n) ;
static int  uty_cli_forwards(qstring cl, int n) ;
static int  uty_cli_backwards(qstring cl, int n) ;
static void uty_cli_del_forwards(qstring cl, int n) ;
static void uty_cli_del_backwards(qstring cl, int n) ;
static void uty_cli_bol(qstring cl) ;
static void uty_cli_eol(qstring cl) ;
static int  uty_cli_word_forwards_delta(qstring cl) ;
static void uty_cli_word_forwards(qstring cl) ;
static int  uty_cli_word_backwards_delta(qstring cl) ;
static void uty_cli_word_backwards(qstring cl) ;
static void uty_cli_del_word_forwards(qstring cl) ;
static void uty_cli_del_word_backwards(qstring cl) ;
static void uty_cli_del_to_eol(qstring cl) ;
static void uty_cli_clear_line(qstring cl) ;
static void uty_cli_transpose_chars(qstring cl) ;
static void uty_cli_complete_command (vty_cli cli) ;
static void uty_cli_describe_command (vty_cli cli) ;

enum hist_step
{
  hist_previous  = -1,
  hist_next      = +1,
};

static void uty_cli_hist_use(vty_cli cli, enum hist_step) ;

/*------------------------------------------------------------------------------
 * Process keystrokes until run out of input, or get something to cmd_do.
 *
 * Requires the command line to have been drawn, so that what is in cli->cl
 * is what is on the screen.
 *
 * Process keystrokes until run out of stuff to do, or have a "command line"
 * that must now be executed.
 *
 * Updates cli->cl and makes sure screen reflects its contents when returns.
 *
 * Returns: cmd_do_nothing -- command line not complete, yet.
 *
 *          cmd_do_xxx     -- something to do.
 *
 *                            NB: cursor is at the end of the cli->cl.
 *
 * Note that will return cmd_do_eof or cmd_do_timed_out any number of times.
 */
static cmd_do_t
uty_cli_process(vty_cli cli)
{
  keystroke_t stroke ;
  uint8_t  u ;
  cmd_do_t to_do ;

  qassert(cli->drawn) ;

  qs_copy(cli->cls, cli->cl) ;          /* current screen line          */

  /* Now process as much as possible of what there is
   */
  do
    {
      to_do = uty_cli_get_keystroke(cli, stroke) ;

      if (to_do != cmd_do_keystroke)
        break ;

      switch (stroke->type)
      {
        /* Straightforward character -----------------------------------*/
        /* Note: only interested in 8-bit characters !                  */
        case ks_char:
          u = (uint8_t)stroke->value ;

          switch (stroke->value)
          {
            case CONTROL('A'):
              uty_cli_bol(cli->cl) ;
              break;

            case CONTROL('B'):
              uty_cli_backwards(cli->cl, 1);
              break;

            case CONTROL('C'):
              to_do = cmd_do_ctrl_c ;   /* Exit on ^C ..................*/
              break ;

            case CONTROL('D'):
              uty_cli_del_forwards(cli->cl, 1);
              break;

            case CONTROL('E'):
              uty_cli_eol(cli->cl);
              break;

            case CONTROL('F'):
              uty_cli_forwards(cli->cl, 1);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_backwards(cli->cl, 1);
              break;

            case CONTROL('K'):
              uty_cli_del_to_eol(cli->cl);
              break;

            case CONTROL('N'):
              uty_cli_hist_use(cli, hist_next) ;
              break;

            case CONTROL('P'):
              uty_cli_hist_use(cli, hist_previous) ;
              break;

            case CONTROL('T'):
              uty_cli_transpose_chars(cli->cl) ;
              break;

            case CONTROL('U'):
              uty_cli_clear_line(cli->cl) ;
              break;

            case CONTROL('W'):
              uty_cli_del_word_backwards(cli->cl) ;
              break;

            case CONTROL('Z'):
              to_do = cmd_do_ctrl_z ;   /* Exit on ^Z ..................*/
              break;

            case '\n':
            case '\r':
              to_do = cmd_do_command ;  /* Exit on CR or LF.............*/
              break ;

            case '\t':
              if (cmd_token_position(cli->help_parsed, cli->cl))
                uty_cli_insert (cli->cl, " ", 1) ;
              else
                uty_cli_complete_command(cli);
              break;

            case '?':
              if (cmd_token_position(cli->help_parsed, cli->cl))
                uty_cli_insert(cli->cl, (char*)&u, 1) ;
              else
                uty_cli_describe_command(cli);
              break;

            default:
              if ((stroke->value >= 0x20) && (stroke->value < 0x7F))
                uty_cli_insert(cli->cl, (char*)&u, 1) ;
              break;
            }
          break ;

        /* ESC X -------------------------------------------------------------*/
        case ks_esc:
          switch (stroke->value)
          {
            case 'b':
              uty_cli_word_backwards(cli->cl);
              break;

            case 'f':
              uty_cli_word_forwards(cli->cl);
              break;

            case 'd':
              uty_cli_del_word_forwards(cli->cl);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_word_backwards(cli->cl);
              break;

            default:
              break;
          } ;
          break ;

        /* ESC [ ... ---------------------------------------------------------*/
        case ks_csi:
          if (stroke->len == 0)
            {
              /* ESC [ X                                        */
              switch (stroke->value)
                {
                  case ('A'):           /* up arrow     */
                    uty_cli_hist_use(cli, hist_previous) ;
                    break;

                  case ('B'):           /* down arrow   */
                    uty_cli_hist_use(cli, hist_next) ;
                    break;

                  case ('C'):           /* right arrow  */
                    uty_cli_forwards(cli->cl, 1) ;
                    break;

                  case ('D'):           /* left arrow   */
                    uty_cli_backwards(cli->cl, 1) ;
                    break;

                  default:
                    break ;
                } ;
            }
          else if (stroke->len == 1)
            {
              /* ESC [ 3 ~  only !                              */
              if ((stroke->value == '~') && (stroke->buf[0] == '3'))
                uty_cli_del_forwards(cli->cl, 1) ;
            } ;
          break ;

        /* Unknown -----------------------------------------------------------*/
        default:
          zabort("unknown keystroke type") ;
      } ;

    }
  while (to_do == cmd_do_keystroke) ;

  /* Tidy up and return where got to.
   */
  if (to_do != cmd_do_nothing)
    uty_cli_eol(cli->cl) ;              /* go to the end of the line    */

  uty_cli_update_line(cli, qs_cp_nn(cli->cl)) ;

  return to_do ;
} ;

/*------------------------------------------------------------------------------
 * Update the command line to reflect the difference between old line and the
 * new line.
 *
 *   cli->cls  is the old line (currently on screen)
 *   cli->cl   is the new line (possible changed in some way)
 *
 * Leave the screen cursor at the given required cursor position.
 */
static void
uty_cli_update_line(vty_cli cli, uint rc)
{
  const char* np ;      /* new line             */
  const char* sp ;      /* screen line          */

  ulen  nl ;            /* new length           */
  ulen  sl ;            /* screen length        */

  ulen  sc ;            /* screen cursor        */

  ulen s, l ;

  assert(cli->drawn) ;

  np = qs_char_nn(cli->cl) ;
  nl = qs_len_nn(cli->cl) ;

  sp = qs_char_nn(cli->cls) ;
  sl = qs_len_nn(cli->cls) ;
  sc = qs_cp_nn(cli->cls) ;

  /* Find how many characters are the same.                             */
  l = (nl <= sl) ? nl : sl ;
  s = 0 ;
  while ((s < l) && (*(np + s) == *(sp + s)))
    ++s ;

  /* If the screen and new are different lengths, or the strings are not
   * the same, then need to draw stuff to correct what is on the screen.
   * That will leave the screen cursor at the end of the new line, after
   * any spaces required to wipe out excess characters.
   *
   * Note that sc is the current cursor position on the screen, and we keep
   * that up to date as we draw stuff.
   */
  if ((nl != sl) || (s != nl))
    {
      /* Move back if the screen cursor is beyond the same section      */
      if (sc > s)
        {
          uty_cli_write_n(cli, telnet_backspaces, sc - s) ;
          sc = s ;
        } ;

      /* Write from cursor to the end of the new line.                  */
      uty_cli_write(cli, np + sc, nl - sc) ;
      sc = nl ;

      /* If the old line was longer, need to wipe out old stuff         */
      if (sl > nl)
        {
          uty_cli_write_n(cli, telnet_spaces, sl - nl) ;
          sc = sl ;
        } ;
    } ;

  /* Now move cursor to the required cursor position                    */
  if      (sc > rc)
    uty_cli_write_n(cli, telnet_backspaces, sc - rc) ;
  else if (sc < rc)             /* => lines unchanged, but cursor moved */
    uty_cli_write(cli, np + sc, rc - sc) ;
} ;

/*------------------------------------------------------------------------------
 * For password: process keystrokes until run out of input, or get something
 *               to cmd_do.
 *
 * Similar to uty_cli_auth, except accepts a limited number of keystrokes.
 *
 * Does not accept cursor moves, so does not do forwards delete, and ^D means
 * exit.
 *
 * Returns: cmd_do_xxxx
 *
 * Note that will return cmd_do_eof or cmd_do_timed_out any number of times.
 */
static cmd_do_t
uty_cli_auth(vty_cli cli)
{
  keystroke_t stroke ;
  uint8_t  u ;
  cmd_do_t to_do ;
  int     olen, nlen ;

  /* For auth command lines, cursor is always at the end                */
  assert(qs_cp_nn(cli->cl) == qs_len_nn(cli->cl)) ;

  olen = qs_len_nn(cli->cl) ;

  /* Now process as much as possible of what there is                   */
  do
    {
      to_do = uty_cli_get_keystroke(cli, stroke) ;

      if (to_do != cmd_do_keystroke)
        break ;

      switch (stroke->type)
        {
          /* Straightforward character ---------------------------------*/
          /* Note: only interested in 8-bit characters !                */
          case ks_char:
            u = (uint8_t)stroke->value ;

            switch (stroke->value)
              {
                case CONTROL('C'):
                  to_do = cmd_do_ctrl_c ;       /* Exit on ^C           */
                  break ;

                case CONTROL('D'):
                  to_do = cmd_do_ctrl_d ;       /* Exit on ^D           */
                  break;

                case CONTROL('H'):
                case 0x7F:
                  uty_cli_del_backwards(cli->cl, 1);
                  break;

                case CONTROL('U'):
                case CONTROL('W'):
                  uty_cli_clear_line(cli->cl);
                  break;

                case CONTROL('Z'):
                  to_do = cmd_do_ctrl_z ;       /* Exit on ^Z           */
                  break;

                case '\n':
                case '\r':
                  to_do = cmd_do_command ;      /* Exit on CR or LF     */
                  break ;

                default:
                  if ((stroke->value >= 0x20) && (stroke->value < 0x7F))
                    uty_cli_insert(cli->cl, (char*)&u, 1) ;
                  break;
              }
            break ;

          /* ESC X -----------------------------------------------------*/
          case ks_esc:
            switch (stroke->value)
              {
                case CONTROL('H'):
                case 0x7f:
                  uty_cli_clear_line(cli->cl);
                  break;

                default:
                  break;
              } ;
              break ;

        /* ESC [ ... ---------------------------------------------------*/
        case ks_csi:
          break ;

        /* Unknown -----------------------------------------------------*/
        default:
          zabort("unknown keystroke type") ;
      } ;
    }
  while (to_do == cmd_do_keystroke) ;

  /* Tidy up and return where got to.                                   */

  nlen = qs_len_nn(cli->cl) ;

  if      (nlen < olen)
    uty_cli_out_wipe_n(cli, (int)nlen - (int)olen) ;
  else if (nlen > olen)
    uty_cli_write_n(cli, telnet_stars, nlen - olen) ;

  return to_do ;
} ;

/*------------------------------------------------------------------------------
 * Fetch next keystroke.
 *
 * Returns:  cmd_do_keystroke: have a keystroke  -- stroke != ks_null
 *           cmd_do_eof      : eof in keystream  -- stroke == knull_eof
 *           cmd_do_timed_out: timed_ keystream  -- stroke == knull_timed_out
 *           cmd_do_nothing  : nothing available -- stroke == knull_not_eof
 *
 * Note that will return cmd_do_eof or cmd_do_timed_out any number of times.
 *
 * Actual reading occurs autonomously under pselect().
 */
static cmd_do_t
uty_cli_get_keystroke(vty_cli cli, keystroke stroke)
{
  while (1)
    {
      if (keystroke_get(cli->key_stream, stroke))
        {
          if (stroke->flags != 0)
            {
              /* TODO: deal with broken keystrokes                      */
            } ;

          if (stroke->type != ks_iac)
            return cmd_do_keystroke ;           /* have a keystroke     */

          /* Deal with telnet command, so invisible to upper level
           */
          uty_telnet_command(cli->vf, stroke, false) ;
        }
      else
        {
          qassert(stroke->type == ks_null) ;

          switch (stroke->value)
            {
              case knull_not_eof:
                break ;

              case knull_eof:
                return cmd_do_eof ;

              case knull_timed_out:
                return cmd_do_timed_out ;

              default:
                zabort("unknown knull_xxx") ;
                break ;
            } ;

          return cmd_do_nothing ;
        } ;
    } ;
} ;

/*==============================================================================
 * Command line operations.
 *
 * These all affect the given command line, only.  The effect on the screen
 * is taken care of elsewhere -- see  uty_cli_update_line().
 */

/*------------------------------------------------------------------------------
 * Insert 'n' characters at current position in the command line, leaving
 * cursor after the inserted characters.
 */
static void
uty_cli_insert(qstring cl, const char* chars, int n)
{
  qassert((qs_cp_nn(cl) <= qs_len_nn(cl)) && (n >= 0)) ;

  if (n > 0)
    {
      qs_insert_n(cl, chars, n) ;
      qs_move_cp_nn(cl, n) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Replace 'm' characters at the current position, by 'n' characters and leave
 * cursor at the end of the inserted characters.
 */
static void
uty_cli_replace(qstring cl, int m, const char* chars, int n)
{
  qassert((qs_cp_nn(cl) <= qs_len_nn(cl)) && (n >= 0) && (m >= 0)) ;

  qs_replace_n(cl, m, chars, n) ;

  qs_move_cp_nn(cl, n) ;
} ;

/*------------------------------------------------------------------------------
 * Forward 'n' characters -- stop at end of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_forwards(qstring cl, int n)
{
  int have ;

  have = qs_after_cp_nn(cl) ;
  if (have < n)
    n = have ;

  qassert(n >= 0) ;

  qs_move_cp_nn(cl, n) ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Backwards 'n' characters -- stop at start of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_backwards(qstring cl, int n)
{
  if ((int)qs_cp_nn(cl) < n)
    n = qs_cp_nn(cl) ;

  qassert(n >= 0) ;

  qs_move_cp_nn(cl, -n) ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Move forwards (if n > 0) or backwards (if n < 0) -- stop at start or end of
 * line.
 */
static void
uty_cli_move(qstring cl, int n)
{
  if      (n < 0)
    uty_cli_backwards(cl, -n) ;
  else if (n > 0)
    uty_cli_forwards(cl, +n) ;
} ;

/*------------------------------------------------------------------------------
 * Delete 'n' characters -- forwards -- stop at end of line.
 */
static void
uty_cli_del_forwards(qstring cl, int n)
{
  int have ;

  have = qs_after_cp_nn(cl) ;
  if (have < n)
    n = have ;          /* cannot delete more than have */

  qassert(n >= 0) ;

  if (n > 0)
    qs_delete_n(cl, n) ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters before the point -- stopping at start of line.
 */
static void
uty_cli_del_backwards(qstring cl, int n)
{
  uty_cli_del_forwards(cl, uty_cli_backwards(cl, n)) ;
}

/*------------------------------------------------------------------------------
 * Move to the beginning of the line.
 */
static void
uty_cli_bol(qstring cl)
{
  qs_set_cp_nn(cl, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Move to the end of the line.
 */
static void
uty_cli_eol(qstring cl)
{
  qs_set_cp_nn(cl, qs_len_nn(cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Forward word delta -- distance to start of next word.
 *
 * Return number of characters to step over to reach next word.
 *
 * Steps over non-space characters and then any spaces.
 */
static int
uty_cli_word_forwards_delta(qstring cl)
{
  char* cp ;
  char* tp ;
  char* ep ;

  cp = qs_cp_char(cl) ;
  ep = qs_ep_char(cl) ;

  assert(cp <= ep) ;

  tp = cp ;

  while ((tp < ep) && (*tp != ' '))
    ++tp ;

  while ((tp < ep) && (*tp == ' '))
    ++tp ;

  return tp - cp ;
} ;

/*------------------------------------------------------------------------------
 * Forward word -- move to start of next word.
 *
 * Moves past any non-spaces, then past any spaces.
 */
static void
uty_cli_word_forwards(qstring cl)
{
  uty_cli_forwards(cl, uty_cli_word_forwards_delta(cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Backward word delta -- distance to start of next word, back.
 *
 * Return number of characters to step over to reach next word.
 *
 * Steps back over spaces, and then until next (backwards) character is space,
 * or hits start of line.
 */
static int
uty_cli_word_backwards_delta(qstring cl)
{
  char* cp ;
  char* tp ;
  char* sp ;

  assert(qs_cp_nn(cl) <= qs_len_nn(cl)) ;

  cp = qs_cp_char(cl) ;
  sp = qs_char(cl) ;

  tp = cp ;

  while ((tp > sp) && (*(tp - 1) == ' '))
    --tp ;

  while ((tp > sp) && (*(tp - 1) != ' '))
    --tp ;

  return cp - tp ;
} ;

/*------------------------------------------------------------------------------
 * Backward word -- move to start of previous word.
 *
 * Moves past any spaces, then move back until next (backwards) character is
 * space or start of line.
 */
static void
uty_cli_word_backwards(qstring cl)
{
  uty_cli_backwards(cl, uty_cli_word_backwards_delta(cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Delete to end of word -- forwards.
 *
 * Deletes any leading spaces, then deletes upto next space or end of line.
 */
static void
uty_cli_del_word_forwards(qstring cl)
{
  uty_cli_del_forwards(cl, uty_cli_word_forwards_delta(cl)) ;
}

/*------------------------------------------------------------------------------
 * Delete to start of word -- backwards.
 *
 * Deletes any trailing spaces, then deletes upto next space or start of line.
 */
static void
uty_cli_del_word_backwards(qstring cl)
{
  uty_cli_del_backwards(cl, uty_cli_word_backwards_delta(cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill rest of line from current point.
 */
static void
uty_cli_del_to_eol(qstring cl)
{
  qs_set_len_nn(cl, qs_cp_nn(cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill line from the beginning.
 */
static void
uty_cli_clear_line(qstring cl)
{
  qs_set_cp_nn(cl, 0) ;
  qs_set_len_nn(cl, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Transpose current character with previous one, and step forward one.
 *
 * If at end of line, transpose the last two characters.
 */
static void
uty_cli_transpose_chars(qstring cl)
{
  char  ch ;
  char* cp ;

  /* Give up if < 2 characters or at start of line.                     */
  if ((qs_len_nn(cl) < 2) || (qs_cp_nn(cl) < 1))
    return ;

  /* If we are not at the end, step past the second character           */
  if (qs_after_cp_nn(cl) == 0)
    qs_move_cp_nn(cl, 1) ;

  /* Get address of first character                                     */
  cp = qs_cp_char(cl) - 2 ;

  /* swap characters                                                    */
  ch        = *(cp + 1) ;
  *(cp + 1) = *cp ;
  *cp       = ch ;
} ;

/*==============================================================================
 * Command line history handling
 *
 *   cli->hist   is vector of qstrings (created on demand)
 *   cli->h_now  is index of the present time
 *   cli->hp     is index of most recent line read back
 *
 * cli->hist is initialised empty, with h_now == hp == 0.
 *
 * On first use it is set to VTY_MAX_HIST entries, and its size never changes.
 * Before VTY_MAX_HIST lines have been inserted, a NULL entry signals the end
 * of history (to date).
 *
 * h_now is incremented after a line is inserted (and wraps round).  So
 * stepping +1 moves towards the present (down) and -1 moves towards the past
 * (up).
 *
 * hp == h_now means we are in the present.
 *
 * Cannot step forwards from hp == h_now (into the future, which is the
 * same as the oldest thing we can remember !).
 *
 * Before stepping backwards from hp == hp_now, sets hp_now to be a copy of
 * the current line (complete with cp), so can return to the present.
 *
 * Cannot step backwards to hp == hp_now -- that would be to wrap round from
 * the ancient past to the now time.
 *
 * When storing a line in the history, replaces the last line stored if that
 * is the same as the new line, apart from whitespace.
 */

static inline uint
hp_next(uint hp)
{
  return (hp != (VTY_HIST_COUNT - 1)) ? hp + 1 : 0 ;
} ;

static inline uint
hp_prev(uint hp)
{
  return (hp != 0) ? hp - 1 : VTY_HIST_COUNT - 1 ;
} ;

static inline uint
hp_step(uint hp, enum hist_step step)
{
  if (step == hist_previous)
    return hp_prev(hp) ;

  if (step == hist_next)
    return hp_next(hp) ;

  zabort("invalid hist_step") ;
} ;

/*------------------------------------------------------------------------------
 * Add given command line to the history buffer.
 *
 * The 'cp' stored with the line is set to be the end of the line, so that is
 * all ready for when the stored line is used.
 *
 * Resets hp == h_now.
 */
static void
uty_cli_hist_add (vty_cli cli, qstring cl)
{
  qstring   hist_line ;
  int       h_prev ;

  VTY_ASSERT_LOCKED() ;

  /* If the previous line is NULL, that means the history is empty.
   *
   * If the previous line is essentially the same as the current line,
   * replace it with the current line -- so that the latest whitespace
   * version is saved.
   *
   * In both those cases, replace the the previous line entry by moving
   * h_now back to it -- leaving hist_line pointing at it.
   *
   * Otherwise, leave cli->h_now and point hist_line at the most ancient
   * line in history.
   */
  h_prev = hp_prev(cli->h_now) ;

  hist_line = vector_get_item(cli->hist, h_prev) ;

  if ((hist_line == NULL) || (qs_cmp_sig(hist_line, cl) == 0))
    {
      cli->h_now    = h_prev ;
      cli->h_repeat = true ;    /* latest history is a repeat   */
    }
  else
    {
      hist_line = vector_get_item(cli->hist, cli->h_now) ;
      cli->h_repeat = false ;   /* latest history is novel      */
    } ;

  /* Now replace the h_now entry -- setting 'cp' to end of line
   */
  hist_line = qs_copy(hist_line, cl) ;
  qs_set_cp_nn(hist_line, qs_len_nn(hist_line)) ;
  vector_set_item(cli->hist, cli->h_now, hist_line) ;

  /* Advance history                                                    */
  cli->hp = cli->h_now = hp_next(cli->h_now) ;
} ;

/*------------------------------------------------------------------------------
 * Replace command line by current history.
 *
 * This function is called from vty_next_line and vty_previous_line.
 *
 * Step -1 is into the past (up)
 *      +1 is towards the present (down)
 *
 * NB: assumes line will be updated by uty_cli_update_line()
 */
static void
uty_cli_hist_use(vty_cli cli, enum hist_step step)
{
  uint      hp ;
  qstring   hist_line ;

  assert((step == +1) || (step == -1)) ;

  hp = cli->hp ;

  /* Special case of being at the insertion point (the present)
   *
   * Cannot step forwards from the present.
   *
   * Before stepping back from the present, take a copy of the current
   * command line -- so can get back to it.
   *
   * Note that the 'cp' is stored with the line.  So if return to the present,
   * the cursor returns to its current position.  (When lines are added to
   * the history, the cursor is set to the end of the line.)
   */
  if (hp == cli->h_now)
    {
      if (step > 0)
        return ;        /* already in the present               */

      hist_line = vector_get_item(cli->hist, cli->h_now) ;
      vector_set_item(cli->hist, cli->h_now, qs_copy(hist_line, cli->cl)) ;
    } ;

  /* Advance or retreat and get history line.                           */
  hp = hp_step(hp, step) ;

  hist_line = vector_get_item(cli->hist, hp) ;

  /* If moving backwards in time, may not move back to the h_now
   * point (that would be wrapping round to the present) and may not
   * move back to a NULL entry (that would be going back before '.').
   */
  if (step < 0)
    if ((hist_line == NULL) || (hp == cli->h_now))
      return ;

  /* Update the history pointer and copy history line to current line.  */
  cli->hp = hp ;
  qs_copy(cli->cl, hist_line) ;
} ;

/*------------------------------------------------------------------------------
 * Show the contents of the history
 */
extern void
uty_cli_hist_show(vty_cli cli)
{
  uint  hp ;
  uint  h_end ;

  VTY_ASSERT_LOCKED() ;

  if (cli->hist == NULL)
    return ;                    /* if no history                        */

  /* We start with the oldest thing we can remember, which means that
   * we start by stepping "forwards" from "now".
   *
   * Until the history buffer fills, there will be a number of NULL entries
   * between "now" and the oldest thing in the history.
   *
   * We do not show the "now" entry, which is not part of history.
   *
   * We do not show the entry before "now", because that is the current
   * executing command, unless that was a repeat of the command before  !
   */
  hp = cli->h_now ;

  h_end = cli->h_repeat ? hp : hp_prev(hp) ;

  while (1)
    {
      qstring line ;

      hp = hp_next(hp) ;

      if (hp == h_end)
        break ;                 /* reached end of history       */

      line = vector_get_item(cli->hist, hp) ;

      if (line != NULL)
        uty_out(cli->vf->vio, "  %s\n", qs_string(line));
    } ;
} ;

/*==============================================================================
 * Command Completion and Command Description
 *
 * Any changes to the command line are made to cli->cl.  If the command line
 * is redrawn, updates cli->cls.  Otherwise, the screen may need updating to
 * reflect differences between cli->cl and cli->cls.
 */
static uint uty_cli_help_parse(vty_cli cli) ;

static void uty_cli_complete_keyword(vty_cli cli, elstring keyword,
                                                                bool complete) ;
static void uty_cli_complete_list(vty_cli cli, vector item_v) ;

static void uty_cli_describe_list(vty_cli cli, vector item_v) ;
static void uty_cli_describe_line(vector list, uint str_width,
                                                const char* str, ulen str_len,
                                                const char* doc, ulen doc_len) ;
static uint uty_cli_width_to_use(vty_cli cli) ;

static void uty_cli_help_message(vty_cli cli, const char* msg) ;
static void uty_cli_help_newline(vty_cli cli) ;
static void uty_cli_help_finish(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Command completion
 *
 * Requires that cmd_token_position() has been called to tokenise the line and
 * establish which token the cursor is in.  Must NOT call this if the cursor
 * is in a "special" place.
 *
 * This is called from inside "uty_cli_process()".
 */
static void
uty_cli_complete_command (vty_cli cli)
{
  uint        n_items ;
  cmd_parsed  parsed ;
  cmd_item    item ;

  VTY_ASSERT_LOCKED() ;

  parsed = cli->help_parsed ;

  /* Establish what items may be present at the current token position. */
  n_items = uty_cli_help_parse(cli) ;

  if      (n_items > 1)         /* render list of alternatives          */
    {
      elstring_t els ;

      if (cmd_part_complete(parsed, els))
        uty_cli_complete_keyword(cli, els, false) ;
      else
        uty_cli_complete_list(cli, parsed->item_v) ;
    }
  else if (n_items == 1)
    {
      /* One possible item -- one or more possible commands             */
      item = vector_get_item(parsed->item_v, 0) ;

      switch (item->type)
        {
          case item_null:
            zabort("invalid item_null") ;

          case item_eol:

          case item_option_word:

          case item_vararg:

          case item_word:

          case item_ipv6_prefix:
          case item_ipv6_address:
          case item_ipv4_prefix:
          case item_ipv4_address:

          case item_range:
            uty_cli_describe_list(cli, parsed->item_v) ;
            break ;

          case item_keyword:
            uty_cli_complete_keyword(cli, item->str, true) ;
            break ;

          default:
            zabort("unknown item type") ;
        } ;
    } ;

  /* If necessary, redraw the command line                              */
  uty_cli_help_finish(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Command Description
 */
static void
uty_cli_describe_command (vty_cli cli)
{
  uint        n_items ;

  VTY_ASSERT_LOCKED() ;

  /* Establish what items may be present at the current token position. */
  n_items = uty_cli_help_parse(cli) ;

  if (n_items > 0)              /* render list of possibilities         */
    uty_cli_describe_list(cli, cli->help_parsed->item_v) ;

  /* If necessary, redraw the command line                              */
  uty_cli_help_finish(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Parse for command completion and command description.
 *
 * Requires that cmd_token_position() has been called to tokenise the line and
 * establish which token the cursor is in.  Must NOT call this if the cursor
 * is in a "special" place.
 *
 * Deal with all cases which yield no items at all.
 *
 * Returns:  number of items to consider.
 */
static uint
uty_cli_help_parse(vty_cli cli)
{
  const char* msg ;
  cmd_ret_t ret ;
  uint  n_items ;

  /* The preflight checks avoid getting into trouble doing command completion
   * on a line with comment
   */
  msg = cmd_help_preflight(cli->help_parsed) ;
  if (msg != NULL)
    {
      uty_cli_help_message(cli, msg) ;
      return 0 ;
    } ;

  /* Now see what the cmd_completion can come up with.
   */
  cli->help_context = cmd_context_copy(cli->help_context, cli->vf->context) ;
  ret = cmd_completion(cli->help_parsed, cli->help_context) ;

  if (ret == CMD_ERR_PARSING)
    {
      if (cli->help_parsed->eloc >= 0)
        {
          ulen eloc = cli->vf->vio->prompt_len + cli->help_parsed->eloc ;

          uty_cli_help_newline(cli) ;   /* clears cli_drawn etc.        */
          uty_cli_write_n(cli, telnet_dots, eloc) ;
          uty_cli_write_s(cli, "^") ;
        } ;

      uty_cli_help_message(cli, qs_string(cli->help_parsed->emess)) ;

      return 0 ;
    } ;

  /* Will now have 0, 1 or more items which match at the current
   * cursor token.
   */
  n_items = vector_length(cli->help_parsed->item_v) ;

  if (n_items == 0)
    uty_cli_help_message(cli, "command not recognised") ;

  return n_items ;
} ;

/*------------------------------------------------------------------------------
 * Can complete a keyword.
 */
static void
uty_cli_complete_keyword(vty_cli cli, elstring keyword, bool complete)
{
  int pre, rep, ins, mov ;

  cmd_complete_keyword(cli->help_parsed, &pre, &rep, &ins, &mov) ;

  uty_cli_move(cli->cl, pre) ;          /* move to start of token */
  uty_cli_replace(cli->cl, rep, els_body_nn(keyword), els_len_nn(keyword)) ;

  if (complete)
    {
      qassert(ins <= 2) ;
      if (ins > 0)
        uty_cli_insert(cli->cl, "  ", ins) ;

      uty_cli_move(cli->cl, mov) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Show the command completions -- usually more than one.
 *
 * Generates a table of possible items, with fixed width entries, depending
 * on the longest option.
 *
 * NB: the items will have been sorted before we get here.  Inter alia, that
 *     ensures that any <cr> is shown last.
 */
static void
uty_cli_complete_list(vty_cli cli, vector item_v)
{
  uint i, str_width, n ;

  str_width = 6 ;
  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;

      item = vector_get_item(item_v, i) ;

      if (str_width < els_len_nn(item->str))
        str_width = els_len_nn(item->str) ;
    } ;

  n = uty_cli_width_to_use(cli) / (str_width + 2) ;

  if (n == 0)
    n = 1 ;

  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;
      ulen     str_len ;
      ulen     pad ;

      item = vector_get_item(item_v, i) ;

      if ((i % n) == 0)
        uty_cli_out_newline(cli) ;        /* clears cli_drawn     */

      str_len = els_len_nn(item->str) ;
      uty_cli_write(cli, els_body_nn(item->str), str_len) ;

      pad = (str_len < str_width) ? str_width - str_len : 0 ;

      uty_cli_write_n(cli, telnet_spaces, pad + 2) ;
    } ;

  uty_cli_help_newline(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Output list of command description lines.
 *
 * Moves to start of empty line, then zero of more description entries.  Leaves
 * at start of empty line.
 */
static void
uty_cli_describe_list(vty_cli cli, vector item_v)
{
  vector         list ;
  vector_index_t i ;

  uty_cli_help_newline(cli) ;

  list = uty_cli_make_describe_list(item_v, uty_cli_width_to_use(cli)) ;

  for (i = 0 ; i < vector_length(list) ; ++i)
    {
      qstring  qs = vector_get_item(list, i) ;

      uty_cli_write(cli, qs_char_nn(qs), qs_len_nn(qs)) ;
      uty_cli_write(cli, telnet_newline, 2) ;

      qs_free(qs) ;
    } ;

  vector_free(list) ;
} ;

/*------------------------------------------------------------------------------
 * Construct list of command description lines.
 *
 * Generates lines of the form:
 *
 *   word     description text
 *
 * Where the word field is adjusted to suit the longest word, and the
 * description text is wrapped if required (if the width of the console is
 * known) so that get:
 *
 *   word     description ..................................
 *            .............text
 *
 * NB: the items will have been sorted before we get here.  Inter alia, that
 *     ensures that any <cr> is shown last.
 *
 * The vector returned is a vector of qstrings.  Caller must release the
 * qstrings and then the vector.
 */
extern vector
uty_cli_make_describe_list(vector item_v, uint width)
{
  vector   list ;
  uint     i, str_width, doc_width ;

  list = vector_init_new(NULL, 12) ;

  /* Get width of the longest "word"                                    */
  str_width = 0;
  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;
      uint len ;

      item = vector_get_item(item_v, i) ;

      len = els_len_nn(item->str) ;
      if (*((char*)els_body_nn(item->str)) == '.')
        len--;

      if (len > str_width)
        str_width = len ;
    } ;

  /* Set width of description string.
   *
   * Format is:
   *
   *   __wo.....rd__description...
   *                ...continues
   *
   * The width of the word part has been established above as the width of the
   * widest word.
   *
   * There are two spaces on either side of the word, so we here calculate the
   * width of the description part
   */
  if (width > ((str_width + 6) + 20))
    doc_width = width - (str_width + 6) ;
  else
    doc_width = 0 ;

  /* Print out description.                                             */
  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;
      const char* str, * dp, * ep ;
      ulen     str_len ;

      item = vector_get_item(item_v, i) ;

      str =     els_body_nn(item->str) ;
      str_len = els_len_nn(item->str) ;
      if (*str == '.')
        {
          ++str ;
          --str_len ;
        } ;

      dp  = item->doc ;
      ep  = dp + strlen(dp) ;

      /* If have a sensible description width                       */
      if (doc_width > 20)
        {
          while ((ep - dp) > doc_width)
            {
              const char* np ;

              np = dp + doc_width ;         /* target next position */

              while ((np > dp) && (*np != ' '))
                --np ;                      /* seek back to ' '     */

              if (np == dp)                 /* if no space...       */
                np = dp + doc_width ;       /* ...force break       */

              uty_cli_describe_line(list, str_width, str, str_len, dp, np - dp);

              str_len = 0 ;         /* for 2nd and subsequent lines */

              dp = np ;             /* step past what just wrote    */
              while (*dp == ' ')
                ++dp ;              /* skip spaces                  */
            } ;
        } ;

      uty_cli_describe_line(list, str_width, str, str_len, dp, ep - dp) ;
    } ;

  return list ;
} ;

/*------------------------------------------------------------------------------
 * Construct one description line.
 */
static void
uty_cli_describe_line(vector list, uint str_width, const char* str,
                                   ulen str_len, const char* doc, ulen doc_len)
{
  qstring qs ;

  if ((str_len == 0) && (doc_len == 0))
    return ;            /* quit if nothing to say               */

  qs = qs_new(2 + str_width + 2 + doc_len) ;

  if (str_len > 0)
    {
      qs_append_str(qs, "  ") ;
      qs_append_n(qs, str, str_len) ;
    }
  else
    str_width += 2 ;

  if (doc_len > 0)
    {
      ulen pad ;
      pad = (str_len < str_width) ? str_width - str_len : 0 ;
      qs_append_ch_x_n(qs, ' ', pad + 2) ;
      qs_append_n(qs, doc, doc_len) ;
    } ;

  vector_push_item(list, qs) ;
} ;

/*------------------------------------------------------------------------------
 * Return the actual or assumed console width.
 *
 * If we know the width we use it.  Otherwise just assume something reasonable.
 */
static uint
uty_cli_width_to_use(vty_cli cli)
{
  return (cli->width == 0) ? 60 : cli->width ;
} ;

/*------------------------------------------------------------------------------
 * Move to new line, issue message and leave on new line.
 *
 * Deals with updating the command line if we are currently on it.
 */
static void
uty_cli_help_message(vty_cli cli, const char* msg)
{
  uty_cli_help_newline(cli) ;           /* clears cli->drawn etc.       */
  uty_cli_write_s(cli, "% ") ;
  uty_cli_write_s(cli, msg) ;
  uty_cli_write(cli, telnet_newline, 2) ;
} ;

/*------------------------------------------------------------------------------
 * If the command line is drawn, make sure it is up to date, leaving cursor
 * at the end of the line, and then issue newline.
 *
 * Clears cli->drawn.
 */
static void
uty_cli_help_newline(vty_cli cli)
{
  qassert(cli->vf->vio->state == vst_cmd_fetch) ;

  if (cli->drawn)
    uty_cli_update_line(cli, qs_len_nn(cli->cl)) ;

  uty_cli_write(cli, telnet_newline, 2) ;

  cli->drawn = false ;
} ;

/*------------------------------------------------------------------------------
 * If the command line help has "undrawn" the command line, then redraw it now
 * and make a new copy to cli->cls.
 *
 * Sets cli->drawn
 */
static void
uty_cli_help_finish(vty_cli cli)
{
  qassert(cli->vf->vio->state == vst_cmd_fetch) ;

  if (!cli->drawn)
    {
      uty_cli_draw(cli, false /* cursor unchanged */) ;
      qs_copy(cli->cls, cli->cl) ;
    } ;
} ;

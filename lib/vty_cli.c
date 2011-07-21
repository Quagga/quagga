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

/*------------------------------------------------------------------------------
 * Essential stuff.
 */
#define TELNET_NEWLINE "\r\n"
static const char* telnet_newline  = TELNET_NEWLINE ;
       const char* uty_cli_newline = TELNET_NEWLINE ;

/*==============================================================================
 * Construct and destroy CLI object
 */

static bool uty_cli_callback(keystroke_callback_args) ;
static void uty_cli_update_more(vty_cli cli) ;

static void uty_cli_cancel(vty_cli cli) ;

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
   *   hist           = NULL           -- set up when first used.
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
   *
   *   tilde_prompt   = false          -- tilde prompt is not drawn
   *   tilde_enabled  = false          -- set below if ! multi-threaded
   *
   *   prompt_len     = 0              -- not drawn, in any case
   *   extra_len      = 0              -- not drawn, in any case
   *
   *   prompt_node    = NULL_NODE      -- so not set !
   *   prompt_gen     = 0              -- not a generation number
   *   prompt_for_node = NULL          -- not set, yet
   *
   *   dispatched     = false          -- see below
   *   in_progress    = false          -- see below
   *   blocked        = false          -- see below
   *   paused         = false
   *
   *   mon_active     = false
   *   out_active     = false
   *
   *   more_wait      = false
   *   more_enter     = false
   *
   *   more_enabled   = false          -- not in "--More--" state
   *
   *   pause_timer    = NULL           -- set below if multi-threaded
   *
   *   context        = NULL           -- see below
   *   auth_context   = false          -- set by uty_cli_want_command()
   *
   *   parsed         = NULL           -- see below
   *   to_do          = cmd_do_nothing
   *   cl             = NULL qstring   -- set below
   *   cls            = NULL qstring   -- set below
   *   clx            = NULL qstring   -- set below
   *   dispatch       = all zeros      -- nothing to dispatch
   *
   *   cbuf           = NULL           -- see below
   *
   *   olc            = NULL           -- see below
   */
  confirm(NULL_NODE == 0) ;                     /* prompt_node & node   */
  confirm(cmd_do_nothing == 0) ;                /* to_do                */
  confirm(CMD_ACTION_ALL_ZEROS) ;               /* dispatch             */

  cli->vf = vf ;

  /* Allocate and initialise a keystroke stream     TODO: CSI ??        */
  cli->key_stream = keystroke_stream_new('\0', "\x03", uty_cli_callback, cli) ;

  /* Set up cl, cls and clx qstrings, the command line output fifo and
   * the output line control.
   */
  cli->cl   = qs_new(120) ;     /* reasonable line length       */
  cli->cls  = qs_new(120) ;
  cli->clx  = qs_new(120) ;

  cli->cbuf = vio_fifo_new(1000) ;

  cli->olc  = vio_lc_new(0 , 0, telnet_newline) ;

  /* Make an empty context object and an empty parsed object            */
  cli->context = cmd_context_new() ;
  cli->parsed = cmd_parsed_new() ;

  /* Use global 'lines' setting, as default -- but 'lines_set' false.   */
  uty_cli_set_lines(cli, host.lines, false) ;
  uty_cli_update_more(cli) ;    /* and update the olc etc to suit.      */

  /* Enable "~ " prompt and pause timer if multi-threaded
   *
   * TODO decide whether to ditch the '~' prompt, given the priority given
   * to commands.
   */
  if (vty_nexus)
    {
      cli->pause_timer   = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                vty_term_pause_timeout, cli) ;
#if 0
      cli->tilde_enabled = vty_multi_nexus ;
#endif
    } ;

  /* Ready to be started -- paused, out_active & more_wait are false.
   *
   * Is started by the first call of uty_cli_want_command(), which (inter alia)
   * set the cli->context so CLI knows how to prompt etc.
   */
  cli->dispatched   = true ;
  cli->in_progress  = true ;
  cli->blocked      = true ;

  return cli ;
} ;

/*------------------------------------------------------------------------------
 * Close CLI object, if any -- may be called more than once.
 *
 * Shuts down and discards anything to do with the input.
 *
 * Revokes anything that can be revoked, which may allow output to proceed
 * and the vty to clear itself down.
 *
 * If dispatched and not final, keeps the cbuf, the olc and clx if dispatched.
 * If not final, keeps the cbuf and the olc.
 *
 * Destroys self if final.
 *
 * Expects other code to:
 *
 *   - close down command loop and empty the vin/vout stacks.
 *
 *   - turn of any monitor status.
 *
 *   - half close the telnet connection.
 */
extern vty_cli
uty_cli_close(vty_cli cli, bool final)
{
  if (cli == NULL)
    return NULL ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  /* Bring as much of the command handling to a stop as possible, and
   * turn off "---more--" etc. so that output can proceed.
   */
  cli->more_enabled = false ;
  cli->lines        = 0 ;       /* so will not be re-enabled            */

  uty_cli_cancel(cli) ;         /* " ^C\r\n" unless believed blank      */

  cli->more_wait  = false ;     /* exit more_wait (if was in it)        */
  cli->more_enter = false ;     /* and so cannot be this                */

  cli->blocked     = true ;     /* do not renter the CLI                */
  cli->out_active  = true ;     /* if there is any output, it can go    */

  /* Ream out the history.                                              */
  {
    qstring line ;
    while ((line = vector_ream(cli->hist, free_it)) != NULL)
      qs_reset(line, free_it) ;
    cli->hist = NULL ;
  } ;

  /* Empty the keystroke handling.                                      */
  cli->key_stream = keystroke_stream_free(cli->key_stream) ;

  /* Can discard active command line if not dispatched                  */
  if (!cli->dispatched || final)
    cli->clx = qs_reset(cli->clx, free_it) ;

  /* Can discard context and parsed objects                             */
  cli->context = cmd_context_free(cli->context, true) ; /* its a copy   */
  cli->parsed  = cmd_parsed_free(cli->parsed) ;

  /* Discard any pause_timer, and suppress */
  cli->pause_timer    = qtimer_free(cli->pause_timer) ;
  cli->paused         = false ;
#if 0
  cli->tilde_enabled  = false ;
#endif

  /* If final, free the CLI object.                                     */
  if (final)
    {
      cli->prompt_for_node = qs_free(cli->prompt_for_node) ;
      cli->cl   = qs_free(cli->cl) ;
      cli->cls  = qs_free(cli->cls) ;

      cli->cbuf = vio_fifo_free(cli->cbuf) ;
      cli->olc  = vio_lc_free(cli->olc) ;

      XFREE(MTYPE_VTY_CLI, cli) ;               /* sets cli = NULL      */
  } ;

  return cli ;
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
  vty_cli cli = context ;

  switch (stroke->type)
    {
      case ks_char:             /* character -- uint32_t                */
        if ((cli->in_progress || cli->out_active) && !cli->more_wait)
          {
            vty_io vio = cli->vf->vio ;

            uty_cmd_signal(vio, CMD_CANCEL) ;   /* ^C                   */

            return true ;
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
 * Set the effective height for line control, if required, which may enable the
 * "--more--" output handling.
 *
 * If not, want some limit on the amount of stuff output at a time.
 *
 * Sets the line control window width and height.
 * Sets cli_more_enabled if "--more--" is enabled.
 */
static void
uty_cli_update_more(vty_cli cli)
{
  bool    on ;

  on = false ;          /* default state        */

  if (cli->vf->vout_state == vf_open)
    {
      int height ;

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

      if (height > 0)
        on = true ;     /* have a defined height        */
      else
        height = 200 ;  /* but no "--more--"            */

      vio_lc_set_window(cli->olc, cli->width, height) ;
    } ;

  cli->more_enabled = on ;
} ;

/*==============================================================================
 * General mechanism for command execution.
 *
 * Command execution is driven by select/pselect -- which means that the
 * processing of commands is multiplexed with all other activity.  In the
 * following:
 *
 *   -- read_ready and write_ready are events signalled by select/pselect
 *
 *   -- setting read or write on, means enabling the file for select/pselect to
 *      consider it for read_ready or write_ready, respectively.
 *
 * All command actions are dispatched via the command queue -- commands are
 * not executed inside the CLI code.  [For legacy threads the event queue is
 * used.  If that is too slow, a priority event queue will have to be
 * invented.]
 *
 * State of the CLI:
 *
 *   dispatched   -- a command line has been dispatched, and is waiting for
 *   (dsp)           command loop to fetch it.
 *
 *   in_progress  -- a command has been taken by the command loop, and is still
 *   (inp)           running -- or at least no further command has been
 *                   fetched by the command loop.
 *
 *                   The command may be an in_pipe, so there may be many
 *                   commands to be completed before the CLI level command is.
 *
 *                   or: the CLI has been closed.
 *
 *   blocked      -- is in_progress and a further command is now ready to be
 *   (bkd)           dispatched.
 *
 *                   or: the CLI has been closed.
 *
 *   out_active   -- the command output FIFO is being emptied.
 *   (oa)
 *                   This is set when output is pushed, and cleared when
 *                   everything is written away and in_progress is not set.
 *                   When it is set, any current command line is wiped.
 *
 *                   Note that what this flag does is prevent the CLI from
 *                   running until the output completes, and in particular
 *                   prevents it from writing anything to the CLI buffer.
 *
 *   more_wait    -- is in "--more--" wait state.  => out_active !
 *   (mwt)
 *                   The "--more--" CLI uses the CLI output buffer to draw
 *                   and undraw the "--more--" prompt.  The buffer will
 *                   otherwise be empty because is out_active.
 *
 *   more_enter   -- is in the process of entering the "--more--" wait state,
 *   (men)           waiting to write the "--more--" prompt and prepare the
 *                   keystroke input.
 *
 * The following are the valid combinations:
 *
 *     dsp:inp:bkd: oa:mwt:men:
 *     ---:---:---:---:---:---:-----------------------------------------
 *      0 : 0 : 0 : 0 : 0 : 0 : collecting a new command
 *      1 : 0 : 0 : 0 : 0 : 0 : waiting for command to be fetched
 *      1 : 1 : 0 : X : 0 : 0 : command fetched and running
 *      1 : 1 : 1 : X : 0 : 0 : waiting for command to complete
 *      0 : 0 : 0 : 1 : 0 : 0 : waiting for command output to finish
 *      1 : X : X : 1 : 1 : 1 : waiting for "--more--" to start
 *      1 : X : X : 1 : 1 : 0 : waiting for "--more--" response
 *      1 : 1 : 1 : 1 : 0 : 0 : waiting for command to complete,
 *                                                after the CLI has been closed
 *
 * There are two output FIFOs:
 *
 *   1. for the CLI itself               -- uty_cli_out and friends
 *
 *   2. for output generated by commands -- vty_out and friends.
 *
 * The CLI FIFO is emptied whenever possible, in preference to the command
 * FIFO.  The command FIFO is emptied when out_active.  While a command is
 * in_progress all its output is collected in the command output buffer,
 * to be written away when the command completes, or if it pushes the output
 * explicitly.  Note that where the CLI level command is an in_pipe, the first
 * output will set out_active, and that will persist until returns to the CLI
 * level.  Between commands in the pipe, the output will be pushed, so will
 * output stuff more or less as it is generated.
 *
 * It is expected that each command's output will end with a newline.  However,
 * the line control imposes some discipline, and holds on to incomplete lines
 * until a newline arrives, or the output is because in_progress is false -- so
 * that when the CLI is kicked, the cursor will be at the start of an empty
 * line.
 *
 * Note that is read ready until the keystroke stream hits eof.  So any input
 * will be hoovered up as soon as it is available.  The CLI process is driven
 * mostly by write_ready, except for when all output is complete and the
 * input keystroke buffer has been emptied.
 *
 * Note also that after each command dispatch the CLI processor exits, to be
 * re-entered again on write_ready/read_ready -- so does one command line at
 * a time, yielding the processor after each one.
 *
 *------------------------------------------------------------------------------
 * The "--more--" handling.
 *
 * This is largely buried in the output handling.
 *
 * When command is pushed to written away, out_active will be set (and any
 * command line will be wiped).  The output process is then kicked.
 *
 * The output process used the line_control structure to manage the output, and
 * occasionally enter the trivial "--more--" CLI.  This is invisible to the
 * main CLI.  (See the more_wait and more_enter flags and their handling.)
 *
 * If the user decides to abandon output at the "--more--" prompt, then the
 * then contents of the command output FIFO are discarded.
 *
 *------------------------------------------------------------------------------
 * The qpthreads/qpnexus extension.
 *
 * When running in qnexus mode, many commands are not executed directly in the
 * CLI, but are queued for execution by the main "routeing" nexus.
 *
 * The parsing of commands is context sensitive.  The context depends may
 * change during the execution of a command.  So... it is not possible to
 * dispatch a command until the previous one has completed.
 *
 * In qnexus mode, when a command is queued, the CLI does not go cli_blocked,
 * even if some output has already been generated.  This allows a further
 * command to be entered while the previous one is executed.  However, if the
 * command is dispatched before the previous one completes, then the cli will
 * block.
 *
 * While the previous command is executing, the current command line has a
 * minimal prompt -- to show that the context is not known.  When the previous
 * command completes, the command line is redrawn in the new context.
 *
 *------------------------------------------------------------------------------
 * Command line drawn state.
 *
 * When the cli_drawn flag is set, the current console line contains the
 * current prompt and the user input to date.  The cursor is positioned where
 * the user last placed it.
 *
 * The command line can be "wiped" -- see uty_cli_wipe() -- which removes all
 * output and prompt, and leaves the console at the start of an empty line
 * where the command line used to be.
 *
 * On entry to the CLI, it will draw the command line again if it has been
 * wiped.
 *
 * This mechanism is used to support the partial command line that may be
 * entered while a queued command executes.
 *
 * It is also used for the command help/completion system.
 *
 * It is also used to support the "monitor" output.
 */

/*==============================================================================
 * The CLI
 */

#define CONTROL(X)  ((X) & 0x1F)

static enum vty_readiness uty_cli_standard(vty_cli cli) ;
static cmd_do_t uty_cli_auth(vty_cli cli) ;
static enum vty_readiness uty_cli_more_wait(vty_cli cli) ;
static void uty_cli_draw(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * CLI for VTY_TERMINAL
 *
 * Called by uty_term_ready(), so driven by read/write ready.
 *
 * Do nothing at all if half closed.
 *
 * Otherwise do: standard CLI
 *           or: "--more--" CLI
 *
 * NB: on return, requires that an attempt is made to write away anything that
 *     may be ready for that.
 */
extern vty_readiness_t
uty_cli(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  if (cli->vf->vin_state == vf_closed)
    return not_ready ;          /* Nothing more from CLI if closed      */

  /* Standard or "--more--" CLI ?                                       */
  if (cli->more_wait)
    return uty_cli_more_wait(cli) ;
  else
    return uty_cli_standard(cli) ;
} ;

/*==============================================================================
 * The Standard CLI
 */

static cmd_do_t uty_cli_process(vty_cli cli) ;
static void uty_cli_response(vty_cli cli, cmd_do_t cmd_do) ;

static bool uty_cli_dispatch(vty_cli cli) ;
static cmd_do_t uty_cli_command(vty_cli cli) ;
static void uty_cli_hist_add (vty_cli cli, qstring clx) ;

static void uty_cli_pause_start(vty_cli cli) ;
static void uty_cli_goto_end_if_drawn(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Standard CLI for VTY_TERM -- if not blocked, runs until:
 *
 *   * runs out of keystrokes
 *   * executes a command
 *
 * Note that this executes at most one command each time it is called.  This
 * is to allow for a modicum of sharing of the system.  For real keyboard input
 * this will make no difference at all !
 *
 * Returns: not_ready    blocked and was blocked when entered
 *          write_ready  if there is anything in the keystroke stream
 *          read_ready   otherwise
 */
static vty_readiness_t
uty_cli_standard(vty_cli cli)
{
  bool need_prompt ;

  VTY_ASSERT_LOCKED() ;

  assert(!cli->more_wait) ;     /* cannot be here in more_wait state !  */

  /* cli_blocked is set when is waiting for a command, or some output to
   * complete.
   *
   * NB: in both these cases, assumes that other forces are at work to
   *     keep things moving.
   */
  if (cli->blocked || cli->out_active || cli->paused || cli->mon_active)
    return not_ready ;

  /* Make sure that the command line is drawn.
   *
   * If there is nothing pending, then can run the CLI until there is
   * something to do, or runs out of input.
   */
  if (!cli->drawn)
    uty_cli_draw(cli) ;

  if (cli->to_do == cmd_do_nothing)
    cli->to_do = cli->auth_context ? uty_cli_auth(cli)
                                   : uty_cli_process(cli) ;

  /* If have something to do, do it if we can.                          */
  need_prompt = false ;

  if (cli->to_do != cmd_do_nothing)
    {
      /* Reflect immediate response                                     */
      uty_cli_response(cli, cli->to_do) ;

      /* If command not already in progress, dispatch this one, which may
       * set the CLI blocked.
       *
       * Otherwise is now blocked until queued command completes.
       */
      if (cli->dispatched)
        cli->blocked = true ;                   /* waiting for previous */
      else
        need_prompt = uty_cli_dispatch(cli) ;   /* dispatch latest      */
    } ;

  /* If blocked, must wait for some other event to continue in CLI.
   *
   * Note that will be blocked if have just dispatched a command, and is
   * not "tilde_enabled" -- which is the case if single threaded, and may be
   * so for other reasons.
   *
   * If the keystroke stream is not empty, use write_ready as a proxy for
   * CLI ready -- no point doing anything until any buffered output has been
   * written away.
   *
   * If command prompt has been redrawn, need to kick writer to deal with
   * that -- will reenter to then process any keystrokes.
   */
  assert(!cli->paused) ;

  if (cli->blocked)
    return not_ready ;

  if (!keystroke_stream_empty(cli->key_stream) || need_prompt)
    return write_ready ;

  /* The keystroke stream is empty, but CLI is not blocked.
   *
   * If a command is dispatched, and output is not active, and the command
   * line is not drawn, then go to paused state, so that will delay output
   * of the prompt slightly (or until a keystroke arrives, or the current
   * command completes, or something else happens).
   *
   * Note that if a command has been dispatched, and is !tilde_enabled, then
   * will now be blocked, so won't be here.
   */
  if (cli->dispatched && !cli->out_active && !cli->drawn)
    uty_cli_pause_start(cli) ;

  return read_ready ;
} ;

/*------------------------------------------------------------------------------
 * Dispatch the current cli->to_do -- queueing it if necessary.
 *
 * Requires that are NOT blocked and NO command is queued.
 *
 * Expects to be on new blank line, and when returns will be on new, blank
 * line.
 *
 * Generally sets cli->to_do = cmd_do_nothing and clears cli->cl to empty.
 *
 * Can set cli->cl_do = and cli->cl to be a follow-on command.
 *
 * Returns:  true <=> nothing, in fact, to do: prompt has been redrawn.
 */
static bool
uty_cli_dispatch(vty_cli cli)
{
  qstring   tmp ;
  cmd_do_t  to_do_now ;

  vty_io  vio = cli->vf->vio ;

  VTY_ASSERT_LOCKED() ;

  /* About to dispatch a command, so must be in the following state.    */
  qassert(!cli->dispatched && !cli->in_progress
                                        && !cli->blocked && !cli->out_active) ;
  qassert(cli->context->node == vio->vty->exec->context->node) ;

  /* Set cli->clx to the command about to execute & pick up cli->to_do.
   *
   * Clear cli->cl  and cli->to_do.
   */
  tmp       = cli->clx ;                /* swap clx and cl              */
  cli->clx  = cli->cl ;
  cli->cl   = tmp ;

  to_do_now = cli->to_do ;              /* current operation            */

  cli->to_do = cmd_do_nothing ;         /* clear                        */
  qs_clear(cli->cl) ;                   /* set cl empty                 */

  /* Dispatch command                                                   */
  if (cli->auth_context)
    to_do_now |= cmd_do_auth ;
  else
    {
      /* All other nodes...                                             */
      switch (to_do_now)
      {
        case cmd_do_nothing:
        case cmd_do_ctrl_c:
        case cmd_do_eof:
        case cmd_do_timed_out:
          break ;

        case cmd_do_command:
          to_do_now = uty_cli_command(cli) ;
          break ;

        case cmd_do_ctrl_d:
          zabort("invalid cmd_do_ctrl_d") ;
          break ;

        case cmd_do_ctrl_z:
          to_do_now = uty_cli_command(cli) ;

          if (to_do_now == cmd_do_nothing)
            to_do_now = cmd_do_ctrl_z ;
          else
            cli->to_do = cmd_do_ctrl_z ;        /* defer the ^Z       */

          break ;

        default:
          zabort("unknown cmd_do_xxx value") ;
      } ;
    } ;

  cli->hp = cli->h_now ;        /* in any event, back to the present    */

  if (to_do_now != cmd_do_nothing)
    {
      cmd_action_set(cli->dispatch, to_do_now, cli->clx) ;
      cli->dispatched = true ;

      uty_cmd_signal(vio, CMD_SUCCESS) ;

      cli->blocked = (to_do_now != cmd_do_command)
#if 0
                                                   || !cli->tilde_enabled
#endif
          ;
    }
  else
    {
      cmd_action_clear(cli->dispatch) ;
      cli->dispatched = false ;

      uty_cli_draw(cli) ;
    } ;

  return !cli->dispatched ;
} ;

/*------------------------------------------------------------------------------
 * Check if command is empty, if not add to history
 *
 * Returns: cmd_do_nothing   -- empty command line
 *          cmd_do_command   -- command ready to be executed (added to history)
 */
static cmd_do_t
uty_cli_command(vty_cli cli)
{
  if (cmd_is_empty(cli->clx))
    return cmd_do_nothing ;     /* easy when nothing to do !            */

  /* Add not empty command to history                                   */
  uty_cli_hist_add(cli, cli->clx) ;

  return cmd_do_command ;
} ;

/*------------------------------------------------------------------------------
 * Want another command line from the CLI.
 *
 * If in_progress this <=> any previous command has completed, and output may
 * be active writing the results away.  Will:
 *
 *   * clear cli->dispatched
 *   * clear cli->in_progress
 *   * clear cli->blocked
 *
 * May be in more_wait state -- so avoids touching that.
 *
 * If not in_progress, then if dispatched, we have a new command ready to pass
 * to the command loop -- which we do here, and set cli->in_progress.
 *
 * Returns:  CMD_SUCCESS -- the given action has been set to next command
 *       or: CMD_WAITING -- no command available (yet)
 *
 * Note that for the CLI eof and read time-out are handled as cmd_do_eof and
 * cmd_do_timed_out -- so will float through as CMD_SUCCESS and be processed
 * as commands.
 *
 * Write I/O errors and time-outs are signalled by uty_vf_error(), and
 * therefore caught in the command loop.
 *
 * Read I/O errors are signalled by uty_vf_error().  Read timeout is treated
 * as above.
 */
extern cmd_return_code_t
uty_cli_want_command(vty_cli cli, cmd_action action, cmd_context context)
{
  VTY_ASSERT_LOCKED() ;

  if      (cli->in_progress)
    {
      /* Previous command has completed
       *
       * Make sure state reflects the fact that we are now waiting for a
       * command.
       */
      cli->dispatched  = false ;    /* no longer have a dispatched command  */
      cli->in_progress = false ;    /* command complete                     */
      cli->blocked     = false ;    /* releases possibly blocked command    */
      cli->paused      = false ;    /* override paused state                */

      *cli->context    = *context ; /* make sure is up to date              */
      cli->auth_context = (   (cli->context->node == AUTH_NODE)
                           || (cli->context->node == AUTH_ENABLE_NODE) ) ;

      /* If the output is owned by command output, then when the buffers
       * empty, and in_progress is seen to be false, out_active will be
       * cleared.
       *
       * If the output side is not owned by command output, rewrite the
       * command line, so that prompt is up to date and visible.
       *
       * In any case, kick write_ready to ensure output clears and prompt is
       * written and so on.
       */
      if (!cli->out_active)
        uty_cli_draw(cli) ;

      uty_term_set_readiness(cli->vf, write_ready) ;
    }
  else if (cli->dispatched)
    {
      /* New command has been dispatched -- can now pass that to the
       * command loop -- setting it in_progress.
       */
      assert(cli->dispatch->to_do != cmd_do_nothing) ;
      cmd_action_take(action, cli->dispatch) ;

      cli->in_progress = true ;
    } ;

  return cli->in_progress ? CMD_SUCCESS : CMD_WAITING ;
} ;

/*------------------------------------------------------------------------------
 * Start pause timer and set paused.
 */
static void
uty_cli_pause_start(vty_cli cli)
{
  qtimer_set(cli->pause_timer, qt_add_monotonic(QTIME(0.2)), NULL) ;
  cli->paused = true ;
} ;

/*==============================================================================
 * The "--more--" CLI
 *
 * While command output is being cleared from its FIFO, the CLI is blocked.
 *
 * When the output side signals that "--more--" is required, it sets the
 * more_wait and more_enter flags.
 *
 * The first stage of handling "--more--" is to suck the input dry, so that
 * (as far as is reasonably possible) does not steal a keystroke as the
 * "--more--" response which was typed before the prompt was issued.
 *
 * The more_enter flag indicates that the CLI is in this first stage.
 */

/*------------------------------------------------------------------------------
 * Change the CLI to the "--more--" CLI.
 *
 * Outputs the new prompt line.
 */
extern void
uty_cli_enter_more_wait(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  qassert(cli->out_active && !cli->more_wait && !cli->drawn) ;

  cli->more_wait  = true ;      /* new state                            */
  cli->more_enter = true ;      /* drawing the "--more--" etc.          */
} ;

/*------------------------------------------------------------------------------
 * Handle the "--more--" state.
 *
 * If is paused or the monitor is active, do nothing -- those override the
 * "--more--" state.
 *
 * If more_enter is set, then need to make sure the "--more--" prompt is
 * written, and that any keystrokes which arrived before the prompt is
 * written are taken into the keystroke stream -- so not stolen.
 *
 * Note that the "--more--" state may be interrupted by monitor output,
 * which, once complete, sets more_enter again.
 *
 * If more_enter is not set, tries to steal a keystroke, and if succeeds wipes
 * the "--more--" prompt and exits cli_more_wait -- and may cancel all
 * outstanding output.
 *
 * EOF on input causes immediate exit from cli_more_state.
 *
 * Returns: read_ready   -- waiting to steal a keystroke
 *          write_ready  -- waiting to draw or undraw the "--more--" prompt
 *          not_ready    -- otherwise
 */
static vty_readiness_t
uty_cli_more_wait(vty_cli cli)
{
  keystroke_t steal ;
  bool  cancel, stolen ;

  VTY_ASSERT_LOCKED() ;

  assert(cli->more_wait) ;      /* must be in more_wait state !         */

  if (cli->paused || cli->mon_active)
    return not_ready ;

  /* Deal with the first stage of "--more--"                            */
  if (cli->more_enter)
    {
      int  get ;

      if (!cli->drawn)
        uty_cli_draw(cli) ;             /* draw the "--more--"          */

      /* If the CLI buffer is not yet empty, then is waiting for the
       * initial prompt to clear, so nothing to be done here.
       */
      if (!vio_fifo_empty(cli->cbuf))
        return write_ready ;

      cli->more_enter = false ;

      /* empty the input buffer into the keystroke stream               */
      do
        {
          get = uty_term_read(cli->vf) ;
        } while (get > 0) ;
    } ;

  /* Try to get a stolen keystroke.  If the keystroke stream has hit
   * EOF (for any reason, including error or timed-out), will get a ks_null
   * stolen keystroke.
   *
   * If nothing to be stolen exit.
   */
  stolen = keystroke_steal(cli->key_stream, steal) ;

  if (!stolen)
    return read_ready ;

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
   * If cancelling, make sure is at end of "--more--" prompt, and then
   * clear the "drawn".  The cancel process will append a " ^C\n" !
   *
   * If not cancelling, wipe out the prompt and update state.
   *
   * Return write_ready to tidy up the screen and, unless cleared, write
   * some more.
   */
  if (cancel)
    {
      vio_fifo_clear(cli->vf->obuf, false) ;
      vio_lc_clear(cli->olc) ;  /* clear & reset counter        */

      uty_cli_goto_end_if_drawn(cli) ;
      cli->drawn = false ;

      qassert(cli->out_active) ;

      uty_cmd_signal(cli->vf->vio, CMD_CANCEL) ;        /* ^C   */
    }
  else
    {
      vio_lc_counter_reset(cli->olc) ;
      uty_cli_wipe(cli, 0) ;
    } ;

  cli->more_wait  = false ;     /* exit more_wait               */
  cli->more_enter = false ;     /* tidy                         */

  return write_ready ;
} ;

/*==============================================================================
 * Monitor output.
 *
 * To prepare for monitor output, wipe as much as is necessary for the
 * monitor line to appear correctly.
 *
 * After monitor output, may need to do two things:
 *
 *   * if the output was incomplete, place the rump in the CLI buffer,
 *     so that:
 *
 *       a. don't mess up the console with partial lines
 *
 *       b. don't lose part of a message
 *
 *       c. act as a brake on further monitor output -- cannot do any more
 *          until the last, part, line is dealt with.
 *
 *   * restore the command line, unless it is empty !
 */

 /*-----------------------------------------------------------------------------
  * Prepare for new monitor output line.
  *
  * Wipe any existing command line.
  */
extern void
uty_cli_pre_monitor(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  uty_cli_wipe(cli, 0) ;

  cli->mon_active = true ;      /* block cli & enable empty of fifo     */
} ;

/*------------------------------------------------------------------------------
 * Recover from monitor line output.
 *
 * If monitor line failed to complete, append the rump to the CLI buffer.
 *
 * If have a non-empty command line, or is cli_more_wait, redraw the command
 * line.
 *
 * Returns:  0 => rump was empty and no command line stuff written
 *         > 0 => rump not empty or some command line stuff written
 */
extern void
uty_cli_post_monitor(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  uty_cli_pause_start(cli) ;    /* do not draw prompt immediately       */

  cli->more_enter = cli->more_wait ;
                                /* revert to more_enter state           */

  cli->mon_active = false ;     /* unblock cli & turn off output        */
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

static void uty_cli_write_n(vty_cli cli, cli_rep_char chars, int n) ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf fprintf()
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
 */
extern void
uty_cli_write(vty_cli cli, const char *this, int len)
{
  vio_fifo_put_bytes(cli->cbuf, this, len) ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- write 'n' characters using a cli_rep_char string
 */
static void
uty_cli_write_n(vty_cli cli, cli_rep_char chars, int n)
{
  int len ;

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
 * Returns length of string.
 */
static int
uty_cli_write_s(vty_cli cli, const char *str)
{
  int len ;

  len = strlen(str) ;
  if (len != 0)
    uty_cli_write(cli, str, len) ;

  return len ;
} ;

/*==============================================================================
 * Prompts and responses
 */

/*------------------------------------------------------------------------------
 * Send newline to the console.
 *
 * Clears the cli_drawn flag.
 */
extern void
uty_cli_out_newline(vty_cli cli)
{
  uty_cli_goto_end_if_drawn(cli) ;

  uty_cli_write(cli, telnet_newline, 2) ;
  cli->drawn = false ;
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
 * If the command line is drawn, then show that it has been cancelled.
 *
 * Clears cli->drawn.
 */
static void
uty_cli_cancel(vty_cli cli)
{
  if (cli->drawn)
    {
      uty_cli_goto_end_if_drawn(cli) ;

      uty_cli_write_s(cli, " ^C" TELNET_NEWLINE) ;

      cli->drawn = false ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * If the command line is drawn and the cursor is not at the end of the line,
 * move the physical cursor to the end of the line.
 *
 * Assumes about to issue newline.
 */
static void
uty_cli_goto_end_if_drawn(vty_cli cli)
{
  ulen after ;

  if (cli->drawn && ( (after = qs_after_cp_nn(cli->cl)) != 0 ))
    uty_cli_write(cli, qs_cp_char_nn(cli->cl), after) ;
} ;

/*------------------------------------------------------------------------------
 * Send response to the given cmd_do
 *
 * If no command is in progress, then will send newline to signal that the
 * command is about to be dispatched.
 *
 * If command is in progress, then leaves cursor on '^' to signal that is now
 * waiting for previous command to complete.
 */
static const char* cli_response [2][cmd_do_count] =
{
  { /* when not waiting for previous command to complete        */
    [cmd_do_command]   = "",
    [cmd_do_ctrl_c]    = "^C",
    [cmd_do_ctrl_d]    = "^D",
    [cmd_do_ctrl_z]    = "^Z",
    [cmd_do_eof]       = "^*",
    [cmd_do_timed_out] = "^!"
  },
  { /* when waiting for a previous command to complete          */
    [cmd_do_command]   = "^",
    [cmd_do_ctrl_c]    = "^C",
    [cmd_do_ctrl_d]    = "^D",
    [cmd_do_ctrl_z]    = "^Z",
    [cmd_do_eof]       = "^*",
    [cmd_do_timed_out] = "^!"
  }
} ;

static void
uty_cli_response(vty_cli cli, cmd_do_t to_do)
{
  const char* str ;
  int len ;

  assert((to_do != cmd_do_nothing) && cli->drawn
                                 && (qs_cp_nn(cli->cl) == qs_len_nn(cli->cl))) ;

  str = (to_do < cmd_do_count)
          ? cli_response[cli->dispatched ? 1 : 0][to_do]  : NULL ;
  assert(str != NULL) ;

  len = uty_cli_write_s(cli, str) ;

  if (cli->dispatched)
    {
      cli->extra_len = len ;
      uty_cli_write_n(cli, telnet_backspaces, len) ;
    }
  else
    {
      uty_cli_out_newline(cli) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Current prompt length
 */
extern ulen
uty_cli_prompt_len(vty_cli cli)
{
  return cli->prompt_len ;
} ;

/*------------------------------------------------------------------------------
 * Wipe the current console line -- if any.
 *
 * If it is known that some characters will immediately be written, then
 * passing the number of them as "len" will reduce the number of characters
 * that have to be sent.
 */
extern void
uty_cli_wipe(vty_cli cli, int len)
{
  int a ;
  int b ;

  if (!cli->drawn)
    return ;                    /* quit if already wiped        */

  assert(qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) ;

  /* Establish how much ahead and how much behind the cursor            */
  a = cli->extra_len ;
  b = cli->prompt_len ;

  if (!cli->more_wait)
    {
      a += qs_len_nn(cli->cl) - qs_cp_nn(cli->cl) ;
      b += qs_cp_nn(cli->cl) ;
    } ;

  /* Wipe anything ahead of the current position and ahead of new len   */
  if ((a + b) > len)
    uty_cli_out_wipe_n(cli, +a) ;

  /* Wipe anything behind current position, but ahead of new len        */
  if (b > len)
    {
      uty_cli_out_wipe_n(cli, -(b - len)) ;
      b = len ;         /* moved the cursor back        */
    } ;

  /* Back to the beginning of the line                                  */
  uty_cli_write_n(cli, telnet_backspaces, b) ;

  /* Nothing there any more                                             */
  cli->drawn = false ;
} ;

/*------------------------------------------------------------------------------
 * Draw prompt and entire command line, leaving current position where it
 * should be.
 *
 * If command line is currently drawn, this wipes and redraws.
 *
 * Otherwise, assumes is positioned at start of an empty line.
 *
 * Draws prompt according to the given 'node', except:
 *
 *   * if is closing, draw nothing -- wipes the current line
 *
 *   * if is more_wait, draw the "--more--" prompt
 *
 *   * if is dispatched, draw the vestigial prompt.
 *
 *     By the time the current command completes, the node may have changed, so
 *     the current prompt may be invalid.
 *
 * Sets: cli->drawn         = true
 *       cli->prompt_len    = length of prompt used
 *       cli->extra_len     = 0
 */
static void
uty_cli_draw(vty_cli cli)
{
  const char* prompt ;
  size_t l_len ;
  int    p_len ;

  /* Sort out what the prompt is.                                       */
  if (cli->vf->vin_state != vf_open)
    {
      prompt = "" ;
      p_len  = 0 ;
      l_len  = 0 ;
    }
  else if (cli->more_wait)
    {
      prompt = "--more--" ;
      p_len  = strlen(prompt) ;
      l_len  = 0 ;
    }
  else if (cli->dispatched)
    {
      /* If there is a queued command, the prompt is a minimal affair.  */
      prompt = "~ " ;
      p_len  = strlen(prompt) ;
      l_len  = qs_len_nn(cli->cl) ;
    }
  else
    {
      /* The prompt depends on the node, and is expected to include the
       * host name.
       *
       * Caches the prompt so doesn't re-evaluate it every time.
       *
       * If the host name changes, the cli_prompt_set flag is cleared.
       */
      node_type_t node = cli->context->node ;

      if ((node != cli->prompt_node) || (host.name_gen != cli->prompt_gen))
        {
          const char* prompt ;

          prompt = cmd_prompt(node) ;
          if (prompt == NULL)
            {
              zlog_err("vty %s has node %d", uty_get_name(cli->vf->vio), node) ;
              prompt = "%s ???: " ;
            } ;

          cli->prompt_for_node =
                            qs_printf(cli->prompt_for_node, prompt, host.name) ;

          cli->prompt_node = node ;
          cli->prompt_gen  = host.name_gen ;
        } ;

      prompt = qs_char(cli->prompt_for_node) ;
      p_len  = qs_len(cli->prompt_for_node) ;
      l_len  = qs_len_nn(cli->cl) ;
    } ;

  /* Now, if line is currently drawn, time to wipe it                   */
  if (cli->drawn)
    uty_cli_wipe(cli, p_len + l_len) ;

  /* Set about writing the prompt and the line                          */
  cli->drawn         = true ;
  cli->extra_len     = false ;

  cli->prompt_len    = p_len ;

  uty_cli_write(cli, prompt, p_len) ;

  if (l_len != 0)
    {
      if (cli->auth_context)
        uty_cli_write_n(cli, telnet_stars, l_len) ;
      else
        uty_cli_write(cli, qs_char(cli->cl), l_len) ;

      if (qs_cp_nn(cli->cl) < l_len)
        uty_cli_write_n(cli, telnet_backspaces, l_len - qs_cp_nn(cli->cl)) ;
    } ;
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
 * Expects the command line to have been drawn, so that what is in cli->cl
 * is what is on the screen.
 *
 * Process keystrokes until run out of stuff to do, or have a "command line"
 * that must now be executed.  Updates cli->cl.
 *
 * Processes the contents of the keystroke stream.  If exhausts that, will set
 * ready to read and return.  (To give some "sharing".)
 *
 * Returns: cmd_do_xxxx
 *
 * Note that will return cmd_do_eof or cmd_do_timed_out any number of times.
 */
static cmd_do_t
uty_cli_process(vty_cli cli)
{
  keystroke_t stroke ;
  uint8_t  u ;
  cmd_do_t to_do ;

  qs_copy(cli->cls, cli->cl) ;          /* current screen line          */

  assert(cli->drawn) ;

  /* Now process as much as possible of what there is                   */
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
              if (cmd_token_position(cli->parsed, cli->cl))
                uty_cli_insert (cli->cl, " ", 1) ;
              else
                uty_cli_complete_command(cli);
              break;

            case '?':
              if (cmd_token_position(cli->parsed, cli->cl))
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

  /* Tidy up and return where got to.                                   */

  if (to_do != cmd_do_nothing)
    uty_cli_eol(cli->cl) ;                   /* go to the end of the line    */

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
    uty_cli_out_wipe_n(cli, nlen - olen) ;
  else if (nlen > olen)
    uty_cli_write_n(cli, telnet_stars, nlen - olen) ;

  return to_do ;
} ;

/*------------------------------------------------------------------------------
 * Fetch next keystroke, reading from the file if required.
 *
 * Returns:  cmd_do_keystroke: have a keystroke  -- stroke != ks_null
 *           cmd_do_eof      : eof in keystream  -- stroke == knull_eof
 *           cmd_do_timed_out: timed_ keystream  -- stroke == knull_timed_out
 *           cmd_do_nothing  : nothing available -- stroke == knull_not_eof
 *
 * Note that will return cmd_do_eof or cmd_do_timed_out any number of times.
 *
 * Note that any I/O error is signalled to the command loop, and is passed
 * out of here as "nothing available".  If the command loop is running, it
 * will see the CMD_IO_ERROR signal and deal with the error.
 */
static cmd_do_t
uty_cli_get_keystroke(vty_cli cli, keystroke stroke)
{
  qassert(cli->vf->vin_state == vf_open) ;

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

          /* Deal with telnet command, so invisible to upper level      */
          uty_telnet_command(cli->vf, stroke, false) ;
        }
      else
        {
          int get ;

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

          get = uty_term_read(cli->vf) ;        /* sets eof in key_stream
                                                   if hit eof or error      */
          if ((get == 0) || (get == -1))
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
      qs_insert(cl, chars, n) ;
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

  qs_replace(cl, m, chars, n) ;

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
    qs_delete(cl, n) ;
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
 * Create cli->hist.
 */
static void
uty_cli_hist_make(vty_cli cli)
{
  cli->hist = vector_init_new(cli->hist, VTY_HIST_COUNT) ;
  vector_set_min_length(cli->hist, VTY_HIST_COUNT) ;
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
uty_cli_hist_add (vty_cli cli, qstring clx)
{
  qstring   hist_line ;
  int       h_prev ;

  VTY_ASSERT_LOCKED() ;

  if (cli->hist == NULL)
    uty_cli_hist_make(cli) ;            /* create if required   */

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

  if ((hist_line == NULL) || (qs_cmp_sig(hist_line, clx) == 0))
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
  hist_line = qs_copy(hist_line, clx) ;
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

  if (cli->hist == NULL)
    uty_cli_hist_make(cli) ;            /* create if required   */

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
        uty_out(cli->vf->vio, "  %s\n", qs_make_string(line));
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

static void uty_cli_complete_keyword(vty_cli cli, elstring keyword) ;
static void uty_cli_complete_list(vty_cli cli, vector item_v) ;

static void uty_cli_describe_list(vty_cli cli, vector item_v) ;
static void uty_cli_describe_line(vty_cli cli, uint str_width, const char* str,
                                  ulen str_len, const char* doc, ulen doc_len) ;
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

  parsed = cli->parsed ;

  /* Establish what items may be present at the current token position. */
  n_items = uty_cli_help_parse(cli) ;

  if      (n_items > 1)         /* render list of alternatives          */
    uty_cli_complete_list(cli, parsed->item_v) ;
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
            uty_cli_complete_keyword(cli, item->str) ;
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
    uty_cli_describe_list(cli, cli->parsed->item_v) ;

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
  cmd_return_code_t ret ;
  uint  n_items ;

  /* The preflight checks avoid getting into trouble doing command completion
   * on a line with comment
   */
  msg = cmd_help_preflight(cli->parsed) ;
  if (msg != NULL)
    {
      uty_cli_help_message(cli, msg) ;
      return 0 ;
    } ;

  /* Now see what the cmd_completion can come up with.                  */
  ret = cmd_completion(cli->parsed, cli->context) ;

  if (ret == CMD_ERR_PARSING)
    {
      if (cli->parsed->eloc >= 0)
        {
          uint eloc = cli->prompt_len + cli->parsed->eloc ;

          uty_cli_help_newline(cli) ;   /* clears cli_drawn etc.        */
          uty_cli_write_n(cli, telnet_dots, eloc) ;
          uty_cli_write_s(cli, "^") ;
        } ;

      uty_cli_help_message(cli, qs_make_string(cli->parsed->emess)) ;

      return 0 ;
    } ;

  /* Will now have 0, 1 or more items which match at the current
   * cursor token.
   */
  n_items = vector_length(cli->parsed->item_v) ;

  if (n_items == 0)
    uty_cli_help_message(cli, "command not recognised") ;

  return n_items ;
} ;

/*------------------------------------------------------------------------------
 * Can complete a keyword.
 */
static void
uty_cli_complete_keyword(vty_cli cli, elstring keyword)
{
  int pre, rep, ins, mov ;

  cmd_complete_keyword(cli->parsed, &pre, &rep, &ins, &mov) ;

  uty_cli_move(cli->cl, pre) ;          /* move to start of token */
  uty_cli_replace(cli->cl, rep, els_body_nn(keyword), els_len_nn(keyword)) ;

  assert(ins <= 2) ;
  if (ins > 0)
    uty_cli_insert(cli->cl, "  ", ins) ;

  uty_cli_move(cli->cl, mov) ;

  return ;
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
 * Show the command description.
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
 */
static void
uty_cli_describe_list(vty_cli cli, vector item_v)
{
  uint     i, str_width, doc_width, width ;

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
  width = uty_cli_width_to_use(cli) ;

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

              uty_cli_describe_line(cli, str_width, str, str_len, dp, np - dp) ;

              str_len = 0 ;         /* for 2nd and subsequent lines */

              dp = np ;             /* step past what just wrote    */
              while (*dp == ' ')
                ++dp ;              /* skip spaces                  */
            } ;
        } ;

      uty_cli_describe_line(cli, str_width, str, str_len, dp, ep - dp) ;
    } ;

  uty_cli_help_newline(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Show one description line.
 */
static void
uty_cli_describe_line(vty_cli cli, uint str_width, const char* str,
                                    ulen str_len, const char* doc, ulen doc_len)
{
  if ((str_len == 0) && (doc_len == 0))
    return ;            /* quit if nothing to say               */

  uty_cli_help_newline(cli) ;

  if (str_len > 0)
    {
      uty_cli_write(cli, "  ", 2) ;
      uty_cli_write(cli, str, str_len) ;
    }
  else
    str_width += 2 ;

  if (doc_len > 0)
    {
      ulen pad ;
      pad = (str_len < str_width) ? str_width - str_len : 0 ;
      uty_cli_write_n(cli, telnet_spaces, pad + 2) ;
      uty_cli_write(cli, doc, doc_len) ;
    } ;
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
 * Deals with udating the command line if we are currently on it.
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
  if (!cli->drawn)
    {
      uty_cli_draw(cli) ;
      qs_copy(cli->cls, cli->cl) ;
    } ;
} ;

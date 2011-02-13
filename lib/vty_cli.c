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
 * Construct and destroy CLI object
 *
 *
 *
 */
static bool uty_cli_iac_callback(keystroke_iac_callback_args) ;
static void uty_cli_update_more(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Construct and initialise a new CLI object -- never embedded.
 *
 * The CLI is started up in the state it is in waiting for a command to
 * complete.  This means that all start-up messages etc. are handled as the
 * output from an implied start command.  uty_cli_start() is called when the
 * start-up messages etc. are complete.
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
   *   vf           = NULL           -- set below
   *
   *   hist         = empty vector (embedded) -- set up when first used.
   *   hp           = 0              -- hp == h_now => in the present ...
   *   h_now        = 0              --    ... see uty_cli_hist_add()
   *
   *   width        = 0              -- unknown width  ) Telnet window size
   *   height       = 0              -- unknown height )
   *
   *   lines        = 0              -- set below
   *   lines_set    = false          -- set below
   *
   *   monitor      = false          -- not a "monitor"
   *   monitor_busy = false          -- so not busy either
   *
   *   v_timeout    = ???
   *
   *   key_stream   = X              -- set below
   *
   *   drawn        = false
   *   dirty        = false
   *
   *   prompt_len   = 0              -- not drawn, in any case
   *   extra_len    = 0              -- not drawn, in any case
   *
   *   echo_suppress = false
   *
   *   prompt_node  = NULL_NODE      -- so not set !
   *   prompt_gen   = 0              -- not a generation number
   *   prompt_for_node = empty qstring (embedded)
   *
   *   password_fail = 0             -- so far, so good.
   *
   *   blocked      = false          -- see below
   *   in_progress  = false          -- see below
   *   out_active   = false
   *
   *   more_wait    = false
   *   more_active  = false
   *
   *   out_done     = false          -- not that it matters
   *
   *   more_enabled = false          -- not in "--More--" state
   *
   *   node         = NULL_NODE      -- set in vty_cli_start()
   *   to_do        = cmd_do_nothing
   *
   *   cl           = NULL qstring   -- set below
   *   clx          = NULL qstring   -- set below
   *
   *   parsed       = all zeros      -- empty parsed object
   *   cbuf         = empty fifo (embedded)
   *
   *   olc          = empty line control (embedded)
   */
  confirm(VECTOR_INIT_ALL_ZEROS) ;              /* hist                 */
  confirm(QSTRING_INIT_ALL_ZEROS) ;             /* prompt_for_node      */
  confirm(NULL_NODE == 0) ;                     /* prompt_node & node   */
  confirm(cmd_do_nothing == 0) ;                /* to_do                */
  confirm(VIO_FIFO_INIT_ALL_ZEROS) ;            /* cbuf                 */
  confirm(VIO_LINE_CONTROL_INIT_ALL_ZEROS) ;    /* olc                  */
  confirm(CMD_PARSED_INIT_ALL_ZEROS) ;          /* parsed               */

  cli->vf = vf ;

  /* Allocate and initialise a keystroke stream     TODO: CSI ??        */
  cli->key_stream = keystroke_stream_new('\0', uty_cli_iac_callback, cli) ;

  /* Set up cl and clx qstrings and the command line output fifo        */
  cli->cl  = qs_init_new(NULL, 120) ;   /* reasonable line length       */
  cli->clx = qs_init_new(NULL, 120) ;

  vio_fifo_init_new(cli->cbuf, 500) ;   /* just for the CLI stuff       */

  /* Use global 'lines' setting, as default -- but 'lines_set' false.   */
  uty_cli_set_lines(cli, host.lines, false) ;
  uty_cli_update_more(cli) ;    /* and update the olc etc to suit.      */

  /* Ready to be started -- out_active & more_wait are false.           */
  cli->blocked      = true ;
  cli->in_progress  = true ;

  return cli ;
} ;

/*------------------------------------------------------------------------------
 * Start the CLI.
 *
 * The implied start "command" is complete and everything is ready to start
 * the CLI.
 *
 * Note that before releasing any pending output, calls
 *
 * Returns: write_ready -- so the first event is a write event, to flush
 *                         any output to date.
 */
extern void
uty_cli_start(vty_cli cli, node_type_t node)
{
  uty_cli_done_command(cli, node) ;     /* implied push output          */
} ;

/*------------------------------------------------------------------------------
 * Close CLI object, if any -- may be called more than once.
 *
 * Shuts down and discards anything to do with the input.
 *
 * Revokes anything that can be revoked, which may allow output to proceed
 * and the vty to clear itself down.
 *
 * If in_progress and not final, keeps the cbuf, the olc and clx if in_progress.
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

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* Revoke any command that is in flight.                              */
  cq_revoke(cli->vf->vio->vty) ;

  /* Bring as much of the command handling to a stop as possible, and
   * turn off "---more--" etc. so that output can proceed.
   */
  cli->more_enabled = false ;
  cli->lines        = 0 ;       /* so will not be re-enabled            */

  uty_cli_wipe(cli, 0) ;        /* wipe anything the CLI has on screen  */

  cli->more_wait   = false ;    /* exit more_wait (if was in it)        */
  cli->more_active = false ;    /* and so cannot be this                */

  cli->out_active = true ;      /* if there is any output, it can go    */

  /* Ream out the history.                                              */
  {
    qstring line ;
    while ((line = vector_ream(cli->hist, keep_it)) != NULL)
      qs_reset(line, free_it) ;
  } ;

  /* Empty the keystroke handling.                                      */
  cli->key_stream = keystroke_stream_free(cli->key_stream) ;

  /* Can discard active command line if not in_progress.                */
  if (!cli->in_progress || final)
    cli->clx = qs_reset(cli->clx, free_it) ;

  /* Can discard parsed object.                                         */
  cmd_parsed_reset(cli->parsed, keep_it) ;

  /* If final, free the CLI object.                                     */
  if (final)
    {
      qs_reset(cli->prompt_for_node, keep_it) ;
      cli->cl = qs_reset(cli->cl, free_it) ;

      vio_fifo_reset(cli->cbuf, keep_it) ;  /* embedded fifo            */
      vio_lc_reset(cli->olc, keep_it) ;     /* embedded line control    */

      XFREE(MTYPE_VTY_CLI, cli) ;               /* sets cli = NULL      */
  } ;

  return cli ;
} ;

/*------------------------------------------------------------------------------
 * The keystroke iac callback function.
 *
 * This deals with IAC sequences that should be dealt with as soon as they
 * are read -- not stored in the keystroke stream for later processing.
 */
static bool
uty_cli_iac_callback(keystroke_iac_callback_args)
{
  return uty_telnet_command(((vty_cli)context)->vf, stroke, true) ;
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
 * not executed in the cli.  [For legacy threads the event queue is used.  If
 * that is too slow, a priority event queue will have to be invented.]
 *
 * State of the CLI:
 *
 *   in_progress  -- a command has been dispatched and has not yet completed.
 *
 *                   The command may be a in_pipe, so there may be many commands
 *                   to be completed before the CLI level command is.
 *
 *                   or: the CLI has been closed.
 *
 *   blocked      -- is in_progress and a further command is now ready to be
 *                   dispatched.
 *
 *                   or: the CLI has been closed.
 *
 *   out_active   -- the command output FIFO is being emptied.
 *
 *                   This is set when a command completes, and cleared when
 *                   everything is written away.
 *
 *                   Note that in this context a command is an individual
 *                   command -- where in_progress may cover any number of
 *                   command if the CLI level command is an in_pipe.
 *
 *   more_wait    -- is in "--more--" wait state
 *
 *   more_active  -- is in "--more--" wait state, and the "--more--" prompt
 *                   has not been written away, yet.
 *
 * The following are the valid combinations:
 *
 *     in_p: blkd: o_ac: m_wt: m_ac:
 *     ----:-----:-----:-----:-----:-----------------------------------------
 *       0 :   0 :   0 :   0 :   0 : collecting a new command
 *       0 :   0 :   1 :   0 :   0 : waiting for command output to finish
 *       1 :   0 :   X :   0 :   0 : command dispatched
 *       1 :   1 :   X :   0 :   0 : waiting for command to complete
 *       X :   X :   0 :   1 :   1 : waiting for "--more--" to be written away
 *       X :   X :   0 :   1 :   0 : waiting for "--more--" response
 *       1 :   1 :   1 :   0 :   0 : waiting for command to complete, after the
 *                                                           CLI has been closed
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
 * to be written away when the command completes.
 *
 * Note that only sets read on when the keystroke stream is empty and has not
 * yet hit eof.  The CLI process is driven mostly by write_ready -- which
 * invokes read_ready.
 *
 * Note also that after each command dispatch the CLI processor exits, to be
 * re-entered again on write_ready/read_ready -- so does one command line at
 * a time, yielding the processor after each one.
 *
 * Note that select/pselect treat a socket which is at "EOF", or has seen an
 * error, or has been half closed, etc. as readable and writable.  This means
 * that the CLI will continue to move forward even after the socket is no
 * longer delivering any data.
 *
 *------------------------------------------------------------------------------
 * The "--more--" handling.
 *
 * This is largely buried in the output handling.
 *
 * When a command completes and its output is pushed to written away,
 * out_active will be set (and any command line will be wiped).  The output
 * process is then kicked.
 *
 * The output process used the line_control structure to manage the output, and
 * occasionally enter the trivial "--more--" CLI.  This is invisible to the
 * main CLI.  (See the more_wait and more_active flags and their handling.)
 *
 * When all the output has completed the out_active flag is cleared and the CLI
 * will be kicked.
 *
 * It is expected that the output will end with a newline -- so that when the
 * CLI is kicked, the cursor will be at the start of an empty line.
 *
 * If the user decides to abandon output at the "--more--" prompt, then the
 * contents of the command output FIFO are discarded.
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
static enum vty_readiness uty_cli_more_wait(vty_cli cli) ;
static void uty_cli_draw(vty_cli cli) ;
static void uty_cli_draw_this(vty_cli cli, node_type_t node) ;

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

  if (cli->vf->vin_state != vf_open)
    return not_ready ;          /* Nothing more from CLI if closing     */

  /* Standard or "--more--" CLI ?                                       */
  if (cli->more_wait)
    return uty_cli_more_wait(cli) ;
  else
    return uty_cli_standard(cli) ;
} ;

/*==============================================================================
 * The Standard CLI
 */

static cmd_do_t uty_cli_process(vty_cli cli, node_type_t node) ;
static void uty_cli_response(vty_cli cli, cmd_do_t cmd_do) ;


static void uty_cli_dispatch(vty_cli cli) ;
static cmd_do_t uty_cli_command(vty_cli cli) ;
static void uty_cli_hist_add (vty_cli cli, qstring clx) ;


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
  VTY_ASSERT_LOCKED() ;

  assert(!cli->more_wait) ;     /* cannot be here in more_wait state !  */

  /* cli_blocked is set when is waiting for a command, or some output to
   * complete.
   *
   * NB: in both these cases, assumes that other forces are at work to
   *     keep things moving.
   */
  if (cli->blocked || cli->out_active)
    return not_ready ;

  /* If there is nothing pending, then can run the CLI until there is
   * something to do, or runs out of input.
   *
   * If there is something to do, that is because a previous command has
   * now completed, which may have wiped the pending command or changed
   * the required prompt.
   */
  if (cli->to_do == cmd_do_nothing)
    cli->to_do = uty_cli_process(cli, cli->node) ;
  else
    uty_cli_draw_this(cli, cli->node) ;

  /* If have something to do, do it if we can.                          */
  if (cli->to_do != cmd_do_nothing)
    {
      /* Reflect immediate response                                     */
      uty_cli_response(cli, cli->to_do) ;

      /* If command not already in progress, dispatch this one, which may
       * set the CLI blocked.
       *
       * Otherwise is now blocked until queued command completes.
       */
      if (cli->in_progress)
        cli->blocked = true ;   /* blocked waiting for previous         */
      else
        uty_cli_dispatch(cli) ; /* can dispatch the latest              */
    } ;

  /* Use write_ready as a proxy for read_ready on the keystroke stream.
   *
   * Also, if the command line is not drawn, then return write_ready, so
   * that
   *
   * Note that if has just gone cli_blocked, still returns ready.  This is
   * defensive: at worst will generate one unnecessary read_ready/write_ready
   * event.
   */
  if (keystroke_stream_empty(cli->key_stream))
    return read_ready ;
  else
    return write_ready ;
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
 */
static void
uty_cli_dispatch(vty_cli cli)
{
  qstring   tmp ;
  cmd_do_t  to_do_now ;

  vty_io  vio = cli->vf->vio ;

  VTY_ASSERT_LOCKED() ;

  /* About to dispatch a command, so must be in the following state.    */
  assert(!cli->in_progress && !cli->out_active && !cli->blocked) ;
  assert(cli->node == vio->vty->node) ;

  /* Set cli->clx to the command about to execute & pick up cli->to_do.
   *
   * Clear cli->cl  and cli->to_do.
   */
  tmp      = cli->clx ;                 /* swap clx and cl              */
  cli->clx = cli->cl ;
  cli->cl  = tmp ;

  to_do_now = cli->to_do ;              /* current operation            */

  cli->to_do = cmd_do_nothing ;         /* clear                        */
  qs_clear(cli->cl) ;                   /* set cl empty                 */

  /* Reset the command output FIFO and line_control TODO                */
//uty_out_clear(cli->vio) ;             /* clears FIFO and line control */

  /* Dispatch command                                                       */
  if      ((cli->node == AUTH_NODE)        && (to_do_now != cmd_do_nothing))
    to_do_now |= cmd_do_auth ;
  else if ((cli->node == AUTH_ENABLE_NODE) && (to_do_now != cmd_do_nothing))
    to_do_now |= cmd_do_auth_enable ;
  else
    {
      /* All other nodes...                                                 */
      switch (to_do_now)
      {
        case cmd_do_nothing:
        case cmd_do_ctrl_c:
        case cmd_do_eof:
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
      cli->in_progress = true ;
      uty_cmd_dispatch(vio, to_do_now, cli->clx) ;
    }
  else
    uty_cli_draw(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Enqueue command -- adding to history -- if is not empty or just comment
 *
 * This is for VTY_TERM type VTY.
 *
 * Returns: CMD_SUCCESS  => empty command line (or just comment)
 *          CMD_WAITING  => enqueued for parse and execute
 */
static cmd_do_t
uty_cli_command(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (cmd_is_empty(qs_els_nn(cli->clx)))
    return cmd_do_nothing ;     /* easy when nothing to do !            */

  /* Add not empty command to history                                   */
  uty_cli_hist_add(cli, cli->clx) ;

  return cmd_do_command ;
} ;

/*------------------------------------------------------------------------------
 * A command has completed, and there is (or may be) some output to now
 * write away.
 */
extern void
uty_cli_out_push(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  if (cli->more_wait)
    return ;                    /* can do nothing                       */

  if (vio_fifo_empty(cli->vf->obuf))
    {
      assert(!cli->out_active ) ;
      return ;                  /* need do nothing if is empty          */
    } ;

  uty_cli_wipe(cli, 0) ;        /* wipe any partly constructed line     */

  cli->out_active   = true ;    /* enable the output                    */

  uty_term_set_readiness(cli->vf, write_ready) ;
                                /* kick the write side into action      */
} ;

/*------------------------------------------------------------------------------
 * Queued command has completed.
 *
 * Note that sets write on whether there is anything in the output buffer
 * or not... write_ready will kick read_ready.


 * Command has completed, so:
 *
 *   * clear cmd_in_progress
 *   * set   cmd_out_active -- so any output can now proceed
 *   * set   cli_blocked    -- waiting for output to complete
 *   * and prepare the line control for output
 *
 * If the return is CMD_CLOSE, then also now does the required half close.
 *
 * Note that apart from CMD_CLOSE, don't really care what the return was.  Any
 * diagnostics or other action must be dealt with elsewhere (as part of the
 * command execution.
 *
 * Note that everything proceeds as if there is some output.  So after every
 * command goes through at least one write_ready event.
 *
 * This ensures some multiplexing at the command level.
 *
 * It also means that the decision about whether there is anything to output
 * is left to the output code.



 */
extern void
uty_cli_done_command(vty_cli cli, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;

  uty_cli_out_push(cli) ;       /* just in case                         */

  cli->in_progress  = false ;   /* command complete                     */
  cli->blocked      = false ;   /* releases possibly blocked command    */
  cli->node         = node ;    /* and now in this node                 */

  /* Reach all the way back, and get the now current node.              */
  cli->node = cli->vf->vio->vty->node ;

  uty_term_set_readiness(cli->vf, write_ready) ;
                                /* kick the write side into action      */
} ;

/*==============================================================================
 * All commands are dispatched to command_queue.
 *
 * Some commands are VLI specific... and end up back here.
 */

/*------------------------------------------------------------------------------
 * Authentication of vty
 *
 * Note that if the AUTH_NODE password fails too many times, the terminal is
 * closed.
 *
 * Returns: CMD_SUCCESS  -- OK, one way or another
 *          CMD_WARNING  -- with error message sent to output
 *          CMD_CLOSE    -- too many password failures
 */
extern cmd_return_code_t
uty_cli_auth(vty_cli cli)
{
  char *crypt (const char *, const char *);

  char*         passwd    = NULL ;
  bool          encrypted = false ;
  node_type_t   next_node = 0 ;
  cmd_return_code_t  ret ;
  vty_io        vio ;
  cmd_exec      exec ;

  VTY_ASSERT_LOCKED() ;

  vio  = cli->vf->vio ;
  exec = vio->vty->exec ;

  /* Deal with the exotic terminators.                                  */
  switch (exec->to_do & cmd_do_mask)
  {
    case cmd_do_nothing:
    case cmd_do_ctrl_c:
    case cmd_do_ctrl_z:
      return CMD_SUCCESS ;

    case cmd_do_command:
      break ;

    case cmd_do_ctrl_d:
    case cmd_do_eof:
      return uty_cmd_close(vio, "End") ;

    default:
      zabort("unknown or invalid cmd_do") ;
  } ;

  /* Ordinary command dispatch -- see if password is OK.
   *
   * Select the password we need to check against.
   */
  if      ((exec->to_do & ~cmd_do_mask) == cmd_do_auth)
    {
      passwd    = host.password ;
      encrypted = host.password_encrypted ;

      if (host.advanced)
        next_node = host.enable ? VIEW_NODE : ENABLE_NODE;
      else
        next_node = VIEW_NODE;
    }
  else if ((exec->to_do & ~cmd_do_mask) == cmd_do_auth_enable)
    {
      passwd    = host.enable ;
      encrypted = host.enable_encrypted ;

      next_node = ENABLE_NODE;
    }
  else
    zabort("unknown to_do_value") ;

  /* Check against selected password (if any)                           */
  if (passwd != NULL)
    {
      char* candidate = qs_make_string(exec->line) ;

      if (encrypted)
        candidate = crypt(candidate, passwd) ;

      if (strcmp(candidate, passwd) == 0)
        {
          cli->password_fail = 0 ;      /* forgive any recent failures  */
          vio->vty->node = next_node;

          return CMD_SUCCESS ;          /* <<< SUCCESS <<<<<<<<         */
        } ;
    } ;

  /* Password failed -- or none set !                                   */
  cli->password_fail++ ;

  ret = CMD_SUCCESS ;           /* OK so far                            */

  if (cli->password_fail >= 3)
    {
      if ((exec->to_do & ~cmd_do_mask) == cmd_do_auth)
        {
          ret = uty_cmd_close(vio, "Bad passwords, too many failures!") ;
        }
      else
        {
          /* AUTH_ENABLE_NODE                                       */
          cli->password_fail = 0 ;  /* allow further attempts       */
          uty_out(vio, "%% Bad enable passwords, too many failures!\n") ;
          vio->vty->node = host.restricted_mode ? RESTRICTED_NODE
                                                : VIEW_NODE ;
          ret = CMD_WARNING ;
        } ;
    } ;

  return ret ;
} ;

/*==============================================================================
 * The "--more--" CLI
 *
 * While command output is being cleared from its FIFO, the CLI is blocked.
 *
 * When the output side signals that "--more--" is required, it sets the
 * more_wait flag and clears the out_active flag.
 *
 * The first stage of handling "--more--" is to suck the input dry, so that
 * (as far as is reasonably possible) does not steal a keystroke as the
 * "--more--" response which was typed before the prompt was issued.
 *
 * The cli_blocked flag indicates that the CLI is in this first stage.
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

  assert(cli->out_active && !cli->more_wait) ;

  uty_cli_wipe(cli, 0) ;        /* make absolutely sure that command line is
                                   wiped before change the CLI state    */

  cli->out_active  = false ;    /* stop output pro tem                  */
  cli->more_wait   = true ;     /* new state                            */
  cli->more_active = true ;     /* drawing the "--more--"               */

  uty_cli_draw(cli) ;           /* draw the "--more--"                  */
} ;

/*------------------------------------------------------------------------------
 * Exit the "--more--" CLI.
 *
 * Wipes the "--more--" prompt.
 *
 * This is used when the user responds to the prompt.
 *
 * It is also used when the vty is "half-closed".  In this case, it is (just)
 * possible that the '--more--' prompt is yet to be completely written away,
 * so:
 *
 *   * assert that is either: !cli->blocked (most of the time it will)
 *                        or: !vio_fifo_empty(cli->cbuf)
 *
 *   * note that can wipe the prompt even though it hasn't been fully
 *     written away yet.  (The effect is to append the wipe action to the
 *     cli_obuf !)
 */
extern void
uty_cli_exit_more_wait(vty_cli cli)
{
  VTY_ASSERT_LOCKED() ;

  assert(cli->more_wait) ;

  uty_cli_wipe(cli, 0) ;        /* wipe the prompt ('--more--')
                                   before changing the CLI state        */

  cli->more_wait   = false ;    /* exit more_wait                       */
  cli->more_active = false ;    /* tidy                                 */
  cli->out_active  = true ;     /* re-enable output                     */
} ;

/*------------------------------------------------------------------------------
 * Handle the "--more--" state.
 *
 * Deals with the first stage if cli_blocked.
 *
 * Tries to steal a keystroke, and when succeeds wipes the "--more--"
 * prompt and exits cli_more_wait -- and may cancel all outstanding output.
 *
 * EOF on input causes immediate exit from cli_more_state.
 *
 * Returns: read_ready  -- waiting to steal a keystroke
 *          now_ready   -- just left cli_more_wait
 *          not_ready   -- otherwise
 */
static vty_readiness_t
uty_cli_more_wait(vty_cli cli)
{
  struct keystroke steal ;

  VTY_ASSERT_LOCKED() ;

  assert(cli->more_wait) ;      /* must be in more_wait state !         */

  /* Deal with the first stage of "--more--"                            */
  if (cli->more_active)
    {
      int  get ;

      /* If the CLI buffer is not yet empty, then is waiting for the
       * initial prompt to clear, so nothing to be done here.
       */
      if (!vio_fifo_empty(cli->cbuf))
        return write_ready ;

      cli->more_active = false ;

      /* empty the input buffer into the keystroke stream               */
      do
        {
          get = uty_term_read(cli->vf, NULL) ;
        } while (get > 0) ;
    } ;

  /* Go through the "--more--" process, unless closing                  */
  if (cli->vf->vout_state == vf_open)
    {
      /* The read fetches a reasonable lump from the I/O -- so if there
       * is a complete keystroke available, expect to get it.
       *
       * If no complete keystroke available to steal, returns ks_null.
       *
       * If has hit EOF (or error etc), returns knull_eof.
       */
      uty_term_read(cli->vf, &steal) ;

      /* If nothing stolen, make sure prompt is drawn and wait for more
       * input.
       */
      if ((steal.type == ks_null) && (steal.value != knull_eof))
        {
          if (uty_cli_draw_if_required(cli))    /* "--more--" if req.   */
            {
              cli->more_active = true ;  /* written "--more--" again    */
              return write_ready ;
            }
          else
            return read_ready ;
        } ;

      /* Stolen a keystroke -- a (very) few terminate all output        */
      if (steal.type == ks_char)
        {
          switch (steal.value)
          {
            case CONTROL('C'):
            case 'q':
            case 'Q':
              uty_out_clear(cli->vf->vio) ;
              break;

            default:                /* everything else, thrown away     */
              break ;
          } ;
        } ;
    } ;

  /* End of "--more--" process
   *
   * Wipe out the prompt and update state.
   *
   * Return write_ready to tidy up the screen and, unless cleared, write
   * some more.
   */
  uty_cli_exit_more_wait(cli) ;

  return now_ready ;
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
 * keyboard, or "monitor" output has filled the buffers, then may need to
 * have intermediate buffering.
 *
 * No actual I/O takes place here-- all "output" is to cli->cbuf
 *
 * The "cli_echo" functions discard the output if cli->echo_suppress.
 * This is used while passwords are entered and to allow command line changes
 * to be made while the line is not visible.
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

CONFIRM(sizeof(telnet_spaces)  == (sizeof(cli_rep_char) + 1)) ;
CONFIRM(sizeof(telnet_dots)    == (sizeof(cli_rep_char) + 1)) ;

static const char* telnet_newline = "\r\n" ;

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
 * Completely empty the cli command buffer
 */
extern void
uty_cli_out_clear(vty_cli cli)
{
  vio_fifo_clear(cli->cbuf, true) ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo user input
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE) or not write_open
 */
static void
uty_cli_echo(vty_cli cli, const char *this, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  if (!cli->echo_suppress)
    uty_cli_write(cli, this, len) ;
}

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo 'n' characters using a cli_rep_char string
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE)
 */
static void
uty_cli_echo_n(vty_cli cli, cli_rep_char chars, int n)
{
  VTY_ASSERT_LOCKED() ;

  if (!cli->echo_suppress)
    uty_cli_write_n(cli, chars, n) ;
}

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf write()
 */
extern void
uty_cli_write(vty_cli cli, const char *this, int len)
{
  VTY_ASSERT_LOCKED() ;

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
 * Standard Messages
 */

/*------------------------------------------------------------------------------
 * Send newline to the console.
 *
 * Clears the cli_drawn and the cli_dirty flags.
 */
extern void
uty_cli_out_newline(vty_cli cli)
{
  uty_cli_write(cli, telnet_newline, 2) ;
  cli->drawn = false ;
  cli->dirty = false ;
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
    [cmd_do_command]  = "",
    [cmd_do_ctrl_c]   = "^C",
    [cmd_do_ctrl_d]   = "^D",
    [cmd_do_ctrl_z]   = "^Z",
    [cmd_do_eof]      = "^*"
  },
  { /* when waiting for a previous command to complete          */
    [cmd_do_command]  = "^",
    [cmd_do_ctrl_c]   = "^C",
    [cmd_do_ctrl_d]   = "^D",
    [cmd_do_ctrl_z]   = "^Z",
    [cmd_do_eof]      = "^*"
  }
} ;

static void
uty_cli_response(vty_cli cli, cmd_do_t to_do)
{
  const char* str ;
  int len ;

  if ((to_do == cmd_do_nothing) || (cli->vf->vin_state != vf_open))
    return ;

  str = (to_do < cmd_do_count)
          ? cli_response[cli->in_progress ? 1 : 0][to_do]  : NULL ;
  assert(str != NULL) ;

  len = uty_cli_write_s(cli, str) ;

  if (cli->in_progress)
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
 * Send various messages with trailing newline.
 */
#if 0
static void
uty_cli_out_CMD_ERR_AMBIGUOUS(vty_cli cli)
{
  uty_cli_write_s(cli, "% " MSG_CMD_ERR_AMBIGUOUS ".") ;
  uty_cli_out_newline(cli) ;
} ;

static void
uty_cli_out_CMD_ERR_NO_MATCH(vty_cli cli)
{
  uty_cli_write_s(cli, "% " MSG_CMD_ERR_NO_MATCH ".") ;
  uty_cli_out_newline(cli) ;
} ;
#endif
/*==============================================================================
 * Command line draw and wipe
 */

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

  if (!cli->echo_suppress && !cli->more_wait)
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
  cli->dirty = false ;
} ;

/*------------------------------------------------------------------------------
 * If not currently drawn, draw prompt etc according to the current state
 * and node.
 *
 * See uty_cli_draw().
 */
extern bool
uty_cli_draw_if_required(vty_cli cli)
{
  if (cli->drawn)
    return false ;

  uty_cli_draw(cli) ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Draw prompt etc for the current vty node.
 *
 * See uty_cli_draw_this()
 */
static void
uty_cli_draw(vty_cli cli)
{
  uty_cli_draw_this(cli, cli->node) ;
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
 *   * if is cli_more_wait, draw the "--more--" prompt
 *
 *   * if is cmd_in_progress, draw the vestigial prompt.
 *
 *     By the time the current command completes, the node may have changed, so
 *     the current prompt may be invalid.
 *
 * Sets: cli_drawn         = true
 *       cli_dirty         = false
 *       cli_prompt_len    = length of prompt used
 *       cli_extra_len     = 0
 *       cli_echo_suppress = (AUTH_NODE or AUTH_ENABLE_NODE)
 */
static void
uty_cli_draw_this(vty_cli cli, enum node_type node)
{
  const char* prompt ;
  size_t l_len ;
  int    p_len ;

  if (cli->dirty)
    uty_cli_out_newline(cli) ;  /* clears cli_dirty and cli_drawn       */

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
  else if (cli->in_progress)
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
      if ((node != cli->prompt_node) || (host.name_gen != cli->prompt_gen))
        {
          const char* prompt ;

          prompt = cmd_prompt(node) ;
          if (prompt == NULL)
            {
              zlog_err("vty %s has node %d", uty_get_name(cli->vf->vio), node) ;
              prompt = "%s ???: " ;
            } ;

          qs_printf(cli->prompt_for_node, prompt, host.name);

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
  cli->echo_suppress = (node == AUTH_NODE || node == AUTH_ENABLE_NODE) ;

  cli->prompt_len = p_len ;

  uty_cli_write(cli, prompt, p_len) ;

  if (l_len != 0)
    {
      uty_cli_write(cli, qs_char(cli->cl), l_len) ;
      if (qs_cp_nn(cli->cl) < l_len)
        uty_cli_write_n(cli, telnet_backspaces, l_len - qs_cp_nn(cli->cl)) ;
    } ;
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
uty_cli_pre_monitor(vty_cli cli, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  uty_cli_wipe(cli, len) ;
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
extern int
uty_cli_post_monitor(vty_cli cli, const char* buf, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  if (len != 0)
    uty_cli_write(cli, buf, len) ;

  if (cli->more_wait || (qs_len_nn(cli->cl) != 0))
    {
      uty_cli_draw(cli) ;
      ++len ;
    } ;

  return len ;
} ;

/*==============================================================================
 * Command line processing loop
 */

static int uty_cli_insert (vty_cli cli, const char* chars, int n) ;
static int uty_cli_overwrite (vty_cli cli, char* chars, int n) ;
//static int uty_cli_word_overwrite (vty_cli cli, char *str) ;
static int uty_cli_forwards(vty_cli cli, int n) ;
static int uty_cli_backwards(vty_cli cli, int n) ;
static int uty_cli_del_forwards(vty_cli cli, int n) ;
static int uty_cli_del_backwards(vty_cli cli, int n) ;
static int uty_cli_bol (vty_cli cli) ;
static int uty_cli_eol (vty_cli cli) ;
static int uty_cli_word_forwards_delta(vty_cli cli) ;
static int uty_cli_word_forwards(vty_cli cli) ;
static int uty_cli_word_backwards_delta(vty_cli cli, int eat_spaces) ;
//static int uty_cli_word_backwards_pure (vty_cli cli) ;
static int uty_cli_word_backwards (vty_cli cli) ;
static int uty_cli_del_word_forwards(vty_cli cli) ;
static int uty_cli_del_word_backwards(vty_cli cli) ;
static int uty_cli_del_to_eol (vty_cli cli) ;
static int uty_cli_clear_line(vty_cli cli) ;
static int uty_cli_transpose_chars(vty_cli cli) ;
static void uty_cli_hist_use(vty_cli cli, int step) ;
static void uty_cli_hist_next(vty_cli cli) ;
static void uty_cli_hist_previous(vty_cli cli) ;
static void uty_cli_complete_command (vty_cli cli, node_type_t node) ;
static void uty_cli_describe_command (vty_cli cli, node_type_t node) ;

/*------------------------------------------------------------------------------
 * Fetch next keystroke, reading from the file if required.
 */
static bool
uty_cli_get_keystroke(vty_cli cli, keystroke stroke)
{
  int got ;

  do
    {
      if (keystroke_get(cli->key_stream, stroke))
        return true ;

      got = uty_term_read(cli->vf, NULL) ;    /* not stealing */
    } while (got > 0) ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Process keystrokes until run out of input, or get something to cmd_do.
 *
 * If required, draw the prompt and command line.
 *
 * Process keystrokes until run out of stuff to do, or have a "command line"
 * that must now be executed.
 *
 * Processes the contents of the keystroke stream.  If exhausts that, will set
 * ready to read and return.  (To give some "sharing".)
 *
 * Returns: cmd_do_xxxx
 *
 * When returns the cl is '\0' terminated.
 */
static cmd_do_t
uty_cli_process(vty_cli cli, node_type_t node)
{
  struct   keystroke stroke ;
  uint8_t  u ;
  bool     auth ;
  cmd_do_t to_do ;

  auth = (node == AUTH_NODE || node == AUTH_ENABLE_NODE) ;

  /* Now process as much as possible of what there is                   */
  to_do = cmd_do_nothing ;
  while (to_do == cmd_do_nothing)
    {
      if (!cli->drawn)
        uty_cli_draw_this(cli, node) ;

      if (!uty_cli_get_keystroke(cli, &stroke))
        {
          to_do = (stroke.value == knull_eof) ? cmd_do_eof : cmd_do_nothing ;
          break ;
        } ;

      if (stroke.flags != 0)
        {
          /* TODO: deal with broken keystrokes                          */
          continue ;
        } ;

      switch (stroke.type)
      {
        /* Straightforward character -----------------------------------*/
        /* Note: only interested in 8-bit characters !                  */
        case ks_char:
          u = (uint8_t)stroke.value ;

          switch (stroke.value)
          {
            case CONTROL('A'):
              uty_cli_bol (cli);
              break;

            case CONTROL('B'):
              uty_cli_backwards(cli, 1);
              break;

            case CONTROL('C'):
              to_do = cmd_do_ctrl_c ;   /* Exit on ^C ..................*/
              break ;

            case CONTROL('D'):
              if (auth)
                to_do = cmd_do_ctrl_d ; /* Exit on ^D ..................*/
              else
                uty_cli_del_forwards(cli, 1);
              break;

            case CONTROL('E'):
              uty_cli_eol (cli);
              break;

            case CONTROL('F'):
              uty_cli_forwards(cli, 1);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_backwards(cli, 1);
              break;

            case CONTROL('K'):
              uty_cli_del_to_eol (cli);
              break;

            case CONTROL('N'):
              uty_cli_hist_next (cli);
              break;

            case CONTROL('P'):
              uty_cli_hist_previous(cli);
              break;

            case CONTROL('T'):
              uty_cli_transpose_chars (cli);
              break;

            case CONTROL('U'):
              uty_cli_clear_line(cli);
              break;

            case CONTROL('W'):
              uty_cli_del_word_backwards (cli);
              break;

            case CONTROL('Z'):
              to_do = cmd_do_ctrl_z ;   /* Exit on ^Z ..................*/
              break;

            case '\n':
            case '\r':
              to_do = cmd_do_command ;  /* Exit on CR or LF.............*/
              break ;

            case '\t':
              if (auth)
                uty_cli_insert (cli, " ", 1) ;
              else if (cmd_token_position(cli->parsed, cli->cl))
                uty_cli_insert (cli, " ", 1) ;
              else
                uty_cli_complete_command (cli, node);
              break;

            case '?':
              if       (auth)
                uty_cli_insert (cli, (char*)&u, 1) ;
              else if (cmd_token_position(cli->parsed, cli->cl))
                uty_cli_insert (cli, (char*)&u, 1) ;
              else
                uty_cli_describe_command (cli, node);
              break;

            default:
              if ((stroke.value >= 0x20) && (stroke.value < 0x7F))
                uty_cli_insert (cli, (char*)&u, 1) ;
              break;
            }
          break ;

        /* ESC X -------------------------------------------------------------*/
        case ks_esc:
          switch (stroke.value)
          {
            case 'b':
              uty_cli_word_backwards (cli);
              break;

            case 'f':
              uty_cli_word_forwards (cli);
              break;

            case 'd':
              uty_cli_del_word_forwards (cli);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_word_backwards (cli);
              break;

            default:
              break;
          } ;
          break ;

        /* ESC [ X -----------------------------------------------------------*/
        case ks_csi:
          if (stroke.len != 0)
            break ;             /* only recognise 3 byte sequences      */

          switch (stroke.value)
          {
            case ('A'):         /* up arrow     */
              uty_cli_hist_previous(cli);
              break;

            case ('B'):         /* down arrow   */
              uty_cli_hist_next (cli);
              break;

            case ('C'):         /* right arrow  */
              uty_cli_forwards(cli, 1);
              break;

            case ('D'):         /* left arrow   */
              uty_cli_backwards(cli, 1);
              break;

            default:
              break ;
          } ;
          break ;

        /* Telnet Command ----------------------------------------------------*/
        case ks_iac:
          uty_telnet_command(cli->vf, &stroke, false) ;
          break ;

        /* Unknown -----------------------------------------------------------*/
        default:
          zabort("unknown keystroke type") ;
      } ;

    } ;

  /* Tidy up and return where got to.                                   */

  if (to_do != cmd_do_nothing)
    uty_cli_eol (cli) ;         /* go to the end of the line    */

  return to_do ;
} ;

/*==============================================================================
 * Command line operations
 */

/*------------------------------------------------------------------------------
 * Insert 'n' characters at current position in the command line
 *
 * Returns number of characters inserted -- ie 'n'
 */
static int
uty_cli_insert (vty_cli cli, const char* chars, int n)
{
  int after ;

  VTY_ASSERT_LOCKED() ;

  assert((qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) && (n >= 0)) ;

  if (n <= 0)
    return n ;          /* avoid trouble        */

  after = qs_insert(cli->cl, chars, n) ;

  uty_cli_echo(cli, qs_cp_char(cli->cl), after) ;

  if ((after - n) != 0)
    uty_cli_echo_n(cli, telnet_backspaces, after - n) ;

  qs_move_cp_nn(cli->cl, n) ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Overstrike 'n' characters at current position in the command line
 *
 * Move current position forwards.
 *
 * Returns number of characters inserted -- ie 'n'
 */
static int
uty_cli_overwrite (vty_cli cli, char* chars, int n)
{
  VTY_ASSERT_LOCKED() ;

  assert((qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) && (n >= 0)) ;

  if (n > 0)
    {
      qs_replace(cli->cl, n, chars, n) ;
      uty_cli_echo(cli, chars, n) ;

      qs_move_cp_nn(cli->cl, n) ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Replace 'm' characters at the current position, by 'n' characters and leave
 * cursor at the end of the inserted characters.
 *
 * Returns number of characters inserted -- ie 'n'
 */
static int
uty_cli_replace(vty_cli cli, int m, const char* chars, int n)
{
  int a, b ;

  VTY_ASSERT_LOCKED() ;

  assert((qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) && (n >= 0) && (m >= 0)) ;

  b = qs_len_nn(cli->cl) - qs_cp_nn(cli->cl) ;
  a = qs_replace(cli->cl, m, chars, n) ;

  uty_cli_echo(cli, qs_cp_char(cli->cl), a) ;

  if (a < b)
    uty_cli_echo_n(cli, telnet_spaces, b - a) ;
  else
    b = a ;

  if (b > n)
    uty_cli_echo_n(cli, telnet_backspaces, b - n) ;

  qs_move_cp_nn(cli->cl, n) ;

  return n ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * Insert a word into vty interface with overwrite mode.
 *
 * NB: Assumes result will then be the end of the line.
 *
 * Returns number of characters inserted -- ie length of string
 */
static int
uty_cli_word_overwrite (vty_cli cli, char *str)
{
  int n ;
  VTY_ASSERT_LOCKED() ;

  n = uty_cli_overwrite(cli, str, strlen(str)) ;

  qs_set_len_nn(cli->cl, qs_cp_nn(cli->cl)) ;

  return n ;
}
#endif

/*------------------------------------------------------------------------------
 * Forward 'n' characters -- stop at end of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_forwards(vty_cli cli, int n)
{
  int have ;
  VTY_ASSERT_LOCKED() ;

  have = qs_after_cp_nn(cli->cl) ;
  if (have < n)
    n = have ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cli_echo(cli, qs_cp_char(cli->cl), n) ;
      qs_move_cp_nn(cli->cl, n) ;
    } ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Backwards 'n' characters -- stop at start of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_backwards(vty_cli cli, int n)
{
  VTY_ASSERT_LOCKED() ;

  if ((int)qs_cp_nn(cli->cl) < n)
    n = qs_cp_nn(cli->cl) ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cli_echo_n(cli, telnet_backspaces, n) ;
      qs_move_cp_nn(cli->cl, -n) ;
    } ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Move forwards (if n > 0) or backwards (if n < 0) -- stop at start or end of
 * line.
 *
 * Returns number of characters actually moved -- signed
 */
static int
uty_cli_move(vty_cli cli, int n)
{
  VTY_ASSERT_LOCKED() ;

  if (n < 0)
    return - uty_cli_backwards(cli, -n) ;

  if (n > 0)
    return + uty_cli_forwards(cli, +n) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Delete 'n' characters -- forwards -- stop at end of line.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cli_del_forwards(vty_cli cli, int n)
{
  int after ;
  int have ;

  VTY_ASSERT_LOCKED() ;

  have = qs_after_cp_nn(cli->cl) ;
  if (have < n)
    n = have ;          /* cannot delete more than have */

  assert(n >= 0) ;

  if (n <= 0)
    return 0 ;

  after = qs_delete(cli->cl, n) ;

  if (after > 0)
    uty_cli_echo(cli, qs_cp_char(cli->cl), after) ;

  uty_cli_echo_n(cli, telnet_spaces,     n) ;
  uty_cli_echo_n(cli, telnet_backspaces, after + n) ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters before the point.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cli_del_backwards(vty_cli cli, int n)
{
  return uty_cli_del_forwards(cli, uty_cli_backwards(cli, n)) ;
}

/*------------------------------------------------------------------------------
 * Move to the beginning of the line.
 *
 * Returns number of characters moved over.
 */
static int
uty_cli_bol (vty_cli cli)
{
  return uty_cli_backwards(cli, qs_cp_nn(cli->cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Move to the end of the line.
 *
 * Returns number of characters moved over
 */
static int
uty_cli_eol (vty_cli cli)
{
  return uty_cli_forwards(cli, qs_after_cp_nn(cli->cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Forward word delta -- distance to start of next word.
 *
 * Return number of characters to step over to reach next word.
 *
 * Steps over non-space characters and then any spaces.
 */
static int
uty_cli_word_forwards_delta(vty_cli cli)
{
  char* cp ;
  char* tp ;
  char* ep ;

  VTY_ASSERT_LOCKED() ; ;

  assert(qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) ;

  cp = qs_cp_char(cli->cl) ;
  ep = qs_ep_char(cli->cl) ;

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
static int
uty_cli_word_forwards(vty_cli cli)
{
  return uty_cli_forwards(cli, uty_cli_word_forwards_delta(cli)) ;
} ;

/*------------------------------------------------------------------------------
 * Backward word delta -- distance to start of next word, back.
 *
 * Return number of characters to step over to reach next word.
 *
 * If "eat_spaces", starts by stepping over spaces.
 * Steps back until next (backwards) character is space, or hits start of line.
 */
static int
uty_cli_word_backwards_delta(vty_cli cli, int eat_spaces)
{
  char* cp ;
  char* tp ;
  char* sp ;

  VTY_ASSERT_LOCKED() ; ;

  assert(qs_cp_nn(cli->cl) <= qs_len_nn(cli->cl)) ;

  cp = qs_cp_char(cli->cl) ;
  sp = qs_char(cli->cl) ;

  tp = cp ;

  if (eat_spaces)
    while ((tp > sp) && (*(tp - 1) == ' '))
      --tp ;

  while ((tp > sp) && (*(tp - 1) != ' '))
    --tp ;

  return cp - tp ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * Backward word, but not trailing spaces.
 *
 * Move back until next (backwards) character is space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cli_word_backwards_pure (vty_cli cli)
{
  return uty_cli_backwards(cli, uty_cli_word_backwards_delta(cli, 0)) ;
} ;
#endif

/*------------------------------------------------------------------------------
 * Backward word -- move to start of previous word.
 *
 * Moves past any spaces, then move back until next (backwards) character is
 * space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cli_word_backwards (vty_cli cli)
{
  return uty_cli_backwards(cli, uty_cli_word_backwards_delta(cli, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Delete to end of word -- forwards.
 *
 * Deletes any leading spaces, then deletes upto next space or end of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_word_forwards(vty_cli cli)
{
  return uty_cli_del_forwards(cli, uty_cli_word_forwards_delta(cli)) ;
}

/*------------------------------------------------------------------------------
 * Delete to start of word -- backwards.
 *
 * Deletes any trailing spaces, then deletes upto next space or start of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_word_backwards(vty_cli cli)
{
  return uty_cli_del_backwards(cli, uty_cli_word_backwards_delta(cli, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill rest of line from current point.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_to_eol (vty_cli cli)
{
  return uty_cli_del_forwards(cli, qs_after_cp_nn(cli->cl)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill line from the beginning.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_clear_line(vty_cli cli)
{
  uty_cli_bol(cli) ;
  return uty_cli_del_to_eol(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Transpose chars before or at the point.
 *
 * Return number of characters affected.
 */
static int
uty_cli_transpose_chars(vty_cli cli)
{
  char  chars[2] ;
  char* cp ;

  VTY_ASSERT_LOCKED() ;

  /* Give up if < 2 characters or at start of line.                     */
  if ((qs_len_nn(cli->cl) < 2) || (qs_cp_nn(cli->cl) < 1))
    return 0 ;

  /* Move back to first of characters to exchange                       */
  if (qs_cp_nn(cli->cl) == qs_len_nn(cli->cl))
    uty_cli_backwards(cli, 2) ;
  else
    uty_cli_backwards(cli, 1) ;

  /* Pick up in the new order                                           */
  cp = qs_cp_char(cli->cl) ;
  chars[1] = *cp++ ;
  chars[0] = *cp ;

  /* And overwrite                                                      */
  return uty_cli_overwrite(cli, chars, 2) ;
} ;

/*==============================================================================
 * Command line history handling
 *
 *   cli->hist   is vector of qstrings
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
 * Cannot step forwards from hp == h_now (into the future !).
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

/*------------------------------------------------------------------------------
 * Add given command line to the history buffer.
 *
 * This is inserting the vty->buf line into the history.
 *
 * Resets hp == h_now.
 */
static void
uty_cli_hist_add (vty_cli cli, qstring clx)
{
  qstring   hist_line ;
  int       prev ;

  VTY_ASSERT_LOCKED() ;

  /* make sure have a suitable history vector                           */
  vector_set_min_length(cli->hist, VTY_MAXHIST) ;

  /* get the previous command line                                      */
  prev = cli->h_now - 1 ;
  if (prev < 0)
    prev = VTY_MAXHIST - 1 ;

  hist_line = vector_get_item(cli->hist, prev) ;

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
  if ((hist_line == NULL) || (qs_cmp_sig(hist_line, clx) == 0))
    cli->h_now = prev ;
  else
    hist_line = vector_get_item(cli->hist, cli->h_now) ;

  /* Now replace the h_now entry
   *
   * Note that the line inserted in the history has it's 'cp' set to the end of
   * the line -- so that it is there when it comes back out again.
   */
  hist_line = qs_copy(hist_line, clx) ;
  qs_set_cp_nn(hist_line, qs_len_nn(hist_line)) ;
  vector_set_item(cli->hist, cli->h_now, hist_line) ;

  /* Advance to the near future and reset the history pointer           */
  cli->h_now++;
  if (cli->h_now == VTY_MAXHIST)
    cli->h_now = 0;

  cli->hp = cli->h_now;
} ;

/*------------------------------------------------------------------------------
 * Replace command line by current history.
 *
 * This function is called from vty_next_line and vty_previous_line.
 *
 * Step -1 is into the past (up)
 *      +1 is towards the present (down)
 */
static void
uty_cli_hist_use(vty_cli cli, int step)
{
  int       hp ;
  ulen      old_len ;
  ulen      new_len ;
  ulen      after ;
  ulen      back ;
  qstring   hist_line ;

  VTY_ASSERT_LOCKED() ;

  assert((step == +1) || (step == -1)) ;

  hp = cli->hp ;

  /* Special case of being at the insertion point                       */
  if (hp == cli->h_now)
    {
      if (step > 0)
        return ;        /* already in the present               */

      /* before stepping back from the present, take a copy of the
       * current command line -- so can get back to it.
       *
       * Note that the 'cp' is stored with the line.  If return to here
       * and later enter the line it will replace this.
       */
      hist_line = vector_get_item(cli->hist, cli->h_now) ;
      vector_set_item(cli->hist, cli->h_now, qs_copy(hist_line, cli->cl)) ;
    } ;

  /* Advance or retreat                                                 */
  hp += step ;
  if      (hp < 0)
    hp = VTY_MAXHIST - 1 ;
  else if (hp >= VTY_MAXHIST)
    hp = 0 ;

  hist_line = vector_get_item(cli->hist, hp) ;

  /* If moving backwards in time, may not move back to the h_now
   * point (that would be wrapping round to the present) and may not
   * move back to a NULL entry (that would be going back before '.').
   *
   * If moving forwards in time, may return to the present, with
   * hp == cli->h_now.
   */
  if (step < 0)
    if ((hist_line == NULL) || (hp == cli->h_now))
      return ;

  cli->hp = hp ;

  /* Move back to the start of the current line                         */
  uty_cli_bol(cli) ;

  /* Get previous line from history buffer and echo that                */
  old_len = qs_len_nn(cli->cl) ;
  qs_copy(cli->cl, hist_line) ;
  new_len = qs_len_nn(cli->cl) ;

  /* Sort out wiping out any excess and setting the cursor position     */
  if (old_len > new_len)
    after = old_len - new_len ;
  else
    after = 0 ;

  /* Return cursor to stored 'cp' -- which will be end of line unless
   * this is the copy of the original current line stored above.
   */
  back = after + qs_after_cp_nn(cli->cl) ;

  if (new_len > 0)
    uty_cli_echo(cli, qs_char_nn(cli->cl), new_len) ;

  if (after > 0)
    uty_cli_echo_n(cli, telnet_spaces,     after) ;

  if (back > 0)
    uty_cli_echo_n(cli, telnet_backspaces, back) ;

  return ;
} ;

/*------------------------------------------------------------------------------
 * Use previous history line, if any (up arrow).
 */
static void
uty_cli_hist_previous(vty_cli cli)
{
  uty_cli_hist_use(cli, -1) ;
}

/*------------------------------------------------------------------------------
 * Use next history line, if any (down arrow).
 */
static void
uty_cli_hist_next(vty_cli cli)
{
  uty_cli_hist_use(cli, +1) ;
}

/*------------------------------------------------------------------------------
 * Show the contents of the history
 */
extern void
uty_cli_hist_show(vty_cli cli)
{
  int       hp ;

  VTY_ASSERT_LOCKED() ;

  hp = cli->h_now ;

  while(1)
    {
      qstring line ;

      ++hp ;
      if (hp == VTY_MAXHIST)
        hp = 0 ;

      if (hp == cli->h_now)
        break ;                 /* wrapped round to "now"               */

      line = vector_get_item(cli->hist, hp) ;

      if (line == NULL)
        break ;                 /* reached limit of history so far      */

      uty_out(cli->vf->vio, "  %s\n", qs_string(line));
    }
} ;

/*==============================================================================
 * Command Completion and Command Description
 *
 */
static uint uty_cli_help_parse(vty_cli cli, node_type_t node) ;
static void uty_cli_out_message(vty_cli cli, const char* msg) ;

static void uty_cli_complete_keyword(vty_cli cli, const char* keyword) ;
static void uty_cli_complete_list(vty_cli cli, vector item_v) ;

static void uty_cli_describe_list(vty_cli cli, vector item_v) ;
static void uty_cli_describe_line(vty_cli cli, uint str_width, const char* str,
                                                    const char* doc, uint len) ;

static uint uty_cli_width_to_use(vty_cli cli) ;

/*------------------------------------------------------------------------------
 * Command completion
 *
 * Requires that cmd_token_position() has been called to tokenise the line and
 * establish which token the cursor is in.  Must NOT call this if the cursor
 * is in a "special" place.
 *
 */
static void
uty_cli_complete_command (vty_cli cli, node_type_t node)
{
  uint        n_items ;
  cmd_parsed  parsed ;
  cmd_item    item ;

  VTY_ASSERT_LOCKED() ;

  parsed = cli->parsed ;

  /* Establish what items may be present at the current token position. */
  n_items = uty_cli_help_parse(cli, node) ;

  if (n_items == 0)             /* quit if nothing to consider          */
    return ;

  if (n_items > 1)              /* render list of alternatives          */
    return uty_cli_complete_list(cli, parsed->item_v) ;

  /* One possible item -- one or more possible commands                 */
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
      return uty_cli_describe_list(cli, parsed->item_v) ;

    case item_keyword:
      return uty_cli_complete_keyword(cli, item->str) ;

    default:
      zabort("unknown item type") ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Command Description
 */
static void
uty_cli_describe_command (vty_cli cli, node_type_t node)
{
  uint        n_items ;

  VTY_ASSERT_LOCKED() ;

  /* Establish what items may be present at the current token position. */
  n_items = uty_cli_help_parse(cli, node) ;

  if (n_items > 0)              /* render list of possibilities         */
    uty_cli_describe_list(cli, cli->parsed->item_v) ;
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
uty_cli_help_parse(vty_cli cli, node_type_t node)
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
      uty_cli_out_message(cli, msg) ;
      return 0 ;
    } ;

  /* Now see what the cmd_completion can come up with.                  */
  ret = cmd_completion(cli->parsed, node) ;

  if (ret == CMD_ERR_PARSING)
    {
      uint eloc = cli->prompt_len + cli->parsed->eloc ;

      uty_cli_out_newline(cli) ;              /* clears cli_drawn     */
      uty_cli_write_n(cli, telnet_dots, eloc) ;
      uty_cli_write_s(cli, "^") ;

      uty_cli_out_message(cli, cli->parsed->emess) ;

      return 0 ;
    } ;

  /* Will now have 0, 1 or more items which match at the current
   * cursor token.
   */
  n_items = vector_length(cli->parsed->item_v) ;

  if (n_items == 0)
    uty_cli_out_message(cli, "command not recognised") ;

  return n_items ;
} ;








static void
uty_cli_out_message(vty_cli cli, const char* msg)
{
  uty_cli_out_newline(cli) ;              /* clears cli_drawn     */
  uty_cli_write_s(cli, "% ") ;
  uty_cli_write_s(cli, msg) ;
  uty_cli_out_newline(cli) ;
} ;



static void
uty_cli_complete_keyword(vty_cli cli, const char* keyword)
{
  int pre, rep, ins, mov ;

  cmd_complete_keyword(cli->parsed, &pre, &rep, &ins, &mov) ;

  uty_cli_move(cli, pre) ;              /* move to start of token */
  uty_cli_replace(cli, rep, keyword, strlen(keyword)) ;

  if (ins > 0)
    uty_cli_insert(cli, " ", ins) ;

  uty_cli_move(cli, mov) ;

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
  uint i, len, n ;

  len = 6 ;
  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;
      uint sl ;

      item = vector_get_item(item_v, i) ;

      sl = strlen(item->str) ;
      if (len < sl)
        len = sl ;
    } ;

  n = uty_cli_width_to_use(cli) / (len + 2) ;

  if (n == 0)
    n = 1 ;

  for (i = 0 ; i < vector_length(item_v) ; ++i)
    {
      cmd_item item ;
      item = vector_get_item(item_v, i) ;

      if ((i % n) == 0)
        uty_cli_out_newline(cli) ;        /* clears cli_drawn     */

      uty_cli_out(cli, "%-*s  ", len, item->str) ;
    }
  uty_cli_out_newline(cli) ;
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

      len = strlen(item->str) ;
      if (item->str[0] == '.')
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

      item = vector_get_item(item_v, i) ;

      str = item->str[0] == '.' ? item->str + 1 : item->str;
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

              uty_cli_describe_line(cli, str_width, str, dp, np - dp) ;

              str = "";             /* for 2nd and subsequent lines */

              dp = np ;             /* step past what just wrote    */
              while (*dp == ' ')
                ++dp ;              /* skip spaces                  */
            } ;
        } ;

      uty_cli_describe_line(cli, str_width, str, dp, ep - dp) ;
    } ;

  uty_cli_out_newline(cli) ;
} ;

/*------------------------------------------------------------------------------
 * Show one description line.
 */
static void
uty_cli_describe_line(vty_cli cli, uint str_width, const char* str,
                                                   const char* doc, uint len)
{
  if ((*str == '\0') && (len == 0))
    return ;            /* quit if nothing to say               */

  uty_cli_out_newline(cli) ;

  if (len == 0)
    uty_cli_out(cli, "  %s", str) ;     /* left justify */
  else
    {
      uty_cli_out(cli, "  %-*s  ", str_width, str) ;
      uty_cli_write(cli, doc, len) ;
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

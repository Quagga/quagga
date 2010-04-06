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

#include <zebra.h>

#include "keystroke.h"
#include "vty.h"
#include "uty.h"
#include "vty_cli.h"
#include "vty_io.h"
#include "vio_lines.h"

#include "command.h"
#include "command_queue.h"

#include "memory.h"

/*==============================================================================
 * Host name handling
 *
 * The host name is used in the command line prompt.  The name used is either
 * the name set by "hostname" command, or the current machine host name.
 *
 * Static variables -- under the VTY_LOCK !
 */

static char* vty_host_name = NULL ;
int vty_host_name_set      = 0 ;

static void uty_new_host_name(const char* name) ;

/*------------------------------------------------------------------------------
 * Update vty_host_name as per "hostname" or "no hostname" command
 */
extern void
uty_set_host_name(const char* name)
{
  VTY_ASSERT_LOCKED() ;

  vty_host_name_set = (name != NULL) ;

  if (vty_host_name_set)
    uty_new_host_name(name) ;
  else
    uty_check_host_name() ;
} ;

/*------------------------------------------------------------------------------
 * If vty_host_name is set, free it.
 */
extern void
uty_free_host_name(void)
{
  if (vty_host_name != NULL)
    XFREE (MTYPE_HOST, vty_host_name) ; /* sets vty_host_name = NULL */
} ;

/*------------------------------------------------------------------------------
 * If the host name is not set by command, see if the actual host name has
 * changed, and if so change it.
 *
 * This is done periodically in case the actual host name changes !
 */
extern void
uty_check_host_name(void)
{
  struct utsname names ;

  VTY_ASSERT_LOCKED() ;

  if (vty_host_name_set)
    return ;                    /* nothing to do if set by command      */

  uname (&names) ;

  if ((vty_host_name == NULL) || (strcmp(vty_host_name, names.nodename) != 0))
    uty_new_host_name(names.nodename) ;
} ;

/*------------------------------------------------------------------------------
 * Set new vty_host_name and run along list of VTYs to mark the change.
 */
static void
uty_new_host_name(const char* name)
{
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  uty_free_host_name() ;
  vty_host_name = XSTRDUP(MTYPE_HOST, name) ;

  vio = vio_list_base ;
  while (vio != NULL)
    {
      vio->cli_prompt_set = 0 ;
      vio = sdl_next(vio, vio_list) ;
    } ;
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
 * State of the CLI:
 *
 *   cli_blocked      -- the CLI is unable to process any further keystrokes.
 *
 *   cmd_in_progress  -- a command has been dispatched and has not yet
 *                       completed (may have been queued).
 *
 *   cmd_out_enabled  -- the command FIFO is may be emptied.
 *
 *                       This is set when a command completes, and cleared when
 *                       everything is written away.
 *
 *   cli_more_wait     -- is in "--more--" wait state
 *
 * The following are the valid combinations:
 *
 *     blkd :  cip : o_en : m_wt :
 *     -----:------:------:------:--------------------------------------------
 *       0  :   0  :   0  :   0  : collecting a new command
 *       0  :   1  :   0  :   0  : command dispatched
 *       1  :   1  :   0  :   0  : waiting for (queued) command to complete
 *       1  :   0  :   1  :   0  : waiting for command output to complete
 *       1  :   0  :   0  :   1  : waiting for "--more--" to be written away
 *       0  :   0  :   0  :   1  : waiting for "--more--" response
 *       1  :   1  :   1  :   0  : waiting for command to complete, after the
 *                                                           CLI has been closed
 *
 * There are two output FIFOs:
 *
 *   1. for the CLI itself               -- uty_cli_out and friends
 *
 *   2. for output generated by commands -- vty_out and friends.
 *
 * The CLI FIFO is emptied whenever possible, in preference to the command
 * FIFO.  The command FIFO is emptied when cmd_out_enabled.  While
 * cmd_in_progress is also !cmd_out_enabled -- so that all the output from a
 * given command is collected together before being sent to the file.
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
 * While cmd_in_progress is true cmd_out_enabled will be false.  When the
 * command completes:
 *
 *  * cmd_in_progress is cleared
 *
 *  * cmd_out_enabled is set
 *
 *  * cli_blocked will be set
 *
 *  * the line_control structure is reset
 *
 *  * the output process is kicked off by setting write on
 *
 * The output process used the line_control structure to manage the output, and
 * occasionally enter the trivial "--more--" CLI.  This is invisible to the
 * main CLI.  (See the cli_more_wait flag and its handling.)
 *
 * When all the output has completed the CLI will be kicked, which will see
 * that the output buffer is now empty, and it can proceed.
 *
 * It is expected that the output will end with a newline -- so that when the
 * CLI is kicked, the cursor will be at the start of an empty line.
 *
 * This mechanism means that the command output FIFO only ever contains the
 * output from the last command executed.
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

#define CONTROL(X)  ((X) - '@')

static void uty_cli_cmd_complete(vty_io vio, enum cmd_return_code ret) ;
static enum vty_readiness uty_cli_standard(vty_io vio) ;
static enum vty_readiness uty_cli_more_wait(vty_io vio) ;
static void uty_cli_draw(vty_io vio) ;
static void uty_cli_draw_this(vty_io vio, enum node_type node) ;
static void uty_cli_wipe(vty_io vio, int len) ;

static void uty_will_echo (vty_io vio) ;
static void uty_will_suppress_go_ahead (vty_io vio) ;
static void uty_dont_linemode (vty_io vio) ;
static void uty_do_window_size (vty_io vio) ;
static void uty_dont_lflow_ahead (vty_io vio) ;

/*------------------------------------------------------------------------------
 * Initialise CLI.
 *
 * It is assumed that the following have been initialised, empty or zero:
 *
 *   cli_prompt_for_node
 *   cl
 *   clx
 *   cli_vbuf
 *   cli_obuf
 *
 *   cli_drawn
 *   cli_dirty
 *
 *   cli_prompt_set
 *
 *   cli_blocked
 *   cmd_in_progress
 *   cmd_out_enabled
 *   cli_wait_more
 *
 *   cli_more_enabled
 *
 * Sets the CLI such that there is apparently a command in progress, so that
 * further initialisation (in particular hello messages and the like) is
 * treated as a "start up command".
 *
 * Sends a suitable set of Telnet commands to start the process.
 */
extern void
uty_cli_init(vty_io vio)
{
  assert(vio->type == VTY_TERM) ;

  vio->cmd_in_progress  = 1 ;
  vio->cli_blocked      = 1 ;

  vio->cli_do           = cli_do_nothing ;

  /* Setting up terminal.                                               */
  uty_will_echo (vio);
  uty_will_suppress_go_ahead (vio);
  uty_dont_linemode (vio);
  uty_do_window_size (vio);
  if (0)
    uty_dont_lflow_ahead (vio) ;
} ;

/*------------------------------------------------------------------------------
 * Start the CLI.
 *
 * All start-up operations are complete -- so the "command" is now complete.
 *
 * Returns: write_ready -- so the first event is a write event, to flush
 *                         any output to date.
 */
extern enum vty_readiness
uty_cli_start(vty_io vio)
{
  uty_cli_cmd_complete(vio, CMD_SUCCESS) ;
  return write_ready ;
} ;

/*------------------------------------------------------------------------------
 * Close the CLI
 *
 * Note that if any command is revoked, then will clear cmd_in_progress and
 * set cmd_out_enabled -- so any output can now clear.
 */
extern void
uty_cli_close(vty_io vio)
{
  cq_revoke(vio->vty) ;

  vio->cli_blocked     = 1 ;    /* don't attempt any more       */
  vio->cmd_out_enabled = 1 ;    /* allow output to clear        */
} ;

/*------------------------------------------------------------------------------
 * CLI for VTY_TERM
 *
 * Do nothing at all if half closed.
 *
 * Otherwise do: standard CLI
 *           or: "--more--" CLI
 *
 * NB: on return, requires that an attempt is made to write away anything that
 *     may be ready for that.
 */
extern enum vty_readiness
uty_cli(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;
  assert(vio->type == VTY_TERM) ;

  if (vio->half_closed)
    return not_ready ;          /* Nothing more if half closed          */

  /* Standard or "--more--" CLI ?                                       */
  if (vio->cli_more_wait)
    return uty_cli_more_wait(vio) ;
  else
    return uty_cli_standard(vio) ;
} ;

/*==============================================================================
 * The Standard CLI
 */

static enum cli_do uty_cli_process(vty_io vio, enum node_type node) ;
static void uty_cli_response(vty_io vio, enum cli_do cli_do) ;
static bool uty_cli_dispatch(vty_io vio) ;

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
static enum vty_readiness
uty_cli_standard(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;
  assert(vio->type == VTY_TERM) ;

  /* cli_blocked is set when is waiting for a command, or its output to
   * complete -- unless either of those has happened, is still blocked.
   *
   * NB: in both these cases, assumes that other forces are at work to
   *     keep things moving.
   */
  if (vio->cli_blocked)
    {
      assert(vio->cmd_in_progress || vio->cmd_out_enabled) ;

      if (vio->cmd_in_progress)
        {
          assert(!vio->cmd_out_enabled) ;
          return not_ready ;
        } ;

      if (!vio_fifo_empty(&vio->cmd_obuf))
        return not_ready ;

      vio->cli_blocked     = 0 ;
      vio->cmd_out_enabled = 0 ;
    } ;

  /* If there is nothing pending, then can run the CLI until there is
   * something to do, or runs out of input.
   *
   * If there is something to do, that is because a previous command has
   * now completed, which may have wiped the pending command or changed
   * the required prompt.
   */
  if (vio->cli_do == cli_do_nothing)
    vio->cli_do = uty_cli_process(vio, vio->vty->node) ;
  else
    uty_cli_draw_this(vio, vio->vty->node) ;

  /* If have something to do, do it.                                */
  if (vio->cli_do != cli_do_nothing)
    {
      /* Reflect immediate response                                 */
      uty_cli_response(vio, vio->cli_do) ;

      /* If command not already in progress, dispatch this one, which may
       * set the CLI blocked.
       *
       * Otherwise is now blocked until queued command completes.
       */
      if (!vio->cmd_in_progress)
        vio->cli_blocked = uty_cli_dispatch(vio) ;
      else
        vio->cli_blocked = 1 ;
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
  if (keystroke_stream_empty(vio->key_stream))
    return read_ready ;
  else
    return write_ready ;
} ;

/*------------------------------------------------------------------------------
 * Dispatch the current vio->cli_do -- queueing it if necessary.
 *
 * Requires that are NOT blocked and NO command is queued.
 *
 * Expects to be on new blank line, and when returns will be on new, blank
 * line.
 *
 * Returns: true <=> command completed and output is pending
 *         false  => command has been queued and is now in progress
 *
 * Generally sets vio->cl_do = cli_do_nothing and clears vio->cl to empty.
 *
 * Can set vio->cl_do = and vio->cl to be a follow-on command.
 */
static bool
uty_cli_dispatch(vty_io vio)
{
  qstring_t tmp ;
  enum cli_do cli_do ;
  enum cmd_return_code ret ;

  struct vty* vty = vio->vty ;

  VTY_ASSERT_LOCKED() ;
  assert(!vio->cli_blocked && !vio->cmd_in_progress) ;

  /* Set vio->clx to the command about to execute.
   *
   * Clear vio->cl  and vio->cl_do.
   */
  vio->cmd_in_progress  = 1 ;           /* => vty->buf is valid         */
  vio->cmd_out_enabled  = 0 ;           /* => collect all output        */

  tmp      = vio->clx ;                 /* swap clx and cl              */
  vio->clx = vio->cl ;
  vio->cl  = tmp ;

  qs_term(&vio->clx) ;                  /* ensure string is terminated  */
  vty->buf = qs_chars(&vio->clx) ;      /* terminated command line      */
  cli_do = vio->cli_do ;                /* current operation            */

  vio->cli_do = cli_do_nothing ;        /* clear                        */
  qs_clear(&vio->cl) ;                  /* set cl empty (with '\0')     */

  /* Reset the command output FIFO and line_control                     */
  assert(vio_fifo_empty(&vio->cmd_obuf)) ;
  uty_out_clear(vio) ;                  /* clears FIFO and line control */

  /* Dispatch command                                                       */
  if ((vty->node == AUTH_NODE) || (vty->node == AUTH_ENABLE_NODE))
    {
      /* AUTH_NODE and AUTH_ENABLE_NODE are unique                          */
      ret = uty_auth(vty, vty->buf, cli_do) ;
    }
  else
    {
      /* All other nodes...                                                 */
      switch (cli_do)
      {
        case cli_do_nothing:
          break ;

        case cli_do_command:
          ret = uty_command(vty) ;
          break ;

        case cli_do_ctrl_c:
          ret = uty_stop_input(vty) ;
          break ;

        case cli_do_ctrl_d:
          ret = uty_down_level(vty) ;
          break ;

        case cli_do_ctrl_z:
          ret = uty_command(vty) ;
          if (ret == CMD_QUEUED)
            vio->cli_do = cli_do_ctrl_z ;       /* defer the ^Z action  */
          else
            ret = uty_end_config(vty) ;         /* do the ^Z now        */
          break ;

        case cli_do_eof:
          ret = uty_cmd_close(vio->vty, "End") ;
          break ;

        default:
          zabort("unknown cli_do_xxx value") ;
      } ;
    } ;

  if (ret == CMD_QUEUED)
    {
      uty_cli_draw(vio) ;       /* draw the prompt              */
      return 0 ;                /* command not complete         */
    }
  else
    {
      uty_cli_cmd_complete(vio, ret) ;
      return 1 ;                /* command complete             */
    } ;
} ;

/*------------------------------------------------------------------------------
 * Queued command has completed.
 *
 * Note that sets write on whether there is anything in the output buffer
 * or not... write_ready will kick read_ready.
 */
extern void
vty_queued_result(struct vty *vty, enum cmd_return_code ret)
{
  vty_io vio ;

  VTY_LOCK() ;

  vio = vty->vio ;

  if (!vio->closed)
    {
      uty_cli_wipe(vio, 0) ;    /* wipe any partly constructed line     */

      /* Do the command completion actions that were deferred because the
       * command was queued.
       *
       * Return of CMD_QUEUED => command was revoked before being executed.
       * However interesting that might be... frankly don't care.
       */
      uty_cli_cmd_complete(vio, ret) ;

      /* Kick the socket -- to write away any outstanding output, and
       * re-enter the CLI when that's done.
       */
      uty_sock_set_readiness(&vio->sock, write_ready) ;
    }
  else
    {
      /* If the VTY is closed, the only reason it still exists is because
       * there was cmd_in_progress.
       */
      vio->cmd_in_progress = 0 ;

      uty_close(vio) ;          /* Final close                          */
    } ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Command has completed, so:
 *
 *   * clear cmd_in_progress
 *   * set   cmd_out_enabled -- so any output can now proceed
 *   * set   cli_blocked     -- waiting for output to complete
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
static void
uty_cli_cmd_complete(vty_io vio, enum cmd_return_code ret)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->cmd_in_progress && !vio->cmd_out_enabled) ;

  if (ret == CMD_CLOSE)
    uty_half_close(vio, NULL) ;

  vio->cmd_in_progress  = 0 ;   /* command complete                     */
  vio->cmd_out_enabled  = 1 ;   /* enable the output                    */
  vio->cli_blocked      = 1 ;   /* now blocked waiting for output       */

  vio->vty->buf = NULL ;        /* finished with command line           */

  uty_cmd_output_start(vio) ;   /* reset line control (belt & braces)   */
} ;

/*==============================================================================
 * The "--more--" CLI
 *
 * While command output is being cleared from its FIFO, the CLI is cli_blocked.
 *
 * When the output side signals that "--more--" is required, it sets the
 * cli_more_wait flag and clears the cmd_out_enabled flag.
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
uty_cli_go_more_wait(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->cli_blocked && vio->cmd_out_enabled && !vio->cli_more_wait) ;

  uty_cli_wipe(vio, 0) ;        /* make absolutely sure that command line is
                                   wiped before change the CLI state    */

  vio->cmd_out_enabled  = 0 ;   /* stop output pro tem                  */
  vio->cli_more_wait    = 1 ;   /* new state                            */

  uty_cli_draw(vio) ;           /* draw the "--more--"                  */
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
static enum vty_readiness
uty_cli_more_wait(vty_io vio)
{
  struct keystroke steal ;

  VTY_ASSERT_LOCKED() ;

  /* Deal with the first stage of "--more--"                            */
  if (vio->cli_blocked)
    {
      int  get ;

      /* If the CLI buffer is not yet empty, then is waiting for the
       * initial prompt to clear, so nothing to be done here.
       */
      if (!vio_fifo_empty(&vio->cli_obuf))
        return not_ready ;

      vio->cli_blocked = 0 ;

      /* empty the input buffer into the keystroke stream               */
      do
        {
          get = uty_read(vio, NULL) ;
        } while (get > 0) ;
    } ;

  /* Go through the "--more--" process, unless no longer write_open (!) */
  if (vio->sock.write_open)
    {
      /* The read fetches a reasonable lump from the I/O -- so if there
       * is a complete keystroke available, expect to get it.
       *
       * If no complete keystroke available to steal, returns ks_null.
       *
       * If has hit EOF (or error etc), returns knull_eof.
       */
      uty_read(vio, &steal) ;

      /* If nothing stolen, make sure prompt is drawn and wait for more
       * input.
       */
      if ((steal.type == ks_null) && (steal.value != knull_eof))
        {
          if (uty_cli_draw_if_required(vio))    /* "--more--" if req.   */
            return write_ready ;
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
              uty_out_clear(vio) ;
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
  uty_cli_wipe(vio, 0) ;

  vio->cli_blocked      = 1 ;   /* back to blocked waiting for output   */
  vio->cli_more_wait    = 0 ;   /* exit more_wait                       */
  vio->cmd_out_enabled  = 1 ;   /* re-enable output                     */

  return now_ready ;
} ;

/*==============================================================================
 * CLI VTY output
 *
 * This is buffered separately from the general (command) VTY output above.
 *
 * Has a dedicated buffer in the struct vty, which is flushed regularly during
 * command processing.
 *
 * It is expected that can flush straight to the file, since this is running at
 * CLI speed.  However, if the CLI is being driven by something other than a
 * keyboard, or "monitor" output has filled the buffers, then may need to
 * have intermediate buffering.
 *
 * No actual I/O takes place here-- all "output" is to vio->cli_obuf
 *                                              and/or vio->cli_ex_obuf
 *
 * The "cli_echo" functions discard the output if vio->cli_echo_suppress != 0.
 * This is used while passwords are entered and to allow command line changes
 * to be made while the line is not visible.
 */

enum { cli_rep_count = 32 } ;

typedef const char cli_rep_char[cli_rep_count] ;

static const char telnet_backspaces[] =
                          { 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
                            0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08
                          } ;
CONFIRM(sizeof(telnet_backspaces) == sizeof(cli_rep_char)) ;

static const char telnet_spaces[] =
                          {  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
                             ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
                             ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',
                             ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' ',  ' '
                          } ;
CONFIRM(sizeof(telnet_spaces)     == sizeof(cli_rep_char)) ;

static const char* telnet_newline = "\r\n" ;

static void uty_cli_write(vty_io vio, const char *this, int len) ;
static void uty_cli_write_n(vty_io vio, cli_rep_char chars, int n) ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf fprintf()
 */
static void
uty_cli_out(vty_io vio, const char *format, ...)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->sock.write_open)
    {
      va_list args ;

      va_start (args, format);
      vio_fifo_vprintf(&vio->cli_obuf, format, args) ;
      va_end(args);
    } ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo user input
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE) or not write_open
 */
static void
uty_cli_echo(vty_io vio, const char *this, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->cli_echo_suppress || !vio->sock.write_open)
    return ;

  uty_cli_write(vio, this, len) ;
}

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo 'n' characters using a cli_rep_char string
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE)
 */
static void
uty_cli_echo_n(vty_io vio, cli_rep_char chars, int n)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->cli_echo_suppress || !vio->sock.write_open)
    return ;

  uty_cli_write_n(vio, chars, n) ;
}

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf write()
 */
inline static void
uty_cli_write(vty_io vio, const char *this, int len)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->sock.write_open)
    vio_fifo_put(&vio->cli_obuf, this, len) ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- write 'n' characters using a cli_rep_char string
 */
static void
uty_cli_write_n(vty_io vio, cli_rep_char chars, int n)
{
  int len ;

  len = sizeof(cli_rep_char) ;
  while (n > 0)
    {
      if (n < len)
        len = n ;
      uty_cli_write(vio, chars, len) ;
      n -= len ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- write given string
 *
 * Returns length of string.
 */
static int
uty_cli_write_s(vty_io vio, const char *str)
{
  int len ;

  len = strlen(str) ;
  if (len != 0)
    uty_cli_write(vio, str, len) ;

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
static void
uty_cli_out_newline(vty_io vio)
{
  uty_cli_write(vio, telnet_newline, 2) ;
  vio->cli_drawn = 0 ;
  vio->cli_dirty = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Wipe 'n' characters.
 *
 * If 'n' < 0, wipes characters backwards and moves cursor back.
 *    'n' > 0, wipes characters forwards, leaving cursor where it is
 */
static void
uty_cli_out_wipe_n(vty_io vio, int n)
{
  if (n < 0)
    {
      n = abs(n) ;
      uty_cli_write_n(vio, telnet_backspaces, n);
    } ;

  if (n > 0)
    {
      uty_cli_write_n(vio, telnet_spaces,     n) ;
      uty_cli_write_n(vio, telnet_backspaces, n) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Send response to the given cli_do
 *
 * If no command is in progress, then will send newline to signal that the
 * command is about to be dispatched.
 *
 * If command is in progress, then leaves cursor on '^' to signal that is now
 * waiting for previous command to complete.
 */
static const char* cli_response [2][cli_do_count] =
{
  { /* when not waiting for previous command to complete        */
    [cli_do_command]  = "",
    [cli_do_ctrl_c]   = "^C",
    [cli_do_ctrl_d]   = "^D",
    [cli_do_ctrl_z]   = "^Z",
    [cli_do_eof]      = "^*"
  },
  { /* when waiting for a previous command to complete          */
    [cli_do_command]  = "^",
    [cli_do_ctrl_c]   = "^C",
    [cli_do_ctrl_d]   = "^D",
    [cli_do_ctrl_z]   = "^Z",
    [cli_do_eof]      = "^*"
  }
} ;

static void
uty_cli_response(vty_io vio, enum cli_do cli_do)
{
  const char* str ;
  int len ;

  if ((cli_do == cli_do_nothing) || (vio->half_closed))
    return ;

  str = (cli_do < cli_do_count)
          ? cli_response[vio->cmd_in_progress ? 1 : 0][cli_do]  : NULL ;
  assert(str != NULL) ;

  len = uty_cli_write_s(vio, str) ;

  if (vio->cmd_in_progress)
    {
      vio->cli_extra_len = len ;
      uty_cli_write_n(vio, telnet_backspaces, len) ;
    }
  else
    {
      uty_cli_out_newline(vio) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Send various messages with trailing newline.
 */

static void
uty_cli_out_CMD_ERR_AMBIGUOUS(vty_io vio)
{
  uty_cli_write_s(vio, "% " MSG_CMD_ERR_AMBIGUOUS ".") ;
  uty_cli_out_newline(vio) ;
} ;

static void
uty_cli_out_CMD_ERR_NO_MATCH(vty_io vio)
{
  uty_cli_write_s(vio, "% " MSG_CMD_ERR_NO_MATCH ".") ;
  uty_cli_out_newline(vio) ;
} ;

/*==============================================================================
 * Command line draw and wipe
 */

/*------------------------------------------------------------------------------
 * Wipe the current console line -- if any.
 */
static void
uty_cli_wipe(vty_io vio, int len)
{
  int a ;
  int b ;

  if (!vio->cli_drawn)
    return ;                    /* quit if already wiped        */

  assert(vio->cl.cp <= vio->cl.len) ;

  /* Establish how much ahead and how much behind the cursor            */
  a = vio->cli_extra_len ;
  b = vio->cli_prompt_len ;

  if (!vio->cli_echo_suppress && !vio->cli_more_wait)
    {
      a += vio->cl.len - vio->cl.cp ;
      b += vio->cl.cp ;
    } ;

  /* Stuff ahead of the current position if any ahead of new len        */
  if ((a + b) > len)
    uty_cli_out_wipe_n(vio, +a) ;

  /* Stuff behind current position, but ahead of new len                */
  if (b > len)
    {
      uty_cli_out_wipe_n(vio, -(b - len)) ;
      b = len ;         /* moved the cursor back        */
    } ;

  /* Back to the beginning of the line                                  */
  uty_cli_write_n(vio, telnet_backspaces, b) ;

  /* Nothing there any more                                             */
  vio->cli_drawn = 0 ;
  vio->cli_dirty = 0 ;
} ;

/*------------------------------------------------------------------------------
 * If not currently drawn, draw prompt etc according to the current state
 * and node.
 *
 * See uty_cli_draw().
 */
extern bool
uty_cli_draw_if_required(vty_io vio)
{
  if (vio->cli_drawn)
    return false ;

  uty_cli_draw(vio) ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Draw prompt etc for the current vty node.
 *
 * See uty_cli_draw_this()
 */
static void
uty_cli_draw(vty_io vio)
{
  uty_cli_draw_this(vio, vio->vty->node) ;
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
 *   * if is half_closed, draw nothing -- wipes the current line
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
uty_cli_draw_this(vty_io vio, enum node_type node)
{
  const char* prompt ;
  size_t l_len ;
  int    p_len ;

  if (vio->cli_dirty)
    uty_cli_out_newline(vio) ;  /* clears cli_dirty and cli_drawn       */

  /* Sort out what the prompt is.                                       */
  if (vio->half_closed)
    {
      prompt = "" ;
      p_len  = 0 ;
      l_len  = 0 ;
    }
  else if (vio->cli_more_wait)
    {
      prompt = "--more--" ;
      p_len  = strlen(prompt) ;
      l_len  = 0 ;
    }
  else if (vio->cmd_in_progress)
    {
      /* If there is a queued command, the prompt is a minimal affair.  */
      prompt = "~ " ;
      p_len  = strlen(prompt) ;
      l_len  = vio->cl.len ;
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
      if (!vio->cli_prompt_set || (node != vio->cli_prompt_node))
        {
          const char* prompt ;

          if (vty_host_name == NULL)
            uty_check_host_name() ;         /* should never be required  */

          prompt = cmd_prompt(node) ;
          if (prompt == NULL)
            {
              zlog_err("vty %s has node %d", uty_get_name(vio), node) ;
              prompt = "%s ???: " ;
            } ;

          qs_printf(&vio->cli_prompt_for_node, prompt, vty_host_name);

          vio->cli_prompt_node = node ;
          vio->cli_prompt_set  = 1 ;
        } ;

      prompt = vio->cli_prompt_for_node.body ;
      p_len  = vio->cli_prompt_for_node.len ;
      l_len  = vio->cl.len ;
    } ;

  /* Now, if line is currently drawn, time to wipe it                   */
  if (vio->cli_drawn)
    uty_cli_wipe(vio, p_len + l_len) ;

  /* Set about writing the prompt and the line                          */
  vio->cli_drawn     = 1 ;
  vio->cli_extra_len = 0 ;
  vio->cli_echo_suppress = (node == AUTH_NODE || node == AUTH_ENABLE_NODE) ;

  vio->cli_prompt_len = p_len ;

  uty_cli_write(vio, prompt, p_len) ;

  if (l_len != 0)
    {
      uty_cli_write(vio, qs_chars(&vio->cl), l_len) ;
      if (vio->cl.cp < l_len)
        uty_cli_write_n(vio, telnet_backspaces, l_len - vio->cl.cp) ;
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
uty_cli_pre_monitor(vty_io vio, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  uty_cli_wipe(vio, len) ;
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
uty_cli_post_monitor(vty_io vio, const char* buf, size_t len)
{
  VTY_ASSERT_LOCKED() ;

  if (len != 0)
    uty_cli_write(vio, buf, len) ;

  if (vio->cli_more_wait || (vio->cl.len != 0))
    {
      uty_cli_draw(vio) ;
      ++len ;
    } ;

  return len ;
} ;

/*==============================================================================
 * Command line processing loop
 */

static bool uty_telnet_command(vty_io vio, keystroke stroke, bool callback) ;
static int uty_cli_insert (vty_io vio, const char* chars, int n) ;
static int uty_cli_overwrite (vty_io vio, char* chars, int n) ;
static int uty_cli_word_overwrite (vty_io vio, char *str) ;
static int uty_cli_forwards(vty_io vio, int n) ;
static int uty_cli_backwards(vty_io vio, int n) ;
static int uty_cli_del_forwards(vty_io vio, int n) ;
static int uty_cli_del_backwards(vty_io vio, int n) ;
static int uty_cli_bol (vty_io vio) ;
static int uty_cli_eol (vty_io vio) ;
static int uty_cli_word_forwards_delta(vty_io vio) ;
static int uty_cli_word_forwards(vty_io vio) ;
static int uty_cli_word_backwards_delta(vty_io vio, int eat_spaces) ;
static int uty_cli_word_backwards_pure (vty_io vio) ;
static int uty_cli_word_backwards (vty_io vio) ;
static int uty_cli_del_word_forwards(vty_io vio) ;
static int uty_cli_del_word_backwards(vty_io vio) ;
static int uty_cli_del_to_eol (vty_io vio) ;
static int uty_cli_clear_line(vty_io vio) ;
static int uty_cli_transpose_chars(vty_io vio) ;
static void uty_cli_history_use(vty_io vio, int step) ;
static void uty_cli_next_line(vty_io vio) ;
static void uty_cli_previous_line (vty_io vio) ;
static void uty_cli_complete_command (vty_io vio, enum node_type node) ;
static void uty_cli_describe_command (vty_io vio, enum node_type node) ;

/*------------------------------------------------------------------------------
 * Fetch next keystroke, reading from the file if required.
 */
static inline bool
uty_cli_get_keystroke(vty_io vio, keystroke stroke)
{
  if (keystroke_get(vio->key_stream, stroke))
    return 1 ;

  uty_read(vio, NULL) ;         /* not stealing */

  return keystroke_get(vio->key_stream, stroke) ;
} ;

/*------------------------------------------------------------------------------
 * Process keystrokes until run out of input, or get something to cli_do.
 *
 * If required, draw the prompt and command line.
 *
 * Process keystrokes until run out of stuff to do, or have a "command line"
 * that must now be executed.
 *
 * Processes the contents of the keystroke stream.  If exhausts that, will set
 * ready to read and return.  (To give some "sharing".)
 *
 * Returns: cli_do_xxxx
 *
 * When returns the cl is '\0' terminated.
 */
static enum cli_do
uty_cli_process(vty_io vio, enum node_type node)
{
  struct keystroke stroke ;
  uint8_t u ;
  int auth ;
  enum cli_do ret ;

  auth = (node == AUTH_NODE || node == AUTH_ENABLE_NODE) ;

  /* Now process as much as possible of what there is                   */
  ret = cli_do_nothing ;
  while (1)
    {
      if (!vio->cli_drawn)
        uty_cli_draw_this(vio, node) ;

      if (!uty_cli_get_keystroke(vio, &stroke))
        {
          ret = (stroke.value == knull_eof) ? cli_do_eof : cli_do_nothing ;
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
              uty_cli_bol (vio);
              break;

            case CONTROL('B'):
              uty_cli_backwards(vio, 1);
              break;

            case CONTROL('C'):
              ret = cli_do_ctrl_c ;     /* Exit on ^C ..................*/
              break ;

            case CONTROL('D'):
              if (vio->cl.len == 0)     /* if at start of empty line    */
                ret = cli_do_ctrl_d ;   /* Exit on ^D ..................*/
              else
                uty_cli_del_forwards(vio, 1);
              break;

            case CONTROL('E'):
              uty_cli_eol (vio);
              break;

            case CONTROL('F'):
              uty_cli_forwards(vio, 1);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_backwards(vio, 1);
              break;

            case CONTROL('K'):
              uty_cli_del_to_eol (vio);
              break;

            case CONTROL('N'):
              uty_cli_next_line (vio);
              break;

            case CONTROL('P'):
              uty_cli_previous_line (vio);
              break;

            case CONTROL('T'):
              uty_cli_transpose_chars (vio);
              break;

            case CONTROL('U'):
              uty_cli_clear_line(vio);
              break;

            case CONTROL('W'):
              uty_cli_del_word_backwards (vio);
              break;

            case CONTROL('Z'):
              ret = cli_do_ctrl_z ;     /* Exit on ^Z ..................*/
              break;

            case '\n':
            case '\r':
              ret = cli_do_command ;    /* Exit on CR or LF.............*/
              break ;

            case '\t':
              if (auth)
                break ;
              else
                uty_cli_complete_command (vio, node);
              break;

            case '?':
              if (auth)
                uty_cli_insert (vio, (char*)&u, 1);
              else
                uty_cli_describe_command (vio, node);
              break;

            default:
              if ((stroke.value >= 0x20) && (stroke.value < 0x7F))
                uty_cli_insert (vio, (char*)&u, 1) ;
              break;
            }
          break ;

        /* ESC X -------------------------------------------------------------*/
        case ks_esc:
          switch (stroke.value)
          {
            case 'b':
              uty_cli_word_backwards (vio);
              break;

            case 'f':
              uty_cli_word_forwards (vio);
              break;

            case 'd':
              uty_cli_del_word_forwards (vio);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cli_del_word_backwards (vio);
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
            case ('A'):
              uty_cli_previous_line (vio);
              break;

            case ('B'):
              uty_cli_next_line (vio);
              break;

            case ('C'):
              uty_cli_forwards(vio, 1);
              break;

            case ('D'):
              uty_cli_backwards(vio, 1);
              break;
            default:
              break ;
          } ;
          break ;

        /* Telnet Command ----------------------------------------------------*/
        case ks_iac:
          uty_telnet_command(vio, &stroke, false) ;
          break ;

        /* Unknown -----------------------------------------------------------*/
        default:
          zabort("unknown keystroke type") ;
      } ;

      /* After each keystroke.....                                      */

      if (ret != cli_do_nothing)
        {
          uty_cli_eol (vio) ;   /* go to the end of the line    */
          break ;               /* stop processing              */
        } ;
    } ;

  /* Tidy up and return where got to.                                   */

  qs_term(&vio->cl) ;           /* add '\0'                     */

  return ret ;
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
uty_cli_insert (vty_io vio, const char* chars, int n)
{
  int after ;

  VTY_ASSERT_LOCKED() ;

  assert((vio->cl.cp <= vio->cl.len) && (n >= 0)) ;

  if (n <= 0)
    return n ;          /* avoid trouble        */

  after = qs_insert(&vio->cl, chars, n) ;

  uty_cli_echo(vio, qs_cp_char(&vio->cl), after + n) ;

  if (after != 0)
    uty_cli_echo_n(vio, telnet_backspaces, after) ;

  vio->cl.cp  += n ;

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
uty_cli_overwrite (vty_io vio, char* chars, int n)
{
  VTY_ASSERT_LOCKED() ;

  assert((vio->cl.cp <= vio->cl.len) && (n >= 0)) ;

  if (n > 0)
    {
      qs_replace(&vio->cl, chars, n) ;
      uty_cli_echo(vio, chars, n) ;

      vio->cl.cp += n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Insert a word into vty interface with overwrite mode.
 *
 * NB: Assumes result will then be the end of the line.
 *
 * Returns number of characters inserted -- ie length of string
 */
static int
uty_cli_word_overwrite (vty_io vio, char *str)
{
  int n ;
  VTY_ASSERT_LOCKED() ;

  n = uty_cli_overwrite(vio, str, strlen(str)) ;

  vio->cl.len = vio->cl.cp ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Forward 'n' characters -- stop at end of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_forwards(vty_io vio, int n)
{
  int have ;
  VTY_ASSERT_LOCKED() ;

  have = vio->cl.len - vio->cl.cp ;
  if (have < n)
    n = have ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cli_echo(vio, qs_cp_char(&vio->cl), n) ;
      vio->cl.cp += n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Backwards 'n' characters -- stop at start of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cli_backwards(vty_io vio, int n)
{
  VTY_ASSERT_LOCKED() ;

  if ((int)vio->cl.cp < n)
    n = vio->cl.cp ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cli_echo_n(vio, telnet_backspaces, n) ;
      vio->cl.cp -= n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters -- forwards -- stop at end of line.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cli_del_forwards(vty_io vio, int n)
{
  int after ;
  int have ;

  VTY_ASSERT_LOCKED() ;

  have = vio->cl.len - vio->cl.cp ;
  if (have < n)
    n = have ;          /* cannot delete more than have */

  assert(n >= 0) ;

  if (n <= 0)
    return 0 ;

  after = qs_delete(&vio->cl, n) ;

  if (after > 0)
    uty_cli_echo(vio, qs_cp_char(&vio->cl), after) ;

  uty_cli_echo_n(vio, telnet_spaces,     n) ;
  uty_cli_echo_n(vio, telnet_backspaces, after + n) ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters before the point.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cli_del_backwards(vty_io vio, int n)
{
  return uty_cli_del_forwards(vio, uty_cli_backwards(vio, n)) ;
}

/*------------------------------------------------------------------------------
 * Move to the beginning of the line.
 *
 * Returns number of characters moved over.
 */
static int
uty_cli_bol (vty_io vio)
{
  return uty_cli_backwards(vio, vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Move to the end of the line.
 *
 * Returns number of characters moved over
 */
static int
uty_cli_eol (vty_io vio)
{
  return uty_cli_forwards(vio, vio->cl.len - vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Forward word delta -- distance to start of next word.
 *
 * Return number of characters to step over to reach next word.
 *
 * Steps over non-space characters and then any spaces.
 */
static int
uty_cli_word_forwards_delta(vty_io vio)
{
  char* cp ;
  char* tp ;
  char* ep ;

  VTY_ASSERT_LOCKED() ; ;

  assert(vio->cl.cp <= vio->cl.len) ;

  cp = qs_cp_char(&vio->cl) ;
  ep = qs_ep_char(&vio->cl) ;

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
uty_cli_word_forwards(vty_io vio)
{
  return uty_cli_forwards(vio, uty_cli_word_forwards_delta(vio)) ;
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
uty_cli_word_backwards_delta(vty_io vio, int eat_spaces)
{
  char* cp ;
  char* tp ;
  char* sp ;

  VTY_ASSERT_LOCKED() ; ;

  assert(vio->cl.cp <= vio->cl.len) ;

  cp = qs_cp_char(&vio->cl) ;
  sp = qs_chars(&vio->cl) ;

  tp = cp ;

  if (eat_spaces)
    while ((tp > sp) && (*(tp - 1) == ' '))
      --tp ;

  while ((tp > sp) && (*(tp - 1) != ' '))
    --tp ;

  return cp - tp ;
} ;

/*------------------------------------------------------------------------------
 * Backward word, but not trailing spaces.
 *
 * Move back until next (backwards) character is space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cli_word_backwards_pure (vty_io vio)
{
  return uty_cli_backwards(vio, uty_cli_word_backwards_delta(vio, 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Backward word -- move to start of previous word.
 *
 * Moves past any spaces, then move back until next (backwards) character is
 * space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cli_word_backwards (vty_io vio)
{
  return uty_cli_backwards(vio, uty_cli_word_backwards_delta(vio, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Delete to end of word -- forwards.
 *
 * Deletes any leading spaces, then deletes upto next space or end of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_word_forwards(vty_io vio)
{
  return uty_cli_del_forwards(vio, uty_cli_word_forwards_delta(vio)) ;
}

/*------------------------------------------------------------------------------
 * Delete to start of word -- backwards.
 *
 * Deletes any trailing spaces, then deletes upto next space or start of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_word_backwards(vty_io vio)
{
  return uty_cli_del_backwards(vio, uty_cli_word_backwards_delta(vio, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill rest of line from current point.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_del_to_eol (vty_io vio)
{
  return uty_cli_del_forwards(vio, vio->cl.len - vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Kill line from the beginning.
 *
 * Returns number of characters deleted.
 */
static int
uty_cli_clear_line(vty_io vio)
{
  uty_cli_bol(vio) ;
  return uty_cli_del_to_eol(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Transpose chars before or at the point.
 *
 * Return number of characters affected.
 */
static int
uty_cli_transpose_chars(vty_io vio)
{
  char  chars[2] ;
  char* cp ;

  VTY_ASSERT_LOCKED() ;

  /* Give up if < 2 characters or at start of line.                     */
  if ((vio->cl.len < 2) || (vio->cl.cp < 1))
    return 0 ;

  /* Move back to first of characters to exchange                       */
  if (vio->cl.cp == vio->cl.len)
    uty_cli_backwards(vio, 2) ;
  else
    uty_cli_backwards(vio, 1) ;

  /* Pick up in the new order                                           */
  cp = qs_cp_char(&vio->cl) ;
  chars[1] = *cp++ ;
  chars[0] = *cp ;

  /* And overwrite                                                      */
  return uty_cli_overwrite(vio, chars, 2) ;
} ;

/*==============================================================================
 * Command line history handling
 */

/*------------------------------------------------------------------------------
 * Add given command line to the history buffer.
 *
 * This is inserting the vty->buf line into the history.
 */
extern void
uty_cli_hist_add (vty_io vio, const char* cmd_line)
{
  qstring   prev_line ;
  qstring_t line ;
  int   prev_index ;

  VTY_ASSERT_LOCKED() ;

  /* Construct a dummy qstring for the given command line               */
  qs_dummy(&line, cmd_line, 1) ;        /* set cursor to the end        */

  /* make sure have a suitable history vector                           */
  vector_set_min_length(&vio->hist, VTY_MAXHIST) ;

  /* find the previous command line in the history                      */
  prev_index = vio->hindex - 1 ;
  if (prev_index < 0)
    prev_index = VTY_MAXHIST - 1 ;

  prev_line = vector_get_item(&vio->hist, prev_index) ;

  /* If the previous line is NULL, that means the history is empty.
   *
   * If the previous line is essentially the same as the current line,
   * replace it with the current line -- so that the latest whitespace
   * version is saved.
   *
   * Either way, replace the the previous line entry by moving hindex
   * back !
   */
  if ((prev_line == NULL) || (qs_cmp_sig(prev_line, &line) == 0))
    vio->hindex = prev_index ;
  else
    prev_line = vector_get_item(&vio->hist, vio->hindex) ;

  /* Now replace the hindex entry                                       */
  vector_set_item(&vio->hist, vio->hindex, qs_copy(prev_line, &line)) ;

  /* Advance to the near future and reset the history pointer           */
  vio->hindex++;
  if (vio->hindex == VTY_MAXHIST)
    vio->hindex = 0;

  vio->hp = vio->hindex;
} ;

/*------------------------------------------------------------------------------
 * Replace command line by current history.
 *
 * This function is called from vty_next_line and vty_previous_line.
 *
 * Step +1 is towards the present
 *      -1 is into the past
 */
static void
uty_cli_history_use(vty_io vio, int step)
{
  int       index ;
  unsigned  old_len ;
  unsigned  after ;
  unsigned  back ;
  qstring   hist ;

  VTY_ASSERT_LOCKED() ;

  assert((step == +1) || (step == -1)) ;

  index = vio->hp ;

  /* Special case of being at the insertion point                       */
  if (index == vio->hindex)
    {
      if (step > 0)
        return ;        /* already in the present               */

      /* before stepping back from the present, take a copy of the
       * current command line -- so can get back to it.
       */
      hist = vector_get_item(&vio->hist, vio->hindex) ;
      vector_set_item(&vio->hist, vio->hindex, qs_copy(hist, &vio->cl)) ;
    } ;

  /* Advance or retreat                                                 */
  index += step ;
  if      (index < 0)
    index = VTY_MAXHIST - 1 ;
  else if (index >= VTY_MAXHIST)
    index = 0 ;

  hist = vector_get_item(&vio->hist, index) ;

  /* If moving backwards in time, may not move back to the insertion
   * point (that would be wrapping round to the present) and may not
   * move back to a NULL entry (that would be going back before '.').
   */
  if (step < 0)
    if ((hist == NULL) || (index == vio->hindex))
      return ;

  /* Now, if arrived at the insertion point, this is returning to the
   * present, which is fine.
   */
  vio->hp = index;

  /* Move back to the start of the current line                         */
  uty_cli_bol(vio) ;

  /* Get previous line from history buffer and echo that                */
  old_len = vio->cl.len ;
  qs_copy(&vio->cl, hist) ;

  /* Sort out wiping out any excess and setting the cursor position     */
  if (old_len > vio->cl.len)
    after = old_len - vio->cl.len ;
  else
    after = 0 ;

  back = after ;
  if (vio->cl.len > vio->cl.cp)
    back += (vio->cl.len - vio->cl.cp) ;

  if (vio->cl.len > 0)
    uty_cli_echo(vio, vio->cl.body, vio->cl.len) ;

  if (after > 0)
    uty_cli_echo_n(vio, telnet_spaces,     after) ;

  if (back > 0)
    uty_cli_echo_n(vio, telnet_backspaces, back) ;

  return ;
} ;

/*------------------------------------------------------------------------------
 * Use next history line, if any.
 */
static void
uty_cli_next_line(vty_io vio)
{
  uty_cli_history_use(vio, +1) ;
}

/*------------------------------------------------------------------------------
 * Use previous history line, if any.
 */
static void
uty_cli_previous_line (vty_io vio)
{
  uty_cli_history_use(vio, -1) ;
}

/*==============================================================================
 * Command Completion and Command Description
 *
 */
static void uty_cli_describe_show(vty_io vio, vector describe) ;
static void uty_cli_describe_fold (vty_io vio, int cmd_width,
                                   unsigned int desc_width, struct desc *desc) ;
static void uty_cli_describe_line(vty_io vio, int cmd_width, const char* cmd,
                                                             const char* str) ;

static vector uty_cli_cmd_prepare(vty_io vio, int help) ;

/*------------------------------------------------------------------------------
 * Command completion
 */
static void
uty_cli_complete_command (vty_io vio, enum node_type node)
{
  unsigned i ;
  int ret ;
  int len ;
  int n ;
  vector matched ;
  vector vline ;

  VTY_ASSERT_LOCKED() ;

  /* Try and match the tokenised command line                           */
  vline = uty_cli_cmd_prepare(vio, 1) ;
  matched = cmd_complete_command (vline, node, &ret);
  cmd_free_strvec (vline);

  /* Show the result.                                                   */
  switch (ret)
    {
      case CMD_ERR_AMBIGUOUS:
        uty_cli_out_newline(vio) ;              /* clears cli_drawn     */
        uty_cli_out_CMD_ERR_AMBIGUOUS(vio) ;
        break ;

      case CMD_ERR_NO_MATCH:
        uty_cli_out_newline(vio) ;              /* clears cli_drawn     */
        uty_cli_out_CMD_ERR_NO_MATCH(vio) ;
        break ;

      case CMD_COMPLETE_FULL_MATCH:
        uty_cli_eol (vio) ;
        uty_cli_word_backwards_pure (vio);
        uty_cli_word_overwrite (vio, vector_get_item(matched, 0));
        uty_cli_insert(vio, " ", 1);
        break ;

      case CMD_COMPLETE_MATCH:
        uty_cli_eol (vio) ;
        uty_cli_word_backwards_pure (vio);
        uty_cli_word_overwrite (vio, vector_get_item(matched, 0));
        break ;

      case CMD_COMPLETE_LIST_MATCH:
        len = 6 ;
        for (i = 0; i < vector_end(matched); i++)
          {
            int sl = strlen((char*)vector_get_item(matched, i)) ;
            if (len < sl)
              len = sl ;
          } ;

        n = vio->width ;
        if (n == 0)
          n = 60 ;
        n = n / (len + 2) ;
        if (n == 0)
          n = 1 ;

        for (i = 0; i < vector_end(matched); i++)
          {
            if ((i % n) == 0)
              uty_cli_out_newline(vio) ;        /* clears cli_drawn     */
            uty_cli_out(vio, "%-*s  ", len, (char*)vector_get_item(matched, i));
          }
        uty_cli_out_newline(vio) ;

        break;

      case CMD_COMPLETE_ALREADY:
      default:
        break;
    } ;

  cmd_free_strvec(matched);
} ;

/*------------------------------------------------------------------------------
 * Command Description
 */
static void
uty_cli_describe_command (vty_io vio, enum node_type node)
{
  int ret;
  vector vline;
  vector describe;

  VTY_ASSERT_LOCKED() ;

  /* Try and match the tokenised command line                           */
  vline = uty_cli_cmd_prepare(vio, 1) ;
  describe = cmd_describe_command (vline, node, &ret);
  cmd_free_strvec (vline);

  uty_cli_out_newline(vio);     /* clears cli_drawn     */

  /* Deal with result.                                                  */
  switch (ret)
    {
    case CMD_ERR_AMBIGUOUS:
      uty_cli_out_CMD_ERR_AMBIGUOUS(vio) ;
      break ;

    case CMD_ERR_NO_MATCH:
      uty_cli_out_CMD_ERR_NO_MATCH(vio) ;
      break ;

    default:
      uty_cli_describe_show(vio, describe) ;
      break ;
    } ;

  if (describe != NULL)
    vector_free (describe);
}

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
 * If one of the options is '<cr>', that is always shown last.
 */
static void
uty_cli_describe_show(vty_io vio, vector describe)
{
  unsigned int i, cmd_width, desc_width;
  struct desc *desc, *desc_cr ;

  /* Get width of the longest "word"                                    */
  cmd_width = 0;
  for (i = 0; i < vector_active (describe); i++)
    if ((desc = vector_slot (describe, i)) != NULL)
      {
        unsigned int len;

        if (desc->cmd[0] == '\0')
          continue;

        len = strlen (desc->cmd);
        if (desc->cmd[0] == '.')
          len--;

        if (cmd_width < len)
          cmd_width = len;
      }

  /* Set width of description string.                                   */
  desc_width = vio->width - (cmd_width + 6);

  /* Print out description.                                             */
  desc_cr = NULL ;      /* put <cr> last if it appears  */

  for (i = 0; i < vector_active (describe); i++)
    if ((desc = vector_slot (describe, i)) != NULL)
      {
        if (desc->cmd[0] == '\0')
          continue;

        if (strcmp (desc->cmd, command_cr) == 0)
          {
            desc_cr = desc;
            continue;
          }

        uty_cli_describe_fold (vio, cmd_width, desc_width, desc);
      }

  if (desc_cr != NULL)
    uty_cli_describe_fold (vio, cmd_width, desc_width, desc_cr);
} ;

/*------------------------------------------------------------------------------
 * Show one word and the description, folding the description as required.
 */
static void
uty_cli_describe_fold (vty_io vio, int cmd_width,
                       unsigned int desc_width, struct desc *desc)
{
  char *buf;
  const char *cmd, *p;
  int pos;

  VTY_ASSERT_LOCKED() ;

  cmd = desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd;
  p = desc->str ;

  /* If have a sensible description width                       */
  if (desc_width > 20)
    {
      buf = XCALLOC (MTYPE_TMP, strlen (desc->str) + 1);

      while (strlen (p) > desc_width)
        {
          /* move back to first space                   */
          for (pos = desc_width; pos > 0; pos--)
            if (*(p + pos) == ' ')
              break;

          /* if did not find a space, break at width    */
          if (pos == 0)
            pos = desc_width ;

          strncpy (buf, p, pos);
          buf[pos] = '\0';
          uty_cli_describe_line(vio, cmd_width, cmd, buf) ;

          cmd = "";             /* for 2nd and subsequent lines */

          p += pos ;            /* step past what just wrote    */
          while (*p == ' ')
            ++p ;               /* skip spaces                  */
        } ;

      XFREE (MTYPE_TMP, buf);
    } ;

  uty_cli_describe_line(vio, cmd_width, cmd, p) ;
} ;

/*------------------------------------------------------------------------------
 * Show one description line.
 */
static void
uty_cli_describe_line(vty_io vio, int cmd_width, const char* cmd,
                                                      const char* str)
{
  if (str != NULL)
    uty_cli_out (vio, "  %-*s  %s", cmd_width, cmd, str) ;
  else
    uty_cli_out (vio, "  %-s", cmd) ;
  uty_cli_out_newline(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Prepare "vline" token array for command handler.
 *
 * For "help" (command completion/description), if the command line is empty,
 * or ends in ' ', adds an empty token to the end of the token array.
 */
static vector
uty_cli_cmd_prepare(vty_io vio, int help)
{
  vector vline ;

  vline = cmd_make_strvec(qs_term(&vio->cl)) ;

  /* Note that if there is a vector of tokens, then there is at least one
   * token, so can guarantee that vio->cl.len >= 1 !
   */
  if (help)
    if ((vline == NULL) || isspace(*qs_chars_at(&vio->cl, vio->cl.len - 1)))
      vline = cmd_add_to_strvec(vline, "") ;

  return vline ;
} ;

/*==============================================================================
 * VTY telnet stuff
 */

#define TELNET_OPTION_DEBUG 0           /* 0 to turn off        */

static const char* telnet_commands[256] =
{
  [tn_IAC  ] = "IAC",
  [tn_DONT ] = "DONT",
  [tn_DO   ] = "DO",
  [tn_WONT ] = "WONT",
  [tn_WILL ] = "WILL",
  [tn_SB   ] = "SB",
  [tn_GA   ] = "GA",
  [tn_EL   ] = "EL",
  [tn_EC   ] = "EC",
  [tn_AYT  ] = "AYT",
  [tn_AO   ] = "AO",
  [tn_IP   ] = "IP",
  [tn_BREAK] = "BREAK",
  [tn_DM   ] = "DM",
  [tn_NOP  ] = "NOP",
  [tn_SE   ] = "SE",
  [tn_EOR  ] = "EOR",
  [tn_ABORT] = "ABORT",
  [tn_SUSP ] = "SUSP",
  [tn_EOF  ] = "EOF",
} ;

static const char* telnet_options[256] =
{
  [to_BINARY]       = "BINARY",      /* 8-bit data path                  */
  [to_ECHO]         = "ECHO",        /* echo                             */
  [to_RCP]          = "RCP",         /* prepare to reconnect             */
  [to_SGA]          = "SGA",         /* suppress go ahead                */
  [to_NAMS]         = "NAMS",        /* approximate message size         */
  [to_STATUS]       = "STATUS",      /* give status                      */
  [to_TM]           = "TM",          /* timing mark                      */
  [to_RCTE]         = "RCTE",        /* remote controlled tx and echo    */
  [to_NAOL]         = "NAOL",        /* neg. about output line width     */
  [to_NAOP]         = "NAOP",        /* neg. about output page size      */
  [to_NAOCRD]       = "NAOCRD",      /* neg. about CR disposition        */
  [to_NAOHTS]       = "NAOHTS",      /* neg. about horizontal tabstops   */
  [to_NAOHTD]       = "NAOHTD",      /* neg. about horizontal tab disp.  */
  [to_NAOFFD]       = "NAOFFD",      /* neg. about formfeed disposition  */
  [to_NAOVTS]       = "NAOVTS",      /* neg. about vertical tab stops    */
  [to_NAOVTD]       = "NAOVTD",      /* neg. about vertical tab disp.    */
  [to_NAOLFD]       = "NAOLFD",      /* neg. about output LF disposition */
  [to_XASCII]       = "XASCII",      /* extended ascii character set     */
  [to_LOGOUT]       = "LOGOUT",      /* force logout                     */
  [to_BM]           = "BM",          /* byte macro                       */
  [to_DET]          = "DET",         /* data entry terminal              */
  [to_SUPDUP]       = "SUPDUP",      /* supdup protocol                  */
  [to_SUPDUPOUTPUT] = "SUPDUPOUTPUT",/* supdup output                    */
  [to_SNDLOC]       = "SNDLOC",      /* send location                    */
  [to_TTYPE]        = "TTYPE",       /* terminal type                    */
  [to_EOR]          = "EOR",         /* end or record                    */
  [to_TUID]         = "TUID",        /* TACACS user identification       */
  [to_OUTMRK]       = "OUTMRK",      /* output marking                   */
  [to_TTYLOC]       = "TTYLOC",      /* terminal location number         */
  [to_3270REGIME]   = "3270REGIME",  /* 3270 regime                      */
  [to_X3PAD]        = "X3PAD",       /* X.3 PAD                          */
  [to_NAWS]         = "NAWS",        /* window size                      */
  [to_TSPEED]       = "TSPEED",      /* terminal speed                   */
  [to_LFLOW]        = "LFLOW",       /* remote flow control              */
  [to_LINEMODE]     = "LINEMODE",    /* Linemode option                  */
  [to_XDISPLOC]     = "XDISPLOC",    /* X Display Location               */
  [to_OLD_ENVIRON]  = "OLD_ENVIRON", /* Old - Environment variables      */
  [to_AUTHENTICATION] = "AUTHENTICATION",  /* Authenticate               */
  [to_ENCRYPT]      = "ENCRYPT",     /* Encryption option                */
  [to_NEW_ENVIRON]  = "NEW_ENVIRON", /* New - Environment variables      */
  [to_EXOPL]        = "EXOPL",       /* extended-options-list            */
} ;

/*------------------------------------------------------------------------------
 * For debug.  Put string or value as decimal.
 */
static void
uty_cli_out_dec(vty_io vio, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cli_out(vio, "%s ", str) ;
  else
    uty_cli_out(vio, "%d ", (int)u) ;
} ;

/*------------------------------------------------------------------------------
 * For debug.  Put string or value as hex.
 */
static void
uty_cli_out_hex(vty_io vio, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cli_out(vio, "%s ", str) ;
  else
    uty_cli_out(vio, "0x%02x ", (unsigned)u) ;
} ;

/*------------------------------------------------------------------------------
 * Send telnet: "WILL TELOPT_ECHO"
 */
static void
uty_will_echo (vty_io vio)
{
  unsigned char cmd[] = { tn_IAC, tn_WILL, to_ECHO };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "suppress Go-Ahead"
 */
static void
uty_will_suppress_go_ahead (vty_io vio)
{
  unsigned char cmd[] = { tn_IAC, tn_WILL, to_SGA };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use linemode"
 */
static void
uty_dont_linemode (vty_io vio)
{
  unsigned char cmd[] = { tn_IAC, tn_DONT, to_LINEMODE };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "Use window size"
 */
static void
uty_do_window_size (vty_io vio)
{
  unsigned char cmd[] = { tn_IAC, tn_DO, to_NAWS };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use lflow"       -- not currently used
 */
static void
uty_dont_lflow_ahead (vty_io vio)
{
  unsigned char cmd[] = { tn_IAC, tn_DONT, to_LFLOW };
  VTY_ASSERT_LOCKED() ;
  uty_cli_write (vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * The keystroke iac callback function.
 *
 * This deals with IAC sequences that should be dealt with as soon as they
 * are read -- not stored in the keystroke stream for later processing.
 */
extern bool
uty_cli_iac_callback(keystroke_iac_callback_args)
{
  return uty_telnet_command((vty_io)context, stroke, true) ;
} ;

/*------------------------------------------------------------------------------
 * Process incoming Telnet Option(s)
 *
 * May be called during keystroke iac callback, or when processing CLI
 * keystrokes.
 *
 * In particular: get telnet window size.
 *
 * Returns: true <=> dealt with, for:
 *
 *                    * telnet window size.
 */
static bool
uty_telnet_command(vty_io vio, keystroke stroke, bool callback)
{
  uint8_t* p ;
  uint8_t  o ;
  int left ;
  bool dealt_with ;

  /* Echo to the other end if required                                  */
  if (TELNET_OPTION_DEBUG)
    {
      uty_cli_wipe(vio, 0) ;

      p    = stroke->buf ;
      left = stroke->len ;

      uty_cli_out_hex(vio, telnet_commands[tn_IAC], tn_IAC) ;

      if (left-- > 0)
        uty_cli_out_dec(vio, telnet_commands[*p], *p) ;
      ++p ;

      if (left-- > 0)
        uty_cli_out_dec(vio, telnet_options[*p], *p) ;
      ++p ;

      if (left > 0)
        {
          while(left-- > 0)
            uty_cli_out_hex(vio, NULL, *p++) ;

          if (stroke->flags & kf_truncated)
            uty_cli_out(vio, "... ") ;

          if (!(stroke->flags & kf_broken))
            {
              uty_cli_out_hex(vio, telnet_commands[tn_IAC], tn_IAC) ;
              uty_cli_out_hex(vio, telnet_commands[tn_SE], tn_SE) ;
            }
        } ;

      if (stroke->flags & kf_broken)
        uty_cli_out (vio, "BROKEN") ;

      uty_cli_out (vio, "\r\n") ;
    } ;

  /* Process the telnet command                                         */
  dealt_with = false ;

  if (stroke->flags != 0)
    return dealt_with ;         /* go no further if broken              */

  p    = stroke->buf ;
  left = stroke->len ;

  passert(left >= 1) ;            /* must be if not broken !            */
  passert(stroke->value == *p) ;  /* or something is wrong              */

  ++p ;         /* step past X of IAC X */
  --left ;

  /* Decode the one command that is interesting -- "NAWS"               */
  switch (stroke->value)
  {
    case tn_SB:
      passert(left > 0) ;       /* or parser failed     */

      o = *p++ ;                /* the option byte      */
      --left ;
      switch(o)
      {
        case to_NAWS:
          if (left != 4)
            {
              uzlog(NULL, LOG_WARNING,
                        "RFC 1073 violation detected: telnet NAWS option "
                        "should send %d characters, but we received %d",
                        (3 + 4 + 2), (3 + left + 2)) ;
            }
          else
            {
              vio->width   = *p++ << 8 ;
              vio->width  += *p++ ;
              vio->height  = *p++ << 8 ;
              vio->height += *p ;

              if (TELNET_OPTION_DEBUG)
                uty_cli_out(vio, "TELNET NAWS window size received: "
                                 "width %d, height %d%s",
                                  vio->width, vio->height, telnet_newline) ;
              uty_set_height(vio) ;

              dealt_with = true ;
            } ;
          break ;

        default:        /* no other IAC SB <option>     */
          break ;
      } ;
      break ;

    default:            /* no other IAC X               */
      break ;
  } ;

  return dealt_with ;
} ;

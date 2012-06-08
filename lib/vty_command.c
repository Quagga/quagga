/* VTY for command execution
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
#include "stdio.h"

#include "vty_command.h"
#include "command_local.h"
#include "command_queue.h"
#include "command_execute.h"
#include "command_parse.h"
#include "vty_local.h"
#include "vty_io.h"
#include "vty_cli.h"
#include "vio_fifo.h"
#include "vty_io_file.h"
#include "vty_io_term.h"
#include "vty_io_vtysh.h"
#include "vty_io_std.h"
#include "vty_vtysh.h"
#include "log.h"
#include "list_util.h"
#include "qstring.h"

/*==============================================================================
 * vty_command.c contains functions used by the command processing, where
 * that interacts with the vty -- in particular vty I/O.
 *
 * There are three command loops -- cmd_read_config(), cq_process() and
 * vty_vtysh_command_loop().  Each command loop appears to be a thread of
 * control sucking in command lines, parsing and executing them.  In the
 * process, input and output pipes are opened and closed, and the vty stack
 * grows and shrinks.
 *
 * For cmd_read_config() -- see command_execute.c -- the command loop is,
 * indeed, a thread of control and all the I/O operations are "blocking", so
 * that any waiting required is done inside the loop.
 *
 * For cq_process() -- see command_queue.c -- things are a lot more
 * complicated: first, I/O is non-blocking, so I/O operations may return
 * CMD_WAITING and the command loop must exit and be able to restart, when
 * the I/O completes in the background; second, in a multi-pthread environment
 * some operations must be performed in the CLI pthread while others must be
 * performed in the command (the Routing Engine, generally) pthread.  (It would
 * be easier if each cq_process instance were a separate pthread, but we here
 * implement it as a form of co-routine, driven by messages.)
 *
 * For vty_vtysh_command_loop() -- see vty_vtysh.c -- the command loop is,
 * indeed, a thread of control and all the I/O operations are "blocking", so
 * that any waiting required is done inside the loop.
 *
 * The I/O is structured so that all output is buffered.  When output is
 * "pushed" an attempt will be made to empty the buffers, but the operation
 * will not block.  When an output is closed, the close operation will keep
 * pushing (blocking as required) until all buffers are empty, and only then
 * will the close complete.  Command line fetch for the base level input
 * may wait for all output to complete before proceeding.
 *
 * Generally all commands output only to the output buffer.  Only when the
 * command finishes will the output be "pushed", and at that point it may be
 * discarded (for example, when reading configuration file).
 *
 * So a command loop takes the general form:
 *
 *   loop:   fetch command line             -- break if nothing, fail or cancel
 *           parse command line             -- loop if empty or all comment
 *                                          -- break if parsing fails
 *           reflect command line           -- break if fail or cancel
 *           do any pipe open actions       -- break if fail or cancel
 *           dispatch command (if any)
 *           complete command               -- break if any command error
 *           push command output to output  -- break if fail or cancel
 *             -- loop
 *
 *   hiatus: deal with issue(s) (may loop)
 *             -- return to loop if OK and not waiting for input
 *           exit command loop
 *
 * In the loop, if any operation receives a return code it cannot immediately
 * deal with, it breaks out to the "hiatus".  Once the issue is dealt with, can
 * leave the hiatus and return to the top of the loop.
 *
 * Note that in hiatus the command loop may be waiting for some output I/O
 * to complete -- e.g. while closing an output pipe.
 *
 * So, most obviously for cq_process(), the loop is a co-routine which exits
 * and is re-entered at the hiatus.  When the loop does exit, it has either
 * come to a dead stop (for a number of reasons) or it is waiting for input.
 *
 * The state of the command loop is vty->vio->cl_state.  The main states are:
 *
 *   vcl_cq_running   -- somewhere in the "cq" command loop, doing things.
 *
 *   vcl_cq_waiting   -- waiting for I/O in the "cq" command loop
 *
 *   vcl_running      -- somewhere in the config reading command loop
 *
 * When some event (such as some input arriving) occurs it is signalled to
 * the command loop by uty_cmd_signal(), where the value of the signal is
 * a CMD_XXXX code.  If the loop is in vcl_cq_waiting state, the loop can be
 * re-entered (at the hiatus point) with the return code.  Otherwise, what
 * happens depends on the state and the return code -- see uty_cmd_signal().
 *
 * Functions called by the various steps in the command loop will check for
 * a pending signal, and will force a jump to hiatus.  (This is how the command
 * loop may be "interrupted" by, for example an I/O error detected in the
 * pselect() process.)
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static bool uty_cmd_loop_prepare(vty_io vio) ;
static cmd_ret_t uty_cmd_hiatus(vty_io vio) ;
static cmd_ret_t vty_cmd_auth(vty vty, node_type_t* p_next_node) ;
static void uty_cmd_out_cancel(vty_io vio) ;
static uint uty_show_error_context(vio_fifo ebuf, vio_vf vf) ;
static uint uty_cmd_failed(vty_io vio, cmd_ret_t ret) ;

/*==============================================================================
 * Starting up, communicating with and closing down a command loop.
 */

/*------------------------------------------------------------------------------
 * Prepare to enter the config read command loop.
 *
 * Initialise exec object, and copy required settings from the current vin
 * and vout.
 *
 * The given vty must be VTY_STDOUT, at vin_depth == 1 and vout_depth == 1.
 *
 * Default settings for configuration file reader are:
 *
 *   context->reflect         = false
 *   context->out_ordinary    = false
 *   context->out_warning     = true
 *   context->warn_stop       = true
 *
 * The "ignore_warnings" option disables output of warnings and stop on
 * warning(s) -- result is just like "<*" file input.
 *
 * The "no_stop_on_warning" option disables stopping on warnings, and will
 * not stop when the configuration is read.
 *
 * Returns:  true  <=> acquired or did not need config symbol of power
 *       or: false <=> needed but could not acquire symbol of power
 */
extern bool
vty_cmd_config_loop_prepare(vty vty, int conf_fd, qpath path,
                                                         bool ignore_warnings,
                                                         bool show_warnings)
{
  bool ok ;

  VTY_LOCK() ;

  qassert(vty->type == VTY_STDOUT) ;

  vty->node = CONFIG_NODE ;             /* to read config !             */
  ok = uty_cmd_loop_prepare(vty->vio) ; /* by vty->type & vty->node     */

  if (ok)
    {
      uty_config_read_open(vty->vio, conf_fd, path) ;
      cmd_context_config_set(vty->exec->context, ignore_warnings,
                                                 show_warnings) ;
    } ;

  VTY_UNLOCK() ;

  return ok ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to execute commands from configuration file in the vtysh own vty.
 *
 * For vtysh, the vtysh's own vty is already set up.
 *
 * Expect the vty->node to be set to CONFIG_NODE.
 *
 * The configuration file has been opened, so now is the time to push it as
 * a suitable vin, and then adjust context and prepare to execute configuration
 * commands.
 */
extern void
vty_cmd_vtysh_config_prepare(vty vty, int conf_fd, qpath path,
                                                         bool ignore_warnings,
                                                         bool show_warnings)
{
  vty_io vio ;
  cmd_context context ;

  VTY_LOCK() ;

  vio = vty->vio ;

  qassert(vty->type == VTY_VTYSH) ;
  qassert(vty->exec != NULL) ;
  context = vty->exec->context ;
  qassert(context != NULL) ;
  qassert((vio->vin_depth == 1) && (vio->vin->context == context)) ;
  qassert(vio->vout_depth == 1) ;

  qassert(vio->vin->vin_type == VIN_VTYSH) ;
  qassert( (vio->vout->vout_type == VOUT_VTYSH) ||
           (vio->vout->vout_type == VOUT_STDOUT) ) ;

  /* Push the config reader vin onto the vin stack
   */
  uty_config_read_open(vio, conf_fd, path) ;

  qassert(vio->vin->context == context) ;

  /* Adjust context as required for configuration file reading
   */
  context->node = context->cnode = CONFIG_NODE ;

  cmd_context_config_set(context, ignore_warnings, show_warnings) ;

  uty_cmd_prepare(vio) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Enter the command_queue command loop.
 *
 * The loop is entered as if a phantom "start" command has completed.  The
 * result of that command must now be output, and then if all is OK, can start
 * executing commands.
 *
 * For VTY_TERMINAL, the "start" command puts up the hello message and/or motd,
 * and if required notes that the next thing is user authentication.
 *
 * For VTY_VTYSH_SERVER, the "start" command puts out the daemon name and
 * version number.
 */
extern void
uty_cmd_queue_loop_enter(vty_io vio)
{
  bool ok ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert( (vio->vty->type == VTY_TERMINAL) ||
          (vio->vty->type == VTY_VTYSH_SERVER) ) ;

  ok = uty_cmd_loop_prepare(vio) ;      /* by vty->type & vty->node     */

  if (!ok)
    uty_out(vio, "%% unable to start in config mode\n") ;

  qassert(vio->cl_state == vcl_stopped) ;   /* not yet started      */

  vio->cl_state = vcl_cq_running ;

  cq_loop_enter(vio->vty, ok ? CMD_SUCCESS : CMD_WARNING) ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to execute commands in the vtysh own vty.
 *
 * There is only ever one vty in the vtysh -- so should never be any issue
 * of gaining its own config symbol of power.
 *
 * Returns:  true  <=> acquired or did not need config symbol of power
 *       or: false <=> needed but could not acquire symbol of power
 */
extern bool
uty_cmd_vtysh_prepare(vty_io vio)
{
  assert(vio->vty->type == VTY_VTYSH);

  return uty_cmd_loop_prepare(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to enter a command loop.
 *
 * Create and initialise cmd_exec object.
 *
 * From now on we vst_cmd_running_executing -- executing phantom "start"
 * command -- noting that uty_xxx_out_push() will not actually output stuff
 * if is not vst_cmd_running, and will drop to vst_cmd_complete if is not
 * vst_cmd_executing !
 *
 * The initial state depends on the vty->type and vty->node.
 *
 * The vty->node may be EXIT_NODE, if started up but found that it could not
 * continue... may have issued hello and/or goodbye messages, which will be
 * output and then the command loop will exit.
 *
 * If the vty->node has not been set (so is NULL_NODE) we set EXIT_NODE.
 *
 * Returns:  true  <=> acquired or did not need config symbol of power
 *       or: false <=> needed but could not acquire symbol of power
 */
static bool
uty_cmd_loop_prepare(vty_io vio)
{
  bool ok ;
  vty vty = vio->vty ;

  VTY_ASSERT_LOCKED() ;

  qassert(vty->exec == NULL) ;
  qassert((vio->vin_depth == 1) && (vio->vin->context != NULL)) ;
  qassert(vio->vout_depth == 1) ;

  vty->exec = cmd_exec_new(vty, vio->vin->context) ;
  vio->state = vst_cmd_running_executing ;

  ok = uty_cmd_config_lock(vio, vty->node) ;
  if (!ok)
    vty->node = cmd_node_enable_parent(vty->node) ;

  if (vty->node == NULL_NODE)
    vty->node = EXIT_NODE ;

  cmd_context_init(vio->vin->context, vty->type, vty->node) ;

  vio->vout->out_ordinary = vio->vin->context->out_ordinary ;

  uty_cmd_prepare(vio) ;

  return ok ;
} ;

/*------------------------------------------------------------------------------
 * When entering command loop, or after opening or closing a vin/vout object,
 * update the vty->exec context.
 *
 * Reflection of the command line depends on the current context.
 *
 * Output to the vout_base is suppressed for reading of configuration files,
 * but only while the output is the base output, and only if is not currently
 * reflecting commands.
 */
extern void
uty_cmd_prepare(vty_io vio)             /* TODO Kill ?? */
{
} ;

/*------------------------------------------------------------------------------
 * Signal to the command loop that some I/O has completed -- successfully, or
 * that some exception has been raised.
 *
 * NB: for CMD_CANCEL, CMD_IO_ERROR and CMD_STOP, the primary action is to
 *     raise a vx_xxx exception -- which will post a signal.
 *
 *     This is *not* the mechanism for raising those exceptions.
 *
 * The vty->signal may be set to any of:
 *
 *   CMD_SUCCESS  -- no signal outstanding
 *
 *   CMD_HIATUS   -- signals need to enter the hiatus for some reason.
 *
 *   CMD_CANCEL   -- some exception involving cancel
 *
 *   CMD_IO_ERROR -- some exception involving I/O error or timeout
 *
 * Accepts the above return codes and sets vio->signal if it is not already set
 * to a higher priority value.
 *
 * If the command loop is vcl_cq_waiting, sets it going again and passes in the
 * current vio->signal.
 *
 * Also accepts:
 *
 *   CMD_WAITING  -- which is ignored
 *
 *   CMD_STOP     -- which is translated to CMD_CANCEL
 *
 *                   caller will have raised a vx_stop exception
 *
 * No other return codes are valid for uty_cmd_signal(), and will be ignored.
 *
 * It is convenient for vio->signal == CMD_SUCCESS to mean "no signal", so that
 * success does not interrupt operations which poll for a signal.  Note that
 * CMD_SUCCESS causes the command loop to be woken up if is vcl_cq_waiting.
 *
 * Returns:  the return code, except will be CMD_CANCEL if CMD_STOP passed in.
 *
 * Note: CMD_STOP as a return code from the hiatus means that it has stopped.
 *       The hiatus does accept CMD_STOP as an instruction to stop.  But to
 *       avoid the ambiguity, the signal converted to CMD_CANCEL.
 */
extern cmd_ret_t
uty_cmd_signal(vty_io vio, cmd_ret_t ret)
{
  cmd_ret_t sig ;

  static uint8_t signal_priority [] =
    {
      [CMD_SUCCESS]   = 1,
      [CMD_HIATUS]    = 2,
      [CMD_CANCEL]    = 3,
      [CMD_IO_ERROR]  = 4,
    } ;

  VTY_ASSERT_LOCKED() ;

  sig = ret ;

  switch(ret)
    {
      case CMD_SUCCESS:
        break ;                 /* accept, leave vio->signal as is      */

      case CMD_WAITING:
        return ret ;            /* accept, but ignore                   */

      case CMD_HIATUS:
        break ;

      case CMD_STOP:
        sig = CMD_CANCEL ;
        break ;

      case CMD_CANCEL:
        break ;

      case CMD_IO_ERROR:
        break ;

      default:
        qassert(false) ;        /* nothing else is valid        */
        sig = CMD_HIATUS ;
    } ;

  qassert( (vio->signal == CMD_SUCCESS) || (vio->signal == CMD_HIATUS)
                                        || (vio->signal == CMD_CANCEL)
                                        || (vio->signal == CMD_IO_ERROR) ) ;

  if (signal_priority[sig] > signal_priority[vio->signal])
    vio->signal = sig ;

  if (vio->cl_state == vcl_cq_waiting)
    {
      /* pass in the current signal !
       */
        vio->cl_state = vcl_cq_running ;
        cq_loop_resume(vio->vty, vio->signal) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Tell the command loop to suspend or stop, soonest -- SIGHUP etc.
 *
 * Does nothing if the vty is already set to suspend or is suspended.  Caller
 * needs to resolve the issue of more than one suspend -- or may treat
 * multiple suspends as one !
 *
 * Issues a vx_suspend or vx_stop exception and sets suitable suspend or close
 * reason.  Signals a CMD_CANCEL.
 *
 * The given "why" will be output: "% Suspended: <why>"
 *                             or: "Terminated: <why>"
 * with suitable newlines etc.
 */
extern void
uty_cmd_loop_suspend(vty_io vio, const char* why)
{
  vio_exception_t vx ;

  VTY_ASSERT_LOCKED() ;

  if (vio->state & (vst_suspend | vst_suspended))
    return ;

  /* Not all VTY can be suspended.
   */
  switch(vio->vty->type)
    {
      case VTY_TERMINAL:
      case VTY_VTYSH_SERVER:
        vx = vx_suspend ;       /* can suspend          */
        break ;

      case VTY_STDOUT:
      case VTY_VTYSH:
        vx = vx_stop ;          /* cannot suspend       */
        break ;

      default:
        qassert(false) ;
        vx = vx_stop ;
        break ;
    } ;

  uty_vio_exception(vio, vx) ;  /* raise the exception  */

  /* Set suspend_reason or close reason
   */
  if ((why == NULL) || (*why == '\0'))
    why = "*empty suspend_reason(BUG)*" ;

  if (vx == vx_suspend)
    uty_suspend_reason_set(vio, why) ;
  else
    uty_close_reason_set(vio, why, false) ;
} ;

/*------------------------------------------------------------------------------
 * Tell the command loop to resume processing commands in the given vty
 *
 * In general terms, clear suspension bits and signal CMD_SUCCESS.
 */
extern void
uty_cmd_loop_resume(vty_io vio)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  qassert( (vio->state & (vst_suspend | vst_suspended))
                      == (vst_suspend | vst_suspended) ) ;

  vio->state &= ~(vst_suspend | vst_suspended) ;
  uty_suspend_reason_clear(vio) ;       /* tidy */

  uty_cmd_signal(vio, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Tell the command loop to stop, soonest -- SIGTERM, SIGINT, etc.
 *
 * In general terms, if not "curtains" we signal the command loop to CMD_STOP.
 *
 * The cq command loop has its own stop mechanism, which does its best to
 * bring the command loop under control and if it can, run the hiatus at
 * least once to do as much of the vio->stop action as it can -- who knows
 * how much longer messages/threads will continue to be processed ?
 *
 * If this is "curtains" then there is only one pthread left and never intend
 * to run any command loop ever again, so can force the issue, even if is
 * vcl_running/vcl_cq_running.
 *
 * The "why" will be output: "Terminated: <why>" (if close_reason not already
 * set).
 *
 * NB: if completely successful in stopping the command loop, and at "curtains"
 *     the vty will be freed here and now.
 */
extern void
uty_cmd_loop_stop(vty_io vio, const char* why, bool curtains)
{
  bool done ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  /* The first close_reason set is latched -- so, when vty_terminate() runs,
   * if an earlier vty_stop() has run, then its close_reason will be kept.
   * Similarly, if a time-out is closing the vty when vty_stop() runs, the
   * earlier close_reason is kept.
   */
  uty_close_reason_set(vio, why, false /* don't replace */) ;

  /* If is vcl_cq_running or vcl_cq_waiting, we let the cq command loop have
   * a crack at stopping itself.
   */
  switch (vio->cl_state)
    {
      case vcl_stopped:
      default:
        done = true ;           /* should not happen, but assume done ! */
        break ;

      case vcl_running:
        done = false ;
        break ;

      case vcl_cq_running:
      case vcl_cq_waiting:
        done = cq_loop_stop(vio) ;
        break ;
    } ;

  if (done || curtains)
    uty_close(vio) ;            /* NB: vty will be freed                */
  else
    uty_vio_exception(vio, vx_stop) ;
} ;

/*==============================================================================
 * Command line fetch.
 *
 * Will read command lines from the current input, until that returns eof, or
 * eof is induced (for example by EXIT_NODE).
 *
 * Before attempting to read, will check for a vio->signal, and that the input
 * and output stack depths do not require attention.
 *
 * NB: all closing of inputs and/or outputs is done in the hiatus, below.
 */

/*------------------------------------------------------------------------------
 * Fetch the next command line to be executed.
 *
 * Returns: CMD_SUCCESS  => OK -- have a line ready to be processed.
 *
 *      or: CMD_WAITING  => OK -- but waiting for command line to arrive
 *
 *      or: CMD_HIATUS   => OK -- but may need to close one or more vin/vout
 *                                to adjust stack.
 *
 *      or: CMD_IO_ERROR => failed while reading -- I/O error or timeout.
 *
 *      or: CMD_ERROR    => some semantic failure
 *
 * NB: it is expected that all is well before this is called.
 *
 * NB: command execution is deemed to start from the moment this is called.
 *
 *     So, all output had better be prepared for command results !
 *
 * NB: can be called from any thread -- because does no closing of files or
 *     anything other than read/write.
 *
 * NB: for vin_sh_serv we set vio->ret to CMD_SUCCESS before fetching the next
 *     command.  That will stay as CMD_SUCCESS until overridden by some other
 *     return code in the hiatus.
 *
 *     vty_cmd_complete() will push the result if is CMD_SUCCESS, and the
 *     output side will pick up vio->ret and send that (when at the base vout).
 *
 *     The hiatus updates vio->ret in the event of any error, or CMD_CANCEL.
 */
extern cmd_ret_t
vty_cmd_line_fetch(vty vty)
{
  cmd_ret_t ret ;
  vty_io    vio ;
  vio_vf    vf ;

  VTY_LOCK() ;

  vio = vty->vio ;              /* once locked          */
  vf  = vio->vin ;

  if (vf->context->node == EXIT_NODE)
     {
       /* This is important.  Any command that causes exit runs to completion,
        * as an ordinary command -- we do not cease until start to fetch the
        * next command.
        *
        * For VTY_TERMINAL, this means that all --more-- etc will run and
        * all output will be written away, *before* start to close.
        *
        * Similarly, for VTY_VTYSH_SERVER, will have sent the command complete
        * message to the vtysh *before* starting to close.
        */
       uty_vf_read_stop(vf, vfs_stop_cease) ;
       ret = CMD_HIATUS ;
     }

  else if (vio->vin_depth < vio->vout_depth)
    {
      /* This is where we close output pipes which last only for the duration
       * of one command line.
       */
       ret = CMD_HIATUS ;
    }

  else if ( (vio->vin_depth < 1)
         || ((vf->vin_state | vio->vout->vout_state) & (vf_cease | vf_cancel)))
    {
      /* Do not expect to arrive here in any of these states... but one thing
       * is sure: should let the hiatus deal with them !
       */
      qassert(false) ;
      ret = CMD_HIATUS ;
    }
  else
    {
      /* We know that we are: vin_active and vout_active
       *                      vin_depth >= vout_depth >= 1
       *
       * NB: these functions will uty_vf_read_stop(vf, vfs_stop_cease) when
       *     eof is met and there is nothing more to do -- that is, they do
       *     NOT cease the input until *after* the last line has been
       *     completely processed and we return here to fetch the *next* line.
       *     See note above re "EXIT_NODE".
       */
      qassert(vio->vin_depth != 0) ;

      switch (vf->vin_type)
        {
          case VIN_NONE:
            zabort("invalid VIN_NONE") ;
            break ;

          case VIN_TERM:
            ret = uty_term_cmd_line_fetch(vf) ;
            break ;

          case VIN_VTYSH_SERVER:
            vio->sh_serv_ret = CMD_SUCCESS ;

            ret = uty_sh_serv_cmd_line_fetch(vf) ;
            break ;

          case VIN_VTYSH:               /* vtysh *own* vty      */
            ret = uty_vtysh_cmd_line_fetch(vf) ;
            break ;

          case VIN_DEV_NULL:
            uty_vf_read_stop(vf, vfs_stop_cease) ; /* immediate close ! */
            ret = CMD_HIATUS ;
            break ;

          case VIN_CONFIG:
          case VIN_FILE:
          case VIN_PIPE:
            ret = uty_file_cmd_line_fetch(vf) ;
            break ;

          default:
            zabort("unknown vin_type") ;
            break ;
        } ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*==============================================================================
 * Special command handling functions.
 */

/*------------------------------------------------------------------------------
 * Handle a "special" command -- anything not cmd_do_command.
 *
 * These "commands" are related to VTY_TERMINAL CLI only.
 *
 * NB: these are unlike ordinary commands, in particular:
 *
 *       * the node may change on something other than CMD_SUCCESS.
 *
 *       * may change to one of a number of nodes -- not just to one,
 *         pre-ordained node.
 *
 * Returns:  CMD_SUCCESS  -- OK
 *           CMD_WARNING  -- failed or only partial success
 *           CMD_ERROR    -- definitely failed
 */
extern cmd_ret_t
vty_cmd_special(vty vty)
{
  cmd_ret_t   ret ;
  node_type_t node_next ;
  cmd_context context ;

  context = vty->exec->context ;

  ret = CMD_SUCCESS ;

  vty->node = context->node ;   /* as per all commands  */
  node_next = context->node ;   /* by default.          */

  switch (context->to_do & cmd_do_mask)
    {
      /* For ^C (and cmd_do_nothing) we do nothing -- whatever was on the
       * command line is cancelled and ignored.
       */
      case cmd_do_ctrl_c:
      case cmd_do_nothing:
        break ;

      /* For eof and time-out, proceed to exit.
       */
      case cmd_do_eof:
      case cmd_do_timed_out:
        node_next = EXIT_NODE ;
        break ;

      /* Ordinary command must be cmd_do_auth to have got here !  In which case,
       * that's what we do.
       */
      case cmd_do_command:
        if ((context->to_do & cmd_do_auth) != 0)
          {
            ret = vty_cmd_auth(vty, &node_next) ;
          }
        else
          {
            vty_out(vty, "%%*** %s: invalid cmd_do_command\n", __func__) ;
            ret = CMD_ERROR ;
          } ;
        break ;

      /* ^D for cmd_do_auth means exit.
       *
       * ^D otherwise means nothing, and should not appear !
       */
      case cmd_do_ctrl_d:
        if ((context->to_do & cmd_do_auth) != 0)
          node_next = EXIT_NODE ;
        else
          {
            vty_out(vty, "%%*** %s: invalid cmd_do_ctrl_d\n", __func__) ;
            ret = CMD_ERROR ;
          }
        break ;

      /* ^Z for cmd_do_auth means cancel and go to fall-back node.
       *
       *      for AUTH_ENABLE_NODE, fall-back == where came from
       *
       *      for AUTH_NODE (or anything else), fall-back == EXIT_NODE
       *
       * ^Z for ordinary commands, means "end".
       */
      case cmd_do_ctrl_z:
        if ((context->to_do & cmd_do_auth) != 0)
          {
            if (context->node == AUTH_ENABLE_NODE)
              node_next = context->onode ;
            else
              node_next = EXIT_NODE ;
          }
        else
          node_next = cmd_node_end_to(vty->node) ;
        break ;

      /* Anything else is unknown !
       */
      default:
        vty_out(vty, "%%*** %s: unknown or invalid cmd_do %d\n",
                                                     __func__, context->to_do) ;
        ret = CMD_ERROR ;
    } ;

  qassert(node_next != NULL_NODE) ;
  if (node_next == NULL_NODE)
    node_next = EXIT_NODE ;

  /* Now worry about changing node
   *
   * NB: if is changing down below CONFIG, will release the symbol of power as
   *     required.
   *
   *     if is changing up to CONFIG or SPECIFIC had better ALREADY OWN the
   *     symbol of power !
   */
  if (node_next != context->node)
    {
      cmd_ret_t  nret ;

      nret = vty_cmd_config_lock(vty, node_next) ;
      if      (nret == CMD_SUCCESS)
        context->node = context->cnode = node_next ;
      else if (ret == CMD_SUCCESS)
        ret = nret ;
    } ;

  /* Record and return result
   */
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Check that can enter AUTH_ENABLE_NODE.
 *
 * Must be: VTY_TERMINAL
 *
 *     and: no pipes, in or out -- so talking directly to terminal
 *
 *     and: be VIEW_NODE if there is no enable password.
 *
 * Returns:  CMD_SUCCESS   -- OK, can enter AUTH_ENABLE_NODE
 *           CMD_WARNING   -- cannot -- error message sent to output
 */
extern cmd_ret_t
vty_cmd_can_auth_enable(vty vty)
{
  cmd_ret_t   ret ;
  cmd_context context ;

  VTY_LOCK() ;

  context = vty->exec->context ;

  qassert(vty->exec->parsed->nnode == AUTH_ENABLE_NODE) ;

  qassert( (context->onode == VIEW_NODE) ||
           (context->onode == RESTRICTED_NODE) ) ;

  ret = CMD_WARNING ;

  if      (vty->type != VTY_TERMINAL)
    uty_out(vty->vio, "%% Wrong VTY type (%d) for 'enable'", vty->type) ;
  else if ((context->onode != VIEW_NODE) && (host.enable.text == NULL))
    uty_out(vty->vio, "%% cannot enable because there is no enable password") ;
  else if ((vty->vio->vin_depth != 1) || (vty->vio->vout_depth != 1))
    uty_out(vty->vio, "%% cannot authenticate for 'enable' in a pipe command") ;
  else
    ret = CMD_SUCCESS ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Authentication of vty
 *
 * Note that if the AUTH_NODE password fails too many times, the vty is
 * closed.
 *
 * Quagga authentication is a touch complicated.  The following appear to be
 * the rules:
 *
 *   1. host.no_password_check -- set by "no login" command
 *
 *      Means that a VTY_TERMINAL can start without going through any
 *      password check -- whether or not a host.password is configured.
 *
 *      Note that this does not affect the authentication for enable, except
 *      at startup of a VTY_TERMINAL, in which case:
 *
 *        * if host.restricted_mode -> RESTRICTED_NODE
 *
 *        * else if host.advanced   -> ENABLE_NODE
 *
 *          This is whether or not an enable password exists.
 *
 *        * otherwise               -> VIEW_NODE
 *
 *      So being in RESTRICTED_NODE <=> was host.no_password_check &&
 *      host.restricted_mode *when* the VTY_TERMINAL was started.
 *
 *   2. host.restricted_mode  -- set by "anonymous restricted"
 *
 *      Significant only at VTY_TERMINAL start, and only if no_password_check.
 *
 *      NB: if the enable password is NULL, there is not much point in
 *          RESTRICTED_NODE, since ENABLE_NODE is but one command away.
 *
 *      NB: that behaviour is is modified here... if is in RESTRICTED_MODE,
 *          will not authenticate AUTH_ENABLE_NODE if there is no enable
 *          password.
 *
 *          Note that the check is left to this point, so that is completed
 *          atomically.  Elsewhere, will refuse to enter ENABLE_NODE from
 *          RESTRICTED_NODE if no enable password.  By the time we get here
 *          it is (just) possible that the situation has changed.
 *
 *   3. host.advanced  -- set by "service advanced-vty"
 *
 *      Significant iff there is no enable password, when it sets ENABLE_NODE
 *      as the start up node (if no_password_check) or post AUTH_NODE node.
 *
 *   4. host.password  -- set by "password xx"
 *
 *      Unless no_password_check, if there is no password, you cannot start
 *      a vty.
 *
 *   5. host.enable  -- set by "enable password xx"
 *
 *      If this is set, then must authenticate against it for vty to reach
 *      ENABLE_NODE.
 *
 *      If it is not set, then can enter ENABLE_NODE at any time.
 *
 * If AUTH_ENABLE_NODE fails, falls back to the node we came from -- which has
 * been planted in the context for this purpose.  (If host.restricted_mode has
 * changed since the vty started, could argue this should change where should
 * fall back to... but that seems unnecessarily complicated.)
 *
 * For AUTH_ENABLE_NODE, when succeeds may try to go to CONFIG_NODE -- will
 * try to get gain the symbol of power here, but if cannot, will go to
 * ENABLE_NODE and issue a warning message.
 *
 * Note that we expect:
 *
 *   vty->node       -- current node: AUTH_NODE or AUTH_ENABLE_NODE
 *
 * And, if AUTH_ENABLE_NODE:
 *
 *   context->tnode  -- target node: ENABLE_NODE or CONFIG_NODE
 *   context->onode  -- node before: VIEW_NODE or RESTRICTED_NODE
 *                      returns to that node on too many password failures
 *
 * Returns: CMD_SUCCESS  -- OK, one way or another
 *          CMD_WARNING  -- password failure or could not get to CONFIG_NODE
 *          CMD_ERROR    -- too many password failures
 *
 *          If not CMD_SUCCESS, message sent to vty as required.
 *
 * NB: whatever the return code, returns the node to use next (may be the
 *     same as the current node).
 *
 *     This is unlike ordinary commands, where the node can only change on
 *     CMD_SUCCESS.  Furthermore, the change may be to one of a number of
 *     nodes -- not just to one, pre-ordained node.
 */
static cmd_ret_t
vty_cmd_auth(vty vty, node_type_t* p_next_node)
{
  password_t*   passwd ;
  bool          enable ;
  bool          pass ;
  cmd_exec      exec ;
  cmd_context   context ;

  cmd_ret_t     ret ;
  node_type_t   next_node ;
  node_type_t   fall_back_node ;

  exec    = vty->exec ;
  context = exec->context ;

  /* Select the password we need to check against.
   */
  passwd  = NULL ;
  enable  = false ;
  pass    = false ;

  fall_back_node = EXIT_NODE ;  /* unless is enable                     */

  VTY_LOCK() ;                  /* while access host.xx <-<-<-<-<-<-<-< */

  switch (vty->node)
    {
      case AUTH_NODE:
        passwd = &host.password ;

        if (host.advanced && (host.enable.text == NULL))
          {
            next_node = ENABLE_NODE ;
            enable    = true ;
          }
        else
          {
            next_node = VIEW_NODE ;
            enable    = false ;
          } ;
        break ;

      case AUTH_ENABLE_NODE:
        passwd = &host.enable ;

        if ( (context->onode == VIEW_NODE) ||
             (context->onode == RESTRICTED_NODE) )
          fall_back_node = context->onode ;
        else
          qassert(false) ;

        next_node = context->tnode ;
        enable    = true ;
        break ;

      default:
        qassert(false) ;
        passwd    = NULL ;
        break ;
    } ;

  /* Check against selected password (if any)
   */
  if      (passwd == NULL)              /* unknown node !       */
    pass = false ;
  else if (passwd->text != NULL)
    pass = cmd_password_check(passwd, qs_string(context->line)) ;
  else
    /* The password is empty:
     *
     *   * for AUTH: reject -- in this case context->onode == EXIT_NODE.
     *
     *   * for AUTH_ENABLE: reject, *unless* was in VIEW_NODE.
     *
     * For VIEW_NODE, if there is no enable password, then there is no need
     * to authenticate to go to ENABLE.  If was VIEW_NODE and we are now here,
     * then when the enable command was executed there was an enable password,
     * but now there isn't !  (This is theoretically possible.)  Since there
     * is no password we don't need to authenticate, so can pass whatever the
     * user just said !
     */
    pass = (context->onode == VIEW_NODE) ;

  VTY_UNLOCK() ;                /* done with host.xx <-<-<-<-<-<-<-<-<- */

   /* Now worry about the result
    */
  ret = CMD_SUCCESS ;   /* so far, so good      */

  if (pass)
    {
      if (enable)
        context->can_enable = true ;

      ret = vty_cmd_config_lock(vty, next_node) ;
      if (ret != CMD_SUCCESS)
        {
          next_node = ENABLE_NODE ; /* fall back            */
          ret = CMD_WARNING ;
        } ;

      exec->password_failures = 0 ;     /* forgive any failures */
    }
  else
    {
      bool no_more = false ;

      if (passwd == NULL)
        {
          /* This should be impossible -- is not AUTH_NODE or AUTH_ENABLE_NODE.
           */
          no_more = true ;
          vty_out(vty, "%% BUG: attempting to authenticate in %s\n",
                                                     cmd_node_name(vty->node)) ;
        }
      else if (passwd->text == NULL)
        {
          /* Cannot possibly authenticate !
           */
          no_more = true ;
          vty_out(vty, "%% No password is set, cannot authenticate!\n") ;
        }
      else
        {
          /* Tried to authenticate against the known password, and failed.
           */
          exec->password_failures++ ;

          no_more = exec->password_failures >= 3 ;

          if (no_more)
            vty_out(vty, "%% Password not recognised -- patience exhausted\n") ;
          else
            vty_out(vty, "%% Password not recognised\n") ;
        } ;

      if (no_more)
        {
          exec->password_failures = 0 ; /* allow further attempts */

          next_node = fall_back_node ;
          ret = CMD_ERROR ;
        }
      else
        {
          next_node = vty->node ;
          ret = CMD_WARNING ;
        } ;
    } ;

  *p_next_node = next_node ;
  return ret ;
} ;

/*==============================================================================
 * Hiatus handling.
 *
 * The hiatus must deal with a number of slightly different things:
 *
 *   * closing of inputs and/or outputs, and adjusting the stacks.
 *
 *     All pipe closing is done here.
 *
 *   * waiting for next command line to appear
 *
 *   * waiting for output to complete
 *
 *   * command errors
 *
 *   * I/O errors and time-outs
 *
 *   * cancel -- discard all pending output from latest command
 *
 *   * stop -- close all input and all but the vout_base, cause command loop
 *             to stop.
 *
 * Note that while closing, for non-blocking vio, may return from the hiatus
 * CMD_WAITING, and the hiatus will be called again (possibly a number of
 * times) until all necessary closes and related I/O are complete.
 */
static cmd_ret_t uty_cmd_out_block(vio_vf vf) ;

/*------------------------------------------------------------------------------
 * Deal with return code at the "exec_hiatus" point in the command loop.
 *
 * May be entering the hiatus because a signal has been detected, and the
 * signal is cleared.
 *
 * The command_queue command loop runs until something happens that it
 * cannot immediately deal with, at which point it enters "exec_hiatus", and
 * this function is called.
 *
 * The hiatus will deal with the given return code, any closing of inputs
 * and outputs, and any pending output.  Will return CMD_WAITING while there
 * is non-blocking output to be dealt with.
 *
 * The command loop will deal with CMD_SUCCESS, but otherwise this function
 * must deal with:
 *
 *   CMD_HIATUS      -- something requires attention, eg:
 *
 *                        - the vout_depth > vin_depth, so the vout needs to
 *                          be closed and popped.
 *
 *                        - vin or vout is vf_cease.
 *
 *   CMD_WAITING     -- some input or output I/O is awaited -- probably from
 *                      vty_cmd_line_fetch().
 *
 *                      Treated as CMD_HIATUS.
 *
 *   CMD_CANCEL      -- an exception involving cancel has been raised.
 *
 *   CMD_IO_ERROR    -- an exception involving an I/O error or timeout has been
 *                      raised.  Also, any error message will have been posted.
 *
 *   CMD_STOP        -- an exception stopping the vin_base has been raised.
 *
 *   anything else   -- treated as a command or parsing or other error.
 *
 *                      Will raise a vx_quash exception, which closes down
 *                      everything except vin_base/cout_base, and generates
 *                      a suitable message.
 *
 * NB: entry to the hiatus => current command has completed.  This is
 *     particularly significant for base level commands.
 *
 * This function will return:
 *
 *   CMD_SUCCESS     => OK -- can try and fetch a command line again.
 *
 *                            state == vcl_cq_running
 *
 *   CMD_WAITING     => OK -- but waiting for input to arrive or for output
 *                            to complete or for something to be completely
 *                            closed.     => non-blocking
 *
 *                            state == vcl_cq_waiting
 *
 *   CMD_STOP        => OK -- all done: close the vty and exit command loop.
 *
 *                            state == vcl_stopped
 *
 * And nothing else, except:
 *
 *   CMD_IO_ERROR    => an I/O error or time-out has been hit, probably while
 *                      trying to close something.
 *
 *                      This error should be sent straight back to this
 *                      function -- but is passed out so that any error is not
 *                      hidden -- see cmd_read_config().
 *
 * When the command loop has gone vcl_cq_waiting, the I/O side of things can
 * wake it up by uty_cmd_signal(), which passes in a return code.  When the
 * command loop runs it will call this function to handle the new return code.
 * If CMD_SUCCESS is passed in, will continue trying to adjust the vin/vout
 * stacks, if required.
 *
 * The configuration reader command loop also uses vty_cmd_hiatus() to handle
 * all return codes.  However, it will exit the command loop at the first
 * hint of trouble.
 *
 * NB: can be called from any thread if blocking, otherwise MUST be cli thread.
 */
extern cmd_ret_t
vty_cmd_hiatus(vty vty, cmd_ret_t ret)
{
  vty_io vio ;

  VTY_LOCK() ;
  VTY_ASSERT_CAN_CLOSE(vty) ;

  vio = vty->vio ;

  qassert((vio->cl_state == vcl_cq_running) || (vio->cl_state == vcl_running)) ;

  /* Handle the return code, either:
   *
   *   * nothing further required -- any exception will have updated the
   *     vin/vout stack etc. as required.
   *
   *   * this is a command or parsing error, for which a vx_quash exception
   *     must be raised, and suitable error message generated.
   */
  if (vio->state & vst_final)
    ret = CMD_STOP ;                    /* it is all over       */
  else
    {
      switch (ret)
        {
          case CMD_SUCCESS:
          case CMD_HIATUS:
          case CMD_WAITING:
          case CMD_STOP:
          case CMD_CANCEL:
          case CMD_IO_ERROR:
            break ;

          case CMD_WARNING:
          case CMD_ERROR:
          case CMD_ERR_PARSING:
          case CMD_ERR_NO_MATCH:
          case CMD_ERR_AMBIGUOUS:
          case CMD_ERR_INCOMPLETE:
          default:                  /* assume some sort of error            */
            uty_vio_exception(vio, vx_quash) ;
            uty_cmd_failed(vio, ret) ;
            break ;
        } ;
    } ;

  /* The meat of the hiatus -- loop back as required.
   *
   * The hiatus deals with all closing of files and all output.  When returns
   * CMD_SUCCESS all that has been completed successfully.  That will now be
   * returned, and the command loop will loop round to fetch the next command
   * line, *unless* we are waiting for some *input* to arrive first.  The
   * vin_waiting flag is set iff the vin is non-blocking, and the vin is
   * waiting for input.
   *
   * If the vin_waiting flag is set I/O handling *will* uty_cmd_signal() when
   * there is some input.  A flag is used because for VIN_TERM/VOUT_TERM the
   * position is a little complicated.  In particular, changes on the output
   * side may affect whether is waiting in the CLI.
   */
  do
    {
      vio->signal = CMD_SUCCESS ;       /* we are here !        */

      if (ret != CMD_STOP)
        ret = uty_cmd_hiatus(vio) ;

      switch (ret)
        {
          case CMD_SUCCESS:     /* ready to continue            */
            if (vio->vin->vin_waiting)
              ret = CMD_WAITING ;
            break ;

          case CMD_IO_ERROR:    /* for information              */
            break ;

          case CMD_WAITING:
            if (vio->vout->blocking)
              {
                ret = uty_cmd_out_block(vio->vout) ;
                continue ;
              } ;
            break ;

          case CMD_STOP:
            vio->cl_state = vcl_stopped ;

            uty_cmd_config_lock(vio, EXIT_NODE) ;

            /* Stopped is as good as suspended
             */
            if (vio->state & vst_suspend)
              {
                vio->state |= vst_suspended ;
                uty_suspended_scan() ;
              } ;

            break ;

          default:
            zabort("invalid return code from uty_cmd_hiatus") ;
        } ;
    }
  while (vio->signal != CMD_SUCCESS);

  if (ret == CMD_WAITING)
    {
      qassert(vio->cl_state = vcl_cq_running) ;
      vio->cl_state = vcl_cq_waiting ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Inside of vty_cmd_hiatus() -- see above.
 *
 * This returns only CMD_SUCCESS, CMD_WAITING, CMD_STOP or CMD_IO_ERROR, as
 * described above.
 *
 * If returns CMD_WAITING, is waiting for some *output* operation to complete.
 *
 * If returns CMD_SUCCESS, then the vi
 *
 * If returns CMD_WAITING then will return here when uty_cmd_signal() passes
 * in a new return code.  Note that this means that all the code here, and
 * everything that it calls must keep track of their state, so that previously
 * successful operations are not repeated, and the operation that returned
 * CMD_WAITING can be repeated from where it left off (or from where the
 * pselect() system has moved things forward to).
 */
static cmd_ret_t
uty_cmd_hiatus(vty_io vio)
{
  cmd_ret_t ret ;

  /* (1) Do we need to close one or more vin and/or vout, or are we waiting for
   *     one to close ?
   *
   *     The vin_depth should never be less than the vout_depth.  It may
   *     arrive here at vout_depth - 1, which indicates that the vout
   *     needs to be closed -- unless vout_depth == 1, which is special !
   *
   *     The uty_vio_execpetion() mechanism stops vin and vout, setting them
   *     !vin_active or !vout_active.  Hitting eof on an input will set it
   *     !vin_active.  Here we tidy up and pop no longer active vin and vout.
   *     By the time we are finished, will have either vin_depth >= 1, with
   *     both vin and vout active, or vin_depth == 0, and vout active, unless
   *     some error has affected it.
   *
   *     Closing the vin and vout in step with each other, is required and has
   *     some impact on the restoration of context, particularly
   *     vout->out_ordinary. Note that where vin_depth == vout_depth, we close
   *     the vin first.
   *
   *     For a vout:
   *
   *       Note that we do not pop the vout_base here -- that is left until
   *       later, after any cancel notification, error messages, pipe
   *       stderr return, etc have been dealt with, below.
   *
   *       If we are cancelling, will discard everything (if not vout_base).
   *
   *       Otherwise, the pop will push any remaining output and any remaining
   *       pipe return is shovelled through, and any child process is
   *       collected, along with its termination condition.
   *
   *       For blocking vio, close operations will either complete or fail.
   *
   *       For non-blocking vio, close operations may return CMD_WAITING
   *       (eg: where the output buffers have not yet been written away).
   *
   *       Where a close does not succeed, exit here and expect to come back
   *       later to complete the operation.
   *
   *     For a vin:
   *
   *       The close will immediately close the input, and discard anything
   *       which has been buffered.  The only difficulty with closing inputs
   *       is VIN_PIPE, where the "return" input (from the child stderr) may
   *       not yet have finished, and we may wish to drain the input before
   *       closing it.
   *
   *       For blocking vio, close operations will either complete or fail.
   *
   *       For non-blocking vio, close operations may return CMD_WAITING
   *       (eg: VIN_PIPE where the child stderr is not yet at EOF, or the
   *       child completion status has not yet been collected).
   *
   *       Where a close does not succeed, exit here and expect to come back
   *       later to complete the operation.
   *
   *    NB: in the event of I/O errors or such during uty_vin_pop() or
   *        uty_vout_pop(), more vin/vout may be stopped.  If an error affects
   *        the vout_base, then may find vst_final becomes set.
   *
   *        We expect a CMD_IO_ERROR return in these cases, which exits
   *        expecting to return here if not vst_final.  However, if not then
   *        the loop will simply pop more stuff -- and, just in case, we
   *        look out for vst_final.
   */
  while (vio->vin_depth > 0)
    {
      /* If the vout is deeper, should be at most one deeper, but in any
       * case we close *unless* this is the vout base.
       *
       * If the vout is not vf_open, some error or something else has closed
       * it -- deal with that, unless is vout_base.
       */
      if (vio->vout_depth > vio->vin_depth)
        {
          qassert(vio->vout_depth == (vio->vin_depth + 1)) ;
          qassert(vio->vout_depth > 1) ;        /* 'cos vin_depth > 0   */

          ret = uty_vout_pop(vio) ;

          if (ret != CMD_SUCCESS)
            return ret ;

          if (vio->state & vst_final)
            return CMD_STOP ;
        } ;

      /* Now have vout_depth <= vin_depth & vin_depth >= 1.
       *
       * If neither the topmost vin nor the topmost vout is vf_cease, can
       * now continue.
       *
       * Otherwise close the topmost vin, and loop: which will close another
       * vout if required and/or return to here to see if more vin should be
       * closed.
       */
      if (((vio->vin->vin_state | vio->vout->vout_state) & vf_cease) == 0)
        break ;

      ret = uty_vin_pop(vio) ;

      if (ret != CMD_SUCCESS)
        return ret ;

      if (vio->state & vst_final)
        return CMD_STOP ;
    } ;

  if (vio->vin_depth > 0)
    qassert(vio->vin_depth >= vio->vout_depth) ;
  else
    qassert(vio->vout_depth == 1) ;

  /* (2) Have straightened out the stacks.
   *
   *     If we are now at the vout_base, then:
   *
   *     Want any monitor stuff to clear before going any further.
   *
   *     Need to deal with vst_cancel and vst_notify.  If we have either of
   *     these, then must by now be down to vin_depth <= 1.
   *
   *     If we have vst_cancel on its own, then we need to worry about the
   *     current command line (if any, for CLI), and how else should show the
   *     fact that has cancelled for some reason.  This is also the moment at
   *     which all output other than the ebuf is discarded.  We clear
   *     vst_cancel and set vst_notify.
   *
   *     Append anything in the ps_buf and/or the ebuf to the obuf.  Also,
   *     if required add the suspend_reason.
   *
   *     Note that we don't do any actual I/O at this stage... so the stack
   *     cannot be affected !
   */
  if (vio->vout_depth == 1)
    {
      qassert(vio->vout == vio->vout_base) ;

      if (vio->state & vst_mon_mask)
        {
          if (vio->state & vst_mon_paused)
            return CMD_WAITING ;

          ret = uty_cmd_out_push(vio->vout) ;

          if (ret != CMD_SUCCESS)
            return ret ;                /* CMD_WAITING or CMD_IO_ERROR  */
        } ;

      if (vio->state & (vst_cancel | vst_notify))
        {
          qassert(vio->vin_depth <= 1) ;

          /* If we have vst_vancel on its own, this is where we finally cancel
           * the base vout stuff.  Then, we deal with the output of any initial
           * notification that a cancel has occurred -- uty_cmd_out_cancel().
           *
           * Then we clear vst_cancel and set vst_notify (if not already set).
           *
           * We also force vst_cmd_running... because we want the contents of
           * the obuf to be written away as usual.  Essentially, the output
           * of the cancel notification is the output of a "cancel command" !
           */
          if ((vio->state & vst_notify) == 0)
            uty_cmd_out_cancel(vio) ;

          vio->state = (vio->state & ~(vst_cancel | vst_cmd_mask))
                                                | vst_notify | vst_cmd_running ;
        } ;

      /* We are now past the "cancel" stage... all output to be discarded has
       * been discarded, so we can let the vout_base go again.
       */
      vio->vout->vout_state &= ~vf_cancel ;

      /* Take contents of ps_buf -- will have been cleared if vst_cancel or
       * vst_notify.
       *
       * When an input or output pipe closes, it appends its pipe stderr return
       * stuff to the main ps_buf.
       *
       * If there is anything in the ps_buf, we are not vst_cancel/vst_notify,
       * so a cancel could turn up and discard some or all of this..
       */
      if (!vio_fifo_is_empty(vio->ps_buf))
        {
          qassert((vio->state & (vst_cancel | vst_notify)) == 0) ;

          vio_fifo_trim(vio->obuf, true /* add '\n' if required */) ;
          vio_fifo_copy(vio->obuf, vio->ps_buf) ;
          vio_fifo_clear(vio->ps_buf) ;
          vio_fifo_trim(vio->obuf, true /* add '\n' if required */) ;
        } ;

      /* Take contents of ebuf -- which may have been added to since we went
       * vst_notify, and may contain command or parsing errors (so not
       * exclusively related to vst_cancel/vst_notify.
       *
       * If there is anything in the ebuf, if we are not vst_cancel/vst_notify,
       * then a cancel could turn up and discard some or all of this..
       */
      if (!vio_fifo_is_empty(vio->ebuf))
        {
          vio_fifo_trim(vio->obuf, true /* add '\n' if required */) ;
          vio_fifo_copy(vio->obuf, vio->ebuf) ;
          vio_fifo_clear(vio->ebuf) ;
          vio_fifo_trim(vio->obuf, true /* add '\n' if required */) ;
        } ;

      /* If we have a suspend_reason and we are in the right state, take that
       * too.  In any case, we are done with it.
       */
      if (vio->suspend_reason != NULL)
        {
          if ( (vio->state & (vst_notify | vst_suspend))
                          == (vst_notify | vst_suspend) )
            {
              vio_fifo_trim(vio->obuf, true /* add '\n' if required */) ;
              uty_out(vio, "%% %s\n", qs_string(vio->suspend_reason)) ;
            } ;
          uty_suspend_reason_clear(vio) ;
        } ;
    } ;

  /* (3) Push any outstanding output to the current vout.
   *
   *     This makes sure that all output has at least cleared the obuf before
   *     we go on to fetch and process the next command line, or to close
   *     the vty.  Also, makes sure that if the output side needs to know
   *     about command completion, then it is informed.
   *
   *     If we are at vin_depth <= 1, we can clear vst_cmd_executing !  This
   *     is usually done by vty_cmd_complete().  But we can end up here without
   *     having gone through vty_cmd_complete(), eg when the base level command
   *     is a pipe, and we have just popped it.
   *
   *     If we get any sort of error in the uty_cmd_out_push() we expect to
   *     get a CMD_IO_ERROR, and for the caller to re-enter the hiatus.
   *     However, in the next stage we check whether the vin and vout are
   *     in a suitable state to continue.
   */
  if (vio->vin_depth <= 1)
    {
      qassert(vio->vout_depth == 1) ;

      vio->state &= ~vst_cmd_executing ;

      if (vio->vin_depth == 0)
        uty_vf_write_stop(vio->vout, vfs_stop_cease) ;
    } ;

  ret = uty_cmd_out_push(vio->vout) ;

  if (ret != CMD_SUCCESS)
    return ret ;                /* CMD_WAITING or CMD_IO_ERROR  */

  /* (4) All present and correct -- closing and output wise.
   *
   *     If the vin stack has been completely closed, then now is the time
   *     to pop the base vout.  Once that completes, we can return CMD_STOP.
   *
   *     Otherwise, we should have a functioning stack, with both vin and vout
   *     active -- but, just in case some error has affected this, we check.
   *
   *     If we are at the base vin level, then if we were vst_notify, then the
   *     cancel process is complete, and we can clear vst_notify.  Also, may
   *     have successfully suspended the vty -- so deal with that here.
   *
   * XXX Note: all the output stuff treats output to a vf which is no longer
   *           open as instant CMD_SUCCESS.  The output is going nowhere, and
   *           by treating this as success, buffers empty out promptly.
   *
   *           all the input stuff treats input from a vf which is no longer
   *           open as EOF... signalled as CMD_HIATUS.
   */
  if (vio->vin_depth <= 1)
    {
      qassert(vio->vout_depth == 1) ;
      qassert((vio->state & vst_cancel) == 0) ;

      if (vio->vin_depth == 0)
        return CMD_STOP ;

      if (vio->state & (vst_notify | vst_suspend | vst_suspended))
        {
          vio->state &= ~vst_notify ;

          if (vio->state & vst_suspend)
            {
              if ((vio->state & vst_suspended) == 0)
                {
                  /* Time to go into suspension.
                   *
                   * Drop node to enable, if required.
                   */
                  node_type_t node ;

                  vio->state |= vst_suspended ;

                  node = cmd_node_enable_parent(vio->vin->context->node) ;
                  if (node != NULL_NODE)
                    {
                      vio->vin->context->node = vio->vin->context->cnode = node;
                      uty_cmd_config_lock(vio, node) ;
                    } ;

                  uty_suspended_scan() ;
                } ;

              return CMD_WAITING ;
            } ;
        } ;
    } ;

  /* (5) Finally...
   *
   *     The hiatus deals with all closing of files and all output.  By the
   *     time we get to here, all of that has successfully completed.
   *
   *     Can, therefore, prepare to execute commands at the, possibly/probably
   *     new, stack depth -- no effect if depth has not changed.
   */
  qassert((vio->state & vst_hiatus_mask) == 0) ;
  qassert(ret == CMD_SUCCESS) ;

  uty_cmd_prepare(vio) ;

  return ret ;
} ;

/*==============================================================================
 * Opening of pipes and adjustment of stacks.
 */
static void uty_cmd_command_path(qstring name, cmd_context context) ;
static void uty_cmd_opened_in_pipe(vty_io vio, cmd_context context,
                                             cmd_pipe_type_t type, bool after) ;
static void uty_cmd_opened_out_pipe(vty_io vio, cmd_context context,
                                             cmd_pipe_type_t type, bool after) ;

/*------------------------------------------------------------------------------
 * Open the given file as an in pipe, if possible.
 *
 * Completes the file name, if required.
 *
 * Puts error messages to vty if fails.
 *
 * The before flag indicates that this is being opened immediately before an
 * output -- where both are being opened together.  Note that it is always
 * possible that the output open could fail... so anything that really depends
 * on the success of that should be delayed until then.
 *
 * NB: saves the current context to the current vin, before opening and pushing
 *     the new one.
 */
extern cmd_ret_t
uty_cmd_open_in_pipe_file(vty_io vio, cmd_context context,
                                qstring name, cmd_pipe_type_t type, bool before)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      qpath path ;

      path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
      ret = uty_file_read_open(vio, path) ;

      if (ret == CMD_SUCCESS)
        uty_cmd_opened_in_pipe(vio, context, type, before) ;

      qpath_free(path) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Run the given shell command as an in pipe, if possible.
 *
 * Completes the command part of the command string, if required, changing the
 * command qstring.
 *
 * Puts error messages to vty if fails.
 *
 * The before flag indicates that this is being opened immediately before an
 * output -- where both are being opened together.  Note that it is always
 * possible that the output open could fail... so anything that really depends
 * on the success of that should be delayed until then.
 *
 * NB: saves the current context to the current vin, before opening and pushing
 *     the new one.
 */
extern cmd_ret_t
uty_cmd_open_in_pipe_shell(vty_io vio, cmd_context context, qstring command,
                                              cmd_pipe_type_t type, bool before)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      uty_cmd_command_path(command, context) ;
      ret = uty_pipe_read_open(vio, command) ;

      if (ret == CMD_SUCCESS)
        uty_cmd_opened_in_pipe(vio, context, type, before) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open the given file as an out pipe, if possible.
 *
 * Completes the file name, if required, changing the name qstring.
 *
 * Puts error messages to vty if fails.
 *
 * The after flag indicates that this is being opened immediately after an
 * input -- where both are being opened together.
 */
extern cmd_ret_t
uty_cmd_open_out_pipe_file(vty_io vio, cmd_context context, qstring name,
                                               cmd_pipe_type_t type, bool after)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      qpath path ;

      path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
      ret = uty_file_write_open(vio, path,
                                       ((type & cmd_pipe_append) != 0), after) ;
      if (ret == CMD_SUCCESS)
        uty_cmd_opened_out_pipe(vio, context, type, after) ;

      qpath_free(path) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open the given shell command as an out pipe, if possible.
 *
 * Completes the command part of the command string, if required, changing the
 * command qstring.
 *
 * Puts error messages to vty if fails.
 *
 * The after flag indicates that this is being opened immediately after an
 * input -- where both are being opened together.
 */
extern cmd_ret_t
uty_cmd_open_out_pipe_shell(vty_io vio, cmd_context context, qstring command,
                                               cmd_pipe_type_t type, bool after)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      uty_cmd_command_path(command, context) ;
      ret = uty_pipe_write_open(vio, command,
                                    ((type & cmd_pipe_shell_cmd) != 0), after) ;

      if (ret == CMD_SUCCESS)
        uty_cmd_opened_out_pipe(vio, context, type, after) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open "/dev/null" as an out pipe, if possible.
 *
 * Puts error messages to vty if fails.
 */
extern cmd_ret_t
uty_cmd_open_out_dev_null(vty_io vio, bool after)
{
  cmd_ret_t ret ;
  vio_vf    vf ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      vf = uty_vf_new(vio, "dev_null", -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_DEV_NULL, NULL, 0, after) ;

      uty_cmd_prepare(vio) ;

      ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Complete the given file name, if not rooted.
 *
 * Returns: given or new qpath (if given was NULL)
 */
extern qpath
uty_cmd_path_name_complete(qpath dst, const char* name, cmd_context context)
{
  VTY_ASSERT_LOCKED() ;

  if (*name == '/')
    return qpath_set(dst, name) ;       /* done if is rooted            */

  if      (*name != '~')
    dst = qpath_copy(dst, context->dir_cd) ;
  else
    {
      /* Have a leading '~' -- deal with:
       *
       *   "~~/???" or "~~\0", which for Quagga -> configuration directory
       *
       *   "~./???" or "~.\0", which for Quagga -> "here" (same as enclosing
       *                                                   pipe)
       *
       *   "~/???" or "~\0", which         -> HOME environment variable
       *                                      or initial working directory
       *                                      for login.
       *
       *   "~user/???" or "~user\0", which -> initial working directory
       *                                      for given user
       */
      if      ((*(name + 1) == '~') &&
                             ( (*(name + 2) == '/') || (*(name + 2) == '\0')) )
        dst = qpath_copy(dst, host.config_dir) ;
      else if ((*(name + 1) == '.') &&
                             ( (*(name + 2) == '/') || (*(name + 2) == '\0')) )
        dst = qpath_copy(dst, context->dir_here) ;
      else
        {
          qpath was = dst ;

          dst = qpath_get_home(dst, name + 1) ;

          /* If didn't get a home, return the original name
           */
          if (dst == NULL)
            return qpath_set(was, name) ;
        } ;
    } ;

  return qpath_append_str(dst, name) ;  /* create the full path         */
} ;

/*------------------------------------------------------------------------------
 * If the given qstring starts with a '~' directory or is a relative path,
 * then now is the time to complete it.
 */
static void
uty_cmd_command_path(qstring command, cmd_context context)
{
  const char* p, * s ;
  qstring cmd ;
  qpath   qp ;

  VTY_ASSERT_LOCKED() ;

  s = p = qs_string(command) ;

  if ((*p == '/') || (*p == '\0'))
    return ;                    /* absolute path or empty !     */

  do
    {
      ++p ;
      if (*p <= ' ')
        return ;                /* no path involved             */
    }
  while (*p != '/') ;           /* look for '/'                 */

  do
    ++p ;
  while (*p > ' ') ;            /* look for end                 */

  cmd = qs_set_n(NULL, s, p - s) ;
  qp  = uty_cmd_path_name_complete(NULL, qs_string(cmd), context) ;

  qs_set_cp_nn(command, 0) ;
  qs_replace_n(command, p - s, qpath_string(qp), qpath_len(qp)) ;

  qs_free(cmd) ;
  qpath_free(qp) ;
} ;

/*------------------------------------------------------------------------------
 * Successfully opened an input pipe, complete preparation to read commands
 * from it.
 *
 * The following are inherited from parent, unless overridden by "<<" type
 * pipe:
 *
 *    full_lex      -- set false by "<<"
 *    parse_strict  -- set true by  "<<"
 *
 * The following are inherited from the parent, unless overridden by the
 * pipe command.  We don't expect to find contradictory pipe settings,
 * but if we do, the setting that takes precedence is given here.
 *
 *   context->reflect            -- enable
 *   context->out_ordinary       -- enable
 *   context->out_warning        -- enable
 *   context->warn_stop          -- disable
 *
 * (Precedence is given to maximum information.)
 *
 * The before flag indicates that this is being opened immediately before an
 * output -- where both are being opened together.
 *
 * If is "before" then if no explicit setting for context->out_ordinary,
 * enable it -- doesn't seem much point setting up an output pipe if no
 * output will be sent to it !
 *
 * This means that in "< ... > ..." the presence of the output part has an
 * implicit effect on the settings, just as it does on an ordinary command
 * line.
 *
 * When the vout is opened it can take the current context, and inheritors of
 * the context will inherit and restore the effective state of out_ordinary.
 *
 * If is not "before", then we set the vout->out_ordinary.
 *
 * Note that if is "before", has no effect on the current vout, so this has no
 * effect if the open of the output fails.
 */
static void
uty_cmd_opened_in_pipe(vty_io vio, cmd_context context, cmd_pipe_type_t type,
                                                                    bool before)
{
  if (before)
    context->out_ordinary      = true ;

  if (type & cmd_pipe_strict)
    {
      context->parse_strict    = true ;
      context->full_lex        = false ;
    } ;

  if (type & cmd_pipe_quiet)
    {
      context->reflect         = false ;
      context->out_ordinary    = false ;
      context->out_warning     = false ;
      context->warn_stop       = false ;
    } ;

  if (type & cmd_pipe_reflect_disable)
    context->reflect         = false ;
  if (type & cmd_pipe_reflect_enable)
    context->reflect         = true ;           /* precedence   */

  if (type & cmd_pipe_warn_stop)
    {
      context->warn_stop     = true ;
      context->out_warning   = true ;
    } ;
  if (type & cmd_pipe_warn_continue)
    {
      context->warn_stop     = false ;          /* precedence   */
      context->out_warning   = true ;
    } ;

  if (type & cmd_pipe_out_disable)
    context->out_ordinary    = false ;
  if (type & cmd_pipe_out_enable)
    context->out_ordinary    = true ;           /* precedence   */

  if (!before)
    vio->vout->out_ordinary  = context->out_ordinary ;

  uty_cmd_prepare(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Successfully opened an output pipe, complete preparation to continue
 * execution.
 *
 * The after flag indicates that this is being opened immediately after an
 * input -- where both are being opened together.
 *
 * Sets the now effective vout->out_ordinary.  (Note that there is no
 * equivalent for out_warning, because warning output is redirected to the
 * vout_base !)
 *
 * For shell pipes, we apply the shell pipe options, on the order which
 * gives the maximum of output, where there is any overlap or ambiguity.
 */
static void
uty_cmd_opened_out_pipe(vty_io vio, cmd_context context, cmd_pipe_type_t type,
                                                                     bool after)
{
  bool pr_enabled ;
  bool ps_enabled ;

  if (after)
    vio->vout->out_ordinary = context->out_ordinary ;
  else
    vio->vout->out_ordinary = true ;

  if (type & cmd_pipe_std_quiet)
    {
      pr_enabled = false ;
      ps_enabled = false ;
    }
  else
    {
      pr_enabled = vio->vout->vout_next->out_ordinary ;
      ps_enabled = context->out_warning ;
    } ;

  if (type & cmd_pipe_stdout_disable)
    pr_enabled = false ;
  if (type & cmd_pipe_stdout_enable)
    pr_enabled = true ;

  if (type & cmd_pipe_stderr_disable)
    ps_enabled = false ;
  if (type & cmd_pipe_stderr_enable)
    ps_enabled = true ;

  vio->vout->pr_enabled = pr_enabled ;
  vio->vout->ps_enabled = ps_enabled ;

  uty_cmd_prepare(vio) ;
} ;

/*==============================================================================
 * Output before and after command execution.
 *
 * All output goes to a fifo, after a fifo "end mark".  After reflecting a
 * command and after completing a command, all outstanding output is pushed
 * out -- advancing the end mark past all output to date.
 */

/*------------------------------------------------------------------------------
 * Reflect the prompt and command line to the current vio->obuf.
 *
 * The prompt is based on the current context->node -- so the node in which
 * the command was parsed.
 *
 * Advances the end_mark past the reflected line, so that output (in particular
 * error stuff) is separate.
 *
 * NB: pushes the output, so that if the command takes a long time to process,
 *     it is visible while it proceeds.
 *
 * NB: does not block and does not return CMD_WAITING or CMD_HIATUS.
 *
 * Sets vio->prompt_len to length of prompt.
 *
 * Returns:  CMD_SUCCESS  -- OK
 *           CMD_IO_ERROR -- error or time-out
 *
 * This can be called in any thread.
 *
 * Note that caller need not take any notice of the return code.  All further
 * output will be discarded in any case.
 */
extern cmd_ret_t
vty_cmd_reflect_line(vty vty)
{
  cmd_ret_t ret ;
  vty_io    vio ;
  vio_fifo  obuf ;
  qstring   line ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  obuf = vio->obuf ;
  line = vty->exec->context->line ;

  uty_cmd_prompt(vio, vty->exec->context->node) ;
  vio->prompt_len = qs_len_nn(vio->prompt) ;

  vio_fifo_put_bytes(obuf, qs_char_nn(vio->prompt), vio->prompt_len) ;
  vio_fifo_put_bytes(obuf, qs_char_nn(line), qs_len_nn(line)) ;
  vio_fifo_put_byte(obuf, '\n') ;

  ret = uty_cmd_out_push(vio->vout) ;

  if (ret == CMD_WAITING)
    ret = CMD_SUCCESS ;

  qassert( (ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR) ) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command has returned.
 *
 * The incoming return code is expected to be:
 *
 *    CMD_SUCCESS   -- all well
 *    CMD_WARNING   -- some sort of warning or error message to deal with
 *    CMD_ERROR     -- the command failed
 *    CMD_IO_ERROR  -- while executing the command
 *
 * Note that for in pipe commands, each command in the pipe comes through here,
 * so pushes its own output away.
 *
 * This is where we deal with discarding output if is !context->out_ordinary
 * or !context->out_warning.
 *
 * If all is well, and there is no signal, pushes the output if there is
 * something to do or if the vout requires it in any case.
 *
 * For non-blocking vf, will write as much as possible here, and anything left
 * will be handled by the pselect() process.
 *
 * For blocking vf, will *not* block here, but will instead leave stuff in the
 * buffer to be completed later.
 *
 * Will return CMD_WAITING for blocking as well as non-blocking.
 *
 * If there is a signal, the return code will end up in the hiatus.  The output
 * will get to hear of the command completion either in a push or when the
 * output is closed.
 *
 * Returns:  CMD_SUCCESS  -- done everything possible
 *           CMD_WARNING  -- the incoming return code
 *           CMD_ERROR    -- the incoming return code
 *           CMD_WAITING  -- waiting for output
 *           CMD_IO_ERROR -- error or time-out (possibly by signal)
 *           CMD_CANCEL   -- picked up a cancel signal
 */
extern cmd_ret_t
vty_cmd_complete(vty vty, cmd_ret_t ret)
{
  vty_io vio ;
  vio_vf vf ;

  VTY_LOCK() ;

  vio = vty->vio ;              /* once locked                  */
  vf  = vio->vout ;

  /* Try to deal with the simple cases immediately -- to minimise overhead for
   * configuration file processing.
   */
  switch (ret)
    {
      /* For CMD_SUCCESS, if we have no output, or we discard whatever
       * output there is, things are easy.
       *
       * Note that discarding back to the end marker is very straightforard
       * if there is nothing after the end marker.
       *
       * We pick up any signal, before attempting any output.
       */
      case CMD_SUCCESS:
       if (!vf->out_ordinary)
         vio_fifo_back_to_end_mark(vf->obuf) ;

        ret = vio->signal ;
        break ;

      /* For CMD_WARNING, when we are discarding warnings, do that here.
       * Otherwise, leave for the hiatus to deal with.
       *
       * We pick up any signal, before attempting any output.  If there is no
       * signal, then CMD_WARNING -> CMD_SUCCESS (or whatever the output
       * returns).
       *
       * Expect to discard warnings only if not stopping on warning.  (But
       * may output warning and continue.)
       */
      case CMD_WARNING:
        if (!vio->vin->context->out_warning)
          {
            qassert(!vio->vin->context->warn_stop) ;

            vio_fifo_back_to_end_mark(vf->obuf) ;

            ret = vio->signal ;
          } ;
        break ;

      /* CMD_ERROR we leave for the hiatus to deal with.
       *
       * CMD_IO_ERROR, ditto.
       */
      case CMD_ERROR:
      case CMD_IO_ERROR:
        break ;

      /* Don't expect anything else... but leave for the hiatus.
       */
      default:
        qassert(false) ;
    } ;

  /* If we are at the base vin and vout, clear vst_cmd_executing.
   *
   * If there is something to push, or if we are required to push in any event,
   * and all is still OK (including no signal), then push.
   */
  if ((vio->vin_depth <= 1) && (vio->vout_depth <= 1))
    vio->state &= ~vst_cmd_executing ;

  if (!vio_fifo_is_quite_empty(vio->obuf) || vf->push_complete)
    {
      if (ret == CMD_SUCCESS)
        ret = uty_cmd_out_push(vf) ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, push it to be written, now.
 *
 * This is used by configuration file output and others, which output to the
 * fifo and push every now and then to keep the output moving.
 *
 * Output does not block, but will accumulate in the obuf to be dealt with
 * later.
 *
 * See uty_cmd_out_push() below.
 *
 * Returns:  CMD_SUCCESS   -- OK
 *           CMD_IO_ERROR  -- error or time-out
 *           CMD_CANCEL    -- cancel signal picked up
 *
 * NB: does not return CMD_WAITING -- the caller cannot do anything about it,
 *     and really does not need to know.
 *
 * NB: caller can ignore the return code.  Any further output will be discarded
 *     in due course.
 */
extern cmd_ret_t
vty_cmd_out_push(vty vty)
{
  cmd_ret_t ret ;
  vty_io    vio ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  ret = vio->signal ;           /* signal can interrupt         */

  if ((ret == CMD_SUCCESS) || (ret == CMD_HIATUS))
    ret = uty_cmd_out_push(vio->vout) ;

  if (ret == CMD_WAITING)
    ret = CMD_SUCCESS ;

  qassert( (ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)
                                || (ret == CMD_CANCEL) ) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, advance the end mark and attempt to
 * write away contents of the buffer.
 *
 * For non-blocking vf, will write as much as possible here, and anything left
 * will be handled by the pselect() process.
 *
 * For blocking vf, will *not* block here, but will instead leave stuff in the
 * buffer to be completed later.
 *
 * Will return CMD_WAITING for blocking as well as non-blocking.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible at present
 *           CMD_WAITING   -- waiting for output to complete
 *           CMD_IO_ERROR  -- error or time-out
 *
 * NB: takes no notice of vf->out_ordinary, which applies only to buffered
 *     output present when vty_cmd_complete().
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, either
 * the background pselect process will complete the output or the close will
 * block until all output is dealt with.
 */
extern cmd_ret_t
uty_cmd_out_push(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  vio_fifo_step_end_mark(vf->obuf) ;    /* advance the end mark         */

  ret = CMD_SUCCESS ;

  switch (vf->vout_type)
    {
      case VOUT_NONE:
        zabort("invalid vout_none") ;
        break ;

      case VOUT_TERM:
        ret = uty_term_out_push(vf) ;
        break ;

      case VOUT_VTYSH_SERVER:
        ret = uty_sh_serv_out_push(vf) ;
        break ;

      case VOUT_FILE:
      case VOUT_CONFIG:
      case VOUT_PIPE:
        ret = uty_file_out_push(vf) ;
        break ;

      case VOUT_VTYSH:
        ret = uty_vtysh_out_push(vf) ;
        break ;

      case VOUT_DEV_NULL:
      case VOUT_SH_CMD:
        vio_fifo_clear(vf->obuf) ;
        break ;

      case VOUT_STDOUT:
        ret = uty_std_out_push(vf) ;
        break ;

      default:
        zabort("unknown vout_type") ;
    } ;

  qassert( (ret == CMD_SUCCESS) || (ret == CMD_WAITING)
                                || (ret == CMD_IO_ERROR) ) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * For blocking output vf *only* -- block until can write to the main vout.
 *
 * Output -- uty_out_push() -- will not block for blocking output, and if
 * cannot empty out the buffers, will return CMD_WAITING.  Most of the time
 * this can be ignored.  Will not, however, leave the hiatus until output
 * has either completed or timed out.
 *
 * Returns:  CMD_SUCCESS   -- ready to write or otherwise continue
 *           CMD_HIATUS    -- nothing to block for !
 *           CMD_IO_ERROR  -- error or time-out
 */
static cmd_ret_t
uty_cmd_out_block(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;

  qassert(vf->blocking) ;

  if (!vf->blocking)
    return CMD_HIATUS ;

  ret = CMD_SUCCESS ;

  switch (vf->vout_type)
    {
      case VOUT_NONE:
        zabort("invalid vout_none") ;
        break ;

      case VOUT_TERM:
      case VOUT_VTYSH_SERVER:
        break ;

      case VOUT_FILE:
      case VOUT_CONFIG:
      case VOUT_PIPE:
        ret = uty_file_write_block(vf) ;
        break ;

      case VOUT_VTYSH:
        ret = uty_vtysh_write_block(vf) ;
        break ;

      case VOUT_DEV_NULL:
      case VOUT_SH_CMD:
        break ;

      case VOUT_STDOUT:
        ret = uty_std_write_block(vf) ;
        break ;

      default:
        zabort("unknown vout_type") ;
    } ;

  qassert( (ret == CMD_SUCCESS) || (ret == CMD_HIATUS)
                                || (ret == CMD_IO_ERROR) ) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Perform cancel now that we have reached the vout_base.
 *
 * Discards everything in the obuf, before and after the end mark.  Discards
 * the pipe stderr return.
 *
 * Perform any VOUT_xxx specific cancel actions.  For the CLI in particular,
 * that means making sure that the CLI no longer owns the command line.
 *
 * Depending on where we are with the command line, the VOUT_xxx may issue
 * a suitable indication that has been interrupted by a cancel.  If it does
 * not, we issue the cancel_prefix to the, now empty, obuf.
 *
 * NB: once cancelled, the CLI is *not* the owner of the vst_cmd_xxxx state.
 */
static void
uty_cmd_out_cancel(vty_io vio)
{
  vio_vf vf ;
  bool cntrl_c ;

  VTY_ASSERT_LOCKED() ;

  vf = vio->vout ;
  qassert(vf == vio->vout_base) ;

  vio_fifo_clear(vio->obuf) ;
  vio_fifo_clear(vio->ps_buf) ;

  /* VOUT_xxx specific cancel actions.
   */
  cntrl_c = false ;         /* no cntrl_c, yet              */

  switch(vf->vout_type)
    {
      case VOUT_TERM:
        cntrl_c = uty_term_out_cancelled(vf) ;
        break ;

      case VOUT_VTYSH_SERVER:

        break ;

      case VOUT_FILE:       /* cannot be vout_base !        */
      case VOUT_PIPE:
      case VOUT_SH_CMD:
      case VOUT_CONFIG:
      default:
        qassert(false) ;

      case VOUT_DEV_NULL:
        break ;

      case VOUT_VTYSH:
        break ;

      case VOUT_STDOUT:
        break ;
    } ;

  /* If the VOUT_xxx hasn't looked after it, output the cancel_prefix and
   * clear it.
   */
  if ((!cntrl_c) && (vio->cancel_prefix != NULL))
    vio_fifo_put_string(vio->obuf, vio->cancel_prefix) ;

  vio->cancel_prefix = NULL ;
} ;

/*==============================================================================
 * Error handling
 */

/*------------------------------------------------------------------------------
 * Dealing with error of some kind.
 *
 * In general any error causes the vin/vout stack to be closed either
 * completely or down to the base vin/vout.  vio->err_depth contains the
 * default depth to close back to.  An I/O error in either vin_base or
 * vout_base will set the err_depth to 0.
 *
 * The vio->ebuf contains all error messages collected so far for the vio,
 * and will be output to the vout_base when the stack has been closed to
 * that point.  The vio->ebuf will then be emptied.
 *
 * Sets the sh_serv_ret, unless not stopping on CMD_WARNING.
 *
 * For command and parser errors:
 *
 *   The current vio->obuf will have an end_mark.  After the end_mark will be
 *   any output generated since the start of the current command (or any
 *   out_push since then).  For command errors, that output is expected to be
 *   messages associated with the error.
 *
 *   The location of the error is written to the vio->ebuf, and then the
 *   contents of the vio->obuf are moved to the end the vio->ebuf, possibly
 *   with other diagnostic information.
 *
 * For I/O errors:
 *
 *   The contents of vio->obuf are left untouched -- the closing of the
 *   stack will do what it can with those.
 *
 *   The vio->ebuf will already contain the required error message(s).  The
 *   vio->err_depth will have been set to close as far as vin_base/vout_base,
 *   or to close the vty completely.
 *
 * Deals with:
 *
 *   CMD_WARNING            command: generally failed or not fully succeeded
 *   CMD_ERROR              command: definitely failed
 *
 *   CMD_ERR_PARSING        parser:  general parser error
 *   CMD_ERR_NO_MATCH       parser:  command/argument not recognised
 *   CMD_ERR_AMBIGUOUS      parser:  more than on command matches
 *   CMD_ERR_INCOMPLETE
 *
 *   CMD_IO_ERROR           I/O:     error or time-out -- we expect a
 *                                   diagnostic has already been posted to
 *                                   the ebuf -- but if that is empty, we
 *                                   insert a place-holder.
 *
 * Returns:  target depth = 1 or 0
 *
 * NB: does not expect to see all the possible CMD_XXX return codes (see
 *     below), but treats all as a form of error !
 *
 * NB: can be called in any thread.
 */
static uint
uty_cmd_failed(vty_io vio, cmd_ret_t ret)
{
  cmd_ret_t vret ;
  ulen      indent ;
  uint      depth ;

  VTY_ASSERT_LOCKED() ;

  /* Default stack depth to close back to and what vio->vret to set.
   *
   * These can be overridden by the return code type -- eg CMD_WARNING
   */
  depth = 1  ;
  vret  = ret ;

  /* Now any additional error message if required
   */
  uty_cmd_get_ebuf(vio) ;       /* make sure we have an ebuf    */

  switch (ret)
    {
      case CMD_WARNING:
        if (vio->vin->context->out_warning)
          {
            /* Output the warning to the ebuf, which will accumulate error
             * messages until we get back to the base vout, when they will be
             * dealt with.
             */
            uty_show_error_context(vio->ebuf, vio->vin) ;

            if (vio_fifo_is_tail_empty(vio->obuf))
              uty_cmd_failed_message(vio->ebuf, ret) ;  /* Default      */
          }
        else
          {
            /* Expect to have deal with this already... but for completeness,
             * can cope.
             */
            vio_fifo_back_to_end_mark(vio->obuf) ;
            qassert(!vio->vin->context->warn_stop) ;
          } ;

        /* If not stopping on warning: no need to change the vin depth,
         *                             and no reason to change the sh_serv_ret.
         */
        if (!vio->vin->context->warn_stop)
          {
            depth = vio->vin_depth ;
            vret  = vio->sh_serv_ret ;
          } ;

        break ;

      case CMD_ERROR:
        uty_show_error_context(vio->ebuf, vio->vin) ;

        if (vio_fifo_is_tail_empty(vio->obuf))
          uty_cmd_failed_message(vio->ebuf, ret) ;      /* Default      */

        break ;

      case CMD_ERR_PARSING:
        indent = uty_show_error_context(vio->ebuf, vio->vin) ;
        cmd_get_parse_error(vio->ebuf, vio->vty->exec->parsed, indent) ;
        break ;

      default:
        zlog_err("%s: unexpected return code (%d).", __func__, (int)ret) ;

        fall_through ;

      case CMD_ERR_NO_MATCH:
      case CMD_ERR_AMBIGUOUS:
      case CMD_ERR_INCOMPLETE:
        uty_show_error_context(vio->ebuf, vio->vin) ;
        uty_cmd_failed_message(vio->ebuf, ret) ;        /* Standard     */
        break;

      case CMD_IO_ERROR:        /* Diagnostic already posted to ebuf    */
        if (vio_fifo_is_empty(vio->ebuf))
          uty_cmd_failed_message(vio->ebuf, ret) ;      /* Default      */

        break ;
    } ;

  /* For all errors other than CMD_IO_ERROR, we assume that the tail of the
   * current obuf is part of the error message.
   *
   * Just in case... make sure that the result in the vio->ebuf is properly
   * terminated.
   */
  if (ret != CMD_IO_ERROR)
    {
      vio_fifo_copy_tail(vio->ebuf, vio->obuf) ;
      uty_out_discard(vio) ;
      vio_fifo_trim(vio->ebuf, true /* add '\n' if required */) ;
    } ;

  /* Set the vty_sh_serv return code, and return stack depth to close back to
   */
  vio->sh_serv_ret = vret ;
  return depth ;
} ;

/*------------------------------------------------------------------------------
 * Put standard/default error message according to the given ret, to the given
 * fifo.
 *
 * Expects only warning/error return codes.
 *
 * NB: fifo must not be NULL
 */
extern void
uty_cmd_failed_message(vio_fifo ebuf, cmd_ret_t ret)
{
  switch (ret)
    {
      case CMD_WARNING:
        vio_fifo_printf(ebuf, "%% WARNING: non-specific warning\n") ;
        break ;

      case CMD_ERROR:
        vio_fifo_printf(ebuf, "%% ERROR: non-specific error\n") ;
        break ;

      case CMD_ERR_PARSING:
        vio_fifo_printf(ebuf, "%% PARSING ERROR\n") ;
        break ;

      case CMD_ERR_NO_MATCH:
        vio_fifo_printf(ebuf, "%% Unknown command.\n") ;
        break;

      case CMD_ERR_AMBIGUOUS:
        vio_fifo_printf(ebuf, "%% Ambiguous command.\n");
        break;

      case CMD_ERR_INCOMPLETE:
        vio_fifo_printf(ebuf, "%% Command incomplete.\n");
        break;

      case CMD_IO_ERROR:
        vio_fifo_printf(ebuf, "%% IO Error or Time-out: cause unknown\n") ;
        break ;

      default:
        vio_fifo_printf(ebuf, "%% Unexpected return code (%d).\n", (int)ret);
        break ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * In the given fifo, construct message giving the context in which an error
 * has occurred.
 *
 * By recursing to the base of the vin stack, the context starts with level 1
 * of the vin stack, and ends with the current level.
 *
 * For file and pipe input (including config file), the context is given as:
 *
 *   % on line 99 of xxxx:
 *    <the command line -- noting small indent>
 *
 * Even if command lines are being reflected, this gives extra information
 * (the line number) and saves having to work out if any part of the context
 * output is redundant.
 *
 * For VTY_TERMINAL the last command line will be on the screen, and if
 * the stack level == 1, it will be the last thing on the screen, so we can use
 * the current prompt_len.  At any other stack level we assume the file/pipe
 * context is sufficient -- since it gives the file/shell command plus line
 * number and actual failing line.
 *
 * For VTY_VTYSH_SERVER the vtysh is responsible for for stack level == 1.
 *
 * For VTY_VTYSH the situation is similar to VTY_TERMINAL, except that for
 * -c commands, the command line may not have been reflected, so we compensate
 * for that here, and output:
 *
 *   % in command:
 *    <the command line -- noting small indent>
 *
 * Returns: indent -- start of command line at the current level.
 *
 * NB: the indent is not relevant at all lower stack levels.
 */
static uint
uty_show_error_context(vio_fifo ebuf, vio_vf vf)
{
  vio_vf   vf_next ;
  uint     indent ;

  /* Recurse until hit end of the vin stack
   */
  vf_next = ssl_next(vf, vin_next) ;

  if (vf_next != NULL)
    uty_show_error_context(ebuf, vf_next) ;
  else
    assert(vf == vf->vio->vin_base) ;

  /* On the way back, add the error location for each vin entry
   * and establish the location of the start of the command line as shown.
   */
  indent = 0 ;

  switch (vf->vin_type)
  {
    case VIN_NONE:
      zabort("invalid VIN_NONE") ;
      break ;

    case VIN_TERM:
      /* The command line is always reflected for VTY_TERMINAL.
       *
       */
      indent = vf->vio->prompt_len ;
      break ;

    case VIN_VTYSH_SERVER:
      /* We let the vtysh worry about the current command line context.
       *
       * Do not expect any parsing error, so setting indent == 0 and discarding
       * the "....^" simplifies things, and loses essentially nothing.
       */
      indent = 0 ;
      break ;

    case VIN_CONFIG:
    case VIN_FILE:
    case VIN_PIPE:
      /* For all forms of pipe, and when reading config file we give the
       * source and line number.
       *
       * We return the indent for any parser error.
       */
      vio_fifo_printf(ebuf, "%% on line %d of %s:\n",
                                                    vf->line_number, vf->name) ;
      vio_fifo_printf(ebuf, " %s\n", qs_string(vf->cl)) ;
      indent = 2 ;
      break ;

    case VIN_VTYSH:             /* vtysh *own* vty      */
      /* For vtysh own vty we have roughly three cases:
       *
       * If the stack is at level == 1, then we are either in interactive mode
       * or executing -c commands.  The vio->prompt_len tells us whether the
       * -c command has been reflected or not -- if not, then it is now
       * output for context.
       *
       * If the stack is at level > 1, then the error context will be output
       * showing where in a configuration file or in some pipe things have
       * gone wrong.  Indent is not relevant.
       */
      if (vf->vio->vin_depth == 1)
        {
          indent = vf->vio->prompt_len ;
          if (indent == 0)
            {
              vio_fifo_printf(ebuf, "%% in command:\n") ;
              vio_fifo_printf(ebuf, " %s\n", qs_string(vf->cl)) ;
              indent = 2 ;
            } ;
        } ;

      break ;

    case VIN_DEV_NULL:
      break ;

    default:
      zabort("unknown vin_type") ;
  } ;

  return indent ;
} ;

/*------------------------------------------------------------------------------
 * If there is no vio->ebuf, make one
 */
extern vio_fifo
uty_cmd_get_ebuf(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->ebuf == NULL)
    vio->ebuf = vio_fifo_new(500) ;

  return vio->ebuf ;
} ;

/*==============================================================================
 * Configuration node/state handling
 *
 * At most one VTY may hold the configuration symbol of power at any time.
 *
 * Only at vin_depth == 1 may the symbol of power be acquired, and only at
 * vin_depth <= 1 will the symbol of power be released.  Inter alia, this
 * means that:
 *
 *   * a pipe can only do config stuff if invoked from a config node at the
 *     command line -- this is a "safety" feature.
 *
 *   * do not need to worry about authentication for config node while reading
 *     pipes.
 *
 *   * will not be tripped up if leaves CONFIG or SPECIFIC while reading a
 *     pipe, and needs to reenter same later.
 *
 *   * when restoring node when popping a pipe, can restore a node that
 *     requires the config symbol of power, without worrying.
 *
 *     If result vin_depth > 1, we want to keep the symbol of power if we
 *     have it in any case.
 *
 *     If result vin_depth == 1, then if we own the symbol of power, the node
 *     being restored to must be CONFIG or SPECIFIC, so we want to keep the
 *     symbol of power.
 */

/*------------------------------------------------------------------------------
 * Sort out configuration symbol of power, depending on the given node.
 *
 * See uty_cmd_config_lock().
 *
 * Returns: CMD_SUCCESS  -- hold or do not hold the symbol of power, as required
 *          CMD_ERROR    -- need, but unable to acquire the symbol of power
 */
extern cmd_ret_t
vty_cmd_config_lock(vty vty, node_type_t node)
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  if (uty_cmd_config_lock(vty->vio, node))
    ret = CMD_SUCCESS ;
  else
    {
      if (host.config != 0)
        vty_out(vty, "%% VTY configuration is locked by other VTY\n") ;
      else
        vty_out(vty, "%% VTY configuration is not available\n") ;

      ret = CMD_ERROR ;         /* cannot really continue !     */
    } ;

  VTY_UNLOCK() ;
  return ret;
} ;

/*------------------------------------------------------------------------------
 * If the given node requires it, try to acquire the symbol of power.
 *
 * If the given node does not require it, and we are at vin_depth <= 1,
 * drop the symbol of power if we have it.
 *
 * If is at vin_depth == 0, then ignore the node and drop the symbol of power.
 *
 * Returns: CMD_SUCCESS  -- hold or do not hold the symbol of power, as required
 *          CMD_ERROR    -- need, but unable to acquire the symbol of power
 */
extern bool
uty_cmd_config_lock(vty_io vio, node_type_t node)
{
  bool ok ;

  VTY_ASSERT_LOCKED() ;

  ok = true ;                   /* expect the best      */

  if (cmd_node_is_config_lock(node) && (vio->vin_depth > 0))
    {
      /* We need the configuration symbol of power.
       */
      if (vio->config != 0)
        {
          /* Good news -- we think we own the symbol of power
           */
          qassert(vio->config == host.config) ;

          if (vio->config != host.config)
            {
              /* This is bad -- does not match :-(
               */
              vio->config = 0 ;
              ok = false ;
            } ;
        }
      else if ((host.config == 0) && (vio->vin_depth == 1))
        {
          /* We need, but do not have, the symbol of power.
           *
           * Nobody else owns the symbol of power, and we are at depth == 1
           */
          do
            host.config_brand += 314159265 ;    /* update brand...      */
          while (host.config_brand == 0) ;

          host.config = host.config_brand ;     /* ...apply             */
          vio->config = host.config_brand ;
        }
      else
        {
          /* We need, but do not have, the symbol of power.
           *
           * Somebody else owns the symbol of power, or we are at depth != 1
           */
          ok = false ;
        } ;
    }
  else
    {
      /* We do not need the configuration symbol of power.
       *
       * But we hold on to it if depth > 1.
       */
      if (vio->config != 0)
        {
          /* We think we own it, so we better had
           */
          qassert(host.config == vio->config) ;

          /* Release if at a suitable depth.
           */
          if (vio->vin_depth <= 1)
            {
              host.config = 0 ;
              vio->config = 0 ;
            } ;
        } ;
    } ;

  return ok ;
} ;

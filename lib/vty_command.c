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
#include "list_util.h"
#include "qstring.h"

/*==============================================================================
 * vty_command.c contains functions used by the command processing, where
 * that interacts with the vty -- in particular vty I/O.
 *
 * There are two command loops -- cmd_read_config() and cq_process().  Each
 * command loop appears to be a thread of control sucking in command lines,
 * parsing and executing them.  In the process, input and output pipes are
 * opened and closed, and the vty stack grows and shrinks.
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
 * The I/O is structured so that all output either completes before the I/O
 * operation returns, or will be autonomously completed by the background
 * pselect() process.  Only when waiting for input does the loop need to exit
 * waiting for I/O.
 *
 * So a command loop takes the general form:
 *
 *   loop:   fetch command line
 *           parse command line
 *            loop if empty or all comment
 *           reflect command line
 *           deal with any pipe open actions
 *           dispatch command (if any)
 *           push command output to output
 *            loop
 *
 *   hiatus: deal with issue
 *             loop if OK and not waiting for input
 *           exit command loop
 *
 * In the loop, if any operation receives a return code it cannot immediately
 * deal with, it jumps to the "hiatius".  For everything except the command
 * line fetch this will be some sort of error -- either I/O or command -- or
 * some external event closing down the loop.  For the command line fetch
 * this may be because must now wait for input, or because the current input
 * has reached EOF and must now be closed (which may pop things off the
 * vty stack).  In any event, once the issue is dealt with, can leave the
 * hiatus and return to the top of the loop.
 *
 * Note that in hiatus the command loop may be waiting for some output I/O
 * to complete -- e.g. while closing an output pipe.
 *
 * So, most obviously for cq_process(), the loop is a co-routine which exits
 * and is re-entered at the hiatus.  When the loop does exit, it has either
 * come to a dead stop (for a number of reasons) or it is waiting for input.
 *
 * The state of the command loop is vty->vio->state.  The main states are:
 *
 *   vc_running   -- somewhere in the command loop, doing things.
 *
 *   vc_waiting   -- waiting for I/O
 *
 * When some event (such as some input arriving) occurs it is signalled to
 * the command loop by uty_cmd_signal(), where the value of the signal is
 * a CMD_XXXX code.  If the loop is in vc_waiting state, the loop can be
 * re-entered (at the hiatus point) with the return code.  Otherwise, what
 * happens depends on the state and the return code -- see uty_cmd_signal().
 *
 * Functions called by the various steps in the command loop will check for
 * a pending signal, and will force a jump to hiatus -- using CMD_HIATUS return
 * code.  (This is how the command loop may be "interrupted" by, for example
 * an I/O error detected in the pselect() process.)
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static bool uty_cmd_loop_prepare(vty_io vio) ;
static void uty_cmd_stopping(vty_io vio, bool exeunt) ;
static cmd_return_code_t uty_cmd_hiatus(vty_io vio, cmd_return_code_t ret) ;
static cmd_return_code_t vty_cmd_auth(vty vty, node_type_t* p_next_node) ;
static void uty_cmd_out_cancel(vio_vf vf, bool base) ;
static uint uty_show_error_context(vio_fifo ebuf, vio_vf vf) ;
static uint uty_cmd_failed(vty_io vio, cmd_return_code_t ret) ;
static void uty_cmd_prepare(vty_io vio) ;
static bool uty_cmd_config_lock(vty vty) ;
static void uty_cmd_config_lock_check(struct vty *vty, node_type_t node) ;

/*==============================================================================
 * Starting up, communicating with and closing down a command loop.
 */

/*------------------------------------------------------------------------------
 * Prepare to enter the config read command loop.
 *
 * Initialise exec object, and copy required settings from the current vin
 * and vout.
 *
 * Returns:  true  <=> acquired or did not need config symbol of power
 *       or: false <=> needed but could not acquire symbol of power
 */
extern bool
vty_cmd_config_loop_prepare(vty vty)
{
  bool ok ;

  VTY_LOCK() ;

  assert(vty->type == VTY_CONFIG_READ) ;

  ok = uty_cmd_loop_prepare(vty->vio) ; /* by vty->type & vty->node     */

  VTY_UNLOCK() ;

  return ok ;
} ;

/*------------------------------------------------------------------------------
 * Enter the command_queue command loop.
 */
extern void
uty_cmd_queue_loop_enter(vty_io vio)
{
  bool ok ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert(vio->vty->type == VTY_TERMINAL) ;

  ok = uty_cmd_loop_prepare(vio) ;      /* by vty->type & vty->node     */

  if (!ok)
    uty_out(vio, "%% unable to start in config mode\n") ;

  qassert(vio->state == vc_running) ;
  cq_loop_enter(vio->vty, (vio->vty->node == NULL_NODE) ? CMD_CLOSE
                                                        : ok ? CMD_SUCCESS
                                                             : CMD_WARNING) ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to enter a command loop.
 *
 * Initialise cmd_exec object, and its cmd_context -- given vty->type and
 * vty->node.
 *
 * Returns:  true  <=> acquired or did not need config symbol of power
 *       or: false <=> needed but could not acquire symbol of power
 */
static bool
uty_cmd_loop_prepare(vty_io vio)
{
  bool ok ;

  VTY_ASSERT_LOCKED() ;

  assert(vio->vty->exec == NULL) ;

  vio->vty->exec = cmd_exec_new(vio->vty) ;
  vio->state     = vc_running ;

  ok = true ;
  if (vio->vty->node > MAX_NON_CONFIG_NODE)
    {
      ok = uty_cmd_config_lock(vio->vty) ;
      if (!ok)
        vio->vty->node = ENABLE_NODE ;
    } ;

  uty_cmd_prepare(vio) ;

  return ok ;
} ;

/*------------------------------------------------------------------------------
 * When entering command loop, or after opening or closing a vin/vout object,
 * update the vty->exec context.
 *
 * Output to the vout_base is suppressed for reading of configuration files.
 *
 * Reflection of the command line depends on the current context, and on the
 * state of output suppression.
 */
static void
uty_cmd_prepare(vty_io vio)
{
  cmd_exec exec = vio->vty->exec ;

  exec->reflect       = exec->context->reflect_enabled ;
  exec->out_suppress  = (vio->vty->type == VTY_CONFIG_READ)
                                             && (vio->vout_depth == 1)
                                             && !exec->reflect ;
} ;

/*------------------------------------------------------------------------------
 * Signal to the command loop that some I/O has completed -- successfully, or
 * with some I/O error (including time out), or otherwise.
 *
 * Accepts the following return codes:
 *
 *   CMD_SUCCESS  -- if vc_waiting,  passed in
 *                   otherwise,      ignored
 *
 *   CMD_WAITING  --                 ignored
 *
 *   CMD_IO_ERROR -- if vc_waiting,  passed in
 *                   if vc_running,  set signal, unless already CMD_STOP
 *                   otherwise,      ignored
 *
 *   CMD_CANCEL   -- if vc_waiting,  passed in
 *                   if vc_running,  set signal, unless already CMD_STOP
 *                                                           or CMD_IO_ERROR
 *                   otherwise,      ignored
 *
 * NB: if sets CMD_CANCEL signal, sets vio->cancel.
 *
 *     if passes CMD_CANCEL in, sets vio->cancel.
 */
extern void
uty_cmd_signal(vty_io vio, cmd_return_code_t ret)
{
  VTY_ASSERT_LOCKED() ;

  qassert( (ret == CMD_SUCCESS) || (ret == CMD_WAITING)
                                || (ret == CMD_IO_ERROR)
                                || (ret == CMD_CANCEL) ) ;

  switch (vio->state)
    {
      case vc_running:
        if ((ret == CMD_SUCCESS) || (ret == CMD_WAITING))
          break ;                       /* Ignored                      */

        if (vio->signal == CMD_STOP)
          break ;                       /* Cannot override CMD_STOP     */

        if (ret != CMD_IO_ERROR)
          {
            qassert(ret == CMD_CANCEL) ;

            if (vio->signal == CMD_IO_ERROR)
              break ;                   /* Cannot override CMD_IO_ERROR */

            vio->cancel = true ;
          } ;

        vio->signal = ret ;

        break ;

      case vc_waiting:          /* pass in the return code              */
        if (ret != CMD_WAITING)
          {
            vio->state = vc_running ;
            if (ret == CMD_CANCEL)
              vio->cancel = true ;

            cq_continue(vio->vty, ret) ;
          } ;
        break ;

      case vc_stopped:          /* ignore everything                    */
        break ;

      default:
        zabort("unknown vio->state") ;
        break ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Stop the command loop.
 *
 * If the loop is vc_null, it has never started, so is stopped.
 *
 * If is non-blocking then revoke any in-flight message (or legacy thread
 * event), *and* if does that, then the command loop may be stopped.
 *
 * The loop may then be:
 *
 *   vc_running           -- for multi-pthread world this means that the
 *                           command loop is executing, and must be sent a
 *                           CMD_STOP signal.  (Is not waiting for a message,
 *                           because we just tried revoking.)  Because this
 *                           function is in the vty_cli_nexus, the command loop
 *                           *must* be running in the vty_cmd_nexus.
 *
 *                           for single-pthread (or legacy thread), this should
 *                           be impossible -- this code cannot run at the same
 *                           time as the command loop.  However, we send a
 *                           CMD_STOP signal.
 *
 *   vc_waiting           -- can stop the command loop now.  Setting vc_stopped
 *                           turns off any further signals to the command loop.
 *
 *   vc_stopped           -- already stopped (or never started).
 *
 *                           Note: we still revoke any in-flight messages.
 *                           This deals with the case of the command loop
 *                           picking up a CMD_STOP signal in the vty_cmd_nexus,
 *                           but the message transferring the command loop to
 *                           the vty_cli_nexus has not been picked up yet.
 *
 *                           Note: if the command loop is exiting normally,
 *                           then it will already be vc_stopped -- see
 *                           vty_cmd_loop_exit()
 *
 * NB: if this is "curtains" then this *should* find the command loop already
 *     vc_stopped -- see uty_close() -- but will force the issue.
 *
 * Returns:  true <=> the command loop is vc_stopped
 *          false  => the command loop is running, but a CMD_STOP signal has
 *                    been set.
 */
extern bool
uty_cmd_loop_stop(vty_io vio, bool curtains)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  if (vio->blocking || !cq_revoke(vio->vty))
    {
      if ((vio->state == vc_running) && !curtains)
        {
          uty_set_monitor(vio, off) ;   /* instantly                    */
          vio->signal = CMD_STOP ;

          return false ;
        } ;

      qassert(vio->state != vc_running) ;
    } ;

  uty_cmd_stopping(vio, true) ;         /* -> vc_stopped                */

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Set the command loop stopped -- forced exit.
 */
extern void
vty_cmd_set_stopped(vty vty)
{
  VTY_LOCK() ;
  uty_cmd_stopping(vty->vio, true) ; /* forced exit  */
  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * The command loop is stopping or closing.
 *
 * We drop the config symbol of power and effectively discard all input.
 * Stops any log monitoring, immediately.
 *
 * This is done as soon as a CMD_CLOSE, CMD_STOP or CMD_STOP signal is seen, so
 * that the symbol of power is not held while a vty is closing its vio stack,
 * or while the command loop is being transferred from the vty_cmd_nexus to the
 * vty_cli_nexus.
 *
 * If exeunt, set vc_stopped -- otherwise leave vc_running to tidy up.
 *
 * This can be called any number of times.
 */
static void
uty_cmd_stopping(vty_io vio, bool exeunt)
{
  VTY_ASSERT_LOCKED() ;

  uty_set_monitor(vio, off) ;   /* instantly            */

  vio->vin_true_depth = 0 ;     /* all stop             */
  uty_cmd_config_lock_check(vio->vty, NULL_NODE) ;

  if (exeunt)
    vio->state  = vc_stopped ;  /* don't come back      */
  else
    qassert(vio->state == vc_running) ;
} ;

/*------------------------------------------------------------------------------
 * If we have a CMD_STOP on our hands, then drop the config symbol of power.
 *
 * This is done so that on SIGHUP the symbol of power can be acquired to read
 * the configuration file, without an interlock to wait for closing then
 * current vty.
 */
extern void
vty_cmd_check_stop(vty vty, cmd_return_code_t ret)
{
  VTY_LOCK() ;

  if ((ret == CMD_STOP) || (vty->vio->signal == CMD_STOP))
    {
      vty->vio->signal = CMD_STOP ;             /* make sure signal set */

      uty_cmd_stopping(vty->vio, false) ;       /* not exit, yet        */
    } ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Exit command loop, with final close of the VTY.
 *
 * NB: on exit the VTY has been release, so do NOT attempt to touch the VTY
 *     or any of its components.
 */
extern void
vty_cmd_loop_exit(vty vty)
{
  VTY_LOCK() ;

  VTY_ASSERT_CAN_CLOSE(vty) ;

  uty_cmd_stopping(vty->vio, true) ;    /* exit -- set vc_stopping      */
  uty_close(vty->vio) ;                 /* down close the vty           */

  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Command line fetch.
 *
 * Will read command lines from the current input, until that signals EOF.
 *
 * Before attempting to read, will check for a vio->signal, and that the input
 * and output stack depths do not require attention.  The CMD_HIATUS return
 * code signals that something needs to be dealt with before any input is
 * attempted.
 *
 * NB: all closing of inputs and/or outputs is done in the hiatus, below.
 */

/*------------------------------------------------------------------------------
 * Fetch the next command line to be executed.
 *
 * Returns: CMD_SUCCESS  => OK -- have a line ready to be processed.
 *
 *                          vty->exec->line   points at the line
 *                          vty->exec->to_do  says what to do with it
 *
 *      or: CMD_WAITING  => OK -- but waiting for command line to arrive
 *                                                             <=> non-blocking
 *
 *      or: CMD_EOF      => OK -- but nothing to fetch from the current vin
 *
 *                          Need to close the current vin and pop vin/vout
 *                          as necessary.
 *
 *      or: CMD_HIATUS   => OK -- but need to close one or more vin/vout
 *                                to adjust stack.
 *
 *      or: CMD_CANCEL   => cancel vio->signal detected.
 *
 *      or: CMD_IO_ERROR => failed (here or signal) -- I/O error or timeout.
 *
 *      or: CMD_STOP     => stop vio->signal detected.
 *
 * NB: can be called from any thread -- because does no closing of files or
 *     anything other than read/write.
 */
extern cmd_return_code_t
vty_cmd_fetch_line(vty vty)
{
  cmd_return_code_t ret ;
  vty_io    vio ;
  vio_vf    vf ;
  cmd_exec  exec ;

  VTY_LOCK() ;

  vio    = vty->vio ;                   /* once locked          */
  exec   = vty->exec ;

  cmd_action_clear(exec->action) ;      /* tidy                 */

  vf = vio->vin ;

  ret = vio->signal ;

  if (ret == CMD_SUCCESS)
    {
      if ( (vio->vin_depth < vio->vout_depth) ||
           (vio->vin_depth > vio->vin_true_depth) )
        ret = CMD_HIATUS ;
      else
        {
          qassert(vio->vin_depth == vio->vin_true_depth) ;
          qassert(vio->vin_depth != 0) ;

          switch (vf->vin_state)
            {
              case vf_closed:
                zabort("invalid vf->vin_state (vf_closed)") ;
                break ;

              case vf_open:
                switch (vf->vin_type)
                  {
                    case VIN_NONE:
                      zabort("invalid VIN_NONE") ;
                      break ;

                    case VIN_TERM:
                      ret = uty_term_fetch_command_line(vf, exec->action,
                                                                exec->context) ;
                      break ;

                    case VIN_VTYSH:
                      zabort("invalid VIN_VTYSH") ;
                      break ;

                    case VIN_DEV_NULL:
                      ret = CMD_EOF ;
                      break ;

                    case VIN_FILE:
                    case VIN_CONFIG:
                      ret = uty_file_fetch_command_line(vf, exec->action) ;
                      break ;

                    case VIN_PIPE:
                      ret = uty_pipe_fetch_command_line(vf, exec->action) ;
                      break ;

                    default:
                      zabort("unknown vin_type") ;
                  } ;

                break ;

              case vf_end:         /* Should be dealt with in hiatus... */
                ret = CMD_HIATUS ; /* ...so go (back) there !           */
                break ;

              default:
                zabort("unknown vf->vin_state") ;
                break ;
            } ;
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
 * Returns:  CMD_SUCCESS    -- OK, carry on
 *           CMD_CLOSE      -- bring everything to an orderly stop
 */
extern cmd_return_code_t
vty_cmd_special(vty vty)
{
  cmd_return_code_t ret ;
  cmd_do_t          to_do ;
  node_type_t       next_node ;

  ret = CMD_SUCCESS ;

  to_do = vty->exec->action->to_do ;

  /* Note that the next node handling is special here... we establish
   * the next node explicitly here -- there is no parse operation to preset
   * what CMD_SUCCESS next node will be.
   */

  vty->node = vty->exec->context->node ;        /* as per all commands  */
  next_node = vty->exec->context->node ;        /* by default.          */

  switch (to_do & cmd_do_mask)
    {
      case cmd_do_nothing:
      case cmd_do_ctrl_c:
        break ;

      case cmd_do_eof:
        if (vty->type == VTY_TERMINAL)
          vty_out(vty, "%% Terminal closed\n") ;

        ret = CMD_CLOSE ;
        break ;

      case cmd_do_timed_out:
        if (vty->type == VTY_TERMINAL)
          vty_out(vty, "%% Terminal timed out\n") ;

        ret = CMD_CLOSE ;
        break ;

      case cmd_do_command:
        if ((to_do & cmd_do_auth) != 0)
          ret = vty_cmd_auth(vty, &next_node) ;
        else
          zabort("invalid cmd_do_command") ;
        break ;

      case cmd_do_ctrl_d:
        if ((to_do & cmd_do_auth) != 0)
          next_node = cmd_node_exit_to(vty->node) ;
        else
          zabort("invalid cmd_do_ctrl_d") ;
        break ;

      case cmd_do_ctrl_z:
        next_node = cmd_node_end_to(vty->node) ;
        break ;

      default:
        zabort("unknown or invalid cmd_do") ;
    } ;

  /* Now worry about changing node                                      */
  if      (ret == CMD_CLOSE)
    next_node = NULL_NODE ;
  else if (next_node == NULL_NODE)
    ret = CMD_CLOSE ;

  if (next_node != vty->exec->context->node)
    {
      vty->exec->context->node = next_node ;
      vty_cmd_config_lock_check(vty, next_node) ;
    } ;

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
 * Note that "can_enable" <=> vin_depth == 1 and VTY_TERMINAL (or other VTY
 * that can authenticate).  But it may not => vout_depth == 0.
 */
extern cmd_return_code_t
vty_cmd_can_auth_enable(vty vty)
{
  cmd_return_code_t ret ;

  VTY_LOCK() ;

  assert(vty->exec->parsed->nnode == AUTH_ENABLE_NODE) ;
  assert((vty->exec->context->onode == VIEW_NODE) ||
         (vty->exec->context->onode == RESTRICTED_NODE)) ;

  ret = CMD_WARNING ;

  if      (vty->type != VTY_TERMINAL)
    uty_out(vty->vio, "%% Wrong VTY type (%d) for 'enable'", vty->type) ;
  else if ((vty->exec->context->onode != VIEW_NODE)
           && (host.enable == NULL))
    uty_out(vty->vio, "%% cannot enable because there is no enable password") ;
  else if ((vty->vio->vin_depth != 1) || (vty->vio->vout_depth != 1))
    uty_out(vty->vio,
                    "%% cannot authenticate for 'enable' in a pipe command") ;
  else
    {
      assert(vty->exec->context->can_auth_enable) ;
      ret = CMD_SUCCESS ;
    } ;

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
 *      password check -- including when no host.password is configured.
 *
 *      Note that this does not affect the authentication for enable, except
 *      at startup of a VTY_TERMINAL...
 *
 *      When a VTY_TERMINAL starts:
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
 *      host.restricted_mode when the VTY_TERMINAL was started.
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
 * Returns: CMD_SUCCESS  -- OK, one way or another
 *          CMD_WARNING  -- with error message sent to output
 *          CMD_CLOSE    -- too many password failures
 */
static cmd_return_code_t
vty_cmd_auth(vty vty, node_type_t* p_next_node)
{
  char *crypt (const char *, const char *);

  char*         passwd    = NULL ;
  bool          encrypted = false ;
  bool          enable    = false ;
  bool          advanced ;
  bool          pass ;
  cmd_return_code_t  ret ;
  cmd_exec      exec ;
  cmd_context   context ;

  exec    = vty->exec ;
  context = exec->context ;

  /* Select the password we need to check against.                      */
  passwd    = NULL ;
  encrypted = false ;
  enable    = false ;
  advanced  = false ;

  pass      = false ;

  VTY_LOCK() ;                  /* while access host.xx         */

  switch (vty->node)
    {
      case AUTH_NODE:
        passwd    = host.password ;
        encrypted = host.password_encrypted ;
        enable    = false ;

        context->onode = NULL_NODE ;    /* started from nowhere */

        if (host.advanced && (host.enable == NULL))
          {
            context->tnode = ENABLE_NODE ;
            advanced       = true ;
          }
        else
          {
            context->tnode = VIEW_NODE ;
            advanced   = false ;
          } ;
        break ;

      case AUTH_ENABLE_NODE:
        passwd    = host.enable ;
        encrypted = host.enable_encrypted ;
        enable    = true ;
        advanced  = false ;

        assert((context->onode == VIEW_NODE) ||
               (context->onode == RESTRICTED_NODE)) ;
        break ;

      default:
        zabort("unknown vty->node") ;
        break ;
    } ;

  VTY_UNLOCK() ;

  /* Check against selected password (if any)                           */
  if (passwd == NULL)
    {
      /* Here we reject any attempt to AUTH_NODE against an empty password.
       *
       * Otherwise, is dealing with the (largely) theoretical case of
       * This fails any attempt to AUTH_ENABLE against an empty password
       * if was in RESTRICTED_NODE.
       *
       * This passes the theoretically possible case of enable in VIEW_NODE,
       * when there was an enable password set when the enable command was
       * executed, but it has since been unset !
       */
      pass = context->onode == VIEW_NODE ;
    }
  else
    {
      char* candidate = qs_make_string(exec->action->line) ;

      if (encrypted)
        candidate = crypt(candidate, passwd) ;

      pass = (strcmp(candidate, passwd) == 0) ;
    } ;

  /* Now worry about the result                                         */
  ret = CMD_SUCCESS ;   /* so far, so good      */

  if (pass)
    {
      *p_next_node = context->tnode ;

      if (enable || advanced)
        context->can_enable = true ;

      if (*p_next_node == CONFIG_NODE)
        {
          ret = vty_cmd_config_lock(vty) ;
          if (ret == CMD_WARNING)
            *p_next_node = ENABLE_NODE ;
        } ;

      exec->password_failures = 0 ;         /* forgive any failures */
    }
  else
    {
      bool no_more = false ;

      if (passwd == NULL)
        {
          /* Cannot possibly authenticate !                             */
          no_more = true ;
          vty_out(vty, "%% No password is set, cannot authenticate!\n") ;
        }
      else
        {
          exec->password_failures++ ;

          if (exec->password_failures >= 3)
            {
              no_more = true ;
              vty_out(vty, "%% Bad passwords, too many failures!\n") ;

              exec->password_failures = 0 ; /* allow further attempts */
            } ;
        } ;

        if (no_more)
          {
            if (!enable)
              {
                *p_next_node = NULL_NODE ;
                ret = CMD_CLOSE ;
              }
            else
              {
                *p_next_node = context->onode ;
                ret = CMD_WARNING ;
              } ;
          } ;
    } ;

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
 *   * command errors
 *
 *   * cancel
 *
 *   * I/O errors and time-outs
 *
 *   * closing of vty altogether, either:
 *
 *       CMD_CLOSE   -- which closes everything, completing all I/O
 *
 *       CMD_STOP    -- which stops the command loop, and lets uty_close()
 *                      close everything "final".
 *
 * Note that while closing, for non-blocking vio, may return from the hiatus
 * CMD_WAITING, and the hiatus will be called again (possibly a number of
 * times) until all necessary closes and related I/O are complete.
 */

/*------------------------------------------------------------------------------
 * Deal with return code at the "exec_hiatus" point in the command loop.
 *
 * May be entering the hiatus because a signal has been detected, which may
 * override the given return code.  Any signal is then cleared.
 *
 * The command_queue command loop runs until something happens that it
 * cannot immediately deal with, at which point it enters "exec_hiatus", and
 * this function is called.  The command loop will deal with CMD_SUCCESS and
 * CMD_EMPTY, but otherwise this function must deal with:
 *
 *   CMD_HIATUS      -- something requires attention, eg:
 *
 *                        - the vout_depth > vin_depth, so the vout needs to
 *                          be closed and popped.
 *
 *                        - the vio->state needs to be checked.
 *
 *   CMD_STOP        -- stop the command loop and exit, closing vty
 *
 *   CMD_EOF         -- from vty_cmd_fetch_line() => current vin has hit eof,
 *                      or an end/exit command has forced the issue.
 *
 *                      The vin_real_depth must be reduced, and the top vin
 *                      then closed.
 *
 *   CMD_CLOSE       -- from a command return => must close the entire vty.
 *
 *                      CMD_CLOSE causes the vty to be closed in an orderly
 *                      fashion, dealing with all pending I/O, including
 *                      all pipe return etc.
 *
 *                      Once everything has been closed down to the
 *
 *   CMD_CANCEL      -- cancel all output & close everything except the
 *                      vin/vout base.
 *
 *   CMD_WAITING     -- from vty_cmd_fetch_line() or elsewhere => must go to
 *                      vc_waiting and the command loop MUST exit.
 *
 *   CMD_SUCCESS     -- see below
 *
 *   CMD_EMPTY       -- should not appear, but is treated as CMD_SUCCESS
 *
 *   anything else   -- treated as a command or I/O or other error.
 *
 * The handling of errors depends on the type of error:
 *
 *   * command errors will cause all levels of the stack other than vin_base
 *     and vout_base to be closed, and a suitable error message output to the
 *     vout_base.
 *
 *     Inputs are closed without dealing with any further input and discarding
 *     any buffered input.
 *
 *     Pending output will be pushed out, and pipe return stuff will be sucked
 *     in and blown out, until the return signals EOF.
 *
 *   * I/O errors and timeouts in any level of the stack other than vin_base
 *     and vout_base will cause everything except the vin_base and vout_base
 *     to be closed.
 *
 *     I/O errors and time-outs in vin_base cause everything to be closed, but
 *     will try to put error messages and outstanding stuff to vout_base.
 *
 *     I/O errors and time-outs in vout_base cause everything to be closed.
 *
 *     Does the standard close... so will try to flush all outstanding stuff.
 *
 * This function will return:
 *
 *   CMD_SUCCESS     => OK -- can try and fetch a command line again.
 *
 *                            state == vc_running
 *
 *   CMD_WAITING     => OK -- but waiting for input to arrive or for something
 *                            to be completely closed.     => non-blocking
 *
 *                            state == vc_waiting
 *
 *   CMD_STOP        => OK -- all done: close the vty and exit command loop.
 *
 *                            state == vc_stopped
 *
 *                            If CMD_STOP is passed in, or a CMD_STOP signal
 *                            is picked up, then has done nothing with the
 *                            stacks -- uty_close() will do close "final".
 *
 *                            Otherwise, will have completing all pending stuff,
 *                            and closed everything except the vout_base.
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
 * When the command loop has gone vc_waiting, the I/O side of things can wake
 * it up by uty_cmd_signal(), which passes in a return code.  When the
 * command loop runs it will call this function to handle the new return code.
 * If CMD_SUCCESS is passed in, will continue trying to adjust the vin/vout
 * stacks, if required.
 *
 * The configuration reader command loop also uses vty_cmd_hiatus() to handle
 * all return codes.  However, it will exit the command loop at the first
 * hint of trouble.
 *
 * NB: can be called from any thread if !blocking, otherwise MUST be cli thread.
 */
extern cmd_return_code_t
vty_cmd_hiatus(vty vty, cmd_return_code_t ret)
{
  vty_io vio ;

  VTY_LOCK() ;
  VTY_ASSERT_CAN_CLOSE(vty) ;

  vio = vty->vio ;

  qassert(vio->state == vc_running) ;

  ret = uty_cmd_hiatus(vio, ret) ;

  switch (ret)
    {
      case CMD_SUCCESS:                 /* ready to continue            */
      case CMD_IO_ERROR:                /* for information              */
        break ;

      case CMD_WAITING:
        qassert(!vio->blocking) ;
        vio->state = vc_waiting ;
        break ;

      case CMD_STOP:                    /* exit                         */
        uty_cmd_stopping(vio, true) ;   /* vio->state -> vc_stopped     */
        break ;

      default:
        zabort("invalid return code from uty_cmd_hiatus") ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Inside of vty_cmd_hiatus() -- see above.
 */
static cmd_return_code_t
uty_cmd_hiatus(vty_io vio, cmd_return_code_t ret)
{
  /* (1) Handle any vio->signal.
   *
   *     If there is a signal it overrides most return codes, except:
   *
   *       CMD_CANCEL   -- trumped by: CMD_STOP, CMD_CLOSE or CMD_IO_ERROR
   *
   *       CMD_IO_ERROR -- trumped by: CMD_STOP or CMD_CLOSE
   *
   *       CMD_STOP     -- trumps everything
   *
   *     The vio_signal is now cleared (unless was CMD_STOP).
   */
  switch (vio->signal)
    {
      case CMD_SUCCESS:
        break ;

      case CMD_CANCEL:
        if ((ret != CMD_STOP) && (ret != CMD_CLOSE) && (ret != CMD_IO_ERROR))
          ret = CMD_CANCEL ;

        vio->signal = CMD_SUCCESS ;
        break ;

      case CMD_IO_ERROR:
        if ((ret != CMD_STOP) && (ret != CMD_CLOSE))
          ret = CMD_IO_ERROR ;

        vio->signal = CMD_SUCCESS ;
        break ;

      case CMD_STOP:
        ret = CMD_STOP ;
        break ;

      default:
        zabort("Invalid vio->signal value") ;
    } ;

  /* (2) Handle the return code/signal
   *
   *     Deal here with the return codes that signify success, or signify
   *     success but some vin and/or vout need to be closed.
   *
   *     Call uty_cmd_failed() to deal with return codes that signify some
   *     sort of failure.  A failure generally means closing all the way to
   *     the vin_/vout_base, or possibly completely.
   *
   * CMD_WAITING is immediately returned
   *
   * CMD_STOP, whether passed in or from vio->signal is also immediately
   * returned.
   */
  switch (ret)
    {
      case CMD_SUCCESS:
      case CMD_EMPTY:
      case CMD_HIATUS:
        ret = CMD_SUCCESS ;             /* OK                           */
        break ;

      case CMD_STOP:
        return CMD_STOP ;       /* <<< ...exit here on CMD_STOP         */

      case CMD_WAITING:
        return CMD_WAITING ;    /* <<< ...exit here on CMD_WAITING      */

      case CMD_EOF:
        uty_out_accept(vio) ;           /* accept any buffered stuff.   */

        qassert(vio->vin_true_depth > 0) ;
        --vio->vin_true_depth ;         /* cause vin stack to pop       */

        ret = CMD_SUCCESS ;             /* OK                           */
        break ;

      case CMD_CANCEL:
        qassert(vio->vin_true_depth > 0) ;
        vio->vin_true_depth = 1 ;       /* cause vin stack to pop       */
        vio->cancel = true ;            /* suppress output              */

        ret = CMD_SUCCESS ;             /* OK                           */
        break ;

      case CMD_CLOSE:
        uty_out_accept(vio) ;           /* accept any buffered stuff.   */

        vio->vin_true_depth = 0 ;       /* cause vin stack to close     */

        ret = CMD_SUCCESS ;             /* OK                           */
        break ;

      default:                          /* some sort of error           */
        break ;
    } ;

  /* If the return code is (still) not CMD_SUCCESS, then must be an error
   * of some kind, for which we now construct a suitable error message, and
   * update the vin_true_depth as required.
   */
  if (ret != CMD_SUCCESS)
    {
      uint depth ;

      depth = uty_cmd_failed(vio, ret) ;
      if (depth < vio->vin_true_depth)
        vio->vin_true_depth = depth ;

       ret = CMD_SUCCESS ;               /* Is now OK.                   */
    } ;

  /* Have established the (new) vio->vin_true_depth, so now need to make
   * sure that the stack conforms to that.
   *
   * Adjusting the stack may take a while.  So, if the vin_true_depth is 0,
   * now is a good time to give up the config symbol of power.  (This is
   * for SIGHUP, which closes all vty before reading the configuration.)
   *
   * Note that because vin_true_depth is zero, could not fetch any further
   * command lines or attempt to execute any commands, and don't care
   * whether own the symbol of power or not.
   */
  if (vio->vin_true_depth == 0)
    uty_cmd_stopping(vio, false) ;      /* not exit, yet                */

  /* (3) Do we need to close one or more vin, or are we waiting for one to
   *     close ?
   *
   *     The close will immediately close the input, and discard anything
   *     which has been buffered.  The only difficulty with closing inputs
   *     is VIN_PIPE, where the "return" input (from the child stderr) may
   *     not yet have finished.
   *
   *     For blocking vio, close operations will either complete or fail.
   *
   *     For non-blocking vio, close operations may return CMD_WAITING
   *     (eg: VIN_PIPE where the child stderr is not yet at EOF, or the
   *     child completion status has not yet been collected).
   *
   *     Where a close does not succeed, exit here and expect to come back
   *     later to complete the operation.
   */
  while (vio->vin_depth > vio->vin_true_depth)
    {
      ret = uty_vin_pop(vio, vio->vty->exec->context, false) ;  /* not final */
      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  qassert(vio->vin_depth == vio->vin_true_depth) ;

  /* (4) Do we need to close one or more vout, or are we waiting for
   *     one to close ?
   *
   *     Note that we do not close the vout_base here -- that is left open
   *     to the final minute, in case any parting messages are to be sent.
   *
   *     Any remaining output is pushed and any remaining pipe return is
   *     shovelled through, and any child process is collected, along
   *     with its termination condition.
   *
   *     For blocking vio, close operations will either complete or fail.
   *
   *     For non-blocking vio, close operations may return CMD_WAITING
   *     (eg: where the output buffers have not yet been written away).
   *
   *     Where a close does not succeed, exit here and expect to come back
   *     later to complete the operation.
   */
  while ((vio->vin_depth < vio->vout_depth) && (vio->vout_depth > 1))
    {
      if (vio->cancel)
        uty_cmd_out_cancel(vio->vout, false) ; /* stop output & pipe return
                                                 * not vout_base        */

      ret = uty_vout_pop(vio, false) ;          /* not final            */

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* (5) If we are now at the vout_base, then:
   *
   *     If there is anything in the pipe stderr return, copy that to the
   *     obuff -- unless vio->cancel.
   *
   *     If there is an error message in hand, now is the time to move that to
   *     the obuf and clear the error message buffer.  (If the vout_base has
   *     failed, then the error message is going nowhere, but there's nothing
   *     we can do about that -- the error has been logged in any case.)
   *
   *     Push any outstanding output (including any error message) to the
   *     vout_base.
   *
   *     If the vty is about to be closed, this step ensures that all output
   *     is tidily dealt with, before uty_close() performs its "final" close.
   */
  if (vio->vout_depth == 1)
    {
      if (vio->cancel)
        {
          /* Once we have cleared the output buffer etc., clear the cancel
           * flag and output "^C" to show what has happened.
           */
          uty_cmd_out_cancel(vio->vout, true) ; /* stop output & pipe return
                                                  * is vout_base        */
          uty_out(vio, " ^C\n") ;
        } ;

      if (!vio_fifo_empty(vio->ps_buf))
        {
          if (!vio->cancel)
            vio_fifo_copy(vio->obuf, vio->ps_buf) ;
          vio_fifo_clear(vio->ps_buf, true) ;   /* clear any marks too  */
        } ;

      vio->cancel = false ;

      if (!vio_fifo_empty(vio->ebuf))
        {
          vio_fifo_copy(vio->obuf, vio->ebuf) ;
          vio_fifo_clear(vio->ebuf, true) ;     /* clear any marks too  */
        } ;

      ret = uty_cmd_out_push(vio->vout, false) ;

      if (ret != CMD_SUCCESS)
        return ret ;                    /* CMD_WAITING or CMD_IO_ERROR  */
    } ;

  /* (6) Stacks are straight.
   *
   *     If there is no input left, the command loop must now stop, close the
   *     vty and exit.
   *
   *     Otherwise, prepare to execute commands at the, presumed new, stack
   *     depth.
   */
  qassert(ret == CMD_SUCCESS) ;

  if (vio->vin_depth == 0)
    return CMD_STOP ;

  uty_cmd_prepare(vio) ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * Opening of pipes and adjustment of stacks.
 */
static void uty_cmd_command_path(qstring name, cmd_context context) ;

/*------------------------------------------------------------------------------
 * Open the given file as an in pipe, if possible.
 *
 * Puts error messages to vty if fails.
 *
 * NB: saves the current context to the current vin, before opening and pushing
 *     the new one.
 */
extern cmd_return_code_t
uty_cmd_open_in_pipe_file(vty_io vio, cmd_context context,
                                            qstring name, cmd_pipe_type_t type)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      ret = uty_file_read_open(vio, name, context) ;

      if (ret == CMD_SUCCESS)
        {
          context->reflect_enabled = (type & cmd_pipe_reflect) != 0 ;
          context->parse_strict    = true ;

          uty_cmd_prepare(vio) ;
        } ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Run the given shell command as an in pipe, if possible.
 *
 * Puts error messages to vty if fails.
 *
 * NB: saves the current context to the current vin, before opening and pushing
 *     the new one.
 */
extern cmd_return_code_t
uty_cmd_open_in_pipe_shell(vty_io vio, cmd_context context, qstring command,
                                                           cmd_pipe_type_t type)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      uty_cmd_command_path(command, context) ;
      ret = uty_pipe_read_open(vio, command, context) ;

      if (ret == CMD_SUCCESS)
        {
          context->reflect_enabled = (type & cmd_pipe_reflect) != 0 ;
          context->parse_strict    = true ;

          uty_cmd_prepare(vio) ;
        } ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open the given file as an out pipe, if possible.
 *
 * Puts error messages to vty if fails.
 */
extern cmd_return_code_t
uty_cmd_open_out_pipe_file(vty_io vio, cmd_context context, qstring name,
                                               cmd_pipe_type_t type, bool after)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      ret = uty_file_write_open(vio, name,
                              ((type & cmd_pipe_append) != 0), context, after) ;
      if (ret == CMD_SUCCESS)
        uty_cmd_prepare(vio) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open the given shell command as an out pipe, if possible.
 *
 * Puts error messages to vty if fails.
 */
extern cmd_return_code_t
uty_cmd_open_out_pipe_shell(vty_io vio, cmd_context context, qstring command,
                                               cmd_pipe_type_t type, bool after)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      uty_cmd_command_path(command, context) ;
      ret = uty_pipe_write_open(vio, command,
                                    ((type & cmd_pipe_shell_cmd) != 0), after) ;

      if (ret == CMD_SUCCESS)
        uty_cmd_prepare(vio) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open "/dev/null" as an out pipe, if possible.
 *
 * Puts error messages to vty if fails.
 */
extern cmd_return_code_t
uty_cmd_open_out_dev_null(vty_io vio, bool after)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_ASSERT_LOCKED() ;

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      vf = uty_vf_new(vio, "dev_null", -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_DEV_NULL, NULL, NULL, 0, after) ;

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
        dst = qpath_copy(dst, context->dir_home) ;
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
  qs_replace(command, p - s, qpath_string(qp), qpath_len(qp)) ;

  qs_free(cmd) ;
  qpath_free(qp) ;
} ;

/*==============================================================================
 * Output before and after command execution.
 *
 * All output goes to a fifo, after a fifo "end mark".  After reflecting a
 * command and after completing a command, all outstanding output is pushed
 * out -- advancing the end mark past all output to date.
 */

/*------------------------------------------------------------------------------
 * Reflect the command line to the current vio->obuf.
 *
 * Advances the end_mark past the reflected line, so that output (in particular
 * error stuff) is separate.
 *
 * NB: pushes the output, so that if the command takes a long time to process,
 *     it is visible while it proceeds.
 *
 * Returns:  CMD_SUCCESS   -- all buffers are empty
 *           CMD_WAITING   -- all buffers are not empty
 *           CMD_IO_ERROR  -- error or time-out
 *           CMD_HIATUS    -- the vty is not in vc_running state.
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, the
 * background pselect process will complete the output and may signal the
 * result via uty_cmd_signal() (CMD_SUCCESS or CMD_IO_ERROR).
 */
extern cmd_return_code_t
vty_cmd_reflect_line(vty vty)
{
  cmd_return_code_t  ret ;
  vty_io vio ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      vio_fifo obuf ;
      qstring  line ;

      obuf = vio->obuf ;
      line = vty->exec->action->line ;

      vio_fifo_put_bytes(obuf, qs_char_nn(line), qs_len_nn(line)) ;
      vio_fifo_put_byte(obuf, '\n') ;

      ret = uty_cmd_out_push(vio->vout, false) ;        /* not final    */
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command has completed successfully.
 *
 * An output generated by the command is now pushed unless exec->out_suppress,
 * or discarded.
 */
extern cmd_return_code_t
vty_cmd_success(vty vty)
{
  cmd_return_code_t ret ;
  vty_io vio ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    {
      if (!vio_fifo_tail_empty(vio->obuf))
        {
          if (!vty->exec->out_suppress)
            ret = uty_cmd_out_push(vio->vout, false) ;    /* not final      */
          else
            vio_fifo_back_to_end_mark(vio->obuf, true) ;  /* keep end mark  */
        } ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, push it to be written, now.
 *
 * This is used by configuration file output, which outputs to the fifo and
 * pushes every now and then.
 *
 * See uty_cmd_out_push() below.
 */
extern cmd_return_code_t
vty_cmd_out_push(vty vty)
{
  cmd_return_code_t ret ;
  vty_io vio ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  ret = vio->signal ;           /* signal can interrupt         */

  if (ret == CMD_SUCCESS)
    ret = uty_cmd_out_push(vio->vout, false) ;  /* not final    */

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, advance the end mark and attempt to
 * write away contents of the buffer.
 *
 * For non-blocking vf, will write as much as possible here, and anything left
 * will be left to the pselect() process, unless "final".
 *
 * For blocking vf, may block here, unless "final".
 *
 * If "final", will attempt to write etc., but will not block and may turn
 * off the pselect() processing of this vf.  "final" is used when a pipe of
 * some kind is being closed "final", and the slave output is being pushed.
 *
 * NB: takes no notice of vf->out_suppress, which applies only to buffered
 *     output present when successfully complete a command -- vty_cmd_success().
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, the
 * background pselect process will complete the output and may signal the
 * result via uty_cmd_signal() (CMD_SUCCESS or CMD_IO_ERROR).
 */
extern cmd_return_code_t
uty_cmd_out_push(vio_vf vf, bool final)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  vio_fifo_step_end_mark(vf->obuf) ;    /* advance the end mark         */

  ret = CMD_SUCCESS ;

  switch (vf->vout_state)
    {
      case vf_open:
        switch (vf->vout_type)
          {
            case VOUT_NONE:
              zabort("invalid vout_none") ;
              break ;

            case VOUT_TERM:
              /* Note that we ignore "final" -- the VOUT_TERM runs until
               * it is closed.
               */
              ret = uty_term_out_push(vf, false) ;
              break ;

            case VOUT_VTYSH:
              /* Kick the writer                */
              break ;

            case VOUT_FILE:
              /* push everything if the vty is being closed.            */
              ret = uty_file_out_push(vf, final, vf->vio->vin_depth == 0) ;
              break ;

            case VOUT_PIPE:
              ret = uty_pipe_out_push(vf, final) ;
              break ;

            case VOUT_CONFIG:
              /* push everything if the vty is being closed.            */
              ret = uty_file_out_push(vf, final, vf->vio->vin_depth == 0) ;
              break ;

            case VOUT_DEV_NULL:
            case VOUT_SH_CMD:
              vio_fifo_clear(vf->obuf, false) ;     /* keep end mark    */
              break ;

            case VOUT_STDOUT:
              if (vf->vio->cancel)
                vio_fifo_clear(vf->obuf, false) ;   /* keep end mark        */
              else
                vio_fifo_fwrite(vf->obuf, stdout) ;     // TODO errors
              break ;

            case VOUT_STDERR:
              if (vf->vio->cancel)
                vio_fifo_clear(vf->obuf, false) ;   /* keep end mark        */
              else
                vio_fifo_fwrite(vf->obuf, stderr) ;     // TODO errors
              break ;

            default:
              zabort("unknown vout_type") ;
          } ;
        break ;

      case vf_closed:
      case vf_end:
        vio_fifo_clear(vf->obuf, false) ;       /* keep end mark        */
        break ;                                 /* immediate success !  */

      default:
        zabort("unknown vf->vout_state") ;
        break ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Bring output and any pipe return to a sudden halt.
 */
static void
uty_cmd_out_cancel(vio_vf vf, bool base)
{
  VTY_ASSERT_LOCKED() ;

  /* Dump contents of obuf and if not base: force vf_end (if vf_open)
   */
  vio_fifo_clear(vf->obuf, false) ;

  if (!base && (vf->vout_state == vf_open))
    vf->vout_state = vf_end ;

  /* If there is a pipe return, close that down, too.
   */
  if (vf->pr_state == vf_open)
    uty_pipe_return_cancel(vf) ;
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
 *   CMD_WARNING            command: general failed or not fully succeeded
 *   CMD_ERROR              command: definitely failed
 *
 *   CMD_ERR_PARSING        parser:  general parser error
 *   CMD_ERR_NO_MATCH       parser:  command/argument not recognised
 *   CMD_ERR_AMBIGUOUS      parser:  more than on command matches
 *   CMD_ERR_INCOMPLETE
 *
 *   CMD_IO_ERROR           I/O:     error or time-out
 *
 * NB: does not expect to see all the possible CMD_XXX return codes (see
 *     below), but treats all as a form of error !
 */
static uint
uty_cmd_failed(vty_io vio, cmd_return_code_t ret)
{
  ulen  indent ;
  uint  depth ;

  VTY_ASSERT_LOCKED() ;

  /* Stack depth to close back to.
   *
   * This could be overridden by the return code type.
   */
  depth = vio->err_depth  ;

  /* Now any additional error message if required                       */
  uty_cmd_get_ebuf(vio) ;

  switch (ret)
  {
    case CMD_WARNING:
      uty_show_error_context(vio->ebuf, vio->vin) ;

      if (vio_fifo_tail_empty(vio->obuf))
        vio_fifo_printf(vio->ebuf, "%% WARNING: non-specific warning\n") ;
      break ;

    case CMD_ERROR:
      uty_show_error_context(vio->ebuf, vio->vin) ;

      if (vio_fifo_tail_empty(vio->obuf))
        vio_fifo_printf(vio->ebuf, "%% ERROR: non-specific error\n") ;
      break ;

    case CMD_ERR_PARSING:
      indent = uty_show_error_context(vio->ebuf, vio->vin) ;
      cmd_get_parse_error(vio->ebuf, vio->vty->exec->parsed, indent) ;
      break ;

    case CMD_ERR_NO_MATCH:
      uty_show_error_context(vio->ebuf, vio->vin) ;
      vio_fifo_printf(vio->ebuf, "%% Unknown command.\n") ;
      break;

    case CMD_ERR_AMBIGUOUS:
      uty_show_error_context(vio->ebuf, vio->vin) ;
      vio_fifo_printf(vio->ebuf, "%% Ambiguous command.\n");
      break;

    case CMD_ERR_INCOMPLETE:
      uty_show_error_context(vio->ebuf, vio->vin) ;
      vio_fifo_printf(vio->ebuf, "%% Command incomplete.\n");
      break;

    case CMD_IO_ERROR:          /* Diagnostic already posted to ebuf    */
      break ;

    default:
      zlog_err("%s: unexpected return code (%d).", __func__, (int)ret) ;
      vio_fifo_printf(vio->ebuf, "%% Unexpected return code (%d).\n", (int)ret);
      break ;
  } ;

  /* Now stick the obuf tail onto the end of the ebuf & discard the tail of
   * the obuf.
   */
  vio_fifo_copy_tail(vio->ebuf, vio->obuf) ;
  vio_fifo_back_to_end_mark(vio->obuf, true) ;

  /* Return what stack depth to close back to.                          */
  return depth ;
} ;

/*------------------------------------------------------------------------------
 * In the given fifo, construct message giving the context in which an error
 * has occurred.
 *
 * For file and pipe input (including config file), it is assumed that no
 * command line has been reflected, so the context is given as:
 *
 *   % on line 99 of xxxx:
 *    <the command line -- noting small indent>
 *
 * For interactive input, if the stack depth is 1, then it is assumed that the
 * command line is the last complete line output.  Otherwise the context is
 * given as:
 *
 *   % in command line:
 *    <the command line -- noting small indent>
 *
 * The context starts with level 1 of the vin stack, and ends with the current
 * level.
 *
 * Returns: "eloc" -- start of command line at the current level.
 */
static uint
uty_show_error_context(vio_fifo ebuf, vio_vf vf)
{
  vio_vf   vf_next ;
  uint     indent ;

  /* Recurse until hit end of the vin stack                             */
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
      indent = uty_term_show_error_context(vf, ebuf, vf->vio->vin_depth) ;
      break ;

    case VIN_VTYSH:
//    eloc = uty_term_show_error_context(vf, ebuf, vf->vio->vin_depth) ;
      break ;

    case VIN_FILE:
    case VIN_PIPE:
    case VIN_CONFIG:
      vio_fifo_printf(ebuf, "%% on line %d of %s:\n",
                                                    vf->line_number, vf->name) ;
      vio_fifo_printf(ebuf, " %s\n", qs_make_string(vf->cl)) ;
      indent = 2 ;
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
    vio->ebuf = vio_fifo_new(1000) ;

  return vio->ebuf ;
} ;

/*==============================================================================
 * Configuration node/state handling
 *
 * At most one VTY may hold the configuration symbol of power at any time.
 *
 * Only at vin_depth == 1 may the symbol of power be acquired, and only at
 * vin_depth <= 1 will the symbol of power be released.  Inter alia, this
 * means that the restoration of command context when an input pipe finishes
 * does not have to worry about recovering or releasing the symbol of power.
 */

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power -- may already own it !
 *
 * Returns: true <=> now own the symbol of power (or already did).
 */
extern cmd_return_code_t
vty_cmd_config_lock (vty vty)
{
  bool  locked ;

  VTY_LOCK() ;
  locked = uty_cmd_config_lock(vty) ;
  VTY_UNLOCK() ;

  if (vty->config)
    return CMD_SUCCESS ;

  if (locked)
    vty_out(vty, "VTY configuration is locked by other VTY\n") ;
  else
    vty_out(vty, "VTY configuration is not available\n") ;

  return CMD_WARNING ;
} ;

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power -- may already own it !
 *
 * NB: cannot do this at any vin level except 1 !
 */
static bool
uty_cmd_config_lock (vty vty)
{
  VTY_ASSERT_LOCKED() ;

  if (!host.config)             /* If nobody owns the lock...           */
    {
      if (vty->vio->vin_depth == 1)
        {
          host.config = true ;          /* ...rope it...                */
          vty->config = true ;

          do
            ++host.config_brand ;       /* ...update the brand...       */
          while (host.config_brand == 0) ;

          vty->config_brand = host.config_brand ;       /* ...brand it. */
        } ;
    }
  else                          /* Somebody owns the lock...            */
    {
      if (vty->config)          /* ...if we think it is us, check brand */
        assert(host.config_brand == vty->config_brand) ;
    } ;

  return host.config ;
}

/*------------------------------------------------------------------------------
 * Check that given node and ownership of configuration symbol of power...
 * ...see below.
 */
extern void
vty_cmd_config_lock_check(vty vty, node_type_t node)
{
  VTY_LOCK() ;
  uty_cmd_config_lock_check(vty, node) ;
  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Check that given node and ownership of configuration symbol of power
 * are mutually consistent.
 *
 * If node >  MAX_NON_CONFIG_NODE, must own the symbol of power (unless
 * vio->vin_true_depth == 0, in which case the node is irrelevant).
 *
 * If node <= MAX_NON_CONFIG_NODE, will release symbol of power, if own it,
 * PROVIDED is at vin_true_depth <= 1 !!
 */
static void
uty_cmd_config_lock_check(vty vty, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;

  if (vty->config)
    {
      /* We think we own it, so we better had                   */
      qassert(host.config) ;
      qassert(host.config_brand == vty->config_brand) ;

      /* If no longer need it, release                          */
      if ((node <= MAX_NON_CONFIG_NODE) && (vty->vio->vin_true_depth <= 1))
        {
          host.config  = false ;
          vty->config  = false ;
        } ;
    }
  else
    {
      /* We don't think we own it, so we had better not         */
      if (host.config)
        qassert(host.config_brand != vty->config_brand) ;

      /* Also, node had better not require that we do, noting that
       * the node is irrelevant if the vin_true_depth is 0.
       */
      if (vty->vio->vin_true_depth > 0)
        qassert(node <= MAX_NON_CONFIG_NODE) ;
    } ;
} ;

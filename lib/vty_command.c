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
 * Variables etc.
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */

static void uty_cmd_loop_prepare(vty_io vio) ;
static cmd_return_code_t uty_cmd_hiatus(vty_io vio, cmd_return_code_t ret) ;
static cmd_return_code_t vty_cmd_auth(vty vty, node_type_t* p_next_node) ;
static uint uty_show_error_context(vio_fifo ebuf, vio_vf vf) ;
static uint uty_cmd_failed(vty_io vio, cmd_return_code_t ret) ;
static void uty_cmd_prepare(vty_io vio) ;
static void uty_cmd_config_lock(vty vty) ;
static void uty_cmd_config_lock_check(struct vty *vty, node_type_t node) ;

/*==============================================================================
 * There are two command loops -- cmd_read_config() and cq_process().  These
 * functions support those command loops:  TODO update !!
 *
 *   * uty_cmd_prepare()
 *
 *     After opening or closing a vin/vout object, the state of the execution
 *     context must be set to reflect the current top of the vin/vout stack.
 *
 *   * uty_cmd_dispatch_line()
 *
 *     This is for command lines which come from "push" sources, eg the
 *     Telnet VTY CLI or the VTYSH.
 *
 *     Hands command line over to the vty_cli_nexus message queue.
 *
 *     NB: from this point onwards, the vio is vio->cmd_running ! TODO
 *
 *   * vty_cmd_fetch_line()
 *
 *     This is for command lines which are "pulled" by one of the command
 *     execution loops, eg files and pipes.  If not in cmd_read_config())
 *     can return CMD_WAITING.
 *
 *   * vty_cmd_open_in_pipe_file()
 *   * vty_cmd_open_in_pipe_shell()
 *   * vty_cmd_open_out_pipe_file()
 *   * vty_cmd_open_out_pipe_shell()
 *
 *     These do the I/O side of the required opens, pushing the new vty_vf
 *     onto the vin/vout stack.
 *
 *   * vty_cmd_success()
 *
 *     When a command returns success, this is called either to push all
 *     buffered output to the current vout, or to discard all buffered
 *     output (which is what cmd_read_config() does).
 *
 *     If required, pops any vout(s).
 *
 *
 */

/*------------------------------------------------------------------------------
 * Prepare to enter the config read command loop.
 *
 * Initialise exec object, and copy required settings from the current vin
 * and vout.
 */
extern void
vty_cmd_loop_prepare(vty vty)
{
  VTY_LOCK() ;

  assert(vty->type == VTY_CONFIG_READ) ;

  uty_cmd_loop_prepare(vty->vio) ;   /* by vty->type & vty->node     */
  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Enter the command_queue command loop.
 */
extern void
uty_cmd_loop_enter(vty_io vio)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert(vio->vty->type == VTY_TERMINAL) ;

  uty_cmd_loop_prepare(vio) ;   /* by vty->type & vty->node     */

  cq_loop_enter(vio->vty, vio->vty->node != NULL_NODE ? CMD_SUCCESS
                                                      : CMD_CLOSE) ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to enter a command loop.
 *
 * Initialise cmd_exec object, and its cmd_context -- given vty->type and
 * vty->node.
 */
static void
uty_cmd_loop_prepare(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->vty->exec == NULL) ;
  assert(vio->state     == vc_null) ;

  vio->vty->exec = cmd_exec_new(vio->vty) ;
  vio->state     = vc_running ;

  if (vio->vty->node > MAX_NON_CONFIG_NODE)
    uty_cmd_config_lock(vio->vty) ;             /* TODO cannot fail !?  */

  uty_cmd_prepare(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Signal to the command loop that some I/O has completed -- successfully, or
 * with some I/O error (including time out).
 *
 * If the vio is in vc_waiting state -- so will be exec_hiatus -- send message
 * so that command loop continues.
 *
 * Otherwise, if is vc_running and have CMD_IO_ERROR, set the vc_io_error_trap,
 * so that the next interaction with the vty (other than output) will signal
 * a pending error.
 *
 * Accepts:
 *
 *   CMD_SUCCESS  -- if vc_waiting,       passed in.
 *                   otherwise,           ignored
 *
 *   CMD_WAITING  -- ignored
 *
 *   CMD_IO_ERROR -- if vc_waiting,       passed in
 *                   if vc_running,       set vc_io_error_trap
 *
 *   CMD_CLOSE    -- if vc_waiting,       passed in
 *                   if vc_running,       set vc_close_trap
 *                   if vc_io_error_trap, set vc_close_trap
 */
extern void
uty_cmd_signal(vty_io vio, cmd_return_code_t ret)
{
  VTY_ASSERT_LOCKED() ;

  if (ret == CMD_WAITING)
    return ;

  assert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR) || (ret == CMD_CLOSE)) ;

  switch (vio->state)
    {
      case vc_null:
        zabort("invalid vc_null") ;
        break ;

      case vc_running:          /* ignore CMD_SUCCESS                   */
        if (ret == CMD_IO_ERROR)
          vio->state = vc_io_error_trap ;
        if (ret == CMD_CLOSE)
          vio->state = vc_close_trap ;
        break ;

      case vc_waiting:          /* pass in the return code continue     */
        vio->state = vc_running ;
        cq_continue(vio->vty, ret) ;
        break ;

      case vc_io_error_trap:    /* ignore CMD_SUCCESS or duplicate      */
        if (ret == CMD_CLOSE)   /*                        CMD_IO_ERROR. */
          vio->state = vc_close_trap ;
        break ;

      case vc_close_trap:       /* ignore CMD_SUCCESS, CMD_IO_ERROR,    */
      case vc_stopped:          /*    and duplicate/too late CMD_CLOSE. */
        break ;

      case vc_closed:
        zabort("invalid vc_closed") ;
        break ;

      default:
        zabort("unknown vio->state") ;
        break ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Reset the command loop.
 *
 * This is used by uty_close() to try to shut down the command loop.
 *
 * Does nothing if already closed or never got going.
 *
 * "curtains" means that the program is being terminated, so no message or
 * event handling is running any more, and all threads other than the main
 * thread have stopped.  This means that whatever the state of the command
 * loop, we can terminate it now.  Revokes any outstanding messages/events,
 * in order to tidy up.
 *
 * If not curtains, then push a CMD_CLOSE into the command loop, to bring it
 * to a shuddering halt.
 *
 * Will leave the vio->state:
 *
 *   vc_running      -- a CMD_CLOSE has been passed in (was vc_waiting).
 *                      Will not be the case if "curtains".
 *
 *   vc_close_trap   -- command loop is running, but will stop as soon as it
 *                      sees this trap.
 *                      Will not be the case if "curtains".
 *
 *   vc_stopped      -- command loop is stopped
 *                      Will be the case if "curtains" (unless vc_null or
 *                      vc_closed).
 *                      Otherwise will only be the case if the loop had already
 *                      stopped.
 *
 *   vc_null         -- never been kissed
 *   vc_closed       -- was already closed
 *
 * No other states are possible.
 */
extern void
uty_cmd_loop_close(vty_io vio, bool curtains)
{
  VTY_ASSERT_LOCKED() ;

   if ((vio->state == vc_null) || (vio->state == vc_closed))
    return ;

  if (curtains)
    {
      if (!vio->blocking)
        cq_revoke(vio->vty) ;   /* collect any outstanding message      */

      vio->state = vc_stopped ;
    }
  else
    uty_cmd_signal(vio, CMD_CLOSE) ;
} ;

/*------------------------------------------------------------------------------
 * Exit command loop.
 *
 * Final close of the VTY, giving a reason, if required.
 */
extern void
vty_cmd_loop_exit(vty vty)
{
  VTY_LOCK() ;

  VTY_ASSERT_CAN_CLOSE(vty) ;

  /* Make sure no longer holding the config symbol of power             */
  uty_cmd_config_lock_check(vty, NULL_NODE) ;

  /* Can now close the vty                                              */
  vty->vio->state = vc_stopped ;
  uty_close(vty->vio, NULL, false) ;    /* not curtains */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Handle a "special" command -- anything not cmd_do_command.
 *
 * These "commands" are related to VTY_TERMINAL CLI only.
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

      case cmd_do_ctrl_c:
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
 *   4. host.password  -- set by "password xxx"
 *
 *      Unless no_password_check, if there is no password, you cannot start
 *      a vty.
 *
 *   5. host.enable  -- set by "enable password xxx"
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

  VTY_LOCK() ;                  /* while access host.xxx        */

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
 *      or: any other return code from the current vin when it attempts to
 *          fetch another command line -- including I/O error or timeout.
 *
 * NB: can be called from any thread -- because does no closing of files or
 *     anything other than read/write.
 *
 * TODO -- dealing with states other than vf_open !!
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

  ret = CMD_SUCCESS ;                   /* assume all is well   */

  /* Worry about all the things that stop us from being able to fetch the
   * next command line.
   */
  if ( (vty->vio->state != vc_running)
                                   || (vio->vin_depth < vio->vout->depth_mark)
                                   || (vio->vin_depth > vio->real_depth) )
    ret = CMD_HIATUS ;
  else
    {
      switch (vf->vin_state)
        {
          case vf_open:
          case vf_eof:
          case vf_timed_out:
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

          case vf_closed:       /* TODO treat closed as EOF ?   */
            ret = CMD_EOF ;
            break ;

          case vf_error:
            ret = CMD_IO_ERROR ;
            break ;

          case vf_closing:
            assert(!vf->blocking) ;
            ret = CMD_WAITING ;
            break ;

          default:
            zabort("invalid vf->vin_state") ;
            break ;
        } ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Deal with return code at the "exec_hiatus" point in the command loop.
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
 *   CMD_EOF         -- from vty_cmd_fetch_line() => current vin has hit eof,
 *                      and must be closed and popped.
 *
 *   CMD_CLOSE       -- from a command return (or otherwise) => must close
 *                      and pop the current vin (same as CMD_EOF, really).
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
 *   * I/O errors will cause all levels of  TODO  ... depending on error location ??
 *
 *     I/O errors also cause all closes to be "final", so pending output is
 *     attempted -- but will be abandoned if would block.  Also, any further
 *     I/O errors will be discarded.
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
 *   CMD_EOF         => OK -- but nothing more to fetch, close the vty and exit
 *                            command loop.
 *                                            <=> the vin_base is now closed.
 *
 *                            state == vc_stopped
 *
 *   CMD_IO_ERROR    => some error has occurred while closing stuff.
 *
 *                            state == vc_io_error_trap
 *
 * And nothing else.
 *
 * The CMD_IO_ERROR is returned so that errors are not hidden inside here.
 * At some point vty_cmd_hiatus() must be called again to deal with the
 * error.
 *
 * When the command loop has gone vc_waiting, the I/O side of things can wake
 * it up by uty_cmd_signal(), which passes in a return code.  When the
 * command loop runs it will call this function to handle the new return code.
 * If CMD_SUCCESS is passed in, will continue trying to adjust the vin/vout
 * stacks.
 *
 * The configuration reader command loop also uses vty_cmd_hiatus() to handle
 * all return codes.  However, it will exit the command loop at the slightest
 * hint of trouble.
 *
 * All this TODO (after discussion of error handling)

 * NB: can be called from any thread if !blocking, otherwise MUST be cli thread.
 */
extern cmd_return_code_t
vty_cmd_hiatus(vty vty, cmd_return_code_t ret)
{
  VTY_LOCK() ;
  VTY_ASSERT_CAN_CLOSE(vty) ;

  ret = uty_cmd_hiatus(vty->vio, ret) ;

  VTY_UNLOCK() ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Inside of vty_cmd_hiatus() -- can return at any time.
 */
static cmd_return_code_t
uty_cmd_hiatus(vty_io vio, cmd_return_code_t ret)
{
  /* (0) worry about state of the vio.
   *
   *     We expect it to generally be be vc_running or vc_waiting, otherwise:
   *
   *       vc_io_error_trap => an I/O error has been posted asynchronously
   *
   *                           set state to vc_running and return code to
   *                           the pending CMD_IO_ERROR.
   *
   *       vc_close_trap    => the vty has been reset asynchronously
   *
   *                           set state to vc_running and return code to
   *                           the pending CMD_CLOSE.
   *
   *       vc_stopped       )
   *       vc_closed        )  invalid -- cannot be here in this state
   *       vc_null          )
   */
  switch (vio->state)
    {
      case vc_null:
      case vc_stopped:
      case vc_closed:
        zabort("invalid vc_xxxx") ;
        break ;

      case vc_running:
      case vc_waiting:
        break ;

      case vc_io_error_trap:
        ret = CMD_IO_ERROR ;    /* convert pending IO error             */
        break ;

      case vc_close_trap:
        ret = CMD_CLOSE ;       /* convert pending close                */
        break ;

      default:
        zabort("unknown vio->state") ;
        break ;
    } ;

  vio->state = vc_running ;                     /* running in hiatus    */

  /* (1) Handle the return code.
   *
   * Deal here with the return codes that signify success, or signify
   * success but some vin and/or vout need to be closed.
   *
   * Call uty_cmd_failed() to deal with return codes that signify some
   * sort of failure.  A failure generally means closing all the way to
   * the vin_/vout_base, or possibly completely.
   *
   * Note that CMD_WAITING is immediately returned !
   */
  switch (ret)
  {
    case CMD_SUCCESS:
    case CMD_EMPTY:
    case CMD_HIATUS:
      break ;

    case CMD_WAITING:
      assert(!vio->blocking) ;
      vio->state  = vc_waiting ;
      return ret ;              /* <<< exit here on CMD_WAITING         */

    case CMD_EOF:
      uty_out_accept(vio) ;     /* accept any buffered remarks.         */
      assert(vio->real_depth > 0) ;
      --vio->real_depth ;
      break ;

    case CMD_CLOSE:
      uty_out_accept(vio) ;     /* accept any buffered remarks.         */
      vio->real_depth = 0 ;     /* which it may already be              */
      break ;

    default:
      /* If not any of the above, must be an error of some kind:
       *
       *   * set real_depth to close all pipes and files.
       *
       *     Depending on the type of vin_base/vout_base and the type of
       *     error, may or may not leave the bas I/O open.
       *
       *   * create error messages in the vout_base.
       */
      vio->real_depth = uty_cmd_failed(vio, ret) ;
      break ;
  } ;

  ret = CMD_SUCCESS ;           /* OK, so far                           */

  /* (2) Do we need to close one or more vin, or are we waiting for one to
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
   *     child completion status has not yet been collected).  Where a
   *     close operation does not complete, the vf is marked vf_closing,
   *     and the stack stays at its current level.
   *
   *     For hard errors will do "final" close, which immediately closes the
   *     input (and any pipe return) discarding any buffered input.  Any errors
   *     that occur in the process are discarded.
   */
  while ((vio->vin_depth > vio->real_depth) && (ret == CMD_SUCCESS))
    ret = uty_vin_pop(vio, vio->err_hard, vio->vty->exec->context) ;

  /* (3) And, do we need to close one or more vout, or are we waiting for
   *     one to close ?  If so:
   *
   *     Any output after the current end_mark is discarded.  Note that we
   *     do not actually close the vout_base.
   *
   *     In all cases we push any remaining output and collect any remaining
   *     pipe return, and collect child termination condition.
   *
   *     For blocking vio, close operations will either complete or fail.
   *
   *     For non-blocking vio, close operations may return CMD_WAITING
   *     (eg: where the output buffers have not yet been written away).
   *     Where a close operation does not complete, the vf is marked
   *     vf_closing, and the stack stays at its current level.
   *
   *     For hard errors will attempt to write away anything which is,
   *     pending, but will stop if would block and on any error, and close
   *     the output -- discarding any remaining output and any errors.
   *
   *     NB: when we reach the vout_base, turn off the hard error -- so
   *         never "final" close the vout    TODO error handling.
   *
   * Also: if we are at, or we reach, the vout_base, and there is an error
   *       message in hand, now is the time to move that to the obuf and
   *       push it.
   */
  while (ret == CMD_SUCCESS)
    {
      assert(vio->vout->depth_mark >= vio->vout_depth) ;

      if (vio->vout_depth == 1)
        {
          assert(vio->vout->depth_mark == 1) ;

          vio->err_hard = false ;

          if (vio->ebuf != NULL)
            {
              vio_fifo_copy(vio->obuf, vio->ebuf) ;
              vio->ebuf = vio_fifo_free(vio->ebuf) ;

              ret = uty_cmd_out_push(vio->vout, vio->err_hard) ;

              if (ret != CMD_SUCCESS)
                break ;
            } ;
        } ;

      if (vio->vout_depth == 0)
        assert(vio->vout->depth_mark == 0) ;

      if (vio->vin_depth < vio->vout->depth_mark)
        ret = uty_vout_pop(vio, vio->err_hard) ;
      else
        break ;
    } ;

  /* (4) Quit now if not successful on stack adjustment
   */
  if (ret != CMD_SUCCESS)
    {
      if (ret == CMD_WAITING)
        {
          assert(!vio->blocking) ;
          vio->state  = vc_waiting ;
          return ret ;                  /* <<< exit here on CMD_WAITING */
        } ;

      if (ret == CMD_IO_ERROR)
        {
          vio->state  = vc_io_error_trap ;
          return ret ;
        } ;

      zabort("invalid return code") ;
    } ;

  /* (5) Having dealt with closing of files and adjustment of stack, may
   *     now be EOF on the vin_base.
   */
  if (vio->real_depth == 0)
    {
      vio->state  = vc_stopped ;
      return CMD_EOF ;
    } ;

  /* (6) All ready now to continue processing commands.
   */
  uty_cmd_prepare(vio) ;    /* update vty->exec state               */

  return CMD_SUCCESS ;
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

  exec->out_suppress  = (vio->vty->type == VTY_CONFIG_READ) &&
                                                       (vio->vout_depth == 1) ;
  exec->reflect       = exec->context->reflect_enabled && !exec->out_suppress ;
} ;

/*------------------------------------------------------------------------------
 * Set the full_lex flag for further commands, and for any further pipes.
 */
extern void
vty_cmd_set_full_lex(vty vty, bool full_lex)
{
  VTY_LOCK() ;

  vty->exec->context->full_lex = full_lex ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Reflect the command line to the current vio->obuf.
 *
 * Advances the end_mark past the reflected line, so that output (in particular
 * error stuff) is separate.
 *
 * NB: pushes the output, so that if the command takes a long time to process,
 *     it is visible while it proceeds.
 */
extern cmd_return_code_t
vty_cmd_reflect_line(vty vty)
{
  cmd_return_code_t  ret ;

  VTY_LOCK() ;

  if (vty->vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    {
      vio_fifo obuf ;
      qstring  line ;

      obuf = vty->vio->obuf ;       /* once locked          */
      line = vty->exec->action->line ;

      vio_fifo_put_bytes(obuf, qs_char_nn(line), qs_len_nn(line)) ;
      vio_fifo_put_byte(obuf, '\n') ;

      ret = uty_cmd_out_push(vty->vio->vout, false) ;       /* not final    */
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

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

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
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

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    {
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
                                                          cmd_pipe_type_t type)
{
  cmd_return_code_t ret ;

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    {
      ret = uty_file_write_open(vio, name,
                                     ((type & cmd_pipe_append) != 0), context) ;
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
                                                           cmd_pipe_type_t type)
{
  cmd_return_code_t ret ;

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    {
      ret = uty_pipe_write_open(vio, command,
                                           ((type & cmd_pipe_shell_only) !=0)) ;

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
uty_cmd_open_out_dev_null(vty_io vio)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_ASSERT_LOCKED() ;

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    {
      vf = uty_vf_new(vio, "dev_null", -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_DEV_NULL, NULL, NULL, 0) ;

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
  else if ((*(name + 1) == '/') || (*(name + 1) == '\0'))
    dst = qpath_copy(dst, context->dir_home) ;
  else if ((*(name + 1) == '.') &&
                             ( (*(name + 2) == '/') || (*(name + 2) == '\0')) )
    dst = qpath_copy(dst, context->dir_here) ;
  else
    return qpath_set(dst, name) ;       /* no idea... probably an error */

  return qpath_append_str(dst, name) ;  /* create the full path         */
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

  ret = CMD_SUCCESS ;

  if (vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
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
 * See uty_cmd_out_push() below.
 */
extern cmd_return_code_t
vty_cmd_out_push(vty vty)
{
  cmd_return_code_t ret ;

  VTY_LOCK() ;

  if (vty->vio->state != vc_running)
    ret = CMD_HIATUS ;
  else
    ret = uty_cmd_out_push(vty->vio->vout, false) ;     /* not final    */

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, push it to be written, now.
 *
 * For most outputs we kick the I/O side so that the pselect processing deals
 * with the required write.  For VOUT_STDERR and VOUT_STDOUT, output goes
 * directly to standard I/O.
 *
 * Advances the end_mark past the stuff pushed.
 *
 * NB: takes no notice of vf->out_suppress, which applies only to buffered
 *     output present when successfully complete a command -- vty_cmd_success().
 *
 * TODO: worry about closing state !
 * TODO: need a vf level one of these.
 */
extern cmd_return_code_t
uty_cmd_out_push(vio_vf vf, bool final) /* TODO: write errors & blocking   */
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  vio_fifo_step_end_mark(vf->obuf) ;    /* advance the end mark         */

  ret = CMD_SUCCESS ;

  switch (vf->vout_state)
    {
      case vf_open:
      case vf_closing:
        switch (vf->vout_type)
          {
            case VOUT_NONE:
              zabort("invalid vout_none") ;
              break ;

            case VOUT_TERM:
              ret = uty_term_out_push(vf, final) ;
              break ;

            case VOUT_VTYSH:
              /* Kick the writer    */
              break ;

            case VOUT_FILE:
              ret = uty_file_out_push(vf, final) ;
              break ;

            case VOUT_PIPE:
              ret = uty_pipe_out_push(vf, final) ;
              break ;

            case VOUT_CONFIG:
              ret = uty_file_out_push(vf, final) ;  /* treat as file        */
              break ;

            case VOUT_DEV_NULL:
            case VOUT_SHELL_ONLY:
              vio_fifo_clear(vf->obuf, false) ;     /* keep end mark        */
              break ;

            case VOUT_STDOUT:
              vio_fifo_fwrite(vf->obuf, stdout) ;   // TODO errors
              break ;

            case VOUT_STDERR:
              vio_fifo_fwrite(vf->obuf, stderr) ;   // TODO errors
              break ;

            default:
              zabort("unknown vout_type") ;
          } ;
        break ;

      case vf_closed:
      case vf_timed_out:
        break ;                 /* immediate success !  */

      case vf_eof:
        zabort("vf->vout cannot be vf_eof") ;
        break ;

      case vf_error:
        ret = CMD_IO_ERROR ;
        break ;

      default:
        zabort("unknown vf->vout_state") ;
        break ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Dealing with error of some kind.
 *
 * The current vio->obuf will have an end_mark.  After the end_mark will be
 * any output generated since the start of the current command (or any out_push
 * since then).  For command errors, that output is expected to be error
 * messages associated with the error.
 *
 * A new "ebuf" fifo is created, and the location of the error is written to
 * that fifo.  Once the contents of the "ebuf" are output, the command line in
 * which the error occurred should be the last thing output.
 *
 * Any other error message is then appended to the ebuf.
 *
 * For command errors, the tail of the vio->obuf (stuff after the end_mark) is
 * moved to the ebuf.
 *
 * Deals with:
 *
 *   CMD_WARNING     =                 TODO
 *   CMD_ERROR       =>
 *
 *   CMD_ERR_PARSING,              parser: general parser error
 *   CMD_ERR_NO_MATCH,             parser: command/argument not recognised
 *   CMD_ERR_AMBIGUOUS,            parser: more than on command matches
 *   CMD_ERR_INCOMPLETE,
 *
 *   CMD_IO_ERROR                  I/O -- failed :-(
 *   CMD_IO_TIMEOUT                I/O -- timed out :-(
 *
 * In any event, need to separate out any output from the failing command,
 * so that can report error location and type, before showing the error
 * message(s).
 *
 * NB: does not expect to see all the possible CMD_XXX return codes (see
 *     below), but treats all as a form of error !
 */
static uint
uty_cmd_failed(vty_io vio, cmd_return_code_t ret)
{
  ulen        indent ;

  VTY_ASSERT_LOCKED() ;

  /* Process the vin stack to generate the error location(s)            */
  if (vio->ebuf != NULL)
    vio_fifo_clear(vio->ebuf, true) ;
  else
    vio->ebuf = vio_fifo_new(1000) ;

  indent = uty_show_error_context(vio->ebuf, vio->vin) ;

  /* Now any additional error message if required                       */
  switch (ret)
  {
    case CMD_WARNING:
      if (vio_fifo_tail_empty(vio->obuf))
        vio_fifo_printf(vio->ebuf, "%% WARNING: non-specific warning\n") ;
      break ;

    case CMD_ERROR:
      if (vio_fifo_tail_empty(vio->obuf))
        vio_fifo_printf(vio->ebuf, "%% ERROR: non-specific error\n") ;
      break ;

    case CMD_ERR_PARSING:
      cmd_get_parse_error(vio->ebuf, vio->vty->exec->parsed, indent) ;
      break ;

    case CMD_ERR_NO_MATCH:
      vio_fifo_printf(vio->ebuf, "%% Unknown command.\n") ;
      break;

    case CMD_ERR_AMBIGUOUS:
      vio_fifo_printf(vio->ebuf, "%% Ambiguous command.\n");
      break;

    case CMD_ERR_INCOMPLETE:
      vio_fifo_printf(vio->ebuf, "%% Command incomplete.\n");
      break;

    case CMD_IO_ERROR:
      break ;

    case CMD_IO_TIMEOUT:
      break ;

    default:
      vio_fifo_printf(vio->ebuf, "%% Unexpected return code (%d).\n", (int)ret);
      break ;
  } ;

  /* Now stick the obuf tail onto the end of the ebuf & discard the tail of
   * the obuf.
   */
  vio_fifo_copy_tail(vio->ebuf, vio->obuf) ;
  vio_fifo_back_to_end_mark(vio->obuf, true) ;

  /* Decide what stack level to close back to.                          */
  return (vio->vty->type == VTY_TERMINAL) ? 1 : 0 ;  // TODO I/O error ??
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

/*==============================================================================
 * Configuration node/state handling
 *
 * At most one VTY may hold the configuration symbol of power at any time.
 */

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power -- may already own it !
 *
 * Returns: true <=> now own the symbol of power (or already did).
 */
extern cmd_return_code_t
vty_cmd_config_lock (vty vty)
{
  VTY_LOCK() ;
  uty_cmd_config_lock(vty) ;
  VTY_UNLOCK() ;

  if (vty->config)
    return CMD_SUCCESS ;

  vty_out (vty, "VTY configuration is locked by other VTY\n") ;
  return CMD_WARNING ;
} ;

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power -- may already own it !
 *
 * Returns: true <=> now own the symbol of power (or already did).
 */
static void
uty_cmd_config_lock (vty vty)
{
  if (!host.config)             /* If nobody owns the lock...           */
    {
      host.config = true ;      /* ...rope it...                        */
      vty->config = true ;

      do
        ++host.config_brand ;   /* ...update the brand...               */
      while (host.config_brand == 0) ;

      vty->config_brand = host.config_brand ;   /* ...brand it.         */
    }
  else                          /* Somebody owns the lock...            */
    {
      if (vty->config)          /* ...if we think it is us, check brand */
        assert(host.config_brand == vty->config_brand) ;
    } ;
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
 * If node >  MAX_NON_CONFIG_NODE, must own the symbol of power.
 *
 * If node <= MAX_NON_CONFIG_NODE, will release symbol of power, if own it,
 * PROVIDED is at vin_depth <= 1 !!
 */
static void
uty_cmd_config_lock_check(vty vty, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;

  if (vty->config)
    {
      /* We think we own it, so we better had                   */
      assert(host.config) ;
      assert(host.config_brand == vty->config_brand) ;

      /* If no longer need it, release                          */
      if ((node <= MAX_NON_CONFIG_NODE) && (vty->vio->vin_depth <= 1))
        {
          host.config  = false ;
          vty->config  = false ;
        } ;
    }
  else
    {
      /* We don't think we own it, so we had better not         */
      if (host.config)
        assert(host.config_brand != vty->config_brand) ;

      /* Also, node had better not require that we do.          */
      assert(node <= MAX_NON_CONFIG_NODE) ;
    } ;
} ;

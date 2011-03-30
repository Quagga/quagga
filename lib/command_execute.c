/* Command Line Execution
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Recast and extended: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "misc.h"

#include "command_local.h"
#include "command_parse.h"
#include "command_execute.h"
#include "command_queue.h"
#include "vty_common.h"
#include "vty_command.h"
#include "memory.h"

/*==============================================================================
 * Construct and destroy cmd_exec object.
 *
 *
 */

/*------------------------------------------------------------------------------
 * Construct cmd_exec object and initialise.
 *
 * This is done when a command loop is entered.
 *
 * The initial cmd_context reflects the vty->type and initial vty->node.
 */
extern cmd_exec
cmd_exec_new(vty vty)
{
  cmd_exec     exec ;
  cmd_context  context ;

  exec = XCALLOC(MTYPE_CMD_EXEC, sizeof(struct cmd_exec)) ;

  /* Zeroising has set:
   *
   *   vty          = X         -- set below
   *
   *   action       = all zeros
   *
   *   context      = NULL      -- see below
   *
   *   parsed       = NULL      -- see below
   *
   *   password_failures   = 0  -- none, yet
   *
   *   state        = exec_null
   *   locus        = NULL      -- not significant in exec_null
   *   ret          = CMD_SUCCESS
   *
   *   cq           = NULL      -- no mqb (qpthreads)
   *                            -- no thread (legacy thread)
   */
  confirm(CMD_ACTION_ALL_ZEROS) ;       /* action       */
  confirm(exec_null == 0) ;             /* state        */
  confirm(CMD_SUCCESS == 0) ;           /* ret          */

  exec->vty     = vty ;

  exec->parsed  = cmd_parsed_new() ;

  /* Initialise the context                                             */
  VTY_ASSERT_LOCKED() ;

  exec->context = context = cmd_context_new() ;

  context->node            = vty->node ;

  context->parse_execution = true ;
  context->parse_only      = false ;
  context->reflect_enabled = false ;

  context->onode           = NULL_NODE ;

  context->dir_cd   = qpath_dup(host.cwd) ;
  context->dir_home = qpath_dup(host.config_dir) ;

  switch (vty->type)
    {
      case VTY_TERMINAL:
        context->full_lex      = true ;

        context->parse_strict  = false ;
        context->parse_no_do   = false ;
        context->parse_no_tree = false ;

        context->can_enable    = context->node >= ENABLE_NODE ;
        context->can_auth_enable = true ;

        context->dir_here = qpath_dup(context->dir_cd) ;

        break ;

      case VTY_SHELL_SERVER:
        zabort("missing VTY_SHELL_SERVER context") ;

        context->full_lex      = true ;

        context->parse_strict  = false ;
        context->parse_no_do   = false ;
        context->parse_no_tree = false ;

        context->can_enable    = true ;
        context->can_auth_enable = false ;

        context->dir_here = qpath_dup(context->dir_cd) ;

        break ;

      case VTY_CONFIG_READ:
        context->full_lex      = false ;

        context->parse_strict  = true ;
        context->parse_no_do   = true ;
        context->parse_no_tree = false ;

        context->can_enable    = true ;
        context->can_auth_enable = false ;

        context->dir_here = qpath_dup(context->dir_home) ;

        break ;

      default:
        zabort("vty type unknown to cmd_exec_new()") ;
    } ;

  return exec ;
} ;

/*------------------------------------------------------------------------------
 * Make a new context -- returns a completely empty context object.
 */
extern cmd_context
cmd_context_new(void)
{
  return XCALLOC(MTYPE_CMD_EXEC, sizeof(struct cmd_context)) ;
} ;

/*------------------------------------------------------------------------------
 * Save current context and update for new, child vin.
 *
 *   - updates the dir_here if required
 *
 *   - cannot inherit can_enable unless is ENABLE_NODE or better
 *
 *   - cannot inherit can_auth_enable, no how
 */
extern cmd_context
cmd_context_new_save(cmd_context context, qpath file_here)
{
  cmd_context  saved ;

  saved = cmd_context_new() ;

  *saved = *context ;         /* copy as is           */

  /* The saved copy of the context now owns the current paths, so now need
   * to duplicate (or set new) paths.
   */
  context->dir_cd   = qpath_dup(saved->dir_cd) ;
  context->dir_home = qpath_dup(saved->dir_home) ;

  if (file_here == NULL)
    context->dir_here = qpath_dup(saved->dir_here) ;
  else
    {
      context->dir_here = qpath_dup(file_here) ;
      qpath_shave(context->dir_here) ;
    } ;

  /* The inheritance of can_enable is tricky.  Will not bequeath can_enable
   * is is not already in ENABLE_NODE or better !
   */
  if (context->node <= ENABLE_NODE)
    context->can_enable = false ;

  context->can_auth_enable = false ;

  return saved ;
} ;

/*------------------------------------------------------------------------------
 * Restore given context -- frees the copy restored from.
 *
 * Has to free the directories in the context being restored to.
 *
 * Returns NULL.
 */
extern cmd_context
cmd_context_restore(cmd_context dst, cmd_context src)
{
  assert(src != NULL) ;

  qpath_free(dst->dir_cd) ;
  qpath_free(dst->dir_home) ;
  qpath_free(dst->dir_here) ;

  *dst = *src ;         /* copy as is           */

  return cmd_context_free(src, true) ;
} ;

/*------------------------------------------------------------------------------
 * Free the given context.
 *
 * If the context is a copy of an existing context, then must not free the
 * directories -- they will be freed when that existing context is freed.
 */
extern cmd_context
cmd_context_free(cmd_context context, bool copy)
{
  if (context != NULL)
    {
      if (!copy)
        {
          qpath_free(context->dir_cd) ;
          qpath_free(context->dir_home) ;
          qpath_free(context->dir_here) ;
        } ;

      XFREE(MTYPE_CMD_EXEC, context) ;          /* sets context = NULL  */
    } ;

  return context ;
} ;

/*------------------------------------------------------------------------------
 * Destroy cmd_exec object.
 */
extern cmd_exec
cmd_exec_free(cmd_exec exec)
{
  if (exec != NULL)
    {
      exec->parsed  = cmd_parsed_free(exec->parsed) ;
      exec->context = cmd_context_free(exec->context, false) ;  /* not a copy */

      XFREE(MTYPE_CMD_EXEC, exec) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 *
 */
/*------------------------------------------------------------------------------
 * Parse and execute a command.
 *
 * The command is given by vty->buf and vty->node.
 *
 * Uses vty->parsed.
 *
 *   -- use exact/completion parsing, as required.
 *
 *   -- parse in current node and in ancestors, as required
 *
 *      If does not find in any ancestor, return error from current node.
 *
 *   -- implement the "do" shortcut, as required
 *
 * If qpthreads_enabled, then may queue the command rather than execute it
 * here.
 *
 * The vty->node may be changed during the execution of the command, and may
 * be returned changed once the command has completed.
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 */
#if 0
extern enum cmd_return_code
cmd_execute_command(struct vty *vty,
                             cmd_parse_type_t type, struct cmd_command **cmd)
{
  enum cmd_return_code ret ;

  /* Try to parse in vty->node or, if required, ancestors thereof.      */
  ret = cmd_parse_command(vty->exec->parsed, vty->line, vty->node, type) ;

  if (cmd != NULL)
    *cmd = vty->exec->parsed->cmd ;   /* for vtysh                            */

  if      (ret == CMD_SUCCESS)
    ret = cmd_dispatch(vty, cmd_may_queue) ;

  return ret ;
} ;
#endif

/*------------------------------------------------------------------------------
 * Read configuration from file.
 *
 * In the qpthreads world this assumes that it is running with the vty
 * locked, and that all commands are to be executed directly.
 *
 * If the 'first_cmd' argument is not NULL it is the address of the first
 * command that is expected to appear.  If the first command is not this, then
 * the 'first_cmd' is called with argv == NULL (and argc == 0) to signal the
 * command is being invoked by default.
 *
 * Command processing continues while CMD_SUCCESS is returned by the command
 * parser and command execution.
 *
 * If 'ignore_warning' is set, then any CMD_WARNING returned by command
 * execution is converted to CMD_SUCCESS.  Note that any CMD_WARNING returned
 * by command parsing (or in execution of any default 'first_cmd').
 *
 * Returns: cmd_return_code for last command or I/O operation
 *
 *          when returns, the entire vin/vout stack will have been closed.
 *
 * If reaches EOF on the config file, returns CMD_SUCCESS.  If anything else
 * happens, will generate an error message and exits the command loop.
 */
extern cmd_return_code_t
cmd_read_config(struct vty *vty, cmd_command first_cmd, bool ignore_warning)
{
  cmd_exec          exec   = vty->exec ;
  cmd_parsed        parsed = exec->parsed ;
  cmd_return_code_t ret;

  ret = CMD_SUCCESS ;           /* so far, so good      */

  while (1)
    {
      /* Deal with anything which is not success !
       */
      if ((ret != CMD_SUCCESS) && (ret != CMD_EMPTY))
        {
          /* Will drop straight out of the loop if have anything other
           * than CMD_HIATUS, CMD_EOF or CMD_CLOSE, which are all signals
           * that some adjustment to the vin/vout stacks is required,
           * or that we are all done here.
           *
           * Everything else is deemed to be an error that stops the
           * command loop.
           */
          if ((ret != CMD_HIATUS) && (ret != CMD_EOF) && (ret != CMD_CLOSE))
            break ;

          ret = vty_cmd_hiatus(vty, ret) ;
                                /* for CMD_EOF & CMD_HIATUS only        */

          if (ret == CMD_EOF)
            return CMD_SUCCESS ;        /* eof on the config file is
                                           the expected outcome !       */
          if (ret != CMD_SUCCESS)
            break ;
        } ;

      /* If all is well, need another command line                      */

      ret = vty_cmd_fetch_line(vty) ;   /* sets exec->action    */
      if (ret != CMD_SUCCESS)
        continue ;

      /* Parse the command line we now have                             */
      assert(exec->action->to_do == cmd_do_command) ;

      cmd_tokenize(parsed, exec->action->line, exec->context->full_lex) ;
      ret = cmd_parse_command(parsed, exec->context) ;

      if (ret != CMD_SUCCESS)
        continue ;

      /* Special handling before first active line.                     */
      if (first_cmd != NULL)
        {
          if (first_cmd != parsed->cmd)
            {
              ret = (*first_cmd->func)(first_cmd, vty, -1, NULL) ;
              if (ret != CMD_SUCCESS)
                continue ;
            } ;
          first_cmd = NULL ;
        } ;

      /* reflection now.....                                            */
      if (exec->reflect)
        {
          ret = vty_cmd_reflect_line(vty) ;
          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* Pipe work, if any                                              */
      if ((parsed->parts & cmd_parts_pipe) != 0)
        {
          ret = cmd_open_pipes(vty) ;
          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* Command execution, if any                                      */
      if ((parsed->parts & cmd_part_command) != 0)
        ret = cmd_execute(vty) ;

      /* Deal with success (or suppressed warning).                     */
      if ((ret == CMD_SUCCESS) || ((ret == CMD_WARNING) && ignore_warning))
        ret = vty_cmd_success(vty) ;
    } ;

  /* Arrives here if:
   *
   *   - vty_cmd_fetch_line() returns anything except CMD_SUCCESS, CMD_EOF or
   *     CMD_HIATUS -- which are not errors.
   *
   *   - any other operation returns anything except CMD_SUCCESS
   *     (or CMD_WARNING, if they are being ignored), CMD_EOF or CMD_CLOSE.
   *
   * Deal with any errors -- generate suitable error messages and close back
   * to (but excluding) vout_base.
   *
   * CMD_SUCCESS and CMD_EMPTY are impossible at this point -- they should
   * have been dealt with in the loop.
   *
   * CMD_EOF is also impossible -- vty_cmd_fetch_line() or vty_cmd_hiatus()
   * can return that, but that will have been dealt with.
   *
   * CMD_CLOSE is also impossible -- commands can return that, but that will
   * have been dealt with.
   *
   * CMD_WAITING is not valid for blocking vio !
   */
  assert(ret != CMD_SUCCESS) ;
  assert(ret != CMD_EMPTY) ;
  assert(ret != CMD_EOF) ;
  assert(ret != CMD_CLOSE) ;
  assert(ret != CMD_WAITING) ;

  vty_cmd_hiatus(vty, ret) ;

  return ret ;
} ;

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Open in and/or out pipes
 *
 *  * Returns:
 *
 *   - OK       -- CMD_SUCCESS
 *   - error    -- CMD_ERROR, etc
 */
extern cmd_return_code_t
cmd_open_pipes(vty vty)
{
  cmd_exec          exec   = vty->exec ;
  cmd_parsed        parsed = exec->parsed ;
  cmd_return_code_t ret ;
  vty_io            vio ;
  bool              sync_depth ;
  VTY_LOCK() ;
  vio = vty->vio ;

  ret = CMD_SUCCESS ;

  sync_depth = false ;          /* assuming no in pipe                  */

  /* Deal with any in pipe stuff                                        */
  if ((parsed->parts & cmd_part_in_pipe) != 0)
    {
      qstring args ;

      args = cmd_tokens_concat(parsed, parsed->first_in_pipe,
                                       parsed->num_in_pipe) ;

      if      ((parsed->in_pipe & cmd_pipe_file) != 0)
        ret = uty_cmd_open_in_pipe_file(vio, exec->context, args,
                                                              parsed->in_pipe) ;
      else if ((parsed->in_pipe & cmd_pipe_shell) != 0)
        ret = uty_cmd_open_in_pipe_shell(vio, exec->context, args,
                                                              parsed->in_pipe) ;
      else
        zabort("invalid in pipe state") ;

      qs_free(args) ;

      sync_depth = true ;
    } ;

  /* Deal with any out pipe stuff                                       */
  if (((parsed->parts & cmd_part_out_pipe) != 0) && (ret == CMD_SUCCESS))
    {
      qstring args ;

      args = cmd_tokens_concat(parsed, parsed->first_out_pipe,
                                       parsed->num_out_pipe) ;

      if      ((parsed->out_pipe & cmd_pipe_file) != 0)
        ret = uty_cmd_open_out_pipe_file(vio, exec->context, args,
                                                            parsed->out_pipe) ;
      else if ((parsed->out_pipe & cmd_pipe_shell) != 0)
        ret = uty_cmd_open_out_pipe_shell(vio, exec->context, args,
                                                            parsed->out_pipe) ;
      else if ((parsed->out_pipe & cmd_pipe_dev_null) != 0)
        ret = uty_cmd_open_out_dev_null(vio) ;
      else
        zabort("invalid out pipe state") ;

      if (sync_depth && (ret == CMD_SUCCESS))
        uty_vout_sync_depth(vio) ;

      qs_free(args) ;
    } ;

  VTY_UNLOCK() ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command Execution.
 *
 * If !parse_only, set vty->node and dispatch the command.
 *
 * Returns: CMD_SUCCESS   -- it all want very well
 *          CMD_WARNING   -- not so good: warning message sent by vty_out()
 *          CMD_ERROR     -- not so good: warning message sent by vty_out()
 *          CMD_CLOSE     -- close the current input
 *
 * NB: the distinction between CMD_WARNING and CMD_ERROR is that CMD_WARNING
 *     may be ignored when reading a configuration file.
 *
 * NB: no other returns are acceptable !
 */
extern cmd_return_code_t
cmd_execute(vty vty)
{
  cmd_parsed  parsed  = vty->exec->parsed ;
  cmd_context context = vty->exec->context ;
  cmd_command cmd     = parsed->cmd ;

  cmd_return_code_t ret ;

  vty->node = parsed->cnode ;

  if (context->parse_only)
    ret = CMD_SUCCESS ;
  else
    ret = (*(cmd->func))(cmd, vty, cmd_arg_vector_argc(parsed),
                                   cmd_arg_vector_argv(parsed)) ;

  if (ret == CMD_SUCCESS)
    {
      /* If the node is changed by the command, do that now and make sure
       * that the configuration symbol of power is straight.
       *
       * If the new node is >= CONFIG_NODE, then MUST already have acquired
       * the symbol of power (otherwise the command would have failed !)
       *
       * If the new node is < CONFIG_NODE, then we will here release the
       * symbol of power iff we are at the vin_base !
       *
       * If the new node is NULL_NODE, then treat as CMD_CLOSE.
       */
      if (context->node != parsed->nnode)
        {
          context->node = parsed->nnode ;
          vty_cmd_config_lock_check(vty, context->node) ;

          if (context->node == NULL_NODE)
            ret = CMD_CLOSE ;
        } ;

      /* The command should (no longer) change the vty->node, but if it does,
       * it had better be to the same as what the parser expected -- for if
       * not, that will break "parse_only" and generally cause confusion.
       */
      qassert((vty->node == parsed->cnode) || (vty->node == parsed->nnode)) ;
    }
  else
    {
      /* Enforce restrictions on return codes.                          */
      assert((ret == CMD_WARNING) || (ret == CMD_ERROR)
                                  || (ret == CMD_CLOSE)) ;
    } ;

  return ret ;
} ;

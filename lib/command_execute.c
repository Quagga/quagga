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
#include "vty_io.h"
#include "memory.h"

/*==============================================================================
 * Construct and destroy cmd_exec and cmd_context objects.
 */

/*------------------------------------------------------------------------------
 * Construct cmd_exec object and initialise.
 *
 * This is done when a command loop is entered.
 *
 * The initial cmd_context reflects the vty->type and initial vty->node.
 */
extern cmd_exec
cmd_exec_new(vty vty, cmd_context context)
{
  cmd_exec     exec ;

  exec = XCALLOC(MTYPE_CMD_EXEC, sizeof(cmd_exec_t)) ;

  /* Zeroising has set:
   *
   *   vty          = X         -- set below
   *
   *   context      = X         -- set below
   *
   *   parsed       =           -- see below
   *
   *   reflect      = false
   *
   *   password_failures   = 0  -- none, yet
   *
   *   state        = exec_null
   *   locus        = NULL      -- not significant in exec_null
   *   ret          = CMD_SUCCESS
   *
   *   cq           = NULL      -- no mqb (qpthreads)
   *                            -- no thread (legacy thread)
   *
   *   line_buf     = NULL      -- none, yet
   */
  confirm(exec_null == 0) ;             /* state        */

  exec->vty     = vty ;
  exec->parsed  = cmd_parsed_new() ;
  exec->context = context ;

  return exec ;
} ;

/*------------------------------------------------------------------------------
 * Make a completely empty new context.
 *
 *   node             -- NULL_NODE
 *
 *   daemons          -- DAEMON_NONE
 *
 *   to_do            -- cmd_do_nothing
 *   line             -- NULL => none
 *
 *   vxnode           -- NULL_NODE
 *   vcnode           -- NULL_NODE
 *
 *   full_lex         -- false
 *   parse_execution  -- false
 *   parse_only       -- false
 *
 *   parse_strict     -- false
 *   parse_no_do      -- false
 *   parse_no_tree    -- false
 *
 *   can_enable       -- false
 *
 *   reflect          -- false
 *   out_ordinary     -- false
 *   out_warning      -- false
 *   warn_stop        -- false
 *
 *   onode            -- NULL_NODE
 *   tnode            -- NULL_NODE
 *
 *   dir_cd           -- NULL => none
 *   dir_here         -- NULL => none
 */
extern cmd_context
cmd_context_new(void)
{
  return XCALLOC(MTYPE_CMD_EXEC, sizeof(cmd_context_t)) ;

  confirm(NULL_NODE == 0) ;
  confirm(DAEMON_NONE == 0) ;
  confirm(cmd_do_nothing == 0) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise command context object.
 *
 * This is done when a command loop is entered, and result reflects the given
 * vty_type_t and node_type_t.
 */
extern void
cmd_context_init(cmd_context context, vty_type_t type, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;

  context->node = context->cnode = node ;

  context->daemons         = daemons_set ;

  context->parse_execution = true ;
  context->parse_only      = false ;

  context->dir_cd          = qpath_dup(host.cwd) ;
  context->dir_here        = qpath_dup(host.config_dir) ;

  /* The basic context settings -- some of which may be overridden for
   * some vty types.
   */
  context->full_lex        = false ;

  context->parse_strict    = false ;
  context->parse_no_do     = false ;
  context->parse_no_tree   = false ;

  context->can_enable      = false ;

  context->reflect         = false ;
  context->out_ordinary    = true ;
  context->out_warning     = true ;
  context->warn_stop       = true ;

  /* Particular context settings -- depending on vty type
   */
  switch (type)
    {
      case VTY_TERMINAL:
        context->full_lex        = true ;

        context->can_enable      = cmd_node_is_ecs(context->node) ;

        break ;

      case VTY_VTYSH:
        context->can_enable      = true ;

        break ;

      case VTY_VTYSH_SERVER:
        context->can_enable      = true ;

        context->parse_no_do     = true ;
        context->parse_no_tree   = true ;

        break ;

      case VTY_STDOUT:
        context->can_enable      = false ;

        break ;

      default:
        zabort("vty type unknown to cmd_exec_new()") ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set context specific to reading configuration files along with command line
 * options.
 */
extern void
cmd_context_config_set(cmd_context context, bool ignore_warnings,
                                            bool show_warnings)
{
  context->full_lex        = false ;

  context->parse_strict    = true ;
  context->parse_no_do     = false ;
  context->parse_no_tree   = false ;

  context->can_enable      = true ;

  context->reflect         = false ;
  context->out_ordinary    = false ;
  context->out_warning     = !ignore_warnings ;
  context->warn_stop       = !(ignore_warnings || show_warnings) ;
} ;

/*------------------------------------------------------------------------------
 * Save command context -- creates copy to be saved.
 *
 * Note that the current "tos" is always the same, so this makes a copy of the
 * tos, except that the tos gets new copies of the various paths, and the
 * copy to be saved get the originals.  This is so that the restore operation
 * can restore the tos exactly as it was.
 */
extern cmd_context
cmd_context_save(cmd_context tos)
{
  cmd_context save ;

  save = cmd_context_new() ;

  *save = *tos ;        /* copy everything      */

  tos->dir_cd   = qpath_copy(NULL, save->dir_cd) ;
  tos->dir_here = qpath_copy(NULL, save->dir_here) ;

  return save ;
} ;

/*------------------------------------------------------------------------------
 * Restore command context -- discards the saved copy.
 *
 * Some care is required in the restoring of the node, but otherwise simply
 * copy the context.
 *
 * Note that the current "tos" is always the same, so restore copies the saved
 * copy to it, discarding the paths in the tos.
 *
 * Note that we do not need to worry about the config symbol of power, see
 *
 */
extern cmd_context
cmd_context_restore(cmd_context tos, cmd_context saved)
{
  node_type_t new_node ;

  qpath_free(tos->dir_cd) ;
  qpath_free(tos->dir_here) ;

  new_node = tos->node ;

  *tos = *saved ;       /* copy everything from saved value     */

  XFREE(MTYPE_CMD_EXEC, saved) ;

  tos->node = tos->cnode = cmd_node_restore(tos->node, new_node) ;

  return tos ;
} ;

/*------------------------------------------------------------------------------
 * Copy the given context to the given context -- creating a new one if
 * required.
 *
 * Note that copying to an existing context copies the values of the various
 * qpaths -- the copied to context is responsible for those.
 */
extern cmd_context
cmd_context_copy(cmd_context dst, cmd_context src)
{
  cmd_context_t tmp[1] ;

  if (dst == NULL)
    dst = cmd_context_new() ;

  *tmp = *dst ;         /* to keep existing qpath objects       */
  *dst = *src ;         /* copy everything (else)               */

  dst->dir_cd   = qpath_copy(tmp->dir_cd,   src->dir_cd) ;
  dst->dir_here = qpath_copy(tmp->dir_here, src->dir_here) ;

  return dst ;
} ;

/*------------------------------------------------------------------------------
 * Free the given context.
 */
extern cmd_context
cmd_context_free(cmd_context context)
{
  if (context != NULL)
    {
      qpath_free(context->dir_cd) ;
      qpath_free(context->dir_here) ;

      XFREE(MTYPE_CMD_EXEC, context) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Destroy cmd_exec object.
 */
extern cmd_exec
cmd_exec_free(cmd_exec exec)
{
  if (exec != NULL)
    {
      exec->vty     = NULL ;    /* no longer required.  */
      exec->context = NULL ;    /* ditto                */
      exec->parsed  = cmd_parsed_free(exec->parsed) ;

      if    (vty_nexus)
        exec->cq.mqb = mqb_free(exec->cq.mqb) ;
      else if (exec->cq.thread != NULL)
        {
          thread_cancel(exec->cq.thread) ;
          exec->cq.thread = NULL ;
        } ;

      XFREE(MTYPE_CMD_EXEC, exec) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 *
 */
/*------------------------------------------------------------------------------
 * Read configuration from file -- vio->blocking command loop.
 *
 * In the qpthreads world this assumes that it is running either in the only
 * pthread (in fact, the CLI pthread), early in the morning, or in the command
 * pthread, following SIGHUP.  So, all commands are to be executed directly.
 *
 * If the 'first_cmd' argument is not NULL it is the address of the first
 * command that is expected to appear.  If the first command is not this, then
 * the 'first_cmd' is called with argc == -1 (and argv == NULL) to signal the
 * command is being invoked by default.
 *
 * Command processing continues while CMD_SUCCESS is returned by the command
 * parser and command execution.
 *
 * Returns: CMD_SUCCESS    -- all went swimmingly
 *          CMD_WARNING    -- at least one CMD_WARNING returned, but nothing
 *                            more serious
 *          CMD_ERROR      -- hit an error of some kind, including I/O and
 *                            parsing errors
 *
 *          when returns, the entire vin/vout stack will have been closed,
 *          except for the vout.
 */
extern cmd_ret_t
cmd_read_config(vty vty, uint* warnings)
{
  cmd_context context ;
  cmd_parsed  parsed ;
  cmd_ret_t   ret, hret ;

  qassert((vty->type == VTY_STDOUT) && (vty->vio->blocking)) ;

  VTY_LOCK() ;

  qassert(vty->vio->cl_state == vcl_stopped) ;
  vty->vio->cl_state = vcl_running ;

  VTY_UNLOCK() ;

  /* Deal with the possibility that while reading the configuration file, may
   * use a pipe, and therefore may block waiting to collect a child process.
   *
   * Before there is any chance of a SIGCHLD being raised for a child of the
   * configuration file, invoke the magic required for SIGCHLD to wake up
   * a pselect() while waiting to collect child.
   *
   * NB: we have to clear this again before returning.
   */
  vty_child_signal_nexus_set(vty) ;

  /* Note that we have a single parser object for execution.
   *
   * Also, the context object is kept up to date as pipes are pushed and
   * popped -- so the pointer to it remains valid.
   */
  context = vty->exec->context ;
  parsed  = vty->exec->parsed ;

  qassert(parsed  != NULL) ;
  qassert(context != NULL) ;

  ret       = CMD_SUCCESS ;     /* so far, so good              */
  *warnings = 0 ;               /* no warnings seen             */

  host.first_config_cmd = true ;

  while (1)
    {
      /* Deal with anything which is not success !
       */
      if (ret != CMD_SUCCESS)
        {
          /* If is CMD_WARNING, drop out of the loop if context->warn_stop.
           *
           * Otherwise, drop out of the loop if have anything other than
           * CMD_HIATUS or CMD_WAITING -- the later means that some output
           * is pending, and we don't start a new command in that state.
           *
           * Everything else is deemed to be an error that stops the command
           * loop.
           */
          if (ret == CMD_WARNING)
            {
              if (context->warn_stop)
                break ;
            }
          else
            {
              if ((ret != CMD_HIATUS) && (ret != CMD_WAITING))
                break ;
            } ;

          ret = vty_cmd_hiatus(vty, ret) ;

          if (ret == CMD_STOP)          /* the expected outcome !       */
            break ;

          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* All is well, need another command line
       */
      ret = vty_cmd_line_fetch(vty) ;   /* sets exec->action    */

      if (ret != CMD_SUCCESS)
        continue ;

      /* Parse the command line we now have -- loop if failed or there is
       * nothing to execute.
       */
      qassert(context->to_do == cmd_do_command) ;

      cmd_tokenize(parsed, context->line, context->full_lex);
      ret = cmd_parse_command(parsed, context) ;

      if ( (ret != CMD_SUCCESS) || ((parsed->parts & cmd_parts_execute) == 0) )
        continue ;

      /* reflection now.....
       */
      if (context->reflect)
        {
          ret = vty_cmd_reflect_line(vty) ;
          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* Pipe work, if any
       */
      if ((parsed->parts & cmd_parts_pipe) != 0)
        {
          ret = cmd_open_pipes(vty) ;
          if ((ret != CMD_SUCCESS) || ((parsed->parts & cmd_part_command) == 0))
            continue ;
        } ;

      qassert((parsed->parts & cmd_part_command) != 0) ;

      /* Special handling while host.first_config_cmd.
       *
       * When the configuration file is read the first time, there are some
       * commands which may be executed before the daemon is fully initialised.
       * Most meta commands are in this category, also (and especially) the
       * "pthreads on" and related commands.
       *
       * As soon as we get a command which is *not* CMD_ATTR_FIRST, we turn
       * off the host.first_config_cmd, and if is host.newborn, and there is
       * a "init_second_stage" function, invoke that.
       */
      if (host.first_config_cmd && ((parsed->cmd->attr & CMD_ATTR_FIRST) == 0))
        cmd_init_second_stage() ;

      /* Command execution
       */
      ret = cmd_execute(vty) ;

      if (ret == CMD_WARNING)
        ++(*warnings) ;         /* count all warnings           */

      /* Deal with success or suppressed warning.
       */
      ret = vty_cmd_complete(vty, ret) ;
    } ;

  /* Arrives here if:
   *
   *   - vty_cmd_line_fetch() returns anything except CMD_SUCCESS or CMD_HIATUS,
   *     which are not errors.
   *
   *   - any other operation returns anything except CMD_SUCCESS or CMD_HIATUS
   *     (or CMD_WARNING, if they are being ignored)
   *
   *   - hiatus returns CMD_STOP -- the expected result.
   *
   * Deal with any errors -- generate suitable error messages and close back
   * to (but excluding) vout_base.
   *
   * CMD_WAITING is not valid for blocking vio !
   */
  qassert(ret != CMD_WAITING) ;

  hret = ret ;
  while ((hret != CMD_STOP) && (hret != CMD_SUCCESS))
    hret = vty_cmd_hiatus(vty, hret) ;

  /* Finished with all children
   */
  vty_child_signal_nexus_clear(vty) ;

  /* Return CMD_SUCCESS/CMD_WARNING/CMD_ERROR
   */
  if (ret == CMD_WARNING)
    return CMD_WARNING ;

  if ((ret != CMD_SUCCESS) && (ret != CMD_STOP))
    return CMD_ERROR ;

  return (*warnings == 0) ? CMD_SUCCESS : CMD_WARNING ;
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
extern cmd_ret_t
cmd_open_pipes(vty vty)
{
  cmd_exec   exec ;
  cmd_parsed parsed ;
  cmd_ret_t  ret ;
  vty_io     vio ;
  bool       together ;

  VTY_LOCK() ;

  vio    = vty->vio ;
  exec   = vty->exec ;
  parsed = exec->parsed ;

  ret = CMD_SUCCESS ;

  /* We need to know whether this is opening/pushing a vin and a vout together,
   * or just one at a time.
   *
   * This affects the handling of out_ordinary, because opening a vout
   * implicitly sets out_ordinary *except* when opened together with a vin
   * which explicitly clears out_ordinary.
   *
   * Also, the out_depth setting depends on this.
   *
   * Note that it is assumed elsewhere that when opened together the vin is
   * opened before the vout, and when closed the vin is also closed first.
   *
   * Note that it is always possible that the output open could fail... so
   * anything that really depends on the success of that should be delayed
   * until then.
   */
  together = false ;

  /* Deal with any in pipe stuff
   */
  if ((parsed->parts & cmd_part_in_pipe) != 0)
    {
      qstring args ;

      together = ((parsed->parts & cmd_part_out_pipe) != 0) ;

      args = cmd_tokens_concat(parsed, parsed->first_action,
                                       parsed->num_action) ;

      if      ((parsed->in_pipe & cmd_pipe_file) != 0)
        ret = uty_cmd_open_in_pipe_file(vio, exec->context, args,
                                                    parsed->in_pipe, together) ;
      else if ((parsed->in_pipe & cmd_pipe_shell) != 0)
        ret = uty_cmd_open_in_pipe_shell(vio, exec->context, args,
                                                    parsed->in_pipe, together) ;
      else
        zabort("invalid in pipe state") ;

      qs_free(args) ;
    } ;

  /* Deal with any out pipe stuff
   */
  if (((parsed->parts & cmd_part_out_pipe) != 0) && (ret == CMD_SUCCESS))
    {
      qstring args ;

      args = cmd_tokens_concat(parsed, parsed->first_out_pipe,
                                       parsed->num_out_pipe) ;

      if      ((parsed->out_pipe & cmd_pipe_file) != 0)
        ret = uty_cmd_open_out_pipe_file(vio, exec->context, args,
                                                   parsed->out_pipe, together) ;
      else if ((parsed->out_pipe & cmd_pipe_shell) != 0)
        ret = uty_cmd_open_out_pipe_shell(vio, exec->context, args,
                                                   parsed->out_pipe, together) ;
      else if ((parsed->out_pipe & cmd_pipe_dev_null) != 0)
        ret = uty_cmd_open_out_dev_null(vio, together) ;
      else
        zabort("invalid out pipe state") ;

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
 * Returns: CMD_SUCCESS   -- it all went very well
 *          CMD_WARNING   -- not so good: warning message sent by vty_out()
 *          CMD_ERROR     -- not so good: error   message sent by vty_out()
 *
 *          CMD_IO_ERROR  -- not so good: error   message posted to vio->ebuf
 *          CMD_CANCEL    -- not so good: collected a CANCEL
 *
 *          The above are the only valid returns from a command.
 *
 *          CMD_IO_ERROR and CMD_CANCEL may be returned if the command does
 *          vty_out_push().
 *
 * If get CMD_SUCCESS, then will update the context->node to the parsed->nnode.
 * So all commands which change node state are known to the parser, which also
 * knows what the next node will be, assuming success.
 *
 * EXIT_NODE is set by various forms of "exit".  When the next command line
 * is fetched vty_cmd_line_fetch() will set the input to "eof".  At any stack
 * level above the base "exit" means leave the current file/pipe/etc and return
 * to the parent -- which restores the previous context, including its node.
 *
 * Note that any "exit" goes through the usual command complete process before
 * any file/pipe/etc closing happens.
 *
 * NB: the distinction between CMD_WARNING and CMD_ERROR is that CMD_WARNING
 *     may be ignored.
 *
 * NB: no other returns are acceptable !
 */
extern cmd_ret_t
cmd_execute(vty vty)
{
  cmd_context  context = vty->exec->context ;

  cmd_ret_t ret ;

  if (context->parse_only)
    ret = CMD_SUCCESS ;
  else
    {
      cmd_parsed  parsed = vty->exec->parsed ;
      cmd_command cmd    = parsed->cmd ;

      /* Set the vty->node in case the command cares, and then (finally) do
       * the command.
       *
       * Note that the command is expected to return only one of: CMD_SUCCESS,
       * CMD_WARNING or CMD_ERROR.
       *
       * If the command is a node changer, then if it returns CMD_SUCCESS, then
       * change to the node as established by the parser.
       */
      vty->node = parsed->xnode ;

      ret = (*(cmd->func))(cmd, vty, cmd_arg_vector_argc(parsed),
                                     cmd_arg_vector_argv(parsed)) ;
      if (ret == CMD_SUCCESS)
        {
          /* If the node is changed by the command, do that now and make sure
           * that the configuration symbol of power is straight.
           *
           * If the new node requires the symbol of power, then MUST already
           * have acquired it (otherwise the command would have failed !)
           * (Will get a CMD_ERROR at this late stage, if not !)
           *
           * If the new node does not require the symbol of power, then we will
           * here release it iff we are at the vin_base !
           */
          if (context->node != parsed->nnode)
            {
              ret = vty_cmd_config_lock(vty, parsed->nnode) ;
              if (ret == CMD_SUCCESS)
                context->node = context->cnode = parsed->nnode ;
            } ;

          /* The vty->node is not used within the command handling, the
           * context->node is the value that actually matters.  (Except while
           * a vty is being initialised, when vty->node is the initial node.)
           *
           * Commands which change node (e.g. "address-family ipv4") no longer
           * need to change vty->node.  But if they do, it had better be to
           * the expected next node -- for if not, something is out of kilter.
           */
          qassert( (vty->node == parsed->xnode) ||
                   (vty->node == parsed->nnode) ) ;
        }
      else
        {
          /* Enforce restrictions on return codes.                          */
          qassert( (ret == CMD_WARNING) || (ret == CMD_ERROR)
                                        || (ret == CMD_IO_ERROR)
                                        || (ret == CMD_CANCEL) ) ;
        } ;
    } ;

  /* Return result
   */
  return ret ;
} ;

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
 * The following are set by uty_cmd_prepare() which is called after something
 * has been pushed/popped on the vin/vout stacks, and before any command
 * execution starts:
 *
 *   - parse_type
 *   - reflect_enabled
 *   - out_enabled
 *
 * to reflect the then current VTY state.
 */
extern cmd_exec
cmd_exec_new(vty vty)
{
  cmd_exec exec ;

  exec = XCALLOC(MTYPE_CMD_EXEC, sizeof(struct cmd_exec)) ;

  /* Zeroising has set:
   *
   *   vty                 = X       -- set below
   *
   *   line                = NULL    -- no command line, yet
   *   to_do               = cmd_do_nothing
   *
   *   parse_type          = cmd_parse_standard
   *
   *   reflect_enabled     = false   -- not enabled
   *   out_enabled         = false   -- not enabled
   *
   *   parsed              all zeros -- empty parsed object (embedded)
   *
   *   state               = exec_null
   *   locus               = NULL    -- not significant in exec_null
   *   ret                 = CMD_SUCCESS
   *
   *   cq                  = NULL    -- no mqb (qpthreads)
   *                                 -- no thread (legacy thread)
   */
  confirm(cmd_do_nothing == 0) ;
  confirm(cmd_parse_standard == 0) ;
  confirm(CMD_PARSED_INIT_ALL_ZEROS) ;
  confirm(exec_null == 0) ;
  confirm(CMD_SUCCESS == 0) ;

  exec->vty        = vty ;

  return exec ;
} ;

/*------------------------------------------------------------------------------
 * Destroy cmd_exec object.
 */
extern cmd_exec
cmd_exec_free(cmd_exec exec)
{
  cmd_parsed_reset(exec->parsed, keep_it) ;



  XFREE(MTYPE_CMD_EXEC, exec) ;

  return NULL ;
} ;


/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Set new node.
 *
 * If old node >= CONFIG_NODE, and new node < CONFIG_NODE, give up the config
 * symbol of power.
 *
 * Returns: CMD_SUCCESS -- OK
 *          CMD_CLOSE   -- if new node == NODE_NULL
 */
static cmd_return_code_t
cmd_set_node(vty vty, node_type_t node)
{
  if ((vty->node >= MIN_CONFIG_NODE) && (node < MIN_CONFIG_NODE))
    vty_config_unlock(vty, node) ;
  else
    vty->node = node ;

  return (vty->node != NULL_NODE) ? CMD_SUCCESS : CMD_CLOSE ;
} ;

/*------------------------------------------------------------------------------
 * Command line "end" command
 *
 * Falls back to the current node's end_to node.  If leaves configuration
 * mode, give away the configuration symbol of power.
 *
 * Generally, for all configuration nodes end -> NODE_ENABLE (releasing the
 * configuration lock), and all other nodes end does nothing.
 *
 * Returns: CMD_SUCCESS -- OK
 *          CMD_CLOSE   -- if new node == NODE_NULL
 */
extern cmd_return_code_t
cmd_end(vty vty)
{
  return cmd_set_node(vty, cmd_node_end_to(vty->node)) ;
} ;

/*------------------------------------------------------------------------------
 * Command line "exit" command -- aka "quit"
 *
 * Falls back to the current node's exit_to node.  If leaves configuration
 * mode, give away the configuration symbol of power.
 *
 * Generally:
 *
 *   - for all configuration nodes > NODE_CONFIG exit -> parent node.
 *
 *   - for NODE_CONFIG exit -> ENABLE_NODE (and release configuration symbol
 *     of power)
 *
 *   - for all nodes < NODE_CONFIG -> close the VTY
 *
 * Returns: CMD_SUCCESS -- OK
 *          CMD_CLOSE   -- if new node == NODE_NULL
 */
extern cmd_return_code_t
cmd_exit(vty vty)
{
  return cmd_set_node(vty, cmd_node_exit_to(vty->node)) ;
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
 * Returns: cmd_return_code for last command
 *          vty->buf     is last line processed
 *          vty->lineno  is number of last line processed (1 is first)
 *
 * If the file is empty, will return CMD_SUCCESS.
 *
 * If
 *
 * If return code is not CMD_SUCCESS, the the output buffering contains the
 * output from the last command attempted.
 */
extern cmd_return_code_t
cmd_read_config(struct vty *vty, cmd_command first_cmd, bool ignore_warning)
{
  cmd_exec          exec   = vty->exec ;
  cmd_parsed        parsed = exec->parsed ;
  cmd_return_code_t ret;

  while (1)
    {
      /* Need a command line, pops pipes as required                    */
      ret = vty_cmd_fetch_line(vty) ;   /* sets exec->line              */

      if (ret != CMD_SUCCESS)
        break ;                 /* stop on any and all problems         */

      /* Parse the command line we now have                             */
      cmd_tokenise(parsed, exec->line) ;
      ret = cmd_parse_command(parsed, vty->node,
                                       exec->parse_type | cmd_parse_execution) ;

      if (ret == CMD_EMPTY)
        continue ;              /* easy if empty line                   */

      if (ret != CMD_SUCCESS)
        break ;                 /* stop on *any* parsing issue          */

      /* Special handling before first active line.                     */
      if (first_cmd != NULL)
        {
          if (first_cmd != parsed->cmd)
            {
              ret = (*first_cmd->func)(first_cmd, vty, 0, NULL) ;
              if (ret != CMD_SUCCESS)
                break ;         /* stop on *any* issue with "default"   */
            } ;
          first_cmd = NULL ;
        } ;

      /* reflection now.....                                            */
      if (exec->reflect_enabled)
        vty_cmd_reflect_line(vty) ;

      /* Pipe work, if any                                              */
      if ((parsed->parts & cmd_parts_pipe) != 0)
        {
          ret = cmd_open_pipes(vty) ;
          if (ret != CMD_SUCCESS)
            break ;
        } ;

      /* Command execution, if any                                      */
      if ((parsed->parts & cmd_part_command) != 0)
        {
          ret = cmd_execute(vty) ;

          if (ret != CMD_SUCCESS)
            {
              /* If ignoring warnings, treat CMD_WARNING as CMD_SUCCESS */
              if      (ignore_warning && (ret == CMD_WARNING))
                ret = CMD_SUCCESS ;

              /* Treat CMD_CLOSE as CMD_SUCCESS                         */
              else if (ret == CMD_CLOSE)
                ret = CMD_SUCCESS ;

              /* Everything else -> stop                                */
              else
                break ;
            } ;
        } ;

      /* When we get here the last command was CMD_SUCCESS !
       * (Or CMD_WARNING and ignore_warning.)
       *
       * Deals with closing out pipe(s) if required.
       */
      vty_cmd_success(vty) ;
    } ;

  /* Deal with any errors                                               */
  if (ret == CMD_EOF)
    return CMD_SUCCESS ;

  return ret ;
} ;

/*==============================================================================
 */
static qstring cmd_get_pipe_file_name(cmd_parsed parsed, uint ti, uint nt) ;


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

  ret = CMD_SUCCESS ;

  /* Deal with any in pipe stuff                                        */
  if ((parsed->parts & cmd_part_in_pipe) != 0)
    {
      if      ((parsed->in_pipe & cmd_pipe_file) != 0)
        {
          qstring name ;
          name = cmd_get_pipe_file_name(parsed, parsed->first_in_pipe,
                                                parsed->num_in_pipe) ;

          ret = vty_cmd_open_in_pipe_file(vty, name,
                                    (parsed->in_pipe & cmd_pipe_reflect) != 0) ;

          qs_reset(name, free_it) ;
        }
      else if ((parsed->in_pipe & cmd_pipe_shell) != 0)
        {
        }
      else
        zabort("invalid in pipe state") ;

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* Deal with any out pipe stuff                                       */
  if ((parsed->parts & cmd_part_out_pipe) != 0)
    {
      if      ((parsed->out_pipe & cmd_pipe_file) != 0)
        {
          qstring name ;
          name = cmd_get_pipe_file_name(parsed, parsed->first_out_pipe,
                                                parsed->num_out_pipe) ;

          ret = vty_cmd_open_out_pipe_file(vty, name,
                                  ((parsed->out_pipe & cmd_pipe_append) != 0)) ;

          qs_reset(name, free_it) ;
        }
      else if ((parsed->out_pipe & cmd_pipe_shell) != 0)
        {
          ret = vty_cmd_open_out_dev_null(vty) ;
        }
      else if ((parsed->out_pipe & cmd_pipe_dev_null) != 0)
        {
          ret = vty_cmd_open_out_dev_null(vty) ;
        }
      else
        zabort("invalid out pipe state") ;

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Get pipe file name
 *
 * Returns a brand new qstring that must be discarded after use.
 *
 * Pro tem this just gets the token value !!  TODO
 */
static qstring
cmd_get_pipe_file_name(cmd_parsed parsed, uint ti, uint nt)
{
  cmd_token t ;

  assert(nt == 2) ;

  t = cmd_token_get(parsed->tokens, ti + 1) ;

  return qs_copy(NULL, t->qs) ;
} ;

/*------------------------------------------------------------------------------
 * Command Execution
 *
 * Returns:
 *
 *   - have command ready for execution  -- CMD_SUCCESS
 *   - reached end of command stream     -- CMD_CLOSE
 *   - encounter error of some kind      -- CMD_WARNING, CMD_ERROR, etc
 */
extern cmd_return_code_t
cmd_execute(vty vty)
{
  cmd_parsed  parsed = vty->exec->parsed ;
  cmd_command cmd    = parsed->cmd ;
  cmd_return_code_t ret ;
  node_type_t       onode ;

  onode     = vty->node ;
  vty->node = parsed->cnode ;

  ret = (*(cmd->func))(cmd, vty, cmd_arg_vector_argc(parsed),
                                 cmd_arg_vector_argv(parsed)) ;

  if (((parsed->parts & cmd_part_do) != 0) && (vty->node == ENABLE_NODE))
    vty->node = onode ;

  return ret ;
} ;

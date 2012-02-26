/* VTY IO SHELL -- VTY Shell Command Execution
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
#include "version.h"

#include "vty_vtysh.h"
#include "vty_io_vtysh.h"
#include "vty_common.h"
#include "vty_local.h"
#include "vty_io.h"
#include "vty_command.h"
#include "command_parse.h"
#include "qstring.h"
#include "list_util.h"
#include "qpthreads.h"
#include "pthread_safe.h"

/*==============================================================================
 * Support for the vtysh own vty -- VTY_VTYSH
 *
 * Here we have the mechanics of executing commands in all the connected
 * daemons and in the vtysh itself, collecting the results, dealing with
 * errors and outputting the results.
 */

/* Callbacks set up when the vtysh's own vty is initialised.
 */
vtysh_cmd_call_backs_t* vtysh_cmd_call_backs = NULL ;

/*------------------------------------------------------------------------------
 * Create a new VTY_VTYSH vty -- starting in ENABLE_NODE
 *
 * This is for the single-threaded vtysh *only*.
 *
 * Set the client_list empty.
 *
 * Sets VIN_VTYSH, which always returns CMD_WAITING when any attempt is made to
 * read from it...  this causes the vty_vtysh_command_loop() to exit, without
 * closing the vty.
 *
 * NB: initially the VTY_VTYSH is set with a VOUT_STDOUT.
 *
 *     While is VOUT_STDOUT, vty_err() is directed to stderr.
 *
 *     For -c commands and interactive mode that is "promoted" to VOUT_VTYSH,
 *     which is similar, but understands the vtysh "--more--" handling.
 */
extern vty
vty_vtysh_open(vtysh_cmd_call_backs_t* call_backs, bool no_prefix)
{
  daemon_ord_t ord ;
  vty     vtysh ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_LOCK() ;

  /* Set the call_backs -- link library back to the vtysh itself (dynamically).
   */
  vtysh_cmd_call_backs = call_backs ;

  /* No clients opened, yet.
   */
  vtysh_open_clients = 0 ;
  vtysh_self->next = NULL ;

  vtysh_self->version = qs_set_str(NULL, QUAGGA_VERSION) ;

  for (ord = 0 ; ord < DAEMON_COUNT ; ++ord)
    {
      vtysh_clients[ord].fd = -1 ;      /* make sure    */

      qassert(vtysh_clients[ord].daemon == (1u << ord)) ;
    } ;

  /* Prepare the vty and vio.  NB: sets vio->blocking.
   */
  vtysh = uty_new(VTY_VTYSH, ENABLE_NODE) ;
  vio   = vtysh->vio ;

  /* Complete the initialisation of the vty_io object.
   *
   * Note that the defaults for:
   *
   *   - read_timeout     -- default = 0     => no timeout
   *   - write_timeout    -- default = 0     => no timeout
   *
   * are fine.
   *
   * The name of this vty may be ambiguous.  TODO -- add username and PID ??
   */
  vf = uty_vf_new(vio, "vtysh", -1, vfd_none, vfd_io_none) ;

  uty_vin_push( vio, vf, VIN_VTYSH, NULL, 0) ;  /* ibuf not required    */
  uty_vout_push(vio, vf, VOUT_STDOUT, NULL,
                             file_buffer_size,  /* obuf is required     */
                                        true) ; /* after buddy vin      */

  vf->no_prefix     = no_prefix ;
  vf->push_complete = true ;            /* to set vst_cmd_complete      */

  vf->r_obuf = vio_fifo_new(file_buffer_size) ;   /* results from client  */
  vf->r_ebuf = vio_fifo_new(pipe_buffer_size) ;   /* errors from clients  */

  vio_fifo_set_end_mark(vf->r_obuf) ;     /* has end_mark, same like obuf */

  /* Prepare exec and context etc. for the vtysh command loop.
   */
  uty_cmd_vtysh_prepare(vio) ;

  vio->cl_state = vcl_running ;

  VTY_UNLOCK() ;

  return vtysh ;
} ;

/*------------------------------------------------------------------------------
 * Promote given vty to VOUT_VTYSH.
 *
 * This is for the single-threaded vtysh *only*.
 *
 * This is done once the vtysh starts to execute -c or in interactive mode.
 */
extern void
vty_vtysh_promote(vty vtysh)
{
  vty_io  vio ;

  VTY_LOCK() ;

  vio = vtysh->vio ;

  /* We start the process at the base level vin/vout.  We treat the initial
   * state as executing a phantom "start" command -- so we are now
   * vst_cmd_running_executing.
   */
  qassert(vtysh->type == VTY_VTYSH) ;
  qassert((vio->vin_depth  == 1) && (vio->vin->vin_type  == VIN_VTYSH)) ;
  qassert((vio->vout_depth == 1) && (vio->vin->vout_type == VOUT_STDOUT)) ;

  vtysh->vio->state = vst_cmd_running_executing ;

  vio->vout->vout_type = VOUT_VTYSH ;   /* simple, really !             */
  vio->vout->push_complete = true ;     /* small, but important         */
  uty_vtysh_out_prep(vio, NULL) ;       /* make sure all straight       */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Open the given daemons.
 *
 * Issues error message(s) by vty_err().
 *
 * For the required set of daemons, issue error message if is unable to open
 * because daemon is not running.
 */
extern daemon_set_t
vty_vtysh_open_clients(vty vtysh, daemon_set_t daemons, daemon_set_t required)
{
  daemon_set_t opened ;
  daemon_ord_t ord ;
  vtysh_client client ;
  uint         name_len ;

  name_len = strlen(vtysh_self->name) ;

  /* Try to open all the required daemons, and rebuild the open clients list.
   */
  opened = 0 ;

  for (ord = 0 ; ord < DAEMON_COUNT ; ++ord)
    {
      client = &vtysh_clients[ord] ;

      if (daemons & client->daemon)
        {
          cmd_ret_t ret ;

          ret = vtysh_client_open(vtysh, client) ;

          if (ret == CMD_SUCCESS)
            opened |= client->daemon ;

          if (ret != CMD_WARNING)
            required &= ~client->daemon ;
        } ;

      if (client->fd >= 0)
        {
          uint len ;

          qassert((vtysh_open_clients & client->daemon) != 0) ;

          len = strlen(client->name) ;
          if (len > name_len)
            name_len = len ;
        }
      else
        {
          qassert((vtysh_open_clients & client->daemon) == 0) ;
        } ;
    } ;

  /* Report and daemons that were required, but were not available (and did
   * not fail during open).
   */
  required &= daemons ;

  if (required != 0)
    {
      qstring list ;

      list = cmd_daemons_make_list(NULL, required & daemons) ;

      vty_err(vtysh, "%% did not connect to: %s -- apparently not running\n",
                                                              qs_string(list)) ;
      qs_free(list) ;
    } ;

  /* Update the prefix strings for vtysh and all open daemons.
   *
   * Prefix is: "[<name>] ".
   */
  enum { vtysh_max_name_len = vtysh_max_prefix_len - 3 } ;

  qassert(name_len <= vtysh_max_name_len) ;

  if (name_len > vtysh_max_name_len)
    name_len = vtysh_max_name_len ;

  client = vtysh_self ;
  do
    {
      ulen len ;
      char* p, * e ;

      len = strlen(client->name) ;

      qassert(len <= name_len) ;
      if (len > name_len)
        len = name_len ;

      p = client->prefix ;
      *p++ = '[' ;

      if (len > 0)
        memcpy(p, client->name, len) ;

      e = p + name_len ;
      p += len ;

      while (p < e)
        *p++ = ' ' ;

      *p++ = ']' ;
      *p++ = ' ' ;
      *p   = '\0' ;

      client = client->next ;
    }
  while (client != NULL) ;

  /* Return what was opened
   */
  return opened ;
} ;

/*------------------------------------------------------------------------------
 * Scan the clients to check that is still connected to all the daemons
 * thought to be connected.
 *
 * Issue, by vty_err(), error messages for any lost connections.
 *
 * Returns:  set of daemons seem to be connected to
 */
extern daemon_set_t
vty_vtysh_check_clients(vty vtysh)
{
  daemon_ord_t ord ;
  daemon_set_t found_ok ;

  found_ok = 0 ;

  for (ord = 0 ; ord < DAEMON_COUNT ; ++ord)
    {
      vtysh_client client ;

      client = &vtysh_clients[ord] ;

      if (client->fd >= 0)
        {
          char buf[1] ;
          int  ret ;

          ret = write(client->fd, buf, 0) ;
          if (ret >= 0)
            found_ok |= client->daemon ;
          else
            {
              int err = errno ;

              if (err == EPIPE)
                vty_err(vtysh, "%% lost connection with %s\n", client->name) ;
              else
                vty_err(vtysh, "%% %s failed: %s\n", client->name,
                                                           errtoa(err, 0).str) ;
              vtysh_client_close(client) ;
            } ;
        } ;
    } ;

  qassert(found_ok == vtysh_open_clients) ;

  return found_ok ;
} ;

/*------------------------------------------------------------------------------
 * Close the open daemons -- just before exit from vtysh
 */
extern void
vty_vtysh_close(void)
{
  while (vtysh_self->next != NULL)
    vtysh_client_close(vtysh_self->next) ;

  vtysh_self->version = qs_free(vtysh_self->version) ;
} ;

/*==============================================================================
 * Execution of commands by sending to the various daemons to which the vtysh
 * has made a connection -- where the vtysh is the client and the daemons are
 * the servers.
 */
static cmd_ret_t vty_vtysh_execute(vty vtysh, bool interactive) ;
static cmd_ret_t vty_vtysh_client_execute(vty vtysh, vtysh_client client,
                                               vtysh_client_dispatch dispatch) ;
static cmd_ret_t vty_vtysh_complete(vty vtysh, cmd_ret_t ret,
                                               cmd_ret_t fret,
                                               const char* prefix) ;

/*------------------------------------------------------------------------------
 * Set the vin->cl to the given line, length and position.
 *
 * For safety, scans the incoming line to remove all control characters, mapping
 * them to ' '.
 */
extern qstring
vty_vtysh_prep_line(vty vtysh, const char* line, ulen len, ulen pos)
{
  qstring      qs ;
  vio_vf       vf ;

  const byte* p ;
  const byte* e ;
  byte*       q ;

  vf = vtysh->vio->vin ;

  qassert(vf->vin_type == VIN_VTYSH) ;

  qs = qs_new_size(vf->cl, len) ;
  qs_set_len_nn(qs, len) ;
  qs_set_cp_nn(qs, pos) ;

  p = (const byte*)line ;
  e = p + len ;
  q = (byte*)qs_char_nn(qs) ;

  while (p < e)
    {
      if (*p < 0x20)
        *q++ = ' ' ;
      else
        *q++ = *p ;
      ++p ;
    } ;

  *q = '\0' ;

  return (vf->cl = qs) ;
} ;

/*------------------------------------------------------------------------------
 * Execute command loop for vtysh
 *
 * If entered with not NULL "line", prepare and execute that.  Then continue
 * executing while there is any command pipe to be read, and no error is
 * encountered.
 *
 * If is "interactive", then it is a CMD_ERROR if the command is not recognised
 * by at least one connected daemon or by the vtysh itself.
 *
 * If is "ignore_warnings", then any CMD_WARNING returned is treated as
 * CMD_SUCCESS.
 *
 * The output from the command may be suppressed (for config file reading).
 *
 * Command line reflection is also supported.
 *
 * When returns, will be at the vtysh base.  All output will have been pushed,
 * including any error messages.
 *
 * Returns:  CMD_SUCCESS  => OK
 *           CMD_STOP     => command was "exit"
 *           CMD_WARNING  => command failed with CMD_WARNING
 *           CMD_ERROR    => command failed with CMD_ERROR
 */
extern cmd_ret_t
vty_vtysh_command_loop(vty vtysh, const char* line, bool interactive,
                                                                ulen prompt_len)
{
  cmd_ret_t   ret, hret ;
  cmd_context context ;
  cmd_parsed  parsed ;
  bool        in_hand ;

  qassert(vtysh->type == VTY_VTYSH) ;

  /* Note that we have a single parser object for execution.
   *
   * Also, the context object is kept up to date as pipes are pushed and
   * popped -- so the pointer to it remains valid.
   */
  context = vtysh->exec->context ;
  parsed  = vtysh->exec->parsed ;

  qassert(parsed  != NULL) ;
  qassert(context != NULL) ;

  /* Do we have a line in hand ?
   */
  in_hand = (line != NULL) ;

  if (in_hand)
    {
      context->to_do = cmd_do_command ;
      context->line  = vty_vtysh_prep_line(vtysh, line, strlen(line), 0) ;
    }
  else
    prompt_len = 0 ;

  context->parse_execution = true ;

  vtysh->vio->prompt_len = prompt_len ;

  /* The command loop -- this is similar to the config reader loop -- the
   * VTY_VTYSH is also a "blocking" vty.
   *
   * Note that in the command loop the exec->context will change as pipes are
   * opened and closed.
   */
  vtysh->vio->cl_state = vcl_running ;
  vtysh->vio->state    = vst_cmd_running_executing ;

  ret = CMD_SUCCESS ;

  uty_cmd_prepare(vtysh->vio) ;

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

          ret = vty_cmd_hiatus(vtysh, ret) ;

          if (ret == CMD_STOP)
            break ;

          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* All is well, need another command line -- unless have one in_hand,
       * which we now clear.
       *
       * If we are back to the vtysh "base", then this will return CMD_SUCCESS,
       * and we need to check the state to see if it is now time to exit.
       */
      if (in_hand)
        {
          in_hand = false ;
          qassert(vtysh->vio->state == vst_cmd_running_executing) ;
        }
      else
        {
          vtysh->vio->prompt_len = 0 ;  /* no longer relevant           */

          ret = vty_cmd_line_fetch(vtysh) ;

          if (ret != CMD_SUCCESS)
            continue ;                  /* let the hiatus cope          */

          if ((vtysh->vio->vin_depth == 1)
                                        && (vtysh->vio->state == vst_cmd_fetch))
            return CMD_SUCCESS ;
        } ;

      /* Parse the command line we now have -- loop if failed or there is
       * nothing to execute.
       */
      qassert(context->to_do == cmd_do_command) ;

      cmd_tokenize(parsed, context->line, context->full_lex);
      ret = cmd_parse_command(parsed, context) ;

      if ( (ret != CMD_SUCCESS) || ((parsed->parts & cmd_parts_execute) == 0) )
        continue ;

      /* reflection now..... sets vio->prompt_len if reflects line.
       */
      if (context->reflect)
        {
          ret = vty_cmd_reflect_line(vtysh) ;

          qassert(ret != CMD_WAITING) ;

          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* Pipe work, if any
       */
      if ((parsed->parts & cmd_parts_pipe) != 0)
        {
          ret = cmd_open_pipes(vtysh) ;
          if (ret != CMD_SUCCESS)
            continue ;
        } ;

      /* Command execution
       */
      if (parsed->parts & cmd_part_command)
        ret = vty_vtysh_execute(vtysh, interactive) ;

      /* Deal with result.
       */
      ret = vty_cmd_complete(vtysh, ret) ;
    } ;

  /* Arrives here if:
   *
   *   - vty_cmd_line_fetch() returns anything except CMD_SUCCESS, CMD_HIATUS
   *     or CMD_WAITING -- which are not errors.
   *
   *   - any other operation returns anything except CMD_SUCCESS or CMD_HIATUS
   *     (or CMD_WARNING, if they are being ignored)
   *
   *   - hiatus returns CMD_STOP -- the expected result.
   *
   * Deal with any errors -- generate suitable error messages and close back
   * to (but excluding) vout_base.
   *
   * CMD_WAITING is not valid for blocking vio !  vty_cmd_line_fetch() returns
   * it for vin_depth == 1, but it will have been dealt with, above.
   */
  qassert(ret != CMD_WAITING) ;

  hret = ret ;
  while ((hret != CMD_STOP) && (hret != CMD_SUCCESS))
    hret = vty_cmd_hiatus(vtysh, hret) ;

  qassert(vtysh->vio->vin_depth <= 1) ;         /* at base      */

  if (hret == CMD_STOP)
    return CMD_STOP ;

  if ((ret != CMD_SUCCESS) && (ret != CMD_STOP) && (ret != CMD_WARNING))
    return CMD_ERROR ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * For all currently active clients, ask for configuration, and then pass
 * (one at a time) to the given function.
 *
 * If "show", will output progress indicators and push same.
 */
extern cmd_ret_t
vty_vtysh_fetch_config(vty vtysh,
                          void (*collect)(daemon_set_t daemon, vio_fifo buf),
                                                                      bool show)
{
  vtysh_client_dispatch_t dispatch ;
  cmd_ret_t    ret ;
  daemon_set_t daemons ;
  daemon_ord_t ord ;
  vio_fifo     r_obuf ;

  dispatch->to_do  = cmd_do_command ;
  dispatch->line   = "#vtysh-config-write" ;
  dispatch->len    = strlen(dispatch->line) ;
  dispatch->cnode  = META_NODE ;
  dispatch->xnode  = vtysh->exec->context->node ;

  daemons = vtysh->exec->context->daemons & vtysh_open_clients ;

  r_obuf = vtysh->vio->vout_base->r_obuf ;

  ret = CMD_SUCCESS ;

  for (ord = 0 ; ord < DAEMON_COUNT ; ++ord)
    {
      vtysh_client client ;

      client = &vtysh_clients[ord] ;
      if ((daemons & client->daemon) == 0)
        continue ;

      if (show)
        {
          vty_out(vtysh, "Collecting configuration from %s", client->name) ;
          ret = vty_cmd_out_push(vtysh) ;

          if (ret != CMD_SUCCESS)
            break ;
        } ;

      ret = vty_vtysh_client_execute(vtysh, client, dispatch) ;
      if (ret == CMD_SUCCESS)
        {
          vio_fifo_step_end_mark(r_obuf) ;
          collect(client->daemon, r_obuf) ;
        } ;

      if (show)
        {
          if (ret == CMD_SUCCESS)
            {
              vty_out(vtysh, " -- OK\n") ;
              ret = vty_cmd_out_push(vtysh) ;
            }
          else
            vty_out(vtysh, " -- failed...\n") ;
        } ;
    } ;

  vio_fifo_clear(r_obuf) ;      /* tidy */

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Execute the current command in the current context.
 *
 * The object of the exercise is to execute the command in all the connected
 * daemons to which it applies, including the vtysh itself.
 *
 * A command with the magic marker "TERM" is only run in the vtysh, if at all.
 *
 * The #daemon meta command may reduce the daemons to which a command applies.
 * Meta commands implicitly executable in VTYSH_VD.
 *
 * All output from client daemons, including error messages, is prefixed by the
 * daemon's name, unless vio->vout_base->no_prefix.  Output for commands
 * executed in the vtysh will be prefixed only if the command applies to one or
 * more other daemons (and is not vio->vout_base->no_prefix).
 *
 * If the command does not apply to any connected daemon and does not apply to
 * the vtysh itself, then the result depends:
 *
 *   * if executing "interactive": treat as a CMD_ERROR.
 *
 *   * otherwise is "configuration", and the command is ignored, BUT may
 *     change the current node.
 *
 *     A configuration file may contain commands for daemons which are not
 *     connected.  The node may be changed up to a daemon specific node,
 *     which must be followed to allow commands to be parsed (but ignored)
 *     until the node changes down again.
 *
 * Successful output from each client daemon is pushed as soon as the daemon
 * returns -- so is visible as soon as possible.  Error messages are collected
 * to be output when all daemons, including vtysh, have executed the command.
 *
 * The error handling is a little tricky (!).  We arrange for any error
 * message(s) to appear at the end of the obuf, after the end marker.  Before
 * the end marker there will be any success output that is yet to be pushed.
 * The standard vty_cmd_complete() will then push the success part and deal
 * with the error part in the usual way.
 *
 * Returns:  CMD_SUCCESS  -- all's well, may have output ready to push
 *           CMD_WARNING  -- one or more daemons returned a CMD_WARNING
 *           CMD_ERROR    -- is "interactive" but no daemon recognises the
 *                           command and neither does the vtysh,
 *
 *                           or: one or more daemons returned a CMD_ERROR,
 *
 *                           or: one or more daemons hit an error in the vtysh
 *                           mechanics -- the affected daemons will have been
 *                           disconnected.
 *
 *           CMD_IO_ERROR -- some I/O error while executing commands: error
 *                           posted to the vio->ebuf.
 *
 * NB: if the result context->node is EXIT_NODE, then it is time to close the
 *     vtysh.
 *
 * NB: in the event of CMD_WARNING, CMD_ERROR or CMD_IO_ERROR the vtysh may be
 *     out of step with one or more daemons -- if the command does a node
 *     change, but fails in one or more daemons (including vtysh).
 *
 *     The most obvious case is "configure terminal", when one or more
 *     daemons are unable to gain the required symbol of power.  The effect
 *     will be that the vtysh daemon will stay in ENABLE_NODE, and further
 *     commands for CONFIG_NODE will fail to parse !
 *
 *     Similarly any other command that goes "up" a node level.
 *
 *     The explicit commands that go "down" node levels do not generally fail.
 *     However, commands which implicitly do so, may.
 *
 *     Once a client daemon is out of step, vtysh will not send any further
 *     commands to it, and any commands which might have been sent will be
 *     flagged -- CMD_WARNING.
 *
 * NB: collects all output from all daemons in the obuf.  No actual output
 *     occurs here.
 */
static cmd_ret_t
vty_vtysh_execute(vty vtysh, bool interactive)
{
  cmd_ret_t    ret, fret ;
  daemon_set_t daemons ;
  bool         vtysh_cmd, vtysh_only, term_magic, all_failed ;

  cmd_context  context ;
  cmd_parsed   parsed ;

  qassert(vtysh->type == VTY_VTYSH) ;

  context = vtysh->exec->context ;
  parsed  = vtysh->exec->parsed ;

  /* Do nothing if "parse_only"
   */
  if (context->parse_only)
    return CMD_SUCCESS ;

  /* Work out how many daemons this command applies to, including the vtysh
   * itself.
   *
   * The command itself has a daemon set, which is anded with the node's
   *
   * If none at all, and !configuration -- return error.  If configuration,
   * will find its way to the end and return CMD_SUCCESS, and change node as
   * required.
   */
  if ((parsed->parts & cmd_part_meta) != 0)
    daemons = VTYSH_VD ;                /* always for meta commands     */
  else
    daemons = context->daemons & (vtysh_open_clients | VTYSH_VD) ;
                                        /* daemons can execute for      */

  daemons &= cmd_node_daemons(parsed->cnode) & parsed->cmd->daemons ;

  term_magic = (parsed->cmd->daemons & TERM) != 0 ;
  if (term_magic)
    daemons &= VTYSH_VD ;               /* only vtysh if TERM           */

  qassert((daemons & TERM) == 0) ;

  if ((daemons == 0) && interactive)
    {
      if (term_magic)
        {
          vty_out(vtysh, "%% no effect in vtysh\n") ;
          return CMD_WARNING ;
        }
      else
        {
          vty_out(vtysh, "%% not connected to any daemon for this command\n") ;
          return CMD_ERROR ;
        } ;
    } ;

  vtysh_cmd  = (daemons & VTYSH_VD) != 0 ;
  daemons   &= ALL_RDS ;
  vtysh_only = vtysh_cmd && (daemons == 0) ;

  /* Command results are placed in the vout_base->r_obuf, before being moved
   * to the current obuf, or to the vout_base->r_ebuf.
   *
   * vty_vtysh_complete() processes stuff out of the r_obuf, adding the [...]
   * daemon name as required.
   *
   * Start with an empty r_ebuf, which vty_vtysh_complete() may append to, and
   * we later transfer to the obuf, below.
   */
  vio_fifo_clear(vtysh->vio->vout_base->r_ebuf) ;

  /* Now dispatch command to any and all daemons to which it applies.
   *
   * fret is the overall return.  Starts at CMD_SUCCESS, but may become
   * CMD_WARNING if any daemon returns that, or CMD_ERROR if any daemon
   * returns that -- CMD_ERROR takes precedence.
   *
   * Conversely, all_failed is set false if at least one daemon returned
   * CMD_SUCCESS, or no daemons are involved (and so cannot vote) -- see below.
   */
  fret       = CMD_SUCCESS ;    /* assume the best      */
  all_failed = false ;          /* likewise             */

  if (daemons != 0)
    {
      vtysh_client_dispatch_t dispatch ;
      vtysh_client next_client ;
      uint ti ;

      /* Before sending command to daemon, make sure we send only the command
       * part -- including any # or do.
       *
       * So, get ti = index of first token after the command part, and set
       * length to the start of that.
       */
      qassert((parsed->parts & cmd_part_in_pipe) == 0) ;
      qassert((parsed->parts & cmd_part_command) != 0) ;

      if ((parsed->parts & cmd_part_command) != 0)
        ti = parsed->first_action + parsed->num_action ;
      else
        ti = 0 ;

      dispatch->to_do  = cmd_do_command ;
      dispatch->line   = qs_char(context->line) ;
      dispatch->len    = cmd_token_get(parsed->tokens, ti)->tp ;
      dispatch->cnode  = parsed->cnode ;
      dispatch->xnode  = parsed->xnode ;

      /* Run along open daemons and dispatch to each in turn.
       *
       * The open list means that at least we only consider the currently
       * connected clients.  Also, stop as soon as do all daemons.
       */
      all_failed = true ;               /* fear the worst       */

      next_client = vtysh_self->next ;
      while ((next_client != NULL)      /* to be safe           */
                   && (daemons != 0))   /* stop ASAP            */
        {
          vtysh_client client ;

          client      = next_client ;
          next_client = client->next ;

          if ((daemons & client->daemon) != 0)
            {
              ret = vty_vtysh_client_execute(vtysh, client, dispatch) ;
              if (ret == CMD_SUCCESS)
                all_failed = false ;

              fret = vty_vtysh_complete(vtysh, ret, fret, client->prefix) ;

              daemons &= ~client->daemon ;
            } ;
        }
    } ;

  /* If required, execute the command in the vtysh itself.  In any case,
   * update context->node as required.
   *
   * Note that all the output from daemons has been put in the obuf already,
   * as if that were part of the command output -- but may already have been
   * pushed.
   *
   * The rule is that context->node is set to context->nnode if the command
   * returns CMD_SUCCESS.  The problem is that we may get a mix of responses
   * from the client daemons and, if the command executes in the vtysh, from
   * it:
   *
   *   * if the command executes in the vtysh, we take its word for it, and
   *     set context->node in the usual way, depending on whether the command
   *     succeeds *here* (ignoring what any daemon has to say).
   *
   *     There are a very few commands which execute in the vtysh as well as in
   *     any of the client daemons.  Of those, the most interesting are "exit",
   *     "quit" and "end".  These should not fail anywhere, and won't fail in
   *     the vtysh, and we are best off following vtysh response.
   *
   *   * if the command executes in one or more daemons, then if it fails in
   *     all daemons, then we know that none has changed node, and we had
   *     better not change node here.  We accept the unanimous vote that the
   *     command was somehow invalid.
   *
   *   * if the command does not execute in any daemon, and not in the vtysh,
   *     then we change node.
   *
   *     This is to support the reading of configuration files, where there
   *     may be a section of commands which do not apply to any current
   *     daemon.  Happily, the parser can track the node changing up and
   *     down, assuming the commands in question are valid.
   *
   * Note that the vtysh works on the basis that it can track the node state,
   * and that if or when a daemon gets out of step, it will do its best to
   * get back into step.
   */
  if (vtysh_cmd)
    {
      vio_fifo obuf = NULL ;

      if (!vtysh_only)
        {
          obuf = vtysh->vio->obuf ;
          vtysh->vio->obuf = vtysh->vio->vout_base->obuf
                           = vtysh->vio->vout_base->r_obuf ;
        } ;

      ret = cmd_execute(vtysh) ;        /* sets context->node if succeeds */

      if (!vtysh_only)
        vtysh->vio->obuf = vtysh->vio->vout_base->obuf = obuf ;

      fret = vty_vtysh_complete(vtysh, ret, fret,
                                       vtysh_only ? NULL : vtysh_self->prefix) ;
    }
  else
    {
      if (!all_failed)
        context->node = context->cnode = parsed->nnode ;
    } ;

  /* Anything currently in the vio->obuf is successful output.
   *
   * If there is anything in the vout->r_ebuf, then it is the collected
   * CMD_WARNING or CMD_ERROR messages from the various daemons.  We advance
   * the end_mark past the presumed successful output, and then append the
   * r_ebuf contents.
   *
   * We are here leaving the obuf in the same state as normal command
   * processing -- with accepted stuff (eg reflected command line) behind
   * the end marker, and error message ahead of it.
   */
  if (!vio_fifo_is_empty(vtysh->vio->vout_base->r_ebuf))
    {
      qassert(fret != CMD_SUCCESS) ;

      vio_fifo_step_end_mark(vtysh->vio->obuf) ;
      vio_fifo_trim(vtysh->vio->obuf, true /* terminate*/) ;

      vio_fifo_copy(vtysh->vio->obuf, vtysh->vio->vout_base->r_ebuf) ;
      vio_fifo_clear(vtysh->vio->vout_base->r_ebuf) ;
    } ;

  /* Done: return composite result.
   */
  return fret ;
} ;

/*------------------------------------------------------------------------------
 * Execute the given parsed command in the given daemon.
 *
 * The command is passed to the daemon complete with two node
 * settings:
 *
 *   cnode   -- the node the command is in
 *
 *   xnode   -- the node the vtysh is in
 *
 * The daemon will adjust to match, and should be able to do so.  If the daemon
 * is so out of step that it cannot execute the command, it will generate a
 * suitable error message.
 *
 * Any output from the daemon is collected in the vout_base->r_obuf.
 *
 * Returns:  CMD_SUCCESS  -- OK
 *           CMD_WARNING  -- daemon returned a CMD_WARNING
 *           CMD_ERROR    -- daemon returned some sort of error
 *           CMD_IO_ERROR -- I/O or an error in the vtysh mechanics
 *                           see: vout_base->ebuf (NB: not vout_base->r_ebuf)
 *
 * Closes the client if there is an error in the vtysh mechanics, so cannot
 * depend on the client still being on the vtysh_client_list -- but the rest
 * of the client structure is preserved.
 */
static cmd_ret_t
vty_vtysh_client_execute(vty vtysh, vtysh_client client,
                                                 vtysh_client_dispatch dispatch)
{
  vtysh_client_ret_t cret ;

  cret = vtysh_client_write(client, dispatch) ;

  if (cret == vtysh_client_complete)
    return vtysh_client_read(vtysh, client) ;
  else
    return vtysh_client_fail(vtysh, client, cret) ;
} ;

/*------------------------------------------------------------------------------
 * Deal with completion of command in a daemon.
 *
 * If there is a prefix, then all output from the daemon is sitting in the
 * vout_base->r_obuf.
 *
 * If there is no prefix, the output is in the obuf.
 *
 * If the result is success, push the output.
 *
 * We here implement the "out_ordinary" handling for each daemon, so that
 * can push any success output, and so that remove any unwanted output before
 * later appending any error messages.
 *
 * If is CMD_WARNING, we worry about out_warning here because this is the
 * last moment we can distinguish CMD_WARNING from other errors.  If warning
 * output is suppressed, discard any output and treat as CMD_SUCCESS.
 *
 * We expect to only see CMD_SUCCESS/CMD_WARNING/CMD_ERROR coming from the
 * client daemon.  For CMD_SUCCESS (if the output is not suppressed), then we
 * move it and add prefix (if required) to the obuf.  For CMD_WARNING (if the
 * output is not suppressed) and for CMD_ERROR we move the output and add
 * prefix (if required) to the vout_base->r_ebuf.  When all client commands
 * have been executed, the vio->obuf will have its end_mark moved to the end
 * and the vout->r_ebuf will be appended -- the obuf will then look as
 * intended... with any successful output before the end mark, and any error
 * or warning stuff after it.
 *
 * If is CMD_IO_ERROR it will either be an I/O error or framing error talking
 * to one of the daemons or (just possibly) an I/O error in output.  In both
 * cases, any error message will already be in the vio->ebuf !
 *
 * For any warning or error, we pull error messages into a side buffer,
 * pro tem, so that can eventually deal with all such messages as a single
 * CMD_WARNING/CMD_ERROR/CMD_IO_ERROR.
 *
 * In fret we are given the maximum return so far out of CMD_SUCCESS/
 * CMD_WARNING/CMD_ERROR/CMD_IO_ERROR and we return the new maximum.
 */
static cmd_ret_t
vty_vtysh_complete(vty vtysh, cmd_ret_t ret, cmd_ret_t fret, const char* prefix)
{
  vio_fifo  src ;
  vio_fifo  dst ;
  vty_io    vio ;
  bool      push ;

  VTY_LOCK() ;

  vio  = vtysh->vio ;

  dst  = NULL ;         /* Will be set iff there is something to move   */
  push = false ;        /* Will be set iff something to push            */

  /* If prefix != NULL, then the output to be dealt with is in the rbuf,
   * which will have a end_mark at the start of the fifo.
   *
   * Otherwise, the output is in the tail part of the obuf.
   */
  if (prefix != NULL)
    src = vio->vout_base->r_obuf ;
  else
    src = vio->obuf ;

  /* Worry about out_ordinary and out_warning.
   *
   * Update fret as required.
   *
   * Sets dst to be the obuf if need to move success output from the rbuf
   * to the obuf.
   *
   * Sets dst to be the ebuf if need to move error message to the ebuf.
   */
  switch (ret)
    {
      case CMD_SUCCESS:
        if (!vio_fifo_is_tail_empty(src))
          {
            if (vio->vout->out_ordinary)
              {
                dst  = vio->obuf ;      /* have something to move       */
                push = true ;           /* something to push            */
              }
            else
              vio_fifo_back_to_end_mark(src) ;        /* discard        */
          } ;
        break ;

      case CMD_WARNING:
        if (vio->vin->context->out_warning)
          {
            if (fret == CMD_SUCCESS)
              fret = CMD_WARNING ;

            if (vio_fifo_is_tail_empty(src))
              uty_cmd_failed_message(src, ret) ;    /* Standard/Default */

            dst = vio->vout_base->r_ebuf ;
          }
        else
          {
            vio_fifo_back_to_end_mark(src) ;        /* discard          */
            ret = CMD_SUCCESS ;
          } ;
        break ;

      default:
        qassert(false) ;
        fall_through ;

      case CMD_ERROR:
        if (vio_fifo_is_tail_empty(src))
          uty_cmd_failed_message(src, ret) ;        /* Standard/Default */

        if (fret != CMD_IO_ERROR)
          fret = CMD_ERROR ;

        dst = vio->vout_base->r_ebuf ;
        break ;

      case CMD_IO_ERROR:
        fret = CMD_IO_ERROR ;

        /* I/O error will already have been written to the vio->ebuf in the
         * usual way.
         *
         * If there is anything in the src, then we treat that as stuff which
         * arrived before the I/O error occurred,
         */
        if (!vio_fifo_is_tail_empty(src))
          {
            dst  = vio->obuf ;          /* have something to move       */
            push = true ;               /* something to push            */
          } ;

        break ;
    } ;

  /* If have a destination buffer, then have something to move.
   *
   * If do not have a destination buffer, have emptied out the r_obuf (if there
   * was anything there) or we have success output already sitting in the obuf.
   */
  if (dst != NULL)
    {
      /* Move contents of src to the selected buffer, adding the
       * prefix, if required, making sure that ends tidily with a '\n'.
       */
      vio_fifo_trim(src, true /* terminate*/) ;

      if ((prefix != NULL) && (!vio->vout_base->no_prefix))
        {
          /* Move from src == rbuf to dst, adding prefix to every line
           */
          uint plen ;
          uint used ;
          bool add ;

          qassert(src == vio->vout_base->r_obuf) ;
          vio_fifo_step_end_mark(src) ;

          plen = strlen(prefix) ;
          used = 0 ;
          add  = true ;

          while (1)
            {
              uint have ;
              char* p, * n ;

              have = vio_fifo_step_get(src, used) ;

              if (have == 0)
                break ;

              if (add)
                {
                  vio_fifo_put_bytes(dst, prefix, plen) ;
                  add = false ;
                } ;

              p = vio_fifo_get_ptr(src) ;

              n = memchr(p, '\n', have) ;
              if (n != NULL)
                {
                  used = n - p + 1 ;
                  add  = true ;
                }
              else
                used = have ;

              vio_fifo_put_bytes(dst, p, used) ;
            } ;

          vio_fifo_clear(src) ;
        }
      else if (dst != src)
        {
          /* Move tail from src, verbatim and discard from src.
           *
           * May be moving tail of r_obuf to obuf or to r_ebuf, or may be
           * moving tail of r_obuf to r_ebuf.
           */
          vio_fifo_copy_tail(dst, src) ;
          vio_fifo_back_to_end_mark(src) ;
        } ;
    } ;

  if (push)
    {
      ret = uty_cmd_out_push(vio->vout) ;

      if (ret == CMD_IO_ERROR)
        fret = CMD_IO_ERROR ;
    } ;

  VTY_UNLOCK() ;

  return fret ;
} ;

/*==============================================================================
 * Command output to be sent either to stdout, or to a "more" pipe.
 */

/*------------------------------------------------------------------------------
 * Prepare for output.
 *
 * If the pager is NULL, output will be to stdout.
 *
 * Makes sure that the obuf is empty.
 */
extern void
uty_vtysh_out_prep(vty_io vio, const char* pager)
{
  qassert(vio->vin_depth  == 1) ;
  qassert(vio->vout_depth == 1) ;

  qassert(vio->vout->vout_type == VOUT_VTYSH) ;

  vio->vout->fp    = (pager == NULL) ? stdout : NULL ;
  vio->vout->pager = pager ;

  vio->vout->out_ordinary = vio->vin->context->out_ordinary ;

  vio_fifo_clear(vio->obuf) ;
} ;

/*------------------------------------------------------------------------------
 * Close the pager output, if there is any.
 */
extern void
uty_vtysh_out_close_pager(vio_vf vf)
{
  if ((vf->pager != NULL) && (vf->fp != NULL))
    {
      if (pclose (vf->fp) == -1)
        perror ("pclose failed for pager");

      vf->fp    = NULL ;
      vf->pager = NULL ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Want another command line for the vtysh itself
 *
 * Actually does very little, except return CMD_SUCCESS when everything is
 * quiet, and it is time to obtain another command line -- which is done
 * outside the loop for the vtysh: vst_cmd_fetch state causes exit from the
 * vty_vtysh_command_loop(), above.
 *
 * Does something if is:
 *
 *   * vst_cmd_fetch    -- absolutely on its own, with no vst_hiatus_mask and
 *                                                     no vst_mon_mask bits
 *
 *     Returns CMD_SUCCESS -- ready for next command line
 *
 *   * vst_cmd_complete -- also absolutely on its own
 *
 *     Moves to vst_cmd_fetch and returns CMD_SUCCESS.
 *
 * Otherwise, does nothing and returns CMD_HIATUS, because is:
 *
 *   * vst_cmd_running           )
 *   * vst_cmd_running_executing ) -- previous command still running
 *   * vst_cmd_more              )    output side responsible for this.
 *   * vst_cmd_more_executing    )
 *
 *     Actually, vst_cmd_more/_executing are impossible for VIN_VTYSH !
 *
 *   * something in vst_hiatus_mask -- which the hiatus will take care of.
 *
 *   * something in vst_mon_mask -- which the output side will take care of.
 *
 *     Actually, vst_mon_xxxx are impossible for VIN_VTYSH !
 *
 * Returns:  CMD_SUCCESS  -- ready to fetch another command line
 *           CMD_HIATUS   -- no command line, hiatus attention required
 */
extern cmd_ret_t
uty_vtysh_cmd_line_fetch(vio_vf vf)
{
  cmd_ret_t ret ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                        */

  qassert(vf->vin_type == VIN_VTYSH) ;

  vio = vf->vio ;

  qassert(vio->vin_depth == 1) ;        /* so vst_cmd_xxx is ours !     */

  switch (vio->state)
    {
      /* Ready to fetch another command line.
       */
      case vst_cmd_complete:
        vio->state = vst_cmd_fetch ;

        fall_through ;

      /* In the process of fetching a command line
       */
      case vst_cmd_fetch:
        ret = CMD_SUCCESS ;
        break ;

      case vst_cmd_dispatched:
      case vst_cmd_more:
      case vst_cmd_more_executing:
        qassert(false) ;
        uty_vf_read_stop(vf, vfs_stop_final) ;
        fall_through ;

      /* We are waiting either for input or output before can do anything here.
       */
      default:
        ret = CMD_HIATUS ;
        break ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to vtysh -- VOUT_VTYSH -- vf_open.
 *
 * All output is simple-minded blocking I/O -- assumes stdout or pager will
 * not stall or block for any significant amount of time.
 *
 * After everything is output,
 *
 * If an error occurred earlier, then returns immediately (CMD_SUCCESS).
 *
 * There is a very special case: under some circumstances the vf->obuf will
 * be the same as the vf->rbuf -- see vty_vtysh_execute().  In this case
 * nothing should be output until the contents of vf->rbuf are moved to the
 * real vf->obuf, and the real vf->obuf is restored.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 */
extern cmd_ret_t
uty_vtysh_out_push(vio_vf vf)
{
  qassert(vf->vout_type  == VOUT_VTYSH) ;

  if (vf->obuf == vf->r_obuf)
    return CMD_SUCCESS ;

  /* Do nothing if vf_end.  Also traps vst_final.
   */
  if (vf->vout_state & vf_end)
    return CMD_SUCCESS ;

  qassert((vf->vio->state & vst_final) == 0) ;

  /* If not vst_cmd_running or vst_cmd_running_executing we are done.
   */
  if ((vf->vio->state & (vst_cmd_inner_mask | vst_cancel)) != vst_cmd_running)
    return CMD_SUCCESS ;        /* nothing more to do ATM       */

  /* Empty out the obuf.
   *
   * This is simple-minded blocking I/O -- so we expect to complete.
   */
  if (!vio_fifo_is_empty(vf->obuf))     /* do nothing if empty !      */
    {
      int   have ;

      if ((vf->fp == NULL) && (vf->pager != NULL))
        {
          vf->fp = popen (vf->pager, "w");
          if (vf->fp == NULL)
            {
              perror ("popen failed for pager");
              vf->fp    = stdout ;
              vf->pager = NULL ;
            } ;
        } ;

      have = 0 ;        /* first step == 0      */
      while (vf->fp != NULL)
        {
          int  r ;

          have = vio_fifo_step_get(vf->obuf, have) ;
          if (have == 0)
            break ;

          while (1)
            {
              r = fwrite(vio_fifo_get_ptr(vf->obuf), have, 1, vf->fp) ;
              if (r == 1)
                r = fflush(vf->fp) ;
              else
                r = EOF ;

              confirm(EOF != 1) ;

              if (r == EOF)
                {
                  bool pager ;

                  if (errno == EINTR)
                    continue ;

                  pager = (vf->pager != NULL) ;

                  if (pager && (errno == EPIPE))
                    printf("--quit\n") ;
                  else
                    perror(pager ? "pager output failed"
                                 : "stdout output failed") ;

                  vf->fp = NULL ;
                } ;

              break ;
            } ;
        } ;

      /* If nowhere (left) to output to, clear the buffer
       */
      if (vf->fp == NULL)
        vio_fifo_clear(vf->obuf) ;
    } ;

  /* If is not vst_cmd_executing, then we are at the top command level, and
   * now that the buffer is empty, we can go vst_cmd_complete.
   *
   * Also if have opened a pager, now is the time to close it again.
   */
  if ((vf->vio->state & vst_cmd_executing) == 0)
    {
      qassert(vio_fifo_is_empty(vf->obuf)) ;
      vf->vio->state = (vf->vio->state & ~vst_cmd_mask) | vst_cmd_complete ;

      uty_vtysh_out_close_pager(vf) ;
    } ;

  /* In any event, have done everything that can be done.
   */
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Block waiting to write to vtysh -- own output.
 *
 * For blocking vf *only*.
 *
 * Returns:  CMD_SUCCESS    -- can write or got something to read or something
 */
extern cmd_ret_t
uty_vtysh_write_block(vio_vf vf)
{
  return uty_vtysh_out_push(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Close the output side of vtysh -- VOUT_VTYSH
 *
 * Pretty straightforward... push the output with ucmd_push_closing, which will
 * close down any pager.  There is no real need to close the actual output,
 * since is about to exit the vtysh program.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 */
extern cmd_ret_t
uty_vtysh_write_close(vio_vf vf)
{
  cmd_ret_t ret ;

  ret = uty_vtysh_out_push(vf) ;

  vf->vio->cl_state = vcl_stopped ;
  return ret ;
} ;

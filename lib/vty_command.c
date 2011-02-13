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
#include "list_util.h"
#include "qstring.h"

/*==============================================================================
 * Variables etc.
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void uty_show_error_location(vio_vf vf) ;
static void vty_cmd_failed(vty vty, cmd_return_code_t ret) ;

/*==============================================================================
 * There are two command loops -- cmd_read_config() and cq_process().  These
 * functions support those command loops:
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
 *     NB: from this point onwards, the vio is vio->cmd_running !
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
 *   * vty_cmd_loop_exit()
 *
 *     When a command has completed, successfully or otherwise, this is
 *     called to deal with any errors and return control to.
 *
 *
 *
 *
 */

/*------------------------------------------------------------------------------
 * After opening or closing a vin/vout object, update the vty->exec context.
 */
extern void
uty_cmd_prepare(vty_io vio)
{
  cmd_exec exec = vio->vty->exec ;

  exec->parse_type      = vio->vin->parse_type ;

  exec->out_enabled     = vio->vout->out_enabled ;
  exec->reflect_enabled = vio->vin->reflect_enabled &&
                                                        vio->vout->out_enabled ;
} ;

/*------------------------------------------------------------------------------
 * This pushes the command line into the vty_cli_message queue, where it will
 * be parsed and executed etc.
 *
 * NB: from this moment on, vio->cmd_running is true, so even if the vty is
 *     closed, it cannot be fully closed and then reaped until the flag
 *     is cleared.
 *
 *     While vio->cmd_running the command side is responsible for the vty
 *     and the vty->exec stuff.
 */
extern cmd_return_code_t
uty_cmd_dispatch(vty_io vio, cmd_do_t to_do, qstring line)
{
  VTY_ASSERT_LOCKED() ;

  vio->cmd_running = true ;
  uty_cmd_prepare(vio) ;                /* make sure    */

  cq_dispatch(vio->vty, to_do, line) ;

  return CMD_WAITING ;
} ;

/*------------------------------------------------------------------------------
 * Handle a "special" command -- anything not cmd_do_command
 */
extern cmd_return_code_t
vty_cmd_special(vty vty)
{
  cmd_return_code_t ret ;

  ret = CMD_SUCCESS ;
  /* All other nodes...                                                 */
  switch (vty->exec->to_do)
  {
    case cmd_do_nothing:
      break ;

    case cmd_do_eof:
      ret = CMD_CLOSE ;
      break ;

    case cmd_do_command:
      zabort("invalid cmd_do_command") ;
      break ;

    case cmd_do_ctrl_d:
      zabort("invalid cmd_do_ctrl_d") ;
      break ;

    case cmd_do_ctrl_c:
    case cmd_do_ctrl_z:
      ret = cmd_end(vty) ;
      break ;

    default:            /* must now be cmd_do_auth or cmd_do_auth_enable  */
      VTY_LOCK() ;
      ret = uty_cli_auth(vty->vio->vin->cli) ;
      VTY_UNLOCK() ;
  } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Fetch the next command line to be executed.
 *
 * If possible, get another command line to execute -- pops pipes as required
 * and reflects command line when have one, if required.
 *
 * Action depends on the type of the current input:
 *
 *   VIN_NONE       invalid
 *
 *   VIN_TERM     ) return CMD_EOF
 *   VIN_VTYSH    )
 *
 *   VIN_FILE     )
 *   VIN_PIPE     ) fetch another line if can.
 *   VIN_CONFIG   )
 *
 *   VIN_DEV_NULL  nothing: return CMD_EOF
 *
 * Returns: CMD_SUCCESS => OK -- have a line ready to be processed.
 *
 *                         vty->exec->line points at the line.
 *
 *      or: CMD_EOF     => nothing to fetch -- note that if pops back to the
 *                         VIN_TERM or VIN_VTYSH level, then will return
 *                         CMD_EOF.
 *
 *                         If was VIN_TERM or VIN_VTYSH, signals CMD_EOF to
 *                         exit from the command loop.
 *
 *      or: CMD_WAITING => OK -- but waiting for command line to arrive.
 *
 *      or: ....
 */
extern cmd_return_code_t
vty_cmd_fetch_line(vty vty)
{
  cmd_return_code_t ret ;
  vty_io    vio ;
  qstring*  p_line ;

  VTY_LOCK() ;

  vio    = vty->vio ;           /* once locked  */
  p_line = &vty->exec->line ;

  *p_line = NULL ;
  vty->exec->to_do = cmd_do_nothing ;

  while (1)
    {
      vio_vf    vf ;

      vf = vio->vin ;

      if (vf->vin_state != vf_open)
        {
          switch (vf->vin_state)
          {
            case vf_closed:
            case vf_eof:
              ret = CMD_EOF ;
              break ;

            case vf_error:
              ret = CMD_IO_ERROR ;
              break ;

            case vf_closing:
              ret = CMD_CLOSE ;
              break ;

            default:
              zabort("invalid vf->vin_state") ;
              break ;
          } ;
          break ;
        } ;

      switch (vf->vin_type)
      {
        case VIN_NONE:
          zabort("invalid VIN_NONE") ;
          break ;

        case VIN_TERM:
        case VIN_VTYSH:
        case VIN_DEV_NULL:
          ret = CMD_EOF ;
          break ;

          /* fall through                   */

        case VIN_FILE:
        case VIN_PIPE:
        case VIN_CONFIG:
          ret = uty_file_fetch_command_line(vf, p_line) ;

          if (ret == CMD_SUCCESS)
            {
              vty->exec->to_do = cmd_do_command ;
            }
          else if ((ret == CMD_EOF) && (vio->vin_depth > 0))
            {
              uty_vin_close(vio) ;
              uty_cmd_prepare(vio) ;
              continue ;
            } ;
          break ;

        default:
          zabort("unknown vin_type") ;
      } ;

      break ;
    } ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Reflect the command line to the current vio->obuf.
 *
 * Advances the end_mark past the reflected line, so that output (in particular
 * error stuff) is separate.
 */
extern void
vty_cmd_reflect_line(vty vty)
{
  vio_fifo obuf ;
  qstring  line ;

  VTY_LOCK() ;

  obuf = vty->vio->obuf ;       /* once locked          */
  line = vty->exec->line ;

  vio_fifo_put_bytes(obuf, qs_char_nn(line), qs_len_nn(line)) ;
  vio_fifo_put_byte(obuf, '\n') ;

  uty_out_accept(vty->vio) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Open the given file as in in pipe, if possible.
 *
 * Puts error messages to vty if fails.
 */
extern cmd_return_code_t
vty_cmd_open_in_pipe_file(vty vty, qstring name, bool reflect)
{
  cmd_return_code_t ret ;

  VTY_LOCK() ;

  ret = uty_file_read_open(vty->vio, name, reflect) ;

  if (ret == CMD_SUCCESS)
    uty_cmd_prepare(vty->vio) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

extern cmd_return_code_t
vty_cmd_open_in_pipe_shell(vty vty)
{
  return CMD_SUCCESS ;
} ;

extern cmd_return_code_t
vty_cmd_open_out_pipe_file(vty vty, qstring name, bool append)
{
  cmd_return_code_t ret ;

  VTY_LOCK() ;

  ret = uty_file_write_open(vty->vio, name, append) ;

  if (ret == CMD_SUCCESS)
    uty_cmd_prepare(vty->vio) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

extern cmd_return_code_t
vty_cmd_open_out_dev_null(vty vty)
{
  vio_vf   vf ;

  VTY_LOCK() ;

  vf = uty_vf_new(vty->vio, "dev_null", -1, vfd_none, vfd_io_none) ;
  uty_vout_open(vty->vio, vf, VOUT_DEV_NULL, NULL, NULL, 0) ;

  vf->out_enabled = false ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

extern cmd_return_code_t
vty_cmd_open_out_pipe_shell(vty vty)
{
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Command has completed successfully.
 *
 * An output generated by the command is now pushed if exec->out_enabled,
 * or discarded.
 *
 * If the vin_depth < vout_depth, then close vouts until the depth is equal.
 */
extern cmd_return_code_t
vty_cmd_success(vty vty)
{
  vty_io vio ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  if (!vio_fifo_empty_tail(vio->obuf))
    {
      if (vty->exec->out_enabled)
        uty_cmd_out_push(vio) ;
      else
        uty_out_clear(vio) ;
    } ;

  /* Deal with closing out pipe on individual command           */
  while (vio->vout_depth > vio->vin_depth)
    {
      uty_vout_close(vio, false) ;      /* ignore failures TODO */
      uty_cmd_prepare(vio) ;            /* set new state        */
    } ;

  VTY_UNLOCK() ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * If there is anything after the end_mark, push it to be written, now.
 *
 * See uty_cmd_out_push() below.
 */
extern void
vty_cmd_out_push(vty vty)
{
  VTY_LOCK() ;
  uty_cmd_out_push(vty->vio) ;
  VTY_UNLOCK() ;
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
 * NB: takes no notice of vf->out_enabled...  this is used when outputting
 *     any error messages when reading the configuration file.
 *
 * TODO: worry about closing state !
 */
extern void
uty_cmd_out_push(vty_io vio)
{
  cmd_return_code_t ret ;
  vio_vf    vf ;

  vf     = vio->vout ;

  assert(vio->obuf == vf->obuf) ;

  uty_out_accept(vio) ;         /* advance the end mark         */

  if (vf->vout_state != vf_open)
    {
      switch (vf->vout_state)
      {
        case vf_closed:
        case vf_eof:
          ret = CMD_EOF ;       /* TODO what's to do ?          */
          break ;

        case vf_error:
          ret = CMD_IO_ERROR ;
          break ;

        case vf_closing:
          ret = CMD_CLOSE ;     /* TODO continue with output ?  */
          break ;

        default:
          zabort("invalid vf->vout_state") ;
          break ;
      } ;

      return ;                  /* TODO errors ??               */
    } ;

  switch (vio->vout->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid vout_none") ;
      break ;

    case VOUT_TERM:
      uty_cli_out_push(vf->cli) ;
      break ;

    case VOUT_VTYSH:
      /* Kick the writer    */
      break ;

    case VOUT_FILE:
      uty_file_out_push(vf) ;
      break ;

    case VOUT_PIPE:
      /* Kick the writer    */
      break ;

    case VOUT_CONFIG:
      /* Kick the writer    */
      break ;

    case VOUT_DEV_NULL:
      uty_out_clear(vio) ;      /* clear fifo, keep end mark    */
      break ;

    case VOUT_STDOUT:
      vio_fifo_fwrite(vio->obuf, stdout) ;
      break ;

    case VOUT_STDERR:
      vio_fifo_fwrite(vio->obuf, stderr) ;
      break ;

    default:
      zabort("unknown vout_type") ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * The command loop has exited, with the given return code.
 *
 * There are a number of return codes that are treated as success.  On any
 * of those, does vty_cmd_success() (which has no effect if already done).
 *
 * If not a success return code, then close down any pipe-work, and create
 * suitable error message.
 *
 * If is any form of success, returns CMD_SUCCESS -- otherwise returns the
 * return code as given.
 */
extern cmd_return_code_t
vty_cmd_loop_exit(vty vty, cmd_return_code_t ret)
{
  bool close ;

  /* Deal with the several names of success and the many names of failure.
   */
  close = false ;

  switch(ret)
  {
    case CMD_CLOSE:
    case CMD_EOF:
      close = true ;
      fall_through ;

    case CMD_SUCCESS:
    case CMD_EMPTY:
      ret = vty_cmd_success(vty) ;
      break ;

    default:
      /* If not CMD_SUCCESS, the treat as some sort of error:
       *
       *   * close down all pipes -- leaving just the vin_base/vout_base.
       *   * create error messages in the vout_base.
       */
      vty_cmd_failed(vty, ret) ;
      break ;
  } ;

  /* They think it's all over...                                        */

  VTY_LOCK() ;

  vty->vio->cmd_running = false ;

  if (vty->vio->vin->vin_type == VIN_TERM)
    uty_cli_done_command(vty->vio->vin->cli, vty->node) ;

  /* ... it is now.                                                     */

  if (close)
    uty_close(vty->vio, false, NULL) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Dealing with error of some kind.
 *
 * Error is reported on the vout_base.  Whatever the command has written
 * so far will be appended to the reporting of the error location.
 *
 * If there is only one vout, then:
 *
 *   - if is VOUT_TERM, then need to wipe the command line.  The failing
 *     line will be the last output line on screen (unless monitor output,
 *     which can do nothing about).
 *
 *   - if is VOUT_VTYSH, then the failing line will be the last output line
 *     on the screen.
 *
 *   - if is VOUT_xxx, then nothing will have been reflected.
 *
 * In any event, need to separate out any output from the failing command,
 * so that can report error location and type, before showing the error
 * message(s).
 *
 * NB: does not expect to see all the possible CMD_XXX return codes (see
 *     below), but treats all as a form of error !
 */
static void
vty_cmd_failed(vty vty, cmd_return_code_t ret)
{
  vty_io    vio ;
  vio_fifo  tbuf ;
  ulen      eloc ;

  VTY_LOCK() ;
  vio = vty->vio ;              /* once locked  */

  /* Copy command output to temporary, and remove from the obuf.
   *
   * Any command output should be messages explaining the error, and we want
   * those (a) on the base vout, and (b) after description of where the error
   * occurred.
   */
  tbuf = vio_fifo_copy_tail(NULL, vio->obuf) ;
  vio_fifo_back_to_end_mark(vio->obuf, true) ;

  /* Can now close everything in the vout stack, so all further output
   * is to the vout_base.
   */
  uty_vout_close_stack(vio, false) ;    /* not "final"  */

  /* Process the vin stack to generate the error location(s)            */
  uty_show_error_location(vio->vin) ;

  if (vio->vin->vin_type == VIN_TERM)
    eloc = uty_cli_prompt_len(vio->vin->cli) ;
  else
    eloc = 0 ;

  /* Can now close everything in the vin stack                          */
  uty_vin_close_stack(vio) ;

  /* Now any additional error message if required                       */
  switch (ret)
  {
    qstring qs ;

    case CMD_WARNING:
      if (vio_fifo_empty(tbuf))
        uty_out(vio, "%% WARNING: non-specific warning\n") ;
      break ;

    case CMD_ERROR:
      if (vio_fifo_empty(tbuf))
        uty_out(vio, "%% ERROR: non-specific error\n") ;
      break ;

    case CMD_IO_ERROR:
      break ;

    case CMD_ERR_PARSING:
      qs = qs_set_fill(NULL, eloc + vio->vty->exec->parsed->eloc, "....") ;
      uty_out(vio, "%s^\n%% %s\n", qs_string(qs),
                                   vio->vty->exec->parsed->emess) ;
      qs_reset(qs, free_it) ;
      break ;

    case CMD_ERR_NO_MATCH:
      uty_out(vio, "%% Unknown command.\n") ;
      break;

    case CMD_ERR_AMBIGUOUS:
      uty_out(vio, "%% Ambiguous command.\n");
      break;

    case CMD_ERR_INCOMPLETE:
      uty_out(vio, "%% Command incomplete.\n");
      break;

    case CMD_SUCCESS:
    case CMD_SUCCESS_DAEMON:
    case CMD_CLOSE:
    case CMD_EMPTY:
    case CMD_WAITING:
    case CMD_EOF:
    case CMD_COMPLETE_FULL_MATCH:
    case CMD_COMPLETE_MATCH:
    case CMD_COMPLETE_LIST_MATCH:
    case CMD_COMPLETE_ALREADY:
    default:
      uty_out(vio, "%% Unexpected return code (%d).\n", (int)ret) ;
      break ;
  } ;

  /* Now stick the tbuf onto the end of the output & discard tbuf.      */
  vio_fifo_copy(vio->obuf, tbuf) ;
  vio_fifo_reset(tbuf, free_it) ;

  uty_cmd_out_push(vio) ;

  VTY_UNLOCK() ;
} ;

static void
uty_show_error_location(vio_vf vf)
{
  vio_vf   vf_next ;

  /* Recurse until hit end of the vin stack                             */
  vf_next = ssl_next(vf, vin_next) ;

  if (vf_next != NULL)
    uty_show_error_location(vf_next) ;
  else
    assert(vf == vf->vio->vin_base) ;

  /* On the way back, add the error location for each vin entry         */
  switch (vf->vin_type)
  {
    case VIN_NONE:
      zabort("invalid VIN_NONE") ;
      break ;

    case VIN_TERM:
      /* Wipe the current command line                                  */
//    uty_term_show_error_location(vf, loc) ;
      break ;

    case VIN_VTYSH:
      break ;

    case VIN_FILE:

    case VIN_PIPE:

    case VIN_CONFIG:
      uty_out(vf->vio, "%% on line %d of %s:\n", vf->line_number, vf->name) ;
      uty_out(vf->vio, "%s\n", qs_make_string(vf->cl)) ;
      break ;

    case VIN_DEV_NULL:
      break ;

    default:
      zabort("unknown vin_type") ;
  } ;
} ;

/*==============================================================================
 * Configuration node/state handling
 *
 * At most one VTY may hold the configuration symbol of power at any time.
 */

/*------------------------------------------------------------------------------
 * Attempt to gain the configuration symbol of power
 *
 * If succeeds, set the given node.
 *
 * Returns: true <=> now own the symbol of power.
 */
extern bool
vty_config_lock (struct vty *vty, enum node_type node)
{
  bool result;

  VTY_LOCK() ;

  if (!host.config)
    {
      vty->config = true ;
      host.config = true ;
    } ;

  result = vty->config;

  if (result)
    vty->node = node ;

  VTY_UNLOCK() ;

  return result;
}

/*------------------------------------------------------------------------------
 * Give back the configuration symbol of power -- if own it.
 *
 * Set the given node -- which must be <= MAX_NON_CONFIG_NODE
 */
extern void
vty_config_unlock (struct vty *vty, enum node_type node)
{
  VTY_LOCK() ;
  uty_config_unlock(vty, node);
  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Give back the configuration symbol of power -- if own it.
 *
 * Set the given node -- which must be <= MAX_NON_CONFIG_NODE
 */
extern void
uty_config_unlock (struct vty *vty, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;

  if (host.config && vty->config)
    {
      vty->config = false ;
      host.config  = false ;
    }

  assert(node <= MAX_NON_CONFIG_NODE) ;
  vty->node = node ;
} ;

















/*------------------------------------------------------------------------------
 * Result of command is to close the input.
 *
 * Posts the reason for the close.
 *
 * Returns: CMD_CLOSE
 */
extern cmd_return_code_t
uty_cmd_close(vty_io vio, const char* reason)
{
  vio->close_reason = qs_set(NULL, reason) ;
  return CMD_CLOSE ;
} ;



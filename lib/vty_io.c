/* VTY IO Functions -- top level of VTY IO hierarchy
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

#include "vty.h"
#include "vty_io.h"
#include "vty_io_term.h"
#include "vty_io_file.h"
#include "vty_cli.h"
#include "vty_command.h"
#include "qstring.h"
#include "keystroke.h"
#include "list_util.h"
#include "command_parse.h"
#include "command_execute.h"

#include "memory.h"

#include "prefix.h"
#include "filter.h"
#include "privs.h"
#include "sockunion.h"
#include "network.h"

#include <arpa/telnet.h>
#include <sys/un.h>             /* for VTYSH    */
#include <sys/socket.h>
#include <wait.h>

#define VTYSH_DEBUG 0

/*==============================================================================
 * Basic output to VTY.
 */

/*------------------------------------------------------------------------------
 * VTY output -- cf fprintf !  Same as vty_out, less the VTY_LOCK().
 *
 * This is for command output, which may later be suppressed
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
uty_out(vty_io vio, const char *format, ...)
{
  int     ret ;
  va_list args ;

  VTY_ASSERT_LOCKED() ;

  va_start (args, format) ;
  ret = uty_vprintf(vio, format, args) ;
  va_end (args) ;

  return ret ;
} ;

/*==============================================================================
 * The watch dog.
 *
 * The watch dog starts up every now and checks:
 *
 *   * for changes to the host name, which should be reflected in the
 *     prompt for any terminals.
 */

/* Watch-dog timer                                                      */
static vio_timer_t vty_watch_dog ;

static vty_timer_time uty_watch_dog_bark(vio_timer timer, void* info) ;

/*------------------------------------------------------------------------------
 * Start watch dog -- before a VTY is created.
 *
 * The host name is set during initialisation, so no need to check it here.
 */
extern void
uty_watch_dog_start(void)
{
  vio_timer_init_new(vty_watch_dog, uty_watch_dog_bark, NULL) ;
  vio_timer_set(vty_watch_dog, VTY_WATCH_DOG_INTERVAL) ;
} ;

/*------------------------------------------------------------------------------
 * Stop watch dog timer -- at close down.
 */
extern void
uty_watch_dog_stop(void)
{
  vio_timer_reset(vty_watch_dog, keep_it) ;
} ;

/*------------------------------------------------------------------------------
 * Watch dog vio_timer action
 */
static vty_timer_time
uty_watch_dog_bark(vio_timer timer, void* info)
{
  cmd_host_name(true) ;         /* check for host name change           */

  return VTY_WATCH_DOG_INTERVAL ;
} ;

/*==============================================================================
 * Prototypes.
 */
static void uty_vout_close_reason(vio_vf vf, const char* reason) ;
static cmd_return_code_t uty_vf_read_close(vio_vf vf, bool final) ;
static cmd_return_code_t uty_vf_write_close(vio_vf vf, bool final) ;
static vio_vf uty_vf_free(vio_vf vf) ;

/*==============================================================================
 * Creation and destruction of VTY objects
 */

/*------------------------------------------------------------------------------
 * Allocate new vty structure, including empty vty_io but no exec structure.
 *
 * Sets:
 *
 *   * vio->err_depth     = 1 if VTY_TERMINAL
 *                        = 0 otherwise
 *
 *                          So on everthing except VTY_TERMINAL, any error
 *                          will close the entire VTY.  On VTY_TERMINAL, an
 *                          error will close down to the vin_base/vout_base
 *                          (unless error is in one of those).
 *
 *   * vio->blocking      = true if VTY_CONFIG_READ
 *                        = false otherwise
 *
 * Caller must complete the initialisation of the vty_io, which means:
 *
 *   * constructing a suitable vio_vf and doing uty_vin_push() to set the
 *     vin_base.
 *
 *     All vty_io MUST have a vin_base, even if it is /dev/null.
 *
 *   * constructing a suitable vio_vf and doing uty_vout_push() to set the
 *     vout_base and the vio->obuf.
 *
 *     All vty_io MUST have a vout_base, even if it is /dev/null.
 *
 *   * setting vio->cli, if required
 *
 *   * etc.
 *
 * "exec" is allocated only when the command loop is entered.
 */
extern vty
uty_new(vty_type_t type, node_type_t node)
{
  vty      vty ;
  vty_io   vio ;

  VTY_ASSERT_LOCKED() ;

  /* Basic allocation                                                   */

  /* Zeroising the vty structure will set:
   *
   *   type      = X          -- set to actual type, below
   *
   *   node      = X          -- set to actual node, below
   *
   *   index     = NULL       -- nothing, yet
   *   index_sub = NULL       -- nothing, yet
   *
   *   config    = false      -- not owner of configuration symbol of power
   *   config_brand = 0       -- none, yet
   *
   *   exec      = NULL       -- execution state set up when required
   *   vio       = X          -- set below
   */
  vty = XCALLOC(MTYPE_VTY, sizeof(struct vty)) ;

  vty->type   = type ;
  vty->node   = node ;

  /* Zeroising the vty_io structure will set:
   *
   *   vty          = X           -- set to point to parent vty, below
   *   vio_list     = NULLs       -- not on the vio_list, yet
   *
   *   vin          = NULL        -- empty input stack
   *   vin_base     = NULL        -- empty input stack
   *   vin_depth    = 0           -- no stacked vin's, yet
   *   vin_true_depth = 0         -- ditto
   *
   *   vout         = NULL        -- empty output stack
   *   vout_base    = NULL        -- empty output stack
   *   vout_depth   = 0           -- no stacked vout's, yet
   *
   *   ebuf         = NULL        -- no error at all, yet
   *   err_depth    = 0           -- see below: zero unless VTY_TERMINAL
   *
   *   blocking     = false       -- set below: false unless VTY_CONFIG_READ
   *   state        = vc_stopped  -- not started vty command loop
   *   signal       = CMD_SUCCESS -- OK (null signal)
   *   close_reason = NULL        -- none set
   *
   *   ps_buf       = NULL        -- no pipe stderr return buffer, yet
   *
   *   obuf         = NULL        -- no output buffer, yet
   *
   *   mon_list     = NULLs       -- not on the monitors list
   *   monitor      = false       -- not a monitor
   *   mon_kick     = false
   *   maxlvl       = 0
   *   mbuf         = NULL
   */
  vio = XCALLOC(MTYPE_VTY, sizeof(struct vty_io)) ;

  confirm(vc_stopped  == 0) ;
  confirm(CMD_SUCCESS == 0) ;

  vty->vio = vio ;
  vio->vty = vty ;

  vio->err_depth  = (type == VTY_TERMINAL) ? 1 : 0 ;

  vio->blocking   = (type == VTY_CONFIG_READ) ;

  /* Place on list of known vio/vty                                     */
  sdl_push(vio_live_list, vio, vio_list) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Set timeout value.
 *
 * This is only ever called when a command (eg exec-timeout) sets a new
 * time out value -- which applies only to VIN_TERM and VTY_VTYSH.
 */
extern void
uty_set_timeout(vty_io vio, vty_timer_time timeout)
{
  vio_in_type_t vt ;

  VTY_ASSERT_LOCKED() ;

  vt = vio->vin_base->vin_type ;

  if ((vt == VIN_TERM) || (vt == VIN_VTYSH))
    uty_vf_set_read_timeout(vio->vin_base, timeout) ;
} ;

/*------------------------------------------------------------------------------
 * Return "name" of VTY.
 *
 * The name of the base vin, or (failing that) the base vout.
 */
extern const char*
uty_get_name(vty_io vio)
{
  const char* name ;

  name = vio->vin_base->name ;
  if (name == NULL)
    name = vio->vout_base->name ;

  return (name != NULL) ? name : "?" ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY -- final.
 *
 * Turns off any log monitoring immediately.
 *
 * Close "final" means: terminate any input immediately, but attempt to flush
 * any pending output (and any pipe return) but give up if would block or gets
 * any error.
 *
 * To call this the command loop must be vc_stopped.  When a command loop exits
 * normally, it completes all pending I/O and closes the stacks down to, but
 * not including vout_base.  So there is not much to do here.  When a vty is
 * reset, once the command loop has been stopped, this function will close
 * everything.
 *
 * Once the vio stacks are all closed, except for the vout_base, makes some
 * effort to issue any "close reason" message, then closes the vout_base.
 *
 * The vty can then be closed.
 *
 * NB: releases the vty, the vio, the exec and all related objects.
 *
 *     On return DO NOT attempt to do anything with the one-time VTY.
 */
extern void
uty_close(vty_io vio)
{
  vty vty = vio->vty ;

  VTY_ASSERT_CAN_CLOSE(vio->vty) ;

  qassert(vio->state == vc_stopped) ;

  /* The vio is about to be closed, so take off the live list and make
   * sure that is not a log monitor.
   */
  uty_set_monitor(vio, off) ;
  sdl_del(vio_live_list, vio, vio_list) ;

  /* Close all vin including the vin_base.
   *
   * Note that the vin_base is closed, but is still on the vin stack.
   */
  do
    uty_vin_pop(vio, NULL, true) ;      /* final close, discard context */
  while (vio->vin != vio->vin_base) ;

  /* Close all the vout excluding the vout_base.
   */
  while (vio->vout != vio->vout_base)
    uty_vout_pop(vio, true) ;           /* final close                  */

  /* If the vout_base is not closed, try to output the close reason,
   * if any -- note that this will attempt to output, even if some
   * earlier output has failed.
   */
  uty_vout_close_reason(vio->vout_base, vio->close_reason) ;

  /* Now final close the vout_base.
   */
  uty_vout_pop(vio, true) ;             /* final close                  */

  /* All should now be very quiet indeed.                               */
  if (vty_debug)
    {
      assert(vio->vin == vio->vin_base) ;
      assert(vio->vin_depth      == 0) ;
      assert(vio->vin_true_depth == 0) ;

      assert(vio->vout == vio->vout_base) ;
      assert(vio->vout_depth == 0) ;

      assert(vio->vin->vin_state   == vf_closed) ;
      assert(vio->vout->vout_state == vf_closed) ;

      assert(vty->vio == vio) ;
    } ;

  /* Release what remains of the vio.                                   */
  vio->vty  = NULL ;    /* no longer required.                          */

  vio->ebuf   = vio_fifo_free(vio->ebuf) ;
  vio->obuf   = NULL ;  /* about to discard the vout_base               */
  vio->ps_buf = vio_fifo_free(vio->ps_buf) ;

  XFREE(MTYPE_TMP, vio->close_reason) ;

  if (vio->vin != vio->vout)
    vio->vin = uty_vf_free(vio->vin) ;
  else
    vio->vin = NULL ;

  vio->vout = uty_vf_free(vio->vout) ;

  vio->vin_base  = NULL ;
  vio->vout_base = NULL ;

  assert(!vio->monitor) ;
  vio->mbuf = vio_fifo_free(vio->mbuf) ;

  /* Release the vio and the exec (if any)                              */
  XFREE(MTYPE_VTY, vty->vio) ;
  vty->exec = cmd_exec_free(vty->exec) ;

  /* Finally, release the VTY itself.                                   */
  XFREE(MTYPE_VTY, vty) ;
} ;

/*==============================================================================
 * Pushing and popping vf objects on the vio->vin_stack and vio->vout_stack.
 */

/*------------------------------------------------------------------------------
 * Add a new vf to the vio->vin stack, and set read stuff.
 *
 * Sets the given vf->vin_type and set vf->vin_state = vf_open.
 *
 * Initialises an input buffer if required, and sets line_complete and
 * line_step so that first attempt to fetch a line will give line 1.
 *
 * Sets the read ready action and the read timer timeout action, if required.
 * If the vf is "blocking", then these actions are not required.  Also,
 * if the vin_type is one of the "specials", then these actions are not
 * required -- noting that the vin_type may be "special" while the vout_type
 * is a non-blocking ordinary output.
 *
 * NB: is usually called from the cli thread, but may be called from the cmd
 *     thread for vf which is blocking, or for a "special" vin_type !
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 *
 * NB: see uty_vout_push() for notes on pushing a vin and a vout together.
 */
extern void
uty_vin_push(vty_io vio, vio_vf vf, vio_in_type_t type,
                                    vio_vfd_action* read_action,
                                    vio_timer_action* read_timer_action,
                                    usize ibuf_size)
{
  vf->vin_type  = type ;
  vf->vin_state = vf_open ;

  qassert(type != VIN_NONE) ;

  if ((!vf->blocking) && (vf->vin_type < VIN_SPECIALS))
    {
      vio_vfd_set_read_action(vf->vfd, read_action) ;
      vio_vfd_set_read_timeout_action(vf->vfd, read_timer_action) ;
    } ;

  ssl_push(vio->vin, vf, vin_next) ;
  vio->vin_true_depth = ++vio->vin_depth ;

  if (vio->vin_base == NULL)
    {
      qassert(vio->vin_depth == 1) ;
      vio->vin_base = vf ;
    } ;

  if (ibuf_size != 0)
    {
      vf->ibuf = vio_fifo_new(ibuf_size) ;
      vf->cl   = qs_new(150) ;
      vf->line_complete = true ;
      vf->line_step     = 1 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Save the given context in the current top of the vin stack.
 *
 * This is done when a new pipe is opened, so that:
 *
 *   a) saves the current context in the current vin (the new pipe has
 *      not yet been pushed) so that uty_vin_pop() can restore this context
 *      after closing the then top of the stack.
 *
 *   b) can update context for about to be run vin, eg:
 *
 *        - dir_here -- if required
 *
 *        - can_enable
 *
 * So the top of the vin stack does not contain the current context, that is
 * in the vty->exec !
 */
extern void
uty_vin_new_context(vty_io vio, cmd_context context, qpath file_here)
{
  qassert(vio->vin->context == NULL) ;
  vio->vin->context = cmd_context_new_save(context, file_here) ;
} ;

/*------------------------------------------------------------------------------
 * Push a new vf to the vio->vout stack, and set write stuff.
 *
 * Sets the vf->vout_type and set vf->vout_state = vf_open.
 *
 * Sets the write ready action and the write timer timeout action, if required.
 * If the vf is "blocking", then these actions are not required.  Also,
 * if the vout_type is one of the "specials", then these actions are not
 * required -- noting that the vout_type may be "special" while the vin_type
 * is a non-blocking ordinary input.
 *
 * Initialises an output buffer and sets an end_mark.
 *
 * The new vout_depth depends on whether a vin and vout are being opened
 * together, and if so, in what order they are pushed:
 *
 *   * for a vout being opened on its own (e.g. VOUT_CONFIG_WRITE) the
 *     vout_depth is set to the current vin_depth + 1.  This means that
 *     when the current command completes, vout_depth > vin_depth, so the
 *     vout will automatically be closed.
 *
 *   * for a vout being opened at the same time as a vin, the vout_depth
 *     is set to the same as the *new* vin_depth.  This means that the
 *     vout will not be closed until the vin is.
 *
 *     If the vout is opened before the vin, the new vout_depth will be
 *     vin_depth + 1.
 *
 *     If the vout is opened after the vin, the new vout_depth will be
 *     vin_depth.
 *
 * ...hence the "after" argument.
 *
 * NB: is usually called from the cli thread, but may be called from the cmd
 *     thread for vf which is blocking, or for a "special" vout_type !
 *
 * NB: VOUT_DEV_NULL, VOUT_STDOUT and VOUT_STDERR are special.
 *
 *     The write_action and the write_timer_action are ignored.
 *
 *     All actual I/O to these outputs is direct, blocking and via standard
 *     I/O -- except VOUT_DEV_NULL where all I/O is discarded.
 *
 * NB: all outputs are set up with an obuf, so all output is collected, even
 *     if it is later to be discarded.
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern void
uty_vout_push(vty_io vio, vio_vf vf, vio_out_type_t type,
                                     vio_vfd_action* write_action,
                                     vio_timer_action* write_timer_action,
                                     usize obuf_size,
                                     bool after)
{
  VTY_ASSERT_LOCKED() ;

  vf->vout_type  = type ;
  vf->vout_state = vf_open ;

  qassert(type != VOUT_NONE) ;

  if ((!vf->blocking) && (vf->vout_type < VOUT_SPECIALS))
    {
      vio_vfd_set_write_action(vf->vfd, write_action) ;
      vio_vfd_set_write_timeout_action(vf->vfd, write_timer_action) ;
    } ;

  ssl_push(vio->vout, vf, vout_next) ;
  if (vio->vout_base == NULL)
    {
      qassert(vio->vout_depth == 0) ;
      vio->vout_base = vf ;
    } ;

  vf->obuf = vio_fifo_new(obuf_size) ;
  vio_fifo_set_end_mark(vf->obuf) ;

  vf->depth_mark  = vio->vout_depth ;           /* remember *old* depth */

  if (after)
    {
      qassert(vio->vout_depth < vio->vin_depth) ;
      vio->vout_depth = vio->vin_depth ;        /* set new depth        */
    }
  else
    {
      qassert(vio->vout_depth <= vio->vin_depth) ;
      vio->vout_depth = vio->vin_depth + 1 ;    /* set new depth        */
    } ;

  vio->obuf = vf->obuf ;
} ;

/*------------------------------------------------------------------------------
 * Close top of the vin stack and pop when done -- see uty_vf_read_close().
 *
 * If succeeds in closing, unless is vin_base, pops the vin stack and if this
 * is read-only will free the vio_vf and all its contents.  (So if this is
 * vin_base, it is left on the stack, but vf_closed/vf_closing.)
 *
 * If the given context is not NULL, having popped the vin stack, the context
 * in the new top of stack is restored to the given context.
 *
 * On final close, will not wait for I/O, will completely close the input,
 * even if errors occur (and no errors are posted) and will return CMD_SUCCESS.
 *
 * Returns:  CMD_SUCCESS  -- input completely closed and popped
 *           CMD_WAITING  -- waiting for input to close   <=> non-blocking
 *                                                        <=> not "final"
 *           CMD_IO_ERROR -- error or timeout             <=> try again
 *                                                        <=> not "final"
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern cmd_return_code_t
uty_vin_pop(vty_io vio, cmd_context context, bool final)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = uty_vf_read_close(vio->vin, final) ;

  if ((ret == CMD_SUCCESS) || final)
    {
      assert(vio->vin->vin_state == vf_closed) ;

      if (vio->vin_depth > 1)
        {
          vio_vf vf ;

          vf = ssl_pop(&vf, vio->vin, vin_next) ;
          --vio->vin_depth ;

          if (vf->vout_state == vf_closed)
            uty_vf_free(vf) ;
        }
      else
        {
          assert(vio->vin == vio->vin_base) ;
          vio->vin_depth = 0 ;          /* may already have been closed */
        } ;

      if (vio->vin_true_depth > vio->vin_depth)
        vio->vin_true_depth = vio->vin_depth ;

      if (vio->vin->context != NULL)
        {
          if (context != NULL)
            vio->vin->context = cmd_context_restore(context, vio->vin->context);
          else
            vio->vin->context = cmd_context_free(vio->vin->context, false) ;
                                                    /* Not a copy       */
        } ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Close top of the vout stack and pop when done -- see uty_vf_write_close().
 *
 * If is vout_base, does not completely close, unless is "final" -- if not
 * final, CMD_SUCCESS means that the output buffers are empty and the
 * vout_depth has been reduced, but the vio_vf is still writeable, held in
 * vf_closing state.
 *
 * Unless is vout_base, pops the vout stack.  If this is write-only will free
 * the vio_vf and all its contents.
 *
 * If this is vout_base, does not actually close the vfd and does not close
 * the vout side of the vf.  This leaves an active vio->obuf (inter alia.)
 *
 * Before closing, discard anything after end_mark, then push any outstanding
 * output.
 *
 * Unless "final", the close is soft, that is, if there is any output still
 * outstanding does not close the vout.
 *
 * If there is no outstanding output (or if final) will completely close the
 * vf and free it (except for vout_base).
 *
 * Returns:  CMD_SUCCESS  -- output completely closed and popped
 *           CMD_WAITING  -- waiting for input to close   <=> non-blocking
 *                                                        <=> not "final"
 *           CMD_IO_ERROR -- error or timeout             <=> try again
 *                                                        <=> not "final"
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 *
 *     That is not done here because this may be called from outside the
 *     command loop -- in particular by uty_close().
 *
 *     However, ensures that vio->obuf is up to date !
 */
extern cmd_return_code_t
uty_vout_pop(vty_io vio, bool final)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_LOCKED() ;

  ret = uty_vf_write_close(vio->vout, final) ;

  if (ret != CMD_SUCCESS)
    return ret ;

  if (vio->vout_depth > 1)
    {
      vio_vf vf ;

      vf = ssl_pop(&vf, vio->vout, vout_next) ;

      qassert(vf->depth_mark < vio->vout_depth) ;
      vio->vout_depth = vf->depth_mark ;

      uty_vf_free(vf) ;
    }
  else
    {
      assert(vio->vout == vio->vout_base) ;
      if (final)
        assert(vio->vout->vout_state == vf_closed) ;

      qassert(vio->vout->depth_mark == 0) ;
      vio->vout_depth = 0 ;         /* may already be       */
    } ;

  vio->obuf = vio->vout->obuf ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Try to output the close reason to the vout_base.
 *
 * This is the final act for the vout_base.  Will attempt to output unless
 * the thing is completely closed.
 *
 * Should by now not be any buffered output -- but if there is, that is
 * discarded before the close reason is output.
 *
 * Any actual output may be done when the vout_base is finally closed.
 */
static void
uty_vout_close_reason(vio_vf vf, const char* reason)
{
  if ((vf->vout_state == vf_closed) || (reason == NULL) || (*reason == '\0'))
    return ;

  vio_fifo_clear(vf->obuf, true) ;      /* clear any markers, too       */

  if (vf->vout_type < VOUT_SPECIALS)
    assert(vf->vfd != NULL) ;           /* make sure                    */

  switch(vf->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid VOUT_NONE") ;
      break ;

    case VOUT_TERM:
      uty_term_close_reason(vf, reason) ;
      break ;

    case VOUT_VTYSH:
      break ;

    case VOUT_FILE:
    case VOUT_PIPE:
    case VOUT_SH_CMD:
      break ;

    case VOUT_CONFIG:
      break ;

    case VOUT_DEV_NULL:
      break ;

    case VOUT_STDOUT:
      fprintf(stdout, "%% %s\n", reason) ;
      break ;

    case VOUT_STDERR:
      fprintf(stderr, "%% %s\n", reason) ;
      break ;

    default:
      zabort("unknown VOUT type") ;
  } ;
} ;

/*==============================================================================
 * vio_vf level operations
 */

/*------------------------------------------------------------------------------
 * Create and initialise a new vio_vf structure.
 *
 * There are no errors, yet.
 *
 * This leaves most things unset/NULL/false.  Caller will need to:
 *
 *   - uty_vin_push() and/or uty_vout_push()
 *
 *   - once those are done, the following optional items remain to be set
 *     if they are required:
 *
 *       read_timeout     -- default = 0     => no timeout
 *       write_timeout    -- default = 0     => no timeout
 *
 *   - for pipes the child state needs to be set, and for out pipes the return
 *     state needs to be set in this vio_vf (the master) and in the next
 *     vout (the slave).
 *
 * NB: if there is no fd for this vio_vf, it should be set to -1, and the
 *     type (recommend vfd_none) and io_type are ignored.
 *
 *     A VTY_STDOUT or a VTY_STDERR (output only, to the standard I/O) can
 *     be set up without an fd.
 *
 *     A VTY_CONFIG_READ can be set up with the fd of the input file, but
 *     MUST be type = vfd_file and io_type = vfd_io_read -- because the fd
 *     is for input only.  The required VOUT_STDERR will be set by
 *     uty_vout_push().
 *
 * NB: if the parent vio is blocking, then the vf will be vfd_io_ps_blocking,
 *     and so will any vfd.  An individual vf may be set blocking by setting
 *     vfd_io_blocking or vfd_io_ps_blocking in the io_type.
 *
 *     For vfd_io_ps_blocking, the vfd stuff handles all files, pipes etc.
 *     non-blocking.  The vf simulates blocking by local pselect.  As far as
 *     the vfd level is concerned, once the file, pipe etc. is open, there is
 *     no difference between blocking and non-blocking except that blocking
 *     vfd (of either type) are not allowed to set read/write ready.
 *
 * NB: the name is XSTRDUP() into the vio -- so the caller is responsible for
 *     disposing of its copy, if required.
 */
extern vio_vf
uty_vf_new(vty_io vio, const char* name, int fd, vfd_type_t type,
                                              vfd_io_type_t io_type)
{
  vio_vf  vf ;

  VTY_ASSERT_LOCKED() ;

  vf = XCALLOC (MTYPE_VTY, sizeof(struct vio_vf)) ;

  /* Zeroising the structure has set:
   *
   *   vio              = X     -- set below
   *   name             = X     -- set below
   *
   *   vin_type         = VIN_NONE            -- see uty_vin_push()
   *   vin_state        = vf_closed           -- see uty_vin_push()
   *   vin_next         = NULL                -- see uty_vin_push()
   *
   *   context          = NULL                -- see uty_vin_new_context()
   *
   *   cli              = NULL  -- no CLI, yet
   *
   *   ibuf             = NULL  -- none, yet  -- see uty_vin_push()
   *   cl               = NULL  -- none, yet  -- see uty_vin_push()
   *   line_complete    = false               -- see uty_vin_push()
   *   line_number      = 0     -- nothing yet
   *   line_step        = 0                   -- see uty_vin_push()
   *
   *   vout_type        = VOUT_NONE           -- see uty_vout_push()
   *   vout_state       = vf_closed           -- see uty_vout_push()
   *   vout_next        = NULL                -- see uty_vout_push()
   *
   *   obuf             = NULL  -- none       -- see uty_vout_push()
   *
   *   depth_mark       = 0                   -- see uty_vout_push()
   *
   *   blocking         = X                   -- see below
   *
   *   vfd              = NULL  -- no vfd, yet
   *
   *   read_timeout     = 0     -- none
   *   write_timeout    = 0     -- none
   *
   *   child            = NULL  -- none       -- see uty_pipe_read/write_open()
   *
   *   pr_state         = vf_closed           -- no pipe return vfd
   *                                          -- see uty_pipe_read/write_open()
   *   pr_vfd           = NULL  -- no vfd     -- ditto
   *   pr_timeout       = 0     -- none       -- ditto
   *
   *   ps_state         = vf_closed           -- no pipe stderr return vfd
   *                                          -- see uty_pipe_read/write_open()
   *   ps_vfd           = NULL  -- no vfd     -- ditto
   *   ps_timeout       = 0     -- none       -- ditto
   *   ps_buf           = NULL  -- none, yet
   */
  confirm((VIN_NONE == 0) && (VOUT_NONE == 0)) ;
  confirm(vf_closed == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;

  if (vio->blocking)
    io_type |= vfd_io_ps_blocking ;     /* inherit blocking state       */

  vf->vio      = vio ;
  vf->blocking = (io_type & (vfd_io_blocking | vfd_io_ps_blocking)) != 0 ;

  if (name != NULL)
    vf->name = XSTRDUP(MTYPE_VTY_NAME, name) ;

  if (fd >= 0)
    vf->vfd = vio_vfd_new(fd, type, io_type, vf) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * Close the read side of the given vio_vf -- if can.
 *
 * Read closes the vio_vfd -- if the vio_vfd was read-only, this will fully
 * close it, and the vio_vfd will have been freed.
 *
 * For not-final close, if there is any other related I/O, then waits until
 * that has completed.  For example, with a VIN_PIPE:
 *
 *   * waits for any return input to complete, and pushes it to the relevant
 *     vout.
 *
 *   * waits to collect the child, and its termination state.
 *
 * This means that a not-final close may return errors.  For not-blocking vf
 * may return waiting state.  For blocking vf, may block and may later return
 * timeout error.
 *
 * For a final close, if there is any related I/O then will attempt to complete
 * it -- but will give up if would block.  I/O errors on final close are
 * ignored.  Final close always returns CMD_SUCCESS.
 *
 * Returns:  CMD_SUCCESS  -- closed
 *           CMD_WAITING  -- waiting to complete close    <=> non-blocking
 *                                                        <=> not "final"
 *           CMD_IO_ERROR -- error or timeout             <=> try again
 *                                                        <=> not "final"
 */
static cmd_return_code_t
uty_vf_read_close(vio_vf vf, bool final)
{
  cmd_return_code_t  ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  ret = CMD_SUCCESS ;

  if (vf->vin_state == vf_closed)
    return ret ;                /* quit if already closed               */

  if (vf->vin_state == vf_open)
    vf->vin_state = vf_end ;    /* don't try to read any more !         */
  else
    qassert(vf->vin_state == vf_end) ;

  /* Do the vfd level read close and mark the vf no longer read_open    */
  if (vf->vin_type < VIN_SPECIALS)
    vf->vfd = vio_vfd_read_close(vf->vfd) ;

  /* Now the vin_type specific clean up.                                */
  switch(vf->vin_type)
  {
    case VIN_NONE:
      zabort("invalid VIN_NONE") ;
      break ;

    case VIN_TERM:
      ret = uty_term_read_close(vf, final) ;
      break ;

    case VIN_VTYSH:
       zabort("tba VIN_VTYSH") ;
       break ;

    case VIN_CONFIG:
      ret = uty_config_read_close(vf, final) ;
      break ;

    case VIN_FILE:
      ret = uty_file_read_close(vf, final) ;
      break ;

    case VIN_PIPE:
      ret = uty_pipe_read_close(vf, final) ;
      break ;

    case VIN_DEV_NULL:
      ret = CMD_SUCCESS ;
      break ;

    default:
      zabort("unknown VIN type") ;
  } ;

  if ((ret == CMD_SUCCESS) || final)
    {
      vf->vin_state = vf_closed ;
      assert(vf->pr_state == vf_closed) ;

      ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Close the write side of the given vio_vf, if can.
 *
 * Discards anything beyond the current end_mark, and clears the end_mark.
 *
 * Pushes any outstanding output, and if is pipe will attempt to collect
 * the child.  On not-final close, if cannot complete everything, will return
 * CMD_WAITING for non-blocking, or block (and may time out).  On final close,
 * will do as much as possible without blocking, and will then close even if
 * there is outstanding output or child has not been collected.
 *
 * For not-final close, for example, with a VOUT_PIPE:
 *
 *   * waits for any return input to complete, and pushes it to the relevant
 *     vout.
 *
 *   * waits to collect the child, and its termination state.
 *
 * This means that a not-final close may return errors.
 *
 * For a final close, if there is any related I/O then will attempt to complete
 * it -- but will give up if would block.  I/O errors on final close are
 * ignored.  Will close the vfd and return CMD_SUCCESS.
 *
 * If this is the vout_base, unless "final", does NOT actually close the vf or
 * the vfd -- so the vout_base will still work and is left vf_open.
 * The effect is, essentially, to try to empty out any buffers, but not to
 * do anything that would prevent further output.  This is used so that a
 * command loop can close the vout_base in the usual way, waiting until all
 * output is flushed, but when uty_close() is finally called, it can output
 * any close reason there is to hand.
 *
 * Returns:  CMD_SUCCESS  -- closed
 *           CMD_WAITING  -- waiting to complete close    <=> non-blocking
 *                                                        <=> not "final"
 *           CMD_IO_ERROR -- error or timeout             <=> try again
 *                                                        <=> not "final"
 *
 * NB: must not have open vins at this or a higher level in the stack.
 *
 * NB: does not at this stage discard the obuf.
 */
static cmd_return_code_t
uty_vf_write_close(vio_vf vf, bool final)
{
  cmd_return_code_t  ret ;
  bool base ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  ret = CMD_SUCCESS ;

  if (vf->vout_state == vf_closed)
    return ret ;                        /* quit if already closed       */

  base = (vf == vf->vio->vout_base) ;

  /* Must always close vin before closing vout at the same level.
   *
   * Note that cannot currently be a pipe return slave, because if was
   * slave to a VOUT_PIPE/VOUT_SH_CMD that vout must have been closed
   * already.
   */
  qassert( (vf->vio->vin_depth < vf->vio->vout_depth)
                || ((vf->vio->vin_depth == 0) && (vf->vio->vout_depth == 0)) ) ;
  qassert(vf->vin_state == vf_closed) ;
  qassert(vf->vio->obuf == vf->obuf) ;

  /* If there is anything in the obuf beyond the end_mark, then it is
   * assumed to be surplus to requirements, and we clear the end_mark.
   */
  vio_fifo_back_to_end_mark(vf->obuf, false) ;

  /* The vout_type specific close functions will attempt to write
   * everything away.
   *
   * If "final", will only keep going until would block -- at which point will
   * bring everything to a shuddering halt.
   *
   * NB: at this point vout_state is not vf_closed.
   */
  switch(vf->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid VOUT_NONE") ;
      break ;

    case VOUT_TERM:
      ret = uty_term_write_close(vf, final) ;
      break ;

    case VOUT_VTYSH:
      break ;

    case VOUT_FILE:
    case VOUT_CONFIG:
      ret = uty_file_write_close(vf, final) ;
      break ;

    case VOUT_PIPE:
    case VOUT_SH_CMD:
      ret = uty_pipe_write_close(vf, final) ;
      break ;

    case VOUT_DEV_NULL:
    case VOUT_STDOUT:
    case VOUT_STDERR:
      ret = CMD_SUCCESS ;
      break ;

    default:
      zabort("unknown VOUT type") ;
  } ;

  if (((ret == CMD_SUCCESS) && !base) || final)
    {
      if (vf->vout_type < VOUT_SPECIALS)
        vf->vfd = vio_vfd_close(vf->vfd) ;
      else
        assert(vf->vfd == NULL) ;

      vf->vout_state = vf_closed ;

      ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Free the given vio_vf structure and all its contents.
 *
 * Expects the vfd to already have been closed, but will close it if not.
 *
 * Expects any cli to be closed, but will close it if not.
 *
 * Assumes has been removed from any and all lists !
 */
static vio_vf
uty_vf_free(vio_vf vf)
{
  assert((vf->vin_state == vf_closed) && (vf->vout_state == vf_closed)
                                      && (vf->pr_state == vf_closed)
                                      && (vf->ps_state == vf_closed)) ;

  XFREE(MTYPE_VTY_NAME, vf->name) ;

  assert(vf->cli == NULL) ;

  vf->ibuf   = vio_fifo_free(vf->ibuf) ;
  vf->cl     = qs_reset(vf->cl, free_it) ;
  vf->obuf   = vio_fifo_free(vf->obuf) ;

  vf->context = cmd_context_free(vf->context, false) ;  /* not a copy   */

  vf->vfd    = vio_vfd_close(vf->vfd) ;         /* for completeness     */
  vf->pr_vfd = vio_vfd_close(vf->pr_vfd) ;      /* for completeness     */
  vf->ps_vfd = vio_vfd_close(vf->ps_vfd) ;      /* for completeness     */

  vf->ps_buf = vio_fifo_free(vf->ps_buf) ;

  XFREE(MTYPE_VTY, vf) ;

  return NULL ;
} ;

/*==============================================================================
 * vio_vf level read/write ready and timeout setting
 */

/*------------------------------------------------------------------------------
 * Set required read ready state.  Applies the current read timeout.
 *
 * Forces off if:   vf->vin_state != vf_open
 *
 * Does nothing if: vf->vin_state == vf_closed
 *              or: vf->vfd == NULL
 *
 * NB: must NOT be a "blocking" vf
 */
extern void
uty_vf_set_read(vio_vf vf, on_off_b how)
{
  if (vf->vin_state != vf_open)
    how = off ;

  if (vf->vin_state != vf_closed)
    vio_vfd_set_read(vf->vfd, how, vf->read_timeout) ;
} ;

/*------------------------------------------------------------------------------
 * Set required read ready timeout -- if already read on, restart it.
 *
 * If this is a blocking vf, will set the timeout value, but since can never
 * be "read on", will never attempt to restart any timer !
 */
extern void
uty_vf_set_read_timeout(vio_vf vf, vty_timer_time read_timeout)
{
  vf->read_timeout = read_timeout ;

  if (!vf->blocking)
    uty_vf_set_read(vf, on) ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready state.  Applies the current write timeout.
 *
 * Forces off if:   vf->vout_state != vf_open
 *
 * Does nothing if: vf->vout_state == vf_closed
 *              or: vf->vfd == NULL
 *
 * NB: must NOT be a "blocking" vf
 */
extern void
uty_vf_set_write(vio_vf vf, on_off_b how)
{
  if (vf->vout_state != vf_open)
    how = off ;

  if (vf->vout_state != vf_closed)
    vio_vfd_set_write(vf->vfd, how, vf->write_timeout) ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready timeout -- if already write on, restart it.
 *
 * If this is a blocking vf, will set the timeout value, but since can never
 * be "write on", will never attempt to restart any timer !
 */
extern void
uty_vf_set_write_timeout(vio_vf vf, vty_timer_time write_timeout)
{
  vf->write_timeout = write_timeout ;

  if (!vf->blocking)
    uty_vf_set_write(vf, on) ;
} ;

/*==============================================================================
 * I/O error and timeout reporting.
 *
 * Posts error information and locus to the vio, which is signalled by a
 * CMD_IO_ERROR return code.
 */

/*------------------------------------------------------------------------------
 * Dealing with an I/O error or time-out on the given vio_vf:
 *
 *   * sets the vf->vin_state, vf->vout_state, vf->pr_state or vf->ps_state to
 *     vf_end
 *
 *     Note that this does not signal the error -- it means that any further
 *     I/O on this vin/vout is to be avoided.
 *
 *     Errors and timeouts in either vin or vout also force any pipe return
 *     and/or pipe stderr return to vf_end.
 *
 *     Errors and timeouts in either pipe return or pipe stderr return also
 *     force all of vin, vout, pipe return and pipe stderr return to vf_end.
 *
 *   * if vio is a "monitor", turn that off, *before* issuing log message
 *     wrt to either the vin_base or the vout_base.
 *
 *   * produce suitable log message.
 *
 *   * insert suitable message in vio->ebuf
 *
 *   * set the vio->err_depth to 0 if this is an error in vin_base or
 *     vout_base, or to no more than 1 for errors anywhere else.
 *
 *   * signals CMD_IO_ERROR to the command loop -- which is how the pselect()
 *     process communicates with the command loop.
 *
 * Returns:  CMD_IO_ERROR -- which may be returned to the command loop, as
 *                           well as the vio->signal.
 */
extern cmd_return_code_t
uty_vf_error(vio_vf vf, vio_err_type_t err_type, int err)
{
  vty_io vio = vf->vio ;

  VTY_ASSERT_LOCKED() ;

  /* Set the error level and, if required, turn off any log monitoring *before*
   * issuing any logging message.
   */
  if  ((vf == vio->vin_base) || (vf == vio->vout_base))
    {
      vio->err_depth = 0 ;
      uty_set_monitor(vio, off) ;
    }
  else
    {
      if (vio->err_depth > 1)
        vio->err_depth = 1 ;
    } ;

  /* Set vf->vin_state, vf->vout_state or vf->pr_state, as required.
   */
  switch (err_type)
    {
      case verr_none:
        zabort("verr_io_none invalid") ;
        break ;

      case verr_io_vin:
      case verr_to_vin:
        qassert(vf->vin_state != vf_closed) ;

        vf->vin_state = vf_end ;
        break ;

      case verr_io_vout:
      case verr_to_vout:
        qassert(vf->vout_state != vf_closed) ;

        vf->vout_state = vf_end ;
        break ;

      case verr_io_pr:
      case verr_to_pr:
        qassert(vf->pr_state != vf_closed) ;
        uty_pipe_return_stop(vf) ;
        break ;

      case verr_io_ps:
      case verr_to_ps:
        qassert(vf->ps_state != vf_closed) ;
        uty_pipe_return_stop(vf) ;
        break ;

      default:
        zabort("unknown verr_xxxx") ;
    } ;

  /* If there is a pipe return (still active), then stop it now -- no point
   * continuing after main vin/vout has failed.
   *
   * Ditto pipe stderr return.
   */
  if (vf->pr_state == vf_open)
    vf->pr_state = vf_end ;

  if (vf->ps_state == vf_open)
    vf->ps_state = vf_end ;

  /* Log the error and add an error message to the vio->ebuf.
   */
  zlog_warn("%s", uty_error_message(vf, err_type, err, true).str) ;

  vio_fifo_printf(uty_cmd_get_ebuf(vio), "\n%s\n",
                  uty_error_message(vf, err_type, err, false).str) ;

  /* Signal to the command loop, if required, and return CMD_IO_ERROR.
   *
   * One or both will be collected in the command loop "hiatus" and dealt
   * with -- it does not matter if both arrive.
   */
  uty_cmd_signal(vio, CMD_IO_ERROR) ;

  return CMD_IO_ERROR ;
} ;

/*------------------------------------------------------------------------------
 * Construct error message for given I/O or time-out error
 */
extern verr_mess_t
uty_error_message(vio_vf vf, vio_err_type_t err_type, int err, bool log)
{
  QFB_QFS(verr_mess, qfs) ;

  const char* name ;
  const char* where ;
  const char* what ;
  bool   vout ;
  bool   io ;
  int    fd ;

  VTY_ASSERT_LOCKED() ;

  vout = false ;
  fd   = -1 ;
  what = NULL ;

  switch (err_type & verr_mask)
    {
      case verr_vin:
        vout = false ;
        what = "read" ;
        if (log && (vf->vfd != NULL))
          fd = vio_vfd_fd(vf->vfd) ;
        break ;

      case verr_vout:
        vout = true ;
        what = "write" ;
        if (log && (vf->vfd != NULL))
          fd = vio_vfd_fd(vf->vfd) ;
        break ;

      case verr_pr:
        vout = true ;
        what = "pipe return" ;
        if (log)
          fd = vio_vfd_fd(vf->pr_vfd) ;
        break ;

      case verr_ps:
        vout = (vf->vout_state != vf_closed) ;
        what = "stderr return" ;
        if (log)
          fd = vio_vfd_fd(vf->ps_vfd) ;
        break ;
    } ;

  name  = vf->name ;
  where = NULL ;

  io    = (err_type & verr_to) == 0 ;
  confirm((verr_to != 0) && (verr_io == 0)) ;

  if (vout)
    {
      switch (vf->vout_type)
        {
          case VOUT_NONE:
            zabort("VOUT_NONE invalid") ;
            break ;

          case VOUT_TERM:
            where = "Terminal" ;
            if (!log)
              name = NULL ;
            break ;

          case VOUT_VTYSH:
            where = "VTY Shell" ;
            if (!log)
              name = NULL ;
            break ;

          case VOUT_FILE:
            where = "File" ;
            break ;

          case VOUT_PIPE:
            where = "Pipe" ;
            break ;

          case VOUT_CONFIG:
            where = "Configuration file" ;
            break ;

          case VOUT_DEV_NULL:
            where = "/dev/null" ;
            break ;

          case VOUT_SH_CMD:
            where = "Command" ;
            break ;

          case VOUT_STDOUT:
            where = "stdout" ;
            break ;

          case VOUT_STDERR:
            where = "stderr" ;
            break ;

          default:
            zabort("unknown vout_type") ;
            break ;
        } ;
    }
  else
    {
      switch (vf->vin_type)
        {
          case VIN_NONE:
            zabort("VIN_NONE invalid") ;
            break ;

          case VIN_TERM:
            where = "Terminal" ;
            if (!log)
              name = NULL ;
            break ;

          case VIN_VTYSH:
            where = "VTY Shell" ;
            if (!log)
              fd  = vio_vfd_fd(vf->vfd) ;
            break ;

          case VIN_FILE:
            where = "File" ;
            break ;

          case VIN_PIPE:
            where = "Pipe" ;
            break ;

          case VIN_CONFIG:
            where = "Configuration file" ;
            break ;

          case VIN_DEV_NULL:
            where = "/dev/null" ;
            break ;

          default:
            zabort("unknown vin_type") ;
            break ;
        } ;
    } ;

  qfs_printf(qfs, "%s %s %s", where, what, io ? "I/O error" : "time-out") ;

  if (name != NULL)
    qfs_printf(qfs, " '%s'", name) ;

  if (fd >= 0)
    qfs_printf(qfs, " (fd=%d)", fd) ;

  if (io && (err != 0))
    qfs_printf(qfs, ": %s", errtoa(err, 0).str) ;

  qfs_term(qfs) ;
  return verr_mess ;
} ;

/*==============================================================================
 * Child care.
 *
 * Management of vio_child objects and the vio_childer_list.
 *
 * When child is registered it is placed on the vio_childer_list.  It remains
 * there until it is "collected", that is until a waitpid() has returned a
 * "report" for the child.
 *
 * When SIGCHLD events go off, the return code for the child is collected,
 * and saved until the pipe I/O wants it.
 *
 * When pipe I/O is closing a pipe and requires the return code from the
 * child, if the child has not already been collected (after a SIGCHLD),
 * then:
 *
 *   * for blocking vf (configuration reading), uty_child_collect() is used,
 *     in which a mini-pselect is used to wait and time-out.  A SIGCHLD will
 *     wake up the CLI pthread (or the only thread if not multi-pthreaded)
 *     and, if required, that will wake the waiting pthread by sending a
 *     Quagga SIG_INTERRUPT to it.
 *
 *   * for non-blocking vf, utf_child_awaited() is used, in which a time-out
 *     timer is set, in case the SIGCHLD does not arrive in good time.  When
 *     the child is collected or the timer goes off, a uty_cmd_signal() is
 *     sent.
 *
 * When the parent sees that the child is "collected" or "overdue", it can
 * examine any report and then dismiss the child.
 *
 * NB: time-out while waiting to collect a child is not treated as an error,
 *     here -- so no uty_vf_error() is signalled.
 *
 * When a parent "dismisses" a child, if it has not yet been collected it is
 * "smacked" -- kill(SIGTERM) -- but kept on the register until it is
 * collected.  When a child which has been collected is dismissed it is freed.
 *
 * At "curtains" the register may contain children which have been dismissed,
 * but not yet collected.  Should NOT contain any children with living parents.
 * All children remaining on the register are smacked, and all with no living
 * parents are freed.  (This could leave children on the register, but avoids
 * the possibility of a dangling reference from a parent.)

 * The state of a vio_child object includes:
 *
 *   parent     -- pointer to the parent vf, if any -- set when the child is
 *                 registered.
 *
 *                 A NULL parent pointer <=> the child is an orphan.
 *
 *                 At "curtains" all children are orphaned.
 *
 *   collected  -- this is set true when the child termination code is picked
 *                 up (by uty_waitpid).  It is forced true at "curtains".
 *
 *                 When a child is collected it is removed from the register.
 *
 *                 Once removed from the register, a child is the responsibility
 *                 of its parent, if any.
 *
 *                 When an orphan is removed from the register it can be freed.
 *
 *   awaited    -- this is true iff the parent is non-blocking and is now
 *                 waiting to collect the child.
 *
 *                 "awaited" <=> there is a timer running.
 *
 *                 When the timer goes off "awaited" is cleared, but the timer
 *                 still exists (but is not running).
 *
 *   overdue    -- this is set true if times out waiting for child to be
 *                 collected, or can wait no longer ("final" close).
 */
static void uty_child_collected(vio_child child, int report) ;
static vty_timer_time vty_child_overdue(vio_timer timer, void* action_info) ;
static void uty_child_signal_parent(vio_child child) ;
static void uty_child_free(vio_child child) ;
static pid_t uty_waitpid(pid_t for_pid, int* p_report) ;

/*------------------------------------------------------------------------------
 * Set the vty_child_signal_nexus() -- if required.
 *
 * The SIGCHLD signal will wake up the cli thread.  For blocking I/O (reading
 * configuration file) may need to wake up another thread, for which the
 * SIG_INTERRUPT signal is used.
 *
 * Note that this is set/cleared under VTY_LOCK() and its own mutex.  This
 * allows it to be read under its own mutex and/or VTY_LOCK().
 */
extern void
uty_child_signal_nexus_set(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vio->blocking) ;

  if (!vty_is_cli_thread())
    {
      qpt_mutex_lock(vty_child_signal_mutex) ;

      vty_child_signal_nexus = qpn_find_self() ;

      qpt_mutex_unlock(vty_child_signal_mutex) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * If there is a nexus to signal, do that.
 */
extern void
vty_child_signal_nexus_signal(void)
{
  qpt_mutex_lock(vty_child_signal_mutex) ;

  if (vty_child_signal_nexus != NULL)
    qpt_thread_signal(vty_child_signal_nexus->thread_id,
                                       vty_child_signal_nexus->pselect_signal) ;

  qpt_mutex_unlock(vty_child_signal_mutex) ;
}

/*------------------------------------------------------------------------------
 * Clear the vty_child_signal_nexus() -- if required.
 *
 * Note that this is set/cleared under VTY_LOCK() and its own mutex.  This
 * allows it to be read under its own mutex and/or VTY_LOCK().
 */
extern void
uty_child_signal_nexus_clear(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->blocking) ;

  if (!vty_is_cli_thread())
    {
      qpt_mutex_lock(vty_child_signal_mutex) ;

      vty_child_signal_nexus = NULL ;

      qpt_mutex_unlock(vty_child_signal_mutex) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * New child.
 *
 * NB: must register child promptly (under VTY_LOCK, at same time as fork)
 *     in order to avoid missing the SIG_CHLD !
 */
extern vio_child
uty_child_register(pid_t pid, vio_vf parent)
{
  vio_child child ;

  VTY_ASSERT_LOCKED() ;

  child = XCALLOC(MTYPE_VTY, sizeof(struct vio_child)) ;

  /* Zeroising has set:
   *
   *   list         -- NULLs         -- added to vio_childer_list, below
   *
   *   parent       -- NULL          -- parent vf set, below
   *
   *   pid          -- X             -- child pid set below
   *   collected    -- false         -- not yet collected
   *   report       -- X             -- not relevant until collected
   *
   *   awaited      -- false         -- not waiting for child
   *   overdue      -- false         -- child not overdue
   *   timer        -- NULL          -- no waiting timer set
   */
  child->parent  = parent ;
  child->pid     = pid ;

  sdl_push(vio_childer_list, child, list) ;

  return child ;
} ;

/*------------------------------------------------------------------------------
 * Set waiting for child to be collected -- if not already set.
 *
 * This is for !vf->blocking.
 *
 * If not already waiting for child, set timer.
 * If is already waiting, leave existing timer running.
 */
extern void
uty_child_awaited(vio_child child, vty_timer_time timeout)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  qassert(child->parent != NULL) ;
  qassert(!child->parent->blocking) ;

  if (child->awaited)
    {
      qassert(child->timer != NULL) ;
    }
  else
    {
      child->awaited = true ;

      if (child->timer == NULL)
        child->timer = vio_timer_init_new(NULL, vty_child_overdue, child) ;

      vio_timer_set(child->timer, timeout) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Clear waiting for child to be collected, if that is set,  and discard
 * any timer.
 *
 * NB: child may already be orphaned.
 *
 * NB: timer is not discarded when goes off, so will still exist, even
 *     though is no longer "awaited".
 *
 * If has a timer, then must be in the CLI thread -- which will be, because
 * all !vf->blocking child handling is done in the CLI thread.
 */
static void
uty_child_not_awaited(vio_child child)
{
  VTY_ASSERT_LOCKED() ;

  if (child->awaited)   /* "awaited" => not orphan & timer running      */
    {
      qassert(child->parent != NULL) ;
      qassert(!child->parent->blocking) ;
      qassert(child->timer  != NULL) ;

      child->awaited = false ;
    } ;

  if (child->timer != NULL)     /* => in CLI thread                     */
    {
      VTY_ASSERT_CLI_THREAD() ;

      child->timer   = vio_timer_reset(child->timer, free_it) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * See if parent can collect child -- directly.
 *
 * This is for vf->blocking, or for "final" !vf->blocking.
 *
 * If can, will collect now -- marking collected and taking off the
 * vio_childer_list.
 *
 * If cannot immediately collect, if final mark overdue and return.
 *
 * Otherwise wait for up to timeout seconds for a suitable SIGCHLD or related
 * wake-up signal.
 *
 * Returns:  true <=> collected
 *          false  => timed out or final
 */
extern bool
uty_child_collect(vio_child child, vty_timer_time timeout, bool final)
{
  bool first ;

  VTY_ASSERT_LOCKED() ;

  qassert(child->parent != NULL) ;
  qassert(child->parent->blocking || final) ;
  qassert(child->pid > 0) ;

  uty_child_not_awaited(child) ;        /* clear flag & timer           */

  first = true ;
  while (1)
    {
      pid_t  pid ;
      int    report ;

      qps_mini_t qm ;
      sigset_t*  sig_mask = NULL ;

      /* If already collected, or succeed in collecting, we are done.   */
      if (child->collected)
        return true ;                   /* already collected            */

      pid = uty_waitpid(child->pid, &report) ;

      if (pid == child->pid)
        {
          uty_child_collected(child, report) ;  /* not orphan           */
          return true ;                         /* collected            */
        } ;

      /* If "final" or got an error, mark child overdue and give up     */
      if (final || (pid < 0))
        {
          child->overdue = true ;
          return false ;                /* overdue                      */
        } ;

      /* Need to wait -- if this is the first time through, prepare for
       * that.
       */
      if (first)
        {
          qps_mini_set(qm, -1, 0, 6) ;

          if (vty_is_cli_thread())
            sig_mask  = NULL ;
          else
            sig_mask  = vty_child_signal_nexus->pselect_mask ;

          first = false ;
        } ;

      /* Wait on pselect.                                               */
      if (qps_mini_wait(qm, sig_mask, true) == 0)
        final = true ;          /* timed out => now final               */
  } ;
} ;

/*------------------------------------------------------------------------------
 * Dismiss child
 *
 * If already collected, free the child.
 *
 * If not collected, smack but leave to be collected in due course (or free
 * now at "curtains").
 *
 * This may be called in any thread -- but must be CLI thread if there is
 * a timer, that is: must be CLI thread if this is/was non-blocking.
 *
 * When the register is closed, is done in CLI thread.
 *
 * NB: child will have been freed if was collected or this is "curtains".
 */
extern void
uty_child_dismiss(vio_child child, bool curtains)
{
  VTY_ASSERT_LOCKED() ;

  uty_child_not_awaited(child) ;

  if (child->parent != NULL)
    {
      child->parent->child = NULL ;
      child->parent        = NULL ;     /* orphan from now on   */
    } ;

  if (child->collected)
    uty_child_free(child) ;
  else
    {
      qassert(child->pid > 0) ;

      kill(child->pid, SIGKILL) ;       /* hasten the end       */

      if (curtains)
        uty_child_collected(child, 0) ; /* orphan: so free it   */
    } ;
} ;

/*------------------------------------------------------------------------------
 * At "curtains" -- dismiss any children left in the register.
 */
extern void
vty_child_close_register(void)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  while (vio_childer_list != NULL)
    uty_child_dismiss(vio_childer_list, true) ; /* curtains     */
} ;

/*------------------------------------------------------------------------------
 * See whether any children are ready for collection, and check each one
 * against the register.
 *
 * Perform waitpid( , , WNOHANG) until no child is returned, and process
 * each one against the register.
 *
 * The is done when a SIGCHLD is routed through the event mechanism.
 *
 * If another SIGCHLD occurs while this is being done, that will later cause
 * another call of this function -- which may find that there are no children
 * to be collected.
 *
 * This is also done when about to block waiting for a child.
 *
 * Set any children that can be collected, collected and signal to any parents
 * that their children are now ready.
 */
extern void
uty_sigchld(void)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  while (1)
    {
      vio_child child ;
      pid_t     pid ;
      int       report ;

      pid = uty_waitpid((pid_t)-1, &report) ;

      if (pid <= 0)
        break ;

      child = vio_childer_list ;
      while (1)
        {
          if (child == NULL)
            {
              zlog_err("waitpid(-1) returned pid %d, which is not registered",
                                                                          pid) ;
              break ;
            } ;

          if (child->pid == pid)
            {
              /* Remove child from register and set "collected".  Turn off
               * any timer and clear "awaited".
               *
               * If child is not orphaned: set report and signal parent if
               * required.
               *
               * Otherwise: can free the child now.
               */
              uty_child_collected(child, report) ;

              break ;
            } ;

          child = sdl_next(child, list) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Have collected a child.
 *
 *   * if a parent is waiting, signal them
 *
 *   * clear any awaited state and discard any timer.
 *
 *   * remove from the register
 *
 *   * if the child has a parent, set the child's report (the parent is now
 *     responsible for the child).
 *
 *     otherwise, can free the child now.
 */
static void
uty_child_collected(vio_child child, int report)
{
  qassert(!child->collected) ;          /* can only collect once        */

  if (child->timer != NULL)
    VTY_ASSERT_CLI_THREAD() ;           /* must be to clear timer       */

  if (child->awaited)
    uty_child_signal_parent(child) ;
  uty_child_not_awaited(child) ;        /* clear flag and timer         */

  child->collected = true ;             /* remove from register         */
  sdl_del(vio_childer_list, child, list) ;

  if (child->parent == NULL)
    uty_child_free(child) ;
  else
    child->report = report ;
} ;

/*------------------------------------------------------------------------------
 * Set child as overdue -- vio_timer action routine.
 */
static vty_timer_time
vty_child_overdue(vio_timer timer, void* action_info)
{
  vio_child child ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  child = action_info ;
  assert(timer == child->timer) ;

  if (child->awaited)           /* ignore if no longer awaited  */
    {
      uty_child_signal_parent(child) ;  /* clears "awaited"     */

      child->overdue = true ;
    } ;

  VTY_UNLOCK() ;

  return 0 ;            /* stop timer   */
} ;

/*------------------------------------------------------------------------------
 * Signal that child is ready -- collected or overdue -- clears "awaited".
 *
 * Must be "awaited" -- so not "blocking"
 */
static void
uty_child_signal_parent(vio_child child)
{
  qassert(child->awaited) ;
  qassert(child->parent != NULL) ;
  qassert(!child->parent->vio->blocking) ;

  uty_cmd_signal(child->parent->vio, CMD_SUCCESS) ;

  child->awaited = false ;
} ;

/*------------------------------------------------------------------------------
 * Free the child -- caller must ensure that any parent has dismissed the child,
 * and that it is collected (so not on the vio_childer_list) and that there
 * is no timer running.
 */
static void
uty_child_free(vio_child child)
{
  VTY_ASSERT_LOCKED() ;

  if (child != NULL)
    {
      qassert(child->collected) ;
      qassert(child->parent == NULL) ;
      qassert(child->timer  == NULL) ;

      XFREE(MTYPE_VTY, child) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Wrapper for waitpid() -- deal with and log any errors.
 */
static pid_t
uty_waitpid(pid_t for_pid, int* p_report)
{
  pid_t pid ;

  while (1)
    {
      pid = waitpid(for_pid, p_report, WNOHANG) ;

      if (pid == 0)
        return pid ;            /* nothing to be had    */

      if (pid >  0)             /* got an answer        */
        {
          if ((for_pid < 0) || (pid == for_pid))
            return pid ;

          /* This is absolutely impossible.  If for_pid is > 0, then
           * the only valid response > 0 is for_pid !!
           *
           * Don't know what to do with this, but treating it as a
           * "nothing to be had" return seems safe.
           */
          zlog_err("waitpid(%d) returned pid=%d", for_pid, pid) ;

          return -1 ;
        } ;

      if (errno == EINTR)
        continue ;              /* loop on "Interrupted"                */

      /* Got an error other than EINTR, which is almost impossible...
       *
       *   (1) ECHILD means that the given pid is not a child or there are
       *       no children.
       *
       *       This is possible if this is called following a SIGCHLD, and
       *       all children have been collected between the signal and the
       *       uty_sigchld().
       *
       *       Note that only call uty_waitpid() for a specific child if it
       *       is known to not yet have been collected.
       *
       *   (2) only other known error is EINVAL -- invalid options...
       *       absolutely impossible.
       */
      if ((errno != ECHILD) || (for_pid > 0))
        zlog_err("waitpid(%d) returned %s", for_pid, errtoa(errno, 0).str) ;

      return -1 ;
    } ;
} ;

/*==============================================================================
 * VTY Listener(s)
 *
 * Have listeners for VTY_TERMINAL and VTY_SHELL_SERVER types of VTY.
 */

/* List of listeners so can tidy up.                                    */
static vio_listener vty_listeners_list  = NULL ;

/*------------------------------------------------------------------------------
 * Open VTY listener(s) for VTY_TERMINAL and VTY_SHELL_SERVER.
 *
 *   addr   -- address ) to listen for VTY_TERMINAL connections
 *   port   -- port    )
 *   path   -- path for VTY_SHELL_SERVER connections -- if VTYSH_ENABLED
 */
extern void
uty_open_listeners(const char *addr, unsigned short port, const char *path)
{
  VTY_ASSERT_LOCKED() ;

  /* If port is set to 0, do not listen for VTY_TERMINAL at all!        */
  if (port)
    uty_term_open_listeners(addr, port) ;

#if 0
  /* If want to listen for vtysh, set up listener now                   */
  if (VTYSH_ENABLED && (path != NULL))
    uty_serv_vtysh(path) ;
#endif
} ;

/*------------------------------------------------------------------------------
 * Create listener and set it ready to accept.
 *
 * Adds to list of listeners for close down.
 */
extern void
uty_add_listener(int fd, vio_vfd_accept* accept_action)
{
  vio_listener  vl ;

  VTY_ASSERT_LOCKED() ;

  vl = vio_listener_new(fd, accept_action) ;

  ssl_push(vty_listeners_list, vl, next) ;
} ;

/*------------------------------------------------------------------------------
 * Close all VTY listeners
 */
extern void
uty_close_listeners(void)
{
  vio_listener listener ;

  VTY_ASSERT_LOCKED() ;

  while ((listener = ssl_pop(&listener, vty_listeners_list, next)) != NULL)
    {
      vio_listener_close(listener) ; /* no ceremony, no flowers      */
    } ;
} ;

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
 *
 *   * the death watch list
 */

/* Watch-dog timer.                                                     */
static vio_timer_t vty_watch_dog ;

static vty_timer_time uty_watch_dog_bark(vio_timer timer, void* info) ;
static void uty_death_watch_scan(bool final) ;

static vty_io uty_dispose(vty_io vio) ;

/*------------------------------------------------------------------------------
 * Initialise watch dog -- at start-up time.
 */
extern void
uty_watch_dog_init(void)
{
  vio_timer_init_new(vty_watch_dog, NULL, NULL) ; /* empty         */
} ;

/*------------------------------------------------------------------------------
 * Start watch dog -- before a VTY is created.
 */
extern void
uty_watch_dog_start(void)
{
  vio_timer_init_new(vty_watch_dog, uty_watch_dog_bark, NULL) ;
  vio_timer_set(vty_watch_dog, VTY_WATCH_DOG_INTERVAL) ;
} ;

/*------------------------------------------------------------------------------
 * Stop watch dog timer -- at close down.
 *
 * Final run along the death-watch
 */
extern void
uty_watch_dog_stop(void)
{
  vio_timer_reset(vty_watch_dog, keep_it) ;
  uty_death_watch_scan(true) ;  /* scan the death-watch list            */
}

/*------------------------------------------------------------------------------
 * Watch dog vio_timer action
 */
static vty_timer_time
uty_watch_dog_bark(vio_timer timer, void* info)
{
  cmd_host_name(true) ;         /* check for host name change           */

  uty_death_watch_scan(false) ; /* scan the death-watch list            */

  return VTY_WATCH_DOG_INTERVAL ;
} ;

/*------------------------------------------------------------------------------
 * Process the death watch list -- anything on the list can be disposed of.
 *
 * At curtains...
 */
static void
uty_death_watch_scan(bool final)
{
  VTY_ASSERT_CLI_THREAD() ;

  /* Dispose of anything on the death watch list.                       */

  while (vio_death_watch != NULL)
    {
      vty     vty ;
      vty_io  vio ;

      vio             = vio_death_watch ;
      vio_death_watch = sdl_next(vio, vio_list) ;  /* take off death watch */

      vty = vio->vty ;

      vty->vio = uty_dispose(vty->vio) ;
      assert(vty->exec == NULL) ;

      XFREE(MTYPE_VTY, vty) ;
    } ;
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
 * Allocate new vty structure, including empty vty_io and empty execution
 * structures.
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
 * No "exec" is allocated.  That is done when the command loop is entered.
 */
extern vty
uty_new(vty_type_t type, node_type_t node)
{
  vty      vty ;
  vty_io   vio ;

  VTY_ASSERT_LOCKED() ;

  /* Basic allocation                                                   */

  vty = XCALLOC(MTYPE_VTY, sizeof(struct vty)) ;
  /* Zeroising the vty structure has set:
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
  confirm(NULL_NODE == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;

  vty->type   = type ;
  vty->node   = node ;

  vio = XCALLOC(MTYPE_VTY, sizeof(struct vty_io)) ;

  /* Zeroising the vty_io structure has set:
   *
   *   vty          = X      -- set to point to parent vty, below
   *
   *   name         = NULL   -- no name, yet               TODO ???
   *
   *   vin          = NULL   -- empty input stack
   *   vin_base     = NULL   -- empty input stack
   *   vin_depth    = 0      -- no stacked vin's, yet
   *
   *   real_depth   = 0      -- nothing stacked, yet
   *
   *   vout         = NULL   -- empty output stack
   *   vout_base    = NULL   -- empty output stack
   *   vout_depth   = 0      -- no stacked vout's, yet
   *
   *   err_hard     = false  -- no error at all, yet
   *   ebuf         = NULL   -- no error at all, yet
   *
   *   vio_list     = NULLs  -- not on the vio_list, yet
   *   mon_list     = NULLs  -- not on the monitors list
   *
   *   blocking     = X      -- set below: false unless VTY_CONFIG_READ
   *
   *   state        = vc_null -- not started vty command loop
   *
   *   close_reason = NULL    -- none set
   *
   *   obuf         = NULL     -- no output buffer, yet
   */
  confirm(vc_null == 0) ;

  vty->vio = vio ;
  vio->vty = vty ;

  vio->blocking = (type == VTY_CONFIG_READ) ;

  /* Place on list of known vio/vty                                     */
  sdl_push(vio_live_list, vio, vio_list) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Add a new vf to the vio->vin stack, and set read stuff.
 *
 * Sets the vf->vin_type and set vf->read_open.
 *
 * Initialises an input buffer if required, and sets line_complete and
 * line_step so that first attempt to fetch a line will give line 1.
 *
 * Sets the read ready action and the read timer timeout action.
 *
 * NB: is usually called from the cli thread, but may be called from the cmd
 *     thread for vf which is blocking !
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern void
uty_vin_push(vty_io vio, vio_vf vf, vio_in_type_t type,
                                    vio_vfd_action* read_action,
                                    vio_timer_action* read_timer_action,
                                    usize ibuf_size)
{
  vf->vin_type  = type ;
  vf->vin_state = vf_open ;

  assert(type != VIN_NONE) ;

  if ((type < VIN_SPECIALS) && (!vf->blocking))
    {
      vio_vfd_set_read_action(vf->vfd, read_action) ;
      vio_vfd_set_read_timeout_action(vf->vfd, read_timer_action) ;
    } ;

  ssl_push(vio->vin, vf, vin_next) ;
  vio->real_depth = ++vio->vin_depth ;

  if (vio->vin_base == NULL)
    {
      assert(vio->vin_depth == 1) ;
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
  assert(vio->vin->context == NULL) ;
  vio->vin->context = cmd_context_new_save(context, file_here) ;
} ;

/*------------------------------------------------------------------------------
 * Push a new vf to the vio->vout stack, and set write stuff.
 *
 * Sets the vf->vout_type and set vf->write_open.
 *
 * Sets the write ready action and the write timer timeout action.
 *
 * Initialises an output buffer and sets an end_mark.
 *
 * The depth_mark is set to the current vio->vin_depth + 1.  This is the
 * vin_depth below which the vout should be closed.  Before a command line
 * is fetched (and hence after the previous command line has completed) the
 * vout->depth_mark is checked.  If it is > the current vin_depth, then
 * the vout is closed before a command line can be fetched.
 *
 * NB: where a vin and vout are opened together, so the vout should NOT
 *     be closed until after the vin, need to call uty_vout_sync_depth()
 *     *both* the vin and the vout are pushed, in order to set the correct
 *     depth_mark.
 *
 * NB: is usually called from the cli thread, but may be called from the cmd
 *     thread for vf which is blocking !
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
                                     usize obuf_size)
{
  VTY_ASSERT_LOCKED() ;

  vf->vout_type  = type ;
  vf->vout_state = vf_open ;

  assert(type != VOUT_NONE) ;

  if ((type < VOUT_SPECIALS) && (!vf->blocking))
    {
      vio_vfd_set_write_action(vf->vfd, write_action) ;
      vio_vfd_set_write_timeout_action(vf->vfd, write_timer_action) ;
    } ;

  ssl_push(vio->vout, vf, vout_next) ;
  ++vio->vout_depth ;

  if (vio->vout_base == NULL)
    {
      assert(vio->vout_depth == 1) ;
      vio->vout_base = vf ;
    } ;

  vf->obuf = vio_fifo_new(obuf_size) ;
  vio_fifo_set_end_mark(vf->obuf) ;

  vf->depth_mark = vio->vin_depth + 1 ;

  vio->obuf = vf->obuf ;
} ;

/*------------------------------------------------------------------------------
 * Synchronise vout->depth_mark to current vin_depth.
 *
 * This *must* be called after a vin and vout have been opened together, and
 * they are intended to be at the same depth.  This applies when the base
 * vin/vout are opened, and when an in pipe and an out pipe are present together
 * on a command line.
 */
extern void
uty_vout_sync_depth(vty_io vio)
{
  vio->vout->depth_mark = vio->vin_depth ;
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
 * Two forms: "curtains" and "not-curtains".
 *
 * At "curtains" the system is being terminated, and all message and event
 * handling has stopped.  Can assume that a vty is not in any command loop,
 * so can be killed off here and now.
 *
 * At "not-curtains" messages and event handling is still running.  It is
 * possible that a vty is running a command in the cmd_thread, so cannot
 * completely close things down -- must leave enough for the command loop
 * to continue to work, up to the point that the closing of the vty is
 * detected.
 *
 * For everything that is closed, uses "final" close, which stops things
 * instantly -- will not block, or set any read/write ready, or continue the
 * command loop, or take any notice of errors.
 *
 * This close is called by:
 *
 *    * uty_reset()     -- SIGHUP -- !curtains
 *
 *      Will close "final" everything except vout_base.
 *
 *      If the command loop is not already stopped, it is signalled to stop
 *      as soon as possible.  When it does it will return here via
 *      vty_cmd_loop_exit().
 *
 *      If command loop has already stopped, then will proceed to complete
 *      close -- see below.
 *
 *    * uty_reset()     -- SIGTERM -- curtains
 *
 *      Will close "final" everything except vout_base.
 *
 *      The command loop will be set stopped, and will proceed to complete
 *      close -- see below.
 *
 *    * vty_cmd_loop_exit()  -- when the command loop has stopped -- !curtains
 *
 *      The command loop may have stopped in response to a vc_close_trap,
 *      which means that have been here before, and the vty is pretty much
 *      closed already.
 *
 *      The command loop may have stopped:
 *
 *         - because has reached the end of the vin_base input
 *         - vin_base has timed out
 *         - there was a command error, on non-interactive vty
 *         - there was an I/O error
 *
 *      In all these cases, the stack will already have been closed final or
 *      otherwise, except for vout_base, and all output will have been pushed.
 *
 *      vout_base will have been closed, but not "final", so will be sitting
 *      in vf_closing state.
 *
 *      So the vio is ready for complete close.
 *
 * For complete close any remaining vf are closed final, and the close reason
 * is output to the vout_base, if any and if possible.
 *
 * The vty is then closed and placed on death watch to be finally reaped.
 */
extern void
uty_close(vty_io vio, const char* reason, bool curtains)
{
  VTY_ASSERT_CAN_CLOSE(vio->vty) ;

  /* Stamp on any monitor output instantly.                             */
  uty_set_monitor(vio, off) ;

  /* Save the close reason for later, unless one is already set.        */
  if ((reason != NULL) && (vio->close_reason == NULL))
    vio->close_reason = XSTRDUP(MTYPE_TMP, reason) ;

  /* Close the command loop -- if not already stopped (or closed !)
   *
   * If command loop is not already stopped, the if "curtains" will stop it
   * instantly, otherwise will signal to the command loop to close, soonest.
   */
  uty_cmd_loop_close(vio, curtains) ;

  /* Close all vin including the vin_base.
   *
   * Note that the vin_base is closed, but is still on the vin stack.
   */
  do
    uty_vin_pop(vio, true, NULL) ;      /* final close, discard context */
  while (vio->vin != vio->vin_base) ;

  /* Close all the vout excluding the vout_base.                        */
  while (vio->vout != vio->vout_base)
    uty_vout_pop(vio, true) ;         /* final close                  */

  /* If command loop is still running, this is as far as can go.
   *
   * The command loop will hit the vc_close_trap or execute CMD_CLOSE, and
   * that will cause this function to be called again, in vc_stopped state.
   *
   * Save the close_reason for later.
   */
  if ((vio->state == vc_running) || (vio->state == vc_close_trap))
    return ;

  /* If the vout_base is not closed, try to output the close reason,
   * if any.
   */
  if ((vio->close_reason != NULL) && (vio->vout_base->vout_state != vf_closed))
    uty_vout_close_reason(vio->vout_base, vio->close_reason) ;

  /* Now final close the vout_base.
   *
   * Note that the vout_base will be closed, but on the vout stack with an
   * empty obuf... just in case TODO ?
   */
  uty_vout_pop(vio, true) ;           /* final close                  */

  assert(vio->obuf == vio->vout_base->obuf) ;
  vio_fifo_clear(vio->obuf, true) ;     /* and discard any marks        */

  /* All should now be very quiet indeed.                               */
  if (vty_debug)
    {
      assert(vio->vin == vio->vin_base) ;
      assert(vio->vin_depth  == 0) ;
      assert(vio->real_depth == 0) ;

      assert(vio->vout == vio->vout_base) ;
      assert(vio->vout_depth == 0) ;

      assert(vio->vin->vin_state   == vf_closed) ;
      assert(vio->vout->vout_state == vf_closed) ;
    } ;

  /* Can dispose of these now -- leave vin/vout for final disposition   */
  vio->vty->exec = cmd_exec_free(vio->vty->exec) ;
  vio->ebuf = vio_fifo_free(vio->ebuf) ;

  /* Command loop is not running, so can place on death watch for final
   * disposition.
   */
  if (vio->state == vc_stopped)
    {
      vio->state = vc_closed ;

      sdl_del(vio_live_list, vio, vio_list) ;
      sdl_push(vio_death_watch, vio, vio_list) ;
    } ;

  assert(vio->state == vc_closed) ;     /* thank you and good night     */
} ;

/*------------------------------------------------------------------------------
 * Dispose unwanted vty.
 *
 * Called from deathwatch -- must already be removed from deathwatch list.
 */
static vty_io
uty_dispose(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->state == vc_closed) ;

  /* Stop pointing at vout_base obuf                                    */
  vio->obuf = NULL ;

  /* Clear out vout and vin (may be the same)                           */
  assert(vio->vin == vio->vin_base) ;
  vio->vin_base = uty_vf_free(vio->vin_base) ;

  assert(vio->vout == vio->vout_base) ;
  if (vio->vout != vio->vin)
    vio->vout_base = uty_vf_free(vio->vout_base) ;

  vio->vin  = NULL ;
  vio->vout = NULL ;

  /* Remainder of contents of the vio                                   */
  vio->ebuf = vio_fifo_free(vio->ebuf) ;

  /* Really cannot be a monitor any more !                              */
  assert(!vio->monitor) ;
  vio->mbuf = vio_fifo_free(vio->mbuf) ;

  XFREE(MTYPE_VTY, vio) ;

  return NULL ;
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
 * On final close, will completely close the input, even if errors occur (and
 * no errors are posted).
 *
 * Returns:  CMD_SUCCESS  -- input completely closed
 *           CMD_WAITING  -- waiting for input to close   <=> not-blocking
 *                           (not if final)
 *           CMD_IO_ERROR -- error or timeout
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern cmd_return_code_t
uty_vin_pop(vty_io vio, bool final, cmd_context context)
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

      if (vio->real_depth > vio->vin_depth)
        vio->real_depth = vio->vin_depth ;

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
 * outstanding does not actually close the vout.
 *
 * If there is no outstanding output (or if final) will completely close the
 * vf and free it (except for vout_base).
 *
 * Returns:  CMD_SUCCESS  -- input completely closed
 *           CMD_WAITING  -- waiting for input to close   <=> not-blocking
 *           CMD_IO_ERROR -- error or timeout
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

  if ((ret == CMD_SUCCESS) || final)
    {
      if (vio->vout_depth > 1)
        {
          vio_vf vf ;

          vf = ssl_pop(&vf, vio->vout, vout_next) ;
          --vio->vout_depth ;

          uty_vf_free(vf) ;
        }
      else
        {
          assert(vio->vout == vio->vout_base) ;
          if (final)
            assert(vio->vout->vout_state == vf_closed) ;

          vio->vout_depth       = 0 ;   /* may already have been closed */

          assert(vio->vin_depth == 0) ;
          vio->vout->depth_mark = 0 ;   /* align with the end stop      */
        } ;

      vio->obuf = vio->vout->obuf ;
    } ;

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
    case VOUT_SHELL_ONLY:
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
 * NB: if the parent vio is blocking, then the vf will be blocking, and so
 *     will any vfd.  An individual vf may be set blocking by setting
 *     vfd_io_blocking in the io_type.
 *
 *     The vty stuff opens all files, pipes etc. non-blocking.  A non-blocking
 *     vf simulates blocking by local pselect.  As far as the vfd level is
 *     concerned, once the file, pipe etc. is open, there is no difference
 *     between blocking and non-blocking except that non-blocking vfd are not
 *     allowed to set read/write ready.
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
   *   pr_master        = NULL  -- none       -- see uty_pipe_write_open()
   *
   *   obuf             = NULL  -- none       -- see uty_vout_push()
   *
   *   depth_mark       = 0                   -- see uty_vout_push()
   *
   *   vfd              = NULL  -- no vfd, yet
   *
   *   blocking         = X                   -- see below
   *   closing          = false -- not on the closing list, yet.
   *
   *   error_seen       = 0     -- no error seen, yet
   *
   *   read_timeout     = 0     -- none
   *   write_timeout    = 0     -- none
   *
   *   child            = 0     -- none   )
   *   terminated       = false -- not    )   -- see uty_pipe_read/write_open()
   *   term_status      = X     -- none   )
   *
   *   pr_state         = vf_closed           -- no pipe return vfd
   *                                          -- see uty_pipe_read/write_open()
   *   pr_vfd           = NULL  -- no vfd     -- see uty_pipe_read/write_open()
   *
   *   pr_slave         = NULL  -- none       -- see uty_pipe_read/write_open()
   *
   *   pr_only          = false               -- see uty_pipe_read/write_open()
   */
  confirm((VIN_NONE == 0) && (VOUT_NONE == 0)) ;
  confirm(vf_closed == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;

  if (vio->blocking)
    io_type |= vfd_io_blocking ;        /* inherit blocking state       */

  vf->vio      = vio ;
  vf->blocking = (io_type & vfd_io_blocking) != 0 ;

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
 * ignored.  A final close may be called in terminating state, so does not
 * do any "vty_cmd_signal".
 *
 * Returns: CMD_SUCCESS     -- is all closed
 *          CMD_WAITING     -- cannot close at the moment   <=> non-blocking
 *                             (not if "final")
 *          CMD_IO_ERROR    -- something went wrong
 *
 * NB: on "final" close returns CMD_SUCCESS no matter what happened, and all
 *     input will have been closed down, and the vf closed.
 */
static cmd_return_code_t
uty_vf_read_close(vio_vf vf, bool final)
{
  cmd_return_code_t  ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  ret = CMD_SUCCESS ;

  if (vf->vin_state == vf_closed)
    return ret ;                /* quit if already closed               */

  vf->vin_state = vf_closing ;  /* TODO wipes out error etc ?           */

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
 * ignored.  A final close may be called in terminating state, so does not
 * do any "vty_cmd_signal".
 *
 * If this is the vout_base, unless "final", does NOT actually close the vf or
 * the vfd -- so the vout_base will still work -- but is marked vf_closing !
 * The effect is, essentially, to try to empty out any buffers, but not to
 * do anything that would prevent further output.  This is used so that a
 * command loop can close the vout_base in the usual way, waiting until all
 * output is flushed, but when uty_close() is finally called, it can output
 * any close reason there is to hand.
 *
 * Returns: CMD_SUCCESS     -- is all closed
 *          CMD_WAITING     -- cannot close at the moment   <=> non-blocking
 *                             (not if "final")
 *          CMD_IO_ERROR    -- something went wrong
 *
 * NB: on "final" all output will have been closed down, and the vfd closed,
 *     no matter what the return value says.
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
    return ret ;                /* quit if already closed               */

  vf->vout_state = vf_closing ; /* TODO wipes out error etc ?           */

  base = (vf == vf->vio->vout_base) ;

  /* Must close vin before closing vout at the same level.
   *
   * Note that cannot currently be a pipe return slave, because if was
   * slave to a VOUT_PIPE/VOUT_SHELL_ONLY that vout must have been closed
   * already, and if was slave to a VIN_PIPE, then that too will have been
   * closed already (because of the above).
   */
  assert( (vf->vio->vin_depth < vf->vio->vout->depth_mark)
                                           || (vf->vio->vout_depth ==0) ) ;
  assert(vf->pr_master == NULL) ;

  /* If there is anything in the obuf beyond the end_mark, then it is
   * assumed to be surplus to requirements, and we clear the end_mark.
   */
  vio_fifo_back_to_end_mark(vf->obuf, false) ;

  /* The vout_type specific close functions will attempt to write
   * everything away.
   *
   * If "final", will only keep going until blocks -- at which point will
   * bring everything to a shuddering halt.
   */
  switch(vf->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid VOUT_NONE") ;
      break ;

    case VOUT_TERM:
      ret = uty_term_write_close(vf, final, base) ;
      break ;

    case VOUT_VTYSH:
      break ;

    case VOUT_FILE:
      ret = uty_file_write_close(vf, final, base) ;
      break ;

    case VOUT_PIPE:
    case VOUT_SHELL_ONLY:
      ret = uty_pipe_write_close(vf, final, base,
                                         vf->vout_type == VOUT_SHELL_ONLY) ;
      break ;

    case VOUT_CONFIG:
      ret = uty_file_write_close(vf, final, base) ;   /* treat as file */
      break ;

    case VOUT_DEV_NULL:
    case VOUT_STDOUT:
    case VOUT_STDERR:
      ret = CMD_SUCCESS ;
      break ;

    default:
      zabort("unknown VOUT type") ;
  } ;

  assert(vf->vin_state == vf_closed) ;

  if (((ret == CMD_SUCCESS) && !base) || final)
    {

      assert(vf->vio->obuf == vf->obuf) ;
      assert(vio_fifo_empty(vf->obuf)) ;


      if (vf->vout_type < VOUT_SPECIALS)
        vf->vfd = vio_vfd_close(vf->vfd) ;
      else
        assert(vf->vfd == NULL) ;

      vf->vout_state = vf_closed ;
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
                                      && (vf->pr_state == vf_closed)) ;

  XFREE(MTYPE_VTY_NAME, vf->name) ;

  assert(vf->cli == NULL) ;

  vf->ibuf   = vio_fifo_free(vf->ibuf) ;
  vf->cl     = qs_reset(vf->cl, free_it) ;
  vf->obuf   = vio_fifo_free(vf->obuf) ;

  vf->context = cmd_context_free(vf->context, false) ;  /* not a copy   */

  vf->vfd    = vio_vfd_close(vf->vfd) ;         /* for completeness     */
  vf->pr_vfd = vio_vfd_close(vf->pr_vfd) ;      /* for completeness     */

  XFREE(MTYPE_VTY, vf) ;

  return NULL ;
} ;




/*------------------------------------------------------------------------------
 * Dealing with an I/O error on VTY socket
 *
 * If this is the first error for this VTY, produce suitable log message.
 *
 * If is a "monitor", turn that off, *before* issuing log message.
 */
extern int
uty_vf_error(vio_vf vf, const char* what, int err)
{
  vty_io vio = vf->vio ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* can no longer be a monitor !  *before* any logging !               */
  uty_set_monitor(vio, off) ;

  /* if this is the first error, log it                                 */
  if (vf->error_seen == 0)
    {
      const char* type = "?" ;  /* TODO */


      vf->error_seen = err ;
      zlog(NULL, LOG_WARNING, "%s: %s failed on fd %d: %s",
                          type, what, vio_vfd_fd(vf->vfd), errtoa(err, 0).str) ;
    } ;

  return -1 ;
} ;




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
  if ((vf->vout_state != vf_open) && (vf->vout_state != vf_closing))
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
 * Child care.
 *
 * Management of vio_child objects and the vio_childer_list.
 *
 * When child is registered it is placed on the vio_childer_list.  It remains
 * there until it is "collected", that is until a waitpid() has returned a
 * "report" for the child.
 *
 * When a parent waits for a child to be collected, it may be in one of three
 * states:
 *
 * TODO
 *
 * When a child is collected, or becomes overdue, the parent is signalled (if
 * required) and the child->awaited state is cleared.
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
 */
static void uty_child_collected(vio_child child, int report) ;
static vty_timer_time vty_child_overdue(vio_timer timer, void* action_info) ;
static void uty_child_signal_parent(vio_child child) ;
static vio_child uty_child_free(vio_child child) ;

/*------------------------------------------------------------------------------
 * Set vty_child_signal_nexus() -- if required.
 *
 * Note that this is set/cleared under VTY_LOCK() and its own mutex.  This
 * allows it to be read under its own mutex and/or VTY_LOCK().
 */
extern void
uty_child_signal_nexus_set(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  assert(vio->blocking) ;

  if (!vty_is_cli_thread())
    {
      qpt_mutex_lock(vty_child_signal_mutex) ;

      vty_child_signal_nexus = qpn_find_self() ;

      qpt_mutex_unlock(vty_child_signal_mutex) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * If there is a nexus to signal, clear the indicator and signal the
 * associated thread.
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
 * Set vty_child_signal_nexus() -- if required.
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
 * Set waiting for child to be collected.
 *
 * This is for !vf->blocking: set timer and leave, waiting for SIGCHLD event.
 */
extern void
uty_child_awaited(vio_child child, vty_timer_time timeout)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert(child->parent != NULL) ;
  assert(!child->parent->blocking) ;

  child->awaited = true ;

  if (child->timer == NULL)
    child->timer = vio_timer_init_new(NULL, vty_child_overdue, child) ;

  vio_timer_set(child->timer, timeout) ;
} ;

/*------------------------------------------------------------------------------
 * See if parent can collect child -- directly.
 *
 * This is for vf->blocking,
 *
 * If can, will collect now -- marking collected and taking off the
 * vio_childer_list.
 *
 * If cannot immediately collect, if final mark overdue and return.
 *
 * Otherwise wait for up to timeout seconds for a suitable SIGCHLD or related
 * wake-up signal.
 *
 * NB: this blocks with the VTY_LOCK() in its hands !!  But this is only
 *     required for configuration file reading...  and timeout is limited.
 *
 * Returns:  true <=> collected
 *          false  => timed out or final
 */
extern bool
uty_child_collect(vio_child child, vty_timer_time timeout, bool final)
{
  bool first ;

  VTY_ASSERT_LOCKED() ;

  assert(child->parent != NULL) ;
  assert(child->parent->blocking) ;
  assert(child->timer == NULL) ;

  assert(child->pid > 0) ;

  first = true ;
  while (1)
    {
      pid_t      pid ;
      int        report ;

      qps_mini_t qm ;
      sigset_t*  sig_mask = NULL ;

      if (child->collected)
        return true ;                   /* have collected               */

      pid = waitpid(child->pid, &report, WNOHANG) ;

      if (pid == child->pid)
        {
          /* Collected the child                                        */
          uty_child_collected(child, report) ;

          return true ;                 /* have collected               */
        } ;

      if (pid != 0)
        {
          int err = 0 ;

          /* With the exception of EINTR, all other returns are, essentially,
           * impossible...
           *
           *   (1) ECHLD means that the given pid is not a child...
           *       ...which is impossible if not collected -- but treat as
           *       "overdue".
           *
           *   (2) only other known error is EINVAL -- invalid options...
           *       absolutely impossible.
           *
           *   (3) some pid other than the given child is an invalid response !
           */
          if (pid < 0)
            {
              if (errno == EINTR)
                continue ;

              err = errno ;

              zlog_err("waitpid(%d) returned %s", child->pid,
                                                          errtoa(err, 0).str) ;
            }
          else
            zlog_err("waitpid(%d) returned pid=%d", child->pid, pid) ;

          if (err != ECHILD)
            zabort("impossible return from waitpid()") ;

          final = true ;                /* treat ECHLD as last straw !  */
        } ;

      /* Waiting for child.                                             */
      if (final)
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
 * Dismiss child -- if not collected, smack but leave to be collected in
 * due course (or swept up at "curtains").
 */
extern vio_child
uty_child_dismiss(vio_child child, bool final)
{
  VTY_ASSERT_LOCKED() ;

  if (child != NULL)
    {
      if (!child->collected)
        {
          assert(child->pid > 0) ;

          kill(child->pid, SIGKILL) ;   /* hasten the end       */

          if (final)
            {
              assert(child->parent == NULL) ;
              sdl_del(vio_childer_list, child, list) ;
              child->collected = true ;         /* forceably    */
            } ;

          child->overdue = true ;       /* too late for parent  */
        } ;

      child->parent  = NULL ;   /* orphan from now on   */
      child->awaited = false ;  /* nobody waiting       */

      if (child->collected)
        uty_child_free(child) ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * At "curtains" -- empty out anything left in the child register.
 *
 * The only children that can be left are dismissed children that have yet to
 * be collected.
 */
extern void
vty_child_close_register(void)
{
  while (vio_childer_list != NULL)
    uty_child_dismiss(vio_childer_list, true) ;         /* final        */
} ;

/*------------------------------------------------------------------------------
 * See whether any children are ready for collection, and check each one
 * against the register.
 *
 * Perform waitpid( , , WNOHANG) until no child is returned, and process
 * each one against the register.
 *
 * The is done when a SIGCHLD is route through the event mechanism.
 *
 * If another SIGCHLD occurs while this is being done,  that will later cause
 * another call of this function -- at worst it may find that the child in
 * question has already been collected.
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

      pid = waitpid(-1, &report, WNOHANG) ;

      if (pid == 0)
        break ;

      if (pid < 0)
        {
          if (errno == EINTR)
            continue ;          /* loop on "Interrupted"        */

          if (errno != ECHILD)  /* returns ECHLD if no children */
            zlog_err("waitpid(-1) returned %s", errtoa(errno, 0).str) ;

          break ;
        } ;

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
              /* Have collected child.
               *
               * Remove from the vio_childer_list, set collected flag.
               *
               * We can leave any timer object -- if it goes off it will be
               * ignored, because the child is no longer awaited.  Timer will
               * be discarded when the child is dismissed/freed.
               *
               * If no parent, can free child object now.
               */
              uty_child_collected(child, report) ;

              if      (child->parent == NULL)
                uty_child_free(child) ;
              else if (child->awaited)
                uty_child_signal_parent(child) ;

              break ;
            } ;

          child = sdl_next(child, list) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set the child collected and set the report.
 *
 * Remove from the vio_childer_list -- is now either back in the hands of the
 * parent, or ready to be freed.
 */
static void
uty_child_collected(vio_child child, int report)
{
  assert(!child->collected) ;   /* can only collect once        */

  sdl_del(vio_childer_list, child, list) ;

  child->collected = true ;
  child->report    = report ;
} ;

/*------------------------------------------------------------------------------
 * Set child as overdue -- vio_timer action routine.
 *
 * NB: the timer may go off after the child has been collected, but before the
 *     parent has got round to stopping the timer.
 */
static vty_timer_time
vty_child_overdue(vio_timer timer, void* action_info)
{
  vio_child child ;

  VTY_LOCK() ;

  child = action_info ;
  assert(timer == child->timer) ;

  if (child->awaited)
    {
      child->overdue = true ;
      uty_child_signal_parent(child) ;
    } ;

  VTY_UNLOCK() ;

  return 0 ;            /* stop timer   */
} ;

/*------------------------------------------------------------------------------
 * Signal that child is ready -- collected or overdue.
 *
 * Must be "awaited" -- so not "blocking"
 */
static void
uty_child_signal_parent(vio_child child)
{
  assert(child->awaited && (child->parent != NULL)) ;

  assert(!child->parent->vio->blocking) ;

  child->awaited = false ;
  uty_cmd_signal(child->parent->vio, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Free the child -- caller must ensure that any parent has disowned the child,
 * and that it is collected (so not on the vio_childer_list).
 */
static vio_child
uty_child_free(vio_child child)
{
  if (child != NULL)
    {
      assert(child->collected && (child->parent == NULL)) ;

      child->timer = vio_timer_reset(child->timer, free_it) ;
      XFREE(MTYPE_VTY, child) ;         /* sets child = NULL    */
    } ;

  return child ;
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

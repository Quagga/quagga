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
#include "vty_io_std.h"
#include "vty_io_vtysh.h"
#include "vty_log.h"
#include "vty_vtysh.h"
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
#include "sockopt.h"
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

/* Watch-dog timer
 */
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
static void uty_vout_base_close(vty_io vio) ;
static cmd_ret_t uty_vf_read_close(vio_vf vf) ;
static cmd_ret_t uty_vf_write_close(vio_vf vf) ;
static vio_vf uty_vf_free(vio_vf vf) ;

/*==============================================================================
 * Creation and destruction of VTY objects
 */

/*------------------------------------------------------------------------------
 * Allocate new vty structure, including empty vty_io but no exec structure.
 *
 * Sets:
 *
 *   * vio->blocking      = true if VTY_VTYSH or VTY_STDOUT
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
 *
 * NB: *must* *immediately* push a vin and a vout -- very little works if
 *     there is no vin_base or no vout_base !!
 *
 *     The *only* things expected to work are:
 *
 *       uty_vin_push()
 *       uty_vout_push()
 *       uty_close()
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
   *   type         = X           -- set to actual type, below
   *
   *   node         = X           -- set to actual node, below
   *
   *   index        = NULL        -- nothing, yet
   *   index_sub    = NULL        -- nothing, yet
   *
   *   config_to_vtysh = false    -- not writing config to vtysh !
   *
   *   exec         = NULL        -- execution state set up when required
   *   vio          = X           -- set below
   */
  vty = XCALLOC(MTYPE_VTY, sizeof(struct vty)) ;

  vty->type   = type ;
  vty->node   = node ;

  /* Zeroising the vty_io structure will set:
   *
   *   vty          = X           -- set to point to parent vty, below
   *   vio_list     = NULLs       -- not on the vio_list, yet
   *
   *   config       = 0           -- not owner of configuration symbol of power
   *
   *   vin          = NULL        -- empty input stack
   *   vin_base     = NULL        -- empty input stack
   *   vin_depth    = 0           -- no stacked vin's, yet
   *
   *   vout         = NULL        -- empty output stack
   *   vout_base    = NULL        -- empty output stack
   *   vout_depth   = 0           -- no stacked vout's, yet
   *
   *   state        = vst_cmd_fetch  -- default initial state !
   *
   *   cancel_prefix  = NULL      -- none set
   *   suspend_reason = NULL      -- none set
   *   close_reason   = NULL      -- none set
   *
   *   obuf         = NULL        -- no output buffer, yet
   *
   *   prompt_node  = NULL_NODE   -- => no prompt set, yet
   *   prompt_gen   = 0           -- => no prompt set, yet
   *   prompt       = X           -- set to an empty qstring, below
   *
   *   prompt_len   = 0           -- no prompt written, yet
   *
   *   ebuf         = NULL        -- no error at all, yet
   *
   *   blocking     = false       -- set below: false unless VTY_VTYSH
   *                                                      or VTY_STDOUT
   *   cl_state     = vcl_stopped  -- not started vty command loop
   *   signal       = CMD_SUCCESS -- OK (null signal)
   *   sh_serv_ret  = CMD_SUCCESS -- OK (so far)
   *
   *   ps_buf       = NULL        -- no pipe stderr return buffer, yet
   *
   *   mon_list     = NULLs       -- not on the monitors list
   *   monitor      = false       -- not a monitor
   *   mwrite       = false       -- no monitor write required
   *   maxlvl       = 0
   *   mbuf         = NULL        -- no buffer
   */
  vio = XCALLOC(MTYPE_VTY, sizeof(struct vty_io)) ;

  confirm(vst_cmd_fetch  == 0) ;
  confirm(vcl_stopped    == 0) ;
  confirm(CMD_SUCCESS    == 0) ;
  confirm(NULL_NODE      == 0) ;

  vty->vio = vio ;
  vio->vty = vty ;

  vio->prompt     = qs_new(30) ;

  switch (type)
    {
      case VTY_STDOUT:
        vio->blocking   = true ;
        break ;

      case VTY_TERMINAL:
        vio->blocking   = false ;
        break ;

      case VTY_VTYSH_SERVER:
        vio->blocking   = false ;
        break ;

      case VTY_VTYSH:
        vio->blocking   = true ;
        break ;

      default:
        zabort("unknown vty_type") ;
    } ;

  /* Place on list of known vio/vty                                     */
  ddl_append(vio_live_list, vio, vio_list) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Set timeout value.
 *
 * This is only ever called when a command (eg exec-timeout) sets a new
 * time out value -- which applies only to VIN_TERM.
 */
extern void
uty_set_timeout(vty_io vio, vty_timer_time timeout)
{
  vio_vf        vin_base ;

  VTY_ASSERT_LOCKED() ;

  vin_base = vio->vin_base ;

  if (vin_base->vin_type == VIN_TERM)
    {
      qassert(!vin_base->blocking && !vio_vfd_blocking(vin_base->vfd)) ;

      vin_base->read_timeout = timeout ;

      uty_vf_set_read_ready(vin_base, on) ;
    } ;
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
 * Set close reason -- NULL or empty close reason => nothing to say
 *
 * If "replace", then will replace any existing reason -- this is used when
 * setting an I/O error close reason.  Generally, lets the first reason set
 * be the reason given -- so late in the close process can set "default"
 * reason.
 *
 * The reason given is wrapped "Terminated: <why>".  The <why> should not
 * contain any "\n" -- may be output as part of a logging message.
 */
extern void
uty_close_reason_set(vty_io vio, const char* why, bool replace)
{
  VTY_ASSERT_LOCKED() ;

  if ((vio->close_reason != NULL) && !replace)
    return ;

  if ((why == NULL) || (*why == '\0'))
    vio->close_reason = qs_free(vio->close_reason) ;
  else
    vio->close_reason = qs_printf(vio->close_reason, "Terminated: %s", why) ;
} ;

/*------------------------------------------------------------------------------
 * Set suspend reason -- replaces any existing reason
 *
 * The reason given is wrapped "Suspended: <why>".  The <why> should not
 * contain any "\n" -- may be output as part of a logging message.
 */
extern void
uty_suspend_reason_set(vty_io vio, const char* why)
{
  VTY_ASSERT_LOCKED() ;

  if ((why == NULL) || (*why == '\0'))
    why = "*reason unknown (BUG)*" ;

  vio->suspend_reason = qs_printf(vio->suspend_reason, "Suspended: %s", why) ;
} ;

/*------------------------------------------------------------------------------
 * Clear suspend reason
 */
extern void
uty_suspend_reason_clear(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  vio->suspend_reason = qs_free(vio->suspend_reason) ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY -- final.
 *
 * NB: on exit the VTY has been released, so do NOT attempt to touch the VTY
 *     or any of its components.
 */
extern void
vty_close(vty vty)
{
  VTY_LOCK() ;

  VTY_ASSERT_CAN_CLOSE(vty) ;

  uty_close(vty->vio) ;                 /* down close the vty           */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY -- final.
 *
 * In normal running, the hiatus takes care of closing things.  It deals with
 * outstanding output, error messages, stderr pipe return etc.  When the
 * base vin is closed, the hiatus keeps running until everything is tidy, and
 * only the base vout is left and all buffers are empty.  Then this is called
 * to finish off the job, and free the vty etc.
 *
 * However, this may also be called if the vty is to be brought to a short,
 * sharp stop.
 *
 * Turns off any log monitoring immediately.
 *
 * Discards any output beyond any end mark and discards any buffered errors and
 * stderr pipe return.
 *
 * Close "final" means: terminate any input immediately, but attempt to flush
 * any pending output (and any pipe return) but give up if would block or gets
 * any error.
 *
 * To call this the command loop must be vcl_stopped.  When a command loop exits
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

  qassert(vty->vio      == vio) ;
  qassert(vio->cl_state == vcl_stopped) ;

  /* The vio is about to be closed, final, so set state (which clears log
   * monitor, make sure we are not about to take the configuration symbol of
   * power with us, and remove from the live list.
   *
   * Note that vst_final means we are beyond caring about trying to empty
   * buffers or anything else.
   */
  vio->state |= vst_final| vst_cancel ;

  uty_cmd_config_lock(vio, EXIT_NODE) ;
  ddl_del(vio_live_list, vio, vio_list) ;

  /* Close all vin and vout, ensuring that closes vout after vin at the
   * same depth.
   *
   * Although it is a rule that there must be a vin_base and a vout_base,
   * the following will work if none has ever been set up.
   *
   * Note that retains both vin_base and vout_base after they have been closed.
   */
  uty_vio_exception(vio, vx_stop_final) ;

  while (vio->vin_depth > 0)
    {
      if (vio->vout_depth > vio->vin_depth)
        uty_vout_pop(vio) ;
      else
        uty_vin_pop(vio) ;
    } ;

  qassert(vio->vin  == vio->vin_base) ;         /* even if both NULL    */
  qassert(vio->vout == vio->vout_base) ;

  /* If there is a vout_base, try to push out the close_reason, if any, and
   * finally pop it.
   */
  if (vio->vout_base != NULL)
    uty_vout_base_close(vio) ;

  /* Release what remains of the vio
   */
  vio->vty    = NULL ;

  vio->ebuf   = vio_fifo_free(vio->ebuf) ;
  vio->obuf   = NULL ;          /* about to discard the vout_base       */
  vio->ps_buf = vio_fifo_free(vio->ps_buf) ;
  vio->prompt = qs_free(vio->prompt) ;

  vio->close_reason   = qs_free(vio->close_reason) ;
  vio->suspend_reason = qs_free(vio->suspend_reason) ;

  if (vio->vin != NULL)
    {
      if (vio->vin != vio->vout)
        vio->vin = uty_vf_free(vio->vin) ;
      else
        vio->vin = NULL ;
    } ;

  if (vio->vout != NULL)
    vio->vout = uty_vf_free(vio->vout) ;

  vio->vin_base  = NULL ;
  vio->vout_base = NULL ;

  qassert(!vio->monitor) ;
  vio->mbuf = vio_fifo_free(vio->mbuf) ;

  /* Release the vio and the exec (if any)
   */
  XFREE(MTYPE_VTY, vty->vio) ;
  vty->exec = cmd_exec_free(vty->exec) ;

  /* Finally, release the VTY itself.
   */
  XFREE(MTYPE_VTY, vty) ;
} ;

/*==============================================================================
 * Pushing and popping vf objects on the vio->vin_stack and vio->vout_stack.
 */

/*------------------------------------------------------------------------------
 * Add a new vf to the vio->vin stack, and set read stuff.
 *
 * Sets the given vf->vin_type and set vf_open.
 *
 * If creating the vin_base, creates a new, empty context -- to be set later,
 * see cmd_context_init().
 *
 * If not creating the vin_base, copies the current vin context.
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
 * NB: MUST ensure the context is fully up to date, and do a uty_cmd_prepare()
 *     *before* command processing can start/continue.
 *
 * NB: see uty_vout_push() for notes on pushing a vin and a vout together.
 */
extern void
uty_vin_push(vty_io vio, vio_vf vf, vio_in_type_t type,
                                    vio_vfd_action* read_action,
                                    usize ibuf_size)
{
  cmd_context tos ;

  qassert(type != VIN_NONE) ;

  vf->vin_type  = type ;
  vf->vin_state = vf_open ;

  if (vio->blocking)            /* vio->blocking => vf->blocking        */
    qassert(vf->blocking) ;

  if (vf->vin_type < VIN_SPECIALS)
    {
      if (vf->blocking)
        qassert(vio_vfd_blocking(vf->vfd)) ;
      else
        {
          qassert(!vio_vfd_blocking(vf->vfd)) ;

          vio_vfd_set_read_action(vf->vfd, read_action) ;
        } ;
    } ;

  if (vio->vin_base == NULL)
    {
      qassert((vio->vin_depth == 0) && (vio->vin == NULL)) ;
      vio->vin_base = vf ;

      tos = cmd_context_new() ;
    }
  else
    {
      tos = vio->vin->context ;
      vio->vin->context = cmd_context_save(tos) ;
    } ;

  vf->context = tos ;

  ssl_push(vio->vin, vf, vin_next) ;
  ++vio->vin_depth ;

  if (ibuf_size != 0)
    {
      vf->ibuf          = vio_fifo_new(ibuf_size) ;
      vf->cl            = qs_new(150) ;
      vf->line_complete = true ;
      vf->line_step     = 1 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set a new dir_here in the (now) current context
 */
extern void
uty_vin_set_here(vty_io vio, qpath here)
{
  qpath dir_here = vio->vin->context->dir_here ;

  qassert(dir_here != NULL) ;

  qpath_copy(dir_here, here) ;
  qpath_shave(dir_here) ;
} ;

/*------------------------------------------------------------------------------
 * Push a new vf to the vio->vout stack, and set write stuff.
 *
 * Sets the vf->vout_type and set vf_open
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
 * together:
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
 *     vin_depth + 1 -- which is do different to opening the vout on its
 *     own.
 *
 *     If the vout is opened after the vin, the new vout_depth will be
 *     vin_depth.
 *
 * So the "after" flag tells us that this vout is being opened with, but after
 * a vin.
 *
 * NB: is usually called from the cli thread, but may be called from the cmd
 *     thread for vf which is blocking, or for a "special" vout_type !
 *
 * NB: VOUT_DEV_NULL, VOUT_VTYSH and VOUT_STDOUT are special.
 *
 *     The write_action and the write_timer_action are ignored.
 *
 *     All actual I/O to these outputs is direct, blocking and via standard
 *     I/O -- except VOUT_DEV_NULL where all I/O is discarded.
 *
 * NB: all outputs are set up with an obuf, so all output is collected, even
 *     if it is later to be discarded.
 *
 *     The push_complete flag is set for the few vout types where the output
 *     side must know when a command completes (even if there is no output).
 *
 *     The out_ordinary defauts to true except for VOUT_DEV_NULL.
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern void
uty_vout_push(vty_io vio, vio_vf vf, vio_out_type_t type,
                                     vio_vfd_action* write_action,
                                     usize obuf_size,
                                     bool after)
{
  VTY_ASSERT_LOCKED() ;

  qassert(type != VOUT_NONE) ;

  vf->vout_type  = type ;
  vf->vout_state = vf_open ;

  if (vio->blocking)            /* vio->blocking => vf->blocking        */
    qassert(vf->blocking) ;

  if (vf->vout_type < VOUT_SPECIALS)
    {
      if (vf->blocking)
        qassert(vio_vfd_blocking(vf->vfd)) ;
      else
        {
          qassert(!vio_vfd_blocking(vf->vfd)) ;

          vio_vfd_set_write_action(vf->vfd, write_action) ;
       } ;
    } ;

  ssl_push(vio->vout, vf, vout_next) ;
  if (vio->vout_base == NULL)
    {
      qassert(vio->vout_depth == 0) ;
      vio->vout_base = vf ;
    } ;

  vf->obuf = vio_fifo_new(obuf_size) ;
  vio_fifo_set_end_mark(vf->obuf) ;

  vf->push_complete = (type == VOUT_TERM) || (type == VOUT_VTYSH_SERVER)
                                          || (type == VOUT_VTYSH) ;
  vf->out_ordinary  = (type != VOUT_DEV_NULL) ; /* Default state        */

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

  vf->fp    = NULL ;    /* for completeness     */
  vf->pager = NULL ;

  vio->obuf = vf->obuf ;
} ;

/*------------------------------------------------------------------------------
 * Close top of the vin stack and pop when done -- see uty_vf_read_close().
 *
 * If succeeds in closing, unless is vin_base, pops the vin stack and if this
 * is read-only will free the vio_vf and all its contents.  (So if this is
 * vin_base, it is left on the stack, but vf_closed/vf_closing.)
 *
 * Popping the vin implicitly pops (and discards) the context.
 *
 * Returns:  CMD_SUCCESS  -- input completely closed and popped
 *           CMD_WAITING  -- waiting for input to close    => non-blocking
 *           CMD_IO_ERROR -- error or timeout
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern cmd_ret_t
uty_vin_pop(vty_io vio)
{
  cmd_ret_t ret ;
  vio_vf    vf_pop ;

  VTY_ASSERT_LOCKED() ;

  vf_pop = vio->vin ;

  ret = uty_vf_read_close(vf_pop) ;

  if (vf_pop->vin_state != vf_closed)
    {
      /* Did not complete close -- cannot be vst_final
       */
      qassert(((vio->state & vst_final) == 0) && (ret != CMD_SUCCESS)) ;
      return ret ;
    } ;

  if (vio->vin_depth > 1)
    {
      /* Closing vin which is not the vin_base.
       */
      ssl_pop(&vf_pop, vio->vin, vin_next) ;
      --vio->vin_depth ;

      /* The current context object is pointed at by the vin we have
       * just popped.  The new top of stack vin contains a saved copy of
       * its context.  We copy back the saved context, and discard it,
       * so the vin we just popped will no longer own the context.
       */
      vio->vin->context =
                       cmd_context_restore(vf_pop->context, vio->vin->context) ;
      vf_pop->context = NULL ;

      /* If the vin_depth is or was greater than the vout_depth, then
       * we here restore the effective out_ordinary.
       */
      if (vio->vin_depth >= vio->vout_depth)
        vio->vout->out_ordinary = vio->vin->context->out_ordinary ;

      /* Is now !vin_open -- unless there is a vout_open, we can free the vf.
       */
      if (vf_pop->vout_state == vf_closed)
        uty_vf_free(vf_pop) ;
    }
  else
    {
      /* Closing the vin_base.
       *
       * Sets the depth zero to signal all closed, but preserves the
       * vf at the top of the stack, along with all the other state.
       *
       * The vin_base will be taken care of once the vout_base is closed,
       * and we always close vout after vin.
       */
      qassert(vio->vin == vio->vin_base) ;

      vio->vin_depth = 0 ;          /* may already have been closed */
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Close top of the vout stack and pop when done -- see uty_vf_write_close().
 *
 * Before closing, discard anything after end_mark, then push any outstanding
 * output.
 *
 * May return CMD_WAITING or CMD_IO_ERROR met while trying to output pending
 * stuff -- without having closed the vout.  Will need to return later to try
 * again.
 *
 * If is not vout_base, pops the vout stack.  If this is write-only will free
 * the vio_vf and all its contents.
 *
 * Note that we *never* actually pop the vout_base, so vout_depth > 0 at all
 * times, once the vio is initialised.
 *
 * Returns:  CMD_SUCCESS  -- output completely closed and popped
 *           CMD_WAITING  -- waiting for input to close    => non-blocking
 *           CMD_IO_ERROR -- error or timeout
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 */
extern cmd_ret_t
uty_vout_pop(vty_io vio)
{
  cmd_ret_t ret ;
  vio_vf    vf_pop ;

  VTY_ASSERT_LOCKED() ;

  vf_pop = vio->vout ;

  ret = uty_vf_write_close(vf_pop) ;

  if (vf_pop->vout_state != vf_closed)
    {
      /* Did not complete close -- cannot be vst_final
       */
      qassert(((vio->state & vst_final) == 0) && (ret != CMD_SUCCESS)) ;
      return ret ;
    } ;

  if (vio->vout_depth > 1)
    {
      vf_pop = ssl_pop(&vf_pop, vio->vout, vout_next) ;

      qassert(vf_pop->depth_mark < vio->vout_depth) ;
      qassert(vf_pop->depth_mark > 0) ;

      vio->vout_depth = vf_pop->depth_mark ;
      uty_vf_free(vf_pop) ;
    }
  else
    {
      qassert(vio->vout == vio->vout_base) ;
      qassert(vio->vout->depth_mark == 0) ;

      vio->vout_depth = 0 ;
    } ;

  vio->obuf = vio->vout->obuf ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Try to output the close reason, if any, to the vout_base and then close it.
 *
 * This is the final act for the vout_base.  Will attempt to output unless
 * the thing is actually closed.  As a rule, although the vf is set !vout_open
 * and !vout_active, we avoid closing the underlying vfd or other output up
 * to this point -- so that can write away the close reason.
 *
 * Will ignore errors at this point.  Will try to output even if there has
 * been an earlier failure.
 *
 * NB: the output at this stage *will* be non-blocking (or guaranteed not to
 *     block) !
 */
static void
uty_vout_base_close(vty_io vio)
{
  vio_vf vf ;

  vf = vio->vout ;

  qassert((vf != NULL) && (vf == vf->vio->vout_base)) ;

  if (vio->close_reason != NULL)
    {
      qstring wrapped ;

      wrapped = qs_printf(NULL, "---\n%s\n", qs_string(vio->close_reason)) ;

      switch(vf->vout_type)
        {
          case VOUT_NONE:
            qassert(false) ;
            break ;

          case VOUT_TERM:
            uty_term_close_reason(vf, wrapped) ;
            break ;

          case VOUT_VTYSH_SERVER:
            break ;

          case VOUT_FILE:
          case VOUT_PIPE:
          case VOUT_SH_CMD:
            break ;

          case VOUT_CONFIG:
            break ;

          case VOUT_VTYSH:
            break ;

          case VOUT_DEV_NULL:
            break ;

          case VOUT_STDOUT:
            uty_std_close_reason(vf, wrapped) ;
            break ;

          default:
            qassert(false) ;
            break ;
        } ;

      qs_free(wrapped) ;
    } ;

  uty_vout_pop(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Stop vin and/or vout as required for the given exception.
 *
 * NB: the purpose of all this is to make sure that the local state of every
 *     vin and vout reflects the global state -- so that vin and vout handling
 *     can make local decisions without having to constantly also check some
 *     global state.
 *
 * The exceptions are:
 *
 *    vx_quash      -- stop all vin apart from vin_base.
 *
 *                     e.g. when there is a command or parser error.
 *
 *                     NB: if there is a pipe stderr return, there is some
 *                         magic in the background to soak up all input,
 *                         so that source of input can finish cleanly.
 *
 *    vx_cancel     -- stop all vin apart from vin_base
 *                     stop all vout apart from vin base
 *
 *                     set vst_cancel, if vst_notify is not set.
 *                     set cancel_prefix to "...^C\n"
 *
 *                     e.g. when get ^C from keyboard, or an I/O error, not
 *                          in vin/vout_base
 *
 *                     NB: it is the caller's responsibility to set any
 *                         cancel_prefix
 *
 *                     NB: stops all pipe return of all kinds.  Since we are
 *                         no longer interested in any results, do not care
 *                         whether the child process finishes cleanly, or
 *                         not and will discard any return code.
 *
 *    vx_io_error   -- as vx_cancel, but set cancel_prefix to "***^C\n"
 *
 *                     e.g. or an I/O error that is not in vin/vout_base
 *
 *    vx_suspend    -- as vx_cancel, but sets vx_suspend as well.
 *
 *                     e.g. on SIGHUP.
 *
 *                     NB: it is the caller's responsibility to set any
 *                         suspend_reason.
 *
 *                     NB: it is the caller's responsibility to worry what it
 *                         means if vst_suspend and/or vst_suspended is already
 *                         set !
 *
 *    vx_stop       -- as vx_cancel, but stop the vin_base as well and stop
 *                     any log monitor.
 *
 *                     e.g. on SIGTERM
 *
 *    vx_stop_io_error -- as vx_io_error, but stop the vin_base as well
 *                        and stop any log monitor.
 *
 *                     e.g. on I/O error in vin_base
 *
 *    vx_stop_final -- as vx_stop, but stop the vout_base as well
 *
 *                     sets vst_final and clears cancel_prefix (to be tidy)
 *
 *                     e.g. on terminate or an I/O error in vout_base.
 *
 * NB: if is already vst_notify, then we do not set vst_cancel, and we do
 *     not set the cancel_prefix.
 *
 *     Raising any sort of exception while is dealing with an earlier cancel
 *     is not an issue, any additional information will be included in
 *     the vst_notify.
 *
 *     If vst_cancel is already set, does not set the cancel_prefix, either.
 *
 * NB: if
 */
extern void
uty_vio_exception(vty_io vio, vio_exception_t vx)
{
  vst_state_t add_state ;
  vfs_stop_t  vstp ;
  bool        stop_vin_base ;
  const char* prefix ;
  vio_vf      vf ;

  VTY_ASSERT_LOCKED() ;

  switch (vx)
    {
      case vx_quash:
        add_state      = 0 ;
        stop_vin_base  = false ;
        prefix         = NULL ;
        break ;

      case vx_cancel:
        add_state      = vst_cancel ;
        stop_vin_base  = false ;
        prefix         = "...^C\n" ;
        break ;

      case vx_io_error:
        add_state      = vst_cancel ;
        stop_vin_base  = false ;
        prefix         = "***^C\n" ;
        break ;

      case vx_suspend:
        add_state      = vst_cancel | vst_suspend ;
        stop_vin_base  = false ;
        prefix         = "...^C\n" ;
        break ;

      case vx_stop:
        add_state      = vst_cancel ;
        stop_vin_base  = true ;
        prefix         = "...^C\n" ;
        break ;

      case vx_stop_io_error:
        add_state      = vst_cancel ;
        stop_vin_base  = true ;
        prefix         = "***^C\n" ;
        break ;

      case vx_stop_final:
      default:
        add_state      = vst_cancel | vst_final ;
        stop_vin_base  = true ;
        prefix         = NULL ;

        vio->cancel_prefix = prefix ;
        break ;
    } ;

  if (vio->state & (vst_cancel | vst_notify))
    add_state &= ~vst_cancel ;
  else
    vio->cancel_prefix = prefix ;

  vio->state |= add_state ;

  if (vio->state & vst_final)
    vio->state |= vst_cancel ;  /* horse and carriage   */

  /* Process the vin stack
   *
   * For all but vin_base: vfs_stop_final  (vst_final)
   *                    or vfs_stop_cancel (vst_cancel)
   *                    or vfs_stop_cease  (otherwise)
   *
   * For vin_base: if we are stopping vin_base, as above.
   *               if we are vst_final, also as above
   *               otherwise, leave the vin_base alone.
   */
  if      (vio->state & vst_final)
    vstp = vfs_stop_final ;
  else if (vio->state & vst_cancel)
    vstp = vfs_stop_cancel ;
  else
    vstp = vfs_stop_cease ;

  if (vio->vin_depth > 0)
    {
      vf = vio->vin ;
      while (vf != vio->vin_base)
        {
          uty_vf_read_stop(vf, vstp) ;

          vf = vf->vin_next ;
        } ;

      if (stop_vin_base)
        qassert(vio->state & (vst_cancel | vst_final)) ;

      if (stop_vin_base || (vio->state & vst_final))
        {
          uty_vf_read_stop(vf, vstp) ;
          uty_monitor_set(vio, off) ;
        } ;
    } ;

  /* Process the vout stack, if cancel and/or final.
   *
   * For all but vout_base: vfs_stop_final  (vst_final)
   *                     or vfs_stop_cancel (vst_cancel)
   *
   * For vout_base: if is vst_final, vfs_stop_final it too
   *                otherwise vfs_stop_pause.
   *
   * Otherwise, vfs_stop_pause the vout_base.
   */
  if ((vio->state & (vst_cancel | vst_final)) == 0)
    return ;

  if (vio->vout_depth > 0)
    {
      vf = vio->vout ;
      while (vf != vio->vout_base)
        {
          uty_vf_write_stop(vf, vstp) ;
          vf = vf->vout_next ;
        } ;

      uty_vf_write_stop(vf, (vio->state & vst_final) ? vstp : vfs_stop_pause) ;
    };
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
 *     A VTY_STDOUT (output only, to the standard I/O) is set up without an fd.
 *
 * NB: if the parent vio is blocking, then the vio_vf is set blocking, and the
 *     vio_vfd will be set vfd_io_ps_blocking.
 *
 *     An individual vio_vf may be set blocking by setting vfd_io_os_blocking
 *     or vfd_io_ps_blocking in the io_type.  For example, this is used for
 *     configuration file writing, where the Routing Engine thread wants
 *     to open, write and then close the output file.
 *
 *     For vfd_io_ps_blocking, the vio_vfd opens all files, pipes etc. as
 *     non-blocking at the I/O level.  For vfd_io_os_blocking the files, pipes
 *     etc. are opened blocking at the I/O level.  In both cases the vio_vfd
 *     does *not* set up a qfile, and the fd will not become a member of any
 *     qpselect selection.  This means that any attempt to set read/write ready
 *     and timeouts will be ignored.  It also means that the vio_vfd may be
 *     opened/closed in any pthread (not just in the CLI pthread) !
 *
 *     If the vio_vf is blocking, the file, pipe etc. read/write functions
 *     will simulate blocking (with time-out) using a mini-pselect().
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
   *   vin_type         = VIN_NONE          ) -- see uty_vin_push()
   *   vin_state        = vf_closed         )
   *
   *   vin_next         = NULL                -- see uty_vin_push()
   *
   *   context          = NULL                -- see uty_cmd_loop_prepare()
   *
   *   cli              = NULL  -- no CLI, yet
   *
   *   ibuf             = NULL  -- none, yet  -- see uty_vin_push()
   *   cl               = NULL  -- none, yet  -- see uty_vin_push()
   *   line_complete    = false               -- see uty_vin_push()
   *   line_number      = 0     -- nothing yet
   *   line_step        = 0                   -- see uty_vin_push()
   *
   *   vout_type        = VOUT_NONE         ) -- see uty_vout_push()
   *   vout_state       = vf_closed         )
   *
   *   obuf             = NULL  -- none       -- see uty_vout_push()
   *
   *   depth_mark       = 0                   -- see uty_vout_push()
   *   out_ordinary     = false               -- see uty_vout_push()
   *                                             and uty_cmd_loop_prepare()
   *   push_complete    = false               -- see uty_vout_push()
   *
   *   io_error         = false               -- so far, so good
   *
   *   blocking         = vio->blocking || io_type is blocking
   *
   *   vin_waiting      = false               -- not yet waiting
   *
   *   vfd              = NULL  -- no vfd, yet
   *
   *   read_timeout     = 0     -- none
   *   write_timeout    = 0     -- none
   *
   *   child            = NULL  -- none       -- see uty_pipe_read/write_open()
   *
   *   pr_state         = vf_closed         ) -- see uty_pipe_read/write_open()
   *   pr_enabled       = false             )
   *   pr_vfd           = NULL  -- no vfd   )
   *   pr_timeout       = 0     -- none     )
   *
   *   ps_state         = vf_closed         ) -- see uty_pipe_read/write_open()
   *   ps_enabled       = false             )
   *   ps_vfd           = NULL  -- no vfd   )
   *   ps_timeout       = 0     -- none     )
   *   ps_buf           = NULL  -- none, yet
   *
   *   fp               = NULL  -- no VOUT_VTYSH output (yet)
   *   pager            = NULL  -- no VOUT_VTYSH pager
   *   rbuf             = NULL  -- no results from client daemon
   *   ebuf             = NULL  -- no errors from client daemon(s)
   *   no_prefix        = false -- add [name] (default)
   */
  confirm((VIN_NONE == 0) && (VOUT_NONE == 0)) ;
  confirm(vf_closed == 0) ;

  if (vio->blocking)
    io_type |= vfd_io_ps_blocking ;     /* all I/O is at least this     */

  vf->vio = vio ;
  vf->blocking = (io_type & (vfd_io_ps_blocking | vfd_io_os_blocking)) != 0 ;

  if (name != NULL)
    vf->name = XSTRDUP(MTYPE_VTY_NAME, name) ;

  if (fd >= 0)
    vf->vfd = vio_vfd_new(fd, type, io_type, vf) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * Stop the given vin, and its pipe stderr return (ps), if any.
 *
 * Does not touch any underlying vfd, so can be run in any thread.
 *
 * The stop types are:
 *
 *  vfs_stop_end       -- set vf_end -- ps: unchanged.
 *
 *  vfs_stop_cease     -- ordinary cease, as when an "exit" is executed,
 *                        or get a parsing or command execution error.
 *
 *                        vin: set vf_cease
 *                             if there is no ps, set vf_end
 *                             if is vf_cancel,   set vf_end
 *
 *                        ps:  unchanged
 *
 *  vfs_stop_cancel    -- set vf_cease and vf_cancel and vf_end
 *                        -- on vin and any ps.
 *
 *                        This immediately terminates
 *
 *  vfs_stop_pause     -- N/A to vin... treat as vfs_stop_cancel.
 *
 *  vfs_stop_final     -- set vf_cease and vf_cancel and vf_end.
 *
 *                        For vin, this the same as vfs_stop_cancel.
 *
 * This can be done any number of times.
 */
extern void
uty_vf_read_stop(vio_vf vf, vfs_stop_t vstp)
{
  if (vf->vin_state == vf_closed)
    {
      qassert(vf->ps_state == vf_closed) ;
      qassert(vf->pr_state == vf_closed) ;

      return ;
    } ;

  qassert(vf->vin_state & vf_open) ;

  if (vf->vin_type != VIN_PIPE)
    qassert(vf->ps_state == vf_closed) ;

  switch (vstp)
    {
      case vfs_stop_end:
        vf->vin_state |= vf_end ;

        return ;                /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

      case vfs_stop_cease:
        vf->vin_state |= (vf_open | vf_cease) ;

        if ((vf->ps_state == vf_closed) || (vf->vin_state & vf_cancel))
          vf->vin_state |= vf_end ;

        return ;                /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

      case vfs_stop_cancel:
        break ;

      case vfs_stop_pause:
        qassert(vstp != vfs_stop_pause) ;
        break ;

      case vfs_stop_final:
      default:
        qassert(vstp == vfs_stop_final) ;
        break ;
    } ;

  vf->vin_state = (vf_open | vf_cease | vf_cancel | vf_end) ;
  uty_vf_return_stop(&vf->ps_state, vfs_stop_cancel) ;
} ;

/*------------------------------------------------------------------------------
 * If the given pr_state/ps_state is not closed, then stop it.
 *
 * Does not touch any underlying vfd, so can be run in any thread.
 *
 * The stop types are:
 *
 *  vfs_stop_end       -- set vf_end
 *
 *  vfs_stop_cease     -- set vf_cease
 *
 *                        if is vf_cancel, set vf_end.
 *
 *                        in the absence of vf_cancel, will read rest of input.
 *
 *  vfs_stop_cancel    -- set vf_cease and vf_cancel and vf_end.
 *
 *  vfs_stop_pause     -- treat as vfs_stop_cancel
 *
 *  vfs_stop_final     -- set vf_cease and vf_cancel and vf_end.
 *
 *                        For pr/ps, this the same as vfs_stop_cancel.
 *
 * NB: cannot have pr/ps and vout_base !!  However, if did then we are busy
 *     cancelling everything before continuing... treat as vfs_stop_cancel !
 *
 * This can be done any number of times.
 */
extern void
uty_vf_return_stop(vf_state_t* p_state, vfs_stop_t vstp)
{
  if (*p_state != vf_closed)
    {
      switch (vstp)
        {
          case vfs_stop_end:
            *p_state |= vf_end ;

            return ;            /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

          case vfs_stop_cease:
            *p_state |= (vf_open | vf_cease) ;

            if (*p_state & vf_cancel)
              *p_state |= vf_end ;

            return ;            /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

          case vfs_stop_cancel:
          case vfs_stop_pause:
            break ;

          case vfs_stop_final:
          default:
            qassert(vstp == vfs_stop_final) ;
            break ;
        } ;

      *p_state = (vf_open | vf_cease | vf_cancel | vf_end) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Close the read side of the given vio_vf -- if can.
 *
 * Read closes the vio_vfd -- if the vio_vfd was read-only, this will fully
 * close it, and the vio_vfd will have been freed.
 *
 * If there is any other related I/O, then waits or blocks until that has
 * completed.  For example, with a VIN_PIPE waits for any return input to
 * complete, and pushes it to the relevant vout, and then waits to collect
 * the child, and its termination state.
 *
 * Note that if is vst_final, will do any I/O, and will ignore errors.
 *
 * Returns:  CMD_SUCCESS  -- closed (guaranteed if vst_final)
 *           CMD_WAITING  -- waiting to complete close     => non-blocking
 *           CMD_IO_ERROR -- error or timeout
 */
static cmd_ret_t
uty_vf_read_close(vio_vf vf)
{
  cmd_ret_t  ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  ret = CMD_SUCCESS ;

  if (vf->vin_state == vf_closed)
    return ret ;                /* already closed               */

  qassert(vf->vin_state & vf_open) ;

  /* If not already ceased, cease the vin -- leaves any underlying vfd
   * for the VIN_xxx close, or for final disposition below.
   */
  uty_vf_read_stop(vf, vfs_stop_cease) ;

  /* Now the vin_type specific clean up.
   */
  switch(vf->vin_type)
  {
    case VIN_NONE:
      zabort("invalid VIN_NONE") ;
      break ;

    case VIN_TERM:
      ret = uty_term_read_close(vf) ;
      break ;

    case VIN_VTYSH_SERVER:
      ret = uty_sh_serv_read_close(vf) ;
      break ;

    case VIN_CONFIG:
    case VIN_FILE:
    case VIN_PIPE:
      ret = uty_file_read_close(vf) ;
      break ;

    case VIN_VTYSH:             /* vtysh *own* vty      */
    case VIN_DEV_NULL:
      ret = CMD_SUCCESS ;
      break ;

    default:
      zabort("unknown VIN type") ;
  } ;

  /* If did not succeed, return the bad news, unless is vst_final.
   */
  if ((ret != CMD_SUCCESS) && ((vf->vio->state & vst_final) == 0))
    return ret ;

  /* The vin is now closed -- make sure !vin_open and any underlying vfd
   *                          is read closed.
   */
  vf->vin_state = vf_closed ;
  vf->vfd       = vio_vfd_read_close(vf->vfd) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Stop the given vout, and its pipe return(s) (pr/ps), if any.
 *
 * Does not touch any underlying vfd, so can be run in any thread.
 *
 * The stop types are:
 *
 *  vfs_stop_end       -- signal that all buffers are empty, so ready to close.
 *
 *                        if is vf_cease, set vf_end.
 *
 *                        pr/ps: unaffected
 *
 *  vfs_stop_cease     -- ordinary cease, as when vout_depth > vin_depth.
 *
 *                        set vf_cease.
 *
 *                        pr/ps: unaffected
 *
 *  vfs_stop_cancel    -- set vf_cease and vf_cancel
 *
 *                        on pr/ps also set vf_end.
 *
 *  vfs_stop_pause     -- if not vout_base, treat as vfs_stop_cancel
 *
 *                        This means that all input other than vin_base, and
 *                        all output is being cancelled.  Once the hiatus has
 *                        completed that, both the vin_base and the vout_base
 *                        will have vf_cancel cleared, and will continue.
 *
 *                        if is vf_cease or vf_end already, treat as
 *                        vfs_stop_cancel
 *
 *  vfs_stop_final     -- set vf_cease and vf_cancel and vf_end.
 *
 * This can be done any number of times.
 *
 * Note that, unlike inputs, stop-cease and stop-cancel do *not* set
 * vf_end.  For stop-cease, we keep going until buffers are empty.  For
 * stop-cancel, may output something before finally closing.  Further,
 * stop-end only sets vf_end if is vf_cease.
 */
extern void
uty_vf_write_stop(vio_vf vf, vfs_stop_t vstp)
{
  if (vf->vout_state == vf_closed)
    {
      qassert(vf->ps_state == vf_closed) ;
      qassert(vf->pr_state == vf_closed) ;

      return ;
    } ;

  if ((vf->vout_type != VOUT_PIPE) && (vf->vout_type != VOUT_SH_CMD))
    qassert((vf->pr_state == vf_closed) && (vf->ps_state == vf_closed)) ;

  switch (vstp)
    {
      case vfs_stop_end:
        if (vf->vout_state & vf_cease)
          vf->vout_state |= vf_end ;

        return ;                /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

      case vfs_stop_cease:
        vf->vout_state |= vf_cease ;

        return ;                /* <-<- NB: exits here <-<-<-<-<-<-<-<- */

      case vfs_stop_pause:
        qassert(vf == vf->vio->vout_base) ;

        if (vf == vf->vio->vout_base)
          {
            vf->vout_state |= vf_cancel ;       /* assume will be well  */

            if (vf->vout_state  == (vf_open | vf_cancel))
              break ;                           /* valid result         */
          } ;

        vstp = vfs_stop_cancel ;
        fall_through ;

      case vfs_stop_cancel:
        vf->vout_state = (vf_open | vf_cease | vf_cancel)
                                                   | (vf->vout_state & vf_end) ;
        break ;

      default:
        qassert(vstp == vfs_stop_final) ;

        vstp = vfs_stop_final ;
        fall_through ;

      case vfs_stop_final:
        vf->vout_state = (vf_open | vf_cease | vf_cancel | vf_end) ;
        break ;
    } ;

  uty_vf_return_stop(&vf->pr_state, vstp) ;
  uty_vf_return_stop(&vf->ps_state, vstp) ;
} ;

/*------------------------------------------------------------------------------
 * Close the write side of the given vio_vf, if can.
 *
 * Discards anything beyond the current end_mark.
 *
 * Pushes any outstanding output.  For pipes will suck up any outstanding pipe
 * return stuff, and push that; when the pipe is dry, will attempt to collect
 * the child.  If cannot complete everything, will return CMD_WAITING for
 * non-blocking, or block (and may time out).  If returns CMD_WAITING, should
 * return here at a later date to continue the close.
 *
 * If is vst_final, then no I/O will be attempted and the vf *will* be closed.
 *
 * If this is the vout_base, does everything to close the vf, and clears
 * vf->vout_open and vf->vout_active, but does not actually close the
 * underlying vfd -- that is left for uty_close(), after uty_
 *
 * Returns:  CMD_SUCCESS  -- closed (guaranteed if vst_final)
 *           CMD_WAITING  -- waiting to complete close     => non-blocking
 *           CMD_IO_ERROR -- error or timeout
 *
 * NB: must not have open vins at this or a higher level in the stack.
 *
 * NB: does not at this stage discard the vf or the obuf.
 */
static cmd_ret_t
uty_vf_write_close(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  ret = CMD_SUCCESS ;

  if (vf->vout_state == vf_closed)
    return ret ;                        /* quit if already closed       */

  qassert(vf->vout_state & vf_open) ;

  uty_vf_write_stop(vf, vfs_stop_cease) ;

  /* Must always close vin before closing vout at the same level.
   */
  qassert(vf->vio->vin_depth < vf->vio->vout_depth) ;
  qassert(vf->vin_state == vf_closed) ;

  /* If there is anything in the obuf beyond the end_mark, then it is
   * assumed to be surplus to requirements.
   */
  vio_fifo_back_to_end_mark(vf->obuf) ;

  /* The vout_type specific close functions will attempt to write
   * everything away.
   *
   * NB: at this point is vout_open, but may not be vout_active.
   */
  switch(vf->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid VOUT_NONE") ;
      break ;

    case VOUT_TERM:
      ret = uty_term_write_close(vf) ;
      break ;

    case VOUT_VTYSH_SERVER:
      ret = uty_sh_serv_write_close(vf) ;
      break ;

    case VOUT_FILE:
    case VOUT_CONFIG:
    case VOUT_PIPE:
    case VOUT_SH_CMD:
      ret = uty_file_write_close(vf) ;
      break ;

    case VOUT_VTYSH:
      ret = uty_vtysh_out_close(vf) ;
      break ;

    case VOUT_DEV_NULL:
      ret = CMD_SUCCESS ;

    case VOUT_STDOUT:
      ret = uty_std_write_close(vf) ;
      break ;

    default:
      zabort("unknown VOUT type") ;
  } ;

  /* If did not succeed, return the bad news, unless is vst_final.
   */
  if ((ret != CMD_SUCCESS) && ((vf->vio->state & vst_final) == 0))
    return ret ;

  /* We are now ready to stop and close the vout.
   */
  vf->vout_state = vf_closed ;
  vf->vfd        = vio_vfd_close(vf->vfd) ;

  return CMD_SUCCESS ;
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
  qassert( (vf->vin_state == vf_closed) && (vf->vout_state == vf_closed)
                                        && (vf->pr_state   == vf_closed)
                                        && (vf->ps_state   == vf_closed) ) ;

  XFREE(MTYPE_VTY_NAME, vf->name) ;

  qassert(vf->cli   == NULL) ;
  qassert(vf->vtysh == NULL) ;

  vf->ibuf   = vio_fifo_free(vf->ibuf) ;
  vf->cl     = qs_reset(vf->cl, free_it) ;
  vf->obuf   = vio_fifo_free(vf->obuf) ;

  vf->context = cmd_context_free(vf->context) ; /* not a copy           */

  vf->vfd    = vio_vfd_close(vf->vfd) ;         /* for completeness     */
  vf->pr_vfd = vio_vfd_close(vf->pr_vfd) ;      /* for completeness     */
  vf->ps_vfd = vio_vfd_close(vf->ps_vfd) ;      /* for completeness     */

  vf->ps_buf = vio_fifo_free(vf->ps_buf) ;

  vf->rbuf   = vio_fifo_free(vf->rbuf) ;
  vf->ebuf   = vio_fifo_free(vf->ebuf) ;

  XFREE(MTYPE_VTY, vf) ;

  return NULL ;
} ;

/*==============================================================================
 * vio_vf level read/write ready and timeout setting
 */

/*------------------------------------------------------------------------------
 * Set required read-ready state.  Applies the current read timeout.
 *
 * Overrides request and sets off if: is vf_end
 *
 *                  Note that returns CMD_SUCCESS.
 *
 * Will end up off if:  vf->vfd == NULL or the vfd has been closed
 *                                      or the vfd is is not io_read
 *                                      or the vfd is non-blocking !
 *
 * NB: can set uty_vf_set_read_ready(..., off) whatever the state of the vf,
 *     including non-blocking or no vfd etc.  So, can call this in exit and
 *     close down code, and if read-ready is set, it will be cleared.
 *
 * Returns: CMD_WAITING  -- is on,  as requested
 *          CMD_SUCCESS  -- is off, as requested
 *                                  or because request was overridden
 *          CMD_IO_ERROR -- is off but on was requested and not overridden
 *                          NB: will have posted "?NOT-OPEN?"
 */
extern cmd_ret_t
uty_vf_set_read_ready(vio_vf vf, on_off_b how)
{
  on_off_b get ;

  if ((vf->vin_state & (vf_open | vf_end)) != vf_open)
    how = off ;

  get = vio_vfd_set_read(vf->vfd, how, vf->read_timeout) ;

  if (get == on)
    return CMD_WAITING ;

  if (how == off)
    return CMD_SUCCESS ;

  return uty_vf_not_open_error(vf, verr_vin) ;
} ;

/*------------------------------------------------------------------------------
 * Set required write-ready state.  Applies the current write timeout.
 *
 * Same behaviour as uty_vf_set_read_ready() -- but for write-ready.
 */
extern cmd_ret_t
uty_vf_set_write_ready(vio_vf vf, on_off_b how)
{
  on_off_b get ;

  if ((vf->vout_state & (vf_open | vf_end)) != vf_open)
    how = off ;

  get = vio_vfd_set_write(vf->vfd, how, vf->write_timeout) ;

  if (get == on)
    return CMD_WAITING ;

  if (how == off)
    return CMD_SUCCESS ;

  return uty_vf_not_open_error(vf, verr_vout) ;
} ;

/*==============================================================================
 * Command prompt is cached in the vty->vio to avoid repeated evaluation of
 * the prompt string.  The host name is part of the prompt, and each time the
 * host name is seen to change, the host name generation is updated (the
 * generation is never zero).
 */

/*------------------------------------------------------------------------------
 * Get the command prompt for the given node and the current host name.
 *
 * Use or update the vio prompt cache.
 *
 * The prompt has space at the end.
 */
extern const char*
uty_cmd_prompt(vty_io vio, node_type_t node)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  if (vio->vin->vin_type == VIN_VTYSH)
    {
      /* For all but the vtysh the vty watchdog will refresh the hostname
       * if that is changed elsewhere.
       *
       * For the vtysh, if we are at the command line or interactive levels,
       * we refresh the host name here.
       */
      cmd_host_name(true) ;
    } ;

  /* Note that vio->prompt_gen is initialised to zero, but host.name_gen is
   * never zero -- so is guaranteed to set up the vio->prompt the first time
   * through here.   */
  if ((node != vio->prompt_node) || (host.name_gen != vio->prompt_gen))
    {
      const char* prompt ;

      /* NB: qs_printf() creates a '\0' terminated string, and sets the
       * qstring length to match.
       */

      prompt = cmd_prompt(node) ;
      if (prompt != NULL)
        vio->prompt    = qs_printf(vio->prompt, prompt, host.name) ;
      else
        vio->prompt    = qs_printf(vio->prompt, "%s node ???: ",
                                                          cmd_node_name(node)) ;

      vio->prompt_node = node ;
      vio->prompt_gen  = host.name_gen ;
    } ;

  return qs_char_nn(vio->prompt) ;
} ;

/*==============================================================================
 * Other common I/O functions
 */

/*------------------------------------------------------------------------------
 * Try to complete a command line for the given vf, from the current input
 * fifo -- performs *no* I/O -- does uty_vf_read_stop() when fifo is empty
 * and vf_end indicates no more input is available.
 *
 * If gets complete command line, sets the given cmd_action -- including its
 * context.
 *
 * Expects that most of the time will be able to set the vf->cl to point
 * directly at a command line in the fifo -- but at the edge of fifo buffers
 * (and if get continuation lines) will copy line fragments to the vf->cl.
 *
 * Returns:  CMD_SUCCESS  -- have a command line
 *           CMD_HIATUS   -- there is no more or is vf_cease or vf_cancel
 *           CMD_WAITING  -- need more           => !vf_end and buffer empty
 *
 * If is vf_cease or vf_cancel, returns CMD_HIATUS, immediately.
 *
 * If vf_end, will return CMD_SUCCESS if have last part line in hand.
 * Otherwise will set vf_cease and return CMD_HIATUS -- note that does *not*
 * do this until is trying to fetch a line and there is nothing available.
 *
 * Note that vf_end may be cleared asynchronously -- so is only significant
 * when the ibuf is empty and have no part line in hand.
 */
extern cmd_ret_t
uty_fifo_cmd_line_fetch(vio_vf vf, bool cont_lines)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  if (vf->vin_state & (vf_cease | vf_cancel))
    return CMD_HIATUS ;

  if (vf->line_complete)
    {
      vio_fifo_set_hold_mark(vf->ibuf) ;        /* advance hold         */

      vf->line_complete = false ;
      vf->line_number  += vf->line_step ;

      qs_set_len_nn(vf->cl, 0) ;
      vf->line_step = 0 ;
    } ;

  while (1)
    {
      char* s, * p, * e ;
      ulen    have ;
      ulen    len ;
      bool    eol ;

      /* Get what we can from the fifo
       */
      have = vio_fifo_get(vf->ibuf) ;

      /* If fifo is empty, may be last line before eof, eof or waiting
       */
      if (have == 0)
        {
          if (vf->vin_state & vf_end)
            {
              if (qs_len_nn(vf->cl) > 0)
                break ;                 /* have non-empty last line     */

              uty_vf_read_stop(vf, vfs_stop_cease) ;
              return CMD_HIATUS ;
            } ;

          return CMD_WAITING ;          /* should be more to come       */
        } ;

      /* Try to find a '\n' -- converting all other control chars to ' '
       *
       * When we find '\n' step back across any trailing ' ' (which includes
       * any control chars before the '\n').
       *
       * This means that we cope with "\r\n" line terminators.  But not
       * anything more exotic.
       */
      p = s = vio_fifo_get_ptr(vf->ibuf) ;
      e = s + have ;       /* have != 0    */

      eol = false ;
      while (p < e)
        {
          if (*p++ < 0x20)
            {
              if (*(p-1) != '\n')
                {
                  *(p-1) = ' ' ;        /* everything other than '\n'   */
                  continue ;
                } ;

              ++vf->line_step ;         /* got a '\n'                   */

              eol = true ;
              break ;
            } ;
        } ;

      /* Step past what have just consumed -- we have a hold_mark, so
       * stuff is still in the fifo.
       */
      vio_fifo_step(vf->ibuf, p - s) ;

      /* If not found '\n', then we have a line fragment that needs to be
       * appended to any previous line fragments.
       *
       * Loops back to try to get some more form the fifo.
       */
      if (!eol)
        {
          qs_append_n(vf->cl, s, p - s) ;
          continue ;
        } ;

      /* Have an eol.  Step back across the '\n' and any trailing spaces
       * we have in hand.
       */
      do --p ; while ((p > s) && (*(p-1) == ' ')) ;

      /* If we have nothing so far, set alias to point at what we have in
       * the fifo.  Otherwise, append to what we have.
       *
       * End up with: s = start of entire line, so far.
       *              p = end of entire line so far.
       */
      len = p - s ;                     /* length to add        */
      if (qs_len_nn(vf->cl) == 0)
        qs_set_alias_n(vf->cl, s, len) ;
      else
        {
          if (len != 0)
            qs_append_n(vf->cl, s, len) ;

          s = qs_char_nn(vf->cl) ;
          p = s + qs_len_nn(vf->cl) ;

          if ((len == 0) && (p > s) && (*(p-1) == ' '))
            {
              /* Have an empty end of line section, and the last character
               * of what we have so far is ' ', so need now to trim trailing
               * spaces off the stored stuff.
               */
              do --p ; while ((p > s) && (*(p-1) == ' ')) ;

              qs_set_len_nn(vf->cl, p - s) ;
            } ;
        } ;

      /* Now worry about possible trailing '\'.                         */

      if (!cont_lines || (p == s) || (*(p-1) != '\\'))
        break ;                 /* no \ => no continuation => success   */

      /* Have a trailing '\'.
       *
       * If there are an odd number of '\', strip the last one and loop
       * round to collect the continuation.
       *
       * If there are an even number of '\', then this is not a continuation.
       *
       * Note that this rule deals with the case of the continuation line
       * being empty... e.g. ....\\\     n     n  -- where n is '\n'
       */
      e = p ;
      do --p ; while ((p > s) && (*(p-1) == '\\')) ;

      if (((e - p) & 1) == 0)
        break ;                 /* even => no continuation => success   */

      qs_set_len_nn(vf->cl, p - s - 1) ;        /* strip odd '\'        */

      continue ;                /* loop back to fetch more              */
    } ;

  /* Success: have a line in hand                                       */

  vf->line_complete = true ;

  vf->context->to_do = cmd_do_command ;
  vf->context->line  = vf->cl ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * I/O error and timeout handling.
 *
 * Posts error information and locus to the vio, which is signalled by a
 * CMD_IO_ERROR return code.
 */

/*------------------------------------------------------------------------------
 * If no other error has been posted for the vf, post a "?NOT-OPEN?" error.
 *
 * This is a get out of jail mechanism to cope with unexpected events.  The
 * code is generally careful to avoid doing things with a vf if it is not
 * vf_open.  When things go wrong, a vf will be set vf_end so will not try to
 * do I/O once the vf has failed.
 *
 * If, however, some I/O operation has started, but a vf is found to no longer
 * be open as expected/required, this function can be called.  It is *most*
 * likely that the vf is not open because it has failed, and an error has
 * already been posted, in which case no further message is required.  If not,
 * then something is not right, and we post a "?NOT-OPEN?" error, so that this
 * is at least visible (even if not explicable).
 *
 * In any case, the caller can return CMD_IO_ERROR safe in the knowledge that
 * the error handling will close things down cleanly, with a suitable error
 * message and logging.
 *
 * Returns:  CMD_IO_ERROR
 */
extern cmd_ret_t
uty_vf_not_open_error(vio_vf vf, vio_err_type_t err_type)
{
  return uty_vf_error(vf, verr_not_open | err_type, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Dealing with an I/O error or time-out on the given vio_vf.
 *
 *
 *
 * If there is a stack of inputs and/or outputs, it is not at all clear what
 * it would mean if an error occurred somewhere in the stack, and the rest
 * of the stack tried to keep going -- some part of the result would be
 * incomplete, but it would not be clear which part.  So, take the simple
 * approach of cancelling everything -- keeping the vin_base/vout_base, unless
 * the error occurred in either (or both).
 *
 *   * if the error is not in the vin_base or the vout_base, then cancel
 *
 *   * if the error is in the vin_base or the vout_base, stop the vty and
 *     stop log monitor.
 *
 *   * if the error is an I/O error, and the error is in the vout_base, then
 *     stop everything, final !
 *
 *     There is no point going on if the vout_base has failed.  Note that this
 *     test picks up an I/O error in the vin_base, if the vin_base and
 *     vout_base are the same.
 *
 *   * if the error is an output timeout on the vout_base, then also stop
 *     everything, final !
 *
 *     There is no point going on if the vout_base has timed out.
 *
 * Note that all error messages are output to vout_base -- unless that has
 * failed or timed out.  Error processing will:
 *
 *   * produce suitable log message.
 *
 *     Logs all real I/O errors.  After the first error the vf will be set
 *     not-open, so should not be any more real I/O errors, but if there are,
 *     seems wrong not to log same.  (I/O errors are rare as hens teeth, so
 *     probably better not to throw away information.)
 *
 *     For other errors -- time-out, vtysh, and not-open -- after the first
 *     error those are almost certainly artifacts, so are ignored.
 *
 *   * unless we are vst_final: insert suitable message in vio->ebuf, if this
 *     is the first error on the vf.
 *
 *     After the first error on a vf, any subsequent errors are very likely
 *     related to the first error, so no point cluttering the output.  If
 *     subsequent errors do matter, see the logging.
 *
 *   * if we are vst_final, set the "close_reason" to the error message.
 *
 *     when closing "final", the close_reason is shoved at the vout_base,
 *     ignoring errors, just in case can get the information away.  If not,
 *     there's the logging.
 *
 * In any event signals CMD_IO_ERROR to the command loop.  CMD_IO_ERROR may be
 * signalled any number of times.  Returning CMD_IO_ERROR as well as signalling
 * is fine, too.
 *
 * Returns:  CMD_IO_ERROR -- which may be returned to the command loop, as
 *                           well as the vio->signal.
 *       or  CMD_SUCCESS  => is vst_final, already
 */
extern cmd_ret_t
uty_vf_error(vio_vf vf, vio_err_type_t err_type, int err)
{
  vio_exception_t vx ;
  vty_io          vio ;
  bool            was_final ;

  VTY_ASSERT_LOCKED() ;

  vio = vf->vio ;

  /* Decide how to handle the error and post the exception.
   */
  if ((vf != vio->vin_base) && (vf != vio->vout_base))
    vx = vx_io_error ;
  else
    {
      vx = vx_stop_io_error ;

      if (vf == vio->vout_base)
        {
          if ((err_type & verr_io) || ((err_type & verr_mask) == verr_vout))
            vx = vx_stop_final ;
        } ;
    } ;

  was_final = (vio->state & vst_final) != 0 ;

  uty_vio_exception(vio, vx) ;

  /* Log error and create error message as required.
   *
   * Note that we log the first error on the vf and all I/O errors.
   *
   * We only output an error message for the first error on the vf, and then
   * only if was not vst_final before the error.
   */
  if (!vf->io_error || (err_type & verr_io))
    zlog_warn("%s", uty_error_message(vf, err_type, err, true /* log */).str) ;

  if (!vf->io_error && !was_final)
    {
      verr_mess_t err_mess ;

      err_mess = uty_error_message(vf, err_type, err, false /* not log */) ;

      if (vx == vx_stop_final)
        uty_close_reason_set(vio, err_mess.str, true /* replace */) ;
      else
        vio_fifo_printf(uty_cmd_get_ebuf(vio), "%% %s\n", err_mess.str) ;
    } ;

  /* Signal to the command loop and return CMD_IO_ERROR.
   *
   * One or both will be collected in the command loop "hiatus" and dealt
   * with -- it does not matter if both arrive.
   */
  return uty_cmd_signal(vio, CMD_IO_ERROR) ;
} ;

/*------------------------------------------------------------------------------
 * Construct error message for given I/O or time-out error
 *
 * Construct for logging the error or for output.
 *
 * Message does not include any leading decoration (so no "% "), nor does it
 * have a trailing "\n" (but is "\0" terminated).
 */
extern verr_mess_t
uty_error_message(vio_vf vf, vio_err_type_t err_type, int err, bool log)
{
  verr_mess_t QFB_QFS(qfb, qfs) ;

  const char* name ;
  const char* where ;
  const char* what ;
  const char* sort ;
  bool   vout ;
  int    fd ;

  VTY_ASSERT_LOCKED() ;

  vout = false ;
  fd   = -1 ;
  what = "*unknown verr_xxx (bug)*" ;

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
        vout = (vf->vout_state & vf_open) ;
        what = "stderr return" ;
        if (log)
          fd = vio_vfd_fd(vf->ps_vfd) ;
        break ;

      default:
        qassert(false) ;
    } ;

  name  = vf->name ;
  where = NULL ;

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

          case VOUT_VTYSH_SERVER:
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

          case VOUT_VTYSH:      /* vtysh *own* vty      */
            where = "vtysh" ;
            break ;

          case VOUT_STDOUT:
            where = "stdout" ;
            break ;

          default:
            qassert(false) ;

            where = "*unknown VOUT_XXX (bug)*" ;
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

          case VIN_VTYSH_SERVER:
            where = "VTY Shell Server" ;
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

          case VIN_VTYSH:       /* vtysh *own* vty      */
            where = "vtysh" ;
            break ;

          case VIN_DEV_NULL:
            where = "/dev/null" ;
            break ;

          default:
            qassert(false) ;

            where = "*unknown VIN_XXX (bug)*" ;
            break ;
        } ;
    } ;

  if      ((err_type & verr_io) != 0)
    sort = "I/O error" ;
  else if ((err_type & verr_vtysh) != 0)
    sort = "vtysh error" ;
  else if ((err_type & verr_not_open) != 0)
    sort = "?NOT-OPEN?" ;
  else
    sort = "time-out" ;

  qfs_printf(qfs, "%s %s %s", where, what, sort) ;

  if (name != NULL)
    qfs_printf(qfs, " '%s'", name) ;

  if (fd >= 0)
    qfs_printf(qfs, " (fd=%d)", fd) ;

  if      (((err_type & verr_io) != 0) && (err != 0))
    qfs_printf(qfs, ": %s", errtoa(err, 0).str) ;
  else if ((err_type & verr_vtysh) != 0)
    {
      switch (err)
        {
          case verr_vtysh_vin_eof:      /* eof in the middle of something */
            qfs_printf(qfs, ": unexpected eof") ;
            break ;

          case verr_vtysh_vin_nonce:
            qfs_printf(qfs, ": unexpected nonce value") ;
            break ;

          case verr_vtysh_vin_type:     /* unrecognised message type      */
            qfs_printf(qfs, ": unknown message type") ;
            break ;

          case verr_vtysh_vin_length:   /* invalid length field           */
            qfs_printf(qfs, ": invalid message length") ;
            break ;

          default:
            qfs_printf(qfs, ": *unknown error %d (bug)*", err) ;
            break ;
        } ;
    } ;

  if (qfs_term(qfs) != 0)
    qfs_term_string(qfs, "...", sizeof("...")) ;

  return qfb ;
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
 *
 * NB: this is used in the cmd_read_config() command loop *only*, only one
 *     instance of which runs at any time.
 */
extern void
vty_child_signal_nexus_set(vty vty)
{
  VTY_LOCK() ;

  qassert(vty->vio->blocking) ;
  qassert(vty_child_signal_nexus == NULL) ;

  if (!vty_is_cli_thread())
    {
      qpt_mutex_lock(vty_child_signal_mutex) ;

      vty_child_signal_nexus = qpn_find_self() ;

      qpt_mutex_unlock(vty_child_signal_mutex) ;
    } ;

  VTY_UNLOCK() ;
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
vty_child_signal_nexus_clear(vty vty)
{
  VTY_LOCK() ;

  qassert(vty->vio->blocking) ;

  if (!vty_is_cli_thread())
    {
      qpt_mutex_lock(vty_child_signal_mutex) ;

      vty_child_signal_nexus = NULL ;

      qpt_mutex_unlock(vty_child_signal_mutex) ;
    } ;

  VTY_UNLOCK() ;
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
 * This is for non-blocking.
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
 * all non-blocking child handling is done in the CLI thread.
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
 * This is for blocking, or for "final" non-blocking.
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
          qps_mini_set(qm, -1, 0) ;
          qm->timeout_set = false ;

          if (vty_is_cli_thread())
            sig_mask  = NULL ;
          else
            sig_mask  = vty_child_signal_nexus->pselect_mask ;

          first = false ;
        } ;

      /* Wait on pselect -- no fd set, so this is a timer.
       */
      if (qps_mini_wait(qm, 6, sig_mask) >= -1)
        final = true ;          /* timed out or error => now final      */
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
  qassert(!child->parent->blocking) ;

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
 * Have listeners for VTY_TERMINAL and VTY_VTYSH_SERVER types of VTY.
 */

/* List of listeners so can tidy up.                                    */
static vio_listener vty_listeners_list  = NULL ;

/*------------------------------------------------------------------------------
 * Open VTY listener(s) for VTY_TERMINAL and VTY_VTYSH_SERVER.
 *
 *   host.vty_listen_addr   -- address ) to listen for VTY_TERMINAL connections
 *   host.vty_listen_port   -- port    )
 *   host.vtysh_listen_path -- path for VTY_VTYSH_SERVER connections...
 *                                                          ...if VTYSH_ENABLED
 */
extern void
uty_open_listeners(void)
{
  VTY_ASSERT_LOCKED() ;

  /* If port is set to 0, do not listen for VTY_TERMINAL at all!        */
  if (host.vty_listen_port != 0)
    uty_term_open_listeners(host.vty_listen_addr, host.vty_listen_port) ;

  /* If want to listen for vtysh, set up listener now                   */
  if (VTYSH_ENABLED && (host.vtysh_listen_path != NULL))
    uty_sh_serv_open_listener(host.vtysh_listen_path) ;
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

  XFREE(MTYPE_HOST, host.vty_accesslist_name) ;
                                /* sets host.vty_accesslist_name = NULL */

  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
                           /* sets host.vty_ipv6_accesslist_name = NULL */

  while ((listener = ssl_pop(&listener, vty_listeners_list, next)) != NULL)
    {
      vio_listener_close(listener) ; /* no ceremony, no flowers      */
    } ;
} ;

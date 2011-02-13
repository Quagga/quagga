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

static vty_timer_time uty_watch_dog_bark(vio_timer_t* timer, void* info) ;
static bool uty_death_watch_scan(bool final) ;

/*------------------------------------------------------------------------------
 * Initialise watch dog -- at start-up time.
 */
extern void
uty_watch_dog_init(void)
{
  vio_timer_init(&vty_watch_dog, NULL, NULL) ; /* empty         */
} ;

/*------------------------------------------------------------------------------
 * Start watch dog -- before a VTY is created.
 */
extern void
uty_watch_dog_start(void)
{
  vio_timer_init(&vty_watch_dog, uty_watch_dog_bark, NULL) ;
  vio_timer_set(&vty_watch_dog, VTY_WATCH_DOG_INTERVAL) ;
} ;

/*------------------------------------------------------------------------------
 * Stop watch dog timer -- at close down.
 *
 * Final run along the death-watch
 */
extern void
uty_watch_dog_stop(void)
{
  vio_timer_reset(&vty_watch_dog) ;
  uty_death_watch_scan(true) ;  /* scan the death-watch list            */
}

/*------------------------------------------------------------------------------
 * Watch dog vio_timer action
 */
static vty_timer_time
uty_watch_dog_bark(vio_timer_t* timer, void* info)
{
  cmd_host_name(true) ;         /* check for host name change           */

  uty_death_watch_scan(false) ; /* scan the death-watch list            */

  return VTY_WATCH_DOG_INTERVAL ;
} ;

/*------------------------------------------------------------------------------
 * Scan the death watch list.
 *
 * A vty can finally be freed if it is closed and there is no command running.
 *
 * At curtains the command running state is forced off.
 */
static bool
uty_death_watch_scan(bool curtains)
{
  vty_io  vio ;
  vty_io  next ;

  next = vio_death_watch ;
  while (next != NULL)
    {
      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      /* If this is curtains, override cmd_running !            */
      if (curtains)
        vio->cmd_running = false ;

      if (uty_close(vio, curtains, NULL))
        {
          vty vty = vio->vty ;

          sdl_del(vio_death_watch, vio, vio_list) ;

          cmd_exec_free(vty->exec) ;
          XFREE(MTYPE_VTY, vty->vio) ;
          XFREE(MTYPE_VTY, vty) ;
        } ;
    } ;

  return (vio_death_watch == NULL) ;
} ;

/*==============================================================================
 * Prototypes.
 */

static void uty_vf_read_close(vio_vf vf) ;
static bool uty_vf_write_close(vio_vf vf, bool final) ;
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
 *   * constructing a suitable vio_vf and doing uty_vin_open() to set the
 *     vin_base.
 *
 *     All vty_io MUST have a vin_base, even if it is /dev/null.
 *
 *   * constructing a suitable vio_vf and doing uty_vout_open() to set the
 *     vout_base.
 *
 *     All vty_io MUST have a vout_base, even if it is /dev/null.
 *
 *   * setting the vio->obuf to the vout_base obuf...   TODO ???
 *
 *   * setting vio->cli, if required.
 *
 * Caller must also set the initial vty->node, if any.
 */
extern vty
uty_new(vty_type_t type)
{
  vty      vty ;
  vty_io   vio ;

  bool             execution ;

  VTY_ASSERT_LOCKED() ;

  /* Basic allocation                                                   */

  vty = XCALLOC(MTYPE_VTY, sizeof(struct vty)) ;
  /* Zeroising the vty structure has set:
   *
   *   type      = X          -- set to actual type, below
   *
   *   node      = NULL_NODE  -- set to something sensible elsewhere
   *
   *   index     = NULL       -- nothing, yet
   *   index_sub = NULL       -- nothing, yet
   *
   *   config    = false      -- not in configure mode
   *
   *   execution = X          -- set below
   *   vio       = X          -- set below
   */
  confirm(NULL_NODE == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;

  vty->type   = type ;

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
   *   vout         = NULL   -- empty output stack
   *   vout_base    = NULL   -- empty output stack
   *   vout_depth   = 0      -- no stacked vout's, yet
   *
   *   vout_closing = NULL   -- nothing closing yet
   *
   *   vio_list     = NULLs  -- not on the vio_list, yet
   *   mon_list     = NULLs  -- not on the monitors list
   *
   *   blocking     = X      -- set below: false unless VTY_CONFIG_READ
   *   cmd_running  = false  -- no commands running, yet
   *
   *   state        = X      -- set vf_open, below.
   *
   *   close_reason = NULL   -- not closed, yet
   *
   *   obuf         = NULL   -- no output buffer, yet
   */

  vty->vio = vio ;
  vio->vty = vty ;

  vio->blocking = (type == VTY_CONFIG_READ) ;
  vio->state    = vf_open ;

  /* Create and initialise the command execution environment (if any)   */
  execution = true ;

  switch(type)
  {
    case VTY_TERMINAL:
    case VTY_SHELL_SERVER:
    case VTY_SHELL_CLIENT:
    case VTY_CONFIG_READ:
      execution = true ;
      break ;

    case VTY_STDOUT:
    case VTY_STDERR:
      execution = false ;
      break ;

    default:
      zabort("unknown vty type") ;
  } ;

  if (execution)
    vty->exec = cmd_exec_new(vty) ;

  /* Place on list of known vio/vty                                     */
  sdl_push(vio_list_base, vio, vio_list) ;

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
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 *
 *     That is not done here because the vin state may not be complete (in
 *     particular the vin->parse_type and vin->reflect_enabled !).
 */
extern void
uty_vin_open(vty_io vio, vio_vf vf, vio_in_type_t type,
                                    vio_vfd_action* read_action,
                                    vio_timer_action* read_timer_action,
                                    usize ibuf_size)
{
  vf->vin_type  = type ;
  vf->vin_state = vf_open ;

  if (type < VIN_SPECIALS)
    {
      vio_vfd_set_read_action(vf->vfd, read_action) ;
      vio_vfd_set_read_timeout_action(vf->vfd, read_timer_action) ;
    } ;

  ssl_push(vio->vin, vf, vin_next) ;
  if (vio->vin_base == NULL)
    {
      assert(vio->vin_depth == 0) ;
      vio->vin_base = vf ;
    }
  else
    {
      assert(type != VIN_NONE) ;
      ++vio->vin_depth ;
    } ;

  if (ibuf_size != 0)
    {
      vf->ibuf = vio_fifo_init_new(NULL, ibuf_size) ;

      vf->line_complete = true ;
      vf->line_step     = 1 ;
    } ;
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
 *
 *     That is not done here because the vout state may not be complete (in
 *     particular the vout->out_enabled !).
 */
extern void
uty_vout_open(vty_io vio, vio_vf vf, vio_out_type_t type,
                                    vio_vfd_action* write_action,
                                    vio_timer_action* write_timer_action,
                                    usize obuf_size)
{
  vf->vout_type  = type ;
  vf->vout_state = vf_open ;

  if (type < VOUT_SPECIALS)
    {
      vio_vfd_set_write_action(vf->vfd, write_action) ;
      vio_vfd_set_write_timeout_action(vf->vfd, write_timer_action) ;
    } ;

  ssl_push(vio->vout, vf, vout_next) ;
  if (vio->vout_base == NULL)
    {
      assert(vio->vout_depth == 0) ;
      vio->vout_base = vf ;
    }
  else
    {
      assert(type != VOUT_NONE) ;
      ++vio->vout_depth ;
    } ;

  vf->obuf = vio_fifo_init_new(NULL, obuf_size) ;
  vio_fifo_set_end_mark(vf->obuf) ;

  vio->obuf = vio->vout->obuf ;
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
 * Set/Clear "monitor" state:
 *
 *  set:   if VTY_TERM and not already "monitor" (and write_open !)
 *  clear: if is "monitor"
 */
extern void
uty_set_monitor(vty_io vio, bool on)
{
  VTY_ASSERT_LOCKED() ;
#if 0
  if      (on && !vio->monitor)
    {
      if ((vio->vty->type == VTY_TERMINAL) && !vio->closing)
        {
          vio->monitor = 1 ;
          sdl_push(vio_monitors_base, vio, mon_list) ;
        } ;
    }
  else if (!on && vio->monitor)
    {
      vio->monitor = 0 ;
      sdl_del(vio_monitors_base, vio, mon_list) ;
    }
#endif
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
 * Close all vin excluding the vin_base.
 *
 * This is done on close and when there is a command error.
 */
extern void
uty_vin_close_stack(vty_io vio)
{
  while (vio->vin != vio->vin_base)
    uty_vin_close(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Close all vout except for the vout_base.
 *
 * This is done on close and when there is a command error.
 *
 * Should only be "final" on a call from the watch-dog !
 */
extern void
uty_vout_close_stack(vty_io vio, bool final)
{
  while (vio->vout != vio->vout_base)
    uty_vout_close(vio, final) ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY -- may be called any number of times !
 *
 * If no reason for the close has already been set, sets the given reason.
 * The close reason is reported at the end of the output to the vout_base.
 *
 * Once a VTY has been closed, it will provide no further input.  However, until
 * "final" close, it will continue to output anything which is pending at the
 * time of the close, which includes: (a) anything in the output buffer,
 * (b) any further output from a then running command and (c) anything that an
 * out_pipe may be in the process of returning.
 *
 * The main complication is that in a multi-threaded world there may be a
 * vio->cmd_running.  The close shuts down everything on the input stack, so
 * any active command loop will come to a halt as soon as the current command
 * does complete.  The output stack is preserved, but will be closed on
 * completion of the command.
 *
 * Closing a VTY places it on the death-watch list.  Once any pending output
 * has been dealt with and any cmd_running has completed, then the death-watch
 * will apply the coup de grace.
 *
 * Getting the death-watch to finally close the VTY also allows the vty to be
 * closed from somewhere deep (e.g. when there is an I/O error) without
 * destroying the VTY and upsetting code that might not realise the VTY has
 * vanished.
 *
 * The close sequence is:
 *
 *   1. if a close reason has not already been set, set any given one.
 *
 *   2. if not already closing -- ie this is the first call of uty_close()
 *
 *       a. transfer to death-watch list & set closing.
 *
 *       b. turn off any monitor state
 *
 *       c. close down the input/read side completely.
 *
 *          Empties the vin stack down to vin_base, closing all input.
 *
 *          Close the vin_base.  The vin_base is left !read_open.
 *
 *          All read-only vio_vf (except the vin_base) are freed.
 *
 *          Note that everything is read closed before anything is write
 *          closed.
 *
 *          A vio_vf is read closed once and only once.
 *
 *          The vin_base is closed only by this function.  Until the final
 *          uty_close, there is always at least the vin_base, even if it is
 *          read_closed.
 *
 *   3. try to close everything on the vout_closing list.
 *
 *      even if cmd_running
 *
 *
 */
extern bool
uty_close(vty_io vio, bool final, qstring reason)
{
  vio_vf vf_next ;

  VTY_ASSERT_LOCKED() ;

  /* quit if already closed                                             */
  if (vio->state == vf_closed)
    return true ;

  /* Set the close reason, if not already set.                          */
  if (reason != NULL)
    {
      if (vio->close_reason == NULL)
        vio->close_reason = reason ;
      else
        qs_reset(reason, free_it) ;
    } ;

  /* If not already closing, set closing and transfer to the death watch
   * list -- turn off any "monitor" status immediately.
   */
  if (vio->state == vf_open)
    {
      vio->state = vf_closing ;

      /* Transfer to the death-watch                                    */
      sdl_del(vio_list_base, vio, vio_list) ;
      sdl_push(vio_death_watch, vio, vio_list) ;

      /* TODO turn off any "monitor" IMMEDIATELY.                       */

      /* Flush the vin stack.
       *
       * Where possible this will revoke commands bring command processing to
       * as sudden a halt as possible -- though may still be processing
       * commands.
       */
      uty_vin_close_stack(vio) ;
      assert(vio->vin == vio->vin_base) ;
      uty_vf_read_close(vio->vin) ;
    } ;

  /* If there is anything on the vout_closing list, give it a shove in case
   * that manages to close anything -- which it will do if "final".
   */
  vf_next = vio->vout_closing ;
  while (vf_next != NULL)
    {
      vio_vf vf = vf_next ;
      vf_next = ssl_next(vf, vout_next) ;

      uty_vf_write_close(vf, final) ;   /* removes from vout_closing if
                                           is now completely closed.    */
    } ;

  /* If is vio->cmd_running, this is as far as we can go this time.     */
  if (vio->cmd_running)
    return false ;

  /* Make sure no longer holding the config symbol of power             */
  uty_config_unlock(vio->vty, NULL_NODE) ;

  /* Flush the vout stack.
   *
   * If cannot completely close a vf, places it on the vout_closing list,
   * except for the vout_base.
   */
  uty_vout_close_stack(vio, final) ;
  assert(vio->vout == vio->vout_base) ;
  uty_vf_write_close(vio->vout, final) ;

  /* See if have now successfully closed everything.                    */
  if ((vio->vout_closing != NULL) || (vio->vout_base->vout_state != vf_closed))
    return false ;              /* something is still open      */

  /* Empty everything out of the vio and return the good news.
   *
   * NB: retains vio->vty & the vio->vio_list for death-watch.
   */
  assert((vio->vin == vio->vin_base) && (vio->vin_depth == 0)) ;

  if (vio->vin != vio->vout)
    vio->vin = uty_vf_free(vio->vin) ;
  else
    vio->vin = NULL ;
  vio->vin_base = NULL ;

  assert((vio->vout == vio->vout_base) && (vio->vout_depth == 0)) ;

  vio->vout = uty_vf_free(vio->vout) ;

  vio->close_reason = qs_reset(vio->close_reason, free_it) ;

  vio->obuf = NULL ;

  vio->state = vf_closed ;
  return true ;
} ;

/*------------------------------------------------------------------------------
 * Pop and close the top of the vin stack -- MUST NOT be vin_base.
 *
 * Read closes the vio_vfd -- if the vio_vfd was read-only, this will fully
 * close it, and the vio_vfd will have been freed.
 *
 * If this is read-only will free the vio_vf and all its contents.
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 *
 *     That is not done here because this may be called from outside the
 *     command loop -- in particular by uty_close().
 */
extern void
uty_vin_close(vty_io vio)
{
  vio_vf vf ;

  vf = vio->vin ;

  assert(vf != vio->vin_base) ;
  assert(vio->vin_depth > 0) ;

  ssl_del_head(vio->vin, vin_next) ;
  --vio->vin_depth ;

  uty_vf_read_close(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Pop and close the top of the vout stack -- MUST NOT be vout_base.
 *
 * Moves the vio_vf to the vout_closing list.
 *
 * Before closing, push any outstanding output.
 *
 * Unless "final", the close is soft, that is, if there is any output still
 * outstanding does not actually close the vout.
 *
 * If there is no outstanding output (or if final) will completely close the
 * vf and free it.
 *
 * NB: a uty_cmd_prepare() is required before command processing can continue.
 *
 *     That is not done here because this may be called from outside the
 *     command loop -- in particular by uty_close().
 */
extern void
uty_vout_close(vty_io vio, bool final)
{
  vio_vf vf ;

  vf = vio->vout ;

  assert(vf != vio->vout_base) ;
  assert(vio->vout_depth > 0) ;

  ssl_del_head(vio->vout, vout_next) ;
  --vio->vout_depth ;

  ssl_push(vio->vout_closing, vf, vout_next) ;

  vio->obuf = vio->vout->obuf ;

  uty_vf_write_close(vf, final) ;
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
 *   - uty_vin_open() and/or uty_vout_open()
 *
 *   - once those are done, the following optional items remain to be set
 *     if they are required:
 *
 *       read_timeout     -- default = 0     => no timeout
 *       write_timeout    -- default = 0     => no timeout
 *
 *       parse_type       -- default = cmd_parse_standard
 *       reflect_enabled  -- default = false
 *       out_enabled      -- default = true iff vfd_io_write
 *
 * If vio->blocking, adds vfd_io_blocking to the io_type.
 *
 * NB: if there is no fd for this vio_vf, it should be set to -1, and the
 *     type (recommend vfd_none) and io_type are ignored -- except for
 *     vfd_io_write and the out_enabled state.
 *
 *     A VTY_STDOUT or a VTY_STDERR (output only, to the standard I/O) can
 *     be set up without an fd, and with/without out_enabled.
 *
 *     A VTY_CONFIG_READ can be set up with the fd of the input file, but
 *     MUST be type = vfd_file and io_type = vfd_io_read -- because the fd
 *     is for input only.  This will set out_enabled false (as required).
 *     The required VOUT_STDERR will be set by uty_vout_open().
 */
extern vio_vf
uty_vf_new(vty_io vio, const char* name, int fd, vfd_type_t type,
                                              vfd_io_type_t io_type)
{
  vio_vf  vf ;

  VTY_ASSERT_LOCKED() ;

  if (vio->blocking)
    io_type |= vfd_io_blocking ;

  vf = XCALLOC (MTYPE_VTY, sizeof(struct vio_vf)) ;

  /* Zeroising the structure has set:
   *
   *   vio              = X     -- set below
   *   name             = X     -- set below
   *
   *   vin_type         = VIN_NONE            -- see uty_vin_open()
   *   vin_state        = vf_closed           -- see uty_vin_open()
   *   vin_next         = NULL                -- see uty_vin_open()
   *
   *   cli              = NULL  -- no CLI, yet
   *
   *   ibuf             = NULL  -- none, yet  -- see uty_vin_open()
   *
   *   cl               = zeros -- empty qstring (embedded)
   *   line_complete    = false               -- see uty_vout_open()
   *   line_number      = 0     -- nothing yet
   *   line_step        = 0                   -- see uty_vout_open()
   *
   *   parse_type       = cmd_parse_standard
   *   reflect_enabled  = false
   *
   *   vout_type        = VOUT_NONE           -- see uty_vout_open()
   *   vout_state       = vf_closed           -- see uty_vout_open()
   *   vout_next        = NULL                -- see uty_vout_open()
   *
   *   obuf             = NULL  -- none, yet  -- see uty_vout_open()
   *
   *   out_enabled      = X     -- set below: true iff vfd_io_write
   *
   *   vfd              = NULL  -- no vfd, yet
   *
   *   blocking         = false -- default is non-blocking I/O
   *   closing          = false -- definitely not
   *
   *   error_seen       = 0     -- no error seen, yet
   *
   *   read_on          = false
   *   write_on         = false
   *
   *   read_timeout     = 0     -- none
   *   write_timeout    = 0     -- none
   */
  confirm((VIN_NONE == 0) && (VOUT_NONE == 0)) ;
  confirm(vf_closed == 0) ;
  confirm(QSTRING_INIT_ALL_ZEROS) ;
  confirm(cmd_parse_standard == 0) ;

  vf->vio = vio ;

  if (name != NULL)
    vf->name = XSTRDUP(MTYPE_VTY_NAME, name) ;

  if (fd >= 0)
    vf->vfd = vio_vfd_new(fd, type, io_type, vf) ;

  vf->out_enabled = ((io_type & vfd_io_write) != 0) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * Close the read side of the given vio_vf.
 *
 * Read closes the vio_vfd -- if the vio_vfd was read-only, this will fully
 * close it, and the vio_vfd will have been freed.
 *
 * If this is read-only (ie vout_type == VOUT_NONE) will free the vio_vf as
 * well as the vio_vfd -- unless this is the vin_base.
 *
 * Note that read_close is effective immediately, there is no delay as there is
 * with write_close -- so no need for a "final" option.
 */
static void
uty_vf_read_close(vio_vf vf)
{
  assert(vf->vin_state != vf_closed) ;

  /* Do the vfd level read close and mark the vf no longer read_open    */
  if (vf->vin_type < VIN_SPECIALS)
    vf->vfd = vio_vfd_read_close(vf->vfd) ;

  vf->vin_state = vf_closed ;
  vf->read_on   = off ;

  /* Now the vin_type specific clean up.                                */
  switch(vf->vin_type)
  {
    case VIN_NONE:
      zabort("invalid VIN_NONE") ;
      break ;

    case VIN_TERM:
      uty_term_read_close(vf) ;
      break ;

    case VIN_VTYSH:
      break ;

    case VIN_FILE:
      uty_file_read_close(vf) ;
      break ;

    case VIN_PIPE:
      break ;

    case VIN_CONFIG:
      break ;

    case VIN_DEV_NULL:
      break ;

    default:
      zabort("unknown VIN type") ;
  } ;

  if ((vf->vout_type == VOUT_NONE) && (vf != vf->vio->vin_base))
    uty_vf_free(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Close the write side of the given vio_vf, if can.
 *
 * Pushes any outstanding output -- non-blocking if not a blocking vty.
 *
 * The vio_vf MUST either be on the vout_closing list or be vout_base.
 * The vio_vf MUST be write_open and MUST NOT be read_open.
 *
 * If output is empty, or if final, close the vf, then if it is on the
 * vout_closing list, remove and free it.
 *
 * Returns whether output was empty or not.
 */
static bool
uty_vf_write_close(vio_vf vf, bool final)
{
  bool  empty ;

  assert((vf->vout_state != vf_closed) && (vf->vin_state == vf_closed)) ;

  /* Worry about remaining return input from vout pipe        TODO      */

  /* Worry about appending the close reason to the vout_base  TODO      */

  /* Now the vout_type specific clean up.                               */
  empty = false ;

  switch(vf->vout_type)
  {
    case VOUT_NONE:
      zabort("invalid VOUT_NONE") ;
      break ;

    case VOUT_TERM:
      empty = uty_term_write_close(vf, final) ;
      break ;

    case VOUT_VTYSH:
      break ;

    case VOUT_FILE:
      empty = uty_file_write_close(vf, final) ;
      break ;

    case VOUT_PIPE:
      break ;

    case VOUT_CONFIG:
      break ;

    case VOUT_DEV_NULL:
    case VOUT_STDOUT:
    case VOUT_STDERR:
      empty = true ;
      break ;

    default:
      zabort("unknown VOUT type") ;
  } ;

  if (empty || final)
    {
      /* Do the vfd level close and mark the vf no longer write_open        */
      if (vf->vout_type < VOUT_SPECIALS)
        vf->vfd = vio_vfd_close(vf->vfd) ;
      else
        assert(vf->vfd == NULL) ;

      vf->vout_state = vf_closed ;
      vf->write_on   = off ;

      if (ssl_del(vf->vio->vout_closing, vf, vout_next))
        uty_vf_free(vf) ;
      else
        assert(vf == vf->vio->vout_base) ;
    } ;

  return empty ;
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
  XFREE(MTYPE_VTY_NAME, vf->name) ;

  vf->ibuf = vio_fifo_reset(vf->ibuf, free_it) ;
  vf->obuf = vio_fifo_reset(vf->obuf, free_it) ;

  qs_reset(vf->cl, keep_it) ;

  vf->cli = uty_cli_close(vf->cli, true) ;
  vf->vfd = vio_vfd_close(vf->vfd) ;   /* for completeness     */

  XFREE(MTYPE_VTY, vf) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 *
 *
 */





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
  uty_set_monitor(vio, 0) ;

  /* if this is the first error, log it                                 */
  if (vf->error_seen == 0)
    {
      const char* type = "?" ;  /* TODO */


      vf->error_seen = err ;
      uzlog(NULL, LOG_WARNING, "%s: %s failed on fd %d: %s",
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
 * Returns new state of vf->read_on
 */
extern on_off_b
uty_vf_set_read(vio_vf vf, on_off_b how)
{
  if (vf->vin_state != vf_open)
    how = off ;

  return vf->read_on = (vf->vin_state != vf_closed)
                          ? vio_vfd_set_read(vf->vfd, how, vf->read_timeout)
                          : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required read ready timeout -- if already read_on, restart it.
 *
 * Returns new state of vf->read_on
 */
extern on_off_b
uty_vf_set_read_timeout(vio_vf vf, vty_timer_time read_timeout)
{
  vf->read_timeout = read_timeout ;

  return vf->read_on ? uty_vf_set_read(vf, on)
                     : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready state.  Applies the current write timeout.
 *
 * Forces off if:   vf->vout_state != vf_open
 *
 * Does nothing if: vf->vout_state == vf_closed
 *              or: vf->vfd == NULL
 *
 * Returns new state of vf->write_on
 */
extern on_off_b
uty_vf_set_write(vio_vf vf, on_off_b how)
{
  if (vf->vout_state != vf_open)
    how = off ;

  return vf->write_on = (vf->vout_state != vf_closed)
                           ? vio_vfd_set_write(vf->vfd, how, vf->write_timeout)
                           : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready timeout -- if already write_on, restart it.
 *
 * Returns new state of vf->write_on
 */
extern on_off_b
uty_vf_set_write_timeout(vio_vf vf, vty_timer_time write_timeout)
{
  vf->write_timeout = write_timeout ;

  return vf->write_on ? uty_vf_set_write(vf, on)
                      : off ;
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

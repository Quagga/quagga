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
#include "vty_cli.h"
#include "qstring.h"
#include "keystroke.h"
#include "list_util.h"

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
 *
 *
 */

/*------------------------------------------------------------------------------
 * UTY output function -- cf fprintf
 *
 * NB: this is NOT suppressed by ! vty->output_enabled
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
uty_output(struct vty *vty, const char *format, ...)
{
  int     ret ;
  va_list args ;

  VTY_ASSERT_LOCKED() ;

  va_start (args, format) ;
  ret = uty_vprintf(vty, format, args) ;
  va_end (args) ;

  return ret ;
}

/*------------------------------------------------------------------------------
 * UTY reflect command line, if not already reflected
 *
 * NB: this is NOT suppressed by ! vty->output_enabled
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 */
extern int
uty_reflect(struct vty *vty)
{
  int   ret ;

  if (!vty->reflected)
    ret = uty_output(vty, "%s\n", vty->buf) ;
  else
    ret = 0 ;

  vty->reflected = true ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * VTY output function -- cf vfprintf
 *
 * Returns: >= 0 => OK
 *          <  0 => failed (see errno)
 *
 * NB: for VTY_TERM and for VTY_SHELL_SERV -- this is command output:
 *
 *     * MAY NOT do any command output if !cmd_enabled
 *
 *        * first, the life of a vty is not guaranteed unless cmd_in_progress,
 *          so should not attempt to use a vty anywhere other than command
 *          execution.
 *
 *        * second, cmd_out_enabled is false most of the time, and is only
 *          set true when a command completes, and it is time to write away
 *          the results.
 *
 *     * all output is placed in the vio->cmd_obuf.  When the command completes,
 *       the contents of the cmd_obuf will be written away -- subject to line
 *       control.
 *
 *     * output is discarded if the vty is no longer write_open
 */
extern int
uty_vprintf(struct vty *vty, const char *format, va_list args)
{
  vio_vf vf ;
  int    ret ;

  VTY_ASSERT_LOCKED() ;

  vf = vty->vio->vout ;

  switch (vf->vout_type)
  {
    case VOUT_NONE:
      ret = 0 ;
      break ;

    case VOUT_TERM:
      ret = uty_term_vprintf(vf, format, args) ;
      break ;

    case VOUT_SHELL:
      ret = uty_shell_vprintf(vf, format, args) ;
      break ;

    case VOUT_FILE:
      ret = uty_file_vprintf(vf, format, args) ;
      break ;

    case VOUT_PIPE:
      ret = uty_pipe_vprintf(vf, format, args) ;
      break ;

    case VOUT_STDOUT:
      ret = uty_stdout_vprintf(vf, format, args) ;
      break ;

    case VOUT_STDERR:
      ret = uty_stderr_vprintf(vf, format, args) ;
      break ;

    default:
      zabort("impossible VTY Output type") ;
  } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Clear the contents of the command output FIFO etc.
 *
 * NB: does not change any of the cli_blocked/cmd_in_progress/cli_wait_more/etc
 *     flags -- competent parties must deal with those
 */
extern void
uty_out_clear(vty_io vio)
{
  VTY_ASSERT_LOCKED() ;

  if (vio->vout != NULL)
    {
      vio_fifo_clear(vio->vout->obuf) ;
      vio_lc_clear(vio->vout->olc) ;
    } ;
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
  uty_check_host_name() ;       /* check for host name change           */

  uty_death_watch_scan(false) ; /* scan the death-watch list            */

  return VTY_WATCH_DOG_INTERVAL ;
} ;

/*------------------------------------------------------------------------------
 * Scan the death watch list.
 *
 * A vty may finally be freed if it is closed and there is no command in
 * progress.
 */
static bool
uty_death_watch_scan(bool final)
{
  vty_io  vio ;
  vty_io  next ;

  next = vio_death_watch ;
  while (next != NULL)
    {
      vio  = next ;
      next = sdl_next(vio, vio_list) ;

      if (final && !vio->closed)


      if (vio->closed)


      if (vio->closed && !vio->cmd_in_progress)
        {
          uty_close(vio) ;      /* closes again to ensure that all buffers
                                   are released.                            */

          sdl_del(vio_death_watch, vio, vio_list) ;

          XFREE(MTYPE_VTY, vio->vty) ;
          XFREE(MTYPE_VTY, vio) ;
        } ;
    } ;

  return (vio_death_watch == NULL) ;
} ;

/*==============================================================================
 * Prototypes.
 */

static void uty_vf_half_close(vio_vf vf) ;
static void uty_vf_close(vio_vf vf) ;

/*==============================================================================
 * Creation and destruction of VTY objects
 */

/*------------------------------------------------------------------------------
 * Allocate new vty struct
 *
 *   VTY_TERMINAL         a telnet terminal server
 *
 *     Must be in cli thread.
 *
 *     Requires fd = socket for telnet terminal.
 *
 *   VTY_SHELL_SERVER     a vty_shell server
 *
 *     Must be in cli thread.
 *
 *     Requires fd = socket for talking to the VTY_SHELL_CLIENT
 *
 *   VTY_SHELL_CLIENT     a vty_shell client
 *
 *   VTY_CONFIG_READ      configuration file reader
 *
 *     Requires fd = file descriptor for reading config file
 *
 *   VTY_STDOUT           stdout
 *   VTY_STDERR           stderr
 *
 *
 *
 *
 *
 *
 *
 *
 * Allocates and initialises basic vty and vty_io structures, setting the
 * given type.
 *
 * Note that where is not setting up a vty_file, this *may* be called from
 * any thread.
 *
 * Returns: new vty
 */
extern struct vty *
uty_new(enum vty_type type, int fd)
{
  vty      vty ;
  vty_io   vio ;

  VTY_ASSERT_LOCKED() ;

  /* Basic allocation                                                   */

  vty = XCALLOC (MTYPE_VTY, sizeof (struct vty)) ;
  vio = XCALLOC (MTYPE_VTY, sizeof (struct vty_io)) ;

  vty->vio = vio ;
  vio->vty = vty ;

  /* Zeroising the vty_io structure has set:
   *
   *   name                = NULL -- no name, yet
   *
   *   vio_list      both pointers NULL
   *   mon_list      both pointers NULL
   *
   *   half_closed         = 0    -- NOT half closed (important !)
   *   closed              = 0    -- NOT closed      (important !)
   *   close_reason        = NULL -- no reason, yet
   *
   *   real_type           = 0    -- not material
   *   file_fd             = 0    -- not material
   *   file_error          = 0    -- not material
   *
   *   key_stream          = NULL -- no key stream (always empty, at EOF)
   *
   *   cli_drawn           = 0    -- not drawn
   *   cli_dirty           = 0    -- not dirty
   *   cli_prompt_len      = 0 )
   *   cli_extra_len       = 0 )     not material
   *   cli_echo_suppress   = 0 )
   *
   *   cli_prompt_node     = 0    -- not material
   *   cli_prompt_set      = 0    -- so prompt needs to be constructed
   *
   *   cli_blocked         = 0    -- not blocked
   *   cmd_in_progress     = 0    -- no command in progress
   *   cmd_out_enabled     = 0    -- command output is disabled
   *   cli_wait_more       = 0    -- not waiting for response to "--more--"
   *
   *   cli_more_enabled    = 0    -- not enabled for "--more--"
   *
   *   cmd_out_done        = 0    -- not material
   *
   *   cli_do              = 0    == cli_do_nothing
   *
   *   cmd_lc              = NULL -- no line control
   *
   *   fail                = 0    -- no login failures yet
   *
   *   hist                = empty vector
   *   hp                  = 0    -- at the beginning
   *   hindex              = 0    -- the beginning
   *
   *   width               = 0    -- unknown console width
   *   height              = 0    -- unknown console height
   *
   *   lines               = 0    -- no limit
   *   lines_set           = 0    -- no explicit setting
   *
   *   monitor             = 0    -- not a monitor
   *   monitor_busy        = 0    -- not a busy monitor
   *
   *   config              = 0    -- not holder of "config" mode
   */
  confirm(cli_do_nothing == 0) ;
  confirm(AUTH_NODE == 0) ;     /* default node type    */

  vty->type   = type ;





  /* Zeroising the vty structure has set:
   *
   *   node      = 0  TODO: something better for node value ????
   *   buf       = NULL -- no command line, yet
   *   parsed    = NULL -- no parsed command, yet
   *   lineno    = 0    -- nothing read, yet
   *   index     = NULL -- nothing, yet
   *   index_sub = NULL -- nothing, yet
   */

  /* If this is a VTY_TERM or a VTY_SHELL, place     */
  switch (type)
  {
    case VTY_TERMINAL:          /* Require fd -- Telnet session         */
      VTY_ASSERT_CLI_THREAD() ;
      assert(fd >= 0) ;

      uty_term_new(vio, fd) ;
      break ;

    case VTY_SHELL_SERVER:      /* Require fd -- Unix socket            */
      VTY_ASSERT_CLI_THREAD() ;
      assert(fd >= 0) ;


      break ;

    case VTY_CONFIG_READ:       /* Require fd -- file to read           */
      assert(fd >= 0) ;
      break ;

    case VTY_STDOUT:
    case VTY_STDERR:
    case VTY_SHELL_CLIENT:
      fd = -1 ;                 /* No fd -- output to stdout/stderr     */
      break ;

    default:
      zabort("unknown VTY type") ;
  } ;




  /* Make sure all buffers etc. are initialised clean and empty.
   *
   * Note that no buffers are actually allocated at this stage.
   */
  qs_init_new(&vio->cli_prompt_for_node, 0) ;

  qs_init_new(&vio->cl,  0) ;
  qs_init_new(&vio->clx, 0) ;

  vio_fifo_init_new(&vio->cli_obuf, 2 * 1024) ; /* allocate in 2K lumps */

  vio_fifo_init_new(&vio->cmd_obuf, 8 * 1024) ;

  /* Place on list of known vio/vty                                     */
  sdl_push(vio_list_base, vio, vio_list) ;

  return vty;
} ;

/*------------------------------------------------------------------------------
 * Add a new vf to the vio->vin stack, and set read stuff.
 *
 * Sets the vf->vin_type and set vf->read_open.
 *
 * Sets the read ready action and the read timer timeout action.
 *
 * NB: may add a VIN_NONE *only* as the first vin item.
 *
 *     Can have a write only VTY_XXX object, which requires a vin entry so that
 *     do not have to everywhere deal with NULL vio_vf pointers.
 *
 *     But for subsequent write only vio_vf objects, must not add to the vin
 *     stack.
 */
extern void
uty_vin_add(vty_io vio, vio_vf vf, vio_in_type_t type,
                vio_fd_action* read_action, vio_timer_action* read_timer_action)
{
  vf->vin_type  = type ;
  vf->read_open = true ;

  vio_fd_set_read_action(vf->vfd, read_action) ;
  vio_fd_set_read_timeout_action(vf->vfd, read_timer_action) ;

  ssl_push(vio->vin, vf, vin_next) ;
  if (vio->vin_base == NULL)
    vio->vin_base = vf ;
  else
    assert(type != VIN_NONE) ;
} ;

/*------------------------------------------------------------------------------
 * Add a new vf to the vio->vout stack, and set write stuff.
 *
 * Sets the vf->vout_type and set vf->write_open.
 *
 * Sets the write ready action and the write timer timeout action.
 *
 * NB: may add a VOUT_NONE *only* as the first vout item.
 *
 *     Can have a read only VTY_XXX object, which requires a vout entry so that
 *     do not have to everywhere deal with NULL vio_vf pointers.
 *
 *     But for subsequent read only vio_vf objects, must not add to the vout
 *     stack.
 */
extern void
uty_vout_add(vty_io vio, vio_vf vf, vio_out_type_t type,
              vio_fd_action* write_action, vio_timer_action* write_timer_action)
{
  vf->vout_type  = type ;
  vf->write_open = true ;

  vio_fd_set_write_action(vf->vfd, write_action) ;
  vio_fd_set_write_timeout_action(vf->vfd, write_timer_action) ;

  ssl_push(vio->vout, vf, vout_next) ;
  if (vio->vout_base == NULL)
    vio->vout_base = vf ;
  else
    assert(type != VOUT_NONE) ;
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

  if      (on && !vio->monitor)
    {
      if ((vio->vty->type == VTY_TERMINAL) && !vio->half_closed)
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
} ;

/*------------------------------------------------------------------------------
 * Return "name" of VTY
 *
 * For VTY_TERM this is the IP address of the far end of the telnet connection.
 */
extern const char*
uty_get_name(vty_io vio)
{
  return (vio->name != NULL) ? vio->name : "?" ;
} ;

/*------------------------------------------------------------------------------
 * Close all the readers.
 *
 * Half-close everything on the vin stack.  Empties the vin stack down to the
 * base entry.  Discards any read-only vio_vf (except for last vin entry).
 *
 * VIO is placed on death watch, and will stay there until:
 *
 *   * outstanding commands complete.
 *
 *   * outstanding output completes, or times out, or program terminates.
 *
 * For VTY_TERMINAL  (must be in CLI thread):
 *
 *   * shut the socket for reading
 *   * discard all buffered input, setting it to "EOF"
 *   * turns off any monitor status !
 *   * drop down to RESTRICTED_NODE
 *
 * For VTY_SHELL_SERVER  (must be in CLI thread):
 *
 *   * shut the socket for reading
 *   * discard all buffered input
 *   * drop down to RESTRICTED_NODE
 *
 * If no reason for the close has already been set, sets the given reason.
 *
 * If already half-closed, does nothing else.
 *
 * Returns true <=> first half close.
 */
static bool
uty_do_half_close(vty_io vio, const char* reason)
{
  vio_vf vf ;

  VTY_ASSERT_LOCKED() ;

  if ((vio->close_reason == NULL) && (reason != NULL))
    vio->close_reason = XSTRDUP(MTYPE_TMP, reason) ;

  if (vio->half_closed)
    return false ;

  /* Half close everything on the vin stack.
   *
   * Leave stack with just the base entry (closed for read).
   */
  while (1)
    {
      vf = vio->vin ;                   /* Current first on list        */

      uty_vf_half_close(vf) ;           /* fd level half close etc.     */

      switch(vf->vin_type)              /* tidy up each type            */
      {
        case VIN_NONE:
          break ;

        case VIN_TERM:
          uty_term_half_close(vf) ;
          uty_cli_close(vio) ;        /* tell the CLI to stop             */
          break ;

        case VIN_SHELL:
          break ;

        case VIN_FILE:
          break ;

        case VIN_PIPE:
          break ;

        case VIN_CONFIG:
          break ;

        default:
          zabort("unknown VIN type") ;
      } ;

      /* Finished if half closed the last on the list                   */
      if (vf == vio->vin_base)
        break ;

      /* Hack off head of list.  If it is read-only, close & free.      */
      ssl_del_head(vio->vin, vin_next) ;

      if (vf->vout_type == VOUT_NONE)
        uty_vf_close(vf) ;
    } ;

  /* Make sure no longer holding the config symbol of power             */
  uty_config_unlock(vio->vty, RESTRICTED_NODE) ;

  /* Move to the death watch list                                       */
  sdl_del(vio_list_base, vio, vio_list) ;
  sdl_push(vio_death_watch, vio, vio_list) ;

  vio->half_closed = true ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Close VTY.
 *
 *
 *
 * If no reason for the close has already been set, sets the given reason.
 *
 * If not already half-closed, close all readers as described above, and then
 * kick everything on the vout stack and apply VTY_HALF_CLOSE_TIMEOUT.
 *
 *
 */
extern void
uty_close (vty_io vio, const char* reason)
{
  if (uty_do_half_close(vio, reason))
    {
      vio_vf vf ;

      /* Run down the vout stack, and write-ready enable everything with
       * the half close timeout set.
       *
       * This will have the effect of kicking all output.
       */
      vf = vio->vout ;
      while (vf != NULL)
        {
          vf->write_timeout = 0 ;
          uty_vf_set_write(vf, on) ;

          vf = ssl_next(vf, vout_next) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Closing down VTY.
 *
 * Shuts down everything and discards all buffers etc. etc.
 *
 * If cmd_in_progress, cannot complete the process -- but sets the closed
 * flag.
 *
 * Can call vty_close() any number of times.
 *
 * The vty structure is placed on death watch, which will finally free the
 * structure once no longer cmd_in_progress.
 *
 *
 * If no reason for the close has already been set, sets the given reason.
 *
 */
extern void
uty_close_final(vty_io vio, const char* reason)
{
  VTY_ASSERT_LOCKED() ;

  /* If not already closed, close.                                      */
  uty_do_half_close(vio, reason) ;  /* set reason if required, and
                                       make sure is half closed.        */
  if (!vio->closed)
    {
      vio_vf vf ;

      /* Empty the vin stack                                            */
      assert(vio->vin != NULL) ;
      assert(vio->vin == vio->vin_base) ;
      if (vio->vin != vio->vout_base)
        uty_vf_close(vio->vin) ;
      vio->vin_base = vio->vin = NULL ;

      /* Now kick everything in the vout stack, in case can get stuff
       * written away.  And then close.
       */
      while (ssl_pop(&vf, vio->vout, vout_next) != NULL)
        {
          uty_vf_set_write(vf, off) ;   /* stop any write ready...      */
          vf->closing = true ;          /* ...permanently.              */

          switch(vf->vout_type)         /* tidy up each type            */
          {
            case VOUT_NONE:
              break ;

            case VOUT_TERM:
              break ;

            case VOUT_SHELL:
              break ;

            case VOUT_FILE:
              break ;

            case VOUT_PIPE:
              break ;

            case VOUT_STDOUT:
              break ;

            case VOUT_STDERR:
              break ;

            default:
              zabort("unknown VOUT type") ;
          } ;

          uty_vf_close(vf) ;            /* close and free               */
        } ;

      vio->vout_base = NULL ;           /* stack is empty               */
      vio->closed = true ;              /* now closed                   */
    } ;

  /* Nothing more should happen, so can now release almost everything,
   * the exceptions being the things that are related to a cmd_in_progress.
   *
   * All writing to buffers is suppressed, and as the sock has been closed,
   * there will be no more read_ready or write_ready events.
   */
  if (vio->name != NULL)
    XFREE(MTYPE_VTY_NAME, vio->name) ;

  vio->key_stream = keystroke_stream_free(vio->key_stream) ;

  qs_reset(&vio->cli_prompt_for_node, keep_it) ;
  qs_reset(&vio->cl, keep_it) ;

  {
    qstring line ;
    while ((line = vector_ream(vio->hist, keep_it)) != NULL)
      qs_reset_free(line) ;
  } ;

  /* The final stage cannot be completed if cmd_in_progress.
   *
   * The clx is pointed at by vty->buf -- containing the current command.
   *
   * Once everything is released, can take the vty off death watch, and
   * release the vio and the vty.
   */
  if (!vio->cmd_in_progress)
    {
      qs_reset(&vio->clx, keep_it) ;
      vio->vty->buf = NULL ;
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
 * This leaves most things unset/NULL/false.  Caller will want to set:
 *
 *
 *
 * Sets timeout to no timeout at all -- timeout is optional.
 */
extern vio_vf
uty_vf_new(vty_io vio, int fd, vfd_type_t type, vfd_io_type_t io_type)
{
  vio_vf  vf ;

  VTY_ASSERT_LOCKED() ;

  vf = XCALLOC (MTYPE_VTY, sizeof(struct vio_vf)) ;

  /* Zeroising the structure has set:
   *
   *   vin_type       = 0     -- VIN_NONE
   *   vin_next       = NULL  -- not on a vin list, yet
   *
   *   vout_type      = NULL  -- VOUT_NONE
   *   vout_next      = NULL  -- not on a vout list, yet
   *
   *   obuf           = NULL  -- none, yet
   *   olc            = NULL  -- none, yet
   *
   *   blocking       = false
   *   closing        = false
   *
   *   read_open      = false
   *   write_open     = false
   *   error_seen     = 0     -- no error seen, yet
   *
   *   read_timeout   = 0     -- none
   *   write_timeout  = 0     -- none
   */
  confirm((VIN_NONE == 0) && (VOUT_NONE == 0)) ;

  vf->vio = vio ;
  vf->vfd = vio_fd_new(fd, type, io_type, vf) ;

  return vf ;
} ;

/*------------------------------------------------------------------------------
 * Half close a vio_vf.
 *
 * Half closes the vio_fd and shuts down all reading.  If the vio_fd was
 * read-only, the half-close will fully close it, and the vio_fd will have
 * been freed.
 */
static void
uty_vf_half_close(vio_vf vf)
{
  /* the vfd level half close will close completely if is read only.    */
  vf->vfd = vio_fd_half_close(vf->vfd) ;

  vf->read_open = false ;       /* no more read operations      */
  vf->read_on   = off ;         /* of course                    */

  if (vf->vfd == NULL)          /* check really was read-only   */
    assert(!vf->write_open && !vf->read_on) ;
} ;

/*------------------------------------------------------------------------------
 * Close given vio_vf and free the vio_vf structure and all its contents.
 *
 * Assumes has been removed from vio->vin and vio->vout !
 */
static void
uty_vf_close(vio_vf vf)
{
  vf->vfd  = vio_fd_close(vf->vfd) ;

  vf->obuf = vio_fifo_reset_free(vf->obuf) ;
  vf->olc  = vio_lc_reset_free(vf->olc) ;

  XFREE(MTYPE_VTY, vf) ;
} ;

/*------------------------------------------------------------------------------
 * Dealing with an I/O error on VTY socket
 *
 * If this is the first error for this VTY, produce suitable log message.
 *
 * If is a "monitor", turn that off, *before* issuing log message.
 */
static int
uty_vf_error(vty_io vio, const char* what)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  /* can no longer be a monitor !  *before* any logging !               */
  uty_set_monitor(vio, 0) ;

  /* if this is the first error, log it                                 */
  if (vio->sock.error_seen == 0)
    {
      const char* type ;
      switch (vio->vty_type)
      {
        case VTY_TERM:
          type = "VTY Terminal" ;
          break ;
        case VTY_SHELL:
          type = "VTY Shell Server" ;
          break ;
        default:
          zabort("unknown VTY type for uty_file_error()") ;
      } ;

      vio->sock.error_seen = errno ;
      uzlog(NULL, LOG_WARNING, "%s: %s failed on fd %d: %s",
                type, what, vio->sock.fd, errtoa(vio->sock.error_seen, 0).str) ;
    } ;

  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Set required read ready state.  Applies the current read timeout.
 *
 * Forces off if:   vf->closing
 *
 * Does nothing if: !vf->read_open
 *              or: vf->vfd == NULL
 */
extern on_off_t
uty_vf_set_read(vio_vf vf, on_off_t how)
{
  if (vf->closing)
    how = off ;
  return vf->read_on = vf->read_open
                          ? vio_fd_set_read(vf->vfd, how, vf->read_timeout)
                          : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required read ready timeout -- if already read_on, restart it.
 *
 * Forces read ready off if: vf->closing
 *
 * Does nothing if: !vf->read_open
 *              or: vf->vfd == NULL
 */
extern on_off_t
uty_vf_set_read_timeout(vio_vf vf, vty_timer_time read_timeout)
{
  vf->read_timeout = read_timeout ;
  return vf->read_on ? uty_vf_set_read(vf, on)
                     : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready state.  Applies the current write timeout.
 *
 * Forces off if:   vf->closing
 *
 * Does nothing if: !vf->write_open
 *              or: vf->vfd == NULL
 */
extern on_off_t
uty_vf_set_write(vio_vf vf, on_off_t how)
{
  if (vf->closing)
    how = off ;

  return vf->write_on = vf->write_open
                           ? vio_fd_set_write(vf->vfd, how, vf->write_timeout)
                           : off ;
} ;

/*------------------------------------------------------------------------------
 * Set required write ready timeout -- if already write_on, restart it.
 *
 * Forces write ready off if: vf->closing
 *
 * Does nothing if: !vf->write_open
 *              or: vf->vfd == NULL
 */
extern on_off_t
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

  /* If want to listen for vtysh, set up listener now                   */
  if (VTYSH_ENABLED && (path != NULL))
    uty_serv_vtysh(path) ;
} ;

/*------------------------------------------------------------------------------
 * Create listener and set it ready to accept.
 *
 * Adds to list of listeners for close down.
 */
extern void
uty_add_listener(int fd, vio_fd_accept* accept_action)
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

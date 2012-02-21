/* VTY I/O for Files and Pipes
 *
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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
#include <fcntl.h>
#include <sys/wait.h>
#include <errno.h>

#include "command_local.h"

#include "vty_io.h"
#include "vty_io_file.h"
#include "vty_io_std.h"
#include "vty_io_basic.h"
#include "vty_command.h"
#include "network.h"
#include "sigevent.h"
#include "pthread_safe.h"
#include "qlib_init.h"

/*==============================================================================
 * Here we handle:
 *
 *   * VIN_CONFIG/VOUT_CONFIG  -- configuration file read/write
 *
 *   * VIN_FILE/VOUT_FILE      -- file pipe read/write
 *
 *   * VIN_PIPE/VOUT_PIPE      -- shell command pipe read/write
 */

#define PIPEFILE_MODE CONFIGFILE_MASK

static void uty_file_read_ready(vio_vfd vfd, void* action_info, bool time_out) ;

/*==============================================================================
 * VTY Configuration file I/O -- VIN_CONFIG and VOUT_CONFIG.
 *
 * The creation of configuration files is handled elsewhere, as is the actual
 * opening of input configuration files.
 *
 * Reading configuration uses VIN_FILE read and close.
 *
 * Writing configuration uses VOUT_FILE write, but has its own close.
 */

/*------------------------------------------------------------------------------
 * Push vio_vf to read configuration file -- using already open fd.
 *
 * Assumes the given path is a complete path -- from /
 *
 * Sets the "here" directory to the same as the given path.
 *
 * Apart from the type (VIN_CONFIG) and the buffer size, this is the same as
 * a VIN_FILE.
 */
extern void
uty_config_read_open(vty_io vio, int fd, qpath path)
{
  vio_vf  vf ;

  VTY_ASSERT_LOCKED() ;

  /* Create and push new VIN_CONFIG vio_vf.
   */
  vf = uty_vf_new(vio, qpath_string(path), fd, vfd_file, vfd_io_read) ;

  uty_vin_push(vio, vf, VIN_CONFIG, uty_file_read_ready, config_buffer_size) ;
  vf->read_timeout = file_timeout ;

  uty_vin_set_here(vio, path) ;
} ;

/*------------------------------------------------------------------------------
 * Push the given fd as the VOUT_CONFIG.
 *
 * NB: the fd is expected to be non-blocking, but is written as pseudo-
 *     blocking for simplicity but will time-out reasonably promptly.
 *
 * NB: the vfd is marked vfd_io_no_close so that when vout is closed the fd
 *     is not actually closed, because configuration writer still need it.
 */
extern void
vty_config_write_open(vty vty, int fd)
{
  vty_io vio ;
  vio_vf vf ;

  VTY_LOCK() ;

  vio = vty->vio ;

  vf = uty_vf_new(vio, "config write", fd, vfd_file,
                          vfd_io_write | vfd_io_ps_blocking | vfd_io_no_close) ;
  uty_vout_push(vio, vf, VOUT_CONFIG, NULL, file_buffer_size,
                                                      false /* not "after" */) ;
  vf->write_timeout = file_timeout ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Write away any pending stuff, and pop the VOUT_CONFIG.
 *
 * It is assumed that the output has been pushed, and since this is
 * vfd_io_ps_blocking, there should be no outstanding output, unless
 * something has failed.
 */
extern cmd_ret_t
vty_config_write_close(struct vty* vty)
{
  cmd_ret_t ret ;

  VTY_LOCK() ;

  qassert(vty->vio->vout->vout_type == VOUT_CONFIG) ;

  ret = uty_vout_pop(vty->vio) ;

  VTY_UNLOCK() ;

  return ret ;
} ;

/*==============================================================================
 * VTY File I/O -- VIN_FILE and VOUT_FILE
 *                 also read/write/close for VIN_CONFIG and VOUT_CONFIG.
 *
 * This is for input and output of configuration and piped files.
 *
 * When reading the configuration (and piped stuff in the configuration) I/O
 * is blocking... nothing else can run while this is going on.  Otherwise,
 * all I/O is non-blocking.  The actual I/O is non-blocking, the "blocking"
 * I/O is manufactured using a mini-pselect, so can time-out file writing
 * before too long.
 */
static cmd_ret_t uty_file_read_result(vio_vf vf, int get) ;
static cmd_ret_t uty_file_read_block(vio_vf vf) ;
static cmd_ret_t uty_file_write(vio_vf vf) ;
static void uty_file_write_ready(vio_vfd vfd, void* action_info, bool timeout);

static cmd_ret_t uty_pipe_close(vio_vf vf) ;
static cmd_ret_t uty_pipe_return_suck(vio_vf vf) ;
static cmd_ret_t uty_pipe_return_set_read_ready(vio_vf vf) ;

/*------------------------------------------------------------------------------
 * Buffer used when throwing away either stdout or stderr of the child, or
 * throwing away pipe input.
 *
 * Since we are throwing stuff away, doesn't matter that there is only one of
 * these !
 */
static char dev_null[4096] ;

/*------------------------------------------------------------------------------
 * Open file for input, to be read as commands -- VIN_FILE.
 *
 * If could not open, issues message to the vio.
 *
 * If opens OK, save the current context in the current vin (before pushing
 * the new vin).
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_ERROR    -- failed to open
 *
 * If "blocking" vf, this can be called from any thread, otherwise must be the
 * cli thread -- see uty_vfd_new().
 */
extern cmd_ret_t
uty_file_read_open(vty_io vio, qpath path)
{
  cmd_ret_t   ret ;
  const char* pns ;
  int     fd ;
  vio_vf  vf ;
  vfd_io_type_t iot ;

  VTY_ASSERT_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  /* Do the basic file open.
   *
   * Note that we here open for non-blocking I/O.  In uty_vf_new() will set
   * vfd_io_ps_blocking if required.
   */
  iot = vfd_io_read ;
  pns = qpath_string(path) ;

  fd = uty_fd_file_open(pns, iot, (mode_t)0) ;  /* cmode not required   */

  /* If failed, report.
   * If OK save context & update "here" then create and push the vin.
   */
  if (fd < 0)
    {
      uty_out(vio, "%% Could not open input file %s: %s\n", pns,
                                                         errtoa(errno, 0).str) ;
      ret = CMD_ERROR ;
    }
  else
    {
      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vin_push(vio, vf, VIN_FILE, uty_file_read_ready, file_buffer_size) ;
      vf->read_timeout = file_timeout ;
      uty_vin_set_here(vio, path) ;

      ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open file for output -- VOUT_FILE.
 *
 * If could not open, issues message to the vio.
 *
 * "after" <=> this vout is being opened after a vin being opened at the same
 *             time.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_ERROR    -- failed to open
 *
 * If "blocking", this can be called from any thread, otherwise must be the
 * cli thread -- see uty_vfd_new().
 */
extern cmd_ret_t
uty_file_write_open(vty_io vio, qpath path, bool append, bool after)
{
  cmd_ret_t   ret ;
  const char* pns ;
  int   fd ;
  vio_vf vf ;
  vfd_io_type_t iot ;

  VTY_ASSERT_LOCKED() ;

  /* Do the basic file open.
   *
   * Note that we here open for non-blocking I/O.  In uty_vf_new() will set
   * vfd_io_ps_blocking if required.
   */
  iot = vfd_io_write | (append ? vfd_io_append : 0) ;
  pns = qpath_string(path) ;

  fd = uty_fd_file_open(pns, iot, PIPEFILE_MODE) ;

  /* If failed, report.
   * If OK, create and push the vout.
   */
  if (fd < 0)
    {
      uty_out(vio, "%% Could not open output file %s: %s\n", pns,
                                                         errtoa(errno, 0).str) ;
      ret = CMD_ERROR ;
    }
  else
    {
      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vout_push(vio, vf, VOUT_FILE, uty_file_write_ready,
                                                      file_buffer_size, after) ;
      vf->write_timeout = file_timeout ;

      ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command line fetch from a file -- VIN_FILE, VIN_CONFIG or VIN_PIPE.
 *
 * If gets line, sets it in the vf->context.
 *
 * Returns:  CMD_SUCCESS   -- have another command line ready to go
 *           CMD_WAITING   -- waiting for input   => non-blocking
 *           CMD_HIATUS    -- probably eof and/or vf_cease, possibly vf_cancel
 *           CMD_IO_ERROR  -- ran into an I/O error or time-out
 *
 * In "non-blocking" state, on CMD_WAITING sets read ready, which will
 * vty_cmd_signal() the command loop to come round again.
 *
 * In "blocking" state, will not return until have something, or there is
 * nothing to be had, or times out.  For VIN_PIPE, if gets nothing on the
 * input, and is not eof, will suck on the pipe stderr return, to make sure
 * that the other end is not stalled trying to write to stderr (!)
 *
 * This can be called in any thread.
 *
 * NB: if is not vin_active, then will return CMD_HIATUS -- ie eof.
 */
extern cmd_ret_t
uty_file_cmd_line_fetch(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert( (vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)
                                      || (vf->vin_type == VIN_PIPE) ) ;

  while (1)
    {
      cmd_ret_t ret ;
      int       get ;

      vf->vin_waiting = false ;         /* until proved otherwise       */

      /* Try to complete line straight from the buffer.  If gets line, sets
       * that in the vf->context.
       *
       * If is vf_cease or vf_cancel, uty_fifo_cmd_line_fetch() returns
       * CMD_HIATUS.
       *
       * If is vf_end, uty_fifo_cmd_line_fetch() returns CMD_SUCCESS or
       * CMD_HIATUS.
       *
       * If is vin_eof, uty_fifo_cmd_line_fetch() will clear vin_active, and
       * return CMD_HIATUS if the current line and the buffer are empty.
       */
      ret = uty_fifo_cmd_line_fetch(vf, true /* with '\' */) ;

      if (ret != CMD_WAITING)
        return ret ;

      qassert(vf->vin_state == vf_open) ;
      qassert(vio_fifo_is_empty(vf->ibuf)) ;

      /* Try to get more from the file/pipe
       */
      get = vio_fifo_read_nb(vf->ibuf, uty_vf_read_fd(vf), 0, 1) ;
                                /* read a lump at a time        */
      if (get > 0)
        continue ;              /* got something                */

      ret = uty_file_read_result(vf, get) ;

      if (ret == CMD_HIATUS)
        continue ;              /* eof                          */

      if (ret != CMD_SUCCESS)
        return ret ;            /* CMD_WAITING or CMD_IO_ERROR  */

      qassert(vf->blocking) ;

      ret = uty_file_read_block(vf) ;

      if (ret != CMD_SUCCESS)
        return ret ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Deal with result of read from vin -- assuming is not "have something".
 *
 * For non-blocking, sets read-ready and vin_waiting iff required.
 *
 * Returns:  CMD_SUCCESS   -- waiting for blocking I/O
 *           CMD_WAITING   -- waiting for non-blocking I/O -- is read-ready
 *           CMD_HIATUS    -- result is eof
 *           CMD_IO_ERROR  -- error or timeout
 *
 * NB: CMD_WAITING => vf->vin_waiting.
 */
static cmd_ret_t
uty_file_read_result(vio_vf vf, int get)
{
  cmd_ret_t  ret ;

  if (get < 0)
    {
      if (get != -2)
        return uty_vf_error(vf, verr_io_vin, errno) ;

      /* Hit end of file for the first time, set vf_end.
       */
      uty_vf_read_stop(vf, vfs_stop_end) ;

      return CMD_HIATUS ;
    } ;

  qassert(get == 0) ;

  /* We are waiting... for non-blocking, set read-ready and vin_waiting.
   */
  if (vf->blocking)
    return CMD_SUCCESS ;

  ret = uty_vf_set_read_ready(vf, on) ;

  vf->vin_waiting = (ret == CMD_WAITING) ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Block waiting for vin and/or pipe stderr return.
 *
 * Returns:  CMD_SUCCESS    -- got something (may be stderr return)
 *           CMD_HIATUS     -- nothing is vf_end.
 *           CMD_IO_ERROR   -- error or timeout
 */
static cmd_ret_t
uty_file_read_block(vio_vf vf)
{
  qps_mini_t qm ;

  qm->timeout_set = false ;

  qassert( (vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)
                                      || (vf->vin_type == VIN_PIPE) ) ;
  qassert(vf->blocking) ;

  while (1)
    {
      vio_timer_time timeout ;
      cmd_ret_t  ret ;
      int        get ;

      /* Before blocking, suck on any pipe stderr return, to make sure empty.
       */
      if (vf->vin_type == VIN_PIPE)
        {
          ret = uty_pipe_return_suck(vf) ;

          qassert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)) ;

          if (ret != CMD_SUCCESS)
            return ret ;                /* cannot continue          */
        } ;

      /* Implement blocking I/O, with timeout
       *
       * For VIN_PIPE, we "block" on both the main vin and the stderr return.
       *
       * It may be that by the time we get here there is nothing more to wait
       * for -- eg: the uty_pipe_stderr_suck() above may have drained the
       * pipe stderr return.
       */
      qassert(vio_vfd_blocking(vf->vfd)) ;

      timeout = 0 ;

      if (vf_active(vf->vin_state))
        {
          qps_mini_set(qm, uty_vf_read_fd(vf), qps_read_mnum) ;
          timeout = vf->read_timeout ;

          qassert(timeout != 0) ;
        } ;

      if (vf_active(vf->ps_state))
        {
          qassert(vf->vin_type == VIN_PIPE) ;

          qps_mini_add(qm, vio_vfd_fd(vf->ps_vfd), qps_read_mnum) ;
          if (timeout == 0)
            {
              timeout = pipe_return_timeout ;
              qassert(timeout != 0) ;
            } ;
        } ;

      if (timeout == 0)
        return CMD_HIATUS ;             /* nothing to get       */

      get = qps_mini_wait(qm, timeout, NULL) ;

      if (get > 0)
        return CMD_SUCCESS ;            /* got something        */

      if (get == -2)
        continue ;                      /* EINTR                */

      if (get == 0)
        return uty_vf_error(vf, verr_to_vin, 0) ;
      else
        return uty_vf_error(vf, verr_io_vin, errno) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Ready to read or timed-out: call-back for VIN_FILE, VIN_CONFIG or VIN_PIPE.
 *
 * This is used if the vin is non-blocking.  VIN_CONFIG is usually blocking.
 *
 * If timeout, if is vf_end signal the timeout error.
 *
 * Otherwise, signal CMD_SUCCESS.  Actual input is done in the command loop.
 * It is possible that the signal is no longer required, but we err on the
 * safe side.
 *
 * A vin is set read-ready when has tried to read and nothing is available.
 * The read-ready is not then cleared until either it goes off, or the vin is
 * closed.  So, if reading ceases, or is cancelled, then a timeout could go
 * off, before the close.  We avoid spurious timeout by checking for vf_end
 * and !vf_cancel.
 *
 * In any case, is no longer vin_waiting.
 *
 * Note that the read_ready state and any time out are automatically disabled
 * when they go off -- so is now not read-ready.
 */
static void
uty_file_read_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  cmd_ret_t ret ;
  vio_vf    vf ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  qassert((vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)
                                     || (vf->vin_type == VIN_PIPE)) ;
  qassert(!vf->blocking) ;

  vf->vin_waiting = false ;             /* read-ready cleared   */

  if (time_out && ((vf->vin_state & (vf_cancel | vf_end)) == vf_end))
    ret = uty_vf_error(vf, verr_to_vin, 0) ;
  else
    ret = CMD_SUCCESS ;

  uty_cmd_signal(vf->vio, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to a file/pipe: VOUT_FILE, VOUT_CONFIG, VOUT_PIPE or
 *                                                                 VOUT_SH_CMD
 * Does not block.
 *
 * It is unlikely that a file/pipe would block.  If it would, then for
 * non-blocking we set write-ready and let the pselect() process progress
 * things (unless vst_final).   For blocking, we simply leave what has not
 * been output for the next "push", or until the vf is closed.
 *
 * If is vf_cease, then stops the main vout (clearing vout_active) as soon
 * as the buffer empties.  The pipe return(s) are set !xx_open as soon as EOF
 * is received.
 *
 * Will return CMD_WAITING for blocking as well as non-blocking.
 *
 * Returns:  CMD_SUCCESS  -- was vf_active and obuf is (now) empty -- will have
 *                                                   set vf_end iff is vf_cease
 *                           or: was and still is *not* vf_active
 *           CMD_WAITING  -- waiting to be able to *output*
 *           CMD_IO_ERROR -- error or time-out
 *
 * This can be called in any thread.
 */
extern cmd_ret_t
uty_file_out_push(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert( (vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)
                                        || (vf->vout_type == VOUT_PIPE)
                                        || (vf->vout_type == VOUT_SH_CMD) ) ;

  if ((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG))
    qassert((vf->pr_state == vf_closed) && (vf->ps_state == vf_closed)) ;

  if (vf->vout_type == VOUT_SH_CMD)
    qassert(vf->vout_state & vf_end) ;

  /* For blocking, need to push the pipe return(s), to keep things moving.
   *
   * For non-blocking, the pipe returns run autonomously.  They are set
   * read-ready when opened, and continue until closed or cancelled.  Note
   * that for pipe return vf_cancel => vf_cease => vf_end.  (So do not
   * have to worry about restarting input if vf_cancel had been set and then
   * cleared again !)
   *
   * Note that for pipe return(s) we do not do vf_end with vf_cease.
   */
  if (vf->blocking &&
      ( (vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD) ))
    {
      ret = uty_pipe_return_suck(vf) ;

      qassert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)) ;

      if (ret != CMD_SUCCESS)
        return ret ;                    /* cannot continue      */
    } ;

  /* Write as much as possible
   *
   * For non-blocking, may set write-ready and return CMD_WAITING.
   *
   * For blocking, will do as much as can, and may return CMD_WAITING if would
   * block on *output*.
   */
  return uty_file_write(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Write to file/pipe -- VOUT_FILE, VOUT_PIPE, VOUT_CONFIG or VOUT_SH_CMD.
 *
 * For VOUT_FILE and VOUT_CONFIG, until is vf_cease, will not write the
 * end_lump of the fifo, so output is in units of the fifo size -- which should
 * be "chunky".
 *
 * If is not active, does nothing and returns CMD_SUCCESS.
 *
 * If empties out the buffer, and is vf_cease, sets vf_end.
 *
 * Does not block.
 *
 * Will return CMD_WAITING for blocking as well as non-blocking.
 *
 * Returns:  CMD_SUCCESS  -- was vf_active and obuf is (now) empty -- will have
 *                                                   set vf_end iff is vf_cease
 *                           or: was and still is *not* vf_active
 *           CMD_WAITING  -- waiting to be able to *output*
 *           CMD_IO_ERROR -- error or time-out
 *
 * This can be called in any thread.
 */
static cmd_ret_t
uty_file_write(vio_vf vf)
{
  int   put ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert( (vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)
                                        || (vf->vout_type == VOUT_PIPE)
                                        || (vf->vout_type == VOUT_SH_CMD) ) ;

  if (!vf_active(vf->vout_state))
    return CMD_SUCCESS ;

  put = vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd),
                ((vf->vout_state & vf_cease) || (vf->vout_type == VOUT_PIPE))) ;

  if (put > 0)
    return (vf->blocking) ? CMD_WAITING : uty_vf_set_write_ready(vf, on) ;

  if (put < 0)
    return uty_vf_error(vf, verr_io_vout, errno) ;

  if (vf->vout_state & vf_cease)
    vf->vout_state |= vf_end ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * File is ready to write or time-out: call-back for VOUT_FILE or VOUT_PIPE.
 *
 * This is used if is non-blocking.
 *
 * If the vout is vf_end, do uty_file_write() and deal with timeout.
 *
 * Unless is CMD_WAITING, whatever happens we signal the command loop.  The
 * signal may not be required, but we err on the safe side.
 *
 * A vout is set write-ready a write() cannot write everything.  The write-
 * ready is not then cleared until either it goes off, or the vout is closed.
 * So, if writing ceases, or is cancelled, then a timeout could go off, before
 * the vout is closed, or otherwise when it is not required.  What we do is
 * call uty_file_write(), and if that returns CMD_WAITING it means that still
 * want to output something, and still cannot output everything, so *then* we
 * know we have a genuine timeout.
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
uty_file_write_ready(vio_vfd vfd, void* action_info, bool timeout)
{
  cmd_ret_t ret ;
  vio_vf    vf ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  qassert((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_PIPE)) ;
  qassert(!vf->blocking) ;

  ret = uty_file_write(vf) ;

  if (timeout && (ret == CMD_WAITING))
     ret = uty_vf_error(vf, verr_to_vout, 0) ;

  /* Signal CMD_SUCCESS or CMD_IO_ERROR -- CMD_WAITING will be ignored.
   *
   * CMD_IO_ERROR should already have been signalled, but does no harm to
   * signal it again.
   */
  uty_cmd_signal(vf->vio, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Block waiting to write to vout and/or read from pipe return(s).
 *
 * For blocking vf *only*.
 *
 * Returns:  CMD_SUCCESS    -- can write or got something to read or something
 *           CMD_HIATUS     -- nothing to block for
 *           CMD_IO_ERROR   -- error or timeout
 */
extern cmd_ret_t
uty_file_write_block(vio_vf vf)
{
  qps_mini_t qm ;
  vio_err_type_t err_type ;
  vio_timer_time timeout ;

  qassert( (vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)
                                        || (vf->vout_type == VOUT_PIPE)
                                        || (vf->vout_type == VOUT_SH_CMD) ) ;
  qassert(vf->blocking) ;

  if (!vf->blocking)
    return CMD_HIATUS ;

  /* We are blocking, so now is the time to run the pseudo-blocking,
   * for all remaining parts.
   *
   * We run the timeout on the vout, the pr and the ps, in that order.
   */
  qps_mini_set(qm, -1, 0) ;

  timeout = 0 ;

  if (vf_active(vf->ps_state))
    {
      qassert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD)) ;

      timeout  = pipe_return_timeout ;
      err_type = verr_ps ;
      qps_mini_add(qm, vio_vfd_fd(vf->ps_vfd), qps_read_mnum) ;

      confirm(pipe_return_timeout != 0) ;
    } ;

  if (vf_active(vf->pr_state))
    {
      qassert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD)) ;

      timeout   = pipe_return_timeout ;
      err_type = verr_pr ;
      qps_mini_add(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum) ;

      confirm(pipe_return_timeout != 0) ;
    } ;

  if (vf_active(vf->vout_state))
    {
      timeout  = (vf->write_timeout == 0) ? pipe_timeout : vf->write_timeout ;
      err_type = verr_vout ;
      qps_mini_add(qm, uty_vf_write_fd(vf), qps_write_mnum) ;

      confirm(pipe_timeout != 0) ;
    } ;

  if (timeout == 0)
    return CMD_HIATUS ;

  qm->timeout_set = false ;

  while (1)
    {
      int get ;

      get = qps_mini_wait(qm, timeout, NULL) ;

      if (get > 0)
        break ;

      if (get == 0)
        return uty_vf_error(vf, verr_to | err_type, 0) ;

      if (get == -1)
        return uty_vf_error(vf, verr_io | err_type, errno) ;

      qassert(get == -2) ;

      continue ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Input file/pipe is being closed: VIN_FILE, VIN_CONFIG or VIN_PIPE.
 *
 * For VIN_FILE and VIN_CONFIG, we are done.
 *
 * For VIN_PIPE:
 *
 *   * if is ps_enabled and we need to drain the main vin, and suck the pipe
 *     pipe stderr return dry, and then collect the child and deal with any
 *     pipe stderr return and child return code stuff.
 *
 *   * if is not ps_enabled, can close the input and pipe stderr return, and
 *     will make a perfunctory effort to collect the child, but will discard
 *     any pipe stderr return stuff and the child return code.
 *
 * If we are vst_cancel or vst_final, will clear ps_enabled.  So, for
 * vst_final, this will not block and will close, promptly.
 *
 * Will block as required.
 *
 * Returns:  CMD_SUCCESS  -- done everything possible
 *           CMD_WAITING  -- waiting for output to complete => non-blocking
 *                                                          => not final
 *           CMD_IO_ERROR -- error encountered when writing (may be final)
 *
 * For blocking vf this can be called in any thread.  For non-blocking, must be
 * the CLI thread.
 */
extern cmd_ret_t
uty_file_read_close(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  qassert( (vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)
                                      || (vf->vin_type == VIN_PIPE) ) ;
  qassert(vf->vin_state & vf_cease) ;

  vf->vin_waiting = false ;         /* definitely not !         */

  /* Closing files is easy.
   */
  if (vf->vin_type != VIN_PIPE)
    {
      qassert(vf->ps_state == vf_closed) ;
      return CMD_SUCCESS ;      /* Easy !  Caller closes vfd    */
    } ;

  /* For VIN_PIPE deal with draining the input and sucking up any pipe stderr
   * return and collection of child.
   */
  while (vf_active(vf->vin_state))
    {
      /* Drain the vin
       *
       * Read into scratch buffer and deal errors, eof, blocking etc.
       *
       * For non-blocking, the pipe stderr return proceeds autonomously.
       */
      int get ;

      do
        get = read_nb(uty_vf_read_fd(vf), dev_null, sizeof(dev_null)) ;
      while (get > 0) ;

      ret = uty_file_read_result(vf, get) ;

      if (ret == CMD_HIATUS)
        continue ;                      /* eof                          */

      if (ret != CMD_SUCCESS)
        return ret ;                    /* CMD_WAITING or CMD_IO_ERROR  */

      qassert(vf->blocking) ;

      ret = uty_file_read_block(vf) ;   /* sucks pipe stderr return     */

      if (ret == CMD_IO_ERROR)
        return ret ;
    } ;

  qassert(vf->ps_state != vf_closed) ;

  while (vf_active(vf->ps_state))
    {
      /* Drain the pipe stderr return.
       *
       * For non-blocking, the pipe stderr return proceeds autonomously, but
       * if no timeout is set, now is the time to set it.
       *
       * For blocking, keep reading away.
       */
      if (!vf->blocking)
        {
          if (!vf->ps_timeout)
            return uty_pipe_return_set_read_ready(vf) ;
          return CMD_WAITING ;
        } ;

      ret = uty_file_read_block(vf) ;   /* sucks pipe stderr return     */

      if (ret == CMD_IO_ERROR)
        return ret ;
    } ;

  /* All I/O is complete, now is the time to close pipe returns, collect the
   * child process and post the pipe stderr return stuff.
   */
  return uty_pipe_close(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Flush output buffer ready for close: VOUT_FILE, VOUT_CONFIG, VOUT_PIPE or
 *                                                                  VOUT_SH_CMD.
 *
 * For blocking we need to make sure that all output is complete, and any
 * pipe return(s) are empty and closed.  Then for VOUT_PIPE and VOUT_SH_CMD,
 * need to collect the child and deal with an pipe stderr return stuff.
 *
 * Will block as required.
 *
 * Returns:  CMD_SUCCESS  -- done everything possible
 *           CMD_WAITING  -- waiting for output to complete => non-blocking
 *           CMD_IO_ERROR -- error encountered when writing (may be final)
 *
 * For blocking vf this can be called in any thread.  For non-blocking, must be
 * the CLI thread.
 *
 * NB: for blocking vf will not return until all output has completed, and
 *     for VOUT_PIPE and VOUT_SH_CMD, all pipe return(s) have been read to
 *     eof.
 */
extern cmd_ret_t
uty_file_write_close(vio_vf vf)
{
  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  qassert( (vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)
                                        || (vf->vout_type == VOUT_PIPE)
                                        || (vf->vout_type == VOUT_SH_CMD) ) ;
  qassert(vf->vout_state & vf_cease) ;

  /* Loop to process any outstanding/remaining I/O.
   *
   * Loops if is blocking.
   *
   * If is non-blocking, leaves pselect() to do the work, and returns
   * CMD_WAITING.
   *
   * If all I/O is complete, breaks out of the loop.
   */
  while (1)
    {
      cmd_ret_t  ret ;

      /* We push to do as much output as possible.
       *
       * uty_file_out_push() returns CMD_SUCCESS when obuf is empty, or the
       * vout_state is not active -- either way, we are done with the vout, and
       * for VOUT_PIPE this is a good moment to close it, to signal to the
       * child process that we are done.
       */
      ret = uty_file_out_push(vf) ;

      if (ret == CMD_SUCCESS)
        {
          if ((vf->vout_type == VOUT_PIPE) && (vf->vfd != NULL))
            vf->vfd = vio_vfd_close(vf->vfd) ;

          uty_vf_write_stop(vf, vfs_stop_cease) ;
          uty_vf_write_stop(vf, vfs_stop_end) ;
        } ;

      /* If non-blocking then if we have not finished with the vout, leave
       * the pselect() process to deal with that.  Or if hit an error, return
       * that.
       *
       * Otherwise, we now want any pipe return stuff to finish.
       *
       * If the pipe return is still active, make sure it has its timeout
       * set -- and return CMD_WAITING.
       *
       * If the pipe stderr return is still active, similarly.
       *
       * Otherwise, we are done !
       */
      if (!vf->blocking)
        {
          if (ret != CMD_SUCCESS)
            return ret ;                /* waiting for vout, or failed  */

          if (vf_active(vf->pr_state))
            {
              if (!vf->pr_timeout)
                return uty_pipe_return_set_read_ready(vf) ;
              return CMD_WAITING ;
            } ;

          if (vf_active(vf->ps_state))
            {
              if (!vf->ps_timeout)
                return uty_pipe_return_set_read_ready(vf) ;
              return CMD_WAITING ;
            } ;

          break ;
        } ;

      /* Blocking: if completed vout output, then we want any pipe return stuff
       *           to complete.  If we are waiting to complete vout output,
       *           then we need to block on that and any pipe return stuff.
       */
      if ((ret == CMD_SUCCESS) || (ret == CMD_WAITING))
        ret = uty_file_write_block(vf) ;

      if (ret == CMD_SUCCESS)
        continue ;

      if (ret == CMD_HIATUS)
        break ;

      return ret ;
    } ;

  /* All I/O is complete.
   *
   * For pipe types now is the time to close pipe returns, collect the child
   * process and post the pipe stderr return stuff.
   *
   * Otherwise, we are done.
   */
  if ((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD))
    return uty_pipe_close(vf) ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * VTY Pipe I/O -- VIN_PIPE, VOUT_PIPE & VOUT_SH_CMD
 *
 * This is for input/output from/to shell commands.
 *
 * This is complicated by (a) the existence of up to 3 streams of data, and
 * (b) the extra step of collecting the child and its return code on close.
 *
 * The three streams of data, from the perspective of the shell command, are:
 *
 *    stdin  -- input from Quagga (if any)
 *
 *    stdout -- output to Quagga -- may be treated as command lines
 *
 *    stderr -- output to Quagga -- assumed to be diagnostic information
 *
 * There are three variants of pipe I/O:
 *
 *   * VIN_PIPE     -- command line:   <| shell command
 *
 *       stdin   -- none
 *
 *       stdout  -- read by Quagga as command lines
 *
 *       stderr  -- collected by Quagga and output (eventually) to the base
 *                  vout.
 *
 *   * VOUT_PIPE    -- command line:  .... >| shell command
 *
 *       stdin   -- the Quagga command(s) output
 *
 *       stdout  -- collected by Quagga and output to the next vout.
 *
 *                  If the next vout is !out_ordinary, then this will be
 *                  discarded !
 *
 *       stderr  -- as VIN_PIPE.
 *
 *   *  VOUT_SH_CMD -- command line: | shell_command
 *
 *       stdin   -- none
 *
 *       stdout  -- as VOUT_PIPE
 *
 *       stderr  -- as VOUT_PIPE & VIN_PIPE
 *
 * When Quagga is collecting stdout for output, that is known as the
 * "pipe return", and is expected to be the result of the command.
 *
 * In all cases the stderr is assumed to be diagnostic information, which is
 * known as the "pipe stderr return".  This is collected in the vf->ps_buf.
 * When the pipe is closed, any stderr return is then appended to the
 * vio->ps_buf.  When the output stack is closed down to the base vout, the
 * contents of the vio->ps_buf are transferred to the vio->obuf.  This means
 * that:
 *
 *   1. all pipe stderr return for a given pipe is collected and output
 *      all together to the base vout (eg the VTY_TERMINAL).  The output is
 *      bracketted so:
 *
 *        %[--- 'name':
 *        ....
 *        ....
 *        %---]
 *
 *      to give context.
 *
 *   2. the pipe stderr return is not mixed up with any other pipe or other
 *      command output, and in particular cannot be piped to another pipe.
 *
 *   3. but... if VOUT_PIPE are stacked on top of each other, it may be a
 *      while before the pipe stderr return stuff reaches the base vout.
 *
 *      The problem here is that if one VOUT_PIPE is stacked above another,
 *      the lower pipe may or may not be in the middle of something when
 *      the upper pipe is closed -- so to avoid a mix-up, must wait until
 *      the all lower pipes close before the pipe stderr is finally output
 *      (along with all other pending stderr return).
 *
 * While the main vin/vout is open, the pipe I/O is handled as follows:
 *
 *   * VIN_PIPE
 *
 *       The shell command's stdout is connected to the vf's vin, and is read
 *       as command lines, much as VIN_FILE.  A read timeout is set, to deal
 *       with shell commands which take an unreasonable time.
 *
 *       The shell command's stderr is connected to the vf's pipe stderr
 *       return.  For non-blocking, that is read into the vf->ps_buf, by the
 *       pselect() process.  For blocking, that is read in parallel with the
 *       main vin.  No timeout is set until the main vin is closed.
 *
 *   * VOUT_PIPE
 *
 *       The shell command's stdin is connected to the vf's vout, and is
 *       written to much like VOUT_FILE.  A write timeout is set, to deal
 *       with shell commands which take an unreasonable time.
 *
 *       The shell command's stdout is connected to the vf's pipe return.
 *       For non-blocking, the pipe return is read autonomously under
 *       pselect() and pushed to the next vout.  For blocking, the pipe
 *       return is polled whenever the (main) vout is written to, and any
 *       available input is pushed to the slave vout.  No timeout is set
 *       until the main vout is closed.
 *
 *       The shell command's stderr is connected to the vf's pipe stderr
 *       return.  For non-blocking, that is read into the vf->ps_buf, by the
 *       pselect() process.  For blocking, that is read in parallel with the
 *       pipe return.  No timeout is set until the main vout is closed.
 *
 *   * VOUT_SH_CMD
 *
 *       The shell command's stdin is set empty, and the vf's vout set to
 *       vf_closed.
 *
 *       Otherwise, this is the same as VOUT_PIPE.
 *
 * The closing of a VIN_PIPE/VOUT_PIPE/VOUT_SH_CMD is a little involved:
 *
 *   0. close the main vin/vout.
 *
 *      for a VIN_PIPE the vin_state will be:
 *
 *        vf_end   -- eof met or forced by an early close, or error or timeout
 *                    occurred
 *
 *        So phase 0 finishes immediately.
 *
 *      for a VOUT_PIPE the vout_state will be:
 *
 *        vf_open  -- until all output completes or hits error or timeout
 *        vf_end   -- nothing more to be output, or error or timeout occurred
 *
 *        If vf_open, must wait until no longer vf_open, ie until all pending
 *        output completes, or hits and error or timeout.
 *
 *        As soon as is no longer vf_open, must close the vfd to signal to the
 *        child that is now at eof.
 *
 *      for a VOUT_SH_CMD the vout_state will be:
 *
 *        vf_end   -- nothing to be output.
 *
 *        So phase 0 finishes immediately.
 *
 *   1. collect remaining input and close the pipe return.
 *
 *      for a VIN_PIPE there is no pipe return.
 *
 *      for a VOUT_PIPE or VOUT_SH_CMD, must empty the pipe return and then
 *      close it -- stopping immediately if gets error or time-out on either
 *      the pipe return or the slave vout.
 *
 *      Note that up to this point no time-out has been set on the pipe return,
 *      since there is no need for there to be any input.  But expect now to
 *      see at least eof in a reasonable time.
 *
 *      For non-blocking, the pr_vfd is set read ready.  Then for all
 *      non-blocking pipes, the remaining pipe return input proceeds in the
 *      pselect() process.
 *
 *      For blocking pipes, the remaining pipe return must complete (or
 *      time-out) during the close operation.
 *
 *   2. collect remaining input and close the pipe stderr return.
 *
 *      For non-blocking, the ps_vfd is set read ready.  Then for all
 *      non-blocking pipes, the remaining pipe return input proceeds in the
 *      pselect() process -- complete with time-out.
 *
 *      For blocking pipes, the remaining pipe return must complete (or
 *      time-out) during the close operation.
 *
 *   3. once all input has been collected and pr_vfd and ps_vfd are closed,
 *      need to collect the child, so that we can check the return code.
 *
 *      This may time out.
 *
 *      If the return code is not 0, or anything else happens, a diagnostic
 *      message is appended to the vf->rbuf.
 *
 * For non-blocking the close may return CMD_WAITING, and must be called
 * again later to progress the close.
 *
 * For "final" close will not block or return CMD_WAITING, but complete the
 * process as quickly as possible while outputting as much as possible.
 */
typedef int  fd_pair_t[2] ;     /* result of pipe()             */
typedef int* fd_pair ;

typedef enum fd_pair_half       /* naming of parts              */
{
  in_half  = 0,
  out_half = 1,

} fd_pair_half_t ;

CONFIRM(STDIN_FILENO  == 0) ;   /* make assumption explicit     */
CONFIRM(STDOUT_FILENO == 1) ;
CONFIRM(STDERR_FILENO == 2) ;

typedef enum std_id             /* more naming of parts         */
{
  stdin_fd  = STDIN_FILENO,
  stdout_fd = STDOUT_FILENO,
  stderr_fd = STDERR_FILENO,

  stds      = 3

} std_id_t ;

typedef fd_pair_t std_set[stds] ;

typedef enum pipe_type          /* vty level pipe types         */
{
  vin_pipe,
  vout_pipe,
  vout_sh_cmd,

} pipe_type_t ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static pid_t uty_pipe_fork(vty_io vio, const char* cmd_str, std_set set,
                                                            pipe_type_t type) ;
static bool uty_fd_pair(vty_io vio, const char* cmd_str,
                                    const char* what, std_set  set,
                                                      std_id_t id,
                                                      fd_pair_half_t half) ;
static bool uty_pipe_fork_fail(vty_io vio, const char* cmd_str,
                                           const char* what,
                                           std_set     set,
                                           fd_pair     pair,
                                           const char* action) ;
static void uty_pipe_close_half(fd_pair pair, fd_pair_half_t half) ;
static void uty_pipe_open_complete(vio_vf vf, pid_t pid, int pr_fd, int ps_fd) ;
static bool uty_pipe_exec_prepare(vty_io vio, std_set set) ;

static vio_fifo uty_pipe_ps_buf(vio_vf vf) ;

static void uty_pipe_return_ready(vio_vfd vfd, void* action_info,
                                                                bool time_out) ;

/*------------------------------------------------------------------------------
 * Open VIN_PIPE: pipe whose child's stdout is read and executed as commands.
 *
 * The child's stderr is read, separately, as the pipe stderr return, and
 * (eventually) sent to the base vout.
 *
 * The new vin has two vio_fd's, the standard one to read commands, and the
 * other (the ps_vfd) to collect any stderr output (the "stderr return").
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_ERROR    -- failed to open -- message sent to vio.
 */
extern cmd_ret_t
uty_pipe_read_open(vty_io vio, qstring command)
{
  std_set     set ;
  const char* cmd_str ;
  vio_vf      vf ;
  pid_t       child ;

  VTY_ASSERT_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  cmd_str = qs_string(command) ;

  /* Fork it
   */
  child = uty_pipe_fork(vio, cmd_str, set, vin_pipe) ;

  if (child < 0)
    return CMD_ERROR ;

  /* OK -- now push the new input onto the vin_stack.
   *
   * Note that all the naming is from the child's perspective.
   *
   * uty_vf_new() sets vf->blocking if the parent vio is blocking, and sets the
   * vfd_io_ps_blocking.  The subsequent pipe stderr return will be set to
   * suit.
   */
  vf = uty_vf_new(vio, cmd_str, set[stdout_fd][in_half], vfd_pipe, vfd_io_read);
  uty_vin_push(vio, vf, VIN_PIPE, uty_file_read_ready, pipe_buffer_size) ;
  vf->read_timeout = pipe_timeout ;

  /* If command starts with '/', then we will want to set the 'here' directory
   * to its directory.
   */
  if (*cmd_str == '/')
    {
      qpath       dir ;
      const char* p ;

      p = cmd_str ;
      while (*p > ' ')
        ++p ;
      dir = qpath_set_n(NULL, cmd_str, p - cmd_str) ;

      uty_vin_set_here(vio, dir) ;
      qpath_free(dir) ;
    } ;

  /* Record the child pid, set nothing for the pipe return but set the child's
   * stderr as the stderr return.
   */
  uty_pipe_open_complete(vf, child, -1, set[stderr_fd][in_half]) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Open VOUT_PIPE or VOUT_SH_CMD: pipe which is going to be written to
 * (or not, if shell_cmd), where what the pipe returns will be output to the
 * current vout.
 *
 * For VOUT_PIPE, output is sent to the child's stdin.  For VOUT_SH_CMD,
 * nothing is set up for the child's stdin.
 *
 * In both cases the child's stdout is collected as the pipe return, and its
 * stderr is collected as the stderr return.
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_ERROR    -- failed to open -- message sent to vio.
 */
extern cmd_ret_t
uty_pipe_write_open(vty_io vio, qstring command, bool shell_cmd, bool after)
{
  std_set     set ;
  const char* cmd_str ;
  pid_t    child ;
  vio_vf   vf ;

  VTY_ASSERT_LOCKED() ;

  cmd_str = qs_string(command) ;

  /* Do the basic file open.
   */
  child = uty_pipe_fork(vio, cmd_str, set, shell_cmd ? vout_sh_cmd
                                                     : vout_pipe) ;
  if (child < 0)
    return CMD_ERROR ;

  /* OK -- now push the new output onto the vout_stack.
   *
   * Note that for VOUT_SH_CMD no vfd is set up, and the vout_state is
   * immediately set to vf_end.
   *
   * uty_vf_new() sets vf->blocking if the parent vio is blocking, and sets
   * vfd_io_ps_blocking.  The subsequent pipe returns will be set to suit.
   */
  if (shell_cmd)
    {
      vf = uty_vf_new(vio, cmd_str, -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_SH_CMD, NULL, 0, after) ;

      uty_vf_write_stop(vf, vfs_stop_cease) ;   /* immediately  */
      uty_vf_write_stop(vf, vfs_stop_end) ;     /* instantly    */
    }
  else
    {
      vf = uty_vf_new(vio, cmd_str, set[stdin_fd][out_half],
                                                       vfd_pipe, vfd_io_write) ;
      uty_vout_push(vio, vf, VOUT_PIPE, uty_file_write_ready,
                                                      pipe_buffer_size, after) ;
      vf->write_timeout  = pipe_timeout ;
    } ;

  /* Record the child pid, set the child's stdout as the pipe return and set
   * its stderr as the stderr return.
   */
  uty_pipe_open_complete(vf, child, set[stdout_fd][in_half],
                                    set[stderr_fd][in_half]) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Complete the opening of a pipe.
 *
 * Register the child.
 *
 * If there is a pipe return (vout_pipe and vout_sh_cmd), set up the pipe
 * return fd etc.
 *
 * All pipes have a stderr pipe return, so set that fd etc.
 *
 * Pipe return timeouts are set to zero here -- no timeout is required until
 * the vf is closed.
 *
 * Follows the blocking state of the vf.  For non-blocking, sets up the pipe
 * return (if any) and the pipe stderr return ready and timeout actions, but
 * leaves timeouts = 0.
 */
static void
uty_pipe_open_complete(vio_vf vf, pid_t pid, int pr_fd, int ps_fd)
{
  vfd_io_type_t  iot ;

  vf->child = uty_child_register(pid, vf) ;

  /* The pr_vfd and ps_vfd blocking states must follow the vf.
   */
  iot = vfd_io_read | (vf->blocking ? vfd_io_ps_blocking : 0) ;

  /* If there is a pipe return, set up vfd and for non-blocking prepare the
   * read ready and read timeout actions.
   */
  vf->pr_enabled = (pr_fd >= 0) ;
  if (vf->pr_enabled)
    {
      vf->pr_vfd   = vio_vfd_new(pr_fd, vfd_pipe, iot, vf) ;
      vf->pr_state = vf_open ;

      if (!vf->blocking)
        {
          vio_vfd_set_read_action(vf->pr_vfd, uty_pipe_return_ready) ;
          uty_pipe_return_set_read_ready(vf) ;
        } ;
    } ;

  /* Set up vfd for pipe stderr return  and for non-blocking prepare the
   * read ready and read timeout actions.
   */
  qassert(ps_fd >= 0) ;

  vf->ps_enabled = (ps_fd >= 0) ;
  if (vf->ps_enabled)
    {
      vf->ps_vfd   = vio_vfd_new(ps_fd, vfd_pipe, iot, vf) ;
      vf->ps_state = vf_open ;

      if (!vf->blocking)
        {
          vio_vfd_set_read_action(vf->ps_vfd, uty_pipe_return_ready) ;
          uty_pipe_return_set_read_ready(vf) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Fork fork fork
 *
 * Set up pipes according to pipe_type_t of fork:
 *
 *   vin_pipe    -- output to  child's stdin : not set up
 *                  input from child's stdout: set up -> main fd
 *
 *   vout_pipe   -- output to  child's stdin : set up -> main fd
 *                  input from child's stdout: set up -> return
 *
 *   vout_sh_cmd -- output to  child's stdin : not set up
 *                  input from child's stdout: set up -> return
 *
 * In all cases, the input from the child's stdout: set up -> stderr return
 *
 * vfork to create child process and then:
 *
 *   -- in parent close the unused part(s) of the pair(s).
 *
 *   -- in child, close all unused fds, and move relevant part(s) of pair(s)
 *      to stdin, stdout and stderr, sort out signal, set pwd then exec sh -c.
 *
 * Returns:  > 0  -- OK, this is child pid
 *           < 0  -- Failed
 *
 * NB: only returns in the parent process -- exec's in the child.
 */
static pid_t
uty_pipe_fork(vty_io vio, const char* cmd_str, std_set set, pipe_type_t type)
{
  pid_t child ;
  int   id ;

  /* Empty the set
   */
  for (id = 0 ; id < stds ; ++id)
    {
      set[id][in_half]  = -1 ;
      set[id][out_half] = -1 ;
    } ;

  /* Open as many fd pairs as are required -- sets non-blocking and close-on-
   * exec on the parent's half of each one.
   */
  if (type == vin_pipe)
    if (!uty_fd_pair(vio, cmd_str, "input pipe", set, stdout_fd, in_half))
      return -1 ;

  if ((type == vout_pipe) || (type == vout_sh_cmd))
    if (!uty_fd_pair(vio, cmd_str, "return pipe", set, stdout_fd, in_half))
      return -1 ;

  if (type == vout_pipe)
    if (!uty_fd_pair(vio, cmd_str, "output pipe", set, stdin_fd, out_half))
      return -1 ;

  if (!uty_fd_pair(vio, cmd_str, "stderr pipe", set, stderr_fd, in_half))
    return -1 ;

  /* Off to the races                                                   */

  child = vfork() ;

  if      (child == 0)          /* In child                             */
    {
      /* Prepare all file descriptors and then execute the child
       */
      if (uty_pipe_exec_prepare(vio, set))
        execl("/bin/bash", "bash", "-c", cmd_str, NULL) ; /* does not return */
      else
        exit(0x80 | errno) ;
    }
  else if (child > 0)           /* In parent -- success                 */
    {
      /* close the fds we do not need in the parent
       */
      uty_pipe_close_half(set[stdin_fd],  in_half) ;
      uty_pipe_close_half(set[stdout_fd], out_half) ;
      uty_pipe_close_half(set[stderr_fd], out_half) ;
    }
  else if (child < 0)           /* In parent -- failed                  */
    uty_pipe_fork_fail(vio, cmd_str, "vfork", set, NULL, "child") ;

  return child ;
} ;

/*------------------------------------------------------------------------------
 * Open a pipe pair -- generate suitable error message if failed and close
 * any earlier pairs that have been opened.
 *
 * For the parent process half of the pair we set non-blocking and
 * close-on-exec.
 *
 * The close-on-exec ensures that any other children of the main process do
 * *not* inherit the parents connection(s) to other children.  In the
 * uty_pipe_exec_prepare() we arrange for all children forked here to set
 * absolutely everything except the child's pipes to be close-on-exec.
 * The library popen() is not as tidy... so a child could be confused by the
 * popen sibling having copies of the parent's pipes !!
 *
 * Returns:  true <=> success
 */
static bool
uty_fd_pair(vty_io vio, const char* cmd_str,
                          const char* what, std_set    set,
                                            std_id_t   id,
                                            fd_pair_half_t parent_half)
{
  fd_pair pair ;

  pair = set[id] ;

  if (pipe(pair) < 0)
    return uty_pipe_fork_fail(vio, cmd_str, what, set, pair, "open") ;

  if (set_nonblocking(pair[parent_half]) < 0)
    return uty_pipe_fork_fail(vio, cmd_str, what, set, NULL,
                                                      "set non-blocking for") ;

  if (set_close_on_exec(pair[parent_half]) < 0)
    return uty_pipe_fork_fail(vio, cmd_str, what, set, NULL,
                                                      "set close-on-exec for") ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Failed to open pipe: generate suitable error message and close any earlier
 * pairs that have been opened.
 *
 * Returns:  false
 */
static bool
uty_pipe_fork_fail(vty_io vio, const char* cmd_str,
                                           const char* what,
                                           std_set     set,
                                           fd_pair     pair,
                                           const char* action)
{
  int   err = errno ;
  int   id ;

  /* Close anything that has been opened -- starting by setting the failed
   * pair to not opened.
   */
  pair[in_half] = pair[out_half] = -1 ; /* make sure    */

  for (id = 0 ; id < stds ; ++id)
    {
      uty_pipe_close_half(set[id], in_half) ;
      uty_pipe_close_half(set[id], out_half) ;
    } ;

  uty_out(vio, "%% Failed to %s %s for %s\n", action, what, cmd_str) ;

  errno = err ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Close half of an fd_pair, if it is open.
 */
static void
uty_pipe_close_half(fd_pair pair, fd_pair_half_t half)
{
  if (pair[half] >= 0)
    {
      close(pair[half]) ;
      pair[half] = -1 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * In the child process...  prepare to exec the shell.
 *
 * Discard all fd's except for the those required for the shell command.
 *
 * Arrange for the pipe fd's to be stdin/stdout/stderr as required.  The pairs
 * are named wrt to the child process.
 *
 * Set current directory.
 *
 * Reset all signals to default state and unmask everything.
 *
 * Returns:  true <=> good to go
 *          false  => some sort of error -- see errno
 */
static bool
uty_pipe_exec_prepare(vty_io vio, std_set set)
{
  int   std[stds] ;
  int   fd ;

  /* Assign fds to child's std[]                                        */

  std[stdin_fd]  = set[stdin_fd] [in_half] ;
  std[stdout_fd] = set[stdout_fd][out_half] ;
  std[stderr_fd] = set[stderr_fd][out_half] ;

  /* Mark everything to be closed on exec                               */
  for (fd = 0 ; fd < qlib_open_max ; ++fd)
    {
      int fd_flags ;
      fd_flags = fcntl(fd, F_GETFD, 0) ;
      if (fd_flags >= 0)
        fcntl(fd, F_SETFD, fd_flags | FD_CLOEXEC) ;
    } ;

  /* Now dup anything of what we keep to ensure is above 2, making sure that
   * the dup remains FD_CLOEXEC.
   *
   * This is highly unlikely, so no real extra work.  It simplifies the next
   * step which moves fds to the required position, and clears the FD_CLOEXEC
   * flag on the duplicate.
   */
  for (fd = 0 ; fd < stds ; ++fd)
    {
      if ((std[fd] >= 0) && (std[fd] < stds))
        if ((std[fd] = fcntl(std[fd], F_DUPFD_CLOEXEC, stds)) < 0)
          return false ;
    } ;

  /* Now dup2 to the required location -- destination is NOT FD_CLOEXEC.
   */
  for (fd = 0 ; fd < stds ; ++fd)
    {
      if (std[fd] >= 0)
        if (dup2(std[fd], fd) != fd)
          return false ;
    } ;

  /* Finally (for fds) if we don't have a stdout for the child,
   * dup stderr to stdout.
   */
  if ((std[stdout_fd] < 0) && (std[stderr_fd] >= 0))
    if (dup2(stderr_fd, stdout_fd) != stdout_fd)
      return false ;

  /* Clear down all the signals magic.                                  */
  quagga_signal_reset() ;

  /* Set the working directory                                          */

  if (qpath_setcwd(vio->vty->exec->context->dir_cd) != 0)
    return false ;

  return true ;                 /* coming, ready or not         */
} ;

/*------------------------------------------------------------------------------
 * Suck on the pipe return and/or the pipe stderr return:
 *                                      for VIN_PIPE, VOUT_PIPE and VOUT_SH_CMD
 *
 * If the pipe return is active, shovel from the pipe return to the slave
 * output and push (or read and discard, if !pr_enabled).
 *
 * If the pipe stderr return is active, read into the vf->ps_buf (or read and
 * discard, if !ps_enabled.
 *
 * Reads as much as is available but does not block.
 *
 * When has read everything available, but is not EOF, returns either
 * CMD_SUCCESS (blocking or no longer active), or CMD_WAITING (non-blocking,
 * waiting for more on one or both returns).
 *
 * NB: uty_cmd_out_push() does not block.
 *
 * Returns:  CMD_SUCCESS  -- done everything possible (at the moment)
 *           CMD_WAITING  -- waiting for more  => non-blocking
 *           CMD_IO_ERROR -- error or time-out
 *
 * This can be called in any thread.
 */
static cmd_ret_t
uty_pipe_return_suck(vio_vf vf)
{
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert( (vf->vin_type == VIN_PIPE) || (vf->vout_type == VOUT_PIPE)
                                      || (vf->vout_type == VOUT_SH_CMD) ) ;

  ret = CMD_SUCCESS ;

  /* Suck on the pipe return and blow to slave vout
   */
  while (vf_active(vf->pr_state))
    {
      int get ;

      qassert(vf->vin_type != VIN_PIPE) ;

      if (vf->vout_next->vout_state & (vf_cancel | vf_end))
        vf->pr_enabled = false ;

      if (vf->pr_enabled)
        get = vio_fifo_read_nb(vf->vout_next->obuf, vio_vfd_fd(vf->pr_vfd),
                                0, 1) ; /* read a lump at a time        */
      else
        get = read_nb(vio_vfd_fd(vf->pr_vfd), dev_null, sizeof(dev_null)) ;

      if (get > 0)                      /* Read something               */
        {
          if (vf->pr_enabled)
            {
              /* Push to the slave, knowing that this will not block.
               *
               * If the slave fails, then clear enabled, so we discard all
               * further input.
               */
              if (uty_cmd_out_push(vf->vout_next) == CMD_IO_ERROR)
                vf->pr_enabled = false ;
            } ;

          continue ;
        } ;

      if (get < 0)
        {
          if (get == -1)                /* Hit error                    */
            return uty_vf_error(vf, verr_io_pr, errno) ;  /* CMD_IO_ERROR */
                                /* will not return CMD_HIATUS, because
                                 * vf->pr_state == vf_open !            */

          qassert (get == -2) ;         /* eof on the return            */

          uty_vf_return_stop(&vf->pr_state, vfs_stop_end) ;
          break ;                       /* OK, so far                   */
        } ;

      /* Nothing there to read, but not eof.
       */
      qassert(get == 0) ;

      ret = CMD_WAITING ;
      break ;
    } ;

  /* Suck on the pipe stderr return
   */
  while (vf_active(vf->ps_state))
    {
      int get ;

      if (vf->ps_enabled)
        get = vio_fifo_read_nb(uty_pipe_ps_buf(vf), vio_vfd_fd(vf->ps_vfd),
                                  0, 1) ; /* read a lump at a time      */
      else
        get = read_nb(vio_vfd_fd(vf->ps_vfd), dev_null, sizeof(dev_null)) ;

      if (get > 0)              /* Read something                       */
        continue ;

      if (get < 0)
        {
          if (get == -1)        /* Hit error on stderr return   */
            return uty_vf_error(vf, verr_io_ps, errno) ; /* CMD_IO_ERROR */

          qassert (get == -2) ; /* eof on the stderr return             */

          uty_vf_return_stop(&vf->ps_state, vfs_stop_end) ;
          break ;               /* return current ret                   */
        } ;

      /* Nothing there to read, but not eof.
       */
      qassert(get == 0) ;

      ret = CMD_WAITING ;
      break ;
    } ;

  return (vf->blocking) ? CMD_SUCCESS : ret ;
} ;

/*------------------------------------------------------------------------------
 * For non-blocking: if there is an active pipe return or an active pipe stderr
 * return or both, set read-ready and timeout as required.
 *
 * If either of the vin and vout is active: neither pipe return has a timeout.
 *
 * If neither the vin nor the vout is active: the pipe return carries the
 * timeout while it is active, otherwise the pipe stderr return carries it;
 * and if both returns are active, input from either resets the timeout.
 *
 * Returns:  CMD_WAITING   -- set read-ready on one or both
 *           CMD_IO_ERROR  -- failed to set read-ready on either one
 *           CMD_SUCCESS   -- neither is active
 */
static cmd_ret_t
uty_pipe_return_set_read_ready(vio_vf vf)
{
  cmd_ret_t      ret ;
  vio_timer_time timeout ;

  ret = CMD_SUCCESS ;

  qassert(!vf->blocking) ;

  if (vf_active(vf->vin_state) || vf_active(vf->vout_state))
    timeout = 0 ;
  else
    timeout = pipe_return_timeout ;

  if (vf_active(vf->pr_state))
    {
      if (vio_vfd_set_read(vf->pr_vfd, on, timeout) != on)
        return uty_vf_not_open_error(vf, verr_io_pr) ;

      vf->pr_timeout = (timeout != 0) ;

      timeout = 0 ;
      ret = CMD_WAITING ;
    } ;

  if (vf_active(vf->ps_state))
    {
      if (vio_vfd_set_read(vf->ps_vfd, on, timeout) != on)
        return uty_vf_not_open_error(vf, verr_io_ps) ;

      vf->ps_timeout = (timeout != 0) ;

      ret = CMD_WAITING ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Pipe return and/or pipe stderr return ready to read or timed out:
 *                            call-back for VIN_PIPE, VOUT_PIPE and VOUT_SH_CMD.
 *
 * This is used for non-blocking.
 *
 * If time_out and vf_open, signal time-out to the command loop.
 *
 * Otherwise, if vf_open shovel from the return to the slave output, and set
 * read-ready again if that returns CMD_WAITING.
 *
 * If is not vf_open, signals the command loop, just in case.
 *
 * Note that the read-ready state and any time-out are automatically
 * disabled when they go off.
 */
static void
uty_pipe_return_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  vio_vf    vf ;
  cmd_ret_t ret ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert((vf->pr_vfd == vfd) || (vf->ps_vfd == vfd)) ;

  qassert( (vf->vin_type == VIN_PIPE) || (vf->vout_type == VOUT_PIPE)
                                      || (vf->vout_type == VOUT_SH_CMD) ) ;
  qassert(!vf->blocking) ;

  ret = uty_pipe_return_suck(vf) ;

  if (ret == CMD_WAITING)
    {
      if (time_out)
        ret = uty_vf_error(vf, (vf->pr_vfd == vfd) ? verr_to_pr
                                                   : verr_to_ps, 0) ;
      else
        ret = uty_pipe_return_set_read_ready(vf) ;
    } ;

  /* Signal CMD_SUCCESS or CMD_IO_ERROR -- CMD_WAITING will be ignored.
   *
   * CMD_IO_ERROR should already have been signalled, but does no harm to
   * signal it again.
   */
  uty_cmd_signal(vf->vio, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Collect the child and post and pipe stderr return to the vio->ps_buf (or
 * discard, if vst_cancel).
 *
 * This is the final stage of closing a pipe, after all I/O has been dealt
 * with.
 *
 * Phase 0: close any remaining pipe returns
 *
 * Phase 1: collect the child return code and deal with it:
 *
 *   If the child has yet to be collected:
 *
 *     if vst_final, collect child immediately or set overdue.
 *
 *     if not vst_final:
 *
 *       if non-blocking, return CMD_WAITING and wait for the SIGCHLD to do
 *       the business or to time out.
 *
 *       if blocking, collect child or time out and set overdue.
 *
 *   If the child is not collected, or has not terminated cleanly, output
 *   diagnostic to the pipe stderr return.
 *
 *   If the child is not collected, the SIGCHLD handling will tidy up in the
 *   background.
 *
 *   In any case, dismiss child.
 *
 * Phase 2: transfer the pipe stderr return to the main vio pipe stderr return.
 *
 *   This leaves it up to the vout_base to deal with the pipe stderr return,
 *   in its own time -- e.g when the output stack closes.
 *
 *   Does nothing here if is vst_cancel !
 *
 * When this (eventually) returns CMD_SUCCESS, all pipe return has been
 * sucked up and has cleared the slave vout buffer, and the child has been
 * collected -- unless something has gone wrong, and part of that has been
 * short-circuited.
 *
 * Returns:  CMD_SUCCESS   -- closed and child collected
 *                            if "final" we might have needed to wait or to
 *                                                           block, but did not.
 *           CMD_WAITING   -- waiting for output to complete   => ! final
 *                                                             => ! blocking
 *           CMD_IO_ERROR  -- error or time-out                => ! final
 *
 * NB: if "final", whatever happens the pipe return(s) are closed and the
 *     child dismissed.
 */
static cmd_ret_t
uty_pipe_close(vio_vf vf)
{
  vty_io vio ;
  bool cancelled ;
  bool enabled ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  vio = vf->vio ;

  /* Phase 0: close the pipe returns
   *
   * NB: this does not affect ps_enabled
   */
  if (vf->pr_state != vf_closed)
    {
      vf->pr_vfd     = vio_vfd_close(vf->pr_vfd) ;
      vf->pr_state   = vf_closed ;
      vf->pr_enabled = false ;          /* for completeness     */
    }
  else
    qassert(vf->pr_vfd == NULL) ;

  cancelled = vf->ps_state & vf_cancel ;
  enabled   = vf->ps_enabled && !cancelled ;

  if (vf->ps_state != vf_closed)
    {
      vf->ps_vfd     = vio_vfd_close(vf->ps_vfd) ;
      vf->ps_state   = vf_closed ;
      vf->ps_enabled = false ;      /* for completness      */
    }
  else
    qassert(vf->ps_vfd == NULL) ;

  /* Phase 1: if not already collected, collect the child
   *
   *          When does collect, or times out, will dismiss the child and set
   *          vf->child to NULL -- which indicates that the child is done with.
   *
   *          If pipe was "quiet", or we are vst_final or vst_cancel, then
   *          we complete this without blocking or waiting and whether or not
   *          gets CMD_IO_ERROR.
   */
  if (vf->child != NULL)
    {
      /* Collect -- blocking or not blocking
       */
      if (!vf->child->collected && !vf->child->overdue)
       {
          /* If we are blocking or cancelled, try to collect the child here
           * and now -- if not cancelled may block here.
           *
           * For non-blocking and not cancelled, leave the child collection up
           * to the SIGCHLD system.
           */
          if (vf->blocking || cancelled)
            {
              uty_child_collect(vf->child, child_timeout, cancelled) ;
            }
          else
            {
              uty_child_awaited(vf->child, child_timeout) ;
              return CMD_WAITING ;
            } ;
       } ;

      /* If child is overdue, or did not terminate cleanly, write message to
       * the pipe stderr return -- provided that is (still) enabled.
       */
      if (enabled)
        {
          if      (!vf->child->collected)
            {
              vio_fifo_printf(uty_pipe_ps_buf(vf),
                                          "%% child process still running\n") ;
            }
          else if (WIFEXITED(vf->child->report))
            {
              int status = WEXITSTATUS(vf->child->report) ;
              if (status != 0)
                vio_fifo_printf(uty_pipe_ps_buf(vf),
                    "%% child process ended normally, but with status = %d\n",
                                                                       status) ;
            }
          else if (WIFSIGNALED(vf->child->report))
            {
              int signal = WTERMSIG(vf->child->report) ;
              vio_fifo_printf(uty_pipe_ps_buf(vf),
                    "%% child process terminated by signal = %d\n", signal) ;
            }
          else
            {
              vio_fifo_printf(uty_pipe_ps_buf(vf),
                    "%% child process ended in unknown state = %d\n",
                                                            vf->child->report) ;
            } ;
         } ;

      /* Can now dismiss the child -- if not collected, is left on the register.
       */
      uty_child_dismiss(vf->child, false) ;     /* not curtains         */
    } ;

  /* Phase 2: if there is anything in the pipe stderr return buffer, finish
   *          it, and transfer to the vio->ps_buf (unless vio->cancel).
   */
  if (enabled && (vf->ps_buf != NULL))
    {
      vio_fifo_trim(vf->ps_buf, true) ;         /* trim trailing whitespace
                                                 * and '\n' terminate   */
      if (!vio_fifo_is_empty(vf->ps_buf))
        {
          const char* what ;

          if      (vf->vin_type == VIN_PIPE)
            what = "<|" ;
          else if (vf->vout_type == VOUT_PIPE)
            what = ">|" ;
          else
            what = "|" ;

          if (vio->ps_buf == NULL)
            vio->ps_buf = vio_fifo_new(1024) ;

          vio_fifo_printf(vio->ps_buf, "%%--- in '%s %s':\n", what, vf->name) ;
          vio_fifo_copy(vio->ps_buf, vf->ps_buf) ;
        } ;
    } ;

  vf->ps_buf = vio_fifo_free(vf->ps_buf) ;

  /* Complete !  Note that always gets here if "final".
   */
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Get pipe stderr return buffer for the vf -- make one if none yet exists.
 */
static vio_fifo
uty_pipe_ps_buf(vio_vf vf)
{
  if (vf->ps_buf == NULL)
    vf->ps_buf = vio_fifo_new(1024) ;

  return vf->ps_buf ;
} ;


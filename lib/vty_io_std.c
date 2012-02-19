/* VTY I/O for stdout/stderr
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

#include "vty_io_std.h"
#include "vty_io.h"

/*==============================================================================
 * Here we handle:
 *
 *   * VOUT_STDOUT: stdout and stderr output -- for write only
 *
 * This is intended for use early in the morning -- in particular, before
 * and during the reading of configuration file -- and late at night, once
 * everything else has shut down.
 *
 * See vty_err().
 *
 * Uses straightforward FILE* I/O.
 *
 * NB: VOUT_STDOUT may only be a vout_base item.
 *
 * NB: this is simple-minded BLOCKING stuff.  It is assumed:
 *
 *      (a) that stdout and stderr will not block (or not for long)
 *
 *      (b) that these are NOT used in normal running -- so if they do block,
 *          it is not, in any case, critical.
 *
 *      (c) that multiple threads will not use these -- or if they do, then
 *          the usual interleaving of output is acceptable.
 *
 * When output is pushed, will perform a fflush().
 *
 * Post daemonisation:
 *
 *   * stdout output will be lost unless a stdout file is set (TODO).
 *
 *   * stderr output is redirected to logging (at some priority TODO).
 */

/*------------------------------------------------------------------------------
 * Set up VTY on which to write to stdout.
 *
 * NB: sets up a blocking vio -- so output, and any pushed files and pipes will
 *     block waiting for I/O to complete (or to time out).
 *
 * NB: the name is XSTRDUP() into the vio -- so the caller is responsible for
 *     disposing of its copy, if required.
 */
extern vty
vty_std_write_open(const char* name)
{
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_LOCK() ;

  /* Create the vty and its vio.
   *
   * NB: VTY_STDOUT type vty is set vio->blocking.
   */
  vty = uty_new(VTY_STDOUT, NULL_NODE) ;
  vio = vty->vio ;

  /* Create the vin_base/vout_base vf
   *
   * NB: we set this genuinely blocking, because that's what it is !
   */
  qassert(vio->blocking) ;

  vf = uty_vf_new(vio, name, -1, vfd_none, vfd_io_write | vfd_io_os_blocking) ;

  uty_vin_push(vio, vf, VIN_DEV_NULL, NULL, 0) ;
  uty_vf_read_stop(vf, vfs_stop_cease) ;

  uty_vout_push(vio, vf, VOUT_STDOUT, NULL, std_buffer_size, true /* after */) ;

  VTY_UNLOCK() ;

  return vty ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to a file -- VOUT_STDOUT -- vf_open.
 *
 * NB: we take absolutely no notice of any errors -- discards contents of
 *     fifo if fails at all.
 *
 * NB: uses whatever stdout is set up for, assuming it is simple, blocking I/O.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *
 * This can be called in any thread.
 */
extern cmd_ret_t
uty_std_out_push(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert(vf->vout_type == VOUT_STDOUT) ;

  if (vio_fifo_fwrite(vf->obuf, stdout) < 0)
    vio_fifo_clear(vf->obuf) ;

  fflush(stdout) ;              /* make sure                    */

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * stderr output.
 *
 * Before daemonisation, outputs directly to stderr.
 *
 * Post daemonisation, outputs to logging...  TODO
 *                       or to stderr file... TODO
 */
extern void
uty_std_err_vprintf(const char *format, va_list args)
{
  vfprintf(stderr, format, args) ;
} ;

/*------------------------------------------------------------------------------
 * Output the close reason to VOUT_STDOUT
 *
 * Before daemonisation, outputs directly to stderr.
 *
 * Post daemonisation, output to logging...  TODO (output vf->vio->close_reason
 *                      or to stderr file... TODO
 */
extern void
uty_std_close_reason(vio_vf vf, qstring wrapped)
{
  qassert(vf->vout_type == VOUT_STDOUT) ;

  fputs(qs_string(wrapped), stderr) ;
} ;

/*------------------------------------------------------------------------------
 * Write away any pending stuff -- VOUT_STDOUT.
 *
 * This is a blocking vf -- so any outstanding output will complete here.
 *
 * TODO -- flush any pending post daemonise stderr !
 */
extern cmd_ret_t
uty_std_write_close(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;

  return uty_std_out_push(vf) ;
} ;

/*==============================================================================
 * The VIN_DEV_NULL
 */




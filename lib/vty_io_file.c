/* VTY I/O for Files
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
#include "vty_io_basic.h"
#include "vty_command.h"
#include "network.h"
#include "sigevent.h"
#include "pthread_safe.h"
#include "qlib_init.h"

/*==============================================================================
 * Here we handle:
 *
 *   * VIN_CONFIG/VOUT_CONFIG    -- configuration file read/write
 *
 *   * VIN_FILE/VOUT_FILE        -- file pipe read/write
 *
 *   * VIN_PIPE/VOUT_PIPE        -- shell command pipe read/write
 */

#define PIPEFILE_MODE CONFIGFILE_MASK

enum
{
  file_timeout   = 20,          /* for file read/write          */

  pipe_timeout   = 30,          /* for pipe read/write          */

  child_timeout  = 10,          /* for collecting child process */

  config_buffer_size = 64 * 1024,       /* for config reading           */
  file_buffer_size   = 16 * 1024,       /* for other file read/write    */

  pipe_buffer_size   =  4 * 1024,       /* for pipe read/write          */
} ;

/*==============================================================================
 * VTY Configuration file I/O -- VIN_CONFIG and VOUT_CONFIG.
 *
 * The creation of configuration files is handled elsewhere, as is the actual
 * opening of input configuration files.
 *
 * Once open, VIN_CONFIG and VOUT_CONFIG are read/written in the same way as
 * VIN_FILE and VOUT_FILE.
 *
 * These files are handled as "blocking" because the parent vio is marked as
 * "blocking".
 */

/*------------------------------------------------------------------------------
 * Set up VTY on which to read configuration file -- using already open fd.
 *
 * Sets the vout_base to be VOUT_STDERR.
 *
 * NB: sets up a blocking vio -- so that the vin_base, vout_base and all files
 *     and pipes will block waiting for I/O to complete (or to time out).
 *
 * NB: the name is XSTRDUP() into the vio -- so the caller is responsible for
 *     disposing of its copy, if required.
 */
extern vty
vty_config_read_open(int fd, const char* name, bool full_lex)
{
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_LOCK() ;

  /* Create the vty and its vio.
   *
   * NB: VTY_CONFIG_READ type vty is set vio->blocking.
   */
  vty = uty_new(VTY_CONFIG_READ, CONFIG_NODE) ;

  vio = vty->vio ;

  /* Create the vin_base/vout_base vf
   *
   * NB: don't need to specify vfd_io_ps_blocking, because the vio is set
   *     blocking.
   */
  qassert(vio->blocking) ;

  vf = uty_vf_new(vio, name, fd, vfd_file, vfd_io_read) ;

  uty_vin_push( vio, vf, VIN_CONFIG,  NULL, NULL, config_buffer_size) ;
  vf->read_timeout   = file_timeout ;
  uty_vout_push(vio, vf, VOUT_STDERR, NULL, NULL, pipe_buffer_size, true) ;
  vf->write_timeout  = file_timeout ;

  /* Deal with the possibility that while reading the configuration file, may
   * use a pipe, and therefore may block waiting to collect a child process.
   *
   * Before there is any chance of a SIGCHLD being raised for a child of the
   * configuration file, invoke the magic required for SIGCHLD to wake up
   * a pselect() while waiting to collect child.
   */
  uty_child_signal_nexus_set(vio) ;

  VTY_UNLOCK() ;

  return vty ;
} ;

/*------------------------------------------------------------------------------
 * Tidy up after config input file has been closed.
 *
 * There is now no further possibility that will block waiting for child to be
 * collected.
 *
 * Nothing further required -- input comes to a halt.
 */
extern cmd_return_code_t
uty_config_read_close(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vin_state == vf_end) ;

  uty_child_signal_nexus_clear(vf->vio) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Push the given fd as the VOUT_CONFIG.
 *
 * Note that the fd is assumed to be genuine I/O blocking, so this is a
 * "blocking" vf, and so can be opened and closed in the cmd thread.
 */
extern void
vty_config_write_open(vty vty, int fd)
{
  vty_io vio ;
  vio_vf vf ;

  VTY_LOCK() ;

  vio = vty->vio ;

  vf = uty_vf_new(vio, "config write", fd, vfd_file,
                                                vfd_io_read | vfd_io_blocking) ;
  uty_vout_push(vio, vf, VOUT_CONFIG, NULL, NULL, file_buffer_size, false) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Write away any pending stuff, and pop the VOUT_CONFIG.
 *
 * This is a blocking vf -- so any outstanding output will complete here.
 */
extern cmd_return_code_t
vty_config_write_close(struct vty* vty)
{
  cmd_return_code_t ret ;
  VTY_LOCK() ;

  qassert(vty->vio->vout->vout_type == VOUT_CONFIG) ;

  ret = uty_vout_pop(vty->vio, false) ;

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
static void vty_file_read_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time vty_file_read_timeout(vio_timer timer,
                                                            void* action_info) ;

static void vty_file_write_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time vty_file_write_timeout(vio_timer timer,
                                                            void* action_info) ;

static cmd_return_code_t uty_fifo_command_line(vio_vf vf, cmd_action action) ;

/*------------------------------------------------------------------------------
 * Open file for input, to be read as commands -- VIN_FILE.
 *
 * If could not open, issues message to the vio.
 *
 * If opens OK, save the current context in the current vin (before pushing
 * the new vin).
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open
 *
 * If "blocking" vf, this can be called from any thread, otherwise must be the
 * cli thread -- see uty_vfd_new().
 */
extern cmd_return_code_t
uty_file_read_open(vty_io vio, qstring name, cmd_context context)
{
  cmd_return_code_t ret ;
  qpath       path ;
  const char* pns ;
  int     fd ;
  vio_vf  vf ;
  vfd_io_type_t iot ;

  VTY_ASSERT_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  /* Now is the time to complete the name, if required.                 */

  path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
  pns  = qpath_string(path) ;

  /* Do the basic file open.  May be vfd_io_ps_blocking, but we don't care
   * about that here.
   */
  iot = vfd_io_read ;

  fd = uty_fd_file_open(pns, iot, (mode_t)0) ;  /* cmode not required   */

  /* If failed, report.
   * If OK save context & update "here" then create and push the vin.
   */
  if (fd < 0)
    {
      uty_out(vio, "%% Could not open input file %s: %s\n", pns,
                                                         errtoa(errno, 0).str) ;
      ret = CMD_WARNING ;
    }
  else
    {
      uty_vin_new_context(vio, context, path) ;

      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vin_push(vio, vf, VIN_FILE, vty_file_read_ready,
                                      vty_file_read_timeout, file_buffer_size) ;
      vf->read_timeout   = file_timeout ;

      ret = CMD_SUCCESS ;
    } ;

  qpath_free(path) ;
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
 *           CMD_WARNING  -- failed to open
 *
 * If "blocking" vf, this can be called from any thread, otherwise must be the
 * cli thread -- see uty_vfd_new().
 */
extern cmd_return_code_t
uty_file_write_open(vty_io vio, qstring name, bool append, cmd_context context,
                                                                     bool after)
{
  cmd_return_code_t ret ;
  qpath       path ;
  const char* pns ;
  int   fd ;
  vio_vf vf ;
  vfd_io_type_t iot ;

  VTY_ASSERT_LOCKED() ;

  /* Now is the time to complete the name, if required.                 */

  path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
  pns  = qpath_string(path) ;

  /* Do the basic file open.  May be vfd_io_ps_blocking, but we don't care
   * about that here.
   */
  iot =  vfd_io_write | (append ? vfd_io_append : 0) ;

  fd = uty_fd_file_open(pns, iot, PIPEFILE_MODE) ;

  /* If failed, report.
   * If OK, create and push the vout.
   */
  if (fd < 0)
    {
      uty_out(vio, "%% Could not open output file %s: %s\n", pns,
                                                         errtoa(errno, 0).str) ;
      ret = CMD_WARNING ;
    }
  else
    {
      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vout_push(vio, vf, VOUT_FILE, vty_file_write_ready,
                              vty_file_write_timeout, file_buffer_size, after) ;
      vf->write_timeout  = file_timeout ;

      ret = CMD_SUCCESS ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command line fetch from a file -- VIN_FILE or VIN_CONFIG, in vf_open state.
 *
 * Returns: CMD_SUCCESS    -- have another command line ready to go
 *          CMD_WAITING    -- would not wait for input  <=> non-blocking
 *          CMD_EOF        -- ran into EOF (and nothing else)
 *          CMD_IO_ERROR   -- ran into an I/O error or time-out
 *
 * In "non-blocking" state, on CMD_WAITING sets read ready, which will
 * vty_cmd_signal() the command loop to come round again.
 *
 * In "blocking" state, will not return until have something, or there is
 * nothing to be had, or times out.
 *
 * This can be called in any thread.
 *
 * NB: the vin_state is set to vf_end when CMD_EOF is returned, and this
 *     code may not be called again.
 *
 * NB: the vin_state is set to vf_end when CMD_IO_ERROR is returned, and
 *     this code may not be called again.
 *
 *     When an error occurs it is signalled to the command loop.  This function
 *     is called from the command loop -- so, in fact, the CMD_IO_ERROR return
 *     code does the trick.
 */
extern cmd_return_code_t
uty_file_fetch_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert((vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)) ;
  qassert(vf->vin_state == vf_open) ;

  while (1)
    {
      cmd_return_code_t ret ;

      /* Try to complete line straight from the buffer.
       *
       * If buffer is empty and have seen eof on the input, signal CMD_EOF.
       */
      ret = uty_fifo_command_line(vf, action) ;

      if (ret != CMD_WAITING)
        return ret ;

      /* Could not complete line and exhausted contents of fifo         */
      while (1)
        {
          qps_mini_t qm ;
          int  get ;

          get = vio_fifo_read_nb(vf->ibuf, vio_vfd_fd(vf->vfd), 1) ;

          if (get > 0)
            break ;             /* loop back to uty_fifo_command_line() */

          if (get == -1)
            return uty_vf_error(vf, verr_io_vin, errno) ;

          if (get == -2)
            {
              /* Hit end of file, so set the vf into vf_end.
               *
               * NB: does not know has hit eof until tries to read and nothing
               *     is returned.
               *
               * Loop back so that uty_fifo_command_line() can reconsider, now
               * that we know that there is no more data to come -- it may
               * signal CMD_SUCCESS (non-empty final line) or CMD_EOF.
               */
              qassert(vio_fifo_empty(vf->ibuf)) ;

              vf->vin_state = vf_end ;

              break ;           /* loop back to uty_fifo_command_line() */
            } ;

          /* Would block -- for non-blocking return CMD_WAITING, for
           * blocking we block here with a timeout, and when there is more
           * to read, loop back to vio_fifo_read_nb().
           */
          qassert(get == 0) ;

          if (!vf->blocking)
            {
              uty_vf_set_read(vf, on) ;
              return CMD_WAITING ;
            } ;

          /* Implement blocking I/O, with timeout                       */
          qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_read_mnum,
                                                             vf->read_timeout) ;
          if (qps_mini_wait(qm, NULL, false) != 0)
            continue ;          /* loop back to vio_fifo_read_nb()      */

          return uty_vf_error(vf, verr_to_vin, 0) ;
       } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Try to complete a command line for the given vf, from the current input
 * fifo -- performs NO I/O.
 *
 * Expects that most of the time will be able to set the vf->cl to point
 * directly at a command line in the fifo -- but at the edge of fifo buffers
 * (and if get continuation lines) will copy line fragments to the vf->cl.
 *
 * Returns: CMD_SUCCESS   -- have a command line.
 *          CMD_EOF       -- there is no more
 *          CMD_WAITING   -- waiting for more
 *
 * If vf->vin_state == vf_end, will return CMD_SUCCESS if have last part line
 * in hand.  Otherwise will return CMD_EOF, any number of times.
 */
static cmd_return_code_t
uty_fifo_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

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

      /* Get what we can from the fifo                                  */
      have = vio_fifo_get(vf->ibuf) ;

      /* If fifo is empty, may be last line before eof, eof or waiting  */
      if (have == 0)
        {
          if (vf->vin_state == vf_end)
            {
              if (qs_len_nn(vf->cl) > 0)
                break ;         /* have non-empty last line     */
              else
                return CMD_EOF ;
            } ;

          return CMD_WAITING ;
        } ;

      qassert(vf->vin_state != vf_end) ;        /* not empty => not eof */

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
          qs_append_str_n(vf->cl, s, p - s) ;
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
            qs_append_str_n(vf->cl, s, len) ;

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

      if ((p == s) || (*(p-1) != '\\'))
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

  action->to_do = cmd_do_command ;
  action->line  = vf->cl ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * File is ready to read -- call-back for VIN_FILE.
 *
 * This is used if the VIN_FILE is non-blocking.
 *
 * Signals command loop, so may continue if was waiting.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_file_read_ready(vio_vfd vfd, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  /* If the vin is no longer open then read ready should have been turned
   * off -- but kicking the command loop will not hurt.
   */
  uty_cmd_signal(vf->vio, CMD_SUCCESS) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * File has timed out, waiting to read -- call-back for VIN_FILE.
 *
 * This is used if the VIN_FILE is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_file_read_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->read_timer == timer) ;

  uty_vf_error(vf, verr_to_vin, 0) ;   /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Command output push to a file -- VOUT_FILE or VOUT_CONFIG -- vf_open.
 *
 * Unless all || final this will not write the end_lump of the fifo, so output
 * is in units of the fifo size -- which should be "chunky".
 *
 * Although it is unlikely to happen, if blocking, will block if cannot
 * completely write away what is required, or enable write ready.
 *
 * If final, will write as much as possible, but not block and will ignore
 * any errors (but will return an error return code).
 *
 * If an error occurred earlier, then returns immediately (CMD_SUCCESS).
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * If "non-blocking", on CMD_WAITING the pselect() background process
 * will complete the output and signal the result via uty_cmd_signal().
 *
 * If "blocking", will not return until have written away everything there is,
 * or cannot continue.
 *
 * This can be called in any thread.
 */
extern cmd_return_code_t
uty_file_out_push(vio_vf vf, bool final, bool all)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)) ;
  qassert(vf->vout_state == vf_open) ;

  /* If squelching, dump anything we have in the obuf.
   *
   * Otherwise, write contents away.
   */
  if (vf->vio->cancel)
    vio_fifo_clear(vf->obuf, false) ;   /* keep end mark        */
  else
    {
      while (1)
        {
          qps_mini_t qm ;
          int n ;

          n = vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), all || final) ;

          if (n < 0)
            return uty_vf_error(vf, verr_io_vout, errno) ;

          if (n == 0)
            break ;             /* all gone                             */

          /* Cannot write everything away without waiting               */
          if (!vf->blocking)
            {
              if (!final)
                uty_vf_set_write(vf, on) ;
              return CMD_WAITING ;
            } ;

          /* Implement blocking I/O, with timeout                       */
          if (final)
            return CMD_WAITING ;

          qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_write_mnum,
                                                          vf->write_timeout) ;
          if (qps_mini_wait(qm, NULL, false) != 0)
            continue ;          /* Loop back to vio_fifo_write_nb()     */

          return uty_vf_error(vf, verr_to_vout, 0) ;
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * File is ready to write -- call-back for VOUT_FILE.
 *
 * This is used if the VOUT_FILE is non-blocking.
 *
 * Signals command loop, so may continue if was waiting.
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_file_write_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  /* If the vout is no longer open then write ready should have been turned
   * off -- but kicking the command loop will not hurt.
   */
  if (vf->vout_state == vf_open)
    {
      /* Push, not final and not all -- re-enables write ready if required  */
      ret = uty_file_out_push(vf, false, false) ;
    }
  else
    ret = CMD_SUCCESS ;

  if (ret != CMD_WAITING)
    uty_cmd_signal(vf->vio, ret) ;  /* CMD_SUCCESS or CMD_IO_ERROR  */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * File has timed out, waiting to write -- call-back for VOUT_FILE.
 *
 * This is used if the VOUT_FILE is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_file_write_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->write_timer == timer) ;

  uty_vf_error(vf, verr_to_vout, 0) ;  /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Tidy up after input file has been closed -- VIN_FILE.
 *
 * Nothing further required -- input comes to a halt.
 *
 * Returns: CMD_SUCCESS   -- at all times
 */
extern cmd_return_code_t
uty_file_read_close(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vin_type == VIN_FILE) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Flush output buffer ready for close -- VOUT_FILE and VOUT_CONFIG.
 *
 * See uty_file_out_push()
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 */
extern cmd_return_code_t
uty_file_write_close(vio_vf vf, bool final)
{
  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  qassert((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)) ;

  if (vf->vout_state == vf_open)
    return uty_file_out_push(vf, final, true) ; /* write all    */
  else
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
 *       stderr  -- collected by Quagga and output (eventually) to the base
 *                  vout.
 *
 *   *  VOUT_SH_CMD -- command line: | shell_command
 *
 *       stdin   -- none
 *
 *       stdout  -- collected by Quagga and output to the next vout.
 *
 *       stderr  -- collected by Quagga and output (eventually) to the base
 *                  vout.
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
 *       For non-blocking vf, the pipe return is read autonomously under
 *       pselect() and pushed to the next vout.  For blocking vf, the pipe
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
 *      For non-blocking vf, the pr_vfd is set read ready.  Then for all
 *      non-blocking pipes, the remaining pipe return input proceeds in the
 *      pselect() process.
 *
 *      For blocking pipes, the remaining pipe return must complete (or
 *      time-out) during the close operation.
 *
 *   2. collect remaining input and close the pipe stderr return.
 *
 *      For non-blocking vf, the ps_vfd is set read ready.  Then for all
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
 * For non-blocking vf the close may return CMD_WAITING, and must be called
 * again later to progress the close.
 *
 * For "final" close will not block or return CMD_WAITING, but complete the
 * process as quickly as possible while outputting as much as possible.
 */
typedef int  pipe_pair_t[2] ;
typedef int* pipe_pair ;

typedef enum pipe_half
{
  in_fd   = 0,
  out_fd  = 1,

} pipe_half_t ;

CONFIRM(STDIN_FILENO  == 0) ;
CONFIRM(STDOUT_FILENO == 1) ;
CONFIRM(STDERR_FILENO == 2) ;

typedef enum std_id
{
  stdin_fd  = STDIN_FILENO,
  stdout_fd = STDOUT_FILENO,
  stderr_fd = STDERR_FILENO,

  stds      = 3

} std_id_t ;

typedef enum pipe_id
{
  in_pipe,
  out_pipe,
  err_pipe,

  pipe_count

} pipe_id_t ;

typedef enum pipe_type
{
  vin_pipe,
  vout_pipe,
  vout_sh_cmd,

} pipe_type_t ;

typedef pipe_pair_t pipe_set[pipe_count] ;

static pid_t uty_pipe_fork(vty_io vio, const char* cmd_str, pipe_set pipes,
                                                            pipe_type_t type) ;
static bool uty_pipe_pair(vty_io vio, const char* cmd_str,
                                      const char* what, pipe_set    pipes,
                                                        pipe_id_t   id,
                                                        pipe_half_t half) ;
static bool uty_pipe_fork_fail(vty_io vio, const char* cmd_str,
                                           const char* what, pipe_set pipes,
                                           pipe_pair   pair,
                                           const char* action) ;
static void uty_pipe_close_half_pipe(pipe_pair pair, pipe_half_t half) ;
static void uty_pipe_open_complete(vio_vf vf, pid_t pid, int pr_fd, int ps_fd) ;
static bool uty_pipe_exec_prepare(vty_io vio, pipe_set pipes) ;

static cmd_return_code_t uty_pipe_return_close(vio_vf vf, bool final) ;
static cmd_return_code_t uty_pipe_shovel(vio_vf vf, bool final) ;
static cmd_return_code_t uty_pipe_stderr_suck(vio_vf vf, bool final) ;
static vio_fifo uty_pipe_ps_buf(vio_vf vf) ;

static void vty_pipe_read_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time vty_pipe_read_timeout(vio_timer timer,
                                                            void* action_info) ;
static void vty_pipe_return_ready(vio_vfd vfd, void* action_info) ;
static void vty_pipe_stderr_return_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time vty_pipe_return_timeout(vio_timer timer,
                                                            void* action_info) ;
static vty_timer_time vty_pipe_stderr_return_timeout(vio_timer timer,
                                                            void* action_info) ;
static void vty_pipe_write_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time vty_pipe_write_timeout(vio_timer timer,
                                                            void* action_info) ;

/*------------------------------------------------------------------------------
 * Open VIN_PIPE: pipe whose child's stdout is read and executed as commands.
 *
 * The child's stderr is read, separately, as the pipe return, and sent to the
 * current vout.
 *
 * The new vin has two vio_fd's, the standard one to read commands, and the
 * other (the pr_vfd) to collect any stderr output (the "return") from the
 * child, which is sent to the current vout.
 *
 * The current vout is made the "slave" of the new vin "master".  For the
 * return the pr_vd reads into the "slave" obuf.
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open -- message sent to vio.
 */
extern cmd_return_code_t
uty_pipe_read_open(vty_io vio, qstring command, cmd_context context)
{
  pipe_set    pipes ;
  const char* cmd_str ;
  vio_vf      vf ;
  pid_t       child ;
  qpath       dir ;

  VTY_ASSERT_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  cmd_str = qs_make_string(command) ;

  /* Fork it                                                            */
  child = uty_pipe_fork(vio, cmd_str, pipes, vin_pipe) ;

  if (child < 0)
    return CMD_WARNING ;

  /* We have a pipe, so now save context                                */
  dir = NULL ;

  if (*cmd_str == '/')
    {
      const char* p ;
      p = cmd_str ;
      while (*p > ' ')
        ++p ;
      dir = qpath_set_n(NULL, cmd_str, p - cmd_str) ;
    } ;

  uty_vin_new_context(vio, context, dir) ;

  if (dir != NULL)
    qpath_free(dir) ;

  /* OK -- now push the new input onto the vin_stack.                   */
  vf = uty_vf_new(vio, cmd_str, pipes[in_pipe][in_fd], vfd_pipe, vfd_io_read) ;
  uty_vin_push(vio, vf, VIN_PIPE, vty_pipe_read_ready,
                                  vty_pipe_read_timeout, pipe_buffer_size) ;
  vf->read_timeout   = pipe_timeout ;

  /* And the err_pair is set as the return from the child               */
  uty_pipe_open_complete(vf, child, -1, pipes[err_pipe][in_fd]) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Open VOUT_PIPE or VOUT_SH_CMD: pipe which is going to be written to
 * (or not, if shell_cmd), where what the pipe returns will be output to the
 * current vout.
 *
 * The new vout becomes the "master", and has two vio_vfd's, one for output to
 * the pipe (this is NULL if shell_only) and the other for the pipe return.
 * The pipe return reads directly into the "slave" obuf.
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open -- message sent to vio.
 */
extern cmd_return_code_t
uty_pipe_write_open(vty_io vio, qstring command, bool shell_cmd, bool after)
{
  pipe_set    pipes ;
  const char* cmd_str ;
  pid_t    child ;
  vio_vf   vf ;

  VTY_ASSERT_LOCKED() ;

  cmd_str = qs_make_string(command) ;

  /* Do the basic file open.                                            */
  child = uty_pipe_fork(vio, cmd_str, pipes, shell_cmd ? vout_sh_cmd
                                                       : vout_pipe) ;
  if (child < 0)
    return CMD_WARNING ;

  /* OK -- now push the new output onto the vout_stack.
   *
   * Note that for VOUT_SH_CMD no vfd is set up, and the vout_state is
   * immediately set to vf_end.
   */
  if (shell_cmd)
    {
      vf = uty_vf_new(vio, cmd_str, -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_SH_CMD, NULL, NULL, 0, after) ;
      vf->vout_state = vf_end ;
    }
  else
    {
      vf = uty_vf_new(vio, cmd_str, pipes[out_pipe][out_fd],
                                                       vfd_pipe, vfd_io_write) ;
      uty_vout_push(vio, vf, VOUT_PIPE, vty_pipe_write_ready,
                              vty_pipe_write_timeout, pipe_buffer_size, after) ;
      vf->write_timeout  = pipe_timeout ;
    } ;

  /* Record the child pid and set up the pr_vf and enslave vout_next    */

  uty_pipe_open_complete(vf, child, pipes[in_pipe][in_fd],
                                                       pipes[err_pipe][in_fd]) ;
  /* Until eof (or error or timeout) on the vin, neither wait for or timeout
   * the pipe return or the pipe stderr return.
   */
  qassert(vf->pr_timeout == 0) ;
  qassert(vf->ps_timeout == 0) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Complete the opening of a pipe.
 *
 * All pipes have a return.  For in_pipes this is the child's stderr, for
 * out_pipes this is the child's stdout and its stderr.
 *
 * For non-blocking, sets up the pipe return and the pipe stderr return ready
 * and timeout actions, but leaves pr_timeout == 0 and ps_timeout == 0
 */
static void
uty_pipe_open_complete(vio_vf vf, pid_t pid, int pr_fd, int ps_fd)
{
  vfd_io_type_t  iot ;

  vf->child = uty_child_register(pid, vf) ;

  iot = vfd_io_read | (vf->blocking ? vfd_io_ps_blocking : 0) ;

  /* If there is a pipe return, set up vfd and for non-blocking prepare the
   * read ready and read timeout actions.
   *
   * Note that do not at this stage set a timeout value, but do set the return
   * read ready, to proceed asynchronously.
   */
  if (pr_fd >= 0)
    {
      vf->pr_vfd   = vio_vfd_new(pr_fd, vfd_pipe, iot, vf) ;
      vf->pr_state = vf_open ;

      qassert(vf->pr_timeout == 0) ;

      if (!vf->blocking)
        {
          vio_vfd_set_read_action(vf->pr_vfd, vty_pipe_return_ready) ;
          vio_vfd_set_read_timeout_action(vf->pr_vfd, vty_pipe_return_timeout) ;

          vio_vfd_set_read(vf->pr_vfd, on, vf->pr_timeout) ;
        } ;
    } ;

  /* Set up vfd for pipe stderr return  and for non-blocking prepare the
   * read ready and read timeout actions.
   *
   * Note that do not at this stage set a timeout value, or set the return
   * read ready.
   */
  vf->ps_vfd   = vio_vfd_new(ps_fd, vfd_pipe, iot, vf) ;
  vf->ps_state = vf_open ;

  qassert(vf->ps_timeout == 0) ;

  if (!vf->blocking)
    {
      vio_vfd_set_read_action(vf->ps_vfd, vty_pipe_stderr_return_ready) ;
      vio_vfd_set_read_timeout_action(vf->ps_vfd, vty_pipe_stderr_return_timeout) ;

      vio_vfd_set_read(vf->ps_vfd, on, vf->ps_timeout) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Fork fork fork
 *
 * Set up pipes according to type of fork:
 *
 *   in_pipe    -- input from child's stdout as main fd
 *                 input from child's stderr as stderr return fd
 *
 *   out_pipe   -- output to  child's stdin  as main fd
 *                 input from child's stdout as return fd
 *                 input from child's stderr as stderr return fd
 *
 *   err_pipe   -- nothing for main fd
 *                 input from child's stdout as return fd
 *                 input from child's stderr as stderr return fd
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
uty_pipe_fork(vty_io vio, const char* cmd_str, pipe_set pipes, pipe_type_t type)
{
  pid_t child ;
  int   id ;

  /* Set pipes empty                                                    */
  for (id = 0 ; id < pipe_count ; ++id)
    {
      pipes[id][in_fd]  = -1 ;
      pipes[id][out_fd] = -1 ;
    } ;

  /* Open as many pipes as are required.                                */
  if (type == vin_pipe)
    if (!uty_pipe_pair(vio, cmd_str, "input pipe", pipes, in_pipe, in_fd))
      return -1 ;

  if ((type == vout_pipe) || (type == vout_sh_cmd))
    if (!uty_pipe_pair(vio, cmd_str, "return pipe", pipes, in_pipe, in_fd))
      return -1 ;

  if (type == vout_pipe)
    if (!uty_pipe_pair(vio, cmd_str, "output pipe", pipes, out_pipe, out_fd))
    return -1 ;

  if (!uty_pipe_pair(vio, cmd_str, "stderr pipe", pipes, err_pipe, in_fd))
    return -1 ;

  /* Off to the races                                                   */

  child = vfork() ;

  if      (child == 0)          /* In child                             */
    {
      /* Prepare all file descriptors and then execute          */
      if (uty_pipe_exec_prepare(vio, pipes))
        execl("/bin/bash", "bash", "-c", cmd_str, NULL) ; /* does not return */
      else
        exit(0x80 | errno) ;
    }
  else if (child > 0)           /* In parent -- success                 */
    {
      /* close the pipe fds we do not need                      */
      uty_pipe_close_half_pipe(pipes[in_pipe],  out_fd) ;
      uty_pipe_close_half_pipe(pipes[out_pipe], in_fd) ;
      uty_pipe_close_half_pipe(pipes[err_pipe], out_fd) ;
    }
  else if (child < 0)           /* In parent -- failed                  */
    uty_pipe_fork_fail(vio, cmd_str, "vfork", pipes, NULL, "child") ;

  return child ;
} ;

/*------------------------------------------------------------------------------
 * Open a pipe pair -- generate suitable error message if failed and close
 * any earlier pipes that have been opened.
 *
 * Returns:  true <=> success
 */
static bool
uty_pipe_pair(vty_io vio, const char* cmd_str,
                          const char* what, pipe_set    pipes,
                                            pipe_id_t   id,
                                            pipe_half_t half)
{
  pipe_pair pair ;

  pair = pipes[id] ;

  if (pipe(pair) < 0)
    return uty_pipe_fork_fail(vio, cmd_str, what, pipes, pair, "open") ;

  if (set_nonblocking(pair[half]) < 0)
    return uty_pipe_fork_fail(vio, cmd_str, what, pipes, NULL,
                                                      "set non-blocking for") ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Failed to open pipe: generate suitable error message and close any earlier
 * pipes that have been opened.
 *
 * Returns:  false
 */
static bool
uty_pipe_fork_fail(vty_io vio, const char* cmd_str,
                                           const char* what, pipe_set pipes,
                                           pipe_pair   pair,
                                           const char* action)
{
  int   err = errno ;
  int   id ;

  /* Close anything that has been opened                                */
  for (id = 0 ; id < pipe_count ; ++id)
    {
      if (pipes[id] == pair)
        continue ;              /* ignore if just failed to open        */

      uty_pipe_close_half_pipe(pipes[id], in_fd) ;
      uty_pipe_close_half_pipe(pipes[id], out_fd) ;
    } ;

  uty_out(vio, "%% Failed to %s %s for %s\n", action, what, cmd_str) ;

  errno = err ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Close half of a pipe pair, if it is open.
 */
static void
uty_pipe_close_half_pipe(pipe_pair pair, pipe_half_t half)
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
 * are named wrt to the parent process:
 *
 *   in_pipe  -- if present, is: stdout for the child
 *   out_pipe -- if present, is: stdin  for the child
 *   err_pipe -- if present, is: stderr for the child
 *                          and: stdout for the child, if no in_pipe
 *
 * Set current directory.
 *
 * Reset all signals to default state and unmask everything.
 *
 * Returns:  true <=> good to go
 *          false  => some sort of error -- see errno
 */
static bool
uty_pipe_exec_prepare(vty_io vio, pipe_set pipes)
{
  int   std[stds] ;
  int   fd ;

  /* Assign fds to child's std[]                                        */

  std[stdin_fd]  = pipes[out_pipe][in_fd] ;     /* stdin for child      */
  std[stdout_fd] = pipes[in_pipe][out_fd] ;     /* stdout for child     */
  std[stderr_fd] = pipes[err_pipe][out_fd] ;    /* stderr for child     */

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
 * Command line fetch from a pipe -- VIN_PIPE in vf_open state.
 *
 * Before attempting to fetch command line, will shovel any return into the
 * slave and push.  This means that any return is output between command lines.
 *
 * In "non-blocking" state, on CMD_WAITING may be waiting for read ready on
 * this vf, or (indirectly) write ready on the slave.
 *
 * In "blocking" state, will not return until have something, or there is
 * nothing to be had, or times out.
 *
 * Returns:  CMD_SUCCESS    -- have another command line ready to go
 *           CMD_WAITING    -- waiting for input not output  <=> non-blocking
 *           CMD_EOF        -- ran into EOF -- on input
 *           CMD_IO_ERROR   -- ran into an I/O error or time-out
 *
 * This can be called in any thread.
 *
 * NB: the vin_state is set to vf_end when CMD_EOF is returned, and this
 *     code may not be called again.
 *
 *     Signals CMD_EOF on the main input.  If this occurs before EOF on the
 *     return input, any remaining return input must be dealt with before
 *     the vf is finally closed -- see uty_pipe_read_close().
 *
 * NB: the vout_state is set to vf_end when CMD_IO_ERROR is returned, and
 *     this code may not be called again.
 *
 *     When an error occurs it is signalled to the command loop.  This function
 *     is called from the command loop -- so, in fact, the CMD_IO_ERROR
 *     return code does the trick.
 */
extern cmd_return_code_t
uty_pipe_fetch_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert(vf->vin_type == VIN_PIPE) ;
  qassert(vf->vin_state == vf_open) ;

  while (1)             /* so blocking stuff can loop round     */
    {
      cmd_return_code_t ret ;
      int     get ;
      qps_mini_t qm ;

      /* Try to complete line straight from the buffer.
       *
       * If buffer is empty and have seen eof on the input, signal CMD_EOF.
       */
      ret = uty_fifo_command_line(vf, action) ;

      if (ret != CMD_WAITING)
        return ret ;

      /* If blocking, worry about the stderr return -- just to keep I/O moving.
       *
       * Expect only CMD_SUCCESS or CMD_IO_ERROR.
       */
      if (vf->blocking && (vf->ps_state == vf_open))
        {
          ret = uty_pipe_stderr_suck(vf, false) ;     /* not final */

          qassert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)) ;

          if (ret != CMD_SUCCESS)
            return ret ;        /* cannot continue      */
        } ;

      /* Need more from the main input.
       */
      get = vio_fifo_read_nb(vf->ibuf, vio_vfd_fd(vf->vfd), 100) ;

      if (get > 0)
        continue ;              /* loop back                            */

      if (get == -1)
        return uty_vf_error(vf, verr_io_vin, errno) ;

      if (get == -2)            /* EOF met immediately                  */
        {
          vf->vin_state = vf_end ;
          continue ;            /* loop back -- deals with possible
                                   final line and the return.           */
        } ;

      qassert(get == 0) ;

      /* We get here if main input is not yet at eof, but has nothing
       * to read at the moment.
       */
      qassert(vf->vin_state == vf_open) ;

      if (!vf->blocking)
        {
          uty_vf_set_read(vf, on) ;
          return CMD_WAITING ;
        } ;

      /* Implement blocking I/O, with timeout
       *
       * Note that waits for both the main vin and the pipe return.
       */
      qps_mini_set(qm, (vf->vin_state == vf_open) ? vio_vfd_fd(vf->vfd)
                                                  : -1,
                                             qps_read_mnum, vf->read_timeout) ;
      if (vf->pr_state == vf_open)
        qps_mini_add(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum) ;

      if (qps_mini_wait(qm, NULL, false) != 0)
        continue ;                      /* loop back            */

      return uty_vf_error(vf, verr_to_vin, 0) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to a pipe and (for blocking) shovel return into
 * slave and push.
 *
 * For blocking, does not push to the pipe while there is return input to be
 * read and pushed to the slave.  For non-blocking the return input/output is
 * handled autonomously.
 *
 * If final, will do final shovel from return to slave and also attempt to
 * empty any output buffer -- will not wait or block, and ignores errors.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * In "non-blocking" state, on CMD_WAITING the slave output will put the
 * master return pr_vfd into read ready state when it sees write ready, or will
 * here set the pr_vfd into read ready state.  This requires no further action
 * from the caller, the background pselect process will complete the output and
 *  may signal the result via uty_cmd_signal().
 *
 * In "blocking" state, will not return until have written everything there is,
 * away, or cannot continue.
 *
 * This can be called in any thread.
 */
extern cmd_return_code_t
uty_pipe_out_push(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  qassert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD)) ;

  if (vf->vout_state != vf_open)
    return CMD_SUCCESS ;        /* Get out if going nowhere     */

  /* If blocking, keep the stderr return moving.
   */
  if (vf->blocking && (vf->ps_state == vf_open))
    {
      cmd_return_code_t ret ;

      ret = uty_pipe_stderr_suck(vf, false) ; /* not final    */

      qassert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)) ;

      if (ret != CMD_SUCCESS)
        return ret ;        /* cannot continue      */
    } ;

  /* If squelching, dump anything we have in the obuf.
   *
   * Otherwise, write contents away.
   */
  if (vf->vio->cancel)
    vio_fifo_clear(vf->obuf, false) ;   /* keep end mark        */
  else
    {
      while (1)
        {
          qps_mini_t qm ;
          int put ;

          /* If blocking, see if there is anything in the return, and shovel.
           *
           * For non-blocking, the pselect process looks after this.
           */
          if (vf->blocking && (vf->pr_state == vf_open))
            {
              cmd_return_code_t ret ;

              ret = uty_pipe_shovel(vf, final) ;

              if (ret != CMD_SUCCESS)
                return ret ;                    /* cannot continue      */
            } ;

          if (vf->vout_state != vf_open)
            break ;     /* Quit if error has stopped the main vout      */

          /* Now write to the main vout, blocking if required.
           */
          put = vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), true) ;

          if (put == 0)                 /* all gone                     */
            break ;

          if (put < 0)
            return uty_vf_error(vf, verr_io_vout, errno) ;

          /* Cannot write everything away without waiting
           */
          if (!vf->blocking)
            {
              if (!final)
                uty_vf_set_write(vf, on) ;
              return CMD_WAITING ;
            } ;

          /* Implement blocking I/O, with timeout
           *
           * Note that waits for both the main vout and the pipe return.
           */
          if (final)
            return CMD_WAITING ;

          qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_write_mnum,
                                                           vf->write_timeout) ;
          if (vf->pr_state == vf_open)
            qps_mini_add(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum) ;

          if (qps_mini_wait(qm, NULL, false) != 0)
            continue ;                  /* Loop back                    */

          return uty_vf_error(vf, verr_to_vout, 0) ;
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Shovel from pipe return to slave, pushing the slave as we go.
 *
 * While the main vout is open, this function is used:
 *
 *   * VOUT_PIPE:   for blocking vf, this is called each time the output is
 *                  pushed -- to keep any returned stuff moving.  Does not
 *                  block reading the return, but may block writing to the
 *                  slave.
 *
 *                  for non-blocking vf, this is called by the pselect
 *                  process, which keeps the return moving autonomously,
 *                  or may be called "final".
 *
 *                  Note that pr_timeout == 0 while the main vout is open.
 *
 *   * VOUT_SH_CMD: the main vin/vout is closed from the get go.
 *
 *                  All pipe return is handled by the close process.
 *
 * When the main vin/vout is closed:
 *
 *   * VOUT_PIPE & VOUT_SH_CMD:
 *
 *                  for blocking vf, this is called by the close function,
 *                  to suck up any remaining return, and push it to the slave.
 *                  May block reading the return and/or the slave.
 *
 *                  for non-blocking vf, this is called by the pselect
 *                  process, which keeps the return moving autonomously, or
 *                  may be called "final".
 *
 *                  Note that pr_timeout != 0 once the main vout is closed.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * NB: if runs into I/O error or time-out on the slave output, then sets
 *     vf_end on the TODO
 *
 * This can be called in any thread.
 */
static cmd_return_code_t
uty_pipe_shovel(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  /* The pipe return MUST still be open, but may be cancelling all I/O
   * or the slave may already be in error or otherwise not open.
   */
  qassert(vf->pr_state == vf_open) ;
  qassert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD)) ;

  if ((vf->vio->cancel) || (vf->vout_next->vout_state != vf_open))
    return CMD_SUCCESS ;

  /* Suck and blow -- may block in uty_cmd_out_push()
   *
   * Note that we do not push the slave if there is nothing new from the
   * return -- there shouldn't be anything pending in the slave obuf.
   */
  while (1)
    {
      cmd_return_code_t ret ;
      int get ;

      get = vio_fifo_read_nb(vf->vout_next->obuf, vio_vfd_fd(vf->pr_vfd), 10) ;

      if (get == 0)                     /* Nothing there, but not EOF   */
        {
          qps_mini_t qm ;

          /* Nothing there to read.
           *
           * Returns if not blocking, if no timeout is set or if is "final".
           *
           * NB: before blocking on the pipe return, poll the pipe stderr
           *     return to keep that moving -- so child cannot stall trying
           *     to output to its stderr !
           */
          if (!vf->blocking || (vf->pr_timeout == 0) || final)
            break ;                     /* do not block reading         */

          if (vf->ps_state == vf_open)
            {
              ret = uty_pipe_stderr_suck(vf, false) ;   /* not final    */

              qassert((ret == CMD_SUCCESS) || (ret == CMD_IO_ERROR)) ;

              if (ret != CMD_SUCCESS)
                return ret ;            /* cannot continue              */
            } ;

          qps_mini_set(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum,
                                                               vf->pr_timeout) ;
          if (qps_mini_wait(qm, NULL, false) != 0)
            continue ;                  /* loop back                    */

          return uty_vf_error(vf, verr_to_pr, 0) ;      /* CMD_IO_ERROR */
        }

      else if (get >  0)                /* Read something               */
        {
          ret = uty_cmd_out_push(vf->vout_next, final) ; /* may block   */

          if (ret == CMD_SUCCESS)
            continue ;                  /* Loop back if emptied buffer  */

          if (ret == CMD_IO_ERROR)
            uty_pipe_return_stop(vf) ;
                                        /* No point continuing          */
          else
            {
              /* Is CMD_WAITING on the slave output.
               *
               * If we are "final":
               *
               *   for blocking vf that means would have blocked, but didn't.
               *
               *   for non-blocking vf, that means could not empty the buffer.
               *
               * If not "final":
               *
               *   cannot be a blocking vf !
               *
               *   for non-blocking vf, the output buffer will be serviced,
               *   in due course and can leave it up to the pselect() process
               *   to read anything more -- no need to do so now.
               *
               * ...in all cases don't want to try to input or output any more,
               * so give up and return CMD_WAITING.
               */
              qassert(ret == CMD_WAITING) ;
            } ;

          return ret ;                  /* CMD_WAITING or CMD_IO_ERROR  */
        }

      else if (get == -1)               /* Hit error                    */
        {
          return uty_vf_error(vf, verr_io_pr, errno) ;  /* CMD_IO_ERROR */
        }

      else
        {
          assert (get == -2) ;          /* eof on the return            */

          vf->pr_state = vf_end ;
          break ;                       /* quit: OK                     */
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Suck the pipe stderr return to vf->ps_buf.
 *
 * While the main vin/vout is open or the pipe return is open, this function
 * is used:
 *
 *   for blocking vf, this is called each time the output is pushed, or the
 *   input is read, or the pipe return is read -- to keep any returned stuff
 *   moving.  Does not block reading the stderr return.
 *
 *   for non-blocking vf, this is called by the pselect which keeps the stderr
 *   return moving autonomously.
 *
 *   Note that ps_timeout == 0 under these conditions.
 *
 * When the main vin/vout and the pipe return are closed:
 *
 *   for blocking vf, this is called by the close function, to suck up any
 *   remaining stderr return, which may block.
 *
 *   for non-blocking vf, this is called by the pselect process, which keeps
 *   the stderr return moving autonomously, or may be called "final".
 *
 *   Note that ps_timeout != 0 under these conditions.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * This can be called in any thread.
 */
static cmd_return_code_t
uty_pipe_stderr_suck(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  /* The pipe return MUST still be open, but may be cancelling all I/O
   * or the slave may already be in error or otherwise not open.
   */
  qassert(vf->ps_state == vf_open) ;

  /* Suck
   */
  while (1)
    {
      int get ;

      if (vf->ps_buf == NULL)
        {
          char buf[100] ;
          get = read_nb(vio_vfd_fd(vf->ps_vfd), buf, sizeof(buf)) ;

          if (get > 0)
            vio_fifo_put_bytes(uty_pipe_ps_buf(vf), buf, get) ;
        }
      else
        get = vio_fifo_read_nb(vf->ps_buf, vio_vfd_fd(vf->ps_vfd), 10) ;

      if (get == 0)                     /* Nothing there, but not EOF   */
        {
          qps_mini_t qm ;

          /* Nothing there to read.
           *
           * Returns if not blocking, if no timeout is set or if is "final".
           *
           * NB: before blocking on the pipe return, poll the pipe stderr
           *     return to keep that moving -- so child cannot stall trying
           *     to output to its stderr !
           */
          if (!vf->blocking || (vf->ps_timeout == 0) || final)
            break ;                     /* do not block reading         */

          qps_mini_set(qm, vio_vfd_fd(vf->ps_vfd), qps_read_mnum,
                                                               vf->ps_timeout) ;
          if (qps_mini_wait(qm, NULL, false) != 0)
            continue ;                  /* loop back                    */

          return uty_vf_error(vf, verr_to_ps, 0) ;      /* CMD_IO_ERROR */
        }

      else if (get >  0)                /* Read something               */
        {
          continue ;
        }

      else if (get == -1)               /* Hit error on stderr return   */
        {
          return uty_vf_error(vf, verr_io_ps, errno) ;  /* CMD_IO_ERROR */
        }

      else
        {
          assert (get == -2) ;          /* eof on the stderr return     */

          vf->ps_state = vf_end ;
          break ;                       /* quit: OK                     */
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Pipe is ready to read -- call-back for VIN_PIPE.
 *
 * This is used if the VIN_PIPE is non-blocking.
 *
 * Signals command loop, so may continue if was waiting.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_pipe_read_ready(vio_vfd vfd, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  /* If the vin is no longer vf_open then read ready should have been turned
   * off -- but kicking the command loop will not hurt.
   */
  uty_cmd_signal(vf->vio, CMD_SUCCESS) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe has timed out, waiting to read -- call-back for VIN_PIPE.
 *
 * This is used if the VIN_PIPE is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_pipe_read_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->read_timer == timer) ;

  uty_vf_error(vf, verr_to_vin, 0) ;   /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Pipe return is ready to read -- call-back for VOUT_PIPE and VOUT_SH_CMD.
 *
 * This is used if the VOUT_PIPE/VOUT_SH_CMD is non-blocking.
 *
 * Shovels any available return input to the output.  If required, the shoveller
 * will set read ready when runs out of input.
 *
 * Signals to the command loop iff hits eof or an error or time-out.  (Also
 * signals to the command loop if is not open... just in case command loop is
 * waiting.)
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_pipe_return_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->pr_vfd == vfd) ;

  /* If the pipe return is no longer open then read ready should have been
   * turned off -- but kicking the command loop will not hurt.
   *
   * Note that uty_pipe_shovel() returns CMD_WAITING, unless hits eof
   * (CMD_SUCCESS) or gets an I/O error or timeout (CMD_IO_ERROR).
   */
  if (vf->pr_state == vf_open)
    ret = uty_pipe_shovel(vf, false) ;  /* not final                    */
  else
    ret = CMD_SUCCESS ;

  if (vf->pr_state == vf_open)
    vio_vfd_set_read(vf->pr_vfd, on, vf->pr_timeout) ;
  else if (vf->ps_state != vf_open)
    uty_cmd_signal(vf->vio, ret) ;      /* CMD_SUCCESS or CMD_IO_ERROR  */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe stderr return is ready to read -- call-back for VIN_PIPE, VOUT_PIPE and
 * VOUT_SH_CMD.
 *
 * This is used if the VIN_PIPE/VOUT_PIPE/VOUT_SH_CMD is non-blocking.
 *
 * Shovels any available return input to the output.  If required, the shoveller
 * will set read ready when runs out of input.
 *
 * Signals to the command loop iff hits eof or an error or time-out.  (Also
 * signals to the command loop if is not open... just in case command loop is
 * waiting.)
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_pipe_stderr_return_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->ps_vfd == vfd) ;

  /* If the pipe stderr return is no longer open then read ready should have
   * been turned off -- but kicking the command loop will not hurt.
   *
   * Note that uty_pipe_shovel() returns CMD_WAITING, unless hits eof
   * (CMD_SUCCESS) or gets an I/O error or timeout (CMD_IO_ERROR).
   */
  if (vf->ps_state == vf_open)
    ret = uty_pipe_stderr_suck(vf, false) ;     /* not final    */
  else
    ret = CMD_SUCCESS ;

  if (vf->ps_state == vf_open)
    vio_vfd_set_read(vf->ps_vfd, on, vf->ps_timeout) ;
  else if (vf->pr_state != vf_open)
    uty_cmd_signal(vf->vio, ret) ;      /* CMD_SUCCESS or CMD_IO_ERROR  */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe return has timed out, waiting to read -- call-back for VIN_PIPE,
 * VOUT_PIPE and VOUT_SH_CMD.
 *
 * This is used if the VIN_PIPE/VOUT_PIPE/VOUT_SH_CMD is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_pipe_return_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->pr_vfd->read_timer == timer) ;

  if (vf->ps_state == vf_open)
    vio_vfd_set_read(vf->ps_vfd, off, 0) ;

  uty_vf_error(vf, verr_to_ps, 0) ;     /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Pipe return has timed out, waiting to read -- call-back for VIN_PIPE,
 * VOUT_PIPE and VOUT_SH_CMD.
 *
 * This is used if the VIN_PIPE/VOUT_PIPE/VOUT_SH_CMD is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_pipe_stderr_return_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->ps_vfd->read_timer == timer) ;

  if (vf->pr_state == vf_open)
    vio_vfd_set_read(vf->pr_vfd, off, 0) ;

  uty_vf_error(vf, verr_to_pr, 0) ;     /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Pipe is ready to write -- call-back for VOUT_PIPE.
 *
 * This is used if the VOUT_PIPE is non-blocking.
 *
 * Signals command loop, so may continue if was waiting.
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_pipe_write_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  /* If the vout is no longer vf_open then write ready should have been turned
   * off -- but kicking the command loop will not hurt.
   */
  if (vf->vout_state == vf_open)
    {
      ret = uty_pipe_out_push(vf, false) ;  /* not final    */
    }
  else
    ret = CMD_SUCCESS ;

  if (ret != CMD_WAITING)
    uty_cmd_signal(vf->vio, ret) ;      /* CMD_SUCCESS or CMD_IO_ERROR  */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe has timed out, waiting to write -- call-back for VOUT_PIPE.
 *
 * This is used if the VOUT_PIPE is non-blocking.
 *
 * Signals a timeout error to the command loop.
 */
static vty_timer_time
vty_pipe_write_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->write_timer == timer) ;

  uty_vf_error(vf, verr_to_vout, 0) ;  /* signals command loop */

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Complete the close of a VIN_PIPE after the vfd has been read closed.
 *
 * Nothing needs to be done with the main input, but must close the return,
 * collect the child and release the slave.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible, the return is done with
 *                            (and the vfd closed) and the child has been
 *                            collected (or is overdue).
 *           CMD_WAITING   -- not "final" => waiting for return I/O to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * NB: if "final", whatever the return code, the pipe return is closed and the
 *     child dismissed.
 *
 * If returns CMD_WAITING (and not "final") then the command loop will be
 * signalled when it is time to call this function again to progress the
 * close operation.
 */
extern cmd_return_code_t
uty_pipe_read_close(vio_vf vf, bool final)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vin_type == VIN_PIPE) ;
  qassert(vf->vin_state == vf_end) ;

  return uty_pipe_return_close(vf, final) ;
} ;

/*------------------------------------------------------------------------------
 * Close VOUT_PIPE or VOUT_SH_CMD.
 *
 * For not-final, wish to flush output buffer.  If final will attempt to empty
 * the output buffer -- but will not wait or block, and ignores errors.
 *
 * Must then close the return, collect the child and release the slave.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible, the vout and the
 *                            return are done with (and the vfds closed) and
 *                            the child has been collected (or is overdue).
 *           CMD_WAITING   -- not "final" => waiting for output or return I/O
 *                                           to complete    <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * NB: if "final", whatever the return code, the pipe return is closed and the
 *     child dismissed.
 *
 * If returns CMD_WAITING (and not "final") then the command loop will be
 * signalled when it is time to call this function again to progress the
 * close operation.
 */
extern cmd_return_code_t
uty_pipe_write_close(vio_vf vf, bool final)
{
  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  qassert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SH_CMD)) ;

  /* If the main vfd is still there, keep pushing (which, for blocking vf,
   * will keep shovelling from pipe return to slave).
   */
  if (vf->vout_state == vf_open)
    {
      cmd_return_code_t ret ;

      ret = uty_pipe_out_push(vf, final) ;

      if ((ret == CMD_WAITING) && !final)
        return ret ;

      vf->vout_state = vf_end ;         /* no further output    */
    } ;

  /* Now need to close the output vfd to signal to the child that we
   * are done -- note that closing an already closed vfd does nothing.
   */
  vf->vfd = vio_vfd_close(vf->vfd) ;

  /* If the return is still open, try to empty and close that.          */
  return uty_pipe_return_close(vf, final) ;
} ;

/*------------------------------------------------------------------------------
 * Close the return on a VIN_PIPE, VOUT_PIPE or a VOUT_SH_CMD.
 *
 * This is the second stage of closing a pipe, after the vin or the main vout
 * has reached vf_end, so all main I/O is complete (or failed).
 *
 * If returns CMD_WAITING from here, may be called again, any number of times,
 * to attempt to complete the process.
 *
 * If "final" then will close after making efforts to complete I/O and collect
 * child, short of blocking or waiting.
 *
 * Phase 1: if the return is vf_open:
 *
 *   Set the read time out for the return (want any remaining return stuff, and
 *   the eof) which (for non-blocking) indicates is now prepared to block.
 *
 *   If non-blocking:
 *
 *     if not "final", set the read ready timeout (if not already set), and
 *     leave CMD_WAITING.
 *
 *     if "final", kill the read ready stuff, and shovel once "final", just
 *     in case there is stuff there.
 *
 *   If blocking:
 *
 *     Shovel stuff from return to slave output.  If not "final", may block and
 *     may time-out.  If "final" will not block, but may return CMD_WAITING.
 *
 *   Once return is all dealt with (or fails) then close the vfd and set the
 *   pipe return vf_closed.
 *
 * Phase 2: if the stderr return is vf_open:
 *
 *   Set the read time out for the stderr return (want any remaining stderr
 *   return stuff, and the eof)  which (for non-blocking) indicates is now
 *   prepared to block.
 *
 *   Suck stderr return.  If "final", do not block or  wait for either input or
 *   output.  If not final, may block and may time-out or may return
 *   CMD_WAITING.
 *
 *   Once return is all dealt with (or fails) then close the vfd and set the
 *   pipe return vf_closed.
 *
 *   If non-blocking:
 *
 *     if not "final", set the read ready timeout (if not already set), and
 *     leave CMD_WAITING.
 *
 *     if "final", kill the read ready stuff, and shovel once "final", just
 *     in case there is stuff there.
 *
 *   If blocking:
 *
 *     Shovel stuff from return to slave output.  If not "final", may block and
 *     may time-out.  If "final" will not block, but may return CMD_WAITING.
 *
 *   Once return is all dealt with (or fails) then close the vfd and set the
 *   pipe return vf_closed.
 *
 * Phase 3: collect the child return code and deal with it:
 *
 *   If the child has yet to be collected:
 *
 *     if "final", collect child immediately or set overdue.
 *
 *     if not "final":
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
 * Phase 4: transfer the pipe stderr return to the main vio pipe stderr return.
 *
 *   This leaves it up to the vout_base to deal with the pipe stderr return,
 *   in its own time -- e.g when the output stack closes.
 *
 * When this (eventually) returns CMD_SUCCESS, all pipe return has been
 * sucked up and has cleared the slave vout buffer, and the child has been
 * collected -- unless something has gone wrong, and part of that has been
 * short-circuited.
 *
 * Returns:  CMD_SUCCESS   -- closed and child collected
 *           CMD_WAITING   -- not "final" => waiting for output to complete
 *                                                          <=> not vf->blocking
 *                                "final" => would have waited *or* blocked,
 *                                           but did not.
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * NB: if "final", whatever the return code, the pipe return is closed and the
 *     child dismissed.
 */
static cmd_return_code_t
uty_pipe_return_close(vio_vf vf, bool final)
{
  vty_io vio ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  vio = vf->vio ;

  /* Phase 1: if return is still open, try to empty it -- subject to "final".
   *          Sets timeout and will now block.
   *
   *          If not blocking but "final", turn off read ready and any timeout,
   *          and shovel one last time.
   *
   *          When return is all done, close it (which the child may see) and
   *          mark it vf_closed.
   */
  if (vf->pr_state == vf_open)
    {
      cmd_return_code_t ret ;
      bool set_timeout ;

      set_timeout = (vf->pr_timeout == 0) ;
      vf->pr_timeout = pipe_timeout ;

      if (!vf->blocking)
        {
          if (!final)
            {
              if (set_timeout)
                vio_vfd_set_read(vf->pr_vfd, on, vf->pr_timeout) ;

              return CMD_WAITING ;      /* in hands of pselect()        */
            } ;

          vio_vfd_set_read(vf->pr_vfd, off, 0) ;
        } ;

      ret = uty_pipe_shovel(vf, final) ;

      if ((ret == CMD_WAITING) && !final)
        return ret ;
    } ;

  if (vf->pr_state != vf_closed)
    {
      vf->pr_vfd   = vio_vfd_close(vf->pr_vfd) ;
      vf->pr_state = vf_closed ;
    } ;

  /* Phase 2: if stderr return is still open, try to empty it -- subject to
   *          "final".  Sets timeout and will now block.
   *
   *          If not blocking but "final", turn off read ready and any timeout,
   *          and suck one last time.
   *
   *          When return is all done, close it (which the child may see) and
   *          mark it vf_closed.
   */
  if (vf->ps_state == vf_open)
    {
      cmd_return_code_t ret ;
      bool set_timeout ;

      set_timeout = (vf->ps_timeout == 0) ;
      vf->ps_timeout = pipe_timeout ;

      if (!vf->blocking)
        {
          if (!final)
            {
              if (set_timeout)
                vio_vfd_set_read(vf->ps_vfd, on, vf->pr_timeout) ;

              return CMD_WAITING ;      /* in hands of pselect()        */
            } ;

          vio_vfd_set_read(vf->pr_vfd, off, 0) ;
        } ;

      ret = uty_pipe_stderr_suck(vf, final) ;

      if ((ret == CMD_WAITING) && !final)
        return ret ;
    } ;

  if (vf->ps_state != vf_closed)
    {
      vf->ps_vfd   = vio_vfd_close(vf->ps_vfd) ;
      vf->ps_state = vf_closed ;
    } ;

  /* Phase 3: if not already collected, collect the child
   *
   * When does collect, or times out, will dismiss the child and set vf->child
   * to NULL -- which indicates that the child is done with.
   *
   * Note that may write diagnostic message to slave, so in last phase we
   * push the slave output to clear that out before releasing the slave.
   */
  if (vf->child != NULL)
    {
      /* Collect -- blocking or not blocking                                */
      if (!vf->child->collected && !vf->child->overdue)
       {
          /* If we are blocking or "final" or vio->cancel, try to collect the
           * child here and now -- if not final or vio->cancel may block here.
           *
           * For non-blocking and not "final", leave the child collection up to
           * the SIGCHLD system.
           */
          if (vf->blocking || final || vf->vio->cancel)
            {
              uty_child_collect(vf->child, child_timeout,
                                                     final || vf->vio->cancel) ;
            }
          else
            {
              uty_child_awaited(vf->child, child_timeout) ;
              return CMD_WAITING ;
            } ;
       } ;

      /* If child is overdue, or did not terminate cleanly, write message to
       * the pipe stderr return.
       */
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

      /* Can now dismiss the child -- if not collected, is left on the register.
       */
      uty_child_dismiss(vf->child, false) ;     /* not curtains         */
    } ;

  /* Phase 4: if there is anything in the pipe stderr return buffer, finish
   * it, and transfer to the vio->ps_buf (unless vio->cancel).
   */
  if (vf->ps_buf != NULL)
    {
      vio_fifo_trim(vf->ps_buf, true) ;         /* trim trailing whitespace
                                                 * and '\n' terminate   */
      if (!vio_fifo_empty(vf->ps_buf) && !vio->cancel)
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

      vf->ps_buf = vio_fifo_free(vf->ps_buf) ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Stop pipe return and/or pipe stderr return.
 *
 * Then if either vin and/or vout is open, set to vf_end to terminate I/O.
 *
 * There is no point continuing with anything once either return is broken.
 */
extern void
uty_pipe_return_stop(vio_vf vf)
{
  if (vf->pr_state == vf_open)
    vf->pr_state = vf_end ;

  if (vf->ps_state == vf_open)
    vf->ps_state = vf_end ;

  if (vf->vin_state == vf_open)
    vf->vin_state = vf_end ;

  if (vf->vout_state == vf_open)
    vf->vout_state = vf_end ;
} ;

/*------------------------------------------------------------------------------
 * Cancel any further input from the pipe return.
 *
 * Note that does not touch anything buffered ready to be output in the
 * slave -- that must be dealt with separately.
 */
extern void
uty_pipe_return_cancel(vio_vf vf)
{
  qassert( (vf->vin_type  == VIN_PIPE) || (vf->vout_type == VOUT_PIPE)
                                       || (vf->vout_type == VOUT_SH_CMD) ) ;
  qassert(vf->pr_state == vf_open) ;

  vf->pr_state = vf_end ;
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

/*==============================================================================
 * stdout and stderr
 *
 *
 */


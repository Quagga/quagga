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

/*==============================================================================
 * VTY Configuration file I/O -- VIN_CONFIG and VOUT_CONFIG.
 *
 *
 *
 */

/*------------------------------------------------------------------------------
 * Set up VTY on which to read configuration file -- using already open fd.
 *
 * Sets TODO
 *
 * NB: sets up a blocking vio -- so the vin_base and vout_base will "block"
 *     (using local pselect() as required), as will any further vin/vout
 *     opened for this vio.
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
   * NB: don't need to specify vfd_io_blocking, because the vio is set blocking.
   */
  vf = uty_vf_new(vio, name, fd, vfd_file, vfd_io_read) ;

  uty_vin_push( vio, vf, VIN_CONFIG, NULL, NULL, 64 * 1024) ;
  uty_vout_push(vio, vf, VOUT_STDERR, NULL, NULL, 4 * 1024) ;

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

  uty_child_signal_nexus_clear(vf->vio) ;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * VTY File I/O -- VIN_FILE and VOUT_FILE
 *
 * This is for input and output of configuration files and piped stuff.
 *
 * When reading the configuration (and piped stuff in the configuration) I/O
 * is blocking... nothing else can run while this is going on.  Otherwise,
 * all I/O is non-blocking.  The actual I/O is non-blocking, the "blocking"
 * I/O is manufactured using a mini-pselect, so can time-out file writing
 * before too long.
 */
static void uty_file_read_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_file_read_timeout(vio_timer timer,
                                                            void* action_info) ;

static void uty_file_write_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_file_write_timeout(vio_timer timer,
                                                            void* action_info) ;

static cmd_return_code_t uty_fifo_command_line(vio_vf vf, cmd_action action) ;

/*------------------------------------------------------------------------------
 * Open file for input, to be read as commands.
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

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  /* Now is the time to complete the name, if required.                 */

  path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
  pns  = qpath_string(path) ;

  /* Do the basic file open.                                            */
  iot = vfd_io_read ;

  fd = uty_vfd_file_open(pns, iot) ;

  if (fd < 0)
    {
      uty_out(vio, "%% Could not open input file %s\n", pns) ;

      ret = CMD_WARNING ;      /* TODO add errno to message ?  */
    }
  else
    {
      /* We have a file, so now save context and update dir "here"          */
      uty_vin_new_context(vio, context, path) ;

      /* OK -- now push the new input onto the vin_stack.                   */
      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vin_push(vio, vf, VIN_FILE, uty_file_read_ready,
                                      uty_file_read_timeout, 16 * 1024) ;
      uty_vf_set_read_timeout(vf, 30) ;

      ret = CMD_SUCCESS ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Open file for output.
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open
 *
 * If "blocking" vf, this can be called from any thread, otherwise must be the
 * cli thread -- see uty_vfd_new().
 */
extern cmd_return_code_t
uty_file_write_open(vty_io vio, qstring name, bool append, cmd_context context)
{
  cmd_return_code_t ret ;
  qpath       path ;
  const char* pns ;
  int   fd ;
  vio_vf vf ;
  vfd_io_type_t iot ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  /* Now is the time to complete the name, if required.                 */

  path = uty_cmd_path_name_complete(NULL, qs_string(name), context) ;
  pns  = qpath_string(path) ;

  /* Do the basic file open.                                            */
  iot =  vfd_io_write | (append ? vfd_io_append : 0) ;

  fd = uty_vfd_file_open(pns, iot) ;

  if (fd < 0)
    {
      uty_out(vio, "%% Could not open output file %s\n", pns) ;

      ret = CMD_WARNING ;      /* TODO add errno to message ?  */
    }
  else
    {
      /* OK -- now push the new output onto the vout_stack.                 */
      vf = uty_vf_new(vio, pns, fd, vfd_file, iot) ;
      uty_vout_push(vio, vf, VOUT_FILE, uty_file_write_ready,
                                            uty_file_write_timeout, 16 * 1024) ;
      ret = CMD_SUCCESS ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Command line fetch from a file or pipe.
 *
 * Returns: CMD_SUCCESS    -- have another command line ready to go
 *          CMD_WAITING    -- would not wait for input  <=> non-blocking
 *          CMD_EOF        -- ran into EOF
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- could wait no longer      <=> blocking
 *
 * In "non-blocking" state, on CMD_WAITING sets read ready, which will
 * vty_cmd_signal() the command loop to come round again.
 *
 * In "blocking" state, will not return until have something, or there is
 * nothing to be had, or times out.
 *
 * This can be called in any thread.
 *
 * NB: the vin_state will become vf_eof the first time that CMD_EOF is
 *     returned.
 */
extern cmd_return_code_t
uty_file_fetch_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  assert((vf->vin_type == VIN_FILE) || (vf->vin_type == VIN_CONFIG)) ;
  assert((vf->vin_state == vf_open) || (vf->vin_state == vf_eof)) ;

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
            return CMD_IO_ERROR ;       /* register error               */

          if (get == -2)
            {
              /* Hit end of file set the vf into vf_eof.
               *
               * NB: does not know has hit eof until tries to read and nothing
               *     is returned.
               *
               * Loop back so that uty_fifo_command_line() can reconsider, now
               * that we know that there is no more data to come -- it may
               * signal CMD_SUCCESS (non-empty final line) or CMD_EOF.
               */
              assert(vio_fifo_empty(vf->ibuf)) ;

              vf->vin_state = vf_eof ;

              break ;           /* loop back to uty_fifo_command_line() */
            } ;

          /* Would block -- for non-blocking return CMD_WAITING, for
           * blocking we block here with a timeout, and when there is more
           * to read, loop back to
           */
          assert(get == 0) ;

          if (!vf->blocking)
            {
              uty_vf_set_read(vf, on) ;
              return CMD_WAITING ;
            } ;

          /* Implement blocking I/O, with timeout                       */
          qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_read_mnum,
                                                             vf->read_timeout) ;
          if (qps_mini_wait(qm, NULL, false) == 0)
            return CMD_IO_TIMEOUT ;     /* TODO vf_timeout ??   */

          /* Loop back to vio_fifo_read_nb()                            */
        }
    } ;
} ;

/*------------------------------------------------------------------------------
 * Try to complete a command line for the given vf, from the current input
 * fifo -- performs NO I/O
 *
 * Returns: CMD_SUCCESS   -- have a command line.
 *          CMD_EOF       -- there is no more
 *          CMD_WAITING   -- waiting for more
 *
 * If vf->vin_state == vf_eof, will return CMD_SUCCESS if have last part line
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
      size_t  have ;
      ulen    len ;
      bool    eol ;

      /* Get what we can from the fifo                                  */
      s = vio_fifo_get(vf->ibuf, &have) ;

      /* If fifo is empty, may be last line before eof, eof or waiting  */
      if (have == 0)
        {
          if (vf->vin_state == vf_eof)
            {
              if (qs_len_nn(vf->cl) > 0)
                break ;         /* have non-empty last line     */
              else
                return CMD_EOF ;
            } ;

          return CMD_WAITING ;
        } ;

      assert(vf->vin_state != vf_eof) ; /* not empty => not eof         */

      /* Try to find a '\n' -- converting all other control chars to ' '
       *
       * When we find '\n' step back across any trailing ' ' (which includes
       * any control chars before the '\n').
       *
       * This means that we cope with "\r\n" line terminators.  But not
       * anything more exotic.
       */
      p = s ;
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

      /* Now worry about we have a trailing '\'.                        */

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
 * File or pipe is ready to read -- this is the call-back planted in the
 * vf->vfd.
 *
 * The command_queue processing can continue.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
uty_file_read_ready(vio_vfd vfd, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  uty_cmd_signal(vf->vio, CMD_SUCCESS) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * File or pipe has timed out, waiting to read -- this is the call-back planted
 * in the vf->vfd.
 *
 * ????
 */
static vty_timer_time
uty_file_read_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->write_timer == timer) ;

//cq_continue(vf->vio->vty) ;   TODO

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Command output push to a file.
 *
 * Until the file becomes vf_closing, will not write the end_lump of the fifo,
 * so all output is in units of the fifo size -- which should be "chunky".
 *
 * Although it is unlikely to happen, if blocking, will block if cannot
 * completely write away what is required, or enable write ready.
 *
 * If final, will write as much as possible, but not block and will ignore
 * any errors -- but the return code will be an error return code.
 *
 * Returns: CMD_SUCCESS    -- written everything
 *          CMD_WAITING    -- could not write everything   (<=> non-blocking)
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- not going to wait any longer (<=> blocking)
 *
 * In "non-blocking" state, on CMD_WAITING the caller will have to take steps
 * to come back later.
 *
 * In "blocking" state, will not return until have written everything there is,
 * away, or cannot continue.
 *
 * This can be called in any thread.
 */
extern cmd_return_code_t
uty_file_out_push(vio_vf vf, bool final)
{
  bool all ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                */
  assert((vf->vout_state == vf_open) || (vf->vout_state == vf_closing)) ;
  assert((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)) ;

  all = (vf->vout_state == vf_closing) ;
                                          /* empty buffers if closing   */
  while (1)
    {
      qps_mini_t qm ;
      int n ;

      n = vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), all || final) ;

      if (n < 0)
        return CMD_IO_ERROR ;   /* TODO         */

      if ((n == 0) || final)    /* all gone (or as much as can go)      */
        return CMD_SUCCESS ;

      /* Cannot write everything away without waiting                   */
      if (!vf->blocking)
        {
          uty_vf_set_write(vf, on) ;
          return CMD_WAITING ;
        } ;

      /* Implement blocking I/O, with timeout                           */
      qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_write_mnum, vf->write_timeout) ;
      if (qps_mini_wait(qm, NULL, false) == 0)
        return CMD_IO_TIMEOUT ;

      /* Loop back to vio_fifo_write_nb()                               */
    } ;
} ;

/*------------------------------------------------------------------------------
 * File or pipe is ready to write -- this is the call-back planted in the
 * vf->vfd.
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 *
 * Since we have gone to all the trouble of going through pselect, might as
 * well write everything away.  This works while the file is being closed, too.
 */
static void
uty_file_write_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  ret = uty_file_out_push(vf, false) ;  /* re-enable write ready if required */

  if (ret != CMD_WAITING)
    uty_cmd_signal(vf->vio, ret) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * File or pipe is ready to write -- this is the call-back planted in the
 * vf->vfd.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static vty_timer_time
uty_file_write_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->write_timer == timer) ;

//uty_file_out_push(vf) ;   // TODO ???????

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Tidy up after input file has been closed.
 *
 * Nothing further required -- input comes to a halt.
 */
extern cmd_return_code_t
uty_file_read_close(vio_vf vf, bool final)
{
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Flush output buffer and close.
 *
 * See uty_file_out_push()
 *
 * Returns: CMD_SUCCESS   -- buffers are empty, or final
 *          CMD_WAITING   -- waiting for I/O to complete
 *          CMD_xxx          TODO
 */
extern cmd_return_code_t
uty_file_write_close(vio_vf vf, bool final, bool base)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;
  assert(vf->vout_state == vf_closing) ;
  assert((vf->vout_type == VOUT_FILE) || (vf->vout_type == VOUT_CONFIG)) ;

  return uty_file_out_push(vf, final) ;
} ;

/*==============================================================================
 * Shell pipe stuff
 *
 */
typedef int  pipe_pair_t[2] ;
typedef int* pipe_pair ;

enum pipe_half
{
  in_fd   = 0,
  out_fd  = 1,
} ;
typedef enum pipe_half pipe_half_t ;

CONFIRM(STDIN_FILENO  == 0) ;
CONFIRM(STDOUT_FILENO == 1) ;
CONFIRM(STDERR_FILENO == 2) ;

enum
{
  stdin_fd  = STDIN_FILENO,
  stdout_fd = STDOUT_FILENO,
  stderr_fd = STDERR_FILENO,

  stds      = 3
} ;

enum pipe_id
{
  in_pipe,
  out_pipe,
  ret_pipe,

  pipe_count
} ;
typedef enum pipe_id pipe_id_t ;

typedef pipe_pair_t pipe_set[pipe_count] ;

static pid_t uty_pipe_fork(vty_io vio, const char* cmd_str, pipe_set pipes,
                                                            pipe_id_t type) ;
static bool uty_pipe_pair(vty_io vio, const char* cmd_str,
                                      const char* what, pipe_set    pipes,
                                                        pipe_id_t   id,
                                                        pipe_half_t half) ;
static bool uty_pipe_fork_fail(vty_io vio, const char* cmd_str,
                                           const char* what, pipe_set pipes,
                                           pipe_pair   pair,
                                           const char* action) ;
static void uty_pipe_close_half_pipe(pipe_pair pair, pipe_half_t half) ;
static void uty_pipe_open_complete(vio_vf vf, pid_t pid, int ret_fd,
                                                                 vio_vf slave) ;
static bool uty_pipe_exec_prepare(vty_io vio, pipe_set pipes) ;

static cmd_return_code_t uty_pipe_return_close(vio_vf vf, bool final) ;
static cmd_return_code_t uty_pipe_return_empty(vio_vf vf, bool final) ;
static cmd_return_code_t uty_pipe_collect_child(vio_vf vf, bool final) ;
static cmd_return_code_t uty_pipe_release_slave(vio_vf vf, bool final) ;

static cmd_return_code_t uty_pipe_shovel(vio_vf vf, bool final, bool closing) ;

static void uty_pipe_read_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_pipe_read_timeout(vio_timer timer,
                                                            void* action_info) ;
static void uty_pipe_return_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_pipe_return_timeout(vio_timer timer,
                                                            void* action_info) ;
static void uty_pipe_write_ready(vio_vfd vfd, void* action_info) ;
static vty_timer_time uty_pipe_write_timeout(vio_timer timer,
                                                            void* action_info) ;

/*------------------------------------------------------------------------------
 * Open pipe whose child's stdout is going to be read and executed as commands.
 *
 * The child's stderr is read, separately, and that is sent to the current
 * vout.
 *
 * If could not open, issues message to the vio.
 *
 * The new vin has two vio_fd's, the standard one to read commands, and the
 * other (the pr_vfd) to collect any stderr output (the "return") from the
 * child, which is sent to the current vout.
 *
 * The current vout is made the "slave" of the new vin "master".  For the
 * return the pr_vd reads into the "slave" obuf.
 *
 * There are three tricky bits:
 *
 *   - while reading from the main vfd, also reads from the return vfd.
 *
 *     Up to eof on the main vfd, reads from the return vfd whenever tries
 *     to fetch a command.  Once a command is fetched, will not shovel anything
 *     further into the slave obuf until the next command is fetched.  This
 *     means that return stuff is output *between* commands.
 *
 *   - in "blocking" state, will not block on the return unless blocks on the
 *     main vfd, or the main vfd is at eof.
 *
 *     in "non-blocking" state, will set read ready on the main vfd, until that
 *     reaches eof, in which case sets read ready on the return vfd (if that
 *     has not yet reached eof).
 *
 *   - the close operation will not succeed until the return vfd reaches EOF,
 *     or is a "final" close.
 *
 * If could not open, issues message to the vio.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open
 */
extern cmd_return_code_t
uty_pipe_read_open(vty_io vio, qstring command, cmd_context context)
{
  pipe_set    pipes ;
  const char* cmd_str ;
  vio_vf      vf ;
  pid_t       child ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  assert(vio->vin != NULL) ;            /* Not expected to be vin_base  */

  cmd_str = qs_make_string(command) ;

  /* Fork it                                                            */
  child = uty_pipe_fork(vio, cmd_str, pipes, in_pipe) ;

  if (child < 0)
    return CMD_WARNING ;

  /* We have a pipe, so now save context                                */
  uty_vin_new_context(vio, context, NULL) ;

  /* OK -- now push the new input onto the vin_stack.                   */
  vf = uty_vf_new(vio, cmd_str, pipes[in_pipe][in_fd], vfd_pipe, vfd_io_read) ;
  uty_vin_push(vio, vf, VIN_PIPE, uty_pipe_read_ready,
                                  uty_pipe_read_timeout, 4 * 1024) ;

  /* And the err_pair is set as the return from the child               */
  uty_pipe_open_complete(vf, child, pipes[ret_pipe][in_fd], vio->vout) ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Open pipe which is going to be written to (or not, if shell_only), and
 * what the pipe returns will be output to the current vout.
 *
 * The new vout becomes the "master", and has two vio_vfd's, one for output to
 * the pipe (this is NULL if shell_only) and the other for the pipe return.
 * The pipe return reads directly into the "slave" obuf.
 *
 * There are three tricky bits:
 *
 *   - while writing to the output vfd, also reads from the return vfd.
 *
 *     This is to avoid any possibility that the child process blocks because
 *     it cannot write to the return pipe.
 *
 *   - when closing the master pipe, it must remain open up to the time it
 *     gets eof on the return pipe.
 *
 *     This means that the all return output is collected before any further
 *     output to the slave can be done.
 *
 *   - in "blocking" state, reads from the return only when writes to the
 *     pipe, and when closing the pipe.
 *
 *     in "non-blocking" state, reads from the return and pushes to the slave
 *     under pselect... so sucks and blows at its own rate.
 *
 * Returns:  CMD_SUCCESS  -- all set to go
 *           CMD_WARNING  -- failed to open -- message sent to vio.
 */
extern cmd_return_code_t
uty_pipe_write_open(vty_io vio, qstring command, bool shell_only)
{
  pipe_set    pipes ;
  const char* cmd_str ;
  pid_t    child ;
  vio_vf   vf ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  cmd_str = qs_make_string(command) ;

  /* Do the basic file open.                                            */
  child = uty_pipe_fork(vio, cmd_str, pipes, shell_only ? ret_pipe : out_pipe) ;
  if (child < 0)
    return CMD_WARNING ;

  /* OK -- now push the new output onto the vout_stack.                 */

  if (shell_only)
    {
      vf = uty_vf_new(vio, cmd_str, -1, vfd_none, vfd_io_none) ;
      uty_vout_push(vio, vf, VOUT_SHELL_ONLY, NULL, NULL, 0) ;
    }
  else
    {
      vf = uty_vf_new(vio, cmd_str, pipes[out_pipe][out_fd],
                                                       vfd_pipe, vfd_io_write) ;
      uty_vout_push(vio, vf, VOUT_PIPE, uty_pipe_write_ready,
                                        uty_pipe_write_timeout, 4 * 1024) ;
    } ;

  /* Record the child pid and set up the pr_vf and enslave vout_next    */

  uty_pipe_open_complete(vf, child, pipes[ret_pipe][in_fd], vf->vout_next) ;

  vf->pr_only = shell_only ;

  /* If not blocking start up the return pipe reader.                   */

  if (!vf->blocking)
    vio_vfd_set_read(vf->pr_vfd, on, 0) ;         /* TODO timeout */

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Complete the opening of a pipe.
 *
 * All pipes have a return.  For in_pipes this is the child's stderr, for
 * out_pipes this is the child's combined stdout/stderr.
 *
 * VIN_PIPE sucks the return in between reading command lines.
 *
 * VOUT_PIPE/VOUT_SHELL_ONLY suck the return at the same time as writing to
 * the child.
 *
 * Sets pr_only false.
 */
static void
uty_pipe_open_complete(vio_vf vf, pid_t pid, int ret_fd, vio_vf slave)
{
  vfd_io_type_t  iot ;

  vf->child = uty_child_register(pid, vf) ;

  /* And configure the pipe return vfd.                                 */
  iot = vfd_io_read | (vf->blocking ? vfd_io_blocking : 0) ;

  vf->pr_vfd   = vio_vfd_new(ret_fd, vfd_pipe, iot, vf) ;
  vf->pr_state = vf_open ;

  vf->pr_only  = false ;

  if (!vf->blocking)
    vio_vfd_set_read_action(vf->pr_vfd, uty_pipe_return_ready) ;

  /* Configure master/slave relationship.                               */
  slave->pr_master  = vf ;
  vf->pr_slave      = slave ;
} ;

/*------------------------------------------------------------------------------
 * Fork fork fork
 *
 * Set up pipes according to type of fork:
 *
 *   in_pipe    -- input from child's stdout as main fd
 *                 input from child's stderr as return fd
 *
 *   out_pipe   -- output to  child's stdin  as main fd
 *                 input from child's stderr & stdout as return fd
 *
 *   ret_pipe   -- nothing for main fd
 *                 input from child's stderr & stdout as return fd
 *
 * vfork to create child process and then:
 *
 *   -- in parent close the unused part(s) of the pair(s).
 *
 *   -- in child, close all unused fds, and move relevant part(s) of pair(s)
 *      to stdin, stdout and stderr, sort out signal, set pwd then exec sh -c.
 */
static pid_t
uty_pipe_fork(vty_io vio, const char* cmd_str, pipe_set pipes, pipe_id_t type)
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
  if (type == in_pipe)
    if (!uty_pipe_pair(vio, cmd_str, "input pipe", pipes, in_pipe, in_fd))
      return -1 ;

  if (type == out_pipe)
    if (!uty_pipe_pair(vio, cmd_str, "output pipe", pipes, out_pipe, out_fd))
    return -1 ;

  if (!uty_pipe_pair(vio, cmd_str, "return pipe", pipes, ret_pipe, in_fd))
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
      uty_pipe_close_half_pipe(pipes[ret_pipe], out_fd) ;
    }
  else if (child < 0)           /* In parent -- failed                  */
    uty_pipe_fork_fail(vio, cmd_str, "vfork", pipes, NULL, "child") ;

  return child ;
} ;

/*------------------------------------------------------------------------------
 * Open a pipe pair -- generate suitable error message if failed and close
 * any early pipes that have been opened.
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
    return uty_pipe_fork_fail(vio, cmd_str, what, pipes, pair,
                                                          "set non-blocking") ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Open a pipe pair -- generate suitable error message if failed and close
 * any early pipes that have been opened.
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
 *   ret_pipe -- if present, is: stderr for the child
 *                          and: stdout for the child, if no in_pipe
 *
 * Set current directory.
 *
 * Reset all signals to default state and unmask everything.
 */
static bool
uty_pipe_exec_prepare(vty_io vio, pipe_set pipes)
{
  int std[stds] ;
  int fd ;

  /* Assign fds to child's std[]                                        */

  std[stdin_fd]  = pipes[out_pipe][in_fd] ;     /* stdin for child      */
  std[stdout_fd] = pipes[in_pipe][out_fd] ;     /* stdout for child     */
  std[stderr_fd] = pipes[ret_pipe][out_fd] ;    /* stderr for child     */

  /* Mark everything to be closed on exec                               */
  for (fd = 0 ; fd <= 1024 ; ++fd)      /* TODO -- max_fd       */
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
        if ((std[fd] = fcntl(std[fd], F_DUPFD, stds)) < 0)
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
 * Command line fetch from a pipe and (for blocking) shovel return into
 * slave and push.
 *
 * In "non-blocking" state, on CMD_WAITING may be waiting for read ready on
 * this vf, or (indirectly) write ready on the slave.
 *
 * In "blocking" state, will not return until have something, or there is
 * nothing to be had.
 *
 * Returns: CMD_SUCCESS    -- have another command line ready to go
 *          CMD_WAITING    -- would not wait for input  <=> non-blocking
 *          CMD_EOF        -- ran into EOF -- on input AND return
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- could wait no longer      <=> blocking
 *
 * If returns CMD_EOF will have vf->vin_state == vf_eof.  Further, will not
 * have vf_eof until returns CMD_EOF (the first time).
 *
 * This can be called in any thread.
 *
 * NB: once this has signalled CMD_EOF (on the main input) the close must deal
 *     with sucking up any remaining return.
 */
extern cmd_return_code_t
uty_pipe_fetch_command_line(vio_vf vf, cmd_action action)
{
  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  assert(vf->vin_type == VIN_PIPE) ;
  assert((vf->vin_state == vf_open) || (vf->vin_state == vf_eof)) ;

  /* TODO police the state of the two vfs ?                     */

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

      if ((ret == CMD_SUCCESS) || (ret == CMD_EOF))
        return ret ;

      /* Worry about the return.
       *
       * If the child is outputting to both stderr and stdout, we have no
       * way of telling the order the data was sent in -- but we treat
       * stderr as a higher priority !
       *
       * Expect only CMD_SUCCESS or CMD_WAITING (non-blocking), CMD_IO_ERROR
       * or CMD_IO_TIMEOUT (blocking).
       */
      if (vf->blocking && (vf->pr_state == vf_open))
        {
          ret = uty_pipe_shovel(vf, false, false) ; /* not final or closing */

          if ((ret != CMD_SUCCESS) && (ret != CMD_EOF))
            return ret ;        /* cannot continue              */
        } ;

      /* Need more from the main input.                                 */

      get = vio_fifo_read_nb(vf->ibuf, vio_vfd_fd(vf->vfd), 100) ;

      if (get > 0)
        continue ;              /* loop back                            */

      if (get == -1)
        return CMD_IO_ERROR ;   /* register error                       */

      if (get == -2)            /* EOF met immediately                  */
        {
          vf->vin_state = vf_eof ;
          continue ;            /* loop back -- deals with possible
                                   final line and the return.           */
        } ;

      assert (get == 0) ;

      /* We get here if main input is not yet at eof.                   */
      assert ((vf->vin_state == vf_open) || (vf->pr_state == vf_open)) ;

      if (!vf->blocking)
        {
          uty_vf_set_read(vf, on) ;
          return CMD_WAITING ;
        } ;

      /* Implement blocking I/O, with timeout                           */
      qps_mini_set(qm, (vf->vin_state == vf_open) ? vio_vfd_fd(vf->vfd)
                                                  : -1,
                                             qps_read_mnum, vf->read_timeout) ;
      if (vf->pr_state == vf_open)
        qps_mini_add(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum) ;

      if (qps_mini_wait(qm, NULL, false) == 0)
        return CMD_IO_TIMEOUT ;

      continue ;                /* loop back                            */
    } ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to a pipe and (for blocking) shovel return into
 * slave and push.
 *
 * Does not push to the pipe while there is return input to be read.  For
 * blocking I/O may block in the slave output.  For non-blocking I/O, will
 * return if would block in the slave output.
 *
 * Until the file becomes vf_closing, will not write the end_lump of the fifo,
 * so all output is in units of the fifo size -- which should be "chunky".
 *
 * Although it is unlikely to happen, if blocking, will block if cannot
 * completely write away what is required, or enable write ready.
 *
 * If final, will do final shovel from return to slave and also attempt to
 * empty any output buffer -- will not wait or block, and ignores errors.
 *
 * Returns: CMD_SUCCESS    -- written everything
 *          CMD_WAITING    -- could not write everything   (<=> non-blocking)
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- not going to wait any longer (<=> blocking)
 *
 * In "non-blocking" state, on CMD_WAITING the slave output will put the
 * master return pr_vfd into read ready state when it sees write ready, or will
 * here set the pr_vfd into read ready state.
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
  assert((vf->vout_state == vf_open) || (vf->vout_state == vf_closing)) ;
  assert((vf->vout_type == VOUT_PIPE) || (vf->vout_type == VOUT_SHELL_ONLY)) ;

  /* If blocking, see if there is anything in the return, and shovel.
   *
   * For non-blocking, the pselect process looks after this.
   */
  if (vf->blocking && (vf->pr_state == vf_open))
    {
      cmd_return_code_t ret ;

      ret = uty_pipe_shovel(vf, final, false) ;   /* not closing        */

      if ((ret != CMD_SUCCESS) && (ret != CMD_EOF) && !final)
        return ret ;
    } ;

  /* Now write away everything we can -- nothing if pr_only.
   */
  if (!vf->pr_only)
    {
      bool all ;

      all = (vf->vout_state == vf_closing) ;

      while (1)
        {
          qps_mini_t qm ;
          int put ;

          put = vio_fifo_write_nb(vf->obuf, vio_vfd_fd(vf->vfd), all) ;

          if (put < 0)
            return CMD_IO_ERROR ;       /* TODO                 */

          if ((put == 0) || final)      /* all gone or final    */
            break ;

          /* Cannot write everything away without waiting       */
          if (!vf->blocking)
            {
              uty_vf_set_write(vf, on) ;
              return CMD_WAITING ;
            } ;

          /* Implement blocking I/O, with timeout               */
          qps_mini_set(qm, vio_vfd_fd(vf->vfd), qps_write_mnum,
                                                            vf->write_timeout) ;
          if (qps_mini_wait(qm, NULL, false) == 0)
            return CMD_IO_TIMEOUT ;

          /* Loop back to vio_fifo_write_nb()                   */
        } ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Shovel from return to slave, pushing the slave as we go.
 *
 * For VIN_PIPE: this is called between command line reads, when a command
 * line read needs more input.  Wants to suck the return dry before proceeding
 * to read any more command lines.  Is also called when the vin is closed.
 *
 * For VOUT_PIPE: for blocking vfs this is called each time the output is
 * pushed -- to keep the process moving.  And when the vout is closed, to
 * complete the process.  For non-blocking vfs,  TODO
 *
 * For VOUT_SHELL_ONLY: same as VOUT_PIPE.
 *
 * For blocking, keeps going until can read no more from the return.  The slave
 * output may block, and could time out.
 *
 * With non-blocking, reads from the return and pushes at the slave.  If the
 * slave obuf is not emptied by the push, the slave will set the master
 * read ready, when the slave goes write-ready.
 *
 * For "final" does not attempt to read anything from the return, but sets it
 * vf_eof.
 *
 * For "closing" (if not "final"), if blocking will block on the return, until
 * get eof (or time out).
 *
 * Returns: CMD_SUCCESS    -- shovelled everything, but not hit EOF
 *                                              if "closing" <=> non-blocking
 *          CMD_EOF        -- shovelled everything and is now at EOF
 *          CMD_WAITING    -- waiting for slave write-ready  <=> non-blocking
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- could wait no longer           <=> blocking
 *
 * This can be called in any thread.
 */
static cmd_return_code_t
uty_pipe_shovel(vio_vf vf, bool final, bool closing)
{
  vio_vf  slave ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                */

  // TODO other pr_state ???  vf_closed/vf_closing/vf_error ??

  /* Suck and blow -- may block in uty_cmd_out_push()
   *
   * Note that we do not push the slave if there is nothing new from the
   * return -- there shouldn't be anything pending in the slave obuf.
   */
  slave = vf->pr_slave ;
  assert(vf == slave->pr_master) ;

  while (!final && (vf->pr_state == vf_open))
    {
      cmd_return_code_t ret ;
      int get ;

      get = vio_fifo_read_nb(slave->obuf, vio_vfd_fd(vf->pr_vfd), 10) ;

      if (get == 0)
        {
          qps_mini_t qm ;

          if (!closing || !vf->blocking)
            return CMD_SUCCESS ;                /* quit if now dry      */

          /* Implement blocking I/O, with timeout               */
          qps_mini_set(qm, vio_vfd_fd(vf->pr_vfd), qps_read_mnum,
                                                             vf->read_timeout) ;
          if (qps_mini_wait(qm, NULL, false) == 0)
            return CMD_IO_TIMEOUT ;
        }

      else if (get >= 0)
        {
          ret = uty_cmd_out_push(slave, final) ; /* may block etc.       */

          if (ret != CMD_SUCCESS)
            return ret ;
        }

      else if (get == -1)
        return CMD_IO_ERROR ;           /* register error TODO  */

      else
        {
          assert (get == -2) ;          /* eof on the return    */
          break ;
        } ;
    } ;

  vf->pr_state = vf_eof ;               /* Set EOF TODO: willy nilly ?  */

  return CMD_EOF ;
} ;

/*------------------------------------------------------------------------------
 * Pipe is ready to read -- this is the call-back planted in the vf->vfd, of
 * type VIN_PIPE.
 *
 * Can restart the command_queue.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
uty_pipe_read_ready(vio_vfd vfd, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  uty_cmd_signal(vf->vio, CMD_SUCCESS) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe has timed out, waiting to read -- this is the call-back planted in the
 * vf->vfd, of type VIN_PIPE.
 *
 * ????
 */
static vty_timer_time
uty_pipe_read_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->read_timer == timer) ;

//  TODO .... signal time out

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Pipe return is ready to read -- this is the call-back planted in the
 * vf->pr_vfd:
 *
 *   VIN_PIPE:        used only when closing the vin, and are waiting for
 *                    the return to be drained.
 *
 *   VOUT_PIPE:       used to continually shovel from the return to the
 *                    slave -- including while closing.
 *
 *   VOUT_SHELL_ONLY: used to continually shovel from the return to the
 *                    slave -- which happens while closing.
 *
 * If all is well, and more return input is expected, re-enables read ready.
 *
 * If uty_pipe_shovel() returns CMD_WAITING, then is waiting for the slave
 * output buffer to clear.  The slave will kick uty_pipe_return_slave_ready().
 *
 * For all other returns, kick the command loop, which if it is waiting is TODO
 */
static void
uty_pipe_return_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->pr_vfd == vfd) ;

  ret = uty_pipe_shovel(vf, false, false) ;     /* not final or closing */
                                                /* TODO -- errors !!    */
  if      (ret == CMD_SUCCESS)
    vio_vfd_set_read(vf->pr_vfd, on, 0) ;       /* expect more          */
  else if (ret != CMD_WAITING)
    uty_cmd_signal(vf->vio, (ret == CMD_EOF) ? CMD_SUCCESS
                                             : ret) ;   /* in case TODO */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe return slave is ready to write.
 *
 * This is called by the pipe return slave, to prompt the master when the slave
 * has been waiting to be able to write away its output buffer.
 *
 * This will set read ready on the return, to keep the process of shovelling
 * from return to slave going.
 */
extern void
uty_pipe_return_slave_ready(vio_vf slave)
{
  vio_vf vf ;

  vf = slave->pr_master ;
  assert(vf->pr_slave == slave) ;

  vio_vfd_set_read(vf->pr_vfd, on, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Pipe return has timed out -- this is the call-back planted in the
 * vf->pr_vfd of either a VIN_PIPE or a VOUT_PIPE ????
 *
 * ????
 */
static vty_timer_time
uty_pipe_return_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->pr_vfd->read_timer == timer) ;

//cq_continue(vf->vio->vty) ;   TODO

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Pipe output is ready to write -- this is the call-back planted in the
 * vf->vfd of a VOUT_PIPE
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
uty_pipe_write_ready(vio_vfd vfd, void* action_info)
{
  cmd_return_code_t ret ;
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  ret = uty_pipe_out_push(vf, false) ;  /* not final    */

  if (ret != CMD_WAITING)
    uty_cmd_signal(vf->vio, ret) ;

  if ((ret == CMD_SUCCESS) && (vf->pr_master != NULL))
    uty_pipe_return_slave_ready(vf) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Pipe output has timed out -- this is the call-back planted in the
 * vf->vfd of a VOUT_PIPE
 *
 * ????
 */
static vty_timer_time
uty_pipe_write_timeout(vio_timer timer, void* action_info)
{
  vio_vf   vf ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd->write_timer == timer) ;

//  TODO

  VTY_UNLOCK() ;

  return 0 ;            /* Do not restart timer */
} ;

/*------------------------------------------------------------------------------
 * Tidy up after input pipe has been read closed.
 *
 * Nothing needs to be done with the main input, but do need to close the
 * return, collect the child and release the slave.
 */
extern cmd_return_code_t
uty_pipe_read_close(vio_vf vf, bool final)
{
  return uty_pipe_return_close(vf, final) ;
} ;

/*------------------------------------------------------------------------------
 * Close VOUT_PIPE or VOUT_SHELL_ONLY.
 *
 * For not-final, wish to flush output buffer.  If final will attempt to empty
 * the output buffer -- but will not wait or block, and ignores errors.
 *
 * Must then close the return, collect the child and release the slave.
 *
 * Returns: CMD_SUCCESS    -- pushed what there was, or final
 *          CMD_WAITING    -- would have blocked        <=> non-blocking
 *          CMD_IO_ERROR   -- ran into an I/O error
 *          CMD_IO_TIMEOUT -- could wait no longer      <=> blocking
 */
extern cmd_return_code_t
uty_pipe_write_close(vio_vf vf, bool final, bool base, bool shell_only)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  /* If the main vfd is still there, keep pushing.
   *
   * Close it if manage to empty out buffers (or final) -- signals end to the
   * child process.
   */
  if (!vf->pr_only)
    {
      ret = uty_pipe_out_push(vf, final) ;

      if ((ret != CMD_SUCCESS) && !final)
        return ret ;

      /* Now need to close the output vfd to signal to the child that we
       * are done.
       *
       * Sets pr_only so that if does go CMD_WAITING, then future call of
       * uty_pipe_out_push() will not attempt any I/O !  vf->vfd is set NULL,
       * which will be ignored by any future vio_vfd_close().
       */
      vf->vfd = vio_vfd_close(vf->vfd) ;
      vf->pr_only = true ;              /* only the return is left      */
    } ;

  /* If the return is still open, try to empty and close that.          */
  return uty_pipe_return_close(vf, final) ;
} ;

/*------------------------------------------------------------------------------
 * Close the return on a pipe.
 *
 * If not final, may need to drain the return.
 *
 * Need to collect the child to complete the process.  Once the child is
 * collected (or have given up waiting, will send a message to the slave if
 * not completed with return code == 0.
 *
 * Once any message has been pushed to the slave, can release the slave
 * and the close process will be complete.
 *
 * If final, then will not block and will ignore errors.  Return code reflects
 * the last operation.
 *
 * Returns:  CMD_SUCCESS    -- closed and child collected
 *           CMD_WAITING
 *           CMD_IO_ERROR
 *           CMD_IO_TIMEOUT
 *           CMD_xxxx       -- child error(s)
 */
static cmd_return_code_t
uty_pipe_return_close(vio_vf vf, bool final)
{
  cmd_return_code_t ret ;

  VTY_ASSERT_CAN_CLOSE_VF(vf) ;

  /* If the return is still open, try to empty and close that.          */
  if (vf->pr_state != vf_closed)
    {
      ret = uty_pipe_return_empty(vf, final) ;

      if (!final)
        {
          if (ret == CMD_SUCCESS)
            {
              vio_vfd_set_read(vf->pr_vfd, on, 0) ;     /* TODO timeout */
              return CMD_WAITING ;
            } ;

          if (ret != CMD_EOF)
            return ret ;
        } ;
    } ;

  /* If not already collected, collect the child.                       */
  if (vf->child != NULL)
    {
      ret = uty_pipe_collect_child(vf, final) ;

      if ((ret != CMD_SUCCESS) && !final)
        return ret ;
    } ;

  /* Finally, release the slave                                         */
  return uty_pipe_release_slave(vf, final) ;
}

/*------------------------------------------------------------------------------
 * If the return is still open, shovel from return to slave.
 *
 * If final will not block or wait, and ignores errors -- return code reflects
 * the last operation.
 *
 * If blocking, will block until return hits EOF or timeout.
 *
 * Returns:  CMD_SUCCESS    -- shovelled what there was, may be more to come
 *                                                            <=> non-blocking
 *           CMD_EOF        -- shovelled what there was, is now *closed*
 *           CMD_WAITING    -- waiting for slave write-ready  <=> non-blocking
 *           CMD_IO_ERROR   -- ran into an I/O error
 *           CMD_IO_TIMEOUT -- could wait no longer           <=> blocking
 */
static cmd_return_code_t
uty_pipe_return_empty(vio_vf vf, bool final)
{
  cmd_return_code_t ret ;

  // worry about pr_state ??

  ret = uty_pipe_shovel(vf, final, true) ;  /* closing !            */

  if ((ret == CMD_EOF) || final)
    {
      vf->pr_vfd   = vio_vfd_close(vf->pr_vfd) ;
      vf->pr_state = vf_closed ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Collect child.
 *
 * When does collect, or times out, will dismiss the child and set vf->child
 * to NULL -- which indicates that the child is done with.
 *
 * Returns:  CMD_SUCCESS   -- child dismissed, vf->child == NULL
 *           CMD_WAITING   -- waiting to collect child        <=> non-blocking
 *
 */
static cmd_return_code_t
uty_pipe_collect_child(vio_vf vf, bool final)
{
  vio_vf slave ;

  assert(vf->child != NULL) ;

  /* Collect -- blocking or not blocking                                */
  if (!vf->child->collected && !vf->child->overdue)
    {
      /* If we are !blocking and !final, leave the child collection up to
       * the SIGCHLD system.
       */
      if (!vf->blocking && !final)
        {
          uty_child_awaited(vf->child, 6) ;
          return CMD_WAITING ;
        } ;

      /* If we are blocking or final, try to collect the child here and
       * now -- if not final may block here.
       */
      uty_child_collect(vf->child, 6, final) ;
    } ;

  /* If child is overdue, or did not terminate cleanly, write message to
   * slave... which we then have to push...
   */
  slave = vf->pr_slave ;
  assert((slave != NULL) && (vf == slave->pr_master)) ;

  if      (!vf->child->collected)
    {
      vio_fifo_printf(slave->obuf, "%% child process still running\n") ;
    }
  else if (WIFEXITED(vf->child->report))
    {
      int status = WEXITSTATUS(vf->child->report) ;
      if (status != 0)
        vio_fifo_printf(slave->obuf,
                  "%% child process ended normally, but with status = %d\n",
                                                                       status) ;
    }
  else if (WIFSIGNALED(vf->child->report))
    {
      int signal = WTERMSIG(vf->child->report) ;
      vio_fifo_printf(slave->obuf,
                      "%% child process terminated by signal = %d\n", signal) ;
    }
  else
    {
      vio_fifo_printf(slave->obuf,
                    "%% child process ended in unknown state = %d\n",
                                                            vf->child->report) ;
    } ;

  /* Can now dismiss the child.                                         */
  vf->child = uty_child_dismiss(vf->child, false) ;     /* not final    */

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Push output to slave a final time -- to shift any final message about state
 * of child on closing.
 *
 * Then release slave.
 */
static cmd_return_code_t
uty_pipe_release_slave(vio_vf vf, bool final)
{
  cmd_return_code_t ret ;
  vio_vf slave ;

  ret = CMD_SUCCESS ;

  slave = vf->pr_slave ;
  if (slave != NULL)
    {
      assert(vf == slave->pr_master) ;

      ret = uty_cmd_out_push(slave, final) ; /* may block etc.          */

      if ((ret != CMD_SUCCESS) && !final)
        return ret ;

      slave->pr_master = NULL ;         /* release slave                */
      vf->pr_slave     = NULL ;
  } ;

  return ret ;
} ;

/*==============================================================================
 * stdout and stderr
 *
 *
 */


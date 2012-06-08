/* VTY IO Basic Functions -- bottom level of VTY IO hierarchy
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

#include <sys/socket.h>
#include <fcntl.h>

#include "vty_io_basic.h"
#include "vty_local.h"
#include "log.h"
#include "pthread_safe.h"
#include "network.h"

#include "memory.h"

/*==============================================================================
 * Base level open operations -- files and pipes
 *
 */

/*------------------------------------------------------------------------------
 * Try to open the given file for the given type of I/O.
 *
 *   vfd_io_write      => set O_CREAT
 *
 *                        vfd_io_append => set O_APPEND
 *                            otherwise => set O_TRUNC
 *
 *                        vfd_io_excl   => set O_EXCL
 *
 *   vfd_io_read       => fail if does not exist (do not set O_CREAT)
 *
 *   vfd_io_read_write => set O_CREAT
 *
 *                        vfd_io_append => set O_APPEND
 *                            otherwise => leave file as is, if exists
 *
 *                        vfd_io_excl   => set O_EXCL
 *
 *   vfd_io_os_blocking => do not open O_NONBLOCK
 *
 *   (if none of the above, treat as vfd_io_read)
 *
 * The cmode is used, in the usual way, only if the file is created.  A
 * cmode == 0 is forced to S_IRUSR | S_IWUSR.  Discards any mode bits which
 * are not the bog standard R/W/X.  Overrides any umask.
 *
 * Note that the file offset is set to the beginning of the file.  The effect
 * of O_APPEND is *permanent*, and has the effect of adding an implied lseek to
 * end of file just before any write, such that the two operations are atomic.
 *
 * Returns: if >= 0  -- the fd of the open file
 *             <  0  -- failed to open file -- see errno
 *
 * NB: "vfd_io_os_blocking" really means that the I/O will be blocking.
 *
 *     "vfd_io_ps_blocking", used elsewhere, means that the actual I/O is
 *     non-blocking, but blocking, with time-out, is simulated by pselect.
 */
extern int
uty_fd_file_open(const char* name, vfd_io_type_t io_type, mode_t cmode)
{
  mode_t  um ;
  int     oflag ;
  int     fd ;
  int     err ;

  oflag = 0 ;
  if      ((io_type & vfd_io_read_write) == vfd_io_read_write)
    oflag = O_RDWR    | O_CREAT ;
  else if ((io_type & vfd_io_write) != 0)
    oflag = O_WRONLY  | O_CREAT ;
  else
    oflag = O_RDONLY ;

  if ((io_type & vfd_io_write) != 0)
    {
      if      ((io_type & vfd_io_append) != 0)
        oflag |= O_APPEND ;
      else if ((io_type & vfd_io_read) == 0)
        oflag |= O_TRUNC ;

      if      ((io_type & vfd_io_excl) != 0)
        oflag |= O_EXCL ;

      if (cmode == 0)
        cmode = S_IRUSR | S_IWUSR ;

      cmode &= (S_IRWXU | S_IRWXG | S_IRWXO) ;
    }
  else
    cmode = 0 ;

  if ((io_type & vfd_io_os_blocking) == 0)
    oflag |= O_NONBLOCK ;

  if (cmode != 0)
    um = umask((S_IRWXU | S_IRWXG | S_IRWXO) & ~cmode) ;
  else
    um = (mode_t)0 ;

  while (1)
    {
      fd = open(name, oflag, cmode) ;
      if (fd >= 0)
        break ;

      err = errno ;
      if (err == EINTR)
        continue ;

      break ;
    } ;

  if (cmode != 0)
    umask(um) ;

  if (fd < 0)
    errno = err ;

  return fd ;
} ;

/*==============================================================================
 * Simple minded file reading and line imaging.
 *
 * Reads into a temporary buffer, replacing all control characters other than
 * '\n' by ' ', and removing trailing space before each '\n'.  Ensures that
 * end of file is immediately preceded by '\n', either by discarding trailing
 * spaces, or by inserting '\n'.
 *
 * Assuming lines are a reasonable length (less than 200 characters), will
 * read from file in regular size lumps.
 */

/*------------------------------------------------------------------------------
 * Open given file for primitive line image reading.
 *
 * Returns:  newly created line image reader object
 *
 * Has failed if vli->fd is < 0.
 */
extern vty_line_image
uty_fd_line_image_open(const char* name)
{
  vty_line_image  vli ;

  vli = XCALLOC(MTYPE_TMP, sizeof(vty_line_image_t)) ;

  /* Zeroising sets:
   *
   *  buffer       -- NULL   -- buffer created below if opened
   *
   *  kept         -- NULL   -- kept == end <=> nothing in the buffer
   *  end          -- NULL
   *
   *  size         -- 0      -- no buffer, yet
   *
   *  name         -- NULL   -- name set, below
   *  fd           -- X      -- set below
   *
   *  when         -- NULL   -- set when error detected
   *  err          -- 0      -- no error, yet
   */

  vli->name = XSTRDUP(MTYPE_TMP, name) ;
  vli->fd   = uty_fd_file_open(vli->name, vfd_io_read | vfd_io_os_blocking, 0) ;

  if (vli->fd >= 0)
    {
      /* Opened OK -- create buffer big enough for the unit we'd like to
       * read in, plus a reasonable amount for a partial line at the end
       * of the buffer.
       */
      vli->size   = vty_line_image_unit + 200 ;
      vli->buffer = XMALLOC(MTYPE_TMP, vli->size) ;
    }
  else
    {
      /* Failed to open -- set parameters for error message.
       */
      vli->when = "opening" ;
      vli->err  = errno ;
    } ;

  return vli ;
} ;

/*------------------------------------------------------------------------------
 * Read next lump into primitive line image buffer
 *
 * Returns:  <  0  => failed -- closes the fd and sets vli->fd == -1
 *           == 0  => eof
 *           >  0  => first n bytes of vli->buffer contain one or more
 *                    complete lines.
 */
extern int
uty_fd_line_image_read(vty_line_image vli)
{
  ulen  have, want ;
  int   r ;
  byte* p, * q, * e, * n ;

  if (vli->fd < 0)
    return 0 ;                          /* already closed/failed        */

  p = vli->kept ;
  e = vli->end ;

  qassert(e >= p) ;
  have = (e - p) ;

  if (have != 0)
    memmove(vli->buffer, p, have) ;

  p = vli->buffer + have ;

  want = vli->size - have - 1 ;         /* leave room for final '\n' !  */
  if (want > vty_line_image_unit)
    want = vty_line_image_unit ;
  else
    qassert(want > 0) ;

  r = readn(vli->fd, p, want) ;

  if (r < 0)
    {
      /* Record error and quit.
       */
      vli->when  = "reading" ;
      vli->err   = errno ;

      close(vli->fd) ;
      vli->fd = -1 ;

      return -1 ;
    } ;

  /* Process control characters and strip spaces before '\n'
   */
  e = p + r ;
  q = p ;
  n = NULL ;

  while (p < e)
    {
      byte ch ;

      ch = *p++ ;

      if (ch < ' ')
        {
          if (ch == '\n')
            {
              while ((q > vli->buffer) && (*(q - 1) == ' '))
                --q ;           /* trim spaces before '\n'              */
              n = q ;           /* last '\n'                            */
            }
          else
            ch = ' ' ;          /* replace all controls except '\n'     */
        } ;

      *q++ = ch ;
    } ;

  /* Worry about eof
   */
  if ((uint)r < want)
    {
      /* Trim spaces before eof
       */
      while ((q > vli->buffer) && (*(q - 1) == ' '))
        --q ;

      /* If there is something in the buffer, and the buffer does not end
       * in '\n', insert '\n'.
       */
      if ((q > vli->buffer) && (*(q - 1) != '\n'))
        {
          qassert(q < (vli->buffer + vli->size)) ;
          n = q ;
          *q++ = '\n' ;
        } ;
    } ;

  /* If there is no '\n' in the buffer, we have a very large amount of data
   * on our hands which we simply return for output.
   *
   * Note that if the buffer ends in spaces, and the next thing to be read is
   * zero or more spaces followed by '\n' or eof, then will fail to trim the
   * trailing spaces... tough.
   */
  if (n == NULL)
    n = q - 1 ;         /* pretend last character is '\n'       */

  /* So... q points at last '\n', we release upto and including the '\n', and
   * keep back anything that follows.
   */
  ++n ;                 /* step past '\n'       */

  vli->kept = n ;
  vli->end  = q ;

  return (n - vli->buffer) ;
} ;

/*------------------------------------------------------------------------------
 * Close given primitive line image reader.
 *
 * Releases the reader and all its contents.
 *
 * Does nothing if vli == NULL
 *
 * Returns:  NULL
 */
extern vty_line_image
uty_fd_line_image_close(vty_line_image vli)
{
  if (vli != NULL)
    {
      XFREE(MTYPE_TMP, vli->buffer) ;
      XFREE(MTYPE_TMP, vli->name) ;

      if (vli->fd >= 0)
        close(vli->fd) ;

      XFREE(MTYPE_TMP, vli) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 * Base level I/O and Timer handling....
 *
 * This is separated out so that the differences between running in a qpnexus
 * and an old thread environment are encapsulated here.
 */

static void vio_vfd_mqb_kick(vio_vfd vfd) ;
static void vio_vfd_mqb_free(vio_vfd vfd) ;

/*==============================================================================
 * File Descriptor handling
 *
 * Provides read/write ready handling in consistent manner -- so don't care
 * whether is qpnexus or old thread environment.
 *
 * NB: in all cases, when a read/write event goes off, the read/write readiness
 *     is cleared and any read/write timer is stopped.
 *
 * In the qpnexus world, there is a small complication... the qpselect stuff
 * for all vty lives in the cli thread, so there is a mechanism here to allow
 * for messages from other threads to implement the necessary qpselect things,
 * see above.
 */

static void vio_vfd_qps_read_action(qps_file qf, void* file_info) ;
static void vio_vfd_qps_write_action(qps_file qf, void* file_info) ;
static int vio_vfd_thread_read_action(struct thread *thread) ;
static int vio_vfd_thread_write_action(struct thread *thread) ;
static vio_timer_time vio_vfd_read_time_out_action(vio_timer timer,
                                                              void* file_info) ;
static vio_timer_time vio_vfd_write_time_out_action(vio_timer timer,
                                                              void* file_info) ;
static void vio_vfd_set_read_off(vio_vfd vfd) ;
static void vio_vfd_set_write_off(vio_vfd vfd) ;

static void vio_timer_squelch(vio_timer timer) ;

Inline void
vio_vfd_do_read_action(vio_vfd vfd, bool time_out)
{
  vio_vfd_set_read_off(vfd) ;   /* turns off read-ready and time-out    */

  if (vfd->read_action != NULL)
    vfd->read_action(vfd, vfd->action_info, time_out) ;
}

Inline void
vio_vfd_do_write_action(vio_vfd vfd, bool time_out)
{
  vio_vfd_set_write_off(vfd) ;  /* turns off write-ready and time-out   */

  if (vfd->write_action != NULL)
    vfd->write_action(vfd, vfd->action_info, time_out) ;
} ;

/*------------------------------------------------------------------------------
 * Create a new vfd structure.
 *
 * Note that sets the same action info for read, write, read timeout and
 * write timeout.
 *
 * Is a "blocking" vio_vfd if the io_type is vfd_io_ps_blocking (where the
 * I/O is, in fact, non-blocking, but the vf level simulates blocking) or if
 * the io_type is vfd_io_os_blocking (where the I/O really is blocking).
 *
 * If is a "blocking" vio_vfd, will ignore any attempt to set read/write
 * ready/timeout, but will allow open/close when not in cli thread.
 *
 * Must be in CLI thread unless is vfd_io_ps_blocking and/or vfd_io_os_blocking.
 */
extern vio_vfd
vio_vfd_new(int fd, vfd_type_t type, vfd_io_type_t io_type, void* action_info)
{
  vio_vfd vfd ;

  vfd = XCALLOC(MTYPE_VTY, sizeof(vio_vfd_t)) ;

  /* Has set:
   *
   *   fd             -- X           -- see below
   *
   *   type           -- X           -- see below
   *   io_type        -- X           -- see below
   *   failed         -- false
   *
   *   action_info    -- NULL        -- set below if ! "blocking" vio_vfd
   *
   *   read_action    -- NULL
   *   write_action   -- NULL
   *
   *   read_timer     -- NULL        -- set below if ! "blocking" vio_vfd
   *   write_timer    -- NULL        -- set below if ! "blocking" vio_vfd
   *
   *   f.qf           -- NULL        -- set below if ! "blocking" vio_vfd
   *
   *   f.thread.read  -- NULL
   *   f.thread.write -- NULL
   *
   *   queued         -- false
   *
   *   read_req       -- all zeros   -- none set
   *   write_req      -- all zeros   -- none set
   *
   *   mqb            -- NULL        -- none, yet
   */
  confirm(VIO_TIMER_INIT_ZERO) ;

  vfd->fd      = fd ;
  vfd->type    = type ;
  vfd->io_type = io_type ;

  if ((io_type & vfd_io_blocking) == 0)
    {
      VTY_ASSERT_CLI_THREAD() ;

      if (vty_nexus)
        {
          vfd->f.qf = qps_file_init_new(NULL, NULL) ;
          qps_add_file(vty_cli_nexus->selection, vfd->f.qf, vfd->fd, vfd) ;
        } ;

      vfd->read_timer  = vio_timer_init_new(NULL,
                                            vio_vfd_read_time_out_action, vfd) ;
      vfd->write_timer = vio_timer_init_new(NULL,
                                           vio_vfd_write_time_out_action, vfd) ;
      vfd->action_info = action_info ;
    } ;

  return vfd ;
} ;

/*------------------------------------------------------------------------------
 * Set the read action field for the given vio_vfd.
 *
 * NB: the read-ready and any time-out are both implicitly unset when either
 *     goes off.
 *
 *     If the action function may set read-ready again, with or without a
 *     new time-out.
 */
extern void
vio_vfd_set_read_action(vio_vfd vfd, vio_vfd_action* action)
{
  vfd->read_action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the write action field for the given vio_vfd.
 *
 * NB: the write-ready and any time-out are both implicitly unset when either
 *     goes off.
 *
 *     If the action function may set write-ready again, with or without a
 *     new time-out.
 */
extern void
vio_vfd_set_write_action(vio_vfd vfd, vio_vfd_action* action)
{
  vfd->write_action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the action_info field for the given vio_vfd read/write action.
 */
extern void
vio_vfd_set_action_info(vio_vfd vfd, void* action_info)
{
  vfd->action_info = action_info ;
} ;

/*------------------------------------------------------------------------------
 * If there is a vfd, set the "failed" flag, which suppresses any further
 * error logging - to avoid clutter.
 */
extern void
vio_vfd_set_failed(vio_vfd vfd)
{
  if (vfd != NULL)
    vfd->failed = true ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * If there is a read action set for the give vio_vfd (if any), then kick it.
 */
extern void
vio_vfd_kick_read_action(vio_vfd vfd)
{
  if ((vfd != NULL) && (vfd->read_action != NULL))
    vio_vfd_do_read_action(vfd) ;
} ;

/*------------------------------------------------------------------------------
 * If there is a write action set for the give vio_vfd (if any), then kick it.
 */
extern void
vio_vfd_kick_write_action(vio_vfd vfd)
{
  if ((vfd != NULL) && (vfd->write_action != NULL))
    vio_vfd_do_read_action(vfd) ;
} ;
#endif

/*------------------------------------------------------------------------------
 * Close the read side of the given vfd (if any).
 *
 * If the vfd is a socket, then does a shutdown of the read side -- whether or
 * not is vfd_io_no_close.
 *
 * If the vfd is not a socket and is read (only) closes the vfd -- honouring
 * vfd_io_no_close in the usual way.
 *
 * In any case, turns off any read ready and read ready timeout.
 *
 * Returns original vfd, or NULL if it has been closed.
 *
 * NB: if this is not a "blocking" vio_vfd, then MUST be in the cli thread.
 */
extern vio_vfd
vio_vfd_read_close(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd == NULL)
    return NULL ;

  if ((vfd->io_type & vfd_io_blocking) == 0)
    VTY_ASSERT_CLI_THREAD() ;

  if ((vfd->io_type & vfd_io_read) != 0)
    {
      if ((vfd->io_type & vfd_io_write) != 0)
        {
          /* read & write, so really half-close if can                  */
          if (vfd->fd >= 0)
            {
              if (vfd->type == vfd_socket)
                {
                  int rc, try ;

                  /* POSIX doesn't list EINTR as a failure for shutdown()
                   * but we handle it the same way as for close() in any case.
                   *
                   * We are completely paranoid and arrange not to get trapped
                   * here indefinitely.
                   */
                  try = 5 ;
                  do
                    rc = shutdown(vfd->fd, SHUT_RD) ;
                  while ((rc < 0) && (errno == EINTR) && (try-- > 0)) ;

                  if ((rc < 0) && !vfd->failed)
                    {
                      zlog_err("%s: shutdown() failed, fd=%d: %s", __func__,
                                                vfd->fd, errtoa(errno, 0).str) ;
                      vfd->failed = true ;
                    } ;
                } ;
              vio_vfd_set_read(vfd, off, 0) ;
              vfd->io_type ^= vfd_io_read ;     /* now write only !     */
            } ;
        }
      else
        {
          /* read only, so fully close                                  */
          vfd = vio_vfd_close(vfd) ;
        } ;
    } ;

  return vfd ;
} ;

/*------------------------------------------------------------------------------
 * Close the given vfd (if any).
 *
 * Stops any read/write waiting, timers etc and releases the vfd and all its
 * contents.
 *
 * Closes the fd (if there is one), unless vfd_io_no_close.
 *
 * NB: if this is not a "blocking" vio_vfd, then MUST be in the cli thread.
 *     Inter alia, this guarantees that cannot be in the middle of a read/write
 *     ready/timeout operation -- so the file can be closed down, and
 *     any pending ready/timeout will be swept away.
 *
 * Returns:  NULL -- new value for vfd !
 */
extern vio_vfd
vio_vfd_close(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd == NULL)
    return NULL ;

  /* Clear out the vfd
   */
  if (vty_nexus)
    {
      if ((vfd->io_type & vfd_io_blocking) == 0)
        VTY_ASSERT_CLI_THREAD() ;

      if (vfd->f.qf != NULL)
        {
          if (vfd->fd >= 0)
            qassert(qps_file_fd(vfd->f.qf) == vfd->fd) ;
          else
            qassert(qps_file_fd(vfd->f.qf) < 0) ;

          qps_remove_file(vfd->f.qf) ;
          qps_file_unset_fd(vfd->f.qf) ; /* so qps_file_free() does not close */
          vfd->f.qf = qps_file_free(vfd->f.qf) ;
        } ;

      vio_vfd_mqb_free(vfd) ;
    }
  else
    {
      if (vfd->f.thread.read != NULL)
        {
          qassert(vfd->fd >= 0) ;
          qassert(vfd->fd == THREAD_FD(vfd->f.thread.read)) ;
          thread_cancel(vfd->f.thread.read) ;
          vfd->f.thread.read  = NULL ;
        } ;

      if (vfd->f.thread.write != NULL)
        {
          qassert(vfd->fd >= 0) ;
          qassert(vfd->fd == THREAD_FD(vfd->f.thread.write)) ;
          thread_cancel(vfd->f.thread.write) ;
          vfd->f.thread.write = NULL ;
        } ;

      qassert(vfd->mqb == NULL) ;
    } ;

  vfd->read_timer  = vio_timer_reset(vfd->read_timer,  free_it) ;
  vfd->write_timer = vio_timer_reset(vfd->write_timer, free_it) ;

  /* Close the underlying fd, if any
   */
  if ((vfd->fd >= 0) && ((vfd->io_type & vfd_io_no_close) == 0))
    {
      int rc, try ;

      /* POSIX lists EINTR as a failure for close().
       *
       * We are completely paranoid and arrange not to get trapped
       * here indefinitely.
       */
      try = 5 ;
      do
        rc = close(vfd->fd) ;
      while ((rc < 0) && (errno == EINTR) && (try-- > 0)) ;

      if ((rc < 0) && !vfd->failed)
        {
          zlog_err("%s: close() failed, fd=%d: %s", __func__, vfd->fd,
                                                        errtoa(errno, 0).str) ;
          vfd->failed = true ;
        } ;
    } ;

  /* Finally, release the vfd itself.
   */
  XFREE(MTYPE_VTY, vfd) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set or unset read ready state on given vio_vfd (if any).
 *
 * Do nothing if vfd NULL, fd < 0 or not a read type of fd.
 *
 * If setting read_on, starts any read timeout timer (or stops it if 0).
 * If setting read off, stops any read timeout timer.
 *
 * NB: this can done from any thread, but if not done from the CLI thread,
 *     a message must be sent to the CLI thread to actually implement.
 *
 * NB: must NOT be a "blocking" vio_vfd !!
 *
 * Returns: how actually set.
 *          Will be off if not in a suitable state to set on.
 */
extern on_off_b
vio_vfd_set_read(vio_vfd vfd, on_off_b how, vio_timer_time timeout)
{
  VTY_ASSERT_LOCKED() ;

  if ((vfd == NULL) || (vfd->fd < 0) || ((vfd->io_type & vfd_io_read) == 0))
    return off ;

  qassert((vfd->io_type & vfd_io_blocking) == 0) ;

  if ((vfd->io_type & vfd_io_blocking) != 0)
    return off ;

  if (vty_is_cli_thread())
    {
      /* In the cli thread (effectively) so do things directly.         */

      vfd->read_req.set = false ;       /* doing or overriding request  */

      if (how == on)
        {
          assert(vfd->read_action != NULL) ;

          if (vty_nexus)
              qps_enable_mode(vfd->f.qf, qps_read_mnum,
                                                     vio_vfd_qps_read_action) ;
          else
            {
              if (vfd->f.thread.read == NULL)
                vfd->f.thread.read = thread_add_read(master,
                                    vio_vfd_thread_read_action, vfd, vfd->fd) ;
            } ;

          vio_timer_set(vfd->read_timer, timeout) ;
        }
      else
        {
          qassert(how == off) ;

          vio_vfd_set_read_off(vfd) ;
        } ;
    }
  else
    {
      /* In other threads, must send message to cli thread
       */
      qassert((how == on) || (how == off)) ;

      vfd->read_req.set     = true ;
      vfd->read_req.how     = how ;
      vfd->read_req.timeout = timeout ;

      vio_timer_squelch(vfd->read_timer) ;

      vio_vfd_mqb_kick(vfd) ;
    } ;

  return how ;
} ;

/*------------------------------------------------------------------------------
 * Unset read ready state on given vio_vfd (if any) and stop any time-out.
 */
static void
vio_vfd_set_read_off(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (vty_nexus)
    qps_disable_modes(vfd->f.qf, qps_read_mbit) ;
  else
    {
      if (vfd->f.thread.read != NULL)
        {
          thread_cancel (vfd->f.thread.read) ;
          vfd->f.thread.read = NULL ;
        } ;
    } ;

  vio_timer_unset(vfd->read_timer) ;
} ;

/*------------------------------------------------------------------------------
 * Set or unset write ready state on given vio_vfd (if any).
 *
 * Do nothing if vfd NULL, fd < 0 or not a write type of fd.
 *
 * If setting write_on, starts any write timeout timer.
 * If setting write off, stops any write timeout timer.
 *
 * NB: this can done from any thread, but if not done from the CLI thread,
 *     a message must be sent to the CLI thread to actually implement.
 *
 * NB: must NOT be a "blocking" vio_vfd !!
 *
 * Returns: how actually set.
 *          Will be off if not in a suitable state to set on.
 */
extern on_off_b
vio_vfd_set_write(vio_vfd vfd, on_off_b how, vio_timer_time timeout)
{
  VTY_ASSERT_LOCKED() ;

  if ((vfd == NULL) || (vfd->fd < 0) || ((vfd->io_type & vfd_io_write) == 0))
    return off ;

  qassert((vfd->io_type & vfd_io_blocking) == 0) ;
  if ((vfd->io_type & vfd_io_blocking) != 0)
    return off ;

  if (vty_is_cli_thread())
    {
      /* In the cli thread (effectively) so do things directly.         */

      vfd->write_req.set = false ;      /* doing or overriding request  */

      if (how == on)
        {
          assert(vfd->write_action != NULL) ;

          if (vty_nexus)
            qps_enable_mode(vfd->f.qf, qps_write_mnum,
                                                     vio_vfd_qps_write_action) ;
          else
            {
              if (vfd->f.thread.write == NULL)
                vfd->f.thread.write = thread_add_write(master,
                                    vio_vfd_thread_write_action, vfd, vfd->fd) ;
            } ;

          vio_timer_set(vfd->write_timer, timeout) ;
        }
      else
        {
          qassert(how == off) ;

          vio_vfd_set_write_off(vfd) ;
        } ;
    }
  else
    {
      /* In other threads, must send message to cli thread
       */
      qassert((how == on) || (how == off)) ;

      vfd->write_req.set     = true ;
      vfd->write_req.how     = how ;
      vfd->write_req.timeout = timeout ;

      vio_timer_squelch(vfd->write_timer) ;

      vio_vfd_mqb_kick(vfd) ;
    } ;

  return how ;
} ;

/*------------------------------------------------------------------------------
 * Unset write ready state on given vio_vfd (if any) and stop any time-out.
 */
static void
vio_vfd_set_write_off(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (vty_nexus)
    qps_disable_modes(vfd->f.qf, qps_write_mbit) ;
  else
    {
      if (vfd->f.thread.write != NULL)
        {
          thread_cancel (vfd->f.thread.write) ;
          vfd->f.thread.write = NULL ;
        } ;
    } ;

  vio_timer_unset(vfd->write_timer) ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to read
 *
 * Clears read ready state and unsets any read timer.
 */
static void
vio_vfd_qps_read_action(qps_file qf, void* file_info)
{
  vio_vfd vfd ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vfd = file_info ;

  assert((vfd->fd == qf->fd) && (vfd->f.qf == qf)) ;

  vio_vfd_do_read_action(vfd, false /* not time-out */) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: ready to read
 *
 * Clears read ready state and unsets any read timer.
 */
static int
vio_vfd_thread_read_action(struct thread *thread)
{
  vio_vfd vfd ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vfd = THREAD_ARG(thread);

  assert(vfd->fd == THREAD_FD(thread)) ;

  vfd->f.thread.read = NULL ;           /* implicitly           */

  vio_vfd_do_read_action(vfd, false /* not time-out */) ;

  VTY_UNLOCK() ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to write
 *
 * Clears write ready state and unsets any write timer.
 */
static void
vio_vfd_qps_write_action(qps_file qf, void* file_info)
{
  vio_vfd vfd = file_info ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert((vfd->fd == qf->fd) && (vfd->f.qf == qf)) ;

  vio_vfd_do_write_action(vfd, false /* not time-out */) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: ready to write
 *
 * Clears write ready state and unsets any write timer.
 */
static int
vio_vfd_thread_write_action(struct thread *thread)
{
  vio_vfd vfd = THREAD_ARG (thread);

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(vfd->fd == THREAD_FD(thread)) ;

  vfd->f.thread.write = NULL ;          /* implicitly   */

  vio_vfd_do_write_action(vfd, false /* not time-out */) ;

  VTY_UNLOCK() ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- read time-out
 *
 * Clears read ready state and unsets any read timer.
 */
static vio_timer_time
vio_vfd_read_time_out_action(vio_timer timer, void* file_info)
{
  vio_vfd vfd ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vfd = file_info ;

  assert(timer == vfd->read_timer) ;

  vio_vfd_do_read_action(vfd, true /* time-out */) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- write time-out
 *
 * Clears write ready state and unsets any write timer.
 */
static vio_timer_time
vio_vfd_write_time_out_action(vio_timer timer, void* file_info)
{
  vio_vfd vfd = file_info ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  assert(timer == vfd->write_timer) ;

  vio_vfd_do_write_action(vfd, true /* time-out */) ;

  return 0 ;
} ;

/*==============================================================================
 * Message handling, so that other threads can signal for output to be
 * dispatched !
 *
 * There is one message block per vfd.  It is only ever touched under the
 * vty_mutex.
 *
 * Once it is dispatched it is marked 'queued'.  Can still be changed, but no
 * further dispatch is required.  When it has been dequeued and processed,
 * it is marked inactive.
 *
 * If the vfd is closed while the message is queued, the pointer from the mqb
 * to the vfd is set NULL, and when the message is dequeued and actioned,
 * the mqb is destroyed.
 */

static void vio_vfd_mqb_action(mqueue_block mqb, mqb_flag_t flag) ;

/*------------------------------------------------------------------------------
 * Dispatch mqb, if not already queued
 */
static void
vio_vfd_mqb_kick(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (!vfd->queued)
    {
      vfd->queued  = true ;

      if (vfd->mqb == NULL)
        vfd->mqb = mqb_init_new(NULL, vio_vfd_mqb_action, vfd) ;

      mqueue_enqueue(vty_cli_nexus->queue, vfd->mqb, mqb_priority) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Free mqb, if exists.
 */
static void
vio_vfd_mqb_free(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd->queued)
    mqb_set_arg0(vfd->mqb, NULL) ;      /* mqb will suicide     */
  else
    mqb_free(vfd->mqb) ;                /* kill now (if any)    */

  vfd->queued  = false ;
  vfd->mqb     = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Action routine for the read/write on/off setting message.
 *
 * If the mqb is marked to close, then it and any qps_file it points to have
 * been cut loose, and now is the time to close the fd and release the
 * qps_file, along with releasing the mqb.
 *
 * If the mqb is being revoked
 */
static void
vio_vfd_mqb_action(mqueue_block mqb, mqb_flag_t flag)
{
  vio_vfd  vfd ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  vfd  = mqb_get_arg0(mqb) ;

  if (vfd != NULL)
    {
      assert(mqb == vfd->mqb) ;
      vfd->queued = false ;
    } ;

  if ((flag != mqb_destroy) && (vfd != NULL))
    {
      if (vfd->read_req.set)
        vio_vfd_set_read(vfd, vfd->read_req.how, vfd->read_req.timeout) ;
      if (vfd->write_req.set)
        vio_vfd_set_write(vfd, vfd->write_req.how, vfd->write_req.timeout) ;
    }
  else
    {
      mqb_free(mqb) ;           /* Suicide              */
      if (vfd != NULL)
        vfd->mqb = NULL ;       /* make sure vfd knows  */
    } ;

  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Listener Handling
 *
 *
 */
static void vio_accept(vio_vfd vfd, void* info, bool time_out) ;

/*------------------------------------------------------------------------------
 * Create a new listener object for the newly opened listener socket.
 *
 * Sets the accept action that will be called, and passed the fd of the listener
 * socket, when the listen socket goes 'read ready'.
 *
 * Returns address of newly created listener structure.
 */
extern vio_listener
vio_listener_new(int fd, vio_vfd_accept* accept_action)
{
  vio_listener listener ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  listener = XCALLOC(MTYPE_VTY, sizeof(struct vio_listener)) ;
  /* sets the next pointer to NULL              */

  listener->vfd = vio_vfd_new(fd, vfd_listener, vfd_io_read, listener) ;

  listener->accept_action = accept_action ;

  vio_vfd_set_read_action(listener->vfd, vio_accept) ;
  vio_vfd_set_read(listener->vfd, on, 0) ;

  return listener ;
} ;

/*------------------------------------------------------------------------------
 * Close listener and free listener structure.
 * Stops any read waiting and releases all memory.
 *
 * NB: assumes that the structure has been removed from any list.
 */
extern void
vio_listener_close(vio_listener listener)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  vio_vfd_close(listener->vfd) ;
  XFREE(MTYPE_VTY, listener) ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- this is the read_action from the listener vfd.
 *
 * info points at the listener object.
 */
static void
vio_accept(vio_vfd vfd, void* info, bool time_out)
{
  vio_listener listener ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  listener = info ;
  assert(vfd == listener->vfd) ;

  vio_vfd_set_read(listener->vfd, on, 0) ;

  listener->accept_action(vfd->fd) ;
} ;

/*==============================================================================
 * Timer Handling
 *
 * Provides timer primitives that work either in qnexus environment or in
 * a thread environment.  Timer times are seconds.
 *
 * NB: the timer is implicitly stopped when it times out.
 *
 *     If the action function returns non-zero, the timer is restarted with
 *     the given time.
 *
 *     If the action function returns zero, the timer is left as it is.  The
 *     action function may have set it going again, but if not, it will be
 *     left stopped.
 */

static void vio_timer_qtr_action(qtimer qtr, void* timer_info, qtime_t when) ;
static int vio_timer_thread_action(struct thread *thread) ;

/*------------------------------------------------------------------------------
 * Allocate and/or initialise vio_timer structure.
 */
extern vio_timer
vio_timer_init_new(vio_timer timer, vio_timer_action* action, void* action_info)
{
  if (timer == NULL)
    timer = XCALLOC(MTYPE_VTY, sizeof(vio_timer_t)) ;
  else
    memset(timer, 0, sizeof(vio_timer_t)) ;

  confirm(VIO_TIMER_INIT_ZERO) ;

  /* Zeroizing has set:
   *
   *   action       -- X       -- set below
   *   action_info  -- X       -- set below
   *
   *   active       -- false   -- not yet active
   *   squelch      -- false   -- not yet squelched
   *
   *   t.qtr        -- NULL    -- no qtimer, yet
   *   t.thread     -- NULL    -- no thread, yet
   */

  timer->action      = action ;
  timer->action_info = action_info ;

  return timer ;
} ;

/*------------------------------------------------------------------------------
 * Set the action field for the given timer.
 */
extern void
vio_timer_set_action(vio_timer timer, vio_timer_action* action)
{
  if (timer != NULL)
    timer->action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the info field for the given timer.
 */
extern void
vio_timer_set_info(vio_timer timer, void* action_info)
{
  if (timer != NULL)
    timer->action_info = action_info ;
} ;

/*------------------------------------------------------------------------------
 * Squelch the given timer -- if it goes off, do not call the action routine,
 * and leave the timer inactive.
 *
 * Used when doing read/write ready from not-cli thread.
 */
static void
vio_timer_squelch(vio_timer timer)
{
  VTY_ASSERT_LOCKED() ;

  if (timer != NULL)
    timer->squelch = true ;
} ;

/*------------------------------------------------------------------------------
 * Reset vio_timer structure.
 *
 * Stops any timer and releases all memory, if required.
 */
extern vio_timer
vio_timer_reset(vio_timer timer, free_keep_b free_structure)
{
  VTY_ASSERT_LOCKED() ;

  if (timer != NULL)
    {
      VTY_ASSERT_CLI_THREAD() ;

      if (timer->t.anon != NULL)
        {
          if (vty_nexus)
            qtimer_free(timer->t.qtr) ; /* stop and discard qtimer even
                                         * if keeping vio_timer         */
          else
            thread_cancel(timer->t.thread) ;

          timer->t.anon = NULL ;
        } ;

      timer->active  = false ;
      timer->squelch = false ;

      if (free_structure)
        XFREE(MTYPE_VTY, timer) ;       /* sets timer = NULL            */
    } ;

  return timer ;
} ;

/*------------------------------------------------------------------------------
 * Set vio_timer going, with the given time (in seconds).
 *
 * If timer is running, set to new time.
 *
 * If the time == 0, stop any current timer, do not restart.
 */
extern void
vio_timer_set(vio_timer timer, vio_timer_time time)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  if (time == 0)
    {
      vio_timer_unset(timer) ;
      return ;
    } ;

  assert(timer->action != NULL) ;

  if (vty_nexus)
    {
      if (timer->t.qtr == NULL)         /* allocate qtr if required     */
        timer->t.qtr = qtimer_init_new(NULL, vty_cli_nexus->pile,
                                                  vio_timer_qtr_action, timer) ;
      qtimer_set(timer->t.qtr, qt_add_monotonic(QTIME(time)), NULL) ;
    }
  else
    {
      if (timer->t.thread != NULL)
        thread_cancel(timer->t.thread) ;
      timer->t.thread = thread_add_timer(master,
                                         vio_timer_thread_action, timer, time) ;
    } ;

  timer->active  = true ;
  timer->squelch = false ;
} ;

/*------------------------------------------------------------------------------
 * Stop vio_timer, if any.
 */
extern void
vio_timer_unset(vio_timer timer)
{
  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  if (timer->active)
    {
      if (vty_nexus)
        {
          assert(timer->t.qtr != NULL) ;
          qtimer_unset(timer->t.qtr) ;
        }
      else
        {
          assert(timer->t.thread != NULL) ;
          thread_cancel(timer->t.thread) ;
          timer->t.thread = NULL ;
        } ;

      timer->active = false ;
    } ;

  timer->squelch = false ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: deal with timer timeout.
 */
static void
vio_timer_qtr_action(qtimer qtr, void* timer_info, qtime_t when)
{
  vio_timer timer ;
  vio_timer_time time ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  timer = timer_info ;

  timer->active = false ;       /* by default   */

  if ((timer->action != NULL) && (!timer->squelch))
    {
      time = timer->action(timer, timer->action_info) ;
      if (time != 0)
        vio_timer_set(timer, time) ;
    }
  else
    timer->squelch = false ;    /* done         */

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: deal with timer timeout.
 */
static int
vio_timer_thread_action(struct thread *thread)
{
  vio_timer      timer ;
  vio_timer_time time ;

  VTY_LOCK() ;
  VTY_ASSERT_CLI_THREAD() ;

  timer = THREAD_ARG(thread) ;
  timer->t.thread = NULL ;      /* implicitly   */

  timer->active = false ;       /* by default   */

  if ((timer->action != NULL) && (!timer->squelch))
    {
      time = timer->action(timer, timer->action_info) ;
      if (time != 0)
        vio_timer_set(timer, time) ;
    }
  else
    timer->squelch = false ;    /* done         */

  VTY_UNLOCK() ;
  return 0;
} ;

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

#include "vty_io_basic.h"
#include "vty_local.h"

#include "memory.h"

#include <sys/socket.h>
#include <fcntl.h>

/*==============================================================================
 * Base level open operations -- files and pipes
 *
 */

/*==============================================================================
 * Try to open the given file for the given type of I/O.
 *
 *   vfd_io_write      => create if does not exist (mode 0600)
 *
 *                        vfd_io_append => set O_APPEND
 *                            otherwise => truncate file if it does exist
 *
 *   vfd_io_read       => fail if does not exist
 *
 *   vfd_io_read_write => create if does not exist (mode 0600)
 *
 *                        vfd_io_append => set O_APPEND
 *                            otherwise => leave file as is
 *
 *   vfd_io_blocking => do not open O_NONBLOCK
 *
 *   (if none of the above, treat as vfd_io_read)
 *
 * Returns: if >= 0  -- the fd of the open file
 *             <  0  -- failed to open file -- see errno
 */
extern int
uty_vfd_file_open(const char* name, vfd_io_type_t io_type)
{
  int oflag ;

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
    } ;

  if ((io_type & vfd_io_blocking) == 0)
    oflag |= O_NONBLOCK ;

  return open(name, oflag, S_IRUSR | S_IWUSR) ;
} ;

/*==============================================================================
 * Base level I/O and Timer handling....
 *
 * This is separated out so that the differences between running in a qpnexus
 * and an old thread environment are encapsulated here.
 */

struct vio_io_set_args                  /* to CLI thread                */
{
  bool      active ;    /* set when queued, cleared when dequeued       */
  bool      close ;     /* close and free the vio_vfd and mqb           */

  bool      readable ;  /* set when read state to be changed            */
  on_off_b  read_on ;   /* what to change read to                       */
  vty_timer_time read_timeout ;
                        /* what to set the timeout to, if any           */

  bool      writable ;  /* set when write state to be changed           */
  on_off_b  write_on ;  /* what to change write to                      */
  vty_timer_time write_timeout ;
                        /* what to set the timeout to, if any           */
} ;
MQB_ARGS_SIZE_OK(vio_io_set_args) ;

static void vio_vfd_mqb_dispatch(vio_vfd vfd) ;
static struct vio_io_set_args* vio_vfd_mqb_args(vio_vfd vfd) ;

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

static void vio_timer_squelch(vio_timer_t* timer) ;

Inline void
vio_vfd_do_read_action(vio_vfd vfd)
{
  if (vfd->active)
    vfd->read_action(vfd, vfd->action_info) ;
}

Inline void
vio_vfd_do_write_action(vio_vfd vfd)
{
  if (vfd->active)
    vfd->write_action(vfd, vfd->action_info) ;
} ;

/*------------------------------------------------------------------------------
 * Create a new vfd structure.
 */
extern vio_vfd
vio_vfd_new(int fd, vfd_type_t type, vfd_io_type_t io_type, void* action_info)
{
  vio_vfd vfd ;

  vfd = XCALLOC(MTYPE_VTY, sizeof(struct vio_vfd)) ;

  /* Has set:
   *
   *   active         -- false !
   *
   *   read_action    -- NULL
   *   write_action   -- NULL
   *
   *   f.qf           -- NULL
   *   f.thread.read  -- NULL
   *   f.thread.write -- NULL
   *
   *   mqb            -- NULL
   */

  vio_vfd_set_fd(vfd, fd, type, io_type) ;

  vio_timer_init(&vfd->read_timer, NULL, NULL) ;
  vio_timer_init(&vfd->write_timer, NULL, NULL) ;

  vio_vfd_set_action_info(vfd, action_info) ;

  return vfd ;
} ;

/*------------------------------------------------------------------------------
 * If vfd was not fully set up when created, set it up now.
 *
 * To close an active vfd, use vio_vfd_close() !
 *
 * NB: for use when vfd has been created, but the fd was not known at that
 *     time -- ie the vfd is NOT active.
 */
extern void
vio_vfd_set_fd(vio_vfd vfd, int fd, vfd_type_t type, vfd_io_type_t io_type)
{
  assert(!vfd->active) ;

  vfd->fd          = fd ;
  vfd->active      = (fd >= 0) ;
  vfd->type        = type ;
  vfd->io_type     = io_type ;
} ;

/*------------------------------------------------------------------------------
 * Set the read action field for the given vio_vfd.
 */
extern void
vio_vfd_set_read_action(vio_vfd vfd, vio_vfd_action* action)
{
  vfd->read_action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the write action field for the given vio_vfd.
 */
extern void
vio_vfd_set_write_action(vio_vfd vfd, vio_vfd_action* action)
{
  vfd->write_action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the read action field for the given vio_vfd.
 */
extern void
vio_vfd_set_read_timeout_action(vio_vfd vfd, vio_timer_action* action)
{
  vio_timer_set_action(&vfd->read_timer, action) ;
} ;

/*------------------------------------------------------------------------------
 * Set the write action field for the given vio_vfd.
 */
extern void
vio_vfd_set_write_timeout_action(vio_vfd vfd, vio_timer_action* action)
{
  vio_timer_set_action(&vfd->write_timer, action) ;
} ;

/*------------------------------------------------------------------------------
 * Set the action_info field for the given vio_vfd read/write action.
 */
extern void
vio_vfd_set_action_info(vio_vfd vfd, void* action_info)
{
  vfd->action_info = action_info ;
  vio_timer_set_info(&vfd->read_timer, action_info) ;
  vio_timer_set_info(&vfd->write_timer, action_info) ;
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
 * If the vfd is a socket, then does a shutdown of the read side.
 *
 * If the vfd is not a socket and is read (only) closes the vfd.
 *
 * In any case, turns off any read ready and read ready timeout.
 *
 * Returns original vfd, or NULL if it has been closed.
 */
extern vio_vfd
vio_vfd_read_close(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd == NULL)
    return NULL ;

  if (vfd->fd >= 0)
    {
      assert(vfd->active) ;

      if (vfd->io_type & vfd_io_read)
        {
          if (vfd->io_type & vfd_io_write)
            {
              /* read & write, so really half-close if can              */
              if (vfd->type == vfd_socket)
                shutdown(vfd->fd, SHUT_RD) ;    /* ignore errors TODO   */
              vio_vfd_set_read(vfd, off, 0) ;
              vfd->io_type ^= vfd_io_read ;         /* now write only ! */
            }
          else
            {
              /* read only, so fully close                              */
              vfd = vio_vfd_close(vfd) ;
            } ;
        } ;
    }
  else
    assert(!vfd->active) ;

  return vfd ;
} ;

/*------------------------------------------------------------------------------
 * If there is an fd, close it.
 *
 * Stops any read/write waiting and releases all memory, including the vfd
 * itself if required..
 */
static void
vio_vfd_do_close(vio_vfd vfd, free_keep_b free)
{
  if (vfd == NULL)
    return ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (vty_nexus)
    {
      if (vfd->f.qf != NULL)
        {
          assert(vfd->fd == qps_file_fd(vfd->f.qf)) ;
          vfd->f.qf = qps_file_free(vfd->f.qf) ;
        } ;

      if (vfd->mqb != NULL)
        {
          mqb_free(vfd->mqb) ;
          vfd->mqb = NULL ;
        } ;
    }
  else
    {
      if (vfd->f.thread.read != NULL)
        {
          assert(vfd->fd == THREAD_FD(vfd->f.thread.read)) ;
          thread_cancel(vfd->f.thread.read) ;
          vfd->f.thread.read  = NULL ;
        } ;

      if (vfd->f.thread.write != NULL)
        {
          assert(vfd->fd == THREAD_FD(vfd->f.thread.write)) ;
          thread_cancel(vfd->f.thread.write) ;
          vfd->f.thread.write = NULL ;
        } ;

      assert(vfd->mqb == NULL) ;
    } ;

  if (vfd->fd >= 0)
    close(vfd->fd) ;            /* ignores errors TODO  */

  vio_timer_reset(&vfd->read_timer) ;
  vio_timer_reset(&vfd->write_timer) ;

  if (free == free_it)
    XFREE(MTYPE_VTY, vfd) ;
} ;

/*------------------------------------------------------------------------------
 * Close the given vfd (if any).
 *
 * If there is an fd, close it.  Stops any read/write waiting and releases all
 * memory.
 *
 * NB: this can done from any thread, but if not done from the CLI thread,
 *     and there is a qf, a message must be sent to the CLI thread to actually
 *     implement: which passes the vio_vfd to the CLI thread for later
 *     close and destruction.
 *
 *     The actual close has to be delayed, so that cannot open another fd
 *     and bang into a still active qps_file !
 *
 *     The message looks after freeing the vio_vfd, the qps_file and the mqb.
 */
extern vio_vfd
vio_vfd_close(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd == NULL)
    return NULL ;

  if (vfd->fd < 0)
    {
      /* closing an inactive vio_vfd -- make sure all is quiet           */
      assert(!vfd->active) ;
      if (vty_nexus)
        {
          assert(vfd->f.qf == NULL) ;
        }
      else
        {
          assert(vfd->f.thread.read  == NULL) ;
          assert(vfd->f.thread.write == NULL) ;
        } ;
      assert(vfd->mqb == NULL) ;
    }
  else
    {
      /* closing an active vio_vfd                                       */
      if (vty_is_cli_thread())
        {
          /* In cli thread, so close directly                           */
          vio_vfd_do_close(vfd, free_it) ;
        }
      else
        {
          /* Rats... have to send message to cli thread to close        */
          struct vio_io_set_args* args = vio_vfd_mqb_args(vfd) ;

          args->close = true ;

          /* in case something goes ready before the close message
           * is processed, squelch.
           */
          vfd->active       = false ;
          vfd->read_action  = NULL ;
          vfd->write_action = NULL ;
          vfd->action_info  = NULL ;

          vio_timer_squelch(&vfd->read_timer) ;
          vio_timer_squelch(&vfd->write_timer) ;

          assert(vfd == mqb_get_arg0(vfd->mqb)) ;
          vio_vfd_mqb_dispatch(vfd) ;
        } ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set or unset read ready state on given vio_vfd (if any) if it is active.
 *
 * If setting read_on, starts any read timeout timer (or stops it if 0).
 * If setting read off, stops any read timeout timer.
 *
 * NB: this can done from any thread, but if not done from the CLI thread,
 *     a message must be sent to the CLI thread to actually implement.
 */
extern on_off_b
vio_vfd_set_read(vio_vfd vfd, on_off_b on, vty_timer_time timeout)
{
  struct vio_io_set_args* args ;

  VTY_ASSERT_LOCKED() ;

  if ((vfd == NULL) || (!vfd->active))
    return off ;

  if (vty_is_cli_thread())
    {
      /* In the cli thread (effectively) so do things directly.         */

      if (vfd->mqb != NULL)
        {
          /* discard/override any pending message setting               */
          args = mqb_get_args(vfd->mqb) ;
          args->readable = false ;
        } ;

      if (on)
        {
          assert(vfd->read_action != NULL) ;

          if (vty_nexus)
            {
              if (vfd->f.qf == NULL)
                {
                  vfd->f.qf = qps_file_init_new(NULL, NULL);
                  qps_add_file(vty_cli_nexus->selection, vfd->f.qf,
                                                           vfd->fd, vfd) ;
                } ;
              qps_enable_mode(vfd->f.qf, qps_read_mnum,
                                                       vio_vfd_qps_read_action) ;
            }
          else
            {
              if (vfd->f.thread.read == NULL)
                vfd->f.thread.read = thread_add_read(vty_master,
                                      vio_vfd_thread_read_action, vfd, vfd->fd) ;
            } ;

          vio_timer_set(&vfd->read_timer, timeout) ;
        }
      else
        {
          if (vty_nexus)
            {
              if (vfd->f.qf != NULL)
                qps_disable_modes(vfd->f.qf, qps_read_mbit) ;
            }
          else
            {
              if (vfd->f.thread.read != NULL)
                thread_cancel (vfd->f.thread.read) ;
            } ;

          vio_timer_unset(&vfd->read_timer) ;
        } ;
    }
  else
    {
      /* In other threads, must send message to cli thread              */

      args = vio_vfd_mqb_args(vfd) ;
      args->readable     = true ;
      args->read_on      = on ;
      args->read_timeout = timeout ;
      vio_timer_squelch(&vfd->read_timer) ;
      vio_vfd_mqb_dispatch(vfd) ;
    } ;

  return on ;
} ;

/*------------------------------------------------------------------------------
 * Set or unset write ready state on given vio_vfd (if any) if it is active.
 *
 * If setting write_on, starts any write timeout timer.
 * If setting write off, stops any write timeout timer.
 *
 * NB: this can done from any thread, but if not done from the CLI thread,
 *     a message must be sent to the CLI thread to actually implement.
 */
extern on_off_b
vio_vfd_set_write(vio_vfd vfd, on_off_b on, vty_timer_time timeout)
{
  struct vio_io_set_args* args ;

  VTY_ASSERT_LOCKED() ;

  if ((vfd == NULL) || (!vfd->active))
    return off ;

  if (vty_is_cli_thread())
    {
      /* In the cli thread (effectively) so do things directly.         */

      if (vfd->mqb != NULL)
        {
          /* discard/override any pending message setting               */
          args = mqb_get_args(vfd->mqb) ;
          args->writable = false ;
        } ;

      if (on)
        {
          assert(vfd->write_action != NULL) ;

          if (vty_nexus)
            {
              if (vfd->f.qf == NULL)
                {
                  vfd->f.qf = qps_file_init_new(NULL, NULL);
                  qps_add_file(vty_cli_nexus->selection, vfd->f.qf,
                                                           vfd->fd, vfd) ;
                } ;
              qps_enable_mode(vfd->f.qf, qps_write_mnum,
                                                     vio_vfd_qps_write_action) ;
            }
          else
            {
              if (vfd->f.thread.write == NULL)
                vfd->f.thread.write = thread_add_write(vty_master,
                                    vio_vfd_thread_write_action, vfd, vfd->fd) ;
            } ;

          vio_timer_set(&vfd->write_timer, timeout) ;
        }
      else
        {
          if (vty_nexus)
            {
              if (vfd->f.qf != NULL)
                qps_disable_modes(vfd->f.qf, qps_write_mbit) ;
            }
          else
            {
              if (vfd->f.thread.write != NULL)
                thread_cancel (vfd->f.thread.write) ;
            } ;

          vio_timer_unset(&vfd->write_timer) ;
        } ;
    }
  else
    {
      /* In other threads, must send message to cli thread              */

      args = vio_vfd_mqb_args(vfd) ;
      args->writable      = true ;
      args->write_on      = on ;
      args->write_timeout = timeout ;
      vio_timer_squelch(&vfd->write_timer) ;
      vio_vfd_mqb_dispatch(vfd) ;
    } ;

  return on ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to read
 *
 * Clears read ready state and unsets any read timer.
 *
 * NB: if !vfd->active, then has been closed in another thread, but close
 *     message is yet to be procesed.
 */
static void
vio_vfd_qps_read_action(qps_file qf, void* file_info)
{
  vio_vfd vfd ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  vfd = file_info ;

  assert((vfd->fd == qf->fd) && (vfd->f.qf == qf)) ;

  qps_disable_modes(vfd->f.qf, qps_read_mbit) ;
  vio_timer_unset(&vfd->read_timer) ;

  vio_vfd_do_read_action(vfd) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: ready to read
 *
 * Clears read ready state and unsets any read timer.
 *
 * NB: if !vfd->active, then has been closed in another thread, but close
 *     message is yet to be procesed.
 */
static int
vio_vfd_thread_read_action(struct thread *thread)
{
  vio_vfd vfd ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  vfd = THREAD_ARG(thread);

  assert(vfd->fd == THREAD_FD(thread)) ;

  vfd->f.thread.read = NULL ;           /* implicitly   */
  vio_timer_unset(&vfd->read_timer) ;

  vio_vfd_do_read_action(vfd) ;

  VTY_UNLOCK() ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to write
 *
 * Clears write ready state and unsets any write timer.
 *
 * NB: if !vfd->active, then has been closed in another thread, but close
 *     message is yet to be procesed.
 */
static void
vio_vfd_qps_write_action(qps_file qf, void* file_info)
{
  vio_vfd vfd = file_info ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  assert((vfd->fd == qf->fd) && (vfd->f.qf == qf)) ;

  qps_disable_modes(vfd->f.qf, qps_write_mbit) ;
  vio_timer_unset(&vfd->write_timer) ;

  vio_vfd_do_write_action(vfd) ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: ready to write
 *
 * Clears write ready state and unsets any write timer.
 *
 * NB: if !vfd->active, then has been closed in another thread, but close
 *     message is yet to be procesed.
 */
static int
vio_vfd_thread_write_action(struct thread *thread)
{
  vio_vfd vfd = THREAD_ARG (thread);

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  assert(vfd->fd == THREAD_FD(thread)) ;
  vio_timer_unset(&vfd->write_timer) ;

  vfd->f.thread.write = NULL ;          /* implicitly   */

  vio_vfd_do_write_action(vfd) ;

  VTY_UNLOCK() ;
  return 0 ;
} ;

/*==============================================================================
 * Message handling, so that other threads can signal for output to be
 * dispatched !
 *
 * There is one message block per vfd.  It is only every touched under the
 * vty_mutex.
 *
 * Once it is dispatched it is marked 'active'.  Can still be changed, but no
 * further dispatch is required.  When it has been dequeued and processed,
 * it is marked inactive.
 *
 * If the vfd is closed while the message is active, it is marked to close,
 * which it will do when it is dequeued and actioned.
 */

static void vio_vfd_set_action(mqueue_block mqb, mqb_flag_t flag) ;

/*------------------------------------------------------------------------------
 * Get mqb for the given vfd -- make one if required.
 */
static struct vio_io_set_args*
vio_vfd_mqb_args(vio_vfd vfd)
{
  VTY_ASSERT_LOCKED() ;

  if (vfd->mqb == NULL)
    vfd->mqb = mqb_init_new(NULL, vio_vfd_set_action, vfd) ;

  return mqb_get_args(vfd->mqb) ;
} ;

/*------------------------------------------------------------------------------
 * Dispatch mqb, if not already active
 */
static void
vio_vfd_mqb_dispatch(vio_vfd vfd)
{
  struct vio_io_set_args* args = mqb_get_args(vfd->mqb) ;

  VTY_ASSERT_LOCKED() ;

  if (!args->active)
    {
      args->active  = true ;
      mqueue_enqueue(vty_cli_nexus->queue, vfd->mqb, mqb_ordinary) ;
    } ;
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
vio_vfd_set_action(mqueue_block mqb, mqb_flag_t flag)
{
  struct vio_io_set_args* args ;
  vio_vfd                 vfd ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  args = mqb_get_args(mqb) ;
  vfd  = mqb_get_arg0(mqb) ;

  args->active = false ;

  if ((flag != mqb_destroy) && (!args->close))
    {
      if (args->readable)
        vio_vfd_set_read(vfd, args->read_on, args->read_timeout) ;
      if (args->writable)
        vio_vfd_set_write(vfd, args->write_on, args->write_timeout) ;
    }
  else
    {
      /* Revoke and/or close.
       *
       * If close can free the vfd.
       *
       * If just revoke (should not happen), then cannot free the vfd, because
       * somewhere there can be a dangling reference.
       */
      vio_vfd_do_close(vfd, args->close) ;         /* frees the mqb        */
    } ;

  VTY_UNLOCK() ;
} ;

/*==============================================================================
 * Listener Handling
 *
 *
 */

static void vio_accept(vio_vfd vfd, void* info) ;

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

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

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
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vio_vfd_close(listener->vfd) ;
  XFREE(MTYPE_VTY, listener) ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- this is the read_action from the listener vfd.
 *
 * info points at the listener object.
 */
static void
vio_accept(vio_vfd vfd, void* info)
{
  vio_listener listener ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  listener = info ;
  assert(vfd == listener->vfd) ;

  vio_vfd_set_read(listener->vfd, on, 0) ;

  listener->accept_action(vfd->fd) ;
} ;

/*==============================================================================
 * Timer Handling
 *
 * Provides timer primitives that work either in qnexus environment or in
 * a thread environment.
 *
 * The main difference is that thread environment timers are 'one-shot', set up
 * for one timing run and then destroyed.
 */

static void vio_timer_qtr_action(qtimer qtr, void* timer_info, qtime_t when) ;
static int vio_timer_thread_action(struct thread *thread) ;

/*------------------------------------------------------------------------------
 * Initialise vio_timer structure.  Assumes is all new.
 *
 * This assumes the vio_timer structure is embedded in another structure.
 */
extern void
vio_timer_init(vio_timer_t* timer, vio_timer_action* action, void* action_info)
{
  memset(timer, 0, sizeof(vio_timer_t)) ;

  /* active     -- 0, false
   * squelch    -- 0, false
   * t          -- NULL, no qtr and no thread
   */

  timer->action      = action ;
  timer->action_info = action_info ;
} ;

/*------------------------------------------------------------------------------
 * Set the action field for the given timer.
 */
extern void
vio_timer_set_action(vio_timer_t* timer, vio_timer_action* action)
{
  timer->action = action ;
} ;

/*------------------------------------------------------------------------------
 * Set the info field for the given timer.
 */
extern void
vio_timer_set_info(vio_timer_t* timer, void* action_info)
{
  timer->action_info = action_info ;
} ;

/*------------------------------------------------------------------------------
 * Kill vio_timer -- used when closing .
 */
static void
vio_timer_squelch(vio_timer_t* timer)
{
  VTY_ASSERT_LOCKED() ;

  timer->squelch = true ;
} ;

/*------------------------------------------------------------------------------
 * Reset vio_timer structure.  Stops any timer and releases all memory.
 *
 * This assumes the vio_timer structure is embedded in another structure.
 */
extern void
vio_timer_reset(vio_timer_t* timer)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  if (timer->t.anon != NULL)
    {
      if (vty_nexus)
        qtimer_free(timer->t.qtr) ;
      else
        thread_cancel(timer->t.thread) ;

      timer->t.anon = NULL ;
    } ;

  timer->active  = false ;
  timer->squelch = false ;
} ;

/*------------------------------------------------------------------------------
 * Set vio_timer going, with the given time.
 *
 * If timer is running, set to new time.
 *
 * If the time == 0, stop any current timer, do not restart.
 */
extern void
vio_timer_set(vio_timer_t* timer, vty_timer_time time)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

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
      timer->t.thread = thread_add_timer(vty_master,
                                         vio_timer_thread_action, timer, time) ;
    } ;

  timer->active  = true ;
  timer->squelch = false ;
} ;

/*------------------------------------------------------------------------------
 * Stop vio_timer, if any.
 */
extern void
vio_timer_unset(vio_timer_t* timer)
{
  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

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
  vio_timer_t* timer ;
  vty_timer_time time ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  timer = timer_info ;

  if ((timer->action != NULL) && (!timer->squelch))
    {
      time = timer->action(timer, timer->action_info) ;
      if (time != 0)
        vio_timer_set(timer, time) ;
      else
        timer->active = false ;
    }
  else
    {
      timer->squelch = false ;
      timer->active  = false ;
    } ;

  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Callback -- thread: deal with timer timeout.
 */
static int
vio_timer_thread_action(struct thread *thread)
{
  vio_timer_t* timer ;
  vty_timer_time time ;

  VTY_ASSERT_CLI_THREAD() ;
  VTY_LOCK() ;

  timer = THREAD_ARG(thread) ;
  timer->t.thread = NULL ;      /* implicitly                   */

  if ((timer->action != NULL) && (!timer->squelch))
    {
      time = timer->action(timer, timer->action_info) ;
      if (time != 0)
        vio_timer_set(timer, time) ;
      else
        timer->active = false ;
    }
  else
    {
      timer->squelch = false ;
      timer->active  = false ;
    } ;

  VTY_UNLOCK() ;
  return 0;
} ;

/* VTY IO Structure and Functions -- header
 * Virtual terminal [aka TeletYpe] interface routine.
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

#ifndef _ZEBRA_VTY_IO_BASIC_H
#define _ZEBRA_VTY_IO_BASIC_H

#include "misc.h"

#include "vty.h"

#include "qpselect.h"
#include "thread.h"
#include "mqueue.h"

/*==============================================================================
 * Here are structures and other definitions which are shared by all the
 * VTY I/O hierarchy, providing the basic read/write ready functions and
 * timer functions.
 *
 * This is separated out so that the differences between running in a qpnexus
 * and an old thread environment are encapsulated here.
 */

enum vfd_type
{
  vfd_none      = 0,
  vfd_socket,
  vfd_file,
  vfd_pipe,
  vfd_listener,
} ;
typedef enum vfd_type vfd_type_t ;

enum vfd_io_type                /* NB: *bit*significant*        */
{
  vfd_io_none       = 0,
  vfd_io_read       = 1,
  vfd_io_write      = 2,
  vfd_io_read_write = 3,
} ;
typedef enum vfd_io_type vfd_io_type_t ;

/*------------------------------------------------------------------------------
 * Timers -- implemented as qtimer or thread timer, depending on environment.
 */
typedef struct vio_timer vio_timer_t ;
typedef vty_timer_time vio_timer_action(vio_timer_t* timer, void* action_info) ;

struct vio_timer
{
  vio_timer_action* action ;    /* who do we call               */
  void*             action_info ;

  bool              active ;
  bool              squelch ;   /* used when message pending    */

  union {
    qtimer          qtr ;       /* when running qnexus          */
    struct thread*  thread ;    /* when running threads         */
    void*           anon ;
  } t ;
} ;

/*------------------------------------------------------------------------------
 * File descriptors -- looks after ready to read and/or write.
 *
 * Implemented as qps_file or as read/write thread, depending on the
 * environment.
 */
typedef struct vio_fd* vio_fd ;
typedef void vio_fd_action(vio_fd vfd, void* action_info) ;

struct vio_fd
{
  int             fd ;
  bool            active ;      /* used for close message               */

  vfd_type_t      type ;        /* used for half-close                  */
  vfd_io_type_t   io_type ;     /* read, write, read/write              */

  vio_fd_action*  read_action ;
  vio_fd_action*  write_action ;

  vio_timer_t     read_timer ;
  vio_timer_t     write_timer ;

  void*           action_info ; /* for all action and time-out          */

  union
  {
    qps_file qf ;               /* when running qnexus                  */

    struct                      /* when running threads                 */
    {
      struct thread* read ;
      struct thread* write ;
    } thread ;
  } f ;

  mqueue_block  mqb ;
} ;

/*------------------------------------------------------------------------------
 * Listeners
 */

typedef struct vio_listener* vio_listener ;

typedef void vio_fd_accept(int fd) ;

struct vio_listener
{
  vio_listener    next ;        /* ssl type list        */
  vio_fd          vfd ;
  vio_fd_accept*  accept_action ;
};

/*==============================================================================
 * Functions
 */

extern vio_fd vio_fd_new(int fd, vfd_type_t type,
                                     vfd_io_type_t io_type, void* action_info) ;
extern void vio_fd_set_fd(vio_fd vfd, int fd, vfd_type_t type,
                                                        vfd_io_type_t io_type) ;
extern void vio_fd_set_read_action(vio_fd vfd, vio_fd_action* action) ;
extern void vio_fd_set_write_action(vio_fd vfd, vio_fd_action* action) ;
extern void vio_fd_set_read_timeout_action(vio_fd vfd,
                                                     vio_timer_action* action) ;
extern void vio_fd_set_write_timeout_action(vio_fd vfd,
                                                     vio_timer_action* action) ;
extern void vio_fd_set_action_info(vio_fd vfd, void* action_info) ;
extern vio_fd vio_fd_half_close(vio_fd vfd) ;
extern vio_fd vio_fd_close(vio_fd vfd) ;
extern on_off_t vio_fd_set_read(vio_fd vfd, on_off_t on,
                                                       vty_timer_time timeout) ;
extern on_off_t vio_fd_set_write(vio_fd vfd, on_off_t on,
                                                       vty_timer_time timeout) ;
Inline int vio_fd_fd(vio_fd vfd) ;

extern vio_listener vio_listener_new(int fd, vio_fd_accept* accept) ;
extern void vio_listener_close(vio_listener listener) ;

extern void vio_timer_init(vio_timer_t* timer, vio_timer_action* action,
                                                            void* action_info) ;
extern void vio_timer_set_action(vio_timer_t* timer, vio_timer_action* action) ;
extern void vio_timer_set_info(vio_timer_t* timer, void* action_info) ;
extern void vio_timer_reset(vio_timer_t* timer) ;
extern void vio_timer_set(vio_timer_t* timer, vty_timer_time time) ;
extern void vio_timer_unset(vio_timer_t* timer) ;

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Return the fd from a vio_fd structure
 */
Inline int
vio_fd_fd(vio_fd vfd)
{
  return vfd->fd ;
} ;

#endif /* _ZEBRA_VTY_IO_BASIC_H */

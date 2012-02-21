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
  vfd_io_none         = 0,

  vfd_io_read         = BIT(0),
  vfd_io_write        = BIT(1),
  vfd_io_read_write   = vfd_io_read | vfd_io_write,

  vfd_io_append       = BIT(2), /* ignored if not vfd_io_write  */
  vfd_io_excl         = BIT(3), /* ignored if not vfd_io_write  */

  vfd_io_os_blocking  = BIT(4), /* genuine, O/S level blocking  */

  vfd_io_ps_blocking  = BIT(5), /* pseudo blocking              */

  vfd_io_blocking     = vfd_io_os_blocking | vfd_io_ps_blocking,

  vfd_io_no_close     = BIT(7),
} ;
typedef enum vfd_io_type vfd_io_type_t ;

/*------------------------------------------------------------------------------
 * Timers -- implemented as qtimer or thread timer, depending on environment.
 *
 * Timer action function returns a new value for the timer; 0 => off.
 */
typedef unsigned long vio_timer_time ;  /* Time out time in seconds     */

typedef struct vio_timer* vio_timer ;
typedef vio_timer_time vio_timer_action(vio_timer timer, void* action_info) ;

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
typedef struct vio_timer vio_timer_t[1] ;

enum { VIO_TIMER_INIT_ZERO = true } ;

/*------------------------------------------------------------------------------
 * File descriptors -- looks after ready to read and/or write.
 *
 * Implemented as qps_file or as read/write thread, depending on the
 * environment.
 */
typedef struct vio_vfd  vio_vfd_t ;
typedef struct vio_vfd* vio_vfd ;

typedef void vio_vfd_action(vio_vfd vfd, void* action_info, bool time_out) ;

typedef struct
{
  bool            set ;
  on_off_b        how ;
  vio_timer_time  timeout ;
} vfd_request_t ;

struct vio_vfd
{
  int             fd ;

  vfd_type_t      type ;        /* used for half-close                  */
  vfd_io_type_t   io_type ;     /* read, write, read/write              */

  /* The rest of the vfd is to do with managing read/write ready and
   * read/write timeouts for *non* blocking vfd.
   *
   * Non-blocking vfd may only be created and closed in the cli thread (or
   * when running in the legacy threads environment).  This is because can
   * only dismantle qps_file structure while in the owning thread.
   */
  void*           action_info ; /* for all action and time-out          */

  vio_vfd_action* read_action ;
  vio_vfd_action* write_action ;

  vio_timer       read_timer ;
  vio_timer       write_timer ;

  union
  {
    qps_file qf ;               /* when running qnexus                  */

    struct                      /* when running threads                 */
    {
      struct thread* read ;
      struct thread* write ;
    } thread ;
  } f ;

  /* To support remote setting clearing of read/write ready and read/write
   * timeout -- by remote we mean anything such action not in the cli thread.
   *
   * This is required because the qpselect and qtimer stuff assumes that those
   * structures are private to the thread whose qnexus they belong to !
   *
   * This stuff is only required when running multi-pthreaded.
   */
  bool  queued ;                /* message is on queue                  */

  vfd_request_t read_req ;
  vfd_request_t write_req ;

  mqueue_block  mqb ;           /* message if any                       */
} ;

/*------------------------------------------------------------------------------
 * Listeners
 */

typedef struct vio_listener* vio_listener ;

typedef void vio_vfd_accept(int fd) ;

struct vio_listener
{
  vio_listener    next ;        /* ssl type list        */
  vio_vfd         vfd ;
  vio_vfd_accept* accept_action ;
};

/*------------------------------------------------------------------------------
 * Primitive line imaging
 */
enum { vty_line_image_unit  = 32 * 1024 } ;

typedef struct
{
  byte*   buffer ;      /* MTYPE_TMP    */

  byte*   kept ;
  byte*   end ;

  usize   size ;

  char*   name ;        /* MTYPE_TMP    */
  int     fd ;

  const char* when ;    /* "opening"/"reading"/...      */
  int     err ;         /* error number when failed     */

} vty_line_image_t ;

typedef vty_line_image_t* vty_line_image ;

/*==============================================================================
 * Functions
 */
extern int uty_fd_file_open(const char* name, vfd_io_type_t io_type,
                                                                 mode_t cmode) ;
extern vty_line_image uty_fd_line_image_open(const char* name) ;
extern int uty_fd_line_image_read(vty_line_image vli) ;
extern vty_line_image uty_fd_line_image_close(vty_line_image vli) ;

extern vio_vfd vio_vfd_new(int fd, vfd_type_t type,
                                   vfd_io_type_t io_type, void* action_info) ;
extern void vio_vfd_set_read_action(vio_vfd vfd, vio_vfd_action* action) ;
extern void vio_vfd_set_write_action(vio_vfd vfd, vio_vfd_action* action) ;
extern void vio_vfd_set_action_info(vio_vfd vfd, void* action_info) ;
extern vio_vfd vio_vfd_read_close(vio_vfd vfd) ;
extern vio_vfd vio_vfd_close(vio_vfd vfd) ;
extern on_off_b vio_vfd_set_read(vio_vfd vfd, on_off_b how,
                                                       vio_timer_time timeout) ;
extern on_off_b vio_vfd_set_write(vio_vfd vfd, on_off_b how,
                                                       vio_timer_time timeout) ;
Inline int vio_vfd_fd(vio_vfd vfd) ;
Inline bool vio_vfd_blocking(vio_vfd vfd) ;

extern vio_listener vio_listener_new(int fd, vio_vfd_accept* accept) ;
extern void vio_listener_close(vio_listener listener) ;

extern vio_timer vio_timer_init_new(vio_timer timer, vio_timer_action* action,
                                                            void* action_info) ;
extern vio_timer vio_timer_reset(vio_timer timer, free_keep_b free_structure) ;
extern void vio_timer_set_action(vio_timer timer, vio_timer_action* action) ;
extern void vio_timer_set_info(vio_timer timer, void* action_info) ;
extern void vio_timer_set(vio_timer timer, vio_timer_time time) ;
extern void vio_timer_unset(vio_timer timer) ;

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Return the fd from a vio_vfd structure
 */
Inline int
vio_vfd_fd(vio_vfd vfd)
{
  return (vfd != NULL) ? vfd->fd : -1 ;
} ;

/*------------------------------------------------------------------------------
 * Whether the given vfd is vfd_io_os_blocking and/or vfd_io_ps_blocking
 *
 * If there is no vfd, return as blocking !
 */
Inline bool
vio_vfd_blocking(vio_vfd vfd)
{
  return (vfd == NULL) || ((vfd->io_type & vfd_io_blocking) != 0) ;
}

#endif /* _ZEBRA_VTY_IO_BASIC_H */

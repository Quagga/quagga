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

#ifndef _ZEBRA_VTY_IO_H
#define _ZEBRA_VTY_IO_H

//#include "zebra.h"
#include "misc.h"
//#include <errno.h>

#include "vty_local.h"
#include "command_local.h"
#include "vty_io_basic.h"
#include "vio_fifo.h"
#include "thread.h"
#include "command_execute.h"
#include "qstring.h"
#include "list_util.h"

/*==============================================================================
 * Structures and other definitions for the top level VTY I/O.
 *
 * There is one struct vty_io per VTY, which contains, inter alia, the vin
 * and vout stacks.
 *
 * The vin and vout stacks contain one or more struct vty_vf -- one per
 * input and/or output associated with the VTY.
 */

enum
{
  VTY_WATCH_DOG_INTERVAL    =   5,      /* interval between barks       */

  VTY_HALF_CLOSE_TIMEOUT    = 120,      /* timeout after half_close     */

  VTY_TIMEOUT_DEFAULT       = 600,      /* terminal timeout value       */
} ;

/*------------------------------------------------------------------------------
 * VTY VIN and OUT types
 */
enum vio_in_type        /* Command input                                */
{
  VIN_NONE  = 0,        /* not a valid input type                       */

  VIN_TERM,             /* telnet terminal                              */
  VIN_VTYSH,            /* vty_shell input                              */

  VIN_FILE,             /* ordinary file input                          */
  VIN_PIPE,             /* pipe (from child process)                    */

  VIN_CONFIG,           /* config file                                  */

  VIN_PIPE_RETURN,      /* */

  /* The VIN types >= VIN_SPECIALS do not have an associated fd.
   *
   * These can coexist with a VOUT which does have an associated fd.
   */
  VIN_SPECIALS,         /* all special from now on                      */

  VIN_DEV_NULL = VIN_SPECIALS,
                        /* black hole input                             */
} ;
typedef enum vio_in_type vio_in_type_t ;

enum vio_out_type       /* Command output                               */
{
  VOUT_NONE  = 0,       /* not a valid output type                      */

  VOUT_TERM,            /* a telnet terminal                            */
  VOUT_VTYSH,           /* a vty_shell output pipe                      */

  VOUT_FILE,            /* ordinary file                                */
  VOUT_PIPE,            /* pipe (to child process, and back again)      */

  VOUT_CONFIG,          /* config file                                  */

  /* The VOUT types >= VOUT_SPECIALS do not have an associated fd.
   *
   * These can coexist with a VIN which does have an associated fd.
   */
  VOUT_SPECIALS,        /* all special from now on                      */

  VOUT_DEV_NULL = VOUT_SPECIALS,
                        /* black hole output                            */

  VOUT_SHELL_ONLY,      /* pipe (but only back from child process       */

  VOUT_STDOUT,          /* stdout                                       */
  VOUT_STDERR,          /* stderr                                       */
};
typedef enum vio_out_type vio_out_type_t ;

/*------------------------------------------------------------------------------
 * State of a vf -- has separate state for vin/vout.
 */
enum vf_state
{
  vf_closed  = 0,       /* the vf has not been opened, or has been
                         * completely closed -- there will be no vfd.   */

  vf_open,              /* the vf has been opened, and any required vfd
                         * is open and I/O is possible.                 */

  vf_eof,               /* for a vin: end of file has been reached or
                         * input has been terminated.  The vfd may have
                         * been closed -- but in any case no further
                         * I/O should be attempted, and any buffered
                         * input will be discarded.                     */

  vf_closing,           /* for a vout: open, but in process of closing.
                         * May still output stuff.                      */

  vf_error,             /* an I/O error has been reported.
                         * The vfd may or may not have been closed and
                         * I/O may or may not be possible TODO          */

  vf_timed_out,         /* a timout has been reported.
                         * The vfd may or may not have been closed and
                         * I/O may or may not be possible TODO          */
} ;
typedef enum vf_state vf_state_t ;

/*------------------------------------------------------------------------------
 * vio_child structure.
 *
 * Lives on the vio_childer_list until collected or "curtains".
 *
 */
typedef enum vio_child_await vio_child_await_t ;

struct vio_vf ;                 /* Forward reference                    */
typedef struct vio_vf* vio_vf ;

typedef struct vio_child* vio_child ;

struct vio_child
{
  struct dl_list_pair(vio_child) list ; /* in the list of children      */

  vio_vf      parent ;

  pid_t       pid ;
  bool        collected ;       /* waitpid() done                       */
  int         report ;          /* from waitpid()                       */

  bool        overdue ;         /* patience exhausted                   */

  bool        awaited ;         /* if child is awaited -- !vf->blocking */
  vio_timer   timer ;           /* limit the waiting time               */
} ;

/*------------------------------------------------------------------------------
 * vty_vf -- "vty file" structure
 *
 * A vio_vf may be a read, write or read/write object.
 *
 * All I/O is via vio_vfd objects, except for VOUT_STDOUT and VOUT_STDERR.
 * The vio_vfd layer hides the differences between the qpthreads an legacy
 * thread environments.
 *
 * The VOUT_STDOUT and VOUT_STDERR are handled as direct output to the standard
 * i/o file handles.  In the case of a VTY_CONFIG_READ, the vin is VIN_CONFIG
 * and the vout is VOUT_STDOUT, and these can share a single vty_vf.
 *
 * Also used for the associated listeners.
 */
struct vty_io ;                 /* Forward reference                    */
typedef struct vty_io* vty_io ;

struct vio_vf
{
  vty_io      vio ;             /* parent                               */

  char*       name ;            /* MTYPE_VTY_NAME (if any)              */

  /* Input side.                                                        */

  vio_in_type_t   vin_type ;
  vf_state_t      vin_state ;
  vio_vf          vin_next ;    /* list of inputs                       */

  cmd_context     context ;     /* pushed exec->context.                */

  struct vty_cli* cli ;         /* NULL if not a VTY_TERMINAL !         */

  vio_fifo    ibuf ;            /* input fifo (if required)             */

  qstring     cl ;              /* command line buffer                  */
  bool        line_complete ;   /* false => line in construction        */
  uint        line_number ;     /* number of first line in cl           */
  uint        line_step ;       /* number of real lines in cl           */

  /* Output side.                                                       */

  vio_out_type_t  vout_type ;
  vf_state_t      vout_state ;
  vio_vf          vout_next ;   /* list of outputs                      */

  vio_vf      pr_master ;       /* receiving return stuff from there    */

  vio_fifo    obuf ;            /* output fifo (if required)            */

  uint        depth_mark ;      /* depth of this vout                   */

  /* I/O                                                                */

  vio_vfd     vfd ;             /* vty_io_basic "file descriptor"       */

  bool        blocking ;        /* using blocking I/O (config read)     */

  int         error_seen ;      /* non-zero => failed                   */

  vty_timer_time  read_timeout ;
  vty_timer_time  write_timeout ;

  /* Pipe extras                                                        */

  vio_child   child ;           /* state of child                       */

  vf_state_t  pr_state ;        /* iff pipe                             */
  vio_vfd     pr_vfd ;          /* if pr_state == vf_open               */

  vio_vf      pr_slave ;        /* sending return stuff to there        */

  bool        pr_only ;         /* no vfd                               */
} ;

enum vty_readiness              /* bit significant      */
{
  not_ready     = 0,
  read_ready    = BIT(0),
  write_ready   = BIT(1),       /* may take precedence  */
} ;
typedef enum vty_readiness vty_readiness_t ;

/*------------------------------------------------------------------------------
 * State of a vty command loop.
 */
enum vc_state
{
  vc_null  = 0,         /* the command loop has not yet been entered.   */

  vc_running,           /* the command loop is running, and the vty is
                         * in its hands.                                */

  vc_io_error_trap,     /* the command loop is running, but an I/O
                         * error has been posted.                       */

  vc_close_trap,        /* the command loop is running, but the vty is
                         * reset and must be closed                     */

  vc_waiting,           /* the command loop is waiting for I/O.         */

  vc_stopped,           /* the command loop has stopped, the vty is due
                           to be closed (loop cannot run again)         */

  vc_closed,            /* the command loop has finished and the vty
                         * has been closed.                             */
} ;
typedef enum vc_state vc_state_t ;

/*------------------------------------------------------------------------------
 * The vty_io structure
 *
 * The main elements of the vty_io object are the vin and vout stacks.
 *
 * One of the challenges is managing the closing of VTY objects.  First,
 * cannot close and free a VTY object which is in the hands of a command
 * function, or which is queued to or from a command function.  Second,
 * do not wish to completely close an output until have given it a chance
 * to clear any buffered output.
 *
 *
 *
 * "cmd_running" means that the VTY is in hands of (or has been passed to) TODO
 * a command loop -- the VTY cannot be fully closed until that is no
 * longer the case.
 *
 * "blocking" is set for configuration reading VTY, so that everything is
 * done with blocking I/O.
 *
 * "closing" means that the vty has been closed (!), but a command
 * and or output may still be active:
 *
 *   - if is a socket, will have shut_down the read side (half-closed)
 *
 *   - any further attempts to read will give "eof"
 *
 *   - there may be a command in execution -- see "cmd_running".  TODO
 *
 *   - further writing will be honoured.
 *
 *   - the write side may still be active, attempting to empty out any
 *     pending output.
 *
 * "closed" means the vty has been fully and finally closed.
 *
 *   - any further attempts to write will be ignored, but return instant
 *     success.
 *
 *   - the file/socket has been fully closed.
 *
 *   - the VTY and all attached structures can be reaped by the death_watch.
 */
struct vty_cli ;                /* forward reference -- vty_cli.h is
                                   *not* included, because that refers
                                   back to the vty_io !                 */

struct vty_io                   /* typedef appears above                */
{
  vty       vty ;               /* the related vty                      */

  vio_vf    vin ;               /* vin stack                            */
  vio_vf    vin_base ;
  uint      vin_depth ;

  uint      real_depth ;        /* less than vin_depth when closing     */

  vio_vf    vout ;              /* vout stack                           */
  vio_vf    vout_base ;
  uint      vout_depth ;

  uint      depth_mark ;        /* vin_depth before pipes opened        */

  /* Error handling                                                     */
  bool      err_hard ;          /* eg I/O error                         */
  vio_fifo  ebuf ;              /* buffer for error message             */

  /* List of all vty_io objects                                         */
  struct dl_list_pair(vty_io) vio_list ;

  /* State
   *
   * "blocking" is set for configuration reading VTY, so that everything is
   * done with blocking I/O.
   *
   * "state" as described above.
   */
  bool  blocking ;              /* => all I/O is blocking.              */

  vc_state_t  state ;

  char*       close_reason ;    /* MTYPE_TMP (if any)                   */

  /* For ease of output, pointer to current vout->obuf
   *
   * Even when the vty is almost closed, there will remain a valid obuf,
   * though anything sent to it under those conditions will be discarded.
   */
  vio_fifo  obuf ;

  /* The following is for "vty monitor".
   *
   * With the exception of the "monitor" flag, need the LOG_MUTEX in order
   * to change any of this.
   */
  bool  monitor ;               /* is in monitor state                  */

  bool  mon_kick ;              /* vty needs a kick                     */
  int   maxlvl ;                /* message level wish to see            */

  vio_fifo  mbuf ;              /* monitor output pending               */

  /* List of all vty_io that are in monitor state                       */
  struct dl_list_pair(vty_io) mon_list ;
} ;

/*==============================================================================
 * Assertions for suitable state to close things !
 */
Inline void
VTY_ASSERT_CAN_CLOSE(vty vty)
{
  if (vty_debug)
    {
      VTY_ASSERT_LOCKED() ;

      if (!vty->vio->blocking && !vty_is_cli_thread())
        VTY_ASSERT_FAILED() ;
    } ;
} ;

Inline void
VTY_ASSERT_CAN_CLOSE_VF(vio_vf vf)
{
  if (vty_debug)
    {
      VTY_ASSERT_LOCKED() ;

      if (!vf->blocking && !vty_is_cli_thread())
        VTY_ASSERT_FAILED() ;
    } ;
} ;

/*==============================================================================
 * Functions
 */

extern int uty_out (vty_io vio, const char* format, ...) PRINTF_ATTRIBUTE(2, 3);
Inline int uty_vprintf(vty_io vio, const char *format, va_list args) ;

Inline void uty_out_clear(vty_io vio) ;
Inline void uty_out_accept(vty_io vio) ;
Inline void uty_out_reject(vty_io vio) ;

extern vty uty_new (vty_type_t type, node_type_t node) ;
extern void uty_close(vty_io vio, const char* reason, bool curtains) ;

extern void uty_set_timeout(vty_io vio, vty_timer_time timeout) ;

extern void uty_vin_new_context(vty_io vio, cmd_context context,
                                                              qpath file_here) ;
extern void uty_vin_push(vty_io vio, vio_vf vf, vio_in_type_t type,
                                          vio_vfd_action* read_action,
                                          vio_timer_action* read_timer_action,
                                          usize ibuf_size) ;
extern void uty_vout_push(vty_io vio, vio_vf vf, vio_out_type_t type,
                                          vio_vfd_action* write_action,
                                          vio_timer_action* write_timer_action,
                                          usize obuf_size) ;
extern cmd_return_code_t uty_vin_pop(vty_io vio, bool final,
                                                          cmd_context context) ;
extern cmd_return_code_t uty_vout_pop(vty_io vio, bool final) ;


extern vio_vf uty_vf_new(vty_io vio, const char* name, int fd, vfd_type_t type,
                                                        vfd_io_type_t io_type) ;
extern void uty_vf_set_read(vio_vf vf, on_off_b on) ;
extern void uty_vf_set_read_timeout(vio_vf vf, vty_timer_time read_timeout) ;
extern void uty_vf_set_write(vio_vf vf, on_off_b on) ;
extern void uty_vf_set_write_timeout(vio_vf vf, vty_timer_time write_timeout) ;
extern int uty_vf_error(vio_vf vf, const char* what, int err) ;

extern vio_child uty_child_register(pid_t pid, vio_vf parent) ;
extern void vty_child_close_register(void) ;
extern void uty_child_awaited(vio_child child, vty_timer_time timeout) ;
extern bool uty_child_collect(vio_child child, vty_timer_time timeout,
                                                                   bool final) ;
extern vio_child uty_child_dismiss(vio_child child, bool final) ;
extern void uty_sigchld(void) ;

extern void uty_child_signal_nexus_set(vty_io vio) ;
extern void vty_child_signal_nexus_signal(void) ;
extern void uty_child_signal_nexus_clear(vty_io vio) ;


extern void uty_open_listeners(const char *addr, unsigned short port,
                                                             const char *path) ;
extern void uty_add_listener(int fd, vio_vfd_accept* accept) ;
extern void uty_close_listeners(void) ;

extern void uty_watch_dog_init(void) ;
extern void uty_watch_dog_start(void) ;
extern void uty_watch_dog_stop(void) ;

extern const char* uty_get_name(vty_io vio) ;

extern void uty_set_monitor(vty_io vio, bool on) ;

/*==============================================================================
 * Inline Functions
 */

Inline bool
uty_is_terminal(struct vty *vty)
{
  return vty->type == VTY_TERMINAL ;
}

Inline bool
uty_is_shell_server(struct vty *vty)
{
  return vty->type == VTY_SHELL_SERVER ;
}

Inline bool
uty_is_shell_client(struct vty *vty)
{
  return vty->type == VTY_SHELL_CLIENT ;
}

/*------------------------------------------------------------------------------
 * Command output -- append to output buffer.
 */
Inline int
uty_vprintf(vty_io vio, const char *format, va_list args)
{
  return vio_fifo_vprintf(vio->obuf, format, args) ;
} ;

/*------------------------------------------------------------------------------
 * Clear command output -- discard anything in the buffer, but keep markers.
 */
Inline void
uty_out_clear(vty_io vio)
{
  vio_fifo_clear(vio->obuf, false) ;
} ;

/*------------------------------------------------------------------------------
 * Accept command output -- advance any end_mark to current put position.
 */
Inline void
uty_out_accept(vty_io vio)
{
  vio_fifo_step_end_mark(vio->obuf) ;
} ;

/*------------------------------------------------------------------------------
 * Reject command output -- discard anything after the end_mark in the buffer,
 * but keep markers.
 */
Inline void
uty_out_reject(vty_io vio)
{
  vio_fifo_back_to_end_mark(vio->obuf, true) ;
} ;

#endif /* _ZEBRA_VTY_IO_H */

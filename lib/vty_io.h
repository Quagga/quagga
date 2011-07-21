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
#include "qfstring.h"
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

  VOUT_SH_CMD,          /* pipe for shell command (no actual output)    */

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

  vf_end,               /* for a vin: EOF has been reached or input has
                         * been terminated, or an error or timeout has
                         * been met.
                         *
                         * for a vout: output has been terminated, or an
                         * error or tineout has been met.
                         *
                         * The vfd may have been closed -- but in any
                         * case no further vfd I/O should be attempted. */
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

  vio_fifo    obuf ;            /* output fifo (if required)            */

  uint        depth_mark ;      /* depth of this vout                   */

  /* General I/O                                                        */

  bool        blocking ;        /* using blocking I/O (eg config read)  */

  vio_vfd     vfd ;             /* vty_io_basic "file descriptor"       */

  vty_timer_time  read_timeout ;
  vty_timer_time  write_timeout ;

  /* Pipe extras -- child and pipe returns                              */

  vio_child   child ;           /* state of child                       */

  vf_state_t  pr_state ;        /* iff VOUT_PIPE/VOUT_SH_CMD            */
  vio_vfd     pr_vfd ;          /* if pr_state != vf_closed             */
  vty_timer_time  pr_timeout ;  /* set once closing pipe return         */

  vf_state_t  ps_state ;        /* stderr for all pipe types            */
  vio_vfd     ps_vfd ;          /* if ps_state != vf_closed             */
  vty_timer_time  ps_timeout ;  /* set once closing pipe return         */

  vio_fifo    ps_buf ;          /* to be moved to vio->ps_buf           */
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
  vc_stopped,           /* the command loop has stopped, and will not run
                         * again.
                         *
                         * or, the command loop has never started.      */

  vc_waiting,           /* the command loop is waiting for I/O.
                         * command queue command loop only              */

  vc_running,           /* the command loop is running, and the vty is
                         * in its hands.                                */
} ;
typedef enum vc_state vc_state_t ;

/*------------------------------------------------------------------------------
 * I/O and time-out error types
 */
enum vio_err_type
{
  verr_none             = 0,

  verr_vin              = 1,
  verr_vout             = 2,
  verr_pr               = 3,    /* pipe return (read)           */
  verr_ps               = 4,    /* pipe stderr return (read)    */

  verr_mask             = BIT(4) - 1,

  verr_io               = BIT(4) * 0,
  verr_to               = BIT(4) * 1,

  verr_io_vin           = verr_vin  | verr_io,
  verr_io_vout          = verr_vout | verr_io,
  verr_io_pr            = verr_pr   | verr_io,
  verr_io_ps            = verr_ps   | verr_io,

  verr_to_vin           = verr_vin  | verr_to,
  verr_to_vout          = verr_vout | verr_to,
  verr_to_pr            = verr_pr   | verr_to,
  verr_to_ps            = verr_ps   | verr_to,
} ;
typedef enum vio_err_type  vio_err_type_t ;

QFB_T(verr_mess, 200) ;

/*------------------------------------------------------------------------------
 * The vty_io structure
 *
 * The main elements of the vty_io (aka vio) object are the vin and vout stacks.
 *
 * The first entry in the vin/vout stacks is the "base" and is a bit special.
 * This entry is at stack depth 1.  Stack depth 0 is reserved for all closed,
 * or about to be.
 *
 * The vin_depth counts the number of command inputs which have been opened
 * and pushed on the stack.
 *
 * The vout_depth reflects the vin_depth at which the vout was opened, and
 * will be:
 *
 *   * vout_depth == vin_depth + 1
 *
 *     this is true after a command line such as:
 *
 *        ...some command... > some_file
 *
 *     When the command completes, and all output is pushed out, then the
 *     vout will be closed and the vout_depth reduced.
 *
 *   * vout_depth == vin_depth
 *
 *     this is true when a vty is set up (and the depths will be 1), and also
 *     if vin and a vout are opened together, as in:
 *
 *        < some_file  > some_other_file
 *
 *     When the vin reaches eof (or fails) and is closed, then the vout_depth
 *     will be vin_depth + 1, which triggers the closing of the vout.
 *
 *  * vout_depth < vin_depth
 *
 *    This is true when one or vins have been opened and are stacked on top
 *    of each other.  As the vins are closed, the vin_depth reduces until
 *    it hits the vout_depth, as above.
 *
 * When a vout is opened, the then current vout_depth is stored in the
 * vf->depth_mark, and restored from there when the vout is closed.
 *
 * The vin_depth drives the closing of vouts.  The vin_true_depth drives the
 * closing of vins.
 */
struct vty_cli ;                /* forward reference -- vty_cli.h is
                                   *not* included, because that refers
                                   back to the vty_io !                 */

struct vty_io                   /* typedef appears above                */
{
  vty       vty ;               /* the related vty                      */

  /* List of all vty_io objects                                         */
  struct dl_list_pair(vty_io) vio_list ;

  /* The vin/vout stacks                                                */

  vio_vf    vin ;               /* vin stack                            */
  vio_vf    vin_base ;
  uint      vin_depth ;
  uint      vin_true_depth ;    /* less than vin_depth when closing     */

  vio_vf    vout ;              /* vout stack                           */
  vio_vf    vout_base ;
  uint      vout_depth ;

  bool      cancel ;

  /* Error handling                                                     */

  vio_fifo  ebuf ;              /* buffer for error message             */

  int       err_depth ;         /* on error, close stack to this depth  */

  /* State
   *
   * "blocking" is set for configuration reading VTY, so that everything is
   * done with blocking I/O.
   *
   * "state" as described above.
   */
  bool      blocking ;          /* => all I/O is blocking.              */

  vc_state_t  state ;           /* command loop state                   */
  cmd_return_code_t signal ;    /* signal sent to command loop          */

  char*     close_reason ;      /* MTYPE_TMP (if any)                   */

  /* Pipe stderr return buffer.
   */
  vio_fifo  ps_buf ;

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
  struct dl_list_pair(vty_io) mon_list ;

  bool      monitor ;           /* is in monitor state                  */

  bool      mon_kick ;          /* vty needs a kick                     */
  int       maxlvl ;            /* message level wish to see            */

  vio_fifo  mbuf ;              /* monitor output pending               */
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
extern void uty_close(vty_io vio) ;

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
                                          usize obuf_size,
                                          bool after) ;
extern cmd_return_code_t uty_vin_pop(vty_io vio, cmd_context context,
                                                                   bool final) ;
extern cmd_return_code_t uty_vout_pop(vty_io vio, bool final) ;


extern vio_vf uty_vf_new(vty_io vio, const char* name, int fd, vfd_type_t type,
                                                        vfd_io_type_t io_type) ;
extern void uty_vf_set_read(vio_vf vf, on_off_b on) ;
extern void uty_vf_set_read_timeout(vio_vf vf, vty_timer_time read_timeout) ;
extern void uty_vf_set_write(vio_vf vf, on_off_b on) ;
extern void uty_vf_set_write_timeout(vio_vf vf, vty_timer_time write_timeout) ;

extern cmd_return_code_t uty_vf_error(vio_vf vf, vio_err_type_t err_type,
                                                                      int err) ;
extern verr_mess_t uty_error_message(vio_vf vf, vio_err_type_t err_type,
                                                            int err, bool log) ;
extern vio_child uty_child_register(pid_t pid, vio_vf parent) ;
extern void vty_child_close_register(void) ;
extern void uty_child_awaited(vio_child child, vty_timer_time timeout) ;
extern bool uty_child_collect(vio_child child, vty_timer_time timeout,
                                                                   bool final) ;
extern void uty_child_dismiss(vio_child child, bool final) ;
extern void uty_sigchld(void) ;

extern void uty_child_signal_nexus_set(vty_io vio) ;
extern void vty_child_signal_nexus_signal(void) ;
extern void uty_child_signal_nexus_clear(vty_io vio) ;


extern void uty_open_listeners(const char *addr, unsigned short port,
                                                             const char *path) ;
extern void uty_add_listener(int fd, vio_vfd_accept* accept) ;
extern void uty_close_listeners(void) ;

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

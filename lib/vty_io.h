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

#include "misc.h"

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
typedef enum            /* Command input                                */
{
  VIN_NONE  = 0,        /* not a valid input type                       */

  VIN_TERM,             /* telnet terminal (for VTY_TERMINAL)           */
  VIN_VTYSH_SERVER,     /* vtysh (for VTY_VTYSH_SERVER)                 */

  VIN_FILE,             /* ordinary file input                          */
  VIN_PIPE,             /* pipe (from child process)                    */

  VIN_CONFIG,           /* config file                                  */

  /* The VIN types >= VIN_SPECIALS do not have an associated fd.
   *
   * These can coexist with a VOUT which does have an associated fd, or
   * one of the VOUT_SPECIALS.
   */
  VIN_SPECIALS,         /* all special from now on                      */

  VIN_DEV_NULL = VIN_SPECIALS,
                        /* black hole input                             */

  VIN_VTYSH,            /* vtysh itself: base of type VTY_VTYSH *only*  */

} vio_in_type_t ;

typedef enum            /* Command output                               */
{
  VOUT_NONE  = 0,       /* not a valid output type                      */

  VOUT_TERM,            /* telnet terminal (for VTY_TERMINAL)           */
  VOUT_VTYSH_SERVER,    /* vtysh (for VTY_VTYSH_SERVER)                 */

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

  VOUT_VTYSH,           /* vtysh itself: base of type VTY_VTYSH *only*  */

  VOUT_STDOUT,          /* stdout                                       */

} vio_out_type_t ;

/*------------------------------------------------------------------------------
 * vio_child structure.
 *
 * Lives on the vio_childer_list until collected or "curtains".
 *
 */
struct vio_vf ;                 /* Forward reference                    */
typedef struct vio_vf* vio_vf ;

typedef enum vio_child_await vio_child_await_t ;

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
 * State of a vf: vin, a vout, a pr (pipe return) or ps (pipe stderr return).
 *
 *
 * uty_vf_read_stop() and uty_vf_write_stop() should be used to change the
 * state of a vf.
 *
 * It is (probably) a mistake to fiddle with any of these bits, except as
 * specified below.
 */
typedef enum vf_state
{
  /* When a vf is opened it is set vf_open.
   *
   * Remains vf_open until it has been successfully closed -- so until all
   * input and output has completed or been cancelled.
   */
  vf_open     = BIT(5),

  /* When a vf is finished with, vf_cease is set.  The hiatus will pop all
   * vin/vout which are set to cease.  Closing a vin/vout sets vf_cease as the
   * first step, if not already set !
   *
   *   for input:   for command line fetch, vf_cease means "no more command
   *                lines"...
   *
   *                ...for all vin it means discard all buffered input, and
   *                stop fetching command lines.
   *
   *                ...for most vin, when vf_cease is set, we are no longer
   *                interested in reading any more, so vf_end will be set.
   *                But for VIN_PIPE, vf_end is not set, so that on close
   *                will read, but discard, rest of child's output.
   *
   *                For pr/ps vf_cease alone means that they are due to be
   *                closed, once all input has been consumed and the child
   *                return code picked up, the vf can be closed.
   *
   *                This interacts with vf_cancel -- see below.
   *
   *                vf_cease is set when a vin or pr/ps is popped/closed.
   *
   *                vf_cease is set by all but a very few vx_xxx exceptions.
   *
   *                vf_cease is also set:
   *
   *                  - when command line fetch or other input reaches end
   *                    of file -- which may be some time after vf_end is
   *                    cleared.
   *
   *                  - when some other event (eg: an "exit" command) brings
   *                    input to a stop.
   *
   *                  - when an input is popped/closed.
   *
   *   for output:  output continues, and in cases where generally output is
   *                done in "chunks", vf_cease forces buffers to be emptied.
   *
   *                vf_cease is set by almost all vx_xxx exceptions.
   *
   *                vf_cease is also set when an output is popped/closed.
   *
   *                Unlike input, will not clear vf_end when sets vf_cease.
   *                But when all buffers are empty, iff vf_cease is set will
   *                then clear vf_end.
   */
  vf_cease    = BIT(4),

  /* Various exceptions -- see uty_vio_exception() -- set vf_cancel on all
   * affected vf.
   *
   *   for input:   for all reading functions, vf_cancel means stop reading,
   *                for the time being.
   *
   *                for all inputs vf_cancel with vf_cease means that vf_end
   *                should be set, all buffered input should be discarded,
   *                and all further input any child process return code
   *                ignored.
   *
   *                for all but vin_base, when vf_cancel is set, vf_cease is
   *                usually set at the same time, and hence vf_end also.
   *
   *   for output:  vf_cancel means stop outputting -- at least for general
   *                output: log monitor output may continue.
   *
   *                On its own this does *not* mean close.  If vf_cancel and
   *                vf_cease are set together -- which they often are -- that
   *                means stop everything and then close.
   *
   *                On vout_base vf_cancel is usually set without vf_cease.
   *                When the dust settles, the hiatus will clear vf_cancel and
   *                continue.
   *
   *                Note that vf_cancel does *not* mean flush buffers, that is
   *                a matter for the hiatus.
   *
   * vf_cancel is set by uty_vio_exception().
   */
  vf_cancel   = BIT(1),

  /* vf_end is set when no further I/O should be attempted.
   *
   *   for input:   vf_end is set when a read operation finds that eof
   *                has been reached.
   *
   *                NB: vf_end may be set even though there is still input
   *                    to be dealt with in some buffer.
   *
   *                vf_end is set when vf_cease is set, unless is reading to
   *                eof to hoover up input.
   *
   *                vf_end is set when vf_cancel is set, unconditionally.
   *
   *                Read operations will not be attempted once vf_end is
   *                set - and read-ready will be overridden.
   *
   *   for output:  vf_end is set when buffers empty out *and* is vf_cease.
   *
   *                vf_end may be set with vf_cease and vf_cancel to crash
   *                close.
   */
  vf_end   = BIT(0),

  /* A vf is fully closed, or has never been opened, if is vf_closed.
   *
   * NB: if vf_open is not set, no other bits should be set, either.  Mainly,
   *     however, do not bother to check that vf_open is set when checking
   *     any one or more of the others.
   */
  vf_closed   = 0,

} vf_state_t ;

/*------------------------------------------------------------------------------
 * Stop actions -- see uty_vf_read_stop() and uty_vf_write_stop().
 */
typedef enum vfs_stop
{
  vfs_stop_end,         /* have just met eof or have just emptied out output
                         * buffers.
                         */
  vfs_stop_cease,       /* cease after completing any I/O that should
                         * be completed
                         */
  vfs_stop_cancel,      /* cease, cancelling all I/O.
                         */
  vfs_stop_pause,       /* for base vout *only*.  If is vf_cease already, then
                         * this acts as vfs_stop_cancel.
                         */
  vfs_stop_final,       /* for when no further activity is required
                         */
} vfs_stop_t ;

/*------------------------------------------------------------------------------
 * vty_vf -- "vty file" structure
 *
 * A vio_vf may be a read, write or read/write object.
 *
 * For ordinary vio_vf I/O is via vio_vfd objects.  The vio_vfd layer hides the
 * differences between the qpthreads and legacy thread environments.
 *
 * VOUT_STDOUT is handled as direct output to the standard I/O file handle.
 *
 * Also used for the associated listeners.
 */
struct vty_cli ;                /* Forward reference                    */
typedef struct vty_cli* vty_cli ;

struct vtysh_server ;           /* Forward reference                    */
typedef struct vtysh_server* vtysh_server ;

struct vio_vf
{
  vty_io      vio ;             /* parent                               */

  char*       name ;            /* MTYPE_VTY_NAME (if any)              */

  /* Input side.
   */
  vio_in_type_t   vin_type ;
  vf_state_t      vin_state ;

  vio_vf      vin_next ;        /* list of inputs                       */

  cmd_context     context ;     /* for current vin == exec->context
                                 * for pushed vin  == saved context     */

  vty_cli         cli ;         /* NULL if not a VTY_TERMINAL !         */
  vtysh_server    vtysh ;       /* NULL if not a VTY_VTYSH_SERVER !     */

  vio_fifo    ibuf ;            /* input fifo (if required)             */

  qstring     cl ;              /* command line buffer                  */
  bool        line_complete ;   /* false => line in construction        */
  uint        line_number ;     /* number of first line in cl           */
  uint        line_step ;       /* number of real lines in cl           */

  /* Output side.
   *
   *   vout_open and vout_active are both set true when vout is opened.
   *
   *   vout_open:   is cleared by uty_vf_write_close(), once the process has
   *                completed.
   *
   *   vout_active: is cleared by uty_vf_write_stop(), which also closes any
   *                underlying vfd.  Also by uty_vf_write_close().
   *
   *                uty_vf_write_stop() is done when output is stopped by
   *                vst_cancel or vst_final.
   *
   *                Note that uty_vf_write_fd() will return -1 once is no
   *                longer vout_active -- which will probably trigger an
   *                I/O error of some kind.
   *
   *                Note that unless stopped by cancel or error or the like,
   *                an output remains active until it is closed (once all
   *                pending stuff has been written away).
   */
  vio_out_type_t  vout_type ;
  vf_state_t      vout_state ;

  vio_vf      vout_next ;       /* list of outputs                      */

  vio_fifo    obuf ;            /* output fifo (if required)            */

  bool        out_ordinary ;    /* enable CMD_SUCCESS output            */
  bool        push_complete ;   /* must push output on command complete */

  uint        depth_mark ;      /* depth of this vout                   */

  /* General I/O
   *
   * Once an error has been posted -- see uty_vf_error() -- it is curtains
   * for the vio_vf.  The io_error flag is to prevent multiple errors being
   * posted for the same vio_vf, because after the first error any subsequent
   * errors are most likely a consequence of the first.
   *
   * If the vio is blocking, then all its vio_vf will also be blocking.
   * In rare cases (eg configuration file writing) an individual vio_vf
   * may be blocking.  All blocking is implemented by mini-pselect (so
   * can time-out) except for VOUT_STDOUT and VOUT_VTYSH.  At the vio_vfd
   * level can distinguish non-blocking/pseudo-blocking/blocking I/O.
   *
   * Note that if is blocking, then no qfile will exist for the vio_vfd,
   * and the vio_vfd may be opened and closed in any pthread.  Conversely,
   * non-blocking vio_vfd may only be opened/closed in the CLI pthread.
   * All vio_vfd may be read/written in any pthread.
   *
   * The vin_waiting flag is set for the hiatus -- iff non-blocking.  The
   * hiatus does all file closing and output.  If any of that goes CMD_WAITING,
   * it exits.  When all of that has succeeded, needs to know whether to return
   * CMD_SUCCESS, which will loop round to attempt to fetch the next command
   * line, or to return CMD_WAITING, which will then wait for some I/O to
   * complete and signal the command loop to proceed.
   */
  bool      io_error ;          /* an I/O or timeout posted             */

  bool      blocking ;          /* all I/O is blocking, pseudo or real  */

  bool      vin_waiting ;       /* if blocking, vin is waiting for I/O  */

  vio_vfd   vfd ;               /* vty_io_basic "file descriptor"       */

  vty_timer_time  read_timeout ;
  vty_timer_time  write_timeout ;

  /* Pipe extras -- child and pipe returns
   *
   *   pr_open and ps_open are set when the respective pipe returns are opened.
   *
   *   ps_enabled and ps_enabled are generally set at the same time, but may
   *   be cleared if the pipe is being run "quiet".  If is not ps_enabled,
   *   will also discard child status.
   *
   *   cancel will close pipe returns (setting !xx_open and !xx_enabled).
   */
  vio_child   child ;           /* state of child                       */

  vf_state_t  pr_state ;
  bool        pr_enabled ;      /* if not, read it but discard it       */
  vio_vfd     pr_vfd ;          /* if pr_state != vf_closed             */
  vty_timer_time  pr_timeout ;  /* set once closing pipe return         */

  vf_state_t  ps_state ;
  bool        ps_enabled ;      /* if not, read it but discard it       */
  vio_vfd     ps_vfd ;          /* set up when ps_open set true         */
  vty_timer_time  ps_timeout ;  /* set once closing pipe return         */

  vio_fifo    ps_buf ;          /* stuff to be moved to vio->ps_buf     */

  /* vtysh extras -- all NULL if not VOUT_VTYSH
   */
  FILE*       fp ;              /* VOUT_VTYSH output                    */

  const char* pager ;           /* whether & how to do paging           */

  vio_fifo    rbuf ;            /* results from client daemon           */
  vio_fifo    ebuf ;            /* errors from client daemon(s)         */
  bool        no_prefix ;       /* do not add [name]                    */
} ;

/*------------------------------------------------------------------------------
 * State of a vty command loop.
 */
typedef enum vcl_state
{
  vcl_stopped,          /* the command loop has stopped, and will not run
                         * again.
                         *
                         * or, the command loop has never started.      */

  vcl_running,          /* the "blocking" command loop is running, and
                         * the vty is in its hands.                     */

  vcl_cq_waiting,       /* the "cq" command loop is waiting for I/O.
                         * command queue command loop only              */

  vcl_cq_running,       /* the "cq" command loop is running, and the
                         * vty is in its hands.                         */
} vcl_state_t ;

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

  verr_to               = 0,
  verr_io               = BIT(4),
  verr_vtysh            = BIT(5),
  verr_not_open         = BIT(6),

  verr_io_vin           = verr_vin  | verr_io,
  verr_io_vout          = verr_vout | verr_io,
  verr_io_pr            = verr_pr   | verr_io,
  verr_io_ps            = verr_ps   | verr_io,

  verr_to_vin           = verr_vin  | verr_to,
  verr_to_vout          = verr_vout | verr_to,
  verr_to_pr            = verr_pr   | verr_to,
  verr_to_ps            = verr_ps   | verr_to,

  verr_vtysh_vin        = verr_vin  | verr_vtysh,
} ;
typedef enum vio_err_type  vio_err_type_t ;

QFB_T(200) verr_mess_t ;

/* The "err" values for verr_vtysh_vin
 */
enum
{
  verr_vtysh_vin_eof,           /* eof in the middle of something       */
  verr_vtysh_vin_nonce,         /* unexpected nonce value               */
  verr_vtysh_vin_type,          /* unrecognised message type            */
  verr_vtysh_vin_length,        /* invalid length field                 */
} ;

#if 0
/* The stages of suspension
 */
typedef enum
{
  vss_void,             /* nothing to worry about       */
  vss_suspend,          /* need to suspend              */
  vss_suspending,       /* busy suspending              */
  vss_suspended         /* is now suspended             */

} vio_suspend_stage_t ;
#endif

/* The types of exception -- see uty_vio_exception()
 */
typedef enum
{
  vx_quash,
  vx_cancel,
  vx_io_error,
  vx_suspend,
  vx_stop,
  vx_stop_io_error,
  vx_stop_final,

} vio_exception_t ;

/*------------------------------------------------------------------------------
 * State of a vty !
 *
 * The vst_state drives both the output/hiatus and the base command handling.
 *
 * The vst_state comprises a number of bits, which are all to do with the
 * output/hiatus side of things, and the base command state field.
 *
 * The hiatus is a key part of the command loop.  It handles all exceptions,
 * closing of files, waiting for input or output, etc.  For the cq command
 * loop (the non-blocking loop used by VTY_TERMINAL and VTY_VTYSH_SERVER),
 * the hiatus is the point at which the command loop exits and resumes.
 * If
 *
 *
 */
typedef enum vst_state
{
  /* final close -- leave hiatus and do uty_close()
   *             -- all I/O will be "final" -- so no actual input or output and
   *                (in any case) ignore errors )other than logging same).
   *
   * when vst_final is set, vst_cancel will also be set.
   */
  vst_final         = BIT(15),

  /* vst_cancel means discard all output and pipe return, other than the
   * "vio->ebuf".  Then output "notify" to say what happened.
   *
   * vst_cancel is set for most exceptions -- see uty_vio_exception().  When
   * this flag was set all vin and vout will have stopped, possibly including
   * vib_base.
   *
   * Anywhere where new output may be generated can check vst_cancel, and stop
   * generating output and stop writing it away.  This includes all pipe return
   * readers.  All inputs which are affected will have been set !vin_active.
   *
   * NB: *must* *not* discard the contents of buffers.  The hiatus will do this
   *     as required.
   *
   * Once the hiatus has closed everything it needs to close, and discarded
   * all output down to, but not including the vout_base, then things get
   * interesting...
   *
   * ...if the vty_notify flag is *not* set, then:
   *
   *   - discards all output associated with the vout_base, except for the
   *     ebuf.  In particular, empties the base obuf (and, for the VOUT_TERM,
   *     the line control).
   *
   *   - if was *not* vst_cmd_running (or vst_cmd_running_executing), then let
   *     command input whether to show a ^C at the end of the current command
   *     line.  This may clear the cancel_prefix.
   *
   *   - if there is a cancel_prefix, output it to the (empty) obuf, and clear
   *     the cancel_prefix.
   *
   *   - set the vst_notify_flag.
   *
   * Then, in any case, clear the vst_cancel flag and any cancel_reason.  Will
   * then be in vst_notify state.
   */
  vst_cancel        = BIT(13),

  /* in notify state we are trying to get a notification output to show why
   * has just cancelled or that there has been some sort of error.
   *
   * While vst_notify is set, any further vst_cancel has no effect on the
   * vout_base output -- the obuf, the ebuf and any suspend_reason will be
   * output, and only then will vty_notify be cleared.
   *
   * Note that while is in vty_notify further I/O errors and/or suspend actions
   * may add stuff to be output as part of the notification.  So, once the
   * cancel process has started, any further cancels or cancel-like actions
   * are added on.
   *
   * Each time passes through the hiatus while vty_notify is set, moves stuff
   * from the ebuf to the obuf, and clears the ebuf.  If vst_suspend_flag is
   * set, appends the suspend_reason to the obuf, and clears it.
   */
  vst_notify        = BIT(12),

  /* need to suspend the vty
   *
   *   vst_suspend   -- will be set, along with vst_cancel, when the vty should
   *                    suspend (unless vst_suspend is already set -- can only
   *                    have one suspend at a time).
   *
   *                    The vst_suspend flag and suspend_reason will also be
   *                    set with vst_suspend, so that the notify phase of
   *                    cancelling will arrange for the suspend reason to be
   *                    shown.
   *
   *                    No further input will be accepted.
   *
   *   vst_suspended -- if is vst_suspend, this will be set when the
   *                    cancel/notify process completes, or if the vty stops.
   *
   *                    When the vty is, finally, suspended, will poll to see
   *                    if all (remaining) vty are suspended, and hence whether
   *                    can now proceed.
   */
  vst_suspend       = BIT( 9),
  vst_suspended     = BIT( 8),

  /*--------------------------------------------------------------------------
   * These are the bits which the hiatus is responsible for.
   *
   * If any of these are set, then the command loop should be in or on its way
   * to the hiatus.
   */
  vst_hiatus_mask   = vst_final
                    | vst_cancel
                    | vst_notify
                    | vst_suspend
                    | vst_suspended,

  /*--------------------------------------------------------------------------
   * The log monitor output is autonomous, handled in the pselect() process.
   *
   * This state is visible here because if any of these are set, the command
   * loop is waiting for them to be cleared.  At present only the VTY_TERMINAL
   * does this.
   *
   *   vst_mon_active  -- has log monitor stuff to output.
   *
   *                      This is set when monitor output is ready, and when it
   *                      is set any current command line is wiped.
   *
   *   vst_mon_blocked -- blocked while writing log monitor stuff.
   *
   *                      Will only appear with vst_mon_active.
   *
   *   vst_mon_paused  -- is pausing after monitor output, before continuing
   *                      to output stuff, or redraw CLI line -- in case more
   *                      monitor stuff rolls up.
   *
   *                      If running threaded, this is set and the associated
   *                      timer is started, when monitor output completes.
   *
   *                      NB: vst_mon_active/vst_mon_blocked and vst_mon_paused
   *                          are generally mutually exclusive.
   *
   *                      NB: vst_mon_active/vst_mon_blocked and vst_mon_paused
   *                          take priority over other output state.
   *
   * NB: these bits are *not* the responsibility of the hiatus, but if any
   *     are set, the hiatus may wait for them to go away.  Also, base level
   *     command handling should wait for these bits to go away.
   */
  vst_mon_active    = BIT( 7),
  vst_mon_blocked   = BIT( 6),
  vst_mon_paused    = BIT( 5),

  vst_mon_mask      = vst_mon_active | vst_mon_blocked | vst_mon_paused,

  /*--------------------------------------------------------------------------
   * The vst_cmd_xxx stuff.
   *
   * These reflect the state of the *base* level command processing: fetch,
   * execute and  output.
   *
   * There is a state value (under vst_cmd_mask) and the vst_cmd_execute bit.
   *
   * State of base level command processing:
   *
   *   vst_cmd_fetch    -- is busy drawing prompt, fetching input, processing
   *                       same, etc. -- preparing the next command.
   *
   *   vst_cmd_dispatched -- a command has been dispatched, and is waiting for
   *                       command loop to fetch it.
   *
   *                       uty_term_ready() will signal success to the command
   *                       loop, once any pending output has completed.
   *
   *                       Note that command line is drawn, and cursor is at
   *                       the end of the line.
   *
   *   vst_cmd_running  -- a command has been taken by the command loop, and is
   *                       still running -- or at least no further command has
   *                       been fetched by the command loop.
   *
   *                       May also be vst_cmd_execute.
   *
   *   vst_cmd_more     -- during the output of results, is in the "--more--"
   *                       cli.
   *
   *                       Will return to vst_cmd_running -- possibly via
   *                       a cancel.
   *
   *                       May also be vst_cmd_execute.
   *
   *   vst_cmd_complete -- this follows vst_cmd_running, and signals that the
   *                       last CLI level command has completed, and all output
   *                       has been written away.
   *
   *                       Is ready to go vst_cmd_fetch.
   *
   *                       Will end up vst_cmd_complete when the input is closed.
   *
   * Plus, the extra bit:
   *
   *   vst_cmd_execute  -- this is set when the command line is picked up, ie
   *                       when vst_cmd_running is set.
   *
   *                       It means that further output may be generated.
   *
   *                       This is cleared when a command finishes, *and* we
   *                       are back at the base vin level.  This means that
   *                       no more output will be generated, and hence once
   *                       all output has been written away, can move from
   *                       vst_cmd_running to vst_cmd_complete.
   *
   *                       This will appear only with vst_cmd_running or
   *                       vst_cmd_more.
   *
   *                       This is a bit so that can (a) ignore the execute
   *                       sub-state when it is not relevant, and (b) clear
   *                       the sub-state without worrying about the rest.
   *
   * NB: this is for the base level command processing.
   *
   *     Fetching commands from from a pipe does *not* change the vst_cmd_state
   *     -- which will be vst_cmd_running and vst_cmd_execute.
   *
   *     Output to a pipe does *not* change the vst_cmd_state.  Only when
   *     the vout_base empties out all buffers will vst_cmd_complete be a
   *     possibility -- if is vst_cmd_running.
   */
  vst_cmd_mask       = BIT(4) - 1,

  vst_cmd_executing  = BIT(3),  /* command executing                    */

  vst_cmd_inner_mask = vst_cmd_executing - 1,

  vst_cmd_fetch      = 0,       /* fetching the next command line       */

  vst_cmd_dispatched,           /* has dispatched command line          */

  vst_cmd_running,              /* command/output being dealt with      */

  vst_cmd_more,                 /* is in "--more--" waiting for user
                                 * will revert to vst_cmd_in_progress   */

  vst_cmd_complete,             /* command and all output has completed
                                 * and is ready to go vst_cmd_fetch     */

  vst_cmd_state_count,          /* see CONFIRM, below                   */

  /* The two possible executing states:
   */
  vst_cmd_running_executing = vst_cmd_running | vst_cmd_executing,
  vst_cmd_more_executing    = vst_cmd_more    | vst_cmd_executing,

  /*--------------------------------------------------------------------------
   */

} vst_state_t ;

CONFIRM(vst_cmd_state_count <= (vst_cmd_inner_mask + 1)) ;

/* Mechanics:
 *
 *  * parsing/command error
 *
 *    Moves error message to ebuf.  Sets: vst_quash.
 *
 *    All input will be closed, but all output and pipe return stuff will
 *    run to completion.
 *
 *    Once reaches the vin_base/vout_base, the contents of the ps_buf (pipe
 *    stderr return) are appended to the obuf, and output in the usual way
 *    (and may be cancelled).
 *
 *    Once the obuf is empty the ebuf will be appended to the obuf, and
 *    vty_notify is set.  This means that if is cancelled for any reason, the
 *    error message (in the ebuf) is preserved.
 *
 *    I/O errors while quashing will append error to the ebuf.
 *
 *  * I/O error and timeout.  Sets vst_cancel
 *
 *      if not in vout_base:  put error message in ebuf
 *                            set: error_flag
 *                            if is in vin_base: set vst_stop.
 *
 *      if in vout_base:      post error message to close_reason
 *                            set: vst_stop, vst_final.
 *
 *    Note if vin_base == vout_base, is in vout_base !
 *
 *  * Cancel -- other than ^C eaten by CLI.
 *
 *    Sets: vst_cancel and vst_cancel_flag.
 *
 *    All further input, other than on vin_base, must be discarded -- this
 *    includes all pipe return input.
 *
 *    All new output may be discarded, but in any case, must not write away
 *    anything more from the obuf.  Output may continue to be put into the obuf,
 *    but that will be discarded.
 *
 *    Once the hiatus has closed all input and output apart from the vout_base,
 *    it empties out all buffers, other than the ebuf, generates any
 *    notification -- including contents of ebuf -- and sets vst_notify
 *    (takes notice of the vst_cancel_flag, vst_error_flag and vst_suspend_flag,
 *    and may take notice of the command line state.)
 *
 *    Once vst_notify is set, vst_cancel is cleared, so output can now proceed.
 *    If vst_cancel is set again, output may be interrupted, but when the
 *    hiatus runs, will clear vst_cancel and add any suspend reason and
 *    contents of the ebuf to the obuf and continue.
 *
 *    Once all output is complete, if vst_notify, can clear all the cancel
 *    flags.
 *
 *    Note: while is vst_cancel or vst_notify:
 *
 *      * ignores keyboard input -- so no further cancel from there
 *
 *        Any further ^C is buffered for the CLI (to be discarded if never
 *        return to the CLI !).
 *
 *      * a SIGHUP or other suspend could arrive
 *
 *        This does nothing if vst_suspend is already set.
 *
 *        This sets the vst_suspend_flag, so the suspend_reason will be added
 *        to the ebuf next time runs in the hiatus.  Then current vst_cancel
 *        or vst_notify can run to completion.
 *
 *      * a SIGTERM or other stop could arrive
 *
 *        This sets vst_stop, and may close the vin_base, but that will have no
 *        effect while is vst_cancel or vst_notify.  Current vst_cancel or
 *        vst_notify can run to completion.
 *
 *    Once cancel process completes, can consider vst_stop and vst_suspend.
 *
 *  * Suspend -- eg SIGHUP
 *
 *    If vst_suspend already set, do nothing.
 *
 *    Sets: vst_cancel, vst_suspend and vst_suspend_flag.
 *
 *    The vst_suspend flag will cause the suspend_reason to be appended to the
 *    ebuf, before the cancel process completes.
 *
 *    When cancel process completes, if is vst_suspend sets vst_suspended.  If
 *    is vst_stop, does that in preference.  In any case, if there is something
 *    waiting for the vty to suspend or stop, it should be released.
 *
 *  * Shut down -- eg SIGTERM
 *
 *    Sets: vst_cancel and vst_stop.  Sets the close reason.  Once output
 *    finishes, will end up in uty_close(), which will output the close
 *    reason.  If the system is terminated, the close reason will still be
 *    output !
 *
 *  * monitor output
 *
 *    This proceeds in the pselect() process.  While these are set, the
 *    vout_base is going to be waiting.
 *
 *    This takes precedence over any other vin_base/vout_base activity.
 *
 *    vst_stop turns off monitor output.  vst_stop and vst_cancel will
 *    discard any buffered monitor output.
 *
 *  *
 *
 */














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
 * The vin_depth drives the closing of vouts.  The state of the top vin drives
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

  /* Ownership of the configuration symbol of power.
   */
  ulong     config ;            /* non-zero => own                      */

  /* The vin/vout stacks                                                */

  vio_vf    vin ;               /* vin stack                            */
  vio_vf    vin_base ;
  uint      vin_depth ;

  vio_vf    vout ;              /* vout stack                           */
  vio_vf    vout_base ;
  uint      vout_depth ;

  /* State of the vio !!  See essay, above.
   */
  vst_state_t   state ;

  const char*  cancel_prefix ;  /* "...^C\n" or "***^C\n"               */
  qstring   suspend_reason ;    /* "%% Suspended: %s\n"                 */
  qstring   close_reason ;      /* "---\n%s\n                           */

  /* Output items -- "global" state across all vout stack.
   *
   * The vio->obuf points at the current output buffer.  All "command level"
   * I/O uses this buffer.  Generally all the output from a single command
   * will be sent to this buffer, and when the command completes, the
   * output will be "pushed" to the current vout stack.  A (very few) commands
   * push the output before they complete.  If command line reflection is
   * active, the reflected line is pushed before the commad actually executes.
   *
   * Generally vio->obuf will be a copy of the vout->obuf.  Even when the vty
   * is almost closed, there will remain a valid obuf, but that may not be
   * associated with any vout, and anything sent to it under those conditions
   * will be discarded.
   */
  vio_fifo  obuf ;

  /* Command prompt cacheing
   */
  node_type_t   prompt_node ;
  name_gen_t    prompt_gen ;
  qstring       prompt ;

  ulen          prompt_len ;    /* of last prompt written               */

  /* Error handling
   */
  vio_fifo  ebuf ;              /* buffer for error message             */

  /* State
   *
   * "blocking" is set for configuration reading VTY, so that everything is
   * done with blocking I/O -- also for the vtysh itself.
   *
   * "cl_state" as described above.
   */
  bool      blocking ;          /* => all I/O is blocking.              */

  vcl_state_t  cl_state ;       /* command loop state                   */
  cmd_ret_t    signal ;         /* signal sent to command loop          */

  /* For vty_sh_serv we want to return the result of the last command,
   * this is where it is dropped so that the output side can send it.
   */
  cmd_ret_t sh_serv_ret ;

  /* Pipe stderr return buffer.
   */
  vio_fifo  ps_buf ;

  /* The following is for "vty monitor".
   *
   * Apart from the mbuf, we need the LOG_LOCK *and* the VTY_LOCK in order to
   * change any of this, but only one of the two in order to read any of it.
   *
   * For the mbuf we need only the LOG_LOCK to change, and must have the
   * LOG_LOCK to read.
   */
  struct dl_list_pair(vty_io) mon_list ;

  bool      monitor ;           /* is in monitor state                  */
  bool      mwrite ;            /* needs to be written away.            */

  int       maxlvl ;            /* message level wish to see            */

  vio_fifo  mbuf ;              /* monitor output pending               */
} ;

/*==============================================================================
 * Constants for file and pipe I/O
 */
enum
{
  file_timeout   = 10,          /* for file read/write          */

  pipe_timeout   = 30,          /* for pipe read/write          */

  child_timeout  = 10,          /* for collecting child process */

  config_buffer_size = 64 * 1024,       /* for config reading           */
  file_buffer_size   = 16 * 1024,       /* for other file read/write    */

  term_buffer_size   =  4 * 1024,       /* for terminal write           */

  pipe_buffer_size   =  4 * 1024,       /* for pipe read/write          */

  vtysh_buffer_size  =  4 * 1024,       /* for vtysh server read/write  */

  std_buffer_size    =  1 * 1024,
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

      if (!vio_vfd_blocking(vf->vfd) && !vty_is_cli_thread())
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
Inline void uty_out_discard(vty_io vio) ;

extern vty uty_new (vty_type_t type, node_type_t node) ;
extern void vty_close(vty vty) ;
extern void uty_close(vty_io vio) ;
extern void uty_close_reason_set(vty_io vio, const char* why, bool replace) ;
extern void uty_suspend_reason_set(vty_io vio, const char* why) ;
extern void uty_suspend_reason_clear(vty_io vio) ;

extern void uty_set_timeout(vty_io vio, vty_timer_time timeout) ;

extern void uty_vin_push(vty_io vio, vio_vf vf, vio_in_type_t type,
                                          vio_vfd_action* read_action,
                                          usize ibuf_size) ;
extern void uty_vin_set_here(vty_io vio, qpath here) ;
extern void uty_vout_push(vty_io vio, vio_vf vf, vio_out_type_t type,
                                          vio_vfd_action* write_action,
                                          usize obuf_size,
                                          bool after) ;
extern cmd_ret_t uty_vin_pop(vty_io vio) ;
extern cmd_ret_t uty_vout_pop(vty_io vio) ;


extern vio_vf uty_vf_new(vty_io vio, const char* name, int fd, vfd_type_t type,
                                                        vfd_io_type_t io_type) ;
extern void uty_vf_read_stop(vio_vf vf, vfs_stop_t vstp) ;
extern void uty_vf_write_stop(vio_vf vf, vfs_stop_t vstp) ;
extern void uty_vf_return_stop(vf_state_t* p_state, vfs_stop_t vstp) ;
extern void uty_vio_exception(vty_io vio, vio_exception_t vx) ;

extern cmd_ret_t uty_fifo_cmd_line_fetch(vio_vf vf, bool cont_lines) ;

extern cmd_ret_t uty_vf_set_read_ready(vio_vf vf, on_off_b how) ;
extern cmd_ret_t uty_vf_set_write_ready(vio_vf vf, on_off_b how) ;

extern const char* uty_cmd_prompt(vty_io vio, node_type_t node) ;

extern cmd_ret_t uty_vf_error(vio_vf vf, vio_err_type_t err_type, int err) ;
extern cmd_ret_t uty_vf_not_open_error(vio_vf vf, vio_err_type_t err_type) ;
extern verr_mess_t uty_error_message(vio_vf vf, vio_err_type_t err_type,
                                                            int err, bool log) ;
extern vio_child uty_child_register(pid_t pid, vio_vf parent) ;
extern void vty_child_close_register(void) ;
extern void uty_child_awaited(vio_child child, vty_timer_time timeout) ;
extern bool uty_child_collect(vio_child child, vty_timer_time timeout,
                                                                   bool final) ;
extern void uty_child_dismiss(vio_child child, bool final) ;
extern void uty_sigchld(void) ;

extern void vty_child_signal_nexus_set(vty vty) ;
extern void vty_child_signal_nexus_signal(void) ;
extern void vty_child_signal_nexus_clear(vty vty) ;

extern void uty_open_listeners(void) ;
extern void uty_add_listener(int fd, vio_vfd_accept* accept) ;
extern void uty_close_listeners(void) ;

extern void uty_watch_dog_start(void) ;
extern void uty_watch_dog_stop(void) ;

extern const char* uty_get_name(vty_io vio) ;

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Fetching the underlying fd, if any.
 *
 * If is vf_end or there is no vfd, returns -1, which will trigger some
 * sort of I/O error if is used !  This is a back-stop in case an I/O error
 * or other exception has changed things, but that has been ignored and I/O
 * is continuing !
 *
 * NB: do *not* use these for close() -- only for read() or write() !!
 */

Inline int
uty_vf_read_fd(vio_vf vf)
{
  return (vf->vin_state & vf_end) ? -1 : vio_vfd_fd(vf->vfd) ;
} ;

Inline int
uty_vf_write_fd(vio_vf vf)
{
  return (vf->vout_state & vf_end) ? -1 : vio_vfd_fd(vf->vfd) ;
} ;

/*------------------------------------------------------------------------------
 * Is output (currently) squelched ?
 *
 * Is squelched if !vout_active, or if is currently vst_final or vst_cancel.
 *
 * Note that response to squelch is *not* to close or discard buffers, that
 * is the job of the hiatus.  The response is to return either CMD_SUCCESS
 * or CMD_HIATUS -- whichever will return control to the hiatus most cleanly.
 * Any new output need not be put into buffers -- it will be discarded later.
 */
Inline bool
uty_vf_write_squelched(vio_vf vf)
{
#if 0
  return (vf->vout_state & vf_end)
                               || (vf->vio->state & (vst_final || vst_cancel)) ;
#endif
  return false ;
} ;

Inline bool
vf_active(vf_state_t vf_state)
{
  return (vf_state & (vf_open | vf_cancel | vf_end)) == vf_open ;
}

/*------------------------------------------------------------------------------
 * Testing type of vty
 */
Inline bool
uty_is_terminal(struct vty *vty)
{
  return vty->type == VTY_TERMINAL ;
}

Inline bool
uty_is_shell_server(struct vty *vty)
{
  return vty->type == VTY_VTYSH_SERVER ;
}

Inline bool
uty_is_shell_client(struct vty *vty)
{
  return vty->type == VTY_VTYSH ;
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
  vio_fifo_clear(vio->obuf) ;
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
 * Discard command output -- discard anything after the end_mark in the buffer,
 * but keep markers.
 */
Inline void
uty_out_discard(vty_io vio)
{
  vio_fifo_back_to_end_mark(vio->obuf) ;
} ;

#endif /* _ZEBRA_VTY_IO_H */

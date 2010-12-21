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

#include "zebra.h"
#include "misc.h"

#include <errno.h>

#include "vty_io_basic.h"
#include "uty.h"
#include "vty.h"
#include "vio_fifo.h"
#include "vio_lines.h"
#include "keystroke.h"
#include "thread.h"
#include "command.h"
#include "qstring.h"

/*==============================================================================
 * Here are structures and other definitions which are shared by:
 *
 *   vty.c      -- the main vty handler
 *   vty_cli.c  -- which handles the command line stuff
 *   vty_io.c   -- ....
 *
 * The "struct vty" is used extensively across the Quagga daemons, where it
 * has two functions relating to command handling as:
 *
 *   1) a "file handle" for output produced by commands
 *
 *   2) the holder of some context -- notably the current command "node" -- for
 *      command execution to use
 *
 * The bulk of "struct vty" is, therefore, private to vty.c and is factored
 * out into the "struct vty_io".
 *
 * To reduce the size of vty.c, some groups of functions are separated into:
 *
 *   vty_cli.c   -- which looks after the keystroke by keystroke handling
 *                  of the command line.
 *
 */

/*==============================================================================
 * VTY CLI and OUT types
 */
enum vio_in_type        /* Command input                                */
{
  VIN_NONE  = 0,        /* no input at all                              */

  VIN_TERM,             /* telnet terminal                              */
  VIN_SHELL,            /* vty_shell input                              */

  VIN_FILE,             /* ordinary file input                          */
  VIN_PIPE,             /* pipe (from child process)                    */

  VIN_CONFIG,           /* config file ??                               */
} ;
typedef enum vio_in_type vio_in_type_t ;

enum vio_out_type       /* Command output                               */
{
  VOUT_NONE  = 0,       /* no output at all                             */

  VOUT_TERM,            /* a telnet terminal                            */
  VOUT_SHELL,           /* a vty_shell output pipe                      */

  VOUT_FILE,            /* ordinary file                                */
  VOUT_PIPE,            /* pipe (to child process)                      */

  VOUT_STDOUT,          /* stdout                                       */
  VOUT_STDERR,          /* stderr                                       */
};
typedef enum vio_out_type vio_out_type_t ;

/*------------------------------------------------------------------------------
 * VIO file structure
 *
 * All I/O is non-blocking for all sources and sinks of VIO stuff.
 *
 * Also used for the associated listeners.
 */
typedef struct vio_vf* vio_vf ;

struct vio_vf
{
  vty_io  vio ;                 /* parent               */

  vio_in_type_t     vin_type ;
  vio_vf            vin_next ;  /* list of inputs       */

  vio_out_type_t    vout_type ;
  vio_vf            vout_next ; /* list of outputs      */

  vio_fifo          obuf ;      /* pointer to fifo      */
  vio_line_control  olc ;       /* pointer to lc        */

  vio_fd  vfd ;

  bool    blocking ;            /* using blocking reads                 */
  bool    closing ;             /* suppress read/write ready            */

  bool    read_open ;           /* reads returns 0 if not               */
  bool    write_open ;          /* writes complete instantly if not     */
  int     error_seen ;          /* non-zero => failed                   */

  on_off_b        read_on ;
  on_off_b        write_on ;

  vty_timer_time  read_timeout ;
  vty_timer_time  write_timeout ;
} ;

enum vty_readiness      /* bit significant      */
{
  not_ready     = 0,
  read_ready    = 1,
  write_ready   = 2,    /* takes precedence     */
  now_ready     = 4
} ;

/*------------------------------------------------------------------------------
 * The vty_io structure
 *
 *
 *
 *
 *
 */

struct vty_io
{
  struct vty*   vty ;           /* the related vty                      */
  char  *name ;                 /* for VTY_TERM is IP address)          */

  /* vin stack                                                          */
  vio_vf    vin ;
  vio_vf    vin_base ;

  /* vout stack                                                         */
  vio_vf    vout ;
  vio_vf    vout_base ;

  /* List of all vty_io objects                                         */
  struct dl_list_pair(vty_io) vio_list ;

  /* List of all vty_io that are in monitor state                       */
  struct dl_list_pair(vty_io) mon_list ;

  /* VTY state                                                          */

  bool  half_closed ;           /* => on death watch list until closed  */
  bool  closed ;                /* => all I/O terminated
                                      will also be half_closed          */

  char* close_reason ;          /* message to be sent, once all other
                                   output has completed, giving reason
                                   for closing the VTY.                 */





  /* When writing configuration file                                    */
  enum vty_type real_type ;

  int   file_fd ;
  int   file_error ;

  /* Failure count for login attempts                                   */
  int           fail;

  /* History of commands                                                */
  vector_t      hist ;
  int           hp ;            /* History lookup current point */
  int           hindex;         /* History insert end point     */

  /* Window width/height as reported by Telnet.  0 => unknown           */
  int           width;
  int           height;

  /* Configure lines.                                                   */
  int           lines;
  bool          lines_set ;     /* true <=> explicitly set              */

  /* Terminal monitor.                                                  */
  bool          monitor ;
  bool          monitor_busy ;

  /* Terminal timeout in seconds -- 0 => none     */
  vty_timer_time v_timeout ;

  /*-------------------------------------------------------------------------
   * CLI_TERM stuff.
   */

  keystroke_stream key_stream ;

  /* cli_drawn <=> the current prompt and user input occupy the current
   *               line on the screen.
   *
   * cli_dirty <=> the last command output did not end with a newline.
   *
   * If cli_drawn is true, the following are valid:
   *
   *   cli_prompt_len    -- the length of the prompt part.
   *                        (will be the "--more--" prompt in cli_more_wait)
   *
   *   cli_extra_len     -- the length of any ^X at the cursor position
   *                        (for when blocked waiting for queued command)
   *
   *   cli_echo_suppress -- the user part of the command line is suppressed
   *
   * NB: cli_echo_suppress is only used for password entry.
   */
  bool          cli_drawn ;
  bool          cli_dirty ;

  int           cli_prompt_len ;
  int           cli_extra_len ;

  bool          cli_echo_suppress ;

  /* "cache" for prompt -- when node or host name changes, prompt does  */
  enum node_type cli_prompt_node ;
  bool          cli_prompt_set ;
  qstring_t     cli_prompt_for_node ;

  /* State of the CLI
   *
   *   cli_blocked      -- blocked from processing keystrokes
   *   cmd_in_progress  -- command dispatched (may be queued)
   *   cmd_out_enabled  -- contents of the command FIFO may be written away
   *   cli_more_wait    -- is in "--more--" wait state
   */
  bool          cli_blocked ;
  bool          cmd_in_progress ;
  bool          cmd_out_enabled ;
  bool          cli_more_wait ;

  /* This is used to control command output, so that each write_ready event
   * generates at most one tranche of output.
   */
  bool          cmd_out_done ;

  /* This is set only if the "--more--" handling is enabled             */
  bool          cli_more_enabled ;

  /* Command Line(s)
   *
   * cli_do  -- when current command being prepared is completed (by
   *            CR/LF or otherwise) this says what there now is to be done.
   *
   * cl      -- current command line being prepared.
   *
   * clx     -- current command line being executed.
   *
   * NB: during command execution vty->buf is set to point at the '\0'
   *     terminated current command line being executed.
   */
  enum cli_do   cli_do ;

  qstring_t     cl ;
  qstring_t     clx ;

  /* CLI output buffering                                               */
  vio_fifo_t    cli_obuf ;

} ;

/*==============================================================================
 * If possible, will use getaddrinfo() to find all the things to listen on.
 */
enum {
#if defined(HAVE_IPV6) && !defined(NRL)
  VTY_USE_ADDRINFO = 1,
#else
  VTY_USE_ADDRINFO = 0,
#endif
} ;

/*==============================================================================
 * Functions
 */

extern vty uty_new (vty_type_t type, int sock_fd) ;
extern void uty_close (vty_io vio, const char* reason) ;
extern void uty_close_final(vty_io vio, const char* reason) ;


extern void uty_vin_add(vty_io vio, vio_vf vf, vio_in_type_t type,
            vio_fd_action* read_action,  vio_timer_action* read_timer_action) ;
extern void uty_vout_add(vty_io vio, vio_vf vf, vio_out_type_t type,
            vio_fd_action* write_action, vio_timer_action* write_timer_action) ;



extern vio_vf uty_vf_new(vty_io vio, int fd, vfd_type_t type,
                                                        vfd_io_type_t io_type) ;
Inline int uty_vf_fd(vio_vf vf) ;
extern on_off_t uty_vf_set_read(vio_vf vf, on_off_t on) ;
extern on_off_t uty_vf_set_read_timeout(vio_vf vf,
                                                  vty_timer_time read_timeout) ;
extern on_off_t uty_vf_set_write(vio_vf vf, on_off_t on) ;
extern on_off_t uty_vf_set_write_timeout(vio_vf vf,
                                                 vty_timer_time write_timeout) ;



extern void uty_open_listeners(const char *addr, unsigned short port,
                                                             const char *path) ;
extern void uty_add_listener(int fd, vio_fd_accept* accept) ;
extern void uty_close_listeners(void) ;

extern void uty_watch_dog_init(void) ;
extern void uty_watch_dog_start(void) ;
extern void uty_watch_dog_stop(void) ;



extern int uty_output (struct vty *vty, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern int uty_vprintf(struct vty *vty, const char *format, va_list args) ;
extern int uty_reflect(struct vty *vty) ;
extern void uty_out_clear(vty_io vio) ;
extern void uty_out_fflush(vty_io vio, FILE* file) ;

extern void uty_set_height(vty_io vio) ;
extern void uty_cmd_output_start(vty_io vio) ;

extern void uty_file_set_readiness(vio_vf vf, enum vty_readiness ready) ;
extern void uty_file_set_timer(vio_vf vf, unsigned long timeout) ;

extern int uty_read (vty_io vio, keystroke steal) ;
extern int utysh_read (vty_io vio, qstring cl, qstring buf) ;

extern const char* uty_get_name(vty_io vio) ;

extern void uty_set_monitor(vty_io vio, bool on) ;

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Return the fd from a vio_fd structure
 */
Inline int
uty_vf_fd(vio_vf vf)
{
  return vio_fd_fd(vf->vfd) ;
} ;

/*------------------------------------------------------------------------------
 * Return the fd from a vio_fd structure
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

#endif /* _ZEBRA_VTY_IO_H */

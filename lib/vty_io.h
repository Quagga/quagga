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

#include <stdbool.h>
#include <errno.h>

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

/*------------------------------------------------------------------------------
 * VTY sock structure
 *
 * Used for VTY_TERM and VTY_SHELL_SERV VTY types, which are attached to TCP
 * and UNIX sockets, respectively.
 *
 * Also used for the associated listeners.
 */

typedef int thread_action(struct thread *) ;

union sock_action
{
  qps_action*    qnexus ;
  thread_action* thread ;
  void*          anon ;
} ;

union timer_action
{
  qtimer_action* qnexus ;
  thread_action* thread ;
  void*          anon ;
} ;

struct vio_sock_actions
{
  union sock_action     read ;
  union sock_action     write ;
  union timer_action    timer ;
};

typedef struct vio_sock* vio_sock ;
struct vio_sock
{
  int fd ;

  void* info ;                  /* for action routines                  */

  struct vio_sock_actions  action ;

  bool  read_open ;             /* read returns 0 if not open           */
  bool  write_open ;            /* write completes instantly if not open */
  int   error_seen ;            /* non-zero => failed                   */

  qps_file qf ;                 /* when running qnexus                  */

  struct thread *t_read;        /* when running threads                 */
  struct thread *t_write;

  unsigned long v_timeout;      /* time-out in seconds -- 0 => none     */
  bool  timer_running ;         /* true when timer is running           */

  qtimer qtr;                   /* when running qnexus                  */
  struct thread *t_timer;       /* when running threads                 */

} ;

enum
{
  on   = true,
  off  = false
} ;

enum vty_readiness      /* bit significant      */
{
  not_ready   = 0,
  read_ready  = 1,
  write_ready = 2,      /* takes precedence     */
  now_ready   = 4
} ;

/*------------------------------------------------------------------------------
 * The vty_io structure
 */

struct vty_io {
  struct vty*   vty ;           /* the related vty                      */
  char  *name ;                 /* for VTY_TERM is IP address)          */

  /* List of all vty_io objects                                         */
  struct dl_list_pair(vty_io) vio_list ;

  /* List of all vty_io that are in monitor state                       */
  struct dl_list_pair(vty_io) mon_list ;

  /* VTY type and sock stuff                                            */
  enum vty_type type;

  struct vio_sock  sock ;       /* for VTY_TERM and VTY_SHELL_SERV      */

  bool  half_closed ;           /* => on death watch list               */
  bool  closed ;                /* => all I/O terminated
                                      will also be half_closed          */

  const char* close_reason ;    /* message to be sent, once all other
                                   output has completed, giving reason
                                   for closing the VTY.                 */

  /* When writing configuration file                                    */
  enum vty_type real_type ;

  int   file_fd ;
  int   file_error ;

  /*--------------------------------------------------------------------*/
  /* Command line and related state                                     */

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

  /* Command output buffering                                           */
  vio_fifo_t    cmd_obuf ;

  vio_line_control cmd_lc ;

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

  /* In configure mode.                                                 */
  bool          config;
} ;

/*==============================================================================
 * Functions
 */

extern struct vty* uty_new (enum vty_type type, int sock_fd) ;

extern void uty_open_listeners(const char *addr, unsigned short port,
                                                             const char *path) ;
extern void uty_close_listeners(void) ;

extern void uty_watch_dog_start(void) ;
extern void uty_watch_dog_stop(void) ;

extern void uty_half_close (vty_io vio, const char* reason) ;
extern void uty_close (vty_io vio) ;

extern int uty_out (struct vty *vty, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
extern int uty_vout(struct vty *vty, const char *format, va_list args) ;
extern void uty_out_clear(vty_io vio) ;
extern void uty_out_fflush(vty_io vio, FILE* file) ;

extern void uty_set_height(vty_io vio) ;
extern void uty_cmd_output_start(vty_io vio) ;

extern void uty_sock_set_readiness(vio_sock sock, enum vty_readiness ready) ;
extern void uty_sock_set_timer(vio_sock sock, unsigned long timeout) ;

extern int uty_read (vty_io vio, keystroke steal) ;
extern int utysh_read (vty_io vio, qstring cl, qstring buf) ;


extern const char* uty_get_name(vty_io vio) ;

extern void uty_set_monitor(vty_io vio, bool on) ;

#endif /* _ZEBRA_VTY_IO_H */

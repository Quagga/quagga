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
 * VTY file structure
 *
 * Used
 */

typedef int thread_action(struct thread *) ;

union file_action
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

struct vio_file_actions
{
  union file_action     read ;
  union file_action     write ;
  union timer_action    timer ;
};

typedef struct vio_file* vio_file ;
struct vio_file
{
  int fd ;

  void* info ;                  /* for action routines                  */

  struct vio_file_actions  action ;

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

struct vty_line_control
{
  int   tba ;
} ;

enum
{
  off = false,
  on  = true
};

/*------------------------------------------------------------------------------
 *
 */

struct vty_io {
  /* List of all vty_io objects                                         */
  struct dl_list_pair(vty_io) vio_list ;

  bool  half_closed ;           /* => on death watch list               */
  bool  timed_out ;             /* closed by timer                      */

  /* List of all vty_io that are in monitor state                       */
  struct dl_list_pair(vty_io) mon_list ;

  /* The attached to this vty                                           */
  struct vty*   vty ;

  /* Type of VTY                                                        */
  enum vty_type type;

  /* File level stuff                                                   */
  struct vio_file  file ;

  /* "name" of the VTY (for VTY_TERM is IP address)                     */
  char  *name ;

  /* Keystroke stream and raw input buffer                              */
  keystroke_stream key_stream ;
  qstring_t     ibuf ;

  /*--------------------------------------------------------------------*/
  /* Command line and related state                                     */

  /* cli_drawn <=> the current prompt and user input occupy the current
   *               line on the screen.
   *
   * If cli_drawn is true, the following are valid:
   *
   *   cli_prompt_len    -- the length of the prompt part.
   *
   *   cli_extra_len     -- the length of any ^X at the cursor position
   *                        (for when blocked waiting for queued command)
   *
   *   cli_echo_suppress -- the user part of the command line is suppressed
   *
   * NB: cli_echo_suppress is only used for password entry.
   */
  int   cli_drawn ;

  int   cli_prompt_len ;        /* for drawn line (if any)              */
  int   cli_extra_len ;         /* for for drawn line (if any)          */

  bool  cli_echo_suppress ;     /* non-zero => suppress cli echo        */

  /* "cache" for prompt -- when node or host name changes, prompt does  */
  enum node_type  cli_prompt_node ;
  bool            cli_prompt_set ;
  qstring_t       cli_prompt_for_node ;

  /* State of the CLI
   *
   *   cli_blocked      -- blocked from processing keystrokes
   *   cmd_in_progress  -- command dispatched (may be queued)
   *
   *   cli_wait_more  -- is in "--more--" wait state
   *
   */
  bool          cli_blocked ;
  bool          cmd_in_progress ;

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

  qstring_t  cl ;
  qstring_t  clx ;

  /* CLI output buffering                                               */
  qstring_t     cli_vbuf ;      /* for uty_cli_out              */
  vio_fifo_t    cli_obuf ;

  /* Command output buffering                                           */
  qstring_t     cmd_vbuf ;      /* for uty_vout()               */
  vio_fifo_t    cmd_obuf ;

  bool          cmd_wait_more ;

  struct vty_line_control line_control ;
  /* Failure count for login attempts                                   */
  int   fail;

  /* History of commands                                                */
  vector_t      hist ;
  int           hp ;            /* History lookup current point */
  int           hindex;         /* History insert end point     */

  /* Window width/height.                                               */
  int width;
  int height;

  /* Configure lines.                                                   */
  int lines;

  /* Terminal monitor.                                                  */
  bool monitor ;

  /* In configure mode.                                                 */
  bool config;
} ;

/*==============================================================================
 * Functions
 */

extern struct vty*
uty_new (int fd, enum vty_type type) ;

extern void
uty_open_listeners(const char *addr, unsigned short port, const char *path) ;
extern void
uty_close_listeners(void) ;

extern void
uty_half_close (vty_io vio) ;
extern void
uty_close (vty_io vio) ;
extern void
uty_full_close (vty_io vio) ;
extern void
uty_watch_dog_stop(void) ;

extern int
uty_out (struct vty *vty, const char *format, ...)     PRINTF_ATTRIBUTE(2, 3) ;
extern int
uty_vout(struct vty *vty, const char *format, va_list args) ;
extern void
uty_out_discard(vty_io vio) ;

extern void
uty_file_set_read(vio_file file, bool on) ;
extern void
uty_file_set_write(vio_file file, bool on) ;
extern void
uty_file_set_timer(vio_file file, unsigned long timeout) ;

extern int
uty_read (vty_io vio, keystroke steal) ;
extern int
utysh_read (vty_io vio, qstring cl, qstring buf) ;


extern const char*
uty_get_name(vty_io vio) ;

extern void
uty_set_monitor(vty_io vio, bool on) ;

#endif /* _ZEBRA_VTY_IO_H */

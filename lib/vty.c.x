/*
 * Virtual terminal [aka TeletYpe] interface routine.
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
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

#include <zebra.h>
#include "miyagi.h"

#include "keystroke.h"
#include "vty_io.h"
#include "vty_cli.h"

#include "linklist.h"
#include "thread.h"
#include "buffer.h"
#include <lib/version.h>
#include "command.h"
#include "sockunion.h"
#include "memory.h"
#include "str.h"
#include "log.h"
#include "prefix.h"
#include "filter.h"
#include "vty.h"
#include "privs.h"
#include "network.h"

#include <arpa/telnet.h>
#include "qpthreads.h"
#include "qpnexus.h"




/* Needs to be qpthread safe */
qpt_mutex_t vty_mutex;
#ifdef NDEBUG
#define LOCK qpt_mutex_lock(&vty_mutex);
#define UNLOCK qpt_mutex_unlock(&vty_mutex);
#else
int vty_lock_count = 0;
int vty_lock_asserted = 0;
#define LOCK qpt_mutex_lock(&vty_mutex);++vty_lock_count;
#define UNLOCK --vty_lock_count;qpt_mutex_unlock(&vty_mutex);
#define ASSERTLOCKED if(vty_lock_count==0 && !vty_lock_asserted){vty_lock_asserted=1;assert(0);}
#endif

/*==============================================================================
 * To make vty qpthread safe we use a single mutex.
 *
 * vty and log recurse through each other, so the same mutex is used
 * for both, i.e. they are treated as being part of the same monitor.
 *
 * A recursive mutex is used.  This simplifies the calling from log to vty and
 * back again.  It also allows for the vty internals to call each other.
 *
 * There are some "uty" functions which assume the mutex is locked.
 *
 * vty is closely bound to the command handling -- the main vty structure
 * contains the context in which commands are parsed and executed.
 */

/*------------------------------------------------------------------------------
 *
 */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static int uty_out (struct vty *vty, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
static int uty_vout(struct vty *vty, const char *format, va_list args);

static int uty_vbuf(vty_io vio, const char *format, va_list args) ;
static void uty_cout (vty_io vio, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
static void uty_cwrite(vty_io vio, const char *this, int len) ;

static void uty_cecho(vty_io vio, const char *this, size_t len) ;
static void uty_cecho_n(vty_io vio, const char *this, size_t len, int n) ;

static void vty_event (enum vty_event, int, struct vty *);
static void uty_close (struct vty *vty);
static int uty_config_unlock (struct vty *vty);
static int uty_read (struct vty *vty);
static int uty_flush (struct vty *vty, int vty_sock);
static void vty_event_t (enum vty_event event, int sock, struct vty *vty);
static void vty_event_r (enum vty_event event, int sock, struct vty *vty);
static int uty_accept (int accept_sock);
static int uty_timeout (struct vty *vty);
static void vty_timeout_r (qtimer qtr, void* timer_info, qtime_t when);
static void vty_read_r (qps_file qf, void* file_info);
static void vty_flush_r (qps_file qf, void* file_info);
void uty_reset (void);

/* Extern host structure from command.c */
extern struct host host;

/*------------------------------------------------------------------------------
 * Static Variables
 */

/* For thread handling need the thread_master -- initialised in vty_init      */
static struct thread_master *master = NULL ;

/* In the qpthreads world, have nexus for the CLI and one for the Routeing
 * Engine.  Some commands are processed directly in the CLI, most have to be
 * sent to the Routeing Engine.
 */
static qpn_nexus cli_nexus      = NULL ;
static qpn_nexus routing_nexus  = NULL ;

/* List of all known vty                                                      */
static struct vty* vty_known    = NULL ;

/* List of all vty which are in monitor state.                                */
static struct vty* vty_monitors = NULL ;

/* Vty timeout value -- see "exec timeout" command                            */
static unsigned long vty_timeout_val = VTY_TIMEOUT_DEFAULT;

/* Vty access-class command                                                   */
static char *vty_accesslist_name = NULL;

/* Vty access-class for IPv6.                                                 */
static char *vty_ipv6_accesslist_name = NULL;

/* VTY server thread.                                                         */
//static vector Vvty_serv_thread;

/* Current directory -- initialised in vty_init()                             */
static char *vty_cwd = NULL;

/* Configure lock -- only one vty may be in CONFIG_NODE or above !            */
static int vty_config;

/* Login password check override.                                             */
static int no_password_check = 0;

/* Restrict unauthenticated logins?                                           */
static const u_char restricted_mode_default = 0;
static       u_char restricted_mode = 0;

/*------------------------------------------------------------------------------
 * Global Variables
 */

/* Integrated configuration file path -- for VTYSH                            */
char integrate_default[] = SYSCONFDIR INTEGRATE_DEFAULT_CONFIG ;

/*==============================================================================
 * Interlock mechanism for command execution -- qpthreads version.
 *
 * The meaning and validity of a given command line depends on all previous
 * command lines having been processed.
 *
 * Most commands are not executed in the CLI thread.  So, an interlock is
 * required so that a command is not dispatched until the previous one has
 * been processed to completion.
 *
 * In the struct vty is the "node" value -- this is the current command context.
 * There are a few other values which provide further context, but those are
 * not required by the command line handling.
 *
 * The processing of commands proceeds as follows:
 *
 * (1) no command queued and no output buffered
 *
 *    The command processor is idle, and waiting for the user to complete
 *    a new command.  The screen will look like:
 *
 *      <prompt>: user ..... input []
 *
 *    (where [] is the cursor).
 *
 *    When the user hits ENTER, a newline is output, and the command is parsed
 *    according to the current "node".  If there is a match, the command can be
 *    dispatched.  Otherwise, may need to work up the "node" tree until get a
 *    match or run out of tree.
 *
 *    If command parses successfully, proceed to:
 *
 *       (2) if the command can be executed in the CLI thread (or if not
 *           running qpthreads-wise).
 *
 *       (3) if the command must be sent to the routeing engine for execution.
 *
 *    If fails to parse the command, then an error message will be output and
 *    buffered.  There is now output pending -- proceed to (4).
 *
 *  (2) command can be executed in the CLI thread.
 *
 *    This is always the case if not running qpthreads-wise.
 *
 *    The command is executed immediately.
 *
 *    During execution the command may generate some output.  All that output
 *    is buffered until the command completes.
 *
 *    No input will be processed while the command is being executed.  So there
 *    will be no partial command in hand.
 *
 *  (3) command must be sent to the routing engine.
 *
 *    The command is dispatched to the routing engine, and there is now a
 *    "command queued".
 *
 *    While the command is executing, any output will be buffered.  So the
 *    command line processor is free to use the console and the user
 *    may enter a further command.
 *
 *    NB: If any context sensitive help is requested, it will be given in the
 *        current known context -- which may NOT be the actual context when the
 *        queued command completes.
 *
 *    If the queued command completes before the user hits ENTER, the command
 *    line will be wiped out, and proceeds to (4) with the partial command
 *    in hand.
 *
 *    If the queued command does not complete before the user hits enter,
 *    then stops processing input until the queued command does complete, and
 *    then proceeds to (4) with the command in hand, and the ENTER pending
 *
 *  (4) command completes (possibly because did not parse !).
 *
 *    Any buffered output is now actually output.  No input processing is
 *    done until all output has been sent.
 *
 *    TODO: The " --More-- " handling ????
 *
 *    Once the output completes, the current prompt is shown, followed by
 *    any partial command line.
 *
 *    Loop back to (1).
 *
 *
 */

/*==============================================================================
 * General VTY output.
 *
 * This is mostly used during command execution, to output the results of the
 * command.
 *
 * All these end up in uty_vout -- see below.
 *
 * For VTY_TERM and VTY_SHELL_SERV, all output is to the vty obuf.  So, all
 * output generated by a command is collected in the obuf, which is flushed
 * to the terminal or to the shell when the command completes.  This means
 * that:
 *
 * TODO: what does this all mean ??
 *
 *   * the breaking up of ******
 *
 *   * other output (in particular "monitor" output) can go directly to the
 *     terminal (or shell ?).
 *
 *   *
 *
 */

/*------------------------------------------------------------------------------
 * VTY output -- cf fprintf !
 */
extern int
vty_out (struct vty *vty, const char *format, ...)
{
  int result;

  LOCK
  va_list args;
  va_start (args, format);
  result = uty_vout(vty, format, args);
  va_end (args);
  UNLOCK
  return result;
}

/*------------------------------------------------------------------------------
 * VTY output -- output a given numnber of spaces
 */

/*                                         1         2         3         4 */
/*                                1234567890123456789012345678901234567890 */
const char vty_spaces_string[] = "                                        " ;
CONFIRM(VTY_MAX_SPACES == (sizeof(vty_spaces_string) - 1)) ;

extern int
vty_out_indent(struct vty *vty, int indent)
{
  while (indent > VTY_MAX_SPACES)
    {
      int ret = vty_out(vty, VTY_SPACES(indent)) ;
      if (ret < 0)
        return ret ;
      indent -= VTY_MAX_SPACES ;
    }
  return vty_out(vty, VTY_SPACES(indent)) ;
} ;

/*------------------------------------------------------------------------------
 * VTY output -- output the current time in standard form, to the second.
 */
extern void
vty_time_print (struct vty *vty, int cr)
{
  char buf [timestamp_buffer_len];

  quagga_timestamp(0, buf, sizeof(buf)) ;

  if (cr)
    vty_out (vty, "%s\n", buf);
  else
    vty_out (vty, "%s ", buf);

  return;
}

/*------------------------------------------------------------------------------
 * Say hello to vty interface.
 */
void
vty_hello (struct vty *vty)
{
  LOCK

#ifdef QDEBUG
  uty_out (vty, "%s%s", debug_banner, VTY_NEWLINE);
#endif
  if (host.motdfile)
    {
      FILE *f;
      char buf[4096];

      f = fopen (host.motdfile, "r");
      if (f)
        {
          while (fgets (buf, sizeof (buf), f))
            {
              char *s;
              /* work backwards to ignore trailing isspace() */
              for (s = buf + strlen (buf); (s > buf) && isspace ((int)*(s - 1));
                   s--);
              *s = '\0';
              uty_out (vty, "%s%s", buf, VTY_NEWLINE);
            }
          fclose (f);
        }
      else
        uty_out (vty, "MOTD file %s not found%s", host.motdfile, VTY_NEWLINE);
    }
  else if (host.motd)
    uty_out (vty, "%s", host.motd);

  UNLOCK
}


/*==============================================================================
 * CLI VTY output
 *
 * This is buffered separately from the general (command) VTY output above.
 *
 * Has a dedicated buffer in the struct vty, which is flushed regularly during
 * command processing.
 *
 * It is expected that can flush straight to the file, since this is running at
 * CLI speed.  However, if the CLI is being driven by something other than a
 * keyboard, or "monitor" output has filled the buffers, then may need to
 * have intermediate buffering.
 *
 *
 */

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf fprintf()
 *
 * No actual I/O takes place -- all "output" is to vio->cbuf and/or vio->cxbuf
 */
static void
uty_cout (vty_io vio, const char *format, ...)
{
  va_list args;
  int len ;

  ASSERTLOCKED

  va_start (args, format);
  len = uty_vbuf(vio, format, args) ;
  va_end(args);

  if (len > 0)
    uty_cwrite(vio, vio->vbuf, len) ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- cf write()
 *
 * No actual I/O takes place -- all "output" is to vio->cbuf and/or vio->cxbuf
 */
static void
uty_cwrite(vty_io vio, const char *this, int len)
{
  ASSERTLOCKED

  while (len > 0)
    {
      int take ;
      take = vio->cbuf_end - vio->cbuf_ptr ;

      if (take == 0)
        {
          take = vty_cout_buffer_size ;
          if (vio->cbuf == NULL)
            {
              vio->cbuf = XMALLOC(MTYPE_VTY_OUT_BUF, take) ;
              vio->cbuf_end = vio->cbuf + vty_cout_buffer_size ;
            }
          else
            {
              assert((vio->cbuf_ptr - vio->cbuf) == take) ;
              buffer_put(&vio->cxbuf, (u_char*)vio->cbuf, take) ;
            } ;
          vio->cbuf_ptr = vio->cbuf ;
        } ;

      if (take > len)
        take = len ;

      memcpy(vio->cbuf_ptr, this, take) ;
      vio->cbuf_ptr += take ;
      this          += take ;
      len           -= take ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo user input
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE)
 *
 * No actual I/O takes place -- all "output" is to vio->cbuf and/or vio->cxbuf
 */
static void
uty_cecho (vty_io vio, const char *this, size_t len)
{
  ASSERTLOCKED

  if (vio->cecho_suppress)
    return ;

  uty_cwrite(vio, this, len) ;
}

/*------------------------------------------------------------------------------
 * CLI VTY output -- echo given stuff 'n' times
 *
 * Do nothing if echo suppressed (eg in AUTH_NODE)
 *
 * No actual I/O takes place -- all "output" is to vio->cbuf and/or vio->cxbuf
 */
static void
uty_cecho_n(vty_io vio, const char *this, size_t len, int n)
{
  ASSERTLOCKED

  if (vio->cecho_suppress)
    return ;

  while (n-- > 0)
    uty_cwrite(vio, this, len) ;
}

/*==============================================================================
 * Output to vty which are set to "monitor".
 *
 *
 *
 */

/*------------------------------------------------------------------------------
 * Output logging information to all vty which are set to "monitor".
 */
extern void
uty_log(struct logline* ll, struct zlog *zl, int priority,
                                                 const char *format, va_list va)
{
  struct vty *vty;

  ASSERTLOCKED

  vty = sdl_head(vty_monitors) ;

  if (vty == NULL)
    return ;                    /* go no further if no "monitor" vtys   */

  /* Prepare line for output.                                           */
  uvzlog_line(ll, zl, priority, format, va, 1) ;  /* with crlf          */

  /* write to all known "monitor" vty                                   */
  while (vty != NULL)
    {
      vty_io vio = vty->vio ;

      if ((vio != NULL) && vio->monitor)
        {
          vio->monitor = 0 ;

          if (write(vio->fd, ll->line, ll->len) < 0)
            {
#if 0   // TODO: deal with error in write in uty_log()

              if (ERRNO_IO_RETRY(errno))
                /* Kernel buffer is full, probably too much debugging output, so just
                   drop the data and ignore. */
                return -1;
              /* Fatal I/O error. */
              vty->vio->monitor = 0; /* disable monitoring to avoid infinite recursion */
              uzlog(NULL, LOG_WARNING, "%s: write failed to vty client fd %d, closing: %s",
                        __func__, vty->vio->fd, safe_strerror(errno));
              buffer_reset(vty->vio->obuf);
              /* cannot call vty_close, because a parent routine may still try
                 to access the vty struct */
              vty->vio->status = VTY_CLOSE;
              shutdown(vty->vio->fd, SHUT_RDWR);
              return -1;
#endif
            }

          vio->monitor = 1 ;
        } ;
      vty = sdl_next(vty, monitor_list) ;
    } ;
} ;

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Allocate new vty struct
 *
 * Allocates and initialises vty_io structure, complete with:
 *
 *   Output buffer
 *   Input buffer
 *   qpselect file -- added to CLI nexus  ) if running CLI nexus
 *   qtimer                               )
 *
 * Adds to the known vty's -- which locks/unlocks momentarily.
 *
 */
struct vty *
vty_new (int fd, int type)
{
  struct vty *vty ;
  struct vty_io* vio ;

  vty = XCALLOC (MTYPE_VTY, sizeof (struct vty));
  vio = XCALLOC (MTYPE_VTY, sizeof (struct vty_io)) ;

  vty->vio = vio ;
  vio->vty = vty ;

  vio->obuf = buffer_new(0);            /* Use default buffer size. */

 // vio->cbuf = XCALLOC (MTYPE_VTY, VTY_BUFSIZ);
 // vio->max  = VTY_BUFSIZ;
 // vio->fd   = fd;
 // vio->type = type;

  if (cli_nexus)
    {
      vio->qf = qps_file_init_new(vio->qf, NULL);
      qps_add_file(cli_nexus->selection, vio->qf, vio->fd, vio);
      vio->qtr = qtimer_init_new(vio->qtr, cli_nexus->pile, vty_timeout_r, vio);
    }

  LOCK
  sdl_push(vty_known, vty, vty_list) ;
  UNLOCK

  return vty;
}



/*==============================================================================
 * VTY telnet stuff
 */

#define TELNET_OPTION_DEBUG

static const char* telnet_commands[256] =
{
  [tn_IAC  ] = "IAC",
  [tn_DONT ] = "DONT",
  [tn_DO   ] = "DO",
  [tn_WONT ] = "WONT",
  [tn_WILL ] = "WILL",
  [tn_SB   ] = "SB",
  [tn_GA   ] = "GA",
  [tn_EL   ] = "EL",
  [tn_EC   ] = "EC",
  [tn_AYT  ] = "AYT",
  [tn_AO   ] = "AO",
  [tn_IP   ] = "IP",
  [tn_BRK  ] = "BRK",
  [tn_DM   ] = "DM",
  [tn_NOP  ] = "NOP",
  [tn_SE   ] = "SE",
  [tn_EOR  ] = "EOR",
  [tn_ABORT] = "ABORT",
  [tn_SUSP ] = "SUSP",
  [tn_EOF  ] = "EOF",
} ;

static const char* telnet_options[256] =
{
  [to_Transmit_Binary] = "Binary",
  [to_Echo           ] = "Echo",
  [to_Suppress_GA    ] = "Suppress_GA",
  [to_Status         ] = "Status",
  [to_Timing_Mark    ] = "Timing_Mark",
  [to_Terminal_Type  ] = "Terminal_Type",
  [to_Window_Size    ] = "Window_Size",         /* NAWS */
  [to_Terminal_Speed ] = "Terminal_Speed",
  [to_Line_Mode      ] = "Line_Mode",
} ;

static const char* telnet_actions[2] =
{
  [ta_Ask  ]  = "Ask",
  [ta_Value]  = "Value",
} ;

static void
uty_cout_dec(vty_io vio, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cout(vio, "%s ", str) ;
  else
    uty_cout(vio, "%d ", (int)u) ;
} ;

static void
uty_cout_hex(vty_io vio, const char* str, unsigned char u)
{
  if (str != NULL)
    uty_cout(vio, "%s ", str) ;
  else
    uty_cout(vio, "0x%02x ", (unsigned)u) ;
} ;

/*------------------------------------------------------------------------------
 * Send telnet: "WILL TELOPT_ECHO"
 */
static void
vty_will_echo (struct vty *vty)
{
  unsigned char cmd[] = { IAC, WILL, TELOPT_ECHO };
  ASSERTLOCKED
  uty_cwrite (vty->vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "suppress Go-Ahead"
 */
static void
vty_will_suppress_go_ahead (struct vty *vty)
{
  unsigned char cmd[] = { IAC, WILL, TELOPT_SGA };
  ASSERTLOCKED
  uty_cwrite (vty->vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use linemode"
 */
static void
vty_dont_linemode (struct vty *vty)
{
  unsigned char cmd[] = { IAC, DONT, TELOPT_LINEMODE };
  ASSERTLOCKED
  uty_cwrite (vty->vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "Use window size"
 */
static void
vty_do_window_size (struct vty *vty)
{
  unsigned char cmd[] = { IAC, DO, TELOPT_NAWS };
  ASSERTLOCKED
  uty_cwrite (vty->vio, (char*)cmd, (int)sizeof(cmd));
}

/*------------------------------------------------------------------------------
 * Send telnet: "don't use lflow"
 */
#if 1 /* Currently not used. */
static void
vty_dont_lflow_ahead (struct vty *vty)
{
  unsigned char cmd[] = { IAC, DONT, TELOPT_LFLOW };
  ASSERTLOCKED
  uty_cwrite (vty->vio, (char*)cmd, (int)sizeof(cmd));
}
#endif /* 0 */

/*------------------------------------------------------------------------------
 * Process incoming Telnet Option(s)
 *
 * In particular: get telnet window size.
 */
static void
uty_telnet_command(vty_io vio, keystroke stroke)
{
  uint8_t* p ;
  uint8_t  o ;
  int left ;

#ifdef TELNET_OPTION_DEBUG
  /* Echo to the other end if required                                  */

  p    = stroke->buf ;
  left = stroke->len ;

  uty_cout_hex(vio, telnet_commands[tn_IAC], tn_IAC) ;

  if (left-- > 0)
    uty_cout_dec(vio, telnet_commands[*p], *p) ;
  ++p ;

  if (left-- > 0)
    uty_cout_dec(vio, telnet_options[*p], *p) ;
  ++p ;

  if (left > 0)
    {
      while(left-- > 0)
        uty_cout_hex(vio, NULL, *p++) ;

      if (stroke->flags & kf_truncated)
        uty_cout(vio, "... ") ;

      if (!(stroke->flags & kf_broken))
        {
          uty_cout_hex(vio, telnet_commands[tn_IAC], tn_IAC) ;
          uty_cout_hex(vio, telnet_commands[tn_SE], tn_SE) ;
        }
    } ;

  if (!(stroke->flags & kf_broken))
    uty_cout (vio, "BROKEN") ;

  uty_cout (vio, "\r\n") ;

#endif

  if (stroke->flags != 0)
    return ;                    /* go no further if broken              */

  p    = stroke->buf ;
  left = stroke->len ;

  passert(left >= 1) ;            /* must be if not broken !            */
  passert(stroke->value == *p) ;  /* or something is wrong              */

  ++p ;         /* step past X of IAC X */
  --left ;

  /* Decode the one command that is interesting -- "NAWS"               */
  switch (stroke->value)
  {
    case tn_SB:
      passert(left > 0) ;       /* or parser failed     */

      o = *p++ ;                /* the option byte      */
      --left ;
      switch(o)
      {
        case to_Window_Size:
          if (left != 4)
            {
              uzlog(NULL, LOG_WARNING,
                        "RFC 1073 violation detected: telnet NAWS option "
                        "should send %d characters, but we received %d",
                        (3 + 4 + 2), (3 + left + 2)) ;
            }
          else
            {
              vio->width   = *p++ << 8 ;
              vio->width  += *p++ ;
              vio->height  = *p++ << 8 ;
              vio->height += *p ;

#ifdef TELNET_OPTION_DEBUG
              uty_cout(vio, "TELNET NAWS window size negotiation completed: "
                                "width %d, height %d%s",
                                  vio->width, vio->height, TERM_NEWLINE) ;
#endif
            } ;
          break ;

        default:        /* no other IAC SB <option>     */
          break ;
      } ;
      break ;

    default:            /* no other IAC X               */
      break ;
  } ;

} ;

/******************************************************************************/


static void uty_cl_ensure (vty_io vio, unsigned length) ;
static char* uty_cl_terminate(vty_io vio) ;
static int uty_cl_insert (vty_io vio, const char* chars, int n) ;
static int uty_cl_overwrite (vty_io vio, char* chars, int n) ;
static int uty_cl_word_overwrite (vty_io vio, char *str) ;
static int uty_cl_forwards(vty_io vio, int n) ;
static int uty_cl_backwards(vty_io vio, int n) ;
static int uty_cl_del_forwards(vty_io vio, int n) ;
static int uty_cl_del_backwards(vty_io vio, int n) ;
static int uty_cl_bol (vty_io vio) ;
static int uty_cl_eol (vty_io vio) ;
static int uty_cl_word_forwards_delta(vty_io vio) ;
static int uty_cl_word_forwards(vty_io vio) ;
static int uty_cl_word_backwards_delta(vty_io vio, int eat_spaces) ;
static int uty_cl_word_backwards_pure (vty_io vio) ;
static int uty_cl_word_backwards (vty_io vio) ;
static int uty_cl_del_word_forwards(vty_io vio) ;
static int uty_cl_del_word_backwards(vty_io vio) ;
static int uty_cl_del_to_eol (vty_io vio) ;
static int uty_cl_clear_line(vty_io vio) ;
static int uty_cl_transpose_chars(vty_io vio) ;
static void uty_cl_prompt (vty_io vio) ;
static int uty_cl_redraw_line (vty_io vio) ;
static int uty_cl_redraw(vty_io vio) ;
static void uty_cl_hist_add (vty_io vio) ;
static void uty_cl_history_use(vty_io vio, int step) ;
static void uty_cl_next_line(vty_io vio) ;
static void uty_cl_previous_line (vty_io vio) ;
static void uty_cl_describe_fold (vty_io vio, int cmd_width,
                      unsigned int desc_width, struct desc *desc) ;
static void uty_cl_describe_command (vty_io vio) ;
static void uty_cl_complete_command (vty_io vio) ;


/*==============================================================================
 * Command line processing loop
 *
 * Process keystrokes until run out of stuff to do, or have a "command line"
 * that must now be executed.
 *
 * Processes the contents of the keystroke stream.  If exhausts that, will set
 * ready to read and return.  (To give some "sharing".)
 *
 * Returns: cli_null      -- no action required
 *          cli_dispatch  -- CR or LF received
 *          cli_ctrl_c    -- ^C received
 *          cli_ctrl_d    -- ^D received, on empty line
 *          cli_ctrl_z    -- ^Z received
 *
 * When returns the vio->cl is the state of the current command line.
 *
 */

#define CONTROL(X)  ((X) - '@')

static enum cli_returns
uty_cl_process(vty_io vio)
{
  struct keystroke stroke ;
  uint8_t u ;

  /* Now process as much as possible of what there is                   */
  while (keystroke_get(vio->key_stream, &stroke))
    {
      if (stroke.flags != 0)
        {
          /* TODO: deal with broken keystrokes                          */
          continue ;
        } ;

      switch (stroke.type)
      {
        /* Straightforward character -----------------------------------*/
        /* Note: only interested in 8-bit characters !                  */
        case ks_char:
          u = (uint8_t)stroke.value ;

          switch (stroke.value)
          {
            case CONTROL('A'):
              uty_cl_bol (vio);
              break;

            case CONTROL('B'):
              uty_cl_backwards(vio, 1);
              break;

            case CONTROL('C'):
              return cli_ctrl_c ;       /* Exit on ^C ..................*/

            case CONTROL('D'):
              if (vio->cl.ep == 0)      /* if at start of empty line    */
                return cli_ctrl_d ;     /* Exit on ^D ..................*/

              uty_cl_del_forwards(vio, 1);
              break;

            case CONTROL('E'):
              uty_cl_eol (vio);
              break;

            case CONTROL('F'):
              uty_cl_forwards(vio, 1);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cl_del_backwards(vio, 1);
              break;

            case CONTROL('K'):
              uty_cl_del_to_eol (vio);
              break;

            case CONTROL('N'):
              uty_cl_next_line (vio);
              break;

            case CONTROL('P'):
              uty_cl_previous_line (vio);
              break;

            case CONTROL('T'):
              uty_cl_transpose_chars (vio);
              break;

            case CONTROL('U'):
              uty_cl_clear_line(vio);
              break;

            case CONTROL('W'):
              uty_cl_del_word_backwards (vio);
              break;

            case CONTROL('Z'):
              return cli_ctrl_z ;       /* Exit on ^Z ..................*/
              break;

            case '\n':
            case '\r':
              return cli_dispatch ;     /* Exit on CR or LF.............*/

            case '\t':
              uty_cl_complete_command (vio);
              break;

            case '?':
              if (vty->node == AUTH_NODE || vty->node == AUTH_ENABLE_NODE)
                uty_cl_insert (vio, (char*)&u, 1);
              else
                uty_cl_describe_command (vio);
              break;

            default:
              if ((stroke.value >= 0x20) && (stroke.value < 0x7F))
                uty_cl_insert (vio, (char*)&u, 1) ;
              break;
            }
          break ;

        /* ESC X -------------------------------------------------------------*/
        case ks_esc:
          switch (stroke.value)
          {
            case 'b':
              uty_cl_word_backwards (vio);
              break;

            case 'f':
              uty_cl_word_forwards (vio);
              break;

            case 'd':
              uty_cl_del_word_forwards (vio);
              break;

            case CONTROL('H'):
            case 0x7f:
              uty_cl_del_word_backwards (vio);
              break;

            default:
              break;
          } ;
          break ;

        /* ESC [ X -----------------------------------------------------------*/
        case ks_csi:
          if (stroke.len != 0)
            break ;             /* only recognise 3 byte sequences      */

          switch (stroke.value)
          {
            case ('A'):
              uty_cl_previous_line (vio);
              break;

            case ('B'):
              uty_cl_next_line (vio);
              break;

            case ('C'):
              uty_cl_forwards(vio, 1);
              break;

            case ('D'):
              uty_cl_backwards(vio, 1);
              break;
            default:
              break ;
          } ;
          break ;

        /* Telnet Command ----------------------------------------------------*/
        case ks_iac:
          uty_telnet_command(vio, &stroke) ;
          break ;

        /* Single byte escape ------------------------------------------------*/
        default:
          zabort("unknown keystroke type") ;
      } ;

      /* After each keystroke.....                                      */


    } ;



  /* Check status. */
  if (vio->status == VTY_CLOSE)
    uty_close (vty);
  else
    {
      vty_event (VTY_WRITE, vio->fd, vty);
      vty_event (VTY_READ,  vio->fd, vty);
    }

  return 0;
}











/*==============================================================================
 * Command line operations
 */

static const char telnet_backward_char = 0x08;
static const char telnet_space_char = ' ';

/*------------------------------------------------------------------------------
 * Ensure length of command line buffer.
 *
 * Allocate or reallocate buffer so is large enough for the given length, PLUS
 * one extra for '\0'.
 */
static void
uty_cl_ensure (vty_io vio, unsigned length)
{
  ASSERTLOCKED

  if (vio->cl.size <= length)   /* allows for trailing '\n'     */
    {
      if (vio->cl.size == 0)
        {
          vio->cl.size = 200 ;
          vio->cl.buf  = XMALLOC(MTYPE_VTY, vio->cl.size) ;
        }
      else
        {
          vio->cl.size *= 2 ;
          vio->cl.buf   = XREALLOC (MTYPE_VTY, vio->cl.buf, vio->cl.size) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Terminate the command line.
 *
 * Allocate or reallocate buffer so is large enough for the given length, plus
 * one extra for '\0'.
 */
static char*
uty_cl_terminate(vty_io vio)
{
  ASSERTLOCKED

  uty_cl_ensure (vio, vio->cl.ep) ;

  vio->cl.buf[vio->cl.ep] = '\0' ;

  return vio->cl.buf ;
} ;

/*------------------------------------------------------------------------------
 * Insert 'n' characters at current position in the command line
 *
 * Returns number of characters inserted -- ie 'n'
 */
static int
uty_cl_insert (vty_io vio, const char* chars, int n)
{
  int after ;

  ASSERTLOCKED

  assert((vio->cl.cp <= vio->cl.ep) && (n >= 0)) ;

  if (n == 0)
    return n ;

  uty_cl_ensure (vio, vio->cl.ep + n) ;

  after = vio->cl.ep - vio->cl.cp ;
  if (after != 0)
    memmove (&vio->cl.buf[vio->cl.cp + n], &vio->cl.buf[vio->cl.cp], after) ;

  memmove(&vio->cl.buf[vio->cl.cp], chars, n) ;

  uty_cecho(vio, (char*)&vio->cl.buf[vio->cl.cp], after + n) ;

  if (after != 0)
    uty_cecho_n(vio, &telnet_backward_char, 1, after) ;

  vio->cl.cp += n ;
  vio->cl.ep += n ;

  return n ;
} ;

/*------------------------------------------------------------------------------
 * Overstrike 'n' characters at current position in the command line
 *
 * Move current position forwards.
 *
 * Returns number of characters inserted -- ie 'n'
 */
static int
uty_cl_overwrite (vty_io vio, char* chars, int n)
{
  ASSERTLOCKED

  assert((vio->cl.cp <= vio->cl.ep) && (n >= 0)) ;

  if (n > 0)
    {
      if ((vio->cl.cp + n) > vio->cl.ep)
        {
          vio->cl.ep = vio->cl.cp + n ;
          uty_cl_ensure (vio, vio->cl.ep) ;
        } ;

      memmove(&vio->cl.buf[vio->cl.cp], chars, n) ;
      uty_cecho(vio, chars, n) ;

      vio->cl.cp += n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Insert a word into vty interface with overwrite mode.
 *
 * NB: Assumes result will then be the end of the line.
 *
 * Returns number of characters inserted -- ie length of string
 */
static int
uty_cl_word_overwrite (vty_io vio, char *str)
{
  int n ;
  ASSERTLOCKED

  n = uty_cl_overwrite(vio, str, strlen(str)) ;

  vio->cl.ep = vio->cl.cp ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Forward 'n' characters -- stop at end of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cl_forwards(vty_io vio, int n)
{
  int c ;
  ASSERTLOCKED

  c = vio->cl.ep - vio->cl.cp ;
  if (n > c)
    n = c ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cecho(vio, (char*)&vio->cl.buf[vio->cl.cp], n) ;
      vio->cl.cp += n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Backwards 'n' characters -- stop at start of line.
 *
 * Returns number of characters actually moved
 */
static int
uty_cl_backwards(vty_io vio, int n)
{
  int c ;
  ASSERTLOCKED

  c = vio->cl.ep - vio->cl.cp ;
  if (n > c)
    n = c ;

  assert(n >= 0) ;

  if (n > 0)
    {
      uty_cecho(vio, &telnet_backward_char, n);
      vio->cl.cp -= n ;
    } ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters -- forwards -- stop at end of line.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cl_del_forwards(vty_io vio, int n)
{
  int after ;

  ASSERTLOCKED

  after = (vio->cl.ep - vio->cl.cp) ;
  assert((n >= 0) && (after >= 0)) ;

  if ((n == 0) || (after == 0))
    return 0 ;                          /* completion need here? */

  if (n > after)
    n = after ;

  after -= n ;

  if (after > 0)
    {
      memmove (&vio->cl.buf[vio->cl.cp], &vio->cl.buf[vio->cl.cp + n], after) ;
      uty_cecho(vio, (char*)&vio->cl.buf[vio->cl.cp], after) ;
    } ;

  uty_cecho_n(vio, &telnet_space_char, 1, n) ;
  uty_cecho_n(vio, &telnet_backward_char, 1, after + n) ;

  vio->cl.ep -= n ;

  return n ;
}

/*------------------------------------------------------------------------------
 * Delete 'n' characters before the point.
 *
 * Returns number of characters actually deleted.
 */
static int
uty_cl_del_backwards(vty_io vio, int n)
{
  return uty_cl_del_forwards(vio, uty_cl_backwards(vio, n)) ;
}

/*------------------------------------------------------------------------------
 * Move to the beginning of the line.
 *
 * Returns number of characters moved over.
 */
static int
uty_cl_bol (vty_io vio)
{
  return uty_cl_backwards(vio, vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Move to the end of the line.
 *
 * Returns number of characters moved over
 */
static int
uty_cl_eol (vty_io vio)
{
  return uty_cl_forwards(vio, vio->cl.ep - vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Forward word delta -- distance to start of next word.
 *
 * Return number of characters to step over to reach next word.
 *
 * Steps over non-space characters and then any spaces.
 */
static int
uty_cl_word_forwards_delta(vty_io vio)
{
  unsigned tp = vio->cl.cp ;

  ASSERTLOCKED ;

  assert(vio->cl.cp <= vio->cl.ep) ;

  while ((tp < vio->cl.ep) && (vio->cl.buf[tp] != ' '))
    ++tp ;

  while ((tp < vio->cl.ep) && (vio->cl.buf[tp] == ' '))
    ++tp ;

  return tp - vio->cl.cp ;
} ;

/*------------------------------------------------------------------------------
 * Forward word -- move to start of next word.
 *
 * Moves past any non-spaces, then past any spaces.
 */
static int
uty_cl_word_forwards(vty_io vio)
{
  return uty_cl_forwards(vio, uty_cl_word_forwards_delta(vio)) ;
} ;

/*------------------------------------------------------------------------------
 * Backward word delta -- distance to start of next word, back.
 *
 * Return number of characters to step over to reach next word.
 *
 * If "eat_spaces", starts by stepping over spaces.
 * Steps back until next (backwards) character is space, or hits start of line.
 */
static int
uty_cl_word_backwards_delta(vty_io vio, int eat_spaces)
{
  int tp = vio->cl.cp ;

  ASSERTLOCKED ;

  assert(vio->cl.cp <= vio->cl.ep) ;

  if (eat_spaces)
    while ((tp > 0) && (vio->cl.buf[tp - 1] == ' '))
      --tp ;

  while ((tp > 0) && (vio->cl.buf[tp - 1] != ' '))
    --tp ;

  return vio->cl.cp - tp ;
} ;

/*------------------------------------------------------------------------------
 * Backward word, but not trailing spaces.
 *
 * Move back until next (backwards) character is space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cl_word_backwards_pure (vty_io vio)
{
  return uty_cl_backwards(vio, uty_cl_word_backwards_delta(vio, 0)) ;
} ;

/*------------------------------------------------------------------------------
 * Backward word -- move to start of previous word.
 *
 * Moves past any spaces, then move back until next (backwards) character is
 * space or start of line.
 *
 * Returns number of characters stepped over.
 */
static int
uty_cl_word_backwards (vty_io vio)
{
  return uty_cl_backwards(vio, uty_cl_word_backwards_delta(vio, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Delete to end of word -- forwards.
 *
 * Deletes any leading spaces, then deletes upto next space or end of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cl_del_word_forwards(vty_io vio)
{
  return uty_cl_del_forwards(vio, uty_cl_word_forwards_delta(vio)) ;
}

/*------------------------------------------------------------------------------
 * Delete to start of word -- backwards.
 *
 * Deletes any trailing spaces, then deletes upto next space or start of line.
 *
 * Returns number of characters deleted.
 */
static int
uty_cl_del_word_backwards(vty_io vio)
{
  return uty_cl_del_backwards(vio, uty_cl_word_backwards_delta(vio, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Kill rest of line from current point.
 *
 * Returns number of characters deleted.
 */
static int
uty_cl_del_to_eol (vty_io vio)
{
  return uty_cl_del_forwards(vio, vio->cl.ep - vio->cl.cp) ;
} ;

/*------------------------------------------------------------------------------
 * Kill line from the beginning.
 *
 * Returns number of characters deleted.
 */
static int
uty_cl_clear_line(vty_io vio)
{
  uty_cl_bol(vio) ;
  return uty_cl_del_to_eol(vio) ;
} ;

/*------------------------------------------------------------------------------
 * Transpose chars before or at the point.
 *
 * Return number of characters affected.
 */
static int
uty_cl_transpose_chars(vty_io vio)
{
  char chars[2] ;

  ASSERTLOCKED

  /* Give up if < 2 characters or at start of line.                     */
  if ((vio->cl.ep < 2) || (vio->cl.cp < 1))
    return 0 ;

  /* Move back to first of characters to exchange                       */
  if (vio->cl.cp == vio->cl.ep)
    uty_cl_backwards(vio, 2) ;
  else
    uty_cl_backwards(vio, 1) ;

  /* Pick up in the new order                                           */
  chars[0] = vio->cl.buf[vio->cl.cp + 1] ;
  chars[1] = vio->cl.buf[vio->cl.cp + 0] ;

  /* And overwrite                                                      */
  return uty_cl_overwrite(vio, chars, 2) ;
} ;

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * If this is a VTY of type VTY_TERM, output the prompt.
 *
 *
 *
 * TODO: fix prompting....
 */
static void
uty_cl_prompt (vty_io vio)
{
  struct utsname names;
  const char* hostname;
  const char* prompt ;

  ASSERTLOCKED

  if (vio->type == VTY_TERM)
    {
      hostname = host.name;
      if (!hostname)
        {
          uname (&names);
          hostname = names.nodename;
        }

      prompt = cmd_prompt(vty->node) ;
      if (prompt == NULL)
        {
          zlog_err("vty %s has node %d", vio->cbuf, vty->node) ;
          prompt = "%s ???: " ;
        } ;

      uty_cout (vio, prompt, hostname);
    }
}

/*------------------------------------------------------------------------------
 * Redraw entire command line, leaving current position at the end of the line
 *
 * Assumes is positioned at start of the command line (just after the prompt),
 * and there is nothing that needs to be wiped out, first.
 *
 * Returns number of characters in line.
 */
static int
uty_cl_redraw_line (vty_io vio)
{
  ASSERTLOCKED

  if (vio->cl.ep != 0)
    uty_cecho(vio, (char*)vio->cl.buf, vio->cl.ep) ;

  return (vio->cl.cp = vio->cl.ep) ;
} ;

/*------------------------------------------------------------------------------
 * Redraw prompt and entire command line, leaving current position at the end
 * of the line.
 *
 * Assumes is positioned at start of line, and there is nothing that needs to
 * be wiped out, first.
 *
 * Returns number of characters in line.
 */
static int
uty_cl_redraw(vty_io vio)
{
  uty_cl_prompt(vio) ;
  return uty_cl_redraw_line(vio) ;
} ;

/*==============================================================================
 * Command line history handling
 */

/*------------------------------------------------------------------------------
 * Add current command line to the history buffer.
 */
static void
uty_cl_hist_add (vty_io vio)
{
  int index;

  ASSERTLOCKED

  /* Do nothing if current command line is empty                        */
  if (vio->cl.ep == 0)
    return;

  /* Make sure line is terminated                                       */
  uty_cl_terminate(vio) ;

  index = vio->hindex - 1 ;
  if (index < 0)
    index = VTY_MAXHIST - 1 ;

  /* Ignore the same string as previous one.                            */
  if ((vio->hist[index] != NULL) &&
      (strcmp(vio->cl.buf, vio->hist[index]) == 0))
    {
      vio->hp = vio->hindex ;
      return;
    } ;

  /* Insert history entry. */
  if (vio->hist[vio->hindex])
    XFREE (MTYPE_VTY_HIST, vio->hist[vio->hindex]);
  vio->hist[vio->hindex] = XSTRDUP (MTYPE_VTY_HIST, vio->cl.buf) ;

  /* History index rotation. */
  vio->hindex++;
  if (vio->hindex == VTY_MAXHIST)
    vio->hindex = 0;

  vio->hp = vio->hindex;
} ;

/*------------------------------------------------------------------------------
 * Replace command line by current history.
 *
 * This function is called from vty_next_line and vty_previous_line.
 */
static void
uty_cl_history_use(vty_io vio, int step)
{
  int       index ;
  unsigned  length ;
  char*     hist ;

  ASSERTLOCKED

  /* See if have anything usable                                        */
  index = vio->hp ;

  if ((step > 0) && (index == vio->hindex))
    return ;    /* cannot step forward past the insertion point         */

  index += step ;
  if      (index < 0)
    index = VTY_MAXHIST - 1 ;
  else if (index >= VTY_MAXHIST) ;
    index = 0 ;

  if ((step < 0) && (index == vio->hindex))
    return ;    /* cannot step back to the insertion point              */

  hist = vio->hist[index] ;
  if (hist == NULL)
    return ;    /* cannot step to unused entry                          */

  vio->hp = index;

  /* Move back to the start of the current line                         */
  uty_cl_bol(vio) ;

  /* Get previous line from history buffer and echo that                */
  length = strlen(hist) ;
  uty_cl_ensure(vio, length) ;

  if (length != 0)
    {
      memcpy(vio->cl.buf, hist, length) ;
      uty_cecho(vio, hist, length) ;
    } ;

  /* Move "cursor" to end of the new line                               */
  vio->cl.cp = length;

  /* Set new end of of line -- clearing stuff if old line was longer    */
  if (length < vio->cl.ep)
    uty_cl_del_to_eol(vio) ;
  else
    vio->cl.ep = length ;

  return ;
} ;

/*------------------------------------------------------------------------------
 * Use next history line, if any.
 */
static void
uty_cl_next_line(vty_io vio)
{
  uty_cl_history_use(vio, +1) ;
}

/*------------------------------------------------------------------------------
 * Use previous history line, if any.
 */
static void
uty_cl_previous_line (vty_io vio)
{
  uty_cl_history_use(vio, -1) ;
}

/*==============================================================================
 * Command Description
 *
 */

/* NB: this is a console level operation        */
static void
uty_cl_describe_fold (vty_io vio, int cmd_width,
                      unsigned int desc_width, struct desc *desc)
{
  char *buf;
  const char *cmd, *p;
  int pos;

  ASSERTLOCKED

  cmd = desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd;

  if (desc_width <= 0)
    {
      uty_cout (vio, "  %-*s  %s%s", cmd_width, cmd, desc->str, TERM_NEWLINE);
      return;
    }

  buf = XCALLOC (MTYPE_TMP, strlen (desc->str) + 1);

  for (p = desc->str; strlen (p) > desc_width; p += pos + 1)
    {
      for (pos = desc_width; pos > 0; pos--)
      if (*(p + pos) == ' ')
        break;

      if (pos == 0)
      break;

      strncpy (buf, p, pos);
      buf[pos] = '\0';
      uty_cout (vio, "  %-*s  %s%s", cmd_width, cmd, buf, TERM_NEWLINE);

      cmd = "";
    }

  uty_cout (vio, "  %-*s  %s%s", cmd_width, cmd, p, TERM_NEWLINE);

  XFREE (MTYPE_TMP, buf);
}

/* Describe matched command function. */
/* NB: this is a console level command  */
static void
uty_cl_describe_command (vty_io vio)
{
   int ret;
  vector vline;
  vector describe;
  unsigned int i, width, desc_width;
  struct desc *desc, *desc_cr = NULL;

  ASSERTLOCKED

  /* Construct vector of tokens from current command line               */
  uty_cl_terminate(vio) ;
  vline = cmd_make_strvec (vio->cl.buf);

  /* In case of '> ?' or line ending in ' '
   *
   * Note that if there is a vector of tokens, then there is at least one
   * token, so can guarantee that vio->cl.ep >= 1 !
   */
  if ((vline == NULL) || (isspace ((int) vio->cbuf[vio->cl.ep - 1])))
    vline = cmd_add_to_strvec(vline, "") ;

  describe = cmd_describe_command (vline, vio->vty->node, &ret);

  uty_cout (vio, TERM_NEWLINE);

  /* Ambiguous error. */
  switch (ret)
    {
    case CMD_ERR_AMBIGUOUS:
      uty_cout (vio, "%% Ambiguous command.%s", TERM_NEWLINE);
      goto out;
      break;
    case CMD_ERR_NO_MATCH:
      uty_cout (vio, "%% There is no matched command.%s", TERM_NEWLINE);
      goto out;
      break;
    }

  /* Get width of command string. */
  width = 0;
  for (i = 0; i < vector_active (describe); i++)
    if ((desc = vector_slot (describe, i)) != NULL)
      {
        unsigned int len;

        if (desc->cmd[0] == '\0')
          continue;

        len = strlen (desc->cmd);
        if (desc->cmd[0] == '.')
          len--;

        if (width < len)
          width = len;
      }

  /* Get width of description string. */
  desc_width = vio->width - (width + 6);

  /* Print out description. */
  for (i = 0; i < vector_active (describe); i++)
    if ((desc = vector_slot (describe, i)) != NULL)
      {
        if (desc->cmd[0] == '\0')
          continue;

        if (strcmp (desc->cmd, command_cr) == 0)
          {
            desc_cr = desc;
            continue;
          }

        if (!desc->str)
          uty_cout (vio, "  %-s%s",
                   desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd,
                   TERM_NEWLINE);
        else if (desc_width >= strlen (desc->str))
          uty_cout (vio, "  %-*s  %s%s", width,
                   desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd,
                   desc->str, TERM_NEWLINE);
        else
          uty_cl_describe_fold (vio, width, desc_width, desc);

#if 0
        uty_cout (vio, "  %-*s %s%s", width
                 desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd,
                 desc->str ? desc->str : "", TERM_NEWLINE);
#endif /* 0 */
      }

  if ((desc = desc_cr))
    {
      if (!desc->str)
        uty_cout (vio, "  %-s%s",
                 desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd,
                 TERM_NEWLINE);
      else if (desc_width >= strlen (desc->str))
        uty_cout (vio, "  %-*s  %s%s", width,
                 desc->cmd[0] == '.' ? desc->cmd + 1 : desc->cmd,
                 desc->str, TERM_NEWLINE);
      else
        uty_cl_describe_fold (vio, width, desc_width, desc);
    }

out:
  cmd_free_strvec (vline);
  if (describe)
    vector_free (describe);

  uty_cl_redraw(vio);
}

/*==============================================================================
 * Command completion
 *
 *
 */
static void
uty_cl_complete_command (vty_io vio)
{
  enum node_type node ;
  unsigned i ;
  int ret ;
  vector matched ;
  vector vline ;

  ASSERTLOCKED

  node = vio->vty->node ;

  if (node == AUTH_NODE || node == AUTH_ENABLE_NODE)
    return;

  /* Construct vector of tokens from current command line               */
  uty_cl_terminate(vio) ;
  vline = cmd_make_strvec (vio->cl.buf);

  /* In case of '> ?' or line ending in ' '
   *
   * Note that if there is a vector of tokens, then there is at least one
   * token, so can guarantee that vio->cl.ep >= 1 !
   */
  if ((vline == NULL) || (isspace ((int) vio->cl.buf[vio->cl.ep - 1])))
    vline = cmd_add_to_strvec(vline, "") ;

  /* Try and match the tokenised command line                           */
  matched = cmd_complete_command (vline, node, &ret);

  cmd_free_strvec (vline);

  uty_cout (vio, TERM_NEWLINE);
  switch (ret)
    {
    case CMD_ERR_AMBIGUOUS:
      uty_cout (vio, "%% Ambiguous command.%s", TERM_NEWLINE);
      uty_cl_redraw(vio);
      break;
    case CMD_ERR_NO_MATCH:
      uty_cout (vio, "%% There is no matched command.%s", TERM_NEWLINE);
      uty_cl_redraw(vio);
      break;
    case CMD_COMPLETE_FULL_MATCH:
      uty_cl_redraw(vio);
      uty_cl_word_backwards_pure (vio);
      uty_cl_word_overwrite (vio, vector_get_item(matched, 0));
      uty_cl_insert(vio, " ", 1);
      break;
    case CMD_COMPLETE_MATCH:
      uty_cl_redraw(vio);
      uty_cl_word_backwards_pure (vio);
      uty_cl_word_overwrite (vio, vector_get_item(matched, 0));
      break;
    case CMD_COMPLETE_LIST_MATCH:
      for (i = 0; i < vector_end(matched); i++)
        {
          if (i != 0 && ((i % 6) == 0))
            uty_cout (vio, "%s", TERM_NEWLINE);
          uty_cout (vio, "%-10s ", (char*)vector_get_item(matched, i));
        }
      uty_cout (vio, "%s", TERM_NEWLINE);

      uty_cl_redraw(vio);
      break;
    case CMD_ERR_NOTHING_TODO:
      uty_cl_redraw(vio);
      break;
    default:
      break;
    }

  cmd_free_strvec(matched);
} ;

/*==============================================================================
 *
 */

/* Authentication of vty */
static void
vty_auth (struct vty *vty, char *buf)
{
  char *passwd = NULL;
  enum node_type next_node = 0;
  int fail;
  char *crypt (const char *, const char *);

  ASSERTLOCKED

  switch (vty->node)
    {
    case AUTH_NODE:
      if (host.encrypt)
	passwd = host.password_encrypt;
      else
	passwd = host.password;
      if (host.advanced)
	next_node = host.enable ? VIEW_NODE : ENABLE_NODE;
      else
	next_node = VIEW_NODE;
      break;
    case AUTH_ENABLE_NODE:
      if (host.encrypt)
	passwd = host.enable_encrypt;
      else
	passwd = host.enable;
      next_node = ENABLE_NODE;
      break;
    }

  if (passwd)
    {
      if (host.encrypt)
	fail = strcmp (crypt(buf, passwd), passwd);
      else
	fail = strcmp (buf, passwd);
    }
  else
    fail = 1;

  if (! fail)
    {
      vty->vio->fail = 0;
      vty->node = next_node;	/* Success ! */
    }
  else
    {
      vty->vio->fail++;
      if (vty->vio->fail >= 3)
	{
	  if (vty->node == AUTH_NODE)
	    {
	      uty_out (vty, "%% Bad passwords, too many failures!%s", VTY_NEWLINE);
	      vty->vio->status = VTY_CLOSE;
	    }
	  else
	    {
	      /* AUTH_ENABLE_NODE */
	      vty->vio->fail = 0;
	      uty_out (vty, "%% Bad enable passwords, too many failures!%s", VTY_NEWLINE);
	      vty->node = restricted_mode ? RESTRICTED_NODE : VIEW_NODE;
	    }
	}
    }
}


/* ^C stop current input and do not add command line to the history. */
static void
vty_stop_input (struct vty *vty)
{
  ASSERTLOCKED
  vty->vio->cp = vty->vio->length = 0;
  vty_clear_buf (vty);
  uty_cout (vty, "%s", VTY_NEWLINE);

  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      /* Nothing to do. */
      break;
    case CONFIG_NODE:
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case BGP_NODE:
    case RMAP_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case KEYCHAIN_KEY_NODE:
    case MASC_NODE:
    case VTY_NODE:
      uty_config_unlock (vty);
      vty->node = ENABLE_NODE;
      break;
    default:
      /* Unknown node, we have to ignore it. */
      break;
    }
  vty_prompt (vty);

  /* Set history pointer to the latest one. */
  vty->vio->hp = vty->vio->hindex;
}



/* When '^D' is typed at the beginning of the line we move to the down
   level. */
static void
vty_down_level (struct vty *vty)
{
  ASSERTLOCKED
  uty_out (vty, "%s", VTY_NEWLINE);
  (*config_exit_cmd.func)(NULL, vty, 0, NULL);
  vty_prompt (vty);    /* TODO: should command action issue prompt ?    */
  vty->vio->cp = 0;
}

/* When '^Z' is received from vty, move down to the enable mode. */
static void
vty_end_config (struct vty *vty)
{
  ASSERTLOCKED
  uty_out (vty, "%s", VTY_NEWLINE);

  switch (vty->node)
    {
    case VIEW_NODE:
    case ENABLE_NODE:
    case RESTRICTED_NODE:
      /* Nothing to do. */
      break;
    case CONFIG_NODE:
    case INTERFACE_NODE:
    case ZEBRA_NODE:
    case RIP_NODE:
    case RIPNG_NODE:
    case BGP_NODE:
    case BGP_VPNV4_NODE:
    case BGP_IPV4_NODE:
    case BGP_IPV4M_NODE:
    case BGP_IPV6_NODE:
    case BGP_IPV6M_NODE:
    case RMAP_NODE:
    case OSPF_NODE:
    case OSPF6_NODE:
    case ISIS_NODE:
    case KEYCHAIN_NODE:
    case KEYCHAIN_KEY_NODE:
    case MASC_NODE:
    case VTY_NODE:
      uty_config_unlock (vty);
      vty->node = ENABLE_NODE;
      break;
    default:
      /* Unknown node, we have to ignore it. */
      break;
    }

  vty_prompt (vty);   /* TODO: command action and prompt        */
  vty->vio->cp = 0;
}


/*------------------------------------------------------------------------------
 *
 */
/* Command execution over the vty interface. */
static int
vty_command (struct vty *vty, char *buf)
{
  int ret;
  vector vline;
  const char *protocolname;

  ASSERTLOCKED

  /* Split readline string up into the vector */
  vline = cmd_make_strvec (buf);

  if (vline == NULL)
    return CMD_SUCCESS;

#ifdef CONSUMED_TIME_CHECK
  {
    RUSAGE_T before;
    RUSAGE_T after;
    unsigned long realtime, cputime;

    GETRUSAGE(&before);
#endif /* CONSUMED_TIME_CHECK */

  UNLOCK
  ret = cmd_execute_command (vline, vty, NULL, routing_nexus, cli_nexus, 0);
  LOCK

  /* Get the name of the protocol if any */
  protocolname = uzlog_get_proto_name(NULL);

#ifdef CONSUMED_TIME_CHECK
    GETRUSAGE(&after);
    if ((realtime = thread_consumed_time(&after, &before, &cputime)) >
        CONSUMED_TIME_CHECK)
      /* Warn about CPU hog that must be fixed. */
      uzlog(NULL, LOG_WARNING, "SLOW COMMAND: command took %lums (cpu time %lums): %s",
                realtime/1000, cputime/1000, buf);
  }
#endif /* CONSUMED_TIME_CHECK */

  if (ret != CMD_SUCCESS)
    switch (ret)
      {
      case CMD_WARNING:
        if (vty->vio->type == VTY_FILE)
          uty_out (vty, "Warning...%s", VTY_NEWLINE);
        break;
      case CMD_ERR_AMBIGUOUS:
        uty_out (vty, "%% Ambiguous command.%s", VTY_NEWLINE);
        break;
      case CMD_ERR_NO_MATCH:
        uty_out (vty, "%% [%s] Unknown command: %s%s", protocolname, buf, VTY_NEWLINE);
        break;
      case CMD_ERR_INCOMPLETE:
        uty_out (vty, "%% Command incomplete.%s", VTY_NEWLINE);
        break;
      }
  cmd_free_strvec (vline);

  return ret;
}

/*------------------------------------------------------------------------------
 *
 */
/* queued command has completed */
extern void
vty_queued_result(struct vty *vty, int result, int action)
{
  LOCK

  vty_prompt(vty);

  /* Wake up */
  if (cli_nexus)
    {
      vty_event (VTY_WRITE, vty->vio->fd, vty);
      if (qpthreads_enabled)
        qpt_thread_signal(cli_nexus->thread_id, SIGMQUEUE);
    }

  UNLOCK
}

/*------------------------------------------------------------------------------
 * Execute current command line.
 *
 * For qpthreads: this is called *only* if there is no command outstanding.  So
 * can execute the command, which may be dispatched to another thread, and
 * there will then be a command outstanding.
 */
static int
uty_execute (struct vty *vty)
{
  int ret;

  ret = CMD_SUCCESS;

  switch (vty->node)
    {
    case AUTH_NODE:
    case AUTH_ENABLE_NODE:
      vty_auth (vty, vty->vio->cbuf);
      break;
    default:
      ret = vty_command (vty, vty->vio->cbuf);
      if (vty->vio->type == VTY_TERM)
	vty_hist_add (vty);
      break;
    }

  if (ret == CMD_QUEUED)
    vty->vio->interlock = vty_cQueued ;

  /* Clear command line buffer. */
  vty->vio->cp = vty->vio->length = 0;
  vty_clear_buf (vty);

  if (vty->vio->status != VTY_CLOSE  && ret != CMD_QUEUED)
    vty_prompt (vty);

  return ret;
}

/* Quit print out to the buffer. */
static void
vty_buffer_reset (struct vty *vty)
{
  ASSERTLOCKED
  buffer_reset (vty->vio->obuf);
  vty_redraw (vty);
}

/*============================================================================*/

/*------------------------------------------------------------------------------
 * Callback -- qpthreads: Read data via vty socket.
 *
 */
static void
vty_read_r (qps_file qf, void* file_info)
{
  int vty_sock = qf->fd;
  struct vty *vty = (struct vty *)file_info;

  LOCK

  assert(vty->vio->fd == vty_sock) ;

  /* is this necessary? */
  qps_disable_modes(qf, qps_read_mbit);
  uty_read(vty);

  UNLOCK
}

/*------------------------------------------------------------------------------
 * Callback -- threads: Read data via vty socket.
 *
 */
static int
vty_read (struct thread *thread)
{
  int vty_sock = THREAD_FD (thread);
  struct vty *vty = THREAD_ARG (thread);
  int result ;

  LOCK

  assert(vty->vio->fd == vty_sock) ;

  vty->vio->t_read = NULL;
  result = uty_read(vty);

  UNLOCK
  return result;
}

/*------------------------------------------------------------------------------
 * Callback: Read from the vty and execute command line stuff.
 */

#define CONTROL(X)  ((X) - '@')

enum vty_escape_state
{
  VTY_ESCAPE_0    = 0,
  VTY_ESCAPE_1    = 1,
  VTY_ESCAPE_2    = 2
} ;

static int
uty_read (struct vty *vty)
{
  vty_io vio ;
  int pending ;
//int i ;
  int nbytes ;
  unsigned char* ptr ;
  unsigned char* end ;

  vio = vty->vio ;

  /* Deal with interlock if present -- for qpthreads only               */
  if (vio->interlock & vty_cCompleted)
    {
      /* Is either at end of current input line, or at start of fresh line */
//    vty_wipe_input(vty) ;
      vty_prompt(vty) ;
//    vty_redraw_input(vty) ;

      vty->vio->interlock = 0 ;
    } ;

  /* Read raw data from socket */
  ptr = vio->ibuf ;
  end = ptr + vio->ibuf_has ;

  nbytes = vio->ibuf_size - vio->ibuf_has ;
  if (nbytes > 0)
    nbytes = read(vio->fd, end, nbytes) ;

  if (nbytes < 0)
    {
      if (!ERRNO_IO_RETRY(errno))
        {
          vty->vio->monitor = 0; /* disable monitoring to avoid infinite recursion */
          uzlog(NULL, LOG_WARNING, "%s: read error on vty client fd %d, closing: %s",
		    __func__, vty->vio->fd, safe_strerror(errno));
        }
    }
  else
    {
      end += nbytes ;
      if (ptr == end)           /* buffer empty after read !            */
        {
          /* EOF !                                                      */

          buffer_reset(vio->obuf);
          vio->status = VTY_CLOSE;
        } ;
    } ;


#if 0

      /* Deal with VTY_MORE                                             */
      if (vio->status == VTY_MORE)
        {
          switch (u)
            {
            case CONTROL('C'):
            case 'q':
            case 'Q':
              vty_buffer_reset (vty);
              break;

            default:
              break;
            }
          continue;
        }
#endif


  if (pending)
    {
      qps_disable_modes(vio->qf, qps_read_mbit);
      vio->interlock |= vty_cPending ;
    } ;






  if (pending)
    {
      qps_disable_modes(vio->qf, qps_read_mbit);
      vio->interlock |= vty_cPending ;
    } ;

  /* Check status. */
  if (vio->status == VTY_CLOSE)
    uty_close (vty);
  else
    {
      vty_event (VTY_WRITE, vio->fd, vty);
      vty_event (VTY_READ,  vio->fd, vty);
    }

  return 0;
}

/* Callback: qpthreads. Flush buffer to the vty. */
static void
vty_flush_r (qps_file qf, void* file_info)
{
  int vty_sock = qf->fd;
  struct vty *vty = (struct vty *)file_info;

  LOCK

  qps_disable_modes(qf, qps_write_mbit);

  /* Temporary disable read thread. */
  if ((vty->vio->lines == 0))
    {
      qps_disable_modes(qf, qps_read_mbit);
    }

  uty_flush(vty, vty_sock);

  UNLOCK
}

/* Callback: threads. Flush buffer to the vty. */
static int
vty_flush (struct thread *thread)
{
  int vty_sock = THREAD_FD (thread);
  struct vty *vty = THREAD_ARG (thread);
  int result;

  LOCK
  vty->vio->t_write = NULL;

  /* Temporary disable read thread. */
  if ((vty->vio->lines == 0) && vty->vio->t_read)
    {
      thread_cancel (vty->vio->t_read);
      vty->vio->t_read = NULL;
    }
  result = uty_flush(vty, vty_sock);

  UNLOCK
  return result;
}

static int
uty_flush (struct vty *vty, int vty_sock)
{
  int erase;
  buffer_status_t flushrc;

  /* Function execution continue. */
  erase = ((vty->vio->status == VTY_MORE || vty->vio->status == VTY_MORELINE));

  /* N.B. if width is 0, that means we don't know the window size. */
  if ((vty->vio->lines == 0) || (vty->vio->width == 0))
    flushrc = buffer_flush_available(vty->vio->obuf, vty->vio->fd);
  else if (vty->vio->status == VTY_MORELINE)
    flushrc = buffer_flush_window(vty->vio->obuf, vty->vio->fd, vty->vio->width,
				  1, erase, 0);
  else
    flushrc = buffer_flush_window(vty->vio->obuf, vty->vio->fd, vty->vio->width,
				  vty->vio->lines >= 0 ? vty->vio->lines :
						    vty->vio->height,
				  erase, 0);
  switch (flushrc)
    {
    case BUFFER_ERROR:
      vty->vio->monitor = 0; /* disable monitoring to avoid infinite recursion */
      uzlog(NULL, LOG_WARNING, "buffer_flush failed on vty client fd %d, closing",
		vty->vio->fd);
      buffer_reset(vty->vio->obuf);
      uty_close(vty);
      break;
    case BUFFER_EMPTY:
      if (vty->vio->status == VTY_CLOSE)
	uty_close (vty);
      else
	{
	  vty->vio->status = VTY_NORMAL;
	  if (vty->vio->lines == 0)
	    vty_event (VTY_READ, vty_sock, vty);
	}
      break;
    case BUFFER_PENDING:
      /* There is more data waiting to be written. */
      vty->vio->status = VTY_MORE;
      if (vty->vio->lines == 0)
	vty_event (VTY_WRITE, vty_sock, vty);
      break;
    }

  return 0;
}

/* Create new vty structure. */
static struct vty *
vty_create (int vty_sock, union sockunion *su)
{
  struct vty *vty;

  ASSERTLOCKED

  /* Allocate new vty structure and set up default values. */
  vty = vty_new (vty_sock, VTY_TERM);



  vty->vio->address = sockunion_su2str (su);
  if (no_password_check)
    {
      if (restricted_mode)
        vty->node = RESTRICTED_NODE;
      else if (host.advanced)
	vty->node = ENABLE_NODE;
      else
	vty->node = VIEW_NODE;
    }
  else
    vty->node = AUTH_NODE;
  vty->vio->fail = 0;
  vty->vio->cp = 0;
  vty_clear_buf (vty);
  vty->vio->length = 0;
  memset (vty->vio->hist, 0, sizeof (vty->vio->hist));
  vty->vio->hp = 0;
  vty->vio->hindex = 0;
//vector_set_index (vtyvec, vty_sock, vty);
  vty->vio->status = VTY_NORMAL;
  vty->vio->v_timeout = vty_timeout_val;
  if (host.lines >= 0)
    vty->vio->lines = host.lines;
  else
    vty->vio->lines = -1;
  vty->vio->iac = 0;
  vty->vio->iac_sb_in_progress = 0;
  vty->vio->sb_len = 0;

  if (! no_password_check)
    {
      /* Vty is not available if password isn't set. */
      if (host.password == NULL && host.password_encrypt == NULL)
	{
	  uty_cout (vty, "Vty password is not set.%s", VTY_NEWLINE);
	  vty->vio->status = VTY_CLOSE;
	  uty_close (vty);
	  return NULL;
	}
    }

  /* Say hello to the world. */
  vty_hello (vty);
  if (! no_password_check)
    uty_cout (vty, "%sUser Access Verification%s%s", VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE);

  /* Setting up terminal. */
  vty_will_echo (vty);
  vty_will_suppress_go_ahead (vty);

  vty_dont_linemode (vty);
  vty_do_window_size (vty);
  /* vty_dont_lflow_ahead (vty); */

  vty_prompt (vty);

  /* Add read/write thread. */
  vty_event (VTY_WRITE, vty_sock, vty);
  vty_event (VTY_READ, vty_sock, vty);

  return vty;
}

/* Callback: qpthreads.  Accept connection from the network. */
static void
vty_accept_r (qps_file qf, void* file_info)
{
  LOCK

  int accept_sock = qf->fd;
  uty_accept(accept_sock);

  UNLOCK
}

/* Callback: threads.  Accept connection from the network. */
static int
vty_accept (struct thread *thread)
{
  int result;

  LOCK

  int accept_sock = THREAD_FD (thread);
  result = uty_accept(accept_sock);

  UNLOCK
  return result;
}

static int
uty_accept (int accept_sock)
{
  int vty_sock;
  struct vty *vty;
  union sockunion su;
  int ret;
  unsigned int on;
  struct prefix *p = NULL;
  struct access_list *acl = NULL;
  char *bufp;

  ASSERTLOCKED

  /* We continue hearing vty socket. */
  vty_event (VTY_SERV, accept_sock, NULL);

  memset (&su, 0, sizeof (union sockunion));

  /* We can handle IPv4 or IPv6 socket. */
  vty_sock = sockunion_accept (accept_sock, &su);
  if (vty_sock < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't accept vty socket : %s", safe_strerror (errno));
      return -1;
    }
  set_nonblocking(vty_sock);

  p = sockunion2hostprefix (&su);

  /* VTY's accesslist apply. */
  if (p->family == AF_INET && vty_accesslist_name)
    {
      if ((acl = access_list_lookup (AFI_IP, vty_accesslist_name)) &&
	  (access_list_apply (acl, p) == FILTER_DENY))
	{
	  char *buf;
	  uzlog (NULL, LOG_INFO, "Vty connection refused from %s",
		(buf = sockunion_su2str (&su)));
	  free (buf);
	  close (vty_sock);

	  /* continue accepting connections */
	  vty_event (VTY_SERV, accept_sock, NULL);

	  prefix_free (p);
	  return 0;
	}
    }

#ifdef HAVE_IPV6
  /* VTY's ipv6 accesslist apply. */
  if (p->family == AF_INET6 && vty_ipv6_accesslist_name)
    {
      if ((acl = access_list_lookup (AFI_IP6, vty_ipv6_accesslist_name)) &&
	  (access_list_apply (acl, p) == FILTER_DENY))
	{
	  char *buf;
	  uzlog (NULL, LOG_INFO, "Vty connection refused from %s",
		(buf = sockunion_su2str (&su)));
	  free (buf);
	  close (vty_sock);

	  /* continue accepting connections */
	  vty_event (VTY_SERV, accept_sock, NULL);

	  prefix_free (p);
	  return 0;
	}
    }
#endif /* HAVE_IPV6 */

  prefix_free (p);

  on = 1;
  ret = setsockopt (vty_sock, IPPROTO_TCP, TCP_NODELAY,
		    (char *) &on, sizeof (on));
  if (ret < 0)
    uzlog (NULL, LOG_INFO, "can't set sockopt to vty_sock : %s",
	  safe_strerror (errno));

  uzlog (NULL, LOG_INFO, "Vty connection from %s (fd %d)",
    (bufp = sockunion_su2str (&su)), vty_sock);
  if (bufp)
    XFREE (MTYPE_TMP, bufp);

  vty = vty_create (vty_sock, &su);

  return 0;
}

#if defined(HAVE_IPV6) && !defined(NRL)
static void
vty_serv_sock_addrinfo (const char *hostname, unsigned short port)
{
  int ret;
  struct addrinfo req;
  struct addrinfo *ainfo;
  struct addrinfo *ainfo_save;
  int sock;
  char port_str[BUFSIZ];

  ASSERTLOCKED

  memset (&req, 0, sizeof (struct addrinfo));
  req.ai_flags = AI_PASSIVE;
  req.ai_family = AF_UNSPEC;
  req.ai_socktype = SOCK_STREAM;
  sprintf (port_str, "%d", port);
  port_str[sizeof (port_str) - 1] = '\0';

  ret = getaddrinfo (hostname, port_str, &req, &ainfo);

  if (ret != 0)
    {
      fprintf (stderr, "getaddrinfo failed: %s\n", gai_strerror (ret));
      exit (1);
    }

  ainfo_save = ainfo;

  do
    {
      if (ainfo->ai_family != AF_INET
#ifdef HAVE_IPV6
	  && ainfo->ai_family != AF_INET6
#endif /* HAVE_IPV6 */
	  )
	continue;

      sock = socket (ainfo->ai_family, ainfo->ai_socktype, ainfo->ai_protocol);
      if (sock < 0)
	continue;

      sockopt_reuseaddr (sock);
      sockopt_reuseport (sock);

      /* set non-blocking */
      ret = set_nonblocking(sock);
      if (ret < 0)
        {
          close (sock);      /* Avoid sd leak. */
          continue;
        }

      ret = bind (sock, ainfo->ai_addr, ainfo->ai_addrlen);
      if (ret < 0)
	{
	  close (sock);	/* Avoid sd leak. */
	continue;
	}

      ret = listen (sock, 3);
      if (ret < 0)
	{
	  close (sock);	/* Avoid sd leak. */
	continue;
	}

      vty_event (VTY_SERV, sock, NULL);
    }
  while ((ainfo = ainfo->ai_next) != NULL);

  freeaddrinfo (ainfo_save);
}
#endif /* HAVE_IPV6 && ! NRL */

#if defined(HAVE_IPV6) && defined(NRL) || !defined(HAVE_IPV6)
/* Make vty server socket. */
static void
vty_serv_sock_family (const char* addr, unsigned short port, int family)
{
  int ret;
  union sockunion su;
  int accept_sock;
  void* naddr=NULL;

  ASSERTLOCKED

  memset (&su, 0, sizeof (union sockunion));
  su.sa.sa_family = family;
  if(addr)
    switch(family)
    {
      case AF_INET:
        naddr=&su.sin.sin_addr;
#ifdef HAVE_IPV6
      case AF_INET6:
        naddr=&su.sin6.sin6_addr;
#endif
    }

  if(naddr)
    switch(inet_pton(family,addr,naddr))
    {
      case -1:
        uzlog(NULL, LOG_ERR, "bad address %s",addr);
	naddr=NULL;
	break;
      case 0:
        uzlog(NULL, LOG_ERR, "error translating address %s: %s",addr,safe_strerror(errno));
	naddr=NULL;
    }

  /* Make new socket. */
  accept_sock = sockunion_stream_socket (&su);
  if (accept_sock < 0)
    return;

  /* This is server, so reuse address. */
  sockopt_reuseaddr (accept_sock);
  sockopt_reuseport (accept_sock);

  /* set non-blocking */
  ret = set_nonblocking(accept_sock);
  if (ret < 0)
    {
      close (accept_sock);      /* Avoid sd leak. */
      return;
    }

  /* Bind socket to universal address and given port. */
  ret = sockunion_bind (accept_sock, &su, port, naddr);
  if (ret < 0)
    {
      uzlog(NULL, LOG_WARNING, "can't bind socket");
      close (accept_sock);	/* Avoid sd leak. */
      return;
    }

  /* Listen socket under queue 3. */
  ret = listen (accept_sock, 3);
  if (ret < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't listen socket");
      close (accept_sock);	/* Avoid sd leak. */
      return;
    }

  /* Add vty server event. */
  vty_event (VTY_SERV, accept_sock, NULL);
}
#endif /* defined(HAVE_IPV6) && defined(NRL) || !defined(HAVE_IPV6) */

#ifdef VTYSH
/* For sockaddr_un. */
#include <sys/un.h>

/* VTY shell UNIX domain socket. */
static void
vty_serv_un (const char *path)
{
  int ret;
  int sock, len;
  struct sockaddr_un serv;
  mode_t old_mask;
  struct zprivs_ids_t ids;

  ASSERTLOCKED

  /* First of all, unlink existing socket */
  unlink (path);

  /* Set umask */
  old_mask = umask (0007);

  /* Make UNIX domain socket. */
  sock = socket (AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0)
    {
      uzlog(NULL, LOG_ERR, "Cannot create unix stream socket: %s", safe_strerror(errno));
      return;
    }

  /* Make server socket. */
  memset (&serv, 0, sizeof (struct sockaddr_un));
  serv.sun_family = AF_UNIX;
  strncpy (serv.sun_path, path, strlen (path));
#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
  len = serv.sun_len = SUN_LEN(&serv);
#else
  len = sizeof (serv.sun_family) + strlen (serv.sun_path);
#endif /* HAVE_STRUCT_SOCKADDR_UN_SUN_LEN */

  ret = bind (sock, (struct sockaddr *) &serv, len);
  if (ret < 0)
    {
      uzlog(NULL, LOG_ERR, "Cannot bind path %s: %s", path, safe_strerror(errno));
      close (sock);	/* Avoid sd leak. */
      return;
    }

  ret = listen (sock, 5);
  if (ret < 0)
    {
      uzlog(NULL, LOG_ERR, "listen(fd %d) failed: %s", sock, safe_strerror(errno));
      close (sock);	/* Avoid sd leak. */
      return;
    }

  umask (old_mask);

  zprivs_get_ids(&ids);

  if (ids.gid_vty > 0)
    {
      /* set group of socket */
      if ( chown (path, -1, ids.gid_vty) )
        {
          uzlog (NULL, LOG_ERR, "vty_serv_un: could chown socket, %s",
                     safe_strerror (errno) );
        }
    }

  vty_event (VTYSH_SERV, sock, NULL);
}

/* #define VTYSH_DEBUG 1 */

/* Callback: qpthreads.  Accept connection */
void int
vtysh_accept_r (qps_file qf, void* file_info)
{
  int accept_sock = qf->fd;
  LOCK
  utysh_accept (accept_sock);
  UNLOCK
}

/* Callback: threads.  Accept connection */
static int
vtysh_accept (struct thread *thread)
{
  int accept_sock = THREAD_FD (thread);
  LOCK
  result = utysh_accept (accept_sock);
  UNLOCK
  return result;
}

static int
utysh_accept (int accept_sock)
{
  int sock;
  int client_len;
  struct sockaddr_un client;
  struct vty *vty;

  ASSERTLOCKED

  vty_event (VTYSH_SERV, accept_sock, NULL);

  memset (&client, 0, sizeof (struct sockaddr_un));
  client_len = sizeof (struct sockaddr_un);

  sock = accept (accept_sock, (struct sockaddr *) &client,
		 (socklen_t *) &client_len);

  if (sock < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't accept vty socket : %s", safe_strerror (errno));
      return -1;
    }

  if (set_nonblocking(sock) < 0)
    {
      uzlog (NULL, LOG_WARNING, "vtysh_accept: could not set vty socket %d to non-blocking,"
                 " %s, closing", sock, safe_strerror (errno));
      close (sock);
      return -1;
    }

#ifdef VTYSH_DEBUG
  printf ("VTY shell accept\n");
#endif /* VTYSH_DEBUG */

  vty = vty_new ();
  vty->vio->fd = sock;
  vty->vio->type = VTY_SHELL_SERV;
  vty->node = VIEW_NODE;

  vty_event (VTYSH_READ, sock, vty);

  return 0;
}

static int
vtysh_flush(struct vty *vty)
{
  ASSERTLOCKED

  switch (buffer_flush_available(vty->vio->obuf, vty->vio->fd))
    {
    case BUFFER_PENDING:
      vty_event(VTYSH_WRITE, vty->vio->fd, vty);
      break;
    case BUFFER_ERROR:
      vty->vio->monitor = 0; /* disable monitoring to avoid infinite recursion */
      uzlog(NULL, LOG_WARNING, "%s: write error to fd %d, closing", __func__, vty->vio->fd);
      buffer_reset(vty->vio->obuf);
      uty_close(vty);
      return -1;
      break;
    case BUFFER_EMPTY:
      break;
    }
  return 0;
}

/* Callback: qpthreads., Read data via vty socket. */
static void
vtysh_read_r (qps_file qf, void* file_info)
{
  int vty_sock = qf->fd;
  struct vty *vty = (struct vty *)file_info;

  LOCK

  /* is this necessary? */
  qps_disable_modes(qf, qps_read_mbit);
  utysh_read(vty, vty_soc);

  UNLOCK
}

/* Callback: threads. Read data via vty socket. */
static int
vtysh_read (struct thread *thread)
{
  int vty_sock = THREAD_FD (thread);
  struct vty *vty = THREAD_ARG (thread);
  int result;

  LOCK

  vty->vio->t_read = NULL;
  result = uty_read(vty, vty_soc);

  UNLOCK
  return result;
}

static int
utysh_read (struct vty *vty, int sock)
{
  int ret;
  int nbytes;
  unsigned char buf[VTY_READ_BUFSIZ];
  unsigned char *p;
  u_char header[4] = {0, 0, 0, 0};

  if ((nbytes = read (sock, buf, VTY_READ_BUFSIZ)) <= 0)
    {
      if (nbytes < 0)
	{
	  if (ERRNO_IO_RETRY(errno))
	    {
	      vty_event (VTYSH_READ, sock, vty);
	      return 0;
	    }
	  vty->vio->monitor = 0; /* disable monitoring to avoid infinite recursion */
	  uzlog(NULL, LOG_WARNING, "%s: read failed on vtysh client fd %d, closing: %s",
		    __func__, sock, safe_strerror(errno));
	}
      buffer_reset(vty->vio->obuf);
      uty_close (vty);
#ifdef VTYSH_DEBUG
      printf ("close vtysh\n");
#endif /* VTYSH_DEBUG */
      return 0;
    }

#ifdef VTYSH_DEBUG
  printf ("line: %.*s\n", nbytes, buf);
#endif /* VTYSH_DEBUG */

  for (p = buf; p < buf+nbytes; p++)
    {
      vty_ensure(vty, vty->vio->length+1);
      vty->vio->cbuf[vty->vio->length++] = *p;
      if (*p == '\0')
	{
	  /* Pass this line to parser. */
	  ret = vty_execute (vty);
	  /* Note that vty_execute clears the command buffer and resets
	     vty->vio->length to 0. */

	  /* Return result. */
#ifdef VTYSH_DEBUG
	  printf ("result: %d\n", ret);
	  printf ("vtysh node: %d\n", vty->node);
#endif /* VTYSH_DEBUG */

	  header[3] = ret;
	  buffer_put(vty->vio->obuf, header, 4);

	  if (!vty->vio->t_write && (vtysh_flush(vty) < 0))
	    /* Try to flush results; exit if a write error occurs. */
	    return 0;
	}
    }

  vty_event (VTYSH_READ, sock, vty);

  return 0;
}

/* Callback: qpthraeds.  Write */
static void
vtysh_write_r (qps_file qf, void* file_info)
{
  struct vty *vty = (struct vty *)file_info;

  LOCK

  qps_disable_modes(qf, qps_write_mbit);
  vtysh_flush(vty);

  UNLOCK
}

//* Callback: thraeds.  Write */
static int
vtysh_write (struct thread *thread)
{
  struct vty *vty = THREAD_ARG (thread);

  LOCK

  vty->vio->t_write = NULL;
  vtysh_flush(vty);

  UNLOCK
  return 0;
}

#endif /* VTYSH */

/* Determine address family to bind. */
void
vty_serv_sock (const char *addr, unsigned short port, const char *path)
{
  LOCK

  /* If port is set to 0, do not listen on TCP/IP at all! */
  if (port)
    {

#ifdef HAVE_IPV6
#ifdef NRL
      vty_serv_sock_family (addr, port, AF_INET);
      vty_serv_sock_family (addr, port, AF_INET6);
#else /* ! NRL */
      vty_serv_sock_addrinfo (addr, port);
#endif /* NRL*/
#else /* ! HAVE_IPV6 */
      vty_serv_sock_family (addr,port, AF_INET);
#endif /* HAVE_IPV6 */
    }

#ifdef VTYSH
  vty_serv_un (path);
#endif /* VTYSH */

  UNLOCK
}

/* Close vty interface.  Warning: call this only from functions that
   will be careful not to access the vty afterwards (since it has
   now been freed).  This is safest from top-level functions (called
   directly by the thread dispatcher). */
void
vty_close (struct vty *vty)
{
  LOCK
  uty_close(vty);
  UNLOCK
}

static void
uty_close (struct vty *vty)
{
  int i;

  ASSERTLOCKED

  uzlog (NULL, LOG_INFO, "Vty connection (fd %d) close", vty->vio->fd) ;

  /* Cancel threads.*/
  if (vty->vio->t_read)
    thread_cancel (vty->vio->t_read);
  if (vty->vio->t_write)
    thread_cancel (vty->vio->t_write);
  if (vty->vio->t_timeout)
    thread_cancel (vty->vio->t_timeout);
  if (vty->vio->qf)
    {
      qps_remove_file(vty->vio->qf);
      qps_file_free(vty->vio->qf);
      vty->vio->qf = NULL;
    }
  if (vty->vio->qtr)
    {
    qtimer_free(vty->vio->qtr);
    vty->vio->qtr = NULL;
    }

  /* Flush buffer. */
  buffer_flush_all (vty->vio->obuf, vty->vio->fd);

  /* Free input buffer. */
  buffer_free (vty->vio->obuf);

  /* Free command history. */
  for (i = 0; i < VTY_MAXHIST; i++)
    if (vty->vio->hist[i])
      XFREE (MTYPE_VTY_HIST, vty->vio->hist[i]);

  /* Unset vector. */
//vector_unset (vtyvec, vty->vio->fd);

  /* Close socket. */
  if (vty->vio->fd > 0)
    close (vty->vio->fd);

  if (vty->vio->address)
    XFREE (MTYPE_TMP, vty->vio->address);
  if (vty->vio->cbuf)
    XFREE (MTYPE_VTY, vty->vio->cbuf);

  /* Check configure. */
  uty_config_unlock (vty);

  /* OK free vty. */
//XFREE (MTYPE_VTY, vty);
}

/* Callback: qpthreads.  When time out occur output message then close connection. */
static void
vty_timeout_r (qtimer qtr, void* timer_info, qtime_t when)
{
  struct vty *vty = (struct vty *)timer_info;
  LOCK
  qtimer_unset(qtr);
  uty_timeout(vty);
  UNLOCK
}

/* Callback: threads.  When time out occur output message then close connection. */
static int
vty_timeout (struct thread *thread)
{
  int result;
  struct vty *vty = THREAD_ARG (thread);
  LOCK
  vty->vio->t_timeout = NULL;
  result = uty_timeout(vty);
  UNLOCK
  return result;
}

static int
uty_timeout (struct vty *vty)
{
  vty->vio->v_timeout = 0;

  /* Clear buffer*/
  buffer_reset (vty->vio->obuf);
  uty_cout (vty, "%sVty connection is timed out.%s", VTY_NEWLINE, VTY_NEWLINE);

  /* Close connection. */
  vty->vio->status = VTY_CLOSE;
  uty_close (vty);

  return 0;
}

/* Read up configuration file from file_name. */
static void
vty_read_file (FILE *confp, void (*after_first_cmd)(void))
{
  int ret;
  struct vty *vty;

  vty = vty_new (0, VTY_TERM); /* stdout */
  vty->node = CONFIG_NODE;

  /* Execute configuration file */
  ret = config_from_file (vty, confp, after_first_cmd);

  LOCK

  if ( !((ret == CMD_SUCCESS) || (ret == CMD_ERR_NOTHING_TODO)) )
    {
      switch (ret)
       {
         case CMD_ERR_AMBIGUOUS:
           fprintf (stderr, "Ambiguous command.\n");
           break;
         case CMD_ERR_NO_MATCH:
           fprintf (stderr, "There is no such command.\n");
           break;
       }
      fprintf (stderr, "Error occurred while processing:\n%s\n", vty->vio->cbuf);
      uty_close (vty);
      exit (1);
    }

  uty_close (vty);
  UNLOCK
}

static FILE *
vty_use_backup_config (char *fullpath)
{
  char *fullpath_sav, *fullpath_tmp;
  FILE *ret = NULL;
  struct stat buf;
  int tmp, sav;
  int c;
  char buffer[512];

  fullpath_sav = malloc (strlen (fullpath) + strlen (CONF_BACKUP_EXT) + 1);
  strcpy (fullpath_sav, fullpath);
  strcat (fullpath_sav, CONF_BACKUP_EXT);
  if (stat (fullpath_sav, &buf) == -1)
    {
      free (fullpath_sav);
      return NULL;
    }

  fullpath_tmp = malloc (strlen (fullpath) + 8);
  sprintf (fullpath_tmp, "%s.XXXXXX", fullpath);

  /* Open file to configuration write. */
  tmp = mkstemp (fullpath_tmp);
  if (tmp < 0)
    {
      free (fullpath_sav);
      free (fullpath_tmp);
      return NULL;
    }

  sav = open (fullpath_sav, O_RDONLY);
  if (sav < 0)
    {
      unlink (fullpath_tmp);
      free (fullpath_sav);
      free (fullpath_tmp);
      return NULL;
    }

  while((c = read (sav, buffer, 512)) > 0)
    write (tmp, buffer, c);

  close (sav);
  close (tmp);

  if (chmod(fullpath_tmp, CONFIGFILE_MASK) != 0)
    {
      unlink (fullpath_tmp);
      free (fullpath_sav);
      free (fullpath_tmp);
      return NULL;
    }

  if (link (fullpath_tmp, fullpath) == 0)
    ret = fopen (fullpath, "r");

  unlink (fullpath_tmp);

  free (fullpath_sav);
  free (fullpath_tmp);
  return ret;
}

/* Read up configuration file from file_name. */
void
vty_read_config (char *config_file,
                 char *config_default_dir)
{
  vty_read_config_first_cmd_special(config_file, config_default_dir, NULL);
}

/* Read up configuration file from file_name.
 * callback after first command */
void
vty_read_config_first_cmd_special(char *config_file,
                 char *config_default_dir, void (*after_first_cmd)(void))
{
  char cwd[MAXPATHLEN];
  FILE *confp = NULL;
  char *fullpath;
  char *tmp = NULL;

  /* If -f flag specified. */
  if (config_file != NULL)
    {
      if (! IS_DIRECTORY_SEP (config_file[0]))
        {
          getcwd (cwd, MAXPATHLEN);
          tmp = XMALLOC (MTYPE_TMP,
 			      strlen (cwd) + strlen (config_file) + 2);
          sprintf (tmp, "%s/%s", cwd, config_file);
          fullpath = tmp;
        }
      else
        fullpath = config_file;

      confp = fopen (fullpath, "r");

      if (confp == NULL)
        {
          fprintf (stderr, "%s: failed to open configuration file %s: %s\n",
                   __func__, fullpath, safe_strerror (errno));

          confp = vty_use_backup_config (fullpath);
          if (confp)
            fprintf (stderr, "WARNING: using backup configuration file!\n");
          else
            {
              fprintf (stderr, "can't open configuration file [%s]\n",
  	               config_file);
              exit(1);
            }
        }
    }
  else
    {
#ifdef VTYSH
      int ret;
      struct stat conf_stat;

      /* !!!!PLEASE LEAVE!!!!
       * This is NEEDED for use with vtysh -b, or else you can get
       * a real configuration food fight with a lot garbage in the
       * merged configuration file it creates coming from the per
       * daemon configuration files.  This also allows the daemons
       * to start if there default configuration file is not
       * present or ignore them, as needed when using vtysh -b to
       * configure the daemons at boot - MAG
       */

      /* Stat for vtysh Zebra.conf, if found startup and wait for
       * boot configuration
       */

      if ( strstr(config_default_dir, "vtysh") == NULL)
        {
          ret = stat (integrate_default, &conf_stat);
          if (ret >= 0)
              return;
        }
#endif /* VTYSH */

      confp = fopen (config_default_dir, "r");
      if (confp == NULL)
        {
          fprintf (stderr, "%s: failed to open configuration file %s: %s\n",
                   __func__, config_default_dir, safe_strerror (errno));

          confp = vty_use_backup_config (config_default_dir);
          if (confp)
            {
              fprintf (stderr, "WARNING: using backup configuration file!\n");
              fullpath = config_default_dir;
            }
          else
            {
              fprintf (stderr, "can't open configuration file [%s]\n",
  		                 config_default_dir);
  	          exit (1);
            }
        }
      else
        fullpath = config_default_dir;
    }

  vty_read_file (confp, after_first_cmd);

  fclose (confp);

#ifdef QDEBUG
  fprintf(stderr, "Reading config file: %s\n", fullpath);
#endif
  host_config_set (fullpath);
#ifdef QDEBUG
  fprintf(stderr, "Finished reading config file\n");
#endif

  if (tmp)
    XFREE (MTYPE_TMP, fullpath);
}

#ifdef QDEBUG
/* Tell all terminals that we are shutting down */
void
vty_goodbye (void)
{
  unsigned int i;
  struct vty *vty;

  LOCK

  if (vtyvec)
    {
      for (i = 0; i < vector_active (vtyvec); i++)
        {
          if (((vty = vector_slot (vtyvec, i)) != NULL) && vty->vio->type == VTY_TERM)
            {
              uty_cout(vty, QUAGGA_PROGNAME " is shutting down%s", VTY_NEWLINE);

              /* Wake up */
              if (cli_nexus)
                vty_event (VTY_WRITE, vty->vio->fd, vty);
            }
        }
      if (qpthreads_enabled)
        qpt_thread_signal(cli_nexus->thread_id, SIGMQUEUE);
    }

    UNLOCK
}
#endif

/* Async-signal-safe version of vty_log for fixed strings. */
void
vty_log_fixed (const char *buf, size_t len)
{
//  unsigned int i;
  struct iovec iov[2];

  /* vty may not have been initialised */
//  if (!vtyvec)
//    return;

  iov[0].iov_base = miyagi(buf) ;
  iov[0].iov_len  = len;
  iov[1].iov_base = miyagi("\r\n") ;
  iov[1].iov_len  = 2;

//  for (i = 0; i < vector_active (vtyvec); i++)
    {
      struct vty *vty;
//      if (((vty = vector_slot (vtyvec, i)) != NULL) && vty->vio->monitor)
	/* N.B. We don't care about the return code, since process is
	   most likely just about to die anyway. */
	writev(vty->vio->fd, iov, 2);
    }
}

int
vty_config_lock (struct vty *vty)
{
  int result;
  LOCK
  if (vty_config == 0)
    {
      vty->vio->config = 1;
      vty_config = 1;
    }
  result = vty->vio->config;
  UNLOCK
  return result;
}

int
vty_config_unlock (struct vty *vty)
{
  int result;
  LOCK
  result = uty_config_unlock(vty);
  UNLOCK
  return result;
}

static int
uty_config_unlock (struct vty *vty)
{
  ASSERTLOCKED
  if (vty_config == 1 && vty->vio->config == 1)
    {
      vty->vio->config = 0;
      vty_config = 0;
    }
  return vty->vio->config;
}

static void
vty_event (enum vty_event event, int sock, struct vty *vty)
{
  if (cli_nexus)
    vty_event_r(event, sock, vty);
  else
    vty_event_t(event, sock, vty);
}

/* thread event setter */
static void
vty_event_t (enum vty_event event, int sock, struct vty *vty)
  {
  struct thread *vty_serv_thread;

  ASSERTLOCKED

  switch (event)
    {
    case VTY_SERV:
      vty_serv_thread = thread_add_read (master, vty_accept, vty, sock);
//    vector_set_index (Vvty_serv_thread, sock, vty_serv_thread);
      break;
#ifdef VTYSH
    case VTYSH_SERV:
      thread_add_read (master, vtysh_accept, vty, sock);
      break;
    case VTYSH_READ:
      vty->vio->t_read = thread_add_read (master, vtysh_read, vty, sock);
      break;
    case VTYSH_WRITE:
      vty->vio->t_write = thread_add_write (master, vtysh_write, vty, sock);
      break;
#endif /* VTYSH */
    case VTY_READ:
      vty->vio->t_read = thread_add_read (master, vty_read, vty, sock);

      /* Time out treatment. */
      if (vty->vio->v_timeout)
	{
	  if (vty->vio->t_timeout)
	    thread_cancel (vty->vio->t_timeout);
	  vty->vio->t_timeout =
	    thread_add_timer (master, vty_timeout, vty, vty->vio->v_timeout);
	}
      break;
    case VTY_WRITE:
      if (! vty->vio->t_write)
	vty->vio->t_write = thread_add_write (master, vty_flush, vty, sock);
        break;
    case VTY_TIMEOUT_RESET:
      if (vty->vio->t_timeout)
	{
	  thread_cancel (vty->vio->t_timeout);
	  vty->vio->t_timeout = NULL;
	}
      if (vty->vio->v_timeout)
	{
	  vty->vio->t_timeout =
	    thread_add_timer (master, vty_timeout, vty, vty->vio->v_timeout);
	}
      break;
    }
}

/* qpthreads event setter */
static void
vty_event_r (enum vty_event event, int sock, struct vty *vty)
  {

  qps_file accept_file = NULL;

  ASSERTLOCKED

  switch (event)
    {
    case VTY_SERV:
      accept_file = NULL ;// vector_get_item(Vvty_serv_thread, sock);
      if (accept_file == NULL)
        {
          accept_file = qps_file_init_new(accept_file, NULL);
          qps_add_file(cli_nexus->selection, accept_file, sock, NULL);
//          vector_set_index(Vvty_serv_thread, sock, accept_file);
        }
      qps_enable_mode(accept_file, qps_read_mnum, vty_accept_r) ;
      break;
#ifdef VTYSH
    case VTYSH_SERV:
      accept_file = vector_get_item(Vvty_serv_thread, sock);
      if (accept_file == NULL)
        {
          accept_file = qps_file_init_new(accept_file, NULL);
          qps_add_file(master, accept_file, sock, NULL);
          vector_set_index(Vvty_serv_thread, sock, accept_file);
        }
      qps_enable_mode(accept_file, qps_read_mnum, vtysh_accept_r) ;
      break;
    case VTYSH_READ:
      qps_enable_mode(vty->vio->file, qps_read_mnum, vtysh_read_r) ;
      break;
    case VTYSH_WRITE:
      qps_enable_mode(vty->vio->file, qps_write_mnum, vtysh_write_r) ;
      break;
#endif /* VTYSH */
    case VTY_READ:
      qps_enable_mode(vty->vio->qf, qps_read_mnum, vty_read_r) ;

      /* Time out treatment. */
      if (vty->vio->v_timeout)
        {
          qtimer_set(vty->vio->qtr, qt_add_monotonic(QTIME(vty->vio->v_timeout)), NULL) ;
        }
      break;
    case VTY_WRITE:
      qps_enable_mode(vty->vio->qf, qps_write_mnum, vty_flush_r) ;
      break;
    case VTY_TIMEOUT_RESET:
      if (vty->vio->qtr == NULL)
        break;
      if (vty->vio->v_timeout)
        {
          qtimer_set(vty->vio->qtr, qt_add_monotonic(QTIME(vty->vio->v_timeout)), NULL) ;
        }
      else
        {
          qtimer_unset(vty->vio->qtr);
        }
      break;
    }
}

/*==============================================================================
 * Commands
 *
 */
DEFUN_CALL (config_who,
       config_who_cmd,
       "who",
       "Display who is on vty\n")
{
  unsigned int i;
  struct vty *v;

  LOCK
//  for (i = 0; i < vector_active (vtyvec); i++)
//    if ((v = vector_slot (vtyvec, i)) != NULL)
      uty_out (vty, "%svty[%d] connected from %s.%s",
	       v->vio->config ? "*" : " ",
	       i, v->vio->address, VTY_NEWLINE);
  UNLOCK
  return CMD_SUCCESS;
}

/* Move to vty configuration mode. */
DEFUN_CALL (line_vty,
       line_vty_cmd,
       "line vty",
       "Configure a terminal line\n"
       "Virtual terminal\n")
{
  LOCK
  vty->node = VTY_NODE;
  UNLOCK
  return CMD_SUCCESS;
}

/* Set time out value. */
static int
exec_timeout (struct vty *vty, const char *min_str, const char *sec_str)
{
  unsigned long timeout = 0;

  LOCK

  /* min_str and sec_str are already checked by parser.  So it must be
     all digit string. */
  if (min_str)
    {
      timeout = strtol (min_str, NULL, 10);
      timeout *= 60;
    }
  if (sec_str)
    timeout += strtol (sec_str, NULL, 10);

  vty_timeout_val = timeout;
  vty->vio->v_timeout = timeout;
  vty_event (VTY_TIMEOUT_RESET, 0, vty);

  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (exec_timeout_min,
       exec_timeout_min_cmd,
       "exec-timeout <0-35791>",
       "Set timeout value\n"
       "Timeout value in minutes\n")
{
  return exec_timeout (vty, argv[0], NULL);
}

DEFUN_CALL (exec_timeout_sec,
       exec_timeout_sec_cmd,
       "exec-timeout <0-35791> <0-2147483>",
       "Set the EXEC timeout\n"
       "Timeout in minutes\n"
       "Timeout in seconds\n")
{
  return exec_timeout (vty, argv[0], argv[1]);
}

DEFUN_CALL (no_exec_timeout,
       no_exec_timeout_cmd,
       "no exec-timeout",
       NO_STR
       "Set the EXEC timeout\n")
{
  return exec_timeout (vty, NULL, NULL);
}

/* Set vty access class. */
DEFUN_CALL (vty_access_class,
       vty_access_class_cmd,
       "access-class WORD",
       "Filter connections based on an IP access list\n"
       "IP access list\n")
{
  LOCK

  if (vty_accesslist_name)
    XFREE(MTYPE_VTY, vty_accesslist_name);

  vty_accesslist_name = XSTRDUP(MTYPE_VTY, argv[0]);

  UNLOCK
  return CMD_SUCCESS;
}

/* Clear vty access class. */
DEFUN_CALL (no_vty_access_class,
       no_vty_access_class_cmd,
       "no access-class [WORD]",
       NO_STR
       "Filter connections based on an IP access list\n"
       "IP access list\n")
{
  int result = CMD_SUCCESS;

  LOCK
  if (! vty_accesslist_name || (argc && strcmp(vty_accesslist_name, argv[0])))
    {
      uty_out (vty, "Access-class is not currently applied to vty%s",
	       VTY_NEWLINE);
      result = CMD_WARNING;
    }
  else
    {
      XFREE(MTYPE_VTY, vty_accesslist_name);
      vty_accesslist_name = NULL;
    }

  UNLOCK
  return result;
}

#ifdef HAVE_IPV6
/* Set vty access class. */
DEFUN_CALL (vty_ipv6_access_class,
       vty_ipv6_access_class_cmd,
       "ipv6 access-class WORD",
       IPV6_STR
       "Filter connections based on an IP access list\n"
       "IPv6 access list\n")
{
  LOCK
  if (vty_ipv6_accesslist_name)
    XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);

  vty_ipv6_accesslist_name = XSTRDUP(MTYPE_VTY, argv[0]);

  UNLOCK
  return CMD_SUCCESS;
}

/* Clear vty access class. */
DEFUN_CALL (no_vty_ipv6_access_class,
       no_vty_ipv6_access_class_cmd,
       "no ipv6 access-class [WORD]",
       NO_STR
       IPV6_STR
       "Filter connections based on an IP access list\n"
       "IPv6 access list\n")
{
  int result = CMD_SUCCESS;

  LOCK

  if (! vty_ipv6_accesslist_name ||
      (argc && strcmp(vty_ipv6_accesslist_name, argv[0])))
    {
      uty_out (vty, "IPv6 access-class is not currently applied to vty%s",
	       VTY_NEWLINE);
      result = CMD_WARNING;
    }
  else
    {
      XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);

      vty_ipv6_accesslist_name = NULL;
    }

  UNLOCK
  return CMD_SUCCESS;
}
#endif /* HAVE_IPV6 */

/* vty login. */
DEFUN_CALL (vty_login,
       vty_login_cmd,
       "login",
       "Enable password checking\n")
{
  LOCK
  no_password_check = 0;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (no_vty_login,
       no_vty_login_cmd,
       "no login",
       NO_STR
       "Enable password checking\n")
{
  LOCK
  no_password_check = 1;
  UNLOCK
  return CMD_SUCCESS;
}

/* initial mode. */
DEFUN_CALL (vty_restricted_mode,
       vty_restricted_mode_cmd,
       "anonymous restricted",
       "Restrict view commands available in anonymous, unauthenticated vty\n")
{
  LOCK
  restricted_mode = 1;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (vty_no_restricted_mode,
       vty_no_restricted_mode_cmd,
       "no anonymous restricted",
       NO_STR
       "Enable password checking\n")
{
  LOCK
  restricted_mode = 0;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (service_advanced_vty,
       service_advanced_vty_cmd,
       "service advanced-vty",
       "Set up miscellaneous service\n"
       "Enable advanced mode vty interface\n")
{
  LOCK
  host.advanced = 1;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_advanced_vty,
       no_service_advanced_vty_cmd,
       "no service advanced-vty",
       NO_STR
       "Set up miscellaneous service\n"
       "Enable advanced mode vty interface\n")
{
  LOCK
  host.advanced = 0;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (terminal_monitor,
       terminal_monitor_cmd,
       "terminal monitor",
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")
{
  LOCK
  vty->vio->monitor = 1;
  UNLOCK
  return CMD_SUCCESS;
}

DEFUN_CALL (terminal_no_monitor,
       terminal_no_monitor_cmd,
       "terminal no monitor",
       "Set terminal line parameters\n"
       NO_STR
       "Copy debug output to the current terminal line\n")
{
  LOCK
  vty->vio->monitor = 0;
  UNLOCK
  return CMD_SUCCESS;
}

ALIAS_CALL (terminal_no_monitor,
       no_terminal_monitor_cmd,
       "no terminal monitor",
       NO_STR
       "Set terminal line parameters\n"
       "Copy debug output to the current terminal line\n")

DEFUN_CALL (show_history,
       show_history_cmd,
       "show history",
       SHOW_STR
       "Display the session command history\n")
{
  int index;

  LOCK

  for (index = vty->vio->hindex + 1; index != vty->vio->hindex;)
    {
      if (index == VTY_MAXHIST)
	{
	  index = 0;
	  continue;
	}

      if (vty->vio->hist[index] != NULL)
	uty_out (vty, "  %s%s", vty->vio->hist[index], VTY_NEWLINE);

      index++;
    }

  UNLOCK
  return CMD_SUCCESS;
}

/* Display current configuration. */
static int
vty_config_write (struct vty *vty)
{
  vty_out (vty, "line vty%s", VTY_NEWLINE);

  if (vty_accesslist_name)
    vty_out (vty, " access-class %s%s",
	     vty_accesslist_name, VTY_NEWLINE);

  if (vty_ipv6_accesslist_name)
    vty_out (vty, " ipv6 access-class %s%s",
	     vty_ipv6_accesslist_name, VTY_NEWLINE);

  /* exec-timeout */
  if (vty_timeout_val != VTY_TIMEOUT_DEFAULT)
    vty_out (vty, " exec-timeout %ld %ld%s",
	     vty_timeout_val / 60,
	     vty_timeout_val % 60, VTY_NEWLINE);

  /* login */
  if (no_password_check)
    vty_out (vty, " no login%s", VTY_NEWLINE);

  if (restricted_mode != restricted_mode_default)
    {
      if (restricted_mode_default)
        vty_out (vty, " no anonymous restricted%s", VTY_NEWLINE);
      else
        vty_out (vty, " anonymous restricted%s", VTY_NEWLINE);
    }

  vty_out (vty, "!%s", VTY_NEWLINE);

  return CMD_SUCCESS;
}

struct cmd_node vty_node =
{
  VTY_NODE,
  "%s(config-line)# ",
  1,
};

/*==============================================================================
 *
 */

/*------------------------------------------------------------------------------
 * Reset all VTY status
 *
 */
void
vty_reset ()
{
  unsigned int i;
  struct vty *vty;
  struct thread *vty_serv_thread;
  qps_file qf;

  LOCK

//  for (i = 0; i < vector_active (vtyvec); i++)
//    if ((vty = vector_slot (vtyvec, i)) != NULL)
      {
	buffer_reset (vty->vio->obuf);
	vty->vio->status = VTY_CLOSE;
	uty_close (vty);
      }

  if (cli_nexus)
    {
//      for (i = 0; i < vector_active (Vvty_serv_thread); i++)
//        if ((qf = vector_slot (Vvty_serv_thread, i)) != NULL)
          {
          qps_remove_file(qf);
          qps_file_free(qf);
//          vector_slot (Vvty_serv_thread, i) = NULL;
          close (i);
          }
    }
  else
    {
      assert(master);
//      for (i = 0; i < vector_active (Vvty_serv_thread); i++)
//        if ((vty_serv_thread = vector_slot (Vvty_serv_thread, i)) != NULL)
          {
            thread_cancel (vty_serv_thread);
 //           vector_slot (Vvty_serv_thread, i) = NULL;
            close (i);
          }
    }

  vty_timeout_val = VTY_TIMEOUT_DEFAULT;

  if (vty_accesslist_name)
    {
      XFREE(MTYPE_VTY, vty_accesslist_name);
      vty_accesslist_name = NULL;
    }

  if (vty_ipv6_accesslist_name)
    {
      XFREE(MTYPE_VTY, vty_ipv6_accesslist_name);
      vty_ipv6_accesslist_name = NULL;
    }

  UNLOCK
}

/*------------------------------------------------------------------------------
 * Save cwd
 *
 * This is done early in the morning so that any future operations on files
 * (in particular the configuration file) can use the original cwd.
 */
static void
vty_save_cwd (void)
{
  char cwd[MAXPATHLEN];
  char *c;

  c = getcwd (cwd, MAXPATHLEN);

  if (!c)
    {
      chdir (SYSCONFDIR);
      getcwd (cwd, MAXPATHLEN);
    }

  vty_cwd = XMALLOC (MTYPE_TMP, strlen (cwd) + 1);
  strcpy (vty_cwd, cwd);
}

/*------------------------------------------------------------------------------
 *
 */
char *
vty_get_cwd ()
{
  return vty_cwd;
}

int
vty_shell (struct vty *vty)
{
  LOCK
  int result;
  result = (vty->vio->type == VTY_SHELL) ? 1 : 0 ;
  UNLOCK
  return result;
}

int
vty_shell_serv (struct vty *vty)
{
  LOCK
  int result;
  result = ((vty->vio->type == VTY_SHELL_SERV) ? 1 : 0);
  UNLOCK
  return result;
}

void
vty_init_vtysh ()
{
  LOCK
//  vtyvec = vector_init (0);
  UNLOCK
}

int
vty_get_node(struct vty *vty)
{
  int result;
  LOCK
  result = vty->node;
  UNLOCK
  return result;
}

void
vty_set_node(struct vty *vty, int node)
{
  LOCK
  vty->node = node;
  UNLOCK
}

int
vty_get_type(struct vty *vty)
{
  int result;
  LOCK
  result = vty->vio->type;
  UNLOCK
  return result;
}

int
vty_get_status(struct vty *vty)
{
  int result;
  LOCK
  result = vty->vio->status;
  UNLOCK
  return result;
}

void
vty_set_status(struct vty *vty, int status)
{
  LOCK
  vty->vio->status = status;
  UNLOCK
}

int
vty_get_lines(struct vty *vty)
{
  int result;
  LOCK
  result = vty->vio->lines;
  UNLOCK
  return result;
}

void
vty_set_lines(struct vty *vty, int lines)
{
  LOCK
  vty->vio->lines = lines;
  UNLOCK
}

/*==============================================================================
 * System initialisation and shut down
 */

/*------------------------------------------------------------------------------
 * Initialise vty handling (threads and pthreads)
 *
 * Install vty's own commands like `who' command.
 */
void
vty_init (struct thread_master *master_thread)
{
  LOCK

  /* Local pointer to the master thread                         */
  master = master_thread;

  /* For further configuration read, preserve current directory */
  vty_save_cwd ();

  /* No vty yet                                                 */
  vty_known     = NULL ;
  vty_monitors  = NULL ;

  /* Initilize server thread vector. */
//  Vvty_serv_thread = vector_init (0);

  /* Install bgp top node. */
  install_node (&vty_node, vty_config_write);

  install_element (RESTRICTED_NODE, &config_who_cmd);
  install_element (RESTRICTED_NODE, &show_history_cmd);
  install_element (VIEW_NODE, &config_who_cmd);
  install_element (VIEW_NODE, &show_history_cmd);
  install_element (ENABLE_NODE, &config_who_cmd);
  install_element (CONFIG_NODE, &line_vty_cmd);
  install_element (CONFIG_NODE, &service_advanced_vty_cmd);
  install_element (CONFIG_NODE, &no_service_advanced_vty_cmd);
  install_element (CONFIG_NODE, &show_history_cmd);
  install_element (ENABLE_NODE, &terminal_monitor_cmd);
  install_element (ENABLE_NODE, &terminal_no_monitor_cmd);
  install_element (ENABLE_NODE, &no_terminal_monitor_cmd);
  install_element (ENABLE_NODE, &show_history_cmd);

  install_default (VTY_NODE);
  install_element (VTY_NODE, &exec_timeout_min_cmd);
  install_element (VTY_NODE, &exec_timeout_sec_cmd);
  install_element (VTY_NODE, &no_exec_timeout_cmd);
  install_element (VTY_NODE, &vty_access_class_cmd);
  install_element (VTY_NODE, &no_vty_access_class_cmd);
  install_element (VTY_NODE, &vty_login_cmd);
  install_element (VTY_NODE, &no_vty_login_cmd);
  install_element (VTY_NODE, &vty_restricted_mode_cmd);
  install_element (VTY_NODE, &vty_no_restricted_mode_cmd);
#ifdef HAVE_IPV6
  install_element (VTY_NODE, &vty_ipv6_access_class_cmd);
  install_element (VTY_NODE, &no_vty_ipv6_access_class_cmd);
#endif /* HAVE_IPV6 */

  UNLOCK
}

/*------------------------------------------------------------------------------
 * Further initialisation for qpthreads.
 *
 * This is done during "second stage" initialisation, when all nexuses have
 * been set up and the qpthread_enabled state established.
 *
 * Need to know where the CLI nexus and the Routeing Engine nexus are.
 *
 * Initialise mutex.
 */
void
vty_init_r (qpn_nexus cli_n, qpn_nexus routing_n)
{
  cli_nexus = cli_n;
  routing_nexus = routing_n;
  qpt_mutex_init(&vty_mutex, qpt_mutex_recursive);
} ;

/*------------------------------------------------------------------------------
 * System shut-down
 *
 * Reset all known vty and release all memory.
 */
void
vty_terminate (void)
{
  LOCK

  if (vty_cwd)
    XFREE (MTYPE_TMP, vty_cwd);

 // if (vtyvec && Vvty_serv_thread)
    {
      uty_reset ();
//      vector_free (vtyvec);
//      vector_free (Vvty_serv_thread);
    }
  UNLOCK

  qpt_mutex_destroy(&vty_mutex, 0);
}

#undef LOCK
#undef UNLOCK
#undef ASSERTLOCKED

/* VTY top level
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

#ifndef _ZEBRA_VTY_H
#define _ZEBRA_VTY_H

#include <stdbool.h>

#include "thread.h"
#include "log.h"
#include "qpthreads.h"
#include "qpselect.h"
#include "qtimers.h"
#include "qpnexus.h"
#include "list_util.h"
#include "vector.h"
#include "qstring.h"
#include "node_type.h"

/* Macro in case there are particular compiler issues.    */
#ifndef Inline
  #define Inline static inline
#endif

/*==============================================================================
 * The VTYSH uses a unix socket to talk to the daemon.
 *
 * The ability to respond to a connection from VTYSH appears to be a *compile*
 * time option.  In the interests of keeping the code up to date, the VTYSH
 * option is turned into a testable constant.
 */
#ifdef VTYSH
# define VTYSH_DEFINED 1
#else
# define VTYSH_DEFINED 0
#endif

enum { VTYSH_ENABLED = VTYSH_DEFINED } ;

#undef VTYSH_DEFINED

/*==============================================================================
 * VTY Types
 */
enum vty_type
{
  VTY_NONE  = 0,        /* no type at all                               */

  VTY_TERM,             /* a telnet terminal    -- input and output     */
  VTY_SHELL_SERV,       /* a vty_shell slave    -- input and output     */

  VTY_CONFIG_READ,      /* reading config file  -- output is to buffer
                                                -- no input             */

  VTY_CONFIG_WRITE,     /* writing config file  -- output is to file
                                                -- no input             */

  VTY_STDOUT,           /* general output       -- output is to stdout
                                                -- no input             */

  VTY_STDERR,           /* general output       -- output is to stderr
                                                -- no input             */

  VTY_SHELL,            /* vty in vtysh         -- output is to stdout  */
} ;

/*==============================================================================
 * VTY struct.
 */

typedef struct vty_io* vty_io ; /* private to vty.c     */

struct cmd_parsed ;             /* in case vty.h expanded before command.h */

struct vty
{
  /*----------------------------------------------------------------------
   * The following are the context in which commands are executed.
   */

  /* Node status of this vty
   *
   * NB: when using qpthreads should lock VTY to access this -- so use
   *     vty_get_node() and vty_set_node().
   */
  enum node_type node ;

  /* For current referencing point of interface, route-map, access-list
   * etc...
   *
   * NB: this value is private to the command execution, which is assumed
   *     to all be in the one thread... so no lock required.
   */
  void  *index ;

  /* For multiple level index treatment such as key chain and key.
   *
   * NB: this value is private to the command execution, which is assumed
   *     to all be in the one thread... so no lock required.
   */
  void  *index_sub ;

  /* String which is newline...  read only -- no locking                */
  const char* newline ;

  /*----------------------------------------------------------------------------
   * The current command line.
   *
   * These are set when a command is parsed and dispatched.
   *
   * They are not touched until the command completes -- so may be read while
   * the command is being parsed and executed.
   */
  const char*           buf ;
  struct cmd_parsed*    parsed ;
  unsigned              lineno ;

  /*----------------------------------------------------------------------
   * The following are used inside vty.c only.
   */

  /* Pointer to related vty_io structure                                */
  vty_io   vio ;
};

/*------------------------------------------------------------------------------
 * Can now include this
 */

#include "command.h"

/*------------------------------------------------------------------------------
 *
 */
#define VTY_BUFSIZ 512
#define VTY_MAXHIST 51

/* Integrated configuration file. */
#define INTEGRATE_DEFAULT_CONFIG "Quagga.conf"

/* Small macro to determine newline is newline only or linefeed needed. */
#define VTY_NEWLINE (((vty) != NULL) ? (vty)->newline : "\n")

/* For indenting, mostly.	*/
extern const char vty_spaces_string[] ;
enum { VTY_MAX_SPACES = 40 } ;
#define VTY_SPACES(n) (vty_spaces_string + ((n) < VTY_MAX_SPACES \
					      ? VTY_MAX_SPACES - (n) : 0))

/* Default time out value */
#define VTY_TIMEOUT_DEFAULT 600

/* Vty read buffer size. */
#define VTY_READ_BUFSIZ 512

/* Directory separator. */
#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif /* DIRECTORY_SEP */

#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(c) ((c) == DIRECTORY_SEP)
#endif

/* GCC have printf type attribute check.  */
#ifdef __GNUC__
#define PRINTF_ATTRIBUTE(a,b) __attribute__ ((__format__ (__printf__, a, b)))
#else
#define PRINTF_ATTRIBUTE(a,b)
#endif /* __GNUC__ */

/* Utility macros to convert VTY argument to unsigned long or integer. */
#define VTY_GET_LONG(NAME,V,STR) \
do { \
  char *endptr = NULL; \
  (V) = strtoul ((STR), &endptr, 10); \
  if (*endptr != '\0' || (V) == ULONG_MAX) \
    { \
      vty_out (vty, "%% Invalid %s value%s", NAME, VTY_NEWLINE); \
      return CMD_WARNING; \
    } \
} while (0)

#define VTY_GET_INTEGER_RANGE(NAME,V,STR,MIN,MAX) \
do { \
  unsigned long tmpl; \
  VTY_GET_LONG(NAME, tmpl, STR); \
  if ( (tmpl < (MIN)) || (tmpl > (MAX))) \
    { \
      vty_out (vty, "%% Invalid %s value%s", NAME, VTY_NEWLINE); \
      return CMD_WARNING; \
    } \
  (V) = tmpl; \
} while (0)

#define VTY_GET_INTEGER(NAME,V,STR) \
  VTY_GET_INTEGER_RANGE(NAME,V,STR,0U,UINT32_MAX)

#define VTY_GET_IPV4_ADDRESS(NAME,V,STR)                                      \
do {                                                                             \
  int retv;                                                                   \
  retv = inet_aton ((STR), &(V));                                             \
  if (!retv)                                                                  \
    {                                                                         \
      vty_out (vty, "%% Invalid %s value%s", NAME, VTY_NEWLINE);              \
      return CMD_WARNING;                                                     \
    }                                                                         \
} while (0)

#define VTY_GET_IPV4_PREFIX(NAME,V,STR)                                       \
do {                                                                             \
  int retv;                                                                   \
  retv = str2prefix_ipv4 ((STR), &(V));                                       \
  if (retv <= 0)                                                              \
    {                                                                         \
      vty_out (vty, "%% Invalid %s value%s", NAME, VTY_NEWLINE);              \
      return CMD_WARNING;                                                     \
    }                                                                         \
} while (0)

/*------------------------------------------------------------------------------
 * Exported variables
 */
extern char integrate_default[];

/*------------------------------------------------------------------------------
 * Prototypes.
 */
extern void vty_init (struct thread_master *);
extern void vty_init_r (qpn_nexus, qpn_nexus);
extern void vty_start(const char *addr, unsigned short port, const char *path) ;
#define vty_serv_sock(addr, port, path) vty_start(addr, port, path)
extern void vty_restart(const char *addr, unsigned short port,
                                                             const char *path) ;
extern struct vty* vty_open(enum vty_type type) ;
extern void vty_close(struct vty *);

extern void vty_init_vtysh (void);
extern void vty_terminate (void);
extern void vty_reset (void);
extern void vty_reset_because(const char* why) ;

extern int vty_out (struct vty *, const char *, ...) PRINTF_ATTRIBUTE(2, 3);
extern void vty_out_indent(struct vty *vty, int indent) ;
extern void vty_out_clear(struct vty *vty) ;

extern void vty_read_config (char *config_file, char *config_default);
extern void vty_read_config_first_cmd_special(
                             char *config_file, char *config_default,
                          struct cmd_element* first_cmd, bool ignore_warnings) ;

extern void vty_time_print (struct vty *, int);

extern char *vty_get_cwd (void);

extern bool vty_shell (struct vty *);
extern bool vty_term (struct vty *);
extern bool vty_shell_serv (struct vty *);
extern void vty_hello (struct vty *);
extern enum node_type vty_get_node(struct vty *);
extern void vty_set_node(struct vty *, enum node_type);
extern void vty_set_lines(struct vty *, int);

#endif /* _ZEBRA_VTY_H */

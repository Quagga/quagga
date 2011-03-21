/* VTY external interface -- header
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

#include "misc.h"
#include "vargs.h"

#include "command_common.h"     /* NB: *not* command.h          */
#include "vty_common.h"         /* struct vty & VTY types       */

#include "thread.h"
#include "log.h"
#include "qpthreads.h"
#include "qpselect.h"
#include "qtimers.h"
#include "qpnexus.h"
#include "list_util.h"
#include "vector.h"
#include "qstring.h"
#include "qpath.h"

/*==============================================================================
 * These are definitions and functions for things which are required outside
 * the vty and command family.
 */

/*------------------------------------------------------------------------------
 *
 */
enum { VTY_HIST_COUNT  = 55 } ;

/* Integrated configuration file. */
#define INTEGRATE_DEFAULT_CONFIG "Quagga.conf"

/* Conversion of "\n" to "\r\n" is done at output time.                 */
#define VTY_NEWLINE "\n"
#define VTY_NL      "\n"

/* For indenting, mostly.	*/
extern const char vty_spaces_string[] ;
enum { VTY_MAX_SPACES = 40 } ;
#define VTY_SPACES(n) (vty_spaces_string + ((n) < VTY_MAX_SPACES \
					      ? VTY_MAX_SPACES - (n) : 0))
/* Vty read buffer size. */
#define VTY_READ_BUFSIZ 512

/* Directory separator. */
#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif /* DIRECTORY_SEP */

#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(c) ((c) == DIRECTORY_SEP)
#endif

/* Utility macros to convert VTY argument to unsigned long or integer. */
#define VTY_GET_LONG(NAME,V,STR) \
do { \
  char *endptr = NULL; \
  (V) = strtoul ((STR), &endptr, 0); \
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
do {                                                                          \
  int retv;                                                                   \
  retv = inet_aton ((STR), &(V));                                             \
  if (!retv)                                                                  \
    {                                                                         \
      vty_out (vty, "%% Invalid %s value%s", NAME, VTY_NEWLINE);              \
      return CMD_WARNING;                                                     \
    }                                                                         \
} while (0)

#define VTY_GET_IPV4_PREFIX(NAME,V,STR)                                       \
do {                                                                          \
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
extern void vty_terminate (void);
extern void vty_reset (void);
extern void vty_reset_because(const char* why) ;

extern int vty_out (struct vty *, const char *, ...) PRINTF_ATTRIBUTE(2, 3);
extern int vty_write(struct vty *vty, const void* buf, int n) ;
extern int vty_out_indent(struct vty *vty, int indent) ;
extern void vty_out_clear(struct vty *vty) ;

extern void vty_sigchld(void) ;

extern void vty_read_config (const char* config_file,
                             const char* config_default);
extern void vty_read_config_first_cmd_special(const char* config_file,
                                              const char* config_default,
                                              cmd_command first_cmd,
                                              bool ignore_warnings) ;

extern qpath vty_getcwd(qpath qp);

/*------------------------------------------------------------------------------
 * vtysh
 */
extern void vty_hello (vty vty);
extern struct vty* vty_open(enum vty_type type, node_type_t node) ;
extern void vty_close_final(vty vty);
extern void vty_init_vtysh (void);

#endif /* _ZEBRA_VTY_H */

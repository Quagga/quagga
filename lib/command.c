/*
   $Id$

   Command interpreter routine for virtual terminal [aka TeletYpe]
   Copyright (C) 1997, 98, 99 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your
option) any later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "zconfig.h"
#include "version.h"

#include "misc.h"

#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include "memory.h"
#include "log.h"
#include "thread.h"
#include "vector.h"
#include "qstring.h"
#include "qtime.h"
#include "workqueue.h"
#include "command.h"
#include "command_local.h"
#include "command_parse.h"
#include "command_execute.h"
#include "command_queue.h"
#include "vty_local.h"
#include "vty_command.h"
#include "vty_io.h"

/* Vector of cmd_node, one for each known node, built during daemon
 * initialisation.
 *
 * Declared extern in command_local.h, so it can get at it.
 */
vector node_vector = NULL ;

/*==============================================================================
 * Default motd string.
 */
#define DEFAULT_MOTD \
"\n" \
"Hello, this is " QUAGGA_PROGNAME " (version " QUAGGA_VERSION ")\n" \
 QUAGGA_COPYRIGHT "\n" \
"\n"

#ifdef QDEBUG
const char *debug_banner =
    QUAGGA_PROGNAME " version " QUAGGA_VERSION " QDEBUG=" QDEBUG " "
    __DATE__ " " __TIME__;
#endif

/*==============================================================================
 * Host information structure -- shared across command/vty
 */
struct host host =
{
    /* Host name of this router.                                */
    .name_set         = false,
    .name             = NULL,   /* set by cmd_init      */
    .name_gen         = 0,      /* set by cmd_init      */

    /* Password for vty interface.                              */
    .password         = NULL,
    .password_encrypted = false,

    /* Enable password                                          */
    .enable           = NULL,
    .enable_encrypted = false,

    /* System wide terminal lines.                              */
    .lines            = -1,     /* unset                */

    /* Log filename.                                            */
    .logfile          = NULL,

    /* config file name of this host                            */
    .config_file      = NULL,

    /* Flags for services                                       */
    .advanced         = false,
    .encrypt          = false,

    /* Banner configuration.                                    */
    .motd             = DEFAULT_MOTD,
    .motdfile         = NULL,

    /* allow VTY to start without password                      */
    .no_password_check = false,

    /* if VTY starts without password, use RESTRICTED_NODE      */
    .restricted_mode   = false,

    /* Vty timeout value -- see "exec timeout" command          */
    .vty_timeout_val   = VTY_TIMEOUT_DEFAULT,

    /* Vty access-class command                                 */
    .vty_accesslist_name = NULL,

    /* Vty access-class for IPv6.                               */
    .vty_ipv6_accesslist_name = NULL,

    /* Current directory -- initialised in vty_init()            */
    .vty_cwd           = NULL,
} ;

/*==============================================================================
 * host.name handling.
 *
 * If the host.name is set explicitly by command then host.name_set is true,
 * and things are simple.
 *
 * Otherwise, need to ask the system.  Does that once at start up, and if the
 * host.name is unset by command -- so there should always be a valid host.name.
 *
 * However, it is conceivable that the host name changes (!).  So, when asking
 * for cmd_host_name(), can ask for the system to be asked afresh (if the name
 * is not explicitly set).
 *
 * The VTY watch-dog timer refreshes the host.name on a regular basis,
 * cmd_check_host_name(), so need not ask for a fresh host.name, unless wish
 * to guarantee to be absolutely up to date.
 *
 * The VTY prompts need the current host name, but that is debounced using the
 * host.name_gen value.  host.name_gen is incremented each time the host.name
 * actually changes.  It is thought unlikely that this will ever wrap round,
 * but it is guaranteed not to be zero.
 *
 * The fact that the host.name can change is reflected in the need to hold
 * the VTY_LOCK while have the host.name in hand.
 */

static void cmd_get_sys_host_name(void) ;
static void cmd_new_host_name(const char* name) ;

/*------------------------------------------------------------------------------
 * Get the host name: (a) from an explicit host name command
 *                or: (b) from the last time the system was asked.
 *
 * Note that the system is asked regularly by the watch dog.
 *
 * NB: needs to be VTY_LOCK() *or* not running qpthreads.
 */
extern const char*
cmd_host_name(bool fresh)
{
  VTY_ASSERT_LOCKED() ;

  if (!host.name_set && fresh)
    cmd_get_sys_host_name() ;

  return host.name ;
} ;

/*------------------------------------------------------------------------------
 * Set (or unset) the host name from an explicit host name command.
 *
 * If unsets, immediately asks the system for the system host name (which may
 * be the same !).
 */
static cmd_return_code_t
cmd_set_host_name(const char* name)
{
  VTY_LOCK() ;

  host.name_set = (name != NULL) ;
  if (host.name_set)
    cmd_new_host_name(name) ;
  else
    cmd_get_sys_host_name() ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Get the host name from the system and set host.name to that.
 *
 * NB: result will not be NULL, whatever happens will get at least an empty
 *     '\0' terminated string.
 *
 * NB: called early in the morning to initialise host.name and to set
 *     host.name_gen != 0.
 */
static void
cmd_get_sys_host_name(void)
{
  struct utsname info ;

  VTY_ASSERT_LOCKED() ;

  uname (&info) ;
  cmd_new_host_name(info.nodename) ;
} ;

/*------------------------------------------------------------------------------
 * Set host.name to (possibly) new value.
 *
 * Does nothing if new name == old name, otherwise increments name_gen.
 */
static void
cmd_new_host_name(const char* name)
{
  if ((host.name == NULL) || (strcmp(host.name, name) != 0))
    {
      XFREE(MTYPE_HOST, host.name) ;
      host.name = XSTRDUP(MTYPE_HOST, name) ;
      do ++host.name_gen ; while (host.name_gen == 0) ;
    } ;
} ;

/*==============================================================================
 * node structures for the basic nodes.
 *
 * Covers everything up to and including the CONFIG_NODE.
 */
static struct cmd_node auth_node =
{
  .node         = AUTH_NODE,
  .prompt       = "Password: ",

  .parent       = AUTH_NODE,    /* self => no parent    */
  .exit_to      = AUTH_NODE,    /* self => no exit      */
  .end_to       = AUTH_NODE,    /* self => no end       */
};

static struct cmd_node view_node =
{
  .node         = VIEW_NODE,
  .prompt       = "%s> ",

  .parent       = AUTH_NODE,    /* self => no parent    */
  .exit_to      = NULL_NODE,    /* close !              */
  .end_to       = VIEW_NODE,    /* self => no end       */
};

static struct cmd_node restricted_node =
{
  .node         = RESTRICTED_NODE,
  .prompt       = "%s$ ",

  .parent       = AUTH_NODE,        /* self => no parent    */
  .exit_to      = NULL_NODE,        /* close !              */
  .end_to       = RESTRICTED_NODE,  /* self => no end       */
};

static struct cmd_node auth_enable_node =
{
  .node         = AUTH_ENABLE_NODE,
  .prompt       = "Password: ",

  .parent       = AUTH_ENABLE_NODE, /* self => no parent    */
  .exit_to      = NULL_NODE,        /* close !              */
  .end_to       = AUTH_ENABLE_NODE, /* self => no end       */
};

static struct cmd_node enable_node =
{
  .node         = ENABLE_NODE,
  .prompt       = "%s# ",

  .parent       = ENABLE_NODE,  /* self => no parent    */
  .exit_to      = NULL_NODE,    /* close !              */
  .end_to       = ENABLE_NODE,  /* self => no end       */
};

static struct cmd_node config_node =
{
  .node         = CONFIG_NODE,
  .prompt       = "%s(config)# ",

  .parent       = CONFIG_NODE,  /* self => no parent            */
  .exit_to      = ENABLE_NODE,  /* exit == end for CONFIG_NODE  */
  .end_to       = ENABLE_NODE,  /* standard end action          */

  .config_to_vtysh = true
};

static const struct facility_map {
  int facility;
  const char *name;
  size_t match;
} syslog_facilities[] =
  {
    { LOG_KERN,   "kern",   1 },
    { LOG_USER,   "user",   2 },
    { LOG_MAIL,   "mail",   1 },
    { LOG_DAEMON, "daemon", 1 },
    { LOG_AUTH,   "auth",   1 },
    { LOG_SYSLOG, "syslog", 1 },
    { LOG_LPR,    "lpr",    2 },
    { LOG_NEWS,   "news",   1 },
    { LOG_UUCP,   "uucp",   2 },
    { LOG_CRON,   "cron",   1 },
#ifdef LOG_FTP
    { LOG_FTP, "ftp", 1 },
#endif
    { LOG_LOCAL0, "local0", 6 },
    { LOG_LOCAL1, "local1", 6 },
    { LOG_LOCAL2, "local2", 6 },
    { LOG_LOCAL3, "local3", 6 },
    { LOG_LOCAL4, "local4", 6 },
    { LOG_LOCAL5, "local5", 6 },
    { LOG_LOCAL6, "local6", 6 },
    { LOG_LOCAL7, "local7", 6 },
    { 0, NULL, 0 },
  };

#if 0
static enum cmd_return_code
cmd_pipe_func(cmd_command self   DEFUN_CMD_ARG_UNUSED,
              struct vty* vty    DEFUN_CMD_ARG_UNUSED,
              int argc           DEFUN_CMD_ARG_UNUSED,
              argv_t argv        DEFUN_CMD_ARG_UNUSED)
{
  return CMD_SUCCESS ;
} ;

static struct cmd_command cmd_pipe_element =
{
  .string    = "< or <|",       /* Dummy                                */
  .func      = cmd_pipe_func,
  .doc       = "Pipe input to command processor",
  .daemon    = 0,
  .items    = NULL,
  .cmdsize   = 0,
  .config    = NULL,
  .subconfig = NULL,
  .attr      = CMD_ATTR_SIMPLE,
} ;
#endif


static const char *
facility_name(int facility)
{
  const struct facility_map *fm;

  for (fm = syslog_facilities; fm->name; fm++)
    if (fm->facility == facility)
      return fm->name;
  return "";
}

static int
facility_match(const char *str)
{
  const struct facility_map *fm;

  for (fm = syslog_facilities; fm->name; fm++)
    if (!strncmp(str,fm->name,fm->match))
      return fm->facility;
  return -1;
}

static int
level_match(const char *s)
{
  int level ;

  for ( level = 0 ; zlog_priority [level] != NULL ; level ++ )
    if (!strncmp (s, zlog_priority[level], 2))
      return level;
  return ZLOG_DISABLED;
}

/* This is called from main when a daemon is invoked with -v or --version. */
void
print_version (const char *progname)
{
  printf ("%s version %s\n", progname, QUAGGA_VERSION);
  printf ("%s\n", QUAGGA_COPYRIGHT);
}


/*------------------------------------------------------------------------------
 * Concatenate argv argument into a single string, inserting ' ' between each
 * argument.
 *
 */
extern char *
argv_concat (const char* const* argv, int argc, int shift)
{
  int i;
  size_t len;
  char *str;
  char *p;

  len = 0;
  for (i = shift; i < argc; i++)
    len += strlen(argv[i])+1;

  if (!len)
    return NULL;

  p = str = XMALLOC(MTYPE_TMP, len);
  for (i = shift; i < argc; i++)
    {
      size_t arglen;
      memcpy(p, argv[i], (arglen = strlen(argv[i])));
      p += arglen;
      *p++ = ' ';
    }
  *(p-1) = '\0';
  return str;
}

#if 0 //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
/* Compare two command's string.  Used in sort_node (). */
static int
cmp_node (const void *p, const void *q)
{
  const struct cmd_command *a = *(struct cmd_command * const *)p;
  const struct cmd_command *b = *(struct cmd_command * const *)q;

  return strcmp (a->string, b->string);
}

static int
cmp_desc (const void *p, const void *q)
{
  const struct desc *a = *(struct desc * const *)p;
  const struct desc *b = *(struct desc * const *)q;

  return strcmp (a->cmd, b->cmd);
}

#endif //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

/*------------------------------------------------------------------------------
 * Install top node of command vector.
 *
 * Initialised as follows:
 *
 *   .node            -- must be set before node is installed
 *   .prompt          -- must be set before node is installed
 *   .config_to_vtysh -- must be set true/false before node is installed
 *
 *   .parent          -- may be set, or if 0 (node_null) will be set to default
 *   .exit_to         -- may be set, or if 0 (node_null) will be set to default
 *   .end_to          -- may be set, or if 0 (node_null) will be set to default
 *
 *   .config_write    -- is set from parameter
 *
 *   .cmd_vector      -- initialised empty
 *
 * Default parent node:
 *
 *   * all nodes >= NODE_CONFIG have NODE_CONFIG as a parent
 *   * all nodes <  NODE_CONFIG are their own parents
 *
 * Default exit_to:
 *
 *   * all nodes >  NODE_CONFIG exit_to their parent
 *   *     node  == NODE_CONFIG exit_to ENABLE_NODE (same as end_to !)
 *   * all nodes <  NODE_CONFIG exit_to close
 *
 * Default end_to:
 *
 *   * all nodes >= NODE_CONFIG end_to ENABLE_NODE
 *   * all nodes <  NODE_CONFIG end_to themselves
 *
 */
extern void
install_node (struct cmd_node *node,
	      int (*config_write) (struct vty *))
{
  confirm(NULL_NODE == 0) ; /* unset value for .parent, .end_to & .exit_to */

  if (node->parent == NULL_NODE)
    {
      if (node->node >= CONFIG_NODE)
        node->parent = CONFIG_NODE ;
      else
        node->parent = node->node ;
    } ;

  if (node->end_to == NULL_NODE)
    {
      if (node->node >= CONFIG_NODE)
        node->end_to = ENABLE_NODE ;
      else
        node->end_to = node->node ;
    } ;

  if (node->exit_to == NULL_NODE)
    {
      if      (node->node > CONFIG_NODE)
        node->exit_to = node->parent ;
      else if (node->node == CONFIG_NODE)
        node->exit_to = ENABLE_NODE ;
      else
        node->exit_to = NULL_NODE ;
    } ;

  node->config_write = config_write ;

  vector_init_new(node->cmd_vector, 0) ;        /* embedded     */

  vector_set_index (node_vector, node->node, node);
} ;

/*------------------------------------------------------------------------------
 * Return address of cmd_node -- asserts is not NULL !
 */
static cmd_node
cmd_cmd_node(node_type_t node)
{
  cmd_node cn ;

  cn = vector_get_item(node_vector, node) ;
  if (cn != NULL)
    return cn ;

  zabort("invalid node") ;
} ;

/*------------------------------------------------------------------------------
 * Return parent node
 */
extern node_type_t
cmd_node_parent(node_type_t node)
{
  return (cmd_cmd_node(node))->parent ;
} ;

/*------------------------------------------------------------------------------
 * Return exit_to node
 */
extern node_type_t
cmd_node_exit_to(node_type_t node)
{
  return (cmd_cmd_node(node))->exit_to ;
} ;

/*------------------------------------------------------------------------------
 * Return parent node
 */
extern node_type_t
cmd_node_end_to(node_type_t node)
{
  return (cmd_cmd_node(node))->end_to ;
} ;



/*------------------------------------------------------------------------------
 * Sorting of all node cmd_vectors.
 */

/* Compare two command's string.  Used in sort_node (). */
static int
cmp_node (const struct cmd_command **a, const struct cmd_command **b)
{
  return strcmp ((*a)->string, (*b)->string);
}

/* Sort each node's command element according to command string. */
extern void
sort_node ()
{
  unsigned int i ;

  for (i = 0; i < vector_length(node_vector); i++)
    {
      struct cmd_node *cnode;
      vector cmd_vector ;

      cnode = vector_get_item(node_vector, i) ;

      if (cnode == NULL)
        continue ;

      cmd_vector = cnode->cmd_vector;
      if (cmd_vector == NULL)
        continue ;

      vector_sort(cmd_vector, (vector_sort_cmp*)cmp_node) ;
    } ;
} ;

#if 0
/*------------------------------------------------------------------------------
 * Take string and break it into tokens -- see cmd_make_tokens().
 *
 * Returns:  NULL => empty line (after white-space trimming) or comment line.
 *      otherwise: is vector containing one or more tokens in qstrings.
 */
extern vector
cmd_make_strvec (const char *string)
{
#if 0
  return cmd_tokenise(NULL, string) ;
#error sort this one out
#endif
  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Add given string to vector of strings.
 *
 * Create vector if required.
 */
extern vector
cmd_add_to_strvec (vector items, const char* str)
{
  if (items == NULL)
    items = vector_init(1) ;

  vector_push_item(items, XSTRDUP(MTYPE_STRVEC, str));

  return items ;
} ;

/*------------------------------------------------------------------------------
 * Free allocated string vector (if any) and all its contents.
 *
 * Note that this is perfectly happy with items == NULL.
 */
extern void
cmd_free_strvec (vector items)
{
  char *cp;

  /* Note that vector_ream_free() returns NULL if items == NULL        */
  while((cp = vector_ream(items, free_it)) != NULL)
    XFREE (MTYPE_STRVEC, cp);
} ;

#endif

/*------------------------------------------------------------------------------
 * Return prompt string for the specified node.
 */
extern const char *
cmd_prompt(node_type_t node)
{
  struct cmd_node *cnode;

  assert(node_vector != NULL) ;
  assert(node_vector->p_items != NULL) ;

  cnode = NULL ;
  if (node < node_vector->limit)
    cnode = vector_get_item (node_vector, node);

  if (cnode == NULL)
    {
      zlog_err("Could not find prompt for node %d for", node) ;
      return NULL ;
    } ;

  return cnode->prompt;
}

/*------------------------------------------------------------------------------
 * Install a command into a node.
 *
 */
extern void
install_element(node_type_t ntype, cmd_command cmd)
{
  cmd_node cnode;

  cnode = vector_get_item (node_vector, ntype);

  if (cnode == NULL)
    {
      fprintf (stderr, "Command node %d doesn't exist, please check it\n",
	       ntype);
      exit (1);
    }

  vector_set (cnode->cmd_vector, cmd);

  /* A cmd_command may appear in a number of cmd_vectors, but the cmd->items
   * etc. need only be set up once.
   *
   * It is assumed that once a cmd_command has been installed it will never be
   * changed !
   *
   * Need now to "compile" the command if not already compiled.
   */
  if (cmd->items == NULL)
    cmd_compile(cmd);

  /* Post compilation check for reasonable cmd_command !                */
  cmd_compile_check(cmd) ;
} ;

/*==============================================================================
 * Password encryption
 */
static const unsigned char itoa64[] =
    "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/* Uses the usual crypt() function.
 *
 * Note that crypt() is not thread safe !
 */
static const char *
zencrypt (const char *passwd)
{
  uint32_t r ;
  char salt[3];

  extern char *crypt (const char *, const char *) ;

  r = qt_random(*passwd) ;

  salt[0] = itoa64[(r >> (32 -  5)) & 0x3F] ;   /* ms 5         */
  salt[1] = itoa64[(r >> (32 - 10)) & 0x3F] ;   /* next ms 5    */
  salt[2] = '\0';

  return crypt(passwd, salt) ;
}

/* This function write configuration of this host. */
static int
config_write_host (struct vty *vty)
{
  VTY_LOCK() ;

  if (qpthreads_enabled)
    vty_out (vty, "threaded%s", VTY_NEWLINE);

  if (host.name_set)
    vty_out (vty, "hostname %s%s", host.name, VTY_NEWLINE);

  if (host.password != NULL)
    {
      if (host.password_encrypted)
        vty_out (vty, "password 8 %s\n", host.password);
      else
        vty_out (vty, "password %s\n", host.password);
    } ;

  if (host.enable != NULL)
    {
      if (host.enable_encrypted)
        vty_out (vty, "enable password 8 %s\n", host.enable);
      else
        vty_out (vty, "enable password %s\n", host.enable);
    } ;

  if (zlog_get_default_lvl(NULL) != LOG_DEBUG)
    {
      vty_out (vty, "! N.B. The 'log trap' command is deprecated.%s",
	       VTY_NEWLINE);
      vty_out (vty, "log trap %s%s",
	       zlog_priority[zlog_get_default_lvl(NULL)], VTY_NEWLINE);
    }

  if (host.logfile && (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != ZLOG_DISABLED))
    {
      vty_out (vty, "log file %s", host.logfile);
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != zlog_get_default_lvl(NULL))
	vty_out (vty, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_FILE)]);
      vty_out (vty, "%s", VTY_NEWLINE);
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != ZLOG_DISABLED)
    {
      vty_out (vty, "log stdout");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != zlog_get_default_lvl(NULL))
	vty_out (vty, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT)]);
      vty_out (vty, "%s", VTY_NEWLINE);
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) == ZLOG_DISABLED)
    vty_out(vty,"no log monitor%s",VTY_NEWLINE);
  else if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) != zlog_get_default_lvl(NULL))
    vty_out(vty,"log monitor %s%s",
	    zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR)],VTY_NEWLINE);

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != ZLOG_DISABLED)
    {
      vty_out (vty, "log syslog");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != zlog_get_default_lvl(NULL))
	vty_out (vty, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG)]);
      vty_out (vty, "%s", VTY_NEWLINE);
    }

  if (zlog_get_facility(NULL) != LOG_DAEMON)
    vty_out (vty, "log facility %s%s",
	     facility_name(zlog_get_facility(NULL)), VTY_NEWLINE);

  if (zlog_get_record_priority(NULL) == 1)
    vty_out (vty, "log record-priority%s", VTY_NEWLINE);

  if (zlog_get_timestamp_precision(NULL) > 0)
    vty_out (vty, "log timestamp precision %d%s",
        zlog_get_timestamp_precision(NULL), VTY_NEWLINE);

  if (host.advanced)
    vty_out (vty, "service advanced-vty%s", VTY_NEWLINE);

  if (host.encrypt)
    vty_out (vty, "service password-encryption%s", VTY_NEWLINE);

  if (host.lines >= 0)
    vty_out (vty, "service terminal-length %d%s", host.lines,
	     VTY_NEWLINE);

  if      (host.motdfile)
    vty_out (vty, "banner motd file %s%s", host.motdfile, VTY_NEWLINE);
  else if (! host.motd)
    vty_out (vty, "no banner motd%s", VTY_NEWLINE);

  VTY_UNLOCK() ;

  return 1;
}

/*============================================================================*/

/*----------------------------------------------------------------------------*/

/* Configration from terminal */
DEFUN_CALL (config_terminal,
       config_terminal_cmd,
       "configure terminal",
       "Configuration from vty interface\n"
       "Configuration terminal\n")
{
  if (vty_config_lock (vty, CONFIG_NODE))
    return CMD_SUCCESS;

  vty_out (vty, "VTY configuration is locked by other VTY%s", VTY_NEWLINE);
  return CMD_WARNING;
}

/* Enable command */
DEFUN_CALL (enable,
       config_enable_cmd,
       "enable",
       "Turn on privileged mode command\n")
{
  /* If enable password is NULL, change to ENABLE_NODE */
  if ((host.enable == NULL) || (vty->type == VTY_SHELL_SERVER))
    vty->node = ENABLE_NODE ;
  else
    vty->node = AUTH_ENABLE_NODE ;

  return CMD_SUCCESS;
}

/* Disable command */
DEFUN_CALL (disable,
       config_disable_cmd,
       "disable",
       "Turn off privileged mode command\n")
{
  if (vty->node == ENABLE_NODE)
    vty->node = VIEW_NODE;
  return CMD_SUCCESS;
}

/* Down vty node level. */
DEFUN_CALL (config_exit,
       config_exit_cmd,
       "exit",
       "Exit current mode and down to previous mode\n")
{
  return cmd_exit(vty) ;
}

/* quit is alias of exit. */
ALIAS_CALL (config_exit,
       config_quit_cmd,
       "quit",
       "Exit current mode and down to previous mode\n")

/* End of configuration. */
DEFUN_CALL (config_end,
       config_end_cmd,
       "end",
       "End current mode and change to enable mode\n")
{
  return cmd_end(vty) ;
}

/* Show version. */
DEFUN_CALL (show_version,
       show_version_cmd,
       "show version",
       SHOW_STR
       "Displays zebra version\n")
{
  vty_out (vty, "Quagga %s (%s).%s", QUAGGA_VERSION, host.name?host.name:"",
	   VTY_NEWLINE);
  vty_out (vty, "%s%s", QUAGGA_COPYRIGHT, VTY_NEWLINE);

  return CMD_SUCCESS;
}

/* Help display function for all node. */
DEFUN_CALL (config_help,
       config_help_cmd,
       "help",
       "Description of the interactive help system\n")
{
  vty_out (vty,
	   "Quagga VTY provides advanced help feature.  When you need help,%s\
anytime at the command line please press '?'.%s\
%s\
If nothing matches, the help list will be empty and you must backup%s\
 until entering a '?' shows the available options.%s\
Two styles of help are provided:%s\
1. Full help is available when you are ready to enter a%s\
command argument (e.g. 'show ?') and describes each possible%s\
argument.%s\
2. Partial help is provided when an abbreviated argument is entered%s\
   and you want to know what arguments match the input%s\
   (e.g. 'show me?'.)%s%s", VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE,
	   VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE,
	   VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE, VTY_NEWLINE);
  return CMD_SUCCESS;
}

/* Help display function for all node. */
DEFUN_CALL (config_list,
       config_list_cmd,
       "list",
       "Print command list\n")
{
  unsigned int i;
  struct cmd_node *cnode = vector_get_item (node_vector, vty->node);
  struct cmd_command *cmd;

  for (i = 0; i < vector_length (cnode->cmd_vector); i++)
    if ((cmd = vector_get_item (cnode->cmd_vector, i)) != NULL
        && !(cmd->attr & (CMD_ATTR_DEPRECATED | CMD_ATTR_HIDDEN)))
      vty_out (vty, "  %s%s", cmd->string,
	       VTY_NEWLINE);
  return CMD_SUCCESS;
}

/* Write current configuration into file. */
DEFUN (config_write_file,
       config_write_file_cmd,
       "write file",
       "Write running configuration to memory, network, or terminal\n"
       "Write to configuration file\n")
{
  unsigned int i;
  int fd;
  int err;
  struct cmd_node *node;
  char *config_file;
  char *config_file_tmp = NULL;
  char *config_file_sav = NULL;
  int ret = CMD_WARNING;

  /* Check and see if we are operating under vtysh configuration */
  if (host.config_file == NULL)
    {
      vty_out (vty, "Can't save to configuration file, using vtysh.%s",
	       VTY_NEWLINE);
      return CMD_WARNING;
    }

  /* Get filename. */
  config_file = host.config_file;

  config_file_sav =
    XMALLOC (MTYPE_TMP, strlen (config_file) + strlen (CONF_BACKUP_EXT) + 1);
  strcpy (config_file_sav, config_file);
  strcat (config_file_sav, CONF_BACKUP_EXT);


  config_file_tmp = XMALLOC (MTYPE_TMP, strlen (config_file) + 8);
  sprintf (config_file_tmp, "%s.XXXXXX", config_file);

  /* Open file to configuration write. */
  fd = mkstemp (config_file_tmp);
  if (fd < 0)
    {
      vty_out (vty, "Can't open configuration file %s.\n", config_file_tmp) ;
      goto finished;
    }

  /* Make vty for configuration file. */
  vty_open_config_write(vty, fd) ;

  /* Config file header print. */
  vty_out (vty, "!\n! Zebra configuration saved from vty\n!   ");
  vty_time_print (vty, 1);
  vty_out (vty, "!\n");

  for (i = 0; i < vector_length (node_vector); i++)
    {
      if ((node = vector_get_item (node_vector, i)) && node->config_write)
        {
          if ((*node->config_write) (vty))
            vty_out (vty, "!\n");

          vty_cmd_out_push(vty) ;       /* Push stuff so far            */
        }
    } ;

  err = vty_close_config_write(vty) ;

  if (err != 0)
    {
      vty_out (vty, "Failed while writing configuration file %s.%s",
                                                  config_file_tmp, VTY_NEWLINE);
      goto finished;
    }

  if (unlink (config_file_sav) != 0)
    if (errno != ENOENT)
      {
	vty_out (vty, "Can't unlink backup configuration file %s.%s",
	                                          config_file_sav, VTY_NEWLINE);
        goto finished;
      } ;

  if (link (config_file, config_file_sav) != 0)
    {
      vty_out (vty, "Can't backup old configuration file %s.%s",
                                                  config_file_sav, VTY_NEWLINE);
      goto finished;
    } ;

  sync () ;

  if (unlink (config_file) != 0)
    {
      vty_out (vty, "Can't unlink configuration file %s.%s",
                                                      config_file, VTY_NEWLINE);
      goto finished;
    } ;

  if (link (config_file_tmp, config_file) != 0)
    {
      vty_out (vty, "Can't save configuration file %s.%s",
                                                      config_file, VTY_NEWLINE);
      goto finished;
    } ;

  sync ();

  if (chmod (config_file, CONFIGFILE_MASK) != 0)
    {
      vty_out (vty, "Can't chmod configuration file %s: %s (%s).\n",
	config_file, errtostr(errno, 0).str, errtoname(errno, 0).str);
      goto finished;
    }

  vty_out (vty, "Configuration saved to %s\n", config_file);

  ret = CMD_SUCCESS;

finished:
  unlink (config_file_tmp);
  XFREE (MTYPE_TMP, config_file_tmp);
  XFREE (MTYPE_TMP, config_file_sav);
  return ret;
}

ALIAS (config_write_file,
       config_write_cmd,
       "write",
       "Write running configuration to memory, network, or terminal\n")

ALIAS (config_write_file,
       config_write_memory_cmd,
       "write memory",
       "Write running configuration to memory, network, or terminal\n"
       "Write configuration to the file (same as write file)\n")

ALIAS (config_write_file,
       copy_runningconfig_startupconfig_cmd,
       "copy running-config startup-config",
       "Copy configuration\n"
       "Copy running config to... \n"
       "Copy running config to startup config (same as write file)\n")

/* Write current configuration into the terminal. */
DEFUN (config_write_terminal,
       config_write_terminal_cmd,
       "write terminal",
       "Write running configuration to memory, network, or terminal\n"
       "Write to terminal\n")
{
  unsigned int i;
  bool vtysh_config ;

  vtysh_config = (vty->type == VTY_SHELL_SERVER) ;

  if (!vtysh_config)
    vty_out (vty, "\n"
                  "Current configuration:\n"
                  "!\n") ;

  for (i = 0 ; i < vector_length(node_vector) ; i++)
    {
      cmd_node node ;

      node = vector_get_item(node_vector, i) ;

      if (node == NULL)
        continue ;

      if (vtysh_config && !node->config_to_vtysh)
        continue ;

      if ((*node->config_write != NULL) && ((*node->config_write)(vty) != 0))
        vty_out (vty, "!\n") ;
    } ;

  if (!vtysh_config)
    vty_out (vty, "end\n") ;

  return CMD_SUCCESS;
}

/* Write current configuration into the terminal. */
ALIAS (config_write_terminal,
       show_running_config_cmd,
       "show running-config",
       SHOW_STR
       "running configuration\n")

/* Write startup configuration into the terminal. */
DEFUN (show_startup_config,
       show_startup_config_cmd,
       "show startup-config",
       SHOW_STR
       "Contentes of startup configuration\n")
{
  char buf[BUFSIZ];
  FILE *confp;

  confp = fopen (host.config_file, "r");
  if (confp == NULL)
    {
      vty_out (vty, "Can't open configuration file [%s]%s",
	       host.config_file, VTY_NEWLINE);
      return CMD_WARNING;
    }

  while (fgets (buf, BUFSIZ, confp))
    {
      char *cp = buf;

      while (*cp != '\r' && *cp != '\n' && *cp != '\0')
	cp++;
      *cp = '\0';

      vty_out (vty, "%s%s", buf, VTY_NEWLINE);
    }

  fclose (confp);

  return CMD_SUCCESS;
}

/* Hostname configuration */
DEFUN_CALL (config_hostname,
       hostname_cmd,
       "hostname WORD",
       "Set system's network name\n"
       "This system's network name\n")
{
  if (!isalpha((int) *argv[0]))
    {
      vty_out (vty, "Please specify string starting with alphabet\n");
      return CMD_WARNING;
    }

  return cmd_set_host_name(argv[0]) ;
}

DEFUN_CALL (config_no_hostname,
       no_hostname_cmd,
       "no hostname [HOSTNAME]",
       NO_STR
       "Reset system's network name\n"
       "Host name of this router\n")
{
  return cmd_set_host_name(NULL) ;
}

/*------------------------------------------------------------------------------
 * Password setting function -- common for password and enable password.
 */
static cmd_return_code_t
do_set_password(vty vty, int argc, argv_t argv, char** p_password,
                                                bool*  p_encrypted)
{
  cmd_return_code_t ret ;

  /* Argument check. */
  if (argc == 0)
    {
      vty_out (vty, "Please specify password.%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  VTY_LOCK() ;
  ret = CMD_SUCCESS ;

  if (argc == 2)
    {
      /* Encrypted password argument                            */

      if (*argv[0] == '8')
        {
          XFREE(MTYPE_HOST, *p_password);
          *p_encrypted = true ;
          *p_password  = XSTRDUP(MTYPE_HOST, argv[1]);
        }
      else
        {
          vty_out(vty, "Unknown encryption type.\n");
          ret = CMD_WARNING;
        }
    }
  else
    {
      /* Plaintext password argument                                    */

      if (!isalnum ((int) *argv[0]))
        {
          vty_out(vty, "Please specify string starting with alphanumeric\n");
          ret = CMD_WARNING ;
        }
      else
        {
          /* If host.encrypt, only keeps the encrypted password.        */

          XFREE (MTYPE_HOST, *p_password);

          *p_encrypted = host.encrypt ;
          if (*p_encrypted)
            *p_password = XSTRDUP (MTYPE_HOST, zencrypt (argv[0]));
          else
            *p_password = XSTRDUP (MTYPE_HOST, argv[0]);
        } ;
    } ;

  VTY_UNLOCK() ;
  return ret ;
} ;

/* VTY interface password set. */
DEFUN_CALL (config_password, password_cmd,
       "password (8) WORD",
       "Assign the terminal connection password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN line password string\n")
{
  return do_set_password(vty, argc, argv, &host.password,
                                          &host.password_encrypted) ;
} ;

ALIAS_CALL (config_password, password_text_cmd,
       "password LINE",
       "Assign the terminal connection password\n"
       "The UNENCRYPTED (cleartext) line password\n")

/* VTY enable password set. */
DEFUN_CALL (config_enable_password, enable_password_cmd,
       "enable password (8) WORD",
       "Modify enable password parameters\n"
       "Assign the privileged level password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN 'enable' password string\n")
{
  return do_set_password(vty, argc, argv, &host.enable,
                                          &host.enable_encrypted) ;
} ;

ALIAS_CALL (config_enable_password,
       enable_password_text_cmd,
       "enable password LINE",
       "Modify enable password parameters\n"
       "Assign the privileged level password\n"
       "The UNENCRYPTED (cleartext) 'enable' password\n")

/* VTY enable password delete. */
DEFUN_CALL (no_config_enable_password, no_enable_password_cmd,
       "no enable password",
       NO_STR
       "Modify enable password parameters\n"
       "Assign the privileged level password\n")
{
  VTY_LOCK() ;

  host.enable_encrypted = false ;
  XFREE (MTYPE_HOST, host.enable);

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (service_password_encrypt,
       service_password_encrypt_cmd,
       "service password-encryption",
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  VTY_LOCK() ;

  host.encrypt = true ;

  /* If we have an unencrypted password in hand, convert that now.
   * If not, retain any already encrypted password.
   */
  if (!host.password_encrypted && (host.password != NULL))
    {
      char* plain = host.password ;
      host.password = XSTRDUP(MTYPE_HOST, zencrypt (plain)) ;
      XFREE(MTYPE_HOST, plain) ;
      host.password_encrypted = true ;
    } ;

  if (!host.enable_encrypted && (host.enable != NULL))
    {
      char* plain = host.enable ;
      host.enable = XSTRDUP(MTYPE_HOST, zencrypt (plain)) ;
      XFREE(MTYPE_HOST, plain) ;
      host.enable_encrypted = true ;
    } ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_password_encrypt,
       no_service_password_encrypt_cmd,
       "no service password-encryption",
       NO_STR
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  VTY_LOCK() ;

  /* Keep any existing passwords, encrypted or not.                     */

  host.encrypt = false ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (config_terminal_length, config_terminal_length_cmd,
       "terminal length <0-512>",
       "Set terminal line parameters\n"
       "Set number of lines on a screen\n"
       "Number of lines on screen (0 for no pausing)\n")
{
  int lines;
  char *endptr = NULL;

  lines = strtol (argv[0], &endptr, 10);
  if (lines < 0 || lines > 512 || *endptr != '\0')
    {
      vty_out (vty, "length is malformed%s", VTY_NEWLINE);
      return CMD_WARNING;
    }
  vty_set_lines(vty, lines);

  return CMD_SUCCESS;
}

DEFUN_CALL (config_terminal_no_length, config_terminal_no_length_cmd,
       "terminal no length",
       "Set terminal line parameters\n"
       NO_STR
       "Set number of lines on a screen\n")
{
  vty_set_lines(vty, -1);
  return CMD_SUCCESS;
}

static cmd_return_code_t
set_host_lines(int lines)
{
  VTY_LOCK() ;
  host.lines = lines ;
  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

DEFUN_CALL (service_terminal_length, service_terminal_length_cmd,
       "service terminal-length <0-512>",
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "Number of lines of VTY (0 means no line control)\n")
{
  int lines;
  char *endptr = NULL;

  lines = strtol (argv[0], &endptr, 10);
  if (lines < 0 || lines > 512 || *endptr != '\0')
    {
      vty_out (vty, "length is malformed%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  return set_host_lines(lines) ;
} ;

DEFUN_CALL (no_service_terminal_length, no_service_terminal_length_cmd,
       "no service terminal-length [<0-512>]",
       NO_STR
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "Number of lines of VTY (0 means no line control)\n")
{
  return set_host_lines(-1) ;
}

DEFUN_HID_CALL (do_echo,
	      echo_cmd,
	      "echo .MESSAGE",
	      "Echo a message back to the vty\n"
	      "The message to echo\n")
{
  char *message;

  message = argv_concat(argv, argc, 0) ;
  vty_out (vty, "%s\n", message ? message : "") ;
  XFREE(MTYPE_TMP, message);

  return CMD_SUCCESS;
}

DEFUN_CALL (config_logmsg,
       config_logmsg_cmd,
       "logmsg "LOG_LEVELS" .MESSAGE",
       "Send a message to enabled logging destinations\n"
       LOG_LEVEL_DESC
       "The message to send\n")
{
  int level;
  char *message;

  if ((level = level_match(argv[0])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;

  message = argv_concat(argv, argc, 1);
  zlog(NULL, level, "%s", (message ? message : ""));
  XFREE(MTYPE_TMP, message);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_logging,
       show_logging_cmd,
       "show logging",
       SHOW_STR
       "Show current logging configuration\n")
{
  vty_out (vty, "Syslog logging: ");
  if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) == ZLOG_DISABLED)
    vty_out (vty, "disabled");
  else
    vty_out (vty, "level %s, facility %s, ident %s",
	     zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG)],
	     facility_name(zlog_get_facility(NULL)), zlog_get_ident(NULL));
  vty_out (vty, "%s", VTY_NEWLINE);

  vty_out (vty, "Stdout logging: ");
  if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) == ZLOG_DISABLED)
    vty_out (vty, "disabled");
  else
    vty_out (vty, "level %s",
	     zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT)]);
  vty_out (vty, "%s", VTY_NEWLINE);

  vty_out (vty, "Monitor logging: ");
  if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) == ZLOG_DISABLED)
    vty_out (vty, "disabled");
  else
    vty_out (vty, "level %s",
	     zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR)]);
  vty_out (vty, "%s", VTY_NEWLINE);

  vty_out (vty, "File logging: ");
  if ((zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) == ZLOG_DISABLED) ||
      !zlog_is_file(NULL))
    vty_out (vty, "disabled");
  else
    {
      char * filename = zlog_get_filename(NULL);
      vty_out (vty, "level %s, filename %s",
	     zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_FILE)],
	     filename);
      free(filename);
    }
  vty_out (vty, "%s", VTY_NEWLINE);

  vty_out (vty, "Protocol name: %s%s",
      zlog_get_proto_name(NULL), VTY_NEWLINE);
  vty_out (vty, "Record priority: %s%s",
  	   (zlog_get_record_priority(NULL) ? "enabled" : "disabled"), VTY_NEWLINE);
  vty_out (vty, "Timestamp precision: %d%s",
      zlog_get_timestamp_precision(NULL), VTY_NEWLINE);

  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_stdout,
       config_log_stdout_cmd,
       "log stdout",
       "Logging control\n"
       "Set stdout logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_STDOUT, zlog_get_default_lvl(NULL));
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_stdout_level,
       config_log_stdout_level_cmd,
       "log stdout "LOG_LEVELS,
       "Logging control\n"
       "Set stdout logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(argv[0])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;
  zlog_set_level (NULL, ZLOG_DEST_STDOUT, level);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_stdout,
       no_config_log_stdout_cmd,
       "no log stdout [LEVEL]",
       NO_STR
       "Logging control\n"
       "Cancel logging to stdout\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_STDOUT, ZLOG_DISABLED);
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_monitor,
       config_log_monitor_cmd,
       "log monitor",
       "Logging control\n"
       "Set terminal line (monitor) logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_MONITOR, zlog_get_default_lvl(NULL));
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_monitor_level,
       config_log_monitor_level_cmd,
       "log monitor "LOG_LEVELS,
       "Logging control\n"
       "Set terminal line (monitor) logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(argv[0])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;
  zlog_set_level (NULL, ZLOG_DEST_MONITOR, level);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_monitor,
       no_config_log_monitor_cmd,
       "no log monitor [LEVEL]",
       NO_STR
       "Logging control\n"
       "Disable terminal line (monitor) logging\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_MONITOR, ZLOG_DISABLED);
  return CMD_SUCCESS;
}

static int
set_log_file(struct vty *vty, const char *fname, int loglevel)
{
  int ret;
  char *p = NULL;
  const char *fullpath;

  /* Path detection. */
  if (! IS_DIRECTORY_SEP (*fname))
    {
      char cwd[MAXPATHLEN+1];
      cwd[MAXPATHLEN] = '\0';

      if (getcwd (cwd, MAXPATHLEN) == NULL)
        {
          zlog_err ("config_log_file: Unable to alloc mem!");
          return CMD_WARNING;
        }

      if ( (p = XMALLOC (MTYPE_TMP, strlen (cwd) + strlen (fname) + 2))
          == NULL)
        {
          zlog_err ("config_log_file: Unable to alloc mem!");
          return CMD_WARNING;
        }
      sprintf (p, "%s/%s", cwd, fname);
      fullpath = p;
    }
  else
    fullpath = fname;

  ret = zlog_set_file (NULL, fullpath, loglevel);

  if (p)
    XFREE (MTYPE_TMP, p);

  if (!ret)
    {
      vty_out (vty, "can't open logfile %s\n", fname);
      return CMD_WARNING;
    }

  if (host.logfile)
    XFREE (MTYPE_HOST, host.logfile);

  host.logfile = XSTRDUP (MTYPE_HOST, fname);

  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_file,
       config_log_file_cmd,
       "log file FILENAME",
       "Logging control\n"
       "Logging to file\n"
       "Logging filename\n")
{
  return set_log_file(vty, argv[0], zlog_get_default_lvl(NULL));
}

DEFUN_CALL (config_log_file_level,
       config_log_file_level_cmd,
       "log file FILENAME "LOG_LEVELS,
       "Logging control\n"
       "Logging to file\n"
       "Logging filename\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(argv[1])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;
  return set_log_file(vty, argv[0], level);
}

DEFUN_CALL (no_config_log_file,
       no_config_log_file_cmd,
       "no log file [FILENAME]",
       NO_STR
       "Logging control\n"
       "Cancel logging to file\n"
       "Logging file name\n")
{
  zlog_reset_file (NULL);

  if (host.logfile)
    XFREE (MTYPE_HOST, host.logfile);

  host.logfile = NULL;

  return CMD_SUCCESS;
}

ALIAS_CALL (no_config_log_file,
       no_config_log_file_level_cmd,
       "no log file FILENAME LEVEL",
       NO_STR
       "Logging control\n"
       "Cancel logging to file\n"
       "Logging file name\n"
       "Logging level\n")

DEFUN_CALL (config_log_syslog,
       config_log_syslog_cmd,
       "log syslog",
       "Logging control\n"
       "Set syslog logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, zlog_get_default_lvl(NULL));
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_syslog_level,
       config_log_syslog_level_cmd,
       "log syslog "LOG_LEVELS,
       "Logging control\n"
       "Set syslog logging level\n"
       LOG_LEVEL_DESC)
{
  int level;

  if ((level = level_match(argv[0])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;
  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, level);
  return CMD_SUCCESS;
}

DEFUN_DEP_CALL (config_log_syslog_facility,
		  config_log_syslog_facility_cmd,
		  "log syslog facility "LOG_FACILITIES,
		  "Logging control\n"
		  "Logging goes to syslog\n"
		  "(Deprecated) Facility parameter for syslog messages\n"
		  LOG_FACILITY_DESC)
{
  int facility;

  if ((facility = facility_match(argv[0])) < 0)
    return CMD_ERR_NO_MATCH;

  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, zlog_get_default_lvl(NULL));
  zlog_set_facility(NULL, facility);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_syslog,
       no_config_log_syslog_cmd,
       "no log syslog [LEVEL]",
       NO_STR
       "Logging control\n"
       "Cancel logging to syslog\n"
       "Logging level\n")
{
  zlog_set_level (NULL, ZLOG_DEST_SYSLOG, ZLOG_DISABLED);
  return CMD_SUCCESS;
}

ALIAS_CALL (no_config_log_syslog,
       no_config_log_syslog_facility_cmd,
       "no log syslog facility "LOG_FACILITIES,
       NO_STR
       "Logging control\n"
       "Logging goes to syslog\n"
       "Facility parameter for syslog messages\n"
       LOG_FACILITY_DESC)

DEFUN_CALL (config_log_facility,
       config_log_facility_cmd,
       "log facility "LOG_FACILITIES,
       "Logging control\n"
       "Facility parameter for syslog messages\n"
       LOG_FACILITY_DESC)
{
  int facility;

  if ((facility = facility_match(argv[0])) < 0)
    return CMD_ERR_NO_MATCH;
  zlog_set_facility(NULL, facility);
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_facility,
       no_config_log_facility_cmd,
       "no log facility [FACILITY]",
       NO_STR
       "Logging control\n"
       "Reset syslog facility to default (daemon)\n"
       "Syslog facility\n")
{
  zlog_set_facility(NULL, LOG_DAEMON);
  return CMD_SUCCESS;
}

DEFUN_DEP_CALL (config_log_trap,
		  config_log_trap_cmd,
		  "log trap "LOG_LEVELS,
		  "Logging control\n"
		  "(Deprecated) Set logging level and default for all destinations\n"
		  LOG_LEVEL_DESC)
{
  int new_level ;

  if ((new_level = level_match(argv[0])) == ZLOG_DISABLED)
    return CMD_ERR_NO_MATCH;

  zlog_set_default_lvl_dest (NULL, new_level);
  return CMD_SUCCESS;
}

DEFUN_DEP_CALL (no_config_log_trap,
		  no_config_log_trap_cmd,
		  "no log trap [LEVEL]",
		  NO_STR
		  "Logging control\n"
		  "Permit all logging information\n"
		  "Logging level\n")
{
  zlog_set_default_lvl(NULL, LOG_DEBUG);
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_record_priority,
       config_log_record_priority_cmd,
       "log record-priority",
       "Logging control\n"
       "Log the priority of the message within the message\n")
{
  zlog_set_record_priority(NULL, 1) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_record_priority,
       no_config_log_record_priority_cmd,
       "no log record-priority",
       NO_STR
       "Logging control\n"
       "Do not log the priority of the message within the message\n")
{
  zlog_set_record_priority(NULL, 0) ;
  return CMD_SUCCESS;
}

DEFUN_CALL (config_log_timestamp_precision,
       config_log_timestamp_precision_cmd,
       "log timestamp precision <0-6>",
       "Logging control\n"
       "Timestamp configuration\n"
       "Set the timestamp precision\n"
       "Number of subsecond digits\n")
{
  int timestamp_precision;

  if (argc != 1)
    {
      vty_out (vty, "Insufficient arguments%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  VTY_GET_INTEGER_RANGE("Timestamp Precision",
      timestamp_precision, argv[0], 0, 6);
  zlog_set_timestamp_precision(NULL, timestamp_precision);

  return CMD_SUCCESS;
}

DEFUN_CALL (no_config_log_timestamp_precision,
       no_config_log_timestamp_precision_cmd,
       "no log timestamp precision",
       NO_STR
       "Logging control\n"
       "Timestamp configuration\n"
       "Reset the timestamp precision to the default value of 0\n")
{
  zlog_set_timestamp_precision(NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (banner_motd_file,
       banner_motd_file_cmd,
       "banner motd file [FILE]",
       "Set banner\n"
       "Banner for motd\n"
       "Banner from a file\n"
       "Filename\n")
{
  if (host.motdfile)
    XFREE (MTYPE_HOST, host.motdfile);

  host.motdfile = XSTRDUP (MTYPE_HOST, argv[0]);

  return CMD_SUCCESS;
}

DEFUN_CALL (banner_motd_default,
       banner_motd_default_cmd,
       "banner motd default",
       "Set banner string\n"
       "Strings for motd\n"
       "Default string\n")
{
  host.motd = DEFAULT_MOTD ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_banner_motd,
       no_banner_motd_cmd,
       "no banner motd",
       NO_STR
       "Set banner string\n"
       "Strings for motd\n")
{
  host.motd = NULL;
  if (host.motdfile)
    XFREE (MTYPE_HOST, host.motdfile);
  host.motdfile = NULL;
  return CMD_SUCCESS;
}

/* Set config filename.  Called from vty.c */
extern void
host_config_set (const char* file_name)
{
  if (host.config_file)
    XFREE (MTYPE_HOST, host.config_file);
  host.config_file = XSTRDUP (MTYPE_HOST, file_name);
}

void
install_default (enum node_type node)
{
  install_element (node, &config_exit_cmd);
  install_element (node, &config_quit_cmd);
  install_element (node, &config_end_cmd);
  install_element (node, &config_help_cmd);
  install_element (node, &config_list_cmd);

  install_element (node, &config_write_terminal_cmd);
  install_element (node, &config_write_file_cmd);
  install_element (node, &config_write_memory_cmd);
  install_element (node, &config_write_cmd);
  install_element (node, &show_running_config_cmd);
}

/* Initialize command interface. Install basic nodes and commands. */
void
cmd_init (int terminal)
{
  /* Allocate initial top vector of commands.                           */
  node_vector = vector_init(0);

  /* Default host value settings are already set, see above             */

  cmd_get_sys_host_name() ; /* start with system name & name_gen == 1   */

  /* Install top nodes. */
  install_node (&view_node, NULL);
  install_node (&enable_node, NULL);
  install_node (&auth_node, NULL);
  install_node (&auth_enable_node, NULL);
  install_node (&restricted_node, NULL);
  install_node (&config_node, config_write_host);

  /* Each node's basic commands. */
  install_element (VIEW_NODE, &show_version_cmd);
  if (terminal)
    {
      install_element (VIEW_NODE, &config_list_cmd);
      install_element (VIEW_NODE, &config_exit_cmd);
      install_element (VIEW_NODE, &config_quit_cmd);
      install_element (VIEW_NODE, &config_help_cmd);
      install_element (VIEW_NODE, &config_enable_cmd);
      install_element (VIEW_NODE, &config_terminal_length_cmd);
      install_element (VIEW_NODE, &config_terminal_no_length_cmd);
      install_element (VIEW_NODE, &show_logging_cmd);
      install_element (VIEW_NODE, &echo_cmd);

      install_element (RESTRICTED_NODE, &config_list_cmd);
      install_element (RESTRICTED_NODE, &config_exit_cmd);
      install_element (RESTRICTED_NODE, &config_quit_cmd);
      install_element (RESTRICTED_NODE, &config_help_cmd);
      install_element (RESTRICTED_NODE, &config_enable_cmd);
      install_element (RESTRICTED_NODE, &config_terminal_length_cmd);
      install_element (RESTRICTED_NODE, &config_terminal_no_length_cmd);
      install_element (RESTRICTED_NODE, &echo_cmd);
    }

  if (terminal)
    {
      install_default (ENABLE_NODE);
      install_element (ENABLE_NODE, &config_disable_cmd);
      install_element (ENABLE_NODE, &config_terminal_cmd);
      install_element (ENABLE_NODE, &copy_runningconfig_startupconfig_cmd);
    }
  install_element (ENABLE_NODE, &show_startup_config_cmd);
  install_element (ENABLE_NODE, &show_version_cmd);

  if (terminal)
    {
      install_element (ENABLE_NODE, &config_terminal_length_cmd);
      install_element (ENABLE_NODE, &config_terminal_no_length_cmd);
      install_element (ENABLE_NODE, &show_logging_cmd);
      install_element (ENABLE_NODE, &echo_cmd);
      install_element (ENABLE_NODE, &config_logmsg_cmd);

      install_default (CONFIG_NODE);
    }

  install_element (CONFIG_NODE, &hostname_cmd);
  install_element (CONFIG_NODE, &no_hostname_cmd);

  if (terminal)
    {
      install_element (CONFIG_NODE, &password_cmd);
      install_element (CONFIG_NODE, &password_text_cmd);
      install_element (CONFIG_NODE, &enable_password_cmd);
      install_element (CONFIG_NODE, &enable_password_text_cmd);
      install_element (CONFIG_NODE, &no_enable_password_cmd);

      install_element (CONFIG_NODE, &config_log_stdout_cmd);
      install_element (CONFIG_NODE, &config_log_stdout_level_cmd);
      install_element (CONFIG_NODE, &no_config_log_stdout_cmd);
      install_element (CONFIG_NODE, &config_log_monitor_cmd);
      install_element (CONFIG_NODE, &config_log_monitor_level_cmd);
      install_element (CONFIG_NODE, &no_config_log_monitor_cmd);
      install_element (CONFIG_NODE, &config_log_file_cmd);
      install_element (CONFIG_NODE, &config_log_file_level_cmd);
      install_element (CONFIG_NODE, &no_config_log_file_cmd);
      install_element (CONFIG_NODE, &no_config_log_file_level_cmd);
      install_element (CONFIG_NODE, &config_log_syslog_cmd);
      install_element (CONFIG_NODE, &config_log_syslog_level_cmd);
      install_element (CONFIG_NODE, &config_log_syslog_facility_cmd);
      install_element (CONFIG_NODE, &no_config_log_syslog_cmd);
      install_element (CONFIG_NODE, &no_config_log_syslog_facility_cmd);
      install_element (CONFIG_NODE, &config_log_facility_cmd);
      install_element (CONFIG_NODE, &no_config_log_facility_cmd);
      install_element (CONFIG_NODE, &config_log_trap_cmd);
      install_element (CONFIG_NODE, &no_config_log_trap_cmd);
      install_element (CONFIG_NODE, &config_log_record_priority_cmd);
      install_element (CONFIG_NODE, &no_config_log_record_priority_cmd);
      install_element (CONFIG_NODE, &config_log_timestamp_precision_cmd);
      install_element (CONFIG_NODE, &no_config_log_timestamp_precision_cmd);
      install_element (CONFIG_NODE, &service_password_encrypt_cmd);
      install_element (CONFIG_NODE, &no_service_password_encrypt_cmd);
      install_element (CONFIG_NODE, &banner_motd_default_cmd);
      install_element (CONFIG_NODE, &banner_motd_file_cmd);
      install_element (CONFIG_NODE, &no_banner_motd_cmd);
      install_element (CONFIG_NODE, &service_terminal_length_cmd);
      install_element (CONFIG_NODE, &no_service_terminal_length_cmd);

      install_element (VIEW_NODE, &show_thread_cpu_cmd);
      install_element (ENABLE_NODE, &show_thread_cpu_cmd);
      install_element (RESTRICTED_NODE, &show_thread_cpu_cmd);
      install_element (VIEW_NODE, &show_work_queues_cmd);
      install_element (ENABLE_NODE, &show_work_queues_cmd);
    }
  srand(time(NULL));
}

void
cmd_terminate ()
{
  cmd_node     cmd_node;
  cmd_command  cmd ;

  while ((cmd_node = vector_ream(node_vector, free_it)) != NULL)
    {
      while ((cmd = vector_ream(cmd_node->cmd_vector, free_it)) != NULL)
        {
          /* Note that each cmd is a static structure, which may appear in
           * more than one cmd_vector.
           */
          cmd_item next_item ;

          while ((next_item = vector_ream(cmd->items, free_it)) != NULL)
            {
              do
                {
                  cmd_item item ;
                  item      = next_item ;
                  next_item = item->next ;
                  XFREE(MTYPE_CMD_ITEM, item);
                }
              while (next_item != NULL) ;
            } ;

          cmd->items = NULL ;                           /* gone         */

          XFREE (MTYPE_CMD_STRING, cmd->r_string) ;     /* sets NULL    */
          XFREE (MTYPE_CMD_STRING, cmd->r_doc) ;
        } ;
    }

  node_vector = NULL ;

  XFREE(MTYPE_HOST, host.name);
  XFREE(MTYPE_HOST, host.password);
  XFREE(MTYPE_HOST, host.enable);
  XFREE(MTYPE_HOST, host.logfile);
  XFREE(MTYPE_HOST, host.motdfile);
  XFREE(MTYPE_HOST, host.config_file);
  XFREE(MTYPE_HOST, host.vty_accesslist_name);
  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
  XFREE(MTYPE_HOST, host.vty_cwd);
} ;

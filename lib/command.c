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

#include "misc.h"
#include "version.h"

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
#include "network.h"

/* Vector of cmd_node, one for each known node, built during daemon
 * initialisation.
 *
 * Declared extern in command_local.h, so it can get at it.
 */
vector node_vector = NULL ;

/*==============================================================================
 * Default motd string and debug hello message.
 */
static const char* default_motd =
"\n"
"Hello, this is " QUAGGA_PROGNAME " (version " QUAGGA_VERSION ")\n"
 QUAGGA_COPYRIGHT "\n"
"\n" ;

const char* debug_banner =
    QUAGGA_PROGNAME " version " QUAGGA_VERSION " QDEBUG=" QDEBUG_NAME " "
    __DATE__ " " __TIME__ ;

/*==============================================================================
 * Host information structure -- shared across command/vty
 *
 * Must have VTY_LOCK() or not be running multiple pthreads to access this !
 */
struct host host =
{
    /* Host name of this router.                                */
    .name              = NULL,  /* set by cmd_init      */
    .name_set          = false,
    .name_gen          = 0,     /* set by cmd_init      */

    /* Password for vty interface.                              */
    .password          = NULL,
    .password_encrypted = false,

    /* Enable password                                          */
    .enable            = NULL,
    .enable_encrypted  = false,

    /* System wide terminal lines.                              */
    .lines             = -1,    /* unset                */

    /* Log filename.                                            */
    .logfile           = NULL,

    /* config file name of this host                            */
    .config_file       = NULL,
    .config_dir        = NULL,

    /* Flags for services                                       */
    .advanced          = false,
    .encrypt           = false,

    /* Banner configuration.                                    */
    .motd              = NULL,
    .motdfile          = NULL,

    /* Nobody has the config symbol of power                    */
    .config            = false,
    .config_brand      = 0,

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

    /* Current directory -- initialised in cmd_getcwd()         */
    .cwd                = NULL,
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
  VTY_ASSERT_LOCKED() ;

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
  .exit_to      = NULL_NODE,    /* close !              */
  .end_to       = AUTH_NODE,    /* self => no end       */
};

static struct cmd_node view_node =
{
  .node         = VIEW_NODE,
  .prompt       = "%s> ",

  .parent       = VIEW_NODE,    /* self => no parent    */
  .exit_to      = NULL_NODE,    /* close !              */
  .end_to       = VIEW_NODE,    /* self => no end       */
};

static struct cmd_node restricted_node =
{
  .node         = RESTRICTED_NODE,
  .prompt       = "%s$ ",

  .parent       = RESTRICTED_NODE,  /* self => no parent    */
  .exit_to      = NULL_NODE,        /* close !              */
  .end_to       = RESTRICTED_NODE,  /* self => no end       */
};

static struct cmd_node auth_enable_node =
{
  .node         = AUTH_ENABLE_NODE,
  .prompt       = "Password: ",

  .parent       = AUTH_ENABLE_NODE, /* self => no parent    */
  .exit_to      = VIEW_NODE,        /* fall back            */
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

  .parent       = ENABLE_NODE,  /* more or less                 */
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
 *   * all nodes >  NODE_CONFIG have NODE_CONFIG as parent
 *   *     node  == NODE_CONFIG has  ENABLE_NODE as parent
 *   * all nodes <  NODE_CONFIG are their own parents (including ENABLE_NODE)
 *
 * Default exit_to:
 *
 *   * all nodes >  NODE_CONFIG exit_to their parent
 *   *     node  == NODE_CONFIG exit_to ENABLE_NODE (its parent)
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
      if      (node->node >  CONFIG_NODE)
        node->parent = CONFIG_NODE ;
      else if (node->node == CONFIG_NODE)
        node->parent = ENABLE_NODE ;
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
      if (node->node >= CONFIG_NODE)
        node->exit_to = node->parent ;
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
 * Return end_to node
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
      struct cmd_node *cn;
      vector cmd_vector ;

      cn = vector_get_item(node_vector, i) ;

      if (cn == NULL)
        continue ;

      cmd_vector = cn->cmd_vector;
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
  return cmd_tokenize(NULL, string) ;
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
  struct cmd_node *cn ;

  assert(node_vector != NULL) ;
  assert(node_vector->p_items != NULL) ;

  cn = NULL ;
  if (node < node_vector->limit)
    cn = vector_get_item (node_vector, node);

  if (cn == NULL)
    {
      zlog_err("Could not find prompt for node %d for", node) ;
      return NULL ;
    } ;

  return cn->prompt;
}

/*------------------------------------------------------------------------------
 * Install a command into a node.
 *
 */
extern void
install_element(node_type_t ntype, cmd_command cmd)
{
  cmd_node cn ;

  cn = vector_get_item (node_vector, ntype);

  if (cn == NULL)
    {
      fprintf (stderr, "Command node %d doesn't exist, please check it\n",
	       ntype);
      exit (1);
    }

  vector_set (cn->cmd_vector, cmd);

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
config_write_host (vty vty)
{
  vty_io vio ;

  VTY_LOCK() ;

  vio = vty->vio ;

  if (qpthreads_enabled)
    uty_out (vio, "threaded\n");

  if (host.name_set)
    uty_out (vio, "hostname %s\n", host.name);

  if (host.password != NULL)
    {
      if (host.password_encrypted)
        uty_out (vio, "password 8 %s\n", host.password);
      else
        uty_out (vio, "password %s\n", host.password);
    } ;

  if (host.enable != NULL)
    {
      if (host.enable_encrypted)
        uty_out (vio, "enable password 8 %s\n", host.enable);
      else
        uty_out (vio, "enable password %s\n", host.enable);
    } ;

  if (zlog_get_default_lvl(NULL) != LOG_DEBUG)
    {
      uty_out (vio, "! N.B. The 'log trap' command is deprecated.\n");
      uty_out (vio, "log trap %s\n", zlog_priority[zlog_get_default_lvl(NULL)]);
    }

  if (host.logfile && (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != ZLOG_DISABLED))
    {
      uty_out (vio, "log file %s", qpath_string(host.logfile));
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_FILE) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_FILE)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != ZLOG_DISABLED)
    {
      uty_out (vio, "log stdout");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_STDOUT)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) == ZLOG_DISABLED)
    uty_out(vio,"no log monitor%s",VTY_NEWLINE);
  else if (zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR) != zlog_get_default_lvl(NULL))
    uty_out(vio,"log monitor %s\n",
	    zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_MONITOR)]);

  if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != ZLOG_DISABLED)
    {
      uty_out (vio, "log syslog");
      if (zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG) != zlog_get_default_lvl(NULL))
	uty_out (vio, " %s",
		 zlog_priority[zlog_get_maxlvl(NULL, ZLOG_DEST_SYSLOG)]);
      uty_out (vio, "\n");
    }

  if (zlog_get_facility(NULL) != LOG_DAEMON)
    uty_out (vio, "log facility %s\n", facility_name(zlog_get_facility(NULL)));

  if (zlog_get_record_priority(NULL) == 1)
    uty_out (vio, "log record-priority\n");

  if (zlog_get_timestamp_precision(NULL) > 0)
    uty_out (vio, "log timestamp precision %d\n",
                                            zlog_get_timestamp_precision(NULL));

  if (host.advanced)
    uty_out (vio, "service advanced-vty\n");

  if (host.encrypt)
    uty_out (vio, "service password-encryption\n");

  if (host.lines >= 0)
    uty_out (vio, "service terminal-length %d\n", host.lines);

  if      (host.motdfile)
    uty_out (vio, "banner motd file %s\n", qpath_string(host.motdfile));
  else if (! host.motd)
    uty_out (vio, "no banner motd\n");

  VTY_UNLOCK() ;

  return 1;
}

/*==============================================================================
 * Commands and other stuff related to:
 *
 *   * end (and ^Z)   -- go to ENABLE_NODE (aka Privileged Exec) if above that,
 *                       otherwise do nothing.
 *
 *                       This is installed in all nodes.
 *
 *   * exit           -- go to parent node, if in CONFIG_NODE or any of its
 *                       sub-nodes.
 *
 *                       Parent of CONFIG_NODE is ENABLE_NODE.
 *
 *                       For all other nodes, this is EOF (which for the
 *                       terminal is "close").
 *
 *                       This is installed in all nodes.
 *
 *   * enable         -- go to ENABLE_NODE, if can.
 *
 *                       This is installed in VIEW_NODE and RESTRICTED_NODE.
 *
 *                       It is also installed in ENABLE_NODE (and hence is
 *                       available anywhere), where it is a synonym for 'end' !
 *
 *                       For configuration reading (VTY_CONFIG_READ) and for
 *                       VTY_SHELL_SERVER, no password is required.
 *
 *                       For VTY_TERMINAL, must already have authenticated
 *                       once, or must be able to enter AUTH_ENABLE_NODE.
 *
 *   * disable        -- go to VIEW_NODE (aka User Exec).
 *
 *                       This is installed in ENABLE_NODE *only*.
 *
 *                       Note, however, that all ENABLE_NODE commands are
 *                       available at ENABLE_NODE and above !
 */

/*------------------------------------------------------------------------------
 * Enter CONFIG_NODE, possibly via password check.
 *
 * If the parser established that can enter CONFIG_NODE directly, that's what
 * happens.
 *
 * If the parser established that must authenticate, then may fail here if
 * we are not in the right state to run the authentication.
 *
 * The authentication itself may fail...
 *
 * NB: installed in VIEW_NODE, RESTRICTED_NODE and ENABLE_NODE.
 */
DEFUN_ATTR (config_terminal,
            config_terminal_cmd,
            "configure terminal",
            "Configuration from vty interface\n"
            "Configuration terminal\n",
            CMD_ATTR_DIRECT + cmd_sp_configure)
{
  if (vty->exec->parsed->nnode == CONFIG_NODE)
    return vty_cmd_config_lock(vty) ; ;

  /* Otherwise, must authenticate to enter CONFIG_NODE.                 */
  return vty_cmd_can_auth_enable(vty) ;
} ;

ALIAS_ATTR (config_terminal,
            config_enable_configure_cmd,
            "enable configure",
            "Turn on privileged mode command\n"
            "Configuration terminal\n",
            CMD_ATTR_DIRECT + cmd_sp_configure)

/*------------------------------------------------------------------------------
 * Enter ENABLE_NODE, possibly via password check.
 *
 * If the parser established that can enter ENABLE_NODE directly, that's what
 * happens.
 *
 * If the parser established that must authenticate, then may fail here if
 * we are not in the right state to run the authentication.
 *
 * The authentication itself may fail...
 *
 * NB: installed in VIEW_NODE, RESTRICTED_NODE and ENABLE_NODE.
 */
DEFUN_ATTR (enable,
            config_enable_cmd,
            "enable",
            "Turn on privileged mode command\n",
            CMD_ATTR_DIRECT + cmd_sp_enable)
{
  if (vty->exec->parsed->nnode == ENABLE_NODE)
    return CMD_SUCCESS ;

  /* Otherwise, must authenticate to enter ENABLE_NODE.                 */
  return vty_cmd_can_auth_enable(vty) ;
} ;

/*------------------------------------------------------------------------------
 * disable command: end enabled state -> VIEW_NODE.
 *
 * NB: although only installed in ENABLE_NODE, it will be implicitly available
 *     in all higher nodes -- as a quick way of crashing out to VIEW_NODE !
 */
DEFUN_ATTR(disable,
           config_disable_cmd,
           "disable",
           "Turn off privileged mode command\n",
           CMD_ATTR_DIRECT + CMD_ATTR_NODE + VIEW_NODE)
{
  return CMD_SUCCESS ;  /* will disable to parsed->nnode        */
}

/*------------------------------------------------------------------------------
 * exit command: down one node level, including exit command processor.
 */
DEFUN_ATTR(config_exit,
           config_exit_cmd,
           "exit",
           "Exit current mode and down to previous mode\n",
           CMD_ATTR_DIRECT + cmd_sp_exit)
{
  return CMD_SUCCESS ;  /* will exit to parsed->nnode   */
}

/* quit is alias of exit.                                               */
ALIAS_ATTR (config_exit,
            config_quit_cmd,
            "quit",
            "Exit current mode and down to previous mode\n",
            CMD_ATTR_DIRECT + cmd_sp_exit) ;

/*------------------------------------------------------------------------------
 * end command: down to enable mode.
 */
DEFUN_ATTR (config_end,
            config_end_cmd,
            "end",
            "End current mode and change to enable mode\n",
            CMD_ATTR_DIRECT + cmd_sp_end)
{
  return CMD_SUCCESS ;  /* will end to parsed->nnode    */
}

/* Show version.                                                        */
DEFUN_CALL (show_version,
            show_version_cmd,
            "show version",
            SHOW_STR
            "Displays zebra version\n")
{
  VTY_LOCK() ;

  uty_out (vty->vio, "Quagga %s (%s).\n", QUAGGA_VERSION,
                                        (host.name != NULL) ? host.name : "") ;
  uty_out (vty->vio, "%s\n", QUAGGA_COPYRIGHT);

  VTY_UNLOCK() ;

  return CMD_SUCCESS;
}

/* Help display function for all node.                                  */
DEFUN_CALL (config_help,
       config_help_cmd,
       "help",
       "Description of the interactive help system\n")
{
  vty_out (vty,
      "Quagga VTY provides advanced help feature.  When you need help,\n"
      "anytime at the command line please press '?'.\n"
      "\n"
      "If nothing matches, the help list will be empty and you must backup\n"
      "until entering a '?' shows the available options.\n"
      "Two styles of help are provided:\n"
      "  1. Full help is available when you are ready to enter a\n"
      "     command argument (e.g. 'show ?') and describes each possible\n"
      "     argument.\n"
      "  2. Partial help is provided when an abbreviated argument is entered\n"
      "     and you want to know what arguments match the input\n"
      "     (e.g. 'show me?'.)\n"
      "\n") ;
  return CMD_SUCCESS;
}

/* Help display function for all node.                                  */
DEFUN_CALL (config_list,
            config_list_cmd,
            "list",
            "Print command list\n")
{
  unsigned int i;
  struct cmd_node *cn ;
  struct cmd_command *cmd;

  cn = vector_get_item (node_vector, vty->node);

  for (i = 0; i < vector_length (cn->cmd_vector); i++)
    if ( ((cmd = vector_get_item (cn->cmd_vector, i)) != NULL)
            && ((cmd->attr & (CMD_ATTR_DEPRECATED | CMD_ATTR_HIDDEN)) == 0) )
      vty_out (vty, "  %s\n", cmd->string);

  return CMD_SUCCESS;
}

/* Write current configuration into file.                               */
DEFUN (config_write_file,
       config_write_file_cmd,
       "write file",
       "Write running configuration to memory, network, or terminal\n"
       "Write to configuration file\n")
{
  qpath  path ;
  qpath  temp ;
  qpath  save ;
  cmd_return_code_t ret, retw ;
  unsigned int i ;
  int fd, err ;
  struct cmd_node *cn;
  const char *config_name ;
  const char *save_name ;
  char* temp_name ;

  err = 0 ;     /* so far, so good      */

  VTY_LOCK() ;
  path = (host.config_file != NULL) ? qpath_dup(host.config_file) : NULL ;
  VTY_UNLOCK() ;

  /* Check and see if we are operating under vtysh configuration        */
  if (path == NULL)
    {
      vty_out (vty, "%% Cannot save to configuration file, using vtysh.\n");
      return CMD_WARNING;
    }

  /* Set up the file names.                                             */
  config_name = qpath_string(path) ;

  save = qpath_dup(path) ;
  qpath_extend_str(save, CONF_BACKUP_EXT) ;
  save_name = qpath_string(save) ;

  temp = qpath_dup(path) ;
  qpath_extend_str(temp, ".XXXXXX") ;
  temp_name = qpath_char_string(temp) ;

  /* Open file to configuration write.                                  */
  fd = mkstemp (temp_name);
  if (fd < 0)
    {
      err = errno ;
      vty_out (vty, "%% Can't open configuration file %s", temp_name) ;
      goto finished;
    }

  /* Make vty for configuration file.                                   */
  vty_open_config_write(vty, fd) ;

  vty_out (vty, "!\n! Zebra configuration saved from vty\n!   ");
  vty_time_print (vty, 1);
  vty_out (vty, "!\n");

  retw = CMD_SUCCESS ;

  for (i = 0; i < vector_length (node_vector); i++)
    {
      if ((cn = vector_get_item (node_vector, i)) && cn->config_write)
        {
          if ((*cn->config_write) (vty))
            vty_out (vty, "!\n");

          retw = vty_cmd_out_push(vty) ; /* Push stuff so far    */

          if (retw != CMD_SUCCESS)
            break ;
        } ;
    } ;

  ret = vty_close_config_write(vty, (retw != CMD_SUCCESS)) ;

  if ((ret != CMD_SUCCESS) || (retw != CMD_SUCCESS))
    {
      vty_out (vty, "%% Failed while writing configuration file %s.\n",
                                                                    temp_name) ;
      goto finished;
    }

  /* Now move files around to make .sav and the real file               */
  ret = CMD_WARNING ;

  if (unlink (save_name) != 0)
    if (errno != ENOENT)
      {
        err = errno ;
        vty_out (vty, "%% Can't unlink backup configuration file %s",
                                                                    save_name) ;
        goto finished;
      } ;

  if (link (config_name, save_name) != 0)
    {
      err = errno ;
      vty_out (vty, "%% Can't backup old configuration file %s", config_name) ;
      goto finished;
    } ;

  sync () ;

  if (unlink (config_name) != 0)
    {
      err = errno ;
      vty_out (vty, "%% Can't unlink configuration file %s", config_name);
      goto finished;
    } ;

  if (link (temp_name, config_name) != 0)
    {
      err = errno ;
      vty_out (vty, "%% Can't save configuration file %s", config_name);
      goto finished;
    } ;

  sync ();

  if (chmod (config_name, CONFIGFILE_MASK) != 0)
    {
      err = errno ;
      vty_out (vty, "%% Can't chmod configuration file %s", config_name) ;
      goto finished;
    }

  vty_out (vty, "Configuration saved to %s\n", config_name) ;

  ret = CMD_SUCCESS;

finished:
  if (err != 0)
    vty_out(vty, ": %s (%s).\n", errtostr(errno, 0).str,
                                 errtoname(errno, 0).str) ;
  if (fd >= 0)
    unlink (temp_name);

  qpath_free(temp) ;
  qpath_free(save) ;
  qpath_free(path) ;

  return ret;
} ;

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

/* Write current configuration into the terminal.                       */
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

/* Write current configuration into the terminal.                       */
ALIAS (config_write_terminal,
       show_running_config_cmd,
       "show running-config",
       SHOW_STR
       "running configuration\n")

/* Write startup configuration into the terminal.                       */
DEFUN (show_startup_config,
       show_startup_config_cmd,
       "show startup-config",
       SHOW_STR
       "Contents of startup configuration\n")
{
  cmd_return_code_t ret ;
  qpath  path ;

  VTY_LOCK() ;
  path = (host.config_file != NULL) ? qpath_dup(host.config_file) : NULL ;
  VTY_UNLOCK() ;

  if (path == NULL)
    {
      vty_out (vty, "%% Cannot show configuration file, using vtysh.\n");
      return CMD_WARNING;
    } ;

  ret = vty_cat_file(vty, path, "configuration file") ;
  qpath_free(path) ;

  return ret ;
}

/* Hostname configuration                                               */
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

/*==============================================================================
 * Logging configuration.
 */

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

/*------------------------------------------------------------------------------
 * Set new logging file and level -- "log file FILENAME [LEVEL]"
 *
 * Note that even if fail to open the new log file, will set host.logfile.
 *
 * Failure here is an error.
 */
static int
set_log_file(struct vty *vty, const char *fname, int loglevel)
{
  int    err ;

  VTY_LOCK() ;

  host.logfile = uty_cmd_path_name_complete(host.logfile, fname,
                                                           vty->exec->context) ;
  err  = zlog_set_file (NULL, qpath_string(host.logfile), loglevel) ;

  VTY_UNLOCK() ;

  if (err == 0)
    return CMD_SUCCESS ;

  vty_out(vty, "%% failed to open log file %s: %s (%s)\n",
                                                  qpath_string(host.logfile),
                                                        errtostr(err, 0).str,
                                                        errtoname(err, 0).str) ;
  return CMD_WARNING ;
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
       "log file FILENAME " LOG_LEVELS,
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
  VTY_LOCK() ;

  zlog_reset_file (NULL);

  host.logfile = qpath_free(host.logfile) ;

  VTY_UNLOCK() ;
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

/*==============================================================================
 * MOTD commands and set up.
 *
 * Note that can set a MOTD file that does not exist at the time.  A friendly
 * message warns about this, but it is not an error.  The message will not be
 * seen while reading the configuration file -- but it is not worth stopping
 * the configuration file reader for this !
 */
DEFUN_CALL (banner_motd_file,
       banner_motd_file_cmd,
       "banner motd file FILE",
       "Set banner\n"
       "Banner for motd\n"
       "Banner from a file\n"
       "Filename\n")
{
  int    err ;

  VTY_LOCK() ;

  host.motdfile = uty_cmd_path_name_complete(host.motdfile,
                                                  argv[0], vty->exec->context) ;
  err  = qpath_stat_is_file(host.motdfile) ;

  if (err != 0)
    {
      vty_out(vty, "NB: '%s': ", qpath_string(host.motdfile)) ;
      if (err < 0)
        vty_out(vty, "is not a file\n") ;
      else
        vty_out(vty, "%s (%s)\n", errtostr(err, 0).str,
                                  errtoname(err, 0).str) ;
    } ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS ;
} ;

DEFUN_CALL (banner_motd_default,
       banner_motd_default_cmd,
       "banner motd default",
       "Set banner string\n"
       "Strings for motd\n"
       "Default string\n")
{
  VTY_LOCK() ;

  host.motd = default_motd ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

DEFUN_CALL (no_banner_motd,
       no_banner_motd_cmd,
       "no banner motd",
       NO_STR
       "Set banner string\n"
       "Strings for motd\n")
{
  VTY_LOCK() ;

  host.motd     = NULL ;
  host.motdfile = qpath_free(host.motdfile) ;

  VTY_UNLOCK() ;
  return CMD_SUCCESS;
}

/*==============================================================================
 * Current directory handling
 */
DEFUN_CALL (do_chdir,
       chdir_cmd,
       "chdir DIR",
       "Set current directory\n"
       "Directory to set\n")
{
  cmd_return_code_t ret ;
  qpath  path ;
  int    err ;

  ret = CMD_SUCCESS ;

  path = uty_cmd_path_name_complete(NULL, argv[0], vty->exec->context) ;
  err  = qpath_stat_is_directory(path) ;

  if (err == 0)
    qpath_copy(vty->exec->context->dir_cd, path) ;
  else
    {
      vty_out(vty, "%% chdir %s: ", qpath_string(path)) ;
      if (err < 0)
        vty_out(vty, "is not a directory\n") ;
      else
        vty_out(vty, "%s (%s)\n", errtostr(err, 0).str,
                                  errtoname(err, 0).str) ;
      ret = CMD_WARNING ;
    } ;

  qpath_free(path) ;
  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Get cwd.
 *
 * This is done very early in the morning, before lowering privileges, to
 * minimise chance of not being able to get the cwd.  If cwd itself is not
 * accessible in lowered privilege state, that will later become clear.
 *
 * Sets host.cwd, which is torn down in cmd_terminate().
 *
 */
extern void
cmd_getcwd(void)
{
  host.cwd = qpath_getcwd(NULL) ;

  if (host.cwd == NULL)
    {
      fprintf(stderr, "Cannot getcwd()\n") ;
      exit(1) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set host.config_file and host.config_dir.
 */
extern void
cmd_host_config_set (qpath config_file)
{
  VTY_LOCK() ;

  host.config_file = qpath_copy(host.config_file, config_file) ;
  host.config_dir  = qpath_copy(host.config_dir,  config_file) ;

  qpath_shave(host.config_dir) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Set the lexical level for further command processing.
 */
DEFUN_CALL (lexical_level,
       lexical_level_cmd,
       "lexical-level <0-1>",
       "Set lexical level\n"
       "The required lexical level\n")
{
  int level ;

  level = strtol(argv[0], NULL, 0) ;

  vty_cmd_set_full_lex(vty, (level != 0)) ;

  return CMD_SUCCESS;
}

/*==============================================================================
 * Command handling initialisation and termination.
 */

/*------------------------------------------------------------------------------
 * Install copy of the default commands in the given node.
 */
extern void
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

/*------------------------------------------------------------------------------
 * Initialise command handling.
 *
 * Install basic nodes and commands.  Initialise the host structure.
 *
 * Sets srand(time(NULL))
 */
extern void
cmd_init (bool terminal)
{
  srand(time(NULL)) ;

  if (host.cwd == NULL)         /* in case cmd_cwd() not called, yet    */
    cmd_getcwd() ;

  /* Allocate initial top vector of commands.                           */
  node_vector = vector_init(0);

  /* Set default motd                                                   */
  host.motd         = default_motd ;
  host.config_brand = rand() ;

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
      install_element (VIEW_NODE, &config_enable_configure_cmd);
      install_element (VIEW_NODE, &config_terminal_cmd);
      install_element (VIEW_NODE, &config_terminal_length_cmd);
      install_element (VIEW_NODE, &config_terminal_no_length_cmd);
      install_element (VIEW_NODE, &show_logging_cmd);
      install_element (VIEW_NODE, &echo_cmd);
      install_element (VIEW_NODE, &chdir_cmd);

      install_element (RESTRICTED_NODE, &config_list_cmd);
      install_element (RESTRICTED_NODE, &config_exit_cmd);
      install_element (RESTRICTED_NODE, &config_quit_cmd);
      install_element (RESTRICTED_NODE, &config_help_cmd);
      install_element (RESTRICTED_NODE, &config_enable_cmd);
      install_element (RESTRICTED_NODE, &config_enable_configure_cmd);
      install_element (RESTRICTED_NODE, &config_terminal_cmd);
      install_element (RESTRICTED_NODE, &config_terminal_length_cmd);
      install_element (RESTRICTED_NODE, &config_terminal_no_length_cmd);
      install_element (RESTRICTED_NODE, &echo_cmd);
      install_element (RESTRICTED_NODE, &chdir_cmd);
    }

  if (terminal)
    {
      install_default (ENABLE_NODE);
      install_element (ENABLE_NODE, &config_disable_cmd);
      install_element (ENABLE_NODE, &config_enable_cmd);
      install_element (ENABLE_NODE, &config_enable_configure_cmd);
      install_element (ENABLE_NODE, &config_terminal_cmd);
      install_element (ENABLE_NODE, &copy_runningconfig_startupconfig_cmd);
    }

  install_element (ENABLE_NODE, &show_startup_config_cmd);
  install_element (ENABLE_NODE, &show_version_cmd);
  install_element (ENABLE_NODE, &lexical_level_cmd);
  install_element (ENABLE_NODE, &chdir_cmd);

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
  install_element (CONFIG_NODE, &lexical_level_cmd);

  if (terminal)
    {
      install_element (CONFIG_NODE, &echo_cmd);

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

      install_element (RESTRICTED_NODE, &show_thread_cpu_cmd);
      install_element (VIEW_NODE, &show_thread_cpu_cmd);
      install_element (ENABLE_NODE, &show_thread_cpu_cmd);

      install_element (ENABLE_NODE, &clear_thread_cpu_cmd);
      install_element (VIEW_NODE, &show_work_queues_cmd);
      install_element (ENABLE_NODE, &show_work_queues_cmd);
    } ;
} ;

/*------------------------------------------------------------------------------
 * Close down command interface.
 *
 * Dismantle the node_vector and all commands.
 *
 * Clear out the host structure.
 */
void
cmd_terminate ()
{
  cmd_node     cmd_node;
  cmd_command  cmd ;

  /* Ream out the vector of command nodes.                              */
  while ((cmd_node = vector_ream(node_vector, free_it)) != NULL)
    {
      /* Ream out the (embedded) vector of commands per node.           */
      while ((cmd = vector_ream(cmd_node->cmd_vector, keep_it)) != NULL)
        {
          /* Ream out the vector of items for each command.
           *
           * Note that each cmd is a static structure, which may appear in
           * more than one cmd_vector -- but the "compiled" portions are
           * dynamically allocated.
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
  host.logfile     = qpath_free(host.logfile) ;
  host.motdfile    = qpath_free(host.motdfile) ;
  host.config_file = qpath_free(host.config_file) ;
  host.config_dir  = qpath_free(host.config_dir) ;
  XFREE(MTYPE_HOST, host.vty_accesslist_name);
  XFREE(MTYPE_HOST, host.vty_ipv6_accesslist_name);
  host.cwd        = qpath_free(host.cwd) ;
} ;

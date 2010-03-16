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

#include <zebra.h>


#include "memory.h"
#include "log.h"
#include <lib/version.h>
#include "thread.h"
#include "vector.h"
#include "vty.h"
#include "uty.h"
#include "qstring.h"
#include "command.h"
#include "workqueue.h"
#include "command_queue.h"

/* Command vector which includes some level of command lists. Normally
   each daemon maintains each own cmdvec. */
vector cmdvec = NULL;

struct desc desc_cr;
char *command_cr = NULL;

/* Host information structure. */
struct host host;

/* Standard command node structures. */
static struct cmd_node auth_node =
{
  AUTH_NODE,
  "Password: ",
};

static struct cmd_node view_node =
{
  VIEW_NODE,
  "%s> ",
};

static struct cmd_node restricted_node =
{
  RESTRICTED_NODE,
  "%s$ ",
};

static struct cmd_node auth_enable_node =
{
  AUTH_ENABLE_NODE,
  "Password: ",
};

static struct cmd_node enable_node =
{
  ENABLE_NODE,
  "%s# ",
};

static struct cmd_node config_node =
{
  CONFIG_NODE,
  "%s(config)# ",
  1
};

/* Default motd string. */
const char *default_motd =
"\r\n\
Hello, this is " QUAGGA_PROGNAME " (version " QUAGGA_VERSION ").\r\n\
" QUAGGA_COPYRIGHT "\r\n\
\r\n";

#ifdef QDEBUG
const char *debug_banner =
    QUAGGA_PROGNAME " version " QUAGGA_VERSION " QDEBUG=" QDEBUG " "
    __DATE__ " " __TIME__;
#endif

static struct facility_map {
  int facility;
  const char *name;
  size_t match;
} syslog_facilities[] =
  {
    { LOG_KERN, "kern", 1 },
    { LOG_USER, "user", 2 },
    { LOG_MAIL, "mail", 1 },
    { LOG_DAEMON, "daemon", 1 },
    { LOG_AUTH, "auth", 1 },
    { LOG_SYSLOG, "syslog", 1 },
    { LOG_LPR, "lpr", 2 },
    { LOG_NEWS, "news", 1 },
    { LOG_UUCP, "uucp", 2 },
    { LOG_CRON, "cron", 1 },
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

static const char *
facility_name(int facility)
{
  struct facility_map *fm;

  for (fm = syslog_facilities; fm->name; fm++)
    if (fm->facility == facility)
      return fm->name;
  return "";
}

static int
facility_match(const char *str)
{
  struct facility_map *fm;

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


/* Utility function to concatenate argv argument into a single string
   with inserting ' ' character between each argument.  */
char *
argv_concat (const char **argv, int argc, int shift)
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

/* Install top node of command vector. */
void
install_node (struct cmd_node *node,
	      int (*func) (struct vty *))
{
  vector_set_index (cmdvec, node->node, node);
  node->func = func;
  node->cmd_vector = vector_init (0);
}

/* Compare two command's string.  Used in sort_node (). */
static int
cmp_node (const struct cmd_element *a, const struct cmd_element *b)
{
  return strcmp (a->string, b->string);
}

static int
cmp_desc (const struct desc *a, const struct desc *b)
{
  return strcmp (a->cmd, b->cmd);
}

/* Sort each node's command element according to command string. */
void
sort_node ()
{
  unsigned int i, j;
  struct cmd_node *cnode;
  struct cmd_element *cmd_element;

  for (i = 0; i < vector_active (cmdvec); i++)
    if ((cnode = vector_slot (cmdvec, i)) != NULL)
      {
	vector cmd_vector = cnode->cmd_vector;
	vector_sort(cmd_vector, (vector_sort_cmp*)cmp_node) ;

	for (j = 0; j < vector_active (cmd_vector); j++)
	  if ((cmd_element = vector_slot (cmd_vector, j)) != NULL
	      && vector_active (cmd_element->strvec))
	    {
	      vector descvec = vector_slot (cmd_element->strvec,
				     vector_active (cmd_element->strvec) - 1);
	      vector_sort(descvec, (vector_sort_cmp*)cmp_desc) ;
	    }
      }
}

/*------------------------------------------------------------------------------
 * Take string and break it into tokens.
 *
 * Discards leading and trailing white-space.
 *
 * Treats lines that start with '!' or '#' (after any leading white-space)
 * as empty -- these are comment lines.
 *
 * Tokens are non-whitespace separated by one or more white-space.
 *
 * White-space is anything that isspace() thinks is a space.  (Which in the
 * 'C' locale is ' ', '\t', '\r, '\n', '\f' and '\v'.)
 *
 * Returns:  NULL => empty line (after white-space trimming) or comment line.
 *      otherwise: is vector containing one or more tokens.
 *
 * Note: all the tokens in the vector have at least one character, and no
 *       entries are NULL.
 *
 * NB: it is the caller's responsibility to release the vector and its contents,
 *     see cmd_free_strvec().
 */
extern vector
cmd_make_strvec (const char *string)
{
  const char *cp, *start;
  char *token;
  int strlen;
  vector strvec;

  if (string == NULL)
    return NULL;

  cp = string;

  /* Skip white spaces.                                                 */
  while (isspace((int) *cp))
    cp++;

  /* Return if line is empty or effectively empty                       */
  if ((*cp == '\0') || (*cp == '!') || (*cp == '#'))
    return NULL;

  /* Prepare return vector -- expect some reasonable number of tokens.  */
  strvec = vector_init (10) ;

  /* Copy each command piece and set into vector.                       */
  while (1)
    {
      start = cp;
      while (!isspace((int) *cp) && (*cp != '\0'))
	cp++ ;                 /* eat token characters */
      strlen = cp - start;
      token = XMALLOC (MTYPE_STRVEC, strlen + 1);
      memcpy (token, start, strlen);
      *(token + strlen) = '\0';
      vector_push_item(strvec, token);

      while (isspace((int) *cp))
	cp++ ;                 /* skip white-space     */

      if (*cp == '\0')
	return strvec;
    }
}

/*------------------------------------------------------------------------------
 * Add given string to vector of strings.
 *
 * Create vector if required.
 */
extern vector
cmd_add_to_strvec (vector strvec, const char* str)
{
  if (strvec == NULL)
    strvec = vector_init(1) ;

  vector_push_item(strvec, XSTRDUP(MTYPE_STRVEC, str));

  return strvec ;
} ;

/*------------------------------------------------------------------------------
 * Free allocated string vector (if any) and all its contents.
 *
 * Note that this is perfectly happy with strvec == NULL.
 */
extern void
cmd_free_strvec (vector strvec)
{
  char *cp;

  /* Note that vector_ream_free() returns NULL if strvec == NULL        */
  while((cp = vector_ream_free(strvec)) != NULL)
    XFREE (MTYPE_STRVEC, cp);
} ;

/*----------------------------------------------------------------------------*/

/* Fetch next description.  Used in cmd_make_descvec(). */
static char *
cmd_desc_str (const char **string)
{
  const char *cp, *start;
  char *token;
  int strlen;

  cp = *string;

  if (cp == NULL)
    return NULL;

  /* Skip white spaces. */
  while (isspace ((int) *cp) && *cp != '\0')
    cp++;

  /* Return if there is only white spaces */
  if (*cp == '\0')
    return NULL;

  start = cp;

  while (!(*cp == '\r' || *cp == '\n') && *cp != '\0')
    cp++;

  strlen = cp - start;
  token = XMALLOC (MTYPE_STRVEC, strlen + 1);
  memcpy (token, start, strlen);
  *(token + strlen) = '\0';

  *string = cp;

  return token;
}

/* New string vector. */
static vector
cmd_make_descvec (const char *string, const char *descstr)
{
  int multiple = 0;
  const char *sp;
  char *token;
  int len;
  const char *cp;
  const char *dp;
  vector allvec;
  vector strvec = NULL;
  struct desc *desc;

  cp = string;
  dp = descstr;

  if (cp == NULL)
    return NULL;

  allvec = vector_init (0);

  while (1)
    {
      while (isspace ((int) *cp) && *cp != '\0')
	cp++;

      if (*cp == '(')
	{
	  multiple = 1;
	  cp++;
	}
      if (*cp == ')')
	{
	  multiple = 0;
	  cp++;
	}
      if (*cp == '|')
	{
	  if (! multiple)
	    {
	      fprintf (stderr, "Command parse error!: %s\n", string);
	      exit (1);
	    }
	  cp++;
	}

      while (isspace ((int) *cp) && *cp != '\0')
	cp++;

      if (*cp == '(')
	{
	  multiple = 1;
	  cp++;
	}

      if (*cp == '\0')
	return allvec;

      sp = cp;

      while (! (isspace ((int) *cp) || *cp == '\r' || *cp == '\n' || *cp == ')' || *cp == '|') && *cp != '\0')
	cp++;

      len = cp - sp;

      token = XMALLOC (MTYPE_STRVEC, len + 1);
      memcpy (token, sp, len);
      *(token + len) = '\0';

      desc = XCALLOC (MTYPE_DESC, sizeof (struct desc));
      desc->cmd = token;
      desc->str = cmd_desc_str (&dp);

      if (multiple)
	{
	  if (multiple == 1)
	    {
	      strvec = vector_init (0);
	      vector_set (allvec, strvec);
	    }
	  multiple++;
	}
      else
	{
	  strvec = vector_init (0);
	  vector_set (allvec, strvec);
	}
      vector_set (strvec, desc);
    }
}

/* Count mandantory string vector size.  This is to determine inputed
   command has enough command length. */
static int
cmd_cmdsize (vector strvec)
{
  unsigned int i;
  int size = 0;
  vector descvec;
  struct desc *desc;

  for (i = 0; i < vector_active (strvec); i++)
    if ((descvec = vector_slot (strvec, i)) != NULL)
    {
      if ((vector_active (descvec)) == 1
        && (desc = vector_slot (descvec, 0)) != NULL)
	{
	  if (desc->cmd == NULL || CMD_OPTION (desc->cmd))
	    return size;
	  else
	    size++;
	}
      else
	size++;
    }
  return size;
}

/* Return prompt character of specified node. */
const char *
cmd_prompt (enum node_type node)
{
  struct cmd_node *cnode;

  assert(cmdvec != NULL) ;
  assert(cmdvec->p_items != NULL) ;

  cnode = NULL ;
  if (node < cmdvec->limit)
    cnode = vector_slot (cmdvec, node);

  if (cnode == NULL)
    {
      zlog_err("Could not find prompt for node %d for", node) ;
      return NULL ;
    } ;

  return cnode->prompt;
}

/* Install a command into a node. */
void
install_element (enum node_type ntype, struct cmd_element *cmd)
{
  struct cmd_node *cnode;

  /* cmd_init hasn't been called */
  if (!cmdvec)
    return;

  cnode = vector_slot (cmdvec, ntype);

  if (cnode == NULL)
    {
      fprintf (stderr, "Command node %d doesn't exist, please check it\n",
	       ntype);
      exit (1);
    }

  vector_set (cnode->cmd_vector, cmd);

  if (cmd->strvec == NULL)
    cmd->strvec = cmd_make_descvec (cmd->string, cmd->doc);

  cmd->cmdsize = cmd_cmdsize (cmd->strvec);
}

static unsigned char itoa64[] =
"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

static void
to64(char *s, long v, int n)
{
  while (--n >= 0)
    {
      *s++ = itoa64[v&0x3f];
      v >>= 6;
    }
}

static char *
zencrypt (const char *passwd)
{
  char salt[6];
  struct timeval tv;
  char *crypt (const char *, const char *);

  gettimeofday(&tv,0);

  to64(&salt[0], random(), 3);
  to64(&salt[3], tv.tv_usec, 3);
  salt[5] = '\0';

  return crypt (passwd, salt);
}

/* This function write configuration of this host. */
static int
config_write_host (struct vty *vty)
{
  if (qpthreads_enabled)
    vty_out (vty, "threaded%s", VTY_NEWLINE);

  if (host.name)
    vty_out (vty, "hostname %s%s", host.name, VTY_NEWLINE);

  if (host.encrypt)
    {
      if (host.password_encrypt)
        vty_out (vty, "password 8 %s%s", host.password_encrypt, VTY_NEWLINE);
      if (host.enable_encrypt)
        vty_out (vty, "enable password 8 %s%s", host.enable_encrypt, VTY_NEWLINE);
    }
  else
    {
      if (host.password)
        vty_out (vty, "password %s%s", host.password, VTY_NEWLINE);
      if (host.enable)
        vty_out (vty, "enable password %s%s", host.enable, VTY_NEWLINE);
    }

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

  if (host.motdfile)
    vty_out (vty, "banner motd file %s%s", host.motdfile, VTY_NEWLINE);
  else if (! host.motd)
    vty_out (vty, "no banner motd%s", VTY_NEWLINE);

  return 1;
}

/* Utility function for getting command vector. */
static vector
cmd_node_vector (vector v, enum node_type ntype)
{
  struct cmd_node *cnode = vector_slot (v, ntype);
  return cnode->cmd_vector;
}

#if 0
/* Filter command vector by symbol.  This function is not actually used;
 * should it be deleted? */
static int
cmd_filter_by_symbol (char *command, char *symbol)
{
  int i, lim;

  if (strcmp (symbol, "IPV4_ADDRESS") == 0)
    {
      i = 0;
      lim = strlen (command);
      while (i < lim)
	{
	  if (! (isdigit ((int) command[i]) || command[i] == '.' || command[i] == '/'))
	    return 1;
	  i++;
	}
      return 0;
    }
  if (strcmp (symbol, "STRING") == 0)
    {
      i = 0;
      lim = strlen (command);
      while (i < lim)
	{
	  if (! (isalpha ((int) command[i]) || command[i] == '_' || command[i] == '-'))
	    return 1;
	  i++;
	}
      return 0;
    }
  if (strcmp (symbol, "IFNAME") == 0)
    {
      i = 0;
      lim = strlen (command);
      while (i < lim)
	{
	  if (! isalnum ((int) command[i]))
	    return 1;
	  i++;
	}
      return 0;
    }
  return 0;
}
#endif

/*==============================================================================
 * Match functions.
 *
 * Is the given string a, possibly incomplete, value of the required kind ?
 */

/* Completion match types. */
enum match_type
{
  no_match,             /* nope                                         */
  extend_match,

  ipv4_prefix_match,
  ipv4_match,
  ipv6_prefix_match,
  ipv6_match,
  range_match,
  vararg_match,

  partly_match,         /* OK as far as it went                         */
  exact_match           /* Syntactically complete                       */
};

/*------------------------------------------------------------------------------
 * Is this an IPv4 Address:
 *
 *   999.999.999.999    -- where no part may be > 255
 *
 * TODO: cmd_ipv4_match() seems to accept leading '.' ?
 * TODO: cmd_ipv4_match() seems to accept leading zeros ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 */
static enum match_type
cmd_ipv4_match (const char *str)
{
  const char *sp;
  int dots = 0, nums = 0;
  char buf[4];

  if (str == NULL)
    return partly_match;

  for (;;)
    {
      memset (buf, 0, sizeof (buf));
      sp = str;
      while (*str != '\0')
	{
	  if (*str == '.')
	    {
	      if (dots >= 3)
		return no_match;

	      if (*(str + 1) == '.')
		return no_match;

	      if (*(str + 1) == '\0')
		return partly_match;

	      dots++;
	      break;
	    }
	  if (!isdigit ((int) *str))
	    return no_match;

	  str++;
	}

      if (str - sp > 3)
	return no_match;

      strncpy (buf, sp, str - sp);
      if (atoi (buf) > 255)
	return no_match;

      nums++;

      if (*str == '\0')
	break;

      str++;
    }

  if (nums < 4)
    return partly_match;

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv4 Prefix:
 *
 *   999.999.999.999/99  -- where no part may be > 255,
 *                          and prefix length may not be > 32
 *
 * TODO: cmd_ipv4_prefix_match() seems to accept leading '.' ?
 * TODO: cmd_ipv4_prefix_match() seems to accept leading zeros ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static enum match_type
cmd_ipv4_prefix_match (const char *str)
{
  const char *sp;
  int dots = 0;
  char buf[4];

  if (str == NULL)
    return partly_match;

  for (;;)
    {
      memset (buf, 0, sizeof (buf));
      sp = str;
      while (*str != '\0' && *str != '/')
	{
	  if (*str == '.')
	    {
	      if (dots == 3)
		return no_match;

	      if (*(str + 1) == '.' || *(str + 1) == '/')
		return no_match;

	      if (*(str + 1) == '\0')
		return partly_match;

	      dots++;
	      break;
	    }

	  if (!isdigit ((int) *str))
	    return no_match;

	  str++;
	}

      if (str - sp > 3)
	return no_match;

      strncpy (buf, sp, str - sp);
      if (atoi (buf) > 255)
	return no_match;

      if (dots == 3)
	{
	  if (*str == '/')
	    {
	      if (*(str + 1) == '\0')
		return partly_match;

	      str++;
	      break;
	    }
	  else if (*str == '\0')
	    return partly_match;
	}

      if (*str == '\0')
	return partly_match;

      str++;
    }

  sp = str;
  while (*str != '\0')
    {
      if (!isdigit ((int) *str))
	return no_match;

      str++;
    }

  if (atoi (sp) > 32)
    return no_match;

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv6 Address:
 *
 * TODO: cmd_ipv6_match() only returns "partly_match" for empty string ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 */

#define IPV6_ADDR_STR		"0123456789abcdefABCDEF:.%"
#define IPV6_PREFIX_STR		"0123456789abcdefABCDEF:.%/"
#define STATE_START		1
#define STATE_COLON		2
#define STATE_DOUBLE		3
#define STATE_ADDR		4
#define STATE_DOT               5
#define STATE_SLASH		6
#define STATE_MASK		7

#ifdef HAVE_IPV6

static enum match_type
cmd_ipv6_match (const char *str)
{
  int state = STATE_START;
  int colons = 0, nums = 0, double_colon = 0;
  const char *sp = NULL;
  struct sockaddr_in6 sin6_dummy;
  int ret;

  if (str == NULL)
    return partly_match;

  if (strspn (str, IPV6_ADDR_STR) != strlen (str))
    return no_match;

  /* use inet_pton that has a better support,
   * for example inet_pton can support the automatic addresses:
   *  ::1.2.3.4
   */
  ret = inet_pton(AF_INET6, str, &sin6_dummy.sin6_addr);

  if (ret == 1)
    return exact_match;

  while (*str != '\0')
    {
      switch (state)
	{
	case STATE_START:
	  if (*str == ':')
	    {
	      if (*(str + 1) != ':' && *(str + 1) != '\0')
		return no_match;
     	      colons--;
	      state = STATE_COLON;
	    }
	  else
	    {
	      sp = str;
	      state = STATE_ADDR;
	    }

	  continue;
	case STATE_COLON:
	  colons++;
	  if (*(str + 1) == ':')
	    state = STATE_DOUBLE;
	  else
	    {
	      sp = str + 1;
	      state = STATE_ADDR;
	    }
	  break;
	case STATE_DOUBLE:
	  if (double_colon)
	    return no_match;

	  if (*(str + 1) == ':')
	    return no_match;
	  else
	    {
	      if (*(str + 1) != '\0')
		colons++;
	      sp = str + 1;
	      state = STATE_ADDR;
	    }

	  double_colon++;
	  nums++;
	  break;
	case STATE_ADDR:
	  if (*(str + 1) == ':' || *(str + 1) == '\0')
	    {
	      if (str - sp > 3)
		return no_match;

	      nums++;
	      state = STATE_COLON;
	    }
	  if (*(str + 1) == '.')
	    state = STATE_DOT;
	  break;
	case STATE_DOT:
	  state = STATE_ADDR;
	  break;
	default:
	  break;
	}

      if (nums > 8)
	return no_match;

      if (colons > 7)
	return no_match;

      str++;
    }

#if 0
  if (nums < 11)
    return partly_match;
#endif /* 0 */

  return exact_match;
}

/*------------------------------------------------------------------------------
 * Is this an IPv6 Prefix:
 *
 * TODO: cmd_ipv6_prefix_match() hardly returns "partly_match" ?
 * TODO: cmd_ipv6_prefix_match() possibly accepts invalid address before '/' ?
 *
 * Returns: no_match       -- improperly formed
 *          partly_match   -- accepts empty string
 *          exact_match    -- syntactically complete
 *
 * NB: partly_match is returned for anything valid before the '/', but which
 *     has no '/' or no number after the '/'.
 */
static enum match_type
cmd_ipv6_prefix_match (const char *str)
{
  int state = STATE_START;
  int colons = 0, nums = 0, double_colon = 0;
  int mask;
  const char *sp = NULL;
  char *endptr = NULL;

  if (str == NULL)
    return partly_match;

  if (strspn (str, IPV6_PREFIX_STR) != strlen (str))
    return no_match;

  while (*str != '\0' && state != STATE_MASK)
    {
      switch (state)
	{
	case STATE_START:
	  if (*str == ':')
	    {
	      if (*(str + 1) != ':' && *(str + 1) != '\0')
		return no_match;
	      colons--;
	      state = STATE_COLON;
	    }
	  else
	    {
	      sp = str;
	      state = STATE_ADDR;
	    }

	  continue;
	case STATE_COLON:
	  colons++;
	  if (*(str + 1) == '/')
	    return no_match;
	  else if (*(str + 1) == ':')
	    state = STATE_DOUBLE;
	  else
	    {
	      sp = str + 1;
	      state = STATE_ADDR;
	    }
	  break;
	case STATE_DOUBLE:
	  if (double_colon)
	    return no_match;

	  if (*(str + 1) == ':')
	    return no_match;
	  else
	    {
	      if (*(str + 1) != '\0' && *(str + 1) != '/')
		colons++;
	      sp = str + 1;

	      if (*(str + 1) == '/')
		state = STATE_SLASH;
	      else
		state = STATE_ADDR;
	    }

	  double_colon++;
	  nums += 1;
	  break;
	case STATE_ADDR:
	  if (*(str + 1) == ':' || *(str + 1) == '.'
	      || *(str + 1) == '\0' || *(str + 1) == '/')
	    {
	      if (str - sp > 3)
		return no_match;

	      for (; sp <= str; sp++)
		if (*sp == '/')
		  return no_match;

	      nums++;

	      if (*(str + 1) == ':')
		state = STATE_COLON;
	      else if (*(str + 1) == '.')
		state = STATE_DOT;
	      else if (*(str + 1) == '/')
		state = STATE_SLASH;
	    }
	  break;
	case STATE_DOT:
	  state = STATE_ADDR;
	  break;
	case STATE_SLASH:
	  if (*(str + 1) == '\0')
	    return partly_match;

	  state = STATE_MASK;
	  break;
	default:
	  break;
	}

      if (nums > 11)
	return no_match;

      if (colons > 7)
	return no_match;

      str++;
    }

  if (state < STATE_MASK)
    return partly_match;

  mask = strtol (str, &endptr, 10);
  if (*endptr != '\0')
    return no_match;

  if (mask < 0 || mask > 128)
    return no_match;

/* I don't know why mask < 13 makes command match partly.
   Forgive me to make this comments. I Want to set static default route
   because of lack of function to originate default in ospf6d; sorry
       yasu
  if (mask < 13)
    return partly_match;
*/

  return exact_match;
}

#endif /* HAVE_IPV6  */

/*------------------------------------------------------------------------------
 * Is this a decimal number in the allowed range:
 *
 * Returns: 1 => OK -- *including* empty string
 *          0 => not a valid number, or not in required range
 *               (or invalid range !!)
 */

#define DECIMAL_STRLEN_MAX 10

static int
cmd_range_match (const char *range, const char *str)
{
  char *p;
  char buf[DECIMAL_STRLEN_MAX + 1];
  char *endptr = NULL;
  unsigned long min, max, val;

  if (str == NULL)
    return 1;

  val = strtoul (str, &endptr, 10);
  if (*endptr != '\0')
    return 0;

  range++;
  p = strchr (range, '-');
  if (p == NULL)
    return 0;
  if (p - range > DECIMAL_STRLEN_MAX)
    return 0;
  strncpy (buf, range, p - range);
  buf[p - range] = '\0';
  min = strtoul (buf, &endptr, 10);
  if (*endptr != '\0')
    return 0;

  range = p + 1;
  p = strchr (range, '>');
  if (p == NULL)
    return 0;
  if (p - range > DECIMAL_STRLEN_MAX)
    return 0;
  strncpy (buf, range, p - range);
  buf[p - range] = '\0';
  max = strtoul (buf, &endptr, 10);
  if (*endptr != '\0')
    return 0;

  if (val < min || val > max)
    return 0;

  return 1;
}

/*==============================================================================
 * Command "filtering".
 *
 * The command parsing process starts with a (shallow) copy of the cmd_vector
 * entry for the current "node".
 *
 * So cmd_v contains pointers to struct cmd_element values.  When match fails,
 * the pointer is set NULL -- so parsing is a process of reducing the cmd_v
 * down to just the entries that match.
 *
 * Each cmd_element has a vector "strvec", which contains an entry for each
 * "token" position.  That entry is a vector containing the possible values at
 * that position.
 *
 *
 */

/*------------------------------------------------------------------------------
 * Make completion match and return match type flag.
 *
 * Takes:   command   -- address of candidate token
 *          cmd_v     -- vector of commands that is being reduced/filtered
 *          index     -- index of token (position in line -- 0 == first)
 *
 * Returns: any of the enum match_type values:
 *
 *          no_match           => no match of any kind
 *
 *          extend_match       => saw an optional token
 *          ipv4_prefix_match )
 *          ipv4_match        )
 *          ipv6_prefix_match ) saw full or partial match for this
 *          ipv6_match        )
 *          range_match       )
 *          vararg_match      )
 *
 *          partly_match       => saw partial match for a keyword
 *          exact_match        => saw exact match for a keyword
 *
 * Note that these return values are in ascending order of preference.  So,
 * if there are multiple possibilities at this position, will return the one
 * furthest down this list.
 */
static enum match_type
cmd_filter_by_completion (char *command, vector cmd_v, unsigned int index)
{
  unsigned int i;
  enum match_type match_type;

  match_type = no_match;

  /* If command and cmd_element string does not match set NULL to vector */
  for (i = 0; i < vector_active (cmd_v); i++)
    {
      const char *str;
      struct cmd_element *cmd_element;
      vector descvec;
      struct desc *desc;
      unsigned int j;
      int matched ;

      /* Skip past cmd_v entries that have already been set NULL        */
      if ((cmd_element = vector_slot (cmd_v, i)) == NULL)
        continue ;

      /* Discard cmd_v entry that has no token at the current position  */
      if (index >= vector_active (cmd_element->strvec))
        {
          vector_slot (cmd_v, i) = NULL;
          continue ;
        } ;

      /* See if get any sort of match at current position               */
      matched = 0 ;
      descvec = vector_slot (cmd_element->strvec, index);

      for (j = 0; j < vector_active (descvec); j++)
        if ((desc = vector_slot (descvec, j)))
          {
            str = desc->cmd;

            if (CMD_VARARG (str))
              {
                if (match_type < vararg_match)
                  match_type = vararg_match;
                matched++;
              }
            else if (CMD_RANGE (str))
              {
                if (cmd_range_match (str, command))
                  {
                    if (match_type < range_match)
                      match_type = range_match;

                    matched++;
                  }
              }
#ifdef HAVE_IPV6
            else if (CMD_IPV6 (str))
              {
                if (cmd_ipv6_match (command))
                  {
                    if (match_type < ipv6_match)
                      match_type = ipv6_match;

                    matched++;
                  }
              }
            else if (CMD_IPV6_PREFIX (str))
              {
                if (cmd_ipv6_prefix_match (command))
                  {
                    if (match_type < ipv6_prefix_match)
                      match_type = ipv6_prefix_match;

                    matched++;
                  }
              }
#endif /* HAVE_IPV6  */
            else if (CMD_IPV4 (str))
              {
                if (cmd_ipv4_match (command))
                  {
                    if (match_type < ipv4_match)
                      match_type = ipv4_match;

                    matched++;
                  }
              }
            else if (CMD_IPV4_PREFIX (str))
              {
                if (cmd_ipv4_prefix_match (command))
                  {
                    if (match_type < ipv4_prefix_match)
                      match_type = ipv4_prefix_match;
                    matched++;
                  }
              }
            else if (CMD_OPTION (str) || CMD_VARIABLE (str))
              /* Check is this point's argument optional ? */
              {
                if (match_type < extend_match)
                  match_type = extend_match;
                matched++;
              }
            else if (strncmp (command, str, strlen (command)) == 0)
              {
                if (strcmp (command, str) == 0)
                  match_type = exact_match;
                else
                  {
                    if (match_type < partly_match)
                      match_type = partly_match;
                  }
                matched++;
              } ;
          } ;

      /* Discard cmd_v entry that has no match at this position         */
      if (!matched)
        vector_slot (cmd_v, i) = NULL;
    }

  return match_type;
}

/*------------------------------------------------------------------------------
 * Filter vector by command character with index.
 *
 * This appears to be identical to cmd_filter_by_completion(), except that
 * when matching keywords, requires an exact match.
 *
 * TODO: see if can merge cmd_filter_by_completion() & cmd_filter_by_string()
 */
static enum match_type
cmd_filter_by_string (char *command, vector cmd_v, unsigned int index)
{
  unsigned int i;
  const char *str;
  struct cmd_element *cmd_element;
  enum match_type match_type;
  vector descvec;
  struct desc *desc;

  match_type = no_match;

  /* If command and cmd_element string does not match set NULL to vector */
  for (i = 0; i < vector_active (cmd_v); i++)
    if ((cmd_element = vector_slot (cmd_v, i)) != NULL)
      {
	/* If given index is bigger than max string vector of command,
	   set NULL */
	if (index >= vector_active (cmd_element->strvec))
	  vector_slot (cmd_v, i) = NULL;
	else
	  {
	    unsigned int j;
	    int matched = 0;

	    descvec = vector_slot (cmd_element->strvec, index);

	    for (j = 0; j < vector_active (descvec); j++)
	      if ((desc = vector_slot (descvec, j)))
		{
		  str = desc->cmd;

		  if (CMD_VARARG (str))
		    {
		      if (match_type < vararg_match)
			match_type = vararg_match;
		      matched++;
		    }
		  else if (CMD_RANGE (str))
		    {
		      if (cmd_range_match (str, command))
			{
			  if (match_type < range_match)
			    match_type = range_match;
			  matched++;
			}
		    }
#ifdef HAVE_IPV6
		  else if (CMD_IPV6 (str))
		    {
		      if (cmd_ipv6_match (command) == exact_match)
			{
			  if (match_type < ipv6_match)
			    match_type = ipv6_match;
			  matched++;
			}
		    }
		  else if (CMD_IPV6_PREFIX (str))
		    {
		      if (cmd_ipv6_prefix_match (command) == exact_match)
			{
			  if (match_type < ipv6_prefix_match)
			    match_type = ipv6_prefix_match;
			  matched++;
			}
		    }
#endif /* HAVE_IPV6  */
		  else if (CMD_IPV4 (str))
		    {
		      if (cmd_ipv4_match (command) == exact_match)
			{
			  if (match_type < ipv4_match)
			    match_type = ipv4_match;
			  matched++;
			}
		    }
		  else if (CMD_IPV4_PREFIX (str))
		    {
		      if (cmd_ipv4_prefix_match (command) == exact_match)
			{
			  if (match_type < ipv4_prefix_match)
			    match_type = ipv4_prefix_match;
			  matched++;
			}
		    }
		  else if (CMD_OPTION (str) || CMD_VARIABLE (str))
		    {
		      if (match_type < extend_match)
			match_type = extend_match;
		      matched++;
		    }
		  else
		    {
		      if (strcmp (command, str) == 0)
			{
			  match_type = exact_match;
			  matched++;
			}
		    }
		}
	    if (!matched)
	      vector_slot (cmd_v, i) = NULL;
	  }
      }
  return match_type;
}

/*------------------------------------------------------------------------------
 * Check for ambiguous match
 *
 * Given the best that cmd_filter_by_completion() or cmd_filter_by_string()
 * found, check as follows:
 *
 *   1. discard all commands for which do not have the type of match selected.
 *
 *      See above for the ranking of matches.
 *
 *   2. for "partial match", look out for matching more than one keyword, and
 *      return 1 if finds that.
 *
 *   3. for "range match", look out for matching more than one range, and
 *       return 1 if finds that.
 *
 *   4. for ipv4_prefix_match and ipv6_prefix_match, if get a "partial match",
 *      return 2.
 *
 *      This appears to catch things which are supposed to be prefixes, but
 *      do not have a '/' or do not have any digits after the '/'.
 *
 * Takes:   command   -- address of candidate token
 *          cmd_v     -- vector of commands that is being reduced/filtered
 *          index     -- index of token (position in line -- 0 == first)
 *          type      -- as returned by cmd_filter_by_completion()
 *                                   or cmd_filter_by_string()
 *
 * Returns: 0 => not ambiguous
 *          1 => ambiguous -- the candidate token matches more than one
 *                            keyword, or the candidate number matches more
 *                            than one number range.
 *          2 => partial match for ipv4_prefix or ipv6_prefix
 *               (missing '/' or no digits after '/').
 */
static int
is_cmd_ambiguous (char *command, vector cmd_v, int index, enum match_type type)
{
  unsigned int i;
  unsigned int j;
  const char *str = NULL;
  struct cmd_element *cmd_element;
  const char *matched = NULL;
  vector descvec;
  struct desc *desc;

  for (i = 0; i < vector_active (cmd_v); i++)
    if ((cmd_element = vector_slot (cmd_v, i)) != NULL)
      {
	int match = 0;

	descvec = vector_slot (cmd_element->strvec, index);

	for (j = 0; j < vector_active (descvec); j++)
	  if ((desc = vector_slot (descvec, j)))
	    {
	      enum match_type ret;

	      str = desc->cmd;

	      switch (type)
		{
		case exact_match:
		  if (!(CMD_OPTION (str) || CMD_VARIABLE (str))
		      && strcmp (command, str) == 0)
		    match++;
		  break;
		case partly_match:
		  if (!(CMD_OPTION (str) || CMD_VARIABLE (str))
		      && strncmp (command, str, strlen (command)) == 0)
		    {
		      if (matched && strcmp (matched, str) != 0)
			return 1;	/* There is ambiguous match. */
		      else
			matched = str;
		      match++;
		    }
		  break;
		case range_match:
		  if (cmd_range_match (str, command))
		    {
		      if (matched && strcmp (matched, str) != 0)
			return 1;
		      else
			matched = str;
		      match++;
		    }
		  break;
#ifdef HAVE_IPV6
		case ipv6_match:
		  if (CMD_IPV6 (str))
		    match++;
		  break;
		case ipv6_prefix_match:
		  if ((ret = cmd_ipv6_prefix_match (command)) != no_match)
		    {
		      if (ret == partly_match)
			return 2;	/* There is incomplete match. */

		      match++;
		    }
		  break;
#endif /* HAVE_IPV6 */
		case ipv4_match:
		  if (CMD_IPV4 (str))
		    match++;
		  break;
		case ipv4_prefix_match:
		  if ((ret = cmd_ipv4_prefix_match (command)) != no_match)
		    {
		      if (ret == partly_match)
			return 2;	/* There is incomplete match. */

		      match++;
		    }
		  break;
		case extend_match:
		  if (CMD_OPTION (str) || CMD_VARIABLE (str))
		    match++;
		  break;
		case no_match:
		default:
		  break;
		}
	    }
	if (!match)
	  vector_slot (cmd_v, i) = NULL;
      }
  return 0;
}

/*------------------------------------------------------------------------------
 * If src matches dst return dst string, otherwise return NULL
 *
 * Returns NULL if dst is an option, variable of vararg.
 *
 * NULL or empty src are deemed to match.
 */
static const char *
cmd_entry_function (const char *src, const char *dst)
{
  if (CMD_OPTION (dst) || CMD_VARIABLE (dst) || CMD_VARARG (dst))
    return NULL;

  if ((src == NULL) || (*src == '\0'))
    return dst;

  if (strncmp (src, dst, strlen (src)) == 0)
    return dst;

  return NULL;
}

/* If src matches dst return dst string, otherwise return NULL */
/* This version will return the dst string always if it is
   CMD_VARIABLE for '?' key processing */
static const char *
cmd_entry_function_desc (const char *src, const char *dst)
{
  if (CMD_VARARG (dst))
    return dst;

  if (CMD_RANGE (dst))
    {
      if (cmd_range_match (dst, src))
	return dst;
      else
	return NULL;
    }

#ifdef HAVE_IPV6
  if (CMD_IPV6 (dst))
    {
      if (cmd_ipv6_match (src))
	return dst;
      else
	return NULL;
    }

  if (CMD_IPV6_PREFIX (dst))
    {
      if (cmd_ipv6_prefix_match (src))
	return dst;
      else
	return NULL;
    }
#endif /* HAVE_IPV6 */

  if (CMD_IPV4 (dst))
    {
      if (cmd_ipv4_match (src))
	return dst;
      else
	return NULL;
    }

  if (CMD_IPV4_PREFIX (dst))
    {
      if (cmd_ipv4_prefix_match (src))
	return dst;
      else
	return NULL;
    }

  /* Optional or variable commands always match on '?' */
  if (CMD_OPTION (dst) || CMD_VARIABLE (dst))
    return dst;

  /* In case of 'command \t', given src is NULL string. */
  if (src == NULL)
    return dst;

  if (strncmp (src, dst, strlen (src)) == 0)
    return dst;
  else
    return NULL;
}

/*------------------------------------------------------------------------------
 * Check same string element existence.
 *
 * Returns: 0 => found same string in the vector
 *          1 => NOT found same string in the vector
 */
static int
cmd_unique_string (vector v, const char *str)
{
  unsigned int i;
  char *match;

  for (i = 0; i < vector_active (v); i++)
    if ((match = vector_slot (v, i)) != NULL)
      if (strcmp (match, str) == 0)
	return 0;
  return 1;
}

/* Compare string to description vector.  If there is same string
   return 1 else return 0. */
static int
desc_unique_string (vector v, const char *str)
{
  unsigned int i;
  struct desc *desc;

  for (i = 0; i < vector_active (v); i++)
    if ((desc = vector_slot (v, i)) != NULL)
      if (strcmp (desc->cmd, str) == 0)
	return 1;
  return 0;
}

static int
cmd_try_do_shortcut (enum node_type node, char* first_word) {
  return (node >= MIN_DO_SHORTCUT_NODE)
      && (first_word != NULL)
      && (strcmp( "do", first_word) == 0) ? 1 : 0 ;
}

/* '?' describe command support. */
static vector
cmd_describe_command_real (vector vline, int node, int *status)
{
  unsigned int i;
  vector cmd_vector;
#define INIT_MATCHVEC_SIZE 10
  vector matchvec;
  struct cmd_element *cmd_element;
  unsigned int index;
  int ret;
  enum match_type match;
  char *command;

  /* Set index. */
  if (vector_active (vline) == 0)
    {
      *status = CMD_ERR_NO_MATCH;
      return NULL;
    }
  else
    index = vector_active (vline) - 1;

  /* Make copy vector of current node's command vector. */
  cmd_vector = vector_copy (cmd_node_vector (cmdvec, node));

  /* Prepare match vector */
  matchvec = vector_init (INIT_MATCHVEC_SIZE);

  /* Filter commands. */
  /* Only words precedes current word will be checked in this loop. */
  for (i = 0; i < index; i++)
    if ((command = vector_slot (vline, i)))
      {
	match = cmd_filter_by_completion (command, cmd_vector, i);

	if (match == vararg_match)
	  {
	    struct cmd_element *cmd_element;
	    vector descvec;
	    unsigned int j, k;

	    for (j = 0; j < vector_active (cmd_vector); j++)
	      if ((cmd_element = vector_slot (cmd_vector, j)) != NULL
		  && (vector_active (cmd_element->strvec)))
		{
		  descvec = vector_slot (cmd_element->strvec,
					 vector_active (cmd_element->strvec) - 1);
		  for (k = 0; k < vector_active (descvec); k++)
		    {
		      struct desc *desc = vector_slot (descvec, k);
		      vector_set (matchvec, desc);
		    }
		}

	    vector_set (matchvec, &desc_cr);
	    vector_free (cmd_vector);

	    return matchvec;
	  }

	if ((ret = is_cmd_ambiguous (command, cmd_vector, i, match)) == 1)
	  {
	    vector_free (cmd_vector);
	    vector_free (matchvec);
	    *status = CMD_ERR_AMBIGUOUS;
	    return NULL;
	  }
	else if (ret == 2)
	  {
	    vector_free (cmd_vector);
	    vector_free (matchvec);
	    *status = CMD_ERR_NO_MATCH;
	    return NULL;
	  }
      }

  /* Prepare match vector */
  /*  matchvec = vector_init (INIT_MATCHVEC_SIZE); */

  /* Make sure that cmd_vector is filtered based on current word */
  command = vector_slot (vline, index);
  if (command)
    match = cmd_filter_by_completion (command, cmd_vector, index);

  /* Make description vector. */
  for (i = 0; i < vector_active (cmd_vector); i++)
    if ((cmd_element = vector_slot (cmd_vector, i)) != NULL)
      {
	vector strvec = cmd_element->strvec;

	/* if command is NULL, index may be equal to vector_active */
	if (command && index >= vector_active (strvec))
	  vector_slot (cmd_vector, i) = NULL;
	else
	  {
	    /* Check if command is completed. */
	    if (command == NULL && index == vector_active (strvec))
	      {
		if (!desc_unique_string (matchvec, command_cr))
		  vector_set (matchvec, &desc_cr);
	      }
	    else
	      {
		unsigned int j;
		vector descvec = vector_slot (strvec, index);
		struct desc *desc;

		for (j = 0; j < vector_active (descvec); j++)
		  if ((desc = vector_slot (descvec, j)))
		    {
		      const char *string;

		      string = cmd_entry_function_desc (command, desc->cmd);
		      if (string)
			{
			  /* Uniqueness check */
			  if (!desc_unique_string (matchvec, string))
			    vector_set (matchvec, desc);
			}
		    }
	      }
	  }
      }
  vector_free (cmd_vector);

  if (vector_slot (matchvec, 0) == NULL)
    {
      vector_free (matchvec);
      *status = CMD_ERR_NO_MATCH;
      return NULL;
    }

  *status = CMD_SUCCESS;
  return matchvec;
}

/*------------------------------------------------------------------------------
 * Get description of current (partial) command
 *
 * Returns: NULL => no description available
 *
 *                  status set to CMD_ERR_NO_MATCH or CMD_ERR_AMBIGUOUS
 *
 *      or: address of vector of "struct desc" values available.
 *
 * NB: when a vector is returned it is the caller's responsibility to
 *     vector_free() it.  (The contents are all effectively const, so do not
 *     themselves need to be freed.)
 */
vector
cmd_describe_command (vector vline, int node, int *status)
{
  vector ret;

  if ( cmd_try_do_shortcut(node, vector_slot(vline, 0) ) )
    {
       vector shifted_vline;
      unsigned int index;

      /* We can try it on enable node, cos' the vty is authenticated */

      shifted_vline = vector_init (vector_count(vline));
      /* use memcpy? */
      for (index = 1; index < vector_active (vline); index++)
	{
	  vector_set_index (shifted_vline, index-1, vector_lookup(vline, index));
	}

      ret = cmd_describe_command_real (shifted_vline, ENABLE_NODE, status);

      vector_free(shifted_vline);
      return ret;
  }

  return cmd_describe_command_real (vline, node, status);
}

/*------------------------------------------------------------------------------
 * Check LCD of matched command.
 *
 * Scan list of matched keywords, and by comparing them pair-wise, find the
 * longest common leading substring.
 *
 * Returns: 0 if zero or one matched keywords
 *          length of longest common leading substring, otherwise.
 */
static int
cmd_lcd (vector matchvec)
{
  int n ;
  int i ;
  int lcd ;
  char *sp, *sq, *ss ;

  n = vector_end(matchvec) ;
  if (n < 2)
    return 0 ;

  ss = vector_get_item(matchvec, 0) ;
  lcd = strlen(ss) ;

  for (i = 1 ; i < n ; i++)
    {
      sq = ss ;
      ss = vector_get_item(matchvec, i) ;
      sp = ss ;

      while ((*sp == *sq) && (*sp != '\0'))
        {
          ++sp ;
          ++sq ;
        } ;

      if (lcd > (sp - ss))
        lcd = (sp - ss) ;
    }
  return lcd;
}

/*------------------------------------------------------------------------------
 * Command line completion support.
 */
static vector
cmd_complete_command_real (vector vline, int node, int *status)
{
  unsigned int i;
  unsigned int ivl ;
  unsigned int last_ivl ;
  vector cmd_v ;
#define INIT_MATCHVEC_SIZE 10
  vector matchvec;
  struct cmd_element *cmd_element;
  unsigned int index;
  struct desc *desc;
  vector descvec;
  char *token;
  int n ;

  /* Stop immediately if the vline is empty.                            */
  if (vector_active (vline) == 0)
    {
      *status = CMD_ERR_NO_MATCH;
      return NULL;
    }

  /* Take (shallow) copy of cmdvec for given node.                      */
  cmd_v = vector_copy (cmd_node_vector (cmdvec, node));

  /* First, filter upto, but excluding last token                       */
  last_ivl = vector_active (vline) - 1;

  for (ivl = 0; ivl < last_ivl; ivl++)
    {
      enum match_type match;
      int ret;

      /* TODO: does this test make any sense ?                          */
      if ((token = vector_slot (vline, ivl)) == NULL)
        continue ;

      /* First try completion match, return best kind of match          */
      index = ivl ;
      match = cmd_filter_by_completion (token, cmd_v, index) ;

      /* Eliminate all but the selected kind of match                   */
      ret = is_cmd_ambiguous (token, cmd_v, index, match) ;

      if (ret == 1)
        {
          /* ret == 1 => either token matches more than one keyword
           *                 or token matches more than one number range
           */
          vector_free (cmd_v);
	  *status = CMD_ERR_AMBIGUOUS;
	  return NULL;
	}
#if 0
      /* For command completion purposes do not appear to care about
       * incomplete ipv4 or ipv6 prefixes (missing '/' or digits after).
       */
      else if (ret == 2)
        {
          vector_free (cmd_v);
	   *status = CMD_ERR_NO_MATCH;
	   return NULL;
        }
#endif
      }

  /* Prepare match vector.                                              */
  matchvec = vector_init (INIT_MATCHVEC_SIZE);

  /* Now we got into completion                                         */
  index = last_ivl ;
  token = vector_slot(vline, last_ivl) ;    /* is now the last token    */

  for (i = 0; i < vector_active (cmd_v); i++)
    {
      unsigned int j;
      const char *string;

      if ((cmd_element = vector_slot (cmd_v, i)) == NULL)
        continue ;

      vector strvec = cmd_element->strvec;

      /* Check field length                                             */
      if (index >= vector_active (strvec))
        {
	  vector_slot (cmd_v, i) = NULL;
	  continue ;
        }

      descvec = vector_slot (strvec, index);

      for (j = 0; j < vector_active (descvec); j++)
        if ((desc = vector_slot (descvec, j)) != NULL)
          {
            string = cmd_entry_function(token, desc->cmd) ;
            if ((string != NULL) && cmd_unique_string(matchvec, string))
              cmd_add_to_strvec (matchvec, string) ;
          }
    } ;

  n = vector_end(matchvec) ;    /* number of entries in the matchvec    */

  /* We don't need cmd_v any more.                                      */
  vector_free (cmd_v);

  /* No matched command */
  if (n == 0)
    {
      vector_free (matchvec);

      /* In case of 'command \t' pattern.  Do you need '?' command at
         the end of the line. */
      if (*token == '\0')
	*status = CMD_ERR_NOTHING_TODO;
      else
	*status = CMD_ERR_NO_MATCH;
      return NULL;
    }

/* XXX: TODO: stop poking around inside vector	*/
  /* Only one matched                                                   */
  if (n == 1)
    {
      *status = CMD_COMPLETE_FULL_MATCH;
      return matchvec ;
    }

  /* Check LCD of matched strings.                                      */
  if (token != NULL)
    {
      unsigned lcd = cmd_lcd (matchvec) ;

      if (lcd != 0)
	{
	  if (strlen(token) < lcd)
	    {
	      char *lcdstr;

	      lcdstr = XMALLOC (MTYPE_STRVEC, lcd + 1);
	      memcpy (lcdstr, vector_get_item(matchvec, 0), lcd) ;
	      lcdstr[lcd] = '\0';

	      cmd_free_strvec(matchvec) ;      /* discard the match vector */

	      matchvec = vector_init (INIT_MATCHVEC_SIZE);
	      vector_push_item(matchvec, lcdstr) ;

	      *status = CMD_COMPLETE_MATCH;
	      return matchvec ;
	    }
	}
    }

  *status = CMD_COMPLETE_LIST_MATCH;
  return matchvec ;
}

/*------------------------------------------------------------------------------
 * Can the current command be completed ?
 */
extern vector
cmd_complete_command (vector vline, int node, int *status)
{
  vector ret;

  if ( cmd_try_do_shortcut(node, vector_slot(vline, 0) ) )
    {
      vector shifted_vline;
      unsigned int index;

      /* We can try it on enable node, cos' the vty is authenticated */

      shifted_vline = vector_init (vector_count(vline));
      /* use memcpy? */
      for (index = 1; index < vector_active (vline); index++)
	{
	  vector_set_index (shifted_vline, index-1, vector_lookup(vline, index));
	}

      ret = cmd_complete_command_real (shifted_vline, ENABLE_NODE, status);

      vector_free(shifted_vline);
      return ret;
  }

  return cmd_complete_command_real (vline, node, status);
}

/*------------------------------------------------------------------------------
 * Return parent node
 *
 * All nodes > CONFIG_NODE are descended from CONFIG_NODE
 */
enum node_type
node_parent ( enum node_type node )
{
  assert (node > CONFIG_NODE);

  switch (node)
    {
    case BGP_VPNV4_NODE:
    case BGP_IPV4_NODE:
    case BGP_IPV4M_NODE:
    case BGP_IPV6_NODE:
    case BGP_IPV6M_NODE:
      return BGP_NODE;

    case KEYCHAIN_KEY_NODE:
      return KEYCHAIN_NODE;

    default:
      return CONFIG_NODE;
    }
}

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", if possible, ready for execution.
 *
 * If "strict" use: cmd_filter_by_string()
 *   otherwise use: cmd_filter_by_completion()
 *
 * Takes the node from parsed->cnode.
 *
 * Returns:  CMD_SUCCESS => successfully parsed command, and the result is
 *                          in the given parsed structure, ready for execution.
 *
 *           CMD_SUCCESS_DAEMON => as CMD_SUCCESS, and the command has a
 *                                 "daemon" value.
 *
 *           otherwise   => some failure to parse
 *
 * NB: the argv[] in the parsed structure contains *copies* of the char*
 *     pointers in the given vline.
 *
 *     The vline may not be released until the parsed structure is.
 *
 *     The parsed structure may be released without worrying about the contents
 *     of the argv[].
 */
enum cmd_parse_type
{
  cmd_parse_completion  = 0,
  cmd_parse_strict      = 1,
};

static int
cmd_parse_command(vector vline, int first, struct cmd_parsed* parsed,
                                                       enum cmd_parse_type type)
{
  unsigned int i ;
  unsigned int ivl ;
  unsigned index ;
  vector cmd_v;
  struct cmd_element *cmd_element;
  struct cmd_element *matched_element;
  unsigned int matched_count, incomplete_count;
  int argc;
  enum match_type match = 0;
  int varflag;
  char *command;

  int strict = (type == cmd_parse_strict) ;

  parsed->cmd  = NULL ;         /* return this if parsing fails */
  parsed->argc = 0 ;

  /* Make copy of command elements.                             */
  cmd_v = vector_copy (cmd_node_vector (cmdvec, parsed->cnode));

  /* Look for an unambiguous result                             */
  index = 0 ;
  for (ivl = first; ivl < vector_active (vline); ivl++)
    if ((command = vector_slot (vline, ivl)) != NULL)
      {
	int ret ;

	index = ivl - first ;
	match = strict ? cmd_filter_by_string(command, cmd_v, index)
	               : cmd_filter_by_completion(command, cmd_v, index) ;

	if (match == vararg_match)
	  break;

	ret = is_cmd_ambiguous (command, cmd_v, index, match);

	if (ret != 0)
	  {
	    assert((ret == 1) || (ret == 2)) ;
            vector_free (cmd_v);
            return (ret == 1) ? CMD_ERR_AMBIGUOUS : CMD_ERR_NO_MATCH ;
	  }
      }

  /* Check matched count.                                       */
  matched_element  = NULL;
  matched_count    = 0;
  incomplete_count = 0;

  for (i = 0; i < vector_active (cmd_v); i++)
    if ((cmd_element = vector_slot (cmd_v, i)) != NULL)
      {
	if (match == vararg_match || index >= cmd_element->cmdsize)
	  {
	    matched_element = cmd_element;
#if 0
	    printf ("DEBUG: %s\n", cmd_element->string);
#endif
	    matched_count++;
	  }
	else
	  {
	    incomplete_count++;
	  }
      }

  /* Finished with cmd_v.                                               */
  vector_free (cmd_v);

  /* To execute command, matched_count must be 1.                       */
  if (matched_count != 1)
    {
      if (matched_count == 0)
        return (incomplete_count) ? CMD_ERR_INCOMPLETE : CMD_ERR_NO_MATCH ;
      else
        return CMD_ERR_AMBIGUOUS ;
    } ;

  /* Found command -- process the arguments ready for execution         */
  varflag = 0 ;
  argc    = 0 ;

  for (ivl = first; ivl < vector_active (vline); ivl++)
    {
      int take = varflag ;

      if (!varflag)
	{
	  int index = ivl - first ;
	  vector descvec = vector_slot (matched_element->strvec, index);

	  if (vector_active (descvec) == 1)
	    {
	      struct desc *desc = vector_slot (descvec, 0);

	      if (CMD_VARARG (desc->cmd))
		take = varflag = 1 ;
	      else
		take = (CMD_VARIABLE (desc->cmd) || CMD_OPTION (desc->cmd)) ;
	    }
	  else
	    take = 1 ;
	}

      if (take)
        {
          if (argc >= CMD_ARGC_MAX)
            return CMD_ERR_EXCEED_ARGC_MAX ;
          parsed->argv[argc++] = vector_slot (vline, ivl);
        } ;
    } ;

  /* Everything checks out... ready to execute command                  */
  parsed->cmd  = matched_element ;
  parsed->argc = argc ;

  if (parsed->cmd->daemon)
    return CMD_SUCCESS_DAEMON;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", or any of its ancestors.
 *
 * Takes the node to start in from parsed->cnode.
 *
 * Returns:  CMD_SUCCESS => successfully parsed command, and the result is
 *                          in the given parsed structure, ready for execution.
 *
 *                          NB: parsed->cnode may have changed.
 *
 *           CMD_SUCCESS_DAEMON => as CMD_SUCCESS, and the command has a
 *                                 "daemon" value.
 *
 *           otherwise   => some failure to parse
 *
 *                          NB: returns error from attempt to parse in the
 *                              starting parsed->cnode (which is returned
 *                              unchanged).
 *
 * See cmd_parse_command() for more detail.
 */
static int
cmd_parse_command_tree(vector vline, int first, struct cmd_parsed* parsed,
                                                       enum cmd_parse_type type)
{
  int             ret ;
  int             first_ret ;
  enum node_type  first_node ;

  /* Try in the current node                                            */
  ret = cmd_parse_command(vline, first, parsed, type) ;

  if ((ret == CMD_SUCCESS) || (ret == CMD_SUCCESS_DAEMON))
    return ret ;                /* done if found command        */

  /* Try in parent node(s)                                              */
  first_node = parsed->cnode ;
  first_ret  = ret ;

  while (parsed->cnode <= CONFIG_NODE)
    {
      parsed->cnode = node_parent(parsed->cnode) ;
      ret = cmd_parse_command(vline, first, parsed, type) ;

      if ((ret == CMD_SUCCESS) || (ret == CMD_SUCCESS_DAEMON))
        return ret ;            /* done if found command        */
    } ;

  parsed->cnode = first_node ;  /* restore node state           */
  return first_ret ;            /* return original result       */
}

/*------------------------------------------------------------------------------
 * Prepare to parse command -- deal with possible "do" shortcut.
 *
 * Initialises parsed structure, setting cnode and onode to vty->node.
 *
 * Checks to see if this is a valid "do" shortcut, and adjusts cnode if so.
 *
 * Returns: 0 => not "do" -- first word of actual command is first word
 *          1 => is "do"  -- first word of actual command is second word
 */
static int
cmd_pre_command(struct vty* vty, struct cmd_parsed* parsed, vector vline)
{
  int first ;

  parsed->onode = parsed->cnode = vty_get_node(vty) ;

  parsed->cmd  = NULL ;
  parsed->argc = 0 ;

  /* 'do' forces command to be parsed in ENABLE_NODE (if allowed)       */
  if (cmd_try_do_shortcut(parsed->cnode, vector_slot(vline, 0)))
    {
      parsed->cnode = ENABLE_NODE ;
      parsed->do_shortcut = 1 ;
      first = 1 ;
    }
  else
    {
      parsed->do_shortcut = 0 ;
      first = 0 ;
    } ;

  return first ;
} ;

/*------------------------------------------------------------------------------
 * Tidy up after executing command.
 *
 * This is separated out so that can be called when queued command completes.
 *
 * If have just processed a "do" shortcut command, and it has not set the
 * vty->node to something other than ENABLE_NODE, then restore to the original
 * state.
 *
 * Arguments: ret    = CMD_XXXX  -- NB: CMD_QUEUED => command revoked
 *            action = true unless command return is being revoked
 */
extern void
cmd_post_command(struct vty* vty, struct cmd_parsed* parsed, int ret,
                                                             int action)
{
  if (parsed->do_shortcut)
    {
      if (vty_get_node(vty) == ENABLE_NODE)
        vty_set_node(vty, parsed->onode) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Execute command:
 *
 *   -- use cmd_parse_completion type parsing.
 *
 *   -- unless vtysh: if does not parse in current node, try ancestors
 *
 *      If does not find in any ancestor, return error from current node.
 *
 *   -- implement the "do" shortcut
 *
 * If qpthreads_enabled, then may queue the command rather than execute it
 * here.
 *
 * The vty->node may be changed during the execution of the command, and may
 * be returned changed once the command has completed.
 */
extern int
cmd_execute_command (vector vline, struct vty *vty,
                     struct cmd_element **cmd, qpn_nexus to_nexus,
                                               qpn_nexus from_nexus, int vtysh)
{
  int ret ;
  int first ;
  struct cmd_parsed parsed ;

  /* Set up parsed structure & deal with "do" shortcut prefix           */
  first = cmd_pre_command(vty, &parsed, vline) ;

  /* Try to parse in parsed.cnode node or, if required, ancestors thereof.  */
  if (vtysh)
    ret = cmd_parse_command(vline, first, &parsed, cmd_parse_completion) ;
  else
    ret = cmd_parse_command_tree(vline, first, &parsed, cmd_parse_completion) ;

  if (cmd)
    *cmd = parsed.cmd ;         /* for vtysh                            */

  if (ret != CMD_SUCCESS)
    return ret ;                /* NB: CMD_SUCCESS_DAEMON exits here    */

  /* Execute the parsed command                                         */

  vty_set_node(vty, parsed.cnode) ;

  if (qpthreads_enabled && !(parsed.cmd->attr & CMD_ATTR_CALL))
    {
      /* Don't do it now, but send to bgp qpthread */
      cq_enqueue(vty, &parsed, to_nexus, from_nexus) ;
      ret = CMD_QUEUED ;
    }
  else
    {
      ret = (*(parsed.cmd->func))(parsed.cmd, vty, parsed.argc, parsed.argv) ;

      cmd_post_command(vty, &parsed, ret, 1) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Execute command by argument readline.
 *
 *   -- use cmd_parse_strict type parsing.
 *
 *   -- unless vtysh: if does not parse in current node, try ancestors
 *
 *      If does not find in any ancestor, return error from current node.
 *
 *   -- does NOT implement the "do" shortcut
 *
 * At all times executes the command immediately (no queueing, even if
 * qpthreads_enabled).
 *
 * The vty->node may be changed either when parsing or executing the command.
 */
extern int
cmd_execute_command_strict (vector vline, struct vty *vty,
			    struct cmd_element **cmd, int vtysh)
{
#if 0                   /* replaced by cmd_parse_command()      */
  unsigned int i;
  unsigned int index;
  vector cmd_vector;
  struct cmd_element *cmd_element;
  struct cmd_element *matched_element;
  unsigned int matched_count, incomplete_count;
  int argc;
  const char *argv[CMD_ARGC_MAX];
  int varflag;
  enum match_type match = 0;
  char *command;

  /* Make copy of command element */
  cmd_vector = vector_copy (cmd_node_vector (cmdvec, vty_get_node(vty)));

  for (index = 0; index < vector_active (vline); index++)
    if ((command = vector_slot (vline, index)))
      {
	int ret;

	match = cmd_filter_by_string (vector_slot (vline, index),
				      cmd_vector, index);

	/* If command meets '.VARARG' then finish matching. */
	if (match == vararg_match)
	  break;

	ret = is_cmd_ambiguous (command, cmd_vector, index, match);
	if (ret == 1)
	  {
	    vector_free (cmd_vector);
	    return CMD_ERR_AMBIGUOUS;
	  }
	if (ret == 2)
	  {
	    vector_free (cmd_vector);
	    return CMD_ERR_NO_MATCH;
	  }
      }

  /* Check matched count. */
  matched_element = NULL;
  matched_count = 0;
  incomplete_count = 0;
  for (i = 0; i < vector_active (cmd_vector); i++)
    if (vector_slot (cmd_vector, i) != NULL)
      {
	cmd_element = vector_slot (cmd_vector, i);

	if (match == vararg_match || index >= cmd_element->cmdsize)
	  {
	    matched_element = cmd_element;
	    matched_count++;
	  }
	else
	  incomplete_count++;
      }

  /* Finish of using cmd_vector. */
  vector_free (cmd_vector);

  /* To execute command, matched_count must be 1. */
  if (matched_count == 0)
    {
      if (incomplete_count)
	return CMD_ERR_INCOMPLETE;
      else
	return CMD_ERR_NO_MATCH;
    }

  if (matched_count > 1)
    return CMD_ERR_AMBIGUOUS;

  /* Argument treatment */
  varflag = 0;
  argc = 0;

  for (i = 0; i < vector_active (vline); i++)
    {
      if (varflag)
	argv[argc++] = vector_slot (vline, i);
      else
	{
	  vector descvec = vector_slot (matched_element->strvec, i);

	  if (vector_active (descvec) == 1)
	    {
	      struct desc *desc = vector_slot (descvec, 0);

	      if (CMD_VARARG (desc->cmd))
		varflag = 1;

	      if (varflag || CMD_VARIABLE (desc->cmd) || CMD_OPTION (desc->cmd))
		argv[argc++] = vector_slot (vline, i);
	    }
	  else
	    argv[argc++] = vector_slot (vline, i);
	}

      if (argc >= CMD_ARGC_MAX)
	return CMD_ERR_EXCEED_ARGC_MAX;
    }
#endif

  struct cmd_parsed  parsed ;
  int ret ;

  /* Try to parse in parsed.cnode node or, if required, ancestors thereof.  */
  if (vtysh)
    ret = cmd_parse_command(vline, 0, &parsed, cmd_parse_strict) ;
  else
    ret = cmd_parse_command_tree(vline, 0, &parsed, cmd_parse_strict) ;

  if (cmd)
    *cmd = parsed.cmd ;         /* for vtysh                            */

  if (ret != CMD_SUCCESS)
    return ret ;                /* NB: CMD_SUCCESS_DAEMON exits here    */

  /* Now execute matched command        */
  vty_set_node(vty, parsed.cnode) ;

  return (*(parsed.cmd->func)) (parsed.cmd, vty, parsed.argc, parsed.argv);
}

/*------------------------------------------------------------------------------
 * Read configuration from file.
 *
 */
int
config_from_file (struct vty *vty, FILE *fp, void (*after_first_cmd)(void),
                                                                   qstring buf)
{
  int ret;
  vector vline;
  int first_cmd = 1;

  while (fgets (buf->body, buf->size, fp))
    {
      vline = cmd_make_strvec (buf->body);

      /* In case of comment line                                */
      if (vline == NULL)
	continue;

      /* Execute configuration command : this is strict match   */
      ret = cmd_execute_command_strict (vline, vty, NULL, 0);

      /* special handling for after the first command           */
      if (first_cmd && after_first_cmd)
        {
          after_first_cmd();
          first_cmd = 0;
        }

      cmd_free_strvec (vline);

      /* TODO: why does config file not stop on CMD_WARNING ??  */
      if (ret != CMD_SUCCESS && ret != CMD_WARNING
                             && ret != CMD_ERR_NOTHING_TODO)
	return ret;
    } ;

  return CMD_SUCCESS;
}

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
  if ((host.enable == NULL && host.enable_encrypt == NULL) ||
      vty_get_type(vty) == VTY_SHELL_SERV)
    vty_set_node(vty, ENABLE_NODE);
  else
    vty_set_node(vty, AUTH_ENABLE_NODE);

  return CMD_SUCCESS;
}

/* Disable command */
DEFUN_CALL (disable,
       config_disable_cmd,
       "disable",
       "Turn off privileged mode command\n")
{
  if (vty_get_node(vty) == ENABLE_NODE)
    vty_set_node(vty, VIEW_NODE);
  return CMD_SUCCESS;
}

/* Down vty node level. */
DEFUN_CALL (config_exit,
       config_exit_cmd,
       "exit",
       "Exit current mode and down to previous mode\n")
{
  vty_cmd_exit(vty) ;
  return CMD_SUCCESS;
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
       "End current mode and change to enable mode.")
{
  vty_cmd_end(vty) ;
  return CMD_SUCCESS;
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
  struct cmd_node *cnode = vector_slot (cmdvec, vty_get_node(vty));
  struct cmd_element *cmd;

  for (i = 0; i < vector_active (cnode->cmd_vector); i++)
    if ((cmd = vector_slot (cnode->cmd_vector, i)) != NULL
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
  struct cmd_node *node;
  char *config_file;
  char *config_file_tmp = NULL;
  char *config_file_sav = NULL;
  int ret = CMD_WARNING;
  struct vty *file_vty;

  /* Check and see if we are operating under vtysh configuration */
  if (host.config == NULL)
    {
      vty_out (vty, "Can't save to configuration file, using vtysh.%s",
	       VTY_NEWLINE);
      return CMD_WARNING;
    }

  /* Get filename. */
  config_file = host.config;

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
      vty_out (vty, "Can't open configuration file %s.%s", config_file_tmp,
	       VTY_NEWLINE);
      goto finished;
    }

  /* Make vty for configuration file. */
  file_vty = vty_new (fd, VTY_FILE);

  /* Config file header print. */
  vty_out (file_vty, "!\n! Zebra configuration saved from vty\n!   ");
  vty_time_print (file_vty, 1);
  vty_out (file_vty, "!\n");

  for (i = 0; i < vector_active (cmdvec); i++)
    if ((node = vector_slot (cmdvec, i)) && node->func)
      {
	if ((*node->func) (file_vty))
	  vty_out (file_vty, "!\n");
      }
  vty_close (file_vty);

  if (unlink (config_file_sav) != 0)
    if (errno != ENOENT)
      {
	vty_out (vty, "Can't unlink backup configuration file %s.%s", config_file_sav,
		 VTY_NEWLINE);
        goto finished;
      }
  if (link (config_file, config_file_sav) != 0)
    {
      vty_out (vty, "Can't backup old configuration file %s.%s", config_file_sav,
	        VTY_NEWLINE);
      goto finished;
    }
  sync ();
  if (unlink (config_file) != 0)
    {
      vty_out (vty, "Can't unlink configuration file %s.%s", config_file,
	        VTY_NEWLINE);
      goto finished;
    }
  if (link (config_file_tmp, config_file) != 0)
    {
      vty_out (vty, "Can't save configuration file %s.%s", config_file,
	       VTY_NEWLINE);
      goto finished;
    }
  sync ();

  if (chmod (config_file, CONFIGFILE_MASK) != 0)
    {
      vty_out (vty, "Can't chmod configuration file %s: %s (%d).%s",
	config_file, safe_strerror(errno), errno, VTY_NEWLINE);
      goto finished;
    }

  vty_out (vty, "Configuration saved to %s%s", config_file,
	   VTY_NEWLINE);
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
  struct cmd_node *node;

  if (vty_get_type(vty) == VTY_SHELL_SERV)
    {
      for (i = 0; i < vector_active (cmdvec); i++)
	if ((node = vector_slot (cmdvec, i)) && node->func && node->vtysh)
	  {
	    if ((*node->func) (vty))
	      vty_out (vty, "!%s", VTY_NEWLINE);
	  }
    }
  else
    {
      vty_out (vty, "%sCurrent configuration:%s", VTY_NEWLINE,
	       VTY_NEWLINE);
      vty_out (vty, "!%s", VTY_NEWLINE);

      for (i = 0; i < vector_active (cmdvec); i++)
	if ((node = vector_slot (cmdvec, i)) && node->func)
	  {
	    if ((*node->func) (vty))
	      vty_out (vty, "!%s", VTY_NEWLINE);
	  }
      vty_out (vty, "end%s",VTY_NEWLINE);
    }
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

  confp = fopen (host.config, "r");
  if (confp == NULL)
    {
      vty_out (vty, "Can't open configuration file [%s]%s",
	       host.config, VTY_NEWLINE);
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
      vty_out (vty, "Please specify string starting with alphabet%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  VTY_LOCK() ;

  if (host.name)
    XFREE (MTYPE_HOST, host.name);

  host.name = XSTRDUP (MTYPE_HOST, argv[0]);
  uty_set_host_name(host.name) ;

  VTY_UNLOCK() ;

  return CMD_SUCCESS;
}

DEFUN_CALL (config_no_hostname,
       no_hostname_cmd,
       "no hostname [HOSTNAME]",
       NO_STR
       "Reset system's network name\n"
       "Host name of this router\n")
{
  VTY_LOCK() ;

  if (host.name)
    XFREE (MTYPE_HOST, host.name);
  host.name = NULL;
  uty_set_host_name(host.name) ;

  VTY_UNLOCK() ;

  return CMD_SUCCESS;
}

/* VTY interface password set. */
DEFUN_CALL (config_password, password_cmd,
       "password (8|) WORD",
       "Assign the terminal connection password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN line password string\n")
{
  /* Argument check. */
  if (argc == 0)
    {
      vty_out (vty, "Please specify password.%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  if (argc == 2)
    {
      if (*argv[0] == '8')
	{
	  if (host.password)
	    XFREE (MTYPE_HOST, host.password);
	  host.password = NULL;
	  if (host.password_encrypt)
	    XFREE (MTYPE_HOST, host.password_encrypt);
	  host.password_encrypt = XSTRDUP (MTYPE_HOST, argv[1]);
	  return CMD_SUCCESS;
	}
      else
	{
	  vty_out (vty, "Unknown encryption type.%s", VTY_NEWLINE);
	  return CMD_WARNING;
	}
    }

  if (!isalnum ((int) *argv[0]))
    {
      vty_out (vty,
	       "Please specify string starting with alphanumeric%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  if (host.password)
    XFREE (MTYPE_HOST, host.password);
  host.password = NULL;

  if (host.encrypt)
    {
      if (host.password_encrypt)
	XFREE (MTYPE_HOST, host.password_encrypt);
      host.password_encrypt = XSTRDUP (MTYPE_HOST, zencrypt (argv[0]));
    }
  else
    host.password = XSTRDUP (MTYPE_HOST, argv[0]);

  return CMD_SUCCESS;
}

ALIAS_CALL (config_password, password_text_cmd,
       "password LINE",
       "Assign the terminal connection password\n"
       "The UNENCRYPTED (cleartext) line password\n")

/* VTY enable password set. */
DEFUN_CALL (config_enable_password, enable_password_cmd,
       "enable password (8|) WORD",
       "Modify enable password parameters\n"
       "Assign the privileged level password\n"
       "Specifies a HIDDEN password will follow\n"
       "dummy string \n"
       "The HIDDEN 'enable' password string\n")
{
  /* Argument check. */
  if (argc == 0)
    {
      vty_out (vty, "Please specify password.%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  /* Crypt type is specified. */
  if (argc == 2)
    {
      if (*argv[0] == '8')
	{
	  if (host.enable)
	    XFREE (MTYPE_HOST, host.enable);
	  host.enable = NULL;

	  if (host.enable_encrypt)
	    XFREE (MTYPE_HOST, host.enable_encrypt);
	  host.enable_encrypt = XSTRDUP (MTYPE_HOST, argv[1]);

	  return CMD_SUCCESS;
	}
      else
	{
	  vty_out (vty, "Unknown encryption type.%s", VTY_NEWLINE);
	  return CMD_WARNING;
	}
    }

  if (!isalnum ((int) *argv[0]))
    {
      vty_out (vty,
	       "Please specify string starting with alphanumeric%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  if (host.enable)
    XFREE (MTYPE_HOST, host.enable);
  host.enable = NULL;

  /* Plain password input. */
  if (host.encrypt)
    {
      if (host.enable_encrypt)
	XFREE (MTYPE_HOST, host.enable_encrypt);
      host.enable_encrypt = XSTRDUP (MTYPE_HOST, zencrypt (argv[0]));
    }
  else
    host.enable = XSTRDUP (MTYPE_HOST, argv[0]);

  return CMD_SUCCESS;
}

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
  if (host.enable)
    XFREE (MTYPE_HOST, host.enable);
  host.enable = NULL;

  if (host.enable_encrypt)
    XFREE (MTYPE_HOST, host.enable_encrypt);
  host.enable_encrypt = NULL;

  return CMD_SUCCESS;
}

DEFUN_CALL (service_password_encrypt,
       service_password_encrypt_cmd,
       "service password-encryption",
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  if (host.encrypt)
    return CMD_SUCCESS;

  host.encrypt = 1;

  if (host.password)
    {
      if (host.password_encrypt)
	XFREE (MTYPE_HOST, host.password_encrypt);
      host.password_encrypt = XSTRDUP (MTYPE_HOST, zencrypt (host.password));
    }
  if (host.enable)
    {
      if (host.enable_encrypt)
	XFREE (MTYPE_HOST, host.enable_encrypt);
      host.enable_encrypt = XSTRDUP (MTYPE_HOST, zencrypt (host.enable));
    }

  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_password_encrypt,
       no_service_password_encrypt_cmd,
       "no service password-encryption",
       NO_STR
       "Set up miscellaneous service\n"
       "Enable encrypted passwords\n")
{
  if (! host.encrypt)
    return CMD_SUCCESS;

  host.encrypt = 0;

  if (host.password_encrypt)
    XFREE (MTYPE_HOST, host.password_encrypt);
  host.password_encrypt = NULL;

  if (host.enable_encrypt)
    XFREE (MTYPE_HOST, host.enable_encrypt);
  host.enable_encrypt = NULL;

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
  host.lines = lines;

  return CMD_SUCCESS;
}

DEFUN_CALL (no_service_terminal_length, no_service_terminal_length_cmd,
       "no service terminal-length [<0-512>]",
       NO_STR
       "Set up miscellaneous service\n"
       "System wide terminal length configuration\n"
       "Number of lines of VTY (0 means no line control)\n")
{
  host.lines = -1;
  return CMD_SUCCESS;
}

DEFUN_HID_CALL (do_echo,
	      echo_cmd,
	      "echo .MESSAGE",
	      "Echo a message back to the vty\n"
	      "The message to echo\n")
{
  char *message;

  vty_out (vty, "%s%s", ((message = argv_concat(argv, argc, 0)) ? message : ""),
	   VTY_NEWLINE);
  if (message)
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
  if (message)
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
  host.motd = default_motd;
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
void
host_config_set (char *filename)
{
  if (host.config)
    XFREE (MTYPE_HOST, host.config);
  host.config = XSTRDUP (MTYPE_HOST, filename);
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
  command_cr = XSTRDUP(MTYPE_STRVEC, "<cr>");
  desc_cr.cmd = command_cr;
  desc_cr.str = XSTRDUP(MTYPE_STRVEC, "");

  /* Allocate initial top vector of commands. */
  cmdvec = vector_init (0);

  /* Default host value settings. */
  host.name = NULL;
  host.password = NULL;
  host.enable = NULL;
  host.logfile = NULL;
  host.config = NULL;
  host.lines = -1;
  host.motd = default_motd;
  host.motdfile = NULL;

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
  unsigned int i, j, k, l;
  struct cmd_node *cmd_node;
  struct cmd_element *cmd_element;
  struct desc *desc;
  vector cmd_node_v, cmd_element_v, desc_v;

  if (cmdvec)
    {
      for (i = 0; i < vector_active (cmdvec); i++)
        if ((cmd_node = vector_slot (cmdvec, i)) != NULL)
          {
            cmd_node_v = cmd_node->cmd_vector;

            for (j = 0; j < vector_active (cmd_node_v); j++)
              if ((cmd_element = vector_slot (cmd_node_v, j)) != NULL &&
                  cmd_element->strvec != NULL)
                {
                  cmd_element_v = cmd_element->strvec;

                  for (k = 0; k < vector_active (cmd_element_v); k++)
                    if ((desc_v = vector_slot (cmd_element_v, k)) != NULL)
                      {
                        for (l = 0; l < vector_active (desc_v); l++)
                          if ((desc = vector_slot (desc_v, l)) != NULL)
                            {
                              if (desc->cmd)
                                XFREE (MTYPE_STRVEC, desc->cmd);
                              if (desc->str)
                                XFREE (MTYPE_STRVEC, desc->str);

                              XFREE (MTYPE_DESC, desc);
                            }
                        vector_free (desc_v);
                      }

                  cmd_element->strvec = NULL;
                  vector_free (cmd_element_v);
                }

            vector_free (cmd_node_v);
          }

      vector_free (cmdvec);
      cmdvec = NULL;
    }

  if (command_cr)
    XFREE(MTYPE_STRVEC, command_cr);
  if (desc_cr.str)
    XFREE(MTYPE_STRVEC, desc_cr.str);
  if (host.name)
    XFREE (MTYPE_HOST, host.name);
  if (host.password)
    XFREE (MTYPE_HOST, host.password);
  if (host.password_encrypt)
    XFREE (MTYPE_HOST, host.password_encrypt);
  if (host.enable)
    XFREE (MTYPE_HOST, host.enable);
  if (host.enable_encrypt)
    XFREE (MTYPE_HOST, host.enable_encrypt);
  if (host.logfile)
    XFREE (MTYPE_HOST, host.logfile);
  if (host.motdfile)
    XFREE (MTYPE_HOST, host.motdfile);
  if (host.config)
    XFREE (MTYPE_HOST, host.config);
}

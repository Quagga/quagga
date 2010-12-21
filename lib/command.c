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
#include "command_execute.h"
#include "workqueue.h"
#include "command_queue.h"
#include "command_parse.h"

/* Command vector which includes some level of command lists. Normally
   each daemon maintains each own cmdvec. */
vector cmdvec = NULL;

struct desc desc_cr;
char *command_cr = NULL;

/* Host information structure. */
struct host host;

/* Store of qstrings, used for parsing.         */
token_vector_t spare_token_strings ;

/* Standard command node structures. */
static struct cmd_node auth_node =
{
  .node  = AUTH_NODE,
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
static const char *default_motd =
"\r\n\
Hello, this is " QUAGGA_PROGNAME " (version " QUAGGA_VERSION ").\r\n\
" QUAGGA_COPYRIGHT "\r\n\
\r\n";

#ifdef QDEBUG
const char *debug_banner =
    QUAGGA_PROGNAME " version " QUAGGA_VERSION " QDEBUG=" QDEBUG " "
    __DATE__ " " __TIME__;
#endif

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

static struct cmd_element cmd_pipe =
{
  .string    = "< or <|",       /* Dummy                                */
  .func      = cmd_pipe_func,
  .doc       = "Pipe input to command processor",
  .daemon    = 0,
  .strvec    = NULL,
  .cmdsize   = 0,
  .config    = NULL,
  .subconfig = NULL,
  .attr      = CMD_ATTR_SIMPLE,
} ;


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


/* Utility function to concatenate argv argument into a single string
   with inserting ' ' character between each argument.  */
char *
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
  const struct cmd_element *a = *(struct cmd_element * const *)p;
  const struct cmd_element *b = *(struct cmd_element * const *)q;

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
cmp_node (const struct cmd_element **a, const struct cmd_element **b)
{
  return strcmp ((*a)->string, (*b)->string);
}

static int
cmp_desc (const struct desc **a, const struct desc **b)
{
  return strcmp ((*a)->cmd, (*b)->cmd);
}

/* Sort each node's command element according to command string. */
void
sort_node ()
{
  unsigned int i ;

  for (i = 0; i < vector_length(cmdvec); i++)
    {
      struct cmd_node *cnode;
      vector cmd_vector ;
      unsigned int j;

      cnode = vector_get_item(cmdvec, i) ;

      if (cnode == NULL)
        continue ;

      cmd_vector = cnode->cmd_vector;
      if (cmd_vector == NULL)
        continue ;

      vector_sort(cmd_vector, (vector_sort_cmp*)cmp_node) ;

      for (j = 0; j < vector_length(cmd_vector); j++)
        {
          struct cmd_element *cmd_element ;
          vector descvec ;

          cmd_element = vector_get_item (cmd_vector, j);
          if (cmd_element == NULL)
            continue ;

          descvec = vector_get_last_item(cmd_element->strvec) ;
          if (descvec == NULL)
            continue ;

          vector_sort(descvec, (vector_sort_cmp*)cmp_desc) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Take string and break it into tokens -- see cmd_make_tokens().
 *
 * Returns:  NULL => empty line (after white-space trimming) or comment line.
 *      otherwise: is vector containing one or more tokens in qstrings.
 */
extern vector
cmd_make_strvec (const char *string)
{
  return cmd_tokenise(NULL, string) ;
#error sort this one out
} ;

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
  while((cp = vector_ream(strvec, free_it)) != NULL)
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

      while (! (isspace ((int) *cp) || *cp == ')' || *cp == '|') && *cp != '\0')
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

  for (i = 0; i < vector_length(strvec); i++)
    {
      vector descvec;

      descvec = vector_get_item (strvec, i) ;
      if (descvec == NULL)
        continue ;

      if (vector_length(descvec) == 1)
        {
          struct desc *desc;

          desc = vector_get_item(descvec, 0) ;
          if (desc != NULL)
            if (desc->cmd == NULL || CMD_OPTION (desc->cmd))
              break ;
        }
      size++;
    } ;

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
    cnode = vector_get_item (cmdvec, node);

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

  cnode = vector_get_item (cmdvec, ntype);

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

static const unsigned char itoa64[] =
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
  struct cmd_node *cnode = vector_get_item (v, ntype);
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
 * Make strict or completion match and return match type flag.
 *
 * Takes:   command   -- address of candidate token
 *          cmd_v     -- vector of commands that is being reduced/filtered
 *          index     -- index of token (position in line -- 0 == first)
 *          min_match -- any_match    => allow partial matching
 *                       exact_match  => must match completely
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
cmd_filter(const char *command, vector cmd_v, unsigned int index,
                                                         match_type_t min_match)
{
  unsigned int i;
  unsigned int k;
  enum match_type best_match;
  size_t c_len ;

  best_match = no_match ;
  c_len = strlen(command) ;

  /* If command and cmd_element string do match, keep in vector         */
  k = 0 ;
  for (i = 0; i < vector_length (cmd_v); i++)
    {
      const char *str;
      struct cmd_element *cmd_element;
      vector descvec;
      struct desc *desc;
      unsigned int j;
      bool matched ;

      cmd_element = vector_get_item(cmd_v, i) ;

      /* Skip past NULL cmd_v entries (just in case)                    */
      if (cmd_element == NULL)
        continue ;

      /* Discard cmd_v entry that has no token at the current position  */
      descvec = vector_get_item(cmd_element->strvec, index) ;
      if (descvec == NULL)
        continue ;

      /* See if get any sort of match at current position               */
      matched = 0 ;
      for (j = 0; j < vector_length (descvec); j++)
        {
          desc = vector_get_item(descvec, j) ;
          if (desc == NULL)
            continue ;

          str = desc->cmd;

          if (CMD_VARARG (str))
            {
              if (best_match < vararg_match)
                best_match = vararg_match;
              matched = true ;
            }
          else if (CMD_RANGE (str))
            {
              if (cmd_range_match (str, command))
                {
                  if (best_match < range_match)
                    best_match = range_match;
                  matched = true ;
                }
            }
#ifdef HAVE_IPV6
          else if (CMD_IPV6 (str))
            {
              if (cmd_ipv6_match (command) >= min_match)
                {
                  if (best_match < ipv6_match)
                    best_match = ipv6_match;
                  matched = true ;
                }
            }
          else if (CMD_IPV6_PREFIX (str))
            {
              if (cmd_ipv6_prefix_match (command) >= min_match)
                {
                  if (best_match < ipv6_prefix_match)
                    best_match = ipv6_prefix_match;
                  matched = true ;
                }
            }
#endif /* HAVE_IPV6  */
          else if (CMD_IPV4 (str))
            {
              if (cmd_ipv4_match (command) >= min_match)
                {
                  if (best_match < ipv4_match)
                    best_match = ipv4_match;
                  matched = true ;
                }
            }
          else if (CMD_IPV4_PREFIX (str))
            {
              if (cmd_ipv4_prefix_match (command) >= min_match)
                {
                  if (best_match < ipv4_prefix_match)
                    best_match = ipv4_prefix_match;
                  matched = true ;
                }
            }
          else if (CMD_OPTION (str) || CMD_VARIABLE (str))
            {
              if (best_match < extend_match)
                best_match = extend_match;
              matched = true ;
            }
          else
            {
              if (strcmp (command, str) == 0)
                {
                  best_match = exact_match ;
                  matched = true ;
                }
              else if (min_match <= partly_match)
                {
                  if (strncmp (command, str, c_len) == 0)
                    {
                      if (best_match < partly_match)
                        best_match = partly_match ;
                      matched = true ;
                    } ;
                } ;
            } ;
        } ;

      /* Keep cmd_element if have a match                               */
      if (matched)
        vector_set_item(cmd_v, k++, cmd_element) ;
    } ;

  vector_set_length(cmd_v, k) ;    /* discard what did not keep    */

  return best_match;
} ;

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
 *
 * NB: it is assumed that cannot find both 1 and 2 states.  But in any case,
 *     returns 1 in preference.
 */
static int
is_cmd_ambiguous (const char *command, vector cmd_v, int index,
                                                           enum match_type type)
{
  unsigned int i;
  unsigned int k;
  int ret ;

  ret = 0 ;             /* all's well so far    */
  k = 0 ;               /* nothing kept, yet    */

  for (i = 0; i < vector_length (cmd_v); i++)
    {
      unsigned int j;
      struct cmd_element *cmd_element;
      const char *str_matched ;
      vector descvec;
      struct desc *desc;
      bool matched ;
      enum match_type mt ;

      cmd_element = vector_get_item (cmd_v, i) ;

      /* Skip past NULL cmd_v entries (just in case)                    */
      if (cmd_element == NULL)
        continue ;

      /* The cmd_v entry MUST have a token at the current position      */
      descvec = vector_get_item (cmd_element->strvec, index) ;
      assert(descvec != NULL) ;

      /* See if have a match against any of the current possibilities
       *
       * str_matched is set the first time get a partial string match,
       *                 or the first time get a number range match.
       *
       *   If get a second partial string match or number range match, then
       *   unless
       */
      str_matched = NULL ;
      matched     = 0;
      for (j = 0; j < vector_length (descvec); j++)
        {
          enum match_type ret;
          const char *str ;

          desc = vector_get_item (descvec, j) ;
          if (desc == NULL)
            continue ;

          str = desc->cmd;

          switch (type)
          {
            case exact_match:
              if (!(CMD_OPTION (str) || CMD_VARIABLE (str))
                                                 && strcmp (command, str) == 0)
                matched = true ;
              break;

            case partly_match:
              if (!(CMD_OPTION (str) || CMD_VARIABLE (str))
                               && strncmp (command, str, strlen (command)) == 0)
                {
                  if (str_matched && (strcmp (str_matched, str) != 0))
                    ret = 1;            /* There is ambiguous match. */
                  else
                    str_matched = str;
                  matched = true ;
                }
              break;

            case range_match:
              if (cmd_range_match (str, command))
                {
                  if (str_matched && strcmp (str_matched, str) != 0)
                    ret = 1;
                  else
                    str_matched = str;
                  matched = true ;
                }
              break;

#ifdef HAVE_IPV6
            case ipv6_match:
              if (CMD_IPV6 (str))
                matched = true ;
              break;

            case ipv6_prefix_match:
              if ((mt = cmd_ipv6_prefix_match (command)) != no_match)
                {
                  if ((mt == partly_match) && (ret != 1))
                    ret = 2;            /* There is incomplete match. */
                  matched = true ;
                }
              break;
#endif /* HAVE_IPV6 */

            case ipv4_match:
              if (CMD_IPV4 (str))
                matched = true ;
              break;

            case ipv4_prefix_match:
              if ((mt = cmd_ipv4_prefix_match (command)) != no_match)
                {
                  if ((mt == partly_match) && (ret != 1))
                    ret = 2;            /* There is incomplete match. */
                  matched = true ;
                }
              break;

            case extend_match:
              if (CMD_OPTION (str) || CMD_VARIABLE (str))
                matched = true ;
              break;

            case no_match:
            default:
              break;
          } ;
        } ;

      /* Keep cmd_element if have a match                               */
      if (matched)
        vector_set_item(cmd_v, k++, cmd_element) ;
    } ;

  vector_set_length(cmd_v, k) ;    /* discard what did not keep    */

  return ret ;
} ;

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
static bool
cmd_unique_string (vector v, const char *str)
{
  unsigned int i;
  char *match;

  for (i = 0; i < vector_length (v); i++)
    if ((match = vector_get_item (v, i)) != NULL)
      if (strcmp (match, str) == 0)
	return 0;
  return 1;
}

/* Compare string to description vector.  If there is same string
   return 1 else return 0. */
static bool
desc_unique_string (vector v, const char *str)
{
  unsigned int i;
  struct desc *desc;

  for (i = 0; i < vector_length (v); i++)
    if ((desc = vector_get_item (v, i)) != NULL)
      if (strcmp (desc->cmd, str) == 0)
	return 1;
  return 0;
}

/*------------------------------------------------------------------------------
 * Special parsing for leading 'do', if current mode allows it.
 *
 * If finds a valid "do", sets current node and do_shortcut flag, and discards
 * the "do" token.
 *
 * Returns: true <=> dealt with the "do"
 *          false => no do, or no do allowed.
 */
static bool
cmd_try_do_shortcut(cmd_parsed parsed)
{
  const char* ts ;

  if (parsed->cnode < MIN_DO_SHORTCUT_NODE)
    return false ;

  ts = cmd_token_string(cmd_token_get(&parsed->tokens, 0)) ;
  if (strcmp("do", ts) != 0)
    return false ;

  parsed->cnode = ENABLE_NODE ;
  parsed->do_shortcut = true ;
  cmd_token_discard(cmd_token_shift(&parsed->tokens)) ;

  return true ;
} ;

/*==============================================================================
 * '?' describe command support.
 */

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
extern vector
cmd_describe_command (const char* line, node_type_t node,
                                                      cmd_return_code_t* status)
{
  vector ret ;
  struct cmd_parsed parsed_s ;
  cmd_parsed        parsed ;
  cmd_token_type_t  tok_total ;

  /* Set up a parser object and tokenise the command line               */
  parsed = cmd_parse_init_new(&parsed_s) ;
  tok_total = cmd_tokenise(parsed, line, node) ;







  /* Stop immediately if line is empty apart from comment               */
  if ((tok_total & ~cmd_tok_comment) == cmd_tok_null)
    return CMD_EMPTY ;          /* NB: parsed->cmd == NULL              */

  /* Level 1 parsing
   *
   * Strip quotes and escapes from all the tokens.
   */
  if (tok_total != cmd_tok_simple)
    {
      ret = cmd_parse_phase_one(parsed) ;
      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* If allowed to 'do', see if there.
   *
   * 'do' forces command to be parsed in ENABLE_NODE (if allowed)
   */
  if (type & cmd_parse_do)
    cmd_try_do_shortcut(parsed) ;




  return cmd_describe_command_real (tokens, node, status);



  static vector
  cmd_describe_command_real (vector tokens, int node, int *status)
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
    if (vector_length (tokens) == 0)
      {
        *status = CMD_ERR_NO_MATCH;
        return NULL;
      }
    else
      index = vector_length (tokens) - 1;

    /* Make copy vector of current node's command vector. */
    cmd_vector = vector_copy (cmd_node_vector (cmdvec, node));

    /* Prepare match vector */
    matchvec = vector_init (INIT_MATCHVEC_SIZE);

    /* Filter commands. */
    /* Only words precedes current word will be checked in this loop. */
    for (i = 0; i < index; i++)
      if ((command = vector_get_item (tokens, i)))
        {
          match = cmd_filter(command, cmd_vector, i, any_match) ;

          if (match == vararg_match)
            {
              struct cmd_element *cmd_element;
              vector descvec;
              unsigned int j, k;

              for (j = 0; j < vector_length (cmd_vector); j++)
                if ((cmd_element = vector_get_item (cmd_vector, j)) != NULL
                    && (vector_length (cmd_element->strvec)))
                  {
                    descvec = vector_get_item (cmd_element->strvec,
                                           vector_length (cmd_element->strvec) - 1);
                    for (k = 0; k < vector_length (descvec); k++)
                      {
                        struct desc *desc = vector_get_item (descvec, k);
                        vector_set (matchvec, desc);
                      }
                  }

              vector_set (matchvec, &desc_cr);
              vector_free (cmd_vector);

              return matchvec;
            } ;

          ret = is_cmd_ambiguous (command, cmd_vector, i, match) ;
          if (ret != 0)
            {
              vector_free (cmd_vector);
              vector_free (matchvec);
              *status = (ret == 1) ? CMD_ERR_AMBIGUOUS
                                   : CMD_ERR_NO_MATCH ;
              return NULL ;
            } ;
        }

    /* Prepare match vector */
    /*  matchvec = vector_init (INIT_MATCHVEC_SIZE); */

    /* Make sure that cmd_vector is filtered based on current word        */
    command = vector_get_item (tokens, index);
    if (command)
      match = cmd_filter(command, cmd_vector, index, any_match);

    /* Make description vector. */
    for (i = 0; i < vector_length (cmd_vector); i++)
      {
        vector strvec ;

        cmd_element = vector_get_item (cmd_vector, i) ;
        if (cmd_element == NULL)
          continue ;

        /* Ignore cmd_element if no tokens at index position.
         *
         * Deal with special case of possible <cr> completion.
         */
        strvec = cmd_element->strvec;
        if (index >= vector_length (strvec))
          {
            if (command == NULL && index == vector_length (strvec))
              {
                if (!desc_unique_string (matchvec, command_cr))
                  vector_push_item(matchvec, &desc_cr);
              }
            continue ;
          } ;

        /* Check if command is completed.                                 */
        unsigned int j;
        vector descvec = vector_get_item (strvec, index);
        struct desc *desc;

        for (j = 0; j < vector_length (descvec); j++)
          if ((desc = vector_get_item (descvec, j)))
            {
              const char *string;

              string = cmd_entry_function_desc (command, desc->cmd);
              if (string)
                {
                  /* Uniqueness check */
                  if (!desc_unique_string (matchvec, string))
                    vector_push_item(matchvec, desc);
                }
            } ;
        } ;

    vector_free (cmd_vector);

    if (vector_length(matchvec) == 0)
      {
        vector_free (matchvec);
        *status = CMD_ERR_NO_MATCH;
        return NULL;
      }

    *status = CMD_SUCCESS;
    return matchvec;
  }


















  cmd_parse_reset(parsed, false) ;

  return
} ;

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
cmd_complete_command_real (vector tokens, int node, int *status)
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

  /* Stop immediately if the tokens is empty.                           */
  if (vector_length (tokens) == 0)
    {
      *status = CMD_ERR_NO_MATCH;
      return NULL;
    }

  /* Take (shallow) copy of cmdvec for given node.                      */
  cmd_v = vector_copy (cmd_node_vector (cmdvec, node));

  /* First, filter upto, but excluding last token                       */
  last_ivl = vector_length (tokens) - 1;

  for (ivl = 0; ivl < last_ivl; ivl++)
    {
      enum match_type match;
      int ret;

      /* TODO: does this test make any sense ?                          */
      if ((token = vector_get_item (tokens, ivl)) == NULL)
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
  token = vector_get_item(tokens, last_ivl) ;  /* is now the last token  */

  for (i = 0; i < vector_length (cmd_v); i++)
    {
      unsigned int j;
      const char *string;

      if ((cmd_element = vector_get_item (cmd_v, i)) == NULL)
        continue ;

      descvec = vector_get_item (cmd_element->strvec, index);
      if (descvec == NULL)
        continue ;

      for (j = 0; j < vector_length (descvec); j++)
        {
          desc = vector_get_item (descvec, j) ;
          if (desc == NULL)
            continue ;

          string = cmd_entry_function(token, desc->cmd) ;
          if ((string != NULL) && cmd_unique_string(matchvec, string))
            cmd_add_to_strvec (matchvec, string) ;
        } ;
    } ;

  n = vector_length(matchvec) ; /* number of entries in the matchvec    */

  /* We don't need cmd_v any more.                                      */
  vector_free (cmd_v);

  /* No matched command */
  if (n == 0)
    {
      vector_free (matchvec);

      /* In case of 'command \t' pattern.  Do you need '?' command at
         the end of the line. */
      if (*token == '\0')
	*status = CMD_COMPLETE_ALREADY;
      else
	*status = CMD_ERR_NO_MATCH;
      return NULL;
    }

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

	      matchvec = vector_init (1);
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
cmd_complete_command (vector tokens, int node, int *status)
{
  vector ret;

  if ( cmd_try_do_shortcut(node, vector_get_item(tokens, 0) ) )
    {
      vector shifted_tokens;
      unsigned int index;

      /* We can try it on enable node, cos' the vty is authenticated */

      shifted_tokens = vector_init (vector_count(tokens));
      /* use memcpy? */
      for (index = 1; index < vector_length (tokens); index++)
	{
	  vector_set_index (shifted_tokens, index-1,
	                                         vector_lookup(tokens, index)) ;
	}

      ret = cmd_complete_command_real (shifted_tokens, ENABLE_NODE, status);

      vector_free(shifted_tokens);
      return ret;
  }

  return cmd_complete_command_real (tokens, node, status);
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
    } ;
} ;

/*==============================================================================
 * Parsing of command lines
 */

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", if possible, ready for execution.
 *
 * If 'strict':   use cmd_filter_by_string()
 *   otherwise:   use cmd_filter_by_completion()
 *
 * If 'do':       see if there is a 'do' at the front and proceed accordingly.
 *
 * If 'tree':     move up the node tree to find command if not found in the
 *                current node.
 */

static enum cmd_return_code cmd_parse_phase_one(cmd_parsed parsed) ;
static enum cmd_return_code cmd_parse_phase_two(struct cmd_parsed* parsed,
                                                                  bool strict) ;

/*------------------------------------------------------------------------------
 * Parse a command in the given "node", or (if required) any of its ancestors.
 *
 * Returns:  CMD_SUCCESS => successfully parsed command, and the result is
 *                          in the given parsed structure, ready for execution.
 *
 *                          NB: parsed->cnode may have changed.
 *
 *                          NB: parsed->cmd->daemon => daemon
 *
 *           CMD_EMPTY   => empty or comment line
 *
 *                          NB: parsed->cmd == NULL
 *
 *           CMD_SUCCESS_DAEMON => parsed successfully.  Something for vtysh ??
 *
 *           CMD_ERR_NO_MATCH   )
 *           CMD_ERR_AMBIGUOUS  )  failed to parse
 *           CMD_ERR_INCOMPLETE )
 *
 *                          NB: if has failed to parse in the current node
 *                              and in any ancestor nodes, returns the error
 *                              from the attempt to parse in the current node
 *                              (parsed->cnode which is returned unchanged).
 *
 * NB: the command line MUST be preserved until the parsed command is no
 *     longer required -- no copy is made.
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 *
 * See elsewhere for description of parsed structure.
 */
extern enum cmd_return_code
cmd_parse_command(struct vty* vty, cmd_parse_type_t type)
{
  enum cmd_return_code ret ;
  enum cmd_return_code first_ret ;
  cmd_parsed parsed ;
  cmd_token_type_t tok_total ;
  bool varflag ;
  unsigned int i, ivl ;

  /* Initialise the parsed structure -- assuming no 'do'                */
  if (vty->parsed == NULL)
    parsed = vty->parsed = cmd_parse_init_new(NULL) ;
  else
    {
      parsed = vty->parsed ;

      parsed->cmd         = NULL ;
      parsed->do_shortcut = false ;

      if (parsed->pipes != cmd_pipe_none)
        {
          parsed->pipes = cmd_pipe_none ;
          cmd_empty_parsed_tokens(parsed) ;
        } ;
    } ;

  /* Parse the line into tokens, set parsed->line, ->cnode & ->onode    */
  tok_total = cmd_tokenise(parsed, vty->buf, vty->node) ;

  /* Stop immediately if line is empty apart from comment               */
  if ((tok_total & ~cmd_tok_comment) == cmd_tok_null)
    return CMD_EMPTY ;          /* NB: parsed->cmd == NULL              */

  /* Level 1 parsing
   *
   * Strip quotes and escapes from all the tokens.
   */
  if (tok_total != cmd_tok_simple)
    {
      ret = cmd_parse_phase_one(parsed) ;
      if (ret != CMD_SUCCESS)
        return ret ;
    } ;

  /* If allowed to 'do', see if there.
   *
   * 'do' forces command to be parsed in ENABLE_NODE (if allowed)
   */
  if (type & cmd_parse_do)
    cmd_try_do_shortcut(parsed) ;

  /* Level 2 parsing
   * Try in the current node
   */
  ret = cmd_parse_phase_two(parsed, type)  ;

  if (ret != CMD_SUCCESS)
    {
      if (((type & cmd_parse_tree) == 0) || parsed->do_shortcut)
        return ret ;                /* done if not allowed to walk tree
                                       or just tried to parse a 'do'    */

      /* Try in parent node(s)                                          */
      first_ret  = ret ;

      while (ret != CMD_SUCCESS)
        {
          if (parsed->cnode <= CONFIG_NODE)
            {
              parsed->cnode = parsed->onode ;   /* restore node state       */
              return first_ret ;                /* return original result  */
            } ;

          parsed->cnode = node_parent(parsed->cnode) ;
          ret = cmd_parse_phase_two(parsed, type) ;
        } ;
    } ;

  /* Parsed successfully -- construct the arg_vector                    */

  varflag = false ;
  ivl     = vector_length(parsed->cmd->strvec) ;

  cmd_arg_vector_empty(parsed) ;
  for (i = 0; i < ivl ; i++)
    {
      bool take = varflag ;

      if (!varflag)
        {
          vector descvec = vector_get_item (parsed->cmd->strvec, i);

          if (vector_length (descvec) == 1)
            {
              struct desc *desc = vector_get_item (descvec, 0);

              if (CMD_VARARG (desc->cmd))
                take = varflag = true ;
              else
                take = (CMD_VARIABLE (desc->cmd) || CMD_OPTION (desc->cmd)) ;
            }
          else
            take = true ;
        }

      if (take)
        cmd_arg_vector_push(parsed,
                           cmd_token_value(cmd_token_get(&parsed->tokens, i))) ;
    } ;

  /* Return appropriate form of success                                 */
  return parsed->cmd->daemon ? CMD_SUCCESS_DAEMON
                             : CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Phase 1 of command parsing
 *
 *    * At start of line look for:
 *
 *   * '<'  -- pipe-in command of some sort
 *
 *     Sets the pipe type and the read_pipe_tokens -- all tokens up to
 *     '>', '|', '!' or '#'..
 *
 * Scan for '>', '|', '!' or '#'
 *
 *   * '>' or '|' -- pipe-out command of some sort
 *
 *     Collect type of pipe, and then all tokens up to '!' or '#'
 *
 *
 *
 *   * '!', '#', comment -- discards
 *
 * Returns:  CMD_SUCCESS        -- parsed successfully
 *           CMD_ERR_NO_MATCH   )
 *           CMD_ERR_AMBIGUOUS  )  failed to parse
 *           CMD_ERR_INCOMPLETE )
 */
static enum cmd_return_code
cmd_parse_phase_one(cmd_parsed parsed)
{






} ;

/*------------------------------------------------------------------------------
 * Phase 2 of command parsing
 *
 * Takes a parsed structure, with the:
 *
 *   cnode       -- node to parse in
 *   tokens      -- the line broken into words
 *   do_shortcut -- true if first word is 'do' (to be ignored)
 *
 * and parses either strictly or with command completion.
 *
 * Returns:  CMD_SUCCESS        -- parsed successfully
 *           CMD_ERR_NO_MATCH   )
 *           CMD_ERR_AMBIGUOUS  )  failed to parse
 *           CMD_ERR_INCOMPLETE )
 */
static enum cmd_return_code
cmd_parse_phase_two(cmd_parsed parsed, cmd_parse_type_t type)
{
  unsigned int i ;
  unsigned int ivl ;
  unsigned index ;
  vector cmd_v;
  struct cmd_element *cmd_element;
  struct cmd_element *matched_element;
  unsigned int matched_count, incomplete_count;
  enum match_type match ;
  enum match_type filter_level ;
  const char *command;

  /* Need number of tokens                                      */
  ivl = cmd_token_count(&parsed->tokens) ;

  /* Make copy of command elements.                             */
  cmd_v = vector_copy (cmd_node_vector (cmdvec, parsed->cnode));

  /* Look for an unambiguous result                             */
  filter_level = (type & cmd_parse_strict) ? exact_match : any_match ;
  match = no_match ;            /* in case of emptiness         */
  for (index = 0 ; index < ivl; index++)
    {
      int ret ;

      command = cmd_token_string(cmd_token_get(&parsed->tokens, index)) ;

      match = cmd_filter(command, cmd_v, index, filter_level) ;

      if (match == vararg_match)
        break;

      ret = is_cmd_ambiguous (command, cmd_v, index, match);

      if (ret != 0)
        {
          assert((ret == 1) || (ret == 2)) ;
          vector_free (cmd_v);
          return (ret == 1) ? CMD_ERR_AMBIGUOUS : CMD_ERR_NO_MATCH ;
        }
    } ;

  /* Check matched count.                                       */
  matched_element  = NULL;
  matched_count    = 0;
  incomplete_count = 0;

  for (i = 0; i < vector_length(cmd_v); i++)
    {
      cmd_element = vector_get_item(cmd_v, i) ;
      if (cmd_element == NULL)
        continue ;

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
    } ;

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

  /* Everything checks out... ready to execute command                  */
  parsed->cmd  = matched_element ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Dispatch a parsed command.
 *
 * Returns: command return code.  NB: may be CMD_QUEUED (unless no_queue).
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 */
extern enum cmd_return_code
cmd_dispatch(struct vty* vty, bool no_queue)
{
  cmd_parsed parsed = vty->parsed ;
  enum cmd_return_code ret ;

  if (parsed->cmd == NULL)
    return CMD_SUCCESS ;                /* NULL commands are easy       */

  vty->node = parsed->cnode ;

  if (no_queue || !vty_cli_nexus)
    {
      ret = cmd_dispatch_call(vty) ;
      cmd_post_command(vty, ret) ;
    }
  else
    {
      /* Don't do it now, but send to bgp qpthread */
      if (parsed->cmd->attr & CMD_ATTR_CALL)
        cq_enqueue(vty, vty_cli_nexus) ;
      else
        cq_enqueue(vty, vty_cmd_nexus) ;

      ret = CMD_QUEUED ;
    } ;

  return ret ;
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
 */
extern void
cmd_post_command(struct vty* vty, int ret)
{
  if (vty->parsed->do_shortcut)
    {
      if (vty->node == ENABLE_NODE)
        vty->node = vty->parsed->onode ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Parse and execute a command.
 *
 * The command is given by vty->buf and vty->node.
 *
 * Uses vty->parsed.
 *
 *   -- use strict/completion parsing, as required.
 *
 *   -- parse in current node and in ancestors, as required
 *
 *      If does not find in any ancestor, return error from current node.
 *
 *   -- implement the "do" shortcut, as required
 *
 * If qpthreads_enabled, then may queue the command rather than execute it
 * here.
 *
 * The vty->node may be changed during the execution of the command, and may
 * be returned changed once the command has completed.
 *
 * NB: expects to have free run of everything in the vty structure (except
 *     the contents of the vty_io sub-structure) until the command completes.
 */
extern enum cmd_return_code
cmd_execute_command(struct vty *vty,
                             enum cmd_parse_type type, struct cmd_element **cmd)
{
  enum cmd_return_code ret ;

  /* Try to parse in vty->node or, if required, ancestors thereof.      */
  ret = cmd_parse_command(vty, type) ;

  if (cmd != NULL)
    *cmd = vty->parsed->cmd ;   /* for vtysh                            */

  if      (ret == CMD_SUCCESS)
    ret = cmd_dispatch(vty, cmd_may_queue) ;
  else if (ret == CMD_EMPTY)
    ret = CMD_SUCCESS ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Read configuration from file.
 *
 * In the qpthreads world this assumes that it is running with the vty
 * locked, and that all commands are to be executed directly.
 *
 * If the 'first_cmd' argument is not NULL it is the address of the first
 * command that is expected to appear.  If the first command is not this, then
 * the 'first_cmd' is called with argv == NULL (and argc == 0) to signal the
 * command is being invoked by default.
 *
 * Command processing continues while CMD_SUCCESS is returned by the command
 * parser and command execution.
 *
 * If 'ignore_warning' is set, then any CMD_WARNING returned by command
 * execution is converted to CMD_SUCCESS.  Note that any CMD_WARNING returned
 * by command parsing (or in execution of any default 'first_cmd').
 *
 * Returns: cmd_return_code for last command
 *          vty->buf     is last line processed
 *          vty->lineno  is number of last line processed (1 is first)
 *
 * If the file is empty, will return CMD_SUCCESS.
 *
 * Never returns CMD_EMPTY -- that counts as CMD_SUCCESS.
 *
 * If
 *
 * If return code is not CMD_SUCCESS, the the output buffering contains the
 * output from the last command attempted.
 */
extern enum cmd_return_code
config_from_file (struct vty *vty, FILE *fp, struct cmd_element* first_cmd,
                                              qstring buf, bool ignore_warning)
{
  enum cmd_return_code ret;

  vty->buf    = buf->body ;
  vty->lineno = 0 ;

  ret      = CMD_SUCCESS ;      /* in case file is empty        */
  vty_out_clear(vty) ;

  while (fgets (buf->body, buf->size, fp))
    {
      ++vty->lineno ;

      /* Execute configuration command : this is strict match           */
      ret = cmd_parse_command(vty, cmd_parse_strict + cmd_parse_tree) ;

      if (ret == CMD_EMPTY)
        continue ;              /* skip empty/comment                   */

      if (ret != CMD_SUCCESS)
        break ;                 /* stop on *any* parsing issue          */

      /* special handling before of first command                       */
      if (first_cmd != NULL)
        {
          if (first_cmd != vty->parsed->cmd)
            {
              ret = (*first_cmd->func)(first_cmd, vty, 0, NULL) ;
              if (ret != CMD_SUCCESS)
                break ;         /* stop on *any* issue with "default"   */
            } ;
          first_cmd = NULL ;
        }

      /* Standard command handling                                      */
      ret = cmd_dispatch(vty, cmd_no_queue) ;

      if (ret != CMD_SUCCESS)
        {
          /* Ignore CMD_WARNING if required
           *
           * Ignore CMD_CLOSE at all times.
           */
          if ( ((ret == CMD_WARNING) && ignore_warning)
             || (ret == CMD_CLOSE) )
            ret = CMD_SUCCESS ;   /* in case at EOF     */
          else
            break ;               /* stop               */
        } ;

      vty_out_clear(vty) ;
    } ;

  if (ret == CMD_EMPTY)
    ret = CMD_SUCCESS ;         /* OK if end on empty line              */

  return ret ;
} ;

/*==============================================================================
 */

static cmd_return_code_t cmd_fetch_command(struct vty* vty) ;

/*------------------------------------------------------------------------------
 * Command Loop
 *
 * Read and dispatch commands until can no longer do so for whatever reason:
 *
 *   - reached end of command stream     -- CMD_CLOSE
 *   - encounter error of some kind      -- CMD_WARNING, CMD_ERROR, etc
 *   - waiting for input to arrive       -- CMD_WAIT_INPUT
 *   - waiting for command to complete   -- CMD_QUEUED
 *   - waiting for output to complete    -- ??
 *
 */
extern cmd_return_code_t
cmd_command_loop(struct vty *vty, struct cmd_element* first_cmd,
                                              qstring buf, bool ignore_warning)
{
  cmd_return_code_t ret ;

  vty->buf    = buf->body ;
  vty->lineno = 0 ;


  /*
   *
   */

  vty_out_clear(vty) ;

  while (1) {

    /* Fetch a command line                                             */

    ret = cmd_fetch_command(vty) ;

    if (ret != CMD_SUCCESS)
      break ;

    ++vty->lineno ;

    /* Parse the command line we now have                               */

    ret = cmd_parse_command(vty, cmd_parse_strict + cmd_parse_tree) ;

    if (ret == CMD_EMPTY)
      continue ;                /* skip empty/comment                   */

    if (ret != CMD_SUCCESS)
      break ;                   /* stop on *any* parsing issue          */

    /* special handling before of first command                         */
    if (first_cmd != NULL)
      {
        if (first_cmd != vty->parsed->cmd)
          {
            ret = (*first_cmd->func)(first_cmd, vty, 0, NULL) ;
            if (ret != CMD_SUCCESS)
              break ;           /* stop on *any* issue with "default"   */
          } ;
        first_cmd = NULL ;
      } ;

    /* Reflect command line if required                                 */

    /* Standard command handling                                        */

    ret = cmd_dispatch(vty, cmd_no_queue) ;

    if (ret == CMD_QUEUED)
      break ;

    /* Output Handling.....                                             */


    /* Return code handling....                                         */

    if (ret != CMD_SUCCESS)
      {
        if ((ret == CMD_WARNING) && !ignore_warning)
          break ;
        if (ret != CMD_CLOSE)
          break ;
      } ;

    vty_out_clear(vty) ;
  } ;

  return ret ;
} ;


/*------------------------------------------------------------------------------
 * Fetch the next command.
 *
 *
 *
 *
 */
static cmd_return_code_t
cmd_fetch_command(struct vty* vty)
{




} ;











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
  if ((host.enable == NULL && host.enable_encrypt == NULL) ||
                                                          vty_shell_server(vty))
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
  return vty_cmd_exit(vty) ;
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
  return vty_cmd_end(vty) ;
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
  struct cmd_node *cnode = vector_get_item (cmdvec, vty_get_node(vty));
  struct cmd_element *cmd;

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
  vty_open_config_write(vty, fd) ;

  /* Config file header print. */
  vty_out (vty, "!\n! Zebra configuration saved from vty\n!   ");
  vty_time_print (vty, 1);
  vty_out (vty, "!\n");

  for (i = 0; i < vector_length (cmdvec); i++)
    if ((node = vector_get_item (cmdvec, i)) && node->func)
      {
	if ((*node->func) (vty))
	  vty_out (vty, "!\n");
      }

  err = vty_close_config_write(vty) ;
  close(fd) ;

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
  struct cmd_node *node;

  if (vty_shell_serv(vty))
    {
      for (i = 0; i < vector_length (cmdvec); i++)
	if ((node = vector_get_item (cmdvec, i)) && node->func && node->vtysh)
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

      for (i = 0; i < vector_length (cmdvec); i++)
	if ((node = vector_get_item (cmdvec, i)) && node->func)
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

  /* Allocate initial top vector of commands.           */
  cmdvec = vector_init(0);

  /* Allocate vector of spare qstrings for tokens       */
  cmd_spare_tokens_init() ;

  /* Default host value settings. */
  host.name     = NULL;
  host.password = NULL;
  host.enable   = NULL;
  host.logfile  = NULL;
  host.config   = NULL;
  host.lines    = -1;
  host.motd     = default_motd;
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

  cmd_spare_tokens_free() ;

  if (cmdvec)
    {
      for (i = 0; i < vector_length (cmdvec); i++)
        {
          cmd_node = vector_get_item (cmdvec, i) ;
          if (cmd_node == NULL)
            continue ;

          cmd_node_v = cmd_node->cmd_vector;

          for (j = 0; j < vector_length (cmd_node_v); j++)
            {
              cmd_element = vector_get_item (cmd_node_v, j) ;
              if (cmd_element == NULL)
                continue ;

              cmd_element_v = cmd_element->strvec ;
              if (cmd_element_v == NULL)
                continue ;

              for (k = 0; k < vector_length (cmd_element_v); k++)
                {
                  desc_v = vector_get_item (cmd_element_v, k) ;
                  if (desc_v == NULL)
                    continue ;

                  for (l = 0; l < vector_length (desc_v); l++)
                    {
                      desc = vector_get_item (desc_v, l) ;
                      if (desc == NULL)
                        continue ;

                      if (desc->cmd)
                        XFREE (MTYPE_STRVEC, desc->cmd);
                      if (desc->str)
                        XFREE (MTYPE_STRVEC, desc->str);

                      XFREE (MTYPE_DESC, desc);
                    } ;
                  vector_free (desc_v);
                } ;

              cmd_element->strvec = NULL;
              vector_free (cmd_element_v);
            } ;

          vector_free (cmd_node_v);
        } ;

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

/* BGP community-list and extcommunity-list.
   Copyright (C) 1999 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include <zebra.h>

#include "command.h"
#include "prefix.h"
#include "memory.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_community.h"
#include "bgpd/bgp_ecommunity.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_regex.h"
#include "bgpd/bgp_clist.h"


/* Community-list handler.
 *
 * Contains a set of community and extcommunity filters.
 *
 * NB: have separate name-spaces for community-list and extcommunity-list.
*/
struct community_list_handler
{
  /* Community-list.  */
  struct symbol_table community_list ;

  /* Exteded community-list.  */
  struct symbol_table extcommunity_list ;
};


/* Lookup master structure for community-list or
   extcommunity-list.  */
struct symbol_table*
community_list_master_lookup (struct community_list_handler *ch, int master)
{
  if (ch)
    switch (master)
      {
      case COMMUNITY_LIST_MASTER:
	return &ch->community_list;
      case EXTCOMMUNITY_LIST_MASTER:
	return &ch->extcommunity_list;
      }
  return NULL;
}

/* Allocate a new community list entry.  */
static struct community_entry *
community_entry_new (void)
{
  return XCALLOC (MTYPE_COMMUNITY_LIST_ENTRY, sizeof (struct community_entry));
}

/* Free community list entry.  */
static void
community_entry_free (struct community_entry *entry)
{
  switch (entry->style)
    {
    case COMMUNITY_LIST_STANDARD:
      if (entry->u.com)
        community_free (entry->u.com);
      break;
    case EXTCOMMUNITY_LIST_STANDARD:
      /* In case of standard extcommunity-list, configuration string
         is made by ecommunity_ecom2str().  */
      if (entry->config)
        XFREE (MTYPE_ECOMMUNITY_STR, entry->config);
      if (entry->u.ecom)
        ecommunity_free (entry->u.ecom);
      break;
    case COMMUNITY_LIST_EXPANDED:
    case EXTCOMMUNITY_LIST_EXPANDED:
      if (entry->config)
        XFREE (MTYPE_COMMUNITY_LIST_CONFIG, entry->config);
      if (entry->reg)
        bgp_regex_free (entry->reg);
    default:
      break;
    }
  XFREE (MTYPE_COMMUNITY_LIST_ENTRY, entry);
}

/* Allocate a new community-list.  */
static struct community_list *
community_list_new (void)
{
  return XCALLOC (MTYPE_COMMUNITY_LIST, sizeof (struct community_list));
}

/* Free community-list.  */
static void
community_list_free (struct community_list *list)
{
  XFREE (MTYPE_COMMUNITY_LIST, list);
}
struct community_list *
community_list_lookup (struct community_list_handler *ch,
		       const char *name, int master)
{
  struct symbol_table* table;

  if (!name)
    return NULL;

  table = community_list_master_lookup (ch, master);
  if (!table)
    return NULL;

  return symbol_get_value(symbol_seek(table, name)) ;
}

static struct community_list *
community_list_get (struct community_list_handler *ch,
		    const char *name, int master)
{
  struct community_list *list;
  struct symbol_table* table;
  struct symbol* sym ;

  if (!name)
    return NULL;

  table = community_list_master_lookup (ch, master);
  if (!table)
    return NULL;

  sym = symbol_find(table, name) ;
  list = symbol_get_value(sym) ;
  if (!list)
    {
      /* Allocate new community_list and tie symbol and list together.  */
      list = community_list_new ();

      symbol_set_value(sym, list) ;
      list->sym = symbol_inc_ref(sym) ;
    }

  return list;
}

static void
community_list_delete (struct community_list *list)
{
  struct community_entry *entry, *next;

  /* Easy if the list is not defined !                  */
  if (list == NULL)
    return ;

  /* Free body of list                                  */
  for (entry = list->head; entry; entry = next)
    {
      next = entry->next;
      community_entry_free (entry);
    }

  /* Kill value in related symbol and drop reference.   */
  symbol_unset_value(list->sym) ;
  list->sym = symbol_dec_ref(list->sym) ;

  /* Free community list structure                      */
  community_list_free (list);
}

static int
community_list_empty_p (struct community_list *list)
{
  return (list->head == NULL && list->tail == NULL) ? 1 : 0;
}

/* Add community-list entry to the list.  */
static void
community_list_entry_add (struct community_list *list,
                          struct community_entry *entry)
{
  entry->next = NULL;
  entry->prev = list->tail;

  if (list->tail)
    list->tail->next = entry;
  else
    list->head = entry;
  list->tail = entry;
}

/* Delete community-list entry from the list.  */
static void
community_list_entry_delete (struct community_list *list,
                             struct community_entry *entry, int style)
{
  if (entry->next)
    entry->next->prev = entry->prev;
  else
    list->tail = entry->prev;

  if (entry->prev)
    entry->prev->next = entry->next;
  else
    list->head = entry->next;

  community_entry_free (entry);

  if (community_list_empty_p (list))
    community_list_delete (list);
}

/* Lookup community-list entry from the list.  */
static struct community_entry *
community_list_entry_lookup (struct community_list *list, const void *arg,
                             int direct)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      switch (entry->style)
        {
        case COMMUNITY_LIST_STANDARD:
          if (community_cmp (entry->u.com, arg))
            return entry;
          break;
        case EXTCOMMUNITY_LIST_STANDARD:
          if (ecommunity_cmp (entry->u.ecom, arg))
            return entry;
          break;
        case COMMUNITY_LIST_EXPANDED:
        case EXTCOMMUNITY_LIST_EXPANDED:
          if (strcmp (entry->config, arg) == 0)
            return entry;
          break;
        default:
          break;
        }
    }
  return NULL;
}

/* Internal function to perform regular expression match for community
   attribute.  */
static int
community_regexp_match (struct community *com, regex_t * reg)
{
  const char *str;

  /* When there is no communities attribute it is treated as empty
     string.  */
  if (com == NULL || com->size == 0)
    str = "";
  else
    str = community_str (com);

  /* Regular expression match.  */
  if (regexec (reg, str, 0, NULL, 0) == 0)
    return 1;

  /* No match.  */
  return 0;
}

static int
ecommunity_regexp_match (struct ecommunity *ecom, regex_t * reg)
{
  const char *str;

  /* When there is no communities attribute it is treated as empty
     string.  */
  if (ecom == NULL || ecom->size == 0)
    str = "";
  else
    str = ecommunity_str (ecom);

  /* Regular expression match.  */
  if (regexec (reg, str, 0, NULL, 0) == 0)
    return 1;

  /* No match.  */
  return 0;
}

/* Delete community attribute using regular expression match.  Return
   modified communites attribute.  */
static struct community *
community_regexp_delete (struct community *com, regex_t * reg)
{
  int i;
  u_int32_t comval;
  /* Maximum is "65535:65535" + '\0'. */
  char c[12];
  const char *str;

  if (!com)
    return NULL;

  i = 0;
  while (i < com->size)
    {
      memcpy (&comval, com_nthval (com, i), sizeof (u_int32_t));
      comval = ntohl (comval);

      switch (comval)
        {
        case COMMUNITY_INTERNET:
          str = "internet";
          break;
        case COMMUNITY_NO_EXPORT:
          str = "no-export";
          break;
        case COMMUNITY_NO_ADVERTISE:
          str = "no-advertise";
          break;
        case COMMUNITY_LOCAL_AS:
          str = "local-AS";
          break;
        default:
          sprintf (c, "%d:%d", (comval >> 16) & 0xFFFF, comval & 0xFFFF);
          str = c;
          break;
        }

      if (regexec (reg, str, 0, NULL, 0) == 0)
        community_del_val (com, com_nthval (com, i));
      else
        i++;
    }
  return com;
}

/* When given community attribute matches to the community-list return
   1 else return 0.  */
int
community_list_match (struct community *com, struct community_list *list)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      if (entry->any)
        return entry->direct == COMMUNITY_PERMIT ? 1 : 0;

      if (entry->style == COMMUNITY_LIST_STANDARD)
        {
          if (community_include (entry->u.com, COMMUNITY_INTERNET))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;

          if (community_match (com, entry->u.com))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
      else if (entry->style == COMMUNITY_LIST_EXPANDED)
        {
          if (community_regexp_match (com, entry->reg))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
    }
  return 0;
}

int
ecommunity_list_match (struct ecommunity *ecom, struct community_list *list)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      if (entry->any)
        return entry->direct == COMMUNITY_PERMIT ? 1 : 0;

      if (entry->style == EXTCOMMUNITY_LIST_STANDARD)
        {
          if (ecommunity_match (ecom, entry->u.ecom))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
      else if (entry->style == EXTCOMMUNITY_LIST_EXPANDED)
        {
          if (ecommunity_regexp_match (ecom, entry->reg))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
    }
  return 0;
}

/* Perform exact matching.  In case of expanded community-list, do
   same thing as community_list_match().  */
int
community_list_exact_match (struct community *com,
                            struct community_list *list)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      if (entry->any)
        return entry->direct == COMMUNITY_PERMIT ? 1 : 0;

      if (entry->style == COMMUNITY_LIST_STANDARD)
        {
          if (community_include (entry->u.com, COMMUNITY_INTERNET))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;

          if (community_cmp (com, entry->u.com))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
      else if (entry->style == COMMUNITY_LIST_EXPANDED)
        {
          if (community_regexp_match (com, entry->reg))
            return entry->direct == COMMUNITY_PERMIT ? 1 : 0;
        }
    }
  return 0;
}

/* Delete all permitted communities in the list from com.  */
struct community *
community_list_match_delete (struct community *com,
                             struct community_list *list)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      if (entry->any)
        {
          if (entry->direct == COMMUNITY_PERMIT)
            {
              /* This is a tricky part.  Currently only
               * route_set_community_delete() uses this function.  In the
               * function com->size is zero, it free the community
               * structure.
               */
              com->size = 0;
            }
          return com;
        }

      if ((entry->style == COMMUNITY_LIST_STANDARD)
          && (community_include (entry->u.com, COMMUNITY_INTERNET)
              || community_match (com, entry->u.com) ))
        {
              if (entry->direct == COMMUNITY_PERMIT)
                community_delete (com, entry->u.com);
              else
                break;
        }
      else if ((entry->style == COMMUNITY_LIST_EXPANDED)
               && community_regexp_match (com, entry->reg))
        {
          if (entry->direct == COMMUNITY_PERMIT)
            community_regexp_delete (com, entry->reg);
          else
            break;
        }
    }
  return com;
}

/* To avoid duplicated entry in the community-list, this function
   compares specified entry to existing entry.  */
static int
community_list_dup_check (struct community_list *list,
                          struct community_entry *new)
{
  struct community_entry *entry;

  for (entry = list->head; entry; entry = entry->next)
    {
      if (entry->style != new->style)
        continue;

      if (entry->direct != new->direct)
        continue;

      if (entry->any != new->any)
        continue;

      if (entry->any)
        return 1;

      switch (entry->style)
        {
        case COMMUNITY_LIST_STANDARD:
          if (community_cmp (entry->u.com, new->u.com))
            return 1;
          break;
        case EXTCOMMUNITY_LIST_STANDARD:
          if (ecommunity_cmp (entry->u.ecom, new->u.ecom))
            return 1;
          break;
        case COMMUNITY_LIST_EXPANDED:
        case EXTCOMMUNITY_LIST_EXPANDED:
          if (strcmp (entry->config, new->config) == 0)
            return 1;
          break;
        default:
          break;
        }
    }
  return 0;
}

/* Set community-list.  */
int
community_list_set (struct community_list_handler *ch,
                    const char *name, const char *str, int direct, int style)
{
  struct community_entry *entry = NULL;
  struct community_list *list;
  struct community *com = NULL;
  regex_t *regex = NULL;

  /* Get community list. */
  list = community_list_get (ch, name, COMMUNITY_LIST_MASTER);

  /* When community-list already has entry, new entry should have same
     style.  If you want to have mixed style community-list, you can
     comment out this check.  */
  if (!community_list_empty_p (list))
    {
      struct community_entry *first;

      first = list->head;

      if (style != first->style)
	{
	  return (first->style == COMMUNITY_LIST_STANDARD
		  ? COMMUNITY_LIST_ERR_STANDARD_CONFLICT
		  : COMMUNITY_LIST_ERR_EXPANDED_CONFLICT);
	}
    }

  if (str)
    {
      if (style == COMMUNITY_LIST_STANDARD)
	com = community_str2com (str);
      else
	regex = bgp_regcomp (str);

      if (! com && ! regex)
	return COMMUNITY_LIST_ERR_MALFORMED_VAL;
    }

  entry = community_entry_new ();
  entry->direct = direct;
  entry->style = style;
  entry->any = (str ? 0 : 1);
  entry->u.com = com;
  entry->reg = regex;
  entry->config = (regex ? XSTRDUP (MTYPE_COMMUNITY_LIST_CONFIG, str) : NULL);

  /* Do not put duplicated community entry.  */
  if (community_list_dup_check (list, entry))
    community_entry_free (entry);
  else
    community_list_entry_add (list, entry);

  return 0;
}

/* Unset community-list.  When str is NULL, delete all of
   community-list entry belongs to the specified name.  */
int
community_list_unset (struct community_list_handler *ch,
                      const char *name, const char *str,
                      int direct, int style)
{
  struct community_entry *entry = NULL;
  struct community_list *list;
  struct community *com = NULL;
  regex_t *regex = NULL;

  /* Lookup community list.  */
  list = community_list_lookup (ch, name, COMMUNITY_LIST_MASTER);
  if (list == NULL)
    return COMMUNITY_LIST_ERR_CANT_FIND_LIST;

  /* Delete all of entry belongs to this community-list.  */
  if (!str)
    {
      community_list_delete (list);
      return 0;
    }

  if (style == COMMUNITY_LIST_STANDARD)
    com = community_str2com (str);
  else
    regex = bgp_regcomp (str);

  if (! com && ! regex)
    return COMMUNITY_LIST_ERR_MALFORMED_VAL;

  if (com)
    entry = community_list_entry_lookup (list, com, direct);
  else
    entry = community_list_entry_lookup (list, str, direct);

  if (com)
    community_free (com);
  if (regex)
    bgp_regex_free (regex);

  if (!entry)
    return COMMUNITY_LIST_ERR_CANT_FIND_LIST;

  community_list_entry_delete (list, entry, style);

  return 0;
}

/* Set extcommunity-list.  */
int
extcommunity_list_set (struct community_list_handler *ch,
                       const char *name, const char *str,
                       int direct, int style)
{
  struct community_entry *entry = NULL;
  struct community_list *list;
  struct ecommunity *ecom = NULL;
  regex_t *regex = NULL;

  entry = NULL;

  /* Get community list. */
  list = community_list_get (ch, name, EXTCOMMUNITY_LIST_MASTER);

  /* When community-list already has entry, new entry should have same
     style.  If you want to have mixed style community-list, you can
     comment out this check.  */
  if (!community_list_empty_p (list))
    {
      struct community_entry *first;

      first = list->head;

      if (style != first->style)
	{
	  return (first->style == EXTCOMMUNITY_LIST_STANDARD
		  ? COMMUNITY_LIST_ERR_STANDARD_CONFLICT
		  : COMMUNITY_LIST_ERR_EXPANDED_CONFLICT);
	}
    }

  if (str)
    {
      if (style == EXTCOMMUNITY_LIST_STANDARD)
	ecom = ecommunity_str2com (str, 0, 1);
      else
	regex = bgp_regcomp (str);

      if (! ecom && ! regex)
	return COMMUNITY_LIST_ERR_MALFORMED_VAL;
    }

  if (ecom)
    ecom->str = ecommunity_ecom2str (ecom, ECOMMUNITY_FORMAT_DISPLAY);

  entry = community_entry_new ();
  entry->direct = direct;
  entry->style = style;
  entry->any = (str ? 0 : 1);
  if (ecom)
    entry->config = ecommunity_ecom2str (ecom, ECOMMUNITY_FORMAT_COMMUNITY_LIST);
  else if (regex)
    entry->config = XSTRDUP (MTYPE_COMMUNITY_LIST_CONFIG, str);
  else
    entry->config = NULL;
  entry->u.ecom = ecom;
  entry->reg = regex;

  /* Do not put duplicated community entry.  */
  if (community_list_dup_check (list, entry))
    community_entry_free (entry);
  else
    community_list_entry_add (list, entry);

  return 0;
}

/* Unset extcommunity-list.  When str is NULL, delete all of
   extcommunity-list entry belongs to the specified name.  */
int
extcommunity_list_unset (struct community_list_handler *ch,
                         const char *name, const char *str,
                         int direct, int style)
{
  struct community_entry *entry = NULL;
  struct community_list *list;
  struct ecommunity *ecom = NULL;
  regex_t *regex = NULL;

  /* Lookup extcommunity list.  */
  list = community_list_lookup (ch, name, EXTCOMMUNITY_LIST_MASTER);
  if (list == NULL)
    return COMMUNITY_LIST_ERR_CANT_FIND_LIST;

  /* Delete all of entry belongs to this extcommunity-list.  */
  if (!str)
    {
      community_list_delete (list);
      return 0;
    }

  if (style == EXTCOMMUNITY_LIST_STANDARD)
    ecom = ecommunity_str2com (str, 0, 1);
  else
    regex = bgp_regcomp (str);

  if (! ecom && ! regex)
    return COMMUNITY_LIST_ERR_MALFORMED_VAL;

  if (ecom)
    entry = community_list_entry_lookup (list, ecom, direct);
  else
    entry = community_list_entry_lookup (list, str, direct);

  if (ecom)
    ecommunity_free (ecom);
  if (regex)
    bgp_regex_free (regex);

  if (!entry)
    return COMMUNITY_LIST_ERR_CANT_FIND_LIST;

  community_list_entry_delete (list, entry, style);

  return 0;
}

/* Initializa community-list.  Return community-list handler.  */
struct community_list_handler*
community_list_init (void)
{
  struct community_list_handler *ch;
  ch = XCALLOC (MTYPE_COMMUNITY_LIST_HANDLER,
                sizeof (struct community_list_handler));

  symbol_table_init_new(&ch->community_list,    &ch, 20, 200, NULL, NULL) ;
  symbol_table_init_new(&ch->extcommunity_list, &ch, 20, 200, NULL, NULL) ;

  return ch;
}

/* Terminate community-list.
 *
 * Any references to community lists must be released by the owners of those
 * references.
 */
void
community_list_terminate (struct community_list_handler *ch)
{
  struct community_list *list ;

  while ((list = symbol_table_ream_keep(&ch->community_list)))
    community_list_delete(list) ;

  while ((list = symbol_table_ream_keep(&ch->extcommunity_list)))
    community_list_delete(list) ;

  XFREE (MTYPE_COMMUNITY_LIST_HANDLER, ch);
}

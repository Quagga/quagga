/* Quagga command line parsing -- header
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Recast and extended: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _ZEBRA_COMMAND_PARSE_H
#define _ZEBRA_COMMAND_PARSE_H

#include <zebra.h>
#include "misc.h"

#include "node_type.h"
#include "vector.h"
#include "qstring.h"

/*==============================================================================
 * Parsing of tokens
 */

enum cmd_token_spec
{
  cmd_ts_simple         = 0,

  cmd_ts_keyword,
  cmd_ts_number,
  cmd_ts_word,
  cmd_ts_option,

  cmd_ts_ipv4_addr,
  cmd_ts_ipv4_prefix,

  cmd_ts_ipv6_addr,
  cmd_ts_ipv6_prefix,

  cmd_ts_vararg,



} ;
typedef enum cmd_token_spec cmd_token_spec_t ;

/*==============================================================================
 * Completion match types.
 *
 * NB: the order of these is significant -- in particular as confirmed below.
 */
enum match_type
{
  no_match       = 0,   /* nope                                         */
  any_match      = 1,

  extend_match   = any_match,

  ipv4_prefix_match,
  ipv4_match,
  ipv6_prefix_match,
  ipv6_match,
  range_match,
  vararg_match,

  partly_match,         /* OK as far as it went                         */
  exact_match,          /* Syntactically complete -- greatest match     */

  match_type_count      /* Number of match types                        */
} ;
typedef enum match_type match_type_t ;

CONFIRM(no_match == false) ;
CONFIRM(extend_match == (no_match + 1)) ;
CONFIRM(partly_match == (exact_match - 1)) ;
CONFIRM(exact_match  == (match_type_count - 1)) ;

/*==============================================================================
 *
 */

/* Command parsing options                                              */
enum cmd_parse_type               /* bit significant      */
{
  cmd_parse_completion  = 0,
  cmd_parse_strict      = BIT(0),

  cmd_parse_do          = BIT(1),
  cmd_parse_tree        = BIT(2),
} ;
typedef enum cmd_parse_type cmd_parse_type_t ;

/* Pipe types                                                           */
enum cmd_pipe_type              /* bit significant      */
{
  cmd_pipe_none       = 0,

  cmd_pipe_in_file    = BIT(0),
  cmd_pipe_in_shell   = BIT(1),

  cmd_pipe_reflect    = BIT(4),
  cmd_pipe_output     = BIT(5),
  cmd_pipe_more       = BIT(6),

  cmd_pipe_out_file         = BIT( 8),
  cmd_pipe_out_file_append  = BIT( 9),
  cmd_pipe_out_shell        = BIT(10),
} ;
typedef enum cmd_pipe_type cmd_pipe_type_t ;

/*------------------------------------------------------------------------------
 * Token object -- a qstring and some other properties.
 */
enum cmd_token_type     /* *bit* significant    */
{
  cmd_tok_null          = 0,            /* used for empty lines */

  cmd_tok_simple        = BIT( 0),
  cmd_tok_trailing      = BIT( 1),

  cmd_tok_sq            = BIT( 8),
  cmd_tok_dq            = BIT( 9),      /* '\\' within "..." are not
                                           registered separately.       */
  cmd_tok_esc           = BIT(10),

  cmd_tok_incomplete    = (cmd_tok_sq | cmd_tok_dq | cmd_tok_esc),

  cmd_tok_pipe_in       = BIT(12),
  cmd_tok_pipe_out      = BIT(13),
  cmd_tok_comment       = BIT(14),
} ;
typedef enum cmd_token_type cmd_token_type_t ;

struct token
{
  cmd_token_type_t  type ;
  qstring_t         qs ;
  size_t            tp ;
} ;
typedef struct token  token_t[1] ;
typedef struct token* token ;

/*------------------------------------------------------------------------------
 * Token vector -- a vector of token objects
 */
struct token_vector
{
  vector_t      body ;
} ;

typedef struct token_vector  token_vector_t[1] ;
typedef struct token_vector* token_vector ;

/*------------------------------------------------------------------------------
 * Argument vector -- a vector of const char*
 */
struct arg_vector
{
  vector_t      body ;
} ;
typedef struct arg_vector  arg_vector_t[1] ;
typedef struct arg_vector* arg_vector ;

/*------------------------------------------------------------------------------
 * Parsed command line
 *
 * The current command line is only valid while command is being parsed and
 * executed -- in between it is nonsense.  The pointer can be overwritten at
 * any time -- responsibility for the memory lies elsewhere.
 *
 * The args vector is a set of pointers to the strings in the relevant tokens.
 * This vector is only valid while command is being parsed and executed -- in
 * between it too is nonsense.  The pointers can be overwritten or discarded at
 * any time -- responsibility for the memory lies elsewhere.
 *
 * The token vectors contain the tokens for the current command being parsed
 * and executed.  After the command has completed these vectors can be
 * emptied -- see cmd_empty_parsed_tokens() -- but the next command to be
 * parsed will tidy up before proceeding.
 */
typedef struct cmd_parsed* cmd_parsed ;
struct cmd_parsed
{
  struct cmd_element *cmd ;     /* NULL if empty command
                                        or fails to parse       */

  const char*     line ;        /* the current line             */

  enum node_type  cnode ;       /* node command is in           */
  enum node_type  onode ;       /* node the parser started in   */

  bool            do_shortcut ; /* true => is "do" command      */

  token_vector_t  tokens ;      /* vector of token objects      */
  arg_vector_t    args ;        /* vector of arguments          */

  cmd_pipe_type_t pipes ;       /* if any                       */

  token_vector_t  read_pipe_tokens ;
  token_vector_t  write_pipe_tokens ;
} ;

/* Command dispatch options                                             */
enum {
  cmd_no_queue  = true,
  cmd_may_queue = false,
} ;

/*------------------------------------------------------------------------------
 * Vector of spare token objects -- declared here to allow inlines, defined
 * in command_parse.c
 */
extern token_vector_t spare_tokens ;

/*==============================================================================
 * Prototypes
 */
extern void cmd_spare_tokens_init(void) ;
extern void cmd_spare_tokens_free(void) ;

extern cmd_parsed cmd_parse_init_new(cmd_parsed parsed) ;
extern cmd_parsed cmd_parse_reset(cmd_parsed parsed, bool free_structure) ;

extern cmd_token_type_t cmd_tokenise(cmd_parsed parsed, const char *line,
                                                             node_type_t node) ;
Inline const char* cmd_token_string(token t) ;
Inline int cmd_token_count(token_vector tv) ;
Inline token cmd_token_get(token_vector tv, vector_index_t i) ;
Inline token cmd_token_pop(token_vector tv) ;
Inline void cmd_token_push(token_vector tv, token t) ;
Inline token cmd_token_shift(token_vector tv) ;
Inline void cmd_token_unshift(token_vector tv, token t) ;
Inline token cmd_token_make(void) ;
Inline token cmd_token_new(cmd_token_type_t type, const char* p,
                                                        size_t len, size_t tp) ;

Inline void cmd_empty_token_vector(token_vector tv) ;
Inline void cmd_empty_parsed_tokens(cmd_parsed parsed) ;

Inline bool cmd_token_complete(token t) ;
extern bool cmd_token_do_complete(token t) ;

extern match_type_t cmd_ipv4_match (const char *str) ;
extern match_type_t cmd_ipv4_prefix_match (const char *str) ;
#if HAVE_IPV6
extern match_type_t cmd_ipv6_match (const char *str) ;
extern match_type_t cmd_ipv6_prefix_match (const char *str) ;
#endif
extern bool cmd_range_match (const char *range, const char *str) ;

/*==============================================================================
 * Inline Functions
 */

/*------------------------------------------------------------------------------
 * Get pointer to token value.
 *
 * Returns NULL if token NULL or no string.
 */
Inline char*
cmd_token_value(token t)
{
  return (t == NULL) ? NULL : qs_chars(t->qs) ;
} ;

/*------------------------------------------------------------------------------
 * Get string value of given token.
 *
 * Returns an empty (not NULL) string if token NULL or no string.
 */
Inline const char*
cmd_token_string(token t)
{
  const char* s = cmd_token_value(t) ;
  return (s == NULL) ? "" : s ;
} ;

/*------------------------------------------------------------------------------
 * Get number of tokens in the given token vector
 */
Inline int
cmd_token_count(token_vector tv)
{
  return vector_length(tv->body) ;
} ;

/*------------------------------------------------------------------------------
 * Get i'th token from given token vector -- zero origin
 */
Inline token
cmd_token_get(token_vector tv, vector_index_t i)
{
  return vector_get_item(tv->body, i) ;
} ;

/*------------------------------------------------------------------------------
 * Pop token from end of given token vector -- if any.
 */
Inline token
cmd_token_pop(token_vector tv)
{
  return vector_pop_item(tv->body) ;
} ;

/*------------------------------------------------------------------------------
 * Push token onto end of given token vector.
 */
Inline void
cmd_token_push(token_vector tv, token t)
{
  vector_push_item(tv->body, t) ;
} ;

/*------------------------------------------------------------------------------
 * Shift first token off front of given token vector -- if any.
 */
Inline token
cmd_token_shift(token_vector tv)
{
  return vector_shift_item(tv->body) ;
} ;

/*------------------------------------------------------------------------------
 * Unshift token onto front of given token vector.
 */
Inline void
cmd_token_unshift(token_vector tv, token t)
{
  vector_unshift_item(tv->body, t) ;
} ;

/*------------------------------------------------------------------------------
 * Make a brand new token object
 */
Inline token
cmd_token_make(void)
{
  return XCALLOC(MTYPE_TOKEN, sizeof(struct token)) ;

  /* Zeroising the new structure sets:
   *
   *   type       = 0    -- cmd_tok_null
   *   qs         = zeroised qstring  -- empty string
   */
  confirm(cmd_tok_null == 0) ;
} ;

/*------------------------------------------------------------------------------
 * Create new 'cmd_tok_simple' token from given characters + length
 */
Inline token
cmd_token_new(cmd_token_type_t type, const char* p, size_t len, size_t tp)
{
  token t ;

  t = cmd_token_pop(spare_tokens) ;
  if (t == NULL)
    t = cmd_token_make() ;

  t->type = type ;
  qs_set_n(&t->qs, p, len) ;
  t->tp   = tp ;

  return t ;
} ;

/*------------------------------------------------------------------------------
 * Discard given token -- give back to spare tokens list
 */
Inline void
cmd_token_discard(token t)
{
  if (t != NULL)
    vector_push_item(spare_tokens->body, t) ;
} ;

/*------------------------------------------------------------------------------
 * Release contents of token vector -- move to the spare_token_strings.
 */
Inline void
cmd_empty_token_vector(token_vector tv)
{
  if (cmd_token_count(tv) != 0)
    vector_move_append(spare_tokens->body, tv->body) ;
} ;

/*------------------------------------------------------------------------------
 * Release contents of all token vectors in given parsed object.
 */
Inline void
cmd_empty_parsed_tokens(cmd_parsed parsed)
{
  cmd_empty_token_vector(parsed->tokens) ;
  cmd_empty_token_vector(parsed->read_pipe_tokens) ;
  cmd_empty_token_vector(parsed->write_pipe_tokens) ;
} ;

/*------------------------------------------------------------------------------
 * If token is incomplete (contains quotes or escapes) process those down.
 *
 * Returns: true <=> OK
 *          false => invalid escape
 */
Inline bool
cmd_token_complete(token t)
{
  return ((t->type & cmd_tok_incomplete) == 0) ? true
                                               : cmd_token_do_complete(t) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise arg_vector object in cmd_parsed.
 */
Inline void
cmd_arg_vector_init(cmd_parsed parsed)
{
  vector_init_new(parsed->args->body, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Free the body of the arg_vector object in cmd_parsed.
 */
Inline void
cmd_arg_vector_free(cmd_parsed parsed)
{
  vector_reset(parsed->args->body, keep_it) ;
} ;

/*------------------------------------------------------------------------------
 * Empty the body of the arg_vector object in cmd_parsed.
 */
Inline void
cmd_arg_vector_empty(cmd_parsed parsed)
{
  vector_set_length(parsed->args->body, 0) ;
} ;

/*------------------------------------------------------------------------------
 * Empty the body of the arg_vector object in cmd_parsed.
 */
Inline void
cmd_arg_vector_push(cmd_parsed parsed, char* arg)
{
  vector_push_item(parsed->args->body, arg) ;
} ;

/*------------------------------------------------------------------------------
 * Get the body of the argument vector.
 */
Inline const char * const*
cmd_arg_vector_argv(cmd_parsed parsed)
{
  return (const char* const*)vector_body(parsed->args->body) ;
} ;

/*------------------------------------------------------------------------------
 * Get length of the body of the argument vector.
 */
Inline unsigned
cmd_arg_vector_argc(cmd_parsed parsed)
{
  return vector_length(parsed->args->body) ;
} ;

#endif /* _ZEBRA_COMMAND_PARSE_H */

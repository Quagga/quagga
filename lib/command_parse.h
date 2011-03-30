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

#include "misc.h"

#include "command_common.h"
#include "vector.h"
#include "vio_fifo.h"
#include "qstring.h"
#include "elstring.h"
#include "qpath.h"
#include "elstring.h"
#include "memory.h"

#if 0
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
#endif

/*==============================================================================
 * Sexing of token types in command descriptions
 */
#define CMD_OPTION(S)   ((S[0]) == '[')
#define CMD_VARIABLE(S) (((S[0]) >= 'A' && (S[0]) <= 'Z') || ((S[0]) == '<'))
#define CMD_VARARG(S)   ((S[0]) == '.')
#define CMD_RANGE(S)    ((S[0] == '<'))

#define CMD_IPV4(S)        ((strcmp ((S), "A.B.C.D")    == 0))
#define CMD_IPV4_PREFIX(S) ((strcmp ((S), "A.B.C.D/M")  == 0))
#define CMD_IPV6(S)        ((strcmp ((S), "X:X::X:X")   == 0))
#define CMD_IPV6_PREFIX(S) ((strcmp ((S), "X:X::X:X/M") == 0))

/*==============================================================================
 * Command Items.
 *
 * A command is compiled into a vector of lists of command items -- the
 * cmd_command->items (so called).
 *
 */

/* Command item types
 *
 * NB: the command items sort according to this -- highest first.
 *
 * NB: this is in the same order as the match_type -- and should remain so.
 *     See match_type below for further discussion of the hierarchy.
 */
enum cmd_item_type
{
  item_null,

  item_eol,

  item_option_word,

  item_vararg,                  /* rest of the line                     */
  item_word,

  item_ipv6_prefix,
  item_ipv6_address,
  item_ipv4_prefix,
  item_ipv4_address,

  item_range,

  item_keyword,

  item_type_count,              /* number of types                      */
} ;
typedef enum cmd_item_type cmd_item_type_t ;

enum {
  item_max_number = 0xFFFFFFFF  /* can be +/- this                      */
} ;

CONFIRM(LONG_MAX >= item_max_number) ;

enum cmd_item_type_bit
{
  item_null_bit          = BIT(item_null),
  item_eol_bit           = BIT(item_eol),
  item_option_word_bit   = BIT(item_option_word),
  item_vararg_bit        = BIT(item_vararg),
  item_word_bit          = BIT(item_word),
  item_ipv6_prefix_bit   = BIT(item_ipv6_prefix),
  item_ipv6_address_bit  = BIT(item_ipv6_address),
  item_ipv4_prefix_bit   = BIT(item_ipv4_prefix),
  item_ipv4_address_bit  = BIT(item_ipv4_address),
  item_range_bit         = BIT(item_range),
  item_keyword_bit       = BIT(item_keyword),
} ;
typedef enum cmd_item_type_bit cmd_item_type_bit_t ;

/*------------------------------------------------------------------------------
 * Sex cmd_item_type and return whether it is an "option" type, or not.
 *
 * NB: tolerates item_null -- others may well not.
 */
Inline bool
cmd_item_is_option(cmd_item_type_t itt)
{
  const static bool is_option[item_type_count] =
      {
          [item_null]         = false,

          [item_eol]          = false,

          [item_option_word]  = true,

          [item_vararg]       = false,
          [item_word]         = false,

          [item_ipv6_prefix]  = false,
          [item_ipv6_address] = false,
          [item_ipv4_prefix]  = false,
          [item_ipv4_address] = false,

          [item_range]        = false,

          [item_keyword]      = false,
      } ;

  assert((itt >= 0) && (itt < item_type_count)) ;

  return is_option[itt] ;
} ;

/*------------------------------------------------------------------------------
 * Sex cmd_item_type and return whether it is an "vararg" type, or not.
 *
 * NB: tolerates item_null -- others may well not.
 */
Inline bool
cmd_item_is_vararg(cmd_item_type_t itt)
{
  const static bool is_vararg[item_type_count] =
      {
          [item_null]         = false,

          [item_eol]          = false,

          [item_option_word]  = false,

          [item_vararg]       = true,
          [item_word]         = false,

          [item_ipv6_prefix]  = false,
          [item_ipv6_address] = false,
          [item_ipv4_prefix]  = false,
          [item_ipv4_address] = false,

          [item_range]        = false,

          [item_keyword]      = false,
      } ;

  assert((itt >= 0) && (itt < item_type_count)) ;

  return is_vararg[itt] ;
} ;

/*------------------------------------------------------------------------------
 * The command item structure.
 */
typedef struct cmd_item* cmd_item ;
struct cmd_item
{
  elstring_t   str ;            /* in some word_lump                    */

  const char*  doc ;            /* in r_doc    -- description text      */

  cmd_item     next ;           /* Next possibility (if any)            */

  cmd_item_type_t  type ;
  bool         arg ;            /* include in argv                      */

  /* For item_range values                                              */
  bool  range_sign_allowed ;
  bool  range_sign_required ;
  long  range_min ;
  long  range_max ;
} ;

/*==============================================================================
 * Match strengths types and filter settings.
 *
 * When matching a token, may have a number of competing items, possibly of
 * different types.
 *
 * For execution a token must match completely -- or, for keyword items,
 * match partially at most one possible keyword.
 */

/* Match strength
 *
 * When matching a token against an item, the following are the possible
 * results.
 */
enum match_strength
{
  ms_no_match     = 0,  /* match failed: token is definitely NOT
                         * the item in question.
                         */

  ms_min_parse    = ms_no_match + 1,
                        /* for parsing must match somehow !             */

  ms_partial,           /* match OK up to the end of the token, but
                         * more is required to complete it.
                         * This is used by the variable matches.
                         */

  ms_min_execute  = ms_partial + 1,
                        /* for execution must be at least this          */

  ms_anything,          /* matches because will match anything.
                         * This is used for WORD and such like items.
                         */

  ms_kwd_incomplete,    /* keyword match OK, but not complete.
                         */

  ms_var_complete,      /* match succeeded: token is a complete and
                         * valid instance of the item in question
                         */

  ms_kwd_complete       /* match succeeded: token is a complete and
                         * valid instance of the item in question
                         */
} ;
typedef enum match_strength match_strength_t ;


/* The match type indicates what has been matched and the strength of the
 * match.
 *
 * NB: the order of these is significant, higher numbered match types are
 *     preferred over lower numbered ones.
 *
 * NB: this is in the same order as the cmd_item_type -- and should remain so.
 *
 * During the command filtering, when a token and an item match, and the
 * match type is better than the match type to date, then all previous matches
 * are discarded.
 *
 * The hierarchy means that, for example, '22' will match an IPv4 prefix
 * partially, but that will be forgotten if it later matches a WORD, and that
 * will be forgotten if it later matches a number range.
 *
 * The IPv6 items have a lower priority so that a simple decimal will prefer
 * the simpler IPv4 address form.  As soon as there is an 'a'..'f' or a ':',
 * the IPv4 will no longer match.
 *
 * The partial keyword match is preferred over a partial anything else, but
 * cannot mask a complete value !
 *
 * The relative ranking of: mt_option_word_match, mt_vararg_match, and
 * mt_word_match, is more or less arbitrary -- if these are ever candidates
 * at the same time, the commands are ambiguous.
 *
 */
enum match_type
{
  mt_no_match,

  mt_eol_partial,               /* a partial match !                    */

  mt_ipv6_address_partial,
  mt_ipv6_prefix_partial,
  mt_ipv4_address_partial,
  mt_ipv4_prefix_partial,

  mt_range_partial,

  mt_eol,                       /* an ms_anything match                 */

  mt_option_word_match,         /* anything can match a [WORD]          */

  mt_vararg_match,              /* anything can match a .vararg         */
  mt_word_match,                /* anything can match a WORD            */

  mt_keyword_incomplete,

  mt_ipv6_prefix_complete,
  mt_ipv6_address_complete,
  mt_ipv4_prefix_complete,
  mt_ipv4_address_complete,

  mt_range_complete,

  mt_keyword_complete,

  match_type_count      /* Number of match types                        */
} ;
typedef enum match_type match_type_t ;

/*------------------------------------------------------------------------------
 * Map match_type -> cmd_item_type
 *
 * From a match type extract the type of item which has been match, partially
 * or completely.
 */
Inline cmd_item_type_t
match_item_type(match_type_t mt)
{
  static cmd_item_type_t  match_item_type[match_type_count] =
    {
        [mt_no_match]                = item_null,

        [mt_eol_partial]             = item_eol,

        [mt_ipv4_prefix_partial]     = item_ipv4_prefix,
        [mt_ipv4_address_partial]    = item_ipv4_address,
        [mt_ipv6_prefix_partial]     = item_ipv6_prefix,
        [mt_ipv6_address_partial]    = item_ipv6_address,

        [mt_range_partial]           = item_range,

        [mt_eol]                     = item_eol,

        [mt_option_word_match]       = item_option_word,

        [mt_vararg_match]            = item_vararg,
        [mt_word_match]              = item_word,

        [mt_keyword_incomplete]      = item_keyword,

        [mt_ipv6_prefix_complete]    = item_ipv6_prefix,
        [mt_ipv6_address_complete]   = item_ipv6_address,
        [mt_ipv4_prefix_complete]    = item_ipv4_prefix,
        [mt_ipv4_address_complete]   = item_ipv4_address,

        [mt_range_complete]          = item_range,

        [mt_keyword_complete]        = item_keyword,
    } ;

  assert((mt >= 0) && (mt < match_type_count)) ;

  return match_item_type[mt] ;
} ;

/*------------------------------------------------------------------------------
 * Map match_type -> match_strength.
 *
 * From a match type extract the strength of the match.
 */
Inline match_strength_t
match_match_strength(match_type_t mt)
{
  const static match_strength_t  match_match_strength[match_type_count] =
    {
        [mt_no_match]                = ms_no_match,

        [mt_eol_partial]             = ms_partial,

        [mt_ipv6_prefix_partial]     = ms_partial,
        [mt_ipv6_address_partial]    = ms_partial,
        [mt_ipv4_prefix_partial]     = ms_partial,
        [mt_ipv4_address_partial]    = ms_partial,

        [mt_range_partial]           = ms_partial,

        [mt_eol]                     = ms_anything,

        [mt_option_word_match]       = ms_anything,

        [mt_vararg_match]            = ms_anything,
        [mt_word_match]              = ms_anything,

        [mt_keyword_incomplete]      = ms_kwd_incomplete,

        [mt_ipv6_prefix_complete]    = ms_var_complete,
        [mt_ipv6_address_complete]   = ms_var_complete,
        [mt_ipv4_prefix_complete]    = ms_var_complete,
        [mt_ipv4_address_complete]   = ms_var_complete,

        [mt_range_complete]          = ms_var_complete,

        [mt_keyword_complete]        = ms_kwd_complete,
    } ;

  assert((mt >= 0) && (mt < match_type_count)) ;

  return match_match_strength[mt] ;
} ;

/*------------------------------------------------------------------------------
 * Map cmd_item_type -> best possible match type.
 *
 * This gives the most optimistic outcome of an attempt to match to the given
 * type of item.
 *
 * NB: tolerates item_null -- others may not
 */
Inline match_type_t
item_best_match(cmd_item_type_t it)
{
  const static match_type_t  item_best_match[item_type_count] =
    {
        [item_null]          = mt_no_match,

        [item_eol]           = mt_eol,

        [item_option_word]   = mt_option_word_match,

        [item_vararg]        = mt_vararg_match,
        [item_word]          = mt_word_match,

        [item_ipv6_prefix]   = mt_ipv6_prefix_complete,
        [item_ipv6_address]  = mt_ipv6_address_complete,
        [item_ipv4_prefix]   = mt_ipv4_prefix_complete,
        [item_ipv4_address]  = mt_ipv4_address_complete,

        [item_range]         = mt_range_complete,

        [item_keyword]       = mt_keyword_complete,
    } ;

  assert((it >= 0) && (it < item_type_count)) ;

  return item_best_match[it] ;
} ;

/*==============================================================================
 *
 */

/* Pipe types                                                           */
enum cmd_pipe_type              /* bit significant      */
{
  cmd_pipe_none       = 0,

  cmd_pipe_file       = BIT(0),
  cmd_pipe_shell      = BIT(1),
  cmd_pipe_dev_null   = BIT(2),         /* out pipe only -- black hole  */

  /* For in pipes                                                       */
  cmd_pipe_reflect    = BIT(4),         /* + option                     */

  /* For out file pipes                                                 */
  cmd_pipe_append     = BIT(4),         /* >>                           */

  /* For out shell pipes                                                */
  cmd_pipe_shell_only = BIT(4),         /* | at start of line           */
} ;
typedef enum cmd_pipe_type cmd_pipe_type_t ;

/* Parsed parts                                                         */
enum cmd_parts                  /* bit significant      */
{
  cmd_parts_none      = 0,

  cmd_part_do         = BIT(0),
  cmd_part_command    = BIT(1),

  cmd_part_in_pipe    = BIT(2),
  cmd_part_out_pipe   = BIT(3),

  cmd_parts_pipe      = (cmd_part_in_pipe | cmd_part_out_pipe),

  cmd_part_comment    = BIT(4),
} ;
typedef enum cmd_parts cmd_parts_t ;


/*------------------------------------------------------------------------------
 * Token object -- a qstring and some other properties.
 */
enum cmd_token_type     /* *bit* significant    */
{
  cmd_tok_eol           = 0,            /* all lines have one           */

  cmd_tok_simple        = BIT( 0),

  cmd_tok_sq            = BIT( 4),
  cmd_tok_dq            = BIT( 5),      /* '\\' within "..." are not
                                           registered separately.       */
  cmd_tok_esc           = BIT( 6),

  cmd_tok_incomplete    = (cmd_tok_sq | cmd_tok_dq | cmd_tok_esc),

  cmd_tok_in_pipe       = BIT( 8),      /* token starting '<'           */
  cmd_tok_out_pipe      = BIT( 9),      /* token starting '>'           */
  cmd_tok_out_shell     = BIT(10),      /* token starting '|'           */

  cmd_tok_comment       = BIT(14),      /* token starting '!' or '#"    */
} ;
typedef enum cmd_token_type cmd_token_type_t ;

struct cmd_token
{
  cmd_token_type_t  type ;
  usize             tp ;        /* location of token in the line        */

  bool              term ;      /* token has been '\0' terminated       */

  qstring_t         qs ;        /* token string                         */
  elstring_t        ot ;        /* original token in the line           */

  const void*       word ;      /* set when a word match is done        */
  ulen              w_len ;     /* length of word match                 */
  int               cw ;        /* result of the word match             */

  cmd_item_type_bit_t  seen ;   /* bit set when match done              */
  match_type_t         match[item_type_count] ;
                                /* result of match seen                 */
} ;

typedef struct cmd_token* cmd_token ;

/*------------------------------------------------------------------------------
 * Token vector -- a vector of token objects
 */
struct token_vector
{
  vector_t      body ;
} ;

typedef struct token_vector  token_vector_t[1] ;
typedef struct token_vector* token_vector ;

enum {
  TOKEN_VECTOR_INIT_ALL_ZEROS = VECTOR_INIT_ALL_ZEROS
} ;

/*------------------------------------------------------------------------------
 * Argument vector -- a vector of const char*
 */
struct arg_vector
{
  vector_t      body ;
} ;
typedef struct arg_vector  arg_vector_t[1] ;
typedef struct arg_vector* arg_vector ;

enum {
  ARG_VECTOR_INIT_ALL_ZEROS = VECTOR_INIT_ALL_ZEROS
} ;

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
struct cmd_parsed
{
  cmd_parts_t   parts ;         /* What parts are present               */

  cmd_token_type_t tok_total ;  /* What token types are present         */

  usize         elen ;          /* effective length (less trailing spaces) */
  usize         tsp ;           /* number of trailing spaces            */

  uint          num_tokens ;    /* number of tokens parsed              */

  token_vector  tokens ;      /* vector of token objects              */

  /* NB: the following are significant only if there is a command part
   *     or a do part
   */
  struct cmd_command *cmd ;     /* NULL if empty command
                                        or fails to parse               */
  node_type_t   cnode ;         /* node command is in                   */
  node_type_t   nnode ;         /* node to set if command succeeds      */

  arg_vector    args ;          /* vector of arguments                  */

  /* NB: the following are significant only if an error is returned     */

  qstring       emess ;         /* parse error                          */
  ssize         eloc ;          /* error location                       */

  /* NB: the following are significant only if respective part is
   *     present.
   */
  cmd_pipe_type_t in_pipe ;     /* if any                               */
  cmd_pipe_type_t out_pipe ;    /* if any                               */

  uint          first_in_pipe ;
  uint          num_in_pipe ;

  uint          first_do ;
  uint          num_do ;

  uint          first_command ;
  uint          num_command ;

  uint          first_out_pipe ;
  uint          num_out_pipe ;

  uint          first_comment ;
  uint          num_comment ;

  /* The following are significant after cmd_token_position()           */

  uint          cti ;   /* cursor token index -- may be eol token       */
  uint          ctl ;   /* cursor token length -- 0 <=> eol token       */
  int           rp ;    /* cursor relative to start of cursor token     */

  /* The following are used while filtering commands                    */

  vector        cmd_v ;         /* working vector                       */
  vector        item_v ;        /* working vector                       */

  match_strength_t  strongest ;
  match_type_t      best_complete ;

  match_strength_t  min_strength ;      /* for execution                */
  bool              strict ;            /* for strict keyword match     */
} ;

enum
{
  CMD_PARSED_INIT_ALL_ZEROS = (cmd_pipe_none == 0)
} ;

typedef struct cmd_parsed  cmd_parsed_t[1] ;
typedef struct cmd_parsed* cmd_parsed ;

/*==============================================================================
 * This is the stuff that defines the context in which commands are parsed,
 * and then executed.
 *
 * The context lives in the cmd_exec.  The CLI has a copy of the context,
 * which it uses for the prompt and for command line help handling.
 *
 * Each time a new vin is pushed, the current context is copied to the current
 * TOS (before the push).
 *
 * Easch time a vin is popped, the context is restored.
 *
 * Each time a vin or vout is pushed or popped the context the cmd_exec
 * out_suppress and reflect flags must be updated.
 */
struct cmd_context
{
  /* The node between commands.                                         */

  node_type_t   node ;                  /* updated on CMD_SUCCESS       */

  /* These properties affect the parsing of command lines.              */

  bool          full_lex ;              /* as required                  */

  bool          parse_execution ;       /* parsing to execute           */

  bool          parse_only ;            /* do not execute               */

  bool          parse_strict ;          /* no incomplete keywords       */
  bool          parse_no_do ;           /* no 'do' commands             */
  bool          parse_no_tree ;         /* no tree walking              */

  bool          can_auth_enable ;       /* if required                  */
  bool          can_enable ;            /* no (further) password needed */

  /* These properties affect the execution of parsed commands.          */

  bool          reflect_enabled ;       /* per the pipe                 */

  /* Special for AUTH_ENABLE_NODE -- going from/to                      */

  node_type_t   onode ;                 /* VIEW_NODE or RESTRICTED_NODE */
  node_type_t   tnode ;                 /* ENABLE_NODE or CONFIG_NODE   */

  /* The current directories.                                           */

  qpath         dir_cd ;                /* chdir directory              */
  qpath         dir_home ;              /* "~/" directory               */
  qpath         dir_here ;              /* "~./" directory              */
} ;

typedef struct cmd_context  cmd_context_t[1] ;
typedef struct cmd_context* cmd_context ;

/*==============================================================================
 * Prototypes
 */
extern void cmd_compile(cmd_command cmd) ;
extern void cmd_compile_check(cmd_command cmd) ;

extern cmd_parsed cmd_parsed_new(void) ;
extern cmd_parsed cmd_parsed_free(cmd_parsed parsed) ;

extern bool cmd_is_empty(qstring line) ;
extern void cmd_tokenize(cmd_parsed parsed, qstring line, bool full_lex) ;
extern qstring cmd_tokens_concat(cmd_parsed parsed, uint ti, uint nt) ;

extern cmd_return_code_t cmd_parse_command(cmd_parsed parsed,
                                                          cmd_context context) ;

extern bool cmd_token_position(cmd_parsed parsed, qstring line) ;
extern const char* cmd_help_preflight(cmd_parsed parsed) ;
extern cmd_return_code_t cmd_completion(cmd_parsed parsed, cmd_context context);

extern void cmd_complete_keyword(cmd_parsed parsed,
                                       int* pre, int* rep, int* ins, int* mov) ;

extern void cmd_get_parse_error(vio_fifo ebuf, cmd_parsed parsed, uint indent) ;

extern void cmd_parser_init(void) ;



//extern vector cmd_make_strvec (const char *);
//extern vector cmd_add_to_strvec (vector v, const char* str) ;
//extern void cmd_free_strvec (vector);
extern vector cmd_describe_command (const char* line, node_type_t node,
                                                    cmd_return_code_t* status) ;
extern vector cmd_complete_command (vector, int, int *status);



//extern cmd_return_code_t cmd_parse_error(cmd_parsed parsed, cmd_token t,
//                                                usize off, const char* mess) ;

//Inline const char* cmd_token_string(cmd_token t) ;
//Inline char* cmd_token_make_string(cmd_token t) ;
//Inline int cmd_token_count(token_vector tv) ;
//Inline cmd_token cmd_token_get(token_vector tv, vector_index_t i) ;
//Inline void  cmd_token_set(token_vector tv, vector_index_t i,
//                    cmd_token_type_t type, const char* p, usize len, usize tp) ;

//extern cmd_return_code_t cmd_token_complete(cmd_parsed parsed, cmd_token t) ;
//Inline const char* cmd_token_string(cmd_token t) ;
//extern cmd_return_code_t cmd_parse_in_pipe(cmd_parsed parsed, cmd_token t) ;
//extern cmd_return_code_t cmd_parse_out_pipe(cmd_parsed parsed, cmd_token t) ;

//extern match_type_t cmd_ipv4_match (const char *str) ;
//extern match_type_t cmd_ipv4_prefix_match (const char *str) ;
#if HAVE_IPV6
//extern match_type_t cmd_ipv6_match (const char *str) ;
//extern match_type_t cmd_ipv6_prefix_match (const char *str) ;
#endif
//extern bool cmd_range_match (const char *range, const char *str) ;

/*==============================================================================
 * Inlines
 */

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

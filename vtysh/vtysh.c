/* Virtual terminal interface shell.
 * Copyright (C) 2000 Kunihiro Ishiguro
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

#include "zebra.h"
#include "misc.h"

#include <sys/un.h>
#include <setjmp.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/stat.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "lib/command.h"
#include "lib/command_local.h"
#include "lib/command_execute.h"
#include "lib/memory.h"
#include "lib/log.h"
#include "bgpd/bgp_vty.h"

#include "lib/vty_command.h"
#include "lib/vty_io_vtysh.h"
#include "lib/vty_vtysh.h"
#include "lib/vty_cli.h"
#include "vtysh/vtysh.h"

#include "lib/distribute.h"
#include "lib/filter.h"
#include "lib/if.h"
#include "lib/if_rmap.h"
#include "lib/keychain.h"
#include "lib/plist.h"
#include "lib/routemap.h"

/* Using integrated config from Quagga.conf. Default is no.
 */
bool vtysh_integrated_vtysh_config = false ;

extern char config_default[];

/*==============================================================================
 * The vtysh has its own vty, which is used:
 *
 *   a) to parse commands to decide which (if any) daemon they belong to
 *
 *   b) to parse and execute commands in the vtysh itself.
 *
 * This vtysh has no input and its output is implicitly to the file handle
 * "fp" in the vout_base.  The output may be paged -- it is by default.  Just
 * before a top-level vtysh command is executed:
 *
 *   if it is to be paged: vout_base->fp    is set NULL
 *                         vout_base->pager is set to name of pager to use
 *
 *                         if there is any output, the pager is opened, and
 *                         vout_base->fp set appropriately.
 *
 *                         at end of top level command, any pager that was
 *                         opened is then closed.
 *
 *              otherwise: vout_base->fp    is set to stdout
 *                         vout_base->pager is set NULL
 *
 * vysh_pager_form set early in the morning, from VTYSH_PAGER or to the
 * default.  The default is:
 *
 *    more;more -%d
 *
 * where everything before the ';' is used if no terminal length is set, and
 * everything after the ';' is used if a terminal length > 0 is set.  If there
 * is no ':',
 *
 * So:  terminal length 12      -- turns on pager, with explicit length 12
 *      terminal length none    -- turns off pager
 *      terminal length default -- turns on pager, with its default length
 */

vty vtysh_vty     = NULL ;      /* The vtysh's own vty  */
vty vtysh_stderr  = NULL ;      /* For error messages   */

static char* vtysh_pager       = NULL ;         /* NULL => pager is off */

static char* vtysh_pager_form  = NULL ;         /* used to make pager   */

static int   vtysh_pager_lines = -1 ;

/*------------------------------------------------------------------------------
 * Execute given command, in the current vtysh_vty (using current context), in
 * all connected daemons.
 *
 * This is at the top level of the vtysh, and is used for:
 *
 *   * the interactive command mode
 *
 *   * the command line command mode
 *
 *   * commands to enter and exit CONFIG_NODE in configuration file reading.
 *
 *     All daemons, including the vtysh itself, can do these commands, so
 *     the only likely error is that a daemon is unable to enter configuration
 *     mode, and that is reported.
 *
 * If the command is not recognised by any connected daemon and not recognised
 * by the vtysh itself, an error is reported.  (Unlike the configuration file
 * reader, where this is simply ignored.)
 *
 * Uses the current setting of vtysh_pager.
 *
 * If "ignore_warnings" then will return CMD_SUCCESS instead of CMD_WARNING,
 * and if the command opens an input pipe, then will ignore warnings in there
 * too.
 *
 * Returns:  CMD_SUCCESS    -- OK, keep going
 *           CMD_STOP       -- OK, but exit
 *           CMD_WARNING    -- got a CMD_WARNING from at least one daemon
 *           CMD_ERROR      -- got a CMD_ERROR from at least one daemon
 */
extern cmd_ret_t
vtysh_execute(vty vty, const char* line, ulen prompt_len)
{
  uty_vtysh_out_prep(vty->vio, vtysh_pager) ;

  return vty_vtysh_command_loop(vty, line, true /* interactive */, prompt_len) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise vtysh_pager_form, vtysh_pager_lines and vtysh_pager.
 *
 * Sets the pager *off*.
 */
static void
vtysh_pager_init(void)
{
  const char* form ;

  form = getenv ("VTYSH_PAGER") ;

  if (form == NULL)
    form = "more;more -%d" ;

  vtysh_pager_form  = strdup(form) ;
  vtysh_pager       = NULL ;
  vtysh_pager_lines = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set the vtysh_pager for the given number of lines, where:
 *
 *   lines <  0 -- sets to default for the pager
 *   lines == 0 -- turns of pager
 *   lines >  0 -- sets to given number of lines, if possible
 *
 * Returns:  previous vtysh_pager_lines setting
 */
extern int
vtysh_pager_set(int lines)
{
  int lines_was ;

  lines_was = vtysh_pager_lines ;
  vtysh_pager_lines = lines ;

  /* Turn off pager and free any existing name string.
   */
  if (vtysh_pager != NULL)
    {
      free(vtysh_pager) ;
      vtysh_pager = NULL ;
    } ;

  /* For lines == 0 we leave the pager off, otherwise we set the number of
   * lines given if > 0, or the default for the pager if < 0
   */
  if (lines != 0)
    {
      const char* s ;
      ulen l ;

      /* Find the ';', if any.
       *
       * Set s to start of form to use with explicit length if any.
       */
      s = strchr(vtysh_pager_form, ';') ;

      if ((lines > 0) && (s != NULL))
        {
          ulen l ;

          ++s ;                         /* step past ';'                */
          l = strlen(s) + 10 + 1 ;      /* form + some for digits       */

          vtysh_pager = malloc(l) ;
          snprintf(vtysh_pager, l, s, lines) ;
        }
      else
        {
          if (s != NULL)
            l = s - vtysh_pager_form ;
          else
            l = strlen(vtysh_pager_form) ;

          vtysh_pager = malloc(l + 1) ;
          memcpy(vtysh_pager, vtysh_pager_form, l) ;
          vtysh_pager[l] = '\0' ;
        } ;
    } ;

  /* Returns the now current pager setting
   */
  return lines_was ;
} ;

/*==============================================================================
 * Uses readline and some moderately deep readline completion magic.
 */

/* Result of cmd_complete_command() call will be stored here
 * and used in new_completion() in order to put the space in
 * correct places only. */
int complete_status;


static rl_command_func_t vtysh_describe ;
//static rl_compentry_func_t vtysh_completion_entry_function ;
static rl_completion_func_t vtysh_attempted_completion_function ;
static rl_compentry_func_t vtysh_entry_func ;

static int vtysh_completion(void) ;
static int vtysh_help_parse(const char* literal) ;
static void vtysh_show_descriptions(void) ;
static void vtysh_complete_keyword(elstring keyword, bool complete) ;

/*------------------------------------------------------------------------------
 *
 */
extern void
vtysh_readline_init (void)
{
  /* readline related settings.
   */
  rl_bind_key ('?', vtysh_describe);

//rl_completion_entry_function = vtysh_completion_entry_function;
  rl_attempted_completion_function = vtysh_attempted_completion_function;

  /* do not append space after completion. It will be appended
   * in new_completion() function explicitly.
   */
  rl_completion_append_character = '\0';
}

#if 0
/*------------------------------------------------------------------------------
 * Minimal rl_completion_entry_function(), to disable readline's filename
 * completion.
 */
static char *
vtysh_completion_entry_function(const char *ignore, int invoking_key)
{
  return NULL;
} ;
#endif

/*------------------------------------------------------------------------------
 * Respond to '?' on the command line.
 */
static int
vtysh_describe (int count, int key)
{
  uint  n_items ;

  /* Parse etc. to get list of possibilities -- deals with exception cases.
   *
   * If then have something to show, show it !
   */
  n_items = vtysh_help_parse("?") ;

  if (n_items > 0)
    vtysh_show_descriptions() ;

  return 0;
} ;

/*------------------------------------------------------------------------------
 * For 'rl_attempted_completion_function' -- ie: respond to TAB on command line
 *
 *   text   -- string that completion may replace
 *   start  )  boundaries of the string in the rl_line_buffer
 *   end    )
 *
 * For a command line:
 *
 *   ... abcde ...
 *
 * if the cursor is on the 'c', then get: text  == "ab"
 *                                        start == location of the 'a'
 *                                        end   == location of the 'c'
 */
static char **
vtysh_attempted_completion_function (const char *text, int start, int end)
{
  char** matches;

  matches = rl_completion_matches(text, vtysh_entry_func) ;

  if (matches)
    {
#if 0
      rl_point = rl_end;
      if (complete_status == CMD_COMPLETE_FULL_MATCH)
        rl_pending_input = ' ';
#endif
    }

  rl_attempted_completion_over = true ;

  return matches;
} ;

/*------------------------------------------------------------------------------
 * Return next possible completion -- called by rl_completion_matches().
 *
 * If state == 0, this is the first call of an attempted completion, so is
 * where all the work is done.  On all subsequent calls, the second and
 * subsequent completions are returned.
 */
static char *
vtysh_entry_func(const char *text, int state)
{
  static int index   = 0 ;
  static int n_items = 0 ;

  if (state == 0)
    {
      n_items = vtysh_completion() ;
      index = 0 ;
    } ;

  if (index < n_items)
    {
      cmd_item item ;
      ulen     str_len ;
      char*    entry ;

      item = vector_get_item(vtysh_vty->exec->parsed->item_v, index) ;
      ++index ;

      str_len = els_len_nn(item->str) ;
      entry = malloc(str_len + 1) ;

      memcpy(entry, els_body_nn(item->str), str_len) ;
      entry[str_len] = '\0' ;

      return entry ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Work out whether or how can complete the current work in the current
 * command line.
 *
 * If there are multiple ways to do this, then vtysh_vty->exec->parsed->item_v
 * is set to a list of the possibilities.
 *
 * If there is one way to do this, then does that here.
 */
static int
vtysh_completion(void)
{
  cmd_item item ;
  uint     n_items ;

  /* Parse etc. to get list of possibilities -- deals with exception cases.
   */
  n_items = vtysh_help_parse(" ") ;

  if (n_items == 0)
    return 0 ;                  /* no completions       */

  /* If have more than one possible completions then see if we should
   * extend the current token or move to the end of the current token.
   *
   * If not return the number of completions to be displayed.
   */
  if (n_items > 1)
    {
      elstring_t els ;

      if (cmd_part_complete(vtysh_vty->exec->parsed, els))
        {
          vtysh_complete_keyword(els, false) ;
          n_items = 0 ;
        } ;

      return n_items ;
    } ;

  /* One possible completion.
   *
   * If is keyword, replace current token and leave positioned after it.
   * Otherwise, give hint as to possible value for token.
   */
  item = vector_get_item(vtysh_vty->exec->parsed->item_v, 0) ;

  switch (item->type)
    {
      case item_null:
        zabort("invalid item_null") ;

      case item_eol:

      case item_option_word:

      case item_vararg:

      case item_word:

      case item_ipv6_prefix:
      case item_ipv6_address:
      case item_ipv4_prefix:
      case item_ipv4_address:

      case item_range:
        vtysh_show_descriptions() ;
        break ;

      case item_keyword:
        vtysh_complete_keyword(item->str, true) ;
        break ;

      default:
        zabort("unknown item type") ;
    } ;

  return 0 ;    /* nothing more to do   */
} ;

/*------------------------------------------------------------------------------
 * Deal with parsing of current line and discovering how much help can be
 * given.
 *
 * Sets: vtysh_vty->exec->context and vtysh->exec->context->line
 *       vtysh_vty->exec->parsed
 *
 * Returns: number of entries in the exec->parsed item_v vector (if any)
 *
 * If returns 0, there is nothing further to be done -- may have issued
 * error message or inserted the literal character.
 */
static int
vtysh_help_parse(const char* literal)
{
  cmd_context  context ;
  cmd_parsed   parsed ;

  const char* msg ;
  uint        n_items ;
  cmd_ret_t   ret ;

  /* First, fill command line from: rl_line_buffer, rl_point and rl_end.
   */
  context = vtysh_vty->exec->context ;
  parsed  = vtysh_vty->exec->parsed ;

  context->line  = vty_vtysh_prep_line(vtysh_vty, rl_line_buffer, rl_end,
                                                                     rl_point) ;

  /* Tokenise and work out where we are on the line...
   *
   * ...if we are in "special" place, simply insert the literal and return.
   */
  if (cmd_token_position(parsed, context->line))
    {
      rl_insert_text(literal) ;
      rl_redisplay() ;
      return 0 ;
    } ;

  /* The preflight checks avoid getting into trouble doing command completion
   * on a line with comment.
   */
  msg = cmd_help_preflight(parsed) ;
  if (msg != NULL)
    {
      printf("\n%% %s\n", msg) ;
      rl_on_new_line() ;
      return 0 ;
    } ;

  /* Now see what the cmd_completion can come up with.
   */
  ret = cmd_completion(parsed, context) ;

  if (ret == CMD_ERR_PARSING)
    {
#if 0
      if (cli->help_parsed->eloc >= 0)
        {
          uint eloc = cli->prompt_len + cli->help_parsed->eloc ;

          uty_cli_help_newline(cli) ;   /* clears cli_drawn etc.        */
          uty_cli_write_n(cli, telnet_dots, eloc) ;
          uty_cli_write_s(cli, "^") ;
        } ;
#endif
      printf("\n%% %s\n", qs_string(parsed->emess)) ;
      rl_on_new_line() ;
      return 0 ;
    } ;

  /* Will now have 0, 1 or more items which match at the current
   * cursor token.
   */
  n_items = vector_length(parsed->item_v) ;

  if (n_items == 0)
    {
      printf("\n%% %s\n", "command not recognised") ;
      rl_on_new_line() ;
    } ;

  return n_items ;
} ;

/*------------------------------------------------------------------------------
 * Show descriptions for all items in vtysh_vty->exec->parsed->item_v
 *
 * Starts by moving to new blank line, and ends by telling readline to redraw
 * the command line.
 */
static void
vtysh_show_descriptions(void)
{
  vector         list ;
  vector_index_t i ;

  int rows, cols ;

  rl_crlf() ;

  rl_get_screen_size(&rows, &cols) ;

  list = uty_cli_make_describe_list(vtysh_vty->exec->parsed->item_v, cols) ;

  for (i = 0 ; i < vector_length(list) ; ++i)
    {
      qstring  qs = vector_get_item(list, i) ;

      printf("%s\n", qs_string(qs)) ;

      qs_free(qs) ;
    } ;

  vector_free(list) ;

  rl_on_new_line();
} ;

/*------------------------------------------------------------------------------
 * Completion, replacing an existing keyword or inserting at the end.
 */
static void
vtysh_complete_keyword(elstring keyword, bool complete)
{
  int pre, rep, ins, mov ;
  char* str ;
  ulen str_len ;

  cmd_complete_keyword(vtysh_vty->exec->parsed, &pre, &rep, &ins, &mov) ;

  str_len = els_len_nn(keyword) ;
  str = malloc(str_len + ins + 1) ;
  memcpy(str, els_body_nn(keyword), str_len) ;
  if (complete)
    {
      while (ins-- > 0)
        str[str_len++] = ' ' ;
    } ;

  str[str_len] = '\0' ;

  rl_begin_undo_group() ;
  rl_point += pre ;
  rl_delete_text(rl_point, rl_point + rep) ;
  rl_insert_text(str) ;
  if (complete)
    rl_point += mov ;
  rl_end_undo_group() ;

  rl_redisplay() ;
} ;

/*==============================================================================
 * The client daemons
 */

static daemon_set_t vtysh_daemons_list(vty vty, int argc, argv_t argv) ;
static bool vtysh_daemons_check_ok(vty vty, daemon_set_t daemons) ;

/*------------------------------------------------------------------------------
 * Connect to all given daemons or to all available daemons.
 *
 * If given daemons is NULL, connect to all available -- which may be none
 * at all.
 *
 * Otherwise, deamons string is expected to contain names separated by spaces,
 * or commas: all of which must be valid names, and all of which must be
 * available.
 *
 * In the event of errors, uses vty_err(vty).
 *
 * Returns: true  => connected as required...
 *                   ...noting that if no daemons were required, may not have
 *                      any open.
 *          false => failed to connect exactly as required...
 *                   ...message sent to vty_err(vty)
 *                      will have connected to as many of the required daemons
 *                      as possible.
 */
extern bool
vtysh_daemons_connect(vty vtysh, daemon_set_t daemons, daemon_set_t required)
{
  daemon_set_t connected ;

  connected = vty_vtysh_open_clients(vtysh, daemons, required) ;

  return (required == 0) || (connected == required) ;
} ;

/*------------------------------------------------------------------------------
 * See if given daemons list from the -d options contains one or more valid
 * daemons, and if so which.
 *
 * List should not be empty because this is from the -d option(s)
 *
 * Returns:  0 => list empty, or contains unrecognised or invalid daemon
 *                                   names -- error message output by vty_err().
 *           Otherwise is set of daemons to be connected to.
 */
extern daemon_set_t
vtysh_daemons_list_ok(vty vtysh, qstring daemon_list)
{
  daemon_set_t daemons ;
  qstring  list ;
  bool     ok ;

  list = qs_set(NULL, daemon_list) ;

  daemons = cmd_daemons_from_list(list) ;

  ok = vtysh_daemons_check_ok(vtysh, daemons) ;

  if (qs_len(list) != 0)
    {
      vty_err(vtysh, "%% Failed: did not recognise: -d %s\n",
                                                        qs_string(list)) ;
      ok = false ;
    } ;

  if ((daemons == 0) && ok)
    {
      vty_err(vtysh, "%% Failed: empty -d !\n") ;
      ok = false ;
    } ;

  qs_free(list) ;

  return ok ? daemons : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Connect to the given daemons
 */
DEFUN (vtysh_connect_daemons,
       vtysh_connect_daemons_cmd,
       "connect .DAEMONS",
       "Connect to daemons"
       "List of daemons to connect to\n")
{
  daemon_set_t daemons ;

  daemons = vtysh_daemons_list(vty, argc, argv) ;
  if (daemons == 0)
    return CMD_WARNING ;

  vty_out(vty, "TBD: %s\n", __func__) ;

  return CMD_SUCCESS;
} ;

/*------------------------------------------------------------------------------
 * Disconnect from the given daemons
 */
DEFUN (vtysh_disconnect_daemons,
       vtysh_disconnect_daemons_cmd,
       "disconnect .DAEMONS",
       "Disconnect from daemons"
       "List of daemons to disconnect from\n")
{
  daemon_set_t daemons ;

  daemons = vtysh_daemons_list(vty, argc, argv) ;
  if (daemons == 0)
    return CMD_WARNING ;

  vty_out(vty, "TBD: %s\n", __func__) ;

  return CMD_SUCCESS;
} ;

ALIAS (vtysh_disconnect_daemons,
       no_vtysh_connect_daemons_cmd,
       "no connect .DAEMONS",
       NO_STR
       "Disconnect from daemons"
       "List of daemons to disconnect from\n")

/*------------------------------------------------------------------------------
 * Show which daemons is currently connected to
 */
DEFUN (vtysh_show_daemons,
       vtysh_show_daemons_cmd,
       "show daemons",
       SHOW_STR
       "Show list of running daemons\n")
{
  vtysh_connected_ok(vty, false /* not quiet */, false /* not fail */) ;
  return CMD_SUCCESS;
} ;

/*------------------------------------------------------------------------------
 * Show clients to which we are currently connected
 *
 * If connected to at least one daemon, show which -- unless "quiet".  Return
 * true <=> OK.
 *
 * If not connected to any daemon: if "fail", issue error message to vty_err
 * and return false, otherwise output and NB message and return true.  (Note
 * that outputs the NB, even if is "quiet".
 */
extern bool
vtysh_connected_ok(vty vtysh, bool quiet, bool fail)
{
  daemon_set_t daemons ;

  daemons = vty_vtysh_check_clients(vtysh) ;

  if (daemons != 0)
    {
      if (!quiet)
        {
          qstring  list ;

          list = cmd_daemons_make_list(NULL, daemons) ;
          vty_out(vtysh, "Connected to: %s\n", qs_string(list)) ;

          qs_free(list) ;
        } ;
    }
  else if (!fail)
    {
      vty_out(vtysh, "Not connected to any daemon\n") ;
    }
  else
    {
      vty_err(vtysh, "%% not connected to any daemon\n") ;
      return false ;
    }

  vty_cmd_out_push(vtysh) ;
  return true ;
} ;

/*------------------------------------------------------------------------------
 * Process command arguments as a list of daemon names.
 *
 * If there are any duff daemons, report error by vty_err().
 */
static daemon_set_t
vtysh_daemons_list(vty vtysh, int argc, argv_t argv)
{
  daemon_set_t daemons ;

  daemons = cmd_deamon_list_arg(vtysh, argc, argv) ;

  if (!vtysh_daemons_check_ok(vtysh, daemons))
    daemons = 0 ;

  return daemons ;
} ;

/*------------------------------------------------------------------------------
 * Filter daemons to remove any that vtysh cannot connect to.
 *
 * If there are any duff daemons, report error by vty_err().
 *
 * Returns:  true <=> OK
 */
static bool
vtysh_daemons_check_ok(vty vtysh, daemon_set_t daemons)
{
  if ((daemons & ~ALL_RDS) == 0)
    return true ;

  vty_err(vtysh, "%% vtysh cannot connect to ") ;

  daemons &= ~ALL_RDS ;

  if (daemons & VTYSH_VD)
    {
      daemons &= ~VTYSH_VD ;
      vty_err(vtysh, "itself(!)%s", (daemons != 0) ? " or " : "\n") ;
    } ;

  if (daemons != 0)
    {
      qstring list ;

      list = cmd_daemons_make_list(NULL, daemons) ;
      vty_err(vtysh, "%s\n", qs_string(list)) ;

      qs_free(list) ;
    } ;

  return false ;
} ;

/*==============================================================================
 *
 */


/*------------------------------------------------------------------------------
 * Collect the integrated config, and then squirt all over the terminal.
 *
 * Note that we force the pager on, in default state, if it is off.
 */
static cmd_ret_t
vtysh_show_integrated_config(vty vty)
{
  cmd_ret_t ret ;

  if (true)
    {
      vty_out(vty, "%% %s TBA\n", __func__) ;
      return CMD_WARNING ;
    } ;

  /* First collect the integrated config.
   */
  ret = vtysh_config_collect_integrated(vty, true) ;

  /* Force out any pending output, and if the pager was off, set it on at the
   * default for the terminal.
   */
  if (ret == CMD_SUCCESS)
    ret = vty_cmd_out_push(vty) ;

  /* Spray result of collected configuration all over the screen, forcing the
   * pager on
   */
  if (ret == CMD_SUCCESS)
    {
      int       lines_was ;

      lines_was = vtysh_pager_lines ;
      if (lines_was == 0)
        vtysh_pager_set(-1) ;

      vty->config_to_vtysh = false ;
      ret = vty_write_config(vty, vtysh_config_write_config_node) ;

      if (lines_was == 0)
        vtysh_pager_set(0) ;
    } ;

  /* Discard the collected configuration & return
   */
  vtysh_config_reset_integrated() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Collect the integrated config, and then write to the integrated config
 * file.
 */
static cmd_ret_t
vtysh_write_integrated_config(vty vty)
{
  cmd_ret_t ret ;

  if (true)
    {
      vty_out(vty, "%% %s TBA\n", __func__) ;
      return CMD_WARNING ;
    } ;

  /* First collect the integrated config.
   */
  ret = vtysh_config_collect_integrated(vty, true) ;

  /* Send result of collected configuration to the integrated configuration
   * file.
   */
  if (ret == CMD_SUCCESS)
    ret = vty_write_config_file(vty, host.int_config_file,
                                         vtysh_config_write_config_node, true) ;

  /* Discard the collected configuration & return
   */
  vtysh_config_reset_integrated() ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Turning on/off the integrated config
 *
 * When set, "service integrated-vtysh-config" affects "write file" and all its
 * aliases, such that they all become aliasies of "write integrated".  Also,
 * sets the host.config_dir (~~/) to the integrated configuration file
 * directory.
 *
 * When clear, "write file" and all its aliases, write each daemon's own
 * configuration, and the host.config_dir (~~/) is set to the vtysh's own
 * configuration file directory.
 */
extern cmd_ret_t
vtysh_set_integrated_config(on_off_b integrated)
{
  vtysh_integrated_vtysh_config = integrated ;

  cmd_set_integrated_vtysh_config(integrated) ;

  return CMD_SUCCESS ;
} ;

DEFUN (vtysh_integrated_config,
       vtysh_integrated_config_cmd,
       "service integrated-vtysh-config",
       "Set up miscellaneous service\n"
       "Write configuration into integrated file\n")
{
  return vtysh_set_integrated_config(on) ;
} ;

DEFUN (no_vtysh_integrated_config,
       no_vtysh_integrated_config_cmd,
       "no service integrated-vtysh-config",
       NO_STR
       "Set up miscellaneous service\n"
       "Write configuration into integrated file\n")
{
  return vtysh_set_integrated_config(off) ;
} ;

/*------------------------------------------------------------------------------
 * Set the new terminal length.
 *
 * Has no effect on the current, top level, command.
 */
static cmd_ret_t
vtysh_terminal_length(vty vty, int lines)
{
  vtysh_pager_set(lines) ;

  return CMD_SUCCESS;
} ;

/*------------------------------------------------------------------------------
 * Show the vtysh history
 */
static cmd_ret_t
vtysh_show_history(vty vty)
{
  HIST_ENTRY** history ;
  HIST_ENTRY*  entry ;

  history = history_list() ;
  if (history != NULL)
    entry = *history++ ;
  else
    entry = NULL ;

  while (entry != NULL)
    {
      vty_out(vty, "%s\n", entry->line) ;
      entry = *history++ ;
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Execute command in a child process.
 */
static int
execute_command (const char *command, int argc, const char *arg1,
		 const char *arg2)
{
  int ret;
  pid_t pid;
  int status;

  /* Call fork(). */
  pid = fork ();

  if (pid < 0)
    {
      /* Failure of fork(). */
      fprintf (stderr, "Can't fork: %s\n", safe_strerror (errno));
      exit (1);
    }
  else if (pid == 0)
    {
      /* This is child process. */
      switch (argc)
	{
	case 0:
	  ret = execlp (command, command, (const char *)NULL);
	  break;
	case 1:
	  ret = execlp (command, command, arg1, (const char *)NULL);
	  break;
	case 2:
	  ret = execlp (command, command, arg1, arg2, (const char *)NULL);
	  break;
	}

      /* When execlp succeeds, this part is not executed.
       */
      fprintf (stderr, "Can't execute %s: %s\n", command, safe_strerror (errno));
      exit (1);
    }
  else
    {
      /* This is parent. */
      execute_flag = 1;
      ret = wait4 (pid, &status, 0, NULL);
      execute_flag = 0;
    }
  return 0;
}

DEFUN (vtysh_ping,
       vtysh_ping_cmd,
       "ping WORD",
       "Send echo messages\n"
       "Ping destination address or hostname\n")
{
  execute_command ("ping", 1, argv[0], NULL);
  return CMD_SUCCESS;
}

ALIAS (vtysh_ping,
       vtysh_ping_ip_cmd,
       "ping ip WORD",
       "Send echo messages\n"
       "IP echo\n"
       "Ping destination address or hostname\n")

DEFUN (vtysh_traceroute,
       vtysh_traceroute_cmd,
       "traceroute WORD",
       "Trace route to destination\n"
       "Trace route to destination address or hostname\n")
{
  execute_command ("traceroute", 1, argv[0], NULL);
  return CMD_SUCCESS;
}

ALIAS (vtysh_traceroute,
       vtysh_traceroute_ip_cmd,
       "traceroute ip WORD",
       "Trace route to destination\n"
       "IP trace\n"
       "Trace route to destination address or hostname\n")

#ifdef HAVE_IPV6
DEFUN (vtysh_ping6,
       vtysh_ping6_cmd,
       "ping ipv6 WORD",
       "Send echo messages\n"
       "IPv6 echo\n"
       "Ping destination address or hostname\n")
{
  execute_command ("ping6", 1, argv[0], NULL);
  return CMD_SUCCESS;
}

DEFUN (vtysh_traceroute6,
       vtysh_traceroute6_cmd,
       "traceroute ipv6 WORD",
       "Trace route to destination\n"
       "IPv6 trace\n"
       "Trace route to destination address or hostname\n")
{
  execute_command ("traceroute6", 1, argv[0], NULL);
  return CMD_SUCCESS;
}
#endif

DEFUN (vtysh_telnet,
       vtysh_telnet_cmd,
       "telnet WORD",
       "Open a telnet connection\n"
       "IP address or hostname of a remote system\n")
{
  execute_command ("telnet", 1, argv[0], NULL);
  return CMD_SUCCESS;
}

DEFUN (vtysh_telnet_port,
       vtysh_telnet_port_cmd,
       "telnet WORD PORT",
       "Open a telnet connection\n"
       "IP address or hostname of a remote system\n"
       "TCP Port number\n")
{
  execute_command ("telnet", 2, argv[0], argv[1]);
  return CMD_SUCCESS;
}

DEFUN (vtysh_ssh,
       vtysh_ssh_cmd,
       "ssh WORD",
       "Open an ssh connection\n"
       "[user@]host\n")
{
  execute_command ("ssh", 1, argv[0], NULL);
  return CMD_SUCCESS;
}

DEFUN (vtysh_start_shell,
       vtysh_start_shell_cmd,
       "start-shell",
       "Start UNIX shell\n")
{
  execute_command ("sh", 0, NULL, NULL);
  return CMD_SUCCESS;
}

DEFUN (vtysh_start_bash,
       vtysh_start_bash_cmd,
       "start-shell bash",
       "Start UNIX shell\n"
       "Start bash\n")
{
  execute_command ("bash", 0, NULL, NULL);
  return CMD_SUCCESS;
}

DEFUN (vtysh_start_zsh,
       vtysh_start_zsh_cmd,
       "start-shell zsh",
       "Start UNIX shell\n"
       "Start Z shell\n")
{
  execute_command ("zsh", 0, NULL, NULL);
  return CMD_SUCCESS;
}

/*==============================================================================
 * Commands and command setup
 */

/* The table of commands collected by vtysh/extract.pl
 */
extern cmd_table vtysh_collected_cmd_table ;

/* Commands for vtysh itself
 */
CMD_INSTALL_TABLE(static, vtysh_own_cmd_table, VTYSH_VD) =
{
  { VIEW_NODE,       &vtysh_show_daemons_cmd                            },
  { VIEW_NODE,       &vtysh_connect_daemons_cmd                         },
  { VIEW_NODE,       &vtysh_disconnect_daemons_cmd                      },
  { VIEW_NODE,       &no_vtysh_connect_daemons_cmd                      },

  { ENABLE_NODE,     &vtysh_show_daemons_cmd                            },
  { ENABLE_NODE,     &vtysh_connect_daemons_cmd                         },
  { ENABLE_NODE,     &vtysh_disconnect_daemons_cmd                      },
  { ENABLE_NODE,     &no_vtysh_connect_daemons_cmd                      },

  { VIEW_NODE,       &vtysh_ping_cmd                                    },
  { VIEW_NODE,       &vtysh_ping_ip_cmd                                 },
  { VIEW_NODE,       &vtysh_traceroute_cmd                              },
  { VIEW_NODE,       &vtysh_traceroute_ip_cmd                           },
  { VIEW_NODE,       &vtysh_ping6_cmd                                   },
  { VIEW_NODE,       &vtysh_traceroute6_cmd                             },
  { VIEW_NODE,       &vtysh_telnet_cmd                                  },
  { VIEW_NODE,       &vtysh_telnet_port_cmd                             },
  { VIEW_NODE,       &vtysh_ssh_cmd                                     },
  { VIEW_NODE,       &vtysh_start_shell_cmd                             },
  { VIEW_NODE,       &vtysh_start_bash_cmd                              },
  { VIEW_NODE,       &vtysh_start_zsh_cmd                               },

  { ENABLE_NODE,     &vtysh_ping_cmd                                    },
  { ENABLE_NODE,     &vtysh_ping_ip_cmd                                 },
  { ENABLE_NODE,     &vtysh_traceroute_cmd                              },
  { ENABLE_NODE,     &vtysh_traceroute_ip_cmd                           },
  { ENABLE_NODE,     &vtysh_ping6_cmd                                   },
  { ENABLE_NODE,     &vtysh_traceroute6_cmd                             },
  { ENABLE_NODE,     &vtysh_telnet_cmd                                  },
  { ENABLE_NODE,     &vtysh_telnet_port_cmd                             },
  { ENABLE_NODE,     &vtysh_ssh_cmd                                     },
  { ENABLE_NODE,     &vtysh_start_shell_cmd                             },
  { ENABLE_NODE,     &vtysh_start_bash_cmd                              },
  { ENABLE_NODE,     &vtysh_start_zsh_cmd                               },

  { CONFIG_NODE,     &vtysh_integrated_config_cmd                       },
  { CONFIG_NODE,     &no_vtysh_integrated_config_cmd                    },

  CMD_INSTALL_END
} ;

/* The table of call backs for the small number of common commands which have
 * special handling in the vtysh.
 */
vtysh_cmd_call_backs_t vtysh_cmd_call_back_table =
{
    .terminal_length         = vtysh_terminal_length,
    .show_history            = vtysh_show_history,
    .show_integrated_config  = vtysh_show_integrated_config,
    .write_integrated_config = vtysh_write_integrated_config,
} ;

/*------------------------------------------------------------------------------
 * Inititialise command handling for vtysh and install all nodes and vtysh
 * specific commands, then all the commands collected by vtysh/extract.pl.
 */
extern void
vtysh_cmd_init(void)
{
  /* Initialize commands -- install the basic nodes and the VTY_NODE
   */
  cmd_table_init (VTYSH_VD | ALL_RDS);

  /* Fire up all the lib/xx things which have commands that we now need.
   *
   * extract.pl does *not* pick these up -- there's no need, we can do it now !
   */
  distribute_list_cmd_init () ;
  access_list_cmd_init() ;
  if_cmd_init(NULL) ;
  if_rmap_cmd_init() ;
  keychain_cmd_init() ;
  prefix_list_cmd_init() ;
  route_map_cmd_init () ;

  /* Install specific vtysh commands
   */
  cmd_install_table(vtysh_own_cmd_table) ;

  /* Install the collected commands for all other daemons
   */
  cmd_install_table(vtysh_collected_cmd_table) ;

  /* All commands have now been installed
   */
  cmd_table_complete() ;
} ;

/*------------------------------------------------------------------------------
 * Initialise vty for vtysh, set up vtysh_vty and set output with no pager.
 *
 * Set up vtysh_vty initially for stdout/stderr.
 */
extern void
vtysh_vty_init (bool no_prefix)
{
  vty_init_for_vtysh() ;

  vtysh_vty = vty_vtysh_open(&vtysh_cmd_call_back_table, no_prefix) ;

  vtysh_pager_init() ;
} ;

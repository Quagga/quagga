/* Keystroke Buffering
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#include "misc.h"

#include "keystroke.h"

#include "list_util.h"

#include "memory.h"
#include "mqueue.h"

/*==============================================================================
 */

/*------------------------------------------------------------------------------
 * Parsing of incoming keystrokes.
 *
 * At present this code only handles 8-bit characters, which one assumes are
 * ISO8859-1 (or similar).  The encoding of keystrokes allows for characters
 * of up to 32-bits -- so could parse UTF-8 (or similar).
 *
 * Handles:
 *
 *   0. Nothing, returned as:
 *
 *        type      =  ks_null
 *        value     =  knull_eof
 *                     knull_not_eof
 *
 *        len       =  0
 *        truncated =  false
 *        broken    =  false
 *        buf       -- not used
 *
 *      This is returned when there is nothing there.
 *
 *      Note that this is NOT returned for NUL ('\0') characters.  Those are
 *      real characters (whose value just happens to be null).
 *
 *   1. Characters, returned as:
 *
 *        type      =  ks_char
 *        value     =  0x0..    -- the character value
 *                                 '\0' if truncated, malformed or EOF met
 *
 *        len       =  1..n     => length of character representation, or
 *                                 number of bytes in raw form if no good.
 *        truncated => too long for buffers
 *        broken    => malformed or EOF met
 *        buf       -- if OK, the representation for the character (UTF-8 ?)
 *                     if truncated or broken, the raw bytes
 *
 *      See notes below on the handling of '\r' and '\n'.
 *
 *   2. ESC X  -- where X is single character, other than '['.
 *
 *      Returned as:
 *
 *        type      =  ks_esc
 *        value     =  0x0..   -- the character value X
 *                                ('\0' if hit EOF)
 *
 *        len       =  1 (or 0 if EOF met)
 *        truncated =  false
 *        broken    => EOF met
 *        buf       =  copy of X (unless EOF met)
 *
 *   3. ESC [ ... X or CSI ... X -- ANSI escape
 *
 *      Returned as:
 *
 *        type      =  ks_csi
 *        value     =  0x0..   -- the character value X
 *                                ('\0' if hit EOF or malformed)
 *
 *        len       =  number of bytes in buf (excluding '\0' terminator)
 *        truncated => too long for buffers
 *        broken    => malformed or EOF met
 *        buf       -- bytes between ESC [ or CSI and terminating X
 *                     NB: '\0' terminated.
 *
 *      Note: an ANSI escape is malformed if a byte outside the range
 *            0x20..0x7F is found before the terminating byte.  The illegal
 *            byte is deemed not to be part of the sequence.
 *
 *   4. Telnet command -- IAC X ...
 *
 *      IAC IAC is treated as character 0xFF, so appears as a ks_char, or
 *      possibly as a component of an ESC or other sequence.
 *
 *      Returned as:
 *
 *        type      =  ks_iac
 *        value     =  0x0..   -- the value of X
 *                                ('\0' if hit EOF)
 *
 *        len       =  number of bytes in buf (0 if hit EOF after IAC)
 *        truncated => too long for buffers
 *        broken    => malformed or EOF met
 *        buf       -- the X and any further bytes,
 *                     but *excluding* any terminating IAC SE
 *
 *      Note: a Telnet command may appear at any time, including in the
 *            middle of any of the above "real" keystrokes.
 *
 *            This is made invisible to the "real" keystrokes, and the Telnet
 *            command(s) will appear before the incomplete real keystroke in
 *            the keystroke stream.
 *
 *      Note: there are three forms of Telnet command, and one escape.
 *
 *        1) IAC IAC is an escape, and maps simply to the value 0xFF,
 *           wherever it appears (except when reading an <option>).
 *
 *        2) IAC X   -- two bytes, where X < 250 (SB)
 *
 *        3) IAC X O -- three bytes, where X is 251..254 (WILL/WONT/DO/DONT)
 *
 *                      the O may be 0x00..0xFF (where 0xFF is *NOT* escaped)
 *
 *        4) IAC SB O ... IAC SE -- many bytes.
 *
 *                      the O may be 0x00..0xFF (where 0xFF is *NOT* escaped)
 *
 *                      the data ... is subject to IAC IAC escaping (so that
 *                      the terminating IAC SE is unambiguous).
 *
 *                      This implementation treats IAC X (where X is not IAC
 *                      and not SE) as an error -- terminating the command
 *                      before the IAC, and marking it malformed.  The IAC X
 *                      will then be taken as a new command.
 *
 *      Extended Option objects (O = 0xFF) are exotic, but the above will
 *      parse them.
 *
 *------------------------------------------------------------------------------
 * CR, LF and NUL
 *
 * Telnet requires CR LF newlines.  Where a CR is to appear alone it must be
 * followed by NUL.
 *
 * This code accepts:
 *
 *   * CR LF  pair, returning LF ('\n')  -- discards CR
 *
 *   * CR NUL pair, returning CR ('\r')  -- discards NUL
 *
 *   * CR CR  pair, returning CR ('\r')  == discards one CR (seems pointless)
 *
 *   * CR XX  pair, returning CR and XX  -- where XX is anything other than
 *                                          CR, LF or NUL
 *
 * It is tempting to throw away all NUL characters... but that doesn't seem
 * like a job for this level.
 *
 * As a small compromise, will not steal a NUL character.
 *
 * Note that NUL appears as a real character.  ks_null means literally nothing.
 */

/*------------------------------------------------------------------------------
 * Encoding of keystrokes in the keystroke FIFO.
 *
 * The first byte of a keystroke is as follows:
 *
 *   1. 0x00..0x7F  -- simple character   -- complete keystroke
 *
 *   2. 0x80..0xFF  -- compound keystroke -- further bytes (may) follow
 *
 * A compound keystroke comprises:
 *
 *   1. type of keystroke -- see enum keystroke_type
 *
 *   2. number of bytes that make up the keystroke
 *
 *      This is limited to keystroke_max_len.  Any keystroke longer than that
 *      is marked as truncated.
 *
 *   3. the bytes (0..keystroke_max_len of them)
 *
 * This is encoded as <first> <length> [ <bytes> ]
 *
 * Where:
 *
 *   <first> is 0x80..0xFF (as above):
 *
 *      b7 = 1
 *      b6 = 0 : "reserved"
 *      b5     : 0 => OK
 *               1 => broken object
 *      b4     : 0 => OK
 *               1 => truncated
 *
 *      b3..0  : type of keystroke -- so 16 types of keystroke
 *
 *   <length> is 0..keystroke_max_len
 *
 *   <bytes> (if present) are the bytes:
 *
 *      ks_null   -- should not appear in the FIFO
 *
 *      ks_char   -- character value, in network order, length 1..4
 *
 *                   BUT, if broken or truncated, the raw bytes received (must
 *                   be at least one).
 *
 *      ks_esc    -- byte that followed the ESC, length = 0..1
 *
 *                   Length 0 => EOF met => broken
 *
 *      ks_csi    -- bytes that followed the ESC [ or CSI, length 1..n
 *
 *                   The last byte is *always* the byte that terminated the
 *                   sequence, or '\0' if is badly formed, or hit EOF.
 *
 *                   If the sequence is truncated, then the last byte of the
 *                   sequence is written over the last buffered byte.
 *
 *      ks_iac    -- bytes of the telnet command, excluding the leading
 *                   IAC and the trailing IAC SE, length 1..n.
 *
 *                   IAC IAC pairs are not telnet commands.
 *
 *                   IAC IAC pairs between SB X O and IAC SE are reduced to
 *                   0xFF.
 *
 *                   Telnet commands are broken if EOF is met before the end
 *                   of the command, or get IAC X between SB X O and IAC SE
 *                   (where X is not IAC or SE).
 */

enum stream_state
{
  kst_between,          /* between keystrokes                           */

  kst_char,             /* collecting a multi-byte character            */
  kst_cr,               /* collecting '\r''\0' or '\r''\n'              */
  kst_esc,              /* collecting an ESC sequence                   */
  kst_csi,              /* collecting an ESC '[' or CSI sequence        */

  kst_iac,              /* seen an IAC -- previous state pushed.        */
  kst_iac_option,       /* waiting for option (just seen IAC X)         */
  kst_iac_sub,          /* waiting for IAC SE                           */
  kst_iac_sub_iac,      /* seen an IAC in iac_sub state                 */
} ;
typedef enum stream_state stream_state_t ;

struct keystroke_state
{
  stream_state_t state ;
  uint           len ;
  uint8_t        raw[keystroke_max_len] ;
} ;
typedef struct keystroke_state keystroke_state_t ;

typedef enum
{
  steal_none  = 0,           /* not stealing                         */
  steal_next,                   /* steal next complete kestroke         */
  steal_this,                   /* steal this keystroke when complete   */
  steal_done                    /* have a stolen keystroke in hand      */
} steal_state_t ;

enum { max_cntrl = 0x1F } ;

struct keystroke_stream
{
  vio_fifo      fifo ;          /* the keystrokes                       */

  keystroke_callback* callback ;
  void*               callback_context ;

  uint8_t       CSI ;           /* CSI character value (if any)         */

  bool          eof_met ;       /* nothing more to come                 */
  bool          timed_out ;     /* eof because timed out                */

  steal_state_t steal ;         /* whether/how/if stealing              */
  keystroke_t   stolen ;        /* what has been stolen                 */

  keystroke_state_t  in ;       /* current keystroke being collected    */

  keystroke_state_t  pushed_in ;
                                /* keystroke interrupted by IAC         */

  bool          interrupt[max_cntrl + 1] ;
} ;

/* Buffering of keystrokes                                              */

enum { keystroke_buffer_len = 2000 } ;  /* should be plenty !   */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void keystroke_in_push(keystroke_stream stream) ;
static void keystroke_in_pop(keystroke_stream stream) ;

inline static void keystroke_set_null(keystroke stroke,
                                                      keystroke_stream stream) ;
inline static void keystroke_set_char(keystroke stroke, uint8_t u) ;
inline static void keystroke_set_esc(keystroke stroke, keystroke_type_t type,
                                                      keystroke_stream stream) ;


inline static void keystroke_add_raw(keystroke_stream stream, uint8_t u) ;

static void keystroke_put_char(keystroke_stream stream, uint8_t u) ;
static void keystroke_put_esc(keystroke_stream stream, uint8_t u,
                                                      keystroke_flags_t flags) ;
static void keystroke_put_iac(keystroke_stream stream, keystroke_flags_t flags);

static void keystroke_put(keystroke_stream stream,
                               keystroke_type_t type, keystroke_flags_t flags,
                                                     uint8_t* bytes, uint len) ;

/*==============================================================================
 * Creating and freeing keystroke streams and keystroke stream buffers.
 */

/*------------------------------------------------------------------------------
 * Create and initialise a keystroke stream.
 *
 * Can set CSI character value.  '\0' => none.  (As does '\x1B' !)
 *
 * Can set any control character (other than '\0', '\r', '\n', ESC or CSI)
 * to be an "interrupt" character.
 *
 * The callback function is called when an IAC sequence or an "interrupt"
 * control character is seen.  The callback is:
 *
 *    bool callback(void* context, keystroke stroke)
 *
 * see: #define keystroke_iac_callback_args
 * and: typedef for keystroke_callback
 *
 * The callback must return true iff the keystroke has been dealt with, and
 * should NOT be stored for later processing.
 */
extern keystroke_stream
keystroke_stream_new(uint8_t csi_char, const char* interrupts,
                          keystroke_callback* callback, void* callback_context)
{
  keystroke_stream stream ;

  stream = XCALLOC(MTYPE_KEY_STREAM, sizeof(struct keystroke_stream)) ;

  /* Zeroising the structure sets:
   *
   *    fifo             = NULL -- see below
   *
   *    callback         = NULL -- see below
   *    callback_context = NULL -- see below
   *
   *    CSI        = '\0'  -- see below
   *
   *    eof_met    = false -- no EOF yet
   *    timed_out  = false -- not timed out yet
   *
   *    steal      = steal_none  -- no stealing going on
   *    stolen     = all zeros      not significant until steal_done
   *
   *    in.state   = kst_between
   *    in.len     = 0     -- nothing in the buffer
   *
   *    pushed_in.state ) ditto
   *    pushed_in.len   )
   *
   *    interrupt  = all false
   */
  confirm(kst_between == 0) ;
  confirm(steal_none == 0) ;

  stream->callback         = callback ;
  stream->callback_context = callback_context ;

  stream->fifo = vio_fifo_new(keystroke_buffer_len) ;

  stream->CSI = (csi_char != '\0') ? csi_char : 0x1B ;

  if (interrupts != NULL)
    {
      while (*interrupts != '\0')
        {
          if ((*interrupts > max_cntrl) || (*interrupts == '\r')
                                        || (*interrupts == '\n')
                                        || (*interrupts == '\x1B')
                                        || (*interrupts == stream->CSI))
            zabort("invalid 'interrupt' character") ;

          stream->interrupt[(uint8_t)*interrupts++] = true ;
        } ;
    } ;

  return stream ;
} ;

/*------------------------------------------------------------------------------
 * Free keystroke stream and all associated buffers.
 *
 * Returns NULL
 */
extern keystroke_stream
keystroke_stream_free(keystroke_stream stream)
{
  if (stream != NULL)
    {
      stream->fifo = vio_fifo_free(stream->fifo) ;

      XFREE(MTYPE_KEY_STREAM, stream) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 * Keystroke stream state
 */

/*------------------------------------------------------------------------------
 * See if given keystroke stream is empty
 *
 *             case kst_iac_option:
 * May or may not be at "EOF", see below.
 *
 * Returns: true <=> is empty
 */
extern bool
keystroke_stream_empty(keystroke_stream stream)
{
  return (stream == NULL) || vio_fifo_is_empty(stream->fifo) ;
} ;

/*------------------------------------------------------------------------------
 * See if given keystroke stream is at "EOF", that is:
 *
 *   * keystroke stream is empty
 *
 *   * EOF has been signalled by a suitable call of keystroke_input().
 *
 * Returns: true <=> is at EOF (or is NULL !)
 */
extern bool
keystroke_stream_at_eof(keystroke_stream stream)
{
  /* Note that when EOF is signalled, any partial keystroke in construction
   * is converted to a broken keystroke and placed in the stream.
   * (So eof_met => no partial keystroke.)
   */
  return (stream == NULL) || (vio_fifo_is_empty(stream->fifo)
                                                           && stream->eof_met) ;
} ;

/*------------------------------------------------------------------------------
 * See if given keystroke stream has seen "EOF" yet, that is:
 *
 *   * EOF has been signalled by a suitable call of keystroke_input().
 *
 * NB: the keystroke stream may not be empty.
 *
 * Returns: true <=> is at EOF (or is NULL !)
 */
extern bool
keystroke_stream_met_eof(keystroke_stream stream)
{
  return (stream == NULL) || stream->eof_met ;
} ;

/*------------------------------------------------------------------------------
 * Set keystroke stream to "EOF", that is:
 *
 *   * discard contents of the stream, including any partial keystroke.
 *
 *   * set the stream "eof_met" with or without timed_out
 */
extern void
keystroke_stream_set_eof(keystroke_stream stream, bool timed_out)
{
  vio_fifo_clear(stream->fifo) ;

  stream->eof_met         = true ;      /* essential information        */
  stream->timed_out       = timed_out ; /* variant of eof               */

  stream->steal           = steal_none ;        /* keep tidy            */
  stream->in.state        = kst_between ;
  stream->pushed_in.state = kst_between ;
} ;

/*------------------------------------------------------------------------------
 * Clear keystroke stream:
 *
 *   * discard contents of the stream, excluding any partial keystroke.
 *
 *   * if have stolen a keystroke, forget it and set back to steal_next.
 *
 *   * leaves eof and timed_out state as is
 *
 * May be used during the stream->callback() so that ^C cancels out anything
 * pending.
 */
extern void
keystroke_stream_clear(keystroke_stream stream)
{
  vio_fifo_clear(stream->fifo) ;

  if (stream->steal == steal_done)
    stream->steal = steal_next ;
} ;

/*==============================================================================
 * Getting and stealing keystrokes.
 */

/*------------------------------------------------------------------------------
 * Get next keystroke from keystroke stream
 *
 * Returns: true  => have a stroke type != ks_null
 *          false => stroke type is ks_null (may be EOF).
 */
extern bool
keystroke_get(keystroke_stream stream, keystroke stroke)
{
  int       b ;

  /* Get first byte and deal with FIFO empty response                   */
  b = vio_fifo_get_byte(stream->fifo) ;

  if (b < 0)
    {
      keystroke_set_null(stroke, stream) ;
      return false ;
    } ;

  /* Fetch first byte and deal with the simple character case           */
  if ((b & kf_compound) == 0)   /* Simple character ?                   */
    {
      keystroke_set_char(stroke, b) ;
      return true ;
    } ;

  /* Sex the compound keystroke                                         */

  stroke->type  = b & kf_type_mask ;
  stroke->value = 0 ;
  stroke->flags = b & kf_flag_mask ;
  stroke->len   = vio_fifo_get_byte(stream->fifo) ;     /* -1 <=> end   */

  /* Fetch what we need to the stroke buffer                            */
  if (stroke->len > 0)
    {
      uint get ;

      assert(stroke->len <= keystroke_max_len) ;

      get = vio_fifo_get_bytes(stream->fifo, stroke->buf, stroke->len) ;

      assert(get == stroke->len) ;
    }
  else
    assert(stroke->len == 0) ;

  /* Complete the process, depending on the type                        */
  switch (stroke->type)
  {
    case ks_null:
      zabort("ks_null found in FIFO") ;

    case ks_char:
      /* If character is well formed, set its value             */
      if (stroke->flags == 0)
        {
          uint i ;
          assert((stroke->len > 0) && (stroke->len <= 4)) ;
          for (i = 0 ; i < stroke->len ; ++i)
            stroke->value = (stroke->value << 8) + stroke->buf[i] ;

          /* NB: to do UTF-8 would need to create UTF form here */

        } ;
      break ;

    case ks_esc:
      /* If have ESC X, set value = X                           */
      if (stroke->len == 1)
        stroke->value = stroke->buf[0] ;
      else
        assert(stroke->len == 0) ;
      break ;

    case ks_csi:
      /* If have the final X, set value = X                     */
      /* Null terminate the parameters                          */
      if (stroke->len != 0)
        {
          --stroke->len ;
          stroke->value = stroke->buf[stroke->len] ;
        } ;
      stroke->buf[stroke->len] = '\0' ;
      break ;

    case ks_iac:
      /* If have the command byte after IAC, set value          */
      if (stroke->len > 0)
        stroke->value = stroke->buf[0] ;
      break ;

    default:
      zabort("unknown keystroke type") ;
  } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Steal keystroke from keystroke stream
 *
 * The first time this is called, sets the stream into "steal_next" state.  As
 * keystroke input is read -- see keystroke_input() -- the next keystroke to
 * arrive will be stolen.  The first time that this is called after a keystroke
 * has been stolen, the stolen keystroke will be returned, and the stealing
 * state reset.
 *
 * Returns: true  => have a stolen keystroke type (may be EOF)
 *          false => nothing yet
 */
extern bool
keystroke_steal(keystroke_stream stream, keystroke stroke)
{
  /* At EOF we immediately "steal" an eof or timed out ks_null.
   */
  if (stream->eof_met)
    {
      keystroke_set_null(stroke, stream) ;
      return true ;
    } ;

  /* If have a stolen keystroke in hand -- return that and clear
   * stealing state.
   */
  if (stream->steal == steal_done)
    {
      *stroke = *stream->stolen ;
      stream->steal = steal_none ;
      return true ;
    } ;

  /* Otherwise, if not already stealing, set stealing state.
   *
   * In any event, do not have a stolen keystroke, yet.
   */
  if (stream->steal == steal_none)
    stream->steal = steal_next ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Are we in the process of trying to steal a keystroke ?
 */
extern bool
keystroke_steal_state(keystroke_stream stream)
{
  return (stream->steal != steal_none) ;
} ;

/*------------------------------------------------------------------------------
 * Clear any keystroke stealing state.
 *
 * If keystroke has been stolen, it will be lost.
 */
extern void
keystroke_steal_clear(keystroke_stream stream)
{
  stream->steal = steal_none ;
} ;

/*==============================================================================
 * Reading of bytes into keystroke stream.
 */

/*------------------------------------------------------------------------------
 * Input raw bytes to given keystroke stream.
 *
 * Note that never steals broken or truncated keystrokes, or IAC.
 *
 * Passing len < 0 signals EOF to the keystroke_stream.
 *
 * Updates the stream and returns updated raw
 */
extern void
keystroke_input(keystroke_stream stream, uint8_t* ptr, uint len)
{
  uint8_t*  end ;

  /* Once EOF has been signalled, do not expect to receive any further input.
   *
   * However, keystroke_stream_set_eof() can set the stream artificially at
   * EOF, and that is honoured here.
   */
  if (stream->eof_met)
    return ;

  /* Normal processing
   *
   * Note that when manages to steal a keystroke sets steal == NULL, and
   * proceeds to collect any following keystrokes.
   */
  end = ptr + len ;
  while (ptr < end)
    {
      uint8_t u = *ptr++ ;

      /* IAC handling takes precedence over everything, except the <option>
       * byte, which may be EXOPL, which happens to be 255 as well !
       *
       * stream->iac means that the last thing seen was an IAC.
       *
       * First IAC changes state as required to process the next byte in
       * suitable (sub) state.
       */
      if (u == tn_IAC)
        {
          switch (stream->in.state)
          {
            /* An IAC may interrupt any ordinary keystroke sequence.
             *
             * Switch to kst_iac state and continue by fetching the next byte,
             * if any.
             */
            case kst_between:
            case kst_cr:
            case kst_esc:
            case kst_csi:
              keystroke_in_push(stream) ;       /* enter kst_iac state  */
              continue ;                        /* loop back            */

            /* An IAC in kst_iac state is an escaped IAC value, so pop the
             * state and proceed with the tn_IAC value.
             */
            case kst_iac:
              keystroke_in_pop(stream) ;
              break ;

            /* An IAC in kst_iac_option state is actually an EXOPL, so
             * proceed with the EXOPL value.
             */
            case kst_iac_option:
              confirm((uint)tn_IAC == (uint)to_EXOPL) ;
              break ;

            /* An IAC in kst_iac_sub state may be IAC IAC or IAC SE below.
             *
             * Switch to kst_iac_sub_iac state and continue by fetching the
             * next byte, if any.
             */
            case kst_iac_sub:
              stream->in.state = kst_iac_sub_iac ;
              continue ;                        /* loop back            */

            /* An IAC in kst_iac state is an escaped IAC value, so return
             * to kst_iac_sub state and proceed with the tn_IAC value.
             */
            case kst_iac_sub_iac:
              stream->in.state = kst_iac_sub ;
              break ;

            case kst_char:              /* TBD                  */
              zabort("impossible keystroke stream state") ;

            default:
              zabort("unknown keystroke stream state") ;
          } ;
        } ;

      /* Dealt with an IAC, proceed with byte per current state.
       */
      switch (stream->in.state)
      {
        /* In kst_between state we are between keystrokes, and may get anything.
         *
         * If waiting to steal the next keystroke, this is the (start of the)
         * next keystroke.
         */
        case kst_between:
          if (stream->steal == steal_next)
            stream->steal = steal_this ;

          if (u <= max_cntrl)
            {
              stream->in.len = 0 ;      /* make sure                    */

              if (u == '\0')            /* discard NUL                  */
                break ;

              if      (u == '\r')
                {
                  stream->in.state  = kst_cr ;
                  break ;
                } ;

              if (u == 0x1B)
                {
                  stream->in.state  = kst_esc ;
                  break ;
                } ;

              if (u == stream->CSI)     /* NB: CSI == 0x1B => no CSI    */
                {
                  stream->in.state  = kst_csi ;
                  break ;
                } ;

              if (stream->interrupt[u] && (stream->callback != NULL))
                {
                  keystroke_t stroke ;

                  keystroke_set_char(stroke, u) ;

                  if ((*stream->callback)(stream->callback_context, stroke))
                    break ;
                } ;
            } ;

          /* Simple single byte character: put or steal -- no state change.
           */
          keystroke_put_char(stream, u) ;

          break ;

        /* Multi-byte character handling -- TBD
         */
        case kst_char:
          zabort("impossible keystroke stream state") ;

        /* Byte following a '\r'
         */
        case kst_cr:            /* expecting something after CR         */
          stream->in.state  = kst_between ;

          switch (u)
            {
              case '\0':
                u = '\r' ;      /* treat '\r''\0' as '\r' (as expected) */

              case '\r':        /* treat '\r''\r' as '\r' (why not)     */
              case '\n':        /* treat '\r''\n' as '\n'               */
                break ;

              default:
                --ptr ;                 /* put back the duff XX         */
                if (u == tn_IAC)
                  keystroke_in_push(stream) ;
                                        /* re-enter kst_iac state       */
                u = '\r' ;      /* treat '\r'XX as '\r'                 */
            } ;

          keystroke_put_char(stream, u) ;       /* put or steal         */

          break ;

        /* Byte following ESC
         */
        case kst_esc:
          if      (u == '[')
            stream->in.state  = kst_csi ;
          else
            {
              keystroke_put_esc(stream, u, kf_null) ;
              stream->in.state = kst_between ;
            } ;
          break ;

        /* Byte(s) following CSI or ESC '['
         */
        case kst_csi:
          if ((u >= 0x20) && (u <= 0x3F))
            keystroke_add_raw(stream, u) ;
          else
            {
              keystroke_flags_t flags = kf_null ;

              stream->in.state = kst_between ;  /* done                 */

              if (stream->in.len >= keystroke_max_len)
                {
                  flags |= kf_truncated ;
                  stream->in.len = keystroke_max_len - 1 ;
                } ;

              if ((u < 0x40) || (u > 0x7E))
                {
                  --ptr ;               /* put back the duff XX         */
                  if (u == tn_IAC)
                    keystroke_in_push(stream) ;
                                        /* re-enter kst_iac state       */
                  u = '\0' ;
                  flags |= kf_broken ;
                } ;

              keystroke_put_esc(stream, u, flags) ;
            } ;
          break ;

        /* Byte following IAC -- where that byte is not itself IAC
         */
        case kst_iac:           /* IAC XX -- process the XX             */
          keystroke_add_raw(stream, u) ;

          if (u < tn_SB)
            {
              /* This is a simple IAC XX, one byte IAC
               */
              keystroke_put_iac(stream, kf_null) ;
              keystroke_in_pop(stream) ;
            }
          else
            {
              /* This is a multi-byte IAC, so set into kst_iac_option state.
               */
              stream->in.state   = kst_iac_option ;
            } ;
          break ;

        /* Byte following IAC XX -- expecting <option>
         *
         * If the XX was SB -> kst_iac_sub state: IAC SB <option> .... IAC SE
         *
         * Otherwise, this is a 3 byte IAC: IAC XX <option>
         */
        case kst_iac_option:
          assert(stream->in.len == 1) ;
          keystroke_add_raw(stream, u) ;

          if (stream->in.raw[0] == tn_SB)
            stream->in.state = kst_iac_sub ;
          else
            {
              keystroke_put_iac(stream, kf_null) ;
              keystroke_in_pop(stream) ;
            } ;
          break ;

        /* Bytes following IAC SB <option> ...
         *
         * This IAC is terminated by IAC SE.  If an IAC is seen, the next byte
         * is dealt with in kst_iac_sub_iac state.
         */
        case kst_iac_sub:
          assert(stream->in.raw[0] == tn_SB) ;

          keystroke_add_raw(stream, u) ;
          break ;

        /* in kst_iac_sub_iac state, expect:
         *
         *   IAC -- escaped 0xFF value -- dealt with already.
         *
         *   SE  -- end of kst_iac_sub state
         *
         *   Nothing else.
         *
         * So here the IAC is terminated, broken if don't have an SE, and the
         * stack popped.
         *
         * Then if was broken, put back the XX to be processed in kst_iac state.
         */
        case kst_iac_sub_iac:
          assert(stream->in.raw[0] == tn_SB) ;

          keystroke_put_iac(stream, ((u == tn_SE) ? kf_null : kf_broken)) ;
          keystroke_in_pop(stream) ;

          if (u != tn_SE)
            {
              --ptr ;                           /* put back the XX      */
              keystroke_in_push(stream) ;       /* enter kst_iac state  */
            } ;
          break ;


        default:
          zabort("unknown keystroke stream state") ;
      }
    } ;

  assert(ptr == end) ;
} ;

/*------------------------------------------------------------------------------
 * Have reached eof on the input.
 *
 * Any partial keystroke is converted into a broken keystroke and placed
 * at the end of the stream.
 *
 * Note that neither steals nor calls back broken keystrokes.
 *
 * Note that keystroke_stream_set_eof() is more brutal, and discards all
 * buffered and/or stolen keystrokes.
 */
extern void
keystroke_input_eof(keystroke_stream stream)
{
  stream->eof_met    = true ;
  stream->timed_out  = false ;

  /* Loop to deal with any pending IAC and partial keystroke.
   *
   * An IAC in the middle of a real keystroke sequence appears before
   * it.  Do the same here, even with broken sequences.
   *
   * A partial IAC sequence may have pushed a partial real keystroke
   * sequence -- so loop until have dealt with that.
   */
  while (stream->in.state != kst_between)
    {
      switch (stream->in.state)
      {
        case kst_cr:            /* expecting something after CR     */
          keystroke_add_raw(stream, '\r') ;
          keystroke_put(stream, ks_char, kf_broken,
                                           stream->in.raw, stream->in.len) ;
          break ;

        case kst_esc:           /* expecting rest of escape         */
        case kst_csi:
          keystroke_put_esc(stream, '\0', kf_broken) ;
          break ;

        /* For incomplete IAC sequences, insert a broken sequence.
         *
         * Note that where have just seen an IAC itself, we insert that
         * now at the end of the sequence.
         */
        case kst_iac:           /* expecting something after IAC    */
        case kst_iac_sub_iac:
          keystroke_add_raw(stream, tn_IAC) ;
          fall_through ;

        case kst_iac_option:    /* expecting rest of IAC XX         */
        case kst_iac_sub:
          keystroke_put_iac(stream, kf_broken) ;

          break ;

        case kst_between:       /* Cannot be this                   */
        case kst_char:          /* TBD                              */
          zabort("impossible keystroke stream state") ;

        default:
          zabort("unknown keystroke stream state") ;
      } ;

      keystroke_in_pop(stream) ;    /* pops kst_between, when all done */
    } ;
} ;

/*------------------------------------------------------------------------------
 * Single level stack for keystroke input state, so that can handle IAC
 * sequences transparently.
 */

/* Push current state and set new current state to process byte after IAC.
 */
static void
keystroke_in_push(keystroke_stream stream)
{
  assert(stream->pushed_in.state == kst_between) ;

  stream->pushed_in = stream->in ;

  stream->in.state  = kst_iac ;
  stream->in.len    = 0 ;
} ;

/* Pop the pushed state and clear the pushed state to kst_between (so further
 * pop will return that)
 */
static void
keystroke_in_pop(keystroke_stream stream)
{
  stream->in = stream->pushed_in ;

  stream->pushed_in.state = kst_between ;
  stream->pushed_in.len   = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set given keystroke to ks_null -- and set to knull_eof if stream->eof_met
 */
inline static void
keystroke_set_null(keystroke stroke, keystroke_stream stream)
{
  stroke->type  = ks_null ;
  stroke->value = stream->eof_met ? (stream->timed_out ? knull_timed_out
                                                       : knull_eof)
                                  : knull_not_eof ;
  stroke->flags = 0 ;
  stroke->len   = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set given keystroke to simple character
 */
inline static void
keystroke_set_char(keystroke stroke, uint8_t u)
{
  stroke->type    = ks_char ;
  stroke->value   = u ;

  stroke->flags   = 0 ;
  stroke->len     = 1 ;
  stroke->buf[0]  = u ;
} ;

/*------------------------------------------------------------------------------
 * Set given keystroke to given escape
 */
inline static void
keystroke_set_esc(keystroke stroke, keystroke_type_t type,
                                                        keystroke_stream stream)
{
  int len ;

  len = stream->in.len - 1 ;    /* exclude the escape terminator        */
  assert((len >= 0) && (len < keystroke_max_len)) ;

  if (len > 0)
    memcpy(stroke->buf, stream->in.raw, len) ;
  stroke->buf[len] = '\0' ;

  stroke->type   = type ;
  stroke->value  = stream->in.raw[len] ;        /* last character       */
  stroke->flags  = 0 ;
  stroke->len    = len ;
} ;

/*------------------------------------------------------------------------------
 * Set given keystroke to given escape
 */
inline static void
keystroke_set_iac(keystroke stroke, keystroke_stream stream)
{
  assert((stream->in.len >= 1) && (stream->in.len <= keystroke_max_len)) ;

  memcpy(stroke->buf, stream->in.raw, stream->in.len) ;

  stroke->type  = ks_iac ;
  stroke->value = stream->in.raw[0] ;
  stroke->flags = 0 ;
  stroke->len   = stream->in.len ;
} ;

/*==============================================================================
 * Functions to support keystroke_input.
 */

/*------------------------------------------------------------------------------
 * If possible, add character to the stream->in.raw[] buffer
 *
 * Note that the stream->in.len continues counting beyond the end of the buffer.
 */
static inline void
keystroke_add_raw(keystroke_stream stream, uint8_t u)
{
  if (stream->in.len < keystroke_max_len)
    stream->in.raw[stream->in.len] = u ;

  ++stream->in.len ;
} ;

/*------------------------------------------------------------------------------
 * Store simple 8 bit character value, or steal it
 */
static void
keystroke_put_char(keystroke_stream stream, uint8_t u)
{
  if (stream->steal == steal_this)
    {
      keystroke_set_char(stream->stolen, u) ;
      stream->steal = steal_done ;
    }
  else
    {
      if (u < kf_compound)
        vio_fifo_put_byte(stream->fifo, u) ;
      else
        keystroke_put(stream, ks_char, kf_null, &u, 1) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Store simple ESC or CSI, or steal it (but won't steal if broken/truncated)
 */
static void
keystroke_put_esc(keystroke_stream stream, uint8_t u, keystroke_flags_t flags)
{
  keystroke_type_t type ;

  keystroke_add_raw(stream, u) ;

  type = (stream->in.state == kst_esc) ? ks_esc : ks_csi ;

  if ((stream->steal == steal_this)
                                 && ((flags & (kf_broken | kf_truncated)) == 0))
    {
      keystroke_set_esc(stream->stolen, type, stream) ;
      stream->steal = steal_done ;
    }
  else
    keystroke_put(stream, type, flags, stream->in.raw, stream->in.len) ;
} ;

/*------------------------------------------------------------------------------
 * Store IAC -- if not broken, send it via any call-back.
 */
static void
keystroke_put_iac(keystroke_stream stream, keystroke_flags_t flags)
{
  bool dealt_with = false ;

  if ((stream->callback != NULL) && ((flags & (kf_broken | kf_truncated)) == 0))
    {
      keystroke_t stroke ;

      keystroke_set_iac(stroke, stream) ;

      dealt_with = (*stream->callback)(stream->callback_context, stroke) ;
    } ;

  if (!dealt_with)
    keystroke_put(stream, ks_iac, flags, stream->in.raw, stream->in.len) ;
} ;

/*------------------------------------------------------------------------------
 * Store <first> <len> [<bytes>] -- compound keystroke.
 *
 * If len == 0, bytes may be NULL
 */
static void
keystroke_put(keystroke_stream stream, keystroke_type_t type,
                                          keystroke_flags_t flags,
                                                       uint8_t* bytes, uint len)
{
  if (len > keystroke_max_len)
    {
      len = keystroke_max_len ;
      flags |= kf_truncated ;
    } ;

  vio_fifo_put_byte(stream->fifo, kf_compound | flags | type) ;
  vio_fifo_put_byte(stream->fifo, len) ;

  if (len > 0)
    vio_fifo_put_bytes(stream->fifo, (void*)bytes, len) ;
} ;

/*==============================================================================
 */

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

#include <stdbool.h>
#include <string.h>

#include "keystroke.h"

#include "list_util.h"

#include "memory.h"
#include "mqueue.h"
#include "zassert.h"

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
  kst_null,             /* nothing special (but see iac)                */

  kst_char,             /* collecting a multi-byte character            */
  kst_cr,               /* collecting '\r''\0' or '\r''\n'              */
  kst_esc,              /* collecting an ESC sequence                   */
  kst_csi,              /* collecting an ESC '[' or CSI sequence        */

  kst_iac_option,       /* waiting for option (just seen IAC X)         */
  kst_iac_sub,          /* waiting for IAC SE                           */
} ;

struct keystroke_state
{
  enum stream_state state ;
  unsigned      len ;
  uint8_t       raw[keystroke_max_len] ;
} ;

struct keystroke_stream
{
  vio_fifo_t    fifo ;          /* the keystrokes -- embedded           */

  keystroke_callback* iac_callback ;
  void*               iac_callback_context ;

  uint8_t       CSI ;           /* CSI character value (if any)         */

  bool          eof_met ;       /* nothing more to come                 */

  bool          steal_this ;    /* steal current keystroke when complete */

  bool          iac ;           /* last character was an IAC            */

  struct keystroke_state  in ;  /* current keystroke being collected    */

  struct keystroke_state  pushed_in ;
                                /* keystroke interrupted by IAC         */
} ;

/* Buffering of keystrokes                                              */

enum { keystroke_buffer_len = 2000 } ;  /* should be plenty !   */

/*------------------------------------------------------------------------------
 * Prototypes
 */
static void keystroke_in_push(keystroke_stream stream, uint8_t u) ;
static void keystroke_in_pop(keystroke_stream stream) ;

inline static bool keystroke_set_null(keystroke_stream stream,
                                                             keystroke stroke) ;
inline static uint8_t keystroke_get_byte(keystroke_stream stream) ;
inline static void keystroke_add_raw(keystroke_stream stream, uint8_t u) ;
static void keystroke_put_char(keystroke_stream stream, uint32_t u) ;
inline static void keystroke_put_esc(keystroke_stream stream, uint8_t u,
                                                                      int len) ;
inline static void keystroke_put_csi(keystroke_stream stream, uint8_t u) ;
static void keystroke_put_iac_one(keystroke_stream stream, uint8_t u);
static void keystroke_put_iac_long(keystroke_stream stream, bool broken) ;
static void keystroke_clear_iac(keystroke_stream stream) ;
static void keystroke_steal_char(keystroke steal, keystroke_stream stream,
                                                                    uint8_t u) ;
static void keystroke_steal_esc(keystroke steal, keystroke_stream stream,
                                                                    uint8_t u) ;
static void keystroke_steal_csi(keystroke steal, keystroke_stream stream,
                                                                    uint8_t u) ;
static void keystroke_put(keystroke_stream stream, enum keystroke_type type,
                                         bool broken, uint8_t* bytes, int len) ;

/*==============================================================================
 * Creating and freeing keystroke streams and keystroke stream buffers.
 */

/*------------------------------------------------------------------------------
 * Create and initialise a keystroke stream.
 *
 * Can set CSI character value.  '\0' => none.  (As does '\x1B' !)
 *
 * The callback function is called when an IAC sequence is seen, the callback
 * is:
 *
 *    bool callback(void* context, keystroke stroke)
 *
 * see: #define keystroke_iac_callback_args
 * and: typedef for keystroke_callback
 *
 * The callback must return true iff the IAC sequence has been dealt with, and
 * should NOT be stored for later processing.
 */
extern keystroke_stream
keystroke_stream_new(uint8_t csi_char, keystroke_callback* iac_callback,
                                                     void* iac_callback_context)
{
  keystroke_stream stream ;

  stream = XCALLOC(MTYPE_KEY_STREAM, sizeof(struct keystroke_stream)) ;

  /* Zeroising the structure sets:
   *
   *    iac_callback         = NULL -- none
   *    iac_callback_context = NULL -- none
   *
   *    eof_met  = false -- no EOF yet
   *    steal    = false -- no stealing set
   *    iac      = false -- last character was not an IAC
   *
   *    in.state = kst_null
   *    in.len   = 0     -- nothing in the buffer
   *
   *    pushed_in.state ) ditto
   *    pushed_in.len   )
   */
  confirm(kst_null == 0) ;

  stream->iac_callback         = iac_callback ;
  stream->iac_callback_context = iac_callback_context ;

  vio_fifo_init_new(stream->fifo, keystroke_buffer_len) ;

  stream->CSI = (csi_char != '\0') ? csi_char : 0x1B ;

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
      vio_fifo_reset(stream->fifo, keep_it) ;

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
 * May or may not be at "EOF", see below.
 *
 * Returns: true <=> is empty
 */
extern bool
keystroke_stream_empty(keystroke_stream stream)
{
  return (stream == NULL) || vio_fifo_empty(stream->fifo) ;
} ;

/*------------------------------------------------------------------------------
 * See if given keystroke stream is at "EOF", that is:
 *
 *   * keystroke stream is empty
 *
 *   * there is no partial keystroke in construction
 *
 *   * EOF has been signalled by a suitable call of keystroke_input().
 *
 * Returns: true <=> is at EOF
 */
extern bool
keystroke_stream_eof(keystroke_stream stream)
{
  /* Note that when EOF is signalled, any partial keystroke in construction
   * is converted to a broken keystroke and placed in the stream.
   * (So eof_met => no partial keystroke.)
   */
  return (stream == NULL) || (vio_fifo_empty(stream->fifo) && stream->eof_met);
} ;

/*------------------------------------------------------------------------------
 * Set keystroke stream to "EOF", that is:
 *
 *   * discard contents of the stream, including any partial keystroke.
 *
 *   * set the stream "eof_met".
 */
extern void
keystroke_stream_set_eof(keystroke_stream stream)
{
  vio_fifo_reset(stream->fifo, keep_it) ;

  stream->eof_met         = true ;      /* essential information        */

  stream->steal_this      = false ;     /* keep tidy                    */
  stream->iac             = false ;
  stream->in.state        = kst_null ;
  stream->pushed_in.state = kst_null ;
} ;

/*==============================================================================
 * Input raw bytes to given keyboard stream.
 *
 * To steal the next keystroke, pass 'steal' = address of a keystroke structure.
 * Otherwise, pass NULL.
 *
 * Note: when trying to steal, will complete any partial keystroke before
 *       stealing the next one.  May exit from here:
 *
 *         a. without having completed the partial keystroke.
 *
 *         b. without having completed the keystroke to be stolen.
 *
 *       State (b) is remembered by the keystroke_stream.
 *
 *       Caller may have to call several times with steal != NULL to get a
 *       keystroke.
 *
 * If steal != NULL the keystroke will be set to the stolen keystroke.  That
 * will be type ks_null if nothing was available, and may be knull_eof.
 *
 * Note that never steals broken or truncated keystrokes.
 *
 * Passing len == 0 and ptr == NULL signals EOF to the keystroke_stream.
 *
 * Updates the stream and returns updated raw
 */
extern void
keystroke_input(keystroke_stream stream, uint8_t* ptr, size_t len,
                                                                keystroke steal)
{
  uint8_t*  end ;

  /* Deal with EOF if required
   *
   * Any partial keystroke is converted into a broken keystroke and placed
   * at the end of the stream.
   *
   * Note that this occurs before any attempt to steal a keystroke -- so can
   * never steal a broken keystroke.
   */
  if ((len == 0) && (ptr == NULL))
    {
      stream->eof_met    = true ;
      stream->steal_this = false ;

      /* Loop to deal with any pending IAC and partial keystroke.
       *
       * An IAC in the middle of a real keystroke sequence appears before
       * it.  Do the same here, even with broken sequences.
       *
       * A partial IAC sequence may have pushed a partial real keystroke
       * sequence -- so loop until have dealt with that.
       */
      do
        {
          switch (stream->in.state)
          {
            case kst_null:          /* not expecting anything, unless iac  */
              keystroke_clear_iac(stream) ;
              break ;

            case kst_cr:            /* expecting something after CR         */
              keystroke_clear_iac(stream) ;

              stream->in.len = 0 ;
              keystroke_add_raw(stream, '\r') ;
              keystroke_put(stream, ks_char, true,
                                               stream->in.raw, stream->in.len) ;
              break ;

            case kst_esc:           /* expecting rest of escape             */
              keystroke_clear_iac(stream) ;

              keystroke_put_esc(stream, '\0', 0) ;
              break ;

            case kst_csi:
              keystroke_clear_iac(stream) ;

              keystroke_put_csi(stream, '\0') ;
              break ;

            case kst_iac_option:    /* expecting rest of IAC                */
              assert(!stream->iac) ;
              /* fall through   */
            case kst_iac_sub:
              keystroke_put_iac_long(stream, true) ;

              /* For kst_iac_sub, an incomplete IAC could be anything, so
               * don't include in the broken IAC, but don't lose it
               * either.
               */
              keystroke_clear_iac(stream) ;
              break ;

            case kst_char:              /* TBD                          */
              zabort("impossible keystroke stream state") ;

            default:
              zabort("unknown keystroke stream state") ;
          } ;

          assert(!stream->iac) ;        /* must have dealt with this    */

          keystroke_in_pop(stream) ;    /* pops kst_null, when all done */

        } while (stream->in.state != kst_null) ;
    } ;

  /* Update the stealing state
   *
   *      steal != NULL => want to steal a keystroke
   *
   *                       If do not wish to steal now, must clear any
   *                       remembered steal_this state.
   *
   * stream->steal_this => steal the next keystroke to complete.
   *
   *                       If want to steal a keystroke, this is set if
   *                       currently "between" keystrokes, or later when
   *                       reach that condition.
   *
   *                       Once set, this is remembered across calls to
   *                       keystroke_input(), while still wish to steal.
   */
  if (steal == NULL)
    stream->steal_this = false;   /* not now required                   */
  else
    stream->steal_this = (stream->in.state == kst_null) ;
                                  /* want to and can can steal the next
                                     keystroke  that completes          */

  /* Once EOF has been signalled, do not expect to receive any further input.
   *
   * However, keystroke_stream_set_eof() can set the stream artificially at
   * EOF, and that is honoured here.
   */
  if (stream->eof_met)
    len = 0 ;

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
       * First IAC sets the flag.  On the next byte:
       *
       *   * if is IAC, clears stream->iac, and lets the escaped IAC value
       *     through for further processing -- NOT in IAC state.
       *
       *   * if is not IAC, will be let through for further processing, in
       *     IAC state.
       */
      if ((u == tn_IAC) && (stream->in.state != kst_iac_option))
        {
          if (stream->iac)
            stream->iac = false ;   /* IAC IAC => single IAC byte value */
          else
            {
              stream->iac = true ;  /* seen an IAC                      */
              continue ;            /* wait for next character          */
            } ;
        } ;

      /* If stream->iac, then need to worry about IAC XX
       *
       * Note that IAC sequences are entirely invisible to the general
       * stream of keystrokes.  So... IAC sequences may appear in the middle
       * of multi-byte general keystroke objects.
       *
       * If this is not a simple 2 byte IAC, then must put whatever was
       * collecting to one side, and deal with the IAC.
       *
       * Note: not interested in stealing an IAC object.
       */
      if (stream->iac)
        {
          stream->iac = false ;         /* expect will eat the IAC XX   */

          switch (stream->in.state)
          {
            case kst_null:
            case kst_cr:
            case kst_esc:
            case kst_csi:
              if (u < tn_SB)
                /* This is a simple IAC XX, one byte IAC                */
                keystroke_put_iac_one(stream, u) ;
              else
                /* This is a multi-byte IAC, so push whatever real
                 * keystroke sequence is currently on preparation, and
                 * set into kst_iac_option state.
                 */
                keystroke_in_push(stream, u) ;
              break ;

            case kst_iac_sub:
              assert(stream->in.raw[0] == tn_SB) ;

              if (u != tn_SE)
                {
                  --ptr ;               /* put back the XX              */
                  stream->iac = true ;  /* put back the IAC             */
                } ;

              keystroke_put_iac_long(stream, (u != tn_SE)) ;
              keystroke_in_pop(stream) ;
              break ;

            case kst_char:              /* TBD                  */
            case kst_iac_option:
              zabort("impossible keystroke stream state") ;

            default:
              zabort("unknown keystroke stream state") ;
          } ;

          continue ;
        } ;

      /* No IAC complications... proceed per current state              */
      switch (stream->in.state)
      {
        case kst_null:          /* Expecting anything                   */
          stream->steal_this = (steal != NULL) ;

          if      (u == '\r')
            stream->in.state  = kst_cr ;
          else if (u == 0x1B)
            stream->in.state  = kst_esc ;
          else if (u == stream->CSI)    /* NB: CSI == 0x1B => no CSI    */
            {
              stream->in.len    = 0 ;
              stream->in.state  = kst_csi ;
            }
          else
            {
              /* Won't steal NUL                        */
              if (!stream->steal_this || (u == '\0'))
                keystroke_put_char(stream, u) ;
              else
                {
                  keystroke_steal_char(steal, stream, u) ;
                  stream->steal_this = false ;
                  steal = NULL ;
                } ;

              stream->in.state  = kst_null ;
            } ;
          break ;

        case kst_char:          /* TBD                                  */
          zabort("impossible keystroke stream state") ;

        case kst_cr:            /* expecting something after CR         */
          if ((u != '\n') && (u != '\r'))
            {
              if (u != '\0')
                {
                  --ptr ;       /* put back the duff XX         */
                  stream->iac = (u == tn_IAC) ;
                                /* re=escape if is IAC          */
                } ;
              u = '\r' ;
            } ;

          if (!stream->steal_this)
            keystroke_put_char(stream, u) ;
          else
            {
              keystroke_steal_char(steal, stream, u) ;
              stream->steal_this = false ;
              steal = NULL ;
            } ;

          stream->in.state  = kst_null ;
          break ;

        case kst_esc:           /* Expecting XX after ESC               */
          if      (u == '[')
            {
              stream->in.len    = 0 ;
              stream->in.state  = kst_csi ;
            }
          else
            {
              if (!stream->steal_this)
                keystroke_put_esc(stream, u, 1) ;
              else
                {
                  keystroke_steal_esc(steal, stream, u) ;
                  stream->steal_this = false ;
                  steal = NULL ;
                } ;

              stream->in.state = kst_null ;
            } ;
          break ;

        case kst_csi:           /* Expecting ... after ESC [ or CSI     */
          if ((u >= 0x20) && (u <= 0x3F))
            keystroke_add_raw(stream, u) ;
          else
            {
              bool ok ;

              ok = stream->in.len < keystroke_max_len ;
                                         /* have room for terminator    */

              if ((u < 0x40) || (u > 0x7E))
                {
                  --ptr ;               /* put back the duff XX         */
                  stream->iac = (u == tn_IAC) ;
                                        /* re=escape if is IAC          */
                  u = '\0' ;
                  ok = false ;          /* broken                       */
                } ;

              if (!stream->steal_this || !ok)
                keystroke_put_csi(stream, u) ;
              else
                {
                  keystroke_steal_csi(steal, stream, u) ;
                  stream->steal_this = false ;
                  steal = NULL ;
                } ;
              stream->in.state = kst_null ;
            } ;
          break ;

        case kst_iac_option:    /* Expecting <option> after IAC XX      */
          assert(stream->in.len == 1) ;
          keystroke_add_raw(stream, u) ;

          if (stream->in.raw[0] == tn_SB)
            stream->in.state = kst_iac_sub ;
          else
            {
              keystroke_put_iac_long(stream, false) ;
              keystroke_in_pop(stream) ;
            } ;
          break ;

        case kst_iac_sub:       /* Expecting sub stuff                  */
          assert(stream->in.raw[0]== tn_SB) ;

          keystroke_add_raw(stream, u) ;
          break ;

        default:
          zabort("unknown keystroke stream state") ;
      }
    } ;
  assert(ptr == end) ;

  /* If did not steal a keystroke, return a ks_null -- which may be
   * a knull_eof.
   */
  if (steal != NULL)
    keystroke_set_null(stream, steal) ;
} ;

/*------------------------------------------------------------------------------
 * Single level stack for keystroke input state, so that can handle IAC
 * sequences transparently.
 */

/* Push current state and set new current state for start of IAC option
 * sequence.
 */
static void
keystroke_in_push(keystroke_stream stream, uint8_t u)
{
  assert(stream->pushed_in.state == kst_null) ;

  stream->pushed_in    = stream->in ;

  stream->in.len       = 1 ;
  stream->in.raw[0]    = u ;

  stream->in.state     = kst_iac_option ;
} ;

/* Pop the pushed state and clear the pushed state to kst_null          */
static void
keystroke_in_pop(keystroke_stream stream)
{
  stream->in = stream->pushed_in ;

  stream->pushed_in.state = kst_null ;
  stream->pushed_in.len   = 0 ;
} ;

/*==============================================================================
 * Fetch next keystroke from keystroke stream
 *
 * Returns: true  => have a stroke type != ks_null
 *          false => stroke type is ks_null (may be EOF).
 */
extern bool
keystroke_get(keystroke_stream stream, keystroke stroke)
{
  int       b ;
  uint8_t*  p ;
  uint8_t*  e ;

  /* Get first byte and deal with FIFO empty response                   */
  b = vio_fifo_get_byte(stream->fifo) ;

  if (b < 0)
    return keystroke_set_null(stream, stroke) ;

  /* Fetch first byte and deal with the simple character case           */
  if ((b & kf_compound) == 0)   /* Simple character ?                   */
    {
      stroke->type    = ks_char ;
      stroke->value   = b ;

      stroke->flags   = 0 ;
      stroke->len     = 1 ;
      stroke->buf[0]  = b ;

      return true ;
    } ;

  /* Sex the compound keystroke                                         */

  stroke->type  = b & kf_type_mask ;
  stroke->value = 0 ;
  stroke->flags = b & kf_flag_mask ;
  stroke->len   = keystroke_get_byte(stream) ;

  assert(stroke->len <= keystroke_max_len) ;

  /* Fetch what we need to the stroke buffer                            */
  p = stroke->buf ;
  e = p + stroke->len ;

  while (p < e)
    *p++ = keystroke_get_byte(stream) ;

  p = stroke->buf ;

  /* Complete the process, depending on the type                        */
  switch (stroke->type)
  {
    case ks_null:
      zabort("ks_null found in FIFO") ;

    case ks_char:
      /* If character is well formed, set its value             */
      if (stroke->flags == 0)
        {
          assert((stroke->len > 0) && (stroke->len <= 4)) ;
          while (p < e)
            stroke->value = (stroke->value << 8) + *p++ ;

          /* NB: to do UTF-8 would need to create UTF form here */

        } ;
      break ;

    case ks_esc:
      /* If have ESC X, set value = X                           */
      if (stroke->len == 1)
        stroke->value = *p ;
      else
        assert(stroke->len == 0) ;
      break ;

    case ks_csi:
      /* If have the final X, set value = X                     */
      /* Null terminate the parameters                          */
      if (stroke->len != 0)
        {
          --e ;
          stroke->value = *e ;
          --stroke->len ;
        } ;
      *e = '\0' ;
      break ;

    case ks_iac:
      /* If have the command byte after IAC, set value          */
      if (stroke->len > 0)
        stroke->value = *p ;
      break ;

    default:
      zabort("unknown keystroke type") ;
  } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Set given keystroke to ks_null -- and set to knull_eof if stream->eof_met
 *
 * Returns: 0
 */
inline static bool
keystroke_set_null(keystroke_stream stream, keystroke stroke)
{
  stroke->type  = ks_null ;
  stroke->value = stream->eof_met ? knull_eof : knull_not_eof ;

  stroke->flags = 0 ;
  stroke->len   = 0 ;

  return false ;
} ;

/*------------------------------------------------------------------------------
 * Fetch 2nd or subsequent byte of keystroke.
 *
 * NB: it is impossible for partial keystrokes to be written, so this treats
 *     buffer empty as a FATAL error.
 */
inline static uint8_t
keystroke_get_byte(keystroke_stream stream)
{
  int b = vio_fifo_get_byte(stream->fifo) ;

  passert(b >= 0) ;

  return b ;
} ;

/*==============================================================================
 * Functions to support keystroke_input.
 */

/*------------------------------------------------------------------------------
 * If possible, add character to the stream->in.raw[] buffer
 */
static inline void
keystroke_add_raw(keystroke_stream stream, uint8_t u)
{
  if (stream->in.len < keystroke_max_len)
    stream->in.raw[stream->in.len] = u ;

  ++stream->in.len ;
} ;

/*------------------------------------------------------------------------------
 * Store simple character value
 */
static void
keystroke_put_char(keystroke_stream stream, uint32_t u)
{
  if (u < 0x80)
    vio_fifo_put_byte(stream->fifo, (uint8_t)u) ;
  else
    {
      uint8_t  buf[4] ;
      uint8_t* p ;

      p = buf + 4 ;

      do
        {
          *(--p) = u & 0xFF ;
          u >>= 8 ;
        }
      while (u != 0) ;

      keystroke_put(stream, ks_char, 0, p, (buf + 4) - p) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Store simple ESC.  Is broken if length (after ESC) == 0 !
 */
static void
keystroke_put_esc(keystroke_stream stream, uint8_t u, int len)
{
  keystroke_put(stream, ks_esc, (len == 0), &u, len) ;
} ;

/*------------------------------------------------------------------------------
 * Store CSI.
 *
 * Plants the last character of the CSI in the buffer, even if has to overwrite
 * the existing last character -- the sequence is truncated, but this way at
 * least the end of the sequence is preserved.
 *
 * Is broken if u == '\0'.  May also be truncated !
 */
static void
keystroke_put_csi(keystroke_stream stream, uint8_t u)
{
  int l ;

  l = stream->in.len++ ;

  if (l >= keystroke_max_len)
    l = keystroke_max_len - 1 ;

  stream->in.raw[l] = u ;       /* plant terminator     */

  keystroke_put(stream, ks_csi, (u == '\0'), stream->in.raw, stream->in.len) ;
} ;

/*------------------------------------------------------------------------------
 * Store IAC -- if not broken, send it via any call-back.
 */
static void
keystroke_put_iac(keystroke_stream stream, bool broken, uint8_t* bytes, int len)
{
  bool dealt_with = false ;

  if (!broken && (stream->iac_callback != NULL))
    {
      struct keystroke stroke ;

      assert((len >= 1) && (bytes != NULL) && (len <= keystroke_max_len)) ;

      stroke.type  = ks_iac ;
      stroke.value = bytes[0] ;
      stroke.flags = 0 ;
      stroke.len   = len ;

      memcpy(&stroke.buf, bytes, len) ;

      dealt_with = (*stream->iac_callback)(stream->iac_callback_context,
                                                                      &stroke) ;
    } ;

  if (!dealt_with)
    keystroke_put(stream, ks_iac, broken, bytes, len) ;
} ;

/*------------------------------------------------------------------------------
 * Store one byte IAC.
 */
static void
keystroke_put_iac_one(keystroke_stream stream, uint8_t u)
{
  keystroke_put_iac(stream, false, &u, 1) ;
} ;

/*------------------------------------------------------------------------------
 * Store long IAC.  Is broken if says it is.
 */
static void
keystroke_put_iac_long(keystroke_stream stream, bool broken)
{
  keystroke_put_iac(stream, broken, stream->in.raw, stream->in.len) ;
} ;

/*------------------------------------------------------------------------------
 * If in IAC state, issue broken IAC and clear state.
 */
static void
keystroke_clear_iac(keystroke_stream stream)
{
  if (stream->iac)
    {
      keystroke_put_iac(stream, true, NULL, 0) ;
      stream->iac = 0 ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Store <first> <len> [<bytes>]
 *
 * If len == 0, bytes may be NULL
 */
static void
keystroke_put(keystroke_stream stream, enum keystroke_type type, bool broken,
                                                        uint8_t* bytes, int len)
{
  if (len > keystroke_max_len)
    {
      len = keystroke_max_len ;
      type |= kf_truncated ;
    } ;

  vio_fifo_put_byte(stream->fifo,
                                kf_compound | (broken ? kf_broken : 0) | type) ;
  vio_fifo_put_byte(stream->fifo, len) ;

  if (len > 0)
    vio_fifo_put_bytes(stream->fifo, (void*)bytes, len) ;
} ;

/*------------------------------------------------------------------------------
 * Steal character value -- cannot be broken
 */
static void
keystroke_steal_char(keystroke steal, keystroke_stream stream, uint8_t u)
{
  steal->type   = ks_char ;
  steal->value  = u ;
  steal->flags  = 0 ;
  steal->len    = 1 ;
  steal->buf[0] = u ;
} ;

/*------------------------------------------------------------------------------
 * Steal simple escape -- cannot be broken
 */
static void
keystroke_steal_esc(keystroke steal, keystroke_stream stream, uint8_t u)
{
  steal->type   = ks_esc ;
  steal->value  = u ;
  steal->flags  = 0 ;
  steal->len    = 1 ;
  steal->buf[0] = u ;
} ;

/*------------------------------------------------------------------------------
 * Steal CSI escape -- cannot be broken or truncated.
 *
 * In the stream-in.raw buffer the last character is the escape terminator,
 * after the escape parameters.
 *
 * In keystroke buffer the escape parameters are '\0' terminated, and the
 * escape terminator is the keystroke value.
 */
static void
keystroke_steal_csi(keystroke steal, keystroke_stream stream, uint8_t u)
{
  int len ;

  len = stream->in.len ;        /* excludes the escape terminator       */
  assert((len < keystroke_max_len) && (u >= 0x40) && (u <= 0x7E)) ;

  steal->type   = ks_esc ;
  steal->value  = u ;
  steal->flags  = 0 ;
  steal->len    = len ;

  memcpy(steal->buf, stream->in.raw, len) ;
  steal->buf[len] = '\0' ;
} ;

/*==============================================================================
 */

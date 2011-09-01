/* BGP-4 bd routine
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

#include "log.h"
#include "vty.h"
#include "stream.h"
#include "sockunion.h"
#include "command.h"
#include "prefix.h"
#include "linklist.h"
#include "qpath.h"
#include "qstring.h"
#include "memory.h"
#include "qtimers.h"
#include "mqueue.h"
#include "qiovec.h"

#include "bgpd/bgp_dump.h"
#include "bgpd/bgpd.h"
#include "bgpd/bgp_table.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_engine.h"

/*==============================================================================
 * This will bd BGP state in MRT ("Multi-threaded Routing Toolkit") form.
 * See draft-ietf-grow-mrt.
 *
 * Three, independent, dumps are supported (simultaneously):
 *
 *   ALL:     dumps all BGP messages received, plus FSM state transitions.
 *
 *   UPDATES: dumps all BGP UPDATE messages received
 *
 *   TABLE:   periodically dumps entire BGP RIB
 *
 * For ALL and UPDATES, a new file is opened periodically.  For TABLE, a new
 * file is opened for each bd.
 *
 * The ALL and UPDATES dumps are done in the BGP Engine, though the periodic
 * opening of new files is done in the Routing Engine and the result passed to
 * the BGP Engine.
 */

/* MRT message types
 */
enum MRT_MT_TYPES {
   MRT_MT_NULL              =  0,       /* deprecated   */
   MRT_MT_START             =  1,       /* deprecated   */
   MRT_MT_DIE               =  2,       /* deprecated   */
   MRT_MT_I_AM_DEAD         =  3,       /* deprecated   */
   MRT_MT_PEER_DOWN         =  4,       /* deprecated   */
   MRT_MT_BGP               =  5,       /* deprecated   */
   MRT_MT_RIP               =  6,       /* deprecated   */
   MRT_MT_IDRP              =  7,       /* deprecated   */
   MRT_MT_RIPNG             =  8,       /* deprecated   */
   MRT_MT_BGP4PLUS          =  9,       /* deprecated   */
   MRT_MT_BGP4PLUS_01       = 10,       /* deprecated   */

   MRT_MT_OSPFv2            = 11,
   MRT_MT_TABLE_DUMP        = 12,       /* BGP routing table dump       */
   MRT_MT_TABLE_DUMP_V2     = 13,       /* BGP routing table dump, v2   */
   MRT_MT_BGP4MP            = 16,       /* BGP4 with MP extensions      */
   MRT_MT_BGP4MP_ET         = 17,       /* as above with Extended Times */
   MRT_MT_ISIS              = 32,
   MRT_MT_ISIS_ET           = 33,       /* as above with Extended Times */
   MRT_MT_OSPFv3            = 48,
   MRT_MT_OSPFv3_ET         = 49,       /* as above with Extended Times */

} ;

/* MRT Common Header and other sizes
 */
enum
{
  MRT_COMMON_HEADER_SIZE  = 12,         /* Timestamp(4), Type(2), Subtype(2)
                                         * Length(4)
                                         * NB: length excludes header   */
  MRT_BGP4MP_HEADER_SIZE  = 44          /* Peer AS(4), Local AS(4),
                                         * Interface Index(2),
                                         * Address Family(2),
                                         * Peer IP(16), Local IP(16)    */
} ;

/* MRT subtypes of MRT_MT_BGP4MP
 */
enum MRT_MT_BGP4MP_SUBTYPES
{
  MRT_MST_BGP4MP_STATE_CHANGE      = 0,
  MRT_MST_BGP4MP_MESSAGE           = 1,

  MRT_MST_BGP4MP_ENTRY             = 2, /* deprecated   */
  MRT_MST_BGP4MP_SNAPSHOT          = 3, /* deprecated   */

  MRT_MST_BGP4MP_MESSAGE_AS4       = 4,
  MRT_MST_BGP4MP_STATE_CHANGE_AS4  = 5,

  MRT_MST_BGP4MP_MESSAGE_LOCAL     = 6,
  MRT_MST_BGP4MP_MESSAGE_AS4_LOCAL = 7,
} ;

/* MRT subtypes of MRT_MT_TABLE_DUMP_V2
 */
enum MRT_MT_TABLE_DUMP_V2_SUBTYPES
{
  MRT_MST_TDV2_PEER_INDEX_TABLE    = 1,
  MRT_MST_TDV2_RIB_IPV4_UNICAST    = 2,
  MRT_MST_TDV2_RIB_IPV4_MULTICAST  = 3,
  MRT_MST_TDV2_RIB_IPV6_UNICAST    = 4,
  MRT_MST_TDV2_RIB_IPV6_MULTICAST  = 5,
  MRT_MST_TDV2_RIB_GENERIC         = 6,
} ;

/* Values for MRT_MST_TDV2_PEER_INDEX_TABLE message
 */
enum
{
  MRT_TDV2_PEER_INDEX_TABLE_IPV4   = 0,
  MRT_TDV2_PEER_INDEX_TABLE_IPV6   = 1,
  MRT_TDV2_PEER_INDEX_TABLE_AS2    = 0,
  MRT_TDV2_PEER_INDEX_TABLE_AS4    = 2,
} ;

/* Values for FSM states
 */
enum
{
  MRT_FSM_UNDEF        = 0,     /* Not defined in the standard  */

  MRT_FSM_Idle         = 1,
  MRT_FSM_Connect      = 2,
  MRT_FSM_Active       = 3,
  MRT_FSM_OpenSent     = 4,
  MRT_FSM_OpenConfirm  = 5,
  MRT_FSM_Established  = 6,
} ;

/* Values for AFI in BGP4MP messages
 */
enum
{
  MRT_AFI_IPv4         = 1,
  MRT_AFI_IPv6         = 2,
} ;

/*------------------------------------------------------------------------------
 * Dump control definitions, structures etc.
 */
enum
{
  /* This is the size allocated for MRT messages.
   *
   * For dumping packets in MRT_MST_BGP4MP_MESSAGE_AS4 form, the maximum
   * required is as per the CONFIRM below.
   *
   * The previous code used a buffer of that size.  UNFORTUNATELY, the TABLE
   * dumps actually require indefinite size buffering -- so we here allocate
   * a sizeable lump.
   *
   * TODO: fix TABLE dump and indefinite size MRT messages.
   */
  BGP_DUMP_BUFFER_SIZE  = 16 * 1024
} ;

CONFIRM(BGP_DUMP_BUFFER_SIZE >= (MRT_COMMON_HEADER_SIZE +
                                 MRT_BGP4MP_HEADER_SIZE +
                                 (BGP_MAX_PACKET_SIZE * 2))) ;

typedef enum bgp_dump_type
{
  BGP_DUMP_ALL,
  BGP_DUMP_UPDATES,
  BGP_DUMP_TABLE,

  BGP_DUMP_TABLE_NOW,           /* One shot                     */

  BGP_DUMP_TYPE_COUNT,          /* for arrays of dumps          */
} bgp_dump_type_t ;

struct bgp_dump
{
  const char* typename ;        /* for logging                  */

  char* filename ;      /* complete path, as opened             */
  int   fd ;            /* < 0 if not open                      */

  struct stream* obuf ; /* set up when file is opened           */

  uint  seq ;           /* for TABLE (and TABLE_NOW) dumps      */

  char* buf ;           /* for TABLE (and TABLE_NOW) dumps...   */
  char* p ;             /* ...put pointer                       */
  uint  s ;             /* ...buffer size                       */
  uint  h ;             /* ...amount of space have left         */
} ;

typedef struct bgp_dump  bgp_dump_t ;
typedef struct bgp_dump* bgp_dump ;

struct bgp_dump_control
{
  bgp_dump_type_t type ;

  uint      interval;

  char*     template ;
  char*     interval_str;

  qtimer    qtr ;
  bgp_dump  bd ;
} ;

typedef struct bgp_dump_control  bgp_dump_control_t ;
typedef struct bgp_dump_control* bgp_dump_control ;

/* The control structures for the various bd types -- these belong to the
 * Routing Engine.
 *
 * Start with none at all.
 */
static bgp_dump_control bgp_dumps[BGP_DUMP_TYPE_COUNT] = { NULL } ;

/* These belong to the BGP Engine
 */
static bgp_dump bd_all     = NULL ;
static bgp_dump bd_updates = NULL ;

bool bgp_dump_state_flag   = false ;
bool bgp_dump_packet_flag  = false ;

/* Types of dump -- in CLI form for ALL, UPDATES and TABLE
 */
static const char* bgp_dump_name[] =
{
    [BGP_DUMP_ALL]        = "all",
    [BGP_DUMP_UPDATES]    = "updates",
    [BGP_DUMP_TABLE]      = "routes-mrt",

    [BGP_DUMP_TABLE_NOW]  = "one-shot routes-mrt",
} ;

/*==============================================================================
 * Dump control
 */
static void bgp_dump_engine_set(bgp_dump bd, bgp_dump_type_t type) ;
static void bgp_dump_timer_expired(qtimer qtr, void* timer_info,
                                                            qtime_mono_t when) ;
static void bgp_dump_table(bgp_dump_control bdc) ;
static void bgp_dump_control_free(bgp_dump_control bdc) ;
static bgp_dump bgp_dump_free(bgp_dump bd) ;

static bgp_dump bgp_dump_bytes(bgp_dump bd, void* bytes, uint n) ;
static bgp_dump bgp_dump_flush(bgp_dump bd) ;
static bgp_dump bgp_dump_put(bgp_dump bd, void* p0, uint n0, void* p1, uint n1);
static bgp_dump bgp_dump_truncate(bgp_dump bd) ;

/*------------------------------------------------------------------------------
 * Open new file for the given dump
 *
 * This is called when an interval timer goes off, or a new ALL or UPDATES
 * dump is set, or a TABLES_NOW dump is set.
 *
 * If this is ALL or UPDATES, then after successful file open, pass the bd
 * to the BGP Engine !  Note that does nothing if file open fails, so any
 * existing dump will continue.
 *
 * NB: no file can currently be open on the Routing Engine side.
 *
 * NB: creates file if required.  Does not, however, truncate -- that is done
 *     just before the file is to be used.  (This means that if the same file
 *     is opened for ALL or UPDATES, then the file is truncated on the
 *     BGP Engine side, so between writes to the file !)
 */
static bool
bgp_dump_open_file (bgp_dump_control bdc, struct vty* vty)
{
  int ret;
  time_t clock;
  struct tm tm;
  qpath   path ;
  qstring name ;
  const char* typename ;
  int fd ;
  bgp_dump bd ;

  assert(bdc->bd == NULL) ;             /* Nothing should be open       */

  typename = bgp_dump_name[bdc->type] ;

  /* Construct filename from template and attempt to open the file.
   */
  time (&clock);
  localtime_r(&clock, &tm);

  name = qs_new_size(NULL, PATH_MAX) ;

  ret = strftime (qs_char_nn(name), qs_size_nn(name), bdc->template, &tm);

  if (ret != 0)
    {
      mode_t oldumask ;

      qs_set_len_nn(name, ret) ;
      path = qpath_complete(qpath_set_qs(NULL, name), vty_getcwd(NULL)) ;

      oldumask = umask(0777 & ~LOGFILE_MASK);
      fd = open(qpath_string(path), O_WRONLY | O_CREAT, LOGFILE_MASK) ;

      if (fd < 0)
        {
          int err ;

          err = errno ;

          zlog_warn("Failed to open %s dump file %s: %s", typename,
                                       qpath_string(path), errtoa(err, 0).str) ;
          if (vty != NULL)
            vty_out(vty, "Failed to open file %s: %s\n",
                                       qpath_string(path), errtoa(err, 0).str) ;
        } ;

      umask(oldumask) ;
    }
  else
    {
      fd = -1 ;
      path = NULL ;

      zlog_warn ("Failed to open %s dump file: strftime failed on '%s'",
                                                      typename, bdc->template) ;
      if (vty != NULL)
        vty_out(vty, "Failed in strftime on '%s'\n", bdc->template) ;
    } ;

  /* If opened OK, construct the bgp_dump object, log success and then either
   * set the bdc->bd or pass the bd to the BGP Engine.
   */
  if (fd >= 0)
    {
      bd = XCALLOC(MTYPE_BGP_DUMP, sizeof(bgp_dump_t)) ;

      /* Zeroising sets bd->buf etc NULL
       */
      bd->typename = typename ;
      bd->filename = XSTRDUP(MTYPE_BGP_DUMP, qpath_string(path)) ;
      bd->fd       = fd ;
      bd->obuf     = stream_new(BGP_DUMP_BUFFER_SIZE) ;

      zlog_info("Opened %s dump file: %s", bd->typename, bd->filename) ;

      /* If this is ALL or UPDATE, then we now pass the open file to the
       * BGP Engine, and forget all about it.
       *
       * Otherwise, set the bgp_dump_control's bgp_dump
       */
      if ( (bdc->type == BGP_DUMP_ALL) || (bdc->type == BGP_DUMP_UPDATES) )
        bgp_dump_engine_set(bd, bdc->type) ;
      else
        bdc->bd = bd ;
    } ;

  /* Release buffer and return success/failure.
   */
  qs_free(name) ;
  qpath_free(path) ;

  return (fd >= 0) ;
}

/*------------------------------------------------------------------------------
 * Set a new interval -- if zero, turn off timer.
 */
static void
bgp_dump_set_timer(bgp_dump_control bdc)
{
  uint interval = bdc->interval ;

  if (interval > 0)
    {
      /* Periodic dump every interval seconds
       */
      if ((interval < (24 * 60 * 60)) && (((24 * 60 * 60) % interval) == 0))
	{
          /* Dump at predictable times: if a day has a whole number of
           * intervals, dump every interval seconds starting from midnight
           */
          int secs_into_day;
          time_t t;
          struct tm tm;

	  (void) time(&t);
	  localtime_r(&t, &tm);
	  secs_into_day = tm.tm_sec + 60 * (tm.tm_min + 60 * tm.tm_hour) ;
	  interval = interval - secs_into_day % interval; /* always > 0 */
	}

      if (bdc->qtr == NULL)
        bdc->qtr = qtimer_init_new(NULL, routing_nexus->pile,
                                                 bgp_dump_timer_expired, bdc) ;
      qtimer_set(bdc->qtr, qt_add_monotonic(QTIME(interval)), NULL) ;
    }
  else
    {
      if (bdc->qtr != NULL)
        qtimer_unset(bdc->qtr) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Timer expired function -- called by qtimer.
 *
 * For all except BGP_DUMP_TABLE_NOW, open a new dump file.
 *
 * For BGP_DUMP_TABLE and BGP_DUMP_TABLE_NOW dump the table and close the
 * dump file.
 *
 * For BGP_DUMP_ALL and BGP_DUMP_UPDATES, pass the new file to the BGP Engine,
 * which will close the current and adopt the new.
 *
 * For all but BGP_DUMP_TABLE_NOW, set a new interval time.
 */
static void
bgp_dump_timer_expired(qtimer qtr, void* timer_info, qtime_mono_t when)
{
  bgp_dump_control bdc = timer_info ;

  assert(bdc->qtr == qtr) ;

  switch(bdc->type)
    {
      case BGP_DUMP_ALL:
      case BGP_DUMP_UPDATES:
        bgp_dump_open_file(bdc, NULL) ; /* rotate -- posts new file to
                                         * the BGP Engine               */
        break ;

      case BGP_DUMP_TABLE:
        if (bgp_dump_open_file(bdc, NULL))
          bgp_dump_table(bdc) ;         /* if open succeeds             */
        break ;

      case BGP_DUMP_TABLE_NOW:
        bgp_dump_table(bdc) ;           /* file already open            */
        bdc->interval = 0 ;             /* do not repeat                */
        break ;

      default:                          /* should not happen !          */
        bdc->interval = 0 ;
        break ;
    } ;

  /* Unless is BGP_DUMP_TABLE_NOW, set a new interval time.  Note that for a
   * TABLE dump, if the current dump took longer than the interval, then will
   * skip one or more dumps.
   *
   * Otherwise, this is a BGP_DUMP_TABLE_NOW, which has completed, and can
   * now be forgotten.
   */
  if (bdc->type != BGP_DUMP_TABLE_NOW)
    bgp_dump_set_timer(bdc) ;
  else
    bgp_dump_control_free(bdc) ;
} ;

/*------------------------------------------------------------------------------
 * Free the given bgp_dump control, completely.
 *
 * Closes and frees any bgp_dump.
 */
static void
bgp_dump_control_free(bgp_dump_control bdc)
{
  if (bdc != NULL)
    {
      bgp_dump_free(bdc->bd) ;

      XFREE(MTYPE_BGP_DUMP, bdc->template) ;
      XFREE(MTYPE_BGP_DUMP, bdc->interval_str) ;

      qtimer_free(bdc->qtr) ;   /* unsets any running timer     */

      assert(bgp_dumps[bdc->type] == bdc) ;
      bgp_dumps[bdc->type] = NULL ;

      XFREE(MTYPE_BGP_DUMP, bdc) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Free the given bgp_dump, closing any file.
 *
 * Touches nothing beyond the bgp_dump object, so can be used in both the
 * Routing Engine and the BGP Engine.
 */
static bgp_dump
bgp_dump_free(bgp_dump bd)
{
  if (bd != NULL)
    {
      if (bd->fd >= 0)
        {
          close(bd->fd) ;
          zlog_info("Closed %s dump file: %s", bd->typename, bd->filename) ;
        } ;

      XFREE(MTYPE_BGP_DUMP, bd->filename) ;
      stream_free(bd->obuf) ;
      XFREE(MTYPE_TMP, bd->buf) ;

      XFREE(MTYPE_BGP_DUMP, bd) ;
    } ;

  return NULL ;
} ;

/*==============================================================================
 * TABLE (and TABLE_NOW) dumps
 */
static bgp_dump bgp_dump_routes_index_table(bgp_dump bd, struct bgp *bgp) ;
static bgp_dump bgp_dump_routes_family(bgp_dump bd, struct bgp *bgp, afi_t afi);

static struct stream* bgp_dump_header (bgp_dump bd, int type, int subtype) ;
static void bgp_dump_set_size (struct stream *s, int plus) ;

/*------------------------------------------------------------------------------
 * Perform a TABLE or TABLE_NOW dump, and close the dump file when finished.
 */
static void
bgp_dump_table(bgp_dump_control bdc)
{
  struct bgp *bgp ;
  bgp_dump bd ;

  if (qdebug)
    assert((bdc->bd != NULL) && (bdc->bd->fd >= 0)) ;   /* will be open */

  bd = bgp_dump_truncate(bdc->bd) ;     /* could (conceivably) fail     */

  bgp = bgp_get_default();

  if ((bgp != NULL) && (bd != NULL))
    {
      /* Set up buffer for dumping
       */
      bd->s   = 128 * 1024 ;    /* a chunky buffer      */

      bd->buf = XMALLOC(MTYPE_TMP, bd->s) ;
      bd->p   = bd->buf ;
      bd->h   = bd->s ;

      bd->seq = 0 ;

      /* Construct the index table for all peers and output it.
       */
      bd = bgp_dump_routes_index_table(bd, bgp) ;

      /* Now dump all the routes
       */
      bd = bgp_dump_routes_family(bd, bgp, AFI_IP) ;
#ifdef HAVE_IPV6
      bd = bgp_dump_routes_family(bd, bgp, AFI_IP6) ;
#endif /* HAVE_IPV6 */

      /* Flush anything left to go
       */
      bgp_dump_flush(bd) ;
    } ;

  bdc->bd = bgp_dump_free(bd) ;         /* Frees the buf, if any        */
} ;

/*------------------------------------------------------------------------------
 * Put the Peer Index Table for the coming route dump
 *
 * Returns:  bd if OK, or NULL if failed (or was NULL already).
 */
static bgp_dump
bgp_dump_routes_index_table(bgp_dump bd, struct bgp *bgp)
{
  struct stream* s ;
  struct peer *peer ;
  struct listnode *node ;
  uint16_t peerno, len ;

  if (bd == NULL)
    return NULL ;               /* get out, quick, if no file   */

  /* MRT header
   */
  s = bgp_dump_header(bd, MRT_MT_TABLE_DUMP_V2, MRT_MST_TDV2_PEER_INDEX_TABLE) ;

  stream_put_in_addr(s, &bgp->router_id) ;      /* Collector BGP ID     */

  len = (bgp->name != NULL) ? strlen(bgp->name) : 0 ;
  stream_putw(s, len) ;                         /* View name            */
  if (len != 0)
    stream_put(s, bgp->name, len) ;

  stream_putw (s, listcount(bgp->peer));        /* Peer count           */

  /* Walk down all peers and construct peer index entries for the known
   * address families.
   *
   * Note that the type field gives only the type of the address of the peer
   * and the form of the AS number -- it has nothing to do with the
   * capabilities of the peer; in particular nothing to do with whether the
   * peer is an AS4 speaker.
   */
  peerno = 0 ;                  /* index for first entry        */
  for(ALL_LIST_ELEMENTS_RO (bgp->peer, node, peer))
    {
      sockunion su ;
      uint  type ;

      su = &peer->su ;

      type = MRT_TDV2_PEER_INDEX_TABLE_AS4 ;    /* always, for simplicity */

      switch (sockunion_family(su))
        {
          case AF_INET:
            type |= MRT_TDV2_PEER_INDEX_TABLE_IPV4 ;
            break ;

#ifdef HAVE_IPV6
          case AF_INET6:
            type |= MRT_TDV2_PEER_INDEX_TABLE_IPV6 ;
            break ;
#endif /* HAVE_IPV6 */

          default:
            continue ;          /* Ignore if not known family ! */
        } ;

      stream_putc (s, type) ;                   /* Peer's type          */
      stream_put_in_addr (s, &peer->remote_id); /* Peer's BGP ID        */
      stream_write (s, sockunion_get_addr(su),  /* Peer's IP address    */
                       sockunion_get_addr_len(su)) ;
      stream_putl (s, peer->as);                /* Peer's AS (AS4-wise) */

      peer->table_dump_index = peerno ;         /* set peer number      */

      ++peerno ;
    } ;

  bgp_dump_set_size(s, 0);

  return bgp_dump_bytes(bd, STREAM_DATA(s), stream_get_endp (s)) ;
} ;

/*------------------------------------------------------------------------------
 * Dump all routes for the given address family.
 *
 * Note: previous comments suggest that this might run under a child
 *       process -- which would be a good idea -- but does not appear to
 *       ever have been the case ?
 *
 * Returns:  bd if OK, or NULL if failed (or was NULL already).
 *
 * Updates the sequence number.
 *
 * NB: assumes that th afi is known !
 */
static bgp_dump
bgp_dump_routes_family(bgp_dump bd, struct bgp *bgp, afi_t afi)
{
  struct bgp_node *rn;
  struct bgp_table *table;
  int subtype ;

  if (bd == NULL)
    return NULL ;               /* get out, quick, if no file   */

  /* Establish subtype of dump entries.
   */
  switch (afi)
    {
      case AFI_IP:
        subtype = MRT_MST_TDV2_RIB_IPV4_UNICAST ;
        break ;

#ifdef HAVE_IPV6
      case AFI_IP6:
        subtype = MRT_MST_TDV2_RIB_IPV6_UNICAST ;
        break ;
#endif /* HAVE_IPV6 */

      default:
        return bd ;             /* do nothing if family unknown */
    } ;

  /* Get the required table -- exit, quick, if none
   */
  table = bgp->rib[afi][SAFI_UNICAST] ;

  if (table == NULL)
    return bd ;

  /* Walk down each BGP route
   */
  for (rn = bgp_table_top (table) ; rn != NULL ; rn = bgp_route_next (rn))
    {
      struct stream* s;
      struct bgp_info *info;
      int sizep ;
      uint16_t entry_count ;

      if (rn->info == NULL)
        continue;

      /* MRT header for MRT_MT_TABLE_DUMP_V2 type message
       */
      s = bgp_dump_header (bd, MRT_MT_TABLE_DUMP_V2, subtype) ;

      stream_putl(s, bd->seq) ;                 /* Sequence number      */
      stream_putc(s, rn->p.prefixlen) ;         /* Prefix length        */
      stream_write(s, &rn->p.u.prefix,          /* Prefix               */
                      (rn->p.prefixlen+7)/8) ;  /* (zero is OK)         */

      sizep = stream_get_endp(s);               /* will set count later */
      entry_count = 0;
      stream_putw(s, entry_count);              /* entry count, so far  */

      /* Cycle through the known attributes for this prefix             */
      for (info = rn->info ; info != NULL ; info = info->info_next)
        {
          entry_count++;

          /* Peer index */
          stream_putw(s, info->peer->table_dump_index);

          /* Originated */
          stream_putl (s, bgp_wall_clock(info->uptime));

          /* Dump attribute.                                            */
          /* Skip prefix & AFI/SAFI for MP_NLRI                         */
          bgp_dump_routes_attr (s, info->attr, &rn->p);
        }

      /* Overwrite the entry count, now that we know the right number */
      stream_putw_at (s, sizep, entry_count);

      bgp_dump_set_size(s, 0) ;

      bd = bgp_dump_bytes(bd, STREAM_DATA(s), stream_get_endp (s)) ;
      if (bd == NULL)
        break ;

      ++bd->seq ;
    }

  return bd ;
} ;

/*==============================================================================
 * TABLE (and TABLE_NOW) dumps
 */

/*------------------------------------------------------------------------------
 * Reset given stream and construct common header for all MRT packets
 */
static struct stream*
bgp_dump_header (bgp_dump bd, int type, int subtype)
{
  static struct stream* s ;

  s = bd->obuf ;
  stream_reset (s);

  stream_putl (s, time(NULL));
  stream_putw (s, type);
  stream_putw (s, subtype);
  stream_putl (s, 0);            /* len */

  return s ;
}

/*------------------------------------------------------------------------------
 * Set size of MRT packet to be size of stuff in the obuf, less the header,
 * plus the given size.
 *
 * NB: depends on the header being at the start of the given stream.
 */
static void
bgp_dump_set_size (struct stream *s, int plus)
{
  stream_putl_at (s, 8, stream_get_endp (s) - MRT_COMMON_HEADER_SIZE + plus);
}

/*------------------------------------------------------------------------------
 * Put bytes to dump buffer
 *
 * Returns:  bd if OK, or NULL if failed (or was NULL already).
 *
 * If fails, closes the file and frees the bd.
 */
static bgp_dump
bgp_dump_bytes(bgp_dump bd, void* bytes, uint n)
{
  if (bd == NULL)
    return NULL ;               /* get out, quick, if no file   */

  while (n > 0)
    {
      uint t ;

      t = bd->h ;

      if (t >= n)
        t = n ;
      else
        {
          if (t == 0)
            {
              if ((bd = bgp_dump_flush(bd)) == NULL)
                break ;

              t = bd->h ;
              if (t >= n)
                t = n ;
            } ;
        } ;

      memcpy(bd->p, bytes, t) ;

      bd->p += t ;
      bd->h -= t ;

      bytes = (char*)bytes + t ;
      n -= t ;
    } ;

  return bd ;
} ;

/*------------------------------------------------------------------------------
 * Flush dump buffer to dump
 *
 * Returns:  bd if OK, or NULL if failed (or was NULL already).
 *
 * If fails, closes the file and frees the bd.
 */
static bgp_dump
bgp_dump_flush(bgp_dump bd)
{
  uint n ;

  if (bd != NULL)
    {
      n = bd->p - bd->buf ;
      if (n > 0)
        bd = bgp_dump_put(bd, bd->buf, n, NULL, 0) ;

      if (bd != NULL)
        {
          bd->p = bd->buf ;
          bd->h = bd->s ;
        } ;
    } ;

  return bd ;
} ;

/*------------------------------------------------------------------------------
 * Put one or two lumps of data to dump -- first lump may NOT be empty.
 *
 * Returns:  bd if OK, or NULL if failed (or was NULL already).
 *
 * If fails, closes the file and frees the bd.
 */
static bgp_dump
bgp_dump_put(bgp_dump bd, void* p0, uint n0, void* p1, uint n1)
{
  struct iovec iov[2] ;
  int n ;
  int w ;

  if (bd == NULL)
    return NULL ;               /* get out, quick, if no file   */

  iov[0].iov_base = p0 ;
  iov[0].iov_len  = n0 ;

  if (n1 > 0)
    {
      iov[1].iov_base = p1 ;
      iov[1].iov_len  = n1 ;

      n = 2 ;
    }
  else
    n = 1 ;

  do
    w = iovec_write_nb(bd->fd, iov, n) ;
  while (w > 0) ;

  if (w == 0)
    return bd ;

  /* Error writing to the dump file.
   */
  zlog_warn("failed writing %s dump file %s: %s", bd->typename, bd->filename,
                                                        errtoa(errno, 0).str) ;
  return bgp_dump_free(bd) ;
} ;

/*------------------------------------------------------------------------------
 * Truncate the dump file (if any) and seek to start of (now) empty file
 *
 * Returns:  bd if OK, or NULL if failed (or was already NULL).
 *
 * If fails, closes the file and frees the bd.
 */
static bgp_dump
bgp_dump_truncate(bgp_dump bd)
{
  if (bd != NULL)
    {
      /* Truncate: extremely unlikely to fail, but let's be careful out there.
       *
       * Something very odd is going on if does fail, so log as "error".
       */
      lseek(bd->fd, 0, SEEK_SET) ;      /* start from the beginning     */

      while (1)
        {
          int r ;

          r = ftruncate(bd->fd, 0) ;    /* redundant in most cases      */

          if (r >= 0)
            break ;                     /* OK                   */

          if (errno == EINTR)
            continue ;                  /* Try again            */

          zlog_err("failed truncating %s dump file %s: %s", bd->typename,
                                           bd->filename, errtoa(errno, 0).str) ;
          bd = bgp_dump_free(bd) ;

          break ;                       /* Failed immediately ! */
        } ;
    } ;

  return bd ;
} ;

/*------------------------------------------------------------------------------
 * Parse interval string:
 *
 *   (\d+[hH])?(\d+[mM])\d+[sS]?  returns -1 if invalid
 */
static int
bgp_dump_parse_time (const char *str)
{
  int i;
  int len;
  bool seen_h, seen_m, seen_d ;
  int time;
  int total;

  time = 0;
  total = 0;
  seen_h = false ;
  seen_m = false ;
  seen_d = false ;
  len = strlen (str);

  for (i = 0; i < len; i++)
    {
      if (isdigit ((int) str[i]))
	{
          seen_d = true ;
	  time *= 10;
	  time += str[i] - '0';
	}
      else
	{
          if (!seen_d)
            return -1 ;         /* must have digit before non-digit     */

          switch (str[i])
            {
              case 'H':
              case 'h':
                if (seen_h || seen_m)
                  return -1 ;   /* must not have seen 'h' or 'm'        */

                seen_h = true ;
                total += time * 60 * 60;
                break ;

              case 'M':
              case 'm':
                if (seen_m)
                  return -1 ;   /* must not have seen 'm'               */

                seen_m = true ;
                total += time * 60;
                break ;

              case 'S':
              case 's':
                if (i != (len - 1))
                  return -1 ;   /* must be at end                       */

              default:
                return -1 ;     /* unknown character                    */
            } ;

          seen_d = false ;      /* accept only digit or end             */
          time = 0 ;            /* no further value, yet                */
	} ;
    } ;

  return total + time ;
} ;

/*------------------------------------------------------------------------------
 * Command function to set (or change) state of a dump.
 *
 * If an interval is given, then:
 *
 *   If a dump is set, and both the interval and the path are unchanged, then
 *   do nothing -- debounce.
 *
 *   Otherwise, close any existing dump, and set the given one running.
 *
 * If no interval is given, then:
 *
 *   TABLE:    schedule an immediate, one off TABLE_NOW dump to the given file.
 *             Leaves any existing periodic dump alone.
 *
 *   ALL:
 *   UPDATES:  set a new dump going.
 *
 *             If a dump is already exists with the same file name, then the
 *             existing file will be truncated.
 *
 *             In any case, leave with no rotation interval set.
 */
static int
bgp_dump_set (struct vty *vty, bgp_dump_type_t type, const char *template,
                                                     const char *interval_str)
{
  enum cmd_return_code ret ;
  uint interval;
  bgp_dump_control bdc ;
  bool unchanged ;

  assert((type >= 0) && (type < BGP_DUMP_TYPE_COUNT)) ;

  bdc = bgp_dumps[type] ;
  unchanged = false ;

  /* If we have an interval string then extract interval, and if no actual
   * change in the bd specification, do nothing.
   *
   * If no interval string, then look out for special TABLE_NOW
   */
  if (interval_str != NULL)
    {
      /* Check interval string. */
      int get = bgp_dump_parse_time (interval_str);
      if (get < 60)
	{
          if (get < 0)
            vty_out (vty, "Malformed interval string%s", VTY_NEWLINE) ;
          else
            vty_out (vty, "Interval < 60 seconds%s", VTY_NEWLINE) ;

          return CMD_WARNING;
	} ;

      interval = get ;

      /* Don't schedule duplicate dumps if the dump command is given twice  */
      if (bdc != NULL)
        unchanged = (interval == bdc->interval) &&
                                        (strcmp(template, bdc->template) == 0) ;
    }
  else
    {
      interval = 0 ;

      if (type == BGP_DUMP_TABLE)
        {
          /* A "routes-mrt" type dump with no interval is a one-shot dump, so
           * set actual type.
           *
           * There can be at most one one-shot dump active at any time !
           *
           * NB: it's a bit klunky, but we arrange for BGP_DUMP_TABLE_NOW to
           *     run in 1 second's time -- so that command completes before the
           *     big work starts.
           */
          type = BGP_DUMP_TABLE_NOW ;
          interval = 1 ;
          bdc  = NULL ;

          if (bgp_dumps[BGP_DUMP_TABLE_NOW] != NULL)
            {
              vty_out (vty, "one-shot routes-mrt dump already pending%s",
                                                                  VTY_NEWLINE) ;
              return CMD_WARNING;
            } ;
        } ;
    } ;

  /* If required, create the bgp bd control.
   *
   * Zeroising sets:
   *
   *    type          -- 0     -- set below
   *
   *    interval      -- 0,    none, yet
   *    template      -- NULL, none, yet
   *    interval_str  -- NULL  none, yet
   *    qtr           -- NULL  none, yet
   *
   *    bd            -- NULL, no file, yet
   */
  if (bdc == NULL)
    {
      bdc = (bgp_dump_control)XCALLOC(MTYPE_BGP_DUMP,
                                                  sizeof(bgp_dump_control_t)) ;
      bdc->type = type ;

      bgp_dumps[type] = bdc ;
    } ;

  /* Set interval, interval string and template.
   *
   * Even if the interval has not changed, we keep the latest interval string,
   * just in case that has changed.
   */
  bdc->interval = interval ;

  XFREE(MTYPE_BGP_DUMP, bdc->interval_str) ;    /* sets NULL    */
  if (interval_str != NULL)
    bdc->interval_str = XSTRDUP(MTYPE_BGP_DUMP, interval_str) ;

  XFREE(MTYPE_BGP_DUMP, bdc->template) ;
  bdc->template = XSTRDUP(MTYPE_BGP_DUMP, template) ;

  /* If is, in fact, unchanged, get out now.
   */
  if (unchanged)
    return CMD_SUCCESS ;

  /* Unless this is TABLE dump, open a file now if either the template or
   * the interval have changed.
   *
   * Note that for BGP_DUMP_TABLE_NOW changed is always true.
   *
   * For ALL and UPDATES dumps, the new file is passed to the BGP Engine.  If
   * the file open fails, any existing file will continue.
   *
   * If fails to open, and no interval was set (so won't be able to try again),
   * then will return CMD_WARNING.  For TABLE_NOW, will discard the abortive
   * bdc -- inter alia, it will not then appear in the configuration output.
   */
  ret = CMD_SUCCESS ;

  if (type != BGP_DUMP_TABLE)
    {
      if (!bgp_dump_open_file(bdc, vty))
        {
          if (interval_str == NULL)
            {
              ret = CMD_WARNING ;

              if (type == BGP_DUMP_TABLE_NOW)
                {
                  bgp_dump_control_free(bdc) ;  /* Discard abortive bdc */
                  return ret ;
                } ;
            } ;
        } ;
    } ;

  /* Set interval timer if required, or stop the current timer
   */
  bgp_dump_set_timer(bdc) ;

  return ret ;
}

/*------------------------------------------------------------------------------
 * Command function to unset a dump.
 */
static int
bgp_dump_unset (struct vty *vty, bgp_dump_type_t type)
{
  bgp_dump_control bdc ;

  assert((type >= 0) && (type < BGP_DUMP_TYPE_COUNT)) ;

  bdc = bgp_dumps[type] ;
  if (bdc != NULL)
    {
      bgp_dump_control_free(bdc) ;

      if ( (type == BGP_DUMP_ALL) || (type == BGP_DUMP_UPDATES) )
        bgp_dump_engine_set(NULL, type) ;
    } ;

  return CMD_SUCCESS;
}

/*==============================================================================
 * The CLI Commands
 */

DEFUN (dump_bgp_all,
       dump_bgp_all_cmd,
       "dump bgp all PATH",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump all BGP packets\n"
       "Output filename\n")
{
  return bgp_dump_set (vty, BGP_DUMP_ALL, argv[0], NULL);
}

DEFUN (dump_bgp_all_interval,
       dump_bgp_all_interval_cmd,
       "dump bgp all PATH INTERVAL",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump all BGP packets\n"
       "Output filename\n"
       "Interval of output\n")
{
  return bgp_dump_set (vty, BGP_DUMP_ALL, argv[0], argv[1]);
}

DEFUN (no_dump_bgp_all,
       no_dump_bgp_all_cmd,
       "no dump bgp all [PATH] [INTERVAL]",
       NO_STR
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump all BGP packets\n")
{
  return bgp_dump_unset (vty, BGP_DUMP_ALL);
}

DEFUN (dump_bgp_updates,
       dump_bgp_updates_cmd,
       "dump bgp updates PATH",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump BGP updates only\n"
       "Output filename\n")
{
  return bgp_dump_set (vty, BGP_DUMP_UPDATES, argv[0], NULL);
}

DEFUN (dump_bgp_updates_interval,
       dump_bgp_updates_interval_cmd,
       "dump bgp updates PATH INTERVAL",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump BGP updates only\n"
       "Output filename\n"
       "Interval of output\n")
{
  return bgp_dump_set (vty, BGP_DUMP_UPDATES, argv[0], argv[1]);
}

DEFUN (no_dump_bgp_updates,
       no_dump_bgp_updates_cmd,
       "no dump bgp updates [PATH] [INTERVAL]",
       NO_STR
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump BGP updates only\n")
{
  return bgp_dump_unset (vty, BGP_DUMP_UPDATES);
}

DEFUN (dump_bgp_routes,
       dump_bgp_routes_cmd,
       "dump bgp routes-mrt PATH",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump whole BGP routing table\n"
       "Output filename\n")
{
  return bgp_dump_set (vty, BGP_DUMP_TABLE, argv[0], NULL);
}

DEFUN (dump_bgp_routes_interval,
       dump_bgp_routes_interval_cmd,
       "dump bgp routes-mrt PATH INTERVAL",
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump whole BGP routing table\n"
       "Output filename\n"
       "Interval of output\n")
{
  return bgp_dump_set (vty, BGP_DUMP_TABLE, argv[0], argv[1]);
}

DEFUN (no_dump_bgp_routes,
       no_dump_bgp_routes_cmd,
       "no dump bgp routes-mrt [PATH] [INTERVAL]",
       NO_STR
       "Dump packet\n"
       "BGP packet dump\n"
       "Dump whole BGP routing table\n")
{
  return bgp_dump_unset (vty, BGP_DUMP_TABLE);
}

/* BGP node structure. */
static struct cmd_node bgp_dump_node =
{
  DUMP_NODE,
  "",
  1
};

#if 0
char *
config_time2str (unsigned int interval)
{
  static char buf[BUFSIZ];

  buf[0] = '\0';

  if (interval / 3600)
    {
      sprintf (buf, "%dh", interval / 3600);
      interval %= 3600;
    }
  if (interval / 60)
    {
      sprintf (buf + strlen (buf), "%dm", interval /60);
      interval %= 60;
    }
  if (interval)
    {
      sprintf (buf + strlen (buf), "%d", interval);
    }
  return buf;
}
#endif

/*------------------------------------------------------------------------------
 * Output the configuration for bgp dumping
 */
static int
config_write_bgp_dump (struct vty *vty)
{
  bgp_dump_type_t   type ;

  for (type = 0 ; type < BGP_DUMP_TYPE_COUNT ; ++type)
    {
      bgp_dump_control  bdc ;

      bdc = bgp_dumps[type] ;

      if ((bdc == NULL) || (type == BGP_DUMP_TABLE_NOW))
        continue ;

      vty_out (vty, "dump bgp %s %s", bgp_dump_name[type], bdc->template) ;

      if (bdc->interval_str != NULL)
        vty_out (vty, " %s", bdc->interval_str) ;

      vty_out (vty, VTY_NEWLINE) ;
    } ;
  return 0;
}

/*------------------------------------------------------------------------------
 * Initialize BGP MRT dumping.
 *
 * NB: second stage initialisation -- after pthreads start.
 */
extern void
bgp_dump_init (void)
{
  install_node (&bgp_dump_node, config_write_bgp_dump);

  install_element (CONFIG_NODE, &dump_bgp_all_cmd);
  install_element (CONFIG_NODE, &dump_bgp_all_interval_cmd);
  install_element (CONFIG_NODE, &no_dump_bgp_all_cmd);
  install_element (CONFIG_NODE, &dump_bgp_updates_cmd);
  install_element (CONFIG_NODE, &dump_bgp_updates_interval_cmd);
  install_element (CONFIG_NODE, &no_dump_bgp_updates_cmd);
  install_element (CONFIG_NODE, &dump_bgp_routes_cmd);
  install_element (CONFIG_NODE, &dump_bgp_routes_interval_cmd);
  install_element (CONFIG_NODE, &no_dump_bgp_routes_cmd);
} ;

/*------------------------------------------------------------------------------
 * Close down all dumping.
 *
 * This is called after the BGP Engine has stopped -- so can here free the
 * BGP Engine stuff, too.
 */
extern void
bgp_dump_finish (void)
{
  uint d ;

  for (d = 0 ; d < BGP_DUMP_TYPE_COUNT ; ++d)
    bgp_dump_control_free(bgp_dumps[d]) ;

  bd_all     = bgp_dump_free(bd_all) ;
  bd_updates = bgp_dump_free(bd_updates) ;
} ;

/*==============================================================================
 * The BGP_Engine side of bgp_dump.
 *
 * For ALL and UPDATES dumps the BGP_Engine is responsible for all actual
 * I/O.  The Routing Engine will open/rotate files, but pass those to the
 * BGP_Engine for action.  The BGP_Engine looks after the bgp_dump structures,
 * and is responsible for closing files.
 */
struct bgp_dump_engine_set_args         /* to BGP Engine                */
{
  bgp_dump_type_t type ;
  bgp_dump        bd ;
} ;
MQB_ARGS_SIZE_OK(bgp_dump_engine_set_args) ;

static void bgp_dump_engine_do_set(mqueue_block mqb, mqb_flag_t flag) ;
static struct stream* bgp_dump_common (bgp_dump bd, bgp_connection connection,
                                                        int subtype, bool as4) ;
static void bgp_dump_set_flags(void) ;
static int bgp_dump_fsm_state(bgp_fsm_state_t state) ;

/*------------------------------------------------------------------------------
 * Set given dump in BGP Engine.
 *
 * Passes the given bgp_dump to the BGP Engine.  Responsibility for the fd,
 * the filename and the stream buffer pass to the BGP Engine.
 *
 * Pass NULL to stop the relevant dump.
 */
static void
bgp_dump_engine_set(bgp_dump bd, bgp_dump_type_t type)
{
  struct bgp_dump_engine_set_args* args ;
  mqueue_block mqb ;

  mqb = mqb_init_new(NULL, bgp_dump_engine_do_set, bgp_dumps) ;

  args = mqb_get_args(mqb) ;

  args->type = type ;
  args->bd   = bd ;

  bgp_to_bgp_engine(mqb, mqb_priority) ;        /* change file ASAP     */
} ;

/*------------------------------------------------------------------------------
* BGP Engine: set the given dump.
*
* Note that this explicitly truncates the dump file.  This deals with the
* fringe case of a dump being started using the same name as an existing,
* active dump !!  (If the old dump wrote something to the file after the
* new dump had opened the file, then we want to discard the old stuff, now.)
*/
static void
bgp_dump_engine_do_set(mqueue_block mqb, mqb_flag_t flag)
{
  bgp_dump* pbd ;
  bgp_dump  bd ;
  struct bgp_dump_engine_set_args* args = mqb_get_args(mqb) ;

  pbd = (args->type == BGP_DUMP_UPDATES) ? &bd_updates : &bd_all ;

  *pbd = bgp_dump_free(*pbd) ;          /* close down any existing dump */

  bd = bgp_dump_truncate(args->bd) ;    /* truncate before use          */

  if (flag == mqb_action)
    *pbd = bd ;                         /* set new dump                 */
  else
    *pbd = bgp_dump_free(bd) ;          /* close down new dump          */

  bgp_dump_set_flags() ;                /* reflect current state        */

  mqb_free(mqb) ;
} ;

/*------------------------------------------------------------------------------
 * Dump BGP status change, if required -- BGP Engine
 *
 * Does nothing if no bd_all.
 *
 * Frees the dump in the event of any I/O error.
 */
extern void
bgp_dump_state (bgp_connection connection, bgp_fsm_state_t new_state)
{
  struct stream *s;

  if (bd_all != NULL)
    {
      s = bgp_dump_common(bd_all, connection, MRT_MST_BGP4MP_STATE_CHANGE_AS4,
                                                                         true) ;
      stream_putw(s, bgp_dump_fsm_state(connection->state));
      stream_putw(s, bgp_dump_fsm_state(new_state)) ;

      bgp_dump_set_size (s, 0);

      bd_all = bgp_dump_put(bd_all, STREAM_DATA (s),
                                                 stream_get_endp (s), NULL, 0) ;
      if (bd_all != NULL)
        return ;                /* OK, so no flag change        */
    } ;

  bgp_dump_set_flags() ;
} ;

/*------------------------------------------------------------------------------
 * Dump BGP packet received, if required -- BGP Engine
 *
 * Does nothing if no bd_all and no bd_updates.
 *
 * Frees a dump in the event of any I/O error on it.
 */
extern void
bgp_dump_packet (bgp_connection connection)
{
  bgp_dump  bd ;
  struct stream* s ;
  uint plen ;
  bool du ;

  /* If we have nothing to do, get out, quick.
   *
   * If we have something to do, select one of the stream buffers for the
   * MRT header.  Note that in the (unlikely) event of having both ALL and
   * UPDATES dumps, selects the UPDATES obuf.
   */
  du = (bd_updates != NULL) && (connection->msg_type == BGP_MSG_UPDATE) ;

  if (du)
    bd = bd_updates ;
  else if (bd_all != NULL)
    bd = bd_all ;
  else
    return bgp_dump_set_flags() ;       /* nothing to do        */

  /* Construct message header for packet dump
   *
   * Note that in the (unlikely) event of having both ALL and UPDATES dumps,
   * we construct just the one header.
   */
  s = bgp_dump_common(bd, connection,
                                connection->as4 ? MRT_MST_BGP4MP_MESSAGE_AS4
                                                : MRT_MST_BGP4MP_MESSAGE,
                                                              connection->as4) ;
  plen = stream_get_endp(connection->ibuf) ;
  bgp_dump_set_size (s, plen) ;

  /* Output the MRT header and the packet
   *
   * In the event of an I/O failure, free the dump.
   *
   * Note that in the (unlikely) event of having both ALL and UPDATES dumps,
   * we output the UPDATES last -- so that if the ALL fails the message
   * header for the UPDATES dump is still there !
   */
  if (bd_all != NULL)
    {
      bd_all = bgp_dump_put(bd_all, STREAM_DATA(s),
                                         stream_get_endp(s),
                                          STREAM_DATA(connection->ibuf), plen) ;
      if (bd_all == NULL)
        bgp_dump_set_flags() ;
    } ;

  if (du)
    {
      bd_updates = bgp_dump_put(bd_updates, STREAM_DATA(s),
                                         stream_get_endp(s),
                                          STREAM_DATA(connection->ibuf), plen) ;
      if (bd_updates == NULL)
        bgp_dump_set_flags() ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Construct header and common parts of a MSG_PROTOCOL_BGP4MP MRT message
 */
static struct stream*
bgp_dump_common (bgp_dump bd, bgp_connection connection, int subtype, bool as4)
{
  static const char empty[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  struct stream *s ;
  asn_t remote_as ;
  asn_t local_as ;
  sockunion su_remote ;
  sockunion su_local ;
  int sal ;

  s = bgp_dump_header(bd, MRT_MT_BGP4MP, subtype) ;

  /* Source AS number and Destination AS number
   */
  remote_as = connection->session->as_peer ;
  local_as  = connection->session->open_send->my_as ;

  if (as4)
    {
      stream_putl (s, remote_as);
      stream_putl (s, local_as);
    }
  else
    {
      stream_putw (s, remote_as);
      stream_putw (s, local_as);
    }

  /* Interface index
   */
  stream_putw (s, connection->session->ifindex);

  /* Remote and local IP addresses
   *
   * If no connection has been made, there will not be a local IP address.
   *
   * Note that we expect the local and remote addresses to be of the same
   * family.  We use the length from the remote address in both cases -- so if
   * something "impossible" has happened, then the message will still be
   * "syntactically" well formed.
   */
  su_remote = connection->session->su_peer ;
  su_local  = connection->su_local ;

  stream_putw (s, sockunion_get_afi(su_remote)) ;

  confirm(MRT_AFI_IPv4 == AFI_IP) ;
#ifdef HAVE_IPV6
  confirm(MRT_AFI_IPv6 == AFI_IP6) ;
#endif

  sal = sockunion_get_addr_len(su_remote) ;
  stream_put(s, sockunion_get_addr(su_remote), sal) ;

  if (su_local != NULL)
    stream_put(s, sockunion_get_addr(su_local), sal) ;
  else
    stream_put(s, empty, sal) ;

  return s ;
} ;

/*------------------------------------------------------------------------------
 * Set the bgp_dump_state_flag and the bgp_dump_packet_flag as required.
 *
 * These flags simply reflect the state of the ALL and UPDATES dumps,
 * but are used to avoid calling the dump functions when not required (which
 * is most of the time !).
 *
 * Note that it doesn't matter if these flags were to not properly reflect the
 * dump state !
 */
static void
bgp_dump_set_flags(void)
{
  bgp_dump_state_flag   = (bd_all != NULL) ;
  bgp_dump_packet_flag  = (bd_all != NULL) || (bd_updates != NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Map an internal bgp_fsm_state_t value to the MRT Values
 */
static int
bgp_dump_fsm_state(bgp_fsm_state_t state)
{
  switch (state)
    {
      case bgp_fsm_sInitial:
        return MRT_FSM_UNDEF ;

      case bgp_fsm_sIdle:
        return MRT_FSM_Idle ;

      case bgp_fsm_sConnect:
        return MRT_FSM_Connect ;

      case bgp_fsm_sActive:
        return MRT_FSM_Active ;

      case bgp_fsm_sOpenSent:
        return MRT_FSM_OpenSent ;

      case bgp_fsm_sOpenConfirm:
        return MRT_FSM_OpenConfirm ;

      case bgp_fsm_sEstablished:
        return MRT_FSM_Established ;

      case bgp_fsm_sStopping:
        return MRT_FSM_UNDEF ;

      default:
        return MRT_FSM_UNDEF ;
    } ;
} ;

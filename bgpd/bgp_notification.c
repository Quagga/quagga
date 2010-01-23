/* BGP Notification state handling
 * Copyright (C) 1996, 97, 98 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) Chris Hall (GMCH), Highwayman
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

#include <string.h>
#include <netinet/in.h>

#include "lib/zassert.h"
#include "lib/memory.h"

#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_open_state.h"

/*==============================================================================
 * A bgp_notify structure encapsulates the contents of a BGP NOTIFICATION
 * message.
 *
 *
 */

/*------------------------------------------------------------------------------
 * Calculate size for bgp_notify of given length
 *
 * Rounds up to multiple of 32, such that is always at least 16 bytes available.
 */
static inline bgp_size_t
bgp_notify_size(bgp_size_t size)
{
  return (size == 0) ? 0 : ((size + 32 + 16 - 1) / 32) * 32 ;
} ;

/*==============================================================================
 * Create/Destroy bgp_notify
 */

/*------------------------------------------------------------------------------
 * Allocate and initialise new notification
 *
 * Can specify an expected amount of data.
 */
extern bgp_notify
bgp_notify_new(bgp_nom_code_t code, bgp_nom_subcode_t subcode,
                                                              bgp_size_t expect)
{
  bgp_notify notification ;

  notification = XCALLOC(MTYPE_BGP_NOTIFY, sizeof(struct bgp_notify)) ;

  notification->code    = code ;
  notification->subcode = subcode ;
  notification->size    = bgp_notify_size(expect) ;
  notification->length  = 0 ;

  if (notification->size != 0)
    notification->data  = XCALLOC(MTYPE_TMP, notification->size) ;
  else
    notification->data  = NULL ;

  return notification ;
} ;

/*------------------------------------------------------------------------------
 * Free notification structure
 *
 * Does nothing if there is no structure.
 */
extern void
bgp_notify_free(bgp_notify notification)
{
  if (notification != NULL)
    {
      if (notification->data != NULL)
        XFREE(MTYPE_TMP, notification->data) ;
      XFREE(MTYPE_BGP_NOTIFY, notification) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Duplicate existing notification (if any)
 */
extern bgp_notify
bgp_notify_dup(bgp_notify notification)
{
  bgp_notify duplicate ;

  if (notification == NULL)
    return NULL ;

  duplicate = XMALLOC(MTYPE_BGP_NOTIFY, sizeof(struct bgp_notify)) ;
  *duplicate = *notification ;

  if (notification->length == 0)
    {
      duplicate->size = 0 ;
      duplicate->data = NULL ;
    }
  else
    {
      bgp_size_t size = bgp_notify_size(duplicate->length) ;
      duplicate->size = size ;
      duplicate->data = XCALLOC(MTYPE_TMP, size) ;
      memcpy(duplicate->data, notification->data, duplicate->length) ;
    } ;

  return duplicate ;
} ;

/*------------------------------------------------------------------------------
 * Unset pointer to notification and free any existing notification structure.
 *
 * Does nothing if there is no structure.
 */
extern void
bgp_notify_unset(bgp_notify* p_notification)
{
  bgp_notify_free(*p_notification) ;    /* free anything that's there   */
  *p_notification = NULL ;
} ;

/*------------------------------------------------------------------------------
 * Set pointer to notification
 *
 * Frees any existing notification at the destination.
 *
 * NB: copies the source pointer -- so must be clear about responsibility
 *     for the notification structure.
 */
extern void
bgp_notify_set(bgp_notify* p_dst, bgp_notify src)
{
  bgp_notify_free(*p_dst) ;
  *p_dst = src ;
} ;

/*------------------------------------------------------------------------------
 * Set pointer to notification to a *copy* of the source.
 *
 * Frees any existing notification at the destination.
 */
extern void
bgp_notify_set_dup(bgp_notify* p_dst, bgp_notify src)
{
  bgp_notify_set(p_dst, bgp_notify_dup(src)) ;
} ;

/*------------------------------------------------------------------------------
 * Set pointer to notification and unset source pointer
 *
 * Frees any existing notification at the destination.
 *
 * NB: responsibility for the notification structure passes to the destination.
 */
extern void
bgp_notify_set_mov(bgp_notify* p_dst, bgp_notify* p_src)
{
  bgp_notify_free(*p_dst) ;
  *p_dst = *p_src ;
  *p_src = NULL ;
} ;

/*==============================================================================
 * Set new Code and Subcode and discard and data accumulated so far.
 */
extern bgp_notify
bgp_notify_reset(bgp_notify notification, bgp_nom_code_t code,
                                                      bgp_nom_subcode_t subcode)
{
  if (notification == NULL)
    return bgp_notify_new(code, subcode, 0) ;

  notification->code    = code ;
  notification->subcode = subcode ;
  notification->length  = 0 ;

  return notification ;
} ;

/*==============================================================================
 * Append data to given notification
 *
 * Copes with zero length append.
 *
 * NB: returns possibly NEW ADDRESS of the notification.
 */
extern void
bgp_notify_append_data(bgp_notify notification, const void* data,
                                                                 bgp_size_t len)
{
  bgp_size_t new_length = notification->length + len ;

  if (new_length > notification->size)
    {
      bgp_size_t size = bgp_notify_size(new_length) ;

      if (notification->size == 0)
        notification->data = XCALLOC(MTYPE_TMP, size) ;
      else
        notification->data = XREALLOC(MTYPE_TMP, notification->data, size) ;

      memset((char*)notification + notification->size, 0,
                                                    size - notification->size) ;
      notification->size = size ;
    } ;

  if (len > 0)
    memcpy((char*)(notification->data) + notification->length, data, len) ;

  notification->length = new_length ;
} ;

/*------------------------------------------------------------------------------
 * Append one byte
 */
extern void
bgp_notify_append_b(bgp_notify notification, uint8_t b)
{
  bgp_notify_append_data(notification, &b, 1) ;
} ;

/*------------------------------------------------------------------------------
 * Append one word (uint16_t), in network byte order
 */
extern void
bgp_notify_append_w(bgp_notify notification, uint16_t w)
{
  w = htons(w) ;
  bgp_notify_append_data(notification, &w, 2) ;
} ;

/*------------------------------------------------------------------------------
 * Append one long (uint32_t), in network byte order
 */
extern void
bgp_notify_append_l(bgp_notify notification, uint32_t l)
{
  l = htonl(l) ;
  bgp_notify_append_data(notification, &l, 4) ;
} ;

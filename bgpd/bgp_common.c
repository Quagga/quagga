/* BGP Common -- header
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "bgpd/bgp_common.h"
#include "lib/zassert.h"

/*==============================================================================
 * Conversion qafx_bit_t -> qafx_num_t.
 *
 * If no bits are set, returns qafx_num_undef.
 *
 * If more than one bit is set, returns the lowest number qafx.
 *
 * NB: this is not built for speed.
 *
 * NB: it is a mistake to convert a value > qafx_bits_max (FATAL unless NDEBUG)
 */
extern qafx_num_t
qafx_num(qafx_bit_t bit)
{
  qafx_num_t num ;
  dassert(bit <= qafx_bits_max) ;

  if (bit == 0)
    return qafx_num_undef ;

  num = 0 ;

  while ((bit & 0xF) == 0)
    {
      num  += 4 ;
      bit >>= 4 ;
    }

  while ((bit & 1) == 0)
    {
      num  += 1;
      bit >>= 1 ;
    } ;

  return num ;
} ;

/*==============================================================================
 * Conversion tables for qafx_num => qAFI and qSAFI
 *                   and qafx_num => iAFI and iSAFI
 */

const qAFI_t  qAFI_map[] =
  {
    [qafx_ipv4_unicast]     = qAFI_IPV4,
    [qafx_ipv4_multicast]   = qAFI_IPV4,
    [qafx_ipv4_mpls_vpn]    = qAFI_IPV4,
    [qafx_ipv6_unicast]     = qAFI_IPV6,
    [qafx_ipv6_multicast]   = qAFI_IPV6,
    [qafx_ipv6_mpls_vpn]    = qAFI_IPV6,
    [qafx_num_other]        = qAFI_undef
  } ;

const qSAFI_t qSAFI_map[] =
  {
    [qafx_ipv4_unicast]     = qSAFI_Unicast,
    [qafx_ipv4_multicast]   = qSAFI_Multicast,
    [qafx_ipv4_mpls_vpn]    = qSAFI_MPLS_VPN,
    [qafx_ipv6_unicast]     = qSAFI_Unicast,
    [qafx_ipv6_multicast]   = qSAFI_Multicast,
    [qafx_ipv6_mpls_vpn]    = qSAFI_MPLS_VPN,
    [qafx_num_other]        = qSAFI_undef
  } ;

const iAFI_t  iAFI_map[] =
  {
    [qafx_ipv4_unicast]     = iAFI_IPV4,
    [qafx_ipv4_multicast]   = iAFI_IPV4,
    [qafx_ipv4_mpls_vpn]    = iAFI_IPV4,
    [qafx_ipv6_unicast]     = iAFI_IPV6,
    [qafx_ipv6_multicast]   = iAFI_IPV6,
    [qafx_ipv6_mpls_vpn]    = iAFI_IPV6,
    [qafx_num_other]        = iAFI_Reserved
  } ;

const iSAFI_t iSAFI_map[] =
  {
    [qafx_ipv4_unicast]     = iSAFI_Unicast,
    [qafx_ipv4_multicast]   = iSAFI_Multicast,
    [qafx_ipv4_mpls_vpn]    = iSAFI_MPLS_VPN,
    [qafx_ipv6_unicast]     = iSAFI_Unicast,
    [qafx_ipv6_multicast]   = iSAFI_Multicast,
    [qafx_ipv6_mpls_vpn]    = iSAFI_MPLS_VPN,
    [qafx_num_other]        = iSAFI_Reserved,
  } ;

/*==============================================================================
 * Convert iAFI/iSAFI => qafx_num_t  -- tolerates unknown/reserved
 *     and qAFI/qSAFI => qafx_num_t  -- tolerates undef, but not unknown
 */

/*------------------------------------------------------------------------------
 * iAFI/iSAFI => qafx_num_t   unknowns => qafx_num_other
 *                            reserved => qafx_num_undef
 */
extern qafx_num_t
qafx_num_from_iAFI_iSAFI(iAFI_t afi, iSAFI_t safi)
{
  switch (afi)
  {
    case iAFI_Reserved:
      return qafx_num_undef ;           /* no matter what the iSAFI is  */

    case iAFI_IP:
      switch(safi)
      {
        case iSAFI_Reserved:
          return qafx_num_undef ;       /* no matter what the iAFI is   */
        case iSAFI_Unicast:
          return qafx_ipv4_unicast ;
        case iSAFI_Multicast:
          return qafx_ipv4_multicast ;
        case iSAFI_MPLS_VPN:
          return qafx_ipv4_mpls_vpn ;
        default:
          break ;
      } ;
      break ;

    case iAFI_IP6:
      switch(safi)
      {
        case iSAFI_Reserved:
          return qafx_num_undef ;       /* no matter what the iAFI is   */
        case iSAFI_Unicast:
          return qafx_ipv6_unicast ;
        case iSAFI_Multicast:
          return qafx_ipv6_multicast ;
        case iSAFI_MPLS_VPN:
          return qafx_ipv6_mpls_vpn ;
        default:
          break ;
      } ;
      break ;

    default:
      switch(safi)
      {
        case iSAFI_Reserved:
          return qafx_num_undef ;       /* no matter what the iAFI is   */
        default:
          break ;
      } ;
      break ;
  } ;

  return qafx_num_other ;
} ;

/*------------------------------------------------------------------------------
 * qAFI/qSAFI => qafx_num_t
 *
 * NB: qAFI_undef   with any qSAFI_xxx => qafx_num_undef
 *     qSAFI_undef  with any qAFI_xxx  => qafx_num_undef
 *     qSAFI_Unused qith any qAFI_xxx  => qafx_num_undef
 *
 * NB: any unrecognised qAFI/qSAFI combinations => FATAL error
 */
extern qafx_num_t
qafx_num_from_qAFI_qSAFI(qAFI_t afi, qSAFI_t safi)
{
  switch (afi)
  {
    case qAFI_undef:
      if ((safi >= qSAFI_min) && (safi <= qSAFI_max))
        return qafx_num_undef ;         /* for all valid qSAFI  */
      break ;

    case qAFI_IP:
      switch(safi)
      {
        case qSAFI_undef:
        case qSAFI_Unused:
          return qafx_num_undef ;
        case qSAFI_Unicast:
          return qafx_ipv4_unicast ;
        case qSAFI_Multicast:
          return qafx_ipv4_multicast ;
        case qSAFI_MPLS_VPN:
          return qafx_ipv4_mpls_vpn ;
        default:
          break ;
      } ;
      break ;

    case qAFI_IP6:
      switch(safi)
      {
        case qSAFI_undef:
        case qSAFI_Unused:
          return qafx_num_undef ;
        case qSAFI_Unicast:
          return qafx_ipv6_unicast ;
        case qSAFI_Multicast:
          return qafx_ipv6_multicast ;
        case qSAFI_MPLS_VPN:
          return qafx_ipv6_mpls_vpn ;
        default:
          break ;
      } ;
      break ;

    default:
      break ;
  } ;

  zabort("invalid qAFI or qSAFI") ;
} ;

/*==============================================================================
 * Convert iAFI/iSAFI => qafx_bit_t  -- tolerates unknown/reserved
 *     and qAFI/qSAFI => qafx_bit_t  -- tolerates undef, but not unknown
 */

/*------------------------------------------------------------------------------
 * iAFI/iSAFI => qafx_bit_t   unknowns => 0
 *                            reserved => 0
 */
extern qafx_bit_t
qafx_bit_from_iAFI_iSAFI(iAFI_t afi, iSAFI_t safi)
{
  qafx_num_t  qn = qafx_num_from_iAFI_iSAFI(afi, safi) ;

  if ((qn != qafx_num_undef) && (qn != qafx_num_other))
    return qafx_bit(qn) ;
  else
    return 0 ;
} ;

/*------------------------------------------------------------------------------
 * qAFI/qSAFI => qafx_bit_t
 *
 * NB: qAFI_undef   with any qSAFI_xxx => 0
 *     qSAFI_undef  with any qAFI_xxx  => 0
 *     qSAFI_Unused qith any qAFI_xxx  => 0
 *
 * NB: any unrecognised qAFI/qSAFI combinations => FATAL error
 */
extern qafx_bit_t
qafx_bit_from_qAFI_qSAFI(qAFI_t afi, qSAFI_t safi)
{
  qafx_num_t  qn = qafx_num_from_qAFI_qSAFI(afi, safi) ;

  if ((qn != qafx_num_undef) && (qn != qafx_num_other))
    return qafx_bit(qn) ;
  else
    return 0 ;
} ;

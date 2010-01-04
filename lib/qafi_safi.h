/* Quagga AFI/SAFI
 * Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002 Kunihiro Ishiguro
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#ifndef _QUAGGA_AFI_SAFI_H
#define _QUAGGA_AFI_SAFI_H

#include <stdint.h>
#include "lib/zassert.h"

/*==============================================================================
 * Generic AFI and SAFI types.
 */
typedef uint16_t afi_t;
typedef uint8_t  safi_t;

/*==============================================================================
 * iAFI and iSAFI
 *
 * These are the standard IANA registered AFI and SAFI values that Quagga is
 * at all interested in.
 */

typedef enum iAFI  iAFI_t ;

enum iAFI
{
  iAFI_Reserved    = 0,         /* No meaning defined by IANA   */

  iAFI_IP          = 1,         /* IP (IP version 4)            */
  iAFI_IP6         = 2,         /* IP6 (IP version 6)           */

  iAFI_IPV4        = iAFI_IP,   /* locally AKA                  */
  iAFI_IPV6        = iAFI_IP6   /* locally AKA                  */
} ;


typedef enum iSAFI iSAFI_t ;

enum iSAFI
{
  iSAFI_Reserved   =   0,       /* No meaning defined by IANA   */

  iSAFI_Unicast    =   1,       /* unicast forwarding           */
  iSAFI_Multicast  =   2,       /* multicast forwarding         */

  iSAFI_Unused     =   3,       /* also Reserved by IANA        */

  iSAFI_MPLS_VPN   = 128        /* MPLS-labeled VPN address     */
} ;

/*==============================================================================
 * qAFI and qSAFI
 *
 * These are the AFI and SAFI values that Quagga uses internally.
 *
 * They are almost the same as the IANA numbers, but different where that
 * is required to produce a dense set.
 */

typedef enum qAFI  qAFI_t ;

enum qAFI
{
  qAFI_min         = 0,         /* minimum valid qAFI           */
  qAFI_undef       = 0,         /* undefined AFI                */

  qAFI_first       = 1,         /* first real qAFI              */

  qAFI_IP          = 1,
  qAFI_IP6         = 2,

  qAFI_last        = 2,         /* last real qAFI               */

  qAFI_max         = 2,         /* maximum valid qAFI           */
  qAFI_count,                   /* number of distinct qAFI      */

  qAFI_IPV4        = qAFI_IP,
  qAFI_IPV6        = qAFI_IP6
} ;

typedef enum qSAFI qSAFI_t ;

enum qSAFI
{
  qSAFI_min        =   1,       /* minimum valid qSAFI          */
  qSAFI_undef      =   0,       /* undefined SAFI               */

  qSAFI_first      =   1,       /* first real qSAFI             */

  qSAFI_Unicast    =   1,
  qSAFI_Multicast  =   2,
  qSAFI_Unused     =   3,
  qSAFI_MPLS_VPN   =   4,

  qSAFI_last       =   4,       /* last real qSAFI              */

  qSAFI_max        =   4,       /* maximum valid qSAFI          */
  qSAFI_count                   /* number of distinct qSAFI     */
} ;

/*==============================================================================
 * Quagga AFI/SAFI values -- original macro definitions
 */

/* Address family numbers from RFC1700. */
#define AFI_IP                    1
#define AFI_IP6                   2
#define AFI_MAX                   3

CONFIRM( (AFI_IP  == qAFI_IP)
      && (AFI_IP  == iAFI_IP) ) ;
CONFIRM( (AFI_IP6 == qAFI_IP6)
      && (AFI_IP6 == iAFI_IP6) ) ;
CONFIRM(AFI_MAX == qAFI_count) ;

/* Subsequent Address Family Identifier. */
#define SAFI_UNICAST              1
#define SAFI_MULTICAST            2
#define SAFI_UNICAST_MULTICAST    3
#define SAFI_MPLS_VPN             4
#define SAFI_MAX                  5

CONFIRM( (SAFI_UNICAST           == qSAFI_Unicast)
      && (SAFI_UNICAST           == iSAFI_Unicast) ) ;
CONFIRM( (SAFI_MULTICAST         == qSAFI_Multicast)
      && (SAFI_MULTICAST         == iSAFI_Multicast) ) ;
CONFIRM( (SAFI_UNICAST_MULTICAST == qSAFI_Unused)
      && (SAFI_UNICAST_MULTICAST == iSAFI_Unused) ) ;
CONFIRM(SAFI_MPLS_VPN  == qSAFI_MPLS_VPN) ;
CONFIRM(SAFI_MAX       == qSAFI_count) ;

#endif /* _QUAGGA_AFI_SAFI_H */

/*
  PIM for Quagga
  Copyright (C) 2008  Everton da Silva Marques

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING; if not, write to the
  Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
  MA 02110-1301 USA
  
  $QuaggaId: $Format:%an, %ai, %h$ $
*/

#include <zebra.h>

#include "log.h"
#include "prefix.h"

#include "pimd.h"
#include "pim_tlv.h"
#include "pim_str.h"
#include "pim_msg.h"

char *pim_tlv_append_uint16(char *buf,
			    const char *buf_pastend,
			    uint16_t option_type,
			    uint16_t option_value)
{
  uint16_t option_len = 2;

  if ((buf + PIM_TLV_OPTION_SIZE(option_len)) > buf_pastend) {
    zlog_warn("%s: buffer overflow: left=%d needed=%d",
	      __PRETTY_FUNCTION__,
	      buf_pastend - buf, PIM_TLV_OPTION_SIZE(option_len));
    return 0;
  }

  *(uint16_t *) buf = htons(option_type);
  buf += 2;
  *(uint16_t *) buf = htons(option_len);
  buf += 2;
  *(uint16_t *) buf = htons(option_value);
  buf += option_len;

  return buf;
}

char *pim_tlv_append_2uint16(char *buf,
			     const char *buf_pastend,
			     uint16_t option_type,
			     uint16_t option_value1,
			     uint16_t option_value2)
{
  uint16_t option_len = 4;

  if ((buf + PIM_TLV_OPTION_SIZE(option_len)) > buf_pastend) {
    zlog_warn("%s: buffer overflow: left=%d needed=%d",
	      __PRETTY_FUNCTION__,
	      buf_pastend - buf, PIM_TLV_OPTION_SIZE(option_len));
    return 0;
  }

  *(uint16_t *) buf = htons(option_type);
  buf += 2;
  *(uint16_t *) buf = htons(option_len);
  buf += 2;
  *(uint16_t *) buf = htons(option_value1);
  buf += 2;
  *(uint16_t *) buf = htons(option_value2);
  buf += 2;

  return buf;
}

char *pim_tlv_append_uint32(char *buf,
			    const char *buf_pastend,
			    uint16_t option_type,
			    uint32_t option_value)
{
  uint16_t option_len = 4;

  if ((buf + PIM_TLV_OPTION_SIZE(option_len)) > buf_pastend) {
    zlog_warn("%s: buffer overflow: left=%d needed=%d",
	      __PRETTY_FUNCTION__,
	      buf_pastend - buf, PIM_TLV_OPTION_SIZE(option_len));
    return 0;
  }

  *(uint16_t *) buf = htons(option_type);
  buf += 2;
  *(uint16_t *) buf = htons(option_len);
  buf += 2;
  *(uint32_t *) buf = htonl(option_value);
  buf += option_len;

  return buf;
}

#define ucast_ipv4_encoding_len (2 + sizeof(struct in_addr))

char *pim_tlv_append_addrlist_ucast(char *buf,
				    const char *buf_pastend,
				    struct list *ifconnected)
{
  struct listnode *node;
  uint16_t option_len = 0;

  char *curr;

  node = listhead(ifconnected);

  /* Empty address list ? */
  if (!node) {
    return buf;
  }

  /* Skip first address (primary) */
  node = listnextnode(node);

  /* Scan secondary address list */
  curr = buf + 4; /* skip T and L */
  for (; node; node = listnextnode(node)) {
    struct connected *ifc = listgetdata(node);
    struct prefix *p = ifc->address;
    
    if (p->family != AF_INET)
      continue;

    if ((curr + ucast_ipv4_encoding_len) > buf_pastend) {
      zlog_warn("%s: buffer overflow: left=%d needed=%d",
		__PRETTY_FUNCTION__,
		buf_pastend - curr, ucast_ipv4_encoding_len);
      return 0;
    }

    /* Write encoded unicast IPv4 address */
    *(uint8_t *) curr = PIM_MSG_ADDRESS_FAMILY_IPV4; /* notice: AF_INET != PIM_MSG_ADDRESS_FAMILY_IPV4 */
    ++curr;
    *(uint8_t *) curr = 0; /* ucast IPv4 native encoding type (RFC 4601: 4.9.1) */
    ++curr;
    *(struct in_addr *) curr = p->u.prefix4;
    curr += sizeof(struct in_addr);

    option_len += ucast_ipv4_encoding_len; 
  }

  if (PIM_DEBUG_PIM_TRACE) {
    zlog_warn("%s: number of encoded secondary unicast IPv4 addresses: %d",
	      __PRETTY_FUNCTION__,
	      option_len / ucast_ipv4_encoding_len);
  }

  if (option_len < 1) {
    /* Empty secondary unicast IPv4 address list */
    return buf;
  }

  /*
   * Write T and L
   */
  *(uint16_t *) buf       = htons(PIM_MSG_OPTION_TYPE_ADDRESS_LIST);
  *(uint16_t *) (buf + 2) = htons(option_len);

  return curr;
}

static int check_tlv_length(const char *label, const char *tlv_name,
			    const char *ifname, struct in_addr src_addr,
			    int correct_len, int option_len)
{
  if (option_len != correct_len) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: PIM hello %s TLV with incorrect value size=%d correct=%d from %s on interface %s",
	      label, tlv_name,
	      option_len, correct_len,
	      src_str, ifname);
    return -1;
  }

  return 0;
}

static void check_tlv_redefinition_uint16(const char *label, const char *tlv_name,
					  const char *ifname, struct in_addr src_addr,
					  pim_hello_options options,
					  pim_hello_options opt_mask,
					  uint16_t new, uint16_t old)
{
  if (PIM_OPTION_IS_SET(options, opt_mask)) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: PIM hello TLV redefined %s=%u old=%u from %s on interface %s",
	      label, tlv_name,
	      new, old,
	      src_str, ifname);
  }
}

static void check_tlv_redefinition_uint32(const char *label, const char *tlv_name,
					  const char *ifname, struct in_addr src_addr,
					  pim_hello_options options,
					  pim_hello_options opt_mask,
					  uint32_t new, uint32_t old)
{
  if (PIM_OPTION_IS_SET(options, opt_mask)) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: PIM hello TLV redefined %s=%u old=%u from %s on interface %s",
	      label, tlv_name,
	      new, old,
	      src_str, ifname);
  }
}

static void check_tlv_redefinition_uint32_hex(const char *label, const char *tlv_name,
					      const char *ifname, struct in_addr src_addr,
					      pim_hello_options options,
					      pim_hello_options opt_mask,
					      uint32_t new, uint32_t old)
{
  if (PIM_OPTION_IS_SET(options, opt_mask)) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: PIM hello TLV redefined %s=%08x old=%08x from %s on interface %s",
	      label, tlv_name,
	      new, old,
	      src_str, ifname);
  }
}

int pim_tlv_parse_holdtime(const char *ifname, struct in_addr src_addr,
			   pim_hello_options *hello_options,
			   uint16_t *hello_option_holdtime,
			   uint16_t option_len,
			   const char *tlv_curr) 
{
  const char *label = "holdtime";

  if (check_tlv_length(__PRETTY_FUNCTION__, label,
		       ifname, src_addr,
		       sizeof(uint16_t), option_len)) {
    return -1;
  }
  
  check_tlv_redefinition_uint16(__PRETTY_FUNCTION__, label,
				ifname, src_addr,
				*hello_options, PIM_OPTION_MASK_HOLDTIME,
				PIM_TLV_GET_HOLDTIME(tlv_curr),
				*hello_option_holdtime);
  
  PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_HOLDTIME);
  
  *hello_option_holdtime = PIM_TLV_GET_HOLDTIME(tlv_curr);
  
  return 0;
}

int pim_tlv_parse_lan_prune_delay(const char *ifname, struct in_addr src_addr,
				  pim_hello_options *hello_options,
				  uint16_t *hello_option_propagation_delay,
				  uint16_t *hello_option_override_interval,
				  uint16_t option_len,
				  const char *tlv_curr) 
{
  if (check_tlv_length(__PRETTY_FUNCTION__, "lan_prune_delay",
		       ifname, src_addr,
		       sizeof(uint32_t), option_len)) {
    return -1;
  }
  
  check_tlv_redefinition_uint16(__PRETTY_FUNCTION__, "propagation_delay",
				ifname, src_addr,
				*hello_options, PIM_OPTION_MASK_LAN_PRUNE_DELAY,
				PIM_TLV_GET_PROPAGATION_DELAY(tlv_curr),
				*hello_option_propagation_delay);
  
  PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_LAN_PRUNE_DELAY);
  
  *hello_option_propagation_delay = PIM_TLV_GET_PROPAGATION_DELAY(tlv_curr);
  if (PIM_TLV_GET_CAN_DISABLE_JOIN_SUPPRESSION(tlv_curr)) {
    PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_CAN_DISABLE_JOIN_SUPPRESSION);
  }
  else {
    PIM_OPTION_UNSET(*hello_options, PIM_OPTION_MASK_CAN_DISABLE_JOIN_SUPPRESSION);
  }
  ++tlv_curr;
  ++tlv_curr;
  *hello_option_override_interval = PIM_TLV_GET_OVERRIDE_INTERVAL(tlv_curr);
  
  return 0;
}

int pim_tlv_parse_dr_priority(const char *ifname, struct in_addr src_addr,
			      pim_hello_options *hello_options,
			      uint32_t *hello_option_dr_priority,
			      uint16_t option_len,
			      const char *tlv_curr) 
{
  const char *label = "dr_priority";

  if (check_tlv_length(__PRETTY_FUNCTION__, label,
		       ifname, src_addr,
		       sizeof(uint32_t), option_len)) {
    return -1;
  }
  
  check_tlv_redefinition_uint32(__PRETTY_FUNCTION__, label,
				ifname, src_addr,
				*hello_options, PIM_OPTION_MASK_DR_PRIORITY,
				PIM_TLV_GET_DR_PRIORITY(tlv_curr),
				*hello_option_dr_priority);
  
  PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_DR_PRIORITY);
  
  *hello_option_dr_priority = PIM_TLV_GET_DR_PRIORITY(tlv_curr);
  
  return 0;
}

int pim_tlv_parse_generation_id(const char *ifname, struct in_addr src_addr,
				pim_hello_options *hello_options,
				uint32_t *hello_option_generation_id,
				uint16_t option_len,
				const char *tlv_curr) 
{
  const char *label = "generation_id";

  if (check_tlv_length(__PRETTY_FUNCTION__, label,
		       ifname, src_addr,
		       sizeof(uint32_t), option_len)) {
    return -1;
  }
  
  check_tlv_redefinition_uint32_hex(__PRETTY_FUNCTION__, label,
				    ifname, src_addr,
				    *hello_options, PIM_OPTION_MASK_GENERATION_ID,
				    PIM_TLV_GET_GENERATION_ID(tlv_curr),
				    *hello_option_generation_id);
  
  PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_GENERATION_ID);
  
  *hello_option_generation_id = PIM_TLV_GET_GENERATION_ID(tlv_curr);
  
  return 0;
}

int pim_parse_addr_ucast(const char *ifname, struct in_addr src_addr,
			 struct prefix *p,
			 const char *buf,
			 int buf_size)
{
  const int ucast_encoding_min_len = 3; /* 1 family + 1 type + 1 addr */
  const char *addr;
  const char *pastend;
  int family;
  int type;

  if (buf_size < ucast_encoding_min_len) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: unicast address encoding overflow: left=%d needed=%d from %s on %s",
	      __PRETTY_FUNCTION__,
	      buf_size, ucast_encoding_min_len,
	      src_str, ifname);
    return -1;
  }

  addr = buf;
  pastend = buf + buf_size;

  family = *(const uint8_t *) addr;
  ++addr;
  type = *(const uint8_t *) addr;
  ++addr;

  switch (family) {
  case PIM_MSG_ADDRESS_FAMILY_IPV4:
    if (type) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown unicast address encoding type=%d from %s on %s",
		__PRETTY_FUNCTION__,
		type, src_str, ifname);
      return -2;
    }

    if ((addr + sizeof(struct in_addr)) > pastend) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: IPv4 unicast address overflow: left=%d needed=%d from %s on %s",
		__PRETTY_FUNCTION__,
		pastend - addr, sizeof(struct in_addr),
		src_str, ifname);
      return -3;
    }

    p->family = AF_INET; /* notice: AF_INET != PIM_MSG_ADDRESS_FAMILY_IPV4 */
    p->u.prefix4 = *(const struct in_addr *) addr;
    addr += sizeof(struct in_addr);

    break;
  default:
    {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown unicast address encoding family=%d from %s on %s",
		__PRETTY_FUNCTION__,
		family, src_str, ifname);
      return -4;
    }
  }

  return addr - buf;
}

int pim_parse_addr_group(const char *ifname, struct in_addr src_addr,
			 struct prefix *p,
			 const char *buf,
			 int buf_size)
{
  const int grp_encoding_min_len = 4; /* 1 family + 1 type + 1 reserved + 1 addr */
  const char *addr;
  const char *pastend;
  int family;
  int type;
  int mask_len;

  if (buf_size < grp_encoding_min_len) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: group address encoding overflow: left=%d needed=%d from %s on %s",
	      __PRETTY_FUNCTION__,
	      buf_size, grp_encoding_min_len,
	      src_str, ifname);
    return -1;
  }

  addr = buf;
  pastend = buf + buf_size;

  family = *(const uint8_t *) addr;
  ++addr;
  type = *(const uint8_t *) addr;
  ++addr;
  ++addr; /* skip b_reserved_z fields */
  mask_len = *(const uint8_t *) addr;
  ++addr;

  switch (family) {
  case PIM_MSG_ADDRESS_FAMILY_IPV4:
    if (type) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown group address encoding type=%d from %s on %s",
		__PRETTY_FUNCTION__,
		type, src_str, ifname);
      return -2;
    }

    if ((addr + sizeof(struct in_addr)) > pastend) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: IPv4 group address overflow: left=%d needed=%d from %s on %s",
		__PRETTY_FUNCTION__,
		pastend - addr, sizeof(struct in_addr),
		src_str, ifname);
      return -3;
    }

    p->family = AF_INET; /* notice: AF_INET != PIM_MSG_ADDRESS_FAMILY_IPV4 */
    p->u.prefix4 = *(const struct in_addr *) addr;
    p->prefixlen = mask_len;

    addr += sizeof(struct in_addr);

    break;
  default:
    {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown group address encoding family=%d from %s on %s",
		__PRETTY_FUNCTION__,
		family, src_str, ifname);
      return -4;
    }
  }

  return addr - buf;
}

int pim_parse_addr_source(const char *ifname,
			  struct in_addr src_addr,
			  struct prefix *p,
			  uint8_t *flags,
			  const char *buf,
			  int buf_size)
{
  const int src_encoding_min_len = 4; /* 1 family + 1 type + 1 reserved + 1 addr */
  const char *addr;
  const char *pastend;
  int family;
  int type;
  int mask_len;

  if (buf_size < src_encoding_min_len) {
    char src_str[100];
    pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
    zlog_warn("%s: source address encoding overflow: left=%d needed=%d from %s on %s",
	      __PRETTY_FUNCTION__,
	      buf_size, src_encoding_min_len,
	      src_str, ifname);
    return -1;
  }

  addr = buf;
  pastend = buf + buf_size;

  family = *(const uint8_t *) addr;
  ++addr;
  type = *(const uint8_t *) addr;
  ++addr;
  *flags = *(const uint8_t *) addr;
  ++addr;
  mask_len = *(const uint8_t *) addr;
  ++addr;

  switch (family) {
  case PIM_MSG_ADDRESS_FAMILY_IPV4:
    if (type) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown source address encoding type=%d from %s on %s",
		__PRETTY_FUNCTION__,
		type, src_str, ifname);
      return -2;
    }

    if ((addr + sizeof(struct in_addr)) > pastend) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: IPv4 source address overflow: left=%d needed=%d from %s on %s",
		__PRETTY_FUNCTION__,
		pastend - addr, sizeof(struct in_addr),
		src_str, ifname);
      return -3;
    }

    p->family = AF_INET; /* notice: AF_INET != PIM_MSG_ADDRESS_FAMILY_IPV4 */
    p->u.prefix4 = *(const struct in_addr *) addr;
    p->prefixlen = mask_len;

    addr += sizeof(struct in_addr);

    break;
  default:
    {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: unknown source address encoding family=%d from %s on %s",
		__PRETTY_FUNCTION__,
		family, src_str, ifname);
      return -4;
    }
  }

  return addr - buf;
}

#define FREE_ADDR_LIST(hello_option_addr_list) \
{ \
  if (hello_option_addr_list) { \
    list_delete(hello_option_addr_list); \
    hello_option_addr_list = 0; \
  } \
}

int pim_tlv_parse_addr_list(const char *ifname, struct in_addr src_addr,
			    pim_hello_options *hello_options,
			    struct list **hello_option_addr_list,
			    uint16_t option_len,
			    const char *tlv_curr) 
{
  const char *addr;
  const char *pastend;

  zassert(hello_option_addr_list);

  /*
    Scan addr list
   */
  addr = tlv_curr;
  pastend = tlv_curr + option_len;
  while (addr < pastend) {
    struct prefix tmp;
    int addr_offset;

    /*
      Parse ucast addr
     */
    addr_offset = pim_parse_addr_ucast(ifname, src_addr, &tmp,
				       addr, pastend - addr);
    if (addr_offset < 1) {
      char src_str[100];
      pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
      zlog_warn("%s: pim_parse_addr_ucast() failure: from %s on %s",
		__PRETTY_FUNCTION__,
		src_str, ifname);
      FREE_ADDR_LIST(*hello_option_addr_list);
      return -1;
    }
    addr += addr_offset;

    /*
      Debug
     */
    if (PIM_DEBUG_PIM_TRACE) {
      switch (tmp.family) {
      case AF_INET:
	{
	  char addr_str[100];
	  char src_str[100];
	  pim_inet4_dump("<addr?>", tmp.u.prefix4, addr_str, sizeof(addr_str));
	  pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
	  zlog_debug("%s: PIM hello TLV option: list_old_size=%d IPv4 address %s from %s on %s",
		     __PRETTY_FUNCTION__,
		     *hello_option_addr_list ?
		     ((int) listcount(*hello_option_addr_list)) : -1,
		     addr_str, src_str, ifname);
	}
	break;
      default:
	{
	  char src_str[100];
	  pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
	  zlog_debug("%s: PIM hello TLV option: list_old_size=%d UNKNOWN address family from %s on %s",
		     __PRETTY_FUNCTION__,
		     *hello_option_addr_list ?
		     ((int) listcount(*hello_option_addr_list)) : -1,
		     src_str, ifname);
	}
      }
    }

    /*
      Exclude neighbor's primary address if incorrectly included in
      the secondary address list
     */
    if (tmp.family == AF_INET) {
      if (tmp.u.prefix4.s_addr == src_addr.s_addr) {
	  char src_str[100];
	  pim_inet4_dump("<src?>", src_addr, src_str, sizeof(src_str));
	  zlog_warn("%s: ignoring primary address in secondary list from %s on %s",
		    __PRETTY_FUNCTION__,
		    src_str, ifname);
	  continue;
      }
    }

    /*
      Allocate list if needed
     */
    if (!*hello_option_addr_list) {
      *hello_option_addr_list = list_new();
      if (!*hello_option_addr_list) {
	zlog_err("%s %s: failure: hello_option_addr_list=list_new()",
		 __FILE__, __PRETTY_FUNCTION__);
	return -2;
      }
      (*hello_option_addr_list)->del = (void (*)(void *)) prefix_free;
    }

    /*
      Attach addr to list
     */
    {
      struct prefix *p;
      p = prefix_new();
      if (!p) {
	zlog_err("%s %s: failure: prefix_new()",
		 __FILE__, __PRETTY_FUNCTION__);
	FREE_ADDR_LIST(*hello_option_addr_list);
	return -3;
      }
      p->family = tmp.family;
      p->u.prefix4 = tmp.u.prefix4;
      listnode_add(*hello_option_addr_list, p);
    }

  } /* while (addr < pastend) */
  
  /*
    Mark hello option
   */
  PIM_OPTION_SET(*hello_options, PIM_OPTION_MASK_ADDRESS_LIST);
  
  return 0;
}

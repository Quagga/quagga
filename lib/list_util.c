/* List Utilities
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "list_util.h"

/*==============================================================================
 * Single Base, Single Link
 */

/*------------------------------------------------------------------------------
 * Deleting item
 *
 * Have to chase down list to find item.
 *
 * Note that p_this:
 *
 *   * starts as pointer to the base pointer, so should really be void**,
 *     but that causes all sorts of problems with strict-aliasing.
 *
 *     So: have to cast to (void**) before dereferencing to get the address
 *         of the first item on the list.
 *
 *   * as steps along the list p_this points to the "next pointer" in the
 *     previous item.
 *
 *     The _sl_p_next() macro adds the offset of the "next pointer" to the
 *     address of the this item.
 *
 *   * at the end, assigns the item's "next pointer" to the "next pointer"
 *     field pointed at by p_this.
 *
 *     Note again the cast to (void**).
 *
 * Returns: true  => removed item from list
 *          false => item not found on list (or item == NULL)
 */
extern bool
ssl_del_func(void* p_this, void* item, size_t link_offset)
{
  void* this ;

  if (item == NULL)
    return false ;

  while ((this = *(void**)p_this) != item)
    {
      if (this == NULL)
        return false ;

      p_this = _sl_p_next(this, link_offset) ;
    } ;

  *(void**)p_this = _sl_next(item, link_offset) ;

  return true ;
} ;

/*==============================================================================
 * Single Base, Double Link
 */


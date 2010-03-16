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

#include <list_util.h>


/*==============================================================================
 * Single Base, Single Link
 */

/*------------------------------------------------------------------------------
 * Deleting item
 *
 * Have to chase down list to find item.
 *
 * Returns: 0 => OK -- removed item from list (OR item == NULL)
 *         -1 => item not found on list
 */
extern int
ssl_del_func(void** p_this, void* item, size_t link_offset)
{
  if (item == NULL)
    return 0 ;

  while (*p_this != item)
    {
      if (*p_this == NULL)
        return -1 ;

      p_this = _sl_p_next(*p_this, link_offset) ;
    } ;

  *p_this = _sl_next(item, link_offset) ;

  return 0 ;
} ;

/*==============================================================================
 * Single Base, Double Link
 */


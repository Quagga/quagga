/* Compile time CONFIRM gizmo
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *.
 * This file is part of GNU Zebra.
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

/*==============================================================================
 * Compile time CONFIRM gizmo
 *
 * Two forms:  CONFIRM(e) for use at top (file) level
 *             confirm(e) for use inside compound statements
 */
#ifndef CONFIRM

 #define CONFIRM(e)  extern void CONFIRMATION(char CONFIRM[(e) ? 1 : -1]) ;
 #define confirm(e)  { CONFIRM(e) }

#endif

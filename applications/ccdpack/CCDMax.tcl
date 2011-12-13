proc CCDMax { args } {
#+
#  Name:
#     CCDMax

#  Purpose:
#     Returns maximum value of a list

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     args = list (read)
#        List of numeric values.

#  Copyright:
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#.
   set nargs [llength $args]
   if { $nargs > 1 } {
      set max [lindex $args 0]
      for { set i 1 } { $i < $nargs } { incr i } {
         set v [lindex $args $i]
         if { $v > $max } { set max $v }
      }
      return $max
   } elseif { $nargs == 1 } {
      return $args
   } else {
      return -1
   }

#  End of procedure.
}
# $Id$

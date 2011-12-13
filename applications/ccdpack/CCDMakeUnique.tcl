   proc CCDMakeUnique { Box columns key } {
#+
#  Name:
#     CCDMakeUnique

#  Purpose:
#     Makes unique a column in a scrollbox, multitem & table contents.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine reads in the contents of a scrollbox, multitem or
#     table and removes duplicate entries in the key column. It keeps
#     the entry with highest index in the list (i.e. ones appended
#     later)

#  Arguments:
#     Box = window (read)
#        Name of a scrollbox-like window. Must extract rows using the
#        command "get index".
#     columns = integer (read)
#        The number of columns in $Box
#     key = integer (read)
#        The number of the column to make unique.

#  Global Values:
#     CCDlaterunique
#        If true or non-existent then later unique values (and more
#        importantly their related columns) are kept. Otherwise the
#        earlier unique values are kept.

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
#     22-AUG-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global CCDlaterunique

#.

#  Extract the contents of $Box into an array.
      set n [$Box size]
      for { set i 0 } { $i < $n } { incr i } {
         set line($i) [$Box get $i]
      }

#  Now remove any duplicates.
      if { ! [ info exists CCDlaterunique ] } {
         set CCDlaterunique 1
      }
      if { $CCDlaterunique } {

#  Keep later values.
         for { set i [expr $n -1]} { $i > 0 } { incr i -1} {
            set ikey [lindex $line($i) $key]
            for { set j [expr $i -1] } { $j >= 0 } { incr j -1} {
               set jkey [lindex $line($j) $key]
               if { $ikey == $jkey } {
                  set line($j) {}
               }
            }
         }
      } else {

#  Keep earlier values.
         set m [expr $n -1]
         for { set i 0 } { $i < $m } { incr i } {
            set ikey [lindex $line($i) $key]
            for { set j [expr $i +1] } { $j < $n } { incr j } {
               set jkey [lindex $line($j) $key]
               if { $ikey == $jkey } {
                  set line($j) {}
               }
            }

         }
      }

#  Clear the current contents and insert the unique version.
      $Box clear 0 end
      for { set i 0 } { $i < $n } { incr i } {
         if { $line($i) != {} } { eval $Box insert end $line($i) }
      }

#  End of procedure.
   }
# $Id$

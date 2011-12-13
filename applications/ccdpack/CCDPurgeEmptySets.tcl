   proc CCDPurgeEmptySets { Setbox } {
#+
#  Name:
#     CCDPurgeEmptySets

#  Purpose:
#     Remove redundant Set header lines from Set listbox.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine goes through the listbox which is used to display
#     NDFs marked for Set membership and ensures that any SET header
#     lines which have no members are removed.

#  Arguments:
#     Setbox = string
#        Command name of the listbox which contains Sets of NDFs.
#        Lines will be parsed using the CCDItemSetIndex procedure.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     5-JUL-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Start at the top of the listbox.
      set pos 0

#  Work down lines of listbox.  Some delicacy is required since we may
#  be changing the size of the listbox as we go.
      while { $pos < [$Setbox size] } {

#  If we encounter two adjacent SET header lines, or a SET header line
#  adjacent to the bottom of the list, delete it.
         set pos1 [expr $pos + 1]
         if { [lindex [CCDItemSetIndex $Setbox $pos] 0] == "SET" && \
              ( $pos1 == [$Setbox size] || \
                [lindex [CCDItemSetIndex $Setbox $pos1] 0] == "SET" ) } {
            $Setbox clear $pos
         } else {
            incr pos
         }
      }
   }
# $Id$

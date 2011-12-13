   proc CCDAddSetItems { Unsetbox Setbox items } {
#+
#  Name:
#     CCDAddSetItems

#  Purpose:
#     Marks NDFs for Set header addition by adding to existing Sets.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine takes a list of NDFs and appends them to a position
#     in the existing listbox representing NDFs marked for Set header
#     addition.  It does not put them in a new Set.

#  Arguments:
#     Unsetbox = string
#        The name of the listbox containing all the available NDFs.
#     Setbox = string
#        The name of the listbox which will contain Sets of NDFs.
#     items = list of integers
#        A list of integers (indexes) identifying items from the list in
#        Unsetbox which will consitute a new Set in the Setbox.

#  Global Variables:
#     CCDsetindices = list of integers
#        The NDF Set Index values that we know about.

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

#  Global variables:
      global CCDsetindices

#  See if we have values for CCDsetindices.
      set hassetindices [info exists CCDsetindices]
      if { ! $hassetindices } {
         CCDGetSetIndices $Setbox.getsetindices
      }
      set hassetindices [info exists CCDsetindices]

#  Only proceed if this is not an empty list.
      if { [llength $items] > 0 } {
         set usize [$Unsetbox size]

#  Set the position at which we will insert.
         set point [$Setbox size]

#  See how far back the last SET header line is in the setted listbox.
         set srank ""
         set newset 0
         if { $point > 1 } {
            for { set pos [expr $point -1] } { $srank == "" && $pos >= 0 } \
                { incr pos -1 } {
               if { [lindex [CCDItemSetIndex $Setbox $pos] 0] == "SET" } {
                  set srank [expr $point - $pos - 1]
               }
            }
         }

#  If it's further back than the size of a Set, or there is no preceding
#  SET header line, we'll have to start a new Set here.
         if { $srank == "" || \
              $hassetindices && $srank >= [llength $CCDsetindices] } {
            set newset 1
         }

#  Now loop over each of the items to insert.
         foreach item $items {

#  If starting a new Set, insert a SET header line and bump the insertion
#  point.
            if { $newset } {
               set newset 0
               set srank 0
               $Setbox insert $point ""
               CCDItemSetIndex $Setbox $point "SET"
               incr point
            }

#  Get the Set Index.
            if { $hassetindices && $srank < [llength $CCDsetindices] } {
               set sindex [lindex $CCDsetindices $srank]
            } else {
               set sindex [expr $srank + 1]
            }

#  Annotate with the Set Index in the unsetted list.
            set olditem [CCDItemSetIndex $Unsetbox $item $sindex]

#  If it was previously in a Set, remove its old appearance in the
#  setted list.
            if { [regexp {^-?[0-9]*$} [lindex $olditem 0]] } {
               set pos 0
               while { $pos < [$Setbox size] } {
                  if { [CCDItemSetIndex $Setbox $pos] == $olditem } {
                     $Setbox clear $pos
                     if { $pos <= $point } {
                        incr point -1
                     }
                  } else {
                     incr pos
                  }
               }
            }

#  Add the new item to the setted list.
            $Setbox insert $point [$Unsetbox get $item]

#  Bump the insertion position.
            incr point

#  Bump the set rank.
            incr srank
            if { $hassetindices && $srank >= [llength $CCDsetindices] } {
               set newset 1
            }
         }

#  Tidy up any empty Sets we might have left lying around.
         CCDPurgeEmptySets $Setbox

#  Adjust the position of the scroll box so we can see the items just
#  appended.
         $Setbox vmoveto 1
      }
   }
# $Id$

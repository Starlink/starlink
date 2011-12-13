   proc CCDNewSet { Unsetbox Setbox items } {
#+
#  Name:
#     CCDNewSet

#  Purpose:
#     Marks NDFs for Set header addition.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine gets a list of NDFs and marks them for Set header
#     addition.  It basically does the following:
#        - enters them into the setted listbox as a complete Set
#        - marks the names in the unsetted listbox with their Set indices

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
#     18-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDsetindices

#  See if we have values for CCDsetindices.
      set hassetindices [info exists CCDsetindices]

#  Only proceed if this is not an empty list.
      if { [llength $items] > 0 } {
         set usize [$Unsetbox size]

#  Create a new Set item at the end of setlist.
         $Setbox insert end ""
         CCDItemSetIndex $Setbox end "SET"

#  Loop over each of the new items.
         set srank 0
         foreach item $items {

#  Check this item is in the range of the unsetted box.
            if { $item >= 0 && $item < $usize } {

#  Set the Set Index value; it should be one of the values in the
#  CCDsetindices global if that exists.  If it's outside that range
#  the user is asking for trouble anyway; just make something up.
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
                     } else {
                        incr pos
                     }
                  }
               }

#  Add a new item to the setted list.
               $Setbox insert end [$Unsetbox get $item]
               incr srank
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

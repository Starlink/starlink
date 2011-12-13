proc CCDUpdateFactorLists { element Table } {

#+
#  Name:
#     CCDUpdateFactorLists

#  Purpose:
#     Updates the exposure factors for flash and dark frames.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine records the exposure times of dark and flash frames
#     in the appropriate elements of the global array CCDndfs. The times
#     used are assumed to be stored (together with the file names) in
#     a Table widget (or multitem). The number of columns determines
#     the actions (a single time is assumed to be that associated
#     with the given frame type).
#
#     If the data are flash frames then it is assumed that they have a
#     flash exposure time (which is 1 if CCDsame(flashes) is true) and
#     a dark time (which defaults to 0 if unless set).
#
#     If the data are dark frames then it is assumed that they have a
#     dark exposure time (which is 1 if CCDsame(darks) is true) and
#     have a flash time of 0.

#  Arguments:
#     element = string (read)
#        The name of the element of the global array CCDndfs which will
#         contain the (Tcl) list of NDF names returned from the user.
#     Table = window (read)
#        The name of the Table (which may be a multitem) which
#        contains the information to be updated in the global arrays.

#  Global Variables:
#      CCDndfs = array (write)
#         An array of lists of NDF names. The NDFs selected by this
#          procedure are written to element CCDndfs($element).
#      CCDfactors = array (write)
#         If required this array will contain lists of exposure
#         factors for dark counts or pre-flash. The indices are
#         ($element,darks) and ($element,flashes) if used.
#      CCDsame = array (read)
#         This array's elements are (darks) and (flashes) if these are
#         false then values for the dark and/or pre-flash time are
#         assumed to be present.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
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
#     31-MAY-1994 (PDRAPER):
#        Original version.
#     4-SEP-1995 (PDRAPER):
#        Updated to new working methods.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global CCDndfs
   global CCDsame
   global CCDfactors
#.

#  Unset the global variables before update.
   if { [info exists CCDndfs($element)] } {
      unset CCDndfs($element)
   }
   if { [info exists CCDfactors($element,darks)] } {
      unset CCDfactors($element,darks)
   }
   if { [info exists CCDfactors($element,flashes)] } {
      unset CCDfactors($element,flashes)
   }

#  For each entry obtain its value(s) and insert into the global arrays.
   set nvalues [$Table size]
   for { set i 0 } { $i < $nvalues } { incr i } {
      set item [$Table get $i]
      lappend CCDndfs($element) [lindex $item 0]
      set index 1
      if { $element == "darks" } {

#  Darks only have a dark time flash time is always zero.
         if { ! $CCDsame(darks) } {
            lappend CCDfactors($element,darks) [lindex $item 1]
         } else {
            lappend CCDfactors($element,darks) 1
         }
         lappend CCDfactors($element,flashes) 0
      } else {

#  Flashes. Darks times are 0 (unlikely to have much dark time in flashes)
         if { ! $CCDsame(flashes) } {
            lappend CCDfactors($element,flashes) [lindex $item 1]
         } else {
            lappend CCDfactors($element,flashes) 1
         }
         lappend CCDfactors($element,darks) 0
      }
   }

#  return the number of values obtained.
   return $nvalues

#  End of procedure.
}
# $Id$

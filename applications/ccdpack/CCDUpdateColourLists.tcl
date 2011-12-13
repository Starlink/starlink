proc CCDUpdateColourLists { element scrollbox filter } {

#+
#  Name:
#     CCDUpdateColourLists

#  Purpose:
#     Updates the colour related NDF global lists.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine is used to update the associated global lists and
#     arrays of NDF names and exposure factors. If extracts all the
#     values from a named scrollbox or table and enters the values in
#     the appropriate parts of the CCDndfs global array.

#  Arguments:
#     element = string (read)
#        The name of the element of the global array CCDndfs which will
#        contain the (Tcl) list of NDF names returned from the user.
#        This is also indexed by the filter type.
#     scrollbox = window (read)
#        The name of the scrollbox (which may be a multitem) which
#        contains the information to be updated in the global arrays.
#     filter = string (read)
#        Name of the current filter.

#  Global Variables:
#      CCDndfs = array (write)
#         An array of lists of NDF names. The NDFs selected by this
#         procedure are written to element CCDndfs($element,$filter).
#      CCDfactors = array (write)
#         If required this array will contain lists of exposure
#         factors for dark counts or pre-flash. The indices are
#         ($element,$filter,darks) and ($element,$filter,flashes) if used.
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
#     17-JUL-1995 (PDRAPER):
#        Added filter as an argument (changes to whole interface
#        underway at this time).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global CCDndfs
   global CCDsame
   global CCDfactors
#.

#  Find out how many entries are in the named scrollbox/multitem.
   set nvalues [$scrollbox size]

#  Unset the global variables before update.
   catch {
      unset CCDndfs($element,$filter)
      if { ! $CCDsame(darks) } {
         unset CCDfactors($element,$filter,darks)
      }
      if { ! $CCDsame(flashes) } {
         unset CCDfactors($element,$filter,flashes)
      }
   }

#  For each entry obtain its value(s) and insert into the global arrays.
   for { set i 0 } { $i < $nvalues } { incr i } {
      set index 1
      set item [ $scrollbox get $i ]
      lappend CCDndfs($element,$filter) [lindex $item 0]
      if { ! $CCDsame(darks) } {
         lappend CCDfactors($element,$filter,darks) \
            [lindex $item $index]
         incr index
      }
      if { ! $CCDsame(flashes) } {
         lappend CCDfactors($element,$filter,flashes) \
            [lindex $item $index]
      }
   }

#  Return the number of values entered.
   return $nvalues

#  End of procedure.
}
# $Id$

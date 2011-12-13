   proc CCDCreateListofNames { frombox prefix postfix } {
#+
#  Name:
#     CCDCreateListofNames

#  Purpose:
#     To create a list of the names entered in a listbox.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine creates a Tcl list which contains all the entries in
#     a listbox. The names extracted from the listbox are modified using
#     the values of the prefix and postfix arguments (which may be
#     empty).

#  Arguments:
#     frombox = window (read)
#        The name of the listbox whose contents are to be copied into a
#        list.
#     prefix = string (read)
#        A string to prefix to the names extracted from the listbox
#        before entry into the list.
#     postfix = string (read)
#        A string to be appended to the names extracted from the listbox
#        before entry into the list

#  Returned Value:
#     CCDCreateListofNames = list (write)
#        The list of names extracted from the listbox. Null if none are
#         found.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council. All
#     Rights Reserved.

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
#     9-MAR-1994 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
#.

#  Initialise the output list.
      set outlist {}

#  Check the size of the listbox.
      set size [$frombox size]
      if { $size > 0 } {

#  Have some entries. Loop until all are read.
         for { set i 0 } { $i < $size } { incr i } {
            set name "${prefix}[$frombox get $i]${postfix}"
            lappend outlist $name
	 }
      }

#  End of procedure.
      return $outlist
   }
# $Id$

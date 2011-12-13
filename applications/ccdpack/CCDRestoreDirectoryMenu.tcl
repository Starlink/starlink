   proc CCDRestoreDirectoryMenu { menubar menu entry choicebar button } {
#+
#  Name:
#     CCDRestoreDirectoryMenu

#  Purpose:
#     Restores directories which have been visited to menu.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure creates commands in a menu (which is part of a
#     menubar) which will return to a list of directories which have
#     been visited previously. It is used to restore directories entered
#     by the CCDRecordDirectoryinMenu procedure.

#  Arguments:
#     menubar = window (read)
#        The name of (CCDPACK) menubar which is to have the commands
#        added to one of its menus.
#     menu = string (read)
#        The name of the menubar menu which is to have the commands
#        added.
#     entry = window (read)
#        The name of a labelled entry widget to receive the name of the
#        directory when the menu command is invoked.
#     choicebar = window (read)
#        The name of the choice bar which contains the button as a named
#        option.
#     button = window (read)
#        The name of a button in a choice bar which when invoked
#        will perform the actual change of directory.

#  Global Variables:
#     CCDmenudirs$menu() = array (write)
#        The list of directories previously associated with the menu.
#        These are indexed by the directory names.

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
#     20-APR-1994 (PDRAPER):
#        {changes}
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDmenudirs${menu}

#.


#  Look at each of the directories in turn.
      if { [ info exists CCDmenudirs${menu} ] } {
         if { [ array size CCDmenudirs${menu} ] != 0 } {
            foreach directory [array names CCDmenudirs${menu}] {
               $menubar addcommand $menu \
                  "cd $directory" \
                  "$entry clear 0 end
                   $entry insert 0 $directory
                   $choicebar invoke $button
                  "
            }
	 }
      }

#  End of procedure
   }
# $Id$

   proc CCDRemoveFromList { listbox args } {
#+
#  Name:
#      CCDRemoveFromList

#  Purpose:
#      Deletes the current selection from a listbox.

#  Language:
#     TCL

#  Parameters:
#      listbox = window (read)
#        The name of the listbox which should be used.
#      args = string (read)
#        If defined this specifies the command to use when deleting the
#        items from the listbox (which may be a scrollbox or variant in
#        which case "clear" should be used instead of "delete").

#  Copyright:
#     Copyright (C) 1993-1994 Science & Engineering Research Council.
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
#      PDRAPER: Peter Draper (Starlink - Durham University)
#      {enter_new_authors}

#  History:
#      31-AUG-1993 (PDRAPER):
#         Original version.
#      2-MAR-1994 (PDRAPER):
#         Modification of routine to delete NDFs from multiple lists.
#      3-MAR-1994 (PDRAPER):
#         Added check for listbox name.
#      8-MAR-1994 (PDRAPER):
#         Now uses the selection in listbox, does not assume that
#         this is the X11 selection.
#      {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#.

#  Decide on the command to delete the items from the widget.
      if { $args != {} } {
         set command $args
      } {
         set command delete
      }

#  Try to get a selection from the listbox.
      set indices [$listbox curselection]
      if { $indices != {} } {

#  Ok now delete each one. Need to start high and go low as indices change
#  for each modification
         foreach index [lsort -integer -decreasing $indices] {
            $listbox $command $index
         }
      }

#  End of procedure.
   }
# $Id$

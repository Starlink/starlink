   proc CCDRemoveFromList { listbox args } {
#+
#   Name:
#      CCDRemoveFromList

#   Purpose:
#      Deletes the current selection from a listbox.

#   Parameters:
#      listbox = window (read)
#        The name of the listbox which should be used. 
#      args = string (read)
#        If defined this specifies the command to use when deleting the
#	 items from the listbox (which may be a scrollbox or variant in
#	 which case "clear" should be used instead of "delete").

#   Authors:
#      PDRAPER: Peter Draper (Starlink - Durham University)
#      {enter_new_authors}

#   History:
#      31-AUG-1993 (PDRAPER):
#         Original version.
#      2-MAR-1994 (PDRAPER):
#      	  Modification of routine to delete NDFs from multiple lists.
#      3-MAR-1994 (PDRAPER):
#      	  Added check for listbox name.
#      8-MAR-1994 (PDRAPER):
#         Now uses the selection in listbox, does not assume that 
#         this is the X11 selection.
#      {enter_further_changes_here}

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

   proc CCDRestoreDirectoryMenu { menubar menu entry choicebar button } {
#+
#  Name:
#     CCDRestoreDirectoryMenu

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Restores directories which have been visited to menu.

#  Description:
#     This procedure creates commands in a menu (which is part of a
#     menubar) which will return to a list of directories which have
#     been visited previously. It is used to restore directories entered
#     by the CCDRecordDirectoryinMenu procedure.

#  Arguments:
#     menubar = window (read)
#        The name of (CCDPACK) menubar which is to have the commands
#	 added to one of its menus.
#     menu = string (read)
#        The name of the menubar menu which is to have the commands
#	 added.
#     entry = window (read)
#        The name of a labelled entry widget to receive the name of the
#	 directory when the menu command is invoked.
#     choicebar = window (read)
#        The name of the choice bar which contains the button as a named
#	 option.
#     button = window (read)
#	 The name of a button in a choice bar which when invoked
#	 will perform the actual change of directory.

#  Global variables:
#     CCDmenudirs$menu() = array (write)
#        The list of directories previously associated with the menu.
#	 These are indexed by the directory names.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     20-APR-1994 (PDRAPER):
#     	 {changes}
#     {enter_further_changes_here}

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

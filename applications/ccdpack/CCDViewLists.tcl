   proc CCDViewLists { Top description args } {
#+
#  Name:
#     CCDViewLists

#  Purpose:
#     Display a series of lists in an scrollable listbox.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine displays lists from the named global variables in a
#     scrollbale listbox, each list element being an listbox item.
#     If the global variables are arrays then each array element is
#     dealt with as a new list. The "args" variable to this procedure
#     has all the names of the global variables to be displayed.

#  Arguments:
#     Top = window (read)
#        The name of the top-level object to create for displaying the lists.
#     description = string (read)
#         A description for the title bar of the window.
#     args = list (read)
#        The names of the global variables whose contents are to be displayed.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     1-JUN-1994 (PDRAPER):
#        Original version.
#     11-MAY-1995 (PDRAPER):
#        Updated to Tk4.0 (array size now returns 0, rather than an
#        error).
#     24-AUG-1995 (PDRAPER:
#        Converted to new style.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#.

#  Check that we have some variables to list.
      if { $args != {} } { 

#-----------------------------------------------------------------------------
#  Widget creation
#-----------------------------------------------------------------------------
#  Top-level widget to contain the list.
         Ccd_toplevel $Top -title "$description"

#  Scrollable listbox for the contents.
         set Box [Ccd_scrollbox $Top.scrollbox]

#  Choice bar for window control
         set Choice [Ccd_choice $Top.choice -standard 0]

#-----------------------------------------------------------------------------
#  Widget configuration.
#-----------------------------------------------------------------------------
#  Choice.
#  Add a button to exit this display.
         $Choice addbutton {OK} "$Top kill $Top"

#-----------------------------------------------------------------------------
#  Packing
#-----------------------------------------------------------------------------
         pack $Choice -side bottom -fill x
         pack $Box -fill both -expand true

#-----------------------------------------------------------------------------
#  Fill the listbox with the names 
#-----------------------------------------------------------------------------
#  Loop for all items in the args list.
	 foreach gvar $args {

#  "Declare" the global variable.
	    global $gvar
            if { [info exists $gvar] } {
            
#  Check if this is an array.
               if { [array size $gvar] == 0  } { 

#  Isn't an array, just enter the list.
                  eval $Box insert end [eval split $$gvar]
               } else { 

#  Is an array process each index in turn as a list.
                  foreach index [ array names $gvar ] { 
                     eval $Box insert end [ eval split $$gvar($index) ]
                  }
               }
	    }
	 }
      } else {
         CCDIssueInfo "No lists exist"
      } 

#  End of procedure.
   }
# $Id$

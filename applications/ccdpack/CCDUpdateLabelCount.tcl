   proc CCDUpdateLabelCount { Reveal name Box } {
#+
#  Name:
#     CCDUpdateLabelCount

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Updates a named button of a Ccd_reveal widget to reflect the count
#     of entries in a listbox-like widget.

#  Description:
#     This routine is for updating the text description of one of a set
#     of buttons created by the Ccd_reveal widget. The text is updated to 
#     show a count of the number of entries in the associated window 
#     (which should be a listbox-like widget, such as a scrollbox or 
#     table). This is used by the CCDNDFDoImport procedure.

#  Arguments:
#     Reveal = window (read)
#       The name of the Ccd_reveal widget whose button labels are to be 
#       changed to reflect the contents of an associated listbox-like 
#       widget.
#     name = string (read)
#       The name of the button (i.e. its creation name). The new name
#       will be this name followed by a count of the contents in brackets.
#     Box = window (read)
#       Name of the scrollbox-like widget associated with name.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-AUG-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.

#.

#  Get the number of entries in the listbox-like widget.
      if { [winfo exists $Reveal] } { 
         if { [winfo exists $Box] } { 
            set size [$Box size]
            $Reveal resettext $name "$name ($size)"
         }
      }

#  End of procedure.
   }
# $Id$

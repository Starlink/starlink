   proc CCDDialog {Top title text bitmap} {
#+
#  Name:
#     CCDDialog

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Creates a dialog box.

#  Description:
#     Creates a modal dialog box for issuing warnings. The resultant
#     top level widget contains descriptive text an optional bitmap
#     and a button for acknowledgment.

#  Arguments:
#     Top = window (read)
#        The name of the top-level widget created by this routine.
#     title = string (read)
#        A title for the window decorations.
#     text = string (read)
#        The description of the error or warning.
#     bitmap = string (read)
#        The name of a bitmap to display on the left. If blank this is
#        not used.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-JUL-1995 (PDRAPER):
#     	 Original version.
#     23-AUG-1995 (PDRAPER):
#        Added code to centre in parent top-level or screen.
#     {enter_further_changes_here}

#-

#  Global variables.

#.

#  Widget creation.
      Ccd_toplevel $Top -title "$title"
      set Frame [frame $Top.f]
      if { $bitmap != "" } { 
         set Bitmap [label $Frame.label -bitmap $bitmap]
      }
      set Message [message $Frame.message \
                      -text "$text" -width 14c -justify center]
      set Button [Ccd_choice $Top.button -standard 0]

#  Configure widgets.
      $Button addbutton OK "$Top kill $Top"

#  Pack widgets.
      pack $Button -side bottom -fill x
      pack $Frame -side top -fill both -expand true      
      if { $bitmap != "" } { 
         pack $Bitmap -side left -fill y -padx 0.5c -pady 0.5c
      }
      pack $Message -side right -fill both -expand true

#  Make sure this window is on top and reasonable prominent 
#  (centre of screen of parent top-level).
      wm withdraw $Top
      update idletasks
      set Wtop [winfo toplevel $Top]
      if { $Wtop != $Top } {
         set x [expr [winfo rootx $Wtop] + [winfo reqwidth $Wtop]/2 \
                   -[winfo reqwidth $Top]/2]
         set y [expr [winfo rooty $Wtop] + [winfo reqheight $Wtop]/2 \
                   -[winfo reqheight $Top]/2]
      } else {
         set x [expr [winfo screenwidth $Top]/2 - [winfo reqwidth $Top]/2]
         set y [expr [winfo screenheight $Top]/2 - [winfo reqheight $Top]/2]
      }
      wm geometry $Top +$x+$y
      wm deiconify $Top

#  Try to make sure this window stays on Top.
      raise $Top
      bind $Top <Visibility> "raise $Top"

#  Make OK the focus
      $Button focus OK

#  Wait for the acknowledgement.
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$

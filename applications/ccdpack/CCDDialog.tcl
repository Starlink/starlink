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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-JUL-1995 (PDRAPER):
#     	 Original version.
#     23-AUG-1995 (PDRAPER):
#        Added code to centre in parent top-level or screen.
#     13-MAY-1999 (PDRAPER):
#        Changed window control policy to just transient (explicit
#        raising causes problems with some WMs).
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     {enter_further_changes_here}

#-

#  Global variables.

#.

#  Widget creation.
      CCDCcdWidget Topwin topwin Ccd_toplevel $Top -title "$title"
      CCDTkWidget Frame frame frame $topwin.f
      if { $bitmap != "" } {
         CCDTkWidget Bitmapwin bitmapwin label $frame.label -bitmap $bitmap
      }
      CCDTkWidget Message message \
         message $frame.message -text "$text" -width 14c -justify center
      CCDCcdWidget Button button Ccd_choice $Topwin.button -standard 0

#  Configure widgets.
      $Button addbutton OK "$Topwin kill $Topwin"

#  Pack widgets.
      pack $button -side bottom -fill x
      pack $frame -side top -fill both -expand true
      if { $bitmap != "" } {
         pack $bitmapwin -side left -fill y -padx 0.5c -pady 0.5c
      }
      pack $message -side right -fill both -expand true

#  Make sure this window is on top and reasonable prominent
#  (centre of screen of parent top-level).
      wm withdraw $topwin
      update idletasks
      set wtop [winfo toplevel $topwin]
      if { $wtop != $topwin } {
         set x [expr [winfo rootx $wtop] + [winfo reqwidth $wtop]/2 \
                   -[winfo reqwidth $topwin]/2]
         set y [expr [winfo rooty $wtop] + [winfo reqheight $wtop]/2 \
                   -[winfo reqheight $topwin]/2]
      } else {
         set x [expr [winfo screenwidth $topwin]/2 - [winfo reqwidth $topwin]/2]
         set y [expr [winfo screenheight $topwin]/2 - [winfo reqheight $topwin]/2]
      }
      wm geometry $topwin +$x+$y
      wm deiconify $topwin

#  Try to make sure this window stays on Top.
      wm transient $topwin [winfo parent $topwin]

#  Make OK the focus
      $Button focus OK

#  Wait for the acknowledgement.
      CCDWindowWait $Topwin

#  End of procedure.
   }
# $Id$

proc CCDExit {} {

#+
#  Name:
#     CCDExit

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Exits the CCDPACK GUI.

#  Description:
#     This routine offers to close down the GUI and performs a clean
#     exit if requested.

#  Global parameters:
#     MAIN = array (read)
#       MAIN(window) = name of the main top-level window.
#       MAIN(name) = name of application.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     7-NOV-1995 (PDRAPER):
#        Original version.
#     13-MAY-1999 (PDRAPER):
#        Changed window control policy to just transient (explicit
#        raising causes problems with some WMs).
#     {enter_changes_here}

#-

#  Global parameters:
   global MAIN
#.

   if { [winfo exists $MAIN(window)] } { 

#  Widget creation.
      set Top [Ccd_toplevel .exit -title "Exit from $MAIN(name)"]
      set Line1 [frame $Top.f1 -height 3]
      set Message [label $Top.message \
                      -text "Are you sure you want to exit from $MAIN(name)" \
                      -borderwidth 3 -padx 10 -pady 10]
      set Bitmap [label $Top.bitmap -bitmap questhead]
      set Line2 [frame $Top.f2 -height 3]
      set Choice [Ccd_choice $Top.button -standard 0]
               
#  Configure widgets.
      $Choice addbutton Yes \
         "$Top kill $Top;$MAIN(window) kill $MAIN(window); destroy ."
      $Choice addbutton No  "$Top kill $Top"

#  Pack widgets.
      pack $Choice -side bottom -fill x
      pack $Line1 -side top -fill x
      pack $Message -side left -fill both -expand true
      pack $Bitmap -side right
      pack $Line2 -side top -fill x
      
#  Make sure this window is on top and reasonable prominent 
#  (centre of screen of parent top-level).
      wm withdraw $Top
      update idletasks
      set x [expr [winfo screenwidth $Top]/2 - [winfo reqwidth $Top]/2]
      set y [expr [winfo screenheight $Top]/2 - [winfo reqheight $Top]/2]
      wm geometry $Top +$x+$y
      wm deiconify $Top

#  Try to make sure this window stays on Top.
      wm transient $Top .topwin

#  Yes is default.
      $Choice focus Yes
      
#  Wait for the acknowledgement.
      CCDWindowWait $Top
   }
#  End of procedure.
}
# $Id$

   proc CCDGetSetIndices { Topwin } {
#+
#  Name:
#     CCDGetSetIndices

#  Purpose:
#     Get the list of Set Index values we will deal with from the user.

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine asks the user what size CCDPACK Sets should be made,
#     so that the auto-Setting routine can make Sets of the right size.

#  Arguments:
#     Topwin = window (read)
#        The command name of a top-level widget to contain this form.

#  Global Variables:
#     CCDsetindices = list of integers (read and write)
#        The NDF Set Index values got from the user.  If the user 
#        gives an invalid response or invokes a cancel on the dialog,
#        an empty list will be returned.
#     CCDsetindicesvalid = boolean (write)
#        True if the user wishes to use the value of CCDsetindices.

#  Notes:
#     This widget doesn't currently allow the user to specify his own
#     Set Index values (it assumes 1, 2, 3,...).  I don't think this 
#     will often be required, but if it is this routine could be 
#     ehnanced.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     21-JUN-2001 (MBT):
#        Original version.

#-

#  Global variables.
      global CCDsetindices
      global CCDsetindicesvalid
      global setsize

#  Initialise set indices variable list if it does not already exist.
      if { ! [info exists CCDsetindices] } { 
         set CCDsetindices {} 
      }
      set setsize [llength $CCDsetindices]

#  Create the widget components.
      CCDCcdWidget Top top Ccd_toplevel $Topwin -title "Size of each Set"
      CCDTkWidget Frame frame frame $top.center
      CCDTkWidget Value value scale $frame.slider \
         -variable setsize \
         -orient horizontal \
         -from 0 -to 9 \
         -label "Number of NDFs per Set" \
         -showvalue 1 \
         -tickinterval 1 \
         -length 256
      CCDCcdWidget Menu menu Ccd_helpmenubar $Top.menubar
      CCDCcdWidget Choice choice Ccd_choice $Top.choice -standard 1

#  Configure the choice buttons.
      $Choice addcommand OK "
         if { \[llength \$CCDsetindices\] != \$setsize } {
            set CCDsetindices {}
            for { set i 1 } { \$i <= \$setsize } { incr i } {
               lappend CCDsetindices \$i
            }
         }
         set CCDsetindicesvalid 1
         $Top kill $Top
      "
      $Choice addcommand Cancel "
         set CCDsetindicesvalid 0
         $Top kill $Top
      "

#  Set window acceptance as the default button if the current value of 
#  CCDsetindices is sensible.
      if { [llength $CCDsetindices] > 1 } {
         $Choice focus OK
      }

#  Add menu options.
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Exit} CCDExit

#  Pack the widget.
      pack $menu -fill x -side top
      pack $frame -fill both -expand 1
      pack $value -fill x -expand 1
      pack $choice -side bottom -fill x 

#  Position the window.
      wm withdraw $top
      CCDCentreWindow $Top [winfo parent $top]
      wm deiconify $top

#  Wait for interaction to finish.
      CCDWindowWait $Top
   }
# $Id$

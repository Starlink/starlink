proc CCDGetColour { Top element } {
#+
#  Name:
#

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Allows the selection of a colour from a range.

#  Description:
#     This routine displays a list of known colours (from the X11
#     rgb.txt file) and allows uses three sliders to allows a colour
#     to be selected from RBG space.

#  Arguments:
#     Top = window (read)
#        A name for the top-level widget created by this routine.
#        This routine will wait until the interaction is finished
#        before proceeding.
#     element = string (read)
#        The name of the element of the global array CCDprefs to receive 
#        the chosen colour.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
   global CCDprefs
   global RGB

#.

#  Get a known rgb file.
   set rgbfile ""
   foreach oneof {/usr/lib/X11/rgb.txt /usr/openwin/lib/X11/rgb.txt} {
      if { [file readable $oneof] } {
         set rgbfile $oneof
         break
      }
   }
   if { $rgbfile == "" } {
      set uselist 0
   } else {
      set uselist 1
   }

#  Default colour blue.
   if { ![ info exists CCDprefs($element)] } {
      set CCDprefs($element) blue
   }

#-----------------------------------------------------------------------------
#  Widget creation.
#-----------------------------------------------------------------------------
   Ccd_toplevel $Top -title "Choose a colour"
   if { $uselist } {
      set List [Ccd_scrollbox $Top.list -label "Colour List:"]
   }
   set Frame1 [frame $Top.frame1 -borderwidth 0]
   set Rslide [scale $Frame1.red -label "Red" -showvalue 0 \
                  -to 65535 -orient horizontal -variable RGB(red)]
   set Gslide [scale $Frame1.green -label "Green" -showvalue 0 \
                  -to 65535  -orient horizontal -variable RGB(green)]
   set Bslide [scale $Frame1.blue -label "Blue" -showvalue 0 \
                  -to 65535 -orient horizontal -variable RGB(blue)]
   set Choice [Ccd_choice $Top.choice]
   set Frame2 [frame $Top.frame2 -borderwidth 0]
   set Thislabel [label $Frame2.label -text "Colour:              "]
   set Thiscolour [frame $Frame2.colour]

#-----------------------------------------------------------------------------
#  Widget configuration.
#-----------------------------------------------------------------------------

#  Get out options.
   $Choice addcommand OK \
      "global CCDprefs
       set CCDprefs($element) \[$Thiscolour cget -background\]
       $Top kill $Top
      "
   $Choice addcommand Cancel "$Top kill $Top"

#  Bind <1> to choose a colour from the list.
   if { $uselist } {
      $List bind list <ButtonPress-1> \
         {
            global RGB
            set index [%W nearest %y]
            set colour [%W get $index]
            set rgblist [winfo rgb %W $colour]
            set RGB(red) [lindex $rgblist 0]
            set RGB(green) [lindex $rgblist 1]
            set RGB(blue) [lindex $rgblist 2]
         }
      $List bind list <ButtonRelease-1> \
         {
            global RGB
            set index [%W nearest %y]
            set colour [%W get $index]
            set rgblist [winfo rgb %W $colour]
            set RGB(red) [lindex $rgblist 0]
            set RGB(green) [lindex $rgblist 1]
            set RGB(blue) [lindex $rgblist 2]
         }
   }

#-----------------------------------------------------------------------------
#  Packing.
#-----------------------------------------------------------------------------
   pack $Choice -side bottom -fill x
   if { $uselist } { pack $List -side left -fill y }
   pack $Frame1 -side left -fill both
   pack $Frame2 -side left -fill both -expand true

   pack $Thislabel -fill x -side top
   pack $Thiscolour -fill both -expand true

   pack $Rslide $Gslide $Bslide -side top -fill x

#-----------------------------------------------------------------------------
#  Activation.
#-----------------------------------------------------------------------------

#  Fill list with known colours.
   if { $uselist } {
      foreach f $rgbfile {
         set id [open $f]
         while {[gets $id line] >= 0} {
            if {[llength $line] == 4} {
               $List insert end [lindex $line 3]
            }
         }
         close $id
      }
   }

#  Set the display to the current colour.
   $Thiscolour configure -background $CCDprefs($element)
   set rgblist [winfo rgb $Thiscolour $CCDprefs($element)]
   set RGB(red) [lindex $rgblist 0]
   set RGB(green) [lindex $rgblist 1]
   set RGB(blue) [lindex $rgblist 2]

#  Trace changes to RGB
   trace variable RGB w CCDChangeColour
   proc CCDChangeColour { args } "
      global RGB
      set newcolour \
         \[format \"\#%04x%04x%04x\" \$RGB(red) \$RGB(green) \$RGB(blue)\]
      $Thiscolour configure -background \$newcolour
   "

#  Wait for interaction to end.
   CCDWindowWait $Top

#  End of procedure.
}
#  $Id$

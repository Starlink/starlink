proc CCDGetColour { Topwin element } {
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
#     Topwin = window (read)
#        A name for the top-level widget created by this routine.
#        This routine will wait until the interaction is finished
#        before proceeding.
#     element = string (read)
#        The name of the element of the global array CCDprefs to receive 
#        the chosen colour.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     26-SEP-1995 (PDRAPER):
#     	 Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#-

#  Global variables.
   global CCDprefs
   global RGB

#.
   set known_databases "/usr/lib/X11/rgb.txt /usr/openwin/lib/X11/rgb.txt \
/usr/X11R6/lib/X11/rgb.txt"


#  Get a known rgb file.
   set rgbfile ""
   foreach oneof $known_databases {
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
   CCDCcdWidget Top top Ccd::toplevel $Topwin -title "Choose a colour"
   if { $uselist } {
      CCDCcdWidget List list Ccd::scrollbox $Top.list -label "Colour list:"
   }
   CCDTkWidget Frame1 frame1 frame $top.frame1 -borderwidth 0
   CCDTkWidget Rslide rslide \
      scale $frame1.red -label "Red" -showvalue 0 \
                  -to 65535 -orient horizontal -variable RGB(red)
   CCDTkWidget Gslide gslide \
      scale $frame1.green -label "Green" -showvalue 0 \
                  -to 65535  -orient horizontal -variable RGB(green)
   CCDTkWidget Bslide bslide \
      scale $frame1.blue -label "Blue" -showvalue 0 \
                  -to 65535 -orient horizontal -variable RGB(blue)
   CCDCcdWidget Choice choice Ccd::choice $Top.choice
   CCDTkWidget Frame2 frame2 frame $top.frame2 -borderwidth 0
   CCDTkWidget Thislabel thislabel \
      label $frame2.label -text "Colour:              "
   CCDTkWidget Thiscolour thiscolour frame $frame2.colour

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
   pack $choice -side bottom -fill x
   if { $uselist } { pack $list -side left -fill y }
   pack $frame1 -side left -fill both
   pack $frame2 -side left -fill both -expand true

   pack $thislabel -fill x -side top
   pack $thiscolour -fill both -expand true

   pack $rslide $gslide $bslide -side top -fill x

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
   set rgblist [winfo rgb $thiscolour $CCDprefs($element)]
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

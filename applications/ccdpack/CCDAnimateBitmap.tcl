   proc CCDAnimateBitmap { Window args } {
#+
#  Name:
#     CCDAnimateBitmap

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Animate a bitmap holding widget.

#  Description:
#     Control the animatation of a series of bitmaps in a given window. This
#     is achieved by changing the bitmap to one of those given in
#     a global array indexed by incrementing numbers, starting at 1.
#     The animatation may be stopped by calling this routine with a
#     single argument "stop".

#  Arguments:
#     Window = window (read)
#        Name of the window whose bitmap is to be animated, must be
#        configurable by the command "-bitmap".
#     args = list (read)
#        Either:
#          the name of a global array and its size (i.e. highest index).
#        Or:
#          "stop", to terminate the animation.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-AUG-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables:
      global Animbit
#.

#  Set run status to tell AnimBitTimer whether to stop for this window or
#  not.
      if { "$args" == "stop" } {
         set Animbit($Window,run) 0
      } else {

#  Enable AnimTimer to run.
         set Animbit($Window,run) 1

#  Extra the name of the global variable and its size.
         set gvar [lindex $args 0]
         set size [lindex $args 1]
         set nowat 1

#  Run the animation procedure.
         AnimBitTimer $Window $gvar $size $nowat
      }
   }

#  Internal procedure for doing the real work.
   proc AnimBitTimer {Window gvar size nowat} {
      global Animbit
      global $gvar

#  Only proceed if allowed.
      if { $Animbit($Window,run) } {
         if { $nowat > $size } { set nowat 1 }
         eval $Window configure -bitmap @\$$gvar\(\$nowat\)
         incr nowat
         after 250 AnimBitTimer $Window $gvar $size $nowat
      }
   }
# $Id$

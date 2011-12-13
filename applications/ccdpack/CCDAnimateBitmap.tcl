   proc CCDAnimateBitmap { Window args } {
#+
#  Name:
#     CCDAnimateBitmap

#  Purpose:
#     Animate a bitmap holding widget.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

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

#  Copyright:
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-AUG-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

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

#!/bin/csh
#+
#  Name:
#     pazo

#  Purpose:
#     Allows simple panning and zooming without using IDPAZO.

#  Language:
#     Unix C-shell

#  Invocation:
#     pazo [-d device]

#  Description:
#     This script allows the user to explore a displayed NDF by displaying
#     a series of sections from the NDF, selected using the cursor.
#
#     Clicking the left mouse button twice over the displayed image defines
#     a rectangular sub-section of the image to be re-displayed. This may be
#     done repeatedly, resulting in a series of sections being displayed
#     (i.e. zoom in).
#
#     Clicking the left button once followed by the right button, results
#     in the previous sub-section being redisplayed (i.e. zoom out).
#
#     Clicking the right button once results in the script exiting.

#  Arguments:
#     -d
#        The device name.  Defaults to xwindows.

#  Notes:
#     -  Depending on your current settings for DISPLAY, the colour table
#     may change when each new section is displayed, as the scaling
#     algorithm adapts to the different set of pixel values being displayed.
#     -  For ease of use, it's recommended that you set up an alias for
#     this script, for example
#        alias pazo 'source /home/bm/scripts/pazo.csh'

#  Prior Requirements:
#     -  An image must already have been displayed.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
#     DSB: David S. Berry (Starlink)
#     {enter_new_authors_here}

#  History:
#     14-MAR-2001 (DSB):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-


# Conceal the startup messages.
    alias echo "echo > /dev/null"
    source $KAPPA_DIR/kappa.csh
    unalias echo

#  Initialize some shell variables.
    set again = 1
    set devname = xw
    set args = ($argv[1-])

#  Process each of the arguments to the script.
    while ( $#args > 0 )
       switch ($args[1])
       case -d:        #  Device name
          shift args
          set devname = $args[1]
          shift args
          breaksw
       case *:
          echo "Usage: pazo [-d device]"
          exit
       endsw
     end

#  Give instructions.
     echo ""
     echo "  Clicking the left mouse button at two distinct points will zoom"
     echo "  in to the rectangle defined by the two points. Clicking the left"
     echo "  button followed by the right button will zoom out to the"
     echo "  previous area. Clicking the right mouse button alone will exit."
     echo ""

#  The "stack" variable will be used to hold a list of the sections which
#  have been displayed, like a "first-in, last-out" stack. Initialise it
#  to hold the currently dislayed NDF section.
     picin current device=$devname noreport
     set stack = `parget refnam picin`

#  Get the name of the displayed NDF, without any trailing section
#  specifier. First split the total string into two words (the NDF name
#  and the section specifier) by putting a space in front of any parenthesis.
#  Then save the first word as the NDF name.
     set list = `echo $stack | sed -e 's#(# (#'`
     set ndf = $list[1]

#  Loop until less than 2 positons are given.
     while ( $again == 1 )

#  Allow the user to give another single point using the cursor.
        cursor nodescribe device=$devname frame=pixel noinfo maxpos=1 \
               name=data plot=cross quiet

#  Retrieve the number of positions supplied from the NUMBER output
#  parameter. If zero points were given, we exit.
        if( `parget number cursor` == 0 ) then
           set again = 0

#  If a position was supplied using the cursor....
        else

#  Retrieve the position from the LASTPOS output parameter and convert
#  from fractional pixel coordinate to integer pixel index.
           set p = `parget lastpos cursor`
           set x1 = `calc exp="nint($p[1]+0.5)"`
           set y1 = `calc exp="nint($p[2]+0.5)"`

#  Allow the user to give a second point using the cursor.
           cursor nodescribe device=$devname frame=pixel noinfo maxpos=1 \
                  name=data plot=cross quiet

#  If zero points were given, we zoom out by one level.
           if( `parget number cursor` == 0 ) then

#  If there is more than one section on the stack, remove the section just
#  displayed. We always leave the originally dispalyed NDF section on the
#  stack.
              if( $#stack > 1 ) then
                 shift stack
              endif

#  We will display the first NDF section left on the stack.
              set section = $stack[1]

#  If a second position was supplied using the cursor....
           else

#  Retrieve the position from the LASTPOS output parameter and convert
#  from fractional pixel coordinate to integer pixel index.
              set p = `parget lastpos cursor`
              set x2 = `calc exp="nint($p[1]+0.5)"`
              set y2 = `calc exp="nint($p[2]+0.5)"`

#  Ensure the bounds are the correct way round.
              if( $x1 > $x2 ) then
                 set t = $x1
                 set x1 = $x2
                 set x2 = $t
              endif

              if( $y1 > $y2 ) then
                 set t = $y1
                 set y1 = $y2
                 set y2 = $t
              endif

#  Construct a specification for the part of the displayed NDF which
#  falls within the rectangle bounded by the two supplied points.
              set section = $ndf\($x1\:$x2\,$y1\:$y2\)

#  Add this section to the start of the section stack variable.
              set stack = `echo $section $stack`

           endif

#  Clear the current picture.
           gdclear current device=$devname

#  Redisplay the selected section of the ndf. Discard text output.
           display $section accept > /dev/null

       endif
    end
    exit

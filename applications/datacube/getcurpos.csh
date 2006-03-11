#!/bin/csh
#+
#  Name:
#     getcurpos.csh
#
#  Purpose:
#     Uses the cursor to obtain and indicate co-ordinates from a plot.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     getcurpos -a axes [-ci index] [-d device] [-f frame] [-g]
#
#  Description:
#     This shell script activates the cursor upon a selected graphics 
#     device, and returns the co-ordinate(s) of a single point.  There
#     is a choice of co-ordinate axis or axes (see option -a) and 
#     co-ordinate Frame (see option -f).  The plot is also annotated to
#     indicate the chosen position: a cross marks X-Y position, a
#     vertical or horizontal line spanning the data area of the plot 
#     for an X or position respectively.
#
#  Parameters:
#     -a axes
#       The axis or axes to use.  Allowed values are "XY", "X", and "Y".
#       ["X"]
#     -ci index
#       The colour index of the plot annotation.  It should be a
#       positive integer no more than 15, normally 2 or 3 to stand out
#       from other elements of the plot. [2]
#     -d
#       The graphics device. [xwin]
#     -f frame
#       The co-ordinate Frame in which the co-ordinates are required.
#       If not supplied, the Frame used to create the plot is used. 
#       Likely values are "PIXEL" and "SPECTRUM".  []
#     -g
#       If present, this indicates that the returned value(s) should be
#       in pixel grid co-ordinates.  This overrides option -f.
#
#  Returned Variables:
#     $xpos  -- the X co-ordinate for "XY" and "X" provided -g is not set.
#     $ypos  -- the Y co-ordinate for "XY" and "Y" provided -g is not set.
#     $xgrid -- the X co-ordinate for "XY" and "X" when -g is set.
#     $ygrid -- the Y co-ordinate for "XY" and "Y" when -g is set.
#
#  Notes:
#      -  It is assumed that the co-ordinate Frame is stored in the AGI
#      database.  Normally the current Frame is used.
#      -  It uses /tmp/<user>/getcurpos.tmp to record the output from 
#      KAPPA:CURSOR.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2006 March 2 (MJC):
#       Original version.
#     2006 March 9 (MJC):
#       Validate colour index.
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2006 Central Laboratory of the Research Councils
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"
if ( ! -e ${tmpdir}/${user} ) mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/getcurpos.tmp"
touch $curfile

# Options.
set ci = 2
set dir = "X"
set grid = 0
set coframe = ""
set graphdev = "xwin"

set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -a:    # co-ordinate axis/axes required
      shift args
      set dir = $args[1]
      shift args
      breaksw
   case -ci:    # colour index of cross or line
      shift args
      set ci = `calc exp="nint($args[1])"`
      if ( $ci < 0 || $ci > 15 ) set ci = 2
      shift args
      breaksw
   case -d:    # graphics device
      shift args
      set graphdev = $args[1]
      shift args
      breaksw
   case -f:    # co-ordinate frame
      shift args
      set coframe = $args[1]
      shift args
      breaksw
   case -g:    # return grid indices
      shift args
      set grid = 1
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Validate parameters.
set dir = `echo $dir | awk '{print toupper($0)}'`
if ( $dir != "X" &&  $dir != "XY" && $dir != "Y" ) then
   set dir = "X"
endif

set coframe = `echo $coframe | awk '{print toupper($0)}'`
if ( $grid == 1 ) set coframe = "PIXEL"

if ( $graphdev == "" ) set graphdev = "xwin"

# Obtain a position using the cursor.
if ( $dir == "XY" ) then
   if ( "$coframe" == "" ) then
      cursor showpixel=true style="Colour(marker)=${ci}" plot=mark \
             maxpos=1 marker=2 device=${graphdev} >> ${curfile}
   else
      cursor showpixel=true style="Colour(marker)=${ci}" plot=mark \
             maxpos=1 marker=2 device=${graphdev} frame=${coframe} >> ${curfile}
   endif

else if ( $dir == "X" ) then
   if ( "$coframe" == "" ) then
      cursor showpixel=true style="Colour(curves)=${ci}" plot=vline \
             maxpos=1 device=${graphdev} >> ${curfile}
   else
      cursor showpixel=true style="Colour(marker)=${ci}" plot=vline \
             maxpos=1 device=${graphdev} frame=${coframe} >> ${curfile}
   endif

else
   if ( "$coframe" == "" ) then
      cursor showpixel=true style="Colour(curves)=${ci}" plot=hline \
             maxpos=1 device=${graphdev} >> ${curfile}
   else
      cursor showpixel=true style="Colour(marker)=${ci}" plot=hline \
             maxpos=1 device=${graphdev} frame=${coframe} >> ${curfile}
   endif
endif

# Wait for CURSOR output then get X-Y co-ordinates from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${curfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
if ( $grid == 1 ) then
   set xgrid = `calc exp="nint(nint($pos[1]+0.5))" prec=_REAL`
   set ygrid = `calc exp="nint(nint($pos[2]+0.5))" prec=_REAL`

else if ( $dir == "XY" ) then
   set xpos = $pos[1]
   set ypos = $pos[2]
else if ( $dir == "X" ) then
   set xpos = $pos[1]
else if ( $dir == "Y" ) then
   set ypos = $pos[2]
endif

# Clean up the cursor temporary file.
rm -f ${curfile} >& /dev/null

exit
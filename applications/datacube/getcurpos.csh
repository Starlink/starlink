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
#     source ${DATACUBE_DIR}/getcurpos.csh -a axes [-ci index]
#            [-d device] [-f frame] [-g]
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
#     -d device
#       The graphics device. [xwin]
#     -f frame
#       The co-ordinate Frame in which the co-ordinates are required.
#       If not supplied, the Frame used to create the plot is used.
#       Likely values are "PIXEL" and "SPECTRUM".  []
#     -g
#       If present, this indicates that the returned value(s) should be
#       in pixel grid co-ordinates.  This overrides option -f.
#
#  Shell Variables:
#     $xgrid = INTEGER (Returned)
#        The X grid index for "XY" and "X" when -g is set.
#     $xpos = REAL (Returned)
#        The X co-ordinate for "XY" and "X" provided -g is not set.
#     $ygrid = INTEGER (Returned)
#        The Y grid index for "XY" and "Y" when -g is set.
#     $ypos = REAL (Returned)
#        The Y co-ordinate for "XY" and "Y" provided -g is not set.

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
#       Validate colour index.  Renamed "Returned Variables" to "Shell
#       Variables" and reformatted to give access and data type.
#       Correct the Usage.  Use internal variable names (gcp_ prefix) to
#       avoid potential classhes with variables from calling script.
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
set gcp_curfile = "${tmpdir}/${user}/getcurpos.tmp"
touch $gcp_curfile

# Options.
set gcp_ci = 2
set gcp_dir = "X"
set gcp_grid = 0
set gcp_coframe = ""
set gcp_graphdev = "xwin"

set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -a:    # co-ordinate axis/axes required
      shift args
      set gcp_dir = $args[1]
      shift args
      breaksw
   case -ci:    # colour index of cross or line
      shift args
      set gcp_ci = `calc exp="nint($args[1])"`
      if ( $gcp_ci < 0 || $gcp_ci > 15 ) set gcp_ci = 2
      shift args
      breaksw
   case -d:    # graphics device
      shift args
      set gcp_graphdev = $args[1]
      shift args
      breaksw
   case -f:    # co-ordinate frame
      shift args
      set gcp_coframe = $args[1]
      shift args
      breaksw
   case -g:    # return grid indices
      shift args
      set gcp_grid = 1
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Validate parameters.
set gcp_dir = `echo $gcp_dir | awk '{print toupper($0)}'`
if ( $gcp_dir != "X" &&  $gcp_dir != "XY" && $gcp_dir != "Y" ) then
   set gcp_dir = "X"
endif

set gcp_coframe = `echo $gcp_coframe | awk '{print toupper($0)}'`
if ( $gcp_grid == 1 ) set gcp_coframe = "PIXEL"

if ( $gcp_graphdev == "" ) set gcp_graphdev = "xwin"

# Obtain a position using the cursor.
if ( $gcp_dir == "XY" ) then
   if ( "$gcp_coframe" == "" ) then
      cursor showpixel=true style="Colour(marker)=${gcp_ci}" plot=mark \
             maxpos=1 marker=2 device=${gcp_graphdev} >> ${gcp_curfile}
   else
      cursor showpixel=true style="Colour(marker)=${gcp_ci}" plot=mark \
             maxpos=1 marker=2 device=${gcp_graphdev} frame=${gcp_coframe} >> ${gcp_curfile}
   endif

else if ( $gcp_dir == "X" ) then
   if ( "$gcp_coframe" == "" ) then
      cursor showpixel=true style="Colour(curves)=${gcp_ci}" plot=vline \
             maxpos=1 device=${gcp_graphdev} >> ${gcp_curfile}
   else
      cursor showpixel=true style="Colour(marker)=${gcp_ci}" plot=vline \
             maxpos=1 device=${gcp_graphdev} frame=${gcp_coframe} >> ${gcp_curfile}
   endif

else
   if ( "$gcp_coframe" == "" ) then
      cursor showpixel=true style="Colour(curves)=${gcp_ci}" plot=hline \
             maxpos=1 device=${gcp_graphdev} >> ${gcp_curfile}
   else
      cursor showpixel=true style="Colour(marker)=${gcp_ci}" plot=hline \
             maxpos=1 device=${gcp_graphdev} frame=${gcp_coframe} >> ${gcp_curfile}
   endif
endif

# Wait for CURSOR output then get X-Y co-ordinates from
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${gcp_curfile} )
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
if ( $gcp_grid == 1 ) then
   set xgrid = `calc exp="nint(nint($pos[1]+0.5))" prec=_REAL`
   set ygrid = `calc exp="nint(nint($pos[2]+0.5))" prec=_REAL`

else if ( $gcp_dir == "XY" ) then
   set xpos = $pos[1]
   set ypos = $pos[2]
else if ( $gcp_dir == "X" ) then
   set xpos = $pos[1]
else if ( $gcp_dir == "Y" ) then
   set ypos = $pos[2]
endif

# Clean up the cursor temporary file.
rm -f ${gcp_curfile} >& /dev/null

exit
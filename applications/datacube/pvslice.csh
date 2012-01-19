#!/bin/tcsh -xv

#+
#  Name:
#     PVSLICE

#  Purpose:
#     Extracts and displays a velocity-position slice from an (RA,Dec,vel) cube

#  Language:
#     C-shell

#  Type of Module:
#     C-shell script

#  Usage:
#     pvslice -i filename -o filename [-ci index] [-p plane]

#  Description:
#     This script extracts and displays a slice from a position-velocity
#     cube.  The slice need not be parallel to either spatial pixel axis.
#
#     The script displays a chosen spatial plane from the supplied cube
#     in the left half of the current graphics device.  You are then invited
#     to select two spatial positions within the displayed plane using the
#     cursor.  A two-dimensional slice is then extracted from the cube that
#     passes through the two selected spatial positions.  The first axis in
#     this slice measures spatial distance along the line, and the second axis
#     is the velocity axis.  This slice appears in the right-hand side of
#     the current graphics device, and saved to the specified output NDF.

#  Command-line Arguments:
#     -ci index
#       The colour or colour index of the annotations on the left-hand 
#       display indicating the spatial location of the slice.  An index
#       should be a positive integer no more than 15, normally 2 or 3 to 
#       stand out from other elements of the plot.  If absent the
#       annotations will appear in red.  []
#     -i filename
#       The name of an existing three-dimensional (RA,Dec,vel) cube.
#       The script will prompt for the input file if not supplied on
#       the command line.
#     -o filename
#       The name of the NDF in which to store the velocity-position
#       slice.  The script will prompt for this NDF if it is not
#       supplied via this option.
#     -p plane
#       Velocity or pixel index of the plane to display to enable cursor
#       selection of the slice end points.  To specify a velocity
#       supply a floating-point value such as 2.0; for an index supply
#       an integer.  [0]

#  Examples:
#     pvslice -i orion_masked -o orion_pvmap
#        This extracts a user-selected plane from the cube NDF called
#        orion_masked, and saves it to NDF orion_pvmap.  The slice is
#        shown to the right of the spatial image.
#     pvslice -i orion_masked -o orion_pvmap -p 1.5
#        As above but slice selection is from the velocity plane at
#        1.5 as opposed to the middle index.

#  Notes:
#     -  The WCS in the returned NDF is somewhat complex; it has a
#     degenerate pixel axis and a degenerate WCS axis. These could be
#     removed using "ndfcopy trim", but this would result in a WCS
#     with no inverse transformation (from current to pixel), which gives
#     problems when displaying it, etc. Alternatively, the AXIS frame can
#     be used by doing "wcsframe frame=axis".
#     -  To avoid modification of the input cube, the script creates a 
#     temporary copy in the current directory, and deletes the copy upon 
#     completion.
#     -  While the script is intended to make position-velocity slices 
#     from spectral cubes, the contents of the third axis is arbitrary.
#     -  The script clears the graphics database for the current device.
#     It then creates two FRAME pictures with labels a1 and a2.  Within each 
#     of these DISPLAY creates FRAME and DATA pictures, the former holding
#     annotated axes.  On exit a2 is the current picture.
#     -  The label for the spatial (first) axis in the slice plot is
#     "Offset from start".

#  Related Applications:
#     KAPPA: PROFILE; Figaro: SLICE.

#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.

#  Implementation Deficiencies:
#     -  There is no validation that the supplied NDF is indeed a
#     (RA,Dec,vel) cube, only that it exists and is a cube.
#     -  There is no control of scaling limits in the displays.

#  Copyright:
#     Copyright (C) 2010 Science and Technology Facilities Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either Version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#     02110-1301, USA.

#  Authors:
#     DSB: David S. Berry (JACH)
#     MJC: Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}

#  History:
#     11-MAY-2010 (DSB):
#        Original version called makepv.csh
#     12-JULY-2010 (DSB):
#        Make the name of the temporary NDF more specific to script.
#        Add to documentation for KAPPA release
#     2010 July 12 (MJC):
#        Standardize and complete the prologue.  Parameters renamed to
#        IN and OUT to conform with other KAPPA tasks.  Renamed the 
#        temporary files using the process identification number.
#     2010 July 13 (MJC):
#        Moved from KAPPA to DATACUBE so replace positional parameters
#        IN and OUT with C-shell options.  Added an option to specify
#        the annotation colour.
#    2012 January 18 (MJC):
#        Added -p option.
#     {enter_further_changes_here}

#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Ensure ADAM tasks fail rather than prompt for parameter values.
setenv ADAM_NOPROMPT 1

# Set options access flags.
set gotinfile = "FALSE"
set gotoutfile = "FALSE"

# Handle the command-line arguments.
set args = ($argv[1-])
set ci = red
set plane = 0
while ( $#args > 0 )
   switch ($args[1])
   case -ci:    # colour index of annotation
      shift args

# Test whether the value is a colour index or named colour.
# If it's an index, default to red if the index is out of range.
# Assume that any other string is a named colour.
      set cis = `$KAPPA_DIR/calc exp="nint($args[1])"`
      if ( $status == 1 ) then
         if ( $cis < 0 || $cis > 15 ) then
            set ci = red
         else
            set ci = $cis
         endif
      else
         set ci = $args[1]
      endif
      shift args
      breaksw
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output postion-velocity slice
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # velocity plane to display
      shift args
      set plane = $args[1]
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Validate data
# =============

# Obtain the input NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile.
source ${DATACUBE_DIR}/checkndf.csh -q -s pvslice
if ( $status == 1 ) exit

# Setup output filename.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output slice: "
   set outfile = $<
   echo " "
endif

# Obtain the spatial positions of the slice
# =========================================

# Display the middle slice of the cube on the left of the screen.
$KAPPA_DIR/gdclear
$KAPPA_DIR/picdef mode=a xpic=2 ypic=1 prefix=a
$KAPPA_DIR/display $infile"(,,$plane)" mode=perc percentiles=\[5,95\] quiet

# Get a FITS catalogue holding the pixel co-ordinates at the start and
# end of the line.
echo " "
echo "   Use the cursor to left click on the start and end of the slice to be extracted..."
$KAPPA_DIR/cursor quiet outcat=pvslice_tempcat$$ frame=pixel maxpos=2 minpos=2 \
                  plot=chain style="colour=${ci}"
echo " "

# Extract the pixel co-ordinates at the start into shell variables x1
# and y1.
$KAPPA_DIR/listshow pvslice_tempcat$$ first=1 last=1 quiet
set pos = `$KAPPA_DIR/parget posns listshow`
set x1 = $pos[1]
set y1 = $pos[2]

# Extract the pixel co-ordinates at the end into shell variables x2
# and y2.
$KAPPA_DIR/listshow pvslice_tempcat$$ first=2 last=2 quiet
set pos = `$KAPPA_DIR/parget posns listshow`
set x2 = $pos[1]
set y2 = $pos[2]

# Calculate the angle between the line and the X pixel axis, in degrees.
set angle = `$KAPPA_DIR/calc exp="atan2d(pa-pb,pc-pd)" pa=$x1 pb=$x2 pc=$y2 pd=$y1`

# Create new offset co-ordinate system
# ====================================

# Take a copy of the cube so that we can set its attributes without
# changing the original.
echo "   Copying cube to avoid changing it..."
$KAPPA_DIR/ndfcopy $infile pvslice_tempcube$$

# Get the (RA,Dec) at the start of the line, and set it as the reference
# position in the cube copy.
$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$x1\,$y1\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set aref = $pos[1]
set bref = $pos[2]
$KAPPA_DIR/wcsattrib pvslice_tempcube$$ set skyref \'$aref\,$bref\'

# Get the (RA,Dec) at the end of the line, and set it as the meridian
# position in the cube copy.
$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$x2\,$y2\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set amer = $pos[1]
set bmer = $pos[2]
$KAPPA_DIR/wcsattrib pvslice_tempcube$$ set skyrefp \'$amer\,$bmer\'

# Use these reference positions as the origin of a new offset co-ordinate
# system.
$KAPPA_DIR/wcsattrib pvslice_tempcube$$ set skyrefis origin

# Get the co-ordinates of the start and end of the line in the new
# (offset) co-ordinate system.
$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$x1\,$y1\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set aref = $pos[1]
set bref = $pos[2]

$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$x2\,$y2\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set amer = $pos[1]
set bmer = $pos[2]

# Rotate the slice
# ================

# The rotate command rotates about the pixel origin, so move the pixel
# origin to the start of the line (without changing the origin on the
# velocity axis).
$KAPPA_DIR/ndftrace pvslice_tempcube$$ quiet
set lbound = `$KAPPA_DIR/parget lbound ndftrace`
set ra_origin = `$KAPPA_DIR/calc exp="pa-pb" pa=$lbound[1] pb=$x1`
set dec_origin = `$KAPPA_DIR/calc exp="pa-pb" pa=$lbound[2] pb=$y1`
set vel_origin = $lbound[3]

$KAPPA_DIR/setorigin pvslice_tempcube$$ origin=\[$ra_origin,$dec_origin,$vel_origin\]

# Rotate the cube in the (RA,Dec) plane.  This rotation is about the
# pixel origin which is now at the start of the line.  It rotates the
# cube so that the requested line is parallel to the second pixel axis.
echo "   Rotating cube..."
$KAPPA_DIR/rotate pvslice_tempcube$$ makepv_temp angle=$angle

# Extract and display the slice
# =============================

# Get the pixel co-ordinates of the start and end of the line in the rotated
# cube.
$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$aref\,$bref\' framein=sky frameout=pixel quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set y1 = `$KAPPA_DIR/calc exp="nint(pa+0.5)" pa=$pos[2]`

$KAPPA_DIR/wcstran pvslice_tempcube$$ posin=\'$amer\,$bmer\' framein=sky frameout=pixel quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set y2 = `$KAPPA_DIR/calc exp="nint(pa+0.5)" pa=$pos[2]`

# Extract the slice.
$KAPPA_DIR/ndfcopy pvslice_tempcube$$\(0,$y1\:$y2,\) $outfile trimbad

# Set a more-useful axis label.
$KAPPA_DIR/wcsattrib $outfile set Label'(2)' "'Offset from start'"

#  Display the slice on the right of the screen.
echo "   Displaying slice..."
$KAPPA_DIR/picsel a2
$KAPPA_DIR/display $outfile fill mode=perc percentiles=\[5,95\] useaxis=\'2\,3\' quiet

#  Set AXIS arrays.
$KAPPA_DIR/setaxis $outfile 1 wcs
$KAPPA_DIR/setaxis $outfile 2 wcs
$KAPPA_DIR/setaxis $outfile 3 wcs

# Clean up
# ========
cleanup:
rm -f pvslice_temp*
unsetenv ADAM_NOPROMPT

exit

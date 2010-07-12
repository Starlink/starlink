#!/bin/tcsh

#  Purpose:
#    Extract and display a velocity-position slice from an (ra,dec,vel) cube

#  Usage:
#    makepv.csh <cube> <slice>

#  Description:
#    A spatial plane from the supplied cube is displayed in the left half
#    of the current graphics device. The user then uses the cursor to
#    select two spatial positions within the displayed plane. A 2-D slice
#    is then extracted from the cube that passes through the two selected
#    spatial positions. The first axis in this slice measures spatial
#    distance along the line, and the second axis is the velocity axis.
#    This slice is then displayed in the right hand side of the current
#    graphics device, and seved to the specified output NDF.

#  Parameters:
#    <cube> - The name of an existing 3D (RA,Dec,vel) cube
#    <slice> - The name of the NDF in which to store the
#              velocity-position slice

#  Notes:
#    - The WCS in the returned NDF is somewhat complex, it having a
#    degenerate pixel axis and a degenerate WCS axis. These could be
#    removed using "ndfcopy trim", but this would result in a WCS
#    with no inverse transformation (from current to pixel) which gives
#    problems when displaying it, etc. Alternaively, the AXIS frame can
#    be used by doing "wcsframe frame=axis".


#  Ensure ADAM tasks fail rather than prompt for parameter values
setenv ADAM_NOPROMPT 1

#  Check that the required parameters were supplied.
if ( $# < 2 ) then
   echo "Usage: makepv <cube> <slice>"
   exit
endif

# Check the supplied NDF exists (it may be an NDF inside another NDF so
# we cannot just check that the .sdf file exists).
$KAPPA_DIR/ndftrace $1 | grep \!\! > /dev/null
if( $status == 0 ) then
   echo "Cannot access $1"
   exit
endif

#  Display the mid slice of the cube on the left of the screen
$KAPPA_DIR/gdclear
$KAPPA_DIR/picdef mode=a xpic=2 ypic=1 prefix=a
$KAPPA_DIR/display $1'(,,0)' mode=perc percentiles=\[5,95\] quiet

#  Get a FITS catalogue holding the pixel coords at the start and end of
#  the line.
echo " "
echo "   Use the cursor to left click on the start and end of the slice to be extracted..."
$KAPPA_DIR/cursor quiet outcat=makepv_temp frame=pixel maxpos=2 minpos=2 plot=chain style='colour=red'
echo " "

# Extract the pixel coords at the start into shell variables x1 and y1
$KAPPA_DIR/listshow makepv_temp first=1 last=1 quiet
set pos = `$KAPPA_DIR/parget posns listshow`
set x1 = $pos[1]
set y1 = $pos[2]

# Extract the pixel coords at the end into shell variables x2 and y2
$KAPPA_DIR/listshow makepv_temp first=2 last=2 quiet
set pos = `$KAPPA_DIR/parget posns listshow`
set x2 = $pos[1]
set y2 = $pos[2]

#  Calculate the angle between the line and the X pixel axis, in degrees.
set angle = `$KAPPA_DIR/calc exp="atan2d(pa-pb,pc-pd)" pa=$x1 pb=$x2 pc=$y2 pd=$y1`

#  Take a copy of the cube so that we can set its attributes without
#  changing the original.
echo "   Copying cube to avoid changing it..."
$KAPPA_DIR/ndfcopy $1 makepv_temp1

#  Get the (RA,Dec) at the start of the line, and set it as the reference
#  position in the cube copy.
$KAPPA_DIR/wcstran makepv_temp1 posin=\'$x1\,$y1\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set aref = $pos[1]
set bref = $pos[2]
$KAPPA_DIR/wcsattrib makepv_temp1 set skyref \'$aref\,$bref\'

#  Get the (RA,Dec) at the end of the line, and set it as the meridian
#  position in the cube copy.
$KAPPA_DIR/wcstran makepv_temp1 posin=\'$x2\,$y2\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set amer = $pos[1]
set bmer = $pos[2]
$KAPPA_DIR/wcsattrib makepv_temp1 set skyrefp \'$amer\,$bmer\'

#  Use these reference positions as the origin of a new offset coordinate
#  system
$KAPPA_DIR/wcsattrib makepv_temp1 set skyrefis origin

#  Get the coords of the start and end of the line in the new (offset) coords
#  system.
$KAPPA_DIR/wcstran makepv_temp1 posin=\'$x1\,$y1\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set aref = $pos[1]
set bref = $pos[2]

$KAPPA_DIR/wcstran makepv_temp1 posin=\'$x2\,$y2\,0\' framein=pixel frameout=sky quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set amer = $pos[1]
set bmer = $pos[2]

#  The rotate command rotates about the pixel origin, so move the pixel
#  origin to the start of the line (without changing the origin on the
#  velocity axis).
$KAPPA_DIR/ndftrace makepv_temp1 quiet
set lbound = `$KAPPA_DIR/parget lbound ndftrace`
set ra_origin = `$KAPPA_DIR/calc exp="pa-pb" pa=$lbound[1] pb=$x1`
set dec_origin = `$KAPPA_DIR/calc exp="pa-pb" pa=$lbound[2] pb=$y1`
set vel_origin = $lbound[3]

$KAPPA_DIR/setorigin makepv_temp1 origin=\[$ra_origin,$dec_origin,$vel_origin\]

#  Rotate the cube in the (RA,Dec) plane. This rotation is about the
#  pixel origin which is now at the start of the line. It rotates the
#  cube so that the requested line is parallel to the 2nd pixel axis.
echo "   Rotating cube..."
$KAPPA_DIR/rotate makepv_temp1 makepv_temp angle=$angle

#  Get the pixel coords of the start and end of the line in the rotated
#  cube.
$KAPPA_DIR/wcstran makepv_temp posin=\'$aref\,$bref\' framein=sky frameout=pixel quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set y1 = `$KAPPA_DIR/calc exp="nint(pa+0.5)" pa=$pos[2]`

$KAPPA_DIR/wcstran makepv_temp posin=\'$amer\,$bmer\' framein=sky frameout=pixel quiet
set pos = `$KAPPA_DIR/parget posout wcstran`
set y2 = `$KAPPA_DIR/calc exp="nint(pa+0.5)" pa=$pos[2]`

#  Extract the slice
$KAPPA_DIR/ndfcopy makepv_temp\(0,$y1\:$y2,\) $2 trimbad

#  Set more useful axis label
$KAPPA_DIR/wcsattrib $2 set Label'(2)' "'Offset from start'"

#  Display the slice on the right of the screen.
echo "   Displaying slice..."
$KAPPA_DIR/picsel a2
$KAPPA_DIR/display $2 fill mode=perc percentiles=\[5,95\] useaxis=\'2\,3\' quiet

#  Set AXIS arrays
$KAPPA_DIR/setaxis $2 1 wcs
$KAPPA_DIR/setaxis $2 2 wcs
$KAPPA_DIR/setaxis $2 3 wcs

#  Clean up.
rm -f makepv_temp*

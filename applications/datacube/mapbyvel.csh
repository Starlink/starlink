#!/bin/csh
#+
#
#  Name:
#     mapbyvel.csh
#
#  Purpose:
#     Uses a velocity map to extract values from a velocity cube forming a
#     new velocity map.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     mapbyvel [-i filename] [-m filename] [-o filename]
#
#  Description:
#     This shell script uses a velocity map of a spatial region to create
#     a new velocity map.  The spatial co-ordinates and velocity value at
#     each pixel specify the co-ordinates of voxels in a spectral cube.
#     The data value in the cube at this voxel becomes the data value in
#     the output map.
#
#     While creating velocity maps is the expected usage for the script,
#     it makes no specific tests that the Z co-ordinate is velocity, or
#     even that the image value is velocity.  In mathematical form the
#     output image values are given by the following expression.
#
#         Output(x,y) = Cube(x,y,Image(x,y))
#
#     One application is multi-spectral imaging in complex molecular clouds,
#     where one optically thin spectral line provides a clearer mapping of
#     the cloud and system velocities.  Using this specie's velocity map
#     traces the varying velocity, and thus data observed through different
#     spectral lines can be compared, enabling the calculation of cloud
#     parameters such as opacity.
#
#  Parameters:
#     -i filename
#       A three-dimensional NDF with spatial axes and a third velocity axis
#       By default the script will prompt for the input cube.
#     -m filename
#       The name of a two-dimensional velocity map, where the data values are
#       velocities.  It should have the same spatial co-ordinates as the
#       supplied cube.  Such a map can be created using VELMAP or
#       KAPPA:COLLAPSE with the Iwc or Comax estimators.  By default the
#       script will prompt for the input cube.
#     -o filename
#       The filename for the output NDF velocity map.
#
#  Notes:
#     -  The script assumes X and Y axes are spatial and Z is spectral.
#     Use KAPPA:PERMAXES to re-orient the cube if that is not the case.
#     -  A further assumption is the cube's Z-axis co-ordinate matches the
#     data values in the supplied image.  If that is not the case, change
#     the co-ordinate system with WCSFRAME and/or WCSATTRIB.
#     -  It propagates the original variances from the cube, but does not
#     account for errors in the input velocity map affecting the values
#     extracted from the cube.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Copyright:
#     Copyright (C) 2010 Science and Technology Facilities Council.
#     All Rights Reserved.
#
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
#
#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2010 February 20 (MJC):
#        Original version.
#     2010 September 7 (MJC):
#        Incorporated into DATACUBE.  DATACUBE prologue and options added.
#     {enter_further_changes_here}
#
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Invoke the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Do the variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set carpetfile = "${tmpdir}/${user}/carpet$$"
set carpet_th = "${tmpdir}/${user}/carpet_th$$"
set cubemask = "${tmpdir}/${user}/cube_mask$$"
set cubevel = "${tmpdir}/${user}/cube_vel$$"

# Set options access flags.
set gotinfile = "FALSE"
set gotmap = "FALSE"
set gotoutfile = "FALSE"
set plotspec = "FALSE"

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input three-dimensional velocity NDF
      shift args
      set gotinfile = "TRUE"
      set cube = $args[1]
      shift args
      breaksw
   case -m:    # input velocity map
      shift args
      set gotmap = "TRUE"
      set velmap = $args[1]
      shift args
      breaksw
   case -o:    # output velocity map
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Obtain details of the input and output files.
# =============================================

# Obtain the cube NDF if it is not supplied on the command line.  Validate
# that the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
set infile = $cube
source ${DATACUBE_DIR}/checkndf.csh -p 'Input NDF cube' -s mapbyvel -q
if ( $status == 1 ) exit
if ( ${gotinfile} == "FALSE" ) set cube = $infile

# Find out the cube pixel and co-ordinate bounds.
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`
set flbnd = `parget flbnd ndftrace`
set fubnd = `parget fubnd ndftrace`

# Obtain the input map NDF if it is not supplied on the command line.  Validate
# that the NDF exists and is an image.  Obtain $infile, $ndf_section, and $dims.
# Note this uses $gotinfile explicitly rather than some specified variable.  We
# could add another option, but it would then need to be changed in all user
# scripts.  This is the exception needing both a cube and an image.  Thus for
# now we shall handle the special case here.
set gotinfile = $gotmap
set infile = $velmap
source ${DATACUBE_DIR}/checkndf.csh -d 2 -p "Input NDF velocity map" -s mapbyvel -q
if ( $status == 1 ) exit
if ( ${gotmap} == "FALSE" ) set velmap = $infile

# Find out the velocity-map pie and co-ordinate bounds.
set mlbnd = `parget lbound ndftrace`
set mubnd = `parget ubound ndftrace`
set mflbnd = `parget flbnd ndftrace`
set mfubnd = `parget fubnd ndftrace`

# Check that the pixel bounds match.
if ( $mlbnd[1] != $lbnd[1] || $mlbnd[2] != $lbnd[2] || \
     $mubnd[1] != $ubnd[1] || $mubnd[2] != $ubnd[2] ) then
   echo "ERR: The X-Y pixel bounds of the supplied cube and velocity map do not match."
   exit
endif

# Check that the co-ordinate bounds match.
if ( $mflbnd[1] != $flbnd[1] || $mflbnd[2] != $flbnd[2] || \
     $mfubnd[1] != $fubnd[1] || $mfubnd[2] != $fubnd[2] ) then
   echo "ERR: The X-Y co-ordinate bounds of the supplied cube and velocity map do not match."
   exit
endif

# Assume for now that X and Y axes are spatial, Z-axis is velocity and its
# pixels are equally spaced in the Z-axis co-ordinate.  However, checks should
# be added in future.

# Setup output filename.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
   echo " "
endif

# Inquire the original cube's vital statistics.
# =============================================

# Obtain the cube's third-axis co-ordinate bounds.
ndftrace $cube >& /dev/null

set lower = $flbnd[3]
set upper = $fubnd[3]

# Obtain the cube's third-axis dimension.
set string = `parget dims ndftrace`
set dim = $string[3]

# Determine if a VARIANCE component is present.
set var = `parget variance ndftrace`

# Form a carpet-plot style cube from the input image.
# ===================================================

# CARPET uses the pixel centres as limits, not the bounds.  Thus we must
# make a half-pixel correction.
set halfpixel = `calc exp="0.5*(($upper)-($lower))/$dim" prec=_double`
set lower = `calc exp="($lower)+($halfpixel)" prec=_double`
set upper = `calc exp="($upper)-($halfpixel)" prec=_double`

# Create a carpet cube.
carpet in=$velmap out=$carpetfile ndatapix=$dim range=\"${lower},${upper}\" >& /dev/null

# Match its origin to that of the original cube.
setorigin ndf=$carpetfile like=$cube >& /dev/null

# Make and apply a cube mask.
# ===========================

# Make all the values bad except those closest to zero.  These are the
# locations in the original cube who values we want.  I am not completely
# sure this is correct in that CARPET generates values slightly offset
# from zero in the velocity.
thresh in=$carpetfile out=$carpet_th thrlo=-${halfpixel} thrhi=${halfpixel} \
       newlo=bad newhi=bad >& /dev/null

# Make the mask, correcting all the values to zero.
cmult in=$carpet_th scalar=0.0 out=$cubemask >& /dev/null

# To preserve any variance in the original cube, insert a dummy variance
# array in our mask.  The final may not need variance, but this
# propagates the original variances from the cube, but does not
# account for errors in the input image affecting the values extracted
# from the cube.
if ( "$var" == "TRUE" ) then
   setvar ndf=$cubemask variance=0.0 >& /dev/null
endif

# Apply the mask.
add in1=$cube in2=$cubemask out=$cubevel >& /dev/null

# Form the desired image.
collapse in=$cubevel out=$outfile estimator=sum axis=3 wlim=0.0 >& /dev/null

# Clean up
# ========
cleanup:

rm -f ${carpetfile}.sdf >& /dev/null
rm -f ${carpet_th}.sdf >& /dev/null
rm -f ${cubemask} >& /dev/null
rm -f ${cubevel} >& /dev/null

exit

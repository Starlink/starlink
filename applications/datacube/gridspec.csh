#!/bin/csh
#+
#  Name:
#     gridspec.csh
#
#  Purpose:
#     Averages groups of neighbouring spectra of a three-dimensional IFU
#     NDF and then plots these averaged spectra in a grid.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     gridspec [-b string] [-i filename] [-z/+z]
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and
#     if you request zooming the script presents you with a white-light
#     image of the cube.   You can then select the lower and upper
#     spatial limits to plot using the cursor.  You can instead supply
#     an NDF section with the filename to define both spatial and
#     spectral limits to plot.
#
#     The script averages spectra in the chosen region by specified
#     compression factors in the spatial domain.  It then displays the
#     average spectra in a grid, where the exterior axes indicate the
#     spatial co-ordinates of the averaged spectra, and the interior axes
#     the data values against spectra co-ordinates.
#
#  Parameters:
#     -b string
#       The number of spectra to block average along the x and y axes
#       respectively.  This should be a comma-separated list or a single
#       number; the latter case applies the same compression factor to
#       both spatial axes.  The numbers must be positive integers.  [2]
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
#     -z
#       The script will automatically prompt to select a region to zoom
#       before prompting for the region of interest.  [TRUE]
#     +z
#       The program will not prompt for a zoom before requesting the region
#       of interest. [FALSE]
#
#  Notes:
#     -  The compression is trimmed, so that only compression-factor
#     multiples of original pixels are included in the plot.  There
#     -  The spatial averaging is aligned to obtain the expected number
#     of pixels irrespective of the pixel origin of the input cube.
#     Note that this may not be suitable if you wish to preserve alignment
#     with another compressed dataset.  See KAPPA:COMPAVE parameter ALIGN
#     for more details.
#
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     2006 March 1 (MJC):
#       Original version.
#     2006 March 2 (MJC):
#       Use a new script to obtain cursor positions.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script that
#       also checks for a degenerate third axis.
#     2006 March 16 (MJC):
#       Retain any supplied spectral axis section if a spatial region is
#       selected by cursor.
#     2008 January 10 (MJC):
#       Revise command line for CLINPLOT's second iteration parameter
#       names.
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2000-2006 Central Laboratory of the Research Councils.
#     Science and Technology Facilities Council.  All Rights Reserved.
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Clean up from previous runs.
rm -f ${tmpdir}/${user}/gridspec* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/gridspec_col"
set cmpfile = "${tmpdir}/${user}/gridspec_cmp"

set gotinfile = "FALSE"
set gotcmp = "FALSE"
set gotzoom = "ASK"
set cmpstring = 2

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -b:    # blocking factors
      shift args
      set gotcmp = "TRUE"
      set cmpstring = $args[1]
      shift args
      breaksw
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -z:    # zoom?
      set gotzoom = "TRUE"
      shift args
      breaksw
   case +z:    # not zoom?
      set gotzoom = "FALSE"
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain the compression factor parameter value.
# ==============================================

# Set the default.
if ( $gotcmp == "FALSE" ) then
   set cmpstring = 2
endif

# Extract the compression factors.
set cmpfactors = `echo $cmpstring | awk 'BEGIN {FS=","}{print $1, $2}'`
if ( $#cmpfactors == 1 ) then
   set cmpfactors = ( $cmpfactors[1] $cmpfactors[1] )
endif

if ( $cmpfactors[1] < 1 || $cmpfactors[2] < 1 ) then
   echo "GRIDSPEC_ERR: compression factors must be positive."
   exit
endif

# See if we actually need to compress.
set blockave = "FALSE"
if ( $cmpfactors[1] > 1 ||  $cmpfactors[1] > 1 ) then
   set blockave = "TRUE"
   set cmpfac = "${cmpfactors[1]},${cmpfactors[2]},1"
endif

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s gridspec
if ( $status == 1 ) exit

# Select a region to plot.
# ========================

# Setup the plot device.
set plotdev = "xwin"

# Zoom if required.
# -----------------
if ( ${gotzoom} == "ASK") then
   echo " "
   echo -n "Display white-light image to select subset (yes/no): "
   set zoomit = $<
else if ( ${gotzoom} == "TRUE") then
   set zoomit = "yes"
else
   set zoomit = "no"
endif

if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Show the white-light image.
# ===========================

# Collapse white-light image.
   echo "      Collapsing:"
   echo "        White-light image: ${dims[1]} x ${dims[2]}"
   collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null

# Display the collapsed image.
   gdclear device=${plotdev}
   paldef device=${plotdev}
   lutgrey device=${plotdev}
   display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" reset >&/dev/null

# Get the lower limit.
# --------------------
   echo " "
   echo "  Left click on lower zoom boundary."

# Obtain the cursor position in pixel co-ordinates.
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g
   set x1 = $xgrid
   set y1 = $ygrid

# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Find out the cube dimensions.
   set lbnd = `parget lbound ndftrace`
   set ubnd = `parget ubound ndftrace`

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
   set x2 = $xgrid
   set y2 = $ygrid

# Average the spectra in a chosen spatial region.
# ===============================================

# Just in case the user cannot follow the instructions.
   set xl = `calc exp="min($x1,$x2)" prec=_REAL`
   set yl = `calc exp="min($y1,$y2)" prec=_REAL`
   set xu = `calc exp="max($x1,$x2)" prec=_REAL`
   set yu = `calc exp="max($y1,$y2)" prec=_REAL`

   echo " "
   echo "      Zooming:"
   echo "      Extracting:"
   echo "        Lower (X,Y): ${xl},${yl}"
   echo "        Upper (X,Y): ${xu},${yu}"

# Set the bounds but not forgetting any spectral limits (say to focus
# on a single line) supplied with the NDF.
   set bnd = "(${xl}:${xu},${yl}:${yu},${lbnd[3]}:${ubnd[3]})"

# Use the supplied bounds' string from checkndf.csh when
# there is no graphical selection.
else
   set bnd = "$ndf_section"
endif

# Do the averaging in the selected spatial region.
if ( $blockave == "TRUE" ) then
   compave "in=${infile}${bnd} compress=[${cmpfac}] out=${cmpfile} trim align=first"
else
   set cmpfile = "${infile}${bnd}"
endif

# Create the multi-spectrum plot.
# ===============================

# This also helps making the database grow to unwieldy sizes.
gdclear device=${plotdev}

echo " "
echo "      Plotting:"
if ( $blockave == "TRUE" ) then
   echo "        Clinplot: Grid of spectra, averaged $cmpfactors[1] x $cmpfactors[2]"
else
   echo "        Clinplot: Grid of spectra
endif

clinplot ndf=${cmpfile} device=${plotdev} specstyle="colour(curve)=red" \
         axes reflabel margin="[0.15,0.17,0.12,0.1]" \
         style="title=${infile}" reset

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/gridspec* >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

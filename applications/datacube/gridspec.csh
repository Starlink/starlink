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
#     -  A feature of the compression is that you may obtain one fewer
#     pixel in the graph than expected.  Try slightly enlarging or
#     shifting the grid region to circumvent this.
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
#     {enter_further changes_here}
#
#  Copyright:
#     Copyright (C) 2000-2006 Central Laboratory of the Research Councils
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

if ( $cmpfactors[1] <= 1 || $cmpfactors[2] <= 1 ) then
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

# Get the input filename.
if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
   set infile = ${infile:r}
endif

# Obtain the name sans any section.
set inname = `echo $infile | \
              awk '{if (index($0,"(") > 0) print substr($0,1,index($0,"(")-1); else print $0}'`

echo " "
echo "      Input NDF:"
echo "        File: ${inname}.sdf"

# Check that it exists.
if ( ! -e ${inname}.sdf ) then
   echo "GRIDSPEC_ERR: ${inname}.sdf does not exist."
   exit  
endif

# Check that it exists.
ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "GRIDSPEC_ERR: ${infile} is not a datacube."
   exit
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

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
   collapse "in=${infile} out=${colfile} axis=3" >& /dev/null 


# Display the collapsed image.
   gdclear device=${plotdev}
   paldef device=${plotdev}
   lutgrey device=${plotdev}
   display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

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
   set bnd = "(${xl}:${xu},${yl}:${yu},)"
else
   set bnd = " "
endif

# Do the averaging in the selected spatial region.
if ( $blockave == "TRUE" ) then
   compave "in=${inname}${bnd} compress=[${cmpfac}] out=${cmpfile} trim align=origin"
else
   set cmpfile = "${inname}${bnd}"
endif

# Create the multi-spectrum plot.
# ===============================

# This also helps making the database grow to unwieldy sizes.
gdclear device=${plotdev}

echo " "
echo "      Plotting:"

clinplot ndf=${cmpfile} device=${plotdev} lpstyle="colour(curve)=red" \
         axes extaxes axes lpmargin="[0.4,0.15,0.15,0.4]" \
         style="title=${infile}" reset

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/gridspec* >& /dev/null     
rmdir ${tmpdir}/${user} >& /dev/null

#!/bin/csh -v
#+
#  Name:
#     ripper.csh
#
#  Purpose:
#     Extracts a one-dimensional spectrum from a three-dimensional IFU NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     ripper [-i filename] [-o filename] [-p]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     package.  It reads a three-dimensional IFU NDF datacube as input, 
#     presents you with a white-light image of the cube and allows you to
#     select an X-Y position using the cursor.  It then extracts (and
#     optionally displays) the spectrum for that X-Y position.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
#     -o filename
#       The filename for the output spectrum.  By default the script will 
#       prompt for the name of the output file.
#     -p
#       The script will plot the extracted spectrum to the current display 
#       as well as saving it to an NDF file. [FALSE] 
#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     04-SEP-2000 (AALLAN):
#       Original version.
#     06-SEP-2000 (AALLAN):
#       Modified to use PUTAXIS A-Task
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS
#     23-NOV-2000 (AALLAN):
#       Added -i and -o command line options.
#     31-DEC-2000 (AALLAN):
#       Added error trap to image click.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September  2 (MJC):
#       Replaced PUTAXIS with KAPPA:SETAXIS in WCS mode.  Some tidying:
#       remove tabs, spelling corrections.  Added section headings in the code.
#       Avoid :r.
#     2005 October 11 (MJC):
#       Fixed bug converting the cursor position into negative pixel indices.
#     {enter_further_changes_here}
#
#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils
#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Clean up from previous runs.
rm -f ${tmpdir}/${user}/rip* >& /dev/null

# Do variable initialisation.
set tmpuser = "${tmpdir}/${user}"
if ( ! -e $tmpuser ) mkdir $tmpuser >& /dev/null
set tmpfile = "${tmpdir}/${user}/rip_cursor.tmp"
set colfile = "${tmpdir}/${user}/rip_col"
touch $tmpfile

set plotspec = "false"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"

# Handle any command-line arguements.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output ripped spectrum
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectrum
      set plotspec = "true"
      shift args
      breaksw
   endsw   
end

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain details of the input cube.
# =================================

# Get the input filename.
if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
   set infile = ${infile:r}
endif

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# Check that it exists.
if ( ! -e ${infile}.sdf ) then
   echo "RIPPER_ERR: ${infile}.sdf does not exist."
   rm -f ${tmpfile} >& /dev/null
   exit  
endif

# Find out the cube dimensions.
ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "RIPPER_ERR: ${infile}.sdf is not a datacube."
   rm -f ${tmpfile} >& /dev/null
   exit  
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

# Show the white-light image.
# ===========================

# Collapse white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile} axis=3" >& /dev/null

# Setup the plot device.
set plotdev = "xwin"

# Display the collapsed image.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >& /dev/null

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Setup exit condition.
set prev_xpix = 1
set prev_ypix = 1

# Loop marker for spectral extraction.
extract:

# Grab X-Y position.
echo " "
echo "  Left click to extract spectrum."

cursor showpixel=true style="Colour(marker)=2" plot=mark \
       maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${tmpfile}

# Wait for CURSOR output then get X-Y co-ordinates from 
# the temporary file created by KAPPA:CURSOR.
while ( ! -e ${tmpfile} ) 
   sleep 1
end

# Grab the position.
set pos = `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
set xpix = `calc exp="nint(nint($pos[1]+0.5))" prec=_REAL`
set ypix = `calc exp="nint(nint($pos[2]+0.5))" prec=_REAL`

# Check for the exit conditions.
if ( $prev_xpix == $xpix && $prev_ypix == $ypix ) then
   goto cleanup
else if ( $xpix == 1 && $ypix == 1 ) then
   rm -f ${curfile}
   touch ${curfile}
   goto extract
else
   set prev_xpix = $xpix
   set prev_ypix = $ypix
endif

# Extract and plot the selected spectrum.
# =======================================

echo " "	 
echo "      Extracting:"
echo "        (X,Y) pixel: ${xpix},${ypix}"
echo " "

# Get the output filename.
if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
   set outfile = ${outfile:r}
   echo " "
endif

# Extract the spectrum from the cube.
echo "      Output NDF:"
echo "        File: ${outfile}.sdf"
ndfcopy "in=${infile}($xpix,$ypix,) out=${outfile} trim trimwcs=true"
settitle "ndf=${outfile} title='Pixel ($xpix,$ypix)'"

# Check to see if the output file has an AXIS structure.
set axis = `parget axis ndftrace`

# If not, create an array of axis centres derived from the current WCS Frame.
if ( ${axis} == "FALSE" ) then
   setaxis "ndf=${outfile} dim=1 mode=wcs comp=Centre" >& /dev/null
   echo "        Axes: Adding AXIS centres."
endif

# Check to see if we need to plot the output spectrum.
if ( ${plotspec} == "true" ) then
   linplot ${outfile} device=${plotdev} mode=histogram \
           style="Colour(curves)=1" >& /dev/null
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpfile} >& /dev/null
rm -f ${colfile}.sdf >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

#!/bin/csh
#+
#  Name:
#     compare.csh
#
#  Purpose:
#     Compares multiple extracted spectra from a three-dimensional IFU NDF
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     compare
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and
#     presents you with a white-light image of the cube.  You can then
#     select an X-Y position using the cursor.  The script will extract and
#     display this spectrum next to the white-light image.  You can then
#     select another X-Y position using the cursor, and the script will
#     display this spectrum as well, allowing a comparison of the two.
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
#     09-NOV-2000 (AALLAN):
#       Original version.
#     12-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#     23-NOV-2000 (AALLAN):
#       Added interrupt handler.
#     30-JAN-2001 (AALLAN):
#       Fixed the ADASS fixes
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September  5 (MJC):
#       Some tidying of grammar, punctutation, and spelling.  Added section
#       headings in the code.  Attempt removal of files silently.  Avoid :r.
#     2005 October 11 (MJC):
#       Fixed bugs converting the cursor position into negative pixel indices.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.
#     {enter_further_changes_here}
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
rm -f ${tmpdir}/${user}/comp* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/comp_col"
set specone = "${tmpdir}/${user}/comp_s1"
set spectwo = "${tmpdir}/${user}/comp_s2"
set statsfile = "${tmpdir}/${user}/comp_stats.txt"

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Set up the plot device.
set plotdev = "xwin"
gdclear device=${plotdev}

# Obtain details of the input cube.
# =================================

# Get the input filename.
echo -n "NDF input file: "
set infile = $<
set infile = ${infile:r}
echo " "

# Obtain the name sans any section.
set inname = `echo $infile | \
              awk '{if (index($0,"(") > 0) print substr($0,1,index($0,"(")-1); else print $0}'`

echo " "
echo "      Input NDF:"
echo "        File: ${inname}.sdf"

# Check that it exists.
if ( ! -e ${infile}.sdf ) then
   echo "COMPARE_ERR: ${inname}.sdf does not exist."
   exit
endif

# Find out the cube dimensions.
ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "COMPARE_ERR: ${infile} is not a datacube."
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

# Collapse the white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile} axis=3" >& /dev/null 

# Setup the graphics window.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# Create graphics-database frames for the graphical elements.
picdef "mode=cl fraction=[0.4,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="whitelight"

picdef "mode=tr fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="specone" 

picdef "mode=br fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="spectwo" 

# Display the collapsed image.
picsel label="whitelight" device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Setup the exit condition.
set prev_xgrid = 1
set prev_ygrid = 1

# Loop marker for spectral extraction
upp_cont:

# Grab an X-Y position.
   echo " "
   echo "  Left click to extract a spectrum."
   echo "  Right click to exit program."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Check for the exit conditions.
   if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
      goto cleanup
   else if ( $xgrid == 1 && $ygrid == 1 ) then
      goto upp_cont
   else
      set prev_xgrid = $xgrid
      set prev_ygrid = $ygrid
   endif

# Extract and plot the selected spectrum.
# =======================================

   echo " "
   echo "      Extracting:"
   echo "        (X,Y) pixel             : ${xgrid},${ygrid}"

# Extract the spectrum from the cube.
   ndfcopy "in=${infile}($xgrid,$ygrid,) out=${specone} trim=true trimwcs=true"
   settitle "ndf=${specone} title='Pixel (${xgrid},${ygrid})'"

# Change graphics-database frame.
   picsel label="specone" device=${plotdev}

# Plot the ripped spectrum.
   linplot ${specone} device=${plotdev} mode=histogram style="Colour(curves)=2" >& /dev/null

# Show statistics of the spectrum.
   stats ndf=${specone} > ${statsfile}
   rm -f ${specone}.sdf >& /dev/null

# This is not robust if the output of STATS changes!  It would be better to
# report from the line starting with "Title" until the end. --MJC
   cat ${statsfile} | tail -14
   rm -f ${statsfile} >& /dev/null

# Go back to the white-light image.
   picsel label="whitelight" device=${plotdev}

# Loop marker for spectral extraction
low_cont:

# Grab an X-Y position.
   echo " "
   echo "  Left click to extract a spectrum."
   echo "  Right click to exit program."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a XY -g

# Check for exit conditions
   if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
      goto cleanup
   else if ( $xgrid == 1 && $ygrid == 1 ) then
      goto low_cont
   else
      set prev_xgrid = $xgrid
      set prev_ygrid = $ygrid
   endif

# Extract and plot the second selected spectrum.
# ===============================================

   echo " "
   echo "      Extracting:"
   echo "        (X,Y) pixel             : ${xgrid},${ygrid}"

# Extract the spectrum from the cube.
   ndfcopy "in=${infile}($xgrid,$ygrid,) out=${spectwo} trim=true trimwcs=true"
   settitle "ndf=${spectwo} title='Pixel ($xgrid,$ygrid)'"

# Change graphics-database frame.
   picsel label="spectwo" device=${plotdev}

# Plot the ripped spectrum.
   linplot ${spectwo} device=${plotdev} style="Colour(curves)=3" >& /dev/null

# Extract the statistics.
   stats ndf=${spectwo} > ${statsfile}
   rm -f ${spectwo}.sdf >& /dev/null
   cat ${statsfile} | tail -14
   rm -f ${statsfile} >& /dev/null

goto upp_cont

# Clean up.
# =========
cleanup:

rm -f ${colfile}.sdf >& /dev/null
rm -f ${specone}.sdf >& /dev/null
rm -f ${spectwo}.sdf >& /dev/null
rm -f ${statsfile} >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

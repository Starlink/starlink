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
#     This shell script reads a three-dimensional IFU NDF datacube as input,
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
#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.
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
#     2005 November 3 (MJC):
#       Add options waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script that
#       also checks for a degenerate third axis.
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
rm -f ${tmpdir}/${user}/rip* >& /dev/null

# Do variable initialisation.
set tmpuser = "${tmpdir}/${user}"
if ( ! -e $tmpuser ) mkdir $tmpuser >& /dev/null
set colfile = "${tmpdir}/${user}/rip_col"

set plotspec = "false"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"

# Handle any command-line arguments.
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
   case *:     # rubbish disposal
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

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s ripper
if ( $status == 1 ) exit

# Show the white-light image.
# ===========================

# Collapse white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null

# Setup the plot device.
set plotdev = "xwin"

# Display the collapsed image.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" reset >& /dev/null

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Setup exit condition.
set prev_xgrid = 1
set prev_ygrid = 1

# Loop marker for spectral extraction.
extract:

# Grab X-Y position.
echo " "
echo "  Left click to extract spectrum."
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Check for the exit conditions.
if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
   goto cleanup
else if ( $xgrid == 1 && $ygrid == 1 ) then
   goto extract
else
   set prev_xgrid = $xgrid
   set prev_ygrid = $ygrid
endif

# Extract and plot the selected spectrum.
# =======================================

echo " "
echo "      Extracting:"
echo "        (X,Y) pixel: ${xgrid},${ygrid}"
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
ndfcopy "in=${infile}($xgrid,$ygrid,) out=${outfile} trim trimwcs=true"
settitle "ndf=${outfile} title='Pixel ($xgrid,$ygrid)'"

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

rm -f ${colfile}.sdf >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

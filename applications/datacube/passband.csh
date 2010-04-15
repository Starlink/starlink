#!/bin/csh -x
#+
#  Name:
#     passband.csh
#
#  Purpose:
#     Displays multiple passband images from a three-dimensional IFU NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     passband [-i filename] [-o filename] [-z/+z]
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and
#     presents you with a white-light image of the cube.  You can then
#     select and X-Y position using the cursor.  The script will extract
#     and display this spectrum next to the white-light image.  You can
#     then select a spectral range using the cursor and the script will
#     display a passband image of the cube in that spectral range.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
#     -o filename
#       An output two-dimensional NDF of the passband image.  By default the
#       output will only be displayed.
#     -z
#       The script will automatically prompt to select a region to
#       zoom before prompting for the region of interest. [TRUE]
#     +z
#       The program will not prompt for a zoom before requesting the region
#       of interest.  [FALSE]
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
#     20-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     23-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#       Added -i and -o command line options.
#       Added on interrupt handler.
#       Added +-z command line options.
#     31-DEC-2000 (AALLAN):
#       Allowed one-character responses to yes/no prompts.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}.
#     2005 September  2 (MJC):
#       Replaced PUTAXIS with KAPPA:SETAXIS in WCS mode.  Some tidying:
#       remove tabs, spelling corrections.  Added section headings in the
#       code.  Avoid :r.
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
#       also checks for a degenerate third axis.  Reset KAPPA:DISPLAY
#       parameters.
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
rm -f ${tmpdir}/${user}/pass* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/pass_col"
set pasfile = "${tmpdir}/${user}/pass_pas"
set spectral = "${tmpdir}/${user}/pass_rip"
set statsfile = "${tmpdir}/${user}/pass_stats.txt"

set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotzoom = "ASK"

# Setup the plot device.
set plotdev = "xwin"

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
   case -o:    # output passband image to file
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -z:    # zoom
      set gotzoom = "TRUE"
      shift args
      breaksw
   case +z:    # not zoom
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

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s passband
if ( $status == 1 ) exit

# Show the white-light image.
# ===========================

# Collapse the white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile}${ndf_section} out=${colfile} axis=3 estimator=mean" >& /dev/null
settitle "ndf=${colfile} title='White-light Image'"

# Setup the graphics window.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# Display the collapsed image.
picbase device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Setup the exit condition.
set prev_xgrid = 1
set prev_ygrid = 1

# Loop marker for spectral extraction.
extract:

# Select the X-Y position using the cursor.  Plots a cross at the chosen
# pixel position.  Pixel co-ordinates returned in $xgrid and $ygrid.
echo " "
echo "  Left click to extract spectrum."
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Check for exit condtions.
if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
   goto cleanup
else if ( $xgrid == 1 && $ygrid == 1 ) then
   goto extract
else
   set prev_xgrid = $xgrid
   set prev_ygrid = $ygrid
endif

# Extract the spectrum.
# =====================
echo " "
echo "      Extracting:"
echo "        (X,Y) pixel             : ${xgrid},${ygrid}"

# Extract the spectrum from the cube and give it an identifiable TITLE.
ndfcopy "in=${infile}($xgrid,$ygrid,) out=${spectral} trim=true trimwcs=true" >& /dev/null
settitle "ndf=${spectral} title='Pixel (${xgrid},${ygrid})'"

# Obtain the zoom range interactively.
# ====================================

# Plot the ripped spectrum.
linplot ${spectral} device=${plotdev} mode=histogram style="Colour(curves)=1" >& /dev/null
stats ndf=${spectral} > ${statsfile}

# Report the results.
cat ${statsfile} | tail -14
rm -f ${statsfile}

# Zoom if required.
if ( ${gotzoom} == "ASK") then
   echo " "
   echo -n "Zoom in (yes/no): "
   set zoomit = $<
else if ( ${gotzoom} == "TRUE") then
   set zoomit = "yes"
else
   set zoomit = "no"
endif

if ( ${zoomit} == "yes" || ${zoomit} == "y" ) then

# Get the lower limit.
# --------------------
   echo " "
   echo "  Left click on lower zoom boundary."

# Returns cursor position in $pos.
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
   set low_z = $xpos

# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."

# Returns cursor position in $pos.
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
   set upp_z = $pos[1]

   echo " "
   echo "      Zooming:"
   echo "        Lower Boundary: ${low_z}"
   echo "        Upper Boundary: ${upp_z}"

# Replot the spectrum.
   linplot ${spectral} xleft=${low_z} xright=${upp_z} mode=histogram \
           device=${plotdev} style="Colour(curves)=1" >& /dev/null
endif

# Obtain the spectral range interactively.
# ========================================

# Get the lower limit.
# --------------------
echo " "
echo "  Left click on lower boundary."

# Returns cursor position in $pos.
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a X
set low = $xpos

# Get the upper limit.
# --------------------

echo "  Left click on upper boundary."

# Returns cursor position in $pos.
source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a X
set upp = $xpos

echo " "
echo "      Passband:"
echo "        Lower Boundary: ${low}"
echo "        Upper Boundary: ${upp}"

# Create the passband image.
# ==========================

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# Inform the user.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
echo "        ${slabel} : ${low}--${upp} $sunits"

# Collapse the white-light image.
collapse "in=${infile}${ndf_section} out=${pasfile} estimator=sum" \
         "axis=3 low=${low} high=${upp}" >& /dev/null
settitle "ndf=${pasfile} title='$low--$upp'"

# Display the passband image.
# ===========================

# Clean up the graphics device.
gdclear device=${plotdev}

# Setup two plotting frames.
echo "      Plotting:"

picdef "mode=tl fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="left"

picdef "mode=cr fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="right"

# Display the white-light image on the left and the passband image on
# the right.
echo "        Left: White-light image."
picsel label="left" device=${plotdev}
display ${colfile} device=${plotdev} mode=SCALE \
        low='!' high='!' reset >&/dev/null

echo "        Right: Passband image (${low}--${upp} ${sunits})"
picsel label="right" device=${plotdev}
display ${pasfile} device=${plotdev} mode=SCALE \
        low='!' high='!' reset >&/dev/null

# Save passband image as an NDF.
# ==============================

if ( ${gotoutfile} == "TRUE" ) then

# Allow the user to enter the NDF name or filename.
   set outfile = ${outfile:r}
   echo "      Output NDF:"
   echo "        Creating: ${outfile}.sdf"
   ndfcopy "in=${pasfile} out=${outfile}"

# Check to see if the NDF has an AXIS structure.  If one does not exist,
# create an array of axis centres, derived from the current WCS Frame, along
# each axis.
   set axis = `parget axis ndftrace`
   if ( ${axis} == "FALSE" ) then
      echo "        Axes: Creating AXIS centres."
      setaxis "ndf=${outfile} dim=1 mode=wcs comp=Centre" >& /dev/null
      setaxis "ndf=${outfile} dim=2 mode=wcs comp=Centre" >& /dev/null
   endif
endif

# Cleanup.
# ========

cleanup:
rm -f ${colfile}.sdf >&/dev/null
rm -f ${pasfile}.sdf >&/dev/null
rm -f ${spectral}.sdf >&/dev/null
rm -f ${statsfile} >&/dev/null
rmdir ${tmpdir}/${user} >& /dev/null

#!/bin/csh
#+
#  Name:
#     step.csh
#
#  Purpose:
#     Steps through the each X-Y plane of a three-dimensional IFU NDF
#     in the spectral direction using KAPPA:DISPLAY to display the output.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     step [-i filename] [-l number] [-p] [-s number] [-u number]
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and
#     allows you to step through the datacube in the spectral direction
#     in slices.  The output goes to files and (optionally) to the screen.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
#     -l number
#       Lower spectral-axis bound of the region of interest.
#     -p
#       The script will plot the extracted images to the current display
#       as well as saving it to an NDF file. [FALSE]
#     -s number
#       Spectral-axis step size for each passband chunk.
#     -u number
#       Upper spectral-axis bound of the region of interest.
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
#     05-SEP-2000 (AALLAN):
#       Original version.
#     06-SEP-2000 (AALLAN):
#       Modified to use GETBOUND A-Task
#     06-SEP-2000 (AALLAN):
#       Modified to use PUTAXIS A-Task
#     20-SEP-2000 (AALLAN):
#       Shifted some floating point calculation to use the CALC A-task
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS
#     21-NOV-2000 (AALLAN):
#       Moved image display to end of script.
#     23-NOV-2000 (AALLAN):
#       Added interrupt handler.
#       Added -i, -l, -u and -s command line options.
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September  1 (MJC):
#       Replaced GETBOUND with NDFTRACE, accessing frame-bound
#       parameters FLBND/FUBND.
#     2005 September  2 (MJC):
#       Replaced PUTAXIS with KAPPA:SETAXIS in WCS mode.  Some tidying:
#       spelling & punctuation corrections, sorted parameters in
#       alphabetical order.  Added section headings in the code.  Replace
#       explicit wavelength in prompts with the current WCS Frame's label for
#       the spectral axis, and also used the corresponding units in the
#       output commentary.
#     2005 November 3 (MJC):
#       Plot the image slices in an optimally shaped grid rather than
#       full size in quick succession, and thereby permit comparison.
#       Surmised the units for a UK data-cube format NDF.  Add options
#       waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script that
#       also checks for a degenerate third axis.
#    {enter_further_changes_here}
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
rm -f ${tmpdir}/${user}/step* >& /dev/null
rm -f chunk_*.sdf >& /dev/null

# Do variable initialisation.
set pltimg = "FALSE"
set gotinfile = "FALSE"
set gotupper = "FALSE"
set gotlower = "FALSE"
set gotstep = "FALSE"

mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/step_col"

# Handle any command-line arguments.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input three-dimensional IFU NDF file
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -l:    # lower spectral-axis bound
      shift args
      set gotlower = "TRUE"
      set lower = $args[1]
      shift args
      breaksw
   case -p:    # plot each chunk?
      set pltimg = "TRUE"
      shift args
      breaksw
   case -s:    # step in spectral axis
      shift args
      set gotstep = "TRUE"
      set chunk = $args[1]
      shift args
      breaksw
   case -u:    # upper spectral-axis bound
      shift args
      set gotupper = "TRUE"
      set upper = $args[1]
      shift args
      breaksw
   case *:     # rubbish disposal
      shift args
      breaksw
   endsw
end

# Do package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s step
if ( $status == 1 ) exit

# Get the spectral range.
set wlbnd = `parget flbnd ndftrace`
set wubnd = `parget fubnd ndftrace`

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# Check for the old-fashioned UK data-cube format that predates the
# SpecFrame.  Test for non-null units if there is no SpecFrame.  Set the
# label to something clearer than LAMBDA or Axis 3.
if ( "${slabel}" == "LAMBDA" || "${slabel}" == "Axis 3" ) then
   set slabel = "Wavelength"
   if ( "$sunits" == "" ) then
      set sunits = "Angstrom"
   endif
endif

# Inform the user.
echo "        ${slabel} bounds : ${wlbnd[3]}:${wubnd[3]} ${sunits}"
echo " "

# Obtain step limits and interval.
# ================================

# Get the upper and lower bounds and chunk size.
if ( ${gotlower} == "FALSE" ) then
   echo -n "Lower ${slabel}-axis bound: "
   set lower = $<
endif

if ( ${gotupper} == "FALSE" ) then
   echo -n "Upper ${slabel}-axis bound: "
   set upper = $<
endif

if ( ${gotstep} == "FALSE" ) then
   echo -n "${slabel} step size: "
   set chunk = $<
endif
echo " "

echo "      Stepping:"
echo "        Range: ${lower}--${upper} ${sunits}"
echo "        Step : ${chunk}"
echo " "

# Setup the current chunk.
set curr_low = $lower
set curr_upp = `calc exp="'${lower} + ${chunk}'"`

# Collapse the current chunk.
# ===========================

# Step through the spectral-axis range.
set counter = 1
while ( `echo "if ( $curr_upp <= $upper) 1" | bc` )
   echo "      Collapsing:"
   echo "        White-light image: ${dims[1]} x ${dims[2]}"
   echo "        ${slabel} bounds" ": ${curr_low}--${curr_upp} ${sunits}"

# Collapse the white-light image.
   collapse "in=${infile}${ndf_section} out=${colfile} " \
            "axis=3 low=${curr_low} high=${curr_upp}" >& /dev/null

# Check to see whether or not to output the chunk.
   set outfile = "chunk_${counter}"
   echo "      Output NDF:"
   echo "        File: ${outfile}.sdf"

   ndfcopy "in=${colfile} out=${outfile}"

# Test for an AXIS structure.  If one does not exist, create an array of
# axis centres, derived from the current WCS Frame, along each axis.
   set axis = `parget axis ndftrace`
   if ( ${axis} == "FALSE" ) then
      setaxis "ndf=${outfile} dim=1 mode=wcs comp=Centre" >& /dev/null
      setaxis "ndf=${outfile} dim=2 mode=wcs comp=Centre" >& /dev/null
      echo "        Axes: Adding AXIS centres."
   endif
   settitle "ndf=${outfile} title='${curr_low}--${curr_upp} ${sunits}'"
   echo "        Title: Setting to ${curr_low}--${curr_upp} ${sunits}"
   echo " "

# Pause and increment the chunk variables.
   sleep 1
   set curr_low = $curr_upp
   set curr_upp = `calc exp="'${curr_upp} + ${chunk}'"`
   @ counter++

   rm -f ${colfile}.sdf >& /dev/null
end

# Correct to the actual number of chunks.
@ counter--

# Plot the collapsed images.
# ==========================

# Setup the plot device.
set plotdev = "xwin"

# Display the collapsed image, if required.
if ( ${pltimg} == "TRUE" ) then
   set margin = 0.15

# Form grid in which to display all the channels.
# -----------------------------------------------

# Use the whole plotting area.
   gdclear device=${plotdev}

# Get the aspect ratio of the current picture.  x1 and y1 are zero
# for the base picture.
   gdstate device=${plotdev} frame=basepic >& /dev/null
   set xb = `parget x2 gdstate`
   set yb = `parget y2 gdstate`
   set pasp = `calc exp="'${xb}/${yb}'"`

# Get the aspect ratio of the image.
   set iasp = `calc exp="'${dims[1]}/${dims[2]}'"`

# Find the ratio of the aspect ratios.  The idea is to match the shape
# of the images best to the plotting region, and tile accordingly.
# So if this ratio is near 1.0, the number of tiles in each axis
# should be the same, i.e. the next enclosing square.  Smaller than
# 1.0 it means that more images will fit across the width of the
# base picture than will fit the height.
   set asp = `calc exp="'sqrt(${pasp}/${iasp})'"`

# Now find the number of tiles along the side of a square grid to
# accommodate all the channels.
   set gm = `calc exp="'nint(sqrt(${counter})+0.4999)'"`

# Determine optimum shape of the grid of pictures.
# ------------------------------------------------

# To determine the best ratio, create various ratios around the square
# +/-3 tiles.
   set grid_range = 3

# The image has larger aspect ratio than the picture.  There will be fewer
# tiles along the x direction than the y.
   if ( `echo "if ( ${asp} <= 1.0 ) 1" | bc` ) then
      set i = `calc exp="'max(1,${gm}-${grid_range})'"`
      @ j = $gm
      set ip = 0
      set jp = $grid_range

# The image has smaller aspect ratio than the picture.  There will be fewer
# tiles along the y direction than the x.
   else
      @ i = $gm
      set j = `calc exp="'max(1,${gm}-${grid_range})'"`
      set ip = $grid_range
      set jp = 0
   endif

   set best = -1
   set xp = -1
   set yp = -1
   set jps = $j

# Loop through the various combinations of pictures along each axis.
   while ( $i <= $gm + $ip )
      while ( $j <= $gm + $jp )

# Find the number of tiles in the arrangement.
         @ kount = $i * $j

# Can reject immediately if the number of grid frames is fewer than
# the number of images.
         if ( $kount >= $counter ) then

# We need to determine the fraction of the plotting area that would
# contain the channel images.  This is the fraction of used frames
# in the grid, times the fraction of each frame displaying the image.
            set used = `calc exp="'${counter}/${kount}'"`

# The formula for area depends on whether the image is limited in x or
# y.  Find the aspect ratio of a single grid.  If this is greater than
# the image aspect ratio, means the image is y-axis limited.
            set gasp = `calc exp="'${pasp}*${j}/${i}'"`
            if ( `echo "if ( ${gasp} >= ${iasp} ) 1" | bc` ) then
               set area = `calc exp="'((1.0-2.0*${margin})**2)*${iasp}/${gasp}'"`
            else
               set area = `calc exp="'((1.0-2.0*${margin})**2)/${iasp}*${gasp}'"`
            endif

# Determine the efficiency.
            set effic = `calc exp="'${area}*${used}'"`

# Test whether this gets closer to the desired aspect ratio, while still
# offering sufficient slots for the number of images.
            if (  `echo "if ( ${effic} > ${best} ) 1" | bc` ) then
               set best = $effic
               set xp = $i
               set yp = $j
            endif
         endif
         @ j++
      end
      set j = $jps
      @ i++
   end

# Set up the display.
# -------------------
   echo "      Display:"
   paldef device=${plotdev}
   lutgrey device=${plotdev}

# Form the grid of frame pictures within the base picture.
   picdef device=${plotdev} fraction=1.0 mode=array nooutline prefix="step"\
          xpic=${xp} ypic=${yp} >& /dev/null

# Display each channel image in successive grid frames.
# ------------------------------------------------------
   set i = 1
   while ( $i <= $counter )
      set file = "chunk_"${i}

# Select the next picture in Fortran order.
      picsel device=${plotdev} label=step${i}
      echo "        ${file}.sdf"

# Display with the spectral range of each image in the title.
      display "${file} device=${plotdev} mode=SIGMA sigmas=[-3,2] " \
              "style='DrawTitle,Size(Title)=1.3' margin=${margin} " \
              "reset" >& /dev/null

      @ i++
   end
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/step* >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

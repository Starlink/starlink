#!/bin/csh
#+
#  Name:
#     stacker.csh
#
#  Purpose:
#     Plots a stack of spectra extracted from a three-dimensional IFU NDF.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     stacker [-i filename] [-n number] [-o number] [-z/+z]
#
#  Description:
#     This shell script reads a three-dimensional IFU NDF datacube and
#     presents you with a white-light image of the cube.  You can then
#     select a number of X-Y position using the cursor.  The script
#     will then extract and display these spectra in a `stack' with each
#     spectrum plotted offset vertically from the previous one in the stack.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.   By default the script will prompt for
#       the input file.
#     -n number
#       Number of spectra to extract.
#     -o number
#       Offset between the spectra in the stack.
#     -z
#       The script will automatically prompt the user to select a region to
#       zoom before prompting for the region of interest.  [TRUE]
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
#     24-NOV-2000 (AALLAN):
#       Original version.
#     31-DEC-2000 (AALLAN):
#       Allowed 1 character responses to yes/no prompts
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September 6 (MJC):
#       Some tidying of grammar, punctuation, and spelling.  Added section
#       headings in the code.  Attempt removal of files silently.
#       Avoid :r.
#     2005 October 11 (MJC):
#       Fixed bug converting the cursor position into negative pixel indices.
#     2005 October 26 (MJC):
#       Use STATS to obtain the minimum value of the first spectrum to
#       define the lower y bound of the plot rather than fix at zero.
#     2005 November 3 (MJC):
#       Add options waste disposal.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and
#       an NDF section are supplied; this is via the new checkndf script
#       that also checks for a degenerate third axis.
#     2006 March 10 (MJC):
#       Find upper limit of the plots' ordinate so as to include all
#       spectra fully regardless of the offsets.  Also allow a 2-percent
#       margin at the top and bottom of the plot to separate the spectra
#       from the axes.
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
rm -f ${tmpdir}/${user}/stak* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/stak_col"

set gotinfile = "FALSE"
set gotnum = "FALSE"
set gotoff = "FALSE"
set gotzoom = "ASK"

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
   case -n:    # number of spectra to stack
      shift args
      set gotnum = "TRUE"
      set numspec = $args[1]
      shift args
      breaksw
   case -o:    # offset for each spectra
      shift args
      set gotoff = "TRUE"
      set offset = $args[1]
      shift args
      breaksw
    case -p:    # postscript output?
      set gotpost = "TRUE"
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

# Obtain details of the input cube.
# =================================

# Obtain the NDF if it is not supplied on the command line.  Validate that
# the NDF exists and is a cube.  Obtain $infile, $ndf_section, and $dims.
source ${DATACUBE_DIR}/checkndf.csh -s stacker
if ( $status == 1 ) exit

# Show the white-light image.
# ===========================

# Collapse white-light image.
echo "      Collapsing:"
echo "        White-light image: ${dims[1]} x ${dims[2]}"
echo "xollapse in=${infile}${ndf_section} out=${colfile} axis=3"
collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null

# Setup the plot device.
set plotdev = "xwin"

# Display the collapsed image.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null

# Form spectral stack.
# ====================

# Ask for number of spectra to grab.
if ( ${gotnum} == "FALSE" ) then
   echo " "
   echo -n "Number of spectra: "
   set numspec = $<
endif

echo " "

# Loop through the spectra.
set counter = 1
while ( $counter <= $numspec )

# Setup the extraction file.
   set specfile = "${tmpdir}/${user}/stak_${counter}.sdf"

# Grab an X-Y position.
   echo " "
   echo "  Left click on pixel to be extracted."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Extract the spectrum.
   echo " "
   echo "      Extracting:"
   echo "        (X,Y) pixel: ${xgrid},${ygrid}"

# Extract the spectrum from the cube.
   ndfcopy "in=${infile}($xgrid,$ygrid,) out=${specfile} trim=true trimwcs=true"

# Find the minimum in the first spectrum.
   if ( $counter == 1 ) then
     stats "${specfile}" >& /dev/null
     set ybot = `parget minimum stats`
   endif

# Increment spectrum counter
   @ counter = $counter + 1

end

# Apply offsets to spectra.
# =========================

# Why not use MLINPLOT? - MJC

# Get the offset between each spectrum.
if ( ${gotoff} == "FALSE" ) then
   echo " "
   echo -n "Offset: "
   set offset = $<
endif

echo " "
echo "      Adding:"

# Add the offset to each spectrum.
set counter = 1
set ytop = $ybot
while ( $counter <= $numspec )

   set specfile = "${tmpdir}/${user}/stak_${counter}.sdf"
   set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf"

# Do the addition.
   set specoff = `calc exp="'${offset}*(${counter}-1)'" prec=_double`

   echo "        Adding ${specoff} to spectrum ${counter}"

   cadd in=${specfile} out=${outfile} scalar=${specoff}

# Need to find the maximum value.  It may not be in the
# last spectrum even though it has the largest offset.
   stats "${outfile}" >& /dev/null
   set outmax = `parget maximum stats`
   set ytop = `calc exp="'max(${outmax},${ytop})'"`

# Increment the spectrum counter.
   @ counter = $counter + 1
end

# Give a litte breathing room to separate the curves from the
# axes.
set ytop = `calc exp="'${ytop}+0.02*((${ytop})-(${ybot}))'"`
set ybot = `calc exp="'${ybot}-0.02*((${ytop})-(${ybot}))'"`

# Create the multi-spectrum plot.
# ===============================

gdclear device=${plotdev}

echo " "
echo "      Plotting:"

# Plot each spectrum in turn in the same graphic.
set counter = $numspec
while ( $counter > 0 )

   set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf"

   echo "        Spectrum: ${counter} "

   linplot ${outfile} device=${plotdev} style="Colour(curves)=1"\
           mode=histogram clear=no ybot=${ybot} ytop=${ytop} >& /dev/null

   @ counter = $counter - 1

end

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
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
   set low_z = $xpos

# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a X
   set upp_z = $xpos

   echo " "
   echo "      Zooming:"
   echo "        Lower Boundary: ${low_z}"
   echo "        Upper Boundary: ${upp_z}"

# Create the zoomed plot.
# -----------------------
   gdclear device=${plotdev}

   echo " "
   echo "      Plotting:"

   set counter = $numspec
   while ( $counter > 0 )

      set outfile = "${tmpdir}/${user}/stak_${counter}_off"

      echo "        Spectrum: ${counter} "

      linplot ${outfile} xleft=${low_z} xright=${upp_z}\
              device=${plotdev} style="Colour(curves)=1"\
              mode=histogram clear=no ybot=${ybot} ytop=${ytop} >& /dev/null

      @ counter = $counter - 1

   end
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/stak_?.sdf >& /dev/null
rm -f ${tmpdir}/${user}/stak_?_off.sdf >& /dev/null
rm -f ${tmpdir}/${user}/stak_col.sdf >& /dev/null
rm -f ${tmpdir}/${user}/stak_cursor.tmp >& /dev/null
rmdir ${tmpdir}/${user}  >& /dev/null

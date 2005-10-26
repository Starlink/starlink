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
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     and FIGARO packages.  It reads a three-dimensional IFU NDF datacube
#     as input and presents you with a white-light image of the cube.  You
#     can then select a number of X-Y position using the cursor.  The script 
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
rm -f ${tmpdir}/${user}/stak* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/stak_cursor.tmp"
set colfile = "${tmpdir}/${user}/stak_col"
touch ${curfile}

set gotinfile = "FALSE"
set gotnum = "FALSE"
set gotoff = "FALSE"
set gotzoom = "ASK"

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

echo " "
echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# Check that it exists.
if ( ! -e ${infile}.sdf ) then
   echo "STACKER_ERR: ${infile}.sdf does not exist."
   rm -f ${curfile} >& /dev/null
   exit  
endif

# Find out the cube dimensions.
ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "STACKER_ERR: ${infile}.sdf is not a datacube."
   rm -f ${curfile} >& /dev/null
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
   
   cursor showpixel=true style="Colour(marker)=2" plot=mark \
          maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# Wait for CURSOR output then get X-Y co-ordinates from 
# the temporary file created by KAPPA:CURSOR.
   while ( ! -e ${curfile} ) 
      sleep 1
   end

# Grab the position.
   set pos = `parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.  The
# exterior NINT replaces the bug/feature -0 result with the desired 0.
   set xpix = `calc exp="nint(nint($pos[1]+0.5))" prec=_REAL`
   set ypix = `calc exp="nint(nint($pos[2]+0.5))" prec=_REAL`

# Clean up the CURSOR temporary file.
   rm -f ${curfile}
   touch ${curfile}

# Extract the spectrum.
   echo " "
   echo "      Extracting:"
   echo "        (X,Y) pixel: ${xpix},${ypix}"

# Extract the spectrum from the cube.
   ndfcopy "in=${infile}($xpix,$ypix,) out=${specfile} trim=true trimwcs=true"

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
while ( $counter <= $numspec )

   set specfile = "${tmpdir}/${user}/stak_${counter}.sdf" 
   set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf" 
   
# Do the addition.
   set specoff = `calc exp="'${offset}*(${counter}-1)'" prec=_double`

   echo "        Adding ${specoff} to spectrum ${counter}"

   cadd in=${specfile} out=${outfile} scalar=${specoff}

# Increment the spectrum counter.
   @ counter = $counter + 1
end

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
           mode=histogram clear=no ybot=${ybot} >& /dev/null

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
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}

   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set low_z = $pos[1]

# Clean up the CURSOR temporary file.
   rm -f ${curfile} >& /dev/null
   touch ${curfile}
   
# Get the upper limit.
# --------------------
   echo "  Left click on upper zoom boundary."

   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}

   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set upp_z = $pos[1]

   echo " "
   echo "      Zooming:"
   echo "        Lower Boundary: ${low_z}"
   echo "        Upper Boundary: ${upp_z}"

# Clean up the CURSOR temporary file.
   rm -f ${curfile} >& /dev/null
   touch ${curfile}

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
              mode=histogram clear=no ybot=${ybot} >& /dev/null

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

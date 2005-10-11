#!/bin/csh
#+
#  Name:
#     multistack.csh
#
#  Purpose:
#     Averages groups of spectra extracted from a three-dimensional IFU NDF and 
#     then plots these averaged spectra in a stack.
#
#  Type of Module:
#     C-shell script.
#
#  Usage:
#     multistack [-i filename] [-g number] [-n number] [-o number] [-z/+z] 
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     and FIGARO packages.  It reads a three-dimensional IFU NDF as input
#     and presents you with a white-light image of the cube.   You can
#     then select a number of X-Y positions using the cursor.  The script
#     will then group these spectra creating an average spectrum for each
#     group.  It then displays the average spectra in a "stack", where each
#     group spectrum plotted offset vertically from the prevous one in the
#     stack.
#
#  Parameters:
#     -g number
#       The number of spectra in a group.
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a three-dimensional NDF.  By default the script will prompt for the
#       input file.
#     -n number
#       The number of groups to extract.
#     -o number
#       Offset between the spectra in the stack.
#     -z 
#       The script will automatically prompt the user to select a region to
#       zoom before prompting for the region of interest.
#     +z 
#       The program will not prompt for a zoom before requesting the region
#       of interest.

#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     31-DEC-2000 (AALLAN):
#       Original version, based on stacker.csh
#     03-JAN-2001 (AALLAN):
#       Fixed some bugs
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     2005 September 6 (MJC):
#       Some tidying of grammar, punctuation, and spelling.  Added section
#       headings in the code.  Attempt removal of files silently.
#       Clarify the description.  Ordered the parameters alphabetically.
#       Avoid :r.
#     {enter_further changes_here}
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
rm -f ${tmpdir}/${user}/mstk* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user}
set curfile = "${tmpdir}/${user}/mstk_cursor.tmp"
set colfile = "${tmpdir}/${user}/mstk_col"
touch ${curfile}

set gotinfile = "FALSE"
set gotgrp = "FALSE"
set gotnum = "FALSE"
set gotoff = "FALSE"
set gotzoom = "ASK"

# Handle any command-line arguements.
set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -g:    # number of groups
      shift args
      set gotgrp = "TRUE"
      set numgrp = $args[1]
      shift args
      breaksw      
   case -i:    # input three-dimensional IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -n:    # number of spectra in each group
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

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "STACKER_ERR: ${infile}.sdf does not exist."
   rm -f ${curfile} >& /dev/null
   exit  
endif

# Check that it exists.
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

# Form spectral stack of averaged spectra.
# ========================================

# Ask for number of spectra to grab.
if ( ${gotgrp} == "FALSE" ) then
   echo " "
   echo -n "Number of groups: "
   set numgrp = $<
endif

if ( ${gotnum} == "FALSE" ) then
   echo " "
   echo -n "Number of spectra in group: "
   set numspec = $<
endif

echo " "

# Form spectral groups.
# ---------------------

# Loop through the groups.
set grpcount = 1
while ( $grpcount <= $numgrp )

# Setup the mean spectrum file.
   set grpfile = "${tmpdir}/${user}/mstk_g${grpcount}"

# Loop through the spectra.
   set counter = 1
   while ( $counter <= $numspec )

# Setup the extraction file.
      set specfile = "${tmpdir}/${user}/mstk_g${grpcount}_s${counter}"

# Grab an X-Y position.
      echo " "
      echo "  Left click on pixel to be extracted."
   
      cursor showpixel=true style="Colour(marker)=2" plot=mark \
             maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# Wait for CURSOR output.
      while ( ! -e ${curfile} ) 
         sleep 1
      end

# Grab the position.
      set pos=`parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# Get the pixel co-ordinates and convert to grid indices.
      set xpix = `echo $pos[1] | awk '{split($0,a,"."); print a[1]}'`
      set ypix = `echo $pos[2] | awk '{split($0,a,"."); print a[1]}'`
      @ xpix = $xpix + 1
      @ ypix = $ypix + 1

# Clean up the CURSOR temporary file.
      rm -f ${curfile} >& /dev/null
      touch ${curfile}

# Extract the spectrum.
# ---------------------
      echo " "
      echo "      Extracting:"
      echo "        (X,Y) pixel: ${xpix},${ypix}"

# Extract the spectrum from cube.
      ndfcopy in="${infile}($xpix,$ypix,)" out=${specfile} trim=true \
              trimwcs=true

# Add the current spectrum to the group spectrum.
# -----------------------------------------------

# If this is the first spectrum extracted, create the file to store
# the average spectrum for the group.
      if ( ${counter} == 1 ) then
         ndfcopy "in=${specfile} out=${grpfile}"
         echo "        Creating: Group ${grpcount}"

      else
         echo "        Adding: Group ${grpcount}"
         ndfcopy "in=${grpfile} out=${grpfile}_tmp"
         add in1="${grpfile}_tmp" in2=${specfile} out=${grpfile} 
         rm -f ${grpfile}_tmp.sdf >& /dev/null
      endif
      
      # increment counter
      @ counter = $counter + 1

   end
   
# Take the mean of the current group spectrum.
   ndfcopy "in=${grpfile} out=${grpfile}_tmp"
   cdiv in="${grpfile}_tmp" out=${grpfile} scalar=${numgrp}
   rm -f ${grpfile}_tmp.sdf >& /dev/null

# Increment the group counter.
   @ grpcount = $grpcount + 1

end

# At this point we have average spectra for each group.

# Apply offsets to group spectra.
# ===============================

# Get the offset between each spectrum.

if ( ${gotoff} == "FALSE" ) then
   echo " "
   echo -n "Offset: "
   set offset = $<
endif

echo " "
echo "      Adding:"

# Add offsets to each spectrum.
set grpcount = 1
while ( $grpcount <= $numgrp )

   set grpfile = "${tmpdir}/${user}/mstk_g${grpcount}" 
   set outfile = "${tmpdir}/${user}/mstk_g${grpcount}_off" 

# Do the addition.
   set specoff = `calc exp="'${offset}*(${grpcount}-1)'" prec=_double`

   echo "        Adding ${specoff} to spectrum ${grpcount}"

   cadd in=${grpfile} out=${outfile} scalar=${specoff}

# Increment the group counter.
   @ grpcount = $grpcount + 1
end

# Create the multi-spectrum plot.
# ===============================

gdclear device=${plotdev}

echo " "
echo "      Plotting:"

# Plot each group spectrum in turn in the same graphic.
set grpcount = $numgrp
while ( $grpcount > 0 )
   
   set outfile = "${tmpdir}/${user}/mstk_g${grpcount}_off" 

   echo "        Group: ${grpcount} "

   linplot ${outfile} device=${plotdev} style="Colour(curves)=1"\
           mode=histogram clear=no ybot=0 >& /dev/null

   @ grpcount = $grpcount - 1

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

   set grpcount = $numgrp
   while ( $grpcount > 0 )
   
      set outfile = "${tmpdir}/${user}/mstk_g${grpcount}_off" 

      echo "        Group: ${grpcount} "

      linplot ${outfile} xleft=${low_z} xright=${upp_z} \
              device=${plotdev} style="Colour(curves)=1" \
              mode=histogram clear=no ybot=0 >& /dev/null

      @ grpcount = $grpcount - 1

   end
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/mstk* >& /dev/null     
rmdir ${tmpdir}/${user} >& /dev/null

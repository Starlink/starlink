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
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     package.  It reads a three-dimensional IFU NDF as input and allows you
#     to step through the datacube in the spectral direction in slices.
#     The output goes to files and (optionally) to the screen.
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
#       as well as saving it to an NDF file.
#     -s number
#       Spectral-axis step size for each passband chunk.
#     -u number
#       Upper spectral-axis bound of the region of interest.  
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
   endsw      
end

# Do package setup.
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
   echo "STEP_ERR: ${infile}.sdf does not exist."
   exit  
endif

# Find out the cube dimensions.
ndftrace ndf=${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "STEP_ERR: ${infile}.sdf is not a datacube."
   exit  
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

# Report the statistics.
echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

# Get the spectral range.
set wlbnd = `parget flbnd ndftrace`
set wubnd = `parget fubnd ndftrace`

# Get the spectral label and units.
set slabel = `wcsattrib ndf=${infile} mode=get name="Label(3)"`
set sunits = `wcsattrib ndf=${infile} mode=get name="Unit(3)"`

# Inform the user.
printf "%24s%s\n" "${slabel} bounds" ": ${wlbnd[3]}:${wubnd[3]} ${sunits}"

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

echo "      Stepping:"
echo "        Range: ${lower}--${upper} ${sunits}"
echo "        Step : ${chunk}"

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
   printf "%25s%s\n" "${slabel} bounds" ": ${curr_low}--${curr_upp} ${sunits}"

# Collapse the white-light image.
   collapse "in=${infile} out=${colfile} " \
            "axis=3 low=${curr_low} high=${curr_upp}" >& /dev/null

# Check to see whether or not to output the chunk.
   set outfile = "chunk_${counter}"
   echo " "
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
   settitle "ndf=${outfile} title='${curr_low}--${curr_upp}'" 
   echo "        Title: Setting to ${curr_low}--${curr_upp}"

# Pause and increment the chunk variables.
   sleep 2
   set curr_low = `calc exp="'${curr_low} + ${chunk}'"`
   set curr_upp = `calc exp="'${curr_upp} + ${chunk}'"`
   @ counter = $counter + 1 

   rm -f ${colfile}.sdf >& /dev/null
end

# Plot the collapsed image.
# =========================

# Setup the plot device.
set plotdev = "xwin"

# Display the collapsed image, if required.
if ( ${pltimg} == "TRUE" ) then
   echo "      Display:" 
   gdclear device=${plotdev}
   paldef device=${plotdev}
   lutgrey device=${plotdev}
   foreach file ( chunk_?.sdf )
      echo "        ${file}"
      display "${file:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >& /dev/null
   end
   if ( $counter >= 10 ) then
      foreach file ( chunk_??.sdf )
         echo "        ${file}"
         display "${file:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >& /dev/null
      end
   endif
endif

# Clean up.
# =========
cleanup:

rm -f ${tmpdir}/${user}/step* >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

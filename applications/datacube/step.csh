#!/bin/csh
#+
#  Name:
#     step.csh
#
#  Purpose:
#     Steps through the each X-Y plane of a 3D IFU NDF datacube in the
#     spectral direction using KAPPA display to display the output
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     step [-i filename] [-l number] [-u number] [-s number] [-p] 
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     package. It reads a 3D IFU NDF datacube as input and allows the user
#     to step through the datacube in the spectral direction, output goes to
#     file an (optionally) to the screen.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -l number
#       Lower lambda bound of the region of interest.
#     -u number
#       Upper lambda bound of the region of interest.  
#     -s number
#       Lambda step size for each passband chunk.
#     -p 
#       The script will plot the extracted images to the current display 
#       as well as saving it to an NDF file.
#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
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
#     {enter_changes_here}
#
#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils
#-

# on interrupt
onintr cleanup

# get the user name

set user = `whoami`
set tmpdir = "/tmp"

# clean up from previous runs

rm -f ${tmpdir}/${user}/step* >& /dev/null
rm -f chunk_*.sdf >& /dev/null

# do variable initialisation

set pltimg = "FALSE"
set gotinfile = "FALSE"
set gotupper = "FALSE"
set gotlower = "FALSE"
set gotstep = "FALSE"
set args = ($argv[1-])

mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/step_col.sdf"

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# handle any command line arguements

while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input 3D IFU NDF file
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -s:    # step in lambda
      shift args
      set gotstep = "TRUE"
      set chunk = $args[1]
      shift args
      breaksw
   case -u:    # upper lambda bound
      shift args
      set gotupper = "TRUE"
      set upper = $args[1]
      shift args
      breaksw  
   case -l:    # upper lambda bound
      shift args
      set gotlower = "TRUE"
      set lower = $args[1]
      shift args
      breaksw            
   case -p:    # plot each chunk
      set pltimg = "TRUE"
      shift args
      breaksw
   endsw      
end

# get input filename

if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
endif

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "STEP_ERR: ${infile}.sdf does not exist."
   exit  
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
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

# tell the user

echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

# get wavelength range

getbound ${infile} >& /dev/null
set wlbnd = `parget lbound getbound`
set wubnd = `parget ubound getbound`

# tell the user

echo "        Lambda bounds    : ${wlbnd[3]}:${wubnd[3]}"

# get upper and lower bounds and chunk size

if ( ${gotlower} == "FALSE" ) then
   echo -n "Lower lambda bound: "
   set lower = $<
endif

if ( ${gotupper} == "FALSE" ) then
   echo -n "Upper lambda bound: "
   set upper = $<
endif

if ( ${gotstep} == "FALSE" ) then
   echo -n "Lambda step size: "
   set chunk = $<
endif

echo "      Stepping:"
echo "        Range: ${lower} - ${upper}"
echo "        Step: ${chunk}"

# setup current chunk

set curr_low = $lower
set curr_upp = `calc exp="'${lower} + ${chunk}'"`

# step through the wavelength range

set counter = 1
while ( `echo "if ( $curr_upp <= $upper) 1" | bc` )
   echo "      Collapsing:"
   echo "        White light image: ${dims[1]} x ${dims[2]}"
   echo "        Wavelength range: ${curr_low} - ${curr_upp}"

# collapse white light image
   collapse "in=${infile} out=${colfile:r} " \
            "axis=3 low=${curr_low} high=${curr_upp}" >& /dev/null

# check to see whether to output the chunk
   set outfile = "chunk_${counter}.sdf"
   echo "      Output NDF:"
   echo "        File: ${outfile}" 
   cp -f ${colfile} ${outfile}
   set axis = `parget axis ndftrace`
   if ( ${axis} == "FALSE" ) then
      putaxis "${outfile:r} spectral=3" >& /dev/null
      echo "        Axes: Adding AXIS extensions"
   endif 
   settitle "ndf=${outfile:r} title='${curr_low} - ${curr_upp}'" 
   echo "        Title: Setting to ${curr_low} - ${curr_upp}"

# pause and increment the chunk variables
   sleep 2
   set curr_low = `calc exp="'${curr_low} + ${chunk}'"`
   set curr_upp = `calc exp="'${curr_upp} + ${chunk}'"`
   @ counter = $counter + 1 
   rm -f ${colfile}
end

# display collapsed image is required
if ( ${pltimg} == "TRUE" ) then
   echo "      Display:" 
   gdclear device=xwin
   paldef device=xwin
   lutgrey device=xwin
   foreach file ( chunk_?.sdf )
      echo "        ${file}"
      display "${file:r} device=xwin mode=SIGMA sigmas=[-3,2]" >& /dev/null
   end
   if ( $counter >= 10 ) then
      foreach file ( chunk_??.sdf )
         echo "        ${file}"
         display "${file:r} device=xwin mode=SIGMA sigmas=[-3,2]" >& /dev/null
      end
   endif
endif

# cleanup

cleanup:

rm -f ${tmpdir}/${user}/step* >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

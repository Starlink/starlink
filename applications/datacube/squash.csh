#!/bin/csh
#+
#  Name:
#     squash.csh
#
#  Purpose:
#     Extract a 2D white light image from a 3D IFU NDF datacube
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     squash  [-i filename] [-o filename] [-l number] [-u number] [-p]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     package. It reads a 3D IFU NDF datacube as input and allows the user
#     to extract a specific wavelength range from the cube to form a white
#     light image.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -o filename
#       The filename for the output white light or passband image, by default
#       the script will  prompt for the name of the output file.
#     -l number
#       Lower lambda bound of the region of interest.
#     -u number
#       Upper lambda bound of the region of interest.  
#     -p 
#       The script will plot the extracted image to the current display 
#       as well as saving it to an NDF file.
#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     {enter_new_authors_here}
#
#  History:
#     04-SEP-2000 (AALLAN):
#       Original version.
#     06-SEP-2000 (AALLAN):
#       Modified to use PUTAXIS A-Task
#     20-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     23-NOV-2000 (AALLAN):
#       Added -i, -o, -l and -u command line options.
#       Added on interrupt handler.
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

rm -f ${tmpdir}/${user}/squash* >& /dev/null

# do variable initialisation

set pltimg = "FALSE"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotupper = "FALSE"
set gotlower = "FALSE"
set args = ($argv[1-])

mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/squash_col.sdf"

# do package setup

alias echo "echo > /dev/null"
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# handle any command line arguements

set args = ($argv[1-])
while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input 3D IFU NDF file
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output white light file
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
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
   case -p:    # plot output image
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

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "SQUASH_ERR: ${infile}.sdf does not exist."
   exit  
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "SQUASH_ERR: ${infile}.sdf is not a datacube."
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

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
echo "        Wavelength range: ${lower} - ${upper}"

# collapse white light image

collapse "in=${infile} out=${colfile:r} " \
         "axis=3 low=${lower} high=${upper}" >& /dev/null 
settitle "ndf=${colfile:r} title='${lower} - ${upper}'"

# display collapsed image

if ( ${pltimg} == "TRUE" ) then
   gdclear device=xwin
   paldef device=xwin
   lutgrey device=xwin   
   display "${colfile:r} device=xwin mode=SIGMA sigmas=[-3,2]" >& /dev/null
endif

# get output filename

if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
endif

# output the chunk
echo "      Output NDF:"
echo "        File: ${outfile}.sdf" 
cp -f ${colfile} ${outfile}.sdf
 
set axis = `parget axis ndftrace`
if ( ${axis} == "FALSE" ) then
   putaxis "${outfile:r} spectral=3" >& /dev/null
   echo "        Axes: Adding AXIS extensions"
endif      

# clean up
cleanup:

rm -f ${colfile}
rmdir ${tmpdir}/${user}

#!/bin/csh
#+
#  Name:
#     ripper.csh
#
#  Purpose:
#     Extract a 1D spectra from a 3D IFU NDF datacube
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     ripper [-i filename] [-o filename] [-p]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     package. It reads a 3D IFU NDF datacube as input, presents the user
#     with a white light image of the cube and allows the user to select an
#     x,y position using the cursor. It then extracts (and optionally displays)
#     the spectra for that X,Y position.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -o filename
#       The filename for the output spectra, by default the script will 
#       prompt for the name of the output file.
#     -p
#       The script will plot the extracted spectra to the current display 
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
#       Incorporated changes made to source at ADASS
#     23-NOV-2000 (AALLAN):
#       Added -i and -o command line options.
#     31-DEC-2000 (AALLAN):
#       Added error trap to image click.
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

rm -f ${tmpdir}/${user}/rip* >& /dev/null

# do variable initialisation

mkdir "${tmpdir}/${user}" >& /dev/null
set tmpfile = "${tmpdir}/${user}/rip_cursor.tmp"
set colfile = "${tmpdir}/${user}/rip_col.sdf"
touch $tmpfile

set plotspec = "false"
set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set args = ($argv[1-])

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# handle any command line arguements

while ( $#args > 0 )
   switch ($args[1])
   case -i:    # input 3D IFU NDF
      shift args
      set gotinfile = "TRUE"
      set infile = $args[1]
      shift args
      breaksw
   case -o:    # output ripped spectra
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
      shift args
      breaksw
   case -p:    # plot output spectra
      set plotspec = "true"
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
   echo "RIPPER_ERR: ${infile}.sdf does not exist."
   rm -f ${tmpfile} >& /dev/null
   exit  
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "RIPPER_ERR: ${infile}.sdf is not a datacube."
   rm -f ${tmpfile} >& /dev/null
   exit  
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

echo "      Shape:"
echo "        No. of dimensions: ${ndim}"
echo "        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}"
echo "        Pixel bounds     : ${bnd}"
echo "        Total pixels     : $pixnum"

# collapse white light image

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile:r} axis=3" >& /dev/null

# display collapsed image

gdclear device=xwin
paldef device=xwin
lutgrey device=xwin
display "${colfile:r} device=xwin mode=SIGMA sigmas=[-3,2]" >& /dev/null

# setup exit condition
set prev_xpix = 1
set prev_ypix = 1

# loop marker for spectral extraction
extract:

# grab x,y position

echo " "
echo "  Left click to extract spectra"
  
cursor showpixel=true style="Colour(marker)=2" plot=mark \
       maxpos=1 marker=2 device=xwin frame="PIXEL" >> ${tmpfile}

# wait for cursor output then get x,y co-ordinates from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${tmpfile} ) 
   sleep 1
end

# grab position

# set string = `grep "(*)" ${tmpfile}`
# set string = `echo ${string} | sed 's/(/ /'`
# set string = `echo ${string} | sed 's/)/ /'`
# set pos=`echo $string | awk '{split($0,a," ");print a[1], a[2]}'`
set pos=`parget lastpos cursor | awk '{split($0,a," ");print a[1], a[2]}'`

# get pixel co-ordinates

set xpix = `echo $pos[1] | awk '{split($0,a,"."); print a[1]}'`
set ypix = `echo $pos[2] | awk '{split($0,a,"."); print a[1]}'`
@ xpix = $xpix + 1
@ ypix = $ypix + 1

# check for exit condtions

if ( $prev_xpix == $xpix && $prev_ypix == $ypix ) then
   goto cleanup
else if ( $xpix == 1 && $ypix == 1 ) then
   rm -f ${curfile}
   touch ${curfile}
   goto extract
else
   set prev_xpix = $xpix
   set prev_ypix = $ypix
endif

echo " "	 
echo "      Extracing:"
echo "        (X,Y) pixel: ${xpix},${ypix}"

# get output filename

if ( ${gotoutfile} == "FALSE" ) then
   echo -n "NDF output file: "
   set outfile = $<
endif

# extract spectra from cube

echo "      Output NDF:"
echo "        File: ${outfile}.sdf"
ndfcopy "in=${infile}($xpix,$ypix,) out=${outfile} trim=true trimwcs=true"
settitle "ndf=${outfile} title='Pixel ($xpix,$ypix)'"

# check to see if the output file has AXIS extensions
set axis = `parget axis ndftrace`

if ( ${axis} == "FALSE" ) then
   putaxis "${outfile} spectral=1" >& /dev/null
   echo "        Axes: Adding AXIS extensions"
endif

# check to see if we need to plot the output spectra

if ( ${plotspec} == "true" ) then
   linplot ${outfile} device=xwin style="Colour(curves)=1" >& /dev/null
endif

# clean up
cleanup:

rm -f ${tmpfile}
rm -f ${colfile}
rmdir $tmpdir/${user}

#!/bin/csh
#+
#  Name:
#     compare.csh
#
#  Purpose:
#     Comparison of multiple extracted spectra from a 3D IFU NDF
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     compare
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     FIGARO and DATACUBE packages. It reads a 3D IFU NDF datacube as input
#     and presents the user with a white light image of the cube. The user
#     can then select and x,y position using the cursor. The script will
#     extract and display this spectra next to the white light image. The 
#     user can then select another x,y position using the cursor and the 
#     script will display this spectra as well, allowing comparison of the two.

#
#  Authors:
#     AALLAN: Alasdair Allan (Starlink, Keele University)
#     {enter_new_authors_here}
#
#  History:
#     09-NOV-2000 (AALLAN):
#       Original version.
#     12-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#     23-NOV-2000 (AALLAN):
#       Added interrupt handler.
#     30-JAN-2001 (AALLAN):
#       Fixed the ADASS fixes
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

rm -f ${tmpdir}/${user}/comp* >& /dev/null

# do variable initialisation

mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/comp_cursor.tmp"
set colfile = "${tmpdir}/${user}/comp_col.sdf"
set specone = "${tmpdir}/${user}/comp_s1.sdf"
set spectwo = "${tmpdir}/${user}/comp_s2.sdf"
set statsfile = "${tmpdir}/${user}/comp_stats.txt"
touch ${curfile}

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# setup plot device

set plotdev = "xwin"
gdclear device=${plotdev}
gdclear device=${plotdev}

# get input filename

echo -n "NDF input file: "
set infile = $<
echo " "

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "COMPARE_ERR: ${infile}.sdf does not exist."
   rm -f ${curfile} >& /dev/null
   exit  
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   echo "COMPARE_ERR: ${infile}.sdf is not a datacube."
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

# collapse white light image

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile:r} axis=3" >& /dev/null 

# setup the graphics window

gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# setup frames

picdef "mode=cl fraction=[0.4,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="whitelight"

picdef "mode=tr fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="specone" 

picdef "mode=br fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="spectwo" 

# display collapsed image

picsel label="whitelight" device=${plotdev}
display "${colfile:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# setup exit condition
set prev_xpix = 1
set prev_ypix = 1

# loop marker for spectral extraction
upp_cont:

# grab x,y position

echo " "
echo "  Left click to extract spectra"
echo "  Right click to exit program."
  
cursor showpixel=true style="Colour(marker)=2" plot=mark \
       maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# wait for cursor output then get x,y co-ordinates from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position 

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
   goto upp_cont
else
   set prev_xpix = $xpix
   set prev_ypix = $ypix
endif

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# extract the spectra

echo " "
echo "      Extracing:"
echo "        (X,Y) pixel             : ${xpix},${ypix}"

# extract spectra from cube

ndfcopy "in=${infile}($xpix,$ypix,) out=${specone} trim=true trimwcs=true"
settitle "ndf=${specone} title='Pixel (${xpix},${ypix})'"

# change frames

picsel label="specone" device=${plotdev}

# plot the ripped spectra

linplot ${specone} device=${plotdev} style="Colour(curves)=2" >& /dev/null
stats ndf=${specone} > ${statsfile}
rm -f ${specone}

cat ${statsfile} | tail -14
rm -f ${statsfile}

# go back to the white light image

picsel label="whitelight" device=${plotdev}

# loop marker for spectral extraction
low_cont:

#  grab x,y position

echo " "
echo "  Left click on to extra spectra."
echo "  Right click to exit program."
  
cursor showpixel=true style="Colour(marker)=3" plot=mark \
       maxpos=1 marker=2 device=${plotdev} frame="PIXEL" >> ${curfile}

# wait for cursor output then get x,y co-ordinates from 
# the temporary file created by KAPPA cursor.

while ( ! -e ${curfile} ) 
   sleep 1
end

# grab position 

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
   goto low_cont
else
   set prev_xpix = $xpix
   set prev_ypix = $ypix
endif

# clean up the cursor temporary file

rm -f ${curfile}
touch ${curfile}

# extract the spectra

echo " "
echo "      Extracing:"
echo "        (X,Y) pixel             : ${xpix},${ypix}"

# extract spectra from cube

ndfcopy "in=${infile}($xpix,$ypix,) out=${spectwo} trim=true trimwcs=true"
settitle "ndf=${spectwo} title='Pixel ($xpix,$ypix)'"

# change frames

picsel label="spectwo" device=${plotdev}

# plot the ripped spectra

linplot ${spectwo} device=${plotdev} style="Colour(curves)=3" >& /dev/null
stats ndf=${spectwo} > ${statsfile}
rm -f ${spectwo}

cat ${statsfile} | tail -14
rm -f ${statsfile}

goto upp_cont

# clean up
cleanup:

rm -f ${curfile} 
rm -f ${colfile} 
rm -f ${specone} 
rm -f ${spectwo} 
rm -f ${statsfile}
rmdir ${tmpdir}/${user}

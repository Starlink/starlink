#!/bin/csh
#+
#  Name:
#     stacker.csh
#
#  Purpose:
#     Plots a stack of spectra extracted from a 3D IFU NDF
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     stacker [-i filename] [-n number] [-o number] [-z/+z] 
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     FIGARO and DATACUBE packages. It reads a 3D IFU NDF datacube as input
#     and presents the user with a white light image of the cube. The user
#     can then select a number of x,y position using the cursor. The script 
#     will then extract and display these spectra in a "stack" with each
#     spectra plotted offset vertically from the previous one in the stack.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -n number
#       Number of spectra to extract.
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
#     {enter_new_authors_here}
#
#  History:
#     24-NOV-2000 (AALLAN):
#       Original version.
#     31-DEC-2000 (AALLAN):
#       Allowed 1 character responses to yes/no prompts
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

rm -f ${tmpdir}/${user}/stak* >& /dev/null

# do variable initialisation

mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/stak_cursor.tmp"
set colfile = "${tmpdir}/${user}/stak_col.sdf"
touch ${curfile}

set args = ($argv[1-])
set plotdev = "xwin"
set gotinfile = "FALSE"
set gotnum = "FALSE"
set gotoff = "FALSE"
set gotzoom = "ASK"

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
   case -z:    # zoom
      set gotzoom = "TRUE"
      shift args
      breaksw 
   case +z:    # not zoom
      set gotzoom = "FALSE"
      shift args
      breaksw                            
   endsw  
end

# get input filename

if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
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

# find out the cube dimensions

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

# collapse white light image

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
collapse "in=${infile} out=${colfile:r} axis=3" >& /dev/null 

# display collapsed image

gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}
display "${colfile:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# ask for number of spectra to grab

if ( ${gotnum} == "FALSE" ) then
   echo " "
   echo -n "Number of spectra: "
   set numspec = $<
endif

echo " "

# loop round the spectra

set counter = 1
while ( $counter <= $numspec )

# setup extraction file

   set specfile = "${tmpdir}/${user}/stak_${counter}.sdf"

# grab x,y position

   echo " "
   echo "  Left click on pixel to be extracted"
   
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

# clean up the cursor temporary file

   rm -f ${curfile}
   touch ${curfile}

# extract the spectra

   echo " "
   echo "      Extracing:"
   echo "        (X,Y) pixel: ${xpix},${ypix}"

# extract spectra from cube

   ndfcopy "in=${infile}($xpix,$ypix,) out=${specfile} trim=true trimwcs=true"

# increment counter
   @ counter = $counter + 1

end

# get the offset between each spectrum


if ( ${gotoff} == "FALSE" ) then
   echo " "
   echo -n "Offset: "
   set offset = $<
endif

echo " "
echo "      Adding:"

# add offsets to each spectrum

set counter = 1
while ( $counter <= $numspec )

   set specfile = "${tmpdir}/${user}/stak_${counter}.sdf" 
   set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf" 
   
# do the addition
   set specoff = \
                `calc exp="'${offset}*(${counter}-1)'" prec=_double`

   echo "        Adding ${specoff} to spectrum ${counter}"

   cadd in=${specfile} out=${outfile} scalar=${specoff}

# increment counter

   @ counter = $counter + 1
end

# do the plot

gdclear device=${plotdev}

echo " "
echo "      Plotting:"

set counter = $numspec
while ( $counter > 0 )
   
   set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf" 

   echo "        Spectrum: ${counter} "

   linplot ${outfile} device=${plotdev} style="Colour(curves)=1"\
           clear=no ybot=0 >& /dev/null

   @ counter = $counter - 1

end

# zoom if required

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

# get the lower limit

   echo " "
   echo "  Left click on lower zoom boundary"
   
   cursor showpixel=true style="Colour(curves)=3" plot=vline \
          maxpos=1 device=${plotdev} >> ${curfile}
   while ( ! -e ${curfile} ) 
      sleep 1
   end
   set pos = `parget lastpos cursor`
   set low_z = $pos[1]

# clean up the cursor temporary file
   rm -f ${curfile}
   touch ${curfile}
   
# get the upper limit

   echo "  Left click on upper zoom boundary"
   
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

# clean up the cursor temporary file
   rm -f ${curfile}
   touch ${curfile}

# do the plot

   gdclear device=${plotdev}

   echo " "
   echo "      Plotting:"

   set counter = $numspec
   while ( $counter > 0 )
   
      set outfile = "${tmpdir}/${user}/stak_${counter}_off.sdf" 

      echo "        Spectrum: ${counter} "

      linplot ${outfile} xleft=${low_z} xright=${upp_z}\
              device=${plotdev} style="Colour(curves)=1"\
              clear=no ybot=0 >& /dev/null

      @ counter = $counter - 1

   end
endif

# clean up
cleanup:

rm -f ${tmpdir}/${user}/stak_?.sdf >& /dev/null     
rm -f ${tmpdir}/${user}/stak_?_off.sdf >& /dev/null   
rm -f ${tmpdir}/${user}/stak_col.sdf >& /dev/null    
rm -f ${tmpdir}/${user}/stak_cursor.tmp >& /dev/null 
rmdir ${tmpdir}/${user} 

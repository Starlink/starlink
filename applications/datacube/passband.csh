#!/bin/csh
#+
#  Name:
#     passband.csh
#
#  Purpose:
#     Display of multiple passband image from a 3D IFU NDF
#
#  Type of Module:
#     C shell script.
#
#  Usage:
#     passband [-i filename] [-o filename] [-z/+z]
#
#  Description:
#     This shell script sits onto of a collection of A-tasks from the KAPPA
#     FIGARO and DATACUBE packages. It reads a 3D IFU NDF datacube as input
#     and presents the user with a white light image of the cube. The user
#     can then select and x,y position using the cursor. The script will
#     extract and display this spectra next to the white light image. The 
#     user can then select a wavelength range using the cursor and the 
#     script will display a passband image of the cube in that wavelength
#     range.
#
#  Parameters:
#     -i filename
#       The script will use this as its input file, the specified file should
#       be a 3D NDF, by default the script will prompt for the input file.
#     -o filename
#       The script will generate output 2D NDF of passband image, by default
#       the output will be displayed only.
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
#     09-NOV-2000 (AALLAN):
#       Original version.
#     20-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     23-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#       Added -i and -o command line options.
#       Added on interrupt handler.
#       Added +-z command line options.
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

rm -f ${tmpdir}/${user}/pass* >& /dev/null

# do variable initialisation

mkdir ${tmpdir}/${user}
set curfile = "${tmpdir}/${user}/pass_cursor.tmp"
set colfile = "${tmpdir}/${user}/pass_col.sdf"
set pasfile = "${tmpdir}/${user}/pass_pas.sdf"
set spectral = "${tmpdir}/${user}/pass_rip.sdf"
set statsfile = "${tmpdir}/${user}/pass_stats.txt"
touch ${curfile}

set gotinfile = "FALSE"
set gotoutfile = "FALSE"
set gotzoom = "ASK"
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
   case -o:    # output passband image to file
      shift args
      set gotoutfile = "TRUE"
      set outfile = $args[1]
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

# setup plot device

set plotdev = "xwin"
gdclear device=${plotdev}
gdclear device=${plotdev}

# get input filename

if ( ${gotinfile} == "FALSE" ) then
   echo -n "NDF input file: "
   set infile = $<
   echo " "
endif

echo "      Input NDF:"
echo "        File: ${infile}.sdf"

# check that it exists

if ( ! -e ${infile}.sdf ) then
   echo "PASSBAND_ERR: ${infile}.sdf does not exist."
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
   echo "PASSBAND_ERR: ${infile}.sdf is not a datacube."
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
settitle "ndf=${colfile:r} title='White Light Image'"

# setup the graphics window

gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# display collapsed image

picbase device=${plotdev}
display "${colfile:r} device=${plotdev} mode=SIGMA sigmas=[-3,2]" >&/dev/null 

# setup exit condition
set prev_xpix = 1
set prev_ypix = 1

# loop marker for spectral extraction
extract:

# grab x,y position

echo " "
echo "  Left click to extract spectra"
  
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
   goto extract
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

ndfcopy "in=${infile}($xpix,$ypix,) out=${spectral} trim=true trimwcs=true"
settitle "ndf=${spectral} title='Pixel (${xpix},${ypix})'"

# plot the ripped spectra

linplot ${spectral} device=${plotdev} style="Colour(curves)=1" >& /dev/null
stats ndf=${spectral} > ${statsfile}

cat ${statsfile} | tail -14
rm -f ${statsfile}

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
 
#  replot the spectra
   linplot ${spectral} xleft=${low_z} xright=${upp_z} \
           device=${plotdev} style="Colour(curves)=1" >& /dev/null

endif

# get the lower limit

echo " "
echo "  Left click on lower boundary"
   
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}
while ( ! -e ${curfile} ) 
   sleep 1
end
set pos = `parget lastpos cursor`
set low = $pos[1]

# clean up the cursor temporary file
rm -f ${curfile}
touch ${curfile}
   
# get the upper limit

echo "  Left click on upper boundary"
   
cursor showpixel=true style="Colour(curves)=2" plot=vline \
       maxpos=1 device=${plotdev} >> ${curfile}
while ( ! -e ${curfile} ) 
   sleep 1
end
set pos = `parget lastpos cursor`
set upp = $pos[1]

echo " "
echo "      Passband:"
echo "        Lower Boundary: ${low}"
echo "        Upper Boundary: ${upp}"

# clean up the cursor temporary file
rm -f ${curfile}
touch ${curfile}

# create the passband image

echo "      Collapsing:"
echo "        White light image: ${dims[1]} x ${dims[2]}"
echo "        Wavelength range: ${low} - ${upp}"

# collapse white light image

collapse "in=${infile} out=${pasfile:r} " \
         "axis=3 low=${low} high=${upp}" >& /dev/null 
settitle "ndf=${pasfile:r} title='$low - $upp'"

# clean up the graphics device

gdclear device=${plotdev}

# setup frames
echo "      Plotting:"
   
picdef "mode=tl fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="left"

picdef "mode=cr fraction=[0.5,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="right" 

# display the white light image on the left and the passband image on the right
echo "        Left: White light image." 
picsel label="left" device=${plotdev}
display ${colfile:r} device=${plotdev} mode=SCALE \
        low='!' high='!' >&/dev/null 

echo "        Right: Passband image (${low} - ${upp})" 
picsel label="right" device=${plotdev}
display ${pasfile:r} device=${plotdev} mode=SCALE \
        low='!' high='!' >&/dev/null 

if ( ${gotoutfile} == "TRUE" ) then
   echo "      Output NDF:"
   echo "        Creating: ${outfile:r}.sdf" 
   cp -f ${pasfile} ./${outfile:r}.sdf

   # check to see if the NDF has an AXIS extensions
   set axis = `parget axis ndftrace`
   if ( ${axis} == "FALSE" ) then
      echo "        Axes: Creating AXIS extensions"
      putaxis "${outfile:r} spectral=3" >& /dev/null
   endif
endif

# cleanup

cleanup:
rm -f ${curfile} >&/dev/null 
rm -f ${colfile} >&/dev/null 
rm -f ${pasfile} >&/dev/null 
rm -f ${spectral} >&/dev/null 
rm -f ${statsfile} >&/dev/null 
rmdir ${tmpdir}/${user} >& /dev/null


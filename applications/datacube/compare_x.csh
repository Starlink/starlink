#!/bin/csh
#+

#  Name:
#     compare_x.csh

#  Purpose:
#     Comparison of multiple extracted spectra from a IFU datacube

#  Type of Module:
#     C shell script

#  Usage:
#     xcompare [-gtk path] [-xdialog path]

#  Description:
#     This shell script reads a 3D IFU NDF datacube as input and presents
#     the user with a white light image of the cube. The user can then 
#     select and x,y position using the cursor. The script will extract 
#     and display this spectra next to the white light image. The user can 
#     then select another x,y position using the cursor and the script 
#     will display this spectra as well, allowing comparison of the two.

#  Parameters
#    -gtk path
#       Path to search for the GTK+ library, default is /usr/lib
#    -xdialog path
#       Path to search for the Xdialog executable, default is /usr/bin

#  Authors:
#     AALLAN: Alasdair Allan (Starlink, University of Exeter)
#     {enter_new_authors_here}

#  History:
#     09-NOV-2000 (AALLAN):
#       Original command line version compare.csh
#     12-NOV-2000 (AALLAN):
#       Modified to work under Solaris 5.8, problems with bc and csh.
#     20-NOV-2000 (AALLAN):
#       Incorporated changes made to source at ADASS.
#     23-NOV-2000 (AALLAN):
#       Added interrupt handler.
#     06-JAN-2001 (AALLAN)
#       Converted to GUI using XDialog ontop of GTK+
#     18-OCT-2001 (AALLAN):
#       Modified temporary files to use ${tmpdir}/${user}
#     {enter_changes_here}

#  Required:
#     GTK+ >v1.2.0 (v1.2.8 recommended)
#     XDialog v1.5.0 or newer

#  GTK+
#     http://www.gtk.org/

#  XDialog:
#     http://xdialog.free.fr/

#  License:
#     Copyright (C) 2000-2001 Central Laboratory of the Research Councils
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software Foundation,
#     Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA

#-

# on interrupt
onintr cleanup

# get the user name

set user = `whoami`
set tmpdir = "/tmp"

# clean up from previous runs

rm -f ${tmpdir}/${user}/comp* >& /dev/null

# do variable initialisation

set args = ($argv[1-])

mkdir ${tmpdir}/${user} >& /dev/null
set curfile = "${tmpdir}/${user}/comp_cursor.tmp"
set colfile = "${tmpdir}/${user}/comp_col.sdf"
set specone = "${tmpdir}/${user}/comp_s1.sdf"
set spectwo = "${tmpdir}/${user}/comp_s2.sdf"
set statsfile = "${tmpdir}/${user}/comp_stats.txt"
touch ${curfile}

# set default values for the location of libgtk.a and XDialog

set libgtk = /usr/lib
set xdialog = /usr/bin

# handle any command line arguements

while ( $#args > 0 )
   switch ($args[1])
   case -gtk:
      shift args
      set libgtk = $args[1]
      shift args
      breaksw
   case -xdialog:
      shift args
      set xdialog = $args[1]
      shift args
      breaksw
   endsw
end

# Check that GTK+ and XDialog are installed

if ( ! -f ${libgtk}/libgtk.a ) then
  echo "ERROR - Cannot find libgtk"
  echo " "
  echo "Current search path is ${libgtk}, if this is incorrect"
  echo "for you system you can manually specify the correct path to libgtk"
  echo "with the -gtk <path> command line option."
  exit
else if ( ! -f ${xdialog}/Xdialog ) then
  echo "ERROR - Cannot find Xdialog"
  echo " "
  echo "Current search path is ${xdialog}, if this is incorrect"
  echo "for you system you can manually specify the correct path to the"
  echo "Xdialog executable with the -xdialog <path> command line option."
  exit
endif

# do package setup

alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# setup plot device

set plotdev = "xwin"
gdclear device=${plotdev}
gdclear device=${plotdev}

# get input filename

set infile =\
 `Xdialog --stdout --title "Please choose a file" --fselect /home/${user} 40 60`

switch ($?)
   case 0:
      set infile = ${infile:r}
      breaksw
   case 1:
      goto cleanup
      breaksw
   case 255:
      goto cleaup
      breaksw
endsw

# check that it exists

if ( ! -e ${infile}.sdf ) then
    
   Xdialog --no-cancel \
           --buttons-style text \
           --title "Error" \
           --icon /usr/share/doc/Xdialog-1.5.0/samples/warning.xpm \
	   --msgbox "${infile}.sdf does not exist." 0 0 
   switch ($?)
      case 0:
         rm -f ${curfile} >& /dev/null
         exit  
         breaksw
   endsw
endif

# find out the cube dimensions

ndftrace ${infile} >& /dev/null
set ndim = `parget ndim ndftrace`
set dims = `parget dims ndftrace`
set lbnd = `parget lbound ndftrace`
set ubnd = `parget ubound ndftrace`

if ( $ndim != 3 ) then
   Xdialog --no-cancel \
           --buttons-style text \
           --title "Error" \
           --icon /usr/share/doc/Xdialog-1.5.0/samples/warning.xpm \
	   --msgbox "${infile}.sdf is not a datacube." 0 0 
   switch ($?)
      case 0:
         rm -f ${curfile} >& /dev/null
         exit  
         breaksw
   endsw
endif

set bnd = "${lbnd[1]}:${ubnd[1]}, ${lbnd[2]}:${ubnd[2]}, ${lbnd[3]}:${ubnd[3]}"
@ pixnum = $dims[1] * $dims[2] * $dims[3]

Xdialog --left --buttons-style text \
	--title "Datacube shape" \
	--fixed-font \
	--no-buttons \
	--infobox \
	   "      Shape:\n        No. of dimensions: ${ndim}\n        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}\n        Pixel bounds       : ${bnd}\n        Total pixels         : $pixnum\n\n  Left click to extract spectra.\n  Right click to exit program." 0 0 7000 >& /dev/null &

# collapse white light image

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

# extract spectra from cube

ndfcopy "in=${infile}($xpix,$ypix,) out=${specone} trim=true trimwcs=true"
settitle "ndf=${specone} title='Pixel (${xpix},${ypix})'"

# change frames

picsel label="specone" device=${plotdev}

# plot the ripper spectra

linplot ${specone} device=${plotdev} style="Colour(curves)=2" >& /dev/null

# show spectra statistics

stats ndf=${specone} > ${statsfile}
rm -f ${specone}
cat ${statsfile} | tail -14 > ${statsfile}_tail
echo "      Extracing:" > ${statsfile}_final
echo "        (X,Y) pixel             : ${xpix},${ypix}" >> ${statsfile}_final
cat ${statsfile}_tail >> ${statsfile}_final
rm -f ${statsfile} ${statsfile}_tail

cat ${statsfile}_final | Xdialog --no-cancel --buttons-style text \
                                 --title "Spectral Statistics" \
                                 --fixed-font --textbox "-" 40 80
rm -f ${statsfile}_final

switch ($?)
   case 0:
      breaksw
endsw

# go back to the white light image

picsel label="whitelight" device=${plotdev}

# loop marker for spectral extraction
low_cont:

#  grab x,y position

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

# extract spectra from cube

ndfcopy "in=${infile}($xpix,$ypix,) out=${spectwo} trim=true trimwcs=true"
settitle "ndf=${spectwo} title='Pixel ($xpix,$ypix)'"

# change frames

picsel label="spectwo" device=${plotdev}

# plot the ripped spectra

linplot ${spectwo} device=${plotdev} style="Colour(curves)=3" >& /dev/null

# show spectra statistics

stats ndf=${spectwo} > ${statsfile}
rm -f ${spectwo}
cat ${statsfile} | tail -14 > ${statsfile}_tail
echo "      Extracing:" > ${statsfile}_final
echo "        (X,Y) pixel             : ${xpix},${ypix}" >> ${statsfile}_final
cat ${statsfile}_tail >> ${statsfile}_final
rm -f ${statsfile} ${statsfile}_tail

cat ${statsfile}_final | Xdialog --no-cancel --buttons-style text \
                                 --title "Spectral Statistics" \
                                 --fixed-font --textbox "-" 40 80
rm -f ${statsfile}_final

switch ($?)
   case 0:
      breaksw
endsw

goto upp_cont

# clean up
cleanup:

rm -f ${curfile} 
rm -f ${colfile} 
rm -f ${specone} 
rm -f ${spectwo} 
rm -f ${statsfile}
rmdir ${tmpdir}/${user}

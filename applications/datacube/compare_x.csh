#!/bin/csh
#+

#  Name:
#     compare_x.csh

#  Purpose:
#     Compares multiple extracted spectra from a IFU datacube

#  Type of Module:
#     C-shell script

#  Usage:
#     xcompare [-gtk path] [-xdialog path]

#  Description:
#     This shell script reads a three-dimensional IFU NDF as input and
#     presents you with a white-light image of the cube.   You can then
#     select and X-Y position using the cursor.  The script will extract
#     and display this spectrum next to the white-light image.   You can
#     then select another X-Y position using the cursor and the script
#     will display this spectrum as well, allowing comparison of the two.

#  Parameters
#    -gtk path
#       Path to search for the GTK+ library, default is /usr/lib
#    -xdialog path
#       Path to search for the Xdialog executable, default is /usr/bin

#  Implementation Status:
#     This script invokes a collection of A-tasks from the KAPPA package.

#  Authors:
#     AALLAN: Alasdair Allan (Starlink, University of Exeter)
#     MJC: Malcolm J. Currie (Starlink, RAL)
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
#     2005 September  6 (MJC):
#       Some tidying of grammar, punctutation, and spelling.  Added section
#       headings in the code.  Attempt removal of files silently.
#     2005 October 11 (MJC):
#       Fixed bugs converting the cursor position into negative pixel indices.
#     2006 March 2 (MJC):
#       Allow for NDF sections to be supplied with the input filename.
#       Use a new script to obtain cursor positions.
#     2006 March 9 (MJC):
#       Corrected the NDF name extraction when both the file extension and an
#       NDF section are supplied.  Added a check for a degenerate third axis.
#     {enter_further_changes_here}

#  Required:
#     GTK+ >v1.2.0 (v1.2.8 recommended)
#     XDialog v1.5.0 or newer

#  GTK+
#     http://www.gtk.org/

#  XDialog:
#     http://xdialog.free.fr/

#  License:
#     Copyright (C) 2000-2006 Central Laboratory of the Research Councils
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
#     Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

#-

# Preliminaries
# =============

# On interrupt tidy up.
onintr cleanup

# Get the user name.
set user = `whoami`
set tmpdir = "/tmp"

# Clean up from previous runs.
rm -f ${tmpdir}/${user}/comp* >& /dev/null

# Do variable initialisation.
mkdir ${tmpdir}/${user} >& /dev/null
set colfile = "${tmpdir}/${user}/comp_col"
set specone = "${tmpdir}/${user}/comp_s1"
set spectwo = "${tmpdir}/${user}/comp_s2"
set statsfile = "${tmpdir}/${user}/comp_stats.txt"

# Set default values for the location of libgtk.a and XDialog.
set libgtk = /usr/lib
set xdialog = /usr/bin

# Handle any command-line arguments.
set args = ($argv[1-])
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
  echo "Current search path is ${libgtk}.  If this is incorrect for"
  echo "your system you can manually specify the correct path to libgtk"
  echo "with the -gtk <path> command-line option."
  exit

else if ( ! -f ${xdialog}/Xdialog ) then
  echo "ERROR - Cannot find Xdialog"
  echo " "
  echo "Current search path is ${xdialog}.  If this is incorrect for"
  echo "your system you can manually specify the correct path to the"
  echo "Xdialog executable with the -xdialog <path> command-line option."
  exit
endif

# Do the package setup.
alias echo 'echo > /dev/null'
source ${DATACUBE_DIR}/datacube.csh
unalias echo

# Setup the plot device.

set plotdev = "xwin"
gdclear device=${plotdev}

# Obtain details of the input cube.
# =================================

# Get the input filename.
set infile =\
 `Xdialog --stdout --title "Please choose a file" --fselect /home/${user} 40 60`

switch ($?)
   case 0:
      breaksw
   case 1:
      goto cleanup
      breaksw
   case 255:
      goto cleaup
      breaksw
endsw

# Obtain the name sans any section.
set inname = `echo $infile | \
              awk '{if (index($0,"(") > 0) print substr($0,1,index($0,"(")-1); else print $0}'`

# Obtain the section, if present.
set ndf_section = `echo $infile | \
      awk '{if (index($0,"(") > 0) print substr($0,index($0,"(")); else print ""}'`

# This must occur after stripping the section, as the section
# specification must come after the file extension in file names
# presented to the NDF library (cf. SSN/20).  Also this step is
# necessary when the version of KAPPA does not support the .sdf file
# extension being supplied in the name.
set infile = ${inname:r}

# Check that the file exists.
if ( ! -e ${infile}.sdf ) then

   Xdialog --no-cancel \
           --buttons-style text \
           --title "Error" \
           --icon /usr/share/doc/Xdialog-1.5.0/samples/warning.xpm \
	   --msgbox "${infile}.sdf does not exist." 0 0
   switch ($?)
      case 0:
         exit
         breaksw
   endsw
endif

# Find out the cube dimensions.
ndftrace ${infile}${ndf_section} >& /dev/null
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
         exit
         breaksw
   endsw

# Check for a degenerate third axis.
else if ( $dims[3] == 1 ) then
   Xdialog --no-cancel \
           --buttons-style text \
           --title "Error" \
           --icon /usr/share/doc/Xdialog-1.5.0/samples/warning.xpm \
	   --msgbox "${infile}.sdf${ndf_section} is not a datacube." 0 0
   switch ($?)
      case 0:
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
	   "      Shape:\n        No. of dimensions: ${ndim}\n        Dimension size(s): ${dims[1]} x ${dims[2]} x ${dims[3]}\n        Pixel bounds       : ${bnd}\n        Total pixels         : $pixnum\n\n  Left click to extract spectrum.\n  Right click to exit program." 0 0 7000 >& /dev/null &

# Show the white-light image.
# ===========================

# Collapse the white-light image.
collapse "in=${infile}${ndf_section} out=${colfile} axis=3" >& /dev/null

# Setup the graphics window.
gdclear device=${plotdev}
paldef device=${plotdev}
lutgrey device=${plotdev}

# Create graphics-database frames for the graphical elements.
picdef "mode=cl fraction=[0.4,1.0] device=${plotdev} nooutline"
piclabel device=${plotdev} label="whitelight"

picdef "mode=tr fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="specone"

picdef "mode=br fraction=[0.6,0.5] device=${plotdev} nooutline"
piclabel device=${plotdev} label="spectwo"

# Display the collapsed image.
picsel label="whitelight" device=${plotdev}
display "${colfile} device=${plotdev} mode=SIGMA sigmas=[-3,2]" reset >&/dev/null

# Obtain the spatial position of the spectrum graphically.
# ========================================================

# Setup the exit condition.
set prev_xgrid = 1
set prev_ygrid = 1

# Loop marker for spectral extraction
upp_cont:

# Grab an X-Y position.
   source ${DATACUBE_DIR}/getcurpos.csh -ci 2 -a XY -g

# Check for the exit conditions.
   if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
      goto cleanup
   else if ( $xgrid == 1 && $ygrid == 1 ) then
      goto upp_cont
   else
      set prev_xgrid = $xgrid
      set prev_ygrid = $ygrid
   endif

# Extract and plot the selected spectrum.
# =======================================

   ndfcopy "in=${infile}($xgrid,$ygrid,) out=${specone} trim=true trimwcs=true"
   settitle "ndf=${specone} title='Pixel (${xgrid},${ygrid})'"

# Change graphics-database frame.
   picsel label="specone" device=${plotdev}

# Plot the ripped spectrum.
   linplot ${specone} device=${plotdev} mode=histogram style="Colour(curves)=2" >& /dev/null

# Show statistics of the spectrum.
   stats ndf=${specone} > ${statsfile}
   rm -f ${specone}.sdf >& /dev/null

# This is not robust if the output of STATS changes!  It would be better to
# report from the line starting with "Title" until the end. --MJC
   cat ${statsfile} | tail -14 > ${statsfile}_tail

   echo "      Extracting:" > ${statsfile}_final
   echo "        (X,Y) pixel             : ${xgrid},${ygrid}" >> ${statsfile}_final

   cat ${statsfile}_tail >> ${statsfile}_final
   rm -f ${statsfile} ${statsfile}_tail >& /dev/null

   cat ${statsfile}_final | Xdialog --no-cancel --buttons-style text \
                                    --title "Spectral Statistics" \
                                 --fixed-font --textbox "-" 40 80
   rm -f ${statsfile}_final >& /dev/null

   switch ($?)
      case 0:
         breaksw
   endsw

# Extract and plot the second selected spectrum.
# ===============================================

# Go back to the white-light image.
   picsel label="whitelight" device=${plotdev}

# Loop marker for spectral extraction
low_cont:

# Grab an X-Y position.
   source ${DATACUBE_DIR}/getcurpos.csh -ci 3 -a XY -g

# Check for exit conditions.
   if ( $prev_xgrid == $xgrid && $prev_ygrid == $ygrid ) then
      goto cleanup
   else if ( $xgrid == 1 && $ygrid == 1 ) then
      goto low_cont
   else
      set prev_xgrid = $xgrid
      set prev_ygrid = $ygrid
   endif

# Extract spectrum from the cube.
   ndfcopy "in=${infile}($xgrid,$ygrid,) out=${spectwo} trim=true trimwcs=true"
   settitle "ndf=${spectwo} title='Pixel ($xgrid,$ygrid)'"

# Change graphics-database frame.
   picsel label="spectwo" device=${plotdev}

# Plot the ripped spectra
   linplot ${spectwo} device=${plotdev} style="Colour(curves)=3" >& /dev/null

# Extract the statistics.
   stats ndf=${spectwo} > ${statsfile}
   rm -f ${spectwo}.sdf >& /dev/null
   cat ${statsfile} | tail -14 > ${statsfile}_tail

   echo "      Extracting:" > ${statsfile}_final
   echo "        (X,Y) pixel             : ${xgrid},${ygrid}" >> ${statsfile}_final
   cat ${statsfile}_tail >> ${statsfile}_final
   rm -f ${statsfile} ${statsfile}_tail >& /dev/null

   cat ${statsfile}_final | Xdialog --no-cancel --buttons-style text \
                                    --title "Spectral Statistics" \
                                    --fixed-font --textbox "-" 40 80
   rm -f ${statsfile}_final >& /dev/null

   switch ($?)
      case 0:
         breaksw
   endsw

goto upp_cont

# Clean up.
# =========
cleanup:

rm -f ${colfile}.sdf >& /dev/null
rm -f ${specone}.sdf >& /dev/null
rm -f ${spectwo}.sdf >& /dev/null
rm -f ${statsfile} >& /dev/null
rmdir ${tmpdir}/${user} >& /dev/null

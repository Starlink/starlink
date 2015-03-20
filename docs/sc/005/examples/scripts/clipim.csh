#! /bin/csh

# Purpose:
# Allows the user to interactively reduce the size of an image.
#
# It employs KAPPA's DISPLAY and NDFCOPY to do the work.

# Parameters:
#
# Input $1  -  The source image name
# Input $2  -  The output image name

# Note:
# The display device used to show the input/output
# image is xw.

# Typical usage:
# clipim big.fit small.fit
# clipim largesdf.sdf smallsdf.sdf

# Author:
#  GJP: G J Privett (Cardiff)

# History:
#  Spring 97 (GJP): Original version.

# Check the number of input parameters.
set nvar = $#argv
if ( $nvar != "2" ) then
    echo " "
    echo "Wrong number of arguments. 2 expected."
    echo " "
    goto farewell
endif

# Check to see that the file name exists.
if( -e $1 ) then

# Look at the input file name and remove the .sdf if present.
# Most STARLINK programs require all NDF file names to
# be supplied without the .sdf part of the name.
  set endbit = "$1:e"
  if ( $endbit != "sdf") then
    echo "Processing file $1"
    set fname = $1
  else
    echo "Processing NDF $1"
    set fname = "$1:r"
  endif

# Look at the output file name and remove the .sdf if present.
# Most STARLINK programs require all NDF file names to
# be supplied without the .sdf part of the name.
  set endbit = "$2:e"
  if ( $endbit != "sdf") then
    set fname2 = $2
  else
    set fname2 = "$2:r"
  endif

# Turn on STARLINK stuff. Normally done in your .cshrc file.
  source /star/etc/login
  source /star/etc/cshrc

# Turn on KAPPA and CONVERT
  convert
  kappa

# Display the input image.
  echo "in='$fname' device=xw mode=perc percentiles='[10,90]' key=true"
  display in=$fname device=xw mode=perc percentiles='[10,90]' key=true

# Tell the user what to do.
  touch junkjunk junkjunk2
  rm -f junkjunk junkjunk2
  echo " "
  echo "Identify the desired bottom left corner of the image using the mouse."
  echo "Click once with the left button and then with the right"
  cursor device=xw logfile=junkjunk >& /dev/null
  echo " "
  echo "Identify the desired top right corner of the image using the mouse"
  echo "Click once with the left button and then with the right"
  cursor device=xw logfile=junkjunk2 >& /dev/null
  echo " "

# Look at HISTPEAK output. Grab the fields required.
  touch tmp_x1 tmp_y1 tmp_x2 tmp_y2
  rm -f tmp_x1 tmp_y1 tmp_x2 tmp_y2
  more junkjunk | awk '{print $1}' >! tmp_x1
  more junkjunk | awk '{print $2}' >! tmp_y1
  more junkjunk2 | awk '{print $1}' >! tmp_x2
  more junkjunk2 | awk '{print $2}' >! tmp_y2
  rm junkjunk junkjunk2

# Use a clear screen to show the user something is happening.
  gdclear device=xw
  echo "Working."

# Create an output image using NDFCOPY.
  set ndfin = "(`cat tmp_x1`:`cat tmp_x2`,`cat tmp_y1`:`cat tmp_y2`)"
  ndfcopy $fname$ndfin $fname2
  rm -f tmp_x1 tmp_x2 tmp_y1 tmp_y2

# Redisplay image by way of confirmation.
  display in=$fname2 device=xw mode=perc percentiles='[10,90]' key=true

else

  echo " "Z
  echo "That filename does not exist."
  echo " "

endif

farewell:
exit

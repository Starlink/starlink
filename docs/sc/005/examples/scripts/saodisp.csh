#! /bin/csh
 
# Purpose:
# Runs SAOIMAGE on an image but within set thresholds. Bounds are defined
# in terms of ADEV (absolute deviation of estimated sky background count)  
# and are derived via the STARLINK ESP package. For many purposes
# the value of ADEV is a bit bigger than SDEV.
#
# KAPPA's STATS is not used to avoid the influence of outliers on the est>
# sky background variation estimation. 
 
# Parameters:
# Input $1  -  The image name
# Input $2  -  The number of ADEV below sky at which
#              a pixel appears black.
# Input $3  -  The number of ADEV above sky at which
#              a pixel appears a white.
#
# Note:        
# Values of $2 and $3 should both be positive.
 
# Typical usage:
# saodisp halebopp.fit 1 10

# Author:
#  GJP: G J Privett (Cardiff)

# History:
#  Spring 97 (GJP): Original version.
 
# Check the number of input parameters.
# Also check that the file suggested exists.
set nvar = $#argv
if (( $nvar == "3" )&&( -e $1 )) then
 
# Look at the file name and remove the .sdf if present.
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
 
# Turn on STARLINK stuff. Normally done in your .cshrc file.
  source /star/etc/login  
  source /star/etc/cshrc
 
# Turn on ESP and CONVERT
  esp
  convert
  
# Find the background and other stats of the image using ESP's HISTPEAK.
# Run HISTPEAK.
  set histin = "use=w sfact=2 device=! in=$fname"
  histpeak "$histin" >! tmp_hist 
 
# Look at HISTPEAK output. Grab the fields required.
  grep Median   tmp_hist | awk '{print $4}' >! tmp_mode
  grep Abso     tmp_hist | awk '{print $3}' >! tmp_width 
 
# Calculate the upper and lower limits required.
  echo "(`cat tmp_mode` - (`cat tmp_width`*$2))" | bc >! tmp_low
  echo "(`cat tmp_mode` + (`cat tmp_width`*$3))" | bc >! tmp_high 

# Run saoimage
  set saoin = " $1 -quiet -imtool -rmin `cat tmp_low` -rmax `cat tmp_high` "
  echo " "
  echo "saoimage $saoin"
  echo " "
  saoimage $saoin 
 
# Remove most temporary files (avoids use of rm).
  touch junkjunk
  mv junkjunk     tmp_hist 
  mv tmp_hist     tmp_mode
  mv tmp_mode     tmp_width
  mv tmp_width    tmp_low
  mv tmp_low      tmp_high
  mv tmp_high     junkjunk
 
else

  if(-e $1) then
    echo " "
    echo "Wrong number of arguments: 3 expected."
    echo " "
  else
    echo " "
    echo "That filename does not exist." 
    echo " "
  endif
 
endif
 
exit

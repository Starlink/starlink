#! /bin/csh 
 
# Purpose:
# Uncompresses a compressed image file. Runs ESP's HISTPEAK on it to 
# obtain image statistics and outputs the result to a 
# text file. Recompresses the uncompressed file afterwards.
# 
# This version obtains the median pixel value. It may
# be used as a template for routines obtaining other values such as
# standard deviation. 
 
# Parameters:
# Input $1  -  The image name (may contain wild cards).
# Input $2  -  The name of the output text file.
#
# Note:       
# This is only a template. To obtain the fields you 
# specifically want you will need to adjust the awk 
# and grep statements.
 
# Typical usage:
# doapp hyak.fits.Z result
# doapp "*.fit.Z" result   (if wildcard names are involved)    

# Authors:
#  GJP: G J Privett (Cardiff)
#  ACD: A C Davenhall (Edinburgh)

# History:
#  Spring 97 (GJP): Original version.
#  8/4/99    (ACD): Modified for SC/5.2.
 
# Check the number of input parameters.
set nvar = $#argv
if ( $nvar == "2" ) then
 
# Prepare output file.
  touch $2
  rm -f $2
 
# Create a directory listing of the filename provided.
# If it fails go to farewell and exit since no
# file will be found by foreach. 
  set names = "$1"
  ls -al $names >& /dev/null
  if ( $status == 1 ) then
    echo "No files found."
    goto farewell
  endif
 
# Check that the file required is a .Z file.
  set endbit = "$1:e"
  if ( $endbit != "Z" ) then
    echo "$1 is not a compressed file"
    goto farewell
  endif
 
# Turn on STARLINK stuff. Normally done in your .cshrc file.
  source /star/etc/login  
  source /star/etc/cshrc
 
# Turn on ESP and CONVERT
  esp     
  convert
 
# Look at all the file names that $1 refers to.
  foreach filn ($1)
   
# Uncompress it.
    uncompress $filn
 
# Abort if necessary.
    if ( $status == 1 ) then
      echo "File uncompress failed."
      goto farewell
    endif
# New file name without the .Z
    set fname2 = "$filn:r"
 
# Look at the file name and remove the .sdf if present.
# Most STARLINK programs require all NDF file names to
# be supplied without the .sdf part of the name.
    set endbit = "$fname2:e"  
    if ( $endbit != "sdf") then 
 
      echo "Processing file $fname2"
      set fname = $fname2
 
# Set a suitable message for later.
      set msgout = "Median in file $fname2 is "
 
    else
 
      echo "Processing NDF $fname2"
      set fname = "$fname2:r"
 
# Set a suitable message for later.
      set msgout = "Median in NDF file $fname2 is "
    
    endif
 
# Use HISTPEAK on the uncompressed image.
    touch tmp_hist
    rm -f tmp_hist
    set histin = "use=w sfact=1 device=! in=$fname"
    histpeak "$histin" >! tmp_hist
   
# Look at HISTPEAK output. Grab the fields required.
    set median_val = `grep Median   tmp_hist | awk '{print $4}'`
    rm -f tmp_hist
  
# Place the output in the target file.
    echo "$msgout $median_val" >> $2
 
# Recompress the file.
    compress $fname2 
 
  end
 
# Output the results
  echo " "
  echo "The values found were:"
  echo " "
  cat $2
 
else
 
  echo " "
  echo "Wrong number of arguments: 2 expected."
  echo " "
  
endif
 
 
farewell:
 
exit


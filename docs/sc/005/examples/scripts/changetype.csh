#! /bin/csh
 
# Purpose:
# Allows the user to convert a series of files of one type to a 
# different type.
#
# The script employs CONVERT and KAPPA so it is restricted to
# files type such as .fit, .sdf, .imh etc. See SUN/55.
#
# No error checking is done on the image types.
# It avoids the need to employ a temporary .sdf when the both the input
# and output images are both not NDFs.
 
# Parameters:
# Input $1  -  The name of the images required (wildcards allowed).
# Input $2  -  The output type required.
 
# Note:
# Values of $1 involving wildcards must be placed between " characters.
#              
# Values of $2 must be lower case only and must be fit, gif, dst,
# ascii, gasp or tiff.  
 
# Typical usage:
# chnagetype hyak.fit gif
# changetype "*.fit" dst   (if wildcard names are involved) 

# Author:
#  GJP: G J Privett (Cardiff)

# History:
#  Spring 97 (GJP): Original version.
 
# Turn on STARLINK stuff. Normally done in your .cshrc file.
  source /star/etc/login  
  source /star/etc/cshrc
 
# Turn on KAPPA and CONVERT.
  convert >/dev/null
  kappa   >/dev/null
 
# Check the number of input parameters.
set nvar = $#argv
if ( $nvar == "2" ) then
 
# Create a directory listing of the filename provided.
# If it fails go to farewell and exit since no
# file will be found by foreach. 
  set names = "$1"
  ls -al $names >& /dev/null
  if ( $status == 1 ) then
    echo "No files found."
    goto farewell
  endif
 
# Look at all the file names that $1 refers to.
  foreach filn ($1)
 
# Look at the input file name and remove the .sdf if present.
# Most STARLINK programs require all NDF file names to
# be supplied without the .sdf part of the name.
    set endbit = "$filn:e"  
    if ( $endbit != "sdf") then 
 
# Keep the user informed.    
      echo "Processing file $filn"
      set fname = $filn
      if ( $2 != "sdf" ) then 
        set fname2 = "$filn:r"."$2"
      else
        set fname2 = "$filn:r"
      endif
 
    else
 
# Keep the user informed. 
      echo "Processing NDF $filn"
      set fname = "$filn:r"
      if ( $2 != "sdf" ) then
        set fname2 = $fname."$2"
      else
        set fname2 = $fname
      endif
 
    endif 
 
# Use KAPPAS NDFCOPY to create the file.
    set ndfcopyin = "in=$fname out=$fname2"
    echo " "
    echo "ndfcopy $ndfcopyin"
    echo " "
    ndfcopy $ndfcopyin 
 
  end 
 
# Display the final result
  echo "Created file: $fname2"
  
else
 
  echo " "
  echo "Wrong number of arguments: 2 expected."
  echo " "
  
endif
 
farewell:
 
exit


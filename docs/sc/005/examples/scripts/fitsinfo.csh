#! /bin/csh
 
# Purpose:
# Looks at the FITS format files (or NDF files containing FITS extensions)
# chosen by the user and determines the value of a specified FITS header field. 
# The output is placed in a file.
 
# Parameters:
# Input $1  -  The name of the images required (wildcards allowed).
# Input $2  -  The name of the FITS header field required.
# Input $3  -  The name of the files into which the data should be written.
 
# Note:
# Values of $1 involving wildcards must be placed between " characters.
#              
# Values of $2 must be upper case only.
 
# Typical usage:
# fitsinfo hyak.fit EXPO keep
# fitsinfo "*.fit" OBS outfile   (if wildcard names are involved)

# Authors:
#  GJP: G J Privett (Cardiff)
#  ACD: A C Davenhall (Edinburgh)

# History:
#  Spring 97 (GJP): Original version.
#  9/4/99    (ACD): Modified for SC/5.2.
 
# Turn on STARLINK stuff. Normally done in your .cshrc file.
  source /star/etc/login  
  source /star/etc/cshrc
 
# Turn on KAPPA and CONVERT.
  convert
  kappa
 
# Check the number of input parameters.
set nvar = $#argv
if ( $nvar == "3" ) then
 
# Prepare output file.
  touch $3
  rm -f $3
 
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
 
# Remove temporary files.
    touch junkjunk
    rm -f junkjunk
 
# Look at the file name and remove the .sdf if present.
# Most STARLINK programs require all NDF file names to
# be supplied without the .sdf part of the name.
# Then apply suitable KAPPA applications to obtain the 
# required information and place it in file junkjunk.
    set endbit = "$filn:e"  
    if ( $endbit != "sdf") then 
 
# Keep the user informed.    
      echo "Processing file $filn"
      set fname = $filn
 
# Set a suitable message for later.
      set msgout = "In file $filn "
 
    else
 
# Keep the user informed. 
      echo "Processing NDF $filn"
      set fname = "$filn:r"
 
# Set a suitable message for later.
      set msgout = "In NDF file $filn "
 
    endif 
 
# Use FITSLIST to list the FITS header of the image.
    set fitslistin = "in=$fname logfile=junkjunk"
    echo " "
    echo "fitslist $fitslistin"
    echo " "
    fitslist $fitslistin 
 
# Grep the output file for the required heading.
    touch $3
    echo $msgout >> $3 
    grep $2 junkjunk >> $3
 
# Remove the temporary files.
    rm -f junkjunk
 
  end 
 
# Display the final result
  echo "The values found were:"
  echo " "
  cat $3
 
else
 
  echo " "
  echo "Wrong number of arguments: 3 expected."
  echo " "
  
endif
 
farewell:
 
exit


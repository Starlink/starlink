#!/bin/csh
#
# !!begin 
# !!title  Lists all formats of files without repetition
# !!author T.R. Marsh
# !!created   14 January  2005
# !!revised   08 December 2005
# !!root   uniqformat
# !!index  uniqformat.csh
# !!descr  Shell script for listing unique formats of files
# !!head1  Script for listing unique formats of files
#
# !!emph{uniqformat} lists the formats of all sdf CCD data files within
# a given directory tree. It uses 'find' to look for all files of type 
# r*[0-9].sdf starting from the current directory. It only lists different 
# formats. i.e. if it finds files of the same format as an earlier one, 
# it skips it. Thus the file listed with the format is the first one found
# to have it.
#
# !!end 

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login

# set arrays which will be used to
# keep track of the formats

set farray = 0
set warray = 0
set xarray = 0
set yarray = 0
set iarray = 0

set nonomatch

foreach file (`find . -name "r*[0-9].sdf"`)

  set window = `hdstrace $file:r.more.fits nlines=all eachline | grep WINSEC1 | \
  sed -e 's/^.*\[/\[/' -e 's/\].*$/\]'/`

  set xbin = `hdstrace $file:r.more.fits nlines=all eachline | grep CCDXBIN | \
  sed -e 's/^.*= *//' -e 's/ *\/.*$//'`

  set ybin = `hdstrace $file:r.more.fits nlines=all eachline | grep CCDYBIN | \
  sed -e 's/^.*= *//' -e 's/ *\/.*$//'`

  set detector = `hdstrace $file:r.more.fits nlines=all eachline | grep DETECTOR | \
  sed -e "s/^.*= '//" -e 's/\([a-zA-Z0-9]*\).*$/\1/'`

  if($detector == "MARCONI2") then
    set inst = "ISISR   "
  else if($detector == "EEV12") then
    set inst = "ISISB   "
  else if($detector == "TEK2") then
    set inst = "AUX     "
  else
    set inst = "UNKNOWN "
  endif

# check this format has not already appeared

  set diff = 1
  set n    = 0
  while ( $n < $#warray )
    @ n++ 
    if( $window == $warray[$n] && \
        $xbin   == $xarray[$n] && \
        $ybin   == $yarray[$n] && \
        $inst   == $iarray[$n] ) then
      set diff = 0
      break
    endif
  end
 
# append new format

  if($diff) then
  
    set farray = ($farray $file:r)
    set warray = ($warray $window)
    set xarray = ($xarray $xbin)
    set yarray = ($yarray $ybin)
    set iarray = ($iarray $inst)

    echo "$file:r  $inst  $window  $xbin x $ybin"

  endif

end

exit;





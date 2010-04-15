#!/bin/csh
#
# !!begin
# !!title  Lists format of files
# !!author T.R. Marsh
# !!created   11 January 2005
# !!created   08 December 2005
# !!root   listformat
# !!index  listformat.csh
# !!descr  Shell script for listing format of files
# !!head1  Script for listing format of files
#
# !!emph{listformat} lists the format of sdf CCD data files. This
# is essential information for judging whether one can apply bias
# subtraction etc.
#
# Arguments: just give a series of fits file names, without the trailing
# '.sdf'
#
# !!end

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login

if($#argv < 1) then
  echo "usage: listformat file1 file2 ..."
  exit
endif

# used sed to list just the headers

set nonomatch

foreach file ($argv)

  if(-e $file.sdf) then

    set window = `hdstrace $file.more.fits nlines=all eachline | grep WINSEC1 | \
    sed -e 's/^.*\[/\[/' -e 's/\].*$/\]'/`

    set xbin = `hdstrace $file.more.fits nlines=all eachline | grep CCDXBIN | \
    sed -e 's/^.*= *//' -e 's/ *\/.*$//'`

    set ybin = `hdstrace $file.more.fits nlines=all eachline | grep CCDYBIN | \
    sed -e 's/^.*= *//' -e 's/ *\/.*$//'`

    set detector = `hdstrace $file.more.fits nlines=all eachline | grep DETECTOR | \
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

    echo "$file   $inst   $window  $xbin x $ybin"

  else

    echo "File  = $file.sdf does not exist"

  endif

end

exit;





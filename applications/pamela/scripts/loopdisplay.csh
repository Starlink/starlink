#!/bin/csh
#
# !!begin
# !!title  Displays a list of files
# !!author T.R. Marsh
# !!created 12 January 2005
# !!revised 08 December 2005
# !!root   loopdisplay
# !!index  loopdisplay.csh
# !!descr  Shell script for displaying a list of files
# !!head1  Script for displaying a list of files
#
# Prior to combining a set of files to make a flat or bias, you should
# check that they are the same. The best way is to look at them, and this
# script is designed to help this by running figaro's 'image' program
# for each one. You should first use this and set its defaults before
# running this routine.
#
# Arguments: just give a series of file names. Optionally -h n will halt for n seconds halt between each frame.
# This must come first if it used.
#
# !!end

if( ($#argv < 1) || ( ($1 == "-h") && ($#argv < 3) ) ) then
  echo "usage: listformat (-h n) file1 file2 ..."
  exit
endif

if($1 == "-h") then
  set nsec = $2
  shift; shift
else
  set nsec = 0
endif

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login
figaro > /dev/null

foreach file ($argv)

  if(-e $file:r.sdf) then

    echo "Displaying file = $file:r.sdf"
    image $file:r idev=xw \\
    if($nsec != "0") then
      sleep $nsec
    endif
  else

    echo "File  = $file:r.sdf does not exist"

  endif

end

exit





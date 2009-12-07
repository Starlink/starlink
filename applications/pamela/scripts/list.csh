#!/bin/csh -f
#
# Fix run number for SAAO data. Must be run
# after fixhead

set HDSTRACE = $STARLINK_DIR/bin/hdstrace

if($#argv < 2) then
  echo "usage: list.csh hitem file1 file2 ..."
  exit
endif

set hitem = $argv[1]
shift

foreach file ($argv)
    echo $file
  if($file:e != "sdf") then
    echo "Skipped $file as it does not end with .sdf"
  else if(-e "$file") then

    echo "File = $file $file:r.$hitem"
    $HDSTRACE $file:r.$hitem

  endif

end

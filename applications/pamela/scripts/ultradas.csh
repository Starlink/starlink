#!/bin/csh
#
# !!begin 
# !!title  ultradas fixer script
# !!author T.R. Marsh
# !!created 14 January 2001
# !!revised 08 Decemmber 2005
# !!root   ultradas
# !!index  ultradas.csh
# !!descr  C-shell script for fixing sdf files from ultradas at ING
# !!head1  ultrdas fixer script
#
# !!emph{ultradas} is needed to fix the sdf files produced by the ultradas
# data acquisition system on La Palma. This writes the data to a fits
# extension and this needs to be renamed. The script can be invoked 
# with wildcards as in ./ultrdas.csh *.sdf to fix all sdf files.
#
# WARNING! This script is written to first copy then delete the
# appropriate objects. A faster way is to delete the objects about
# to be copied into and then rename the ultradas objects. This is 
# unsafe however as it would destroy not-ultradas files, so do not
# modify this script for efficiency.
#
# !!head2 Arguments
#
# !!table
# !!arg{file1, file2}{are individual files.}
# !!table
#
# !!end

if($#argv == 0) then
  echo "usage: ultradas file1 file2 ..."
  exit
endif

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login
figaro > /dev/null

foreach file ($*)
  if($file:e != "sdf") then
    echo "Skipped $file as it does not end with .sdf"
  else if(-e "$file") then
    set root = $file:r

# save stuff just about to overwrite in order to make it potentially
# recoverable

    renobj $root.data_array.data $root.data_array.saved_data 
    renobj $root.data_array.origin $root.data_array.saved_origin 

# rename

    renobj $root.more.fits_ext_1.data_array.data $root.data_array.data 
    renobj $root.more.fits_ext_1.data_array.origin $root.data_array.origin

    echo "Fixed $file"
  else
    echo "No file called $file"
  endif
end

exit






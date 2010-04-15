#!/bin/csh -f
#
# !!begin
# !!title  Lists sdf files without trailing sdf
# !!author T.R. Marsh
# !!created 11 December 2005
# !!root   lsnosdf
# !!index  lsnosdf.csh
# !!descr  Shell script for listing sdf files in a directory but without trailing .sdfs
# !!head1  List sdf files without the trailing .sdf
#
# It is often necessary to compile file lists of sdf files but the
# Starlink routines do not like the trailing .sdf to be present
# !!emph{lsnosdf} is designed to produce such lists. It only lists
# sdf files and therefore you do not need to add .sdf in your file
# selection.
#
# Example: 'lsnosdf r*' will list all files of the form r*.sdf but without the .sdf
# This can be piped to a file or whatever as usual
# !!end

if($#argv < 1) then
  echo "usage: lsnosdf file1 file2 ..."
  exit
endif

foreach arg ($argv)
  set newpattern = `echo $arg | sed -e 's/\.sdf$//'`
  if($newpattern.sdf == $arg) then
    echo $newpattern
  endif
end

exit;





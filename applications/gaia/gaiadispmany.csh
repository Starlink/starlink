#!/bin/csh

#+
#  Name:
#     gaiadispmany.csh

#  Purpose:
#     Display a list of images in GAIA.

#  Language:
#     C-Shell

#  Invocation:
#     gaiadispmany.csh image1 image2 image3 ...

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     09-SEP-2003 (PWD):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  If available and not set then initialize the CONVERT package.
if ( $?CONVERT_DIR && ! $?NDF_FORMATS_IN ) then
   alias echo 'echo >/dev/null'
   if ( "`alias convert`" != "" ) then 
      convert
   else 
      if ( -e $CONVERT_DIR/convert.csh ) then 
         source $CONVERT_DIR/convert.csh
      endif
   endif
   unalias echo
endif

#  Now start up the application proper. 
if ( $?GAIA_DIR ) then 
   $GAIA_DIR/gaiadispmany.sh "$1" $argv[2-]
else if ( -e /star/bin/gaia/gaiadispmany.sh ) then 
   /star/bin/gaia/gaiadispmany.sh "$1" $argv[2-]
else 
   echo Sorry cannot find 'gaiadispmany' anywhere on your system
endif

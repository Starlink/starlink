#!/bin/csh

#+
#  Name:
#     gaiadisp.csh

#  Purpose:
#     Display an image in the GAIA application.

#  Language:
#     C-Shell

#  Invocation:
#     gaiadisp.csh clone_number

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-JAN-1997 (PDRAPER):
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
   $GAIA_DIR/gaiadisp.sh $*
else if ( -e /star/bin/gaia/gaiadisp.sh ) then 
   /star/bin/gaia/gaiadisp.sh $*
else 
   echo Sorry cannot find 'gaiadisp' anywhere on your system.
endif

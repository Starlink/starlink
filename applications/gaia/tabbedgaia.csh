#!/bin/csh 

#+
#  Name:
#     tabbedgaia.csh

#  Purpose:
#     Start the GAIA application with the tabbed interface.

#  Language:
#     C-Shell

#  Invocation:
#     gaia.csh

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     10-OCT-2003 (PWD):
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
   $GAIA_DIR/tabbedgaia.sh "$1" $argv[2-]
else if ( -e /star/bin/gaia/tabbedgaia.sh ) then 
   /star/bin/gaia/tabbedgaia.sh "$1" $argv[2-]
else 
   echo Sorry cannot find tabbed GAIA anywhere on your system
endif

#!/bin/csh -f

#+
#  Name:
#     gaia_standalone.csh

#  Purpose:
#     Start the GAIA application in standalone mode.

#  Language:
#     C-Shell

#  Invocation:
#     gaia.csh

#  Notes:
#     Needs the release created by the "makecherryrelease" script.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     03-OCT-2000 (PWD):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  All Starlink packages are installed with respect to GAIA_DIR.
if ( ! $?GAIA_DIR ) then
   echo Sorry cannot find GAIA anywhere on your system.
   echo Define the GAIA_DIR environment variable.
   exit
endif
setenv KAPPA_DIR $GAIA_DIR/../kappa
setenv PHOTOM_DIR $GAIA_DIR/../photom
setenv CONVERT_DIR $GAIA_DIR/../convert
setenv EXTRACTOR_DIR $GAIA_DIR/../extractor
setenv ESP_DIR $GAIA_DIR/../esp

#  Initialize the CONVERT package.
alias echo 'echo >/dev/null'
source $CONVERT_DIR/convert.csh
setenv CONVERT_DIR $GAIA_DIR/../convert
unalias echo

#  Now start up the application proper.
$GAIA_DIR/gaia.sh "$*"


#  N.B. the previous line should be blank.
#+
#  Name:
#     GAIA

#  Purpose:
#     Starts the GAIA image display & analysis tool.

#  Type of Module:
#     Bourne shell script

#  Usage:
#     gaia

#  Description:
#     This command starts the GAIA image display & analysis tool.
#
#     GAIA stands for Graphical Astronomy and Image Analysis tool.

#  Authors:
#     PDRAPER: P.W. Draper (STARLINK, Durham University)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PDRAPER):
#        Original version.
#     17-MAY-1999 (PDRAPER):
#        Now starts up single binary version of GAIA.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Check that GAIA_DIR is around.

if test "$GAIA_DIR" = ""; then
  echo "!! Cannot start gaia; the GAIA_DIR environment variable is not set."
  exit 1
fi

#  Notes:
#     The ${1+"$@"} passes on any command line arguments? Perhaps $0
#     is also required?

#  Check that GAIA is installed.
if test -x $GAIA_DIR/gaia_swish; then

#  Try to stop problems with the AMS rendevous files by creating a
#  new directory as ADAM_USER.
  OLD_ADAM_USER=${ADAM_USER:-"${HOME}/adam"}
  ADAM_USER=${OLD_ADAM_USER}/gaia_$$
  export ADAM_USER

#  Make sure directory exists.
  if test -f $ADAM_USER; then
    rm -r -f $ADAM_USER
  fi
  mkdir -p $ADAM_USER

#  Make sure we remove this all on exit.
  trap 'rm -r -f $ADAM_USER;exit' 0 1 2 3 9 15

#  Add the local catalogue configuration file. Do not use broken CURSA 
#  version.
  if test -z "$SKYCAT_CONFIG" -o \
   "$SKYCAT_CONFIG" = "http://ledas-www.star.le.ac.uk/arnieV4/SkyCatConfig.pl"; then
    SKYCAT_CONFIG=file:${GAIA_DIR}/skycat.cfg
    export SKYCAT_CONFIG
  fi

#  And run up the interface.
  $GAIA_DIR/gaia_swish ${1+"$@"}

else
  echo "!! Cannot start gaia; failed to locate $GAIA_DIR/gaia_swish."
  exit 1
fi

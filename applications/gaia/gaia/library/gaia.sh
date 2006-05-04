
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
#     PWD: P.W. Draper (STARLINK, Durham University)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  History:
#     21-NOV-1996 (PWD):
#        Original version.
#     17-MAY-1999 (PWD):
#        Now starts up single binary version of GAIA.
#     23-SEP-2004 (PWD):
#        Added CYGWIN environment variable.
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

#  Set the BLT_LIBRARY variable. This is needed to locate some .pro files.
  BLT_LIBRARY=$GAIA_DIR
  export BLT_LIBRARY

#  Cywgin setup. Need "server" in CYGWIN for cygserver to work.
  if test "$CYGWIN" = ""; then
     CYGWIN="server"
  else
     CYGWIN="$CYGWIN server"
  fi
  export CYGWIN

#  And run up the interface.
  $GAIA_DIR/gaia_swish ${1+"$@"}

else
  echo "!! Cannot start gaia; failed to locate $GAIA_DIR/gaia_swish."
  exit 1
fi

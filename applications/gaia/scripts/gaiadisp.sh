#!/bin/sh
#+
#  Name:
#     gaiadisp.sh

#  Purpose:
#     Display an image in the GAIA application.

#  Language:
#     Bash

#  Invocation:
#     gaiadisp.sh --help for options

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA"

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-NOV-2007 (PWD):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Check if GAIA_DIR is around, if not locate this script.
if test "$GAIA_DIR" = ""; then
   PRG="$0"
   while test -h "$PRG"; do
      ls=`ls -ld "$PRG"`
      link=`expr "$ls" : '.*-> \(.*\)$'`
      if expr "$link" : '/.*' > /dev/null; then
         PRG="$link"
      else
         PRG=`dirname "$PRG"`/"$link"
      fi
   done
   GAIA_DIR=`dirname "$PRG"`
   export GAIA_DIR
fi

#  Initialise the CONVERT package if not already done so.
if test "$CONVERT_DIR" != "" -a "$NDF_FORMATS_IN" = ""; then
   . $CONVERT_DIR/convert.sh > /dev/null
   export CONVERT_DIR
fi

#  Now start up the application proper.
if test "$GAIA_DIR" != ""; then
   $GAIA_DIR/gaiadisp ${1+"$@"}
elif test -f "/star/bin/gaia/gaiadisp"; then
   /star/bin/gaia/gaiadisp ${1+"$@"}
else
   echo Sorry cannot find 'gaiadisp' anywhere on your system
fi
exit

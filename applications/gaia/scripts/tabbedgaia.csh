#!/bin/csh -f

#+
#  Name:
#     tabbedgaia.csh

#  Purpose:
#     Start the GAIA application with the tabbed interface.

#  Language:
#     C-Shell

#  Invocation:
#     gaia.csh

#  Copyright:
#     Copyright (C) 2003-2005 Central Laboratory of the Research Councils.
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
#     02111-1307, USA"

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
   if ( -e $CONVERT_DIR/convert.csh ) then 
      alias echo 'echo >/dev/null'
      source $CONVERT_DIR/convert.csh
      unalias echo
   endif
endif

#  Now start up the application proper. 
if ( $?GAIA_DIR ) then 
   eval $GAIA_DIR/tabbedgaia.sh "$*"
else if ( -e /star/bin/gaia/tabbedgaia.sh ) then 
   eval /star/bin/gaia/tabbedgaia.sh "$*"
else 
   echo Sorry cannot find tabbed GAIA anywhere on your system
endif

#!/bin/csh -f

#+
#  Name:
#     gaiadisp.csh

#  Purpose:
#     Display an image in the GAIA application.

#  Language:
#     C-Shell

#  Invocation:
#     gaiadisp.csh --help for options

#  Copyright:
#     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2007 Science and Technology Facilities Council.
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
#     23-JAN-1997 (PWD):
#        Original version.
#     28-JUL-2000 (PWD):
#        Changed argument passing to quote filename. This may have 
#        special characters to protect.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  If available and not set then initialize the CONVERT package. Note this
#  script only exists for this purpose (the gaiadisp.sh script also
#  initialises CONVERT, but that is bash specific, not sh, so this is more
#  portable).
if ( $?CONVERT_DIR && ! $?NDF_FORMATS_IN ) then
   if ( -e $CONVERT_DIR/convert.csh ) then 
      alias echo 'echo >/dev/null'
      source $CONVERT_DIR/convert.csh
      unalias echo
   endif
endif

#  Now start up the application proper. 
if ( $?GAIA_DIR ) then 
   eval $GAIA_DIR/gaiadisp.sh "$*"
else if ( -e /star/bin/gaia/gaiadisp.sh ) then 
   eval /star/bin/gaia/gaiadisp.sh "$*"
else
   echo Sorry cannot find 'gaiadisp' anywhere on your system
endif

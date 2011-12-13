   proc CCDSetIconBitmap { Topwin } {
#+
#  Name:
#     CCDSetIconBitmap

#  Purpose:
#     Sets the icon bitmap of a top-level widget to the standard CCDPACK
#     icon bitmap.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk

#  Global Parameters:
#     CCDdir = string (read)
#        The name of the directory in which CCDPACK procedure etc.
#        reside.

#  Parameters:
#     topwin = window (read)
#        The name of the top-level object.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2000 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     2-MAR-1994 (PDRAPER):
#        Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global CCDdir
#.

#  Set the icon bitmap.
      set topwin [CCDPathOf $Topwin]
      wm iconbitmap $topwin @$CCDdir/ccdbitmap
      }
# $Id$

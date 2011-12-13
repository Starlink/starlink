   proc CCDReadGlobals { topwin } {
#+
#  Name:
#     CCDReadGlobals

#  Purpose:
#     Reads and restores a setup file.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     First a prompt is made for the name of a file from which to read
#     a saved setup. This defaults to $HOME/.ccdpack. Then this file is
#     sourced restoring the state. (State files are written out by the
#     CCDSaveGlobals procedure and consist of Tcl commands to restore
#     the names and values of all the CCDxxxxx global parameters).

#  Arguments:
#     topwin = window (read)
#        The name of the top-level window in which to make a prompt for
#        a filename.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2001 Central Laboratory of the Research
#     Councils. All Rights Reserved.

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
#     6-MAY-1994 (PDRAPER):
#        Original version.
#     9-MAY-1994 (PDRAPER):
#        Reads a state file.
#     21-AUG-1995 (PDRAPER):
#        Changed to use sensible defaults for CCDimportfilter.
#     29-AUG-1995 (PDRAPER):
#        Changed to use .ccdpack in the current directory as the
#        default.
#     3-JUL-2001 (MBT):
#        Modified the arguments of CCDGetFileName.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global CCDimportfile
      global CCDimportexists
      global CCDimportfilter

#.

#  Get the name of a file to receive the current status.
      set CCDimportfile ".ccdpack"
      if { [ info exists CCDimportfilter] } {
         set oldfilt $CCDimportfilter
      } else {
         set oldfilt "*"
      }
      set CCDimportfilter ".*"
      CCDGetFileName $topwin "Restore state from file" 0
      if { $CCDimportexists } {

#  Source the file in the global scope.
         uplevel #0 source $CCDimportfile
      }

#  Restore input file filter.
      set CCDimportfilter $oldfilt

#  End of procedure.
   }
# $Id$

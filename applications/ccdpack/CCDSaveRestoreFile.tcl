   proc CCDSaveRestoreFile { filename } {
#+
#  Name:
#     CCDSaveRestoreFile.tcl

#  Purpose:
#     Saves the current global parameter setup to the named file.

#  Language:
#     TCL

#  Type of Module:
#     Tk/Tcl

#  Description:
#     This routine opens the named file and writes the contents of the
#     global parameter array CCDglobalpars into it. The form used is
#     the same as that of CCDSETUP and which can also be read by
#     CCDReadRestoreFile . A header of useful information is also
#     written to the file.

#  Arguments:
#     filename = string (read)
#        The name of the file which is to have the current global setup
#        written to it.

#  Global Parameters:
#      CCDglobalpars = array (read)
#         The array of global parameter values.

#  Return:
#      CCDrestorefile = logical (write)
#         If the write is successful then the return is set to 1
#          otherwise it is 0.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
#     22-FEB-1994 (PDRAPER):
#        Original version.
#     4-JUL-2001 (MBT):
#        Prevented it from saving ZEROED.  What is ZEROED?
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global CCDglobalpars
      global env                            # Environment variables

#.
      set ok 0

#  Try to open the file.
      if { ![ catch {open $filename w} fileid ] } {

#  File opened successfully write header.
         catch { exec date } date
         puts $fileid "#"
         puts $fileid "#  CCDPACK - restoration file"
         puts $fileid "#"
         puts $fileid "#  Written by $env(USER) on $date"
         puts $fileid "#"

#  Now add the global contents.
         foreach param [lsort -dictionary [array names CCDglobalpars ]] {
            if { $CCDglobalpars($param) != "" &&
                 ![string match *ZEROED $param] } {
               puts $fileid "$param = $CCDglobalpars($param)"
            }
         }
         flush $fileid
         set ok 1
      } else {

#  Failed to open file.
         CCDIssueError "Failed to open file: $filename"
      }

#  Return
      return $ok
   }
# $Id$

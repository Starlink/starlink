   proc CCDNameListFile { filename names } {
#+
#  Name:
#     CCDNameListFile

#  Purpose:
#     Write a list of names to a file.

#  Language:
#     TCL

#  Description:
#     This routine writes a list of names to a file, one per line.
#     It is intended for use when constructing indirection files for
#     presenting to ADAM tasks.

#  Arguments:
#     filename = string
#        The name of a file to which the names are written.
#     names = list of strings
#        A list of names to write, one per line.

#  Copyright:
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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     18-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Attempt to open the file for writing.
      if { ! [catch { open $filename w } fileid] } {

#  Write short header.
         puts $fileid "#  Temporary file written by CCDNameListFile.tcl"

#  Write all the requested names.
         foreach name $names {
            puts $fileid $name
         }
         close $fileid

#  Open failed - log an error.
      } else {
         CCDIssueError "Failed to open temporary file ($fileid)"
      }
   }
# $Id$

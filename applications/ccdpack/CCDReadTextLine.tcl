   proc CCDReadTextLine { fileid } {

#+
#  Name:
#     CCDReadTextLine.tcl

#  Purpose:
#     Reads a line of information from a text file.

#  Language:
#     TCL

#  Type of Module:
#     Tk/Tcl

#  Description:
#     This procedure reads in a line of information from a file which has
#     its contents in the usual CCDPACK text file format. This allows for
#     comments (indicated by the characters "!" and "#") continuation
#     lines (indicated by a "-" at the end of the line) and blank lines.
#     Comments are skipped or removed, continued lines are joined and
#     blank lines are skipped. The return is an extracted line of
#     information ready for processing. This is in the original case.

#  Arguments:
#     fileid = string (read)
#        The Tcl file descriptor (returned from an open command)

#  Returned Value:
#     CCDReadTextLine = string
#        The extracted (joined, comment stripped etc.) line
#        read from the file. If an error occurs (such as no more lines
#        to read etc.) then a null value is returned.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council. All
#     Rights Reserved.

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
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#        Original version.
#     4-MAR-1994 (PDRAPER):
#        Now named CCDReadTextLine
#     3-MAY-1994 (PDRAPER):
#        Doesn't convert to uppercase any more (case sensitive filenames
#        were being disrupted).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   set append 0
   set result ""
   while { [gets $fileid templine] != "-1" } {

#  If this is the first part do not append to any existing characters
#  in line
      set templine [string trim $templine]
      if { $append == "1" } {
         set line "$line $templine"
      } else {
         set line $templine
      }
      set append 0

#  Skip blank line.
      if { $line != "" } {

#  Is this a complete comment line?
         set commentat [ string first "#" $line ]
         if { $commentat == "-1" } {
            set commentat [string first "!" $line]
         }

#  If # or ! is at the start of the line skip it.
         if { $commentat != "0" } {

#  Is the comment in-line?
            if { $commentat != "-1" } {
               set line [string range $line 0 [expr $commentat-1] ]
            }
            set line [string trim $line]
            set stringlength [expr [string length $line]-1]

#  Look for the continuation character at the end of the line.
            set lastchar [string range $line $stringlength $stringlength]
            if { $lastchar == "-" } {

#  Have a continuation line. Back for next part.
               set line [string range $line 0 [expr $stringlength-1] ]
               set append 1
               continue
            }

#  Lines joined etc. Return value.
         set result $line
         break
         }
      }
   }

#  End of procedure, return string.
   return $result
   }
# $Id$

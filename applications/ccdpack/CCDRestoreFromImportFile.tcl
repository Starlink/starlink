   proc CCDRestoreFromImportFile { importtable fitstable extentable } {
#+
#  Name:
#     CCDRestoreFromImportFile

#  Purpose:
#     Reads the contents of an import table entering the contents into
#     two tables widgets.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine reads in a standard CCDPACK import control table.
#     Any occurrences of FITS item declarations are located and entered
#     into the fitstable window (which should be a table widget of
#     size 2). Occurrences of CCDPACK extension items are entered into
#     the extentable window (which should also be a table of size 2)

#  Parameters:
#     importtable = filename (read)
#        The name of an existing import control table.
#     fitstable = window (read)
#        A table widget to contain the FITS item declarations (will be
#        emptied first).
#     extentable = window (read)
#        A table widget to contain the extension item expression (will be
#        emptied first).

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
#     {enter_new_authors_here}

#  History:
#     4-MAR-1994 (PDRAPER):
#        Original version.
#     8-APR-1994 (PDRAPER):
#        Now inserts into multitem listboxes.
#     22-AUG-1995 (PDRAPER):
#        Now inserts into table widgets.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
#.

#  Open the file in it exists.
      if { [ file readable $importtable ] } {
         set fileid [open $importtable r]

#  Clear the multitem widgets.
	 $fitstable clear 0 end
	 $extentable clear 0 end

#  Read file until no more lines are found.
         while { [set line [CCDReadTextLine $fileid] ] != "" } {

#  Look for _ in the first character. This indicates a FITS item.
#  Anything else should be an extension item expression.
	    switch -glob $line {
	       _* {

#  Leading _, must be FITS item.
                  $fitstable insert end [lindex $line 0] [lrange $line 1 end]
               }

               ?_* {

#  Problem case. May have a type expression as second word. Make line a
#  list and look at second element for leading _.
                  set lineaslist [split $line]
                  set listlength [llength $lineaslist]
                  if { $listlength > 2 } {
                     set secondword [lindex $lineaslist]
                     if { [string match _* $secondword] } {

#  Second word is type, needs removing from line.
                        set newline \
       "[lindex $lineaslist 1] [lrange $lineaslist 3 $listlength]"
                        $extentable insert end [lindex $line 0] [lrange $line 1 end]
                     } else {

#  Must an underscore elsewhere (in FITS function), just accept this.
                        $extentable insert end [lindex $line 0 ] [lrange $line 1 end ]
                     }
                  } else {

#  Only two words must be ok!
                     $extentable insert end [lindex $line 0] [lrange $line 1 end]
                  }
               }
               default {

#  Definitely no type expression. Just enter this into the extension
#  items listbox.
                  $extentable insert end [lindex $line 0 ] [lrange $line 1 end ]
               }
	    }
         }

#  Close file and exit.
         close $fileid
      } else {

#  Cannot read the file. Issue an error message.
         CCDIssueError "Cannot read import control table: $importtable"
      }

#  End of procedure.
   }
# $Id$

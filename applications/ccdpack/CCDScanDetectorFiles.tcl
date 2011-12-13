   proc CCDScanDetectorFiles { Box } {
#+
#  Name:
#     CCDScanDetectorFiles

#  Purpose:
#     Locates and types Detector files.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure tries to locate files in the directories whose
#     name are contained in the CCDdetectorcache global variable. The
#     files should have a type of .DAT and are assumed to contain
#     restoration file details or be an FITS import table.
#
#     To determine which files are which they are scanned for a
#     signature of 'keyword = '. This indicates a restoration file,
#     others are assumed to be import control tables.
#
#     After the type of the file is determined its name is entered
#     into a Ccd::multitem whose name is $Box together with the
#     description "No description available (type)" if the first line
#     of the file is just an empty comment, otherwise the first line is
#     shown as a description (again with the type appended in ()). The
#     types are identified by the strings "setup" and "table".

#  Arguments:
#     Box = window (read)
#       Name of a size 2 Ccd::multitem. The name of any files located
#       together with a description are entered into this.

#  Returns:
#     False if no files are located.

#  Copyright:
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     14-SEP-1995 (PDRAPER):
#        Original version.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global CCDdetectorcache

#.

#  Check we have some directories to scan.
      if { [info exists CCDdetectorcache] } {
         foreach dir $CCDdetectorcache {
            if { [file isdirectory $dir] } {
               set files [lsort -dictionary [glob -nocomplain "$dir/*.DAT"]]
               if { $files != "" } {
                  foreach file $files {
		     set f [open $file r]
		     set contents [read $f]
		     close $f
                     set contents [split $contents "\n"]

#  Check for file with line pattern: space word space = space word, spaces
#  are optional.
		     set index [lsearch -regexp $contents {^[ ]*[^\ ]+[ ]*=[ ]*[^\ ]+}]
                     if { $index == -1 } {

#  Import control table.
                        set type "(table)"
                     } else {

#  Restoration file.
                        set type "(setup)"
                     }

#  Look for a comment.
		     set comment [lindex $contents 0]
                     if { $comment == "\#" || $comment == "" } {
                        set comment "No description available"
                     }
                     set comment "$comment $type"

#  Insert the business.
                     $Box insert end $file $comment
                  }
               }
            }
         }
      }

#  End of procedure.
      set failed [$Box size]
      if { $failed == 0 } {
         return 0
      } else {
         return 1
      }
   }
# $Id$

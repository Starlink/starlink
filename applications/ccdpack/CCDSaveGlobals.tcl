   proc CCDSaveGlobals { Top } {
#+
#  Name:
#     CCDSaveGlobals

#  Purpose:
#     Saves the current setup.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure saves the values of all the current CCDPACK related
#     global parameters into a file. This file may be read by the
#     corresponding application CCDReadGlobals which restores the
#     current state.
#
#     A prompt is made for the name of a file in which to save the
#     current setup. This defaults to .ccdpack in the current directory.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window in which to make a prompt for
#        a filename.

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
#     6-MAY-1994 (PDRAPER):
#        Original version.
#     29-AUG-1995 (PDRAPER):
#        Modified to look for default .ccdpack file in the current
#        directory.
#     14-NOV-1995 (PDRAPER):
#        Stopped CCDdir from being saved.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global CCDimportfile
      global CCDimportavail
      global CCDimportfilter
      global env
#.

#  Get the name of a file to receive the current status.
      set CCDimportfile "[pwd]/.ccdpack"
      set CCDimportfilter ".*"
      CCDNewFileName $Top "Save current state in file"
      if { $CCDimportavail } {

#  Got name of file. Now get all global variables and write their names
#  to this.
	 set globals [ info globals "CCD*" ]
	 if { "$globals" != "" } {

#  Try to open the file.
	    if { ! [ catch { open $CCDimportfile w } fileid ] } {

#  File opened successfully write header.
	       catch { exec date } date
	       puts $fileid "#"
	       puts $fileid "#  CCDPACK - interface status"
	       puts $fileid "#"
	       puts $fileid "#  Written by $env(USER) on $date"
	       puts $fileid "#"

#  Loop for each variable. Check if it is an array if so extract
#  element other wise just write out the variable value.
	       foreach var $globals {
                  global "$var"
                  if { [array exists $var] } {

#  Is an array. Get elements and write out their values.
                     set names [ array names $var ]
		     foreach element $names  {
			eval set value $${var}($element)
			puts $fileid "set ${var}($element) \{$value\}"
		     }
		  } else {

#  Mustn't be an array.
                     if { "$var" != "CCDdir" } {
                        eval set value $${var}
                        puts $fileid "set $var \{$value\}"
                     }
		  }
	       }

#  Make sure that file contents are complete and close it.
	       flush $fileid
	       close $fileid
	    } else {

#  Failed to open file.
	       CCDIssueError "Failed to open file: $CCDimportfile"
	    }
	 } else {
	    CCDIssueError "No setup to save"
	 }
      }
#  End of procedure.
   }
# $Id$

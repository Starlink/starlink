   proc CCDIssueError { message } {
#+
#  Name:
#     CCDIssueError

#  Purpose:
#     Issues an error using a dialog box if called from a Tk script.
#     Otherwise the error is output to standard output.

#  Arguments:
#     message = string (Given)
#        The message to with error.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk shell script

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 1999, 2001 Central Laboratory of the Research
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
#     21-FEB-1994 (PDRAPER):
#        Original version.
#     31-AUG-1995 (PDRAPER):
#        Removed cluttering message about calling routine.
#     13-MAY-1999 (PDRAPER):
#        Modified to use a window that is a child of .topwin (needed
#        to control transient behaviour).
#     19-JUN-2001 (MBT):
#        Upgraded for use with Tcl8.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global tk_version
   global MAIN

#  See if this is a Tk application or not.
   if {[info exists tk_version ]} {
      set is_tk 1
   } else {
      set is_tk 0
   }

#  Now issue the error.
   if { $is_tk } {

#  Check that error window doesn't already exist, if it does wait for
#  it to go away before proceeding.
      set Issueerror $MAIN(window).ccdissueerror
      set issueerror [CCDPathOf $Issueerror]
      if { [winfo exists $issueerror] } {
         tkwait window $issueerror
      }
      CCDDialog $Issueerror "Error..." "$message" error
   } else {
      puts "Error. $message"
   }

#  All done.
   return
   }
# $Id$

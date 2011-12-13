   proc CCDIssueInfo { message } {
#+
#  Name:
#     CCDIssueInfo.tcl

#  Purpose:
#     Issues an informational message a dialog box if called from a Tk
#     script.  Otherwise the message is just output to standard output.
#     If the Tk option is selected an OK button is shown, this needs to
#     be pressed to continue.

#  Arguments:
#     message = string (Given)
#        The message to inform user.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk shell script

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
#     2-MAR-1994 (PDRAPER):
#        Now named CCDIssueInfo.
#     13-MAY-1999 (PDRAPER):
#        Modified to use a window that is a child of .topwin (needed
#        to control transient behaviour).
#     16-MAY (MBT):
#        Upgraded for Tcl8.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global MAIN

#  See if this is a Tk application or not.
   global tk_version
   if {[info exists tk_version ]} {
      set is_tk 1
   } else {
      set is_tk 0
   }

#  Now issue the information message.
   if { $is_tk } {

#  Check that informational window doesn't already exist, if it does
#  wait for it to go away before proceeding.
      set Issueinfo $MAIN(window).ccdissueinfo
      set issueinfo [CCDPathOf $Issueinfo]
      if { [winfo exists $issueinfo] } {
         tkwait window $issueinfo
      }
      CCDDialog $Issueinfo Information "$message" info
   } else {
      puts "Error. $dialogmess"
   }

#  All done.
   return
   }
# $Id$

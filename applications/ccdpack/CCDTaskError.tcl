proc CCDTaskError { task error } {

#+
#  Name:
#     CCDTaskError

#  Purpose:
#     Makes report when application exits in error.

#  Language:
#     TCL

#  Type of Module:
#     Tcl procedure.

#  Arguments:
#     args = list (read)
#        Unused (from trace that calls this routine).

#  Global Variables:
#     TASK = array (write)
#        The array element ($task,error) is set to the error
#        message. This is for use in detecting an error condition from
#        outside this routine.

#  Copyright:
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
#     15-MAR-1995 (PDRAPER):
#        Added header.
#     2-AUG-1995 (PDRAPER):
#        Converted for use with tcl ADAM.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global TASK

#.
   if { $error != "" } {
      set TASK($task,error)  $error
      CCDIssueInfo "Application exited in error:\n$error"
   }
#  End of procedure.
}
# $Id$

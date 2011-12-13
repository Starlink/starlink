proc CCDRestartTask { task } {
#+
#  Name:
#     CCDRestartTask

#  Purpose:
#     Stops then restarts an ADAM monolith.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     task = name (read)
#        Name of application whose monolith is to be restarted.

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
#     29-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global TASK
   global MONOLITH
#.

#  Check application is known.
   if { [info exists TASK($task,monolith)] } {

#  Kill the associated monolith.
      $MONOLITH($TASK($task,monolith),taskname) kill

#  And restart it.
      CCDTaskStart $task
   } else {
      CCDIssueError "Cannot restart unknown task ($task)"
   }

#  End of procedure.
}
# $Id$

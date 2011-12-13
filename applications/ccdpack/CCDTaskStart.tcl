proc CCDTaskStart { app } {

#+
#  Name:
#     CCDTaskStart

#  Purpose:
#     Starts the monolith associated with an application.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine checks if the monolith associated with an
#     application is running. If not it attempts to load it.

#  Arguments:
#     app = string (read)
#        Name of the application that will be run. The associated
#        monolith is loaded here.

#  Global Variables:
#     MONOLITH = array (read)
#        This variable describes the known monoliths, the name of
#        their executable, their status and the command name associated
#        with them. The elements have indices,
#
#           (name,location)        ! where the monolith executable is
#           (name,status)          ! one of enabled,available,unavailable
#           (name,taskname)        ! taskname assigned here
#
#        This also has an additional element (index) that is
#        incremented to give unique tasknames to the monoliths (if
#        they are killed timing problems mean that using the same name
#        may fail)
#     TASK = array (write)
#        This variable holds the names of the available applications
#        and their associated monoliths. It also holds (after the
#        application has run) the output an error message and a a
#        return status. The elements have indices,
#
#           (name,monolith)        ! monolith associated with this command
#           (name,return)          ! only set when application completes
#           (name,error)           ! error message is fails
#           (name,output)          ! full output messages
#
#        Only the (name,monolith) element is used by this routine.

#  Return Value:
#     CCDTaskStart = boolean
#        Returns 1 if ok, 0 otherwise (failed to load monolith).

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
#     11-OCT-1995 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global TASK
   global MONOLITH

#.

#  Check that the task is known.
   if { [info exists TASK($app,monolith)] } {
      set mono $TASK($app,monolith)

#  Check is the monolith is loaded (regardless of apparent status).
      if { $MONOLITH($mono,status) == "enabled" } {
         if { [$MONOLITH($mono,taskname) path] == 0 } {
            set MONOLITH($mono,status) "available"
         }
      }
      if { $MONOLITH($mono,status) == "available" } {

#  Need new task name.
         set taskname "ct[incr MONOLITH(index)]"
         adamtask $taskname $MONOLITH($mono,location)
         set count 0
         while {[$taskname path] == 0} {
            after 250
            incr count
            if { $count > 200 } {
               CCDIssueError \
      "Failed to load the monolith $MONOLITH($mono,location)."
               set MONOLITH($mono,status) unavailable
               $taskname kill
               return 0
            }
         }
         set MONOLITH($mono,status) enabled
         set MONOLITH($mono,taskname) $taskname
      }
   } else {

#  Do not know about this application complain and exit in error.
      CCDIssueInfo "The application $app is unknown to this interface \
(possible programming error)."
      return 0
   }

#  Must be ok if got this far.
   return 1
}

# $Id$

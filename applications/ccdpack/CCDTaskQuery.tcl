proc CCDTaskQuery { app param } {

#+
#  Name:
#     CCDTaskQuery

#  Purpose:
#     Returns the value of an application parameter.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine gets the value of an application parameter. The
#     monolith associated with the application must be already started.
#     If no value exists a "" is returned.

#  Arguments:
#     app = string (read)
#        Name of the application to be queried.
#     param = string (read)
#        The name of the parameter.

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
#     CCDTaskQuery = string
#       Returns the parameter value, otherwise a "" is returned.

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
#     21-OCT-1995 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global TASK
   global MONOLITH
#.

#  Default is unknown.
   set value ""

#  Get the name of the monolith associated with this application.
   if { [info exists TASK($app,monolith)] } {

#  If it exists get the parameter value. An error results if none are
#  available so trap this as "".
      set task $MONOLITH($TASK($app,monolith),taskname)
      puts "Querying task $app about $param"
      $task get $app:$param  \
         -getresponse "global TASK; set TASK($app,output) %V" \
         -inform "" -endmsg ""

#  And wait for response.
      tkwait variable TASK($app,output)
      puts "Parameter value == $TASK($app,output)"

#  Set output value.
      if { [string range $TASK($app,output) 0 1 ] != "!!" } {
         set value "$TASK($app,output)"
      }
   } else {
      CCDIssueError "Unknown application \"$app\" (programming error)"
   }

#  Return the parameter value.
   return $value
}

# $Id$

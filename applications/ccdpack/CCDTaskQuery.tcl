proc CCDTaskQuery { app param } {

#+
#  Name:
#     CCDTaskQuery 

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Returns the value of an application parameter.

#  Description:
#     This routine gets the value of an application parameter. The
#     monolith associated with the application must be already started.
#     If no value exists a "" is returned.

#  Arguments:
#     app = string (read)
#        Name of the application to be queried.
#     param = string (read)
#        The name of the parameter.

#  Return value:
#     CCDTaskQuery = string
#       Returns the parameter value, otherwise a "" is returned.

#  Global variables:
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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-OCT-1995 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

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

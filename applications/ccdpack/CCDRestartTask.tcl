proc CCDRestartTask { task } {
#+
#  Name:
#     CCDRestartTask

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Stops then restarts an ADAM monolith.

#  Arguments:
#     task = name (read)
#        Name of application whose monolith is to be restarted.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

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

proc ccdRunTask { task args } {

#+
#  Name:
#     ccdRunTask

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Runs a non-terminal using CCDPACK task (non-Tk use).

#  Description:
#     This routine runs a task. It monitors the output and keeps
#     so that it may be used. This routine does not allow terminal I/O
#     use ccdRunTermTask if interaction with the user is required.

#  Arguments:
#     task = string (read)
#        The task to run.
#     args = string (read)
#        Optional arguments for the task to use.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     10-OCT-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
    global CCDdir
    global TASK

#.

#  Run the task. Simple method no output monitor.
    set TASK($task,error) ""
    if { [file executable $CCDdir/$task] } {
       if { "$args" == "" } {

#  Run the task and trap the output so that we can peek at it later.
          set TASK($task,output) [exec $CCDdir/$task]
       }  else {
          set TASK($task,output) [exec $CCDdir/$task $args]
       }

#  Check for errors
       foreach line [split "$TASK($task,output)" "\n"] {
          if { [string range $line 0 1 ] == "!!" } {
             puts stderr "   Task failed with return"
             puts stderr "$output"
             set TASK($task,error) "$line"
          }
       }
       
   } else {
       puts stderr "!! Cannot execute file $CCDdir/$task"
       exit 1
    }
 }
# $Id$

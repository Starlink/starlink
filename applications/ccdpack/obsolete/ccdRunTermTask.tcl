proc ccdRunTermTask { task args } {

#+
#  Name:
#     ccdRunTermTask

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Runs a CCDPACK task as if from a terminal (i.e. for non-Tk use).

#  Description:
#     This routine runs a task allowing it to interact with the user
#     at the normal terminal I/O level.

#  Arguments:
#     task = string (read)
#        The task to run.
#     args = string (read)
#        Optional arguments for the task to use.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
    global CCDdir

#.

#  Run the task. Simple method no output monitor.
    if { [file executable $CCDdir/$task] } {
       if { "$args" == "" } {
          exec $CCDdir/$task >&@stdout <@stdin
       }  else {
          eval exec $CCDdir/$task $args >&@stdout <@stdin
       }

#  Run the task and trap the output so that we can peek at it for errors.
#  Unfortunately this has to be done byte-by-byte (since prompts do not have
#  a \n) and is too slow.
#       if { "$args" == "" } {
#          set pipe [open "| $CCDdir/$task" r]
#       }  else {
#          set pipe [open "| $CCDdir/$task $args" r]
#       }
#       set output ""
#       while { ![eof $pipe] } {
#          set char [read $pipe 1]
#          append output $char
#          puts -nonewline $char
#       }

#  Now check output for error messages.
#       ????
#       close $pipe
    } else {
       puts stderr "!! Cannot execute file $CCDdir/$task"
       exit 1
    }
 }
# $Id$

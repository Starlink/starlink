proc CCDTaskError { task error } {

#+
#  Name:
#     CCDTaskError

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Makes report when application exits in error.

#  Arguments:
#     args = list (read)
#        Unused (from trace that calls this routine).

#  Global Variables:
#     TASK = array (write)
#        The array element ($task,error) is set to the error
#        message. This is for use in detecting an error condition from
#        outside this routine.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     15-MAR-1995 (PDRAPER):
#     	 Added header.
#     2-AUG-1995 (PDRAPER):
#        Converted for use with tcl ADAM.
#     {enter_further_changes_here}

#-

#  Global variables.
   global TASK

#.
   if { $error != "" } { 
      set TASK($task,error)  $error
      CCDIssueInfo "Application exited in error:$error"
   }
#  End of procedure.
}
# $Id$

   proc CCDRunSubTask { task args } {

#+
#  Name:
#     CCDRunSubTask

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Runs an ADAM task.

#  Description:
#     This routine runs an ADAM task as a detached subprocess with no 
#     monitoring.

#  Arguments:
#     task = string (read)
#        The ADAM task to be run.
#     args = list (read)
#        The arguments to be used with the task.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-MAR-1995 (PDRAPER):
#     	 Original version to get around blt_bgexec hanging on for
#        return of child processes, even when backgrounded.
#     {enter_further_changes_here}

#-
      global CCDtaskreturn

#.

#  Run the task.
      eval blt_bgexec -keepnewline CCDtaskreturn $task $args
   }
# $Id$

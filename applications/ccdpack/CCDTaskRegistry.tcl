proc CCDTaskRegistry { } {

#+
#  Name:
#     CCDTaskRegistry

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Sets up the monolith-application name associations used by CCDPACK.

#  Description:
#     This routine sets up the application commands that can be used
#     by this interface. All applications used should be registered
#     here before any ADAM tasks are started. After using this routine
#     applications can be run using the appropriate precedures (such as
#     CCDRunTask) by just using the application name.
#
#     The registry process assumes no knowledge about the monolith
#     that the application runs from, it locates the monolith by
#     tracking the softlink.

#  Return value:
#     CCDTaskRegistry = boolean
#        Returns 1 if ok, 0 otherwise (failed to locate CCDPACK monolith).

#  Global variables:
#     MONOLITH = array (write)
#        This variable describes the monoliths that the commands are
#        part of, the name of their executable, their status and the
#        command name associated with them. The elements have indices,
#
#           (name,location)        ! where the monolith executable is
#           (name,status)          ! One of enabled,available,unavailable
#           (name,taskname)        ! taskname should be unique, not set here
#
#        This also has an additional element (index) that should be
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
#        Only the (name,monolith) element is set by this routine.
#     CCDdir = string (read)
#        The CCDPACK installation directory.
#     KAPdir = string (read)
#        The KAPPA installation directory.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     11-OCT-1994 (PDRAPER):
#     	 Original version.
#     31-OCT-1995 (PDRAPER):
#        Recoded to check monoliths by back tracking the link from the
#        command name to the executable.
#     {enter_changes_here}

#-
   global TASK
   global MONOLITH
   global CCDdir
   global KAPdir

#  Local constants.
   set ccdpack_tasks "schedule ccdclear ccdsetup present import picinfo"
   set kappa_tasks "display fitslist lutable"

#.

#-----------------------------------------------------------------------------
#  Initialise the adamtask interface.
#  Happens via autoload at moment.
#-----------------------------------------------------------------------------
#    source $CCDdir/adamtask.tcl
#
#  Use this code when release is available and can use the necessary
#  CCDPACK extensions (dynamic loading?). Otherwise CCDPACK keeps its
#  own copies of the tcl ADAM code.
#   if { [info exists env(TCLADAM_DIR)] } {
#      source $env(TCLADAM_DIR)/tk/adam/adamtask.tcl
#   } else {
#      source /star/lib/tk/adam/adamtask.tcl
#   }

#-----------------------------------------------------------------------------
#  Name the available applications.
#-----------------------------------------------------------------------------

#  KAPPA. These are assumed to be in $KAPdir.
   foreach task "$kappa_tasks"  {
      if { [file type $KAPdir/$task] == "link" } {
         set monolith [file readlink $KAPdir/$task]

#  Make sure name is the full path.
         set monolith $KAPdir/[file tail $monolith]
         set TASK($task,monolith) $monolith
         set MONOLITH($monolith,location) $monolith
         if { ! [file executable $MONOLITH($monolith,location)] } {
            CCDIssueInfo \
"Problems with the KAPPA monolith $MONOLITH($monolith,location). This \
 may restrict the functions available."
            set MONOLITH($monolith,status) unavailable
         } else {
            set MONOLITH($monolith,status) available
         }
      } else {

#  Command must be on its own.
         set TASK($task,monolith) $KAPdir/$task
         set MONOLITH($task,location) $KAPdir/$task
         set MONOLITH($task,status) available
      }
   }

#  CCDPACK.
   foreach task "$ccdpack_tasks"  {
      if { [file type $CCDdir/$task] == "link" } {
         set monolith [file readlink $CCDdir/$task]
         set monolith $CCDdir/[file tail $monolith]
         set TASK($task,monolith) $monolith
         set MONOLITH($monolith,location) $monolith

#  CCDPACK monolith(s) is(are) essential.
         if { ! [file executable $MONOLITH($monolith,location)] } {
            CCDIssueInfo \
       "Problems with the CCDPACK monolith $MONOLITH($monolith,location). \
This interface will not function without this."
            set MONOLITH($monolith,status) unavailable
            return 0
         } else {
            set MONOLITH($monolith,status) available
         }
      } else {

#  Command must be on its own.
         set TASK($task,monolith) $CCDdir/$task
         set MONOLITH($task,location) $CCDdir/$task
         set MONOLITH($task,status) available
      }
   }

#  Initialise task name counter.
   set MONOLITH(index) [pid]

#  Can proceed.
   return 1

}

# $Id$

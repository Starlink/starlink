proc CCDTaskRegistry { } {

#+
#  Name:
#     CCDTaskRegistry

#  Purpose:
#     Sets up the monolith-application name associations used by CCDPACK.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

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

#  Global Variables:
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

#  Return Value:
#     CCDTaskRegistry = boolean
#        Returns 1 if ok, 0 otherwise (failed to locate CCDPACK monolith).

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2001 Central Laboratory of the Research
#     Councils. All Rights Reserved.

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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     11-OCT-1994 (PDRAPER):
#        Original version.
#     31-OCT-1995 (PDRAPER):
#        Recoded to check monoliths by back tracking the link from the
#        command name to the executable.
#     19-JUN-2001 (MBT):
#        Added some tasks: ccdedit, makeset, showset.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global TASK
   global MONOLITH
   global CCDdir
   global KAPdir

#  Local constants.
   set ccdpack_tasks "schedule ccdclear ccdsetup present import picinfo \
                      ccdedit makeset showset"
   set kappa_tasks "display fitslist lutable"

#.

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

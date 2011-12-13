proc CCDInitialize { } {

#+
#  Name:
#     CCDInitialize

#  Purpose:
#     Initializes certain global parameters for XREDUCE interface.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine sets the initial default values of any critical
#     global variables used by the XREDUCE interface. At the moment
#     this just means the values of the global program parameters
#     that might be missed in the interface initialization section.
#
#     It clears all the current global parameters so that these do not
#     pop-up later and upset things. It would use the current values
#     for these if they where available to GET as well as the
#     applications.

#  Copyright:
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
#     21-OCT-1995 (PDRAPER):
#        Original version.
#     13-NOV-1995 (PDRAPER):
#        Added initialization of CCDirflats.
#     3-JUL-2001 (MBT):
#        Added initialization of USESET.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
   global CCDglobalpars
   global env
   global CCDirflats
#.

#  Set some global parameters.
   set CCDglobalpars(LOGTO) BOTH
   set CCDglobalpars(LOGFILE) CCDPACK.LOG
   set CCDglobalpars(USESET) FALSE
   set CCDglobalpars(SATURATE) FALSE
   set CCDglobalpars(SETSAT) TRUE
   set CCDglobalpars(GENVAR) FALSE
   set CCDglobalpars(PRESERVE) TRUE
   set CCDirflats FALSE
   set CCDglobalpars(ZEROED) FALSE

#  Start up the CCDPACK monolith and query the CCDSETUP task about its
#  defaults (these will be the global ones if set).
#
#  This doesn't work.
#
#   CCDTaskStart ccdsetup
#   foreach param " ADC BOUNDS RNOISE MASK DIRECTION DEFERRED \
#                   EXTENT LOGTO LOGFILE PRESERVE GENVAR SATURATE \
#                   SATURATION SETSAT" {
#      set value [CCDTaskQuery ccdsetup $param]
#      if { $value != "" } {
#         set CCDglobalpars($param) $value
#      }
#   }
#
#  And clear any existing values.
   if { ! [info exists env(ADAM_USER)] } {
      set env(ADAM_USER) $env(HOME)/adam/
   }
   if { [file readable $env(ADAM_USER)/GLOBAL.sdf] } {

#  Run task cleanly as no interface exists at this point.
      CCDRunTask ccdclear "byname=false accept reset" 0
   }

#  End of procedure.
}
# $Id$

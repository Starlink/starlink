proc CCDInitialize { } {

#+
#  Name:
#     CCDInitialize

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Initializes certain global parameters for XREDUCE interface.

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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-OCT-1995 (PDRAPER):
#        Original version.
#     13-NOV-1995 (PDRAPER):
#        Added initialization of CCDirflats.
#     {enter_further_changes_here}

#-

#  Global parameters:
   global CCDglobalpars
   global env
   global CCDirflats
#.

#  Set some global parameters.
   set CCDglobalpars(LOGTO) BOTH
   set CCDglobalpars(LOGFILE) CCDPACK.LOG
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

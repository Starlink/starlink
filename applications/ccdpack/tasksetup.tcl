#
#  This script is not currently used by any of the CCDPACK code, but it
#  should work if it is necessary to run monoliths under the control of
#  tcl scripts.  See also taskrun.tcl
# 

   proc tasksetup {} {
#+
#  Name:
#     tasksetup

#  Purpose:
#     Do necessary initialisation for Tcl-based A-tasks.

#  Author:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     11-OCT-2000 (MBT):
#        Initial version.
#-

#  Global variables.
      global env
      global CCDdir
      global KAPdir

#  Set the global variable which controls where to pickup source etc.
      if { [ info exists env(CCDPACK_DIR) ] } {
         set CCDdir $env(CCDPACK_DIR)
      } else {
         set CCDdir /star/bin/ccdpack
      }

#  Locate KAPPA
      if { [info exists env(KAPPA_DIR)] } {
         set KAPdir $env(KAPPA_DIR)
      } else {
         set KAPdir /star/bin/kappa
      }

#  Get ready to use ADAM tasks.
      CCDTaskRegistry
   }
# $Id$

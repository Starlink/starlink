   proc tasksetup {
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

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
      global Tickerbitmap

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

#  Set up bitmaps for the ticking clock animation.
      foreach i { 1 2 3 4 5 6 7 8 } {
         set Tickerbitmap($i) "$CCDdir/c$i.xbm"
      }

#  Get ready to use ADAM tasks.
      CCDTaskRegistry
   }
# $Id$

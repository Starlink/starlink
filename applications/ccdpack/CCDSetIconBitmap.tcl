   proc CCDSetIconBitmap { Topwin } {
#+
#  Name:
#     CCDSetIconBitmap

#  Type of Module:
#     Tcl/Tk

#  Purpose:
#     Sets the icon bitmap of a top-level widget to the standard CCDPACK
#     icon bitmap.

#  Parameters:
#     topwin = window (read)
#        The name of the top-level object.

#  Global parameters:
#     CCDdir = string (read)
#        The name of the directory in which CCDPACK procedure etc.
#	 reside.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     2-MAR-1994 (PDRAPER):
#     	 Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     {enter_changes_here}

#-

#  Global parameters:
      global CCDdir
#.

#  Set the icon bitmap.
      set topwin [CCDPathOf $Topwin]
      wm iconbitmap $topwin @$CCDdir/ccdbitmap
      }
# $Id$

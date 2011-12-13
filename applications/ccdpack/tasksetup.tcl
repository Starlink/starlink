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

#  Language:
#     TCL

#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils.
#     All Rights Reserved.

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

#  Author:
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     11-OCT-2000 (MBT):
#        Initial version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

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

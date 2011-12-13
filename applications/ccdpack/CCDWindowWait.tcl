   proc CCDWindowWait { W } {
#+
#  Name:
#     CCDWindowWait

#  Purpose:
#     Waits for procedure dialog window to terminate.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure waits for the named window to be destroyed.
#     Meanwhile it sets the grab (for all events) to this window so
#     that no other interaction may proceed. All other widgets in the
#     application are disabled and show the busy cursor sign while
#     waiting for this window to proceed.

#  Arguments:
#     W = window (read)
#        The name of the window to wait on.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

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
#     3-MAY-1994 (PDRAPER):
#        Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Changed focus methods.
#     30-JUN-1995 (PDRAPER):
#        Now relies on focus setting (default) of Tk, rather than
#        defining own.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed for itcl3.3.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
#.

#  Get window name from command name passed.
      set w [CCDPathOf $W]

#  Now set the grab. Use a catch as modal operation can fail.
      set old_grab [ grab current ]
      catch { grab set $w }

#  Make all other windows use the "busy" cursor if using mega-widgets.
#  These are released automatically when $w is destroyed.
      if { [catch { $W isa Ccd::toplevel }] == 0 } {
         $W busy hold 0
      }

#  Wait for this procedure to exit (by destroying the main window).
      tkwait window $w

#  Now restore old behaviour. This occasionally fails when the routine
#  is misused (and windows lower in the heirarchy are destroyed), so
#  catch any errors and ignore.
      catch {
               grab release $w
               grab set $old_grab
            }

#  End of procedure.
   }
# $Id$

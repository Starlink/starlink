   proc CCDCentreWindow { Window {Master ""} } {
#+
#  Name:
#     CCDCentreWindow

#  Purpose:
#     Centres a window on the screen or another window.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine takes a window and moves it so that it is centred
#     either over a given 'master' window (though it does not need to
#     be its master in other senses) or on the whole screen.  It also
#     tries to register it as a transient window working on behalf of
#     its master, which the window manager will hopefully take as a
#     hint to keep it on top.  The routine calls update idletasks since
#     it uses winfo reqwidth, which may not be correct yet otherwise.

#  Arguments:
#     Window = string
#        Command name or path name of a window to centre.
#     Master = string
#        If supplied, this gives the command name or path name of a
#        window over which Window is to be centred.  If absent
#        (or the empty string) then Window will be positioned in the
#        centre of the screen.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
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

#  Authors:
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     22-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get the desired location of the window.
      set window [CCDPathOf $Window]

#  Ensure that the reqwidth and reqheight values will be correct.
      update idletasks

#  Centre over a master window.
      if { $Master != "" } {
         set master [CCDPathOf $Master]
         set x [expr [winfo rootx $master] \
                   + [winfo reqwidth $master] / 2 \
                   - [winfo reqwidth $window] / 2]
         set y [expr [winfo rooty $master] \
                   + [winfo reqheight $master] / 2 \
                   - [winfo reqheight $window] / 2]

#  Register it as a slave of the master.
         wm transient $window $master

#  Centre on the whole screen.
      } else {
         set x [expr [winfo screenwidth $window] / 2 \
                   - [winfo reqwidth $window] / 2]
         set y [expr [winfo screenheight $window] / 2 \
                   - [winfo reqheight $window] / 2]

#  Register it as a slave of the main top level window, if we know what
#  that is.
         set master .topwin
         if { [winfo exists $master] } {
            wm transient $window $master
         }
      }

#  Position the window accordingly.
      wm geometry $window +$x+$y
   }
# $Id$

   proc CCDPathOf { cmd } {
#+
#  Name:
#     CCDPathOf

#  Purpose:
#     Finds the pathname of the window for an object.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk commands

#  Description:
#     Given a command name which may refer either to a genuine Tk widget
#     or a Ccd::* widget-like thing, this command returns the pathname
#     of the corresponding window.  This may or may not be the same as
#     the command name.  See also CCDTkWidget and CCDCcdWidget.

#  Arguments:
#     cmd = string
#        The command name of a Tk widget or Ccd::* widget-like thing.

#  Return Value:
#     The pathname of the window corresponding to the command.

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

#  Authors:
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     1-APR-2000 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
      if { [catch { set path [$cmd pathname] }] } {
         return $cmd
      } else {
         return $path
      }
   }
# $Id$

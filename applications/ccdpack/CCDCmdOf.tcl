   proc CCDCmdOf { window } {
#+
#  Name:
#     CCDCmdOf

#  Purpose:
#     Finds the command name corresponding to a window.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk commands

#  Description:
#     Given a window pathname which may refer either to a genuine Tk widget
#     or to a Ccd::* widget-like thing, this command returns the command
#     name which can be used for manipulating it.  This may or may not
#     be the same as the pathname.  See also CCDTkWidget and CCDCcdWidget.
#
#     The current implementation of this relies on knowledge of how the
#     window names are constructed from command names by CCDTkWidget and
#     CCDCcdWidget.  That information ought ideally to be entirely
#     private to the Ccd::base class, but I don't see how to get round it.
#     The whole approach is messy, and forced by having to upgrade a
#     lot of code from Itcl2/Tcl7 to Itcl3/Tcl8.

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
      regsub ^\.- $window . cmd
      return $cmd
   }
# $Id$

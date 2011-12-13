   proc CCDCcdWidget { cmdvar pathvar objtype objname args } {
#+
#  Name:
#     CCDCcdWidget

#  Purpose:
#     Creates a new Ccd::* widget-type thing.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk commands

#  Description:
#     This function creates a CCDPACK superwidget and sets variables
#     giving the pathname of the window and the command name of the widget.
#     These variables will not have the same value.  The purpose of this
#     is so that genuine Tk widgets and Ccd::* widget-type-things
#     can be manipulated using the same form of command, without having
#     to remember which is being manipulated.  This whole approach is
#     messy, and forced by the necessity of converting a lot of Itcl2/Tcl7
#     code to run under Itcl3/Tcl8.  See also CCDTkWidget.

#  Arguments:
#     cmdvar = string
#        Name of the variable in the calling context which is to be filled
#        with the command name of the superwidget.
#     pathvar = string
#        Name of the variable in the calling context which is to be filled
#        with the pathname of the window.
#     objtype = string
#        The Ccd::* superwidget type which is to be created.
#     objname = string
#        The pathname of the superwidget to be created.
#     args = strings
#        Additional arguments to be appended to the superwidget creation
#        command.

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
      upvar $cmdvar cmd
      upvar $pathvar path
      regsub ^.-? $objname . cmd
      regsub ^.-? $objname .- path
      uplevel $objtype $cmd $args
   }

# $Id$

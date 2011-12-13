#+
#  Name:
#     CCDBindings

#  Purpose:
#     Defines bindings for widget events.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk script.

#  Invocation:
#     source CCDBindings.tcl

#  Description:
#     This file contains widget event bindings and associated
#     procedures CCDPACK Tcl/Tk applications. These are all bindings to
#     widget classes that effect all widgets of the class. Bindings for
#     specific widget instances should be embedded in the procedural
#     code. This file (together with the Tk default bindings) defines
#     the "feel" of applications.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
#     PDRAPER: Peter Draper ({affiliation})
#     {enter_new_authors_here}

#  History:
#     14-MAR-1994 (PDRAPER):
#        Original version based on Xadam bindings by Dave Terrett
#        (STARLINK) and Tk bindings by John Ousterhout.
#     22-MAR-1995 (PDRAPER):
#        Added bindings for global context help.
#     5-MAY-1995 (PDRAPER):
#        Major change to Tk4. All bindings reviewed in light of Tk4's
#        enhanced Motif compatibility. No repeat bindings now allowed
#        in this file, hence considerably simplified.
#     {enter_changes_here}
#
#     {bugs}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Entry widgets:
#  Make delete work to the left rather than the right.
      bind Entry <Delete> {
	 if [%W selection present] {
            %W delete sel.first sel.last
	 } else {
            %W delete [expr [%W index insert] -1]
	 }
      }
      bind Entry <Return> {
	 focus [tk_focusNext %W]
      }

#  Buttons:
      bind Button <Return> { tkButtonInvoke %W }

#  Radiobuttons:
      bind Radiobutton <Return> { tkCheckRadioInvoke %W }

#  Checkbuttons:
      bind Checkbutton <Return> { tkCheckRadioInvoke %W }

#  Menubuttons:


#  Listboxes:
#  Need focus for keyboard commands to work.
      bind Listbox <1> {+ focus %W }
      bind Listbox <3> { %W selection clear 0 end }

#  Scales:
#  Need focus for keyboard commands to work.
      bind Scale <ButtonPress-1> {+ focus %W }

# $Id$

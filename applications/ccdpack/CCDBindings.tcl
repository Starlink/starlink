#+
#  Name:
#     CCDBindings

#  Purpose:
#     Defines bindings for widget events.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     This file contains widget event bindings and associated
#     procedures CCDPACK Tcl/Tk applications. These are all bindings to
#     widget classes that effect all widgets of the class. Bindings for
#     specific widget instances should be embedded in the procedural
#     code. This file (together with the Tk default bindings) defines
#     the "feel" of applications.

#  Invocation:
#     source CCDBindings.tcl

#  Authors:
#     PDRAPER: Peter Draper ({affiliation})
#     {enter_new_authors_here}

#  History:
#     14-MAR-1994 (PDRAPER):
#     	 Original version based on Xadam bindings by Dave Terrett
#        (STARLINK) and Tk bindings by John Ousterhout.
#     22-MAR-1995 (PDRAPER):
#        Added bindings for global context help.
#     5-MAY-1995 (PDRAPER):
#        Major change to Tk4. All bindings reviewed in light of Tk4's
#        enhanced Motif compatibility. No repeat bindings now allowed
#        in this file, hence considerably simplified.
#     {enter_changes_here}

#     {bugs}

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

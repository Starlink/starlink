   proc CCDContextHelp { X Y } {
#+
#  Name:
#     CCDContextHelp

#  Purpose:
#     Provides access to Meta-widgets context sensitive help.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine should be used to trap context sensitive help
#     bindings (such as F1 and Help keypresses). It uses the position
#     information associated with the binding to findout the name of
#     the widget that the mouse pointer is over. It then traverses the
#     tree of widgets until it encounters a widget with class Ccd::
#     (this is the prefix given to all Meta widget classes). It then
#     queries this Meta-widget for the name of the document and label
#     to use as help. It passes these to the appropriate procedures
#     for expansion into a full file name and requests that the help
#     is shown in a browser.

#  Arguments:
#     X = number (read)
#        X position of the mouse pointer when help event occurred
#        (the binding %X value).
#     Y = number (read)
#        Y position of the mouse pointer when help event occurred
#        (the binding %Y value).

#  Copyright:
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
#     21-MAR-1995 (PDRAPER):
#        Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.

#.

#  Get the name of the window that the event occurred over.
      set seehelp 1
      set eventwindow [winfo containing $X $Y]
      set Eventwindow [CCDCmdOf $eventwindow]
      if { "$eventwindow" != "" } {

#  Traverse widget tree until we meet a Meta-widget.
	 set window $eventwindow
         set Window $Eventwindow
         set ok 1
         while { $ok } {
            if { [catch { $Window isa Ccd::base }] == 0 } {
               #  Meta-widget.
               break
            }
            set window [winfo parent $window]
            set Window [CCDCmdOf $window]

            #  End of tree.
            if { "$window" == "" } {
               set seehelp 0
               break
            }
	 }
	 if { $seehelp } {

#  Now query Meta-widget about any help it might have.
            set doclabel [$Window showhelp $Eventwindow]
	    if { "$doclabel" == "" } {
               set seehelp 0
            } else {

#  Expand docname and label into full name.
               set fullname [CCDLocateHelpFile \
				[lindex $doclabel 0 ] \
				[lindex $doclabel 1 ] ]
               if { "$fullname" == "" } {
                  set seehelp 0
               } else {

#  Ask for the help to be displayed.
		  CCDShowHelp $fullname
               }
            }
	 }
      } else {
         set seehelp 0
      }

#  If no help was found say so.
      if { ! $seehelp } {
	 CCDIssueInfo "Sorry no help is available for that region"
      }

#  End of procedure.
   }
# $Id$

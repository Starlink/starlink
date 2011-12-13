   proc CCDDialog {Top title text bitmap} {
#+
#  Name:
#     CCDDialog

#  Purpose:
#     Creates a dialog box.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     Creates a modal dialog box for issuing warnings. The resultant
#     top level widget contains descriptive text an optional bitmap
#     and a button for acknowledgment.

#  Arguments:
#     Top = window (read)
#        The name of the top-level widget created by this routine.
#     title = string (read)
#        A title for the window decorations.
#     text = string (read)
#        The description of the error or warning.
#     bitmap = string (read)
#        The name of a bitmap to display on the left. If blank this is
#        not used.

#  Copyright:
#     Copyright (C) 1995, 1999-2001 Central Laboratory of the Research
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
#     21-JUL-1995 (PDRAPER):
#        Original version.
#     23-AUG-1995 (PDRAPER):
#        Added code to centre in parent top-level or screen.
#     13-MAY-1999 (PDRAPER):
#        Changed window control policy to just transient (explicit
#        raising causes problems with some WMs).
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Extracted window centring to external routine CCDCentreWindow.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.

#.

#  Widget creation.
      CCDCcdWidget Topwin topwin Ccd::toplevel $Top -title "$title"
      CCDTkWidget Frame frame frame $topwin.f
      if { $bitmap != "" } {
         CCDTkWidget Bitmapwin bitmapwin label $frame.label -bitmap $bitmap
      }
      CCDTkWidget Message message \
         message $frame.message -text "$text" -width 14c -justify center
      CCDCcdWidget Button button Ccd::choice $Topwin.button -standard 0

#  Configure widgets.
      $Button addbutton OK "$Topwin kill $Topwin"

#  Pack widgets.
      pack $button -side bottom -fill x
      pack $frame -side top -fill both -expand true
      if { $bitmap != "" } {
         pack $bitmapwin -side left -fill y -padx 0.5c -pady 0.5c
      }
      pack $message -side right -fill both -expand true

#  Make sure this window is on top and reasonable prominent
#  (centre of screen of parent top-level).
      wm withdraw $topwin
      CCDCentreWindow $Topwin [winfo parent $topwin]
      wm deiconify $topwin

#  Make OK the focus
      $Button focus OK

#  Wait for the acknowledgement.
      CCDWindowWait $Topwin

#  End of procedure.
   }
# $Id$

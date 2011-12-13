proc CCDExit {} {

#+
#  Name:
#     CCDExit

#  Purpose:
#     Exits the CCDPACK GUI.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine offers to close down the GUI and performs a clean
#     exit if requested.

#  Global Parameters:
#     MAIN = array (read)
#       MAIN(window) = name of the main top-level window.
#       MAIN(name) = name of application.

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
#     7-NOV-1995 (PDRAPER):
#        Original version.
#     13-MAY-1999 (PDRAPER):
#        Changed window control policy to just transient (explicit
#        raising causes problems with some WMs).
#     11-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Extracted window centring to external routine CCDCentreWindow.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
   global MAIN
#.

   set Main $MAIN(window)
   set main [CCDPathOf $Main]
   if { [winfo exists $main] } {

#  Widget creation.
      CCDCcdWidget Top top Ccd::toplevel .exit -title "Exit from $MAIN(name)"
      CCDTkWidget Line1 line1 frame $top.f1 -height 3
      CCDTkWidget Message message \
         label $top.message \
                      -text "Are you sure you want to exit from $MAIN(name)" \
                      -borderwidth 3 -padx 10 -pady 10
      CCDTkWidget Bitmap bitmap label $top.bitmap -bitmap questhead
      CCDTkWidget Line2 line2 frame $top.f2 -height 3
      CCDCcdWidget Choice choice Ccd::choice $Top.button -standard 0

#  Configure widgets.
      $Choice addbutton Yes "$Top kill $Top;$Main kill $Main; destroy ."
      $Choice addbutton No  "$Top kill $Top"

#  Pack widgets.
      pack $choice -side bottom -fill x
      pack $line1 -side top -fill x
      pack $message -side left -fill both -expand true
      pack $bitmap -side right
      pack $line2 -side top -fill x

#  Make sure this window is on top and reasonable prominent
#  (centre of screen of parent top-level).
      wm withdraw $top
      CCDCentreWindow $Top
      wm deiconify $top

#  Try to make sure this window stays on Top.
      wm transient $top [CCDPathOf .topwin]

#  Yes is default.
      $Choice focus Yes

#  Wait for the acknowledgement.
      CCDWindowWait $Top
   }
#  End of procedure.
}
# $Id$

   proc CCDViewFile { Topwin file } {
#+
#  Name:
#     CCDViewFile

#  Purpose:
#     Display the contents of a file.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure displays the contents of a file in a text widget.
#     The text widget appears in its own window which can be removed
#     when finished.

#  Arguments:
#     Topwin = window (read)
#        Name of a top-level window to parent this window. A uniquish
#        name is generated for the actual form so that many instances
#        of this may be created.
#     file = filename (read)
#        Name of the file whose contents are to be viewed.

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
#     14-SEP-1995 (PDRAPER):
#        Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global Viewcount
#.

#  Initialise Viewcount if not already set.
      if { ! [info exists Viewcount] } {
         set Viewcount 0
      }

#  Check file exists.
      if { ! [file readable $file] } {
         CCDIssueInfo "Cannot read file $file"
         return
      }

#-----------------------------------------------------------------------------
#  Widget creation
#-----------------------------------------------------------------------------

#  Toplevel
      set Thiswin $Topwin.view$Viewcount
      incr Viewcount
      CCDCcdWidget Thisw thisw \
         Ccd::toplevel $Thiswin -title "Contents of file $file"

#  Text widget
      CCDCcdWidget Text text Ccd::scrolltext $Thisw.text

#  Choice bar to remove window.
      CCDCcdWidget Choice choice Ccd::choice $Thisw.choice -standard 0

#-----------------------------------------------------------------------------
#  Widget configuration
#-----------------------------------------------------------------------------
      $Choice addbutton {Remove from screen} "$Thisw kill $Thisw"

#-----------------------------------------------------------------------------
#  Packing
#-----------------------------------------------------------------------------
      pack $text -side top -fill both -expand true
      pack $choice -fill x

#-----------------------------------------------------------------------------
#  Interface activation.
#-----------------------------------------------------------------------------

#  Read the file and insert its contents
      set fileid [open $file r]
      while { ! [eof $fileid] } {
         $Text insert end "[gets $fileid]\n"
      }

#  End of procedure.
   }
# $Id$

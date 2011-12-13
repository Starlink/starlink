   proc CCDUpdateLabelCount { Reveal name Box } {
#+
#  Name:
#     CCDUpdateLabelCount

#  Purpose:
#     Updates a named button of a Ccd::reveal widget to reflect the count
#     of entries in a listbox-like widget.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine is for updating the text description of one of a set
#     of buttons created by the Ccd::reveal widget. The text is updated to
#     show a count of the number of entries in the associated window
#     (which should be a listbox-like widget, such as a scrollbox or
#     table). This is used by the CCDNDFDoImport procedure.

#  Arguments:
#     Reveal = window (read)
#       The name of the Ccd::reveal widget whose button labels are to be
#       changed to reflect the contents of an associated listbox-like
#       widget.
#     name = string (read)
#       The name of the button (i.e. its creation name). The new name
#       will be this name followed by a count of the contents in brackets.
#     Box = window (read)
#       Name of the scrollbox-like widget associated with name.

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
#     29-AUG-1995 (PDRAPER):
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

#.

#  Get the number of entries in the listbox-like widget.
      if { [winfo exists [CCDPathOf $Reveal]] } {
         if { [winfo exists [CCDPathOf $Box]] } {
            set size [$Box size]
            $Reveal resettext $name "$name ($size)"
         }
      }

#  End of procedure.
   }
# $Id$

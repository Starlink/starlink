   proc CCDGetSetIndices { Topwin {force 0} } {
#+
#  Name:
#     CCDGetSetIndices

#  Purpose:
#     Get the list of Set Index values we will deal with from the user.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine asks the user what size CCDPACK Sets should be made,
#     so that the auto-Setting routine can make Sets of the right size.

#  Arguments:
#     Topwin = window (read)
#        The command name of a top-level widget to contain this form.
#     force = boolean (read)
#        If force is supplied and true, then this will definitely result
#        in a widget being posted.  Otherwise the widget will only
#        be posted if the user has not seen it before (i.e. if there
#        is not currently a valid CCDsetindices value).

#  Notes:
#     This widget doesn't currently allow the user to specify his own
#     Set Index values (it assumes 1, 2, 3,...).  I don't think this
#     will often be required, but if it is this routine could be
#     ehnanced.

#  Global Variables:
#     CCDsetindices = list of integers (read and write)
#        The NDF Set Index values got from the user.  If the user
#        gives an invalid response or invokes a cancel on the dialog,
#        an empty list will be returned.
#     CCDsetindicesvalid = boolean (write)
#        True if the user wishes to use the value of CCDsetindices.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     PDRAPER: Peter W. Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-JUN-2001 (MBT):
#        Original version.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global CCDsetindices
      global CCDsetindicesvalid
      global setsize

#  Initialise set indices variable list if it does not already exist.
      if { ! [info exists CCDsetindices] || \
           ! [info exists CCDsetindicesvalid] } {
         set CCDsetindices {}
         set CCDsetindicesvalid 0
      }
      set setsize [llength $CCDsetindices]

#  Only bother posting the widget if we need to.
      if { $force || ! $CCDsetindicesvalid } {

#  Create the widget components.
         CCDCcdWidget Top top Ccd::toplevel $Topwin -title "Size of each Set"
         CCDTkWidget Frame frame frame $top.center
         CCDTkWidget Value value scale $frame.slider \
            -variable setsize \
            -orient horizontal \
            -from 1 -to 9 \
            -label "Number of NDFs per Set" \
            -showvalue 1 \
            -tickinterval 1 \
            -length 256
         CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar
         CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 1

#  Configure the choice buttons.
         $Choice addcommand OK "
            if { \[llength \$CCDsetindices\] != \$setsize } {
               set CCDsetindices {}
               for { set i 1 } { \$i <= \$setsize } { incr i } {
                  lappend CCDsetindices \$i
               }
            }
            set CCDsetindicesvalid 1
            $Top kill $Top
         "
         $Choice addcommand Cancel "
            set CCDsetindicesvalid 0
            $Top kill $Top
         "

#  Set window acceptance as the default button if the current value of
#  CCDsetindices is sensible.
         if { [llength $CCDsetindices] > 1 } {
            $Choice focus OK
         }

#  Add menu options.
         $Menu addcommand File {Accept Window} "$Choice invoke OK"
         $Menu addcommand File {Close Window} "$Choice invoke Cancel"
         $Menu addcommand File {Exit} CCDExit
         $Menu addcommand Options {No Sets} "
            set CCDglobalpars(USESET) \"FALSE\"
            $Choice invoke Cancel
         "

#  Pack the widget.
         pack $menu -fill x -side top
         pack $frame -fill both -expand 1
         pack $value -fill x -expand 1
         pack $choice -side bottom -fill x

#  Add contextual help.
         $Top sethelp ccdpack CCDGetSetIndicesWindow
         $Menu sethelpitem {On Window} ccdpack CCDGetSetIndicesWindow
         $Menu sethelp all ccdpack CCDGetSetIndicesMenu

#  Position the window.
         wm withdraw $top
         CCDCentreWindow $Top [winfo parent $top]
         wm deiconify $top

#  Wait for interaction to finish.
         CCDWindowWait $Top
      }
   }
# $Id$

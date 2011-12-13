   proc CCDReduceExtras { Topwin } {
#+
#  Name:
#     CCDReduceExtras

#  Purpose:
#     Allows "extra" parameters to be set.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This procedure allows "extra" parameters to do with controlling a
#     reduction to be changed. These parameters are less relevant
#     parameters such as the extensions to give to NDF names the name of
#     the script to use and its type.

#  Arguments:
#     Topwin = window (read)
#        The name of the top-level window for this form.

#  Notes:
#     Choice of type of script to run removed as ICL will not run
#     run a non-tty.

#  Global Parameters:
#     CCDglobalpars = array (read and write)
#        The values of the parameters to be set.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
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
#     18-MAY-1994 (PDRAPER):
#        Original version.
#     27-MAR-1995 (PDRAPER):
#        Added logfile.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     23-AUG-1995 (PDRAPER):
#        Converted to coding style.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global CCDglobalpars
#.

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------

#  Create a top-level window.
      CCDCcdWidget Top top \
         Ccd::toplevel $Topwin -title "Reduce additional parameters"

#  Add a menubar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar


#  Intermediary frame extensions. Extension to debiassed NDF names.
      CCDCcdWidget Debext debext \
         Ccd::labent $Top.debext \
                     -text "Extension of debiassed frames:" \
                     -textvariable CCDglobalpars(DEBIASEXT)

#  Extension to dark corrected NDF names.
      CCDCcdWidget Darkext darkext \
         Ccd::labent $Top.darkext \
                      -text "Extension of dark corrected frames:" \
                      -textvariable CCDglobalpars(DARKEXT)

#  Extension to flash corrected NDF names.
      CCDCcdWidget Flashext flashext \
         Ccd::labent $Top.flashext \
                       -text "Extension of flash corrected frames:" \
                       -textvariable CCDglobalpars(FLASHEXT)

#  Extension to flatfielded NDF names.
      CCDCcdWidget Flatext flatext \
         Ccd::labent $Top.flatext \
                      -text "Extension of flatfielded frames:" \
                      -textvariable CCDglobalpars(FLATEXT)

#  Names of the master NDFs: Bias.
      CCDCcdWidget Masterbias masterbias \
         Ccd::labent $Top.masterbias \
                         -text "Name of master bias:" \
                         -textvariable CCDglobalpars(MASTERBIAS)

#  Dark
      CCDCcdWidget Masterdark masterdark \
         Ccd::labent $Top.masterdark \
                         -text "Name of master dark:" \
                         -textvariable CCDglobalpars(MASTERDARK)

#  Flash.
      CCDCcdWidget Masterflash masterflash \
         Ccd::labent $Top.masterflash \
                          -text "Name of master flash:" \
                          -textvariable CCDglobalpars(MASTERFLASH)

#  Flat prefix.
      CCDCcdWidget Masterflat masterflat \
         Ccd::labent $Top.masterflat \
                         -text "Prefix name for master flatfields:" \
                         -textvariable CCDglobalpars(MASTERFLAT)

#  Get the name of the script.
      CCDCcdWidget Scriptname scriptname \
         Ccd::labent $Top.scriptname \
                         -text "Name of script:" \
                         -textvariable CCDglobalpars(SCRIPTNAME)

#  Get the type of script.
      CCDCcdWidget Scripttype scripttype \
         Ccd::radioarray $Top.scripttype \
                         -label "Script type:" \
                         -variable CCDglobalpars(SCRIPTTYPE)

#  Get the name of the logfile for the reduce job.
      CCDCcdWidget Exelogfile exelogfile \
         Ccd::labent $Top.exelogfile \
                         -text "Name of log file for background job:" \
                         -textvariable CCDglobalpars(EXELOGFILE)

#  Add choice bar for getting out.
      CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 0

#----------------------------------------------------------------------------
#  Widget configuration.
#----------------------------------------------------------------------------

#  Menu.
#  File items to accept window and exit interface.
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add an option to view the names of the NDFs to be processed.
      $Menu addcommand Options {View frames...} \
         "global CCDallndfs
          CCDViewLists $Top.view {All available frames} CCDallndfs
         "

#  Scripttype.
#  Add known options for script type.
#      $Scripttype addbutton {C-shell} {CSH}
#      $Scripttype addbutton {ICL} {ICL}

#  Choice.
#  Ok button and just proceeds.
      $Choice addbutton {OK} "$Top kill $Top"

#----------------------------------------------------------------------------
#  Associate help.
#----------------------------------------------------------------------------
      $Top sethelp ccdpack CCDReduceExtrasWindow
      $Menu sethelpitem {On Window} ccdpack CCDReduceExtrasWindow
      $Menu sethelp all ccdpack CCDReduceExtrasMenu

#----------------------------------------------------------------------------
#  Pack all widgets.
#----------------------------------------------------------------------------
      pack $menu -fill x
      pack $choice -side bottom -fill x
      pack $debext $darkext $flashext $flatext -fill x
      pack $masterbias $masterdark $masterflash $masterflat -fill x
#      pack $scriptname $scripttype $exelogfile -fill x
      pack $scriptname $exelogfile -fill x

#  End of procedure.
   }
# $Id$

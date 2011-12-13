proc CCDReduce { Topwin } {

#+
#  Name:
#     CCDReduce

#  Purpose:
#     Controls a CCDPACK reduction.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window for this form.

#  Global Parameters:
#     CCDallndfs = list (read)
#        The names of the NDFs be be processed.
#     CCDglobalpars = array (read and write)
#        CCDPACK global parameters.

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
#     17-MAY-1994 (PDRAPER):
#        Original version.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     23-AUG-1995 (PDRAPER):
#        Re-coded to new style.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
   global env
   global CCDallndfs
   global CCDglobalpars
   global CCDhaveframe
   global BIAS
#.

#----------------------------------------------------------------------------
#  Initialise the global parameters that are used.
#----------------------------------------------------------------------------
   if { ! [info exists CCDglobalpars(DEBIASEXT)] } {
      set CCDglobalpars(DEBIASEXT) _db
   }
   if { ! [info exists CCDglobalpars(DARKEXT)] } {
      set CCDglobalpars(DARKEXT) _dk
   }
   if { ! [info exists CCDglobalpars(FLASHEXT)] } {
      set CCDglobalpars(FLASHEXT) _fls
   }
   if { ! [info exists CCDglobalpars(FLATEXT)] } {
      set CCDglobalpars(FLATEXT) _fl
   }
   if { ! [info exists CCDglobalpars(MASTERBIAS)] } {
      set CCDglobalpars(MASTERBIAS) "MASTER_BIAS"
   }
   if { ! [info exists CCDglobalpars(MASTERDARK)] } {
      set CCDglobalpars(MASTERDARK) "MASTER_DARK"
   }
   if { ! [info exists CCDglobalpars(MASTERFLASH)] } {
      set CCDglobalpars(MASTERFLASH) "MASTER_FLASH"
   }
   if { ! [info exists CCDglobalpars(MASTERFLAT)] } {
      set CCDglobalpars(MASTERFLAT) "MASTER_FLAT"
   }
   if { ! [info exists CCDglobalpars(SCRIPTNAME)] } {
      set CCDglobalpars(SCRIPTNAME) "xreduce.csh"
   }
   if { ! [info exists CCDglobalpars(SCRIPTTYPE)] } {
      set CCDglobalpars(SCRIPTTYPE) "CSH"
   }
   if { ! [info exists CCDglobalpars(EXELOGFILE)] } {
      set CCDglobalpars(EXELOGFILE) "xreduce.log"
   }
   if { ! [info exists CCDglobalpars(SPACESAVE)] } {
      set CCDglobalpars(SPACESAVE) NONE
   }

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------

#  Top-level widget.
   CCDCcdWidget Top top Ccd::toplevel $Topwin -title "Perform Reduction"

#  Menubar.
   CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Radioarray for getting the debiassing type.
   CCDCcdWidget Debiastype debiastype \
      Ccd::radioarray $Top.debiastype \
                      -label "Debias using:" \
                      -variable CCDglobalpars(DEBIASTYPE)

#  Radioarray for the interpolation method. Only used if the
#  debiassing type is "interpolation".
   CCDCcdWidget Interp interp \
      Ccd::radioarray $Top.interp \
                  -label "Interpolation method:" \
                  -variable CCDglobalpars(INTERPTYPE)

#  Radioarray for deciding the type of disk space savings we want.
   CCDCcdWidget Spacesave spacesave \
      Ccd::radioarray $Top.spacesave \
                     -label "Save how much disk space:" \
                     -variable CCDglobalpars(SPACESAVE)

#  Choice bar for controlling interface.
   CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 0

#----------------------------------------------------------------------------
#  Widget configuration.
#----------------------------------------------------------------------------

#  Menu.

#  File items to cancel or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add an option to view the names of the NDFs to be processed.
   $Menu addcommand Options {View frames...} \
      "CCDViewLists $Top.view {All available frames} CCDallndfs"

#  Add an option to set additional parameters. Make sure defaults for
#  these exist (in case this window isn't invoked).
   $Menu addcommand Options {Set additional options...} \
      "CCDReduceExtras $Top.extras"

#  Debiastype.
#  Add options for the different types of debiassing we can use.
   set dbtype 0
   if { $BIAS(debias,1) } {
      $Debiastype addbutton {zeroed master bias, offsetting to bias strips} \
         {1} "$Interp state all disabled"
      set dbtype 1
   }
   if { $BIAS(debias,2) } {
      $Debiastype addbutton {unzeroed master bias} {2} \
         "$Interp state all disabled"
      if { $dbtype == 0 } { set dbtype 2 }
   }
   if { $BIAS(debias,3) } {
      $Debiastype addbutton {interpolate using bias strips} {3} \
         "$Interp state all normal"
      if { $dbtype == 0 } { set dbtype 3 }
   }
   if { $BIAS(debias,4) } {
      $Debiastype addbutton {single constant} {4} \
         "$Interp state all disabled"
      if { $dbtype == 0 } { set dbtype 4 }
   }

#  If no default type exists set one and make sure if it does exist
#  that it is sensible.
   if { ![info exists CCDglobalpars(DEBIASTYPE)] } {
      set CCDglobalpars(DEBIASTYPE) $dbtype
   } else {
      if { [regexp {[1-4]} $CCDglobalpars(DEBIASTYPE)] } {
         if { !$BIAS(debias,$CCDglobalpars(DEBIASTYPE)) } {
            set CCDglobalpars(DEBIASTYPE) $dbtype
         }
      } else {
         set CCDglobalpars(DEBIASTYPE) $dbtype
      }
   }

#  Interp.
#  Add options to select differ type of interpolation.
   set haveinterp 0
   if { $BIAS(interp,1) } {
      $Interp addbutton {constant for each line} {1}
      set haveinterp 1
   }
   if { $BIAS(interp,2) } {
      $Interp addbutton {single constant for whole frame} {2}
      if { $haveinterp == 0 } { set haveinterp 2 }
   }
   if { $BIAS(interp,3) } {
      $Interp addbutton {linear fit for each line} {3}
      if { $haveinterp == 0 } { set haveinterp 3 }
   }
   if { $BIAS(interp,4) } {
      $Interp addbutton {plane across frame} {4}
      if { $haveinterp == 0 } { set haveinterp 4 }
   }

#  If no default type exists set one and make sure if it does exist
#  that it is sensible.
   if { ! [info exists CCDglobalpars(INTERPTYPE)] && $haveinterp != 0 } {
      set CCDglobalpars(INTERPTYPE) $haveinterp
      set haveinterp 1
   } else  {
      if { $haveinterp != 0 } {
         if { [regexp {[1-4]} $CCDglobalpars(INTERPTYPE)] } {
            if { !$BIAS(interp,$CCDglobalpars(INTERPTYPE)) } {
               set CCDglobalpars(INTERPTYPE) $haveinterp
            }
         } else {
            set CCDglobalpars(INTERPTYPE) $haveinterp
            set haveinterp 1
         }
      }
   }

#  Initialise the state of the window.
   if { $CCDglobalpars(DEBIASTYPE) != "3" } {
      $Interp state all disabled
   }

#  Savespace.
#  Add options for the types of disk space saving.
   $Spacesave addbutton {none} {NONE}
   $Spacesave addbutton {some} {SOME}
   $Spacesave addbutton {lots} {LOTS}

#  Choice.
#  Ok creates the schedule and runs it (run the SCHEDULE application),
#  then exits.
   $Choice addbutton {OK} "CCDDoReduce $Top; $Top kill $Top"

#  Cancel just gets out without any action.
   $Choice addbutton {Cancel} "$Top kill $Top"

#----------------------------------------------------------------------------
#  Associate help
#----------------------------------------------------------------------------
   $Top sethelp ccdpack CCDReduceWindow
   $Menu sethelpitem {On Window} ccdpack CCDReduceWindow
   $Menu sethelp all ccdpack CCDReduceMenu
   $Choice sethelp all ccdpack CCDReduceOK
   $Debiastype sethelp ccdpack CCDReduceDebias
   $Interp sethelp ccdpack CCDReduceInterpolation
   $Spacesave sethelp ccdpack CCDReduceSavespace

#----------------------------------------------------------------------------
#  Pack all widgets.
#----------------------------------------------------------------------------
   pack $menu -fill x
   pack $choice -side bottom -fill x
   pack $debiastype -fill x
   if { $haveinterp } { pack $interp -fill x }
   pack $spacesave -fill x

#  Wait for interaction in this window to end.
      CCDWindowWait $Top

#  End of procedure.
}
# $Id$

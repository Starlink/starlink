#+
#  Name:
#     CCDMain

#  Purpose:
#     Main routine for CCDPACK X reduction interface.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk script.

#  Invocation:
#     source CCDMain.tcl

#  Description:
#     This is the top-level routine for the X reduction interface for
#     CCDPACK. It creates the initial window and performs global
#     initialisations of bindings, colours, script auto path etc.

#  Notes:
#     This interface requires that the extensions [incr Tcl], BLT and
#     TclADAM are available (built into the wish executable that invokes
#     this file).

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 1997, 2000-2001, 2003 Central Laboratory of
#     the Research Councils. Copyright (C) 2006 Particle Physics &
#     Astronomy Research Council. All Rights Reserved.

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
#     PDRAPER: Peter Draper (STARLINK)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     14-MAR-1994 (PDRAPER):
#        Original version
#     5-MAY-1995 (PDRAPER):
#        Started to make changes for Tk4.
#     23-MAY-1995 (PDRAPER):
#        Considerable change to look.
#     21-JUL-1995 (PDRAPER):
#        Re-write to new style.
#     15-APR-1997 (PDRAPER):
#        Added tests and storage for foreign data formats. Netscape
#        now the default help browser (same as HTX).
#     11-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     4-JUL-2001 (MBT):
#        Upgraded for use with Sets.
#     22-JUL-2003 (MBT):
#        Added option to use Mozilla.
#     01-FEB-2006 (PDRAPER):
#        Removed Mosaic browser and added firefox. Fixed problems
#        with default file filters. Changed to use new meta-widget names
#        (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables (strictly all variables at this level are global).
   global CCDallndfs
   global CCDbrowser
   global CCDdetectorcache
   global CCDdir
   global CCDglobalpars
   global CCDseetasks
   global CCDstarhtml
   global CCDimagefilters
   global GWMDEVICE
   global KAPdir
   global XDEVICE
   global env
   global MAIN
#.

#  Name this application (for xresources etc.)
   tk appname xreduce

#  Withdraw the . window as this cannot be controlled as a metawidget.
   wm withdraw .

#-----------------------------------------------------------------------------
#  Interface initialisation section:
#-----------------------------------------------------------------------------

#  Set the global variable which controls where to pickup source etc.
   if { [ info exists env(CCDPACK_DIR) ] } {
      set CCDdir $env(CCDPACK_DIR)
   } else {
      set CCDdir /star/bin/ccdpack
   }

#  Locate KAPPA
   if { [info exists env(KAPPA_DIR)] } {
      set KAPdir $env(KAPPA_DIR)
   } else {
      set KAPdir /star/bin/kappa
   }

#  Set the HTML docs installation points.
   if { [ info exists env(CCDPACK_HTML) ] } {
      set CCDstarhtml $env(CCDPACK_HTML)
   } else {
      set CCDstarhtml "$CCDdir/../../help:$CCDdir/../../docs:$CCDdir"
   }

#  Set the help browser. This is either set by the HTX_BROWSER variable,
#  or by checking that a known one exists on the PATH. This will
#  be overidden by a ~/.ccdpack assignment.
   if { [info exists env(HTX_BROWSER)] } {
      set CCDbrowser $env(HTX_BROWSER)
   } else {
      set CCDbrowser {}
      foreach browser {netscape Netscape mozilla Mozilla firefox Firefox} {
         foreach directory [split $env(PATH) ":" ] {
            if { [ file executable ${directory}/${browser} ] } {
               set CCDbrowser $browser
               break
            }
         }
         if { $CCDbrowser != {} } { break }
      }
   }

#  Define the autoload path for TCL procedures.
   lappend auto_path $CCDdir

#  Start up the adamtask interface. Initialise the application register.
   if { ! [CCDTaskRegistry] } {

#  If failed no CCDPACK monolith so no need to proceed.
      $Top kill $Top
      destroy .
   }

#  Set any parameters defaults (before reading state etc.). This also
#  starts up the CCDPACK monolith (hence adamtask stuff before it).
   CCDInitialize

#  Set global bindings.
   source $CCDdir/CCDBindings.tcl

#  Global options (colours, fonts, reliefs etc.). Also reads ~/.ccdpack
#  at an appropriate point.
   source $CCDdir/CCDOptions.tcl

#  Find and store all the NDF foreign formats being used. These are
#  used as part of the file filtering mechanisms (note this isn't
#  therefore set by any values in .ccdpack, it's important that
#  the conversion filters are actually setup).
   set CCDimagefilters {*}
   if { [info exists env(NDF_FORMATS_IN)] } {
      set CCDimagefilters {{NDF(.sdf) *.sdf}}
      set new_types [split $env(NDF_FORMATS_IN) ","]
      foreach pair $new_types {
         regexp {([^\(]*).([^\)]*)} $pair dummy name type
         if { $name != "NDF" } {
            lappend CCDimagefilters [list $name\(${type}\) *${type}]
         }
      }
   }

#  Set the directories to search for CCD setups and import control tables
#  (do this after reading ~/.ccdpack).
   if { ! [info exists CCDdetectorcache] } {
      set CCDdetectorcache "$CCDdir"
   } else {
      if { ! [ string match "$CCDdir" $CCDdetectorcache] } {
         append CCDdetectorcache "$CCDdir "
      }
   }
   if { [info exists env(CCDPACK_CONFIG)] } {
      if { ! [ string match "*$env(CCDPACK_CONFIG)*" $CCDdetectorcache] } {
         append CCDdetectorcache "$env(CCDPACK_CONFIG) "
      }
   }

#  The Gwm device. Use a name not likely to clash with any others.
   set GWMDEVICE "xreduce"
   set XDEVICE "xw;xreduce"

#-----------------------------------------------------------------------------
#  Widget creation.
#-----------------------------------------------------------------------------

#  Create top-level widget for main window.
   CCDCcdWidget Top top Ccd::toplevel .topwin -title {CCDPACK reduction GUI}

#  Record this for use everywhere.
   set MAIN(window) $Top
   set MAIN(name) XREDUCE

#  Menubar
   CCDCcdWidget Menubar menubar Ccd::helpmenubar $Top.menubar

#  Frame to contain description and bitmaps.
   CCDTkWidget Separate1 separate1 frame $top.s1 -height 3
   CCDTkWidget Frame frame frame $top.top -borderwidth 0

#  Description of interface.
#  Bitmaps with CCDPACK "logo".
   CCDTkWidget Bitmapl bitmapl \
      label $frame.bitl -bitmap @$CCDdir/ccdbitmap64 -anchor center
   CCDTkWidget Bitmapr bitmapr \
      label $frame.bitr -bitmap @$CCDdir/ccdbitmap64 -anchor center

#  Textual message.
   CCDTkWidget Label label \
      label $frame.label -anchor center \
      -text "xreduce \n\n An X interface to automated CCDPACK reductions."

#  Buttons for the main choices.
#  Configuration of package and CCD characteristics.
   CCDTkWidget Separate2 separate2 frame $top.s2 -height 3
   CCDCcdWidget Config config \
      Ccd::choice $Top.configure \
                  -standard false \
                  -label "\nConfiguration\n" \
                  -stack horizontal \
                  -width 30 \
                  -buttonwidth 18

#  NDF import (FITS and by hand).
   CCDTkWidget Separate3 separate3 frame $top.s3 -height 3
   CCDCcdWidget Import import \
      Ccd::choice $Top.imprt \
                  -standard false \
                  -label "\nData Import\n" \
                  -stack horizontal \
                  -width 30 \
                  -buttonwidth 18

#  Set grouping.
   CCDTkWidget Separate4 separate4 frame $top.s4 -height 3
   CCDCcdWidget Sethead sethead \
      Ccd::choice $Top.sets \
                  -standard false \
                  -label "\nGroup by Set\n" \
                  -stack horizontal \
                  -width 30 \
                  -buttonwidth 18

#  Create & Run reduction script.
   CCDTkWidget Separate5 separate5 frame $top.s5 -height 3
   CCDCcdWidget Run run \
      Ccd::choice $Top.run \
               -standard false \
               -label "\nReduction\n" \
               -stack horizontal \
               -width 30 \
               -buttonwidth 18

#  Scrollbox for showing names (and types?) of current NDFs.
   CCDCcdWidget Names names \
      Ccd::scrollbox $Top.names -label "\nKnown Data Files\n" -anchor c

#  Exit and Update NDF listbox.
   CCDCcdWidget Control control Ccd::choice $Top.control -standard false

#-----------------------------------------------------------------------------
#  Configure widgets.
#-----------------------------------------------------------------------------

#  Override trap of destruction by window manager of top-level, making
#  sure that . is destroyed too.
   wm protocol $top WM_DELETE_WINDOW CCDExit

#  Add file item to exit.
   $Menubar addcommand File Exit CCDExit

#  Add option to select from a known configuration.
   $Menubar addcommand Options {Set detector...} "CCDSetDetector $Top.detect"

#  Add save and restore state commands.
   $Menubar addcommand Options {Restore state...} "CCDReadGlobals $Top.readwin"
   $Menubar addcommand Options {Save current state...} \
      "CCDSaveGlobals $Top.savewin"

#  Monitor output from tasks?
   if { ! [info exists CCDseetasks] } { set CCDseetasks 0 }
   $Menubar addcheckbutton Options {Monitor output from tasks} \
      -variable CCDseetasks

#  Button for setting general configuration options.
   $Config addbutton {General Options}\
      "CCDSetGenGlobals $Top.globals $Import state all normal"

#  Button for setting CCD characteristics (if not getting from FITS).
   $Config addbutton {CCD Characteristics}\
      "CCDSetCCDGlobals $Top.globals $Import state all normal"

#  Button to by-pass this part (usual if state has been restored).
   $Config addbutton {Bypass Stage} "$Import state all normal"

#  Construct the command to move the the stage after data import - may
#  be either Set grouping or run.
   set nextstage "
      global CCDglobalpars
      if { \$CCDglobalpars(USESET) == \"TRUE\" } {
         set Next $Sethead
      } else {
         set Next $Run
      }
      \$Next state all normal
   "

#  Button for organising NDFs into types etc. "by hand".
   $Import addbutton {Manual Organization} \
      "CCDNDFOrganize $Top.doimport $nextstage"

#  Button for importing FITS headers.
   $Import addbutton {Using FITS Headers} \
      "CCDFITSImport $Top.doimport $nextstage"

#  Button for skipping this section.
   $Import addbutton {Bypass Stage} "$nextstage"

#  Disable import buttons for now.
   $Import state all disabled

#  Button for generic Set grouping.
   $Sethead addbutton {Add Set Headers} \
      "CCDAddSetHeaders $Top.setgrp $Run state all normal"

#  Button for skipping this section.
   $Sethead addbutton {Bypass Stage} "$Run state all normal"

#  Disable Set grouping buttons for now.
   $Sethead state all disabled

#  Button for creating and running reduction script.
   $Run addbutton {Setup and Run} \
      "if { \[CCDCheckReduce $Top\] } {
          CCDReduce $Top.doreduce
       }
      "

#  Disable this button.
   $Run state all disabled

#  Button to exit from application.
   $Control addbutton Exit CCDExit

#  Button for updating the contents of the listbox. Add a trivial procedure
#  for updating the contents when the CCDallndfs variable is written to. This
#  doesn't seem to work for appends so add a button to force an Update.
   $Control addbutton Update "CCDUpdateNDFList $Names"
   trace variable CCDallndfs wu "CCDUpdateNDFList $Names"
   proc CCDUpdateNDFList { Box args } {
      global CCDallndfs
      if { [info exists CCDallndfs] } {
         $Box clear 0 end
         eval $Box insert 0 $CCDallndfs
      }
   }
   $Control invoke Update

#------------------------------------------------------------------------------
#  Add context sensitive help now that all sub-components are created.
#------------------------------------------------------------------------------
   $Top sethelp ccdpack CCDMainWindow
   $Menubar sethelpitem {On Window} ccdpack CCDMainWindow
   $Menubar sethelp all ccdpack CCDMainMenu
   $Config sethelp all ccdpack CCDMainConfiguration
   $Import sethelp all ccdpack CCDMainDataImport
   $Sethead sethelp all ccdpack CCDMainSets
   $Run sethelp all ccdpack CCDMainReduction
   $Names sethelp ccdpack CCDMainNames
   $Control sethelp all ccdpack CCDMainKnownData
   $Names sethelp ccdpack CCDMainKnownData

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
   pack $control -fill x -side bottom
   pack $menubar -fill x -side top
   pack $bitmapl -side left
   pack $label -side left -fill x -expand true
   pack $bitmapr -side left
   pack $frame -side top -fill x
   pack $separate1 -fill x
   pack $config  -fill x -side top
   pack $separate2 -fill x
   pack $import -fill x -side top
   pack $separate3 -fill x
   pack $sethead -fill x -side top
   pack $separate4 -fill x
   pack $run -fill x -side top
   pack $separate5 -fill x
   pack $names -fill both -expand true


# $Id$

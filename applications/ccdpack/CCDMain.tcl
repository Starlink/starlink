#+
#  Name:
#     CCDMain

#  Purpose:
#     Main routine for CCDPACK X reduction interface.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     This is the top-level routine for the X reduction interface for
#     CCDPACK. It creates the initial window and performs global
#     initialisations of bindings, colours, script auto path etc.

#  Invocation:
#     source CCDMain.tcl

#  Notes:
#     This interface requires that the extensions [incr Tcl] version
#     2.2, BLT 2.1 and TclADAM are available (built into the wish
#     executable that invokes this file). It is not known to work with
#     any other combinations and will not work with earlier versions
#     of Tcl and Tk or [incr Tcl].


#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
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
#     {enter_changes_here}

#-

#  Global variables (strictly all variables at this level are global).
   global CCDallndfs
   global CCDbrowser
   global CCDdetectorcache
   global CCDdir
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
      if { [info exists env(STARLINK)] } {
         set CCDstarhtml "$env(STARLINK)/docs:$env(STARLINK)/help"
      } else {

#  Use a relative offset to CCDdir.
         set CCDstarhtml "$CCDdir/../../docs:$CCDdir/../../help"
      }
   }

#  Set the help browser. This is either set by the HTX_BROWSER variable,
#  or by checking that a known one exists on the PATH. This will
#  be overidden by a ~/.ccdpack assignment.
   if { [info exists env(HTX_BROWSER)] } { 
      set CCDbrowser $env(HTX_BROWSER)
   } else {
      set CCDbrowser {}
      foreach browser {netscape Netscape Mosaic mosaic} {
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
   set CCDimagefilters {{NDF "*.sdf"}}
   if { [info exists env(NDF_FORMATS_IN)] } { 
      set new_types [split $env(NDF_FORMATS_IN) ","]
      foreach pair $new_types { 
         regexp {([^\(]*).([^\)]*)} $pair dummy name type
         if { $name != "FITS" && $name != "NDF" && $name != "FIT" } { 
            lappend CCDimagefilters [list $name *${type}]
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
   set Top [Ccd_toplevel .topwin -title {CCDPACK reduction GUI}]

#  Record this for use everywhere.
   set MAIN(window) $Top
   set MAIN(name) XREDUCE

#  Menubar
   set Menubar [Ccd_helpmenubar $Top.menubar]

#  Frame to contain description and bitmaps.
   set Separate1 [frame $Top.s1 -height 3]
   set Frame [frame $Top.top -borderwidth 0]

#  Description of interface.
#  Bitmaps with CCDPACK "logo".
   set Bitmapl [label $Frame.bitl -bitmap @$CCDdir/ccdbitmap64 -anchor center ]
   set Bitmapr [label $Frame.bitr -bitmap @$CCDdir/ccdbitmap64 -anchor center ]

#  Textual message.
   set Label [label $Frame.label -anchor center \
      -text "xreduce \n\n An X interface to automated CCDPACK reductions."]

#  Buttons for the main choices.
#  Configuration of package and CCD characteristics.
   set Separate2 [frame $Top.s2 -height 3]
   set Config [Ccd_choice $Top.configure \
                  -standard false \
                  -label "\nConfiguration\n" \
                  -stack horizontal \
                  -width 30 \
                  -buttonwidth 18]

#  NDF import (FITS and by hand).
   set Separate3 [frame $Top.s3 -height 3]
   set Import [Ccd_choice $Top.imprt \
                  -standard false \
                  -label "\nData Import\n" \
                  -stack horizontal \
                  -width 30 \
                  -buttonwidth 18]

#  Create & Run reduction script.
   set Separate4 [frame $Top.s4 -height 3]
   set Run [Ccd_choice $Top.run \
               -standard false \
               -label "\nReduction\n" \
               -stack horizontal \
               -width 30 \
               -buttonwidth 18]

#  Scrollbox for showing names (and types?) of current NDFs.
   set Names [Ccd_scrollbox $Top.names -label "\nKnown Data Files\n" -anchor c]

#  Exit and Update NDF listbox.
   set Control [Ccd_choice $Top.control -standard false]

#-----------------------------------------------------------------------------
#  Configure widgets.
#-----------------------------------------------------------------------------

#  Override trap of destruction by window manager of top-level, making
#  sure that . is destroyed too.
   wm protocol $Top WM_DELETE_WINDOW CCDExit

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

#  Button for organising NDFs into types etc. "by hand".
   $Import addbutton {Manual Organization} \
      "CCDNDFOrganize $Top.doimport $Run state all normal"

#  Button for importing FITS headers.
   $Import addbutton {Using FITS Headers} \
      "CCDFITSImport $Top.doimport $Run state all normal"

#  Button for skipping this section.
   $Import addbutton {Bypass Stage} "$Run state all normal"

#  Disable import buttons for now.
   $Import state all disabled

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
   $Run sethelp all ccdpack CCDMainReduction
   $Names sethelp ccdpack CCDMainNames
   $Control sethelp all ccdpack CCDMainKnownData
   $Names sethelp ccdpack CCDMainKnownData

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
   pack $Control -fill x -side bottom
   pack $Menubar -fill x -side top
   pack $Bitmapl -side left
   pack $Label -side left -fill x -expand true
   pack $Bitmapr -side left
   pack $Frame -side top -fill x
   pack $Separate1 -fill x
   pack $Config  -fill x -side top
   pack $Separate2 -fill x
   pack $Import -fill x -side top
   pack $Separate3 -fill x
   pack $Run -fill x -side top
   pack $Separate4 -fill x
   pack $Names -fill both -expand true


# $Id$

#+
#  Name:
#     CCDFileMonitorMain

#  Purpose:
#     Main routine for filemonitor X interface.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     This is the top-level routine for the X reduction interface for
#     monitoring the contents of a file. It creates the initial window and
#     performs global initialisations of bindings, colours, script
#     auto path etc.

#  Invocation:
#     wish CCDFileMonitorMain file

#  Notes:
#     This interface requires that the extensions [incr Tcl] version
#     2.2, BLT 2.1 and TclADAM are available (built into the wish
#     executable that invokes this file) as well as Tcl7.6 and
#     Tk4.2. It is not known to work with any other combinations and
#     will not work with earlier versions of Tcl and Tk and [incr Tcl].

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     16-NOV-1995 (PDRAPER):
#        Original version
#     {enter_changes_here}

#-

#  Global variables (strictly all variables at this level are global).
   global env
   global MAIN
   global CCDstarhtml
   global CCDbrowser
#.

#  Name this application (for xresources etc.)
   tk appname filemonitor

#  Withdraw the . window as we don't need it.
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

#  Set the interface look and feel. Define the autoload path etc.
   lappend auto_path $CCDdir

#  Global bindings.
   source $CCDdir/CCDBindings.tcl

#  Global options
   source $CCDdir/CCDOptions.tcl

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
      foreach browser {Mosaic mosaic netscape Netscape} {
         foreach directory [split $env(PATH) ":" ] {
            if { [ file executable ${directory}/${browser} ] } {
               set CCDbrowser $browser
               break
            }
         }
         if { $CCDbrowser != {} } { break }
      }
   }

#  Check that we have an input file (and it exists).
   set file [lindex $argv 0]
   if { $file == "" } { 
      CCDIssueError "No input file given"
      exit 1
   }
   if { ! [file readable $file] } { 
      CCDIssueError "Cannot read file \"$file\""
      exit 1
   }

#  Open a pipe to tail -f to monitor the file (do this now so that 
#  we can close pipe cleanly).
   set pipe [open "|tail +0f $file"]
   set pipeproc [pid $pipe]

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------

#  Create top-level widget for main window.
   set Top [Ccd_toplevel .topwin -title {CCDPACK File contents monitor}]

#  Record this for use everywhere.
   set MAIN(window) $Top
   set MAIN(name) FILEMONITOR

#  Menubar
   set Menubar [Ccd_helpmenubar $Top.menubar -standard 0]

#  Textual message.
   set Label [label $Top.label -anchor center \
                 -text "Monitoring file \"$file\""]

#  Text area for displaying contents.
   set Contents [Ccd_scrolltext $Top.text]

#  Exit button
   set Control [Ccd_choice $Top.control -standard false]

#-----------------------------------------------------------------------------
#  Configure widgets.
#-----------------------------------------------------------------------------

#  Override trap of destruction by window manager of top-level, making
#  sure that . is destroyed too.
   wm protocol $Top WM_DELETE_WINDOW \
      "catch {exec kill $pipeproc}
       catch {close $pipe}
       CCDExit
      "

#  Add file item to exit.
   $Menubar addbutton File 0
   $Menubar addcommand File Exit \
      "catch {exec kill $pipeproc}
       catch {close $pipe}
       CCDExit
      "

#  Button to exit from application.
   $Control addbutton Exit \
      "catch {exec kill $pipeproc}
       catch {close $pipe}
       CCDExit
      "
#------------------------------------------------------------------------------
#  Add help.
#------------------------------------------------------------------------------
   $Top sethelp ccdpack CCDFileMonitorMain
   $Menubar sethelpitem {On Window} ccdpack CCDFileMonitorMain

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
   pack $Control -fill x -side bottom
   pack $Menubar -fill x -side top
   pack $Label -side top -fill x -ipady 10
   pack $Contents -fill both -side bottom -expand true

#------------------------------------------------------------------------------
#  Interface activation.
#------------------------------------------------------------------------------
#  Simple procedure to up date the contents
   proc CCDReadLogFile { w pipe file } {
      gets $pipe line
      $w insert end "$line \n"
      update idletasks
   }

#  Set the fileevent handler.
   tkwait visibility $Top
   fileevent $pipe readable [list CCDReadLogFile $Contents $pipe $file]
   
# $Id$

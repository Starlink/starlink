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
#     This interface requires that the extensions [incr Tcl], BLT and
#     TclADAM are available (built into the wish executable that 
#     invokes this file).  

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     16-NOV-1995 (PDRAPER):
#        Original version
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     22-JUL-2003 (MBT):
#        Added option to use Mozilla.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
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
      set CCDstarhtml "$CCDdir/../../help:$CCDdir/../../docs:$CCDdir"
   }

#  Set the help browser. This is either set by the HTX_BROWSER variable,
#  or by checking that a known one exists on the PATH. This will
#  be overidden by a ~/.ccdpack assignment.
   if { [info exists env(HTX_BROWSER)] } { 
      set CCDbrowser $env(HTX_BROWSER)
   } else {
      set CCDbrowser {}
      foreach browser {Mosaic mosaic netscape Netscape mozilla Mozilla} {
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
   CCDCcdWidget Top top \
      Ccd::toplevel .topwin -title "CCDPACK File contents monitor"

#  Record this for use everywhere.
   set MAIN(window) $Top
   set MAIN(name) FILEMONITOR

#  Menubar
   CCDCcdWidget Menubar menubar Ccd::helpmenubar $Top.menubar -standard 0

#  Textual message.
   CCDTkWidget Label label \
      label $top.label -anchor center -text "Monitoring file '$file'"

#  Text area for displaying contents.
   CCDCcdWidget Contents contents Ccd::scrolltext $Top.text

#  Exit button
   CCDCcdWidget Control control Ccd::choice $Top.control -standard false

#-----------------------------------------------------------------------------
#  Configure widgets.
#-----------------------------------------------------------------------------

#  Override trap of destruction by window manager of top-level, making
#  sure that . is destroyed too.
   wm protocol $top WM_DELETE_WINDOW \
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
   pack $control -fill x -side bottom
   pack $menubar -fill x -side top
   pack $label -side top -fill x -ipady 10
   pack $contents -fill both -side bottom -expand true

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
   tkwait visibility $top
   fileevent $pipe readable [list CCDReadLogFile $Contents $pipe $file]
   
# $Id$

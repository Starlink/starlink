#+
#  Name:
#     CCDFileMonitorMain

#  Purpose:
#     Main routine for filemonitor X interface.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk script.

#  Invocation:
#     wish CCDFileMonitorMain file

#  Description:
#     This is the top-level routine for the X reduction interface for
#     monitoring the contents of a file. It creates the initial window and
#     performs global initialisations of bindings, colours, script
#     auto path etc.

#  Notes:
#     This interface requires that the extensions [incr Tcl], BLT and
#     TclADAM are available (built into the wish executable that
#     invokes this file).

#  Copyright:
#     Copyright (C) 1995, 2000, 2003 Central Laboratory of the Research
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

#  Bugs:
#     {note_any_bugs_here}

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
   wm protocol $top WM_DELETE_WINDOW CCDExit

#  Add file item to exit.
   $Menubar addbutton File 0
   $Menubar addcommand File Exit CCDExit

#  Button to exit from application.
   $Control addbutton Exit CCDExit

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
#  Simple procedure to continually update the contents with new content
#  from the given file identifier "pipe".
   proc CCDReadLogFile { w pipe file } {
      if { [gets $pipe line] >= 0 } {
         $w insert end "$line \n"
         update idletasks
      }
      after 1000 CCDReadLogFile $w $pipe $file
   }

#  Wait for UI to realise.
   tkwait visibility $top

#  Open the file to monitor.
   set pipe [::open $file]

#  Display current file contents.
   $Contents insert end [::read $pipe]

#  Start the update display.
   CCDReadLogFile $Contents $pipe $file

# $Id$

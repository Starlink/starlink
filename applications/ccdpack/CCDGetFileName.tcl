   proc CCDGetFileName { Topwin title image } {

#+
#  Name:
#     CCDGetFileName

#  Purpose:
#     Gets the name of a file, allowing directory movement

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Global Parameters:
#     CCDimportfile = filename (write)
#        On exit this variable contains the name of the file
#     CCDimportexists = filename (write)
#        On exit this variable indicates whether the output
#        file exists (or not).
#     CCDimportfilter = list (read)
#        The current file filter value(s). If more than one exists
#        this procedure extends the interface to allow selection
#        from the list. The list format is pairs of symbolic names
#        and the associated filter types.
#     CCDndfcontainers = array (write)
#        An array giving the name of the HDS container file for each
#        NDF which has been encountered (will only be affected if
#        images is true).

#  Method:
#     This is a general get the name of a file routine. It displays
#     a top-level widget which gets the grab and focus. The display
#     includes a file filter a list of the current directory contents,
#     the current directory and a list of directories which may be
#     moved into (by double clicking). Selection of an existing name
#     may be made by double clicking on it, or a new name may be entered.

#  Parameters:
#     Topwin = string (read)
#        The name of the top-level widget which contains the form.
#     title = string (read)
#        The title of the window (use this to indicate the context).
#     image = boolean (read)
#        If true, then the intention is to get an image file; if false
#        the intention is to get any type of file.  Currently the
#        difference is that if image is true any HDS container file
#        which is encountered is passed to ndgexpand in order to
#        look inside it for NDF structures.

#  Copyright:
#     Copyright (C) 1993-1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 1997, 2000-2001 Central Laboratory of the
#     Research Councils. Copyright (C) 2006 Particle Physics &
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
#     PDRAPER: Peter Draper (Starlink - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     7-SEP-1993 (PDRAPER):
#        Original version.
#     2-MAR-1994 (PDRAPER):
#        Now CCDGetFileName, much improved functionality. Global names
#        changed to CCDxxxxxxxx rule.
#     29-MAR-1994 (PDRAPER):
#        Now uses mega-widgets and has keyboard traversal.
#     11-MAY-1995 (PDRAPER):
#        Updated to Tk4.0.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     18-AUG-1995 (PDRAPER):
#        Converted to new standards.
#     23-AUG-1995 (PDRAPER):
#        Make sure that full path name is used.
#     15-APR-1997 (PDRAPER):
#        Converted to use a list of filters.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     25-JUN-2001 (MBT):
#        Added image parameter, and made it keep track of what NDF is in
#        what container file with CCDndfcontainers global.
#     1-JAN-2006 (PDRAPER):
#        Fixed problems handling the default file filter.
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global env
      global CCDimportfile
      global CCDimportexists
      global CCDimportfilter
      global CCDcurrentdirectory
      global CCDndfcontainers
      global gotfilename         ;# Special to this procedure
#.

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------
#  Create the top-level widget.
      CCDCcdWidget Top top Ccd::toplevel $Topwin -title "$title"

#  Menubar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Labelled entry widget for current directory name.
      CCDCcdWidget Directory directory \
         Ccd::labent $Top.direct -text {Directory:}

#  Labelled entry widget for the file filter. If CCDimportfilter
#  is a list of length greater than 1 then need an option widget.
      if { ! [info exists CCDimportfilter] } {
         set CCDimportfilter ""
      }
      if { [llength $CCDimportfilter] > 1 } {
         CCDCcdWidget Filefilter filefilter \
            Ccd::option $Top.filter -text "File Filter:"
      } else {
         CCDCcdWidget Filefilter filefilter \
            Ccd::labent $Top.filter -text "File Filter:"
      }

#  Scrollbox for directory names.
      CCDCcdWidget Directbox directbox Ccd::scrollbox $Top.directbox

#  Scrollbox for names in current directory.
      CCDCcdWidget Filebox filebox Ccd::scrollbox $Top.filebox

#  Labelled entry widget for name of selected file.
      CCDCcdWidget Selected selected \
         Ccd::labent $Top.select \
                       -text "Name of selected file:" \
                       -placelabel top

#  Choice bar for control of form.
      CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 0

#----------------------------------------------------------------------------
#  Widget configuration.
#----------------------------------------------------------------------------

#  Menubar.
#  File items to cancel or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add current directory, home directory and any directories
#  already visited to the Options menu.
      CCDRestoreDirectoryMenu $Menu Options $Directory \
         $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options [pwd] $Directory \
         $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options $env(HOME) $Directory \
         $Choice Filter

#  Directbox. Add bindings for single and double mouse click to put
#  listbox entry into the current directory. Double click also accepts
#  this as the value to use. <Return> also moves to the currently
#  selected directory. Button 1 motion just updates the current
#  directory field.
      $Directbox bind list <Button-1> \
         "$Directory clear 0 end
	  set index \[%W nearest %y \]
          $Directory insert 0 \[ %W get \$index \]
	  %W selection clear 0 end
         "
      $Directbox bind list <B1-Motion> \
         "$Directory clear 0 end
	  set index \[%W nearest %y \]
          $Directory insert 0 \[ %W get \$index \]
         "
      $Directbox bind list <Return> \
         "$Directory clear 0 end
	  set index \[ lindex \[ %W curselection \] 0\]
          $Directory insert 0 \[ %W get \$index \]
	  $Directbox select clear 0 end
	  $Choice invoke Filter
         "
      $Directbox bind list <Double-Button-1> \
         "$Directory clear 0 end
	  set index \[%W nearest %y \]
          $Directory insert 0 \[ %W get \$index \]
	  %W selection clear 0 end
	  $Choice invoke Filter
         "

#  Directory. Add binding to move to the current directory when return
#  is pressed.
      $Directory bind entry <Return> "$Choice invoke Filter;break"

#  Filefilter. Add binding to <Return> to invoke the file filter button.
#  Also add all options if more than one.
      $Filefilter bind entry <Return> "$Choice invoke Filter;break"
      if { [llength $CCDimportfilter] > 1 } {
         foreach pair "$CCDimportfilter" {
            set name [lindex $pair 0]
            set type [lindex $pair 1]
            $Filefilter addoption $name $type "$Choice invoke Filter"
         }
      }

#  Filebox. Add Bindings to single and double mouse click to put
#  listbox entry into the selected file widget. Double also accepts this as
#  the value to use.
      $Filebox bind list <Button-1> \
         "$Selected clear 0 end
          set index \[%W nearest %y\]
          set filename \[ %W get \$index \]
          set dir \$CCDcurrentdirectory
          if { \$dir == \"/\" } {
             $Selected insert 0 \"/\$filename\"
          } else {
             $Selected insert 0 \"\$dir/\$filename\"
          }
	  %W selection clear 0 end
         "
      $Filebox bind list <Double-Button-1> \
         "$Selected clear 0 end
          set index \[%W nearest %y\]
          set filename \[ %W get \$index \]
          set dir \$CCDcurrentdirectory
          if { \$dir == \"/\" } {
             $Selected insert 0 \"/\$filename\"
          } else {
             $Selected insert 0 \"\$dir/\$filename\"
          }
	  %W selection clear 0 end
          set gotfilename 1
         "

#  Bind <Return> to work like double-click.
      $Filebox bind list <Return> \
         "$Selected clear 0 end
          $Selected insert 0 \[%W get \[%W nearest %y\] \]
	  %W selection clear 0 end
          set gotfilename 1
         "

#  Selected. Add binding to <Return> to return the entry contents.
      $Selected bind entry <Return> {set gotfilename 1}

#  Choice. Add the buttons at the bottom for the main
#  options. "Filter" to apply the file filter to the current
#  directory, "OK" to accept the current file selection and "Cancel"
#  to give up without a fight.
#  "OK" button. Set command to allow the procedure to continue.
      $Choice addbutton OK {set gotfilename 1}

#  "Filter" button. Updates the current directory lists and adds a menu
#  item for the directory (also updates CCDcurrentdirectory).
      $Choice addbutton Filter \
         "CCDGetFileUpdate $Filebox $Directbox $Directory $Filefilter $image
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] $Directory $Choice Filter
         "

#  "Cancel" button.
      $Choice addbutton Cancel {set gotfilename 0}

#----------------------------------------------------------------------------
#  Set the help for this window.
#----------------------------------------------------------------------------
      if { $image } {
         set helpref CCDGetImageName
      } else {
         set helpref CCDGetFileName
      }
      $Top sethelp ccdpack ${helpref}Window
      $Menu sethelpitem {On Window} ccdpack ${helpref}Window
      $Menu sethelp all ccdpack ${helpref}Menu

#----------------------------------------------------------------------------
#   Pack all widgets.
#----------------------------------------------------------------------------
      pack $menu       -side top -fill x
      pack $choice     -side bottom -fill x
      pack $selected   -side bottom -fill x
      pack $directory  -side top -fill x
      pack $filefilter -side top -fill x
      pack $directbox  -side left -expand true -fill both
      pack $filebox    -side right -expand true -fill both

#----------------------------------------------------------------------------
#  Now set options and show file names.
#----------------------------------------------------------------------------
#  Deal with the current filter (defaults to *)
      if { ![info exists CCDimportfilter] } {
         set CCDimportfilter "*"
      } elseif { $CCDimportfilter == "" } {
         set CCDimportfilter "*"
      }

#  Deal with the current directory (default to [pwd])
      if { ![info exists CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      } elseif { $CCDcurrentdirectory == "" } {
         set CCDcurrentdirectory [pwd]
      } elseif { ![file isdirectory $CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      }

#  Set value of entry window.
      $Directory insert 0 $CCDcurrentdirectory

#  Invoke the filter button to get first setup.
      if { [llength $CCDimportfilter] > 1 } {
         $Filefilter insert 0 [lindex [lindex $CCDimportfilter 0] 1]
      } else {
         $Filefilter insert 0 "$CCDimportfilter"
      }
      $Choice invoke Filter

#  Wait for an file name to be given or not as the case maybe.
      CCDVariableWait gotfilename $Top $Selected entry

#  Get the name.
      if { $gotfilename } {
         set CCDimportfile [$Selected get]

#  Does the file exist and is it an ordinary one?
         set CCDimportexists \
             [expr [file isfile $CCDimportfile] || \
                   ( $image && [llength [ndgexpand $CCDimportfile]] == 1 )]

#  Set the current file filter.
         set CCDimportfilter [$Filefilter get]
      } else {

#  No file given.
	 set CCDimportexists 0
         set CCDimportfile ""
	 set CCDimportfilter ""
      }

#  And destroy the window before proceeding.
      $Top kill $Top

#  End of procedure.
   }

# $Id$

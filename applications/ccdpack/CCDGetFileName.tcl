   proc CCDGetFileName { Top title } {

#+
#  Name:
#     CCDGetFileName

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Gets the name of a file, allowing directory movement

#  Method:
#     This is a general get the name of a file routine. It displays
#     a top-level widget which gets the grab and focus. The display
#     includes a file filter a list of the current directory contents,
#     the current directory and a list of directories which may be
#     moved into (by double clicking). Selection of an existing name
#     may be made by double clicking on it, or a new name may be entered.

#  Parameters:
#     Top = string (read)
#        The name of the top-level widget which contains the form.
#     title = string (read)
#        The title of the window (use this to indicate the context).

#  Global parameters:
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

#  Authors:
#     PDRAPER: Peter Draper (Starlink - Durham University)
#     {enter_new_authors_here}

#  History:
#     7-SEP-1993 (PDRAPER):
#        Original version.
#     2-MAR-1994 (PDRAPER):
#     	 Now CCDGetFileName, much improved functionality. Global names
#	 changed to CCDxxxxxxxx rule.
#     29-MAR-1994 (PDRAPER):
#     	 Now uses mega-widgets and has keyboard traversal.
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
#     {enter_further_changes_here}

#-

#  Global variables:
      global env
      global CCDimportfile
      global CCDimportexists
      global CCDimportfilter
      global CCDcurrentdirectory
      global gotfilename         ;# Special to this procedure
#.

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------
#  Create the top-level widget.
      Ccd_toplevel $Top -title $title

#  Menubar.
      set Menu [Ccd_helpmenubar $Top.menubar]

#  Labelled entry widget for current directory name.
      set Directory [Ccd_labent $Top.direct -text {Directory:}]

#  Labelled entry widget for the file filter. If CCDimportfilter
#  is a list of length greater than 1 then need an option widget.
      if { ! [info exists CCDimportfilter] } { 
         set CCDimportfilter ""
      }
      if { [llength $CCDimportfilter] > 1 } { 
         set Filefilter [Ccd_option $Top.filter -text {File Filter:}]
      } else {
         set Filefilter [Ccd_labent $Top.filter -text {File Filter:}]
      }

#  Scrollbox for directory names.
      set Directbox [Ccd_scrollbox $Top.directbox]

#  Scrollbox for names in current directory.
      set Filebox [Ccd_scrollbox $Top.filebox]

#  Labelled entry widget for name of selected file.
      set Selected [Ccd_labent $Top.select \
                       -text {Name of selected file:} \
                       -placelabel top]

#  Choice bar for control of form.
      set Choice [Ccd_choice $Top.choice -standard 0]

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
          set directory \$CCDcurrentdirectory
          if { \$directory == \"/\" } {
             $Selected insert 0 \"/\$filename\"
          } else {
             $Selected insert 0 \"\$directory/\$filename\"
          }
	  %W selection clear 0 end
         "
      $Filebox bind list <Double-Button-1> \
         "$Selected clear 0 end
          set index \[%W nearest %y\]
          set filename \[ %W get \$index \]
          set directory \$CCDcurrentdirectory
          if { \$directory == \"/\" } {
             $Selected insert 0 \"/\$filename\"
          } else {
             $Selected insert 0 \"\$directory/\$filename\"
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
         "CCDGetFileUpdate $Filebox $Directbox $Directory $Filefilter
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] $Directory $Choice Filter
         "

#  "Cancel" button.
      $Choice addbutton Cancel {set gotfilename 0}

#----------------------------------------------------------------------------
#  Set the help for this window.
#----------------------------------------------------------------------------
      $Top sethelp ccdpack CCDGetFileNameWindow
      $Menu sethelpitem {On Window} ccdpack CCDGetFileNameWindow
      $Menu sethelp all ccdpack CCDGetFileNameMenu

#----------------------------------------------------------------------------
#   Pack all widgets.
#----------------------------------------------------------------------------
      pack $Menu       -side top -fill x
      pack $Choice     -side bottom -fill x
      pack $Selected   -side bottom -fill x
      pack $Directory  -side top -fill x
      pack $Filefilter -side top -fill x
      pack $Directbox  -side left -expand true -fill both
      pack $Filebox    -side right -expand true -fill both

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
      if { [llength $CCDimportfilter] == 1 } { 
         $Filefilter insert 0 $CCDimportfilter
      } else {
         $Filefilter insert 0 [lindex [lindex $CCDimportfilter 0] 1]
      }
      $Choice invoke Filter

#  Wait for an file name to be given or not as the case maybe.
      CCDVariableWait gotfilename $Top $Selected entry

#  Get the name.
      if { $gotfilename } {
         set CCDimportfile [$Selected get]

#  Does the file exist and is it an ordinary one?
         set CCDimportexists [file isfile $CCDimportfile]

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

   proc CCDGetFileNames { Top title } { 
#+
#  Name:
#     CCDGetFileNames

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Allows the selection of a list of files from various directories.

#  Description:
#     This routine helps in the selection of a list of files. It allows
#     movement between directories and the arbitrary selection of any
#     files in these directories. The final output from this routine is
#     a global list of all the names which have been selected.

#  Notes:
#     This routine is intended for looking around for files to be used
#     in many different places. If just a single file is required use
#     the CCDGetFileName procedure.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window for this form.
#     title = string (read)
#        Title for the window created by this procedure.

#  Global variables:
#     env = array (read)
#        The (Tcl) list of known environment variables.
#     CCDimportfiles = list (write)
#        The names of the files in the selected names listbox on exit.
#	 This is set to null if no files are given.
#     CCDimportfilter = string (read and write)
#        The global file filter.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     8-MAR-1994 (PDRAPER):
#     	 Original version.
#     20-APR-1994 (PDRAPER):
#     	 Now uses mega-widgets.
#     11-MAY-1995 (PDRAPER):
#        Updated to Tk4.0.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     18-AUG-1995 (PDRAPER):
#        Converted to new standards.
#     {enter_further_changes_here}

#-

#  Global variables:
      global env
      global CCDimportfiles
      global CCDimportexists
      global CCDimportfilter
      global CCDcurrentdirectory
      global gotfilename ;# Special to this procedure

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Widget Creation.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Top-level widget for this form.
      Ccd_toplevel $Top -title "$title"

#  Menubar.
      set Menu [Ccd_helpmenubar $Top.menubar]

#  Frame for containing the current directory and file filters.
      set Dirbase [frame $Top.dirbase]

#  Labelled entry for current directory.
      set Directory [Ccd_labent $Dirbase.direct -text {Directory:}]

#  Labelled entry for file filter
      set Filefilter [Ccd_labent $Dirbase.filter -text {File Filter:}]

#  Create frame for containing all scrollboxes.
      set Listbase [frame $Top.listbase]

#  List of directories scrollbox.
      set Directbox [Ccd_scrollbox $Listbase.directbox \
                        -singleselect 1 -label {Directories:} \
                        -exportselect 0]

#  List of files in current directory scrollbox.
      set Filebox [Ccd_scrollbox $Listbase.filebox \
                      -label {Files in directory:} -singleselect 0 \
                      -exportselect 0]

#  Options for adding and removing entries from the list of selected names.
      set Options [Ccd_choice $Listbase.options -standard 0 -stack vertical]

#  Selected files scrollbox.
      set Selectbox [Ccd_scrollbox $Listbase.selectbox \
                        -label {Files selected:} -singleselect 0 \
                        -exportselect 0]

#  Add choice bar for control (OK, Cancel etc.).
      set Choice [Ccd_choice $Top.choice]

#-----------------------------------------------------------------------------
#  Widget configuration.
#-----------------------------------------------------------------------------

#  Menubar. Add home directory and current directory and any directories 
#  already visited this (and possibly previous) session to Options.
      CCDRestoreDirectoryMenu $Menu Options \
         $Directory $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options \
         $env(HOME) $Directory $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options \
         [pwd] $Directory $Choice Filter

#  Directory. Add binding to move to the new directory when return is
#  pressed. This also adds a new command to Options menu to move back here.
      $Directory bind entry <Return> "$Choice invoke Filter"

#  Filefiter. Add binding to invoke the file filter button when
#  <Return> is pressed.
      $Filefilter bind entry <Return> "$Choice invoke Filter"


#  Directbox: Add bindings for single and double mouse clicks to put
#  listbox entry into the current directory entry window. Double also
#  accepts this as the value to use (as does <Return>).  Motion of a
#  depressed mouse-1 button changes the directory name.
      $Directbox bind list <Button-1> "
         $Directory clear 0 end
	 set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
	 $Directbox select clear 0 end
      "
      $Directbox bind list <B1-Motion> "
         $Directory clear 0 end
	 set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
	 $Directbox select clear 0 end
      "
      $Directbox bind list <Return> "
         $Directory clear 0 end
	 set index \[ lindex \[ %W curselection \] 0\]
         $Directory insert 0 \[ %W get \$index \]
	 $Directbox select clear 0 end
	 $Choice invoke Filter
      "
      $Directbox bind list <Double-Button-1> "
         $Directory clear 0 end
	 set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
	 $Directbox select clear 0 end
	 $Choice invoke Filter
      "

#  Filebox. Add binding of double mouse click to put listbox entry into the 
#  selected files window. <Return> adds the current selection.
      $Filebox bind list <Double-Button-1> "
         global CCDcurrentdirectory
         set index \[ %W nearest %y\]
         set filename \[ %W get \$index \]
         set directory \$CCDcurrentdirectory
         if { \$directory == \"/\" } { 
            $Selectbox insert end \"/\$filename\"
         } else {
            $Selectbox insert end \"\$directory/\$filename\"
         }
	 $Filebox select clear 0 end
      "
      $Filebox bind list <Return> "
         global CCDcurrentdirectory
	 set indices \[ %W curselection \]
         set directory \$CCDcurrentdirectory
         foreach i \"\$indices\" {
            set filename \[ %W get \$i \]
            if { \$directory == \"/\" } { 
               $Selectbox insert end \"/\$filename\"
            } else {
               $Selectbox insert end \"\$directory/\$filename\"
            }
         }
	 $Filebox select clear 0 end
      "

#  Options. Add buttons for controlling the insertion and removal of
#  file from the currently selected list.
      $Options addbutton {Add} \
	 "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set directory \"/\"
          } else {
             set directory \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filebox $Selectbox select \$directory
         "
      $Options addbutton {Add all} \
	 "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set directory \"/\"
          } else {
             set directory \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filebox $Selectbox range 0 end \$directory
         "
      $Options addbutton {Remove} "CCDRemoveFromList $Selectbox clear"
      $Options addbutton {Remove all} "$Selectbox clear 0 end"


#  Choice. Add OK, Filter and Cancel buttons. Filter button reads the
#  current directory and changes to it. It also enters a command in
#  the options to return to any directory we have visited.
      $Choice addcommand {OK} {set gotfilename true}
      $Choice addbutton {Filter} \
         "CCDGetFileUpdate $Filebox \
          $Directbox  \
          $Directory \
          $Filefilter
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] $Directory $Choice Filter
         "
      $Choice addcommand {Cancel} {set gotfilename false}

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
      pack $Menu -side top -fill x
      pack $Choice   -side bottom -fill x
      pack $Directory -side top -fill x
      pack $Filefilter -side top -fill x
      pack $Directbox -side left -fill both
      pack $Filebox   -side left -fill both
      pack $Options   -side left -fill both 
      pack $Selectbox -side left -expand true -fill both
      pack $Dirbase  -fill x
      pack $Listbase -expand true -fill both

#------------------------------------------------------------------------------
#  Activate interface restoring current values if they exist.
#------------------------------------------------------------------------------
#  Deal with the current filter (defaults to *)
      if { ![info exists CCDimportfilter] } { 
         set CCDimportfilter "*" 
      } elseif { $CCDimportfilter == "" } {
         set CCDimportfilter "*" 
      }
      $Filefilter insert 0 $CCDimportfilter

#  Deal with the current directory (defaults to [pwd])
      if { ![info exists CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      } elseif { $CCDcurrentdirectory == "" } {
         set CCDcurrentdirectory [pwd]
      }
      $Directory insert 0 $CCDcurrentdirectory

#  Invoke the filter button to get first setup.
      $Choice invoke Filter

#------------------------------------------------------------------------------
#  Wait for an list of filenames to be given or not as the case maybe.
#------------------------------------------------------------------------------
      CCDVariableWait gotfilename $Top $Directory entry
 
#  and now get the file names.
      if { $gotfilename == "true" } { 

#  and insert them all into a list.
         set CCDimportfiles [ CCDCreateListofNames $Selectbox {} {} ]
      } else {

#  No file given.
         set CCDimportfiles {}
      }

#  Set the current file filter.
      set CCDimportfilter [$Filefilter get]

#  Finall destroy the window before exit.
      $Top kill $Top

#  End of procedure
   }
# $Id$

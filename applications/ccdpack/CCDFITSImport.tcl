   proc CCDFITSImport {Top args } {
#+
#  Name:
#     CCDFITSImport

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Controls the import of FITS information into NDFs.

#  Description:
#     This routine controls the import of FITS information from the FITS
#     block of an NDF into the CCDPACK extension. The named NDFs are
#     FITS "imported" using a table which can be created or read from
#     an existing file. The NDFs names can be given using three methods,
#     by a "glob" expansion of an expression, or by browsing many
#     directories or by just using all the known NDFs.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window for this form.
#     args = list (read)
#        If present this should be a command to run if the NDF import
#        runs successfully (such as enabling commands for the next section).

#  Global variables:
#     CCDallndfs = list (read & write)
#        The names of all the known NDFs.
#     CCDimporttable = string (read & write)
#        The name of the import control table.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-APR-1994 (PDRAPER):
#     	 Original version.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     18-AUG-1995 (PDRAPER):
#        Recoded to new standards.
#     {enter_changes_here}

#-

#  Global variables:
      global env
      global CCDallndfs
      global CCDimportfiles
      global CCDimportexists
      global CCDimporttable
      global CCDndfimportfilter
      global CCDimagefilters
      global CCDcurrentdirectory
#.

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------

#  Top-level widget for this form.
      Ccd_toplevel $Top -title {Import FITS information into NDFs}

#  Menubar.
      set Menu [Ccd_helpmenubar $Top.menubar]

#  Labelled entry for name of import table.
      set Frame0 [frame $Top.frame0]
      set Table [Ccd_labent $Frame0.table -text {Import Control Table:} \
                    -textvariable CCDimporttable]

#  Frames for containing the current directory and file filters and the
#  scrollboxes for directories and files.
      set Frame1  [frame $Top.frame1]
      set Frame11 [frame $Frame1.frame1]

#  Labelled entry for current directory.
      set Directory [Ccd_labent $Frame11.direct -text {Directory:}]

#  Labelled entry for file filter
      if { [llength $CCDimagefilters] > 1 } { 
         set Filefilter [Ccd_option $Frame11.filter -text {File Filter:}]
      } else {
         set Filefilter [Ccd_labent $Frame11.filter -text {File Filter:}]
      }

#  List of directories scrollbox.
      set Directbox [Ccd_scrollbox $Frame1.directbox \
                        -singleselect 1 -label {Directories:} \
                        -exportselect 0]

#  List of files in current directory scrollbox.
      set Filebox [Ccd_scrollbox $Frame1.filebox \
                      -label {Files in directory:} -singleselect 0 \
                      -exportselect 0]

#  Selected files scrollbox.
      set Frame2 [frame $Top.frame2]
      set Selectbox [Ccd_scrollbox $Frame2.selectbox \
                        -label {Files selected:} -singleselect 0 \
                        -exportselect 0]

#  Options for adding and removing entries from the list of selected names.
      set Options [Ccd_choice $Frame1.options -standard 0 -stack vertical]


#  Add choice bar for control (OK, Cancel etc.).
      set Choice [Ccd_choice $Top.choice]

#-----------------------------------------------------------------------------
#  Widget configuration.
#-----------------------------------------------------------------------------

#  Menubar. Add file items to close or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add option to select import control table from existing files.
      $Menu addcommand Options \
         {Select import control table from existing files...}\
         "global CCDimportfile
          global CCDimportexists
	  global CCDimportfilter 
          set CCDimportexists 0
	  set CCDimportfilter \"*.DAT\"
          CCDGetFileName $Top.getnames {Select import control table}
          if { \$CCDimportexists } {
             $Table clear 0 end
             $Table insert end \$CCDimportfile
          }
         "

#  Add option to create or inspect a table.
      $Menu addcommand Options {Create/inspect import control table...} \
         "global CCDimportexists
          global CCDimportfile
          set CCDimportexists 0
          CCDCreateImportTable $Top.create \[$Table get\]
          if { \$CCDimportexists } {
             $Table clear 0 end
             $Table insert end \$CCDimportfile
          }
         "

#  Separate these from what follows.
      $Menu addseparator Options

#  Add home directory and current directory and any directories
#  already visited this (and possibly previous) session to Options.
      CCDRestoreDirectoryMenu \
         $Menu Options $Directory $Choice Filter
      CCDRecordDirectoryinMenu \
         $Menu Options $env(HOME) $Directory $Choice Filter
      CCDRecordDirectoryinMenu \
         $Menu Options [pwd] $Directory $Choice Filter

#  Directory. Add binding to move to the new directory when return is
#  pressed. This also adds a new command to Options menu to move back here.
      $Directory bind entry <Return> "$Choice invoke Filter; break"

#  Filefilter. Add binding to invoke the file filter button when
#  <Return> is pressed.
      $Filefilter bind entry <Return> "$Choice invoke Filter; break"

#  If needed add the options for the different foreign file formats.
      if { [llength $CCDimagefilters] > 1 } { 
         foreach pair "$CCDimagefilters" {
            set name [lindex $pair 0]
            set type [lindex $pair 1]
            $Filefilter addoption $name $type "$Choice invoke Filter"
         }
      }

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
      $Choice addcommand {OK} \
         "set size \[$Selectbox size\]
          if { \$size != 0 } {
             set CCDallndfs \[$Selectbox get all\]
             set table \[$Table get\]
             if { \$table != \"\" } {
                CCDFITSDoImport $Top \[$Table get\] $args
                $Top kill $Top
             } else {
                CCDIssueInfo {You must supply the name of an import control table}
             }
          } else {
             CCDIssueInfo {You must select at least one NDF}
          }
         "

      $Choice addbutton {Filter} \
         "CCDGetFileUpdate $Filebox \
          $Directbox  \
          $Directory \
          $Filefilter
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] $Directory $Choice Filter
         "

      $Choice addcommand {Cancel} "$Top kill $Top"

#------------------------------------------------------------------------------
#  Set help for window and widgets.
#------------------------------------------------------------------------------
      $Top sethelp ccdpack CCDFITSImportWindow
      $Menu sethelpitem {On Window} ccdpack CCDFITSImportWindow
      $Menu sethelp all ccdpack CCDFITSImportMenu
      $Directory sethelp ccdpack CCDFITSImportDirectory
      $Filefilter sethelp ccdpack CCDFITSImportFilter
      $Directbox sethelp ccdpack CCDFITSImportDirectories
      $Choice sethelp all ccdpack CCDFITSImportChoice

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
      pack $Menu       -side top -fill x
      pack $Choice     -side bottom -fill x

      pack $Frame11    -side top -fill x
      pack $Directory  -side top -fill x
      pack $Filefilter -side top -fill x
      pack $Directbox  -side left -fill both
      pack $Filebox    -side left -fill both
      pack $Options    -side left -fill both
      pack $Selectbox  -side top  -fill both -expand true
      pack $Table      -side top -fill x

      pack $Frame0     -side top -fill x
      pack $Frame1     -side left -fill both
      pack $Frame2     -side right -fill both -expand true


#------------------------------------------------------------------------------
#  Activate interface restoring current values if they exist.
#------------------------------------------------------------------------------
#  Deal with the current directory (defaults to [pwd])
      if { ![info exists CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      } elseif { $CCDcurrentdirectory == "" } {
         set CCDcurrentdirectory [pwd]
      } elseif { ![file isdirectory $CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      }
      $Directory insert 0 $CCDcurrentdirectory


#  If the CCDallndfs variable exists then populate the selected files
#  scrollbox with these names.
      if { [info exists CCDallndfs] && $CCDallndfs != "" } {
         eval $Selectbox insert end $CCDallndfs
      }

#  Invoke the filter button to get first setup.
      $Filefilter insert 0 [lindex [lindex $CCDimagefilters 0] 1]
      $Choice invoke Filter

#  Wait for interaction to end.
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$

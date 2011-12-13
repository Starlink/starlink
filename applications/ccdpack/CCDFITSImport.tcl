   proc CCDFITSImport {Top args } {
#+
#  Name:
#     CCDFITSImport

#  Purpose:
#     Controls the import of FITS information into NDFs.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

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

#  Global Variables:
#     CCDallndfs = list (read & write)
#        The names of all the known NDFs.
#     CCDimporttable = string (read & write)
#        The name of the import control table.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000-2001 Central Laboratory of the Research
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
#     23-APR-1994 (PDRAPER):
#        Original version.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     18-AUG-1995 (PDRAPER):
#        Recoded to new standards.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Modified the arguments of CCDGetFileName.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#        Fixed problem with use of default file filter.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global env
      global CCDallndfs
      global CCDimportfiles
      global CCDimportexists
      global CCDimporttable
      global CCDimagefilters
      global CCDcurrentdirectory
#.

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------

#  Top-level widget for this form.
      CCDCcdWidget Topwin topwin \
         Ccd::toplevel $Top -title "Import FITS information into NDFs"

#  Menubar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Topwin.menubar

#  Labelled entry for name of import table.
      CCDTkWidget Frame0 frame0 frame $topwin.frame0
      CCDCcdWidget Table table \
         Ccd::labent $Frame0.table -text "Import Control Table:" \
                     -textvariable CCDimporttable

#  Frames for containing the current directory and file filters and the
#  scrollboxes for directories and files.
      CCDTkWidget Frame1 frame1 frame $topwin.frame1
      CCDTkWidget Frame11 frame11 frame $frame1.frame1

#  Labelled entry for current directory.
      CCDCcdWidget Directory directory \
         Ccd::labent $Frame11.direct -text Directory:

#  Labelled entry for file filter
      if { [llength $CCDimagefilters] > 1 } {
         CCDCcdWidget Filefilter filefilter \
            Ccd::option $Frame11.filter -text "File Filter:"
      } else {
         CCDCcdWidget Filefilter filefilter \
            Ccd::labent $Frame11.filter -text "File Filter:"
      }

#  List of directories scrollbox.
      CCDCcdWidget Directbox directbox \
         Ccd::scrollbox $Frame1.directbox \
                        -singleselect 1 -label Directories: \
                        -exportselect 0

#  List of files in current directory scrollbox.
      CCDCcdWidget Filebox filebox \
         Ccd::scrollbox $Frame1.filebox \
                      -label "Images in directory:" -singleselect 0 \
                      -exportselect 0

#  Selected files scrollbox.
      CCDTkWidget Frame2 frame2 frame $topwin.frame2
      CCDCcdWidget Selectbox selectbox \
         Ccd::scrollbox $Frame2.selectbox \
                        -label "Images selected:" -singleselect 0 \
                        -exportselect 0

#  Options for adding and removing entries from the list of selected names.
      CCDCcdWidget Options options \
         Ccd::choice $Frame1.options -standard 0 -stack vertical


#  Add choice bar for control (OK, Cancel etc.).
      CCDCcdWidget Choice choice Ccd::choice $Topwin.choice

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
          CCDGetFileName $Topwin.getnames {Select import control table} 0
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
          CCDCreateImportTable $Topwin.create \[$Table get\]
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
         set dir \$CCDcurrentdirectory
         if { \$dir == \"/\" } {
            $Selectbox insert end \"/\$filename\"
         } else {
            $Selectbox insert end \"\$dir/\$filename\"
         }
	 $Filebox select clear 0 end
      "
      $Filebox bind list <Return> "
         global CCDcurrentdirectory
	 set indices \[ %W curselection \]
         set dir \$CCDcurrentdirectory
         foreach i \"\$indices\" {
            set filename \[ %W get \$i \]
            if { \$dir == \"/\" } {
               $Selectbox insert end \"/\$filename\"
            } else {
               $Selectbox insert end \"\$dir/\$filename\"
            }
         }
	 $Filebox select clear 0 end
      "

#  Options. Add buttons for controlling the insertion and removal of
#  file from the currently selected list.
      $Options addbutton {Add} \
	 "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set dir \"/\"
          } else {
             set dir \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filebox $Selectbox select \$dir
         "
      $Options addbutton {Add all} \
	 "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set dir \"/\"
          } else {
             set dir \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filebox $Selectbox range 0 end \$dir
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
             set tab \[$Table get\]
             if { \$tab != \"\" } {
                CCDFITSDoImport $Topwin \[$Table get\] $args
                $Topwin kill $Topwin
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
          $Filefilter \
          1
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] $Directory $Choice Filter
         "

      $Choice addcommand {Cancel} "$Topwin kill $Topwin"

#------------------------------------------------------------------------------
#  Set help for window and widgets.
#------------------------------------------------------------------------------
      $Topwin sethelp ccdpack CCDFITSImportWindow
      $Menu sethelpitem {On Window} ccdpack CCDFITSImportWindow
      $Menu sethelp all ccdpack CCDFITSImportMenu
      $Directory sethelp ccdpack CCDFITSImportDirectory
      $Filefilter sethelp ccdpack CCDFITSImportFilter
      $Directbox sethelp ccdpack CCDFITSImportDirectories
      $Choice sethelp all ccdpack CCDFITSImportChoice

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
      pack $menu       -side top -fill x
      pack $choice     -side bottom -fill x

      pack $frame11    -side top -fill x
      pack $directory  -side top -fill x
      pack $filefilter -side top -fill x
      pack $directbox  -side left -fill both
      pack $filebox    -side left -fill both
      pack $options    -side left -fill both
      pack $selectbox  -side top  -fill both -expand true
      pack $table      -side top -fill x

      pack $frame0     -side top -fill x
      pack $frame1     -side left -fill both
      pack $frame2     -side right -fill both -expand true


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
      if { [llength $CCDimagefilters] > 1 } {
         $Filefilter insert 0 [lindex [lindex $CCDimagefilters 0] 1]
      } else {
         $Filefilter insert 0 "$CCDimagefilters"
      }
      $Choice invoke Filter

#  Wait for interaction to end.
      CCDWindowWait $Topwin

#  End of procedure.
   }
# $Id$

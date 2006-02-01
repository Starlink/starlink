   proc CCDSetDetector { Topwin } {
#+
#  Name:
#     CCDSetDetector

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Sets the CCDPACK interface to reduce data from a known detector.

#  Description:
#     This routine is used to select from a known list of
#     telescopes/detector combinations and configures the xreduce
#     interface appropriately. The configurations are either
#     CCDSETUP-like files (which describe the CCD characteristics) or
#     an FITS Import Control Table. The two are differentiated by
#     their contents (CCDSETUP-like files have word = statements).
#
#     These files must have a data type .DAT and be stored either in
#     the directory $CCDPACK_DIR(global CCDdir)  or the directory
#     $CCDPACK_CONFIG. The CCDPACK_CONFIG variable is intended for
#     use by users.

#  Global variables:
#     CCDdetectorcache = string (read)
#       The name of any directories to search for tables and setups.

#  Arguments:
#     Top = window (read)
#        Name of the top-level window for this form.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     14-SEP-1995 (PDRAPER):
#     	 Original version.
#     13-MAY-1999 (PDRAPER):
#        Modified width of toplevel window.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#-

#  Global variables.
      global env
      global CCDdir
      global CCDdetectorcache
      global DETectortype
#.

#-----------------------------------------------------------------------------
#  Widget creation.
#-----------------------------------------------------------------------------

#  Top-level widget.
      CCDCcdWidget Top top \
         Ccd::toplevel $Topwin -title "Choose known detector setup"
      wm geometry $top 50x10

#  Menubar
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Descriptive label.
      CCDTkWidget Label label \
         label $top.label -anchor center \
          -text "Choose from the known \n detector/telescope combinations"
      CCDTkWidget S1 s1 frame $top.s1 -height 3

#  Multitem for names and descriptions.
      CCDCcdWidget Box box \
         Ccd::multitem $Top.box -nboxes 2 -singleselect 1 -seealltext 0

#  Labelled entry for name of selected file.
      CCDCcdWidget Name name Ccd::labent $Top.labent -text {File:}

#  Choice bar for control.
      CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 1

#-----------------------------------------------------------------------------
#  Widget configuration.
#-----------------------------------------------------------------------------

#  Menu.
#  File items to cancel or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add option to view selected file contents.
      $Menu addcommand Options {View file contents...} \
         "set file \[$Name get\]
          if { \$file != \"\" } {
             CCDViewFile $Top \$file
          } else {
             CCDIssueInfo {No file is currently selected}
          }
         "

#  Box.
#  Set the box labels.
      $Box label 1 "File:"
      $Box label 2 "Description:"

#  Selection by one mouse click shows name of file. Double click does
#  the same and also exits. <Return> chooses the currently selected option.
      $Box bind <Button-1> \
         "+ set index \[%W nearest %y\]
          set line \[$Box get \$index\]
          set file \[lindex \$line 0\]
          set desc \[lindex \$line 1\]
          $Name clear 0 end
          $Name insert 0 \$file
          global DETectortype
          if { \[string match \"*(setup)*\" \$desc\] } {
             set DETectortype setup
          } else {
             set DETectortype table
          }
         "
      $Box bind <ButtonRelease-1> \
         "+ set index \[%W nearest %y\]
          set line \[$Box get \$index\]
          set file \[lindex \$line 0\]
          set desc \[lindex \$line 1\]
          $Name clear 0 end
          $Name insert 0 \$file
          global DETectortype
          if { \[string match \"*(setup)*\" \$desc\] } {
             set DETectortype setup
          } else {
             set DETectortype table
          }
         "
      $Box bind <Double-Button-1> \
         "+ set index \[%W nearest %y\]
          set line \[$Box get \$index\]
          set file \[lindex \$line 0\]
          set desc \[lindex \$line 1\]
          $Name clear 0 end
          $Name insert 0 \$file
          global DETectortype
          if { \[string match \"*(setup)*\" \$desc\] } {
             set DETectortype setup
          } else {
             set DETectortype table
          }
          $Choice invoke OK
         "
      $Box bind <Return> \
         "+ set index \[ lindex \[ %W curselection \] 0\]
          set line \[$Box get \$index\]
          set file \[lindex \$line 0\]
          set desc \[lindex \$line 1\]
          $Name clear 0 end
          $Name insert 0 \$file
          global DETectortype
          if { \[string match \"*(setup)*\" \$desc\] } {
             set DETectortype setup
          } else {
             set DETectortype table
          }
          $Choice invoke OK
         "

#  Choice bar. OK interprets the file (either setting it as an
#  IMPORT TABLE or reading the global values).
      $Choice addcommand OK \
         "global DETectortype
          global CCDimporttable
          set file \[$Name get\]
          if { \$file != \"\" } {
             if { \$DETectortype == \"setup\" } {
                CCDReadRestoreFile \$file
             } else {
                set CCDimporttable \$file
             }

#  Before exiting check for any warnings and re-issue them.
             set line \[lindex \[$Box get \[$Box curselection\]\] 1\]
             if { \$line != {} } {
                if {\[regexp -nocase {warning(\[^\(\]*)} \$line match message\]} {
                   CCDIssueInfo \
                \"Remember to act on the following warning \\\"\$message\\\"\"
                }
             }
             $Top kill $Top
          } else {
             CCDIssueInfo \"No detector selected\"
          }
         "
      $Choice addcommand Cancel "$Top kill $Top"

#-----------------------------------------------------------------------------
#  Associate help
#-----------------------------------------------------------------------------
      $Menu sethelpitem {On Window} ccdpack CCDSetDetectorWindow
      $Menu sethelp all ccdpack CCDSetDetectorMenu
      $Top sethelp ccdpack CCDSetDetectorWindow

#-----------------------------------------------------------------------------
#  Packing
#-----------------------------------------------------------------------------
      pack $menu -side top -fill x
      pack $choice -side bottom -fill x
      pack $label -side top -fill x
      pack $s1 -side top -fill x
      pack $box -side top -fill both -expand true
      pack $name -side top -fill x

#-----------------------------------------------------------------------------
#  Activate interface
#-----------------------------------------------------------------------------

#  Look for files of type .DAT in the available directories. If
#  available scan them to determine their type and look for a description.
      if { ! [CCDScanDetectorFiles $Box] } {
         CCDIssueInfo "No detectors are known to this system"
      }

#  Wait for interaction to finish.
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$

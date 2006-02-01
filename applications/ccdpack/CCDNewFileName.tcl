   proc CCDNewFileName { Topwin title } {
#+
#  Name:
#     CCDNewFileName

#  Purpose:
#     Gets a name for a new file.

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine displays an entry box to get a name for a new file.
#     If required the names of currently existing files can be examined
#     using the CCDGetFileName procedure. Typically this procedure will
#     be used when getting a name for a file which is expected not to
#     exist.

#  Arguments:
#     Topwin = window (read)
#        The name of the top-level window to contain form.
#     title = string (read)
#        The title for top-level widget.

#  Global variables:
#     CCDimportfile = filename (write)
#        The name of the new file.
#     CCDimportavail = logical (write)
#        If a name has been given then this is true, otherwise it if
#	 false (0).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     7-MAR-1994 (PDRAPER):
#     	 Original version.
#     21-APR-1994 (PDRAPER):
#     	 Now uses mega-widgets.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     21-AUG-1995 (PDRAPER):
#        Converted to new coding style.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Modified the arguments of CCDGetFileName.
#     1-JAN-2006 (PDRAPER):
#        Fixed entry widget width to 20 characters.
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDimportfile
      global CCDimportavail

      global CCDimportexists
      global CCDnewfilename
#.

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------
#  Top-level window for form.
      CCDCcdWidget Top top Ccd::toplevel $Topwin -title "$title"

#  Menubar. This will allow selection from existing files and supply
#  help.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Labelled entry widget for getting name of file.
      CCDCcdWidget Name name Ccd::labent $Top.labent \
         -text {Filename:} -width 30

#  Choice bar for control.
      CCDCcdWidget Choice choice Ccd::choice $Top.choice

#----------------------------------------------------------------------------
#  Widget configuration.
#----------------------------------------------------------------------------

#  Menu. 
#  File items to cancel or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add command to Options menu to select from existing files.
      $Menu addcommand {Options} {Select from existing files...} \
         "CCDGetFileName $Top.getfile \"Select from existing files\" 0
             if { \$CCDimportexists } {
                $Name clear 0 end
                $Name insert 0 \$CCDimportfile
             }
         "

#  Name. Add binding so that <Return> invokes OK choice.
      $Name bind entry <Return> "$Choice invoke OK"

#  Add commands to standard buttons in choice bar.  OK button, this
#  sets CCDnewfilename when invoked.
      $Choice addcommand OK "set CCDnewfilename 1"

#  Cancel button.
      $Choice addcommand Cancel "set CCDnewfilename 0"

#--------------------------------------------------------------------------
#  Set the help.
#--------------------------------------------------------------------------
      $Top sethelp ccdpack CCDNewFileNameWindow
      $Menu sethelpitem {On Window} ccdpack CCDNewFileNameWindow
      $Menu sethelp all ccdpack CCDNewFileNameMenu

#--------------------------------------------------------------------------
#  Packing.
#--------------------------------------------------------------------------
      pack $menu -fill x
      pack $name -expand true -fill x
      pack $choice -fill x

#--------------------------------------------------------------------------
#  Real-time setup and wait for response section.
#--------------------------------------------------------------------------
#  Set value of entry window to old global value if one exists. Make the
#  entry widget large enough to see all name.
      if { [info exists CCDimportfile] } {
         $Name insert 0 $CCDimportfile
         set namelen [ string length $CCDimportfile ]
         set curlen [ $Name cget -width ]
	 if { $namelen > $curlen } {
            $Name configure -width $namelen
         }
      }

#  Wait for an file name to be given or not as the case maybe.
      CCDVariableWait CCDnewfilename $Top $Name entry

#  Get the name.
      if { $CCDnewfilename } {
         set CCDimportfile [$Name get]
         set CCDimportavail 1

#  No file given.
      } else {
         set CCDimportfile ""
         set CCDimportavail 0
      }

#  And destroy the window before proceeding.
      $Top kill $Top

#  End of procedure.
   }
# $Id$

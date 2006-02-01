   proc CCDViewFile { Topwin file } {
#+
#  Name:
#     CCDViewFile

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Display the contents of a file.

#  Description:
#     This procedure displays the contents of a file in a text widget.
#     The text widget appears in its own window which can be removed
#     when finished.

#  Arguments:
#     Topwin = window (read)
#        Name of a top-level window to parent this window. A uniquish
#        name is generated for the actual form so that many instances
#        of this may be created.
#     file = filename (read)
#        Name of the file whose contents are to be viewed.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     14-SEP-1995 (PDRAPER):
#     	 Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#-

#  Global variables.
      global Viewcount
#.

#  Initialise Viewcount if not already set.
      if { ! [info exists Viewcount] } {
         set Viewcount 0
      }

#  Check file exists.
      if { ! [file readable $file] } {
         CCDIssueInfo "Cannot read file $file"
         return
      }

#-----------------------------------------------------------------------------
#  Widget creation
#-----------------------------------------------------------------------------

#  Toplevel
      set Thiswin $Topwin.view$Viewcount
      incr Viewcount
      CCDCcdWidget Thisw thisw \
         Ccd::toplevel $Thiswin -title "Contents of file $file"

#  Text widget
      CCDCcdWidget Text text Ccd::scrolltext $Thisw.text

#  Choice bar to remove window.
      CCDCcdWidget Choice choice Ccd::choice $Thisw.choice -standard 0

#-----------------------------------------------------------------------------
#  Widget configuration
#-----------------------------------------------------------------------------
      $Choice addbutton {Remove from screen} "$Thisw kill $Thisw"

#-----------------------------------------------------------------------------
#  Packing
#-----------------------------------------------------------------------------
      pack $text -side top -fill both -expand true
      pack $choice -fill x

#-----------------------------------------------------------------------------
#  Interface activation.
#-----------------------------------------------------------------------------

#  Read the file and insert its contents
      set fileid [open $file r]
      while { ! [eof $fileid] } {
         $Text insert end "[gets $fileid]\n"
      }

#  End of procedure.
   }
# $Id$

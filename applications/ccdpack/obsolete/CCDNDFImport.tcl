   proc CCDNDFImport topwin {
#+
#  Name:
#     CCDNDFImport

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Controls the import of NDFs.

#  Description:
#     This procedure defines a form for selecting amongst the options
#     for "importing" NDFs into the automated reduction X-Windows
#     interface of CCDPACK (xreduce). The options are organize the NDFs
#     into types etc. by hand, adding exposures times and filters.
#     or import NDFs using FITS information. Each type of import also
#     allows the import of global parameters into the NDFs.

#  Arguments:
#     topwin = window (read)
#        The name of the toplevel widget for this form.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-APR-1994 (PDRAPER):
#     	 Original version.
#     9-MAY-1994 (PDRAPER):
#     	 Removed parameter import from this level.
#     23-MAR-1995 (PDRAPER):
#        Added help.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global parameters:
      global CCDdir
#.

#  Create the top-level object.
      Ccd_toplevel $topwin -title "Import NDFs"

#  Add a menubar.
      Ccd_helpmenubar $topwin.menubar -standard 0
      $topwin.menubar sethelp {On Window} ccdpack CCDNDFImport 

#  Add the buttons to a choice bar for selecting amongst the various
#  options.
      Ccd_choice $topwin.choice -standard 0

#  Add a message briefly explaining the options at this time.
      frame $topwin.mess
      label $topwin.mess.bitl -bitmap @$CCDdir/ccdbitmap64
      label $topwin.mess.bitr -bitmap @$CCDdir/ccdbitmap64
      message $topwin.mess.text -justify center -width 15c \
         -text { 
Import NDFs into package
Use:
"Organize" to organize NDFs into types by hand
"FITS Import" to import FITS information into NDFs
"Exit" to exit from this section
}

#  Button to organize the NDFs into types by hand. This also allows the
#  sorting by filter type and the addition of any exposure times (for
#  dark and flash frames).
      $topwin.choice addbutton \
         {Organize} \
         "$topwin kill $topwin
	  CCDNDFOrganize $topwin
	 "

#  Import FITS information into the extensions of the NDFs. This also
#  allows the interactive creation of import control tables.
      $topwin.choice addbutton \
         {FITS import} \
         "$topwin kill $topwin
	  CCDFITSImport $topwin
	 "

#  And Finished with this section.
      $topwin.choice addbutton \
         {Exit} \
         "$topwin kill $topwin"

#  Pack all widgets.
      pack $topwin.menubar -fill x
      pack $topwin.choice -side bottom -fill both -expand true
      pack $topwin.mess.bitl -side left
      pack $topwin.mess.text -side left -fill both -expand true
      pack $topwin.mess.bitr -side left
      pack $topwin.mess -fill both -expand true

#  Wait for interaction to finish.
      CCDWindowWait $topwin $topwin.choice Organize

#  End of procedure.
   }
# $Id$

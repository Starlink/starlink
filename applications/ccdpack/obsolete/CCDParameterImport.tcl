   proc CCDParameterImport { topwin box } {
#+
#  Name:
#     CCDParameterImport

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Controls the import of global parameters into NDFs.

#  Description:
#     This procedure shows a form which lists the NDFs present in the
#     named list/scrollbox ($box). The option is then given to remove
#     any of the NDFs. On exit the application PRESENT is run to import
#     the current global preferences into the NDFs. This is used from
#     procedure such as CCDFITSImport.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-APR-1994 (PDRAPER):
#     	 Original version.
#     10-MAY-1994 (PDRAPER):
#     	 Just does what is necessary.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global parameters:
      global CCDprefs
      global CCDallndfs
#.

#  Create top-level window for form.
      Ccd_toplevel $topwin -title "Import global parameter information into NDFs"

#  Add menubar with no options.
      Ccd_menubar $topwin.menubar -standard 0
      $topwin.menubar addbutton {Help} {0}

#  Description label.
      frame $topwin.descrip
      label $topwin.descrip.label \
         -text "Import global parameters into NDFs" \
	 -anchor center \
	 -borderwidth 2
      pack  $topwin.descrip.label -fill x

#  Remove button.
      Ccd_choice $topwin.control -standard 0
      $topwin.control addbutton \
         {Remove} \
	 "CCDRemoveFromList $topwin.scrollbox clear"

#  Create scrollable listbox for NDF names.
      Ccd_scrollbox $topwin.scrollbox -singleselect 0 -exportselect 0

#  Execute and Exit buttons.
      Ccd_choice $topwin.choice -standard 0
      $topwin.choice addbutton  {Import} "CCDFITSRunPresent $topwin
                                          $topwin kill $topwin"
      $topwin.choice addbutton {Exit} "$topwin kill $topwin"

#  Pack all widgets into top-level.
      pack $topwin.menubar -fill x
      pack $topwin.choice  -side bottom -fill x
      pack $topwin.descrip -fill x
      pack $topwin.control -fill x
      pack $topwin.scrollbox -expand true -fill both

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Initialize NDF listbox from names in listbox given.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      set size [ $box size ]
      for { set i 0 } { $i < $size } { incr i } {
         $topwin.scrollbox insert end [ $box get $i ]
      }

#  Wait until interaction ends.
      CCDWindowWait $topwin $topwin.control Remove

#  End of procedure.
   }
# $Id$

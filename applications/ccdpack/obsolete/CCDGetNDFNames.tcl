   proc CCDGetNDFNames { topwin title description element gvar } {
#+
#  Name:
#     CCDGetNDFNames

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Gets a list of NDF names.

#  Description
#     This routine gets a list of NDF names from the user. These may be
#     specified using either a list of glob-style expressions
#     (separated by spaces or commas) or by initiating a directory
#     search using a separate form for selecting files from various
#     directories (see procedure CCDGetFileNames). The end result is a
#     list of NDF names, which are returned as a (Tcl) list in the
#     global array element CCDndfs($element). If no NDFs names are
#     specified then CCDndfs($element) is not created. Normally
#     "$element" would be the name of the type of NDFs, something like "bias",
#     "target" etc.

#  Arguments:
#     topwin = window (read)
#        The top-level widget for this form.
#     title = string (read)
#        Title for the title bar created by window manager.
#     description = string (read)
#        A description of the type of NDFs to be selected. This appears
#	 in a label widget below the menubar.
#     element = string (read)
#        The name of the element of the global array CCDndfs which will
#	 contain the (Tcl) list of NDF names returned from the user. If
#	 no NDFs are specified then the element is not created.
#     gvar = string (read)
#        The name of a global variable with which to monitor the
#        condition of this topwindow. On entry this should be set 1
#        (true) and on exit it will be set 0 (false).

#   Global variables:
#      CCDndfs = array (write)
#         An array of lists of NDF names. The NDFs selected by this
#	  procedure are written to element CCDndfs($element).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     21-APR-1994 (PDRAPER):
#     	 Nows uses mega-widgets.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDndfs
      global CCDprefs            ;# CCDPACK widget preferences
      global CCDNDFsel$element   ;# Selection pattern for NDFs
      global $gvar               ;# Window state variable
#.

#  Top-level widget for this form.
      Ccd_toplevel $topwin -title "$title"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Menubar
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_menubar $topwin.menubar

#  Add command to options to allow selection from various directories etc.
      $topwin.menubar addcommand Options \
         {Select using directory browser...} \
         "global CCDimportfiles
	  CCDGetFileNames $topwin.c.getnames {Select NDFs}
          if { \$CCDimportfiles != {} } {
             eval $topwin.c.scrollbox insert end \$CCDimportfiles
             set CCDimportfiles {}
          }
         "

#  Add frame for central parts.
      frame $topwin.c

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Description label
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      frame $topwin.c.descrip
      label $topwin.c.descrip.label \
         -text "$description" \
	 -anchor center \
	 -borderwidth 2 \
	 -font $CCDprefs(italic_font)
      pack  $topwin.c.descrip.label -fill x

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Labelled entry widget for NDF names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if { ! [ info exists CCDNDFsel$element ] } {
         set CCDNDFsel$element "*.sdf"
      } else {

#  Make sure this isn't an array.
         catch { eval set temp \$CCDNDFsel$element }
	 unset CCDNDFsel$element
         if { [ info exists temp ] } {
            set CCDNDFsel$element $temp
         } else {
            set CCDNDFsel$element "*.sdf"
	 }
      }
      Ccd_labent $topwin.c.labent \
         -text {Input NDFs:} \
	 -textvariable CCDNDFsel$element

#  Bind <Return> to invoke the add button to enter the current list of names.
      $topwin.c.labent bind entry <Return> "$topwin.c.control invoke Add"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add and remove buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.c.control -standard 0
      $topwin.c.control addbutton \
         {Add} \
	 "CCDInsertNDFList $topwin.c.labent $topwin.c.scrollbox"

      $topwin.c.control addbutton \
         {Remove all} \
	 "$topwin.c.scrollbox clear 0 end"

      $topwin.c.control addbutton \
         {Remove} \
	 "CCDRemoveFromList $topwin.c.scrollbox clear"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create scrollable listbox for NDF names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_scrollbox $topwin.c.scrollbox -singleselect 0 -exportselect 0

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  OK and Cancel buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.bottom

#  Ok button writes global list of NDF names and destroys top-level window.
      $topwin.bottom addcommand OK \
	 "set CCDndfs($element)  \[ CCDCreateListofNames \
	                              $topwin.c.scrollbox {} {} \]
          global $gvar
          set $gvar 0
          $topwin kill $topwin
         "

#  Cancel set global list of names null and destroys top-level window.
      $topwin.bottom addcommand Cancel \
	 "global $gvar
          set $gvar 0
          $topwin kill $topwin
         "

#  Pack all widgets into top-level.
      pack $topwin.menubar -fill x
      pack $topwin.c -fill both -expand true
      pack $topwin.c.descrip -fill x
      pack $topwin.c.labent  -fill x
      pack $topwin.c.control -fill x
      pack $topwin.c.scrollbox -expand true -fill both
      pack $topwin.bottom -side bottom -fill x

#  Make sure we have a focus to work from in this window.
      $topwin.c.labent focus entry

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Re-initialize NDF listbox from any existing names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Check state of CCDndfs.
      if { ![ info exists CCDndfs($element) ] } {
         set CCDndfs($element) {}
      } else {
         eval $topwin.c.scrollbox insert end $CCDndfs($element)
      }

#  End of procedure.
   }
# $Id$

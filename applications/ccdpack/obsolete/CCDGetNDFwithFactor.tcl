   proc CCDGetNDFwithFactor { topwin title description element gvar } { 
#+
#  Name:
#     CCDGetNDFwithFactor

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Gets a list of NDF names with associated factors.

#  Description
#     This routine gets a list of NDF names and a list of associated
#     (numeric) factors from the user. The NDF names may be specified
#     using either a list of glob-style expressions (separated by
#     spaces or commas) or by initiating a directory search using a
#     separate form for selecting files from various directories (see
#     procedure CCDGetFileNames). The factors associated with NDFs may
#     be set by specifying them at the same time as the NDF names
#     (using the entry widget for factors) or may be modified by
#     selecting from the list of names (which enters the selected NDF
#     name into the NDF entry) and adding a factor.  The end result is
#     a list of NDF names and a list of factors, which are returned as
#     a (Tcl) list in the global array element CCDndfs($element) and
#     CCDfactors($element). If no NDFs names are specified then
#     CCDndfs($element) is not created. Normally "$element" would be
#     the name of the type of NDFs, something like "dark" or "flash".

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

#   Global variables:
#      CCDndfs = array (write)
#         An array of lists of NDF names. The NDFs selected by this
#	  procedure are written to element CCDndfs($element).
#      CCDfactors = array (write)
#	  An array of lists of factors associated with NDFs. The
#	  factors produced by this procedure are written to element
#	  CCDfactors($element).
#      gvar = boolean (write)
#         The name of a global variable which will be set to 0  when
#	  this procedure exists (via OK or Cancel)

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     10-MAR-1994 (PDRAPER):
#     	 Changed from CCDGetNDFnames to now also get an associated factor.
#     4-MAY-1994 (PDRAPER):
#     	 Changed to use mega-widgets.
#     10-MAY-1994 (PDRAPER):
#     	 Added gvar
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDndfs
      global CCDfactors
      global CCDprefs            \# CCDPACK widget preferences
      global CCDNDFsel$element    \# Selection pattern for NDFs
      global CCDexpose$element    \# Current exposure factors
      global $gvar
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
	  CCDGetFileNames $topwin.getnames {Select NDFs}
          if { \$CCDimportfiles != {} } { 

#  Need to make up some exposure factors for these NDFs so use 1.0.
             foreach ndf \$CCDimportfiles {
                $topwin.scrollbox insert end \$ndf 1.0
	     }
             set CCDimportfiles {}
          }
         "

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Description label
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      frame $topwin.descrip 
      label $topwin.descrip.label \
         -text "$description" \
	 -anchor center \
	 -borderwidth 2 \
	 -font $CCDprefs(italic_font)
      pack  $topwin.descrip.label -fill x

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Labelled entry widget for NDF names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if { ! [ info exists CCDNDFsel$element ] } {
         set CCDNDFsel$element "*.sdf"
      } else {

#  Make sure this isn't an array.
         catch { set temp $CCDNDFsel$element }
	 unset CCDNDFsel$element
         if { [ info exists temp ] } { 
            set CCDNDFsel$element $temp
         } else {
            set CCDNDFsel$element "*.sdf"
	 }
      }
      Ccd_labent $topwin.ndfent \
         -text {Input NDFs:} \
	 -textvariable CCDNDFsel$element

#  Bind <Return> to move onto the next entry widget.
      $topwin.ndfent bind entry <Return> "$topwin.expent focus entry"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Labelled entry widget for exposure factors.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_labent $topwin.expent \
         -text {Exposures:} \
	 -textvariable CCDexpose$element

#  Return here inserts the NDFs and factors.
      $topwin.expent bind entry <Return> "$topwin.control invoke Add"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add and remove buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.control -standard 0

#  Add button needs to expand the NDF names and add their exposure factors.
      $topwin.control addbutton \
         {Add} \
	 "CCDInsertNDFswithFactors \
	    $topwin.ndfent \
	    $topwin.scrollbox \
	    $topwin.expent
	 "

      $topwin.control addbutton \
         {Remove all} \
	 "$topwin.scrollbox clear 0 end"

      $topwin.control addbutton \
         {Remove} \
	 "CCDRemoveFromList $topwin.scrollbox clear"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create scrollable listbox for NDF names and factors.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_multitem $topwin.scrollbox -singleselect 0

#  Enable double click <1> to read the current item and insert it into
#  the NDF name and exposure factor entrys.
      $topwin.scrollbox bind <Double-1> "
          set index \[ %W nearest %y \]
          set item \[ $topwin.scrollbox get \$index \]
          $topwin.scrollbox clear \$index
          $topwin.ndfent clear 0 end
          $topwin.ndfent insert 0 \[ lindex \$item 0 \] 
          $topwin.expent clear 0 end
          $topwin.expent insert 0  \[ lindex \$item 1 \] 
         "

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  OK and Cancel buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.bottom

#  Ok button writes global list of NDF names and a list of factors. It
#  then destroys the top-level window and all associated objects.
      $topwin.bottom addcommand OK \
         "set nndfs \[ $topwin.scrollbox size \]
	  for { set i 0 } { \$i < \$nndfs } { incr i } { 
             set item \[ $topwin.scrollbox get \$i \]
             lappend CCDndfs($element) \[ lindex \$item 0 \]
             lappend CCDfactors($element) \[ lindex \$item 1 \]
          }
          $topwin kill $topwin
          global $gvar
	  set $gvar 0
         "

#  Cancel set global list of names null and destroys top-level window.
      $topwin.bottom addcommand Cancel \
	 "$topwin kill $topwin
	  unset CCDndfs($element)
	  unset CCDfactors($element)
          global $gvar
	  set $gvar 0
	 "

#  Pack all widgets into top-level.
      pack $topwin.menubar -fill x
      pack $topwin.bottom -side bottom -fill x
      pack $topwin.descrip -fill x
      pack $topwin.ndfent  -fill x
      pack $topwin.expent  -fill x
      pack $topwin.control -fill x
      pack $topwin.scrollbox -expand true -fill both

#  Make sure we have a focus to work from in this window.
      $topwin.ndfent focus entry

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Re-initialize NDF listbox from any existing names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Check state of CCDndfs.
      if { [ info exists CCDndfs($element) ] &&
           [ info exists CCDfactors($element) ] } {
         set nndfs [ llength $CCDndfs($element) ]
	 for { set i 0 } { $i < $nndfs } { incr i } { 
            set ndf [ lindex $CCDndfs($element) $i ]
	    set factor [ lindex $CCDfactors($element) $i ] 
            $topwin.scrollbox insert end $ndf $factor
         }
      } else {
         set CCDndfs($element) {}
         set CCDfactors($element) {}
      }

#  End of procedure.
   }
# $Id$

   proc CCDGetNDFColourNames { topwin title description element gvar } {
#+
#  Name:
#     CCDGetNDFColourNames

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Gets lists of colour related NDF names.

#  Description
#     This routine gets several lists of NDF names (which may also be 
#     associated with exposure factors) from the user. NDF names may be 
#     specified using either a list of glob-style expressions (separated 
#     by spaces or commas) or by initiating a directory search using a 
#     separate form for selecting files from various directories (see 
#     procedure CCDGetFileNames). Each of the lists managed by this procedure 
#     are related to a filter colour. Only one of the lists is visible at any 
#     time to save on display space, the user selects the list to view at any 
#     time by selecting the filter from a list of radiobuttons. The end result 
#     are several lists of NDF names, with associated factors if required, 
#     which are returned as a (Tcl) list in the global array element 
#     CCDndfs($element,$filter), and CCDfactors($element,$filter,$type). If 
#     no NDFs names are specified then the variables are not created. Normally 
#     "$element" would be the name of the type of NDFs one of "target", 
#     "flatfield" etc. The filternames in use are named in the global array 
#     CCDfilternames.
#
#     The extra factors are only required if the global variables 
#     CCDsame(darks) or CCDsame(flashes) are false.

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
#	 no NDFs are specified then the element is not created. The name
#	 of the relevant filter is also used as an index.
#     gvar = string (read)
#        The name of a global variable with which to monitor the
#        condition of this topwindow. On entry this should be set 1
#        (true) and on exit it will be set 0 (false).

#   Global variables:
#      CCDndfs = array (write)
#         An array of lists of NDF names. The NDFs selected by this
#	  procedure are written to element CCDndfs($element,$filter).
#      CCDfactors = array (write)
#         If required this array will contain lists of exposure
#         factors for dark counts or pre-flash. The indices are
#         ($element,$filter,darks) and ($element,$filter,flashes) if used.
#      CCDfilternames = string (read)
#         The names of the filters.
#      CCDsame = array (read)
#         This array's elements are (darks) and (flashes) if these are
#         false then extra widgets are created to allow the entry of
#         values for the dark and/or pre-flash time.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     21-APR-1994 (PDRAPER):
#     	 Nows uses mega-widgets.
#     28-APR-1994 (PDRAPER):
#     	 Added filter capabilities.
#     24-MAY-1994 (PDRAPER):
#        Now asks for dark and flash exposures.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDndfs             \# NDFs in listboxes
      global CCDsame             \# Exposure times are same
      global CCDfactors          \# exposure factors
      global CCDprefs            \# CCDPACK widget preferences
      global CCDfilternames      \# Known filternames
      global CCDcurrentfilter    \# Current filter selection
      global CCDNDFsel${element} \# Selection pattern for NDFs
      global CCDNDFselda         \# Selection pattern for dark exposures
      global CCDNDFselfa         \# Selection pattern for flash exposures
      global $gvar               \# Window state global variable 
#.

#  Top-level widget for this form.
      Ccd_toplevel $topwin -title "$title"

#  Create dummy entry according existence of extra factors.
      set dummy {}
      if { !$CCDsame(darks) } { lappend dummy 1 }
      if { !$CCDsame(flashes) } { lappend dummy 1 }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Menubar
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_menubar $topwin.menubar

#  Add command to options to allow selection from various directories etc.
      $topwin.menubar addcommand Options \
         {Select using directory browser...} \
	 "global CCDimportfiles
          global CCDndfs
          global CCDfactors
          CCDGetFileNames $topwin.getnames {Select NDFs}
          if { \$CCDimportfiles != {} } { 
             foreach item \$CCDimportfiles {
                eval $topwin.scrollbox insert end \$item $dummy
             }
             set CCDimportfiles {}
          }
          CCDUpdateColourLists $element $topwin.scrollbox
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
#  Radioarray of known filters.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Each button set the global array CCDcurrentfilter to its filter name
#  when set and clears the scrollbox. The contents of the array CCDndfs
#  for this filter and element combination is then entered into the
#  scrollbox and the entry widget is reset to the value of the NDF
#  pattern that is used by this scrollbox.
      if { ! [ info exists CCDcurrentfilter ] } { 
         set CCDcurrentfilter [ lindex $CCDfilternames 0 ]
      }
      Ccd_radioarray $topwin.filters \
         -label {Filters:} \
	 -stack horizontal \
	 -minwidth 0 \
	 -variable CCDcurrentfilter
      foreach filter $CCDfilternames {
         $topwin.filters addbutton \
	    $filter $filter \
            "CCDChangeFilter \
                $topwin.scrollbox \
                $topwin.labent \
                $topwin.darkent \
                $topwin.flashent $element"
      }

#  Set a trace to change the state of listbox if the global variable
#  CCDcurrentfilter is changed outside this routine.
      trace variable CCDcurrentfilter w \
	 "CCDChangeFilter \
             $topwin.scrollbox \
             $topwin.labent \
             $topwin.darkent \
             $topwin.flashent $element"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Labelled entry widget for NDF names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Need to create a global variable for each filter to hold the NDF
#   pattern selection. If this variable has been used we may need to
#   re-initialise it to be an array.
      if { [ info exists CCDNDFsel$element ] } {
         if { [ catch { array size CCDNDFsel$element } ] } {

#  Isn't an array, unset it, and re-initialise
            unset CCDNDFsel$element
            foreach filter $CCDfilternames {
               set CCDNDFsel${element}($filter) "*.sdf"
            }
         } else {

#  Exists and is an array, initialise any unknown filters.
            foreach filter $CCDfilternames {
               if { ! [ info exists CCDNDFsel${element}($filter) ] } { 
                  set CCDNDFsel${element}($filter) "*.sdf"
               }
            }
	 }
      } else {

#  Doesn't exist so initialise it.
         foreach filter $CCDfilternames {
            set CCDNDFsel${element}($filter) "*.sdf"
         }
      }
      Ccd_labent $topwin.labent \
         -text {Input NDFs:} \
         -textvariable CCDNDFsel${element}($CCDcurrentfilter)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add labelled entry widgets for any dark and pre-flash exposures.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Initial counter for number of boxes required.
      set nboxes 0
      set extranames {}

#  See if a dark time is required for each NDF.
      if { ! $CCDsame(darks) } {

#  Initialise for each filter type which isn't already known.
         foreach filter $CCDfilternames {
            if { ! [ info exists CCDNDFselda($filter) ] } {
	       set CCDNDFselda($filter) {}
	    }
	 }
	 Ccd_labent $topwin.darkent \
	    -text {Dark count exposure:} \
	    -textvariable CCDNDFselda($CCDcurrentfilter)
         lappend extranames $topwin.darkent
         incr nboxes
      }

#  See if a pre-flash time is required for each NDF.
      if { ! $CCDsame(flashes) } {

#  Initialise for each filter type which isn't already known.
         foreach filter $CCDfilternames {
            if { ! [ info exists CCDNDFselfa($filter) ] } {
	       set CCDNDFselfa($filter) {}
	    }
	 }
	 Ccd_labent $topwin.flashent \
	    -text {Pre-flash exposure:} \
	    -textvariable CCDNDFselfa($CCDcurrentfilter)
         incr nboxes
         lappend extranames $topwin.flashent
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Control movement between the entry boxes.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Bind <Return> to invoke the add button to enter the current list of
#  names eventually.
      if { $CCDsame(darks) && $CCDsame(flashes) } { 
	 $topwin.labent bind entry <Return> "$topwin.control invoke Add"
      } else {
         if { $CCDsame(flashes) } { 
	    $topwin.labent bind entry <Return> "$topwin.darkent focus entry"
	    $topwin.darkent bind entry <Return> "$topwin.control invoke Add"
         } else {
            if { $CCDsame(darks) } { 
	       $topwin.labent bind entry <Return> "$topwin.flashent focus entry"
	       $topwin.flashent bind entry <Return> "$topwin.control invoke Add"
            } else { 
	       $topwin.labent bind entry <Return> "$topwin.darkent focus entry"
	       $topwin.darkent bind entry <Return> "$topwin.flashent focus entry"
	       $topwin.flashent bind entry <Return> "$topwin.control invoke Add"
	    }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add and remove buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add button interprets the NDF list pattern and the factors (if
#  used) and inserts the results of this into the end of the listbox. 
#  The listbox contents are then appended to the global variables.
      Ccd_choice $topwin.control -standard 0
      $topwin.control addbutton \
         {Add} \
	 "CCDInsertNDFswithFactors $topwin.labent $topwin.scrollbox $extranames
          CCDUpdateColourLists $element $topwin.scrollbox
         "

#  Remove buttons, clears the listbox of its current contents (or just the
#  current selection) and clears the global variable of the removed contents.
      $topwin.control addbutton \
         {Remove all} \
	 "global CCDcurrentfilter
	  global CCDndfs
          global CCDfactors
	  $topwin.scrollbox clear 0 end
          unset CCDndfs($element,\$CCDcurrentfilter)
          if { ! $CCDsame(darks) } { 
             unset CCDfactors($element,\$CCDcurrentfilter,darks)
          }
          if { ! $CCDsame(flashes) } { 
             unset CCDfactors($element,\$CCDcurrentfilter,flashes)
          }
	 "

      $topwin.control addbutton \
         {Remove} \
	 "CCDRemoveFromList $topwin.scrollbox clear
          CCDUpdateColourLists $element $topwin.scrollbox
	 "

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create scrollable listbox(es) for NDF names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if { ! $nboxes } { 
         Ccd_scrollbox $topwin.scrollbox -singleselect 0 -exportselect 0
      } else {
         incr nboxes
         Ccd_multitem $topwin.scrollbox \
	    -singleselect 0 \
	    -exportselect 0 \
	    -nboxes $nboxes
         incr nboxes -1
      }

#  Enable double click <1> and <Return> to read the current item and insert it into
#  the NDF name and other entry boxes. (Note bind commands differ slightly hence 
#  the duplications).
      if { ! $nboxes } { 
	 $topwin.scrollbox bind list <Double-1> "
            set index \[ %W nearest %y \]
            set item \[ $topwin.scrollbox get \$index \]
            $topwin.scrollbox clear \$index
            $topwin.labent clear 0 end
            $topwin.labent insert 0 \[ lindex \$item 0 \] 
            set index 1
            if { ! $CCDsame(darks) } {
               $topwin.darkent clear 0 end
               $topwin.darkent insert 0  \[ lindex \$item \$index \] 
               incr index
            }
            if { ! $CCDsame(flashes) } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateColourLists $element $topwin.scrollbox
           "
	 $topwin.scrollbox bind list <Return> "
            set index \[ %W nearest %y \]
            set item \[ $topwin.scrollbox get \$index \]
            $topwin.scrollbox clear \$index
            $topwin.labent clear 0 end
            $topwin.labent insert 0 \[ lindex \$item 0 \] 
            set index 1
            if { ! $CCDsame(darks) } {
               $topwin.darkent clear 0 end
               $topwin.darkent insert 0  \[ lindex \$item \$index \] 
               incr index
            }
            if { ! $CCDsame(flashes) } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateColourLists $element $topwin.scrollbox
           "
      } else {
	 $topwin.scrollbox bind <Double-1> "
            set index \[ %W nearest %y \]
            set item \[ $topwin.scrollbox get \$index \]
            $topwin.scrollbox clear \$index
            $topwin.labent clear 0 end
            $topwin.labent insert 0 \[ lindex \$item 0 \] 
            set index 1
            if { ! $CCDsame(darks) } {
               $topwin.darkent clear 0 end
               $topwin.darkent insert 0  \[ lindex \$item \$index \] 
               incr index
            }
            if { ! $CCDsame(flashes) } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateColourLists $element $topwin.scrollbox
           "
	 $topwin.scrollbox bind <Return> "
            set index \[ %W nearest %y \]
            set item \[ $topwin.scrollbox get \$index \]
            $topwin.scrollbox clear \$index
            $topwin.labent clear 0 end
            $topwin.labent insert 0 \[ lindex \$item 0 \] 
            set index 1
            if { ! $CCDsame(darks) } {
               $topwin.darkent clear 0 end
               $topwin.darkent insert 0  \[ lindex \$item \$index \] 
               incr index
            }
            if { ! $CCDsame(flashes) } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateColourLists $element $topwin.scrollbox
           "
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  OK and Cancel buttons.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.bottom

#  Ok destroys top-level window.
      $topwin.bottom addcommand OK \
	 "global $gvar
          set $gvar 0
          $topwin kill $topwin"

#  Cancel set global list of names null and destroys top-level window.
      $topwin.bottom addcommand Cancel \
	 "global CCDndfs
          global CCDfilternames
          global $gvar
          foreach filter \$CCDfilternames {
             catch { unset CCDndfs($element,\$filter) }
          }
          $topwin kill $topwin
          set $gvar 0
         "

#  Pack all widgets into top-level.
      pack $topwin.menubar -fill x
      pack $topwin.descrip -fill x
      pack $topwin.filters -fill x
      pack $topwin.labent  -fill x
      if { ! $CCDsame(darks) } { pack $topwin.darkent -fill x }
      if { ! $CCDsame(flashes) } { pack $topwin.flashent -fill x }
      pack $topwin.control -fill x
      pack $topwin.scrollbox -expand true -fill both
      pack $topwin.bottom -side bottom -fill x

#  Make sure we have a focus to work from in this window.
      $topwin.labent focus entry

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Re-initialize the listboxes from any existing names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Check state of CCDndfs.
      if { ! [ info exists CCDndfs($element,$CCDcurrentfilter) ] } {
         set CCDndfs($element,$CCDcurrentfilter) {}
         if { ! $CCDsame(darks) } {
            set CCDfactors($element,$CCDcurrentfilter,darks) {}
         }
         if { ! $CCDsame(flashes) } {
            set CCDfactors($element,$CCDcurrentfilter,flashes) {}
         }
      } else {

#  Quick insertion if no darks and/or flashes are required. Otherwise 
#  need to look at each element in turn.
         if { $CCDsame(darks) && $CCDsame(flashes) } { 
	    $topwin.scrollbox insert end $CCDndfs($element,$CCDcurrentfilter) 
         } else { 
            set i 0 
            foreach item $CCDndfs($element,$CCDcurrentfilter) {
	       if { ! $CCDsame(darks) } {
		  if {[info exists CCDfactors($element,$CCDcurrentfilter,darks)]} {
		     lappend item \
			[ lindex $CCDfactors($element,$CCDcurrentfilter,darks) $i ]
		  } else { 
		     lappend item 1
		  }
	       } 
	       if { ! $CCDsame(flashes) } { 
		  if {[info exists CCDfactors($element,$CCDcurrentfilter,flashes)]} {
		     lappend item \
			[ lindex $CCDfactors($element,$CCDcurrentfilter,flashes) $i ]
		  } else { 
		     lappend item 1
		  }
	       }
	       eval $topwin.scrollbox insert end $item
               incr i
	    }
	 }
      }

#  End of procedure.
   }
# $Id$

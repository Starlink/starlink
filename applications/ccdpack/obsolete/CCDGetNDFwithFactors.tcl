   proc CCDGetNDFwithFactors { topwin title description element gvar } {
#+
#  Name:
#     CCDGetNDFwithFactors

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Gets lists of NDF names and the necessary factors

#  Description
#     This routine gets a lists of NDF names, which may also be associated 
#     with exposure factors. NDF names may be  specified using either a 
#     list of glob-style expressions (separated by spaces or commas) or by 
#     initiating a directory search using a separate form for selecting files 
#     from various directories (see procedure CCDGetFileNames). Each of the 
#     lists manag. The end result is a list of NDF names with associated 
#     factors if required, which are returned as a (Tcl) list in the global 
#     array element CCDndfs($element), and CCDfactors($element,$type). 
#     If no NDFs names are specified then the variables are not created. 
#     Normally "$element" would be the name of the type of NDFs one of 
#     "dark" or "flash'. 
#
#     The extra factors are only required if the global variables 
#     CCDsame(darks) or CCDsame(flashes) are false. If CCDsame(darks)
#     is false then a series of dark exposures are required. If
#     CCDsame(flashes) if false then pre-flash exposures are required
#     as are (if CCDsame(darks) is false) the dark-times for these exposures.

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
#      CCDfactors = array (write)
#         If required this array will contain lists of exposure
#         factors for dark counts or pre-flash. The indices are
#         ($element,darks) and ($element,flashes) if used.
#      CCDsame = array (read)
#         This array's elements are (darks) and (flashes) if these are
#         false then extra widgets are created to allow the entry of
#         values for the dark and/or pre-flash time.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-MAY-1994 (PDRAPER):
#     	 Original version based on CCDGetNDFColourNames.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDndfs             \# NDFs in listboxes
      global CCDsame             \# Exposure times are same
      global CCDfactors          \# exposure factors
      global CCDprefs            \# CCDPACK widget preferences
      global CCDNDFsel${element} \# Selection pattern for NDFs
      global CCDNDFseld          \# Selection pattern for dark exposures
      global CCDNDFself          \# Selection pattern for flash exposures
      global $gvar               \# Window state variable
#.

#  Top-level widget for this form.
      Ccd_toplevel $topwin -title "$title"

#  Create dummy entry according existence of extra factors.
      set dummy {}
      if { !$CCDsame(darks) } { 
	 lappend dummy 1
      }
      if { !$CCDsame(flashes) && "$element" == "flashes" } { 
	 lappend dummy 1
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Menubar
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_menubar $topwin.menubar

#  Add command to options to allow selection from various directories etc.
      $topwin.menubar addcommand Options \
         {Select using directory browser...} \
	 "global CCDimportfiles
          CCDGetFileNames $topwin.getnames {Select NDFs}
          if { \$CCDimportfiles != {} } { 
             foreach item \$CCDimportfiles {
                eval $topwin.scrollbox insert end \$item $dummy
             }
             set CCDimportfiles {}
          }
          CCDUpdateFactorLists $element $topwin.scrollbox
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
#   Need to create a global variable to hold the NDF pattern selection. 
      if { ! [ info exists CCDNDFsel$element ] } {
         set CCDNDFsel${element} "*.sdf"
      }
      Ccd_labent $topwin.labent \
         -text {Input NDFs:} \
         -textvariable CCDNDFsel${element}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add labelled entry widgets for any dark and pre-flash exposures.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Initial counter for number of boxes required.
      set nboxes 0
      set extranames {}
      set havedarks 0
      set haveflashes 0

#  See if a dark time is required for each NDF.
      if { ! $CCDsame(darks) } {

#  Initialise pattern selection.
         if { ! [ info exists CCDNDFseld ] } {
	    set CCDNDFseld {}
	 }
	 Ccd_labent $topwin.darkent \
	    -text {Dark count exposure:} \
	    -textvariable CCDNDFseld
         lappend extranames $topwin.darkent
         incr nboxes
         set havedarks 1
      }

#  See if a pre-flash time is required for each NDF.
      if { "$element" == "flashes" && ! $CCDsame(flashes) } {

	 #  Initialise pattern selection.
	 if { ! [ info exists CCDNDFself ] } {
	    set CCDNDFself {}
	 }
	 Ccd_labent $topwin.flashent \
	    -text {Pre-flash exposure:} \
	    -textvariable CCDNDFself
         incr nboxes
         lappend extranames $topwin.flashent
         set haveflashes 1
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Control movement between the entry boxes.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Bind <Return> to invoke the add button to enter the current list of
#  names eventually.
      if { $havedarks && $haveflashes } { 
	 $topwin.labent bind entry <Return> "$topwin.darkent focus entry"
	 $topwin.darkent bind entry <Return> "$topwin.flashent focus entry"
	 $topwin.flashent bind entry <Return> "$topwin.control invoke Add"
      } else { 
         if { $havedarks } { 
	    $topwin.labent bind entry <Return> "$topwin.darkent focus entry"
	    $topwin.darkent bind entry <Return> "$topwin.control invoke Add"
	 } else { 
            if { $haveflashes } { 
	       $topwin.labent bind entry <Return> "$topwin.flashent focus entry"
	       $topwin.flashent bind entry <Return> "$topwin.control invoke Add"
            } else { 
	       $topwin.labent bind entry <Return> "$topwin.control invoke Add"
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
          CCDUpdateFactorLists $element $topwin.scrollbox
	 "

#  Remove buttons, clears the listbox of its current contents (or just the
#  current selection) and clears the global variable of the removed contents.
      $topwin.control addbutton \
         {Remove all} \
	 "global CCDndfs
          global CCDfactors
	  $topwin.scrollbox clear 0 end
          unset CCDndfs($element)
          if { ! $CCDsame(darks) } { 
             unset CCDfactors($element,darks)
          }
          if { ! $CCDsame(flashes) && \"$element\" == \"flashes\" } { 
             unset CCDfactors($element,flashes)
          }
	 "

      $topwin.control addbutton \
         {Remove} \
	 "CCDRemoveFromList $topwin.scrollbox clear
          CCDUpdateFactorLists $element $topwin.scrollbox
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
            if { ! $CCDsame(flashes) && \"$element\" == \"flashes\" } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateFactorLists $element $topwin.scrollbox
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
            if { ! $CCDsame(flashes) && \"$element\" == \"flashes\" } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateFactorLists $element $topwin.scrollbox
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
            if { ! $CCDsame(flashes) && \"$element\" == \"flashes\" } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateFactorLists $element $topwin.scrollbox
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
            if { ! $CCDsame(flashes) && \"$element\" == \"flashes\" } {
               $topwin.flashent clear 0 end
               $topwin.flashent insert 0  \[ lindex \$item \$index \] 
            }
            CCDUpdateFactorLists $element $topwin.scrollbox
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
         $topwin kill $topwin
        "

#  Cancel set global list of names null and destroys top-level window.
      $topwin.bottom addcommand Cancel \
      "global CCDndfs
       global $gvar
       set $gvar 0
       catch { 
          unset CCDndfs($element) 
          unset CCDfactors($element,darks)
          unset CCDfactors($element,flashes)
       }
       $topwin kill $topwin
      "

#  Pack all widgets into top-level.
      pack $topwin.menubar -fill x
      pack $topwin.descrip -fill x
      pack $topwin.labent  -fill x
      if { ! $CCDsame(darks) } { pack $topwin.darkent -fill x }
      if { ! $CCDsame(flashes) && "$element" == "flashes" } { 
	 pack $topwin.flashent -fill x 
      }
      pack $topwin.control -fill x
      pack $topwin.scrollbox -expand true -fill both
      pack $topwin.bottom -side bottom -fill x

#  Make sure we have a focus to work from in this window.
      $topwin.labent focus entry

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Re-initialize the listboxes from any existing names.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Check state of CCDndfs.
      if { ! [ info exists CCDndfs($element) ] } {
         set CCDndfs($element) {}
         if { ! $CCDsame(darks) } {
            set CCDfactors($element,darks) {}
         }
         if { ! $CCDsame(flashes) && "$element" == "flashes" } {
            set CCDfactors($element,flashes) {}
         }
      } else {

#  Quick insertion if no darks and/or flashes are required. Otherwise 
#  need to look at each element in turn.
         if { $CCDsame(darks) && $CCDsame(flashes) } { 
	    $topwin.scrollbox insert end $CCDndfs($element) 	 
	 } else {
	    set i 0 
	    foreach item $CCDndfs($element) {
	       if { ! $CCDsame(darks) } {
		  if {[info exists CCDfactors($element,darks)]} {
		     lappend item \
			[ lindex $CCDfactors($element,darks) $i ]
		  } else { 
		     lappend item 1
		  }
	       } 
	       if { ! $CCDsame(flashes) && "$element" == "flashes" } { 
		  if {[info exists CCDfactors($element,flashes)]} {
		     lappend item \
			[ lindex $CCDfactors($element,flashes) $i ]
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

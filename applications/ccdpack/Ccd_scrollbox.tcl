   itcl_class Ccd_scrollbox {

#+
#  Name:
#     Ccd_scrollbox

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#    Defines a class of "listbox with scrollbars".

#  Description:
#    This class description defines methods and configurations for
#    creating a listbox with scrollbars a "scrollbox". The scrollbox may
#    have scrollbars positioned either at the top or bottom and left or
#    right of the listbox. The scrollbars are arranged to give a
#    Motif-like look (with spaces in the corners) and may be
#    reconfigured at any time.

#  Invocations:
#
#        Ccd_scrollbox window [-option value]...
#
#     This command create an instance of a scrollbox and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -scrollbarplaces "place1 place2"
#
#     This option configures the placing of the scrollbars. These may
#     be any combination of orthogonal pairs of "left, right" and "top,
#     bottom" or any one or even none of these. The default is "right
#     bottom"
#
#        -singleselect boolean
#
#     This option configures the listbox bindings so that only one
#     item may be selected in the listbox at any time.
#
#        -exportselect boolean
#
#     This option configues the listbox so that the selection is the X11
#     selection or not. If a selection is to be made in more than one
#     place then this will require setting to false.
#
#        -height  value
#
#     The height of the listbox. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#        -width value
#
#     The width of the listbox. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#         -label "text"
#
#     Adds a label over at top of the scrollbox.
#
#         -anchor place
#
#     Sets -anchor for the label, default is w.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the scrollbox widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the scrollbox, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     insert index text
#        Inserts a line of text with the given index. "index" can
#	 be 0 or end which inserts at the beginning and at the end.
#     clear first [last]
#        Clears a range of items from the listbox. If first is "all"
#	 then all lines are deleted. If only first is given then this
#	 clears a single line. "last" may be set as end.
#     get first [last]
#        If last is not given then the element with index $first is
#        returned, unless first is "all" in which case all the
#        elements are returned. If last is given then the values
#        between the range are returned.
#     sethelp document label.
#        Sets the context sensitive help information for the
#        scrollbox.  $document should be the name of a HTML document
#        and $label the label (HTML anchor) within the document that
#        identifies the part to be displayed.
#     size
#        Returns the size of the listbox.
#     _repack place1 place2
#        Repacks the scrollbox (used by configuration option
#	 scrollbarplaces). This is really an internal method and
#	 shouldn't be used.
#     listname
#        Returns the name of the listbox widget.
#     scrollbarnames places
#        Returns the names of any scrollbars at the given places ("left"
#	 "right", "top" or "bottom").
#     select option args
#        Controls the selection in the listbox. "option" is any of those
#	 which are valid for a listbox, args are the qualifiers.
#     curselection
#        Returns a list of the indices of any items selected in the
#	 listbox.

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.


#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-MAR-1994 (PDRAPER):
#     	 Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed. Listboxes do not support -geometry.
#     5-MAY-1995 (PDRAPER):
#        Now uses Tk4 select modes.
#     21-JUL-1995 (PDRAPER):
#        Added sethelp method.
#     {enter_further_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd_scrollbox class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create listbox.
         listbox $oldthis.list

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($oldthis:list) $oldthis.list
         set widgetfocus($oldthis:list) $oldthis.list

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set wid [ _getoption "width Width"]
         if { $wid != {} } { set width $wid }
         set hei [ _getoption "height Height"]
         if { $hei != {} } { set relief $hei }
         set scr [ _getoption \
            "scrollbarplaces scrollbarPlaces ScrollbarPlaces ScrollBarPlaces"]
         if { $scr != {} } { set scrollbarplaces $scr }
         set sin [ _getoption "singleselect singleSelect SingleSelect"]
         if { $sin != {} } { set singleselect $sin }

#  Set default configurations.
         configure -height          $height
         configure -width           $width
         configure -anchor          $anchor
         configure -scrollbarplaces $scrollbarplaces
         configure -singleselect    $singleselect

#  Pack listbox
         pack $oldthis.list -expand true -fill both
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert line of text method.
      method insert { index args } {
         eval $oldthis.list insert $index $args
      }

#  Clear range of lines of text method.
      method clear { args } {
         if { [lindex $args 0 ] != "all" } {
            eval $oldthis.list delete $args
         } else {
            $oldthis.list delete 0 end
         }
      }

#  Return the range of lines with current selection.
      method curselection {} {
         return [$oldthis.list curselection]
      }

#  Get information back from the listbox.
      method get { args } {
         if { [lindex $args 0 ] == "all" } {
            set value [$oldthis.list get 0 end]
            return $value
         } else {
            set value [$oldthis.list get $args]
            return $value
         }
      }

#  Method for assigning context help to all the elements of the object.
      method sethelp {docname label} {
         if $exists {

#  Rather than work out which configuration setting we're using, just
#  extract all the possible widget names and check that they exist.
            foreach oneof [array names widgetnames "$oldthis:*"] {
               set name $widgetnames($oneof)
               if [winfo exists $name] {
                  Ccd_base::sethelp $name $docname $label
               }
            }
         }
      }

#  Size of listbox contents.
      method size {} {
         return [$oldthis.list size]
      }

#  Internal method for creating and or re-packing scrollbars
      method _repack places  {

#  First remove all scrollbars and packing frames, make the listbox
#  forget any scrolling commands.
         foreach side "left right" {
            if { [ winfo exists $oldthis.scroll$side ] } {
               pack forget $oldthis.scroll$side

#  Destroy the scrollbar and make the listbox forget about the scrollcommand.
               destroy $oldthis.scroll$side
               $oldthis.list configure -yscrollcommand {}
            }
         }
         foreach side "top bottom" {
            if { [ winfo exists $oldthis.$side.scroll$side ] } {
               pack forget $oldthis.$side.scroll$side

#  Destroy the scrollbar and make the listbox forget about the scrollcommand.
               destroy $oldthis.$side.scroll$side
               destroy $oldthis.$side
               $oldthis.list configure -xscrollcommand {}
            }

#  Destroy any packing frames.
            if { [ winfo exists $oldthis.$side.left ] } {
               pack forget $oldthis.$side.left
               destroy $oldthis.$side.left
            }
            if { [ winfo exists $oldthis.$side.right ] } {
               pack forget $oldthis.$side.right
               destroy $oldthis.$side.right
            }
         }

#  And unpack the listbox itself (so that rearrangement is easy).
         pack forget $oldthis.list

#  Find out if any packing frames are required (this fill the corners of
#  the base frame so that scrollbars look natural).
         set haveleft   [string match *left*   $places]
         set haveright  [string match *right*  $places]
         set havetop    [string match *top*    $places]
         set havebottom [string match *bottom* $places]

#  Create the necessary scrollbars. Append any names etc. to the
#  sub-widget control variables.
         if { $haveleft } {
            scrollbar $oldthis.scrollleft \
               -orient vertical \
               -command "$oldthis.list yview"
            $oldthis.list configure -yscrollcommand "$oldthis.scrollleft set"
            set widgetnames($oldthis:scrollleft) $oldthis.scrollleft
         }

         if { $haveright } {
            scrollbar $oldthis.scrollright \
               -orient vertical \
               -command "$oldthis.list yview"
            $oldthis.list configure -yscrollcommand "$oldthis.scrollright set"
            set widgetnames($oldthis:scrollright) $oldthis.scrollright
         }

         if { $havetop } {
            frame $oldthis.top -borderwidth 0
            if { $haveleft } {
               frame $oldthis.top.left -width [winfo reqwidth $oldthis.scrollleft]
               pack $oldthis.top.left -side left
            }
            scrollbar $oldthis.top.scrolltop \
               -orient horizontal \
               -command "$oldthis.list xview"
            pack $oldthis.top.scrolltop -side left -fill x -expand true
            $oldthis.list configure -xscrollcommand "$oldthis.top.scrolltop set"
            set widgetnames($oldthis:scrolltop) $oldthis.top.scrolltop
            if { $haveright } {
               frame $oldthis.top.right -width [winfo reqwidth $oldthis.scrollright]
               pack $oldthis.top.right -side left
            }
         }

         if { $havebottom } {
            frame $oldthis.bottom -borderwidth 0
            if { $haveleft } {
               frame $oldthis.bottom.left -width [winfo reqwidth $oldthis.scrollleft]
               pack $oldthis.bottom.left -side left
            }
            scrollbar $oldthis.bottom.scrollbottom \
               -orient horizontal \
               -command "$oldthis.list xview"
            pack $oldthis.bottom.scrollbottom -side left -fill x -expand true
            $oldthis.list configure -xscrollcommand "$oldthis.bottom.scrollbottom set"
            set widgetnames($oldthis:scrollbottom) $oldthis.top.scrollbottom
            if { $haveright } {
               frame $oldthis.bottom.right -width [winfo reqwidth $oldthis.scrollright]
               pack $oldthis.bottom.right -side left
            }
         }

#  Perform packing of main elements (need to do this now to get into
#  correct places.
         if { $havetop }    { pack $oldthis.top         -side top    -fill x }
         if { $havebottom } { pack $oldthis.bottom      -side bottom -fill x }
         if { $haveleft }   { pack $oldthis.scrollleft  -side left   -fill y }
         if { $haveright }  { pack $oldthis.scrollright -side right  -fill y }
         pack $oldthis.list -expand true -fill both
      }

#  Method to return the name of the listbox widget.
      method listname {} {
         return $oldthis.list
      }

#  Method to control selection. This has the same options as the listbox
#  selection.
      method select { option args } {
         eval $oldthis.list selection $option $args
      }

#  Method to return the names of the scrollbar widgets
      method scrollbarnames { places } {
         foreach side $places {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }
         foreach side $places {
            if { [ winfo exists $oldthis.scroll$side ] } {
               lappend barnames $oldthis.scroll$side
            }
         }
         if { [ info exists barnames ] } {
            return "$barnames"
         } else {
            return {}
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert and pack the required scrollbars. Remove any existing
#  scrollbars first.
      public scrollbarplaces { right bottom } {
         foreach side $scrollbarplaces {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }

#  Only proceed if the object exists (this means that constructor has
#  been invoked).
         if $exists {
            _repack $scrollbarplaces
         }
      }

#  If a label has been requested then add one.
      public label {} {
         if { $label != {} } {
            if $exists {
               label $oldthis.label -text "$label"
               pack $oldthis.label -side top -anchor $anchor
               set widgetnames($oldthis:label) $oldthis.label
               set widgetfocus($oldthis:label) $oldthis.label
               _repack $scrollbarplaces
            }
         } else {

#  Remove existing label.
            if { [ winfo exists $oldthis.label ] } {
               pack forget $oldthis.label
	       destroy $oldthis.label
               unset widgetnames($oldthis:label)
               unset widgetfocus($oldthis:label)
	    }
         }
      }

#  Set anchor position of the label.
      public anchor w {
         configure -label $label
      }

#  Can more than one entry be selected at a time?
      public singleselect 1 {
         if $exists {
            if { $singleselect } {
               $oldthis.list configure -selectmode browse
            } else {
               $oldthis.list configure -selectmode extended
            }
         }
      }

#  Is the selection exportable? If not may have more than one selection
#  (one for each instance), otherwise the selection is the X11 one.
      public exportselect 1 {
         if $exists {
            $oldthis.list configure -exportselection $exportselect
         }
      }

#  Height of listbox.
      public height 10 {
         if $exists {
            $oldthis.list configure -height $height
         }
      }

#  Width of listbox.
      public width 20 {
         if $exists {
            $oldthis.list configure -width $width
         }
      }

#  End of class defintion.
   }

# $Id$

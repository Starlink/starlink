   itcl_class Ccd_scrolltext {

#+
#  Name:
#     Ccd_scrolltext

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#    Defines a class of "text widget with scrollbars".

#  Description:
#    This class description defines methods and configurations for
#    creating a text widget with scrollbars. The scrolltext widget may
#    have scrollbars positioned either at the left or right, top or bottom. 
#    The scrollbars are arranged to give a Motif-like look (with spaces in 
#    the corners) and may be reconfigured at any time.

#  Invocations:
#
#        Ccd_scrolltext window [-option value]...
#
#     This command create an instance of a scrolltext and returns a
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
#        -scrollbarplaces "(right|left)" "(top|bottom)"
#
#     This option configures the placing of the scrollbars. These may
#     be either "left" or "right" or "top" or bottom. The default is 
#     "right bottom"
#
#        -exportselect boolean
#
#     This option configues the text widget so that the selection is the X11
#     selection or not. If a selection is to be made in more than one
#     place then this will require setting to false.
#
#        -height  value
#
#     The height of the text widget. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#        -width value
#
#     The width of the text widget. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#         -label "text"
#
#     Adds a label over at top of the text widget. This is anchored west.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the scrolltext widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the scrolltext, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     insert index text
#        Inserts a line of text with the given index. "index" can
#	 be 0 or end which inserts at the beginning and at the end.
#     clear first [last]
#        Clears a range of items from the text widget. If first is "all"
#	 then all lines are deleted. If only first is given then this
#	 clears a single line. "last" may be set as end.
#     get index1 [index2]
#        Gets the item with the given indices from the text widget.
#     _repack place
#        Repacks the scrolltext (used by configuration option
#	 scrollbarplace). This is really an internal method and
#	 shouldn't be used.
#     textname
#        Returns the name of the text widget.
#     scrollbarnames
#        Returns the name of the scrollbars.

#  Inheritance:
#     This class inherits Ccd_base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-MAR-1995 (PDRAPER):
#     	 Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     11-AUG-1995 (PDRAPER):
#        Added option for horizontal scrollbar (Tk 4 enhancement).
#     {enter_further_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd_scrolltext class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create a frame widget. This must have the same name as the class
#  command.
         Ccd_base::constructor

#  Create text widget.
         text $oldthis.text

#  Define sub-component widgets for configuration via the wconfig
#  and focus method.
         set widgetnames($oldthis:text) $oldthis.text
         set widgetfocus($oldthis:text) $oldthis.text

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set wid [ _getoption "width Width"]
         if { $wid != {} } { set width $wid }
         set hei [ _getoption "height Height"]
         if { $hei != {} } { set relief $hei }
         set scr [ _getoption \
            "scrollbarplaces scrollbarPlaces ScrollbarPlaces ScrollBarPlaces"]
         if { $scr != {} } { set scrollbarplace $scr }

#  Set default configurations. Scrollbar placements also packs the widgets.
         configure -height          $height
         configure -width           $width
         configure -label           $label
         configure -scrollbarplaces $scrollbarplaces
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert line of text method.
      method insert { index args } {
         eval $oldthis.text insert $index $args
	 
#  Make sure that the text is visible.
         set textlen [expr int([$oldthis.text index end])]
         $oldthis.text see $index
#         update idletasks
      }
      
#  Clear range of lines of text method.
      method clear { args } {
         if { [lindex $args 0 ] != "all" } {
            eval $oldthis.text delete $args
         } else {
            $oldthis.text delete 0 end
         }
      }

#  Get information back from the text widget.
      method get { index } {
         set contents ""
         if { $index != "all" } {
            set contents [$oldthis.text get $index]
         } else {
            set size [$oldthis.text size]
            for { set i 0 } { $i < $size } { incr i } {
               lappend contents [$oldthis.text get $i]
            }
         }
         return $contents
      }

#  Internal method for creating and or re-packing scrollbars
      method _repack places  {

#  First remove all scrollbars and packing frames, make the text widget
#  forget any scrolling commands.
         foreach side "left right" {
            if { [ winfo exists $oldthis.scroll$side ] } {
               pack forget $oldthis.scroll$side
               destroy $oldthis.scroll$side
               $oldthis.text configure -yscrollcommand {}
            }
         }
         foreach side "top bottom" {
            if { [ winfo exists $oldthis.$side.scroll$side ] } {
               pack forget $oldthis.$side.scroll$side
               destroy $oldthis.$side.scroll$side
               destroy $oldthis.$side
               $oldthis.text configure -xscrollcommand {}
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

#  And unpack the text widget itself (so that rearrangement is easy).
         pack forget $oldthis.text

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
               -command "$oldthis.text yview"
            $oldthis.text configure -yscrollcommand "$oldthis.scrollleft set"
            set widgetnames($oldthis:scrollleft) $oldthis.scrollleft
         }

         if { $haveright } {
            scrollbar $oldthis.scrollright \
               -orient vertical \
               -command "$oldthis.text yview"
            $oldthis.text configure -yscrollcommand "$oldthis.scrollright set"
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
               -command "$oldthis.text xview"
            pack $oldthis.top.scrolltop -side left -fill x -expand true
            $oldthis.text configure -xscrollcommand "$oldthis.top.scrolltop set"
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
               -command "$oldthis.text xview"
            pack $oldthis.bottom.scrollbottom -side left -fill x -expand true
            $oldthis.text configure -xscrollcommand "$oldthis.bottom.scrollbottom set"
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
         pack $oldthis.text -expand true -fill both
      }

#  Method to return the name of the text widget.
      method textname {} {
         return $oldthis.text
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert and pack the required scrollbars. Remove existing scrollbars first.
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
               pack $oldthis.label -side top -anchor w
               _repack $scrollbarplaces
            }
         } else {

#  Remove existing label.
            if { [ winfo exists $oldthis.label ] } {
               pack forget $oldthis.label
	       destroy $oldthis.label
	    }
         }
      }

#  Is the selection exportable? If not may have more than one selection
#  (one for each instance), otherwise the selection is the X11 one.
      public exportselect 1 {
         if { [ winfo exists $oldthis.text ] } {
            $oldthis.text configure -exportselection $exportselect
         }
      }

#  Height of text widget.
      public height 20 {
         if $exists {
            $oldthis.text configure -width $width -height $height
         }
      }

#  Width of text widget.
      public width 80 {
         if $exists {
            $oldthis.text configure -width $width -height $height
         }
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Protected variables: visible to only this instance.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      protected lastupdate 0

#  End of class defintion.
   }


# $Id$
